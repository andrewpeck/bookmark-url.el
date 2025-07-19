;; bookmark-url.el --- -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Andrew Peck

;; Author: Andrew Peck <peckandrew@gmail.com>
;; URL: https://github.com/andrewpeck/bookmark-url.el
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (marginalia "2.0"))
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>

;;; Commentary:
;;

;;; Code:

(require 'marginalia)
(require 'cl-lib)
(require 'json)

(defun bookmark-url--load-from-file (file)
  "Load bookmark alist from a FILE."

  (pcase (file-name-extension file)
    ("el" (with-current-buffer (find-file-noselect file)
            (goto-char (point-min))
            (if-let ((read-value (read (current-buffer))))
                read-value
              (error (format "Failed to read from %s" file)))))
    ("json"
     (let ((json-key-type 'string))
       (json-read-file file)))
    (_ (error (format "Unrecognized extension on %s file. Must be json or el" file)))))

(defun bookmark-url--save-to-file (alist file)
  "Write ALIST to FILE."

  (let ((extension (file-name-extension file)))

    (when (not (member extension '("el" "json")))
      (error "Extension %s not recognized for encoding %s" extension file))

    ;; open buffer, write contents
    (with-temp-buffer
      (goto-char (point-min))
      (delete-region (point-min) (point-max))

      (pcase extension
        ("el" (progn (insert "(\n")
                     (dolist (i alist) (pp i (current-buffer)))
                     (insert ")\n")))
        ("json" (progn (let ((json-encoding-pretty-print t))
                         (if-let ((encoded (json-encode alist)))
                             (insert encoded)
                           (error (format "Failed to encode alist for writing to %s" file)))))))
      (write-file file))))

(defun bookmark-url--add-bookmark (file)
  "Add new bookmark to FILE."
  (let ((url (read-string "URL: "))
        (description (read-string "Description: ")))
    (when (and url description)
      (let ((current-alist (bookmark-url--load-from-file file)))
        (push (cons description url) current-alist)
        (bookmark-url--save-to-file current-alist file)))))

;;;###autoload
(cl-defun bookmark-url-setup (search-function &key bookmarks-file bookmarks-alist open-function prompt)
  "Create a bookmarking function from a BOOKMARKS-ALIST or BOOKMARKS-FILE.

This will create a marginalia annotated completing read SEARCH-FUNCTION.

The BOOKMARKS-ALIST should be of the form (DESCRIPTION . URL):

(setq bookmarks-alist \\=((\"Google\" . \"https://google.com\")
(\"Yahoo\" . \"https://yahoo.com\")))

The search functions can then be set up with

(bookmark-url-create \\=find-search-engines
                    \\=bookmarks-alist
                    :prompt \"Search Engine\")

It should now be searchable via `M-x find-search-engines`.

The optional :prompt argument provides a hint during completing read."

  (when (not (or bookmarks-file bookmarks-alist))
    (error "`bookmark-url-create` requires either a :bookmarks-file or :bookmarks-alist argument"))

  (when (and bookmarks-file bookmarks-alist)
    (error "`bookmark-url-create` cannot have both a :bookmarks-file and a :bookmarks-alist argument"))

  (let ((annotator-function (intern (concat (symbol-name search-function) "--annotator")))
        (add-function (intern (concat (symbol-name search-function) "-add-bookmark")))
        (category (intern (concat (symbol-name search-function) "-category")))
        (alist-name (or bookmarks-alist (intern (concat (symbol-name search-function) "-bookmark-alist")))))

    (unless prompt
      (setq prompt (intern search-function)))


    (when bookmarks-file

      ;; add a defvar for alist-name so it has a docstring?
      (let ((docstr (format "Bookmarks file for for %s" (symbol-name search-function))))
        (eval `(defvar ,alist-name nil ,docstr)))

      ;; add a function for adding bookmarks
      (defalias add-function
        `(lambda ()
           (interactive)
           (bookmark-url--add-bookmark ,bookmarks-file))
        (format "Add a new bookmark to %s" bookmarks-file)))

    (defalias search-function
      `(lambda ()
         (interactive)
         ;; read alist from file, write into variable so it is cached
         (when ,bookmarks-file
           (setq ,alist-name (bookmark-url--load-from-file ,bookmarks-file)))
         (let* ((target (completing-read
                         ,(concat prompt ": ")
                         (mapcar 'car ,alist-name) nil t))
                (url (cdr (assoc target ,alist-name))))
           (funcall (quote ,(or open-function #'browse-url)) url)))
      (concat "Search for " prompt))

    (defalias annotator-function
      `(lambda (cand)
         (marginalia--fields ((cdr (assoc cand ,alist-name)) :face 'link)))
      (format "Marginalia annotator for %s" (symbol-name search-function)))

    (add-to-list 'marginalia-annotators `(,category ,annotator-function none))
    (add-to-list 'marginalia-command-categories `(,search-function . ,category))) t)

(provide 'bookmark-url)
;;; bookmark-url.el ends here
