;;; bookmark-url.el --- -*- lexical-binding: t; -*-
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

(defun bookmark-url-setup (search-function bookmarks &key name)
  ""

  (let ((annotator-function (intern (concat (symbol-name search-function) "--annotator")))
        (category (intern (concat (symbol-name search-function) "-category"))))

    (unless name
      (setq name (intern search-function)))

    (defalias search-function
      `(lambda ()
         (interactive)
         (let* ((target (completing-read
                         ,(concat name ":")
                         (alist-keys ,bookmarks) nil t))
                (url (cdr (assoc target ,bookmarks))))
           (browse-url url)))
      (concat "Search for " name))

    (defalias annotator-function
      `(lambda (cand)
         (marginalia--fields ((cdr (assoc cand ,bookmarks)) :face 'link)))
      (format "Marginalia annotator for %s" (symbol-name search-function)))

    (add-to-list 'marginalia-annotators `(,category ,annotator-function none))
    (add-to-list 'marginalia-command-categories `(,search-function . ,category))) t)

(provide 'bookmark-url)
;;; bookmark-url.el ends here
