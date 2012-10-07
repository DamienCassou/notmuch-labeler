;;; notmuch-labeler.el --- Improve notmuch way of displaying labels
;;
;; Copyright (C) 2012 Damien Cassou
;;
;; Author: Damien Cassou <damien.cassou@gmail.com>
;; Url: https://github.com/DamienCassou/notmuch-labeler
;; GIT: https://github.com/DamienCassou/notmuch-labeler
;; Version: 0.1
;; Created: 2012-10-01
;; Keywords: emacs package elisp notmuch emails
;;
;; This file is NOT part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program ; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
;;
;;; Commentary:
;;
;; By default notmuch presents email labels as plain text. This
;; package improves notmuch by lettings users choose how to present
;; each label (e.g., with a special font, with a picture, ...).
;; Additionally, this package transforms each label into an hyperlink
;; to show all emails with this label.
;;
;; To configure this package, add the following to your
;; .emacs.d/init.el file:
;;
;; (require 'notmuch-labeler)
;;
;; Then, you will get hyperlinks on all your labels. Now, if you want
;; to change the default presentation of a label, write something like
;; the following.
;;
;; For example, the following renames the label "unread" to "new" and
;; changes the label color to blue:
;;
;; (notmuch-labeler-rename "unread" "new" ':foreground "blue")
;;
;; This replaces the label "important" by a tag picture:
;;
;; (notmuch-labeler-image-tag "important")
;;
;; This simply hides the label "unread" (there is no need to show this
;; label because unread messages are already in bold):
;;
;; (notmuch-labeler-hide "unread")
;;
(require 'notmuch)

;;; Code:

(defvar nml--location (file-name-directory (locate-library "notmuch-labeler")))

(defgroup notmuch-labeler nil
  "Enhance the way labels are displayed."
  :group 'notmuch)

(defvar nml--formats (make-hash-table :test 'equal)
  "Map a label to a property list that visually represents it.")

(defun nml--reset-formats ()
  "Remove all formats that the user associated to labels."
  (setq nml--formats (make-hash-table :test 'equal)))

(defun nml--image-path (image)
  "Get full path for IMAGE name in the resources/ sub-directory."
  (expand-file-name (concat "resources/" image) nml--location))

(defun notmuch-labeler-rename (label new-name &rest face)
  "Rename LABEL to NEW-NAME, optionally with a particular FACE."
  (puthash
   label
   (if face
       `(:propertize ,new-name face ,face)
     new-name)
   nml--formats))

(defun notmuch-labeler-hide (label)
  "Do never show LABEL."
  (puthash label "" nml--formats))

(defun notmuch-labeler-image (label file type)
  "Show LABEL as an image taken from FILE with type TYPE.
See Info node `(elisp)Image Formats' for possible values for
TYPE."
  (puthash
   label
   `(:propertize ,label display
		 (image :type ,type
			:file ,file
			:ascent center
			:mask heuristic))
   nml--formats))

(defun notmuch-labeler-provided-image (label image)
  "Show LABEL as an image provided by notmuch-labeler."
  (notmuch-labeler-image
   label
   (nml--image-path image)
   (intern (file-name-extension image))))

(defun notmuch-labeler-image-star (label)
  "Show LABEL as the resources/star.svg image."
  (notmuch-labeler-provided-image label "star.svg"))

(defun notmuch-labeler-image-tag (label)
  "Show LABEL as the resources/tag.svg image."
  (notmuch-labeler-provided-image label "tag.svg"))

(defun nml--separate-elems (list sep)
  "Return a list with all elements of LIST separated by SEP."
  (let ((first t)
	(res nil))
    (dolist (elt (reverse list) res)
      (unless first
	(push sep res))
      (setq first nil)
      (push elt res))))

(defun nml--format-labels (labels)
  "Return a format list for LABELS suitable for use in header line.
See Info node `(elisp)Mode Line Format' for more information."
  (let ((chosen-labels nil))
    (dolist (label labels chosen-labels)
      (let ((format (gethash label nml--formats)))
	(if (null format) ;; no format => we use the original name
	    (push (nml--make-link label label) chosen-labels)
	  (unless (zerop (length format)) ;; explicit discard of the label
	    (push (nml--make-link format label) chosen-labels)))))))

(defun nml--make-link (format target)
  "Return a property list that make FORMAT a link to TARGET."
  (cond
   ((and (consp format) (equal (car format) ':propertize))
    (append format (nml--link-properties target)))
   ((stringp format)
    `(:propertize ,format ,@(nml--link-properties target)))))

(defun nml--link-properties (target)
  "Return a property list for a link to TARGET."
(lexical-let ((target target)) ;; lexical binding so that next
				 ;; lambda becomes a closure
    (let ((map (make-sparse-keymap))
	  (goto (lambda ()
		  (interactive)
		  (nml--goto-target target))))
      (define-key map [mouse-2] goto)
      (define-key map [follow-link] 'mouse-face) ;; handle mouse-1
      (define-key map (kbd "RET") goto)
      (list 'mouse-face '(highlight) ;
	    'help-echo "Search other messages like this" ;
	    'keymap map))))

(defun nml--goto-target (target)
  "Show a `notmuch-search' buffer for the TARGET label."
  (notmuch-search (concat "tag:" target)))

(defun nml--present-labels (labels)
  "Return a property list which nicely presents all LABELS."
  (list
   " ("
   (nml--separate-elems (nml--format-labels labels) ", ")
   ")"))

(require 'notmuch-labeler-plug)

(provide 'notmuch-labeler)

;;; notmuch-labeler.el ends here
