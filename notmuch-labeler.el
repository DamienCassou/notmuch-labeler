;;; notmuch-labeler.el --- Improve notmuch way of displaying labels and make them interactive
;;; Commentary:
;;
;; Add the following to your .emacs.d/init.el file:
;;
;; (require 'notmuch-labeler)
;; (require 'notmuch-labeler-plug)
;;
;; Add more lines that indicate how you want to display each of your
;; labels. For example, the following renames the label "unread" to
;; "new" and change the color:

;; (notmuch-labeler-rename "unread" "new" '(:foreground "blue"))
;;
;; The following replaces the label "Important" with a star icon:
;;
;; (notmuch-labeler-image "Important"
;;   "/home/cassou/.emacs.d/notmuch-labeler/resources/star.svg" 'svg)
;;
(require 'notmuch)

;;; Code:

(defgroup notmuch-labeler nil
  "Enhance the way labels are displayed."
  :group 'notmuch)

(defvar nml--formats (make-hash-table :test 'equal)
  "Map a label to a property list that visually represents it.")

(defun nml--reset-formats ()
  "Remove all formats that the user associated to labels."
  (setq nml--formats (make-hash-table :test 'equal)))

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
  (puthash tag "" nml--formats))

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

(provide 'notmuch-labeler)

(provide 'notmuch-labeler)

;;; notmuch-labeler.el ends here
