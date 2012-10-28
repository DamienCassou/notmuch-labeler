;;; notmuch-labeler-test.el --- Check notmuch-labeler implementation
;;
;; Copyright (C) 2012 Damien Cassou
;;
;; Author: Damien Cassou <damien.cassou@gmail.com>
;; Url: https://github.com/DamienCassou/notmuch-labeler
;; GIT: https://github.com/DamienCassou/notmuch-labeler
;; Version: 0.1
;; Created: 2012-10-01
;; Keywords: emacs package elisp notmuch emails ert tests
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
;;; Code:

(require 'ert)
(require 'notmuch-labeler)

(ert-deftest nml--extract-labels-from-simple-query ()
  "Check that we can find a label within a query"
  (should (equal nil (nml--extract-labels-from-query "")))
  (should (equal nil (nml--extract-labels-from-query "from:someone")))
  (should (equal '("foo") (nml--extract-labels-from-query "label:foo"))))


(ert-deftest nml--extract-labels-from-other-query ()
  "Check that we can find labels within a query"
  :expected-result :failed
  ;; in case of an OR, there is no particular folder to select
  (should (equal nil (nml--extract-labels-from-query "label:foo OR label:bar")))
  ;; in case of an AND, both folders muct be answered
  (should (equal '("foo" "bar") (nml--extract-labels-from-query "label:foo AND label:bar")))
  (should (equal '("foo" "bar") (nml--extract-labels-from-query "label:foo label:bar"))))

(provide 'notmuch-labeler-test)

;;; notmuch-labeler-test.el ends here
