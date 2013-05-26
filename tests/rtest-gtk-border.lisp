;;; ----------------------------------------------------------------------------
;;; rtest-gtk-border.lisp
;;;
;;; Copyright (C) 2011 - 2012 Dieter Kaiser
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU Lesser General Public License for Lisp
;;; as published by the Free Software Foundation, either version 3 of the
;;; License, or (at your option) any later version and with a preamble to
;;; the GNU Lesser General Public License that clarifies the terms for use
;;; with Lisp programs and is referred as the LLGPL.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU Lesser General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with this program and the preamble to the Gnu Lesser
;;; General Public License.  If not, see <http://www.gnu.org/licenses/>
;;; and <http://opensource.franz.com/preamble.html>.
;;; ----------------------------------------------------------------------------

(in-package :gtk-tests)

(define-test gtk-border
  (let* ((border (make-gtk-border :left 1 :right 2 :top 3 :bottom 4))
         ;; button has the style property default-border of type gtk-border.
         (button (make-instance 'gtk-button)))

      (assert-eql 1 (gtk-border-left border))
      (assert-eql 2 (gtk-border-right border))
      (assert-eql 3 (gtk-border-top border))
      (assert-eql 4 (gtk-border-bottom border))

      (let ((copy (gtk-border-copy border)))
        (assert-eql 1 (gtk-border-left copy))
        (assert-eql 2 (gtk-border-right copy))
        (assert-eql 3 (gtk-border-top copy))
        (assert-eql 4 (gtk-border-bottom copy)))

      (setf border (gtk-border-new))

      (assert-eql 0 (gtk-border-left border))
      (assert-eql 0 (gtk-border-right border))
      (assert-eql 0 (gtk-border-top border))
      (assert-eql 0 (gtk-border-bottom border))

      ;; The "default-border" style property is of type gtk-border
      (setf border (gtk-widget-style-get-property button "default-border"))
      (assert-eq 'gtk-border (type-of border))

      (assert-eql 0 (gtk-border-left border))
      (assert-eql 0 (gtk-border-right border))
      (assert-eql 0 (gtk-border-top border))
      (assert-eql 0 (gtk-border-bottom border))))

;;; --- End of file rtest-gtk-border.lisp --------------------------------------
