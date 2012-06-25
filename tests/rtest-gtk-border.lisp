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
           ;; entry has the property inner-border of type gtk-border.
           (entry (make-instance 'gtk-entry :inner-border border)))
      (assert-eql 1 (gtk-border-left border))
      (assert-eql 2 (gtk-border-right border))
      (assert-eql 3 (gtk-border-top border))
      (assert-eql 4 (gtk-border-bottom border))
      
      (assert-eql 1 (gtk-border-left (gtk-entry-get-inner-border entry)))
      (assert-eql 2 (gtk-border-right (gtk-entry-get-inner-border entry)))
      (assert-eql 3 (gtk-border-top (gtk-entry-get-inner-border entry)))
      (assert-eql 4 (gtk-border-bottom (gtk-entry-get-inner-border entry)))
      
      (setq border (make-gtk-border :left 10 :right 20 :top 30 :bottom 40))
      (gtk-entry-set-inner-border entry border)
      
      (assert-eql 10 (gtk-border-left (gtk-entry-get-inner-border entry)))
      (assert-eql 20 (gtk-border-right (gtk-entry-get-inner-border entry)))
      (assert-eql 30 (gtk-border-top (gtk-entry-get-inner-border entry)))
      (assert-eql 40 (gtk-border-bottom (gtk-entry-get-inner-border entry)))
      ))

;;; --- End of file rtest-gtk-border.lisp --------------------------------------
