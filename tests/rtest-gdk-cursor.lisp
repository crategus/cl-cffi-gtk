;;; ----------------------------------------------------------------------------
;;; rtest-gdk-cursor.lisp
;;;
;;; Copyright (C) 2012 Dieter Kaiser
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

(in-package :gdk-tests)

(define-test gdk-cursor
  (let* ((cursor (gdk-cursor-new :hand1))
         (display (gdk-cursor-get-display cursor))
         (image (gdk-cursor-get-image cursor)))
    (assert-eq  :hand1 (gdk-cursor-get-cursor-type cursor))
    (assert-eql 'gdk-display (type-of (gdk-cursor-get-display cursor)))
    (assert-eql 'gdk-pixbuf (type-of (gdk-cursor-get-image cursor)))
    (assert-eql 'gdk-cursor
                (type-of (gdk-cursor-new-from-pixbuf display image 0 0)))
    (assert-eql 'gdk-cursor
                (type-of (gdk-cursor-new-from-name display "hand1")))
    (assert-eql 'gdk-cursor
                (type-of (gdk-cursor-new-for-display display :hand1)))))

