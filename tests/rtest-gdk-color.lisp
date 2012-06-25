;;; ----------------------------------------------------------------------------
;;; rtest-gdk-color.lisp
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

(define-test gdk-color
  (let ((color (make-gdk-color :red 255)))
    (assert-eql 'gdk-color (type-of color))
    (assert-eql 'gdk-color (type-of (gdk-color-copy color)))
    (assert-eql 'gdk-color (type-of (gdk-color-parse "Red")))
    (assert-equal "#00ff00000000" (gdk-color-to-string color))
    (assert-equal "#ffff00000000" (gdk-color-to-string (gdk-color-parse "Red")))
    (assert-true (gdk-color-equal color (gdk-color-copy color)))
    (assert-eql 255 (gdk-color-hash color))))

