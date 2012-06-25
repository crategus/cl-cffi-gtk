;;; ----------------------------------------------------------------------------
;;; rtest-gdk-rgba.lisp
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

(define-test gdk-rgba
  (let ((color (make-gdk-rgba :red 1.0d0)))
    (assert-eql 'gdk-rgba (type-of color))
    (assert-eql 'gdk-rgba (type-of (gdk-rgba-copy color)))
    (assert-eql 'gdk-rgba (type-of (gdk-rgba-parse "Red")))
    (assert-equal "rgba(255,0,0,0)" (gdk-rgba-to-string color))
    (assert-equal "rgb(255,0,0)" (gdk-rgba-to-string (gdk-rgba-parse "Red")))
    (assert-true (gdk-rgba-equal color (gdk-rgba-copy color)))
    (assert-eql 65535 (gdk-rgba-hash color))))

