;;; ----------------------------------------------------------------------------
;;; rtest-gobject-param-spec.lisp
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

(in-package :gobject-tests)

(define-test gobject-param-spec
  (let ((param (g-param-spec-boolean "aBoolean" 
                                     "aBool"
                                     "Description" 
                                     t
                                     '(:readable :writable))))
    (assert-true (g-type-is-param (g-type-from-instance param)))
    (assert-true (g-is-param-spec param))
    (assert-equal "GParamBoolean" (g-type-name (g-param-spec-type param)))
    (assert-equal "GParamBoolean" (g-param-spec-type-name param))
    (assert-equal "gboolean" (g-type-name (g-param-spec-value-type param)))

  ))

(define-test gobject-param-spec-int
  (with-foreign-object (value 'g-value)
    (let ((prop (g-param-spec-int "PropertyInteger"
                                  "PSpecInt"
                                  "Integer Property"
                                  0
                                  100
                                  50
                                  '(:readable :writable))))
    (assert-equal "PropertyInteger"  (g-param-spec-get-name prop))
    (assert-equal "PSpecInt"         (g-param-spec-get-nick prop))
    (assert-equal "Integer Property" (g-param-spec-get-blurb prop))
    (g-value-init value +g-type-int+)
    (g-param-value-set-default prop value)
    (assert-eql 50 (g-value-get-int value))
    (assert-true (g-param-value-defaults prop value))
    (assert-false (g-param-value-validate prop value))
    (g-value-set-int value 199)
    (assert-eql 199 (g-value-get-int value))
    (assert-true (g-param-value-validate prop value))
    (assert-eql 100 (g-value-get-int value))
      )))

