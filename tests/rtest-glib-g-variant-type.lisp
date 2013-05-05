;;; ----------------------------------------------------------------------------
;;; rtest-glib-g-variant-type.lisp
;;;
;;; Copyright (C) 2013 Dieter Kaiser
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

(in-package :glib-tests)

(define-test g-variant-type
  (assert-equal "b"     +g-variant-type-boolean+)
  (assert-equal "y"     +g-variant-type-byte+)
  (assert-equal "n"     +g-variant-type-int16+)
  (assert-equal "q"     +g-variant-type-uint16+)
  (assert-equal "i"     +g-variant-type-int32+)
  (assert-equal "u"     +g-variant-type-uint32+)
  (assert-equal "x"     +g-variant-type-int64+)
  (assert-equal "t"     +g-variant-type-uint64+)
  (assert-equal "h"     +g-variant-type-handle+)
  (assert-equal "d"     +g-variant-type-double+)
  (assert-equal "s"     +g-variant-type-string+)
  (assert-equal "o"     +g-variant-type-object-path+)
  (assert-equal "g"     +g-variant-type-signature+)
  (assert-equal "v"     +g-variant-type-variant+)
  (assert-equal "*"     +g-variant-type-any+)
  (assert-equal "?"     +g-variant-type-basic+)
  (assert-equal "m*"    +g-variant-type-maybe+)
  (assert-equal "a*"    +g-variant-type-array+)
  (assert-equal "r"     +g-variant-type-tuple+)
  (assert-equal "()"    +g-variant-type-unit+)
  (assert-equal "{?*}"  +g-variant-type-dict-entry+)
  (assert-equal "a{?*}" +g-variant-type-dictionary+)
  (assert-equal "as"    +g-variant-type-string-array+)
  (assert-equal "ao"    +g-variant-type-object-path-array+)
  (assert-equal "ay"    +g-variant-type-bytestring+)
  (assert-equal "aay"   +g-variant-type-bytestring-array+)
  (assert-equal "a(sv)" +g-variant-type-vardict+)

  (assert-true (g-variant-type-string-is-valid "b"))
  (assert-true (g-variant-type-string-is-valid "y"))
  (assert-true (g-variant-type-string-is-valid "n"))
  (assert-true (g-variant-type-string-is-valid "q"))
  (assert-true (g-variant-type-string-is-valid "i"))
  (assert-true (g-variant-type-string-is-valid "u"))
  (assert-true (g-variant-type-string-is-valid "x"))
  (assert-true (g-variant-type-string-is-valid "t"))
  (assert-true (g-variant-type-string-is-valid "h"))
  (assert-true (g-variant-type-string-is-valid "d"))
  (assert-true (g-variant-type-string-is-valid "s"))
  (assert-true (g-variant-type-string-is-valid "o"))
  (assert-true (g-variant-type-string-is-valid "g"))
  (assert-true (g-variant-type-string-is-valid "v"))
  (assert-true (g-variant-type-string-is-valid "*"))
  (assert-true (g-variant-type-string-is-valid "?"))
  (assert-true (g-variant-type-string-is-valid "m*"))
  (assert-true (g-variant-type-string-is-valid "a*"))
  (assert-true (g-variant-type-string-is-valid "r"))
  (assert-true (g-variant-type-string-is-valid "()"))
  (assert-true (g-variant-type-string-is-valid "{?*}"))
  (assert-true (g-variant-type-string-is-valid "a{?*}"))
  (assert-true (g-variant-type-string-is-valid "as"))
  (assert-true (g-variant-type-string-is-valid "ao"))
  (assert-true (g-variant-type-string-is-valid "ay"))
  (assert-true (g-variant-type-string-is-valid "aay"))
  (assert-true (g-variant-type-string-is-valid "a(sv)"))

  (let ((type (g-variant-type-new "b")))
    (assert-equal 1 (g-variant-type-get-string-length type))
    (assert-equal "b" (g-variant-type-peek-string type))
    (assert-equal "b" (g-variant-type-dup-string type))
    (assert-equal "b" (g-variant-type-peek-string (g-variant-type-copy type)))
    (assert-true  (g-variant-type-is-definite type))
    (assert-false (g-variant-type-is-container type))
    (assert-true  (g-variant-type-is-basic type))
    (assert-false (g-variant-type-is-maybe type))
    (assert-false (g-variant-type-is-array type))
    (assert-false (g-variant-type-is-tuple type))
    (assert-false (g-variant-type-is-dict-entry type))
    (assert-false (g-variant-type-is-variant type))
    (assert-true  (g-variant-type-equal type (g-variant-type-new "b")))
    (assert-false (g-variant-type-equal type (g-variant-type-new "y")))
    (assert-true  (g-variant-type-is-subtype-of type (g-variant-type-new "b")))
    (assert-true  (g-variant-type-is-subtype-of type (g-variant-type-new "*")))
    (assert-false (g-variant-type-is-subtype-of type (g-variant-type-new "y")))
    (assert-equal "mb" (g-variant-type-dup-string (g-variant-type-new-maybe type)))
    (assert-equal "ab" (g-variant-type-dup-string (g-variant-type-new-array type)))
;    (assert-equal 3 (g-variant-type-dup-string (g-variant-type-new-tuple (list type type))))
  )

)

