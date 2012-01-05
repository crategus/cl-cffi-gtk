;;; ----------------------------------------------------------------------------
;;; rtest-gobject.lisp
;;;
;;; Copyright (C) 2011 - 2012 Dr. Dieter Kaiser
;;;
;;; ----------------------------------------------------------------------------
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

(asdf:operate 'asdf:load-op :cl-gtk-gobject)

(defpackage :gobject-tests
  (:use :gobject :glib :cffi :common-lisp :lisp-unit))

(in-package :gobject-tests)

(define-test gtype-id
  (assert-eql (ash  1 2) (gtype-id (gtype +g-type-void+)))
  (assert-eql (ash  2 2) (gtype-id (gtype +g-type-interface+)))
  (assert-eql (ash  3 2) (gtype-id (gtype +g-type-char+)))
  (assert-eql (ash  4 2) (gtype-id (gtype +g-type-uchar+)))
  (assert-eql (ash  5 2) (gtype-id (gtype +g-type-boolean+)))
  (assert-eql (ash  6 2) (gtype-id (gtype +g-type-int+)))
  (assert-eql (ash  7 2) (gtype-id (gtype +g-type-uint+)))
  (assert-eql (ash  8 2) (gtype-id (gtype +g-type-long+)))
  (assert-eql (ash  9 2) (gtype-id (gtype +g-type-ulong+)))
  (assert-eql (ash 10 2) (gtype-id (gtype +g-type-int64+)))
  (assert-eql (ash 11 2) (gtype-id (gtype +g-type-uint64+)))
  (assert-eql (ash 12 2) (gtype-id (gtype +g-type-enum+)))
  (assert-eql (ash 13 2) (gtype-id (gtype +g-type-flags+)))
  (assert-eql (ash 14 2) (gtype-id (gtype +g-type-float+)))
  (assert-eql (ash 15 2) (gtype-id (gtype +g-type-double+)))
  (assert-eql (ash 16 2) (gtype-id (gtype +g-type-string+)))
  (assert-eql (ash 17 2) (gtype-id (gtype +g-type-pointer+)))
  (assert-eql (ash 18 2) (gtype-id (gtype +g-type-boxed+)))
  (assert-eql (ash 19 2) (gtype-id (gtype +g-type-param+)))
  (assert-eql (ash 20 2) (gtype-id (gtype +g-type-object+))))

(define-test gtype-from-id
  (assert-eql (ash  1 2) (gtype-id (gtype-from-id +g-type-void+)))
  (assert-eql (ash  2 2) (gtype-id (gtype-from-id +g-type-interface+)))
  (assert-eql (ash  3 2) (gtype-id (gtype-from-id +g-type-char+)))
  (assert-eql (ash  4 2) (gtype-id (gtype-from-id +g-type-uchar+)))
  (assert-eql (ash  5 2) (gtype-id (gtype-from-id +g-type-boolean+)))
  (assert-eql (ash  6 2) (gtype-id (gtype-from-id +g-type-int+)))
  (assert-eql (ash  7 2) (gtype-id (gtype-from-id +g-type-uint+)))
  (assert-eql (ash  8 2) (gtype-id (gtype-from-id +g-type-long+)))
  (assert-eql (ash  9 2) (gtype-id (gtype-from-id +g-type-ulong+)))
  (assert-eql (ash 10 2) (gtype-id (gtype-from-id +g-type-int64+)))
  (assert-eql (ash 11 2) (gtype-id (gtype-from-id +g-type-uint64+)))
  (assert-eql (ash 12 2) (gtype-id (gtype-from-id +g-type-enum+)))
  (assert-eql (ash 13 2) (gtype-id (gtype-from-id +g-type-flags+)))
  (assert-eql (ash 14 2) (gtype-id (gtype-from-id +g-type-float+)))
  (assert-eql (ash 15 2) (gtype-id (gtype-from-id +g-type-double+)))
  (assert-eql (ash 16 2) (gtype-id (gtype-from-id +g-type-string+)))
  (assert-eql (ash 17 2) (gtype-id (gtype-from-id +g-type-pointer+)))
  (assert-eql (ash 18 2) (gtype-id (gtype-from-id +g-type-boxed+)))
  (assert-eql (ash 19 2) (gtype-id (gtype-from-id +g-type-param+)))
  (assert-eql (ash 20 2) (gtype-id (gtype-from-id +g-type-object+))))

(define-test gtype-name
  (assert-equal "void"       (gtype-name (gtype +g-type-void+)))
  (assert-equal "GInterface" (gtype-name (gtype +g-type-interface+)))
  (assert-equal "gchar"      (gtype-name (gtype +g-type-char+)))
  (assert-equal "guchar"     (gtype-name (gtype +g-type-uchar+)))
  (assert-equal "gboolean"   (gtype-name (gtype +g-type-boolean+)))
  (assert-equal "gint"       (gtype-name (gtype +g-type-int+)))
  (assert-equal "guint"      (gtype-name (gtype +g-type-uint+)))
  (assert-equal "glong"      (gtype-name (gtype +g-type-long+)))
  (assert-equal "gulong"     (gtype-name (gtype +g-type-ulong+)))
  (assert-equal "gint64"     (gtype-name (gtype +g-type-int64+)))
  (assert-equal "guint64"    (gtype-name (gtype +g-type-uint64+)))
  (assert-equal "GEnum"      (gtype-name (gtype +g-type-enum+)))
  (assert-equal "GFlags"     (gtype-name (gtype +g-type-flags+)))
  (assert-equal "gfloat"     (gtype-name (gtype +g-type-float+)))
  (assert-equal "gdouble"    (gtype-name (gtype +g-type-double+)))
  (assert-equal "gchararray" (gtype-name (gtype +g-type-string+)))
  (assert-equal "gpointer"   (gtype-name (gtype +g-type-pointer+)))
  (assert-equal "GBoxed"     (gtype-name (gtype +g-type-boxed+)))
  (assert-equal "GParam"     (gtype-name (gtype +g-type-param+)))
  (assert-equal "GObject"    (gtype-name (gtype +g-type-object+))))

(define-test gtype-from-name
  (assert-eql (ash  1 2) (gtype-id (gtype-from-name "void")))
  (assert-eql (ash  2 2) (gtype-id (gtype-from-name "GInterface")))
  (assert-eql (ash  3 2) (gtype-id (gtype-from-name "gchar")))
  (assert-eql (ash  4 2) (gtype-id (gtype-from-name "guchar")))
  (assert-eql (ash  5 2) (gtype-id (gtype-from-name "gboolean")))
  (assert-eql (ash  6 2) (gtype-id (gtype-from-name "gint")))
  (assert-eql (ash  7 2) (gtype-id (gtype-from-name "guint")))
  (assert-eql (ash  8 2) (gtype-id (gtype-from-name "glong")))
  (assert-eql (ash  9 2) (gtype-id (gtype-from-name "gulong")))
  (assert-eql (ash 10 2) (gtype-id (gtype-from-name "gint64")))
  (assert-eql (ash 11 2) (gtype-id (gtype-from-name "guint64")))
  (assert-eql (ash 12 2) (gtype-id (gtype-from-name "GEnum")))
  (assert-eql (ash 13 2) (gtype-id (gtype-from-name "GFlags")))
  (assert-eql (ash 14 2) (gtype-id (gtype-from-name "gfloat")))
  (assert-eql (ash 15 2) (gtype-id (gtype-from-name "gdouble")))
  (assert-eql (ash 16 2) (gtype-id (gtype-from-name "gchararray")))
  (assert-eql (ash 17 2) (gtype-id (gtype-from-name "gpointer")))
  (assert-eql (ash 18 2) (gtype-id (gtype-from-name "GBoxed")))
  (assert-eql (ash 19 2) (gtype-id (gtype-from-name "GParam")))
  (assert-eql (ash 20 2) (gtype-id (gtype-from-name "GObject"))))

(define-test %g-type-name
  (assert-equal "void"       (g::%g-type-name +g-type-void+))
  (assert-equal "GInterface" (g::%g-type-name +g-type-interface+))
  (assert-equal "gchar"      (g::%g-type-name +g-type-char+))
  (assert-equal "guchar"     (g::%g-type-name +g-type-uchar+))
  (assert-equal "gboolean"   (g::%g-type-name +g-type-boolean+))
  (assert-equal "gint"       (g::%g-type-name +g-type-int+))
  (assert-equal "guint"      (g::%g-type-name +g-type-uint+))
  (assert-equal "glong"      (g::%g-type-name +g-type-long+))
  (assert-equal "gulong"     (g::%g-type-name +g-type-ulong+))
  (assert-equal "gint64"     (g::%g-type-name +g-type-int64+))
  (assert-equal "guint64"    (g::%g-type-name +g-type-uint64+))
  (assert-equal "GEnum"      (g::%g-type-name +g-type-enum+))
  (assert-equal "GFlags"     (g::%g-type-name +g-type-flags+))
  (assert-equal "gfloat"     (g::%g-type-name +g-type-float+))
  (assert-equal "gdouble"    (g::%g-type-name +g-type-double+))
  (assert-equal "gchararray" (g::%g-type-name +g-type-string+))
  (assert-equal "gpointer"   (g::%g-type-name +g-type-pointer+))
  (assert-equal "GBoxed"     (g::%g-type-name +g-type-boxed+))
  (assert-equal "GParam"     (g::%g-type-name +g-type-param+))
  (assert-equal "GObject"    (g::%g-type-name +g-type-object+)))

(define-test g-type-is-value-type
  (assert-false (g-type-is-value-type (gtype +g-type-void+)))
  (assert-false (g-type-is-value-type (gtype +g-type-interface+)))
  (assert-true  (g-type-is-value-type (gtype +g-type-char+)))
  (assert-true  (g-type-is-value-type (gtype +g-type-uchar+)))
  (assert-true  (g-type-is-value-type (gtype +g-type-boolean+)))
  (assert-true  (g-type-is-value-type (gtype +g-type-int+)))
  (assert-true  (g-type-is-value-type (gtype +g-type-uint+)))
  (assert-true  (g-type-is-value-type (gtype +g-type-long+)))
  (assert-true  (g-type-is-value-type (gtype +g-type-ulong+)))
  (assert-true  (g-type-is-value-type (gtype +g-type-int64+)))
  (assert-true  (g-type-is-value-type (gtype +g-type-uint64+)))
  (assert-false (g-type-is-value-type (gtype +g-type-enum+)))
  (assert-false (g-type-is-value-type (gtype +g-type-flags+)))
  (assert-true  (g-type-is-value-type (gtype +g-type-float+)))
  (assert-true  (g-type-is-value-type (gtype +g-type-double+)))
  (assert-true  (g-type-is-value-type (gtype +g-type-string+)))
  (assert-true  (g-type-is-value-type (gtype +g-type-pointer+)))
  (assert-false (g-type-is-value-type (gtype +g-type-boxed+)))
  (assert-true  (g-type-is-value-type (gtype +g-type-param+)))
  (assert-true  (g-type-is-value-type (gtype +g-type-object+))))

(define-test g-type-depth
  ;; The depth of a fundamental type is always 1. The examples show the
  ;; different possibilieties to pass an argument to a foreign function.
  (assert-eql 1 (g-type-depth +g-type-void+))         ; an ID as argument
  (assert-eql 1 (g-type-depth "GInterface"))          ; a string as an argument
  (assert-eql 1 (g-type-depth (gtype +g-type-char+))) ; a GType as an argument
  )

(define-test g-type-is-a
  ;; Again different methods to pass an argument
  (assert-true  (g-type-is-a +g-type-long+ (gtype +g-type-long+)))
  (assert-false (g-type-is-a +g-type-long+ (gtype +g-type-string+)))
  (assert-true  (g-type-is-a "gdouble" +g-type-double+)))

(define-test g-type-flags
  (assert-equal '(:abstract)
                (cffi:foreign-bitfield-symbols 'g-type-flags (ash 1 4)))
  (assert-equal '(:value-abstract)
                (cffi:foreign-bitfield-symbols 'g-type-flags (ash 1 5)))
  (assert-equal '(:abstract :value-abstract)
                (cffi:foreign-bitfield-symbols 'g-type-flags
                                               (+ (ash 1 4) (ash 1 5))))
  (assert-eql (ash 1 4)
              (cffi:foreign-bitfield-value 'g-type-flags '(:abstract)))
  (assert-eql (ash 1 5)
              (cffi:foreign-bitfield-value 'g-type-flags '(:value-abstract)))
  (assert-eql (+ (ash 1 4) (ash 1 5))
              (cffi:foreign-bitfield-value 'g-type-flags
                                           '(:abstract :value-abstract))))

(define-test g-param-spec-int
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

;;; --- End of file rtest-gobject.lisp -----------------------------------------
