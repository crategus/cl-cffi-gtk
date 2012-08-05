;;; ----------------------------------------------------------------------------
;;; rtest-gobject-type-info.lisp
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

(define-test g-type-info
  (assert-eql 2 gobject::+g-type-fundamental-shift+)
  
  (assert-eql (ash  0 2) +g-type-invalid+)
  (assert-eql (ash  1 2) +g-type-void+)
  (assert-eql (ash  2 2) +g-type-interface+)
  (assert-eql (ash  3 2) +g-type-char+)
  (assert-eql (ash  4 2) +g-type-uchar+)
  (assert-eql (ash  5 2) +g-type-boolean+)
  (assert-eql (ash  6 2) +g-type-int+)
  (assert-eql (ash  7 2) +g-type-uint+)
  (assert-eql (ash  8 2) +g-type-long+)
  (assert-eql (ash  9 2) +g-type-ulong+)
  (assert-eql (ash 10 2) +g-type-int64+)
  (assert-eql (ash 11 2) +g-type-uint64+)
  (assert-eql (ash 12 2) +g-type-enum+)
  (assert-eql (ash 13 2) +g-type-flags+)
  (assert-eql (ash 14 2) +g-type-float+)
  (assert-eql (ash 15 2) +g-type-double+)
  (assert-eql (ash 16 2) +g-type-string+)
  (assert-eql (ash 17 2) +g-type-pointer+)
  (assert-eql (ash 18 2) +g-type-boxed+)
  (assert-eql (ash 19 2) +g-type-param+)
  (assert-eql (ash 20 2) +g-type-object+)
  (assert-eql (ash 21 2) +g-type-variant+)
  (assert-eql (ash 22 2) +g-type-reserved-glib-first+)
  (assert-eql (ash 31 2) +g-type-reserved-glib-last+)
  (assert-eql (ash 32 2) +g-type-reserved-bse-first+)
  (assert-eql (ash 48 2) +g-type-reserved-bse-last+)
  (assert-eql (ash 49 2) +g-type-reserved-user-first+)
    
  (assert-eql (ash 255 2) +g-type-fundamental-max+)
  (assert-eql (ash 256 2) (g-type-make-fundamental 256))
  
  (assert-equal "GType"      (gtype-name (gtype (g-type-gtype))))
  (assert-eql (g-type-gtype) (gtype-id (gtype (g-type-gtype))))
  
  (assert-false (g-type-is-abstract (gtype "gboolean")))
  (assert-false (g-type-is-abstract (gtype "GObject")))
  (assert-true  (g-type-is-abstract (gtype "GtkWidget")))
  (assert-true  (g-type-is-abstract (gtype "GtkContainer")))
  (assert-false (g-type-is-abstract (gtype "GtkWindow")))
  
  (assert-false (g-type-is-derived (gtype "gboolean")))
  (assert-false (g-type-is-derived (gtype "GObject")))
  (assert-true  (g-type-is-derived (gtype "GtkWidget")))
  (assert-true  (g-type-is-derived (gtype "GtkWindow")))
  
  (assert-true  (g-type-is-fundamental (gtype "gboolean")))
  (assert-true  (g-type-is-fundamental (gtype "GObject")))
  (assert-false (g-type-is-fundamental (gtype "GtkWidget")))
  (assert-false (g-type-is-fundamental (gtype "GtkWindow")))
  
  )

(define-test g-type-info-g-type-flags
  (assert-eql 4 (foreign-type-size 'g-type-flags))
  (assert-equal '(:abstract)
                (foreign-bitfield-symbols 'g-type-flags (ash 1 4)))
  (assert-equal '(:value-abstract)
                (foreign-bitfield-symbols 'g-type-flags (ash 1 5)))
  (assert-equal '(:abstract :value-abstract)
                (foreign-bitfield-symbols 'g-type-flags
                                          (+ (ash 1 4) (ash 1 5))))
  (assert-eql (ash 1 4)
              (foreign-bitfield-value 'g-type-flags '(:abstract)))
  (assert-eql (ash 1 5)
              (foreign-bitfield-value 'g-type-flags '(:value-abstract)))
  (assert-eql (+ (ash 1 4) (ash 1 5))
              (foreign-bitfield-value 'g-type-flags
                                      '(:abstract :value-abstract))))

(define-test g-type-info-g-type-fundamental-flags
  (assert-eql 4 (foreign-type-size 'g-type-fundamental-flags))
  (assert-equal '(:classed)
                (foreign-bitfield-symbols 'g-type-fundamental-flags (ash 1 0)))
  (assert-equal '(:instantiatable)
                (foreign-bitfield-symbols 'g-type-fundamental-flags (ash 1 1)))
  (assert-equal '(:derivable)
                (foreign-bitfield-symbols 'g-type-fundamental-flags (ash 1 2)))
  (assert-equal '(:deep-derivable)
                (foreign-bitfield-symbols 'g-type-fundamental-flags (ash 1 3)))
  (assert-equal '(:classed :instantiatable :derivable :deep-derivable)
                (foreign-bitfield-symbols 'g-type-fundamental-flags
                                          (+ (ash 1 0) (ash 1 1)
                                             (ash 1 2) (ash 1 3))))
  (assert-eql (ash 1 0)
              (foreign-bitfield-value 'g-type-fundamental-flags '(:classed)))
  (assert-eql (ash 1 1)
              (foreign-bitfield-value 'g-type-fundamental-flags
                                      '(:instantiatable)))
  (assert-eql (ash 1 2)
              (foreign-bitfield-value 'g-type-fundamental-flags '(:derivable)))
  (assert-eql (ash 1 3)
              (foreign-bitfield-value 'g-type-fundamental-flags
                                      '(:deep-derivable)))
  (assert-eql (+ (ash 1 0) (ash 1 1) (ash 1 2) (ash 1 3))
              (foreign-bitfield-value 'g-type-fundamental-flags
                                      '(:classed :instantiatable
                                        :derivable :deep-derivable))))

(define-test g-type-info-gtype-from-id
  (assert-eql (ash  0 2) (gtype-id (gtype-from-id +g-type-invalid+)))
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
  (assert-eql (ash 20 2) (gtype-id (gtype-from-id +g-type-object+)))
  (assert-eql (ash 21 2) (gtype-id (gtype-from-id +g-type-variant+))))

(define-test g-type-info-gtype-from-name
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
  (assert-eql (ash 20 2) (gtype-id (gtype-from-name "GObject")))
  (assert-eql (ash 21 2) (gtype-id (gtype-from-name "GVariant"))))

(define-test g-type-info-gtype
  (assert-eql (ash  1 2) (gtype-id (gtype "void")))
  (assert-eql (ash  2 2) (gtype-id (gtype "GInterface")))
  (assert-eql (ash  3 2) (gtype-id (gtype "gchar")))
  (assert-eql (ash  4 2) (gtype-id (gtype "guchar")))
  (assert-eql (ash  5 2) (gtype-id (gtype "gboolean")))
  (assert-eql (ash  6 2) (gtype-id (gtype "gint")))
  (assert-eql (ash  7 2) (gtype-id (gtype "guint")))
  (assert-eql (ash  8 2) (gtype-id (gtype "glong")))
  (assert-eql (ash  9 2) (gtype-id (gtype "gulong")))
  (assert-eql (ash 10 2) (gtype-id (gtype "gint64")))
  (assert-eql (ash 11 2) (gtype-id (gtype "guint64")))
  (assert-eql (ash 12 2) (gtype-id (gtype "GEnum")))
  (assert-eql (ash 13 2) (gtype-id (gtype "GFlags")))
  (assert-eql (ash 14 2) (gtype-id (gtype "gfloat")))
  (assert-eql (ash 15 2) (gtype-id (gtype "gdouble")))
  (assert-eql (ash 16 2) (gtype-id (gtype "gchararray")))
  (assert-eql (ash 17 2) (gtype-id (gtype "gpointer")))
  (assert-eql (ash 18 2) (gtype-id (gtype "GBoxed")))
  (assert-eql (ash 19 2) (gtype-id (gtype "GParam")))
  (assert-eql (ash 20 2) (gtype-id (gtype "GObject")))
  (assert-eql (ash 21 2) (gtype-id (gtype "GVariant"))))

(define-test g-type-info-gtype-id
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
  (assert-eql (ash 20 2) (gtype-id (gtype +g-type-object+)))
  (assert-eql (ash 21 2) (gtype-id (gtype +g-type-variant+))))

(define-test g-type-info-gtype-name
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
  (assert-equal "GObject"    (gtype-name (gtype +g-type-object+)))
  (assert-equal "GVariant"   (gtype-name (gtype +g-type-variant+))))



