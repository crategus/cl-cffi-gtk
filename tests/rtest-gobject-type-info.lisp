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
  (assert-eql (ash  1 2) +g-type-none+)
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
  (assert-eql 22 +g-type-reserved-glib-first+)
  (assert-eql 31 +g-type-reserved-glib-last+)
  (assert-eql 32 +g-type-reserved-bse-first+)
  (assert-eql 48 +g-type-reserved-bse-last+)
  (assert-eql 49 +g-type-reserved-user-first+)

  ;; +g-type-fundamental-max+
  (assert-eql (ash 255 2) +g-type-fundamental-max+)

  ;; g-type-make-fundamental
  (assert-eql (ash 256 2) (g-type-make-fundamental 256))
  
  ;; g-type-gtype
  (assert-equal "GType" (gtype-name (gtype (g-type-gtype))))
  (assert-eql 134818672 (gtype-id (gtype (g-type-gtype))))

  ;; g-type-is-abstract
  (assert-false (g-type-is-abstract +g-type-invalid+))
  (assert-false (g-type-is-abstract +g-type-none+))
  (assert-false (g-type-is-abstract +g-type-interface+))
  (assert-false (g-type-is-abstract +g-type-char+))
  (assert-false (g-type-is-abstract +g-type-uchar+))
  (assert-false (g-type-is-abstract +g-type-boolean+))
  (assert-false (g-type-is-abstract +g-type-int+))
  (assert-false (g-type-is-abstract +g-type-uint+))
  (assert-false (g-type-is-abstract +g-type-long+))
  (assert-false (g-type-is-abstract +g-type-ulong+))
  (assert-false (g-type-is-abstract +g-type-int64+))
  (assert-false (g-type-is-abstract +g-type-uint64+))
  (assert-true  (g-type-is-abstract +g-type-enum+))
  (assert-true  (g-type-is-abstract +g-type-flags+))
  (assert-false (g-type-is-abstract +g-type-float+))
  (assert-false (g-type-is-abstract +g-type-double+))
  (assert-false (g-type-is-abstract +g-type-string+))
  (assert-false (g-type-is-abstract +g-type-pointer+))
  (assert-true  (g-type-is-abstract +g-type-boxed+))
  (assert-true  (g-type-is-abstract +g-type-param+))
  (assert-false (g-type-is-abstract +g-type-object+))
  (assert-false (g-type-is-abstract +g-type-variant+))

  (assert-true  (g-type-is-abstract "GtkWidget"))
  (assert-true  (g-type-is-abstract "GtkContainer"))
  (assert-false (g-type-is-abstract "GtkBox"))
  (assert-false (g-type-is-abstract "GtkWindow"))
  (assert-false (g-type-is-abstract "GtkOrientable"))
  (assert-false (g-type-is-abstract "GtkAccelFlags"))
  (assert-false (g-type-is-abstract "GtkArrowPlacement"))
  (assert-false (g-type-is-abstract "GdkRGBA"))
  (assert-false (g-type-is-abstract "GtkTreePath"))

  ;; g-type-is-derived
  (assert-false (g-type-is-derived +g-type-invalid+))
  (assert-false (g-type-is-derived +g-type-none+))
  (assert-false (g-type-is-derived +g-type-interface+))
  (assert-false (g-type-is-derived +g-type-char+))
  (assert-false (g-type-is-derived +g-type-uchar+))
  (assert-false (g-type-is-derived +g-type-boolean+))
  (assert-false (g-type-is-derived +g-type-int+))
  (assert-false (g-type-is-derived +g-type-uint+))
  (assert-false (g-type-is-derived +g-type-long+))
  (assert-false (g-type-is-derived +g-type-ulong+))
  (assert-false (g-type-is-derived +g-type-int64+))
  (assert-false (g-type-is-derived +g-type-uint64+))
  (assert-false (g-type-is-derived +g-type-enum+))
  (assert-false (g-type-is-derived +g-type-flags+))
  (assert-false (g-type-is-derived +g-type-float+))
  (assert-false (g-type-is-derived +g-type-double+))
  (assert-false (g-type-is-derived +g-type-string+))
  (assert-false (g-type-is-derived +g-type-pointer+))
  (assert-false (g-type-is-derived +g-type-boxed+))
  (assert-false (g-type-is-derived +g-type-param+))
  (assert-false (g-type-is-derived +g-type-object+))
  (assert-false (g-type-is-derived +g-type-variant+))

  (assert-true  (g-type-is-derived "GtkWidget"))
  (assert-true  (g-type-is-derived "GtkContainer"))
  (assert-true  (g-type-is-derived "GtkBox"))
  (assert-true  (g-type-is-derived "GtkWindow"))
  (assert-true  (g-type-is-derived "GtkOrientable"))
  (assert-true  (g-type-is-derived "GtkAccelFlags"))
  (assert-true  (g-type-is-derived "GtkArrowPlacement"))
  (assert-true  (g-type-is-derived "GdkRGBA"))
  (assert-true  (g-type-is-derived "GtkTreePath"))

  ;; g-type-is-fundamental
  (assert-true  (g-type-is-fundamental +g-type-invalid+))
  (assert-true  (g-type-is-fundamental +g-type-none+))
  (assert-true  (g-type-is-fundamental +g-type-interface+))
  (assert-true  (g-type-is-fundamental +g-type-char+))
  (assert-true  (g-type-is-fundamental +g-type-uchar+))
  (assert-true  (g-type-is-fundamental +g-type-boolean+))
  (assert-true  (g-type-is-fundamental +g-type-int+))
  (assert-true  (g-type-is-fundamental +g-type-uint+))
  (assert-true  (g-type-is-fundamental +g-type-long+))
  (assert-true  (g-type-is-fundamental +g-type-ulong+))
  (assert-true  (g-type-is-fundamental +g-type-int64+))
  (assert-true  (g-type-is-fundamental +g-type-uint64+))
  (assert-true  (g-type-is-fundamental +g-type-enum+))
  (assert-true  (g-type-is-fundamental +g-type-flags+))
  (assert-true  (g-type-is-fundamental +g-type-float+))
  (assert-true  (g-type-is-fundamental +g-type-double+))
  (assert-true  (g-type-is-fundamental +g-type-string+))
  (assert-true  (g-type-is-fundamental +g-type-pointer+))
  (assert-true  (g-type-is-fundamental +g-type-boxed+))
  (assert-true  (g-type-is-fundamental +g-type-param+))
  (assert-true  (g-type-is-fundamental +g-type-object+))
  (assert-true  (g-type-is-fundamental +g-type-variant+))

  (assert-false (g-type-is-fundamental "GtkWidget"))
  (assert-false (g-type-is-fundamental "GtkContainer"))
  (assert-false (g-type-is-fundamental "GtkBox"))
  (assert-false (g-type-is-fundamental "GtkWindow"))
  (assert-false (g-type-is-fundamental "GtkOrientable"))
  (assert-false (g-type-is-fundamental "GtkAccelFlags"))
  (assert-false (g-type-is-fundamental "GtkArrowPlacement"))
  (assert-false (g-type-is-fundamental "GdkRGBA"))
  (assert-false (g-type-is-fundamental "GtkTreePath"))

  ;; g-type-is-value-type
  (assert-false (g-type-is-value-type +g-type-invalid+))
  (assert-false (g-type-is-value-type +g-type-none+))
  (assert-false (g-type-is-value-type +g-type-interface+))
  (assert-true  (g-type-is-value-type +g-type-char+))
  (assert-true  (g-type-is-value-type +g-type-uchar+))
  (assert-true  (g-type-is-value-type +g-type-boolean+))
  (assert-true  (g-type-is-value-type +g-type-int+))
  (assert-true  (g-type-is-value-type +g-type-uint+))
  (assert-true  (g-type-is-value-type +g-type-long+))
  (assert-true  (g-type-is-value-type +g-type-ulong+))
  (assert-true  (g-type-is-value-type +g-type-int64+))
  (assert-true  (g-type-is-value-type +g-type-uint64+))
  (assert-false (g-type-is-value-type +g-type-enum+))
  (assert-false (g-type-is-value-type +g-type-flags+))
  (assert-true  (g-type-is-value-type +g-type-float+))
  (assert-true  (g-type-is-value-type +g-type-double+))
  (assert-true  (g-type-is-value-type +g-type-string+))
  (assert-true  (g-type-is-value-type +g-type-pointer+))
  (assert-false (g-type-is-value-type +g-type-boxed+))
  (assert-true  (g-type-is-value-type +g-type-param+))
  (assert-true  (g-type-is-value-type +g-type-object+))
  (assert-true  (g-type-is-value-type +g-type-variant+))

  (assert-true  (g-type-is-value-type "GtkWidget"))
  (assert-true  (g-type-is-value-type "GtkContainer"))
  (assert-true  (g-type-is-value-type "GtkBox"))
  (assert-true  (g-type-is-value-type "GtkWindow"))
  (assert-true  (g-type-is-value-type "GtkOrientable"))
  (assert-true  (g-type-is-value-type "GtkAccelFlags"))
  (assert-true  (g-type-is-value-type "GtkArrowPlacement"))
  (assert-true  (g-type-is-value-type "GdkRGBA"))
  (assert-true  (g-type-is-value-type "GtkTreePath"))

  ;; g-type-has-value-table
;  (assert-false (g-type-has-value-table +g-type-invalid+))
;  (assert-false (g-type-has-value-table +g-type-none+))
  (assert-false (g-type-has-value-table +g-type-interface+))
  (assert-true  (g-type-has-value-table +g-type-char+))
  (assert-true  (g-type-has-value-table +g-type-uchar+))
  (assert-true  (g-type-has-value-table +g-type-boolean+))
  (assert-true  (g-type-has-value-table +g-type-int+))
  (assert-true  (g-type-has-value-table +g-type-uint+))
  (assert-true  (g-type-has-value-table +g-type-long+))
  (assert-true  (g-type-has-value-table +g-type-ulong+))
  (assert-true  (g-type-has-value-table +g-type-int64+))
  (assert-true  (g-type-has-value-table +g-type-uint64+))
  (assert-true  (g-type-has-value-table +g-type-enum+))
  (assert-true  (g-type-has-value-table +g-type-flags+))
  (assert-true  (g-type-has-value-table +g-type-float+))
  (assert-true  (g-type-has-value-table +g-type-double+))
  (assert-true  (g-type-has-value-table +g-type-string+))
  (assert-true  (g-type-has-value-table +g-type-pointer+))
  (assert-false (g-type-has-value-table +g-type-boxed+))
  (assert-true  (g-type-has-value-table +g-type-param+))
  (assert-true  (g-type-has-value-table +g-type-object+))
  (assert-true  (g-type-has-value-table +g-type-variant+))

  (assert-true  (g-type-has-value-table "GtkWidget"))
  (assert-true  (g-type-has-value-table "GtkContainer"))
  (assert-true  (g-type-has-value-table "GtkBox"))
  (assert-true  (g-type-has-value-table "GtkWindow"))
  (assert-true  (g-type-has-value-table "GtkOrientable"))
  (assert-true  (g-type-has-value-table "GtkAccelFlags"))
  (assert-true  (g-type-has-value-table "GtkArrowPlacement"))
  (assert-true  (g-type-has-value-table "GdkRGBA"))
  (assert-true  (g-type-has-value-table "GtkTreePath"))

  ;; g-type-is-classed
  (assert-false (g-type-is-classed +g-type-invalid+))
  (assert-false (g-type-is-classed +g-type-none+))
  (assert-false (g-type-is-classed +g-type-interface+))
  (assert-false (g-type-is-classed +g-type-char+))
  (assert-false (g-type-is-classed +g-type-uchar+))
  (assert-false (g-type-is-classed +g-type-boolean+))
  (assert-false (g-type-is-classed +g-type-int+))
  (assert-false (g-type-is-classed +g-type-uint+))
  (assert-false (g-type-is-classed +g-type-long+))
  (assert-false (g-type-is-classed +g-type-ulong+))
  (assert-false (g-type-is-classed +g-type-int64+))
  (assert-false (g-type-is-classed +g-type-uint64+))
  (assert-true  (g-type-is-classed +g-type-enum+))
  (assert-true  (g-type-is-classed +g-type-flags+))
  (assert-false (g-type-is-classed +g-type-float+))
  (assert-false (g-type-is-classed +g-type-double+))
  (assert-false (g-type-is-classed +g-type-string+))
  (assert-false (g-type-is-classed +g-type-pointer+))
  (assert-false (g-type-is-classed +g-type-boxed+))
  (assert-true  (g-type-is-classed +g-type-param+))
  (assert-true  (g-type-is-classed +g-type-object+))
  (assert-false (g-type-is-classed +g-type-variant+))

  (assert-true  (g-type-is-classed "GtkWidget"))
  (assert-true  (g-type-is-classed "GtkContainer"))
  (assert-true  (g-type-is-classed "GtkBox"))
  (assert-true  (g-type-is-classed "GtkWindow"))
  (assert-false (g-type-is-classed "GtkOrientable"))
  (assert-true  (g-type-is-classed "GtkAccelFlags"))
  (assert-true  (g-type-is-classed "GtkArrowPlacement"))
  (assert-false (g-type-is-classed "GdkRGBA"))
  (assert-false (g-type-is-classed "GtkTreePath"))

  ;; g-type-is-instantiatable
  (assert-false (g-type-is-instantiatable +g-type-invalid+))
  (assert-false (g-type-is-instantiatable +g-type-none+))
  (assert-false (g-type-is-instantiatable +g-type-interface+))
  (assert-false (g-type-is-instantiatable +g-type-char+))
  (assert-false (g-type-is-instantiatable +g-type-uchar+))
  (assert-false (g-type-is-instantiatable +g-type-boolean+))
  (assert-false (g-type-is-instantiatable +g-type-int+))
  (assert-false (g-type-is-instantiatable +g-type-uint+))
  (assert-false (g-type-is-instantiatable +g-type-long+))
  (assert-false (g-type-is-instantiatable +g-type-ulong+))
  (assert-false (g-type-is-instantiatable +g-type-int64+))
  (assert-false (g-type-is-instantiatable +g-type-uint64+))
  (assert-false (g-type-is-instantiatable +g-type-enum+))
  (assert-false (g-type-is-instantiatable +g-type-flags+))
  (assert-false (g-type-is-instantiatable +g-type-float+))
  (assert-false (g-type-is-instantiatable +g-type-double+))
  (assert-false (g-type-is-instantiatable +g-type-string+))
  (assert-false (g-type-is-instantiatable +g-type-pointer+))
  (assert-false (g-type-is-instantiatable +g-type-boxed+))
  (assert-true  (g-type-is-instantiatable +g-type-param+))
  (assert-true  (g-type-is-instantiatable +g-type-object+))
  (assert-false (g-type-is-instantiatable +g-type-variant+))

  (assert-true  (g-type-is-instantiatable "GtkWidget"))
  (assert-true  (g-type-is-instantiatable "GtkContainer"))
  (assert-true  (g-type-is-instantiatable "GtkBox"))
  (assert-true  (g-type-is-instantiatable "GtkWindow"))
  (assert-false (g-type-is-instantiatable "GtkOrientable"))
  (assert-false (g-type-is-instantiatable "GtkAccelFlags"))
  (assert-false (g-type-is-instantiatable "GtkArrowPlacement"))
  (assert-false (g-type-is-instantiatable "GdkRGBA"))
  (assert-false (g-type-is-instantiatable "GtkTreePath"))

  ;; g-type-is-derivable
  (assert-false (g-type-is-derivable +g-type-invalid+))
  (assert-false (g-type-is-derivable +g-type-none+))
  (assert-true  (g-type-is-derivable +g-type-interface+))
  (assert-true  (g-type-is-derivable +g-type-char+))
  (assert-true  (g-type-is-derivable +g-type-uchar+))
  (assert-true  (g-type-is-derivable +g-type-boolean+))
  (assert-true  (g-type-is-derivable +g-type-int+))
  (assert-true  (g-type-is-derivable +g-type-uint+))
  (assert-true  (g-type-is-derivable +g-type-long+))
  (assert-true  (g-type-is-derivable +g-type-ulong+))
  (assert-true  (g-type-is-derivable +g-type-int64+))
  (assert-true  (g-type-is-derivable +g-type-uint64+))
  (assert-true  (g-type-is-derivable +g-type-enum+))
  (assert-true  (g-type-is-derivable +g-type-flags+))
  (assert-true  (g-type-is-derivable +g-type-float+))
  (assert-true  (g-type-is-derivable +g-type-double+))
  (assert-true  (g-type-is-derivable +g-type-string+))
  (assert-true  (g-type-is-derivable +g-type-pointer+))
  (assert-true  (g-type-is-derivable +g-type-boxed+))
  (assert-true  (g-type-is-derivable +g-type-param+))
  (assert-true  (g-type-is-derivable +g-type-object+))
  (assert-true  (g-type-is-derivable +g-type-variant+))

  (assert-true  (g-type-is-derivable "GtkWidget"))
  (assert-true  (g-type-is-derivable "GtkContainer"))
  (assert-true  (g-type-is-derivable "GtkBox"))
  (assert-true  (g-type-is-derivable "GtkWindow"))
  (assert-true  (g-type-is-derivable "GtkOrientable"))
  (assert-true  (g-type-is-derivable "GtkAccelFlags"))
  (assert-true  (g-type-is-derivable "GtkArrowPlacement"))
  (assert-true  (g-type-is-derivable "GdkRGBA"))
  (assert-true  (g-type-is-derivable "GtkTreePath"))

  ;; g-type-is-deep-derivable
  (assert-false (g-type-is-deep-derivable +g-type-invalid+))
  (assert-false (g-type-is-deep-derivable +g-type-none+))
  (assert-false (g-type-is-deep-derivable +g-type-interface+))
  (assert-false (g-type-is-deep-derivable +g-type-char+))
  (assert-false (g-type-is-deep-derivable +g-type-uchar+))
  (assert-false (g-type-is-deep-derivable +g-type-boolean+))
  (assert-false (g-type-is-deep-derivable +g-type-int+))
  (assert-false (g-type-is-deep-derivable +g-type-uint+))
  (assert-false (g-type-is-deep-derivable +g-type-long+))
  (assert-false (g-type-is-deep-derivable +g-type-ulong+))
  (assert-false (g-type-is-deep-derivable +g-type-int64+))
  (assert-false (g-type-is-deep-derivable +g-type-uint64+))
  (assert-false (g-type-is-deep-derivable +g-type-enum+))
  (assert-false (g-type-is-deep-derivable +g-type-flags+))
  (assert-false (g-type-is-deep-derivable +g-type-float+))
  (assert-false (g-type-is-deep-derivable +g-type-double+))
  (assert-false (g-type-is-deep-derivable +g-type-string+))
  (assert-false (g-type-is-deep-derivable +g-type-pointer+))
  (assert-false (g-type-is-deep-derivable +g-type-boxed+))
  (assert-true  (g-type-is-deep-derivable +g-type-param+))
  (assert-true  (g-type-is-deep-derivable +g-type-object+))
  (assert-false (g-type-is-deep-derivable +g-type-variant+))

  (assert-true  (g-type-is-deep-derivable "GtkWidget"))
  (assert-true  (g-type-is-deep-derivable "GtkContainer"))
  (assert-true  (g-type-is-deep-derivable "GtkBox"))
  (assert-true  (g-type-is-deep-derivable "GtkWindow"))
  (assert-false (g-type-is-deep-derivable "GtkOrientable"))
  (assert-false (g-type-is-deep-derivable "GtkAccelFlags"))
  (assert-false (g-type-is-deep-derivable "GtkArrowPlacement"))
  (assert-false (g-type-is-deep-derivable "GdkRGBA"))
  (assert-false (g-type-is-deep-derivable "GtkTreePath"))

  ;; g-type-is-interface
  (assert-false (g-type-is-interface +g-type-invalid+))
  (assert-false (g-type-is-interface +g-type-none+))
  (assert-true  (g-type-is-interface +g-type-interface+))
  (assert-false (g-type-is-interface +g-type-char+))
  (assert-false (g-type-is-interface +g-type-uchar+))
  (assert-false (g-type-is-interface +g-type-boolean+))
  (assert-false (g-type-is-interface +g-type-int+))
  (assert-false (g-type-is-interface +g-type-uint+))
  (assert-false (g-type-is-interface +g-type-long+))
  (assert-false (g-type-is-interface +g-type-ulong+))
  (assert-false (g-type-is-interface +g-type-int64+))
  (assert-false (g-type-is-interface +g-type-uint64+))
  (assert-false (g-type-is-interface +g-type-enum+))
  (assert-false (g-type-is-interface +g-type-flags+))
  (assert-false (g-type-is-interface +g-type-float+))
  (assert-false (g-type-is-interface +g-type-double+))
  (assert-false (g-type-is-interface +g-type-string+))
  (assert-false (g-type-is-interface +g-type-pointer+))
  (assert-false (g-type-is-interface +g-type-boxed+))
  (assert-false (g-type-is-interface +g-type-param+))
  (assert-false (g-type-is-interface +g-type-object+))
  (assert-false (g-type-is-interface +g-type-variant+))

  (assert-false (g-type-is-interface "GtkWidget"))
  (assert-false (g-type-is-interface "GtkContainer"))
  (assert-false (g-type-is-interface "GtkBox"))
  (assert-false (g-type-is-interface "GtkWindow"))
  (assert-true  (g-type-is-interface "GtkOrientable"))
  (assert-false (g-type-is-interface "GtkAccelFlags"))
  (assert-false (g-type-is-interface "GtkArrowPlacement"))
  (assert-false (g-type-is-interface "GdkRGBA"))
  (assert-false (g-type-is-interface "GtkTreePath"))

;;;     GTypeInterface
;;;     GTypeClass
;;;     GTypeInstance
;;;     GTypeInfo
;;;     GTypeFundamentalInfo
;;;     GInterfaceInfo
;;;     GTypeValueTable

;;;     G_TYPE_FROM_INSTANCE
;;;     G_TYPE_FROM_CLASS
;;;     G_TYPE_FROM_INTERFACE
;;;     G_TYPE_INSTANCE_GET_CLASS                * not implemented *
;;;     G_TYPE_INSTANCE_GET_INTERFACE            * not implemented *
;;;     G_TYPE_INSTANCE_GET_PRIVATE              * not implemented *
;;;     G_TYPE_CLASS_GET_PRIVATE                 * not implemented *
;;;     G_TYPE_CHECK_INSTANCE                    * not implemented *
;;;     G_TYPE_CHECK_INSTANCE_CAST               * not implemented *
;;;     G_TYPE_CHECK_INSTANCE_TYPE
;;;     G_TYPE_CHECK_CLASS_CAST                  * not implemented *
;;;     G_TYPE_CHECK_CLASS_TYPE
;;;     G_TYPE_CHECK_VALUE                       * not implemented *
;;;     G_TYPE_CHECK_VALUE_TYPE                  * not implemented *
;;;     G_TYPE_FLAG_RESERVED_ID_BIT              * not implemented *

;;;     g_type_init
;;;
;;;     GTypeDebugFlags                          * not implemented *
;;;
;;;     g_type_init_with_debug_flags             * not implemented *
;;;     g_type_name
;;;     g_type_qname
;;;     g_type_from_name
;;;     g_type_parent
;;;     g_type_depth
;;;     g_type_next_base
;;;     g_type_is_a
;;;     g_type_class_ref
;;;     g_type_class_peek
;;;     g_type_class_peek_static
;;;     g_type_class_unref
;;;     g_type_class_peek_parent
;;;     g_type_class_add_private
;;;     g_type_add_class_private                 * not implemented *
;;;     g_type_interface_peek
;;;     g_type_interface_peek_parent             * not implemented *
;;;     g_type_default_interface_ref
;;;     g_type_default_interface_peek
;;;     g_type_default_interface_unref
;;;     g_type_children
;;;     g_type_interfaces
;;;     g_type_interface_prerequisites
;;;     g_type_set_qdata
;;;     g_type_get_qdata
;;;
;;;     GTypeQuery
;;;
;;;     g_type_query
;;;     g_type_register_static
;;;     g_type_register_static_simple
;;;     g_type_register_dynamic                  * not implemented *
;;;     g_type_register_fundamental              * not implemented *
;;;     g_type_add_interface_static
;;;     g_type_add_interface_dynamic             * not implemented *
;;;     g_type_interface_add_prerequisite
;;;     g_type_get_plugin                        * not implemented *
;;;     g_type_interface_get_plugin              * not implemented *
;;;     g_type_fundamental_next
;;;     g_type_fundamental
;;;     g_type_create_instance                   * not implemented *
;;;     g_type_free_instance                     * not implemented *
;;;     g_type_add_class_cache_func              * not implemented *
;;;     g_type_remove_class_cache_func           * not implemented *
;;;     g_type_class_unref_uncached              * not implemented *
;;;     g_type_add_interface_check               * not implemented *
;;;     g_type_remove_interface_check            * not implemented *
;;;     g_type_value_table_peek
;;;     g_type_ensure
;;;     g_type_get_type_registration_serial
;;;
;;;     G_DEFINE_TYPE                            * not implemented *
;;;     G_DEFINE_TYPE_WITH_CODE                  * not implemented *
;;;     G_DEFINE_ABSTRACT_TYPE                   * not implemented *
;;;     G_DEFINE_ABSTRACT_TYPE_WITH_CODE         * not implemented *
;;;     G_DEFINE_INTERFACE                       * not implemented *
;;;     G_DEFINE_INTERFACE_WITH_CODE             * not implemented *
;;;     G_IMPLEMENT_INTERFACE                    * not implemented *
;;;     G_DEFINE_TYPE_EXTENDED                   * not implemented *
;;;     G_DEFINE_BOXED_TYPE                      * not implemented *
;;;     G_DEFINE_BOXED_TYPE_WITH_CODE            * not implemented *
;;;     G_DEFINE_POINTER_TYPE                    * not implemented *
;;;     G_DEFINE_POINTER_TYPE_WITH_CODE          * not implemented *
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
  (assert-eql (ash  1 2) (gtype-id (gtype-from-id +g-type-none+)))
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
  (assert-eql (ash  1 2) (gtype-id (gtype +g-type-none+)))
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
  (assert-equal "void"       (gtype-name (gtype +g-type-none+)))
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

(define-test g-type-info-char
  (let ((id +g-type-char+)
        (name "gchar")
        (gtype (gtype +g-type-char+)))
  ;; gtype-id
  (assert-eql +g-type-char+ (gtype-id gtype))
  ;; gtype-name
  (assert-equal name (gtype-name gtype))
  ;; gtype-from-id
  (assert-eql gtype (gtype-from-id id))
  ;; gtype-from-name
  (assert-eql gtype (gtype-from-name name))
  ;; gobject::g-type=
  (assert-false (gobject::g-type= gtype +g-type-invalid+))
  ;; gobject::g-type/=
  (assert-true (gobject::g-type/= gtype +g-type-invalid+))
  ;; g-type-is-abstract
  (assert-false (g-type-is-abstract gtype))
  (assert-false (g-type-is-abstract id))
  (assert-false (g-type-is-abstract name))
  ;; g-type-is-derived
  (assert-false (g-type-is-derived gtype))
  (assert-false (g-type-is-derived id))
  (assert-false (g-type-is-derived name))
  ;; g-type-is-fundamental
  (assert-true (g-type-is-fundamental gtype))
  (assert-true (g-type-is-fundamental id))
  (assert-true (g-type-is-fundamental name))
  ;; g-type-is-value-type
  (assert-true (g-type-is-value-type gtype))
  (assert-true (g-type-is-value-type id))
  (assert-true (g-type-is-value-type name))
  ;; g-type-has-value-table
  (assert-true (g-type-has-value-table gtype))
  (assert-true (g-type-has-value-table id))
  (assert-true (g-type-has-value-table name))
  ;; g-type-is-classed
  (assert-false (g-type-is-classed gtype))
  (assert-false (g-type-is-classed id))
  (assert-false (g-type-is-classed name))
  ;; g-type-is-instantiatable
  (assert-false (g-type-is-instantiatable gtype))
  (assert-false (g-type-is-instantiatable id))
  (assert-false (g-type-is-instantiatable name))
  ;; g-type-is-derivable
  (assert-true (g-type-is-derivable gtype))
  (assert-true (g-type-is-derivable id))
  (assert-true (g-type-is-derivable name))
  ;; g-type-is-deep-derivable
  (assert-false (g-type-is-deep-derivable gtype))
  (assert-false (g-type-is-deep-derivable id))
  (assert-false (g-type-is-deep-derivable name))
  ;; g-type-is-interface
  (assert-false (g-type-is-interface gtype))
  (assert-false (g-type-is-interface id))
  (assert-false (g-type-is-interface name))
  ;; g-type-name
  (assert-equal name (g-type-name gtype))
  (assert-equal name (g-type-name id))
  (assert-equal name (g-type-name name))
  ;; g-type-qname
  (assert-equal name (g-type-qname gtype))
  (assert-equal name (g-type-qname id))
  (assert-equal name (g-type-qname name))
  ;; g-type-from-name
  (assert-eql gtype (g-type-from-name name))
  ;; g-type-parent
  (assert-false (g-type-parent gtype))
  (assert-false (g-type-parent id))
  (assert-false (g-type-parent name))
  ;; g-type-depth
  (assert-eql 1 (g-type-depth gtype))
  (assert-eql 1 (g-type-depth id))
  (assert-eql 1 (g-type-depth name))
  ;; g-type-next-base
  (assert-false (g-type-next-base gtype gtype))
  (assert-false (g-type-next-base gtype id))
  (assert-false (g-type-next-base gtype name))
  ;; g-type-is-a
  (assert-false (g-type-is-a gtype +g-type-invalid+))
  (assert-true (g-type-is-a gtype gtype))
  (assert-true (g-type-is-a gtype id))
  (assert-true (g-type-is-a gtype name))
  ;; g-type-children
  (assert-false (g-type-children gtype))
  (assert-false (g-type-children id))
  (assert-false (g-type-children name))
  ;; g-type-interfaces
  (assert-false (g-type-interfaces gtype))
  (assert-false (g-type-interfaces id))
  (assert-false (g-type-interfaces name))
  ;; g-type-interace-prerequisites
  ;; g-type-set-qdata
  (g-type-set-qdata gtype "myData" (null-pointer))
  ;; g-type-get-qdata
  (assert-true  (null-pointer-p (g-type-get-qdata gtype "myData")))
  ;; g-type-query
  ;; FIXME: This does not work for the type "gchar" or other basic types like
  ;; "gboolean". But it is not a problem for "GObject" and derived classes.
  #+nil
  (with-foreign-object (query '(:struct g-type-query))
    (g-type-query name query)
    (assert-false (foreign-slot-value query '(:struct g-type-query) :type))
    (assert-false (foreign-slot-value query '(:struct g-type-query) :type-name))
    (assert-eql 0 (foreign-slot-value query '(:struct g-type-query) :class-size))
    (assert-eql 0 (foreign-slot-value query '(:struct g-type-query) :instance-size)))
  ;; g-type-fundamental
  (assert-eq gtype (g-type-fundamental gtype))
  (assert-eq gtype (g-type-fundamental id))
  (assert-eq gtype (g-type-fundamental name))
  ;; g-type-value-table-peek
  (with-foreign-object (table 'g-type-value-table)
    (setq table (g-type-value-table-peek gtype))
    (assert-false (null-pointer-p (foreign-slot-value table 'g-type-value-table :value-init)))
    (assert-true (null-pointer-p (foreign-slot-value table 'g-type-value-table :value-free)))
    (assert-false (null-pointer-p (foreign-slot-value table 'g-type-value-table :value-copy)))
    (assert-true (null-pointer-p (foreign-slot-value table 'g-type-value-table :value-peek-pointer)))
    (assert-equal "i" (foreign-slot-value table 'g-type-value-table :collect-format))
    (assert-false (null-pointer-p (foreign-slot-value table 'g-type-value-table :collect-value)))
    (assert-equal "p" (foreign-slot-value table 'g-type-value-table :lcopy-format))
    (assert-false (null-pointer-p (foreign-slot-value table 'g-type-value-table :lcopy-value))))))

(define-test g-type-info-gtk-label
  (let ((id (gtype-id (gtype "GtkLabel")))
        (name "GtkLabel")
        (gtype (gtype "GtkLabel")))
    ;; gtype-id
    (assert-eql 134917616 (gtype-id gtype))
    ;; gtype-name
    (assert-equal name (gtype-name gtype))
    ;; gtype-from-id
    (assert-eql gtype (gtype-from-id id))
    ;; gtype-from-name
    (assert-eql gtype (gtype-from-name name))
    ;; gobject::g-type=
    (assert-false (gobject::g-type= gtype +g-type-invalid+))
    (assert-true  (gobject::g-type= gtype name))
    (assert-true  (gobject::g-type= gtype id))
    ;; gobject::g-type/=
    (assert-true  (gobject::g-type/= gtype +g-type-invalid+))
    (assert-false (gobject::g-type/= gtype name))
    (assert-false (gobject::g-type/= gtype id))
    ;; g-type-is-abstract
    (assert-false (g-type-is-abstract gtype))
    (assert-false (g-type-is-abstract id))
    (assert-false (g-type-is-abstract name))
    ;; g-type-is-derived
    (assert-true (g-type-is-derived gtype))
    (assert-true (g-type-is-derived id))
    (assert-true (g-type-is-derived name))
    ;; g-type-is-fundamental
    (assert-false (g-type-is-fundamental gtype))
    (assert-false (g-type-is-fundamental id))
    (assert-false (g-type-is-fundamental name))
    ;; g-type-is-value-type
    (assert-true (g-type-is-value-type gtype))
    (assert-true (g-type-is-value-type id))
    (assert-true (g-type-is-value-type name))
    ;; g-type-has-value-table
    (assert-true (g-type-has-value-table gtype))
    (assert-true (g-type-has-value-table id))
    (assert-true (g-type-has-value-table name))
    ;; g-type-is-classed
    (assert-true (g-type-is-classed gtype))
    (assert-true (g-type-is-classed id))
    (assert-true (g-type-is-classed name))
    ;; g-type-is-instantiatable
    (assert-true (g-type-is-instantiatable gtype))
    (assert-true (g-type-is-instantiatable id))
    (assert-true (g-type-is-instantiatable name))
    ;; g-type-is-derivable
    (assert-true (g-type-is-derivable gtype))
    (assert-true (g-type-is-derivable id))
    (assert-true (g-type-is-derivable name))
    ;; g-type-is-deep-derivable
    (assert-true (g-type-is-deep-derivable gtype))
    (assert-true (g-type-is-deep-derivable id))
    (assert-true (g-type-is-deep-derivable name))
    ;; g-type-is-interface
    (assert-false (g-type-is-interface gtype))
    (assert-false (g-type-is-interface id))
    (assert-false (g-type-is-interface name))
    ;; g-type-from-instance
    (let ((label (make-instance 'gtk-label)))
      (assert-eq gtype (g-type-from-instance label)))
    ;; g-type-from-class
    (assert-eq gtype (g-type-from-class (g-type-class-ref name)))
    ;; g-type-from-interface
    ;; g-type-name
    (assert-equal name (g-type-name gtype))
    (assert-equal name (g-type-name id))
    (assert-equal name (g-type-name name))
    ;; g-type-qname
    (assert-equal name (g-type-qname gtype))
    (assert-equal name (g-type-qname id))
    (assert-equal name (g-type-qname name))
    ;; g-type-from-name
    (assert-eql gtype (g-type-from-name name))
    ;; g-type-parent
    (assert-true (g-type-is-a "GtkMisc" (g-type-parent gtype)))
    (assert-true (g-type-is-a "GtkMisc" (g-type-parent id)))
    (assert-true (g-type-is-a "GtkMisc" (g-type-parent name)))
    ;; g-type-depth
    (assert-eql 5 (g-type-depth gtype))
    (assert-eql 5 (g-type-depth id))
    (assert-eql 5 (g-type-depth name))
    ;; g-type-next-base
    (assert-eq (gtype "GInitiallyUnowned") (g-type-next-base gtype "GObject"))
    (assert-eq (gtype "GInitiallyUnowned") (g-type-next-base id "GObject"))
    (assert-eq (gtype "GInitiallyUnowned") (g-type-next-base name "GObject"))
    ;; g-type-is-a
    (assert-false (g-type-is-a gtype +g-type-invalid+))
    (assert-true (g-type-is-a gtype gtype))
    (assert-true (g-type-is-a gtype id))
    (assert-true (g-type-is-a gtype name))
    ;; g-type-class-ref
    (assert-eq gtype (foreign-slot-value (g-type-class-ref gtype) 'g-type-class :type))
    ;; g-type-class-peek
    (assert-eq gtype (foreign-slot-value (g-type-class-peek gtype) 'g-type-class :type))
    ;; g-type-class-peek-static
    (assert-eq gtype (foreign-slot-value (g-type-class-peek-static gtype) 'g-type-class :type))
    ;; g-type-class-unref
    ;; g-type-class-peek-parent
    (assert-eq (gtype "GtkMisc")
               (foreign-slot-value (g-type-class-peek-parent (g-type-class-peek gtype)) 'g-type-class :type))
    ;; g-type-interface-peek
    ;; g-type-interface-peek-parent
    ;; g-type-default-interface-ref
    ;; g-type-default-interface-peek
    ;; g-type-default-interface-unref
    ;; g-type-children
    (assert-equal '("GtkAccelLabel") (mapcar #'gtype-name (g-type-children gtype)))
    (assert-equal '("GtkAccelLabel") (mapcar #'gtype-name (g-type-children id)))
    (assert-equal '("GtkAccelLabel") (mapcar #'gtype-name (g-type-children name)))
    ;; g-type-interfaces
    (assert-equal '("AtkImplementorIface" "GtkBuildable")
                  (mapcar #'gtype-name (g-type-interfaces gtype)))
    (assert-equal '("AtkImplementorIface" "GtkBuildable")
                  (mapcar #'gtype-name (g-type-interfaces id)))
    (assert-equal '("AtkImplementorIface" "GtkBuildable")
                  (mapcar #'gtype-name (g-type-interfaces name)))
    ;; g-type-interace-prerequisites
    ;; g-type-set-qdata
    (g-type-set-qdata gtype "myData" (null-pointer))
    ;; g-type-get-qdata
    (assert-true  (null-pointer-p (g-type-get-qdata gtype "myData")))
    ;; g-type-query
    (with-foreign-object (query 'g-type-query)
      (g-type-query name query)
      (assert-eq gtype (foreign-slot-value query 'g-type-query :type))
      (assert-equal name (foreign-slot-value query 'g-type-query :type-name))
      (assert-eql 476 (foreign-slot-value query 'g-type-query :class-size))
      (assert-eql  24 (foreign-slot-value query 'g-type-query :instance-size)))
    ;; g-type-fundamental
    (assert-eq (gtype "GObject") (g-type-fundamental gtype))
    (assert-eq (gtype "GObject") (g-type-fundamental id))
    (assert-eq (gtype "GObject") (g-type-fundamental name))
    ;; g-type-value-table-peek
    (with-foreign-object (table 'g-type-value-table)
      (setq table (g-type-value-table-peek gtype))
      (assert-false (null-pointer-p (foreign-slot-value table 'g-type-value-table :value-init)))
      (assert-false (null-pointer-p (foreign-slot-value table 'g-type-value-table :value-free)))
      (assert-false (null-pointer-p (foreign-slot-value table 'g-type-value-table :value-copy)))
      (assert-false (null-pointer-p (foreign-slot-value table 'g-type-value-table :value-peek-pointer)))
      (assert-equal "p" (foreign-slot-value table 'g-type-value-table :collect-format))
      (assert-false (null-pointer-p (foreign-slot-value table 'g-type-value-table :collect-value)))
      (assert-equal "p" (foreign-slot-value table 'g-type-value-table :lcopy-format))
      (assert-false (null-pointer-p (foreign-slot-value table 'g-type-value-table :lcopy-value))))))

;;; --- End of file rtest-gobject-type-info.lisp -------------------------------
