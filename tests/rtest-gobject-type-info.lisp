;;; ----------------------------------------------------------------------------
;;; rtest-gobject.lisp
;;;
;;; Copyright (C) 2011 - 2012 Dr. Dieter Kaiser
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

(define-test g-types
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
    