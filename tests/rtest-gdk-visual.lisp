;;; ----------------------------------------------------------------------------
;;; rtest-gdk-visual.lisp
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

(in-package :gdk-tests)

(define-test gdk-visual
  (assert-true  (g-type-is-object "GdkVisual"))
  (assert-false (g-type-is-abstract "GdkVisual"))
  (assert-true  (g-type-is-derived "GdkVisual"))
  (assert-false (g-type-is-fundamental "GdkVisual"))
  (assert-true  (g-type-is-value-type "GdkVisual"))
  (assert-true  (g-type-has-value-table "GdkVisual"))
  (assert-true  (g-type-is-classed "GdkVisual"))
  (assert-true  (g-type-is-instantiatable "GdkVisual"))
  (assert-true  (g-type-is-derivable "GdkVisual"))
  (assert-true  (g-type-is-deep-derivable "GdkVisual"))
  (assert-false (g-type-is-interface "GdkVisual"))

  ;; Check the registered name
  (assert-eq 'gdk-visual
             (registered-object-type-by-name "GdkVisual"))
  
  (let ((class (g-type-class-ref (gtype "GdkVisual"))))
    (assert-equal (gtype "GdkVisual")  (g-type-from-class class))
    (assert-equal (gtype "GdkVisual") (g-object-class-type class))
    (assert-equal "GdkVisual" (g-object-class-name class))
    (assert-equal (gtype "GdkVisual")
                  (g-type-from-class (g-type-class-peek "GdkVisual")))
    (assert-equal (gtype "GdkVisual")
                  (g-type-from-class (g-type-class-peek-static "GdkVisual")))
    (g-type-class-unref class))
  
  (let ((class (find-class 'gdk-visual)))
    ;; Check the class name and type of the class
    (assert-eq 'gdk-visual (class-name class))
    (assert-eq 'gobject-class (type-of class))
    (assert-eq (find-class 'gobject-class) (class-of class))
    ;; Properties of the metaclass gobject-class
    (assert-equal "GdkVisual" (gobject-class-g-type-name class))
    (assert-equal "GdkVisual" (gobject-class-direct-g-type-name class))
    (assert-equal "gdk_visual_get_type"
                  (gobject-class-g-type-initializer class))
    (assert-false (gobject-class-interface-p class)))
  
  (assert-equal (gtype "GObject") (g-type-parent "GdkVisual"))
  (assert-eql 2 (g-type-depth "GdkVisual"))
  (assert-eql   (gtype "GdkVisual")
                (g-type-next-base "GdkVisual" "GObject"))
  (assert-true  (g-type-is-a "GdkVisual" "GObject"))
  (assert-false (g-type-is-a "GdkVisual" "gboolean"))
  (assert-false (g-type-is-a "GdkVisual" "GtkWindow"))
  (assert-equal '("GdkX11Visual")
                (mapcar #'gtype-name (g-type-children "GdkVisual")))
  (assert-equal '()
                (mapcar #'gtype-name (g-type-interfaces "GdkVisual")))
  
  ;; Query infos about the class
  (with-foreign-object (query 'g-type-query)
    (g-type-query "GdkVisual" query)
    (assert-equal (gtype "GdkVisual")
                  (foreign-slot-value query 'g-type-query :type))
    (assert-equal "GdkVisual"
                  (foreign-slot-value query 'g-type-query :type-name))
    (assert-eql  68 (foreign-slot-value query 'g-type-query :class-size))
    (assert-eql  72 (foreign-slot-value query 'g-type-query :instance-size)))
  
  ;; Get the names of the class properties of "GdkVisual".
  (assert-equal
     '()
     (mapcar #'param-spec-name
             (g-object-class-list-properties (gtype "GdkVisual"))))

  ;; Get the class definition
  (assert-equal
     '(DEFINE-G-OBJECT-CLASS "GdkVisual" GDK-VISUAL
                               (:SUPERCLASS G-OBJECT :EXPORT T :INTERFACES NIL
                                :TYPE-INITIALIZER "gdk_visual_get_type")
                               NIL)
     (get-g-type-definition (gtype "GdkVisual")))

  ;; Create an instance
  (let ((visual (gdk-visual-get-system)))
    ;; Some general checks of the instance
    (assert-equal (gtype "GdkX11Visual") (g-object-type visual))
    (assert-equal "GdkX11Visual" (g-object-type-name visual))
    (assert-true (g-type-is-a "GdkX11Visual"
                              (g-type-from-instance (pointer visual))))

    ;; Call functions
    (assert-equal '(32 24) (gdk-query-depths))
    (assert-equal  '(:DIRECT-COLOR :TRUE-COLOR) (gdk-query-visual-types))
    (assert-eql 32 (length (gdk-list-visuals)))
    (assert-eql 8 (gdk-visual-get-bits-per-rgb visual))    
    (assert-equal (values 255 0 8) (gdk-visual-get-blue-pixel-details visual))
    (assert-eql :lsb-first (gdk-visual-get-byte-order visual))
    (assert-eql 256 (gdk-visual-get-colormap-size visual))
    (assert-eql 24 (gdk-visual-get-depth visual))
    (assert-equal (values  65280 8 8) (gdk-visual-get-green-pixel-details visual))
    (assert-eql (values 16711680 16 8) (gdk-visual-get-red-pixel-details visual))
    (assert-eq :TRUE-COLOR (gdk-visual-get-visual-type visual))
    (assert-eql 32 (gdk-visual-get-best-depth))
    (assert-eq :DIRECT-COLOR (gdk-visual-get-best-type))
    (assert-eq 'gdk-visual (type-of (gdk-visual-get-system)))
    (assert-eq 'gdk-visual (type-of (gdk-visual-get-best)))
    (assert-eq 'gdk-visual (type-of (gdk-visual-get-best-with-depth 32)))
    (assert-eq 'gdk-visual (type-of (gdk-visual-get-best-with-type :true-color)))
    (assert-eq 'gdk-visual (type-of (gdk-visual-get-best-with-both 32 :true-color)))
    (assert-eq 'gdk-screen (type-of (gdk-visual-get-screen visual))))
)

;;; --- End of file rtest-gdk-visual.lisp --------------------------------------
