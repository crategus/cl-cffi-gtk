;;; ----------------------------------------------------------------------------
;;; rtest-gdk-window.lisp
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

(in-package :gdk-tests)

(define-test gdk-window
  (assert-false (g-type-is-abstract "GdkWindow"))
  (assert-true  (g-type-is-derived "GdkWindow"))  
  (assert-false (g-type-is-fundamental "GdkWindow"))  
  (assert-true  (g-type-is-value-type "GdkWindow"))
  (assert-true  (g-type-has-value-table "GdkWindow"))
  (assert-true  (g-type-is-classed "GdkWindow"))  
  (assert-true  (g-type-is-instantiatable "GdkWindow"))
  (assert-true  (g-type-is-derivable "GdkWindow"))
  (assert-true  (g-type-is-deep-derivable "GdkWindow"))
  (assert-false (g-type-is-interface "GtkWindow"))
  
  (let ((class (g-type-class-ref (gtype "GdkWindow"))))
    (assert-equal (gtype "GdkWindow")  (g-type-from-class class))
    (assert-equal (gtype "GdkWindow")
                  (g-type-from-class (g-type-class-peek "GdkWindow")))
    (assert-equal (gtype "GdkWindow")
                  (g-type-from-class (g-type-class-peek-static "GdkWindow")))
    (g-type-class-unref class))
  
  (let ((class (find-class 'gdk-Window)))
    ;; Check the class name and type of the class
    (assert-eq 'gdk-window (class-name class))
    (assert-eq 'gobject-class (type-of class))
    (assert-eq (find-class 'gobject-class) (class-of class))
    ;; Properties of the metaclass gobject-class
    (assert-equal "GdkWindow" (gobject-class-g-type-name class))
    (assert-equal "GdkWindow" (gobject-class-direct-g-type-name class))
    (assert-equal "gdk_window_get_type"
                  (gobject-class-g-type-initializer class))
    (assert-false (gobject-class-interface-p class)))
  
  (assert-equal (gtype "GdkDrawable") (g-type-parent "GdkWindow"))
  (assert-eql 3 (g-type-depth "GdkWindow"))
  (assert-eql   (gtype "GdkDrawable")
                (g-type-next-base "GdkWindow" "GObject"))
  (assert-true  (g-type-is-a "GdkWindow" "GdkWindow"))
  (assert-true  (g-type-is-a "GdkWindow" "GdkDrawable"))
  (assert-false (g-type-is-a "GdkWindow" "gboolean"))
  (assert-false (g-type-is-a "GdkWindow" "GtkWindow"))
  (assert-equal '() (mapcar #'gtype-name (g-type-children "GdkWindow")))
  (assert-equal '() (mapcar #'gtype-name (g-type-interfaces "GdkWindow")))
  
  ;; Query infos about the class "GdkWindow"
  (with-foreign-object (query 'g-type-query)
    (g-type-query "GdkWindow" query)
    (assert-equal (gtype "GdkWindow")
                  (foreign-slot-value query 'g-type-query :type))
    (assert-equal "GdkWindow"
                  (foreign-slot-value query 'g-type-query :type-name))
    (assert-eql 224 (foreign-slot-value query 'g-type-query :class-size))
    (assert-eql 172 (foreign-slot-value query 'g-type-query :instance-size)))
  
  ;; Get the names of the class properties.
  (assert-equal
      '("cursor")
     (mapcar #'param-spec-name
             (g-object-class-list-properties (gtype "GdkWindow"))))
    
  (let ((win (gdk-window-new (null-pointer) (make-gdk-window-attr) '())))
    (assert-true win)
    (assert-true (g-type-is-a "GdkWindow"
                              (g-type-from-instance (pointer win))))
    (assert-true (g-type-is-a "GdkDisplayX11"
                              (g-type-from-instance
                                (pointer (gdk-window-get-display win)))))
    (assert-true (g-type-is-a "GdkScreenX11"
                              (g-type-from-instance
                                (pointer (gdk-window-get-screen win)))))
    (assert-true (g-type-is-a "GdkVisual"
                              (g-type-from-instance
                                (pointer (gdk-window-get-visual win)))))
    (assert-eql 1 (gdk-window-get-width win))
    (assert-eql 1 (gdk-window-get-height win))
    (assert-eq :toplevel (gdk-window-get-window-type win))
    (assert-false (gdk-window-is-destroyed win))
    (assert-false (gdk-window-is-visible win))
    (assert-false (gdk-window-is-viewable win))
    (assert-false (gdk-window-is-shaped win))
    (assert-false (gdk-window-is-input-only win))
    (assert-equal '(:withdrawn) (gdk-window-get-state win))
    
    (gdk-window-maximize win)
    (assert-eql 1 (gdk-window-get-width win))
    (assert-eql 1 (gdk-window-get-height win))
    (gdk-window-unmaximize win)
                 )
                
                
 )

