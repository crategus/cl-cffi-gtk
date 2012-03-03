;;; ----------------------------------------------------------------------------
;;; rtest-gtk-box.lisp
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

(in-package :gtk-tests)

(define-test gtk-box
  (assert-equal "GtkBox" (gtype-name (gtype "GtkBox")))
  (assert-eql 'gtk-box (registered-object-type-by-name "GtkBox"))
  (assert-equal "GtkContainer" (gtype-name (g-type-parent (gtype "GtkBox"))))
  (unordered-equal '("GtkButtonBox" "GtkHBox" "GtkVBox")
                   (mapcar #'gtype-name (g-type-children (gtype "GtkBox"))))
  
  (assert-equal
    '(DEFINE-G-OBJECT-CLASS "GtkBox" GTK-BOX
                       (:SUPERCLASS GTK-CONTAINER :EXPORT T :INTERFACES
                        ("AtkImplementorIface" "GtkBuildable" "GtkOrientable")
                        :TYPE-INITIALIZER "gtk_box_get_type")
                       ((HOMOGENEOUS GTK-BOX-HOMOGENEOUS "homogeneous"
                         "gboolean" T T)
                        (SPACING GTK-BOX-SPACING "spacing" "gint" T T)))
   (gobject::get-g-class-definition (gtype "GtkBox")))
  
  (assert-equal
    '(PROGN
       (DEFCLASS GTK-BOX (GTK-CONTAINER ATK-IMPLEMENTOR-IFACE GTK-BUILDABLE
                                        GTK-ORIENTABLE)
         ((HOMOGENEOUS :ALLOCATION :GOBJECT-PROPERTY :G-PROPERTY-TYPE
           "gboolean" :ACCESSOR GTK-BOX-HOMOGENEOUS :INITARG :HOMOGENEOUS
             :G-PROPERTY-NAME "homogeneous")
          (SPACING :ALLOCATION :GOBJECT-PROPERTY :G-PROPERTY-TYPE "gint"
             :ACCESSOR GTK-BOX-SPACING :INITARG :SPACING :G-PROPERTY-NAME
             "spacing"))
          (:METACLASS GOBJECT-CLASS) (:G-TYPE-NAME . "GtkBox")
         (:G-TYPE-INITIALIZER . "gtk_box_get_type"))
       (EXPORT 'GTK-BOX (FIND-PACKAGE "GTK"))
       (EXPORT 'GTK-BOX-HOMOGENEOUS (FIND-PACKAGE "GTK"))
       (EXPORT 'GTK-BOX-SPACING (FIND-PACKAGE "GTK")))
   (macroexpand-1 (gobject::get-g-class-definition (gtype "GtkBox"))))
  )

(define-test gtk-hbox
  (let* ((hbox (make-instance 'gtk-hbox
                              :homogeneous nil
                              :spacing 0))
         (hbox-type (g-type-from-instance (pointer hbox))))
    (assert-equal "GtkHBox" (gtype-name hbox-type))
    (assert-eql 'gtk-hbox (registered-object-type-by-name "GtkHBox"))
    (assert-equal "GtkBox" (gtype-name (g-type-parent hbox-type)))
    (unordered-equal '("GtkFileChooserButton" "GtkStatusbar")
                     (mapcar #'gtype-name (g-type-children hbox-type)))
    
    (assert-false (gtk-box-homogeneous hbox))
    (assert-eql 0 (gtk-box-spacing hbox))
    
    (assert-true (gtk-box-set-homogeneous hbox t))
    (assert-true (gtk-box-get-homogeneous hbox))
    
    (assert-eql 10 (gtk-box-set-spacing hbox 10))
    (assert-eql 10 (gtk-box-get-spacing hbox))
    
    (assert-equal
      '(DEFINE-G-OBJECT-CLASS "GtkHBox" GTK-HBOX
                       (:SUPERCLASS GTK-BOX :EXPORT T :INTERFACES
                        ("AtkImplementorIface" "GtkBuildable" "GtkOrientable")
                        :TYPE-INITIALIZER "gtk_hbox_get_type")
         NIL)
     (gobject::get-g-class-definition hbox-type))
    
    (assert-equal
      '(PROGN
         (DEFCLASS GTK-HBOX (GTK-BOX ATK-IMPLEMENTOR-IFACE GTK-BUILDABLE
                                      GTK-ORIENTABLE) NIL
           (:METACLASS GOBJECT-CLASS) (:G-TYPE-NAME . "GtkHBox")
           (:G-TYPE-INITIALIZER . "gtk_hbox_get_type"))
         (EXPORT 'GTK-HBOX (FIND-PACKAGE "GTK")))
     (macroexpand-1 (gobject::get-g-class-definition (gtype "GtkHBox"))))
    
))

(define-test gtk-vbox
  (let* ((vbox (make-instance 'gtk-vbox
                              :homogeneous nil
                              :spacing 0))
         (vbox-type (g-type-from-instance (pointer vbox))))
    (assert-equal "GtkVBox" (gtype-name vbox-type))
    (assert-eql 'gtk-vbox (registered-object-type-by-name "GtkVBox"))
    (assert-equal "GtkBox" (gtype-name (g-type-parent vbox-type)))
    (unordered-equal '("GtkColorSelection"
                       "GtkFileChooserWidget"
                       "GtkFontSelection"
                       "GtkGammaCurve"
                       "GtkRecentChooserWidget")
                  (mapcar #'gtype-name (g-type-children vbox-type)))
    
    (assert-false (gtk-box-homogeneous vbox))
    (assert-eql 0 (gtk-box-spacing vbox))
    
    (assert-true (gtk-box-set-homogeneous vbox t))
    (assert-true (gtk-box-get-homogeneous vbox))
    
    (assert-eql 10 (gtk-box-set-spacing vbox 10))
    (assert-eql 10 (gtk-box-get-spacing vbox))
    
    (assert-equal
      '(DEFINE-G-OBJECT-CLASS "GtkVBox" GTK-VBOX
                       (:SUPERCLASS GTK-BOX :EXPORT T :INTERFACES
                        ("AtkImplementorIface" "GtkBuildable" "GtkOrientable")
                        :TYPE-INITIALIZER "gtk_vbox_get_type")
         NIL)
     (gobject::get-g-class-definition vbox-type))
    
    (assert-equal
      '(PROGN
         (DEFCLASS GTK-VBOX (GTK-BOX ATK-IMPLEMENTOR-IFACE GTK-BUILDABLE
                                      GTK-ORIENTABLE) NIL
           (:METACLASS GOBJECT-CLASS) (:G-TYPE-NAME . "GtkVBox")
           (:G-TYPE-INITIALIZER . "gtk_vbox_get_type"))
         (EXPORT 'GTK-VBOX (FIND-PACKAGE "GTK")))
     (macroexpand-1 (gobject::get-g-class-definition (gtype "GtkVBox"))))
    ))

;;; --- End of the file rtest-gtk-box.lisp -------------------------------------
