;;; ----------------------------------------------------------------------------
;;; gtk.arrow.lisp
;;; 
;;; This file contains code from a fork of cl-gtk2.
;;; See http://common-lisp.net/project/cl-gtk2/
;;; 
;;; The documentation has been copied from the GTK 3.2.3 Reference Manual
;;; See http://www.gtk.org.
;;; 
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
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
;;;
;;; GtkArrow
;;; 
;;; Displays an arrow
;;; 
;;; Synopsis
;;; 
;;;     GtkArrow
;;;
;;;     gtk_arrow_new
;;;     gtk_arrow_set
;;; 
;;; Object Hierarchy
;;; 
;;;   GObject
;;;    +----GInitiallyUnowned
;;;          +----GtkWidget
;;;                +----GtkMisc
;;;                      +----GtkArrow
;;; 
;;; Implemented Interfaces
;;; 
;;; GtkArrow implements AtkImplementorIface and GtkBuildable.
;;;
;;; Properties
;;; 
;;;   "arrow-type"               GtkArrowType          : Read / Write
;;;   "shadow-type"              GtkShadowType         : Read / Write
;;; 
;;; Style Properties
;;; 
;;;   "arrow-scaling"            gfloat                : Read
;;; 
;;; Description
;;; 
;;; GtkArrow should be used to draw simple arrows that need to point in one of
;;; the four cardinal directions (up, down, left, or right). The style of the
;;; arrow can be one of shadow in, shadow out, etched in, or etched out. Note
;;; that these directions and style types may be ammended in versions of GTK+
;;; to come.
;;; 
;;; GtkArrow will fill any space alloted to it, but since it is inherited from
;;; GtkMisc, it can be padded and/or aligned, to fill exactly the space the
;;; programmer desires.
;;; 
;;; Arrows are created with a call to gtk_arrow_new(). The direction or style
;;; of an arrow can be changed after creation by using gtk_arrow_set().
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "arrow-type" property
;;; 
;;;   "arrow-type"               GtkArrowType          : Read / Write
;;; 
;;; The direction the arrow should point.
;;; 
;;; Default value: GTK_ARROW_RIGHT
;;;
;;; ----------------------------------------------------------------------------
;;; The "shadow-type" property
;;; 
;;;   "shadow-type"              GtkShadowType         : Read / Write
;;; 
;;; Appearance of the shadow surrounding the arrow.
;;; 
;;; Default value: GTK_SHADOW_OUT
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Style Property Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "arrow-scaling" style property
;;; 
;;;   "arrow-scaling"            gfloat                : Read
;;; 
;;; Amount of space used up by arrow.
;;; 
;;; Allowed values: [0,1]
;;; 
;;; Default value: 0.7
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkArrow
;;; 
;;; struct GtkArrow;
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkArrow" gtk-arrow
  (:superclass gtk-misc
   :export t
   :interfaces ("AtkImplementorIface" "GtkBuildable")
   :type-initializer "gtk_arrow_get_type")
  ((arrow-type
    gtk-arrow-arrow-type
    "arrow-type" "GtkArrowType" t t)
   (shadow-type
    gtk-arrow-shadow-type
    "shadow-type" "GtkShadowType" t t)))

;;; ----------------------------------------------------------------------------
;;; gtk_arrow_new ()
;;; 
;;; GtkWidget * gtk_arrow_new (GtkArrowType arrow_type,
;;;                            GtkShadowType shadow_type);
;;; 
;;; Creates a new GtkArrow widget.
;;; 
;;; arrow_type :
;;;     a valid GtkArrowType
;;; 
;;; shadow_type :
;;;     a valid GtkShadowType
;;; 
;;; Returns :
;;;     the new GtkArrow widget
;;; ----------------------------------------------------------------------------

(defun gtk-arrow-new (arrow-type shadow-type)
  (make-instance 'gtk-arrow
                 :arrow-type arrow-type
                 :shadow-type shadow-type))

(export 'gtk-arrow-new)

;;; ----------------------------------------------------------------------------
;;; gtk_arrow_set ()
;;; 
;;; void gtk_arrow_set (GtkArrow *arrow,
;;;                     GtkArrowType arrow_type,
;;;                     GtkShadowType shadow_type);
;;; 
;;; Sets the direction and style of the GtkArrow, arrow.
;;; 
;;; arrow :
;;;     a widget of type GtkArrow
;;; 
;;; arrow_type :
;;;     a valid GtkArrowType
;;; 
;;; shadow_type :
;;;     a valid GtkShadowType
;;; ----------------------------------------------------------------------------

(defun gtk-arrow-set (arrow arrow-type shadow-type)
  (setf (gtk-arrow-arrow-type arrow) arrow-type
        (gtk-arrow-shadow-type arrow) shadow-type))

(export 'gtk-arrow-set)

;;; --- End of file gtk.arrow.lisp ---------------------------------------------
