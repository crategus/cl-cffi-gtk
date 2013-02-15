;;; ----------------------------------------------------------------------------
;;; gtk.arrow.lisp
;;; 
;;; This file contains code from a fork of cl-gtk2.
;;; See http://common-lisp.net/project/cl-gtk2/
;;; 
;;; The documentation has been copied from the GTK+ 3 Reference Manual
;;; Version 3.4.3. See http://www.gtk.org.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
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
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkArrow
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkArrow" gtk-arrow
  (:superclass gtk-misc
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable")
   :type-initializer "gtk_arrow_get_type")
  ((arrow-type
    gtk-arrow-arrow-type
    "arrow-type" "GtkArrowType" t t)
   (shadow-type
    gtk-arrow-shadow-type
    "shadow-type" "GtkShadowType" t t)))

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-arrow 'type)
 "@version{2013-2-12}
  @begin{short}
    GtkArrow should be used to draw simple arrows that need to point in one of
    the four cardinal directions (up, down, left, or right). The style of the
    arrow can be one of shadow in, shadow out, etched in, or etched out. Note
    that these directions and style types may be ammended in versions of GTK+ to
    come.
  @end{short}

  GtkArrow will fill any space alloted to it, but since it is inherited from
  GtkMisc, it can be padded and/or aligned, to fill exactly the space the
  programmer desires.

  Arrows are created with a call to gtk_arrow_new(). The direction or style of
  an arrow can be changed after creation by using gtk_arrow_set().
  @begin[Style Property Details]{dictionary}
    @subheading{The \"arrow-scaling\" style property}
      @code{\"arrow-scaling\"} of type @code{gfloat} (Read)@br{}
      Amount of space used up by arrow.@br{}
      Allowed values: @code{[0,1]}@br{}
      Default value: @code{0.7}
  @end{dictionary}
  @see-slot{gtk-arrow-arrow-type}
  @see-slot{gtk-arrow-shadow-type}")

;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "arrow-type" 'gtk-arrow) 't)
 "The @code{\"arrow-type\"} property of type @symbol{gtk-arrow-type}
  (Read / Write)@br{}
  The direction the arrow should point.@br{}
  Default value: @code{:right}")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "shadow-type" 'gtk-arrow) 't)
 "The @code{\"shadow-type\"} property of type @symbol{gtk-shadow-type}
  (Read / Write)@br{}
  Appearance of the shadow surrounding the arrow.@br{}
  Default value: @code{:out}")

;;; ----------------------------------------------------------------------------
;;;
;;; Accessors
;;;
;;; ----------------------------------------------------------------------------

;;; --- gtk-arrow-arrow-type ---------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-arrow-arrow-type atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-arrow-arrow-type 'function)
 "@version{2013-2-12}
  @begin{short}
    Accessor of the slot @code{\"arrow-type\"} of the @class{gtk-arrow} class.
  @end{short}")

;;; --- gtk-arrow-shadow-type --------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-arrow-shadow-type atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-arrow-shadow-type 'function)
 "@version{2013-2-12}
  @begin{short}
    Accessor of the slot @code{\"shadow-type\"} of the @class{gtk-arrow} class.
  @end{short}")

;;; ----------------------------------------------------------------------------
;;; gtk_arrow_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-arrow-new))

(defun gtk-arrow-new (arrow-type shadow-type)
 #+cl-cffi-gtk-documentation
 "@version{2013-2-12}
  @argument[arrow_type]{a valid GtkArrowType.}
  @argument[shadow_type]{a valid GtkShadowType.}
  @return{the new GtkArrow widget.}
  @short{Creates a new GtkArrow widget.}"
  (make-instance 'gtk-arrow
                 :arrow-type arrow-type
                 :shadow-type shadow-type))

(export 'gtk-arrow-new)

;;; ----------------------------------------------------------------------------
;;; gtk_arrow_set ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-arrow-set))

(defun gtk-arrow-set (arrow arrow-type shadow-type)
 #+cl-cffi-gtk-documentation
 "@version{2013-2-12}
  @argument[arrow]{a widget of type GtkArrow.}
  @argument[arrow_type]{a valid GtkArrowType.}
  @argument[shadow_type]{a valid GtkShadowType.}
  @short{Sets the direction and style of the GtkArrow, arrow.}"
  (setf (gtk-arrow-arrow-type arrow) arrow-type
        (gtk-arrow-shadow-type arrow) shadow-type))

(export 'gtk-arrow-set)

;;; --- End of file gtk.arrow.lisp ---------------------------------------------
