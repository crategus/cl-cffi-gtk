;;; ----------------------------------------------------------------------------
;;; gtk.frame.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See <http://common-lisp.net/project/cl-gtk2/>.
;;;
;;; The documentation of this file is taken from the GTK+ 3 Reference Manual
;;; Version 3.8.8 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2014 Dieter Kaiser
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
;;; GtkFrame
;;;
;;; A bin with a decorative frame and optional label
;;;
;;; Synopsis
;;;
;;;     GtkFrame
;;;
;;;     gtk_frame_new
;;;     gtk_frame_set_label
;;;     gtk_frame_set_label_widget
;;;     gtk_frame_set_label_align
;;;     gtk_frame_set_shadow_type
;;;     gtk_frame_get_label
;;;     gtk_frame_get_label_align
;;;     gtk_frame_get_label_widget
;;;     gtk_frame_get_shadow_type
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkFrame
;;; ----------------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (register-object-type "GtkFrame" 'gtk-frame))

(define-g-object-class "GtkFrame" gtk-frame
  (:superclass gtk-bin
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable")
   :type-initializer "gtk_frame_get_type")
  ((label
    gtk-frame-label
    "label" "gchararray" t t)
   (label-widget
    gtk-frame-label-widget
    "label-widget" "GtkWidget" t t)
   (label-xalign
    gtk-frame-label-xalign
    "label-xalign" "gfloat" t t)
   (label-yalign
    gtk-frame-label-yalign
    "label-yalign" "gfloat" t t)
   (shadow-type
    gtk-frame-shadow-type
    "shadow-type" "GtkShadowType" t t)))

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-frame 'type)
 "@version{2013-7-15}
  @begin{short}
    The frame widget is a @class{gtk-bin} widget that surrounds its child with
    a decorative frame and an optional label.
  @end{short}
  If present, the label is drawn in a gap in the top side of the frame. The
  position of the label can be controlled with the function
  @fun{gtk-frame-set-label-align}.

  @subheading{GtkFrame as GtkBuildable}
    The @sym{gtk-frame} implementation of the @class{gtk-buildable} interface
    supports placing a child in the label position by specifying
    @code{\"label\"} as the @code{\"type\"} attribute of a @code{<child>}
    element. A normal content child can be specified without specifying a
    @code{<child>} type attribute.

    @b{Example:} A UI definition fragment with @sym{gtk-frame}
    @begin{pre}
 <object class=\"GtkFrame\">
  <child type=\"label\">
    <object class=\"GtkLabel\" id=\"frame-label\"/>
  </child>
  <child>
    <object class=\"GtkEntry\" id=\"frame-content\"/>
  </child>
 </object>
    @end{pre}
  @see-slot{gtk-frame-label}
  @see-slot{gtk-frame-label-widget}
  @see-slot{gtk-frame-label-xalign}
  @see-slot{gtk-frame-label-yalign}
  @see-slot{gtk-frame-shadow-type}
  @see-class{gtk-bin}
  @see-class{gtk-widget}
  @see-class{gtk-buildable}
  @see-symbol{gtk-shadow-type}
  @see-function{gtk-frame-set-label-align}")

;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "label" 'gtk-frame) 't)
 "The @code{\"label\"} property of type  @code{:string} (Read / Write) @br{}
  Text of the frame's label. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "label-widget" 'gtk-frame) 't)
 "The @code{\"label-widget\"} property of type @class{gtk-widget}
  (Read / Write) @br{}
  A widget to display in place of the usual frame label.")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "label-xalign" 'gtk-frame) 't)
 "The @code{\"label-xalign\"} property of type @code{:float}
  (Read / Write) @br{}
  The horizontal alignment of the label. @br{}
  Allowed values: [0,1] @br{}
  Default value: 0")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "label-yalign" 'gtk-frame) 't)
 "The @code{\"label-yalign\"} property of type @code{:float}
  (Read / Write) @br{}
  The vertical alignment of the label. @br{}
  Allowed values: [0,1] @br{}
  Default value: 0.5")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "shadow-type" 'gtk-frame) 't)
 "The @code{\"shadow-type\"} property of type @symbol{gtk-shadow-type}
  (Read / Write) @br{}
  Appearance of the frame border. @br{}
  Default value: @code{:etched-in}")

;;; ----------------------------------------------------------------------------
;;;
;;; Accessors of Properties
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-frame-label atdoc:*function-name-alias*) "Accessor"
      (documentation 'gtk-frame-label 'function)
 "@version{2014-1-19}
  Accessor of the slot @slot[gtk-frame]{label} of the @class{gtk-frame} class.
  @see-class{gtk-frame}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-frame-label-widget atdoc:*function-name-alias*) "Accessor"
      (documentation 'gtk-frame-label-widget 'function)
 "@version{2014-1-19}
  Accessor of the slot @slot[gtk-frame]{label-widget} of the @class{gtk-frame}
  class.
  @see-class{gtk-frame}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-frame-label-xalign atdoc:*function-name-alias*) "Accessor"
      (documentation 'gtk-frame-label-xalign 'function)
 "@version{2014-1-19}
  Accessor of the slot @slot[gtk-frame]{label-xalign} of the @class{gtk-frame}
  class.
  @see-class{gtk-frame}
  @see-function{gtk-frame-label-yalign}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-frame-label-yalign atdoc:*function-name-alias*) "Accessor"
      (documentation 'gtk-frame-label-yalign 'function)
 "@version{2014-1-19}
  Accessor of the slot @slot[gtk-frame]{label-yalign} of the @class{gtk-frame}
  class.
  @see-class{gtk-frame}
  @see-function{gtk-frame-xalign}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-frame-shadow-type atdoc:*function-name-alias*) "Accessor"
      (documentation 'gtk-frame-shadow-type 'function)
 "@version{2014-1-19}
  Accessor of the slot @slot[gtk-frame]{shadow-type} of the @class{gtk-frame}
  class.
  @see-class{gtk-frame}")

;;; ----------------------------------------------------------------------------
;;; gtk_frame_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-frame-new))

(defun gtk-frame-new (label)
 #+cl-cffi-gtk-documentation
 "@version{2014-1-19}
  @argument[label]{the text to use as the label of the frame}
  @return{A new @class{gtk-frame} container.}
  Creates a new @class{gtk-frame} container, with optional label @arg{label}.
  If @arg{label} is @code{nil}, the label is omitted.
  @see-class{gtk-frame}"
  (make-instance 'gtk-frame
                 :label label))

(export 'gtk-frame-new)

;;; ----------------------------------------------------------------------------
;;; gtk_frame_set_label ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-frame-set-label))

(defun gtk-frame-set-label (frame label)
 #+cl-cffi-gtk-documentation
 "@version{2014-1-19}
  @argument[frame]{a @class{gtk-frame} container}
  @argument[label]{the text to use as the label of the frame}
  Sets the text of the label. If @arg{label} is @code{nil}, the current
  label is removed.
  @see-class{gtk-frame}
  @see-function{gtk-frame-get-label}"
  (setf (gtk-frame-label frame) label))

(export 'gtk-frame-set-label)

;;; ----------------------------------------------------------------------------
;;; gtk_frame_set_label_widget ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-frame-set-label-widget))

(defun gtk-frame-set-label-widget (frame label-widget)
 #+cl-cffi-gtk-documentation
 "@version{2014-1-19}
  @argument[frame]{a @class{gtk-frame} container}
  @argument[label-widget]{the new label widget}
  Sets the label widget for the frame. This is the widget that will appear
  embedded in the top edge of the frame as a title.
  @see-class{gtk-frame}
  @see-function{gtk-frame-get-label-widget}"
  (setf (gtk-frame-label-widget frame) label-widget))

(export 'gtk-frame-set-label-widget)

;;; ----------------------------------------------------------------------------
;;; gtk_frame_set_label_align ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-frame-set-label-align))

(defun gtk-frame-set-label-align (frame xalign yalign)
 #+cl-cffi-gtk-documentation
 "@version{2014-1-19}
  @argument[frame]{a @class{gtk-frame} container}
  @argument[xalign]{the position of the label along the top edge of the widget,
    a value of 0.0 represents left alignment; 1.0 represents right alignment}
  @argument[yalign]{the y alignment of the label, a value of 0.0 aligns under
    the frame; 1.0 aligns above the frame, if the values are exactly 0.0 or 1.0
    the gap in the frame will not be painted because the label will be
    completely above or below the frame}
  Sets the alignment of the frame container's label. The default values
  for a newly created frame are 0.0 and 0.5.
  @see-class{gtk-frame}
  @see-function{gtk-frame-get-label-align}"
  (setf (gtk-frame-label-xalign frame) xalign
        (gtk-frame-label-yalign frame) yalign))

(export 'gtk-frame-set-label-align)

;;; ----------------------------------------------------------------------------
;;; gtk_frame_set_shadow_type ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-frame-set-shadow-type))

(defun gtk-frame-set-shadow-type (frame type)
 #+cl-cffi-gtk-documentation
 "@version{2014-1-19}
  @argument[frame]{a @class{gtk-frame} container}
  @argument[type]{the new @symbol{gtk-shadow-type}}
  Sets the shadow type for @arg{frame}.
  @see-class{gtk-frame}
  @see-symbol{gtk-shadow-type}
  @see-function{gtk-frame-get-shadow-type}"
  (setf (gtk-frame-shadow-type frame) type))

(export 'gtk-frame-set-shadow-type)

;;; ----------------------------------------------------------------------------
;;; gtk_frame_get_label ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-frame-get-label))

(defun gtk-frame-get-label (frame)
 #+cl-cffi-gtk-documentation
 "@version{2014-1-19}
  @argument[frame]{a @class{gtk-frame} container}
  @return{The text in the label, or @code{nil} if there was no label widget or
    the label widget was not a @class{gtk-label}.}
  @begin{short}
    If the frame's label widget is a @class{gtk-label}, returns the text
    in the label widget.
  @end{short}
  The frame will have a @class{gtk-label} for the label widget if a
  non-@code{nil} argument was passed to @fun{gtk-frame-new}.
  @see-class{gtk-frame}
  @see-class{gtk-label}
  @see-function{gtk-frame-new}
  @see-function{gtk-frame-set-label}"
  (gtk-frame-label frame))

(export 'gtk-frame-get-label)

;;; ----------------------------------------------------------------------------
;;; gtk_frame_get_label_align ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-frame-get-label-align))

(defun gtk-frame-get-label-align (frame)
 #+cl-cffi-gtk-documentation
 "@version{2014-1-19}
  @argument[frame]{a @class{gtk-frame} container}
  @begin{return}
    @code{xalign} -- x alignment of @arg{frame}'s label, or @code{nil} @br{}
    @code{yalign} -- y alignment of @arg{frame}'s label, or @code{nil}
  @end{return}
  @begin{short}
    Retrieves the x and y alignment of the frame's label.
  @end{short}
  See the function @fun{gtk-frame-set-label-align}.
  @see-class{gtk-frame}
  @see-function{gtk-frame-set-label-align}"
  (values (gtk-frame-label-xalign frame)
          (gtk-frame-label-yalign frame)))

(export 'gtk-frame-get-label-align)

;;; ----------------------------------------------------------------------------
;;; gtk_frame_get_label_widget ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-frame-get-label-widget))

(defun gtk-frame-get-label-widget (frame)
 #+cl-cffi-gtk-documentation
 "@version{2014-1-19}
  @argument[frame]{a @class{gtk-frame} container}
  @return{The label widget, or @code{nil} if there is none.}
  Retrieves the label widget for the frame.
  See the function @fun{gtk-frame-set-label-widget}.
  @see-class{gtk-frame}
  @see-function{gtk-frame-set-label-widget}"
  (gtk-frame-label-widget frame))

(export 'gtk-frame-get-label-widget)

;;; ----------------------------------------------------------------------------
;;; gtk_frame_get_shadow_type ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-frame-get-shadow-type))

(defun gtk-frame-get-shadow-type (frame)
 #+cl-cffi-gtk-documentation
 "@version{2014-1-19}
  @argument[frame]{a @class{gtk-frame} container}
  @return{The current shadow type of the frame.}
  Retrieves the shadow type of the @arg{frame}.
  See the function @fun{gtk-frame-set-shadow-type}.
  @see-class{gtk-frame}
  @see-function{gtk-frame-set-shadow-type}"
  (gtk-frame-shadow-type frame))

(export 'gtk-frame-get-shadow-type)

;;; --- End of file gtk.frame.lisp ---------------------------------------------
