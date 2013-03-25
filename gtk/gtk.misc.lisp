;;; ----------------------------------------------------------------------------
;;; gtk.misc.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See <http://common-lisp.net/project/cl-gtk2/>.
;;;
;;; The documentation has been copied from the GTK+ 3 Reference Manual
;;; Version 3.4.1. See <http://www.gtk.org>. The API documentation of the
;;; Lisp binding is available at <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2013 Dieter Kaiser
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
;;; GtkMisc
;;; 
;;; Base class for widgets with alignments and padding
;;; 
;;;     GtkMisc
;;;     
;;;     gtk_misc_set_alignment
;;;     gtk_misc_set_padding
;;;     gtk_misc_get_alignment
;;;     gtk_misc_get_padding
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkMisc
;;; 
;;; struct GtkMisc;
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkMisc" gtk-misc
  (:superclass gtk-widget
   :export t
   :interfaces ("AtkImplementorIface" "GtkBuildable")
   :type-initializer "gtk_misc_get_type")
  ((xalign
    gtk-misc-xalign
    "xalign" "gfloat" t t)
   (xpad
    gtk-misc-xpad
    "xpad" "gint" t t)
   (yalign
    gtk-misc-yalign
    "yalign" "gfloat" t t)
   (ypad
    gtk-misc-ypad "ypad" "gint" t t)))

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-misc 'type)
 "@version{2013-3-17}
  @short{Base class for widgets with alignments and padding.}

  The GtkMisc widget is an abstract widget which is not useful itself, but is
  used to derive subclasses which have alignment and padding attributes.

  The horizontal and vertical padding attributes allows extra space to be
  added around the widget.

  The horizontal and vertical alignment attributes enable the widget to be
  positioned within its allocated area. Note that if the widget is added to a
  container in such a way that it expands automatically to fill its allocated
  area, the alignment settings will not alter the widgets position.
  @begin[Note]{dictionary}
    Note that the desired effect can in most cases be achieved by using the
    \"halign\", \"valign\" and \"margin\" properties on the child widget, so
    GtkMisc should not be used in new code.
  @end{dictionary}
  @see-slot{gtk-misc-xalign}
  @see-slot{gtk-misc-xpad}
  @see-slot{gtk-misc-yalign}
  @see-slot{gtk-misc-ypad}")

;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "xalign" 'gtk-misc) 't)
 "The @code{\"xalign\"} property of type @code{gfloat} (Read / Write)@br{}
  The horizontal alignment, from 0 (left) to 1 (right). Reversed
  for RTL layouts. @br{}
  Allowed values: [0,1]@br{}
  Default value: 0.5")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "xpad" 'gtk-misc) 't)
 "The @code{\"xpad\"} property of type @code{:int} (Read / Write)@br{}
  The amount of space to add on the left and right of the widget, in
  pixels.@br{}
  Allowed values: >= 0@br{}
  Default value: 0")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "yalign" 'gtk-misc) 't)
 "The @code{\"yalign\"} property of type @code{gfloat} (Read / Write)@br{}
  The vertical alignment, from 0 (top) to 1 (bottom). @br{}
  Allowed values: [0,1]@br{}
  Default value: 0.5")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "ypad" 'gtk-misc) 't)
 "The @code{\"ypad\"} property of type @code{:int} (Read / Write)@br{}
  The amount of space to add on the top and bottom of the widget, in
  pixels. @br{}
  Allowed values: >= 0@br{}
  Default value: 0")

;;; ----------------------------------------------------------------------------
;;;
;;; Accessors
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-misc-xalign atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-misc-xalign 'function)
 "@version{2013-3-17}
  Accessor of the slot @code{\"xalign\"} of the @class{gtk-misc} class.")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-misc-xpad atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-misc-xpad 'function)
 "@version{2013-3-17}
  Accessor of the slot @code{\"xpad\"} of the @class{gtk-misc} class.")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-misc-yalign atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-misc-yalign 'function)
 "@version{2013-3-17}
  Accessor of the slot @code{\"yalign\"} of the @class{gtk-misc} class.")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-misc-ypad atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-misc-ypad 'function)
 "@version{2013-3-17}
  Accessor of the slot @code{\"ypad\"} of the @class{gtk-misc} class.")

;;; ----------------------------------------------------------------------------
;;; gtk_misc_set_alignment ()
;;; ----------------------------------------------------------------------------

(defun gtk-misc-set-alignment (misc xalign yalign)
 #+cl-cffi-gtk-documentation
 "@version{2013-3-2013}
  @argument[misc]{a @class{gtk-misc} widget.}
  @argument[xalign]{the horizontal alignment, from 0 (left) to 1 (right)}
  @argument[yalign]{the vertical alignment, from 0 (top) to 1 (bottom)}
  Sets the alignment of the widget."
  (setf (gtk-misc-xalign misc) xalign
        (gtk-misc-yalign misc) yalign))

(export 'gtk-misc-set-alignment)

;;; ----------------------------------------------------------------------------
;;; gtk_misc_set_padding ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-misc-set-padding))

(defun gtk-misc-set-padding (misc xpad ypad)
 #+cl-cffi-gtk-documentation
 "@version{2013-3-17}
  @argument[misc]{a @class{gtk-misc} widget}
  @argument[xpad]{the amount of space to add on the left and right of the
    widget, in pixels}
  @argument[ypad]{the amount of space to add on the top and bottom of the
    widget, in pixels}
  Sets the amount of space to add around the widget."
  (setf (gtk-misc-xpad misc) xpad
        (gtk-misc-ypad misc) ypad))

(export 'gtk-misc-set-padding)

;;; ----------------------------------------------------------------------------
;;; gtk_misc_get_alignment ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-misc-get-alignment))

(defun gtk-misc-get-alignment (misc)
 #+cl-cffi-gtk-documentation
 "@version{2013-3-17}
  @argument[misc]{a @class{gtk-misc} widget}
  @return{@arg{xalign} -- X alignment of misc, or @code{nil}@br{}
          @arg{yalign} -- Y alignment of misc, or @code{nil}}
  @short{Gets the X and Y alignment of the widget within its allocation.}
  See @fun{gtk-misc-set-alignment}.
  @see-function{gtk-misc-set-alignment}"
  (values (gtk-misc-xalign misc)
          (gtk-misc-yalign misc)))

(export 'gtk-misc-get-alignment)

;;; ----------------------------------------------------------------------------
;;; gtk_misc_get_padding ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-misc-get-padding))

(defun gtk-misc-get-padding (misc)
 #+cl-cffi-gtk-documentation
 "@version{2013-3-17}
  @argument[misc]{a @class{gtk-misc} widget}
  @return{@arg{xpad} -- padding in the X direction, or @code{nil}@br{}
          @arg{ypad} -- padding in the Y direction, or @code{nil}}
  @short{Gets the padding in the X and Y directions of the widget.}
  See @fun{gtk-misc-set-padding}.
  @see-function{gtk-misc-set-padding}"
  (values (gtk-misc-xpad misc)
          (gtk-misc-ypad misc)))

(export 'gtk-misc-get-padding)

;;; --- End of file gtk.misc.lisp ----------------------------------------------
