;;; ----------------------------------------------------------------------------
;;; atdoc-gtk.misc.lisp
;;;
;;; Documentation strings for the library GTK.
;;;
;;; The documentation has been copied from the GTK+ 3 Reference Manual
;;; Version 3.4.1. See http://www.gtk.org.
;;;
;;; Copyright (C) 2013 Dieter Kaiser
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

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "xalign" property
;;; 
;;;   "xalign"                   gfloat                : Read / Write
;;; 
;;; The horizontal alignment, from 0 (left) to 1 (right). Reversed for RTL
;;; layouts.
;;; 
;;; Allowed values: [0,1]
;;; 
;;; Default value: 0.5
;;;
;;; ----------------------------------------------------------------------------
;;; The "xpad" property
;;; 
;;;   "xpad"                     gint                  : Read / Write
;;; 
;;; The amount of space to add on the left and right of the widget, in pixels.
;;; 
;;; Allowed values: >= 0
;;; 
;;; Default value: 0
;;;
;;; ----------------------------------------------------------------------------
;;; The "yalign" property
;;; 
;;;   "yalign"                   gfloat                : Read / Write
;;; 
;;; The vertical alignment, from 0 (top) to 1 (bottom).
;;; 
;;; Allowed values: [0,1]
;;; 
;;; Default value: 0.5
;;;
;;; ----------------------------------------------------------------------------
;;; The "ypad" property
;;; 
;;;   "ypad"                     gint                  : Read / Write
;;; 
;;; The amount of space to add on the top and bottom of the widget, in pixels.
;;; 
;;; Allowed values: >= 0
;;; 
;;; Default value: 0
;;; ----------------------------------------------------------------------------



;;; ----------------------------------------------------------------------------

(setf (documentation 'gtk-misc 'type)
 "@version{2013-1-5}
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

#|
;;; ----------------------------------------------------------------------------
;;; gtk_misc_set_alignment ()
;;; 
;;; void gtk_misc_set_alignment (GtkMisc *misc, gfloat xalign, gfloat yalign);
;;; 
;;; Sets the alignment of the widget.
;;; 
;;; misc :
;;;     a GtkMisc
;;; 
;;; xalign :
;;;     the horizontal alignment, from 0 (left) to 1 (right)
;;; 
;;; yalign :
;;;     the vertical alignment, from 0 (top) to 1 (bottom)
;;; ----------------------------------------------------------------------------

(defun gtk-misc-set-alignment (misc xalign yalign)
  (setf (gtk-misc-xalign misc) xalign
        (gtk-misc-yalign misc) yalign))

(export 'gtk-misc-set-alignment)

;;; ----------------------------------------------------------------------------
;;; gtk_misc_set_padding ()
;;; 
;;; void gtk_misc_set_padding (GtkMisc *misc, gint xpad, gint ypad);
;;; 
;;; Sets the amount of space to add around the widget.
;;; 
;;; misc :
;;;     a GtkMisc
;;; 
;;; xpad :
;;;     the amount of space to add on the left and right of the widget, in
;;;     pixels
;;; 
;;; ypad :
;;;     the amount of space to add on the top and bottom of the widget, in
;;;     pixels
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-misc-set-padding))

(defun gtk-misc-set-padding (misc xpad ypad)
  (setf (gtk-misc-xpad misc) xpad
        (gtk-misc-ypad misc) ypad))

(export 'gtk-misc-set-padding)

;;; ----------------------------------------------------------------------------
;;; gtk_misc_get_alignment ()
;;; 
;;; void gtk_misc_get_alignment (GtkMisc *misc, gfloat *xalign, gfloat *yalign);
;;; 
;;; Gets the X and Y alignment of the widget within its allocation.
;;; See gtk_misc_set_alignment().
;;; 
;;; misc :
;;;     a GtkMisc
;;; 
;;; xalign :
;;;     location to store X alignment of misc, or NULL
;;; 
;;; yalign :
;;;     location to store Y alignment of misc, or NULL
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-misc-get-alignment))

(defun gtk-misc-get-alignment (misc)
  (values (gtk-misc-xalign misc)
          (gtk-misc-yalign misc)))

(export 'gtk-misc-get-alignment)

;;; ----------------------------------------------------------------------------
;;; gtk_misc_get_padding ()
;;; 
;;; void gtk_misc_get_padding (GtkMisc *misc, gint *xpad, gint *ypad);
;;; 
;;; Gets the padding in the X and Y directions of the widget.
;;; See gtk_misc_set_padding().
;;; 
;;; misc :
;;;     a GtkMisc
;;; 
;;; xpad :
;;;     location to store padding in the X direction, or NULL
;;; 
;;; ypad :
;;;     location to store padding in the Y direction, or NULL
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-misc-get-padding))

(defun gtk-misc-get-padding (misc)
  (values (gtk-misc-xpad misc)
          (gtk-misc-ypad misc)))

(export 'gtk-misc-get-padding)
|#

;;; --- End of file atdoc-gtk.misc.lisp ----------------------------------------
