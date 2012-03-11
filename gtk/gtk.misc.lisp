;;; ----------------------------------------------------------------------------
;;; gtk.misc.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See http://common-lisp.net/project/cl-gtk2/
;;;
;;; The documentation has been copied from the GTK+ 3 Reference Manual
;;; Version 3.2.3. See http://www.gtk.org.
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
;;; GtkMisc
;;; 
;;; Base class for widgets with alignments and padding
;;; 
;;; Synopsis
;;; 
;;;     GtkMisc
;;;
;;;     gtk_misc_set_alignment
;;;     gtk_misc_set_padding
;;;     gtk_misc_get_alignment
;;;     gtk_misc_get_padding
;;; 
;;; Object Hierarchy
;;; 
;;;   GObject
;;;    +----GInitiallyUnowned
;;;          +----GtkWidget
;;;                +----GtkMisc
;;;                      +----GtkLabel
;;;                      +----GtkArrow
;;;                      +----GtkImage
;;; 
;;; Implemented Interfaces
;;; 
;;; GtkMisc implements AtkImplementorIface and GtkBuildable.
;;;
;;; Properties
;;; 
;;;   "xalign"                   gfloat                : Read / Write
;;;   "xpad"                     gint                  : Read / Write
;;;   "yalign"                   gfloat                : Read / Write
;;;   "ypad"                     gint                  : Read / Write
;;; 
;;; Description
;;; 
;;; The GtkMisc widget is an abstract widget which is not useful itself, but is
;;; used to derive subclasses which have alignment and padding attributes.
;;; 
;;; The horizontal and vertical padding attributes allows extra space to be
;;; added around the widget.
;;; 
;;; The horizontal and vertical alignment attributes enable the widget to be
;;; positioned within its allocated area. Note that if the widget is added to a
;;; container in such a way that it expands automatically to fill its allocated
;;; area, the alignment settings will not alter the widgets position.
;;; 
;;; Note
;;;
;;; Note that the desired effect can in most cases be achieved by using the
;;; "halign", "valign" and "margin" properties on the child widget, so GtkMisc
;;; should not be used in new code.
;;;
;;; ----------------------------------------------------------------------------
;;; 
;;; Property Details
;;; 
;;; ----------------------------------------------------------------------------
;;; The "xalign" property
;;; 
;;;   "xalign" gfloat                : Read / Write
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
;;;   "xpad" gint                  : Read / Write
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
;;;   "yalign" gfloat                : Read / Write
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
;;;   "ypad" gint                  : Read / Write
;;; 
;;; The amount of space to add on the top and bottom of the widget, in pixels.
;;; 
;;; Allowed values: >= 0
;;; 
;;; Default value: 0
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
;;; gtk_misc_set_alignment ()
;;; 
;;; void gtk_misc_set_alignment (GtkMisc *misc, gfloat xalign, gfloat yalign);
;;; 
;;; Sets the alignment of the widget.
;;; 
;;; misc :
;;;     a GtkMisc.
;;; 
;;; xalign :
;;;     the horizontal alignment, from 0 (left) to 1 (right).
;;; 
;;; yalign :
;;;     the vertical alignment, from 0 (top) to 1 (bottom).
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
;;;     a GtkMisc.
;;; 
;;; xpad :
;;;     the amount of space to add on the left and right of the widget,
;;;     in pixels.
;;; 
;;; ypad :
;;;     the amount of space to add on the top and bottom of the widget,
;;      in pixels.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_misc_get_alignment ()
;;; 
;;; void gtk_misc_get_alignment (GtkMisc *misc, gfloat *xalign, gfloat *yalign)
;;; 
;;; Gets the X and Y alignment of the widget within its allocation.
;;; See gtk_misc_set_alignment().
;;; 
;;; misc :
;;;     a GtkMisc
;;; 
;;; xalign :
;;;     location to store X alignment of misc, or NULL.
;;; 
;;; yalign :
;;;     location to store Y alignment of misc, or NULL.
;;; ----------------------------------------------------------------------------

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
;;;     location to store padding in the X direction, or NULL.
;;; 
;;; ypad :
;;;     location to store padding in the Y direction, or NULL.
;;; ----------------------------------------------------------------------------

;;; --- End of file gtk.misc.lisp ----------------------------------------------
