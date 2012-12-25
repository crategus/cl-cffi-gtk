;;; ----------------------------------------------------------------------------
;;; gtk.alignment.lisp
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
;;; GtkAlignment
;;; 
;;; A widget which controls the alignment and size of its child
;;;     
;;; Synopsis
;;; 
;;;     GtkAlignment
;;;     
;;;     gtk_alignment_new
;;;     gtk_alignment_set
;;;     gtk_alignment_get_padding
;;;     gtk_alignment_set_padding
;;; 
;;; Object Hierarchy
;;; 
;;;   GObject
;;;    +----GInitiallyUnowned
;;;          +----GtkWidget
;;;                +----GtkContainer
;;;                      +----GtkBin
;;;                            +----GtkAlignment
;;; 
;;; Implemented Interfaces
;;; 
;;; GtkAlignment implements AtkImplementorIface and GtkBuildable.
;;; 
;;; Properties
;;; 
;;;   "bottom-padding"           guint                 : Read / Write
;;;   "left-padding"             guint                 : Read / Write
;;;   "right-padding"            guint                 : Read / Write
;;;   "top-padding"              guint                 : Read / Write
;;;   "xalign"                   gfloat                : Read / Write
;;;   "xscale"                   gfloat                : Read / Write
;;;   "yalign"                   gfloat                : Read / Write
;;;   "yscale"                   gfloat                : Read / Write
;;; 
;;; Description
;;; 
;;; The GtkAlignment widget controls the alignment and size of its child widget.
;;; It has four settings: xscale, yscale, xalign, and yalign.
;;; 
;;; The scale settings are used to specify how much the child widget should
;;; expand to fill the space allocated to the GtkAlignment. The values can range
;;; from 0 (meaning the child doesn't expand at all) to 1 (meaning the child
;;; expands to fill all of the available space).
;;; 
;;; The align settings are used to place the child widget within the available
;;; area. The values range from 0 (top or left) to 1 (bottom or right). Of
;;; course, if the scale settings are both set to 1, the alignment settings have
;;; no effect.
;;; 
;;; Note
;;; 
;;; Note that the desired effect can in most cases be achieved by using the
;;; "halign", "valign" and "margin" properties on the child widget, so
;;; GtkAlignment should not be used in new code.
;;; 
;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "bottom-padding" property
;;; 
;;;   "bottom-padding"           guint                 : Read / Write
;;; 
;;; The padding to insert at the bottom of the widget.
;;; 
;;; Allowed values: <= G_MAXINT
;;; 
;;; Default value: 0
;;; 
;;; Since 2.4
;;;
;;; ----------------------------------------------------------------------------
;;; The "left-padding" property
;;; 
;;;   "left-padding"             guint                 : Read / Write
;;; 
;;; The padding to insert at the left of the widget.
;;; 
;;; Allowed values: <= G_MAXINT
;;; 
;;; Default value: 0
;;; 
;;; Since 2.4
;;;
;;; ----------------------------------------------------------------------------
;;; The "right-padding" property
;;; 
;;;   "right-padding"            guint                 : Read / Write
;;; 
;;; The padding to insert at the right of the widget.
;;; 
;;; Allowed values: <= G_MAXINT
;;; 
;;; Default value: 0
;;; 
;;; Since 2.4
;;;
;;; ----------------------------------------------------------------------------
;;; The "top-padding" property
;;; 
;;;   "top-padding"              guint                 : Read / Write
;;; 
;;; The padding to insert at the top of the widget.
;;; 
;;; Allowed values: <= G_MAXINT
;;; 
;;; Default value: 0
;;; 
;;; Since 2.4
;;;
;;; ----------------------------------------------------------------------------
;;; The "xalign" property
;;; 
;;;   "xalign"                   gfloat                : Read / Write
;;; 
;;; Horizontal position of child in available space. 0.0 is left aligned, 1.0 is
;;; right aligned.
;;; 
;;; Allowed values: [0,1]
;;; 
;;; Default value: 0.5
;;;
;;; ----------------------------------------------------------------------------
;;; The "xscale" property
;;; 
;;;   "xscale"                   gfloat                : Read / Write
;;; 
;;; If available horizontal space is bigger than needed for the child, how much
;;; of it to use for the child. 0.0 means none, 1.0 means all.
;;; 
;;; Allowed values: [0,1]
;;; 
;;; Default value: 1
;;;
;;; ----------------------------------------------------------------------------
;;; The "yalign" property
;;; 
;;;   "yalign"                   gfloat                : Read / Write
;;; 
;;; Vertical position of child in available space. 0.0 is top aligned, 1.0 is
;;; bottom aligned.
;;; 
;;; Allowed values: [0,1]
;;; 
;;; Default value: 0.5
;;;
;;; ----------------------------------------------------------------------------
;;; The "yscale" property
;;; 
;;;   "yscale"                   gfloat                : Read / Write
;;; 
;;; If available vertical space is bigger than needed for the child, how much of
;;; it to use for the child. 0.0 means none, 1.0 means all.
;;; 
;;; Allowed values: [0,1]
;;; 
;;; Default value: 1
;;;
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkAlignment
;;; 
;;; struct GtkAlignment;
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkAlignment" gtk-alignment
  (:superclass gtk-bin
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable")
   :type-initializer "gtk_alignment_get_type")
  ((bottom-padding
    gtk-alignment-bottom-padding
    "bottom-padding" "guint" t t)
   (left-padding
    gtk-alignment-left-padding
    "left-padding" "guint" t t)
   (right-padding
    gtk-alignment-right-padding
    "right-padding" "guint" t t)
   (top-padding
    gtk-alignment-top-padding
    "top-padding" "guint" t t)
   (xalign
    gtk-alignment-xalign
    "xalign" "gfloat" t t)
   (xscale
    gtk-alignment-xscale
    "xscale" "gfloat" t t)
   (yalign
    gtk-alignment-yalign
    "yalign" "gfloat" t t)
   (yscale
    gtk-alignment-yscale
    "yscale" "gfloat" t t)))

;;; ----------------------------------------------------------------------------
;;; gtk_alignment_new ()
;;; 
;;; GtkWidget * gtk_alignment_new (gfloat xalign,
;;;                                gfloat yalign,
;;;                                gfloat xscale,
;;;                                gfloat yscale);
;;; 
;;; Creates a new GtkAlignment.
;;; 
;;; xalign :
;;;     the horizontal alignment of the child widget, from 0 (left) to 1
;;;     (right).
;;; 
;;; yalign :
;;;     the vertical alignment of the child widget, from 0 (top) to 1 (bottom).
;;; 
;;; xscale :
;;;     the amount that the child widget expands horizontally to fill up unused
;;;     space, from 0 to 1. A value of 0 indicates that the child widget should
;;;     never expand. A value of 1 indicates that the child widget will expand
;;;     to fill all of the space allocated for the GtkAlignment.
;;; 
;;; yscale :
;;;     the amount that the child widget expands vertically to fill up unused
;;;     space, from 0 to 1. The values are similar to xscale.
;;; 
;;; Returns :
;;;     the new GtkAlignment.
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-alignmnet-new))

(defun gtk-alignment-new (xalign yalign xscale yscale)
  (make-instance 'gtk-alignment
                 :xalign xalign
                 :yalign yalign
                 :xscale xscale
                 :yscale yscale))

(export 'gtk-alignment-new)

;;; ----------------------------------------------------------------------------
;;; gtk_alignment_set ()
;;; 
;;; void gtk_alignment_set (GtkAlignment *alignment,
;;;                         gfloat xalign,
;;;                         gfloat yalign,
;;;                         gfloat xscale,
;;;                         gfloat yscale);
;;; 
;;; Sets the GtkAlignment values.
;;; 
;;; alignment :
;;;     a GtkAlignment.
;;; 
;;; xalign :
;;;     the horizontal alignment of the child widget, from 0 (left) to 1
;;;     (right).
;;; 
;;; yalign :
;;;     the vertical alignment of the child widget, from 0 (top) to 1 (bottom).
;;; 
;;; xscale :
;;;     the amount that the child widget expands horizontally to fill up unused
;;;     space, from 0 to 1. A value of 0 indicates that the child widget should
;;;     never expand. A value of 1 indicates that the child widget will expand
;;;     to fill all of the space allocated for the GtkAlignment.
;;; 
;;; yscale :
;;;     the amount that the child widget expands vertically to fill up unused
;;;     space, from 0 to 1. The values are similar to xscale.
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-alignment-set))

(defun gtk-alignment-set (alignment xalign yalign xscale yscale)
  (setf (gtk-alignment-xalign alignment) xalign
        (gtk-alignment-yalign alignment) yalign
        (gtk-alignment-xscale alignment) xscale
        (gtk-alignment-yscale alignment) yscale))

(export 'gtk-alignment-set)

;;; ----------------------------------------------------------------------------
;;; gtk_alignment_get_padding ()
;;; 
;;; void gtk_alignment_get_padding (GtkAlignment *alignment,
;;;                                 guint *padding_top,
;;;                                 guint *padding_bottom,
;;;                                 guint *padding_left,
;;;                                 guint *padding_right);
;;; 
;;; Gets the padding on the different sides of the widget. See
;;; gtk_alignment_set_padding().
;;; 
;;; alignment :
;;;     a GtkAlignment
;;; 
;;; padding_top :
;;;     location to store the padding for the top of the widget, or NULL
;;; 
;;; padding_bottom :
;;;     location to store the padding for the bottom of the widget, or NULL
;;; 
;;; padding_left :
;;;     location to store the padding for the left of the widget, or NULL
;;; 
;;; padding_right :
;;;     location to store the padding for the right of the widget, or NULL
;;; 
;;; Since 2.4
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-alignment-get-padding))

(defun gtk-alignment-get-padding (alignment)
  (values (gtk-alignment-top-padding alignment)
          (gtk-alignment-bottom-padding alignment)
          (gtk-alignment-left-padding alignment)
          (gtk-alignment-right-padding alignment)))

(export 'gtk-alignment-get-padding)

;;; ----------------------------------------------------------------------------
;;; gtk_alignment_set_padding ()
;;; 
;;; void gtk_alignment_set_padding (GtkAlignment *alignment,
;;;                                 guint padding_top,
;;;                                 guint padding_bottom,
;;;                                 guint padding_left,
;;;                                 guint padding_right);
;;; 
;;; Sets the padding on the different sides of the widget. The padding adds
;;; blank space to the sides of the widget. For instance, this can be used to
;;; indent the child widget towards the right by adding padding on the left.
;;; 
;;; alignment :
;;;     a GtkAlignment
;;; 
;;; padding_top :
;;;     the padding at the top of the widget
;;; 
;;; padding_bottom :
;;;     the padding at the bottom of the widget
;;; 
;;; padding_left :
;;;     the padding at the left of the widget
;;; 
;;; padding_right :
;;;     the padding at the right of the widget.
;;; 
;;; Since 2.4
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-alignment-set-padding))

(defun gtk-alignment-set-padding (alignment top bottom left right)
  (setf (gtk-alignment-top-padding alignment) top
        (gtk-alignment-bottom-padding alignment) bottom
        (gtk-alignment-left-padding alignment) left
        (gtk-alignment-right-padding alignment) right))

(export 'gtk-alignment-set-padding)

;;; --- End of file gtk.alignment.lisp -----------------------------------------
