;;; ----------------------------------------------------------------------------
;;; gtk.frame.lisp
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
;;; 
;;; Object Hierarchy
;;; 
;;;   GObject
;;;    +----GInitiallyUnowned
;;;          +----GtkWidget
;;;                +----GtkContainer
;;;                      +----GtkBin
;;;                            +----GtkFrame
;;;                                  +----GtkAspectFrame
;;; 
;;; Implemented Interfaces
;;; 
;;; GtkFrame implements AtkImplementorIface and GtkBuildable.
;;; 
;;; Properties
;;; 
;;;   "label"                    gchar*                : Read / Write
;;;   "label-widget"             GtkWidget*            : Read / Write
;;;   "label-xalign"             gfloat                : Read / Write
;;;   "label-yalign"             gfloat                : Read / Write
;;;   "shadow-type"              GtkShadowType         : Read / Write
;;; 
;;; Description
;;; 
;;; The frame widget is a Bin that surrounds its child with a decorative frame
;;; and an optional label. If present, the label is drawn in a gap in the top
;;; side of the frame. The position of the label can be controlled with
;;; gtk_frame_set_label_align().
;;; 
;;; GtkFrame as GtkBuildable
;;; 
;;; The GtkFrame implementation of the GtkBuildable interface supports placing a
;;; child in the label position by specifying "label" as the "type" attribute of
;;; a <child> element. A normal content child can be specified without
;;; specifying a <child> type attribute.
;;; 
;;; Example 94. A UI definition fragment with GtkFrame
;;; 
;;; <object class="GtkFrame">
;;;   <child type="label">
;;;     <object class="GtkLabel" id="frame-label"/>
;;;   </child>
;;;   <child>
;;;     <object class="GtkEntry" id="frame-content"/>
;;;   </child>
;;; </object>
;;; 
;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "label" property
;;; 
;;;   "label"                    gchar*                : Read / Write
;;; 
;;; Text of the frame's label.
;;; 
;;; Default value: NULL
;;;
;;; ----------------------------------------------------------------------------
;;; The "label-widget" property
;;; 
;;;   "label-widget"             GtkWidget*            : Read / Write
;;; 
;;; A widget to display in place of the usual frame label.
;;;
;;; ----------------------------------------------------------------------------
;;; The "label-xalign" property
;;; 
;;;   "label-xalign"             gfloat                : Read / Write
;;; 
;;; The horizontal alignment of the label.
;;; 
;;; Allowed values: [0,1]
;;; 
;;; Default value: 0
;;;
;;; ----------------------------------------------------------------------------
;;; The "label-yalign" property
;;; 
;;;   "label-yalign"             gfloat                : Read / Write
;;; 
;;; The vertical alignment of the label.
;;; 
;;; Allowed values: [0,1]
;;; 
;;; Default value: 0.5
;;;
;;; ----------------------------------------------------------------------------
;;; The "shadow-type" property
;;; 
;;;   "shadow-type"              GtkShadowType         : Read / Write
;;; 
;;; Appearance of the frame border.
;;; 
;;; Default value: GTK_SHADOW_ETCHED_IN
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkFrame
;;; 
;;; struct GtkFrame;
;;; ----------------------------------------------------------------------------

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

;;; ----------------------------------------------------------------------------
;;; gtk_frame_new ()
;;; 
;;; GtkWidget * gtk_frame_new (const gchar *label);
;;; 
;;; Creates a new GtkFrame, with optional label label. If label is NULL, the
;;; label is omitted.
;;; 
;;; label :
;;;     the text to use as the label of the frame
;;; 
;;; Returns :
;;;     a new GtkFrame widget
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-frame-new))

(defun gtk-frame-new (label)
  (make-instance 'gtk-frame :label label))

(export 'gtk-frame-new)

;;; ----------------------------------------------------------------------------
;;; gtk_frame_set_label ()
;;; 
;;; void gtk_frame_set_label (GtkFrame *frame, const gchar *label);
;;; 
;;; Sets the text of the label. If label is NULL, the current label is removed.
;;; 
;;; frame :
;;;     a GtkFrame
;;; 
;;; label :
;;;     the text to use as the label of the frame
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-frame-set-label))

(defun gtk-frame-set-label (frame label)
  (setf (gtk-frame-label frame) label))

(export 'gtk-frame-set-label)

;;; ----------------------------------------------------------------------------
;;; gtk_frame_set_label_widget ()
;;; 
;;; void gtk_frame_set_label_widget (GtkFrame *frame, GtkWidget *label_widget);
;;; 
;;; Sets the label widget for the frame. This is the widget that will appear
;;; embedded in the top edge of the frame as a title.
;;; 
;;; frame :
;;;     a GtkFrame
;;; 
;;; label_widget :
;;;     the new label widget
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-frame-set-label-widget))

(defun gtk-frame-set-label-widget (frame label-widget)
  (setf (gtk-frame-label-widget frame) label-widget))

(export 'gtk-frame-set-label-widget)

;;; ----------------------------------------------------------------------------
;;; gtk_frame_set_label_align ()
;;; 
;;; void gtk_frame_set_label_align (GtkFrame *frame,
;;;                                 gfloat xalign,
;;;                                 gfloat yalign);
;;; 
;;; Sets the alignment of the frame widget's label. The default values for a
;;; newly created frame are 0.0 and 0.5.
;;; 
;;; frame :
;;;     a GtkFrame
;;; 
;;; xalign :
;;;     The position of the label along the top edge of the widget. A value of
;;;     0.0 represents left alignment; 1.0 represents right alignment.
;;; 
;;; yalign :
;;;     The y alignment of the label. A value of 0.0 aligns under the frame; 1.0
;;;     aligns above the frame. If the values are exactly 0.0 or 1.0 the gap in
;;;     the frame won't be painted because the label will be completely above or
;;;     below the frame.
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-frame-set-label-align))

(defun gtk-frame-set-label-align (frame xalign yalign)
  (setf (gtk-frame-label-xalign frame) xalign
        (gtk-frame-label-yalign frame) yalign))

(export 'gtk-frame-set-label-align)

;;; ----------------------------------------------------------------------------
;;; gtk_frame_set_shadow_type ()
;;; 
;;; void gtk_frame_set_shadow_type (GtkFrame *frame, GtkShadowType type);
;;; 
;;; Sets the shadow type for frame.
;;; 
;;; frame :
;;;     a GtkFrame
;;; 
;;; type :
;;;     the new GtkShadowType
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-frame-set-shadow-type))

(defun gtk-frame-set-shadow-type (frame type)
  (setf (gtk-frame-shadow-type frame) type))

(export 'gtk-frame-set-shadow-type)

;;; ----------------------------------------------------------------------------
;;; gtk_frame_get_label ()
;;; 
;;; const gchar * gtk_frame_get_label (GtkFrame *frame);
;;; 
;;; If the frame's label widget is a GtkLabel, returns the text in the label
;;; widget. (The frame will have a GtkLabel for the label widget if a non-NULL
;;; argument was passed to gtk_frame_new().)
;;; 
;;; frame :
;;;     a GtkFrame
;;; 
;;; Returns :
;;;     the text in the label, or NULL if there was no label widget or the label
;;;     widget was not a GtkLabel. This string is owned by GTK+ and must not be
;;;     modified or freed.
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-frame-get-label))

(defun gtk-frame-get-label (frame)
  (gtk-frame-label frame))

(export 'gtk-frame-get-label)

;;; ----------------------------------------------------------------------------
;;; gtk_frame_get_label_align ()
;;; 
;;; void gtk_frame_get_label_align (GtkFrame *frame,
;;;                                 gfloat *xalign,
;;;                                 gfloat *yalign);
;;; 
;;; Retrieves the X and Y alignment of the frame's label. See
;;; gtk_frame_set_label_align().
;;; 
;;; frame :
;;;     a GtkFrame
;;; 
;;; xalign :
;;;     location to store X alignment of frame's label, or NULL
;;; 
;;; yalign :
;;;     location to store X alignment of frame's label, or NULL
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-frame-get-label-align))

(defun gtk-frame-get-label-align (frame)
  (values (gtk-frame-label-xalign frame)
          (gtk-frame-label-yalign frame)))

(export 'gtk-frame-get-label-align)

;;; ----------------------------------------------------------------------------
;;; gtk_frame_get_label_widget ()
;;; 
;;; GtkWidget * gtk_frame_get_label_widget (GtkFrame *frame);
;;; 
;;; Retrieves the label widget for the frame. See gtk_frame_set_label_widget().
;;; 
;;; frame :
;;;     a GtkFrame
;;; 
;;; Returns :
;;;     the label widget, or NULL if there is none
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-frame-get-label-widget))

(defun gtk-frame-get-label-widget (frame)
  (gtk-frame-label-widget frame))

(export 'gtk-frame-get-label-widget)

;;; ----------------------------------------------------------------------------
;;; gtk_frame_get_shadow_type ()
;;; 
;;; GtkShadowType gtk_frame_get_shadow_type (GtkFrame *frame);
;;; 
;;; Retrieves the shadow type of the frame. See gtk_frame_set_shadow_type().
;;; 
;;; frame :
;;;     a GtkFrame
;;; 
;;; Returns :
;;;     the current shadow type of the frame.
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-frame-get-shadow-type))

(defun gtk-frame-get-shadow-type (frame)
  (gtk-frame-get-shadow-type frame))

(export 'gtk-framge-get-shadow-type)

;;; --- End of file gtk.frame.lisp ---------------------------------------------
