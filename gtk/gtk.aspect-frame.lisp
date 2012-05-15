;;; ----------------------------------------------------------------------------
;;; gtk.aspect-frame.lisp
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
;;; GtkAspectFrame
;;; 
;;; A frame that constrains its child to a particular aspect ratio
;;;     
;;; Synopsis
;;; 
;;;     GtkAspectFrame
;;;     
;;;     gtk_aspect_frame_new
;;;     gtk_aspect_frame_set
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
;;; GtkAspectFrame implements AtkImplementorIface and GtkBuildable.
;;;
;;; Properties
;;; 
;;;   "obey-child"               gboolean              : Read / Write
;;;   "ratio"                    gfloat                : Read / Write
;;;   "xalign"                   gfloat                : Read / Write
;;;   "yalign"                   gfloat                : Read / Write
;;; 
;;; Description
;;; 
;;; The GtkAspectFrame is useful when you want pack a widget so that it can
;;; resize but always retains the same aspect ratio. For instance, one might be
;;; drawing a small preview of a larger image. GtkAspectFrame derives from
;;; GtkFrame, so it can draw a label and a frame around the child. The frame
;;; will be "shrink-wrapped" to the size of the child.
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "obey-child" property
;;; 
;;;   "obey-child"               gboolean              : Read / Write
;;; 
;;; Force aspect ratio to match that of the frame's child.
;;; 
;;; Default value: TRUE
;;;
;;; ----------------------------------------------------------------------------
;;; The "ratio" property
;;; 
;;;   "ratio"                    gfloat                : Read / Write
;;; 
;;; Aspect ratio if obey_child is FALSE.
;;; 
;;; Allowed values: [0.0001,10000]
;;; 
;;; Default value: 1
;;;
;;; ----------------------------------------------------------------------------
;;; The "xalign" property
;;; 
;;;   "xalign"                   gfloat                : Read / Write
;;; 
;;; X alignment of the child.
;;; 
;;; Allowed values: [0,1]
;;; 
;;; Default value: 0.5
;;;
;;; ----------------------------------------------------------------------------
;;; The "yalign" property
;;; 
;;;   "yalign"                   gfloat                : Read / Write
;;; 
;;; Y alignment of the child.
;;; 
;;; Allowed values: [0,1]
;;; 
;;; Default value: 0.5
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkAspectFrame
;;; 
;;; struct GtkAspectFrame;
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkAspectFrame" gtk-aspect-frame
  (:superclass gtk-frame
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable")
   :type-initializer "gtk_aspect_frame_get_type")
  ((obey-child
    gtk-aspect-frame-obey-child
    "obey-child" "gboolean" t t)
   (ratio
    gtk-aspect-frame-ratio
    "ratio" "gfloat" t t)
   (xalign
    gtk-aspect-frame-xalign
    "xalign" "gfloat" t t)
   (yalign
    gtk-aspect-frame-yalign
    "yalign" "gfloat" t t)))

;;; ----------------------------------------------------------------------------
;;; gtk_aspect_frame_new ()
;;; 
;;; GtkWidget * gtk_aspect_frame_new (const gchar *label,
;;;                                   gfloat xalign,
;;;                                   gfloat yalign,
;;;                                   gfloat ratio,
;;;                                   gboolean obey_child);
;;; 
;;; Create a new GtkAspectFrame.
;;; 
;;; label :
;;;     Label text.
;;; 
;;; xalign :
;;;     Horizontal alignment of the child within the allocation of the
;;;     GtkAspectFrame. This ranges from 0.0 (left aligned) to 1.0 (right
;;;     aligned)
;;; 
;;; yalign :
;;;     Vertical alignment of the child within the allocation of the
;;;     GtkAspectFrame. This ranges from 0.0 (left aligned) to 1.0 (right
;;;     aligned)
;;; 
;;; ratio :
;;;     The desired aspect ratio.
;;; 
;;; obey_child :
;;;     If TRUE, ratio is ignored, and the aspect ratio is taken from the
;;;     requistion of the child.
;;; 
;;; Returns :
;;;     the new GtkAspectFrame.
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-aspect-frame-new))

(defun gkt-aspect-frame-new (label xalign yalign ratio obey-child)
  (make-instance 'gtk-aspect-frame
                 :label label
                 :xalign xalign
                 :yalign yalign
                 :ratio ratio
                 :obey-child obey-child))

(export 'gtk-aspect-frame-new)

;;; ----------------------------------------------------------------------------
;;; gtk_aspect_frame_set ()
;;; 
;;; void gtk_aspect_frame_set (GtkAspectFrame *aspect_frame,
;;;                            gfloat xalign,
;;;                            gfloat yalign,
;;;                            gfloat ratio,
;;;                            gboolean obey_child);
;;; 
;;; Set parameters for an existing GtkAspectFrame.
;;; 
;;; aspect_frame :
;;;     a GtkAspectFrame
;;; 
;;; xalign :
;;;     Horizontal alignment of the child within the allocation of the
;;;     GtkAspectFrame. This ranges from 0.0 (left aligned) to 1.0 (right
;;;     aligned)
;;; 
;;; yalign :
;;;     Vertical alignment of the child within the allocation of the
;;;     GtkAspectFrame. This ranges from 0.0 (left aligned) to 1.0 (right
;;;     aligned)
;;; 
;;; ratio :
;;;     The desired aspect ratio.
;;; 
;;; obey_child :
;;;     If TRUE, ratio is ignored, and the aspect ratio is taken from the
;;;     requistion of the child.
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-aspect-frame-set))

(defun gtk-aspect-frame-set (aspect-frame xalign yalign ratio obey-child)
  (setf (gtk-aspect-frame-xalign aspect-frame) xalign
        (gtk-aspect-frame-yalign aspect-frame) yalign
        (gtk-aspect-frame-ratio aspect-frame) ratio
        (gtk-aspect-frame-obey-child aspect-frame) obey-child))

(export 'gtk-aspect-frame-set)

;;; --- End of file gtk.aspect-frame.lisp --------------------------------------
