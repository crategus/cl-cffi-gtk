;;; ----------------------------------------------------------------------------
;;; gtk.orientable.lisp
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
;;; GtkOrientable
;;; 
;;; An interface for flippable widgets
;;; 
;;; Synopsis
;;; 
;;;    GtkOrientable
;;;
;;;    gtk_orientable_get_orientation
;;;    gtk_orientable_set_orientation
;;; 
;;; Object Hierarchy
;;; 
;;;   GInterface
;;;    +----GtkOrientable
;;; 
;;; Prerequisites
;;; 
;;; GtkOrientable requires GObject.
;;;
;;; Known Implementations
;;; 
;;; GtkOrientable is implemented by GtkAppChooserWidget, GtkBox, GtkButtonBox,
;;; GtkCellAreaBox, GtkCellRendererProgress, GtkCellView, GtkColorSelection,
;;; GtkFileChooserButton, GtkFileChooserWidget, GtkFontChooserWidget,
;;; GtkFontSelection, GtkGrid, GtkHBox, GtkHButtonBox, GtkHPaned, GtkHScale,
;;; GtkHScrollbar, GtkHSeparator, GtkInfoBar, GtkPaned, GtkProgressBar,
;;; GtkRange, GtkRecentChooserWidget, GtkScale, GtkScaleButton, GtkScrollbar,
;;; GtkSeparator, GtkStatusbar, GtkToolPalette, GtkToolbar, GtkVBox,
;;; GtkVButtonBox, GtkVPaned, GtkVScale, GtkVScrollbar, GtkVSeparator and
;;; GtkVolumeButton.
;;;
;;; Properties
;;; 
;;;   "orientation"              GtkOrientation        : Read / Write
;;; 
;;; Description
;;; 
;;; The GtkOrientable interface is implemented by all widgets that can be
;;; oriented horizontally or vertically. Historically, such widgets have been
;;; realized as subclasses of a common base class (e.g GtkBox/GtkHBox/GtkVBox
;;; or GtkScale/GtkHScale/GtkVScale). GtkOrientable is more flexible in that it
;;; allows the orientation to be changed at runtime, allowing the widgets to
;;; 'flip'.
;;; 
;;; GtkOrientable was introduced in GTK+ 2.16.
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "orientation" property
;;; 
;;;   "orientation"              GtkOrientation        : Read / Write
;;; 
;;; The orientation of the orientable.
;;; 
;;; Default value: GTK_ORIENTATION_HORIZONTAL
;;; 
;;; Since 2.16
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkOrientable
;;; 
;;; typedef struct _GtkOrientable GtkOrientable;
;;; ----------------------------------------------------------------------------

(define-g-interface "GtkOrientable" gtk-orientable
  (:export t
   :type-initializer "gtk_orientable_get_type")
  (orientation
   gtk-orientable-orientation
   "orientation" "GtkOrientation" t t))

;;; ----------------------------------------------------------------------------
;;; gtk_orientable_get_orientation ()
;;; 
;;; GtkOrientation gtk_orientable_get_orientation (GtkOrientable *orientable);
;;; 
;;; Retrieves the orientation of the orientable.
;;; 
;;; orientable :
;;;     a GtkOrientable
;;; 
;;; Returns :
;;;     the orientation of the orientable
;;; 
;;; Since 2.16
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-orientable-get-orientation))

(defun gtk-orientable-get-orientation (orientable)
  (gtk-orientable-orientation orientable))

(export 'gtk-orientable-get-orientation)

;;; ----------------------------------------------------------------------------
;;; gtk_orientable_set_orientation ()
;;; 
;;; void gtk_orientable_set_orientation (GtkOrientable *orientable,
;;;                                      GtkOrientation orientation);
;;; 
;;; Sets the orientation of the orientable.
;;; 
;;; orientable :
;;;     a GtkOrientable
;;; 
;;; orientation :
;;;     the orientable's new orientation
;;; 
;;; Since 2.16
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-orientable-set-orientation))

(defun gtk-orientable-set-orientation (orientable orientation)
  (setf (gtk-orientable-orientation orientable) orientation))

(export 'gtk-orientable-set-orientation)

;;; --- End of file gtk.orientable.lisp ----------------------------------------
