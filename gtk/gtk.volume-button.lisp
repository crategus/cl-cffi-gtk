;;; ----------------------------------------------------------------------------
;;; gtk.volume-button.lisp
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
;;; GtkVolumeButton
;;; 
;;; A button which pops up a volume control
;;; 
;;; Synopsis
;;; 
;;;     GtkVolumeButton;
;;;     gtk_volume_button_new
;;; 
;;; Object Hierarchy
;;; 
;;;   GObject
;;;    +----GInitiallyUnowned
;;;          +----GtkWidget
;;;                +----GtkContainer
;;;                      +----GtkBin
;;;                            +----GtkButton
;;;                                  +----GtkScaleButton
;;;                                        +----GtkVolumeButton
;;; 
;;; Implemented Interfaces
;;; 
;;; GtkVolumeButton implements AtkImplementorIface, GtkBuildable,
;;;  GtkActivatable and GtkOrientable.
;;;
;;; Properties
;;; 
;;;   "use-symbolic"             gboolean              : Read / Write
;;; 
;;; Description
;;; 
;;; GtkVolumeButton is a subclass of GtkScaleButton that has been tailored for
;;; use as a volume control widget with suitable icons, tooltips and accessible
;;; labels.
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "use-symbolic" property
;;; 
;;;   "use-symbolic"             gboolean              : Read / Write
;;; 
;;; Whether to use symbolic icons as the icons. Note that if the symbolic icons
;;; are not available in your installed theme, then the normal (potentially
;;; colorful) icons will be used.
;;; 
;;; Default value: FALSE
;;; 
;;; Since 3.0
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkVolumeButton
;;; 
;;; struct GtkVolumeButton;
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkVolumeButton" volume-button
  (:superclass gtk-scale-button
   :export t
   :interfaces  ("AtkImplementorIface" "GtkActivatable" "GtkBuildable"
                  "GtkOrientable")
   :type-initializer "gtk_volume_button_get_type")
  nil)

;;; ----------------------------------------------------------------------------
;;; gtk_volume_button_new ()
;;; 
;;; GtkWidget * gtk_volume_button_new (void);
;;; 
;;; Creates a GtkVolumeButton, with a range between 0.0 and 1.0, with a
;;; stepping of 0.02. Volume values can be obtained and modified using the
;;; functions from GtkScaleButton.
;;; 
;;; Returns :
;;;     a new GtkVolumeButton
;;; 
;;; Since 2.12
;;; ----------------------------------------------------------------------------


;;; --- End of file gtk.volume-button.lisp -------------------------------------
