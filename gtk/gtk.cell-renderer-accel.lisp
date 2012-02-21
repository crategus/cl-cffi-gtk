;;; ----------------------------------------------------------------------------
;;; gtk.cell-renderer-accel.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See http://common-lisp.net/project/cl-gtk2/
;;;
;;; The documentation has been copied from the GTK+ 3 Reference Manual
;;; Version 3.2.3. See http://www.gtk.org.
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
;;; GtkCellRendererAccel
;;; 
;;; Renders a keyboard accelerator in a cell
;;; 
;;; Synopsis
;;; 
;;;     GtkCellRendererAccel
;;;     GtkCellRendererAccelMode
;;;
;;;     gtk_cell_renderer_accel_new
;;; 
;;; Object Hierarchy
;;; 
;;;   GObject
;;;    +----GInitiallyUnowned
;;;          +----GtkCellRenderer
;;;                +----GtkCellRendererText
;;;                      +----GtkCellRendererAccel
;;; 
;;; Properties
;;; 
;;;   "accel-key"                guint                     : Read / Write
;;;   "accel-mode"               GtkCellRendererAccelMode  : Read / Write
;;;   "accel-mods"               GdkModifierType           : Read / Write
;;;   "keycode"                  guint                     : Read / Write
;;; 
;;; Signals
;;; 
;;;   "accel-cleared"                                      : Run Last
;;;   "accel-edited"                                       : Run Last
;;; 
;;; Description
;;; 
;;; GtkCellRendererAccel displays a keyboard accelerator (i.e. a key combination
;;; like Control+a. If the cell renderer is editable, the accelerator can be
;;; changed by simply typing the new combination.
;;; 
;;; The GtkCellRendererAccel cell renderer was added in GTK+ 2.10.
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "accel-key" property
;;; 
;;;   "accel-key"                guint                 : Read / Write
;;; 
;;; The keyval of the accelerator.
;;; 
;;; Allowed values: <= G_MAXINT
;;; 
;;; Default value: 0
;;; 
;;; Since 2.10
;;;
;;; ----------------------------------------------------------------------------
;;; The "accel-mode" property
;;; 
;;;   "accel-mode"               GtkCellRendererAccelMode  : Read / Write
;;; 
;;; Determines if the edited accelerators are GTK+ accelerators. If they are,
;;; consumed modifiers are suppressed, only accelerators accepted by GTK+ are
;;; allowed, and the accelerators are rendered in the same way as they are in
;;; menus.
;;; 
;;; Default value: GTK_CELL_RENDERER_ACCEL_MODE_GTK
;;; 
;;; Since 2.10
;;;
;;; ----------------------------------------------------------------------------
;;; The "accel-mods" property
;;; 
;;;   "accel-mods"               GdkModifierType       : Read / Write
;;; 
;;; The modifier mask of the accelerator.
;;; 
;;; Since 2.10
;;;
;;; ----------------------------------------------------------------------------
;;; The "keycode" property
;;; 
;;;   "keycode"                  guint                 : Read / Write
;;; 
;;; The hardware keycode of the accelerator. Note that the hardware keycode is
;;; only relevant if the key does not have a keyval. Normally, the keyboard
;;; configuration should assign keyvals to all keys.
;;; 
;;; Allowed values: <= G_MAXINT
;;; 
;;; Default value: 0
;;; 
;;; Since 2.10
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Signal Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "accel-cleared" signal
;;; 
;;; void user_function (GtkCellRendererAccel *accel,
;;;                     gchar                *path_string,
;;;                     gpointer              user_data)        : Run Last
;;; 
;;; Gets emitted when the user has removed the accelerator.
;;; 
;;; accel :
;;;     the object reveiving the signal
;;; 
;;; path_string :
;;;     the path identifying the row of the edited cell
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected.
;;; 
;;; Since 2.10
;;;
;;; ----------------------------------------------------------------------------
;;; The "accel-edited" signal
;;; 
;;; void user_function (GtkCellRendererAccel *accel,
;;;                     gchar                *path_string,
;;;                     guint                 accel_key,
;;;                     GdkModifierType       accel_mods,
;;;                     guint                 hardware_keycode,
;;;                     gpointer              user_data)             : Run Last
;;; 
;;; Gets emitted when the user has selected a new accelerator.
;;; 
;;; accel :
;;;     the object reveiving the signal
;;; 
;;; path_string :
;;;     the path identifying the row of the edited cell
;;; 
;;; accel_key :
;;;     the new accelerator keyval
;;; 
;;; accel_mods :
;;;     the new acclerator modifier mask
;;; 
;;; hardware_keycode :
;;;     the keycode of the new accelerator
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected.
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkCellRendererAccel
;;; 
;;; struct GtkCellRendererAccel;
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkCellRendererAccel" gtk-cell-renderer-accel
  (:superclass gtk-cell-renderer-text
   :export t
   :interfaces nil
   :type-initializer "gtk_cell_renderer_accel_get_type")
  ((accel-key
    gtk-cell-renderer-accel-accel-key
    "accel-key" "guint" t t)
   (accel-mode
    gtk-cell-renderer-accel-accel-mode
    "accel-mode" "GtkCellRendererAccelMode" t t)
   (accel-mods
    gtk-cell-renderer-accel-accel-mods
    "accel-mods" "GdkModifierType" t t)
   (keycode
    gtk-cell-renderer-accel-keycode
    "keycode" "guint" t t)))

;;; ----------------------------------------------------------------------------
;;; enum GtkCellRendererAccelMode
;;; 
;;; typedef enum {
;;;   GTK_CELL_RENDERER_ACCEL_MODE_GTK,
;;;   GTK_CELL_RENDERER_ACCEL_MODE_OTHER
;;; } GtkCellRendererAccelMode;
;;; ----------------------------------------------------------------------------

(define-g-enum "GtkCellRendererAccelMode" gtk-cell-renderer-accel-mode
  (:export t
   :type-initializer "gtk_cell_renderer_accel_mode_get_type")
  (:gtk 0)
  (:other 1))

;;; ----------------------------------------------------------------------------
;;; gtk_cell_renderer_accel_new ()
;;; 
;;; GtkCellRenderer * gtk_cell_renderer_accel_new (void);
;;; 
;;; Creates a new GtkCellRendererAccel.
;;; 
;;; Returns :
;;;     the new cell renderer
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

(defun gtk-cell-renderer-accel-new ()
  (make-instance 'gtk-cell-renderer-accel))

(export 'gtk-cell-renderer-accel-new)

;;; --- End of file gtk.cell-renderer-accel.lisp -------------------------------
