;;; ----------------------------------------------------------------------------
;;; gtk.cell-renderer-toggle.lisp
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
;;; GtkCellRendererToggle
;;; 
;;; Renders a toggle button in a cell
;;; 
;;; Synopsis
;;; 
;;;     GtkCellRendererToggle
;;;
;;;     gtk_cell_renderer_toggle_new
;;;     gtk_cell_renderer_toggle_get_radio
;;;     gtk_cell_renderer_toggle_set_radio
;;;     gtk_cell_renderer_toggle_get_active
;;;     gtk_cell_renderer_toggle_set_active
;;;     gtk_cell_renderer_toggle_get_activatable
;;;     gtk_cell_renderer_toggle_set_activatable
;;; 
;;; Object Hierarchy
;;; 
;;;   GObject
;;;    +----GInitiallyUnowned
;;;          +----GtkCellRenderer
;;;                +----GtkCellRendererToggle
;;; 
;;; Properties
;;; 
;;;   "activatable"              gboolean              : Read / Write
;;;   "active"                   gboolean              : Read / Write
;;;   "inconsistent"             gboolean              : Read / Write
;;;   "indicator-size"           gint                  : Read / Write
;;;   "radio"                    gboolean              : Read / Write
;;; 
;;; Signals
;;; 
;;;   "toggled"                                        : Run Last
;;; 
;;; Description
;;; 
;;; GtkCellRendererToggle renders a toggle button in a cell. The button is
;;; drawn as a radio or a checkbutton, depending on the "radio" property. When
;;; activated, it emits the "toggled" signal.
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "activatable" property
;;; 
;;;   "activatable"              gboolean              : Read / Write
;;; 
;;; The toggle button can be activated.
;;; 
;;; Default value: TRUE
;;;
;;; ----------------------------------------------------------------------------
;;; The "active" property
;;; 
;;;   "active"                   gboolean              : Read / Write
;;; 
;;; The toggle state of the button.
;;; 
;;; Default value: FALSE
;;;
;;; ----------------------------------------------------------------------------
;;; The "inconsistent" property
;;; 
;;;   "inconsistent"             gboolean              : Read / Write
;;; 
;;; The inconsistent state of the button.
;;; 
;;; Default value: FALSE
;;;
;;; ----------------------------------------------------------------------------
;;; The "indicator-size" property
;;; 
;;;   "indicator-size"           gint                  : Read / Write
;;; 
;;; Size of check or radio indicator.
;;; 
;;; Allowed values: >= 0
;;; 
;;; Default value: 16
;;;
;;; ----------------------------------------------------------------------------
;;; The "radio" property
;;; 
;;;   "radio"                    gboolean              : Read / Write
;;; 
;;; Draw the toggle button as a radio button.
;;; 
;;; Default value: FALSE
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Signal Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "toggled" signal
;;; 
;;; void user_function (GtkCellRendererToggle *cell_renderer,
;;;                     gchar                 *path,
;;;                     gpointer               user_data)          : Run Last
;;; 
;;; The ::toggled signal is emitted when the cell is toggled.
;;; 
;;; cell_renderer :
;;;     the object which received the signal
;;; 
;;; path :
;;;     string representation of GtkTreePath describing the event location
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkCellRendererToggle
;;; 
;;; struct GtkCellRendererToggle;
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkCellRendererToggle" gtk-cell-renderer-toggle
  (:superclass gtk-cell-renderer
    :export t
    :interfaces nil
    :type-initializer "gtk_cell_renderer_toggle_get_type")
  ((activatable
    gtk-cell-renderer-toggle-activatable
    "activatable" "gboolean" t t)
   (active
    gtk-cell-renderer-toggle-active
    "active" "gboolean" t t)
   (inconsistent
    gtk-cell-renderer-toggle-inconsistent
    "inconsistent" "gboolean" t t)
   (indicator-size
     gtk-cell-renderer-toggle-indicator-size
     "indicator-size" "gint" t t)
   (radio
    gtk-cell-renderer-toggle-radio
    "radio" "gboolean" t t)))

;;; ----------------------------------------------------------------------------
;;; gtk_cell_renderer_toggle_new ()
;;; 
;;; GtkCellRenderer * gtk_cell_renderer_toggle_new (void);
;;; 
;;; Creates a new GtkCellRendererToggle. Adjust rendering parameters using
;;; object properties. Object properties can be set globally (with
;;; g_object_set()). Also, with GtkTreeViewColumn, you can bind a property to a
;;; value in a GtkTreeModel. For example, you can bind the "active" property on
;;; the cell renderer to a boolean value in the model, thus causing the check
;;; button to reflect the state of the model.
;;; 
;;; Returns :
;;;     the new cell renderer
;;; ----------------------------------------------------------------------------

(defun gtk-cell-renderer-toggle-new ()
  (make-instance 'gtk-cell-renderer-toggle))

(export 'gtk-cell-renderer-toggle-new)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_renderer_toggle_get_radio ()
;;; 
;;; gboolean gtk_cell_renderer_toggle_get_radio (GtkCellRendererToggle *toggle)
;;; 
;;; Returns whether we're rendering radio toggles rather than checkboxes.
;;; 
;;; toggle :
;;;     a GtkCellRendererToggle
;;; 
;;; Returns :
;;;     TRUE if we're rendering radio toggles rather than checkboxes
;;; ----------------------------------------------------------------------------

(defun gtk-cell-renderer-toggle-get-radio (toggle)
  (gtk-cell-renderer-toggle-radio toggle))

(export 'gtk-cell-renderer-toggle-get-radio)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_renderer_toggle_set_radio ()
;;; 
;;; void gtk_cell_renderer_toggle_set_radio (GtkCellRendererToggle *toggle,
;;;                                          gboolean radio);
;;; 
;;; If radio is TRUE, the cell renderer renders a radio toggle (i.e. a toggle
;;; in a group of mutually-exclusive toggles). If FALSE, it renders a check
;;; toggle (a standalone boolean option). This can be set globally for the cell
;;; renderer, or changed just before rendering each cell in the model (for
;;; GtkTreeView, you set up a per-row setting using GtkTreeViewColumn to
;;; associate model columns with cell renderer properties).
;;; 
;;; toggle :
;;;     a GtkCellRendererToggle
;;; 
;;; radio :
;;;     TRUE to make the toggle look like a radio button
;;; ----------------------------------------------------------------------------

(defun gtk-cell-renderer-toggle-set-radio (toggle radio)
  (setf (gtk-cell-renderer-toggle-radio toggle) radio))

(export 'gtk-cell-renderer-toggle-set-radio)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_renderer_toggle_get_active ()
;;; 
;;; gboolean gtk_cell_renderer_toggle_get_active (GtkCellRendererToggle *toggle)
;;; 
;;; Returns whether the cell renderer is active.
;;; See gtk_cell_renderer_toggle_set_active().
;;; 
;;; toggle :
;;;     a GtkCellRendererToggle
;;; 
;;; Returns :
;;;     TRUE if the cell renderer is active.
;;; ----------------------------------------------------------------------------

(defun gtk-cell-renderer-toggle-get-active (toggle)
  (gtk-cell-renderer-toggle-active toggle))

(export 'gtk-cell-renderer-toggle-get-active)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_renderer_toggle_set_active ()
;;; 
;;; void gtk_cell_renderer_toggle_set_active (GtkCellRendererToggle *toggle,
;;;                                           gboolean setting);
;;; 
;;; Activates or deactivates a cell renderer.
;;; 
;;; toggle :
;;;     a GtkCellRendererToggle.
;;; 
;;; setting :
;;;     the value to set.
;;; ----------------------------------------------------------------------------

(defun gtk-cell-renderer-toggle-set-active (toggle setting)
  (setf (gtk-cell-renderer-toggle-active toggle) setting))

(export 'gtk-cell-renderer-toggle-set-active)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_renderer_toggle_get_activatable ()
;;; 
;;; gboolean gtk_cell_renderer_toggle_get_activatable
;;;                                              (GtkCellRendererToggle *toggle)
;;; 
;;; Returns whether the cell renderer is activatable.
;;; See gtk_cell_renderer_toggle_set_activatable().
;;; 
;;; toggle :
;;;     a GtkCellRendererToggle
;;; 
;;; Returns :
;;;     TRUE if the cell renderer is activatable.
;;; 
;;; Since 2.18
;;; ----------------------------------------------------------------------------

(defun gtk-cell-renderer-toggle-get-activatable (toggle)
  (gtk-cell-renderer-toggle-activatable toggle))

(export 'gtk-cell-renderer-toggle-get-activatable)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_renderer_toggle_set_activatable ()
;;; 
;;; void gtk_cell_renderer_toggle_set_activatable
;;;                                              (GtkCellRendererToggle *toggle,
;;;                                               gboolean setting);
;;; 
;;; Makes the cell renderer activatable.
;;; 
;;; toggle :
;;;     a GtkCellRendererToggle.
;;; 
;;; setting :
;;;     the value to set.
;;; 
;;; Since 2.18
;;; ----------------------------------------------------------------------------

(defun gtk-cell-renderer-toggle-set-activatable (toggle setting)
  (setf (gtk-cell-renderer-toggle-activatable toggle) setting))

(export 'gtk-cell-renderer-toggle-set-activatable)

;;; --- End of file gtk.cell-renderer-toggle.lisp ------------------------------
