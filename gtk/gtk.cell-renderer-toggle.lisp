;;; ----------------------------------------------------------------------------
;;; gtk.cell-renderer-toggle.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See <http://common-lisp.net/project/cl-gtk2/>.
;;;
;;; The documentation has been copied from the GTK+ 3 Reference Manual
;;; Version 3.4.3. See >http://www.gtk.org>. The API documentation of the
;;; Lisp Binding is available at <http://www.crategus.com/books/cl-cffi-gtk/>.
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
;;;
;;; Signals
;;;
;;;   "toggled"                                        : Run Last
;;;
;;;


(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkCellRendererToggle
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

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-cell-renderer-toggle 'type)
 "@version{2013-2-23}
  @begin{short}
    GtkCellRendererToggle renders a toggle button in a cell.
  @end{short}
  The button is drawn as a radio or a checkbutton, depending on the
  @code{\"radio\"} property. When activated, it emits the @code{\"toggled\"}
  signal.
  @begin[Signal Details]{dictionary}
    @subheading{The \"toggled\" signal}
      @begin{pre}
 void user_function (GtkCellRendererToggle *cell_renderer,
                     gchar                 *path,
                     gpointer               user_data)          : Run Last
      @end{pre}
      The ::toggled signal is emitted when the cell is toggled.
      @begin[code]{table}
        @entry[cell_renderer]{the object which received the signal}
        @entry[path]{string representation of GtkTreePath describing the event
          location}
        @entry[user_data]{user data set when the signal handler was connected.}
      @end{table}
  @end{dictionary}
  @see-slot{gtk-cell-renderer-toggle-activatable}
  @see-slot{gtk-cell-renderer-toggle-active}
  @see-slot{gtk-cell-renderer-toggle-inconsistent}
  @see-slot{gtk-cell-renderer-toggle-indicator-size}
  @see-slot{gtk-cell-renderer-toggle-radio}
")

;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "activatable" 'gtk-cell-renderer-toggle) 't)
 "The @code{\"activatable\"} property of type @code{gboolean}
  (Read / Write)@br{}
  The toggle button can be activated.@br{}
  Default value: TRUE")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "active" 'gtk-cell-renderer-toggle) 't)
 "The @code{\"active\"} property of type @code{gboolean} (Read / Write)@br{}
  The toggle state of the button.@br{}
  Default value: FALSE")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "inconsistent" 'gtk-cell-renderer-toggle) 't)
 "The @code{\"inconsistent\"} property of type @code{gboolean}
  (Read / Write)@br{}
  The inconsistent state of the button.@br{}
  Default value: FALSE")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "indicator-size" 'gtk-cell-renderer-toggle) 't)
 "The @code{\"indicator-size\"} property of type @code{gint} (Read / Write)@br{}
  Size of check or radio indicator.@br{}
  Allowed values: >= 0@br{}
  Default value: 16")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "radio" 'gtk-cell-renderer-toggle) 't)
 "The @code{\"radio\"} property of type @code{gboolean} (Read / Write)@br{}
  Draw the toggle button as a radio button.@br{}
  Default value: FALSE")

;;; ----------------------------------------------------------------------------
;;;
;;; Accessors
;;;
;;; ----------------------------------------------------------------------------

;;; --- gtk-cell-renderer-toggle-activatable -----------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-cell-renderer-toggle-activatable atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-cell-renderer-toggle-activatable 'function)
 "@version{2013-2-23}
  @begin{short}
    Accessor of the slot @code{\"activatable\"} of the
    @class{gtk-cell-renderer-toggle} class.
  @end{short}")

;;; --- gtk-cell-renderer-toggle-active ----------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-cell-renderer-toggle-active atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-cell-renderer-toggle-active 'function)
 "@version{2013-2-23}
  @begin{short}
    Accessor of the slot @code{\"active\"} of the
    @class{gtk-cell-renderer-toggle} class.
  @end{short}")

;;; --- gtk-cell-renderer-toggle-inconsistent ----------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-cell-renderer-toggle-inconsistent atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-cell-renderer-toggle-inconsistent 'function)
 "@version{2013-2-23}
  @begin{short}
    Accessor of the slot @code{\"inconsistent\"} of the
    @class{gtk-cell-renderer-toggle} class.
  @end{short}")

;;; --- gtk-cell-renderer-toggle-indicator-size --------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-cell-renderer-toggle-indicator-size atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-cell-renderer-toggle-indicator-size 'function)
 "@version{2013-2-23}
  @begin{short}
    Accessor of the slot @code{\"indicator-size\"} of the
    @class{gtk-cell-renderer-toggle} class.
  @end{short}")

;;; --- gtk-cell-renderer-toggle-radio -----------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-cell-renderer-toggle-radio atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-cell-renderer-toggle-radio 'function)
 "@version{2013-2-23}
  @begin{short}
    Accessor of the slot @code{\"radio\"} of the
    @class{gtk-cell-renderer-toggle} class.
  @end{short}")

;;; ----------------------------------------------------------------------------
;;; gtk_cell_renderer_toggle_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-cell-renderer-toggle-new))

(defun gtk-cell-renderer-toggle-new ()
 #+cl-cffi-gtk-documentation
 "@version{2013-2-23}
  @return{The new cell renderer.}
  @begin{short}
    Creates a new GtkCellRendererToggle.
  @end{short}
  Adjust rendering parameters using object properties. Object properties can be
  set globally (with g_object_set()). Also, with GtkTreeViewColumn, you can bind
  a property to a value in a GtkTreeModel. For example, you can bind the
  @code{\"active\"} property on the cell renderer to a boolean value in the
  model, thus causing the check button to reflect the state of the model."
  (make-instance 'gtk-cell-renderer-toggle))

(export 'gtk-cell-renderer-toggle-new)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_renderer_toggle_get_radio ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-cell-renderer-toggle-get-radio))

(defun gtk-cell-renderer-toggle-get-radio (toggle)
 #+cl-cffi-gtk-documentation
 "@version{2013-2-23}
  @argument[toggle]{a GtkCellRendererToggle}
  @return{TRUE if we're rendering radio toggles rather than checkboxes}
  @begin{short}
    Returns whether we're rendering radio toggles rather than checkboxes.
  @end{short}"
  (gtk-cell-renderer-toggle-radio toggle))

(export 'gtk-cell-renderer-toggle-get-radio)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_renderer_toggle_set_radio ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-cell-renderer-toggle-set-radio))

(defun gtk-cell-renderer-toggle-set-radio (toggle radio)
 #+cl-cffi-gtk-documentation
 "@version{2013-2-23}
  @argument[toggle]{a GtkCellRendererToggle}
  @argument[radio]{TRUE to make the toggle look like a radio button}
  @begin{short}
    If radio is TRUE, the cell renderer renders a radio toggle (i.e. a toggle in
    a group of mutually-exclusive toggles). If FALSE, it renders a check toggle
    (a standalone boolean option).
  @end{short}
  This can be set globally for the cell renderer, or changed just before
  rendering each cell in the model (for GtkTreeView, you set up a per-row
  setting using GtkTreeViewColumn to associate model columns with cell renderer
  properties)."
  (setf (gtk-cell-renderer-toggle-radio toggle) radio))

(export 'gtk-cell-renderer-toggle-set-radio)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_renderer_toggle_get_active ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-cell-renderer-toggle-get-active))

(defun gtk-cell-renderer-toggle-get-active (toggle)
 #+cl-cffi-gtk-documentation
 "@version{2013-2-23}
  @argument[toggle]{a GtkCellRendererToggle}
  @return{TRUE if the cell renderer is active.}
  @begin{short}
    Returns whether the cell renderer is active. See
    gtk_cell_renderer_toggle_set_active().
  @end{short}"
  (gtk-cell-renderer-toggle-active toggle))

(export 'gtk-cell-renderer-toggle-get-active)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_renderer_toggle_set_active ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-cell-renderer-toggle-set-active))

(defun gtk-cell-renderer-toggle-set-active (toggle setting)
 #+cl-cffi-gtk-documentation
 "@version{2013-2-23}
  @argument[toggle]{a GtkCellRendererToggle.}
  @argument[setting]{the value to set.}
  @begin{short}
    Activates or deactivates a cell renderer.
  @end{short}"
  (setf (gtk-cell-renderer-toggle-active toggle) setting))

(export 'gtk-cell-renderer-toggle-set-active)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_renderer_toggle_get_activatable ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-cell-renderer-toggle-get-activatable))

(defun gtk-cell-renderer-toggle-get-activatable (toggle)
 #+cl-cffi-gtk-documentation
 "@version{2013-2-23}
  @argument[toggle]{a GtkCellRendererToggle}
  @return{TRUE if the cell renderer is activatable.}
  @begin{short}
    Returns whether the cell renderer is activatable.
  @end{short}
  See gtk_cell_renderer_toggle_set_activatable().

  Since 2.18"
  (gtk-cell-renderer-toggle-activatable toggle))

(export 'gtk-cell-renderer-toggle-get-activatable)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_renderer_toggle_set_activatable ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-cell-renderer-toggle-set-activatable))

(defun gtk-cell-renderer-toggle-set-activatable (toggle setting)
 #+cl-cffi-gtk-documentation
 "@version{2013-2-23}
  @argument[toggle]{a GtkCellRendererToggle.}
  @argument[setting]{the value to set.}
  @short{Makes the cell renderer activatable.}

  Since 2.18"
  (setf (gtk-cell-renderer-toggle-activatable toggle) setting))

(export 'gtk-cell-renderer-toggle-set-activatable)

;;; --- End of file gtk.cell-renderer-toggle.lisp ------------------------------
