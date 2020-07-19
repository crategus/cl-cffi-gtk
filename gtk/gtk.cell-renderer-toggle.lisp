;;; ----------------------------------------------------------------------------
;;; gtk.cell-renderer-toggle.lisp
;;;
;;; The documentation of this file is taken from the GTK+ 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GTK+ library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2020 Dieter Kaiser
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
;;;     Renders a toggle button in a cell
;;;
;;; Types and Values
;;;
;;;     GtkCellRendererToggle
;;;
;;; Functions
;;;
;;;     gtk_cell_renderer_toggle_new
;;;     gtk_cell_renderer_toggle_get_radio                 Accessor
;;;     gtk_cell_renderer_toggle_set_radio                 Accessor
;;;     gtk_cell_renderer_toggle_get_active                Accessor
;;;     gtk_cell_renderer_toggle_set_active                Accessor
;;;     gtk_cell_renderer_toggle_get_activatable           Accessor
;;;     gtk_cell_renderer_toggle_set_activatable           Accessor
;;;
;;; Properties
;;;
;;;     gboolean    activatable       Read / Write
;;;     gboolean    active            Read / Write
;;;     gboolean    inconsistent      Read / Write
;;;         gint    indicator-size    Read / Write
;;;     gboolean    radio             Read / Write
;;;
;;; Signals
;;;
;;;         void    toggled           Run Last
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkCellRenderer
;;;             ╰── GtkCellRendererToggle
;;; ----------------------------------------------------------------------------

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

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-cell-renderer-toggle 'type)
 "@version{2020-6-13}
  @begin{short}
    @sym{gtk-cell-renderer-toggle} renders a toggle button in a cell.
  @end{short}
  The button is drawn as a radio button or a check button, depending on the
  @code{radio} property. When activated, it emits the \"toggled\" signal.
  @begin[Signal Details]{dictionary}
    @subheading{The \"toggled\" signal}
      @begin{pre}
 lambda (cell-renderer path)    : Run Last
      @end{pre}
      The \"toggled\" signal is emitted when the cell is toggled.
      @begin[code]{table}
        @entry[cell-renderer]{The @sym{gtk-cell-renderer-toggle} object which
          received the signal.}
        @entry[path]{String representation of the @class{gtk-tree-path}
          structure describing the event location.}
      @end{table}
  @end{dictionary}
  @see-slot{gtk-cell-renderer-toggle-activatable}
  @see-slot{gtk-cell-renderer-toggle-active}
  @see-slot{gtk-cell-renderer-toggle-inconsistent}
  @see-slot{gtk-cell-renderer-toggle-indicator-size}
  @see-slot{gtk-cell-renderer-toggle-radio}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk-cell-renderer-toggle-activatable -----------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "activatable"
                                               'gtk-cell-renderer-toggle) 't)
 "The @code{activatable} property of type @code{:boolean} (Read / Write) @br{}
  Wether the toggle button can be activated. @br{}
  Default value: @em{true}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-cell-renderer-toggle-activatable atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-cell-renderer-toggle-activatable 'function)
 "@version{2020-6-13}
  @syntax[]{(gtk-cell-renderer-toggle-activatable object) => setting}
  @syntax[]{(setf (gtk-cell-renderer-toggle-activatable object) setting)}
  @argument[toggle]{a @class{gtk-cell-renderer-toggle} object}
  @argument[setting]{a boolean value to set}
  @begin{short}
    Accessor of the @slot[gtk-cell-renderer-toggle]{activatable} slot of the
    @class{gtk-cell-renderer-toggle} class.
  @end{short}

  The slot access function @sym{gtk-cell-renderer-toggle-activatable} returns
  whether the cell renderer is activatable. The slot access function
  @sym{(setf gtk-cell-renderer-toggle-activatable)} makes the cell renderer
  activatable.
  @see-class{gtk-cell-renderer-toggle}")

;;; --- gtk-cell-renderer-toggle-active ----------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "active"
                                               'gtk-cell-renderer-toggle) 't)
 "The @code{active} property of type @code{:boolean} (Read / Write) @br{}
  The toggle state of the button. @br{}
  Default value: @em{false}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-cell-renderer-toggle-active atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-cell-renderer-toggle-active 'function)
 "@version{2020-6-13}
  @syntax[]{(gtk-cell-renderer-toggle-active object) => setting}
  @syntax[]{(setf (gtk-cell-renderer-toggle-active object) setting)}
  @argument[toggle]{a @class{gtk-cell-renderer-toggle} object}
  @argument[setting]{a boolean value to set}
  @begin{short}
    Accessor of the @slot[gtk-cell-renderer-toggle]{active} slot of the
    @class{gtk-cell-renderer-toggle} class.
  @end{short}

  The slot access function @sym{gtk-cell-renderer-toggle-active} returns
  whether the cell renderer is active. The slot access function
  @sym{(setf gtk-cell-renderer-toggle-active)} activates or deactivates a cell
  renderer.
  @see-class{gtk-cell-renderer-toggle}")

;;; --- gtk-cell-renderer-toggle-inconsistent ----------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "inconsistent"
                                               'gtk-cell-renderer-toggle) 't)
 "The @code{inconsistent} property of type @code{:boolean} (Read / Write) @br{}
  The inconsistent state of the button. @br{}
  Default value: @em{false}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-cell-renderer-toggle-inconsistent
               atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-cell-renderer-toggle-inconsistent 'function)
 "@version{2020-6-13}
  @syntax[]{(gtk-cell-renderer-toggle-inconsistent object) => setting}
  @syntax[]{(setf (gtk-cell-renderer-toggle-inconsistent object) setting)}
  @argument[toggle]{a @class{gtk-cell-renderer-toggle} object}
  @argument[setting]{a boolean value to set}
  @begin{short}
    Accessor of the @slot[gtk-cell-renderer-toggle]{inconsistent} slot of the
    @class{gtk-cell-renderer-toggle} class.
  @end{short}

  The inconsistent state of the button.
  @see-class{gtk-cell-renderer-toggle}")

;;; --- gtk-cell-renderer-toggle-indicator-size --------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "indicator-size"
                                               'gtk-cell-renderer-toggle) 't)
 "The @code{indicator-size} property of type @code{:int} (Read / Write) @br{}
  Size of check or radio indicator. @br{}
  Allowed values: >= 0 @br{}
  Default value: 16")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-cell-renderer-toggle-indicator-size
               atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-cell-renderer-toggle-indicator-size 'function)
 "@version{2020-6-13}
  @syntax[]{(gtk-cell-renderer-toggle-indicator-size object) => size}
  @syntax[]{(setf (gtk-cell-renderer-toggle-indicator-size object) size)}
  @argument[toggle]{a @class{gtk-cell-renderer-toggle} object}
  @argument[size]{an integer with the size of check or radio indicator}
  @begin{short}
    Accessor of the @slot[gtk-cell-renderer-toggle]{indicator-size} slot of the
    @class{gtk-cell-renderer-toggle} class.
  @end{short}

  Size of check or radio indicator.
  @see-class{gtk-cell-renderer-toggle}")

;;; --- gtk-cell-renderer-toggle-radio -----------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "radio"
                                               'gtk-cell-renderer-toggle) 't)
 "The @code{radio} property of type @code{:boolean} (Read / Write) @br{}
  Draw the toggle button as a radio button. @br{}
  Default value: @em{false}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-cell-renderer-toggle-radio atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-cell-renderer-toggle-radio 'function)
 "@version{2020-6-13}
  @syntax[]{(gtk-cell-renderer-toggle-radio object) => radio}
  @syntax[]{(setf (gtk-cell-renderer-toggle-radio object) radio)}
  @argument[toggle]{a @class{gtk-cell-renderer-toggle} object}
  @argument[radio]{@em{true} to make the toggle look like a radio button}
  @begin{short}
    Accessor of the @slot[gtk-cell-renderer-toggle]{radio} slot of the
    @class{gtk-cell-renderer-toggle} class.
  @end{short}

  If @arg{radio} is @em{true}, the cell renderer renders a radio toggle, i. e.
  a toggle in a group of mutually-exclusive toggles. If @em{false}, it renders
  a check toggle, a standalone boolean option.

  This can be set globally for the cell renderer, or changed just before
  rendering each cell in the model, for @class{gtk-tree-view}, you set up a
  per-row setting using @class{gtk-tree-view-column} to associate model columns
  with cell renderer properties.
  @see-class{gtk-cell-renderer-toggle}
  @see-class{gtk-tree-view}
  @see-class{gtk-tree-view-column}")

;;; ----------------------------------------------------------------------------
;;; gtk_cell_renderer_toggle_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-cell-renderer-toggle-new))

(defun gtk-cell-renderer-toggle-new ()
 #+cl-cffi-gtk-documentation
 "@version{2020-6-13}
  @return{The new @class{gtk-cell-renderer-toggle} object.}
  @begin{short}
    Creates a new cell renderer toggle.
  @end{short}
  Adjust rendering parameters using object properties. Object properties can
  be set globally, with the function @fun{g-object-set-property}. Also, with
  @class{gtk-tree-view-column}, you can bind a property to a value in a
  @class{gtk-tree-model}. For example, you can bind the
  @slot[gtk-cell-renderer-toggle]{active} property on the cell renderer to
  a boolean value in the model, thus causing the check button to reflect the
  state of the model.
  @see-class{gtk-cell-renderer-toggle}
  @see-class{gtk-tree-view-column}
  @see-class{gtk-tree-model}
  @see-function{g-object-set-property}"
  (make-instance 'gtk-cell-renderer-toggle))

(export 'gtk-cell-renderer-toggle-new)

;;; --- End of file gtk.cell-renderer-toggle.lisp ------------------------------
