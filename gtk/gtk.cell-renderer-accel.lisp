;;; ----------------------------------------------------------------------------
;;; gtk.cell-renderer-accel.lisp
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
;;; GtkCellRendererAccel
;;;
;;;     Renders a keyboard accelerator in a cell
;;;
;;; Types and Values
;;;
;;;     GtkCellRendererAccel
;;;     GtkCellRendererAccelMode
;;;
;;; Functions
;;;
;;;     gtk_cell_renderer_accel_new
;;;
;;; Properties
;;;
;;;                    guint    accel-key        Read / Write
;;; GtkCellRendererAccelMode    accel-mode       Read / Write
;;;          GdkModifierType    accel-mods       Read / Write
;;;                    guint    keycode          Read / Write
;;;
;;; Signals
;;;
;;;                     void    accel-cleared    Run Last
;;;                     void    accel-edited     Run Last
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkCellRenderer
;;;             ╰── GtkCellRendererText
;;;                 ╰── GtkCellRendererAccel
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; enum GtkCellRendererAccelMode
;;; ----------------------------------------------------------------------------

(define-g-enum "GtkCellRendererAccelMode" gtk-cell-renderer-accel-mode
  (:export t
   :type-initializer "gtk_cell_renderer_accel_mode_get_type")
  (:gtk 0)
  (:other 1))

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-cell-renderer-accel-mode atdoc:*symbol-name-alias*) "Enum"
      (gethash 'gtk-cell-renderer-accel-mode atdoc:*external-symbols*)
 "@version{2020-6-20}
  @begin{short}
    Determines if the edited accelerators are GTK+ accelerators.
  @end{short}
  If they are, consumed modifiers are suppressed, only accelerators accepted by
  GTK+ are allowed, and the accelerators are rendered in the same way as they
  are in menus.
  @begin{pre}
(define-g-enum \"GtkCellRendererAccelMode\" gtk-cell-renderer-accel-mode
  (:export t
   :type-initializer \"gtk_cell_renderer_accel_mode_get_type\")
  (:gtk 0)
  (:other 1))
  @end{pre}
  @begin[code]{table}
    @entry[:gtk]{GTK+ accelerators mode.}
    @entry[:other]{Other accelerator mode.}
  @end{table}
  @see-class{gtk-cell-renderer-accel}")

;;; ----------------------------------------------------------------------------
;;; struct GtkCellRendererAccel
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

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-cell-renderer-accel 'type)
 "@version{2020-6-20}
  @begin{short}
    @sym{gtk-cell-renderer-accel} displays a keyboard accelerator, i. e. a key
    combination like @code{Control+a}.
  @end{short}
  If the cell renderer is editable, the accelerator can be changed by simply
  typing the new combination.

  The @sym{gtk-cell-renderer-accel} cell renderer was added in GTK+ 2.10.
  @begin[Signal Details]{dictionary}
    @subheading{The \"accel-cleared\" signal}
      @begin{pre}
 lambda (accel path)    : Run Last
      @end{pre}
      Gets emitted when the user has removed the accelerator.
      @begin[code]{table}
        @entry[accel]{The @sym{gtk-cell-renderer-accel} object reveiving the
          signal.}
        @entry[path]{A string with the path identifying the row of the edited
          cell.}
      @end{table}
    @subheading{The \"accel-edited\" signal}
      @begin{pre}
 lambda (accel path accel-key accel-mods hardware-keycode)    : Run Last
      @end{pre}
      Gets emitted when the user has selected a new accelerator.
      @begin[code]{table}
        @entry[accel]{The @sym{gtk-cell-renderer-accel} object reveiving the
          signal.}
        @entry[path]{A string with the path identifying the row of the edited
          cell.}
        @entry[accel-key]{An unsigned integer with the new accelerator keyval.}
        @entry[accel-mods]{A @class{gdk-modifier-type} value with the new
          acclerator modifier mask.}
        @entry[hardware-keycode]{An unsignend integer with the keycode of the
          new accelerator.}
      @end{table}
  @end{dictionary}
  @see-slot{gtk-cell-renderer-accel-accel-key}
  @see-slot{gtk-cell-renderer-accel-accel-mode}
  @see-slot{gtk-cell-renderer-accel-accel-mods}
  @see-slot{gtk-cell-renderer-accel-keycode}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk-cell-renderer-accel-accel-key --------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "accel-key"
                                               'gtk-cell-renderer-accel) 't)
 "The @code{accel-key} property of type @code{:uint} (Read / Write) @br{}
  The keyval of the accelerator. @br{}
  Allowed values: <= @code{G_MAXINT} @br{}
  Default value: 0")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-cell-renderer-accel-accel-key atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-cell-renderer-accel-accel-key 'function)
 "@version{2020-6-20}
  @syntax[]{(gtk-cell-renderer-accel-accel-key object) => accel-key}
  @syntax[]{(setf (gtk-cell-renderer-accel-accel-key object) accel-key)}
  @argument[object]{a @class{gtk-cell-renderer-accel} object}
  @argument[accel-key]{an unsigned integer with the keyval of the accelerator}
  @begin{short}
    Accessor of the @slot[gtk-cell-renderer]{accel-key} slot of the
    @class{gtk-cell-renderer-accel} class.
  @end{short}

  The keyval of the accelerator.
  @see-class{gtk-cell-renderer-accel}")

;;; --- gtk-cell-renderer-accel-accel-mode -------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "accel-mode"
                                               'gtk-cell-renderer-accel) 't)
 "The @code{accel-mode} property of type @symbol{gtk-cell-renderer-accel-mode}
  (Read / Write) @br{}
  Determines if the edited accelerators are GTK+ accelerators. If they are,
  consumed modifiers are suppressed, only accelerators accepted by GTK+ are
  allowed, and the accelerators are rendered in the same way as they are in
  menus. @br{}
  Default value: @code{:gtk}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-cell-renderer-accel-accel-mode atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-cell-renderer-accel-accel-mode 'function)
 "@version{2020-6-20}
  @syntax[]{(gtk-cell-renderer-accel-accel-mode object) => accel-mode}
  @syntax[]{(setf (gtk-cell-renderer-accel-accel-mode object) accel-mode)}
  @argument[object]{a @class{gtk-cell-renderer-accel} object}
  @argument[accel-mode]{a value of the @symbol{gtk-cell-renderer-accel-mode}
    enumeration}
  @begin{short}
    Accessor of the @slot[gtk-cell-renderer-accel]{accel-mode} slot of the
    @class{gtk-cell-renderer-accel} class.
  @end{short}

  Determines if the edited accelerators are GTK+ accelerators. If they are,
  consumed modifiers are suppressed, only accelerators accepted by GTK+ are
  allowed, and the accelerators are rendered in the same way as they are in
  menus.
  @see-class{gtk-cell-renderer-accel}
  @see-symbol{gtk-cell-renderer-accel-mode}")

;;; --- gtk-cell-renderer-accel-accel-mods -------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "accel-mods"
                                               'gtk-cell-renderer-accel) 't)
 "The @code{accel-mods} property of type @symbol{gdk-modifier-type}
  (Read / Write) @br{}
  The modifier mask of the accelerator.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-cell-renderer-accel-accel-mods atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-cell-renderer-accel-accel-mods 'function)
 "@version{2020-6-20}
  @syntax[]{(gtk-cell-renderer-accel-accel-mods object) => accel-mods}
  @syntax[]{(setf (gtk-cell-renderer-accel-accel-mods object) accel-mods)}
  @argument[object]{a @class{gtk-cell-renderer-accel} object}
  @argument[accel-mode]{a @symbol{gtk-modifier-type} value}
  @begin{short}
    Accessor of the @slot[gtk-cell-renderer-accel]{accel-mods} slot of the
    @class{gtk-cell-renderer-accel} class.
  @end{short}

  The modifier mask of the accelerator.
  @see-class{gtk-cell-renderer-accel}
  @see-symbol{gtk-modifier-type}")

;;; --- gtk-cell-renderer-accel-keycode ----------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "keycode"
                                               'gtk-cell-renderer-accel) 't)
 "The @code{keycode} property of type @code{:uint} (Read / Write) @br{}
  The hardware keycode of the accelerator. Note that the hardware keycode is
  only relevant if the key does not have a keyval. Normally, the keyboard
  configuration should assign keyvals to all keys. @br{}
  Allowed values: <= @code{G_MAXINT} @br{}
  Default value: 0")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-cell-renderer-accel-keycode atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-cell-renderer-accel-keycode 'function)
 "@version{2020-6-20}
  @syntax[]{(gtk-cell-renderer-accel-keycode object) => keycode}
  @syntax[]{(setf (gtk-cell-renderer-accel-keycode object) keycode)}
  @argument[object]{a @class{gtk-cell-renderer-accel} object}
  @argument[keycode]{an unsigned integer with the hardware keycode of the
    accelerator}
  @begin{short}
    Accessor of the @slot[gtk-cell-renderer-accel]{keycode} slot of the
    @class{gtk-cell-renderer-accel} class.
  @end{short}

  The hardware keycode of the accelerator. Note that the hardware keycode is
  only relevant if the key does not have a keyval. Normally, the keyboard
  configuration should assign keyvals to all keys.
  @see-class{gtk-cell-renderer-accel}")

;;; ----------------------------------------------------------------------------
;;; gtk_cell_renderer_accel_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-cell-renderer-accel-new))

(defun gtk-cell-renderer-accel-new ()
 #+cl-cffi-gtk-documentation
 "@version{2020-6-20}
  @returns{The new @class{gtk-cell-renderer-accel} object.}
  @short{Creates a new cell renderer accel object.}
  @see-class{gtk-cell-renderer-accel}"
  (make-instance 'gtk-cell-renderer-accel))

(export 'gtk-cell-renderer-accel-new)

;;; --- gtk.cell-renderer-accel.lisp -------------------------------------------
