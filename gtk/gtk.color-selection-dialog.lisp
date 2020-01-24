;;; ----------------------------------------------------------------------------
;;; gtk.color-selection-dialog.lisp
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
;;; GtkColorSelectionDialog
;;;
;;;     Deprecated dialog box for selecting a color
;;;
;;; Types and Values
;;;
;;;     GtkColorSelectionDialog
;;;
;;; Functions
;;;
;;;    gtk_color_selection_dialog_new
;;;    gtk_color_selection_dialog_get_color_selection
;;;
;;; Properties
;;;
;;;     GtkWidget*   cancel-button      Read
;;;     GtkWidget*   color-selection    Read
;;;     GtkWidget*   help-button        Read
;;;     GtkWidget*   ok-button          Read
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkContainer
;;;                 ╰── GtkBin
;;;                     ╰── GtkWindow
;;;                         ╰── GtkDialog
;;;                             ╰── GtkColorSelectionDialog
;;;
;;; Implemented Interfaces
;;;
;;;     GtkColorSelectionDialog implements AtkImplementorIface and GtkBuildable.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkColorSelection
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkColorSelectionDialog" gtk-color-selection-dialog
  (:superclass gtk-dialog
   :export t
   :interfaces ("AtkImplementorIface" "GtkBuildable")
   :type-initializer "gtk_color_selection_dialog_get_type")
  ((cancel-button
    gtk-color-selection-dialog-cancel-button
    "cancel-button" "GtkWidget" t nil)
   (color-selection
    gtk-color-selection-dialog-color-selection
    "color-selection" "GtkWidget" t nil)
   (help-button
    gtk-color-selection-dialog-help-button
    "help-button" "GtkWidget" t nil)
   (ok-button
    gtk-color-selection-dialog-ok-button
    "ok-button" "GtkWidget" t nil)))

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-color-selection-dialog 'type)
 "@version{2020-1-21}
  @begin{short}
    The @sym{gtk-color-selection-dialog} provides a standard dialog which allows
    the user to select a color much like the @class{gtk-file-chooser-dialog}
    provides a standard dialog for file selection.
  @end{short}

  Use the function @fun{gtk-color-selection-dialog-color-selection} to get the
  @class{gtk-color-selection} widget contained within the dialog. Use this
  widget and its function @fun{gtk-color-selection-current-color} to gain
  access to the selected color. Connect a handler for this widget's
  \"color-changed\" signal to be notified when the color changes.
  @begin[GtkColorSelectionDialog as GtkBuildable]{dictionary}
    The @sym{gtk-color-selection-dialog} implementation of the
    @class{gtk-buildable} interface exposes the embedded
    @class{gtk-color-selection} as internal child with the name
    @code{color_selection}. It also exposes the buttons with the names
    @code{ok_button}, @code{cancel_button} and @code{help_button}.
  @end{dictionary}
  @begin[Warning]{dictionary}
    @sym{gtk-color-selection-dialog} is deprecated since GTK+ 3.4 and should
    not be used in newly-written code.
  @end{dictionary}
  @see-slot{gtk-color-selection-dialog-cancel-button}
  @see-slot{gtk-color-selection-dialog-color-selection}
  @see-slot{gtk-color-selection-dialog-help-button}
  @see-slot{gtk-color-selection-dialog-ok-button}
  @see-class{gtk-color-selection}
  @see-function{gtk-color-selection-current-color}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk-color-selection-dialog-cancel-button -------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "cancel-button"
                                               'gtk-color-selection-dialog) 't)
 "The @code{cancel-button} property of type @class{gtk-widget} (Read) @br{}
  The cancel button of the dialog.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-color-selection-dialog-cancel-button
               atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-color-selection-dialog-cancel-button 'function)
 "@version{2020-1-21}
  @begin{short}
    Accessor of the @slot[gtk-color-selection-dialog]{cancel-button} slot of
    the @class{gtk-color-selection-dialog} class.
  @end{short}
  @begin[Warning]{dictionary}
    The function @sym{gtk-color-selection-dialog-cancel-button} is deprecated
    and should not be used in newly-written code.
  @end{dictionary}
  @see-class{gtk-color-selection-dialog}")

;;; --- gtk-color-selection-dialog-color-selection -----------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "color-selection"
                                               'gtk-color-selection-dialog) 't)
 "The @code{color-selection} property of type @class{gtk-widget} (Read) @br{}
  The color selection embedded in the dialog.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-color-selection-dialog-color-selection
               atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-color-selection-dialog-color-selection 'function)
 "@version{2020-1-22}
  @begin{short}
    Accessor of the @slot[gtk-color-selection-dialog]{color-selection} slot of
    the @class{gtk-color-selection-dialog} class.
  @end{short}

  The @sym{gtk-color-selection-dialog-color-selection} slot access function
  retrieves the @class{gtk-color-selection} widget embedded in the dialog.
  @begin[Warning]{dictionary}
    The function @sym{gtk-color-selection-dialog-color-selection} is deprecated
    and should not be used in newly-written code.
  @end{dictionary}
  @see-class{gtk-color-selection-dialog}")

;;; --- gtk-color-selection-dialog-help-button ---------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "help-button"
                                               'gtk-color-selection-dialog) 't)
 "The @code{help-button} property of type @class{gtk-widget} (Read) @br{}
  The help button of the dialog.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-color-selection-dialog-help-button
               atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-color-selection-dialog-help-button 'function)
 "@version{2020-1-21}
  @begin{short}
    Accessor of the @slot[gtk-color-selection-dialog]{help-button} slot of the
    @class{gtk-color-selection-dialog} class.
  @end{short}
  @begin[Warning]{dictionary}
    The function @sym{gtk-color-selection-dialog-help-button} is deprecated
    and should not be used in newly-written code.
  @end{dictionary}
  @see-class{gtk-color-selection-dialog}")

;;; --- gtk-color-selection-dialog-ok-button -----------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "ok-button"
                                               'gtk-color-selection-dialog) 't)
 "The @code{ok-button} property of type @class{gtk-widget} (Read) @br{}
  The OK button of the dialog.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-color-selection-dialog-ok-button
               atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-color-selection-dialog-ok-button 'function)
 "@version{2020-1-21}
  @begin{short}
    Accessor of the @slot[gtk-color-selection-dialog]{ok-button} slot of the
    @class{gtk-color-selection-dialog} class.
  @end{short}
  @begin[Warning]{dictionary}
    The function @sym{gtk-color-selection-dialog-ok-button} is deprecated
    and should not be used in newly-written code.
  @end{dictionary}
  @see-class{gtk-color-selection-dialog}")

;;; ----------------------------------------------------------------------------
;;; gtk_color_selection_dialog_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-color-selection-dialog-new))

(defun gtk-color-selection-dialog-new (title)
 #+cl-cffi-gtk-documentation
 "@version{2020-1-21}
  @argument[title]{a string containing the title text for the dialog}
  @return{A @class{gtk-color-selection-dialog} widget.}
  @begin{short}
    Creates a new @class{gtk-color-selection-dialog}.
  @end{short}
  @begin[Warning]{dictionary}
    The function @sym{gtk-color-selection-dialog-new} is deprecated and should
    not be used in newly-written code.
  @end{dictionary}
  @see-class{gtk-color-selection-dialog}"
  (make-instance 'gtk-color-selection-dialog
                 :title title))

(export 'gtk-color-selection-dialog-new)

;;; --- End of file gtk.color-selection-dialog.lisp ----------------------------
