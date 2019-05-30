;;; ----------------------------------------------------------------------------
;;; gtk.color-selection-dialog.lisp
;;;
;;; The documentation of this file is taken from the GTK+ 3 Reference Manual
;;; Version 3.6.4 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
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
;;; GtkColorSelectionDialog
;;;
;;; Deprecated dialog box for selecting a color
;;;
;;; Synopsis
;;;
;;;    GtkColorSelectionDialog
;;;
;;;    gtk_color_selection_dialog_new
;;;    gtk_color_selection_dialog_get_color_selection
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
 "@version{2013-12-16}
  @begin{short}
    The @sym{gtk-color-selection-dialog} provides a standard dialog which allows
    the user to select a color much like the @class{gtk-file-chooser-dialog}
    provides a standard dialog for file selection.
  @end{short}

  Use the @fun{gtk-color-selection-dialog-get-color-selection} function to get
  the @class{gtk-color-selection} widget contained within the dialog. Use this
  widget and its @fun{gtk-color-selection-get-current-color} function to gain
  access to the selected color. Connect a handler for this widget's
  \"color-changed\" signal to be notified when the color changes.

  @subheading{GtkColorSelectionDialog as GtkBuildable}
    The @sym{gtk-color-selection-dialog} implementation of the
    @class{gtk-buildable} interface exposes the embedded
    @class{gtk-color-selection} as internal child with the name
    \"color_selection\". It also exposes the buttons with the names
    \"ok_button\", \"cancel_button\" and \"help_button\".
  @see-slot{gtk-color-selection-dialog-cancel-button}
  @see-slot{gtk-color-selection-dialog-color-selection}
  @see-slot{gtk-color-selection-dialog-help-button}
  @see-slot{gtk-color-selection-dialog-ok-button}
  @see-class{gtk-color-selection}
  @see-class{gtk-file-chooser-dialog}
  @see-function{gtk-color-selection-get-current-color}
  @see-function{gtk-color-selection-dialog-get-color-selection}")

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
 "@version{2013-12-16}
  Accessor of the @slot[gtk-color-selection-dialog]{cancel-button} slot of the
  @class{gtk-color-selection-dialog} class.
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
 "@version{2013-12-16}
  Accessor of the @slot[gtk-color-selection-dialog]{color-selection} slot of the
  @class{gtk-color-selection-dialog} class.
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
 "@version{2013-12-16}
  Accessor of the @slot[gtk-color-selection-dialog]{help-button} slot of the
  @class{gtk-color-selection-dialog} class.
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
 "@version{2013-12-16}
  Accessor of the @slot[gtk-color-selection-dialog]{ok-button} slot of the
  @class{gtk-color-selection-dialog} class.
  @see-class{gtk-color-selection-dialog}
  @see-function{gtk-color-selection-get-color-selection}")

;;; ----------------------------------------------------------------------------
;;; gtk_color_selection_dialog_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-color-selection-dialog-new))

(defun gtk-color-selection-dialog-new (title)
 #+cl-cffi-gtk-documentation
 "@version{2013-12-16}
  @argument[title]{a string containing the title text for the dialog}
  @return{A @class{gtk-color-selection-dialog} widget.}
  @begin{short}
    Creates a new @class{gtk-color-selection-dialog}.
  @end{short}
  @see-class{gtk-color-selection-dialog}"
  (make-instance 'gtk-color-selection-dialog
                 :title title))

(export 'gtk-color-selection-dialog-new)

;;; ----------------------------------------------------------------------------
;;; gtk_color_selection_dialog_get_color_selection ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-color-selection-dialog-get-color-selection))

(defun gtk-color-selection-dialog-get-color-selection (colorsel)
 #+cl-cffi-gtk-documentation
 "@version{2013-12-16}
  @argument[colorsel]{a @class{gtk-color-selection-dialog} widget}
  @return{The embedded @class{gtk-color-selection} widget.}
  @begin{short}
    Retrieves the @class{gtk-color-selection} widget embedded in the dialog.
  @end{short}
  @see-class{gtk-color-selection-dialog}
  @see-class{gtk-color-selection}"
  (gtk-color-selection-dialog-color-selection colorsel))

(export 'gtk-color-selection-dialog-get-color-selection)

;;; --- End of file gtk.color-selection-dialog.lisp ----------------------------
