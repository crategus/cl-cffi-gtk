;;; ----------------------------------------------------------------------------
;;; gtk.color-selection-dialog.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See <http://common-lisp.net/project/cl-gtk2/>.
;;;
;;; The documentation has been copied from the GTK+ 3 Reference Manual
;;; Version 3.6.4. See <http://www.gtk.org>. The API documentation of the
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
;;; GtkColorSelectionDialog
;;;
;;; Deprecated dialog box for selecting a color
;;;	
;;; Synopsis
;;;
;;;    GtkColorSelectionDialog;
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

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-color-selection-dialog 'type)
 "@version{2013-2-24}
  @begin{short}
    The GtkColorSelectionDialog provides a standard dialog which allows the user
    to select a color much like the GtkFileChooserDialog provides a standard
    dialog for file selection.
  @end{short}

  Use gtk_color_selection_dialog_get_color_selection() to get the
  GtkColorSelection widget contained within the dialog. Use this widget and its
  gtk_color_selection_get_current_color() function to gain access to the
  selected color. Connect a handler for this widget's \"color-changed\" signal
  to be notified when the color changes.

  @b{GtkColorSelectionDialog as GtkBuildable}

  The GtkColorSelectionDialog implementation of the GtkBuildable interface
  exposes the embedded GtkColorSelection as internal child with the name
  \"color_selection\". It also exposes the buttons with the names \"ok_button\",
  \"cancel_button\" and \"help_button\".
  @see-slot{gtk-color-selection-dialog-cancel-button}
  @see-slot{gtk-color-selection-dialog-color-selection}
  @see-slot{gtk-color-selection-dialog-help-button}
  @see-slot{gtk-color-selection-dialog-ok-button}")

;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "cancel-button"
                                               'gtk-color-selection-dialog) 't)
 "The @code{\"cancel-button\"} property of type @code{GtkWidget*} (Read)@br{}
  The cancel button of the dialog.")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "color-selection"
                                               'gtk-color-selection-dialog) 't)
 "The @code{\"color-selection\"} property of type @code{GtkWidget*} (Read)@br{}
  The color selection embedded in the dialog.")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "help-button"
                                               'gtk-color-selection-dialog) 't)
 "The @code{\"help-button\"} property of type @code{GtkWidget*} (Read)@br{}
  The help button of the dialog.")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "ok-button"
                                               'gtk-color-selection-dialog) 't)
 "The @code{\"ok-button\"} property of type @code{GtkWidget*} (Read)@br{}
  The OK button of the dialog.")

;;; ----------------------------------------------------------------------------
;;;
;;; Accessors
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-color-selection-dialog-cancel-button atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-color-selection-dialog-cancel-button 'function)
 "@version{2013-2-24}
  @begin{short}
    Accessor of the slot @code{\"cancel-button\"} of the
    @class{gtk-color-selection-dialog} class.
  @end{short}")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-color-selection-dialog-color-selection atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-color-selection-dialog-color-selection 'function)
 "@version{2013-2-24}
  @begin{short}
    Accessor of the slot @code{\"color-selection\"} of the
    @class{gtk-color-selection-dialog} class.
  @end{short}")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-color-selection-dialog-help-button atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-color-selection-dialog-help-button 'function)
 "@version{2013-2-24}
  @begin{short}
    Accessor of the slot @code{\"help-button\"} of the
    @class{gtk-color-selection-dialog} class.
  @end{short}")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-color-selection-dialog-ok-button atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-color-selection-dialog-ok-button 'function)
 "@version{2013-2-24}
  @begin{short}
    Accessor of the slot @code{\"ok-button\"} of the
    @class{gtk-color-selection-dialog} class.
  @end{short}")

;;; ----------------------------------------------------------------------------
;;; gtk_color_selection_dialog_new ()
;;;
;;; GtkWidget * gtk_color_selection_dialog_new (const gchar *title);
;;;
;;; Creates a new GtkColorSelectionDialog.
;;;
;;; title :
;;;    a string containing the title text for the dialog.
;;;
;;; Returns :
;;;    a GtkColorSelectionDialog.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_color_selection_dialog_get_color_selection ()
;;;
;;; GtkWidget * gtk_color_selection_dialog_get_color_selection
;;;                                         (GtkColorSelectionDialog *colorsel);
;;;
;;; Retrieves the GtkColorSelection widget embedded in the dialog.
;;;
;;; colorsel :
;;;    a GtkColorSelectionDialog
;;;
;;; Returns :
;;;    the embedded GtkColorSelection. [transfer none]
;;;
;;; Since 2.14
;;; ----------------------------------------------------------------------------

;;; --- End of file gtk.color-selection-dialog.lisp ----------------------------
