;;; ----------------------------------------------------------------------------
;;; gtk.font-chooser-dialog.lisp
;;;
;;; The documentation has been copied from the GTK+ 3 Reference Manual
;;; Version 3.4.3. See http://www.gtk.org.
;;;
;;; Copyright (C) 2012 Dieter Kaiser
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
;;; GtkFontChooserDialog
;;;
;;; A dialog for selecting fonts
;;;
;;; Synopsis
;;;
;;;     GtkFontChooserDialog
;;;
;;;     gtk_font_chooser_dialog_new
;;;
;;; Object Hierarchy
;;;
;;;   GObject
;;;    +----GInitiallyUnowned
;;;          +----GtkWidget
;;;                +----GtkContainer
;;;                      +----GtkBin
;;;                            +----GtkWindow
;;;                                  +----GtkDialog
;;;                                        +----GtkFontChooserDialog
;;;
;;; Implemented Interfaces
;;;
;;; GtkFontChooserDialog implements AtkImplementorIface, GtkBuildable and
;;; GtkFontChooser.
;;;
;;; Description
;;;
;;; The GtkFontChooserDialog widget is a dialog for selecting a font. It
;;; implements the GtkFontChooser interface.
;;;
;;; GtkFontChooserDialog as GtkBuildable
;;; The GtkFontChooserDialog implementation of the GtkBuildable interface
;;; exposes the buttons with the names "select_button" and "cancel_button.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkFontChooserDialog
;;;
;;; struct GtkFontChooserDialog;
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkFontChooserDialog" gtk-font-chooser-dialog
  (:superclass gtk-dialog
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable"
                "GtkFontChooser")
   :type-initializer "gtk_font_chooser_dialog_get_type")
  nil)

;;; ----------------------------------------------------------------------------
;;; gtk_font_chooser_dialog_new ()
;;;
;;; GtkWidget * gtk_font_chooser_dialog_new (const gchar *title,
;;;                                          GtkWindow *parent);
;;;
;;; Creates a new GtkFontChooserDialog.
;;;
;;; title :
;;;     Title of the dialog, or NULL
;;;
;;; parent :
;;;     Transient parent of the dialog, or NULL
;;;
;;; Returns :
;;;     a new GtkFontChooserDialog
;;;
;;; Since 3.2
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-font-chooser-dialog-new))

(defun gtk-font-chooser-dialog-new (title parent)
  (make-instance 'gtk-font-chooser-dialog
                 :title title
                 :parent parent))

(export 'gtk-font-chooser-dialog-new)

;;; --- End of file gtk.font-chooser-dialog.lisp -------------------------------
