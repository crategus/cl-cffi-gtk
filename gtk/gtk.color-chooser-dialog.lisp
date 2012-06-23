;;; ----------------------------------------------------------------------------
;;; gtk.color-chooser-dialog.lisp
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
;;; GtkColorChooserDialog
;;; 
;;; A dialog for choosing colors
;;;     
;;; Synopsis
;;; 
;;;     GtkColorChooserDialog
;;;
;;;     gtk_color_chooser_dialog_new
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
;;;                                        +----GtkColorChooserDialog
;;; 
;;; Implemented Interfaces
;;; 
;;; GtkColorChooserDialog implements AtkImplementorIface, GtkBuildable and
;;; GtkColorChooser.
;;;
;;; Properties
;;; 
;;;   "show-editor"              gboolean              : Read / Write
;;; 
;;; Description
;;; 
;;; The GtkColorChooserDialog widget is a dialog for choosing a color. It
;;; implements the GtkColorChooser interface.
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "show-editor" property
;;; 
;;;   "show-editor"              gboolean              : Read / Write
;;; 
;;; Show editor.
;;; 
;;; Default value: FALSE
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkColorChooserDialog
;;; 
;;; struct GtkColorChooserDialog;
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkColorChooserDialog" gtk-color-chooser-dialog
  (:superclass gtk-dialog
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable"
                "GtkColorChooser")
   :type-initializer "gtk_color_chooser_dialog_get_type")
  ((show-editor
    gtk-color-chooser-dialog-show-editor
    "show-editor" "gboolean" t t)))

;;; ----------------------------------------------------------------------------
;;; gtk_color_chooser_dialog_new ()
;;; 
;;; GtkWidget * gtk_color_chooser_dialog_new (const gchar *title,
;;;                                           GtkWindow *parent);
;;; 
;;; Creates a new GtkColorChooserDialog.
;;; 
;;; title :
;;;     Title of the dialog, or NULL.
;;; 
;;; parent :
;;;     Transient parent of the dialog, or NULL.
;;; 
;;; Returns :
;;;     a new GtkColorChooserDialog
;;; 
;;; Since 3.4
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-color-chooser-dialog-new))

(defun gtk-color-chooser-dialog-new (title parent)
  (make-instance 'gtk-color-chooser-dialog
                 :title title
                 :parent parent))

(export 'gtk-color-chooser-dialog-new)

;;; --- End of file gtk.color-chooser-dialog.lisp ------------------------------
