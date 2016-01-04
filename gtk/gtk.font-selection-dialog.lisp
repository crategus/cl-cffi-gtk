;;; ----------------------------------------------------------------------------
;;; gtk.font-selection-dialog.lisp
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
;;; GtkFontSelectionDialog
;;;
;;; Deprecated dialog box for selecting fonts
;;;
;;; Synopsis
;;;
;;;     GtkFontSelectionDialog
;;;
;;;     gtk_font_selection_dialog_new
;;;     gtk_font_selection_dialog_get_font_name
;;;     gtk_font_selection_dialog_set_font_name
;;;     gtk_font_selection_dialog_get_preview_text
;;;     gtk_font_selection_dialog_set_preview_text
;;;     gtk_font_selection_dialog_get_cancel_button
;;;     gtk_font_selection_dialog_get_ok_button
;;;     gtk_font_selection_dialog_get_font_selection
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkFontSelectionDialog
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkFontSelectionDialog" gtk-font-selection-dialog
  (:superclass gtk-dialog
   :export t
   :interfaces ("AtkImplementorIface" "GtkBuildable")
   :type-initializer "gtk_font_selection_dialog_get_type")
  nil)
;  ((:cffi font-name
;          gtk-font-selection-dialog-font-name
;          (g-string :free-from-foreign t :free-to-foreign t)
;          "gtk_font_selection_dialog_get_font_name"
;          "gtk_font_selection_dialog_set_font_name")
;   (:cffi preview-text
;          gtk-font-selection-dialog-preview-text :string
;          "gtk_font_selection_dialog_get_preview_text"
;          "gtk_font_selection_dialog_set_preview_text")
;   (:cffi apply-button
;          gtk-font-selection-dialog-apply-button g-object
;          "gtk_font_selection_dialog_get_apply_button" nil)
;   (:cffi cancel-button
;          gtk-font-selection-dialog-cancel-button g-object
;          "gtk_font_selection_dialog_get_cancel_button" nil)
;   (:cffi ok-button
;          gtk-font-selection-dialog-ok-button g-object
;          "gtk_font_selection_dialog_get_ok_button" nil)))

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-font-selection-dialog 'type)
 "@version{2013-6-18}
  @begin{short}
    The @class{gtk-font-selection-dialog} widget is a dialog box for selecting
    a font.
  @end{short}

  To set the font which is initially selected, use the function
  @fun{gtk-font-selection-dialog-set-font-name}.

  To get the selected font use the function
  @fun{gtk-font-selection-dialog-get-font-name}.

  To change the text which is shown in the preview area, use the function
  @fun{gtk-font-selection-dialog-set-preview-text}.

  In GTK+ 3.2, @sym{gtk-font-selection-dialog} has been deprecated in favor of
  @class{gtk-font-chooser-dialog}.

  @subheading{GtkFontSelectionDialog as GtkBuildable}
    The @sym{gtk-font-selection-dialog} implementation of the
    @class{gtk-buildable} interface exposes the embedded
    @class{gtk-font-selection} as internal child with the name
    \"font_selection\". It also exposes the buttons with the names
    \"ok_button\", \"cancel_button\" and \"apply_button\".")

;;; ----------------------------------------------------------------------------
;;; gtk_font_selection_dialog_new ()
;;;
;;; GtkWidget * gtk_font_selection_dialog_new (const gchar *title);
;;;
;;; Warning
;;;
;;; gtk_font_selection_dialog_new has been deprecated since version 3.2 and
;;; should not be used in newly-written code. Use GtkFontChooserDialog
;;;
;;; Creates a new GtkFontSelectionDialog.
;;;
;;; title :
;;;     the title of the dialog window
;;;
;;; Returns :
;;;     a new GtkFontSelectionDialog
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_font_selection_dialog_get_font_name ()
;;;
;;; gchar * gtk_font_selection_dialog_get_font_name
;;;                                               (GtkFontSelectionDialog *fsd);
;;;
;;; Warning
;;;
;;; gtk_font_selection_dialog_get_font_name has been deprecated since version
;;; 3.2 and should not be used in newly-written code. Use GtkFontChooserDialog
;;;
;;; Gets the currently-selected font name.
;;;
;;; Note that this can be a different string than what you set with
;;; gtk_font_selection_dialog_set_font_name(), as the font selection widget may
;;; normalize font names and thus return a string with a different structure.
;;; For example, "Helvetica Italic Bold 12" could be normalized to "Helvetica
;;; Bold Italic 12". Use pango_font_description_equal() if you want to compare
;;; two font descriptions.
;;;
;;; fsd :
;;;     a GtkFontSelectionDialog
;;;
;;; Returns :
;;;     A string with the name of the current font, or NULL if no font is
;;;     selected. You must free this string with g_free().
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_font_selection_dialog_set_font_name ()
;;;
;;; gboolean gtk_font_selection_dialog_set_font_name
;;;                                                (GtkFontSelectionDialog *fsd,
;;;                                                 const gchar *fontname);
;;;
;;; Warning
;;;
;;; gtk_font_selection_dialog_set_font_name has been deprecated since version
;;; 3.2 and should not be used in newly-written code. Use GtkFontChooserDialog
;;;
;;; Sets the currently selected font.
;;;
;;; fsd :
;;;     a GtkFontSelectionDialog
;;;
;;; fontname :
;;;     a font name like "Helvetica 12" or "Times Bold 18"
;;;
;;; Returns :
;;;     TRUE if the font selected in fsd is now the fontname specified, FALSE
;;;     otherwise.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_font_selection_dialog_get_preview_text ()
;;;
;;; const gchar * gtk_font_selection_dialog_get_preview_text
;;;                                               (GtkFontSelectionDialog *fsd);
;;;
;;; Warning
;;;
;;; gtk_font_selection_dialog_get_preview_text has been deprecated since version
;;; 3.2 and should not be used in newly-written code. Use GtkFontChooserDialog
;;;
;;; Gets the text displayed in the preview area.
;;;
;;; fsd :
;;;     a GtkFontSelectionDialog
;;;
;;; Returns :
;;;     the text displayed in the preview area. This string is owned by the
;;;     widget and should not be modified or freed
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_font_selection_dialog_set_preview_text ()
;;;
;;; void gtk_font_selection_dialog_set_preview_text
;;;                                                (GtkFontSelectionDialog *fsd,
;;;                                                 const gchar *text);
;;;
;;; Warning
;;;
;;; gtk_font_selection_dialog_set_preview_text has been deprecated since version
;;; 3.2 and should not be used in newly-written code. Use GtkFontChooserDialog
;;;
;;; Sets the text displayed in the preview area.
;;;
;;; fsd :
;;;     a GtkFontSelectionDialog
;;;
;;; text :
;;;     the text to display in the preview area
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_font_selection_dialog_get_cancel_button ()
;;;
;;; GtkWidget * gtk_font_selection_dialog_get_cancel_button
;;;                                               (GtkFontSelectionDialog *fsd);
;;;
;;; Warning
;;;
;;; gtk_font_selection_dialog_get_cancel_button has been deprecated since
;;; version 3.2 and should not be used in newly-written code. Use
;;; GtkFontChooserDialog
;;;
;;; Gets the 'Cancel' button.
;;;
;;; fsd :
;;;     a GtkFontSelectionDialog
;;;
;;; Returns :
;;;     the GtkWidget used in the dialog for the 'Cancel' button
;;;
;;; Since 2.14
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_font_selection_dialog_get_ok_button ()
;;;
;;; GtkWidget * gtk_font_selection_dialog_get_ok_button
;;;                                               (GtkFontSelectionDialog *fsd);
;;;
;;; Warning
;;;
;;; gtk_font_selection_dialog_get_ok_button has been deprecated since version
;;; 3.2 and should not be used in newly-written code. Use GtkFontChooserDialog
;;;
;;; Gets the 'OK' button.
;;;
;;; fsd :
;;;     a GtkFontSelectionDialog
;;;
;;; Returns :
;;;     the GtkWidget used in the dialog for the 'OK' button
;;;
;;; Since 2.14
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_font_selection_dialog_get_font_selection ()
;;;
;;; GtkWidget * gtk_font_selection_dialog_get_font_selection
;;;                                               (GtkFontSelectionDialog *fsd);
;;;
;;; Warning
;;;
;;; gtk_font_selection_dialog_get_font_selection has been deprecated since
;;; version 3.2 and should not be used in newly-written code. Use
;;; GtkFontChooserDialog
;;;
;;; Retrieves the GtkFontSelection widget embedded in the dialog.
;;;
;;; fsd :
;;;     a GtkFontSelectionDialog
;;;
;;; Returns :
;;;     the embedded GtkFontSelection
;;;
;;; Since 2.22
;;; ----------------------------------------------------------------------------

;;; --- End of file gtk.font-selection-dialog.lisp -----------------------------

