;;; ----------------------------------------------------------------------------
;;; gtk.font-selection-dialog.lisp
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
;;; GtkFontSelection
;;; 
;;; Deprecated widget for selecting fonts
;;; 
;;; Synopsis
;;; 
;;;     GtkFontSelection
;;;
;;;     gtk_font_selection_new
;;;     gtk_font_selection_get_font_name
;;;     gtk_font_selection_set_font_name
;;;     gtk_font_selection_get_preview_text
;;;     gtk_font_selection_set_preview_text
;;;     gtk_font_selection_get_face
;;;     gtk_font_selection_get_face_list
;;;     gtk_font_selection_get_family
;;;     gtk_font_selection_get_size
;;;     gtk_font_selection_get_family_list
;;;     gtk_font_selection_get_preview_entry
;;;     gtk_font_selection_get_size_entry
;;;     gtk_font_selection_get_size_list
;;; 
;;; Object Hierarchy
;;; 
;;;   GObject
;;;    +----GInitiallyUnowned
;;;          +----GtkWidget
;;;                +----GtkContainer
;;;                      +----GtkBox
;;;                            +----GtkFontSelection
;;; 
;;; Implemented Interfaces
;;; 
;;; GtkFontSelection implements AtkImplementorIface, GtkBuildable and
;;; GtkOrientable.
;;;
;;; Properties
;;; 
;;;   "font-name"                gchar*                : Read / Write
;;;   "preview-text"             gchar*                : Read / Write
;;; 
;;; Description
;;; 
;;; The GtkFontSelection widget lists the available fonts, styles and sizes,
;;; allowing the user to select a font. It is used in the GtkFontSelectionDialog
;;; widget to provide a dialog box for selecting fonts.
;;; 
;;; To set the font which is initially selected, use
;;; gtk_font_selection_set_font_name().
;;; 
;;; To get the selected font use gtk_font_selection_get_font_name().
;;; 
;;; To change the text which is shown in the preview area, use
;;; gtk_font_selection_set_preview_text().
;;; 
;;; In GTK+ 3.2, GtkFontSelection has been deprecated in favor of
;;; GtkFontChooser.
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "font-name" property
;;; 
;;;   "font-name"                gchar*                : Read / Write
;;; 
;;; The string that represents this font.
;;; 
;;; Default value: "Sans 10"
;;;
;;; ----------------------------------------------------------------------------
;;; The "preview-text" property
;;; 
;;;   "preview-text"             gchar*                : Read / Write
;;; 
;;; The text to display in order to demonstrate the selected font.
;;; 
;;; Default value: "abcdefghijk ABCDEFGHIJK"
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkFontSelection
;;; 
;;; struct GtkFontSelection;
;;; 
;;; Warning
;;; 
;;; GtkFontSelection is deprecated and should not be used in newly-written code.
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkFontSelectionDialog" gtk-font-selection-dialog
  (:superclass gtk-dialog
   :export t
   :interfaces ("AtkImplementorIface" "GtkBuildable")
   :type-initializer "gtk_font_selection_dialog_get_type")
  ((:cffi font-name
          gtk-font-selection-dialog-font-name
          (g-string :free-from-foreign t :free-to-foreign t)
          "gtk_font_selection_dialog_get_font_name"
          "gtk_font_selection_dialog_set_font_name")
   (:cffi preview-text
          gtk-font-selection-dialog-preview-text :string
          "gtk_font_selection_dialog_get_preview_text"
          "gtk_font_selection_dialog_set_preview_text")
   (:cffi apply-button
          gtk-font-selection-dialog-apply-button g-object
          "gtk_font_selection_dialog_get_apply_button" nil)
   (:cffi cancel-button
          gtk-font-selection-dialog-cancel-button g-object
          "gtk_font_selection_dialog_get_cancel_button" nil)
   (:cffi ok-button
          gtk-font-selection-dialog-ok-button g-object
          "gtk_font_selection_dialog_get_ok_button" nil)))

;;; ----------------------------------------------------------------------------
;;; gtk_font_selection_new ()
;;; 
;;; GtkWidget * gtk_font_selection_new (void);
;;; 
;;; Warning
;;; 
;;; gtk_font_selection_new is deprecated and should not be used in newly-written
;;; code.
;;; 
;;; Creates a new GtkFontSelection.
;;; 
;;; Returns :
;;;     a new GtkFontSelection
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_font_selection_get_font_name ()
;;; 
;;; gchar * gtk_font_selection_get_font_name (GtkFontSelection *fontsel);
;;; 
;;; Warning
;;; 
;;; gtk_font_selection_get_font_name has been deprecated since version 3.2 and
;;; should not be used in newly-written code. Use GtkFontChooser
;;; 
;;; Gets the currently-selected font name.
;;; 
;;; Note that this can be a different string than what you set with
;;; gtk_font_selection_set_font_name(), as the font selection widget may
;;; normalize font names and thus return a string with a different structure.
;;; For example, "Helvetica Italic Bold 12" could be normalized to
;;; "Helvetica Bold Italic 12". Use pango_font_description_equal() if you want
;;; to compare two font descriptions.
;;; 
;;; fontsel :
;;;     a GtkFontSelection
;;; 
;;; Returns :
;;;     A string with the name of the current font, or NULL if no font is
;;;     selected. You must free this string with g_free().
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_font_selection_set_font_name ()
;;; 
;;; gboolean gtk_font_selection_set_font_name (GtkFontSelection *fontsel,
;;;                                            const gchar *fontname);
;;; 
;;; Warning
;;; 
;;; gtk_font_selection_set_font_name has been deprecated since version 3.2 and
;;; should not be used in newly-written code. Use GtkFontChooser
;;; 
;;; Sets the currently-selected font.
;;; 
;;; Note that the fontsel needs to know the screen in which it will appear for
;;; this to work; this can be guaranteed by simply making sure that the fontsel
;;; is inserted in a toplevel window before you call this function.
;;; 
;;; fontsel :
;;;     a GtkFontSelection
;;; 
;;; fontname :
;;;     a font name like "Helvetica 12" or "Times Bold 18"
;;; 
;;; Returns :
;;;     TRUE if the font could be set successfully; FALSE if no such font exists
;;;     or if the fontsel doesn't belong to a particular screen yet.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_font_selection_get_preview_text ()
;;; 
;;; const gchar * gtk_font_selection_get_preview_text
;;;                                                  (GtkFontSelection *fontsel)
;;; 
;;; Warning
;;; 
;;; gtk_font_selection_get_preview_text has been deprecated since version 3.2
;;; and should not be used in newly-written code. Use GtkFontChooser
;;; 
;;; Gets the text displayed in the preview area.
;;; 
;;; fontsel :
;;;     a GtkFontSelection
;;; 
;;; Returns :
;;;     the text displayed in the preview area. This string is owned by the
;;;     widget and should not be modified or freed
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_font_selection_set_preview_text ()
;;; 
;;; void gtk_font_selection_set_preview_text (GtkFontSelection *fontsel,
;;;                                           const gchar *text);
;;; 
;;; Warning
;;; 
;;; gtk_font_selection_set_preview_text has been deprecated since version 3.2
;;; and should not be used in newly-written code. Use GtkFontChooser
;;; 
;;; Sets the text displayed in the preview area. The text is used to show how
;;; the selected font looks.
;;; 
;;; fontsel :
;;;     a GtkFontSelection
;;; 
;;; text :
;;;     the text to display in the preview area
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_font_selection_get_face ()
;;; 
;;; PangoFontFace * gtk_font_selection_get_face (GtkFontSelection *fontsel);
;;; 
;;; Warning
;;; 
;;; gtk_font_selection_get_face has been deprecated since version 3.2 and should
;;; not be used in newly-written code. Use GtkFontChooser
;;; 
;;; Gets the PangoFontFace representing the selected font group details (i.e.
;;; family, slant, weight, width, etc).
;;; 
;;; fontsel :
;;;     a GtkFontSelection
;;; 
;;; Returns :
;;;     A PangoFontFace representing the selected font group details. The
;;;     returned object is owned by fontsel and must not be modified or freed.
;;; 
;;; Since 2.14
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_font_selection_get_face_list ()
;;; 
;;; GtkWidget * gtk_font_selection_get_face_list (GtkFontSelection *fontsel);
;;; 
;;; Warning
;;; 
;;; gtk_font_selection_get_face_list has been deprecated since version 3.2 and
;;; should not be used in newly-written code. Use GtkFontChooser
;;; 
;;; This returns the GtkTreeView which lists all styles available for the
;;; selected font. For example, 'Regular', 'Bold', etc.
;;; 
;;; fontsel :
;;;     a GtkFontSelection
;;; 
;;; Returns :
;;;     A GtkWidget that is part of fontsel.
;;; 
;;; Since 2.14
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_font_selection_get_family ()
;;; 
;;; PangoFontFamily * gtk_font_selection_get_family (GtkFontSelection *fontsel)
;;; 
;;; Warning
;;; 
;;; gtk_font_selection_get_family has been deprecated since version 3.2 and
;;; should not be used in newly-written code. Use GtkFontChooser
;;; 
;;; Gets the PangoFontFamily representing the selected font family.
;;; 
;;; fontsel :
;;;     a GtkFontSelection
;;; 
;;; Returns :
;;;     A PangoFontFamily representing the selected font family. Font families
;;;     are a collection of font faces. The returned object is owned by fontsel
;;;     and must not be modified or freed.
;;; 
;;; Since 2.14
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_font_selection_get_size ()
;;; 
;;; gint gtk_font_selection_get_size (GtkFontSelection *fontsel);
;;; 
;;; Warning
;;; 
;;; gtk_font_selection_get_size has been deprecated since version 3.2 and
;;; should not be used in newly-written code. Use GtkFontChooser
;;; 
;;; The selected font size.
;;; 
;;; fontsel :
;;;     a GtkFontSelection
;;; 
;;; Returns :
;;;     A n integer representing the selected font size, or -1 if no font size
;;;     is selected.
;;; 
;;; Since 2.14
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_font_selection_get_family_list ()
;;; 
;;; GtkWidget * gtk_font_selection_get_family_list  (GtkFontSelection *fontsel)
;;; 
;;; Warning
;;; 
;;; gtk_font_selection_get_family_list has been deprecated since version 3.2 and
;;; should not be used in newly-written code. Use GtkFontChooser
;;; 
;;; This returns the GtkTreeView that lists font families, for example, 'Sans',
;;; 'Serif', etc.
;;; 
;;; fontsel :
;;;     a GtkFontSelection
;;; 
;;; Returns :
;;;     A GtkWidget that is part of fontsel.
;;; 
;;; Since 2.14
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_font_selection_get_preview_entry ()
;;; 
;;; GtkWidget * gtk_font_selection_get_preview_entry (GtkFontSelection *fontsel)
;;; 
;;; Warning
;;; 
;;; gtk_font_selection_get_preview_entry has been deprecated since version 3.2
;;; and should not be used in newly-written code. Use GtkFontChooser
;;; 
;;; This returns the GtkEntry used to display the font as a preview.
;;; 
;;; fontsel :
;;;     a GtkFontSelection
;;; 
;;; Returns :
;;;     A GtkWidget that is part of fontsel.
;;; 
;;; Since 2.14
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_font_selection_get_size_entry ()
;;; 
;;; GtkWidget * gtk_font_selection_get_size_entry (GtkFontSelection *fontsel)
;;; 
;;; Warning
;;; 
;;; gtk_font_selection_get_size_entry has been deprecated since version 3.2 and
;;; should not be used in newly-written code. Use GtkFontChooser
;;; 
;;; This returns the GtkEntry used to allow the user to edit the font number
;;; manually instead of selecting it from the list of font sizes.
;;; 
;;; fontsel :
;;;     a GtkFontSelection
;;; 
;;; Returns :
;;;     A GtkWidget that is part of fontsel.
;;; 
;;; Since 2.14
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_font_selection_get_size_list ()
;;; 
;;; GtkWidget * gtk_font_selection_get_size_list (GtkFontSelection *fontsel)
;;; 
;;; Warning
;;; 
;;; gtk_font_selection_get_size_list has been deprecated since version 3.2 and
;;; should not be used in newly-written code. Use GtkFontChooser
;;; 
;;; This returns the GtkTreeeView used to list font sizes.
;;; 
;;; fontsel :
;;;     a GtkFontSelection
;;; 
;;; Returns :
;;;     A GtkWidget that is part of fontsel.
;;; 
;;; Since 2.14
;;; ----------------------------------------------------------------------------


;;; --- End of file gtk.font-selection-dialog.lisp -----------------------------
