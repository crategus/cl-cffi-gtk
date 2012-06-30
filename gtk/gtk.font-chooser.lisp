;;; ----------------------------------------------------------------------------
;;; gtk.font-chooser.lisp
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
;;; GtkFontChooser
;;;
;;; Interface implemented by widgets displaying fonts
;;;
;;; Synopsis
;;;
;;;     GtkFontChooser
;;;
;;;     gtk_font_chooser_get_font_family
;;;     gtk_font_chooser_get_font_face
;;;     gtk_font_chooser_get_font_size
;;;     gtk_font_chooser_get_font
;;;     gtk_font_chooser_set_font
;;;     gtk_font_chooser_get_font_desc
;;;     gtk_font_chooser_set_font_desc
;;;     gtk_font_chooser_get_preview_text
;;;     gtk_font_chooser_set_preview_text
;;;     gtk_font_chooser_get_show_preview_entry
;;;     gtk_font_chooser_set_show_preview_entry
;;;     gtk_font_chooser_set_filter_func
;;;
;;; Object Hierarchy
;;;
;;;   GInterface
;;;    +----GtkFontChooser
;;;
;;; Prerequisites
;;;
;;; GtkFontChooser requires GObject.
;;;
;;; Known Implementations
;;;
;;; GtkFontChooser is implemented by GtkFontButton, GtkFontChooserDialog and
;;; GtkFontChooserWidget.
;;;
;;; Properties
;;;
;;;   "font"                     gchar*                 : Read / Write
;;;   "font-desc"                PangoFontDescription*  : Read / Write
;;;   "preview-text"             gchar*                 : Read / Write
;;;   "show-preview-entry"       gboolean               : Read / Write
;;;
;;; Signals
;;;
;;;   "font-activated"                                  : Run First
;;;
;;; Description
;;;
;;; GtkFontChooser is an interface that can be implemented by widgets displaying
;;; the list of fonts. In GTK+, the main objects that implement this interface
;;; are GtkFontChooserWidget, GtkFontChooserDialog and GtkFontButton.
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "font" property
;;;
;;;   "font"                     gchar*                : Read / Write
;;;
;;; The font description as a string, e.g. "Sans Italic 12".
;;;
;;; Default value: "Sans 10"
;;;
;;; ----------------------------------------------------------------------------
;;; The "font-desc" property
;;;
;;;   "font-desc"                PangoFontDescription*  : Read / Write
;;;
;;; The font description as a PangoFontDescription.
;;;
;;; ----------------------------------------------------------------------------
;;; The "preview-text" property
;;;
;;;   "preview-text"             gchar*                : Read / Write
;;;
;;; The string with which to preview the font.
;;;
;;; Default value: "The quick brown fox jumps over the lazy dog."
;;;
;;; ----------------------------------------------------------------------------
;;; The "show-preview-entry" property
;;;
;;;   "show-preview-entry"       gboolean              : Read / Write
;;;
;;; Whether to show an entry to change the preview text.
;;;
;;; Default value: TRUE
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Signal Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "font-activated" signal
;;;
;;; void user_function (GtkFontChooser *fontchooser,
;;;                     gchar          *arg1,
;;;                     gpointer        user_data)        : Run First
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkFontChooser
;;;
;;; typedef struct _GtkFontChooser GtkFontChooser;
;;; ----------------------------------------------------------------------------

(define-g-interface "GtkFontChooser" gtk-font-chooser
  (:export t
   :type-initializer "gtk_font_chooser_get_type")
  (font
   gtk-font-chooser-font
   "font" "gchar" t t)
  (font-desc
   gtk-font-chooser-font-desc
   "font-desc" "PangoFontDescription" t t)
  (preview-text
   gtk-font-chooser-preview-text
   "preview-text" "gchar" t t)
  (show-preview-entry
   gtk-font-chooser-show-preview-entry
   "show-preview-entry" "gboolean" t t))

;;; ----------------------------------------------------------------------------
;;; gtk_font_chooser_get_font_family ()
;;;
;;; PangoFontFamily * gtk_font_chooser_get_font_family
;;;                                               (GtkFontChooser *fontchooser);
;;;
;;; Gets the PangoFontFamily representing the selected font family. Font
;;; families are a collection of font faces.
;;;
;;; If the selected font is not installed, returns NULL.
;;;
;;; fontchooser :
;;;     a GtkFontChooser
;;;
;;; Returns :
;;;     A PangoFontFamily representing the selected font family, or NULL. The
;;;     returned object is owned by fontchooser and must not be modified or
;;;     freed.
;;;
;;; Since 3.2
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_font_chooser_get_font_family" gtk-font-chooser-get-font-family)
    (g-object pango-font-family)
  (fontchooser (g-object gtk-font-chooser)))

(export 'gtk-font-chooser-get-font-family)

;;; ----------------------------------------------------------------------------
;;; gtk_font_chooser_get_font_face ()
;;;
;;; PangoFontFace * gtk_font_chooser_get_font_face
;;;                                               (GtkFontChooser *fontchooser);
;;;
;;; Gets the PangoFontFace representing the selected font group details (i.e.
;;; family, slant, weight, width, etc).
;;;
;;; If the selected font is not installed, returns NULL.
;;;
;;; fontchooser :
;;;     a GtkFontChooser
;;;
;;; Returns :
;;;     A PangoFontFace representing the selected font group details, or NULL.
;;;     The returned object is owned by fontchooser and must not be modified or
;;;     freed.
;;;
;;; Since 3.2
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_font_chooser_get_font_face" gtk-font-chooser-get-font-face)
    (g-object pango-font-face)
  (fontchooser (g-object gtk-font-chooser)))

(export 'gtk-font-chooser-get-font-face)

;;; ----------------------------------------------------------------------------
;;; gtk_font_chooser_get_font_size ()
;;;
;;; gint gtk_font_chooser_get_font_size (GtkFontChooser *fontchooser);
;;;
;;; The selected font size.
;;;
;;; fontchooser :
;;;     a GtkFontChooser
;;;
;;; Returns :
;;;     An integer representing the selected font size, or -1 if no font size
;;;     is selected.
;;;
;;; Since 3.2
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_font_chooser_get_font_size" gtk-font-chooser-get-font-size) :int
  (fontchooser (g-object gtk-font-chooser)))

(export 'gtk-font-chooser-get-font-size)

;;; ----------------------------------------------------------------------------
;;; gtk_font_chooser_get_font ()
;;;
;;; gchar * gtk_font_chooser_get_font (GtkFontChooser *fontchooser);
;;;
;;; Gets the currently-selected font name.
;;;
;;; Note that this can be a different string than what you set with
;;; gtk_font_chooser_set_font(), as the font chooser widget may normalize font
;;; names and thus return a string with a different structure. For example,
;;; "Helvetica Italic Bold 12" could be normalized to
;;; "Helvetica Bold Italic 12".
;;;
;;; Use pango_font_description_equal() if you want to compare two font
;;; descriptions.
;;;
;;; fontchooser :
;;;     a GtkFontChooser
;;;
;;; Returns :
;;;     A string with the name of the current font, or NULL if no font is
;;;     selected. You must free this string with g_free().
;;;
;;; Since 3.2
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-font-chooser-get-font))

(defun gtk-font-chooser-get-font (fontchooser)
  (gtk-font-chooser-font fontchooser))

(export 'gtk-font-chooser-get-font)

;;; ----------------------------------------------------------------------------
;;; gtk_font_chooser_set_font ()
;;;
;;; void gtk_font_chooser_set_font (GtkFontChooser *fontchooser,
;;;                                 const gchar *fontname);
;;;
;;; Sets the currently-selected font.
;;;
;;; fontchooser :
;;;     a GtkFontChooser
;;;
;;; fontname :
;;;     a font name like "Helvetica 12" or "Times Bold 18"
;;;
;;; Since 3.2
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-font-chooser-set-font))

(defun gtk-font-chooser-set-font (fontchooser fontname)
  (setf (gtk-font-chooser-font fontchooser) fontname))

(export 'gtk-font-chooser-set-font)

;;; ----------------------------------------------------------------------------
;;; gtk_font_chooser_get_font_desc ()
;;;
;;; PangoFontDescription * gtk_font_chooser_get_font_desc
;;;                                               (GtkFontChooser *fontchooser);
;;;
;;; Gets the currently-selected font.
;;;
;;; Note that this can be a different string than what you set with
;;; gtk_font_chooser_set_font(), as the font chooser widget may normalize font
;;; names and thus return a string with a different structure. For example,
;;; "Helvetica Italic Bold 12" could be normalized to
;;; "Helvetica Bold Italic 12".
;;;
;;; Use pango_font_description_equal() if you want to compare two font
;;; descriptions.
;;;
;;; fontchooser :
;;;     a GtkFontChooser
;;;
;;; Returns :
;;;     A PangoFontDescription for the current font, or NULL if no font is
;;;     selected.
;;;
;;; Since 3.2
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-font-chooser-get-font-desc))

(defun gtk-font-chooser-get-font-desc (fontchooser)
  (gtk-font-chooser-font-desc fontchooser))

(export 'gtk-font-chooser-get-font-desc)

;;; ----------------------------------------------------------------------------
;;; gtk_font_chooser_set_font_desc ()
;;;
;;; void gtk_font_chooser_set_font_desc (GtkFontChooser *fontchooser,
;;;                                      const PangoFontDescription *font_desc);
;;;
;;; Sets the currently-selected font from font_desc.
;;;
;;; fontchooser :
;;;     a GtkFontChooser
;;;
;;; font_desc :
;;;     a PangoFontDescription
;;;
;;; Since 3.2
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-font-chooser-set-font-desc))

(defun gtk-font-chooser-set-font-desc (fontchooser font-desc)
  (setf (gtk-font-chooser-font-desc fontchooser) font-desc))

(export 'gtk-font-chooser-set-font-desc)

;;; ----------------------------------------------------------------------------
;;; gtk_font_chooser_get_preview_text ()
;;;
;;; gchar * gtk_font_chooser_get_preview_text (GtkFontChooser *fontchooser);
;;;
;;; Gets the text displayed in the preview area.
;;;
;;; fontchooser :
;;;     a GtkFontChooser
;;;
;;; Returns :
;;;     the text displayed in the preview area
;;;
;;; Since 3.2
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-font-chooser-get-preview-text))

(defun gtk-font-chooser-get-preview-text (fontchooser)
  (gtk-font-chooser-preview-text fontchooser))

(export 'gtk-font-chooser-get-preview-text)

;;; ----------------------------------------------------------------------------
;;; gtk_font_chooser_set_preview_text ()
;;;
;;; void gtk_font_chooser_set_preview_text (GtkFontChooser *fontchooser,
;;;                                         const gchar *text);
;;;
;;; Sets the text displayed in the preview area. The text is used to show how
;;; the selected font looks.
;;;
;;; fontchooser :
;;;     a GtkFontChooser
;;;
;;; text :
;;;     the text to display in the preview area
;;;
;;; Since 3.2
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-font-chooser-set-preview-text))

(defun gtk-font-chooser-set-preview-text (fontchooser text)
  (setf (gtk-font-chooser-preview-text fontchooser) text))

(export 'gtk-font-chooser-set-preview-text)

;;; ----------------------------------------------------------------------------
;;; gtk_font_chooser_get_show_preview_entry ()
;;;
;;; gboolean gtk_font_chooser_get_show_preview_entry
;;;                                               (GtkFontChooser *fontchooser);
;;;
;;; Returns whether the preview entry is shown or not.
;;;
;;; fontchooser :
;;;     a GtkFontChooser
;;;
;;; Returns :
;;;     TRUE if the preview entry is shown or FALSE if it is hidden.
;;;
;;; Since 3.2
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-font-chooser-get-show-preview-entry))

(defun gtk-font-chooser-get-show-preview-entry (fontchooser)
  (gtk-font-chooser-show-preview-entry fontchooser))

(export 'gtk-font-chooser-get-show-preview-entry)

;;; ----------------------------------------------------------------------------
;;; gtk_font_chooser_set_show_preview_entry ()
;;;
;;; void gtk_font_chooser_set_show_preview_entry (GtkFontChooser *fontchooser,
;;;                                               gboolean show_preview_entry);
;;;
;;; Shows or hides the editable preview entry.
;;;
;;; fontchooser :
;;;     a GtkFontChooser
;;;
;;; show_preview_entry :
;;;     whether to show the editable preview entry or not
;;;
;;; Since 3.2
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-font-chooser-set-show-preview-entry))

(defun gtk-font-chooser-set-show-preview-entry (fontchooser show-preview-entry)
  (setf (gtk-font-chooser-show-preview-entry fontchooser) show-preview-entry))

(export 'gtk-font-chooser-set-show-preview-entry)

;;; ----------------------------------------------------------------------------
;;; GtkFontFilterFunc ()
;;;
;;; gboolean (*GtkFontFilterFunc) (const PangoFontFamily *family,
;;;                                const PangoFontFace *face,
;;;                                gpointer data);
;;;
;;; The type of function that is used for deciding what fonts get shown in a
;;; GtkFontChooser. See gtk_font_chooser_set_filter_func().
;;;
;;; family :
;;;     a PangoFontFamily
;;;
;;; face :
;;;     a PangoFontFace belonging to family
;;;
;;; data :
;;;     user data passed to gtk_font_chooser_set_filter_func()
;;;
;;; Returns :
;;;     TRUE if the font should be displayed
;;; ----------------------------------------------------------------------------

(defcallback gtk-font-filter-func-cb :boolean
    ((family (g-object pango-font-family))
     (face (g-object pango-font-face))
     (data :pointer))
  (funcall (get-stable-pointer-value data) family face))

;;; ----------------------------------------------------------------------------
;;; gtk_font_chooser_set_filter_func ()
;;;
;;; void gtk_font_chooser_set_filter_func (GtkFontChooser *fontchooser,
;;;                                        GtkFontFilterFunc filter,
;;;                                        gpointer user_data,
;;;                                        GDestroyNotify destroy);
;;;
;;; Adds a filter function that decides which fonts to display in the font
;;; chooser.
;;;
;;; fontchooser :
;;;     a GtkFontChooser
;;;
;;; filter :
;;;     a GtkFontFilterFunc, or NULL
;;;
;;; user_data :
;;;     data to pass to filter
;;;
;;; destroy :
;;;     function to call to free data when it is no longer needed
;;;
;;; Since 3.2
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_font_chooser_set_filter_func" %gtk-font-chooser-set-filter-func)
    :void
  (fontchooser (g-object gtk-font-chooser))
  (filter :pointer)
  (user-data :pointer)
  (destroy :pointer))

(defun gtk-font-chooser-set-filter-func (fontchooser func)
  (%gtk-font-chooser-set-filter-func
                                   fontchooser
                                   (callback gtk-font-filter-func-cb)
                                   (allocate-stable-pointer func)
                                   (callback stable-pointer-destroy-notify-cb)))

(export 'gtk-font-chooser-set-filter-func)

;;; --- End of file gtk.font-chooser.lisp --------------------------------------
