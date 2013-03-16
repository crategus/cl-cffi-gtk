;;; ----------------------------------------------------------------------------
;;; gtk.font-chooser.lisp
;;;
;;; The documentation has been copied from the GTK+ 3 Reference Manual
;;; Version 3.4.3. See <http://www.gtk.org>. The API documentation of the
;;; Lisp Binding is available at <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2012, 2013 Dieter Kaiser
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
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkFontChooser
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

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-font-chooser atdoc:*class-name-alias*) "Interface"
      (documentation 'gtk-font-chooser 'type)
 "@version{2013-3-6}
  @begin{short}
    GtkFontChooser is an interface that can be implemented by widgets displaying
    the list of fonts. In GTK+, the main objects that implement this interface
    are GtkFontChooserWidget, GtkFontChooserDialog and GtkFontButton.
  @end{short}
  @begin[Signal Details]{dictionary}
    @subheading{The \"font-activated\" signal}
      @begin{pre}
 void user_function (GtkFontChooser *fontchooser,
                     gchar          *arg1,
                     gpointer        user_data)        : Run First
      @end{pre}
  @end{dictionary}
  @see-slot{gtk-font-chooser-font}
  @see-slot{gtk-font-chooser-font-desc}
  @see-slot{gtk-font-chooser-preview-text}
  @see-slot{gtk-font-chooser-show-preview-entry}")

;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "font" 'gtk-font-chooser) 't)
 "The \"font\" property of type @code{gchar*} (Read / Write)@br{}
  The font description as a string, e.g. \"Sans Italic 12\".@br{}
  Default value: \"Sans 10\"")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "font-desc" 'gtk-font-chooser) 't)
 "The @code{\"font-desc\"} property of type @code{PangoFontDescription*}
  (Read / Write)@br{}
  The font description as a PangoFontDescription.")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "preview-text" 'gtk-font-chooser) 't)
 "The \"preview-text\" property of type @code{gchar*} (Read / Write)@br{}
  The string with which to preview the font.@br{}
  Default value: \"The quick brown fox jumps over the lazy dog.\"")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "show-preview-entry" 'gtk-font-chooser) 't)
 "The @code{\"show-preview-entry\"} property of type @code{gboolean}
  (Read / Write)@br{}
  Whether to show an entry to change the preview text.@br{}
  Default value: TRUE")

;;; ----------------------------------------------------------------------------
;;;
;;; Accessors
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-font-chooser-font atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-font-chooser-font 'function)
 "@version{2013-3-6}
  @begin{short}
    Accessor of the slot @code{\"font\"} of the @class{gtk-font-chooser} class.
  @end{short}")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-font-chooser-font-desc atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-font-chooser-font-desc 'function)
 "@version{2013-3-6}
  @begin{short}
    Accessor of the slot @code{\"font-desc\"} of the @class{gtk-font-chooser}
    class.
  @end{short}")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-font-chooser-preview-text atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-font-chooser-preview-text 'function)
 "@version{2013-3-6}
  @begin{short}
    Accessor of the slot @code{\"preview-text\"} of the @class{gtk-font-chooser}
    class.
  @end{short}")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-font-chooser-show-preview-entry atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-font-chooser-show-preview-entry 'function)
 "@version{2013-3-6}
  @begin{short}
    Accessor of the slot @code{\"show-preview-entry\"} of the
    @class{gtk-font-chooser} class.
  @end{short}")

;;; ----------------------------------------------------------------------------
;;; gtk_font_chooser_get_font_family ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_font_chooser_get_font_family" gtk-font-chooser-get-font-family)
    (g-object pango-font-family)
 #+cl-cffi-gtk-documentation
 "@version{2013-3-6}
  @argument[fontchooser]{a GtkFontChooser}
  @begin{return}
    A PangoFontFamily representing the selected font family, or NULL. The
    returned object is owned by fontchooser and must not be modified or freed.
  @end{return}
  @begin{short}
    Gets the PangoFontFamily representing the selected font family. Font
    families are a collection of font faces.
  @end{short}

  If the selected font is not installed, returns NULL.

  Since 3.2"
  (fontchooser (g-object gtk-font-chooser)))

(export 'gtk-font-chooser-get-font-family)

;;; ----------------------------------------------------------------------------
;;; gtk_font_chooser_get_font_face ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_font_chooser_get_font_face" gtk-font-chooser-get-font-face)
    (g-object pango-font-face)
 #+cl-cffi-gtk-documentation
 "@version{2013-3-6}
  @argument[fontchooser]{a GtkFontChooser}
  @begin{return}
    A PangoFontFace representing the selected font group details, or NULL.
    The returned object is owned by fontchooser and must not be modified or
    freed.
  @end{return}
  @begin{short}
    Gets the PangoFontFace representing the selected font group details (i.e.
    family, slant, weight, width, etc).
  @end{short}

  If the selected font is not installed, returns NULL.

  Since 3.2"
  (fontchooser (g-object gtk-font-chooser)))

(export 'gtk-font-chooser-get-font-face)

;;; ----------------------------------------------------------------------------
;;; gtk_font_chooser_get_font_size ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_font_chooser_get_font_size" gtk-font-chooser-get-font-size) :int
 #+cl-cffi-gtk-documentation
 "@version{2013-3-6}
  @argument[fontchooser]{a GtkFontChooser}
  @begin{return}
    An integer representing the selected font size, or -1 if no font size
    is selected.
  @end{return}
  @begin{short}
    The selected font size.
  @end{short}

  Since 3.2"
  (fontchooser (g-object gtk-font-chooser)))

(export 'gtk-font-chooser-get-font-size)

;;; ----------------------------------------------------------------------------
;;; gtk_font_chooser_get_font ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-font-chooser-get-font))

(defun gtk-font-chooser-get-font (fontchooser)
 #+cl-cffi-gtk-documentation
 "@version{2013-3-6}
  @argument[fontchooser]{a GtkFontChooser}
  @begin{return}
    A string with the name of the current font, or NULL if no font is
    selected. You must free this string with g_free().
  @end{return}
  @begin{short}
    Gets the currently-selected font name.
  @end{short}

  Note that this can be a different string than what you set with
  gtk_font_chooser_set_font(), as the font chooser widget may normalize font
  names and thus return a string with a different structure. For example,
  \"Helvetica Italic Bold 12\" could be normalized to
  \"Helvetica Bold Italic 12\".

  Use pango_font_description_equal() if you want to compare two font
  descriptions.

  Since 3.2"
  (gtk-font-chooser-font fontchooser))

(export 'gtk-font-chooser-get-font)

;;; ----------------------------------------------------------------------------
;;; gtk_font_chooser_set_font ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-font-chooser-set-font))

(defun gtk-font-chooser-set-font (fontchooser fontname)
 #+cl-cffi-gtk-documentation
 "@version{2013-3-6}
  @argument[fontchooser]{a GtkFontChooser}
  @argument[fontname]{a font name like \"Helvetica 12\" or \"Times Bold 18\"}
  @begin{short}
    Sets the currently-selected font.
  @end{short}

  Since 3.2"
  (setf (gtk-font-chooser-font fontchooser) fontname))

(export 'gtk-font-chooser-set-font)

;;; ----------------------------------------------------------------------------
;;; gtk_font_chooser_get_font_desc ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-font-chooser-get-font-desc))

(defun gtk-font-chooser-get-font-desc (fontchooser)
 #+cl-cffi-gtk-documentation
 "@version{2013-3-6}
  @argument[fontchooser]{a GtkFontChooser}
  @begin{return}
    A PangoFontDescription for the current font, or NULL if no font is
    selected.
  @end{return}
  @begin{short}
    Gets the currently-selected font.
  @end{short}

  Note that this can be a different string than what you set with
  gtk_font_chooser_set_font(), as the font chooser widget may normalize font
  names and thus return a string with a different structure. For example,
  \"Helvetica Italic Bold 12\" could be normalized to
  \"Helvetica Bold Italic 12\".

  Use pango_font_description_equal() if you want to compare two font
  descriptions.

  Since 3.2"
  (gtk-font-chooser-font-desc fontchooser))

(export 'gtk-font-chooser-get-font-desc)

;;; ----------------------------------------------------------------------------
;;; gtk_font_chooser_set_font_desc ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-font-chooser-set-font-desc))

(defun gtk-font-chooser-set-font-desc (fontchooser font-desc)
 #+cl-cffi-gtk-documentation
 "@version{2013-3-6}
  @argument[fontchooser]{a GtkFontChooser}
  @argument[font_desc]{a PangoFontDescription}
  @begin{short}
    Sets the currently-selected font from font_desc.
  @end{short}

  Since 3.2"
  (setf (gtk-font-chooser-font-desc fontchooser) font-desc))

(export 'gtk-font-chooser-set-font-desc)

;;; ----------------------------------------------------------------------------
;;; gtk_font_chooser_get_preview_text ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-font-chooser-get-preview-text))

(defun gtk-font-chooser-get-preview-text (fontchooser)
 #+cl-cffi-gtk-documentation
 "@version{2013-3-6}
  @argument[fontchooser]{a GtkFontChooser}
  @return{the text displayed in the preview area}
  @short{Gets the text displayed in the preview area.}

  Since 3.2"
  (gtk-font-chooser-preview-text fontchooser))

(export 'gtk-font-chooser-get-preview-text)

;;; ----------------------------------------------------------------------------
;;; gtk_font_chooser_set_preview_text ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-font-chooser-set-preview-text))

(defun gtk-font-chooser-set-preview-text (fontchooser text)
 #+cl-cffi-gtk-documentation
 "@version{2013-3-6}
  @argument[fontchooser]{a GtkFontChooser}
  @argument[text]{the text to display in the preview area}
  @begin{short}
    Sets the text displayed in the preview area. The text is used to show how
    the selected font looks.
  @end{short}

  Since 3.2"
  (setf (gtk-font-chooser-preview-text fontchooser) text))

(export 'gtk-font-chooser-set-preview-text)

;;; ----------------------------------------------------------------------------
;;; gtk_font_chooser_get_show_preview_entry ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-font-chooser-get-show-preview-entry))

(defun gtk-font-chooser-get-show-preview-entry (fontchooser)
 #+cl-cffi-gtk-documentation
 "@version{2013-3-6}
  @argument[fontchooser]{a GtkFontChooser}
  @return{TRUE if the preview entry is shown or FALSE if it is hidden.}
  @short{Returns whether the preview entry is shown or not.}

  Since 3.2"
  (gtk-font-chooser-show-preview-entry fontchooser))

(export 'gtk-font-chooser-get-show-preview-entry)

;;; ----------------------------------------------------------------------------
;;; gtk_font_chooser_set_show_preview_entry ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-font-chooser-set-show-preview-entry))

(defun gtk-font-chooser-set-show-preview-entry (fontchooser show-preview-entry)
 #+cl-cffi-gtk-documentation
 "@version{2013-3-6}
  @argument[fontchooser]{a GtkFontChooser}
  @argument[show_preview_entry]{whether to show the editable preview entry or
    not}
  @short{Shows or hides the editable preview entry.}

  Since 3.2"
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
  (funcall (glib::get-stable-pointer-value data) family face))

;;; ----------------------------------------------------------------------------
;;; gtk_font_chooser_set_filter_func ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_font_chooser_set_filter_func" %gtk-font-chooser-set-filter-func)
    :void
  (fontchooser (g-object gtk-font-chooser))
  (filter :pointer)
  (user-data :pointer)
  (destroy :pointer))

(defun gtk-font-chooser-set-filter-func (fontchooser func)
 #+cl-cffi-gtk-documentation
 "@version{2013-3-6}
  @argument[fontchooser]{a GtkFontChooser}
  @argument[filter]{a GtkFontFilterFunc, or NULL}
  @argument[user_data]{data to pass to filter}
  @argument[destroy]{function to call to free data when it is no longer needed}
  @begin{short}
    Adds a filter function that decides which fonts to display in the font
    chooser.
  @end{short}

  Since 3.2"
  (%gtk-font-chooser-set-filter-func
                             fontchooser
                             (callback gtk-font-filter-func-cb)
                             (glib::allocate-stable-pointer func)
                             (callback glib::stable-pointer-destroy-notify-cb)))

(export 'gtk-font-chooser-set-filter-func)

;;; --- End of file gtk.font-chooser.lisp --------------------------------------
