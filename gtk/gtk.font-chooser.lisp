;;; ----------------------------------------------------------------------------
;;; gtk.font-chooser.lisp
;;;
;;; The documentation of this file is taken from the GTK+ 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2012 - 2019 Dieter Kaiser
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
;;;     Interface implemented by widgets displaying fonts
;;;
;;; Types and Values
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
;;;     gtk_font_chooser_set_font_map ()
;;;     gtk_font_chooser_get_font_map ()
;;;     gtk_font_chooser_set_level ()
;;;     gtk_font_chooser_get_level ()
;;;     gtk_font_chooser_get_font_features ()
;;;     gtk_font_chooser_set_language ()
;;;     gtk_font_chooser_get_language ()
;;;
;;; Properties
;;;
;;;                    gchar*  font                  Read / Write
;;;     PangoFontDescription*  font-desc             Read / Write
;;;                    gchar*  font-features         Read
;;;                    gchar*  language              Read / Write
;;;      GtkFontChooserLevel   level                 Read / Write
;;;                    gchar*  preview-text          Read / Write
;;;                 gboolean   show-preview-entry    Read / Write
;;;
;;; Signals
;;;
;;;                     void   font-activated        Run First
;;;
;;; Object Hierarchy
;;;
;;;     GInterface
;;;     ╰── GtkFontChooser
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
  #+gtk-3-22
  (font-features
   gtk-font-chooser-font-features
   "font-features" "gchar" t nil)
  #+gtk-3-22
  (language
   gtk-font-chooser-language
   "language" "gchar" t t)
  #+gtk-3-22
  (level
   gtk-font-chooser-level
   "level" "GtkFontChooserLevel" t t)
  (preview-text
   gtk-font-chooser-preview-text
   "preview-text" "gchar" t t)
  (show-preview-entry
   gtk-font-chooser-show-preview-entry
   "show-preview-entry" "gboolean" t t))

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-font-chooser atdoc:*class-name-alias*) "Interface"
      (documentation 'gtk-font-chooser 'type)
 "@version{2013-6-18}
  @begin{short}
    @sym{gtk-font-chooser} is an interface that can be implemented by widgets
    displaying the list of fonts. In GTK+, the main objects that implement this
    interface are @class{gtk-font-chooser-widget},
    @class{gtk-font-chooser-dialog} and @class{gtk-font-button}.
  @end{short}
  @begin[Signal Details]{dictionary}
    @subheading{The \"font-activated\" signal}
      @begin{pre}
 lambda (fontchooser arg1)   : Run Last
      @end{pre}
  @end{dictionary}
  @see-slot{gtk-font-chooser-font}
  @see-slot{gtk-font-chooser-font-desc}
  @see-slot{gtk-font-chooser-preview-text}
  @see-slot{gtk-font-chooser-show-preview-entry}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk-font-chooser-font --------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "font" 'gtk-font-chooser) 't)
 "The @code{font} property of type @code{:string} (Read / Write) @br{}
  The font description as a string, e. g. \"Sans Italic 12\". @br{}
  Default value: \"Sans 10\"")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-font-chooser-font atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-font-chooser-font 'function)
 "@version{2013-6-18}
  @begin{short}
    Accessor of the slot @slot[gtk-font-chooser]{font} of the
    @class{gtk-font-chooser} class.
  @end{short}
  @see-class{gtk-font-chooser}")

;;; --- gtk-font-chooser-font-desc ---------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "font-desc"
                                               'gtk-font-chooser) 't)
 "The @code{font-desc} property of type @class{pango-font-description}
  (Read / Write) @br{}
  The font description as a @class{pango-font-description}.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-font-chooser-font-desc atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-font-chooser-font-desc 'function)
 "@version{2013-6-18}
  @begin{short}
    Accessor of the slot @slot[gtk-font-chooser]{font-desc} of the
    @class{gtk-font-chooser} class.
  @end{short}
  @see-class{gtk-font-chooser}")

;;; --- gtk-font-chooser-font-features -----------------------------------------

#+(and gtk-3-22 cl-cffi-gtk-documentation)
(setf (documentation (atdoc:get-slot-from-name "font-features"
                                               'gtk-font-chooser) 't)
 "The @code{font-feature} property of type @code{:string} (Read) @br{}
  The selected font features, in a format that is compatible with CSS and with
  Pango attributes. @br{}
  Default value: \"\" @br{}
  Since 3.22")

#+(and gtk-3-22 cl-cffi-gtk-documentation)
(setf (gethash 'gtk-font-chooser-font-features atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-font-chooser-font-features 'function)
 "@version{2019-4-6}
  @begin{short}
    Accessor of the slot @slot[gtk-font-chooser]{font-features} of the
    @class{gtk-font-chooser} class.
  @end{short}
  @see-class{gtk-font-chooser}")

;;; --- gtk-font-chooser-language ----------------------------------------------

#+(and gtk-3-22 cl-cffi-gtk-documentation)
(setf (documentation (atdoc:get-slot-from-name "language"
                                               'gtk-font-chooser) 't)
 "The @code{language} property of type @code{:string} (Read / Write) @br{}
  The language for which the @code{font-features} were selected, in a format
  that is compatible with CSS and with Pango attributes. @br{}
  Default value: \"\" @br{}
  Since 3.22")

#+(and gtk-3-22 cl-cffi-gtk-documentation)
(setf (gethash 'gtk-font-chooser-language atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-font-chooser-language 'function)
 "@version{2019-4-6}
  @begin{short}
    Accessor of the slot @slot[gtk-font-chooser]{language} of the
    @class{gtk-font-chooser} class.
  @end{short}
  @see-class{gtk-font-chooser}")

;;; --- gtk-font-chooser-level -------------------------------------------------

#+(and gtk-3-22 cl-cffi-gtk-documentation)
(setf (documentation (atdoc:get-slot-from-name "level"
                                               'gtk-font-chooser) 't)
 "The @code{level} property of type @sybmol{gtk-font-chooser-level}
  (Read / Write) @br{}
  The level of granularity to offer for selecting fonts. @br{}
  Default value: @code{:style} | @code{:size} @br{}
  Since 3.22")

#+(and gtk-3-22 cl-cffi-gtk-documentation)
(setf (gethash 'gtk-font-chooser-language atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-font-chooser-language 'function)
 "@version{2019-4-6}
  @begin{short}
    Accessor of the slot @slot[gtk-font-chooser]{level} of the
    @class{gtk-font-chooser} class.
  @end{short}
  @see-class{gtk-font-chooser}")

;;; --- gtk-font-chooser-preview-text ------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "preview-text"
                                               'gtk-font-chooser) 't)
 "The @code{preview-text} property of type @code{:string} (Read / Write) @br{}
  The string with which to preview the font. @br{}
  Default value: \"The quick brown fox jumps over the lazy dog.\"")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-font-chooser-preview-text atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-font-chooser-preview-text 'function)
 "@version{2013-6-18}
  @begin{short}
    Accessor of the slot @slot[gtk-font-chooser]{preview-text} of the
    @class{gtk-font-chooser} class.
  @end{short}
  @see-class{gtk-font-chooser}")

;;; --- gtk-font-chooser-show-preview-entry ------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "show-preview-entry"
                                               'gtk-font-chooser) 't)
 "The @code{show-preview-entry} property of type @code{:boolean}
  (Read / Write) @br{}
  Whether to show an entry to change the preview text. @br{}
  Default value: @em{true}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-font-chooser-show-preview-entry atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-font-chooser-show-preview-entry 'function)
 "@version{2013-6-18}
  @begin{short}
    Accessor of the slot @slot[gtk-font-chooser]{show-preview-entry} of the
    @class{gtk-font-chooser} class.
  @end{short}
  @see-class{gtk-font-chooser}")

;;; ----------------------------------------------------------------------------
;;; gtk_font_chooser_get_font_family ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_font_chooser_get_font_family" gtk-font-chooser-get-font-family)
    (g-object pango-font-family)
 #+cl-cffi-gtk-documentation
 "@version{2013-6-18}
  @argument[fontchooser]{a @class{gtk-font-chooser} object}
  @begin{return}
    A @class{pango-font-family} representing the selected font family, or
    @code{nil}. The returned object is owned by @arg{fontchooser} and must not
    be modified or freed.
  @end{return}
  @begin{short}
    Gets the @class{pango-font-family} representing the selected font family.
    Font families are a collection of font faces.
  @end{short}

  If the selected font is not installed, returns @code{nil}.

  Since 3.2"
  (fontchooser (g-object gtk-font-chooser)))

(export 'gtk-font-chooser-get-font-family)

;;; ----------------------------------------------------------------------------
;;; gtk_font_chooser_get_font_face ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_font_chooser_get_font_face" gtk-font-chooser-get-font-face)
    (g-object pango-font-face)
 #+cl-cffi-gtk-documentation
 "@version{2013-6-18}
  @argument[fontchooser]{a @class{gtk-font-chooser} object}
  @begin{return}
    A @class{pango-font-face} representing the selected font group details, or
    @code{nil}. The returned object is owned by @arg{fontchooser} and must not
    be modified or freed.
  @end{return}
  @begin{short}
    Gets the @class{pango-font-face} representing the selected font group
    details (i. e. family, slant, weight, width, etc).
  @end{short}

  If the selected font is not installed, returns @code{nil}.

  Since 3.2"
  (fontchooser (g-object gtk-font-chooser)))

(export 'gtk-font-chooser-get-font-face)

;;; ----------------------------------------------------------------------------
;;; gtk_font_chooser_get_font_size ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_font_chooser_get_font_size" gtk-font-chooser-get-font-size) :int
 #+cl-cffi-gtk-documentation
 "@version{2013-6-18}
  @argument[fontchooser]{a @class{gtk-font-chooser} object}
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
 "@version{2013-6-18}
  @argument[fontchooser]{a @class{gtk-font-chooser} object}
  @begin{return}
    A string with the name of the current font, or @code{nil} if no font is
    selected.
  @end{return}
  @begin{short}
    Gets the currently-selected font name.
  @end{short}

  Note that this can be a different string than what you set with the function
  @fun{gtk-font-chooser-set-font}, as the font chooser widget may normalize font
  names and thus return a string with a different structure. For example,
  \"Helvetica Italic Bold 12\" could be normalized to
  \"Helvetica Bold Italic 12\".

  Use the function @fun{pango-font-description-equal} if you want to compare
  two font descriptions.

  Since 3.2
  @see-fun{pango-font-description-equal}"
  (gtk-font-chooser-font fontchooser))

(export 'gtk-font-chooser-get-font)

;;; ----------------------------------------------------------------------------
;;; gtk_font_chooser_set_font ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-font-chooser-set-font))

(defun gtk-font-chooser-set-font (fontchooser fontname)
 #+cl-cffi-gtk-documentation
 "@version{2013-6-18}
  @argument[fontchooser]{a @class{gtk-font-chooser} object}
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
 "@version{2013-6-18}
  @argument[fontchooser]{a @class{gtk-font-chooser} object}
  @begin{return}
    A @class{pango-font-description} for the current font, or @code{nil} if no
    font is selected.
  @end{return}
  @begin{short}
    Gets the currently-selected font.
  @end{short}

  Note that this can be a different string than what you set with the function
  @fun{gtk-font-chooser-set-font}, as the font chooser widget may normalize font
  names and thus return a string with a different structure. For example,
  \"Helvetica Italic Bold 12\" could be normalized to
  \"Helvetica Bold Italic 12\".

  Use the function @fun{pango-font-description-equal} if you want to compare
  two font descriptions.

  Since 3.2
  @see-function{gtk-font-chooser-set-font}
  @see-function{pango-font-description-equal}"
  (gtk-font-chooser-font-desc fontchooser))

(export 'gtk-font-chooser-get-font-desc)

;;; ----------------------------------------------------------------------------
;;; gtk_font_chooser_set_font_desc ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-font-chooser-set-font-desc))

(defun gtk-font-chooser-set-font-desc (fontchooser font-desc)
 #+cl-cffi-gtk-documentation
 "@version{2013-6-18}
  @argument[fontchooser]{a @class{gtk-font-chooser} object}
  @argument[font-desc]{a @class{pango-font-description} object}
  @begin{short}
    Sets the currently-selected font from @arg{font-desc}.
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
 "@version{2013-6-18}
  @argument[fontchooser]{a @class{gtk-font-chooser} object}
  @return{The text displayed in the preview area.}
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
 "@version{2013-6-18}
  @argument[fontchooser]{a @class{gtk-font-chooser} object}
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
 "@version{2013-6-18}
  @argument[fontchooser]{a @class{gtk-font-chooser} object}
  @return{@em{True} if the preview entry is shown or @code{nil} if it is
    hidden.}
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
 "@version{2013-6-18}
  @argument[fontchooser]{a @class{gtk-font-chooser} object}
  @argument[show-preview-entry]{whether to show the editable preview entry or
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
 "@version{2013-6-18}
  @argument[fontchooser]{a @class{gtk-font-chooser} object}
  @argument[filter]{a @code{GtkFontFilterFunc}, or @code{nil}}
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

#|

gtk_font_chooser_set_font_map ()
void
gtk_font_chooser_set_font_map (GtkFontChooser *fontchooser,
                               PangoFontMap *fontmap);
Sets a custom font map to use for this font chooser widget. A custom font map can be used to present application-specific fonts instead of or in addition to the normal system fonts.

FcConfig *config;
PangoFontMap *fontmap;

config = FcInitLoadConfigAndFonts ();
FcConfigAppFontAddFile (config, my_app_font_file);

fontmap = pango_cairo_font_map_new_for_font_type (CAIRO_FONT_TYPE_FT);
pango_fc_font_map_set_config (PANGO_FC_FONT_MAP (fontmap), config);

gtk_font_chooser_set_font_map (font_chooser, fontmap);
Note that other GTK+ widgets will only be able to use the application-specific font if it is present in the font map they use:

context = gtk_widget_get_pango_context (label);
pango_context_set_font_map (context, fontmap);
Parameters
fontchooser

a GtkFontChooser


fontmap

a PangoFontMap.

[allow-none]
Since: 3.18

gtk_font_chooser_get_font_map ()
PangoFontMap *
gtk_font_chooser_get_font_map (GtkFontChooser *fontchooser);
Gets the custom font map of this font chooser widget, or NULL if it does not have one.

Parameters
fontchooser

a GtkFontChooser


Returns
a PangoFontMap, or NULL.

[nullable][transfer full]

Since: 3.18

gtk_font_chooser_set_level ()
void
gtk_font_chooser_set_level (GtkFontChooser *fontchooser,
                            GtkFontChooserLevel level);
Sets the desired level of granularity for selecting fonts.

Parameters
fontchooser

a GtkFontChooser


level

the desired level of granularity


Since: 3.24

gtk_font_chooser_get_level ()
GtkFontChooserLevel
gtk_font_chooser_get_level (GtkFontChooser *fontchooser);
Returns the current level of granularity for selecting fonts.

Parameters
fontchooser

a GtkFontChooser


Returns
the current granularity level

Since: 3.24

gtk_font_chooser_get_font_features ()
char *
gtk_font_chooser_get_font_features (GtkFontChooser *fontchooser);
Gets the currently-selected font features.

Parameters
fontchooser

a GtkFontChooser


Returns
the currently selected font features

Since: 3.24

gtk_font_chooser_set_language ()
void
gtk_font_chooser_set_language (GtkFontChooser *fontchooser,
                               const char *language);
Sets the language to use for font features.

Parameters
fontchooser

a GtkFontChooser


language

a language


Since: 3.24

gtk_font_chooser_get_language ()
char *
gtk_font_chooser_get_language (GtkFontChooser *fontchooser);
Gets the language that is used for font features.

Parameters
fontchooser

a GtkFontChooser


Returns
the currently selected language

Since: 3.24

|#
;;; --- End of file gtk.font-chooser.lisp --------------------------------------
