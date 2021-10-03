;;; ----------------------------------------------------------------------------
;;; gtk.font-chooser.lisp
;;;
;;; The documentation of this file is taken from the GTK+ 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GTK+ library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2012 - 2021 Dieter Kaiser
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
;;;     GtkFontChooserLevel
;;;     GtkFontChooser
;;;
;;; Functions
;;;
;;;     gtk_font_chooser_get_font_family
;;;     gtk_font_chooser_get_font_face
;;;     gtk_font_chooser_get_font_size
;;;     gtk_font_chooser_get_font                          Accessor
;;;     gtk_font_chooser_set_font                          Accessor
;;;     gtk_font_chooser_get_font_desc                     Accessor
;;;     gtk_font_chooser_set_font_desc                     Accessor
;;;     gtk_font_chooser_get_preview_text                  Accessor
;;;     gtk_font_chooser_set_preview_text                  Accessor
;;;     gtk_font_chooser_get_show_preview_entry            Accessor
;;;     gtk_font_chooser_set_show_preview_entry            Accessor
;;;     gtk_font_chooser_set_filter_func
;;;
;;;     gtk_font_chooser_set_font_map
;;;     gtk_font_chooser_get_font_map
;;;     gtk_font_chooser_set_level                         Accessor
;;;     gtk_font_chooser_get_level                         Accessor
;;;     gtk_font_chooser_get_font_features                 Accessor
;;;     gtk_font_chooser_set_language                      Accessor
;;;     gtk_font_chooser_get_language                      Accessor
;;;
;;; Properties
;;;
;;;                gchar*   font                  Read / Write
;;; PangoFontDescription*   font-desc             Read / Write
;;;                gchar*   font-features         Read
;;;                gchar*   language              Read / Write
;;;  GtkFontChooserLevel    level                 Read / Write
;;;                gchar*   preview-text          Read / Write
;;;             gboolean    show-preview-entry    Read / Write
;;;
;;; Signals
;;;
;;;                 void    font-activated        Run First
;;;
;;; Object Hierarchy
;;;
;;;     GInterface
;;;     ╰── GtkFontChooser
;;;
;;; Prerequisites
;;;
;;;     GtkFontChooser requires GObject.
;;;
;;; Known Implementations
;;;
;;;     GtkFontChooser is implemented by GtkFontButton, GtkFontChooserDialog
;;;     and GtkFontChooserWidget.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkFontChooserLevel
;;; ----------------------------------------------------------------------------

(define-g-flags "GtkFontChooserLevel" gtk-font-chooser-level
  (:export t
   :type-initializer "gtk_font_chooser_level_get_type")
  (:family 0)
  (:style      #.(ash 1 0))
  (:size       #.(ash 1 1))
  (:variations #.(ash 1 2))
  (:features   #.(ash 1 3)))

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-font-chooser-level atdoc:*symbol-name-alias*)
      "Flags"
      (gethash 'gtk-font-chooser-level atdoc:*external-symbols*)
 "@version{2021-1-20}
  @begin{short}
    The @sym{gtk-font-chooser-level} flags specifies the granularity of font
    selection that is desired in a font chooser.
  @end{short}

  These flags may be extended in the future. Applications should ignore
  unknown values.
  @begin{pre}
(define-g-flags \"GtkFontChooserLevel\" gtk-font-chooser-level
  (:export t
   :type-initializer \"gtk_font_chooser_level_get_type\")
  (:family 0)
  (:style      #.(ash 1 0))
  (:size       #.(ash 1 1))
  (:variations #.(ash 1 2))
  (:features   #.(ash 1 3)))
  @end{pre}
  @begin[code]{table}
    @entry[:family]{Allow selecting a font family.}
    @entry[:style]{Allow selecting a specific font face.}
    @entry[:size]{Allow selecting a specific font size.}
    @entry[:variations]{Allow changing OpenType font variation axes.}
    @entry[:features]{Allow selecting specific OpenType font features.}
  @end{table}
  @see-class{gtk-font-chooser}")

;;; ----------------------------------------------------------------------------
;;; GtkFontChooser
;;; ----------------------------------------------------------------------------

(define-g-interface "GtkFontChooser" gtk-font-chooser
  (:export t
   :type-initializer "gtk_font_chooser_get_type")
  (font
   gtk-font-chooser-font
   "font" "gchararray" t t)
  (font-desc
   gtk-font-chooser-font-desc
   "font-desc" "PangoFontDescription" t t)
  #+gtk-3-22
  (font-features
   gtk-font-chooser-font-features
   "font-features" "gchararray" t nil)
  #+gtk-3-22
  (language
   gtk-font-chooser-language
   "language" "gchararray" t t)
  #+gtk-3-22
  (level
   gtk-font-chooser-level
   "level" "GtkFontChooserLevel" t t)
  (preview-text
   gtk-font-chooser-preview-text
   "preview-text" "gchararray" t t)
  (show-preview-entry
   gtk-font-chooser-show-preview-entry
   "show-preview-entry" "gboolean" t t))

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-font-chooser atdoc:*class-name-alias*) "Interface"
      (documentation 'gtk-font-chooser 'type)
 "@version{2021-1-20}
  @begin{short}
    The @sym{gtk-font-chooser} interface is an interface that can be
    implemented by widgets displaying the list of fonts.
  @end{short}
  In GTK+, the main widgets that implement this interface are
  @class{gtk-font-chooser-widget}, @class{gtk-font-chooser-dialog} and
  @class{gtk-font-button}.
  @begin[Signal Details]{dictionary}
    @subheading{The \"font-activated\" signal}
      @begin{pre}
 lambda (fontchooser fontname)    :run-first
      @end{pre}
      Emitted when a font is activated. This usually happens when the user
      double clicks an item, or an item is selected and the user presses one
      of the @kbd{Space}, @kbd{Shift+Space}, @kbd{Return} or @kbd{Enter} keys.
      @begin[code]{table}
        @entry[fontchooser]{The @sym{gtk-font-chooser} widget which received
          the signal.}
        @entry[fontname]{A string with the font name.}
      @end{table}
  @end{dictionary}
  @see-slot{gtk-font-chooser-font}
  @see-slot{gtk-font-chooser-font-desc}
  @see-slot{gtk-font-chooser-font-features}
  @see-slot{gtk-font-chooser-language}
  @see-slot{gtk-font-chooser-level}
  @see-slot{gtk-font-chooser-preview-text}
  @see-slot{gtk-font-chooser-show-preview-entry}
  @see-class{gtk-font-button}
  @see-class{gtk-font-chooser-dialog}
  @see-class{gtk-font-chooser-widget}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk-font-chooser-font --------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "font" 'gtk-font-chooser) 't)
 "The @code{font} property of type @code{:string} (Read / Write) @br{}
  The font description as a string, e.g. \"Sans Italic 12\". @br{}
  Default value: \"Sans 12\"")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-font-chooser-font atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-font-chooser-font 'function)
 "@version{*2021-2-11}
  @syntax[]{(gtk-font-chooser-font object) => fontname}
  @syntax[]{(setf (gtk-font-chooser-font object) fontname)}
  @argument[object]{a @class{gtk-font-chooser} object}
  @argument[fontname]{a string with the font name like \"Helvetica 12\" or
    \"Times Bold 18\"}
  @begin{short}
    Accessor of the @slot[gtk-font-chooser]{font} slot of the
    @class{gtk-font-chooser} class.
  @end{short}

  The slot access function @sym{gtk-font-chooser-font} gets the currently
  selected font name. The slot access function
  @sym{(setf gtk-font-chooser-font)} sets the font name.

  Note that this can be a different string than what you set with the function
  @sym{(setf gtk-font-chooser-font)}, as the font chooser widget may normalize
  the font names and thus return a string with a different structure. For
  example, \"Helvetica Italic Bold 12\" could be normalized to
  \"Helvetica Bold Italic 12\".

  Use the function @fun{pango-font-description-equal} if you want to compare
  two font descriptions.
  @see-class{gtk-font-chooser}
  @see-function{pango-font-description-equal}")

;;; --- gtk-font-chooser-font-desc ---------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "font-desc"
                                               'gtk-font-chooser) 't)
 "The @code{font-desc} property of type @class{pango-font-description}
  (Read / Write) @br{}
  The Pango font description.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-font-chooser-font-desc atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-font-chooser-font-desc 'function)
 "@version{*2021-1-20}
  @syntax[]{(gtk-font-chooser-font-desc object) => font-desc}
  @syntax[]{(setf (gtk-font-chooser-font-desc object) font-desc)}
  @argument[object]{a @class{gtk-font-chooser} object}
  @argument[font-desc]{a @class{pango-font-description} instance}
  @begin{short}
    Accessor of the @slot[gtk-font-chooser]{font-desc} slot of the
    @class{gtk-font-chooser} class.
  @end{short}

  The slot access function @sym{gtk-font-chooser-font-desc} gets the Pango
  font description for the currently selected font. The slot access function
  @sym{(setf gtk-font-chooser-font-desc)} sets the currently selected font.

  Use the function @fun{pango-font-description-equal} if you want to compare
  two Pango font descriptions.
  @see-class{gtk-font-chooser}
  @see-class{pango-font-description}
  @see-function{pango-font-description-equal}")

;;; --- gtk-font-chooser-font-features -----------------------------------------

#+(and gtk-3-22 cl-cffi-gtk-documentation)
(setf (documentation (atdoc:get-slot-from-name "font-features"
                                               'gtk-font-chooser) 't)
 "The @code{font-features} property of type @code{:string} (Read) @br{}
  The selected font features, in a format that is compatible with CSS and with
  Pango attributes. Since 3.22 @br{}
  Default value: @code{nil}")

#+(and gtk-3-22 cl-cffi-gtk-documentation)
(setf (gethash 'gtk-font-chooser-font-features atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-font-chooser-font-features 'function)
 "@version{2021-1-20}
  @syntax[]{(gtk-font-chooser-font-features object) => features}
  @argument[object]{a @class{gtk-font-chooser} object}
  @argument[features]{a string with the currently selected font features}
  @begin{short}
    Accessor of the @slot[gtk-font-chooser]{font-features} slot of the
    @class{gtk-font-chooser} class.
  @end{short}

  The slot access function @sym{gtk-font-chooser-font-features} gets the
  currently selected font features, in a format that is compatible with CSS
  and with Pango attributes. The function
  @sym{(setf gtk-font-chooser-font-features)} sets the font features.

  Since 3.22
  @see-class{gtk-font-chooser}")

;;; --- gtk-font-chooser-language ----------------------------------------------

#+(and gtk-3-22 cl-cffi-gtk-documentation)
(setf (documentation (atdoc:get-slot-from-name "language"
                                               'gtk-font-chooser) 't)
 "The @code{language} property of type @code{:string} (Read / Write) @br{}
  The language for which the @code{font-features} property were selected, in a
  format that is compatible with CSS and with Pango attributes. Since 3.22")

#+(and gtk-3-22 cl-cffi-gtk-documentation)
(setf (gethash 'gtk-font-chooser-language atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-font-chooser-language 'function)
 "@version{2021-1-20}
  @syntax[]{(gtk-font-chooser-language object) => language}
  @syntax[]{(setf (gtk-font-chooser-language object) language)}
  @argument[object]{a @class{gtk-font-chooser} object}
  @argument[language]{a RFC-3066 format string representing the language}
  @begin{short}
    Accessor of the @slot[gtk-font-chooser]{language} slot of the
    @class{gtk-font-chooser} class.
  @end{short}

  The slot access function @sym{gtk-font-chooser-language} gets the language
  that is used for font features. The slot access function
  @sym{(setf gtk-font-chooser-language)} sets the language. See the
  function @fun{pango-language-to-string}.

  Since 3.22
  @begin[Example]{dictionary}
    @begin{pre}
(gtk-font-chooser-language (make-instance 'gtk-font-button)) => \"de-de\"
    @end{pre}
  @end{dictionary}
  @see-class{gtk-font-chooser}
  @see-function{pango-language-to-string}")

;;; --- gtk-font-chooser-level -------------------------------------------------

#+(and gtk-3-22 cl-cffi-gtk-documentation)
(setf (documentation (atdoc:get-slot-from-name "level"
                                               'gtk-font-chooser) 't)
 "The @code{level} property of type @symbol{gtk-font-chooser-level}
  (Read / Write) @br{}
  The level of granularity to offer for selecting fonts. Since 3.22 @br{}
  Default value: @code{(:STYLE :SIZE)}")

#+(and gtk-3-22 cl-cffi-gtk-documentation)
(setf (gethash 'gtk-font-chooser-level atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-font-chooser-level 'function)
 "@version{2021-1-20}
  @syntax[]{(gtk-font-chooser-level object) => level}
  @syntax[]{(setf (gtk-font-chooser-level object) level)}
  @argument[object]{a @class{gtk-font-chooser} object}
  @argument[level]{the desired level of granularity of type
    @symbol{gtk-font-chooser-level}}
  @begin{short}
    Accessor of the @slot[gtk-font-chooser]{level} slot of the
    @class{gtk-font-chooser} class.
  @end{short}

  The slot access function @sym{gtk-font-chooser-level} returns the current
  level of granularity for selecting fonts. The slot access function
  @sym{(setf gtk-font-chooser-level)} sets the desired level of granularity for
  selecting fonts.

  Since 3.22
  @see-class{gtk-font-chooser}
  @see-symbol{gtk-font-chooser-level}")

;;; --- gtk-font-chooser-preview-text ------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "preview-text"
                                               'gtk-font-chooser) 't)
 "The @code{preview-text} property of type @code{:string} (Read / Write) @br{}
  The string with which to preview the font. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-font-chooser-preview-text atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-font-chooser-preview-text 'function)
 "@version{2021-1-20}
  @syntax[]{(gtk-font-chooser-preview-text object) => text}
  @syntax[]{(setf (gtk-font-chooser-preview-text object) text)}
  @argument[object]{a @class{gtk-font-chooser} object}
  @argument[text]{a string with the text to display in the preview area}
  @begin{short}
    Accessor of the @slot[gtk-font-chooser]{preview-text} slot of the
    @class{gtk-font-chooser} class.
  @end{short}

  The slot access function @sym{gtk-font-chooser-preview-text} gets the text
  displayed in the preview area. The slot access function
  @sym{(setf gtk-font-chooser-preview-text)} sets the text displayed in the
  preview area. The text is used to show how the selected font looks. See the
  funcion @fun{pango-language-sample-string}.
  @see-class{gtk-font-chooser}
  @see-function{pango-language-sample-string}")

;;; --- gtk-font-chooser-show-preview-entry ------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "show-preview-entry"
                                               'gtk-font-chooser) 't)
 "The @code{show-preview-entry} property of type @code{:boolean} (Read / Write)
  @br{}
  Whether to show an entry to change the preview text. @br{}
  Default value: @em{true}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-font-chooser-show-preview-entry atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-font-chooser-show-preview-entry 'function)
 "@version{2020-6-5}
  @syntax[]{(gtk-font-chooser-show-preview-entry object) => show-entry}
  @syntax[]{(setf (gtk-font-chooser-show-preview-entry object) show-entry)}
  @argument[object]{a @class{gtk-font-chooser} object}
  @argument[show-entry]{a boolean whether to show the editable preview entry or
    not}
  @begin{short}
    Accessor of the @slot[gtk-font-chooser]{show-preview-entry} slot of the
    @class{gtk-font-chooser} class.
  @end{short}

  Shows or hides the editable preview entry.
  @see-class{gtk-font-chooser}")

;;; ----------------------------------------------------------------------------
;;; gtk_font_chooser_get_font_family () -> gtk-font-chooser-font-family
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_font_chooser_get_font_family" gtk-font-chooser-font-family)
    (g-object pango-font-family)
 #+cl-cffi-gtk-documentation
 "@version{*2021-2-11}
  @argument[fontchooser]{a @class{gtk-font-chooser} object}
  @begin{return}
    A @class{pango-font-family} object representing the selected font family,
    or @code{nil}.
  @end{return}
  @begin{short}
    Gets the Pango font family representing the selected font family.
  @end{short}
  Font families are a collection of font faces. If the selected font is not
  installed, returns @code{nil}.
  @begin[Example]{dictionary}
    @begin{pre}
(defvar fontbutton (make-instance 'gtk-font-button :font \"Serif Bold 10\"))
=> FONTBUTTON
(gtk-font-chooser-font-family fontbutton)
=> #<PANGO-FONT-FAMILY {1002728D63@}>
(pango-font-family-name *)
=> \"Sans\"
    @end{pre}
  @end{dictionary}
  @see-class{gtk-font-chooser}
  @see-class{pango-font-family}"
  (fontchooser (g-object gtk-font-chooser)))

(export 'gtk-font-chooser-font-family)

;;; ----------------------------------------------------------------------------
;;; gtk_font_chooser_get_font_face () -> gtk-font-chooser-font-face
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_font_chooser_get_font_face" gtk-font-chooser-font-face)
    (g-object pango-font-face)
 #+cl-cffi-gtk-documentation
 "@version{*2021-2-11}
  @argument[fontchooser]{a @class{gtk-font-chooser} object}
  @begin{return}
    A @class{pango-font-face} object representing the selected font group
    details, or @code{nil}.
  @end{return}
  @begin{short}
    Gets the Pango font face representing the selected font group details,
    i.e. family, slant, weight, width, etc.
  @end{short}
  If the selected font is not installed, returns @code{nil}.
  @see-class{gtk-font-chooser}
  @see-class{pango-font-face}"
  (fontchooser (g-object gtk-font-chooser)))

(export 'gtk-font-chooser-font-face)

;;; ----------------------------------------------------------------------------
;;; gtk_font_chooser_get_font_size () -> gtk-font-chooser-font-size
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_font_chooser_get_font_size" gtk-font-chooser-font-size) :int
 #+cl-cffi-gtk-documentation
 "@version{*2021-2-11}
  @argument[fontchooser]{a @class{gtk-font-chooser} object}
  @begin{return}
    An integer representing the selected font size in Pango units,
    or -1 if no font size is selected.
  @end{return}
  @begin{short}
    The selected font size.
  @end{short}
  @see-class{gtk-font-chooser}"
  (fontchooser (g-object gtk-font-chooser)))

(export 'gtk-font-chooser-font-size)

;;; ----------------------------------------------------------------------------
;;; GtkFontFilterFunc ()
;;; ----------------------------------------------------------------------------

(defcallback gtk-font-filter-func :boolean
    ((family (g-object pango-font-family))
     (face (g-object pango-font-face))
     (data :pointer))
  (funcall (get-stable-pointer-value data) family face))

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-font-filter-func atdoc:*symbol-name-alias*)
      "Callback"
      (gethash 'gtk-font-filter-func atdoc:*external-symbols*)
 "@version{2021-1-31}
  @begin{short}
    The callback function that is used for deciding what fonts get shown in a
    @class{gtk-font-chooser} object.
  @end{short}
  See the function @fun{gtk-font-chooser-set-filter-func}.
  @begin{pre}
 lambda (family face)
  @end{pre}
  @begin[code]{table}
    @entry[family]{A @class{pango-font-family} object.}
    @entry[face]{A @class{pango-font-face} object belonging to @arg{family}.}
    @entry[Return]{@em{True} if the font should be displayed.}
  @end{table}
  @see-class{gtk-font-chooser}
  @see-class{pango-font-family}
  @see-class{pango-font-face}
  @see-function{gtk-font-chooser-set-filter-func}")

(export 'gtk-font-filter-func)

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
 "@version{*2021-2-11}
  @argument[fontchooser]{a @class{gtk-font-chooser} object}
  @argument[filter]{a @symbol{gtk-font-filter-func} callback, or @code{nil}}
  @begin{short}
    Adds a filter function that decides which fonts to display in the font
    chooser.
  @end{short}
  @begin[Example]{dictionary}
    A callback filter function to select fonts from the font families \"Sans\"
    and \"Serif\":
    @begin{pre}
;; Define the callback function
(defun font-filter (family face)
  (declare (ignore face))
  (member (pango-font-family-name family)
          '(\"Sans\" \"Serif\")
          :test #'equal))
;; Set the function FONT-FILTER as the callback function
(gtk-font-chooser-set-filter-func button #'font-filter)
;; Remove the filter function from the font button
(gtk-font-chooser-set-filter-func button nil)
    @end{pre}
  @end{dictionary}
  @see-class{gtk-font-chooser}
  @see-symbol{gtk-font-filter-func}"
  (if func
      (%gtk-font-chooser-set-filter-func
          fontchooser
          (callback gtk-font-filter-func)
          (allocate-stable-pointer func)
          (callback stable-pointer-destroy-notify-cb))
      (%gtk-font-chooser-set-filter-func
          fontchooser
          (null-pointer)
          (null-pointer)
          (null-pointer))))

(export 'gtk-font-chooser-set-filter-func)

;;; ----------------------------------------------------------------------------
;;; gtk_font_chooser_set_font_map ()
;;; gtk_font_chooser_get_font_map () -> gtk-font-chooser-font-map
;;; ----------------------------------------------------------------------------

#+gtk-3-18
(defun (setf gtk-font-chooser-font-map) (fontmap fontchooser)
  (foreign-funcall "gtk_font_chooser_set_font_map"
                   (g-object gtk-font-chooser) fontchooser
                   (g-object pango-font-map) fontmap
                   :void)
  fontmap)

#+gtk-3-18
(defcfun ("gtk_font_chooser_get_font_map" gtk-font-chooser-font-map)
    (g-object pango-font-map)
 #+cl-cffi-gtk-documentation
 "@version{2021-1-20}
  @syntax[]{(gtk-font-chooser-font-map fontchooser) => fontmap}
  @syntax[]{(setf (gtk-font-chooser-font-map fontchooser) fontmap)}
  @argument[fontchooser]{a @class{gtk-font-chooser} widget}
  @argument[fontmap]{a @class{pango-font-map} object, or @code{nil}}
  @begin{short}
    Accessor of the Pango font map of the font chooser widget.
  @end{short}

  The function @sym{gtk-font-chooser-font-map} gets the custom font map of the
  font chooser widget, or @code{nil} if it does not have one. The function
  @sym{(setf gtk-font-chooser-font-map)} sets a custom font map to use for the
  font chooser widget. A custom font map can be used to present application
  specific fonts instead of or in addition to the normal system fonts.

  Since 3.18
  @begin[Example]{dictionary}
    The example from the C documentation uses the @code{Fontconfig} library for
    configuring and customizing font access. This library is not available
    for the Lisp binding.
    @begin{pre}
FcConfig *config;
PangoFontMap *fontmap;

config = FcInitLoadConfigAndFonts ();
FcConfigAppFontAddFile (config, my_app_font_file);

fontmap = pango_cairo_font_map_new_for_font_type (CAIRO_FONT_TYPE_FT);
pango_fc_font_map_set_config (PANGO_FC_FONT_MAP (fontmap), config);

gtk_font_chooser_set_font_map (font_chooser, fontmap);
    @end{pre}
    Note that other GTK+ widgets will only be able to use the application
    specific font if it is present in the font map they use. The following
    code updates the font map for a @sym{gtk-label} widget with @arg{fontmap}.
    @begin{pre}
(setf (pango-context-font-map (gtk-widget-pango-context label)) fontmap)
    @end{pre}
  @end{dictionary}
  @see-class{gtk-font-chooser}
  @see-class{pango-font-map}"
  (fontchooser (g-object gtk-font-chooser)))

#+gtk-3-18
(export 'gtk-font-chooser-font-map)

;;; --- End of file gtk.font-chooser.lisp --------------------------------------
