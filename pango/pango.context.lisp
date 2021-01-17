;;; ----------------------------------------------------------------------------
;;; pango.context.lisp
;;;
;;; The documentation of this file is taken from the Pango Reference Manual
;;; Version 1.48 and modified to document the Lisp binding to the Pango library.
;;; See <http://www.pango.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2020 Dieter Kaiser
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
;;; Contexts
;;;
;;;     Global context object
;;;
;;; Types and Values
;;;
;;;     PangoContext
;;;
;;; Functions
;;;
;;;     pango_context_new
;;;     pango_context_changed
;;;     pango_context_get_serial
;;;     pango_context_set_font_map
;;;     pango_context_get_font_map
;;;     pango_context_get_font_description
;;;     pango_context_set_font_description
;;;     pango_context_get_language
;;;     pango_context_set_language
;;;     pango_context_get_base_dir
;;;     pango_context_set_base_dir
;;;     pango_context_get_base_gravity
;;;     pango_context_set_base_gravity
;;;     pango_context_get_gravity
;;;     pango_context_get_gravity_hint
;;;     pango_context_set_gravity_hint
;;;     pango_context_get_matrix
;;;     pango_context_set_matrix
;;;     pango_context_get_round_glyph_positions
;;;     pango_context_set_round_glyph_positions
;;;     pango_context_load_font
;;;     pango_context_load_fontset
;;;     pango_context_get_metrics
;;;     pango_context_list_families
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── PangoContext
;;;
;;; Description
;;;
;;;     The PangoContext structure stores global information influencing
;;;     Pango's operation, such as the fontmap used to look up fonts, and
;;;     default values such as the default language, default gravity, or
;;;     default font.
;;; ----------------------------------------------------------------------------

(in-package :pango)

;;; ----------------------------------------------------------------------------
;;; PangoContext
;;; ----------------------------------------------------------------------------

(define-g-object-class "PangoContext" pango-context
  (:superclass g-object
   :export t
   :interfaces nil
   :type-initializer "pango_context_get_type")
  nil)

#+cl-cffi-gtk-documentation
(setf (documentation 'pango-context 'type)
 "@version{2021-1-3}
  @begin{short}
    The @sym{pango-context} object stores global information influencing
    Pango's operation, such as the fontmap used to look up fonts, and default
    values such as the default language, default gravity, or default font.
  @end{short}
  @see-function{pango-context-new}")

;;; ----------------------------------------------------------------------------
;;; pango_context_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline pango-context-new))

(defun pango-context-new ()
 #+cl-cffi-gtk-documentation
 "@version{2021-1-3}
  @return{The newly allocated @class{pango-context} object.}
  @begin{short}
    Creates a new Pango Context initialized to default values.
  @end{short}

  This function is not particularly useful as it should always be followed by
  a call of the function @fun{pango-context-font-map}, and the function
  @fun{pango-font-map-create-context} does these two steps together and hence
  users are recommended to use that.

  If you are using Pango as part of a higher-level system, that system may
  have it's own way of create a @class{pango-context}. For instance, the GTK+
  toolkit has, among others, the functions @fun{gdk-pango-context-for-screen},
  and @fun{gtk-widget-pango-context}. Use those instead.
  @see-class{pango-context}
  @see-function{pango-context-set-font-map}
  @see-function{pango-font-map-create-context}
  @see-function{gdk-pango-context-for-screen}
  @see-function{gtk-widget-pango-context}"
  (make-instance 'pango-context))

(export 'pango-context-new)

;;; ----------------------------------------------------------------------------
;;; pango_context_changed ()
;;; ----------------------------------------------------------------------------

(defcfun ("pango_context_changed" pango-context-changed) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-1-3}
  @argument[context]{a @class{pango-context} object}
  @begin{short}
    Forces a change in the context, which will cause any @class{pango-layout}
    using this context to re-layout.
  @end{short}

  This function is only useful when implementing a new backend for Pango,
  something applications will not do. Backends should call this function if
  they have attached extra data to the context and such data is changed.
  @see-class{pango-context}
  @see-class{pango-layout}"
  (context (g-object pango-context)))

(export 'pango-context-changed)

;;; ----------------------------------------------------------------------------
;;; pango_context_get_serial () -> pango-context-serial
;;; ----------------------------------------------------------------------------

(defcfun ("pango_context_get_serial" pango-context-serial) :uint
 #+cl-cffi-gtk-documentation
 "@version{2021-1-3}
  @argument[context]{a @class{pango-context} object}
  @return{An unsigned integer with the current serial number of @arg{context}.}
  @begin{short}
    Returns the current serial number of @arg{context}.
  @end{short}
  The serial number is initialized to a small number larger than zero when a
  new context is created and is increased whenever the context is changed using
  any of the setter functions, or the @class{pango-font-map} object it uses to
  find fonts has changed. The serial may wrap, but will never have the value 0.
  Since it can wrap, never compare it with \"less than\", always use \"not
  equals\".

  This can be used to automatically detect changes to a @class{pango-context}
  object, and is only useful when implementing objects that need update when
  their Pango context changes, like their Pango layout.
  @see-class{pango-context}
  @see-class{pango-font-map}"
  (context (g-object pango-context)))

(export 'pango-context-serial)

;;; ----------------------------------------------------------------------------
;;; pango_context_get_font_map ()
;;; pango_context_set_font_map () -> pango-context-font-map
;;; ----------------------------------------------------------------------------

(defun (setf pango-context-font-map) (font-map context)
  (foreign-funcall "pango_context_set_font_map"
                   (g-object pango-context) context
                   (g-object pango-font-map) font-map
                   :void)
  font-map)

(defcfun ("pango_context_get_font_map" pango-context-font-map)
    (g-object pango-font-map)
 #+cl-cffi-gtk-documentation
 "@version{2021-1-3}
  @syntax[]{(pango-context-font-map context) => font-map}
  @syntax[]{(setf (pango-context-font-map context) font-map)}
  @argument[context]{a @class{pango-context} object}
  @argument[font-map]{a @class{pango-font-map} object to set}
  @begin{short}
    Accesor of the font map of a Pango context.
  @end{short}

  The function @sym{pango-context-font-map} gets the @class{pango-font-map}
  used to look up fonts for this context. The function
  @sym{(setf pango-context-font-map)} sets the font map to be searched when
  fonts are looked-up in this context. This is only for internal use by Pango
  backends, a @class{pango-context} object obtained via one of the recommended
  methods should already have a suitable font map.
  @see-class{pango-context}
  @see-class{pango-font-map}"
  (context (g-object pango-context)))

(export 'pango-context-font-map)

;;; ----------------------------------------------------------------------------
;;; pango_context_get_font_description ()
;;; pango_context_set_font_description () -> pango-context-font-description
;;; ----------------------------------------------------------------------------

(defun (setf pango-context-font-description) (desc context)
  (foreign-funcall "pango_context_set_font_description"
                   (g-object pango-context) context
                   (g-boxed-foreign pango-font-description) desc
                   :void)
  desc)

(defcfun ("pango_context_get_font_description" pango-context-font-description)
    (g-boxed-foreign pango-font-description)
 #+cl-cffi-gtk-documentation
 "@version{2021-1-3}
  @syntax[]{(pango-context-font-description context) => desc}
  @syntax[]{(setf (pango-context-font-description context) desc)}
  @argument[context]{a @class{pango-context} object}
  @argument[desc]{a @class{pango-font-description} structure}
  @begin{short}
    Accessor of the font description of the Pango context.
  @end{short}

  The function @sym{pango-context-font-description} retrieves the default font
  description for the context. The function
  @sym{(setf pango-context-font-description)} sets the default font description
  for the context.
  @see-class{pango-context}
  @see-class{pango-font-description}"
  (context (g-object pango-context)))

(export 'pango-context-font-description)

;;; ----------------------------------------------------------------------------
;;; pango_context_get_language ()
;;; pango_context_set_language () -> pango-context-language
;;; ----------------------------------------------------------------------------

(defun (setf pango-context-language) (language context)
  (foreign-funcall "pango_context_set_language"
                   (g-object pango-context) context
                   (g-boxed-foreign pango-language) language)
  language)

(defcfun ("pango_context_get_language" pango-context-language)
    (g-boxed-foreign pango-language)
 #+cl-cffi-gtk-documentation
 "@version{2021-1-3}
  @syntax[]{(pango-context-language context) => language}
  @syntax[]{(setf pango-context-language context) language)}
  @argument[context]{a @class{pango-context} object}
  @argument[language]{a @class{pango-language} tag}
  @begin{short}
    Accessor of the global language tag of the Pango context.
  @end{short}

  The function @sym{pango-context-language} retrieves the global language tag
  for the context. The function @sym{(setf pango-context-language)} sets the
  global language tag for the context. The default language for the locale of
  the running process can be found using the function
  @fun{pango-language-default}.
  @see-class{pango-context}
  @see-class{pango-language}
  @see-function{pango-language-default}"
  (context (g-object pango-context)))

(export 'pango-context-language)

;;; ----------------------------------------------------------------------------
;;; pango_context_get_base_dir ()
;;; pango_context_set_base_dir () -> pango-context-base-dir
;;; ----------------------------------------------------------------------------

(defun (setf pango-context-base-dir) (direction context)
  (foreign-funcall "pango_context_set_base_dir"
                   (g-object pango-context) context
                   pango-direction direction
                   :void)
  direction)

(defcfun ("pango_context_get_base_dir" pango-context-base-dir) pango-direction
 #+cl-cffi-gtk-documentation
 "@version{2021-1-3}
  @syntax[]{(pango-context-base-dir context) => direction}
  @syntax[]{(setf (pango-context-base-dir context) direction)}
  @argument[context]{a @class{pango-context}}
  @argument[direction]{the base direction of type @symbol{pango-direction}}
  @begin{short}
    Accessor of the base direction for the Pango context.
  @end{short}

  The function @sym{pango-context-base-dir} retrieves the base direction for
  the context. The function @sym{(setf pango-context-base-dir)} sets the base
  direction for the context.

  The base direction is used in applying the Unicode bidirectional algorithm.
  If the direction is @code{:ltr} or @code{:rtl}, then the value will be used
  as the paragraph direction in the Unicode bidirectional algorithm. A value of
  @code{:weak-ltr} or @code{:weak-rtl} is used only for paragraphs that do not
  contain any strong characters themselves.
  @see-class{pango-context}
  @see-symbol{pango-direction}"
  (context (g-object pango-context)))

(export 'pango-context-base-dir)

;;; ----------------------------------------------------------------------------
;;; pango_context_get_base_gravity ()
;;; pango_context_set_base_gravity () -> pango-context-base-gravity
;;; ----------------------------------------------------------------------------

(defun (setf pango-context-base-gravity) (gravity context)
  (foreign-funcall "pango_context_set_base_gravity"
                   (g-object pango-context) context
                   pango-gravity gravity
                   :void)
  gravity)

(defcfun ("pango_context_get_base_gravity" pango-context-base-gravity)
    pango-gravity
 #+cl-cffi-gtk-documentation
 "@version{2021-1-3}
  @syntax[]{(pango-context-base-gravity context) => gravity}
  @syntax[]{(setf (pango-context-base-gravity context) gravity)}
  @argument[context]{a @class{pango-context} object}
  @argument[gravity]{a base gravity of type @symbol{pango-gravity}}
  @begin{short}
    Accessor of the base gravity for the Pango context.
  @end{short}

  The function @sym{pango-context-base-gravity} retrieves the base gravity for
  the Pango context. The function @sym{(setf pango-context-base-gravity)} sets
  the base gravity.

  The base gravity is used in laying vertical text out.
  @see-class{pango-context}
  @see-symbol{pango-gravity}"
  (context (g-object pango-context)))

(export 'pango-context-base-gravity)

;;; ----------------------------------------------------------------------------
;;; pango_context_get_gravity () -> pango-context-gravity
;;; ----------------------------------------------------------------------------

(defcfun ("pango_context_get_gravity" pango-context-gravity) pango-gravity
 #+cl-cffi-gtk-documentation
 "@version{2021-1-3}
  @argument[context]{a @class{pango-context} object}
  @begin{return}
    The resolved gravity of type @symbol{pango-gravity} for the Pango context.
  @end{return}
  @begin{short}
    Retrieves the gravity for the Pango context.
  @end{short}
  This is similar to the function @fun{pango-context-base-gravity}, except for
  when the base gravity is @code{:auto} for which the function
  @fun{pango-gravity-for-matrix} is used to return the gravity from the current
  context matrix.
  @see-class{pango-context}
  @see-symbol{pango-gravity}
  @see-function{pango-context-base-gravity}
  @see-function{pango-gravity-for-matrix}"
  (context (g-object pango-context)))

(export 'pango-context-gravity)

;;; ----------------------------------------------------------------------------
;;; pango_context_get_gravity_hint ()
;;; pango_context_set_gravity_hint () -> pango-context-gravity-hint
;;; ----------------------------------------------------------------------------

(defun (setf pango-context-gravity-hint) (hint context)
  (foreign-funcall "pango_context_set_gravity_hint"
                   (g-object pango-context) context
                   pango-gravity-hint hint
                   :void)
  hint)

(defcfun ("pango_context_get_gravity_hint" pango-context-gravity-hint)
    pango-gravity-hint
 #+cl-cffi-gtk-documentation
 "@version{2021-1-3}
  @syntax[]{(pango-context-gravity-hint context) => hint}
  @syntax[]{(setf (pango-context-gravity-hint context) hint)}
  @argument[context]{a @class{pango-context} object}
  @argument[hint]{a gravity hint of type @symbol{pango-gravity-hint}}
  @begin{short}
    Accessor of the gravity hint for the Pango context.
  @end{short}

  The function @sym{pango-context-gravity-hint} retrieves the gravity hint for
  the context. The function @sym{(setf pango-context-gravity-hint)} sets the
  gravity hint.

  The gravity hint is used in laying vertical text out, and is only relevant
  if gravity of the context as returned by the function
  @fun{pango-context-gravity} is set @code{:east} or @code{:west}.
  @see-class{pango-context}
  @see-symbol{pango-gravity-hint}"
  (context (g-object pango-context)))

(export 'pango-context-gravity-hint)

;;; ----------------------------------------------------------------------------
;;; pango_context_get_matrix ()
;;; pango_context_set_matrix () -> pango-context-matrix
;;; ----------------------------------------------------------------------------

(defun (setf pango-context-matrix) (matrix context)
  (foreign-funcall "pango_context_set_matrix"
                   (g-object pango-context) context
                   (g-boxed-foreign pango-matrix) matrix
                   :void)
  matrix)

(defcfun ("pango_context_get_matrix" pango-context-matrix)
    (g-boxed-foreign pango-matrix)
 #+cl-cffi-gtk-documentation
 "@version{2021-1-3}
  @syntax[]{(pango-context-matrix context) => matrix}
  @syntax[]{(setf (pango-context-matrix context) matrix)}
  @argument[context]{a @class{pango-context} object}
  @argument[matrix]{a @class{pango-matrix} instance, or @code{nil} to unset any
    existing matrix, no matrix set is the same as setting the identity matrix}
  @begin{short}
    Accessor of the transformation matrix of the Pango context.
  @end{short}

  The function @sym{pango-contex-matrix} gets the transformation matrix that
  will be applied when rendering with this context. The function
  @sym{(setf pango-context-matrix)} sets the transformation matrix that will be
  applied when rendering with this context. Note that reported metrics are in
  the user space coordinates before the application of the matrix, not
  device-space coordinates after the application of the matrix. So, they do not
  scale with the matrix, though they may change slightly for different matrices,
  depending on how the text is fit to the pixel grid.
  @see-class{pango-context}
  @see-class{pango-matrix}"
  (context (g-object pango-context)))

(export 'pango-context-matrix)

;;; ----------------------------------------------------------------------------
;;; pango_context_get_round_glyph_positions ()
;;; pango_context_set_round_glyph_positions ()
;;;   -> pango-context-round-glyph-positions
;;; ----------------------------------------------------------------------------

#+pango-1-44
(defun (setf pango-context-round-glyph-positions) (round-positions context)
  (foreign-funcall "pango_context_set_round_glyph_positions"
                   (g-object pango-context) context
                   :boolean round-positions
                   :void)
  round-positions)

#+pango-1-44
(defcfun ("pango_context_get_round_glyph_positions"
           pango-context-round-glyph-positions) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2021-1-3}
  @syntax[]{(pango-context-round-glyph-positions context) => round-positions}
  @syntax[]{(setf (pango-context-round-glyph-positions context) round-positions)}
  @argument[context]{a @class{pango-context} object}
  @argument[round-positions]{a boolean whether to round glyph positions}
  @begin{short}
    The function @sym{pango-context-round-glyph-positions} returns whether font
    rendering with this context should round glyph positions and widths.
  @end{short}

  The function @sym{(setf pango-context-round-glyph-positions} sets whether
  font rendering with this context should round glyph positions and widths to
  integral positions, in device units.

  This is useful when the renderer cannot handle subpixel positioning of glyphs.

  The default value is to round glyph positions, to remain compatible with
  previous Pango behavior.

  Since 1.44
  @see-class{pango-context}"
  (context (g-object pango-context)))

#+pango-1-44
(export 'pango-context-round-glyph-positions)

;;; ----------------------------------------------------------------------------
;;; pango_context_load_font ()
;;; ----------------------------------------------------------------------------

(defcfun ("pango_context_load_font" pango-context-load-font)
    (g-object pango-font)
 #+cl-cffi-gtk-documentation
 "@version{2021-1-3}
  @argument[context]{a @class{pango-context} object}
  @argument[desc]{a @class{pango-font-description} instance describing the font
    to load}
  @begin{return}
    The newly allocated @class{pango-font} object that was loaded, or
    @code{nil} if no font matched.
  @end{return}
  @begin{short}
    Loads the font in one of the fontmaps in the context that is the closest
    match for @arg{desc}.
  @end{short}
  @see-class{pango-context}
  @see-class{pango-font-description}"
  (context (g-object pango-context))
  (desc (g-boxed-foreign pango-font-description)))

(export 'pango-context-load-font)

;;; ----------------------------------------------------------------------------
;;; pango_context_load_fontset ()
;;; ----------------------------------------------------------------------------

(defcfun ("pango_context_load_fontset" pango-context-load-fontset)
    (g-object pango-fontset)
 #+cl-cffi-gtk-documentation
 "@version{2021-1-3}
  @argument[context]{a @class{pango-context} object}
  @argument[desc]{a @class{pango-font-description} instance describing the
    fonts to load}
  @argument[language]{a @class{pango-language} instance the fonts will be used
    for}
  @begin{return}
    The newly allocated @class{pango-fontset} object loaded, or @code{nil} if
    no font matched.
  @end{return}
  @begin{short}
    Load a set of fonts in the context that can be used to render a font
    matching @arg{desc}.
  @end{short}
  @see-class{pango-context}
  @see-class{pango-font-description}
  @see-class{pango-language}
  @see-class{pango-fontset}"
  (context (g-object pango-context))
  (desc (g-boxed-foreign pango-font-description))
  (language (g-boxed-foreign pango-language)))

(export 'pango-context-load-fontset)

;;; ----------------------------------------------------------------------------
;;; pango_context_get_metrics () -> pango-context-metrics
;;; ----------------------------------------------------------------------------

(defcfun ("pango_context_get_metrics" pango-context-metrics)
    (g-boxed-foreign pango-font-metrics)
 #+cl-cffi-gtk-documentation
 "@version{2021-1-7}
  @argument[context]{a @class{pango-context} object}
  @argument[desc]{a @class{pango-font-description} instance, @code{nil} means
    that the font description from the context will be used}
  @argument[language]{language tag of type @class{pango-language} used to
    determine which script to get the metrics for, @code{nil} means that the
    language tag from the context will be used, if no language tag is set on
    the context, metrics for the default language, as determined by the
    function @fun{pango-language-default}, will be returned}
  @begin{return}
    A @class{pango-font-metrics} instance. The caller must call the function
    @fun{pango-font-metrics-unref} when finished using the object.
  @end{return}
  @begin{short}
    Get overall metric information for a particular font description.
  @end{short}
  Since the metrics may be substantially different for different scripts, a
  language tag can be provided to indicate that the metrics should be retrieved
  that correspond to the script(s) used by that language.

  The @class{pango-font-description} instance is interpreted in the same way as
  by the function @fun{pango-itemize}, and the family name may be a comma
  separated list of figures. If characters from multiple of these families
  would be used to render the string, then the returned fonts would be a
  composite of the metrics for the fonts loaded for the individual families.
  @see-class{pango-context}
  @see-class{pango-font-description}
  @see-class{pango-language}
  @see-function{pango-itemize}"
  (context (g-object pango-context))
  (desc (g-boxed-foreign pango-font-description))
  (language (g-boxed-foreign pango-language)))

(export 'pango-context-metrics)

;;; ----------------------------------------------------------------------------
;;; pango_context_list_families ()
;;; ----------------------------------------------------------------------------

(defcfun ("pango_context_list_families" %pango-context-list-families) :void
  (context (g-object pango-context))
  (families :pointer)
  (n-families (:pointer :int)))

(defun pango-context-list-families (context)
 #+cl-cffi-gtk-documentation
 "@version{2021-1-14}
  @argument[context]{a @class{pango-context} object}
  @begin{return}
    A list of @class{pango-font-family} objects.
  @end{return}
  @begin{short}
    List all families for a context.
  @end{short}
  @see-class{pango-context}
  @see-class{pango-font-family}"
  (with-foreign-objects ((families-ptr :pointer) (n-families :int))
    (%pango-context-list-families context families-ptr n-families)
    (loop with families-ar = (mem-ref families-ptr :pointer)
          for i from 0 below (mem-ref n-families :int)
          for family = (convert-from-foreign (mem-aref families-ar :pointer i)
                                             'g-object)
          collect family
          finally (g-free families-ar))))

(export 'pango-context-list-families)

;;; --- End of file pango.context.lisp -----------------------------------------
