;;; ----------------------------------------------------------------------------
;;; cairo.text.lisp
;;;
;;; The documentation of the file is taken from the Cairo Reference Manual
;;; Version 1.16 and modified to document the Lisp binding to the Cairo
;;; library. See <http://cairographics.org>. The API documentation of the
;;; Lisp binding is available at <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2013 - 2020 Dieter Kaiser
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
;;; Text
;;;
;;;     Rendering text and glyphs
;;;
;;; Types and Values
;;;
;;;     cairo_glyph_t
;;;     cairo_font_slant_t
;;;     cairo_font_weight_t
;;;     cairo_text_cluster_t
;;;     cairo_text_cluster_flags_t
;;;
;;; Functions
;;;
;;;     cairo_select_font_face
;;;     cairo_set_font_size
;;;     cairo_set_font_matrix
;;;     cairo_get_font_matrix
;;;     cairo_set_font_options
;;;     cairo_get_font_options
;;;     cairo_set_font_face
;;;     cairo_get_font_face
;;;     cairo_set_scaled_font
;;;     cairo_get_scaled_font
;;;     cairo_show_text
;;;     cairo_show_glyphs
;;;     cairo_show_text_glyphs
;;;     cairo_font_extents
;;;     cairo_text_extents
;;;     cairo_glyph_extents
;;;     cairo_toy_font_face_create
;;;     cairo_toy_font_face_get_family
;;;     cairo_toy_font_face_get_slant
;;;     cairo_toy_font_face_get_weight
;;;     cairo_glyph_allocate
;;;     cairo_glyph_free
;;;     cairo_text_cluster_allocate
;;;     cairo_text_cluster_free
;;;
;;; Description
;;;
;;;     The functions with text in their name form cairo's toy text API. The toy
;;;     API takes UTF-8 encoded text and is limited in its functionality to
;;;     rendering simple left-to-right text with no advanced features. That
;;;     means for example that most complex scripts like Hebrew, Arabic, and
;;;     Indic scripts are out of question. No kerning or correct positioning of
;;;     diacritical marks either. The font selection is pretty limited too and
;;;     does not handle the case that the selected font does not cover the
;;;     characters in the text. This set of functions are really that, a toy
;;;     text API, for testing and demonstration purposes. Any serious
;;;     application should avoid them.
;;;
;;;     The functions with glyphs in their name form cairo's low-level text API.
;;;     The low-level API relies on the user to convert text to a set of glyph
;;;     indexes and positions. This is a very hard problem and is best handled
;;;     by external libraries, like the pangocairo that is part of the Pango
;;;     text layout and rendering library. Pango is available from
;;;     http://www.pango.org/.
;;; ----------------------------------------------------------------------------

(in-package :cairo)

;;; ----------------------------------------------------------------------------
;;; cairo_glyph_t
;;; ----------------------------------------------------------------------------

(defcstruct cairo-glyph-t
  (index :ulong)
  (x :double)
  (y :double))

#+cl-cffi-gtk-documentation
(setf (gethash 'cairo-glyph-t atdoc:*symbol-name-alias*)
      "CStruct"
      (gethash 'cairo-glyph-t atdoc:*external-symbols*)
 "@version{2020-12-28}
  @begin{short}
    The @sym{cairo-glyph-t} structure holds information about a single glyph
    when drawing or measuring text.
  @end{short}
  A font is (in simple terms) a collection of shapes used to draw text. A glyph
  is one of these shapes. There can be multiple glyphs for a single character
  (alternates to be used in different contexts, for example), or a glyph can be
  a ligature of multiple characters. Cairo does not expose any way of converting
  input text into glyphs, so in order to use the Cairo interfaces that take
  arrays of glyphs, you must directly access the appropriate underlying font
  system.

  Note that the offsets given by x and y are not cumulative. When drawing or
  measuring text, each glyph is individually positioned with respect to the
  overall origin.
  @begin{pre}
(defcstruct cairo-glyph-t
  (index :ulong)
  (x :double)
  (y :double))
  @end{pre}
  @begin[code]{table}
    @entry[index]{Glyph index in the font. The exact interpretation of the
      glyph index depends on the font technology being used.}
    @entry[x]{The offset in the x direction between the origin used for drawing
      or measuring the string and the origin of this glyph.}
    @entry[y]{The offset in the y direction between the origin used for drawing
      or measuring the string and the origin of this glyph.}
  @end{table}
  @see-function{cairo-glyph-path}
  @see-function{cairo-show-glyphs}")

(export 'cairo-glyph-t)

;;; ----------------------------------------------------------------------------
;;; enum cairo_font_slant_t
;;; ----------------------------------------------------------------------------

(defcenum cairo-font-slant-t
  :normal
  :italic
  :oblique)

#+cl-cffi-gtk-documentation
(setf (gethash 'cairo-font-slant-t atdoc:*symbol-name-alias*)
      "CEnum"
      (gethash 'cairo-font-slant-t atdoc:*external-symbols*)
 "@version{2020-12-15}
  @short{Specifies variants of a font face based on their slant.}
  @begin{pre}
(defcenum cairo-font-slant-t
  :normal
  :italic
  :oblique)
  @end{pre}
  @begin[code]{table}
    @entry[:normal]{Upright font style.}
    @entry[:italic]{Italic font style.}
    @entry[:oblique]{Oblique font style.}
  @end{table}
  @see-symbol{cairo-font-weight-t}
  @see-function{cairo-select-font-face}")

(export 'cairo-font-slant-t)

;;; ----------------------------------------------------------------------------
;;; enum cairo_font_weight_t
;;; ----------------------------------------------------------------------------

(defcenum cairo-font-weight-t
  :normal
  :bold)

#+cl-cffi-gtk-documentation
(setf (gethash 'cairo-font-weight-t atdoc:*symbol-name-alias*)
      "CEnum"
      (gethash 'cairo-font-weight-t atdoc:*external-symbols*)
 "@version{2020-12-15}
  @short{Specifies variants of a font face based on their weight.}
  @begin{pre}
(defcenum cairo-font-slant-t
  :normal
  :bold)
  @end{pre}
  @begin[code]{table}
    @entry[:normal]{Normal font weight.}
    @entry[:bold]{Bold font weight.}
  @end{table}
  @see-symbol{cairo-font-slant-t}
  @see-function{cairo-select-font-face}")

(export 'cairo-font-weight-t)

;;; ----------------------------------------------------------------------------
;;; cairo_text_cluster_t
;;; ----------------------------------------------------------------------------

(defcstruct cairo-text-cluster-t
  (num-bytes :int)
  (num-glyphs :int))

#+cl-cffi-gtk-documentation
(setf (gethash 'cairo-text-cluster-t atdoc:*symbol-name-alias*)
      "CStruct"
      (gethash 'cairo-text-cluster-t atdoc:*external-symbols*)
 "@version{2020-12-28}
  @begin{short}
    The @sym{cairo-text-cluster-t} structure holds information about a single
    text cluster.
  @end{short}
  A text cluster is a minimal mapping of some glyphs corresponding to some
  UTF-8 text.

  For a cluster to be valid, both @arg{num-bytes} and @arg{num-glyphs} should
  be non-negative, and at least one should be non-zero. Note that clusters with
  zero glyphs are not as well supported as normal clusters. For example, PDF
  rendering applications typically ignore those clusters when PDF text is
  being selected.

  See the function @fun{cairo-show-text-glyphs} for how clusters are used in
  advanced text operations.
  @begin{pre}
(defcstruct cairo-text-cluster-t
  (num-bytes :int)
  (num-glyphs :int))
  @end{pre}
  @begin[code]{table}
    @entry[num-bytes]{The number of bytes of UTF-8 text covered by cluster.}
    @entry[num-glyphs]{The number of glyphs covered by cluster.}
  @end{table}
  @see-symbol{cairo-text-cluster-flags-t}
  @see-function{cairo-show-text-glyphs}")

(export 'cairo-text-cluster-t)

;;; ----------------------------------------------------------------------------
;;; enum cairo_text_cluster_flags_t
;;; ----------------------------------------------------------------------------

(defcenum cairo-text-cluster-flags-t
  (:backward 1))

#+cl-cffi-gtk-documentation
(setf (gethash 'cairo-text-cluster-flags-t atdoc:*symbol-name-alias*)
      "CEnum"
      (gethash 'cairo-text-cluster-flags-t atdoc:*external-symbols*)
 "@version{2020-12-28}
  @short{Specifies properties of a text cluster mapping.}
  @begin{pre}
(defcenum cairo-text-cluster-flags-t
  (:backward 1))
  @end{pre}
  @begin[code]{table}
    @entry[:backward]{The clusters in the cluster array map to glyphs in the
      glyph array from end to start.}
  @end{table}
  @see-symbol{cairo-text-cluster-t}")

(export 'cairo-text-cluster-flags-t)

;;; ----------------------------------------------------------------------------
;;; cairo_select_font_face ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_select_font_face" %cairo-select-font-face) :void
  (cr (:pointer (:struct cairo-t)))
  (family :string)
  (slant cairo-font-slant-t)
  (weight cairo-font-weight-t))

(defun cairo-select-font-face (cr family &key (slant :normal) (weight :normal))
 #+cl-cffi-gtk-documentation
 "@version{*2021-1-25}
  @argument[cr]{a @symbol{cairo-t} context}
  @argument[family]{a string with the font family name, encoded in UTF-8}
  @argument[slant]{the slant of type @symbol{cairo-font-slant-t} for the font,
    default value is @code{:normal}}
  @argument[weight]{the weight of type @symbol{cairo-font-weight-t} for the
    font, default value is @code{:normal}}
  @begin{short}
    Selects a family and style of font from a simplified description as a
    family name, slant and weight.
  @end{short}
  Cairo provides no operation to list available family names on the system,
  this is a \"toy\", remember, but the standard CSS2 generic family names,
  \"serif\", \"sans-serif\", \"cursive\", \"fantasy\", \"monospace\", are
  likely to work as expected.

  If family starts with the string \"cairo:\", or if no native font backends
  are compiled in, Cairo will use an internal font family. The internal font
  family recognizes many modifiers in the family string, most notably, it
  recognizes the string \"monospace\". That is, the family name
  \"cairo:monospace\" will use the monospace version of the internal font
  family.

  For \"real\" font selection, see the font-backend-specific
  @code{font-face-create} functions for the font backend you are using. For
  example, if you are using the freetype-based @code{cairo-ft} font backend,
  see the functions @code{cairo-ft-font-face-create-for-ft-face} or
  @code{cairo-ft-font-face-create-for-pattern}. The resulting font face could
  then be used with the functions @fun{cairo-scaled-font-create} and
  @fun{cairo-set-scaled-font}.

  Similarly, when using the \"real\" font support, you can call directly into
  the underlying font system, such as fontconfig or freetype, for operations
  such as listing available fonts, etc.

  It is expected that most applications will need to use a more comprehensive
  font handling and text layout library, for example Pango, in conjunction
  with Cairo.

  If text is drawn without a call to the function @sym{cairo-select-font-face},
  nor the function @fun{cairo-set-font-face} nor the function
  @fun{cairo-set-scaled-font}, the default family is platform-specific, but is
  essentially \"sans-serif\". Default slant is @code{:normal}, and default
  weight is @code{:normal}.

  This function is equivalent to a call to the function
  @fun{cairo-toy-font-face-create} followed by the function
  @fun{cairo-set-font-face}.
  @begin[Note]{dictionary}
    The function @sym{cairo-select-font-face} is part of what the Cairo
    designers call the \"toy\" text API. It is convenient for short demos
    and simple programs, but it is not expected to be adequate for serious
    text-using applications.
  @end{dictionary}
  @see-symbol{cairo-t}
  @see-symbol{cairo-font-slant-t}
  @see-symbol{cairo-font-weight-t}
  @see-function{cairo-toy-font-face-create}
  @see-function{cairo-set-font-face}"
  (%cairo-select-font-face cr family slant weight))

(export 'cairo-select-font-face)

;;; ----------------------------------------------------------------------------
;;; cairo_set_font_size ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_set_font_size" %cairo-set-font-size) :void
  (cr (:pointer (:struct cairo-t)))
  (size :double))

(defun cairo-set-font-size (cr size)
 #+cl-cffi-gtk-documentation
 "@version{*2021-1-26}
  @argument[cr]{a @symbol{cairo-t} context}
  @argument[size]{a number coerced to a double float with the new font size,
    in user space units}
  @begin{short}
    Sets the current font matrix to a scale by a factor of @arg{size},
    replacing any font matrix previously set with the functions
    @sym{cairo-set-font-size} or @fun{cairo-set-font-matrix}.
  @end{short}
  This results in a font size of @arg{size} user space units. More precisely,
  this matrix will result in the font's em-square being a @arg{size} by
  @arg{size} square in user space.

  If text is drawn without a call to the function @sym{cairo-set-font-size},
  nor the functions @fun{cairo-set-font-matrix} or @fun{cairo-set-scaled-font},
  the default font size is 10.0.
  @see-symbol{cairo-t}
  @see-function{cairo-set-font-matrix}
  @see-function{cairo-set-scaled-font}"
  (%cairo-set-font-size cr (coerce size 'double-float)))

(export 'cairo-set-font-size)

;;; ----------------------------------------------------------------------------
;;; cairo_set_font_matrix ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_set_font_matrix" cairo-set-font-matrix) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-12-28}
  @argument[cr]{a @symbol{cairo-t} context}
  @argument[matrix]{a @symbol{cairo-matrix-t} instance describing a transform
    to be applied to the current font}
  @begin{short}
    Sets the current font matrix to @arg{matrix}.
  @end{short}
  The font matrix gives a transformation from the design space of the font (in
  this space, the em-square is 1 unit by 1 unit) to user space. Normally, a
  simple scale is used, see the function @fun{cairo-set-font-size}, but a more
  complex font matrix can be used to shear the font or stretch it unequally
  along the two axes.
  @see-symbol{cairo-t}
  @see-symbol{cairo-matrix-t}
  @see-function{cairo-set-font-size}"
  (cr (:pointer (:struct cairo-t)))
  (matrix (:pointer (:struct cairo-matrix-t))))

(export 'cairo-set-font-matrix)

;;; ----------------------------------------------------------------------------
;;; cairo_get_font_matrix ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_get_font_matrix" cairo-get-font-matrix) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-12-28}
  @argument[cr]{a @symbol{cairo-t} context}
  @argument[matrix]{a @symbol{cairo-matrix-t} instance for the value of the
    matrix}
  @begin{short}
    Stores the current font matrix into @arg{matrix}.
  @end{short}
  See the function @fun{cairo-set-font-matrix}.
  @see-symbol{cairo-t}
  @see-symbol{cairo-matrix-t}
  @see-function{cairo-set-font-matrix}"
  (cr (:pointer (:struct cairo-t)))
  (matrix (:pointer (:struct cairo-matrix-t))))

(export 'cairo-get-font-matrix)

;;; ----------------------------------------------------------------------------
;;; cairo_set_font_options ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_set_font_options" cairo-set-font-options) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-12-28}
  @argument[cr]{a @symbol{cairo-t} context}
  @argument[options]{font options of type @symbol{cairo-font-options-t} to use}
  @begin{short}
    Sets a set of custom font rendering options for the Cairo context.
  @end{short}
  Rendering options are derived by merging these @arg{options} with the options
  derived from underlying surface. If the value in options has a default value,
  like @code{:default}, then the value from the surface is used.
  @see-symbol{cairo-t}
  @see-symbol{cairo-font-options-t}"
  (cr (:pointer (:struct cairo-t)))
  (options (:pointer (:struct cairo-font-options-t))))

(export 'cairo-set-font-options)

;;; ----------------------------------------------------------------------------
;;; cairo_get_font_options ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_get_font_options" %cairo-get-font-options) :void
  (cr (:pointer (:struct cairo-t)))
  (options (:pointer (:struct cairo-font-options-t))))

(defun cairo-get-font-options (cr)
 #+cl-cffi-gtk-documentation
 "@version{2020-12-28}
  @argument[cr]{a @symbol{cairo-t} context}
  @begin{return}
    A @symbol{cairo-font-options-t} instance with the retrieved options.
  @end{return}
  @begin{short}
    Retrieves font rendering options set via the function
    @fun{cairo-set-font-options}.
  @end{short}
  Note that the returned options do not include any options derived from the
  underlying surface; they are literally the options passed to the function
  @fun{cairo-set-font-options}.
  @see-symbol{cairo-t}
  @see-symbol{cairo-font-options-t}
  @see-function{cairo-set-font-options}"
  (with-foreign-object (options '(:struct cairo-font-options-t))
    (%cairo-get-font-options cr options)
    options))

(export 'cairo-get-font-options)

;;; ----------------------------------------------------------------------------
;;; cairo_set_font_face ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_set_font_face" cairo-set-font-face) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-12-15}
  @argument[cr]{a @symbol{cairo-t} context}
  @argument[font-face]{a @symbol{cairo-font-face-t} instance, or @code{nil} to
    restore to the default font}
  @begin{short}
    Replaces the current @symbol{cairo-font-face-t} instance in the
    Cairo context with @arg{font-face}.
  @end{short}
  The replaced font face in the Cairo context will be destroyed if there are
  no other references to it.
  @see-symbol{cairo-t}
  @see-symbol{cairo-font-face-t}"
  (cr (:pointer (:struct cairo-t)))
  (font-face (:pointer (:struct cairo-font-face-t))))

(export 'cairo-set-font-face)

;;; ----------------------------------------------------------------------------
;;; cairo_get_font_face ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_get_font_face" cairo-get-font-face)
    (:pointer (:struct cairo-font-face-t))
 #+cl-cffi-gtk-documentation
 "@version{2020-12-28}
  @argument[cr]{a @symbol{cairo-t} instance}
  @begin{return}
    The current font face. This object is owned by Cairo. To keep a reference
    to it, you must call the function @fun{cairo-font-face-reference}. This
    function never returns NULL. If memory cannot be allocated, a special
    \"nil\" @symbol{cairo-font-face-t} instance will be returned on which the
    function @fun{cairo-font-face-status} returns @code{:no-memory}. Using this
    nil instance will cause its error state to propagate to other objects it is
    passed to, for example, calling the function @fun{cairo-set-font-face} with
    a nil font will trigger an error that will shutdown the Cairo context.
  @end{return}
  @begin{short}
    Gets the current font face for a Cairo context.
  @end{short}
  @see-symbol{cairo-t}
  @see-symbol{cairo-font-face-t}
  @see-function{cairo-font-face-reference}
  @see-function{cairo-font-face-status}
  @see-function{cairo-set-font-face}"
  (cr (:pointer (:struct cairo-t))))

(export 'cairo-get-font-face)

;;; ----------------------------------------------------------------------------
;;; cairo_set_scaled_font ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_set_scaled_font" cairo-set-scaled-font) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-12-15}
  @argument[cr]{a @symbol{cairo-t} context}
  @argument[scaled-font]{a @symbol{cairo-scaled-font-t} instance}
  @begin{short}
    Replaces the current font face, font matrix, and font options in the
    Cairo context with those of the @symbol{cairo-scaled-font-t} instance.
  @end{short}
  Except for some translation, the current CTM of the Cairo context should
  be the same as that of the @symbol{cairo-scaled-font-t} instance, which can
  be accessed using the function @fun{cairo-scaled-font-get-ctm}.
  @see-symbol{cairo-t}
  @see-symbol{cairo-scaled-font-t}
  @see-function{cairo-scaled-font-get-ctm}"
  (cr (:pointer (:struct cairo-t)))
  (scaled-font (:pointer (:struct cairo-scaled-font-t))))

(export 'cairo-set-scaled-font)

;;; ----------------------------------------------------------------------------
;;; cairo_get_scaled_font ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_get_scaled_font" cairo-get-scaled-font)
    (:pointer (:struct cairo-scaled-font-t))
 #+cl-cffi-gtk-documentation
 "@version{2020-12-28}
  @argument[cr]{a @symbol{cairo-t} context}
  @begin{return}
    The current scaled font. This object is owned by Cairo. To keep a reference
    to it, you must call the function @fun{cairo-scaled-font-reference}. This
    function never returns NULL. If memory cannot be allocated, a special
    \"nil\" @symbol{cairo-scaled-font-t} instance will be returned on which
    the function @fun{cairo-scaled-font-status} returns @code{:no-memory}.
    Using this nil instance will cause its error state to propagate to other
    objects it is passed to, for example, calling the function
    @fun{cairo-set-scaled-font} with a nil font will trigger an error that will
    shutdown the Cairo context.
  @end{return}
  @begin{short}
    Gets the current scaled font for the Cairo context.
  @end{short}
  @see-symbol{cairo-t}
  @see-symbol{cairo-scaled-font-t}
  @see-function{cairo-scaled-font-reference}
  @see-function{cairo-scaled-font-status}
  @see-function{cairo-set-scaled-font}"
  (cr (:pointer (:struct cairo-t))))

(export 'cairo-get-scaled-font)

;;; ----------------------------------------------------------------------------
;;; cairo_show_text ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_show_text" %cairo-show-text) :void
  (cr (:pointer (:struct cairo-t)))
  (utf8 :string))

(defun cairo-show-text (cr utf8)
 #+cl-cffi-gtk-documentation
 "@version{*2021-1-26}
  @argument[cr]{a @symbol{cairo-t} context}
  @argument[utf8]{a string of text encoded in UTF-8, or @code{nil}}
  @begin{short}
    A drawing operator that generates the shape from a string of UTF-8
    characters, rendered according to the current font face, font size
    (font matrix), and font options.
  @end{short}

  This function first computes a set of glyphs for the string of text. The
  first glyph is placed so that its origin is at the current point. The origin
  of each subsequent glyph is offset from that of the previous glyph by the
  advance values of the previous glyph.

  After this call the current point is moved to the origin of where the next
  glyph would be placed in this same progression. That is, the current point
  will be at the origin of the final glyph offset by its advance values. This
  allows for easy display of a single logical string with multiple calls to
  the function @sym{cairo-show-text}.
  @begin[Note]{dictionary}
    The function @sym{cairo-show-text} is part of what the Cairo designers call
    the \"toy\" text API. It is convenient for short demos and simple programs,
    but it is not expected to be adequate for serious text-using applications.
    See the function @fun{cairo-show-glyphs} for the \"real\" text display API
    in Cairo.
  @end{dictionary}
  @see-symbol{cairo-t}
  @see-function{cairo-show-glyphs}"
  (%cairo-show-text cr (if utf8 utf8 (null-pointer))))

(export 'cairo-show-text)

;;; ----------------------------------------------------------------------------
;;; cairo_show_glyphs ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_show_glyphs" %cairo-show-glyphs) :void
  (cr (:pointer (:struct cairo-t)))
  (glyphs (:pointer (:struct cairo-glyph-t)))
  (num-glyphs :int))

(defun cairo-show-glyphs (cr glyphs)
 #+cl-cffi-gtk-documentation
 "@version{2020-12-29}
  @argument[cr]{a @symbol{cairo-t} context}
  @argument[glyphs]{a list of glyphs to show}
  @begin{short}
    A drawing operator that generates the shape from a list of glyphs,
    rendered according to the current font face, font size (font matrix), and
    font options.
  @end{short}
  @see-symbol{cairo-t}"
  (let ((num-glyphs (length glyphs)))
    (with-foreign-object (glyphs-ptr '(:struct cairo-glyph-t) num-glyphs)
      (loop for count from 0 below num-glyphs
            for glyph in glyphs
            for glyph-ptr = (mem-aptr glyphs-ptr '(:struct cairo-glyph-t) count)
            do (setf (foreign-slot-value glyph-ptr
                                         '(:struct cairo-glyph-t)
                                         'index)
                     (first glyph)
                     (foreign-slot-value glyph-ptr
                                         '(:struct cairo-glyph-t)
                                         'x)
                     (coerce (second glyph) 'double-float)
                     (foreign-slot-value glyph-ptr
                                         '(:struct cairo-glyph-t)
                                         'y)
                     (coerce (third glyph) 'double-float)))
      (%cairo-show-glyphs cr glyphs-ptr num-glyphs))))

(export 'cairo-show-glyphs)

;;; ----------------------------------------------------------------------------
;;; cairo_show_text_glyphs ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_show_text_glyphs" cairo-show-text-glyphs) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-12-28}
  @argument[cr]{a @symbol{cairo-t} context}
  @argument[utf8]{a string of text encoded in UTF-8}
  @argument[utf8-len]{an integer with the length of @arg{utf8} in bytes,
    or -1 if it is NUL-terminated}
  @argument[glyphs]{array of @symbol{cairo-glyph-t} to show}
  @argument[num-glyphs]{an integer with the number of glyphs to show}
  @argument[clusters]{array of @symbol{cairo-text-cluster-t} cluster mapping
    information}
  @argument[num-clusters]{an integer with the number of clusters in the mapping}
  @argument[cluster-flags]{@symbol{cairo-text-cluster-flags-t} cluster mapping
    flags}
  @begin{short}
    This operation has rendering effects similar to the function
    @fun{cairo-show-glyphs} but, if the target surface supports it, uses the
    provided text and cluster mapping to embed the text for the glyphs shown in
    the output.
  @end{short}
  If the target does not support the extended attributes, this function acts
  like the basic the function @fun{cairo-show-glyphs} as if it had been passed
  @arg{glyphs} and @arg{num-glyphs}.

  The mapping between utf8 and glyphs is provided by an array of clusters.
  Each cluster covers a number of text bytes and glyphs, and neighboring
  clusters cover neighboring areas of utf8 and glyphs. The clusters should
  collectively cover utf8 and glyphs in entirety.

  The first cluster always covers bytes from the beginning of utf8. If
  @arg{cluster-flags} do not have the @code{:backward} set, the first cluster
  also covers the beginning of glyphs, otherwise it covers the end of the
  glyphs array and following clusters move backward.

  See the @symbol{cairo-text-cluster-t} structure for constraints on valid
  clusters.
  @see-symbol{cairo-t}
  @see-symbol{cairo-glyph-t}
  @see-symbol{cairo-text-cluster-t}
  @see-symbol{cairo-text-cluster-flags-t}
  @see-function{cairo-show-glyphs}"
  (cr (:pointer (:struct cairo-t)))
  (utf8 :string)
  (utf8-len :int)
  (glyphs (:pointer (:struct cairo-glyph-t)))
  (num-glyphs :int)
  (clusters (:pointer (:struct cairo-text-cluster-t)))
  (num-clusters :int)
  (cluster-flags cairo-text-cluster-flags-t))

(export 'cairo-show-text-glyphs)

;;; ----------------------------------------------------------------------------
;;; cairo_font_extents ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_font_extents" %cairo-font-extents) :void
  (cr (:pointer (:struct cairo-t)))
  (extents (:pointer (:struct cairo-font-extents-t))))

(defun cairo-font-extents (cr)
 #+cl-cffi-gtk-documentation
 "@version{2020-12-15}
  @argument[cr]{a @symbol{cairo-t} context}
  @return{The @symbol{cairo-font-extents-t} instance.}
  @begin{short}
    Gets the font extents for the currently selected font.
  @end{short}
  @see-symbol{cairo-t}
  @see-symbol{cairo-font-extents-t}"
  (with-foreign-object (extents '(:struct cairo-font-extents-t))
    (%cairo-font-extents cr extents)
    extents))

(export 'cairo-font-extents)

;;; ----------------------------------------------------------------------------
;;; cairo_text_extents ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_text_extents" %cairo-text-extents) :void
  (cr (:pointer (:struct cairo-t)))
  (utf8 :string)
  (extents (:pointer (:struct cairo-text-extents-t))))

(defun cairo-text-extents (cr utf8)
 #+cl-cffi-gtk-documentation
 "@version{2020-12-29}
  @argument[cr]{a @symbol{cairo-t} context}
  @argument[utf8]{a string of text encoded in UTF-8}
  @begin{return}
    A @symbol{cairo-text-extents-t} instance with the extents of @arg{utf8}.
  @end{return}
  @begin{short}
    Gets the extents for a string of text.
  @end{short}
  The extents describe a user-space rectangle that encloses the \"inked\"
  portion of the text, as it would be drawn by the function
  @fun{cairo-show-text}. Additionally, the @code{x-advance} and @code{y-advance}
  values indicate the amount by which the current point would be advanced by
  the function @fun{cairo-show-text}.

  Note that whitespace characters do not directly contribute to the size of
  the rectangle (@code{width} and @code{height}). They do contribute indirectly
  by changing the position of non-whitespace characters. In particular, trailing
  whitespace characters are likely to not affect the size of the rectangle,
  though they will affect the @code{x-advance} and @code{y-advance} values.
  @see-symbol{cairo-t}
  @see-symbol{cairo-text-extents-t}
  @see-function{cairo-show-text}"
  (with-foreign-object (extents '(:struct cairo-text-extents-t))
    (%cairo-text-extents cr utf8 extents)
    extents))

(export 'cairo-text-extents)

;;; ----------------------------------------------------------------------------
;;; cairo_glyph_extents ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_glyph_extents" %cairo-glyph-extents) :void
  (cr (:pointer (:struct cairo-t)))
  (glyphs (:pointer (:struct cairo-glyph-t)))
  (num-glyphs :int)
  (extents (:pointer (:struct cairo-text-extents-t))))

(defun cairo-glyph-extents (cr glyphs)
 #+cl-cffi-gtk-documentation
 "@version{2020-12-29}
  @argument[cr]{a @symbol{cairo-t} context}
  @argument[glyphs]{a list of glyphs of the form @code{'((index1 x1 y1)
    (index2 x2 y2) ...)}}
  @begin{return}
    A @symbol{cairo-text-extents-t} instance with the extents of @arg{glyphs}.
  @end{return}
  @begin{short}
    Gets the extents for a list of glyphs.
  @end{short}
  The extents describe a user-space rectangle that encloses the \"inked\"
  portion of the glyphs, as they would be drawn by the function
  @fun{cairo-show-glyphs}. Additionally, the @code{x-advance} and
  @code{y-advance} values indicate the amount by which the current point would
  be advanced by the function @fun{cairo-show-glyphs}.

  Note that whitespace glyphs do not contribute to the size of the rectangle
  (@code{width} and @code{height}).
  @see-symbol{cairo-t}
  @see-symbol{cairo-glyph-t}
  @see-symbol{cairo-text-extents-t}
  @see-function{cairo-show-glyphs}"
  (let ((num-glyphs (length glyphs)))
    (with-foreign-objects ((extents '(:struct cairo-text-extents-t))
                           (glyphs-ptr '(:struct cairo-glyph-t) num-glyphs))
      (loop for count from 0 below num-glyphs
            for glyph-ptr = (mem-aptr glyphs-ptr '(:struct cairo-glyph-t) count)
            for glyph = (pop glyphs)
            do (setf (foreign-slot-value glyph-ptr
                                         '(:struct cairo-glyph-t)
                                         'cairo::index)
                     (first glyph)
                     (foreign-slot-value glyph-ptr
                                         '(:struct cairo-glyph-t)
                                         'cairo::x)
                     (coerce (second glyph) 'double-float)
                     (foreign-slot-value glyph-ptr
                                         '(:struct cairo-glyph-t)
                                         'cairo::y)
                     (coerce (third glyph) 'double-float)))
      (%cairo-glyph-extents cr glyphs-ptr num-glyphs extents)
      extents)))

(export 'cairo-glyph-extents)

;;; ----------------------------------------------------------------------------
;;; cairo_toy_font_face_create ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_toy_font_face_create" cairo-toy-font-face-create)
    (:pointer (:struct cairo-font-face-t))
 #+cl-cffi-gtk-documentation
 "@version{2020-12-28}
  @argument[familiy]{a string with the font family name, encoded in UTF-8}
  @argument[slant]{the @symbol{cairo-font-slant-t} slant for the font}
  @argument[weight]{the @symbol{cairo-font-weight-t}weight for the font}
  @begin{return}
    A newly created @symbol{cairo-font-face-t} instance. Free with the function
    @fun{cairo-font-face-destroy} when you are done using it.
  @end{return}
  @begin{short}
    Creates a font face from a triplet of family, slant, and weight.
  @end{short}
  These font faces are used in implementation of the the Cairo \"toy\" font API.

  If @arg{family} is the zero-length string \"\", the platform-specific default
  family is assumed. The default family then can be queried using the function
  @fun{cairo-toy-font-face-get-family}.

  The function @fun{cairo-select-font-face} uses this to create font faces. See
  that function for limitations and other details of toy font faces.
  @see-symbol{cairo-font-face-t}
  @see-symbol{cairo-font-slant-t}
  @see-symbol{cairo-font-weight-t}
  @see-function{cairo-font-face-destroy}
  @see-function{cairo-toy-font-face-get-family}
  @see-function{cairo-select-font-face}"
  (family :string)
  (slant cairo-font-slant-t)
  (weight cairo-font-weight-t))

(export 'cairo-toy-font-face-create)

;;; ----------------------------------------------------------------------------
;;; cairo_toy_font_face_get_family ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_toy_font_face_get_family" cairo-toy-font-face-get-family)
    :string
 #+cl-cffi-gtk-documentation
 "@version{2020-12-28}
  @argument[font-face]{a @symbol{cairo-font-face-t} toy font face}
  @begin{return}
    The family name. This string is owned by the font face and remains valid
    as long as the font face is alive (referenced).
  @end{return}
  @begin{short}
    Gets the familly name of a toy font.
  @end{short}
  @see-symbol{cairo-font-face-t}"
  (font-face (:pointer (:struct cairo-font-face-t))))

(export 'cairo-toy-font-face-get-family)

;;; ----------------------------------------------------------------------------
;;; cairo_toy_font_face_get_slant ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_toy_font_face_get_slant" cairo-toy-font-face-get-slant)
    cairo-font-slant-t
 #+cl-cffi-gtk-documentation
 "@version{2020-12-28}
  @argument[font-face]{a @symbol{cairo-font-face-t} toy font face}
  @return{The @symbol{cairo-font-slant-t} slant value.}
  @short{Gets the slant a toy font.}
  @see-symbol{cairo-font-face-t}
  @see-symbol{cairo-font-slant-t}"
  (font-face (:pointer (:struct cairo-font-face-t))))

(export 'cairo-toy-font-face-get-slant)

;;; ----------------------------------------------------------------------------
;;; cairo_toy_font_face_get_weight ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_toy_font_face_get_weight" cairo-toy-font-face-get-weight)
    cairo-font-weight-t
 #+cl-cffi-gtk-documentation
 "@version{2020-12-28}
  @argument[font-face]{a @symbol{cairo-font-face-t} toy font face}
  @return{The @symbol{cairo-font-weight-t} weight value.}
  @short{Gets the weight a toy font.}
  @see-symbol{cairo-font-face-t}
  @see-symbol{cairo-font-weight-t}"
  (font-face (:pointer (:struct cairo-font-face-t))))

(export 'cairo-toy-font-face-get-weight)

;;; ----------------------------------------------------------------------------
;;; cairo_glyph_allocate ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_glyph_allocate" cairo-glyph-allocate)
    (:pointer (:struct cairo-glyph-t))
 #+cl-cffi-gtk-documentation
 "@version{2020-12-28}
  @argument[num-glyphs]{an integer with the number of glyphs to allocate}
  @begin{return}
    The newly allocated array of @symbol{cairo-glyph-t} glyphs that should be
    freed using the function @fun{cairo-glyph-free}.
  @end{return}
  @begin{short}
    Allocates an array of @symbol{cairo-glyph-t}'s.
  @end{short}
  This function is only useful in implementations of
  @code{cairo-user-scaled-font-text-to-glyphs-func-t} where the user needs to
  allocate an array of glyphs that cairo will free. For all other uses, user
  can use their own allocation method for glyphs.

  This function returns NULL if @arg{num-glyphs} is not positive, or if out of
  memory. That means, the NULL return value signals out-of-memory only if
  @arg{num-glyphs} was positive.
  @see-symbol{cairo-glyph-t}
  @see-function{cairo-glyph-free}"
  (num-glyphs :int))

(export 'cairo-glyph-allocate)

;;; ----------------------------------------------------------------------------
;;; cairo_glyph_free ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_glyph_free" cairo-glyph-free) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-12-28}
  @argument[glyphs]{array of @symbol{cairo-glyph-t} glyphs to free, or NULL}
  @begin{short}
    Frees an array of @symbol{cairo-glyph-t}'s allocated using the function
    @fun{cairo-glyph-allocate}.
  @end{short}
  This function is only useful to free glyph array returned by the function
  @fun{cairo-scaled-font-text-to-glyphs} where cairo returns an array of glyphs
  that the user will free. For all other uses, user can use their own allocation
  method for glyphs.
  @see-symbol{cairo-glyph-t}
  @see-function{cairo-glyph-allocate}
  @see-function{cairo-scaled-font-text-to-glyphs}"
  (glyphs (:pointer (:struct cairo-glyph-t))))

(export 'cairo-glyph-free)

;;; ----------------------------------------------------------------------------
;;; cairo_text_cluster_allocate ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_text_cluster_allocate" cairo-text-cluster-allocate)
    (:pointer (:struct cairo-text-cluster-t))
 #+cl-cffi-gtk-documentation
 "@version{2020-12-28}
  @argument[num-clusters]{an integer with the number of text clusters to
    allocate}
  @begin{return}
    The newly allocated array of @symbol{cairo-text-cluster-t} text clusters
    that should be freed using the function @fun{cairo-text-cluster-free}.
  @end{return}
  @begin{short}
    Allocates an array of @symbol{cairo-text-cluster-t}'s.
  @end{short}
  This function is only useful in implementations of
  @code{cairo-user-scaled-font-text-to-glyphs-func-t} where the user needs to
  allocate an array of text clusters that Cairo will free. For all other uses,
  user can use their own allocation method for text clusters.

  This function returns NULL if @arg{num-clusters} is not positive, or if out
  of memory. That means, the NULL return value signals out-of-memory only if
  @arg{num-clusters} was positive.

  @see-symbol{cairo-text-cluster-t}
  @see-function{cairo-text-cluster-free}"
  (num-clusters :int))

(export 'cairo-text-cluster-allocate)

;;; ----------------------------------------------------------------------------
;;; cairo_text_cluster_free ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_text_cluster_free" cairo-text-cluster-free) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-12-28}
  @argument[clusters]{array of @symbol{cairo-text-cluster-t} text clusters to
    free, or NULL}
  @begin{short}
    Frees an array of @symbol{cairo-text-cluster-t}'s allocated using the
    function @fun{cairo-text-cluster-allocate}.
  @end{short}
  This function is only useful to free text cluster array returned by the
  function @fun{cairo-scaled-font-text-to-glyphs} where Cairo returns an array
  of text clusters that the user will free. For all other uses, user can use
  their own allocation method for text clusters.
  @see-symbol{cairo-text-cluster-t}
  @see-function{cairo-text-cluster-allocate}
  @see-function{cairo-scaled-font-text-to-glyphs}"
  (clusters (:pointer (:struct cairo-text-cluster-t))))

(export 'cairo-text-cluster-free)

;;; --- End of file cairo.text.lisp --------------------------------------------
