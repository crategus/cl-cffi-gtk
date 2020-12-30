;;; ----------------------------------------------------------------------------
;;; cairo.scaled-font.lisp
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
;;; cairo_scaled_font_t
;;;
;;;     Font face at particular size and options
;;;
;;; Synopsis
;;;
;;;     cairo_scaled_font_t
;;;     cairo_font_extents_t
;;;     cairo_text_extents_t
;;;
;;; Functions
;;;
;;;     cairo_scaled_font_create
;;;     cairo_scaled_font_reference
;;;     cairo_scaled_font_destroy
;;;     cairo_scaled_font_status
;;;
;;;     cairo_scaled_font_extents
;;;     cairo_scaled_font_text_extents
;;;     cairo_scaled_font_glyph_extents
;;;     cairo_scaled_font_text_to_glyphs
;;;     cairo_scaled_font_get_font_face
;;;     cairo_scaled_font_get_font_options
;;;     cairo_scaled_font_get_font_matrix
;;;     cairo_scaled_font_get_ctm
;;;     cairo_scaled_font_get_scale_matrix
;;;     cairo_scaled_font_get_type
;;;     cairo_scaled_font_get_reference_count
;;;     cairo_scaled_font_set_user_data
;;;     cairo_scaled_font_get_user_data
;;;
;;; Description
;;;
;;;     cairo_scaled_font_t represents a realization of a font face at a
;;;     particular size and transformation and a certain set of font options.
;;; ----------------------------------------------------------------------------

(in-package :cairo)

;;; ----------------------------------------------------------------------------
;;; cairo_scaled_font_t
;;; ----------------------------------------------------------------------------

(defcstruct cairo-scaled-font-t)

#+cl-cffi-gtk-documentation
(setf (gethash 'cairo-scaled-font-t atdoc:*symbol-name-alias*)
      "CStruct"
      (gethash 'cairo-scaled-font-t atdoc:*external-symbols*)
 "@version{2020-12-15}
  @begin{short}
    A @sym{cairo-scaled-font-t} structure is a font scaled to a particular size
    and device resolution.
  @end{short}
  A @sym{cairo-scaled-font-t} structure is most useful for low-level font usage
  where a library or application wants to cache a reference to a scaled font to
  speed up the computation of metrics.

  There are various types of scaled fonts, depending on the font backend they
  use. The type of a scaled font can be queried using the function
  @fun{cairo-scaled-font-get-type}.

  Memory management of the @sym{cairo-scaled-font-t} structure is done with the
  functions @fun{cairo-scaled-font-reference} and
  @fun{cairo-scaled-font-destroy}.
  @see-function{cairo-scaled-font-get-type}
  @see-function{cairo-scaled-font-reference}
  @see-function{cairo-scaled-font-destroy}")

(export 'cairo-scaled-font-t)

;;; ----------------------------------------------------------------------------
;;; cairo_font_extents_t
;;; ----------------------------------------------------------------------------

(defcstruct cairo-font-extents-t
  (ascent :double)
  (descent :double)
  (height :double)
  (max-x-advance :double)
  (max-y-advance :double))

#+cl-cffi-gtk-documentation
(setf (gethash 'cairo-font-extents-t atdoc:*symbol-name-alias*)
      "CStruct"
      (gethash 'cairo-font-extents-t atdoc:*external-symbols*)
 "@version{2020-12-29}
  @begin{short}
    The @sym{cairo-font-extents-t} structure stores metric information for a
    font.
  @end{short}
  Values are given in the current user-space coordinate system.

  Because font metrics are in user-space coordinates, they are mostly, but not
  entirely, independent of the current transformation matrix. If you call
  @code{(cairo-scale cr 2.0 2.0)}, text will be drawn twice as big, but the
  reported text extents will not be doubled. They will change slightly due to
  hinting, so you can not assume that metrics are independent of the
  transformation matrix), but otherwise will remain unchanged.
  @begin{pre}
(defcstruct cario-font-extents-t
  (ascent :double)
  (descent :double)
  (height :double)
  (max-x-advance :double)
  (max-y-advance :double))
  @end{pre}
  @begin[code]{table}
    @entry[ascent]{The distance that the font extends above the baseline. Note
      that this is not always exactly equal to the maximum of the extents of
      all the glyphs in the font, but rather is picked to express the font
      designer's intent as to how the font should align with elements above it.}
    @entry[descent]{The distance that the font extends below the baseline. This
      value is positive for typical fonts that include portions below the
      baseline. Note that this is not always exactly equal to the maximum of
      the extents of all the glyphs in the font, but rather is picked to express
      the font designer's intent as to how the font should align with elements
      below it.}
    @entry[height]{The recommended vertical distance between baselines when
      setting consecutive lines of text with the font. This is greater than
      @code{ascent} + @code{descent} by a quantity known as the line spacing or
      external leading. When space is at a premium, most fonts can be set with
      only a distance of @code{ascent} + @code{descent} between lines.}
    @entry[max-x-advance]{The maximum distance in the x direction that the
      origin is advanced for any glyph in the font.}
    @entry[max-y-advance]{The maximum distance in the y direction that the
      origin is advanced for any glyph in the font. This will be zero for normal
      fonts used for horizontal writing. The scripts of East Asia are sometimes
      written vertically.}
  @end{table}
  @see-function{cairo-scale}
  @see-function{cairo-font-extents}")

(export 'cairo-font-extents-t)

;;; --- cairo-font-extents-ascent ----------------------------------------------

(defun cairo-font-extents-ascent (extents)
 #+cl-cffi-gtk-documentation
 "@version{2020-12-29}
  @argument[extents]{a @symbol{cairo-font-extents-t} instance}
  @begin{short}
    Accessor of the @code{ascent} slot of the @symbol{cairo-font-extents-t}
    structure.
  @end{short}

  The distance that the font extends above the baseline. Note that this is not
  always exactly equal to the maximum of the extents of all the glyphs in the
  font, but rather is picked to express the font designer's intent as to how
  the font should align with elements above it.
  @see-symbol{cairo-font-extents-t}"
  (foreign-slot-value extents '(:struct cairo-font-extents-t) 'ascent))

(export 'cairo-font-extents-ascent)

;;; --- cairo-font-extents-descent ---------------------------------------------

(defun cairo-font-extents-descent (extents)
 #+cl-cffi-gtk-documentation
 "@version{2020-12-29}
  @argument[extents]{a @symbol{cairo-font-extents-t} instance}
  @begin{short}
    Accessor of the @code{descent} slot of the @symbol{cairo-font-extents-t}
    structure.
  @end{short}

  The distance that the font extends below the baseline. This value is positive
  for typical fonts that include portions below the baseline. Note that this is
  not always exactly equal to the maximum of the extents of all the glyphs in
  the font, but rather is picked to express the font designer's intent as to
  how the font should align with elements below it.
  @see-symbol{cairo-font-extents-t}"
  (foreign-slot-value extents '(:struct cairo-font-extents-t) 'descent))

(export 'cairo-font-extents-descent)

;;; --- cairo-font-extents-height ----------------------------------------------

(defun cairo-font-extents-height (extents)
 #+cl-cffi-gtk-documentation
 "@version{2020-12-29}
  @argument[extents]{a @symbol{cairo-font-extents-t} instance}
  @begin{short}
    Accessor of the @code{height} slot of the @symbol{cairo-font-extents-t}
    structure.
  @end{short}

  The recommended vertical distance between baselines when setting consecutive
  lines of text with the font. This is greater than @code{ascent} +
  @code{descent} by a quantity known as the line spacing or external leading.
  When space is at a premium, most fonts can be set with only a distance of
  @code{ascent} + @code{descent} between lines.
  @see-symbol{cairo-font-extents-t}"
  (foreign-slot-value extents '(:struct cairo-font-extents-t) 'height))

(export 'cairo-font-extents-height)

;;; --- cairo-font-extents-max-x-advance ---------------------------------------

(defun cairo-font-extents-max-x-advance (extents)
 #+cl-cffi-gtk-documentation
 "@version{2020-12-29}
  @argument[extents]{a @symbol{cairo-font-extents-t} instance}
  @begin{short}
    Accessor of the @code{max-x-advance} slot of the
    @symbol{cairo-font-extents-t} structure.
  @end{short}

  The maximum distance in the x direction that the origin is advanced for any
  glyph in the font.
  @see-symbol{cairo-font-extents-t}"
  (foreign-slot-value extents '(:struct cairo-font-extents-t) 'max-x-advance))

(export 'cairo-font-extents-max-x-advance)

;;; --- cairo-font-extents-max-y-advance ---------------------------------------

(defun cairo-font-extents-max-y-advance (extents)
 #+cl-cffi-gtk-documentation
 "@version{2020-12-29}
  @argument[extents]{a @symbol{cairo-font-extents-t} instance}
  @begin{short}
    Accessor of the @code{max-y-advance} slot of the
    @symbol{cairo-font-extents-t} structure.
  @end{short}

  The maximum distance in the y direction that the origin is advanced for any
  glyph in the font. This will be zero for normal fonts used for horizontal
  writing. The scripts of East Asia are sometimes written vertically.
  @see-symbol{cairo-font-extents-t}"
  (foreign-slot-value extents '(:struct cairo-font-extents-t) 'max-y-advance))

(export 'cairo-font-extents-max-y-advance)

;;; ----------------------------------------------------------------------------
;;; cairo_text_extents_t
;;; ----------------------------------------------------------------------------

(defcstruct cairo-text-extents-t
  (x-bearing :double)
  (y-bearing :double)
  (width :double)
  (height :double)
  (x-advance :double)
  (y-advance :double))

#+cl-cffi-gtk-documentation
(setf (gethash 'cairo-text-extents-t atdoc:*symbol-name-alias*)
      "CStruct"
      (gethash 'cairo-text-extents-t atdoc:*external-symbols*)
 "@version{2020-12-29}
  @begin{short}
    The @sym{cairo-text-extents-t} structure stores the extents of a single
    glyph or a string of glyphs in user-space coordinates.
  @end{short}
  Because text extents are in user-space coordinates, they are mostly, but not
  entirely, independent of the current transformation matrix. If you call
  @code{(cairo-scale cr 2.0 2.0)}, text will be drawn twice as big, but the
  reported text extents will not be doubled. They will change slightly due to
  hinting, so you can not assume that metrics are independent of the
  transformation matrix, but otherwise will remain unchanged.
  @begin{pre}
(defcstruct cairo-text-extents-t
  (x-bearing :double)
  (y-bearing :double)
  (width :double)
  (height :double)
  (x-advance :double)
  (y-advance :double))
  @end{pre}
  @begin[code]{table}
    @entry[x-bearing]{The horizontal distance from the origin to the leftmost
      part of the glyphs as drawn. Positive if the glyphs lie entirely to the
      right of the origin.}
    @entry[y-bearing]{The vertical distance from the origin to the topmost part
      of the glyphs as drawn. Positive only if the glyphs lie completely below
      the origin; will usually be negative.}
    @entry[width]{Width of the glyphs as drawn.}
    @entry[height]{Height of the glyphs as drawn.}
    @entry[x-advance]{Distance to advance in the x direction after drawing
      these glyphs.}
    @entry[y-advance]{Distance to advance in the y direction after drawing
      these glyphs. Will typically be zero except for vertical text layout as
      found in East-Asian languages.}
  @end{table}
  @see-function{cairo-text-extents}
  @see-function{cairo-scaled-font-text-entents}")

(export 'cairo-text-extents-t)

;;; --- cairo-text-extents-x-bearing -------------------------------------------

(defun cairo-text-extents-x-bearing (extents)
 #+cl-cffi-gtk-documentation
 "@version{2020-12-29}
  @argument[extents]{a @symbol{cairo-text-extents-t} instance}
  @begin{short}
    Accessor of the @code{x-bearing} slot of the @symbol{cairo-text-extents-t}
    structure.
  @end{short}

  The horizontal distance from the origin to the leftmost part of the glyphs as
  drawn. Positive if the glyphs lie entirely to the right of the origin.
  @see-symbol{cairo-text-extents-t}"
  (foreign-slot-value extents '(:struct cairo-text-extents-t) 'x-bearing))

(export 'cairo-text-extents-x-bearing)

;;; --- cairo-text-extents-y-bearing -------------------------------------------

(defun cairo-text-extents-y-bearing (extents)
 #+cl-cffi-gtk-documentation
 "@version{2020-12-29}
  @argument[extents]{a @symbol{cairo-text-extents-t} instance}
  @begin{short}
    Accessor of the @code{y-bearing} slot of the @symbol{cairo-text-extents-t}
    structure.
  @end{short}

  The vertical distance from the origin to the topmost part of the glyphs as
  drawn. Positive only if the glyphs lie completely below the origin; will
  usually be negative.
  @see-symbol{cairo-text-extents-t}"
  (foreign-slot-value extents '(:struct cairo-text-extents-t) 'y-bearing))

(export 'cairo-text-extents-y-bearing)

;;; --- cairo-text-extents-width -----------------------------------------------

(defun cairo-text-extents-width (extents)
 #+cl-cffi-gtk-documentation
 "@version{2020-12-29}
  @argument[extents]{a @symbol{cairo-text-extents-t} instance}
  @begin{short}
    Accessor of the @code{width} slot of the @symbol{cairo-text-extents-t}
    structure.
  @end{short}

  Width of the glyphs as drawn.
  @see-symbol{cairo-text-extents-t}"
  (foreign-slot-value extents '(:struct cairo-text-extents-t) 'width))

(export 'cairo-text-extents-width)

;;; --- cairo-text-extents-height ----------------------------------------------

(defun cairo-text-extents-height (extents)
 #+cl-cffi-gtk-documentation
 "@version{2020-12-29}
  @argument[extents]{a @symbol{cairo-text-extents-t} instance}
  @begin{short}
    Accessor of the @code{height} slot of the @symbol{cairo-text-extents-t}
    structure.
  @end{short}

  Height of the glyphs as drawn.
  @see-symbol{cairo-text-extents-t}"
  (foreign-slot-value extents '(:struct cairo-text-extents-t) 'height))

(export 'cairo-text-extents-height)

;;; --- cairo-text-extents-x-advance -------------------------------------------

(defun cairo-text-extents-x-advance (extents)
 #+cl-cffi-gtk-documentation
 "@version{2020-12-29}
  @argument[extents]{a @symbol{cairo-text-extents-t} instance}
  @begin{short}
    Accessor of the @code{x-advance} slot of the @symbol{cairo-text-extents-t}
    structure.
  @end{short}

  Distance to advance in the x direction after drawing these glyphs.
  @see-symbol{cairo-text-extents-t}"
  (foreign-slot-value extents '(:struct cairo-text-extents-t) 'x-advance))

(export 'cairo-text-extents-x-advance)

;;; --- cairo-text-extents-y-advance -------------------------------------------

(defun cairo-text-extents-y-advance (extents)
 #+cl-cffi-gtk-documentation
 "@version{2020-12-29}
  @argument[extents]{a @symbol{cairo-text-extents-t} instance}
  @begin{short}
    Accessor of the @code{y-advance} slot of the @symbol{cairo-text-extents-t}
    structure.
  @end{short}

  Distance to advance in the y direction after drawing these glyphs. Will
  typically be zero except for vertical text layout as found in East-Asian
  languages.
  @see-symbol{cairo-text-extents-t}"
  (foreign-slot-value extents '(:struct cairo-text-extents-t) 'y-advance))

(export 'cairo-text-extents-y-advance)

;;; ----------------------------------------------------------------------------
;;; cairo_scaled_font_create ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_scaled_font_create" cairo-scaled-font-create)
    (:pointer (:struct cairo-scaled-font-t))
 #+cl-cffi-gtk-documentation
 "@version{2020-12-15}
  @argument[font-face]{a @symbol{cairo-font-face-t} structure}
  @argument[font-matrix]{font space to user space transformation
    @symbol{cairo-matrix-t} matrix for the font, in the simplest case of a N
    point font, this matrix is just a scale by N, but it can also be used to
    shear the font or stretch it unequally along the two axes, see the function
    @fun{cairo-set-font-matrix}}
  @argument[ctm]{user to device transformation @symbol{cairo-matrix-t} matrix
    with which the font will be used}
  @argument[options]{a @symbol{cairo-font-options-t} options to use when getting
    metrics for the font and rendering with it}
  @begin{return}
    A newly created @symbol{cairo-scaled-font-t} instance. Destroy with the
    function @fun{cairo-scaled-font-destroy}.
  @end{return}
  @begin{short}
    Creates a @symbol{cairo-scaled-font-t} instance from a font face and
    matrices that describe the size of the font and the environment in which it
    will be used.
  @end{short}
  @see-symbol{cairo-font-face-t}
  @see-symbol{cairo-matrix-t}
  @see-symbol{cairo-font-options-t}
  @see-function{cairo-set-font-matrix}"
  (font-face (:pointer (:struct cairo-font-face-t)))
  (font-matrix (:pointer (:struct cairo-matrix-t)))
  (ctm (:pointer (:struct cairo-matrix-t)))
  (options (:pointer (:struct cairo-font-options-t))))

(export 'cairo-scaled-font-create)

;;; ----------------------------------------------------------------------------
;;; cairo_scaled_font_reference ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_scaled_font_reference" cairo-scaled-font-reference)
    (:pointer (:struct cairo-scaled-font-t))
 #+cl-cffi-gtk-documentation
 "@version{2020-12-28}
  @argument[scaled-font]{a @symbol{cairo-scaled-font-t} instance, may be NULL
    in which case this function does nothing}
  @return{The referenced @symbol{cairo-scaled-font-t} instance.}
  @begin{short}
    Increases the reference count on @arg{scaled-font} by one.
  @end{short}
  This prevents @arg{scaled-font} from being destroyed until a matching call to
  the function @fun{cairo-scaled-font-destroy} is made.

  The number of references to a @symbol{cairo-scaled-font-t} instance can be
  get using the function @fun{cairo-scaled-font-get-reference-count}.
  @see-symbol{cairo-scaled-font-t}
  @see-function{cairo-scaled-font-destroy}
  @see-fun{cairo-scaled-font-get-reference-count}"
  (scaled-font (:pointer (:struct cairo-scaled-font-t))))

(export 'cairo-scaled-font-reference)

;;; ----------------------------------------------------------------------------
;;; cairo_scaled_font_destroy ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_scaled_font_destroy" cairo-scaled-font-destroy) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-12-28}
  @argument[scaled-font]{a @symbol{cairo-scaled-font-t} instance}
  @begin{short}
    Decreases the reference count on @arg{scaled-font} by one.
  @end{short}
  If the result is zero, then @arg{scaled-font} and all associated resources
  are freed. See the function @fun{cairo-scaled-font-reference}.
  @see-symbol{cairo-scaled-font-t}
  @see-function{cairo-scaled-font-reference}"
  (scaled-font (:pointer (:struct cairo-scaled-font-t))))

(export 'cairo-scaled-font-destroy)

;;; ----------------------------------------------------------------------------
;;; cairo_scaled_font_status ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_scaled_font_status" cairo-scaled-font-status) cairo-status-t
 #+cl-cffi-gtk-documentation
 "@version{2020-12-28}
  @argument[scaled-font]{a @symbol{cairo-scaled-font-t} instance}
  @begin{return}
    @code{:success} or another error such as @code{:no-memory}.
  @end{return}
  @begin{short}
    Checks whether an error has previously occurred for this scaled font.
  @end{short}
  @see-symbol{cairo-scaled-font-t}"
  (scaled-font (:pointer (:struct cairo-scaled-font-t))))

(export 'cairo-scaled-font-status)

;;; ----------------------------------------------------------------------------
;;; cairo_scaled_font_extents ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_scaled_font_extents" cairo-scaled-font-extents) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-12-28}
  @argument[scaled-font]{a @symbol{cairo-scaled-font-t} instance}
  @argument[extents]{a @symbol{cairo-font-extents-t} instance which to store
    the retrieved extents}
  @begin{short}
    Gets the metrics for a scaled font.
  @end{short}
  @see-symbol{cairo-scaled-font-t}
  @see-symbol{cairo-font-extents-t}"
  (scaled-font (:pointer (:struct cairo-scaled-font-t)))
  (extents (:pointer (:struct cairo-font-extents-t))))

(export 'cairo-scaled-font-extents)

;;; ----------------------------------------------------------------------------
;;; cairo_scaled_font_text_extents ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_scaled_font_text_extents" cairo-scaled-font-text-extents) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-12-15}
  @argument[scaled-font]{a @symbol{cairo-scaled-font-t} instance}
  @argument[utf8]{a string of text, encoded in UTF-8}
  @argument[extents]{a @symbol{cairo-text-extents-t} instance which to store
    the retrieved extents}
  @begin{short}
    Gets the extents for a string of text.
  @end{short}
  The extents describe a user-space rectangle that encloses the \"inked\"
  portion of the text drawn at the origin (0,0), as it would be drawn by
  the function @fun{cairo-show-text} if the Cairo graphics state were set to
  the same @code{font-face}, @code{font-matrix}, @code{ctm}, and
  @code{font-options} as @arg{scaled-font}. Additionally, the @code{x-advance}
  and @code{y-advance} values indicate the amount by which the current point
  would be advanced by the function @fun{cairo-show-text}.

  Note that whitespace characters do not directly contribute to the size of
  the rectangle (@code{extents.width} and @code{extents.height}). They do
  contribute indirectly by changing the position of non-whitespace characters.
  In particular, trailing whitespace characters are likely to not affect the
  size of the rectangle, though they will affect the @code{x-advance} and
  @code{y-advance} values.
  @see-symbol{cairo-scaled-font-t}
  @see-type{cairo-text-extents-t}
  @see-function{cairo-show-text}"
  (scaled-font (:pointer (:struct cairo-scaled-font-t)))
  (utf8 :string)
  (extents (:pointer (:struct cairo-text-extents-t))))

(export 'cairo-scaled-font-text-extents)

;;; ----------------------------------------------------------------------------
;;; cairo_scaled_font_glyph_extents ()
;;;
;;; void
;;; cairo_scaled_font_glyph_extents (cairo_scaled_font_t *scaled_font,
;;;                                  const cairo_glyph_t *glyphs,
;;;                                  int num_glyphs,
;;;                                  cairo_text_extents_t *extents);
;;;
;;; Gets the extents for an array of glyphs. The extents describe a user-space
;;; rectangle that encloses the "inked" portion of the glyphs, (as they would be
;;; drawn by cairo_show_glyphs() if the cairo graphics state were set to the
;;; same font_face, font_matrix, ctm, and font_options as scaled_font).
;;; Additionally, the x_advance and y_advance values indicate the amount by
;;; which the current point would be advanced by cairo_show_glyphs().
;;;
;;; Note that whitespace glyphs do not contribute to the size of the rectangle
;;; (extents.width and extents.height).
;;;
;;; scaled_font :
;;;     a cairo_scaled_font_t
;;;
;;; glyphs :
;;;     an array of glyph IDs with X and Y offsets.
;;;
;;; num_glyphs :
;;;     the number of glyphs in the glyphs array
;;;
;;; extents :
;;;     a cairo_text_extents_t which to store the retrieved extents.
;;;
;;; Since 1.0
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_scaled_font_glyph_extents" cairo-scaled-font-glyph-extents)
    :void
  (scaled-font (:pointer (:struct cairo-scaled-font-t)))
  (glyphs :pointer) ; (:pointer (:struct cairo-glyph-t))
  (num-glyphs :int)
  (extents (:pointer (:struct cairo-text-extents-t))))

(export 'cairo-scaled-font-glyph-extents)

;;; ----------------------------------------------------------------------------
;;; cairo_scaled_font_text_to_glyphs ()
;;;
;;; cairo_status_t
;;; cairo_scaled_font_text_to_glyphs (cairo_scaled_font_t *scaled_font,
;;;                                   double x,
;;;                                   double y,
;;;                                   const char *utf8,
;;;                                   int utf8_len,
;;;                                   cairo_glyph_t **glyphs,
;;;                                   int *num_glyphs,
;;;                                   cairo_text_cluster_t **clusters,
;;;                                   int *num_clusters,
;;;                                   cairo_text_cluster_flags_t *cluster_flags)
;;;
;;; Converts UTF-8 text to an array of glyphs, optionally with cluster mapping,
;;; that can be used to render later using scaled_font.
;;;
;;; If glyphs initially points to a non-NULL value, that array is used as a
;;; glyph buffer, and num_glyphs should point to the number of glyph entries
;;; available there. If the provided glyph array is too short for the
;;; conversion, a new glyph array is allocated using cairo_glyph_allocate() and
;;; placed in glyphs. Upon return, num_glyphs always contains the number of
;;; generated glyphs. If the value glyphs points to has changed after the call,
;;; the user is responsible for freeing the allocated glyph array using
;;; cairo_glyph_free(). This may happen even if the provided array was large
;;; enough.
;;;
;;; If clusters is not NULL, num_clusters and cluster_flags should not be NULL,
;;; and cluster mapping will be computed. The semantics of how cluster array
;;; allocation works is similar to the glyph array. That is, if clusters
;;; initially points to a non-NULL value, that array is used as a cluster
;;; buffer, and num_clusters should point to the number of cluster entries
;;; available there. If the provided cluster array is too short for the
;;; conversion, a new cluster array is allocated using
;;; cairo_text_cluster_allocate() and placed in clusters. Upon return,
;;; num_clusters always contains the number of generated clusters. If the value
;;; clusters points at has changed after the call, the user is responsible for
;;; freeing the allocated cluster array using cairo_text_cluster_free(). This
;;; may happen even if the provided array was large enough.
;;;
;;; In the simplest case, glyphs and clusters can point to NULL initially and a
;;; suitable array will be allocated. In code:
;;;
;;; cairo_status_t status;
;;;
;;; cairo_glyph_t *glyphs = NULL;
;;; int num_glyphs;
;;; cairo_text_cluster_t *clusters = NULL;
;;; int num_clusters;
;;; cairo_text_cluster_flags_t cluster_flags;
;;;
;;; status = cairo_scaled_font_text_to_glyphs (scaled_font,
;;;                                            x, y,
;;;                                            utf8, utf8_len,
;;;                                            &glyphs, &num_glyphs,
;;;                                            &clusters, &num_clusters,
;;;                                            &cluster_flags);
;;;
;;; if (status == CAIRO_STATUS_SUCCESS) {
;;;     cairo_show_text_glyphs (cr,
;;;                             utf8, utf8_len,
;;;                             glyphs, num_glyphs,
;;;                             clusters, num_clusters, cluster_flags);
;;;
;;;     cairo_glyph_free (glyphs);
;;;     cairo_text_cluster_free (clusters);
;;; }
;;;
;;; If no cluster mapping is needed:
;;;
;;; cairo_status_t status;
;;;
;;; cairo_glyph_t *glyphs = NULL;
;;; int num_glyphs;
;;;
;;; status = cairo_scaled_font_text_to_glyphs (scaled_font,
;;;                                            x, y,
;;;                                            utf8, utf8_len,
;;;                                            &glyphs, &num_glyphs,
;;;                                            NULL, NULL,
;;;                                            NULL);
;;;
;;; if (status == CAIRO_STATUS_SUCCESS) {
;;;     cairo_show_glyphs (cr, glyphs, num_glyphs);
;;;     cairo_glyph_free (glyphs);
;;; }
;;;
;;; If stack-based glyph and cluster arrays are to be used for small arrays:
;;;
;;; cairo_status_t status;
;;;
;;; cairo_glyph_t stack_glyphs[40];
;;; cairo_glyph_t *glyphs = stack_glyphs;
;;; int num_glyphs = sizeof (stack_glyphs) / sizeof (stack_glyphs[0]);
;;; cairo_text_cluster_t stack_clusters[40];
;;; cairo_text_cluster_t *clusters = stack_clusters;
;;; int num_clusters = sizeof (stack_clusters) / sizeof (stack_clusters[0]);
;;; cairo_text_cluster_flags_t cluster_flags;
;;;
;;; status = cairo_scaled_font_text_to_glyphs (scaled_font,
;;;                                            x, y,
;;;                                            utf8, utf8_len,
;;;                                            &glyphs,
;;;                                            &num_glyphs,
;;;                                            &clusters,
;;;                                            &num_clusters,
;;;                                            &cluster_flags);
;;;
;;; if (status == CAIRO_STATUS_SUCCESS) {
;;;     cairo_show_text_glyphs (cr,
;;;                             utf8, utf8_len,
;;;                             glyphs, num_glyphs,
;;;                             clusters, num_clusters, cluster_flags);
;;;
;;;     if (glyphs != stack_glyphs)
;;;         cairo_glyph_free (glyphs);
;;;     if (clusters != stack_clusters)
;;;         cairo_text_cluster_free (clusters);
;;; }
;;;
;;; For details of how clusters, num_clusters, and cluster_flags map input
;;; UTF-8 text to the output glyphs see cairo_show_text_glyphs().
;;;
;;; The output values can be readily passed to cairo_show_text_glyphs(),
;;; cairo_show_glyphs(), or related functions, assuming that the exact same
;;; scaled_font is used for the operation.
;;;
;;; scaled_font :
;;;     a cairo_scaled_font_t
;;;
;;; x :
;;;     X position to place first glyph
;;;
;;; y :
;;;     Y position to place first glyph
;;;
;;; utf8 :
;;;     a string of text encoded in UTF-8
;;;
;;; utf8_len :
;;;     length of utf8 in bytes, or -1 if it is NUL-terminated
;;;
;;; glyphs :
;;;     pointer to array of glyphs to fill
;;;
;;; num_glyphs :
;;;     pointer to number of glyphs
;;;
;;; clusters :
;;;     pointer to array of cluster mapping information to fill, or NULL
;;;
;;; num_clusters :
;;;     pointer to number of clusters, or NULL
;;;
;;; cluster_flags :
;;;     pointer to location to store cluster flags corresponding to the output
;;;     clusters, or NULL
;;;
;;; Returns :
;;;     CAIRO_STATUS_SUCCESS upon success, or an error status if the input
;;;     values are wrong or if conversion failed. If the input values are
;;;     correct but the conversion failed, the error status is also set on
;;;     scaled_font.
;;;
;;; Since 1.8
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_scaled_font_text_to_glyphs" cairo-scaled-font-text-to-glyphs)
    cairo-status-t
  (scaled-font (:pointer (:struct cairo-scaled-font-t)))
  (x :double)
  (y :double)
  (utf8 :string)
  (utf8-len :int)
  (glyphs :pointer)     ; (:pointer (:struct cairo-glyph-t))
  (num-glyphs :int)
  (clusters :pointer)   ; (:pointer (:struct cairo-text-cluster-t))
  (num-clusters :int)
  (cluster-flags :int)) ; cairo-text-cluster-flags-t

(export 'cairo-scaled-font-text-to-glyphs)

;;; ----------------------------------------------------------------------------
;;; cairo_scaled_font_get_font_face ()
;;;
;;; cairo_font_face_t *
;;; cairo_scaled_font_get_font_face (cairo_scaled_font_t *scaled_font)
;;;
;;; Gets the font face that this scaled font uses. This might be the font face
;;; passed to cairo_scaled_font_create(), but this does not hold true for all
;;; possible cases.
;;;
;;; scaled_font :
;;;     a cairo_scaled_font_t
;;;
;;; Returns :
;;;     The cairo_font_face_t with which scaled_font was created. This object is
;;;     owned by cairo. To keep a reference to it, you must call
;;;     cairo_scaled_font_reference().
;;;
;;; Since 1.2
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_scaled_font_get_font_face" cairo-scaled-font-get-font-face)
    (:pointer (:struct cairo-font-face-t))
  (scaled-font (:pointer (:struct cairo-scaled-font-t))))

(export 'cairo-scaled-font-get-font-face)

;;; ----------------------------------------------------------------------------
;;; cairo_scaled_font_get_font_options ()
;;;
;;; void
;;; cairo_scaled_font_get_font_options (cairo_scaled_font_t *scaled_font,
;;;                                     cairo_font_options_t *options);
;;;
;;; Stores the font options with which scaled_font was created into options.
;;;
;;; scaled_font :
;;;     a cairo_scaled_font_t
;;;
;;; options :
;;;     return value for the font options
;;;
;;; Since 1.2
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_scaled_font_get_font_options"
           cairo-scaled-font-get-font-options) :void
  (scaled-font (:pointer (:struct cairo-scaled-font-t)))
  (options (:pointer (:struct cairo-font-options-t))))

(export 'cairo-scaled-font-get-font-options)

;;; ----------------------------------------------------------------------------
;;; cairo_scaled_font_get_font_matrix ()
;;;
;;; void
;;; cairo_scaled_font_get_font_matrix (cairo_scaled_font_t *scaled_font,
;;;                                    cairo_matrix_t *font_matrix);
;;;
;;; Stores the font matrix with which scaled_font was created into matrix.
;;;
;;; scaled_font :
;;;     a cairo_scaled_font_t
;;;
;;; font_matrix :
;;;     return value for the matrix
;;;
;;; Since 1.2
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_scaled_font_get_font_matrix" cairo-scaled-font-get-font-matrix)
    :void
  (scaled-font (:pointer (:struct cairo-scaled-font-t)))
  (font-matrix (:pointer (:struct cairo-matrix-t))))

(export 'cairo-scaled-font-get-font-matrix)

;;; ----------------------------------------------------------------------------
;;; cairo_scaled_font_get_ctm ()
;;;
;;; void
;;; cairo_scaled_font_get_ctm (cairo_scaled_font_t *scaled_font,
;;;                            cairo_matrix_t *ctm);
;;;
;;; Stores the CTM with which scaled_font was created into ctm. Note that the
;;; translation offsets (x0, y0) of the CTM are ignored by
;;; cairo_scaled_font_create(). So, the matrix this function returns always has
;;; 0,0 as x0,y0.
;;;
;;; scaled_font :
;;;     a cairo_scaled_font_t
;;;
;;; ctm :
;;;     return value for the CTM
;;;
;;; Since 1.2
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_scaled_font_get_ctm" cairo-scaled-font-get-ctm) :void
  (scaled-font (:pointer (:struct cairo-scaled-font-t)))
  (ctm (:pointer (:struct cairo-matrix-t))))

(export 'cairo-scaled-font-get-ctm)

;;; ----------------------------------------------------------------------------
;;; cairo_scaled_font_get_scale_matrix ()
;;;
;;; void
;;; cairo_scaled_font_get_scale_matrix (cairo_scaled_font_t *scaled_font,
;;;                                     cairo_matrix_t *scale_matrix);
;;;
;;; Stores the scale matrix of scaled_font into matrix. The scale matrix is
;;; product of the font matrix and the ctm associated with the scaled font, and
;;; hence is the matrix mapping from font space to device space.
;;;
;;; scaled_font :
;;;     a cairo_scaled_font_t
;;;
;;; scale_matrix :
;;;     return value for the matrix
;;;
;;; Since 1.8
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_scaled_font_get_scale_matrix"
           cairo-scaled-font-get-scale-matrix) :void
  (scaled-font (:pointer (:struct cairo-scaled-font-t)))
  (scale-matrix (:pointer (:struct cairo-matrix-t))))

(export 'cairo-scaled-font-get-scale-matrix)

;;; ----------------------------------------------------------------------------
;;; cairo_scaled_font_get_type ()
;;;
;;; cairo_font_type_t
;;; cairo_scaled_font_get_type (cairo_scaled_font_t *scaled_font)
;;;
;;; This function returns the type of the backend used to create a scaled font.
;;; See cairo_font_type_t for available types. However, this function never
;;; returns CAIRO_FONT_TYPE_TOY.
;;;
;;; scaled_font :
;;;     a cairo_scaled_font_t
;;;
;;; Returns :
;;;     The type of scaled_font.
;;;
;;; Since 1.2
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_scaled_font_get_type" cairo-scaled-font-get-type)
    cairo-font-type-t
  (scaled-font (:pointer (:struct cairo-scaled-font-t))))

(export 'cairo-scaled-font-get-type)

;;; ----------------------------------------------------------------------------
;;; cairo_scaled_font_get_reference_count ()
;;;
;;; unsigned int
;;; cairo_scaled_font_get_reference_count (cairo_scaled_font_t *scaled_font)
;;;
;;; Returns the current reference count of scaled_font.
;;;
;;; scaled_font :
;;;     a cairo_scaled_font_t
;;;
;;; Returns :
;;;     the current reference count of scaled_font. If the object is a nil
;;;     object, 0 will be returned.
;;;
;;; Since 1.4
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_scaled_font_get_reference_count"
           cairo-scaled-font-get-reference-count) :uint
  (scaled-font (:pointer (:struct cairo-scaled-font-t))))

(export 'cairo-scaled-font-get-reference-count)

;;; ----------------------------------------------------------------------------
;;; cairo_scaled_font_set_user_data ()
;;;
;;; cairo_status_t
;;; cairo_scaled_font_set_user_data (cairo_scaled_font_t *scaled_font,
;;;                                  const cairo_user_data_key_t *key,
;;;                                  void *user_data,
;;;                                  cairo_destroy_func_t destroy);
;;;
;;; Attach user data to scaled_font. To remove user data from a surface, call
;;; this function with the key that was used to set it and NULL for data.
;;;
;;; scaled_font :
;;;     a cairo_scaled_font_t
;;;
;;; key :
;;;     the address of a cairo_user_data_key_t to attach the user data to
;;;
;;; user_data :
;;;     the user data to attach to the cairo_scaled_font_t
;;;
;;; destroy :
;;;     a cairo_destroy_func_t which will be called when the cairo_t is
;;;     destroyed or when new user data is attached using the same key.
;;;
;;; Returns :
;;;     CAIRO_STATUS_SUCCESS or CAIRO_STATUS_NO_MEMORY if a slot could not be
;;;     allocated for the user data.
;;;
;;; Since 1.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_scaled_font_get_user_data ()
;;;
;;; void *
;;; cairo_scaled_font_get_user_data (cairo_scaled_font_t *scaled_font,
;;;                                  const cairo_user_data_key_t *key);
;;;
;;; Return user data previously attached to scaled_font using the specified
;;; key. If no user data has been attached with the given key this function
;;; returns NULL.
;;;
;;; scaled_font :
;;;     a cairo_scaled_font_t
;;;
;;; key :
;;;     the address of the cairo_user_data_key_t the user data was attached to
;;;
;;; Returns :
;;;     the user data previously attached or NULL.
;;;
;;; Since 1.4
;;; ----------------------------------------------------------------------------

;;; --- End of file cairo.scaled-font.lisp -------------------------------------
