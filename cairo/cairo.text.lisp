;;; ----------------------------------------------------------------------------
;;; cairo.text.lisp
;;;
;;; The documentation of this file is taken from the Cairo Reference Manual
;;; Version 1.12.14 and modified to document the Lisp binding to the Cairo
;;; library. See <http://cairographics.org>. The API documentation of the Lisp
;;; binding is available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2013 Dieter Kaiser
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
;;; text
;;;
;;; Rendering text and glyphs
;;;
;;; Synopsis
;;;
;;;     cairo_glyph_t
;;;     cairo_font_slant_t
;;;     cairo_font_weight_t
;;;     cairo_text_cluster_t
;;;     cairo_text_cluster_flags_t
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
;;; ----------------------------------------------------------------------------
;;;
;;; Description
;;;
;;; The functions with text in their name form cairo's toy text API. The toy API
;;; takes UTF-8 encoded text and is limited in its functionality to rendering
;;; simple left-to-right text with no advanced features. That means for example
;;; that most complex scripts like Hebrew, Arabic, and Indic scripts are out of
;;; question. No kerning or correct positioning of diacritical marks either. The
;;; font selection is pretty limited too and doesn't handle the case that the
;;; selected font does not cover the characters in the text. This set of
;;; functions are really that, a toy text API, for testing and demonstration
;;; purposes. Any serious application should avoid them.
;;;
;;; The functions with glyphs in their name form cairo's low-level text API. The
;;; low-level API relies on the user to convert text to a set of glyph indexes
;;; and positions. This is a very hard problem and is best handled by external
;;; libraries, like the pangocairo that is part of the Pango text layout and
;;; rendering library. Pango is available from http://www.pango.org/.
;;; ----------------------------------------------------------------------------

(in-package :cairo)

;;; ----------------------------------------------------------------------------
;;; cairo_glyph_t
;;;
;;; typedef struct {
;;;     unsigned long        index;
;;;     double               x;
;;;     double               y;
;;; } cairo_glyph_t;
;;;
;;; The cairo_glyph_t structure holds information about a single glyph when
;;; drawing or measuring text. A font is (in simple terms) a collection of
;;; shapes used to draw text. A glyph is one of these shapes. There can be
;;; multiple glyphs for a single character (alternates to be used in different
;;; contexts, for example), or a glyph can be a ligature of multiple characters.
;;; Cairo doesn't expose any way of converting input text into glyphs, so in
;;; order to use the Cairo interfaces that take arrays of glyphs, you must
;;; directly access the appropriate underlying font system.
;;;
;;; Note that the offsets given by x and y are not cumulative. When drawing or
;;; measuring text, each glyph is individually positioned with respect to the
;;; overall origin
;;;
;;; unsigned long index;
;;;     glyph index in the font. The exact interpretation of the glyph index
;;;     depends on the font technology being used.
;;;
;;; double x;
;;;     the offset in the X direction between the origin used for drawing or
;;;     measuring the string and the origin of this glyph.
;;;
;;; double y;
;;;     the offset in the Y direction between the origin used for drawing or
;;;     measuring the string and the origin of this glyph.
;;;
;;; Since 1.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; enum cairo_font_slant_t
;;; ----------------------------------------------------------------------------

(defcenum cairo-font-slant-t
  :normal
  :italic
  :oblique)

#+cl-cffi-gtk-documentation
(setf (gethash 'cairo-font-slant-t atdoc:*symbol-name-alias*) "CEnum"
      (gethash 'cairo-font-slant-t atdoc:*external-symbols*)
 "@version{2013-10-9}
  @short{Specifies variants of a font face based on their slant.}
  @begin{pre}
(defcenum cairo-font-slant-t
  :normal
  :italic
  :oblique)
  @end{pre}
  @begin[code]{table}
    @entry[:normal]{Upright font style, since 1.0.}
    @entry[:italic]{Italic font style, since 1.0.}
    @entry[:oblique]{Oblique font style, since 1.0.}
  @end{table}
  Since 1.0
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
(setf (gethash 'cairo-font-weight-t atdoc:*symbol-name-alias*) "CEnum"
      (gethash 'cairo-font-weight-t atdoc:*external-symbols*)
 "@version{2013-10-9}
  @short{Specifies variants of a font face based on their weight.}
  @begin{pre}
(defcenum cairo-font-slant-t
  :normal
  :bold)
  @end{pre}
  @begin[code]{table}
    @entry[:normal]{Normal font weight, since 1.0.}
    @entry[:bold]{Bold font weight, since 1.0.}
  @end{table}
  Since 1.0
  @see-symbol{cairo-font-slant-t}
  @see-function{cairo-select-font-face}")

(export 'cairo-font-weight-t)

;;; ----------------------------------------------------------------------------
;;; cairo_text_cluster_t
;;;
;;; typedef struct {
;;;     int        num_bytes;
;;;     int        num_glyphs;
;;; } cairo_text_cluster_t;
;;;
;;; The cairo_text_cluster_t structure holds information about a single text
;;; cluster. A text cluster is a minimal mapping of some glyphs corresponding to
;;; some UTF-8 text.
;;;
;;; For a cluster to be valid, both num_bytes and num_glyphs should be
;;; non-negative, and at least one should be non-zero. Note that clusters with
;;; zero glyphs are not as well supported as normal clusters. For example, PDF
;;; rendering applications typically ignore those clusters when PDF text is
;;; being selected.
;;;
;;; See cairo_show_text_glyphs() for how clusters are used in advanced text
;;; operations.
;;;
;;; int num_bytes;
;;;     the number of bytes of UTF-8 text covered by cluster
;;;
;;; int num_glyphs;
;;;     the number of glyphs covered by cluster
;;;
;;; Since 1.8
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; enum cairo_text_cluster_flags_t
;;;
;;; typedef enum {
;;;     CAIRO_TEXT_CLUSTER_FLAG_BACKWARD = 0x00000001
;;; } cairo_text_cluster_flags_t;
;;;
;;; Specifies properties of a text cluster mapping.
;;;
;;; CAIRO_TEXT_CLUSTER_FLAG_BACKWARD
;;;     The clusters in the cluster array map to glyphs in the glyph array from
;;;     end to start. (Since 1.8)
;;;
;;; Since 1.8
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_select_font_face ()
;;;
;;; void cairo_select_font_face (cairo_t *cr,
;;;                              const char *family,
;;;                              cairo_font_slant_t slant,
;;;                              cairo_font_weight_t weight);
;;;
;;; Note: The cairo_select_font_face() function call is part of what the cairo
;;; designers call the "toy" text API. It is convenient for short demos and
;;; simple programs, but it is not expected to be adequate for serious
;;; text-using applications.
;;;
;;; Selects a family and style of font from a simplified description as a family
;;; name, slant and weight. Cairo provides no operation to list available family
;;; names on the system (this is a "toy", remember), but the standard CSS2
;;; generic family names, ("serif", "sans-serif", "cursive", "fantasy",
;;; "monospace"), are likely to work as expected.
;;;
;;; If family starts with the string "cairo:", or if no native font backends are
;;; compiled in, cairo will use an internal font family. The internal font
;;; family recognizes many modifiers in the family string, most notably, it
;;; recognizes the string "monospace". That is, the family name
;;; "cairo:monospace" will use the monospace version of the internal font
;;; family.
;;;
;;; For "real" font selection, see the font-backend-specific font_face_create
;;; functions for the font backend you are using. (For example, if you are using
;;; the freetype-based cairo-ft font backend, see
;;; cairo_ft_font_face_create_for_ft_face() or
;;; cairo_ft_font_face_create_for_pattern().) The resulting font face could then
;;; be used with cairo_scaled_font_create() and cairo_set_scaled_font().
;;;
;;; Similarly, when using the "real" font support, you can call directly into
;;; the underlying font system, (such as fontconfig or freetype), for operations
;;; such as listing available fonts, etc.
;;;
;;; It is expected that most applications will need to use a more comprehensive
;;; font handling and text layout library, (for example, pango), in conjunction
;;; with cairo.
;;;
;;; If text is drawn without a call to cairo_select_font_face(), (nor
;;; cairo_set_font_face() nor cairo_set_scaled_font()), the default family is
;;; platform-specific, but is essentially "sans-serif". Default slant is
;;; CAIRO_FONT_SLANT_NORMAL, and default weight is CAIRO_FONT_WEIGHT_NORMAL.
;;;
;;; This function is equivalent to a call to cairo_toy_font_face_create()
;;; followed by cairo_set_font_face().
;;;
;;; cr :
;;;     a cairo_t
;;;
;;; family :
;;;     a font family name, encoded in UTF-8
;;;
;;; slant :
;;;     the slant for the font
;;;
;;; weight :
;;;     the weight for the font
;;;
;;; Since 1.0
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_select_font_face" cairo-select-font-face) :void
  (cr (:pointer (:struct cairo-t)))
  (family :string)
  (slant cairo-font-slant-t)
  (weight cairo-font-weight-t))

(export 'cairo-select-font-face)

;;; ----------------------------------------------------------------------------
;;; cairo_set_font_size ()
;;;
;;; void cairo_set_font_size (cairo_t *cr, double size);
;;;
;;; Sets the current font matrix to a scale by a factor of size, replacing any
;;; font matrix previously set with cairo_set_font_size() or
;;; cairo_set_font_matrix(). This results in a font size of size user space
;;; units. (More precisely, this matrix will result in the font's em-square
;;; being a size by size square in user space.)
;;;
;;; If text is drawn without a call to cairo_set_font_size(), (nor
;;; cairo_set_font_matrix() nor cairo_set_scaled_font()), the default font size
;;; is 10.0.
;;;
;;; cr :
;;;     a cairo_t
;;;
;;; size :
;;;     the new font size, in user space units
;;;
;;; Since 1.0
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_set_font_size" cairo-set-font-size) :void
  (cr (:pointer (:struct cairo-t)))
  (size :double))

(export 'cairo-set-font-size)

;;; ----------------------------------------------------------------------------
;;; cairo_set_font_matrix ()
;;;
;;; void cairo_set_font_matrix (cairo_t *cr, const cairo_matrix_t *matrix);
;;;
;;; Sets the current font matrix to matrix. The font matrix gives a
;;; transformation from the design space of the font (in this space, the
;;; em-square is 1 unit by 1 unit) to user space. Normally, a simple scale is
;;; used (see cairo_set_font_size()), but a more complex font matrix can be used
;;; to shear the font or stretch it unequally along the two axes
;;;
;;; cr :
;;;     a cairo_t
;;;
;;; matrix :
;;;     a cairo_matrix_t describing a transform to be applied to the current
;;;     font.
;;;
;;; Since 1.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_get_font_matrix ()
;;;
;;; void cairo_get_font_matrix (cairo_t *cr, cairo_matrix_t *matrix);
;;;
;;; Stores the current font matrix into matrix. See cairo_set_font_matrix().
;;;
;;; cr :
;;;     a cairo_t
;;;
;;; matrix :
;;;     return value for the matrix
;;;
;;; Since 1.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_set_font_options ()
;;;
;;; void cairo_set_font_options (cairo_t *cr,
;;;                              const cairo_font_options_t *options);
;;;
;;; Sets a set of custom font rendering options for the cairo_t. Rendering
;;; options are derived by merging these options with the options derived from
;;; underlying surface; if the value in options has a default value (like
;;; CAIRO_ANTIALIAS_DEFAULT), then the value from the surface is used.
;;;
;;; cr :
;;;     a cairo_t
;;;
;;; options :
;;;     font options to use
;;;
;;; Since 1.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_get_font_options ()
;;;
;;; void cairo_get_font_options (cairo_t *cr, cairo_font_options_t *options);
;;;
;;; Retrieves font rendering options set via cairo_set_font_options. Note that
;;; the returned options do not include any options derived from the underlying
;;; surface; they are literally the options passed to cairo_set_font_options().
;;;
;;; cr :
;;;     a cairo_t
;;;
;;; options :
;;;     a cairo_font_options_t object into which to store the retrieved options.
;;;     All existing values are overwritten
;;;
;;; Since 1.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_set_font_face ()
;;;
;;; void cairo_set_font_face (cairo_t *cr, cairo_font_face_t *font_face);
;;;
;;; Replaces the current cairo_font_face_t object in the cairo_t with font_face.
;;; The replaced font face in the cairo_t will be destroyed if there are no
;;; other references to it.
;;;
;;; cr :
;;;     a cairo_t
;;;
;;; font_face :
;;;     a cairo_font_face_t, or NULL to restore to the default font
;;;
;;; Since 1.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_get_font_face ()
;;;
;;; cairo_font_face_t * cairo_get_font_face (cairo_t *cr);
;;;
;;; Gets the current font face for a cairo_t.
;;;
;;; cr :
;;;     a cairo_t
;;;
;;; Returns :
;;;     the current font face. This object is owned by cairo. To keep a
;;;     reference to it, you must call cairo_font_face_reference(). This
;;;     function never returns NULL. If memory cannot be allocated, a special
;;;     "nil" cairo_font_face_t object will be returned on which
;;;     cairo_font_face_status() returns CAIRO_STATUS_NO_MEMORY. Using this nil
;;;     object will cause its error state to propagate to other objects it is
;;;     passed to, (for example, calling cairo_set_font_face() with a nil font
;;;     will trigger an error that will shutdown the cairo_t object).
;;;
;;; Since 1.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_set_scaled_font ()
;;;
;;; void cairo_set_scaled_font (cairo_t *cr,
;;;                             const cairo_scaled_font_t *scaled_font);
;;;
;;; Replaces the current font face, font matrix, and font options in the cairo_t
;;; with those of the cairo_scaled_font_t. Except for some translation, the
;;; current CTM of the cairo_t should be the same as that of the
;;; cairo_scaled_font_t, which can be accessed using
;;; cairo_scaled_font_get_ctm().
;;;
;;; cr :
;;;     a cairo_t
;;;
;;; scaled_font :
;;;     a cairo_scaled_font_t
;;;
;;; Since 1.2
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_get_scaled_font ()
;;;
;;; cairo_scaled_font_t * cairo_get_scaled_font (cairo_t *cr);
;;;
;;; Gets the current scaled font for a cairo_t.
;;;
;;; cr :
;;;     a cairo_t
;;;
;;; Returns :
;;;     the current scaled font. This object is owned by cairo. To keep a
;;;     reference to it, you must call cairo_scaled_font_reference(). This
;;;     function never returns NULL. If memory cannot be allocated, a special
;;;     "nil" cairo_scaled_font_t object will be returned on which
;;;     cairo_scaled_font_status() returns CAIRO_STATUS_NO_MEMORY. Using this
;;;     nil object will cause its error state to propagate to other objects it
;;;     is passed to, (for example, calling cairo_set_scaled_font() with a nil
;;;     font will trigger an error that will shutdown the cairo_t object).
;;;
;;; Since 1.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_show_text ()
;;;
;;; void cairo_show_text (cairo_t *cr, const char *utf8);
;;;
;;; A drawing operator that generates the shape from a string of UTF-8
;;; characters, rendered according to the current font_face, font_size
;;; (font_matrix), and font_options.
;;;
;;; This function first computes a set of glyphs for the string of text. The
;;; first glyph is placed so that its origin is at the current point. The origin
;;; of each subsequent glyph is offset from that of the previous glyph by the
;;; advance values of the previous glyph.
;;;
;;; After this call the current point is moved to the origin of where the next
;;; glyph would be placed in this same progression. That is, the current point
;;; will be at the origin of the final glyph offset by its advance values. This
;;; allows for easy display of a single logical string with multiple calls to
;;; cairo_show_text().
;;;
;;; Note: The cairo_show_text() function call is part of what the cairo
;;; designers call the "toy" text API. It is convenient for short demos and
;;; simple programs, but it is not expected to be adequate for serious
;;; text-using applications. See cairo_show_glyphs() for the "real" text display
;;; API in cairo.
;;;
;;; cr :
;;;     a cairo context
;;;
;;; utf8 :
;;;     a NUL-terminated string of text encoded in UTF-8, or NULL
;;;
;;; Since 1.0
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_show_text" cairo-show-text) :void
  (cr (:pointer (:struct cairo-t)))
  (utf8 :string))

(export 'cairo-show-text)

;;; ----------------------------------------------------------------------------
;;; cairo_show_glyphs ()
;;;
;;; void cairo_show_glyphs (cairo_t *cr,
;;;                         const cairo_glyph_t *glyphs,
;;;                         int num_glyphs);
;;;
;;; A drawing operator that generates the shape from an array of glyphs,
;;; rendered according to the current font face, font size (font matrix), and
;;; font options.
;;;
;;; cr :
;;;     a cairo context
;;;
;;; glyphs :
;;;     array of glyphs to show
;;;
;;; num_glyphs :
;;;     number of glyphs to show
;;;
;;; Since 1.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_show_text_glyphs ()
;;;
;;; void cairo_show_text_glyphs (cairo_t *cr,
;;;                              const char *utf8,
;;;                              int utf8_len,
;;;                              const cairo_glyph_t *glyphs,
;;;                              int num_glyphs,
;;;                              const cairo_text_cluster_t *clusters,
;;;                              int num_clusters,
;;;                              cairo_text_cluster_flags_t cluster_flags)
;;;
;;; This operation has rendering effects similar to cairo_show_glyphs() but, if
;;; the target surface supports it, uses the provided text and cluster mapping
;;; to embed the text for the glyphs shown in the output. If the target does not
;;; support the extended attributes, this function acts like the basic
;;; cairo_show_glyphs() as if it had been passed glyphs and num_glyphs.
;;;
;;; The mapping between utf8 and glyphs is provided by an array of clusters.
;;; Each cluster covers a number of text bytes and glyphs, and neighboring
;;; clusters cover neighboring areas of utf8 and glyphs. The clusters should
;;; collectively cover utf8 and glyphs in entirety.
;;;
;;; The first cluster always covers bytes from the beginning of utf8. If
;;; cluster_flags do not have the CAIRO_TEXT_CLUSTER_FLAG_BACKWARD set, the
;;; first cluster also covers the beginning of glyphs, otherwise it covers the
;;; end of the glyphs array and following clusters move backward.
;;;
;;; See cairo_text_cluster_t for constraints on valid clusters.
;;;
;;; cr :
;;;     a cairo context
;;;
;;; utf8 :
;;;     a string of text encoded in UTF-8
;;;
;;; utf8_len :
;;;     length of utf8 in bytes, or -1 if it is NUL-terminated
;;;
;;; glyphs :
;;;     array of glyphs to show
;;;
;;; num_glyphs :
;;;     number of glyphs to show
;;;
;;; clusters :
;;;     array of cluster mapping information
;;;
;;; num_clusters :
;;;     number of clusters in the mapping
;;;
;;; cluster_flags :
;;;     cluster mapping flags
;;;
;;; Since 1.8
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_font_extents ()
;;;
;;; void cairo_font_extents (cairo_t *cr, cairo_font_extents_t *extents);
;;;
;;; Gets the font extents for the currently selected font.
;;;
;;; cr :
;;;     a cairo_t
;;;
;;; extents :
;;;     a cairo_font_extents_t object into which the results will be stored.
;;;
;;; Since 1.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_text_extents ()
;;;
;;; void cairo_text_extents (cairo_t *cr,
;;;                          const char *utf8,
;;;                          cairo_text_extents_t *extents);
;;;
;;; Gets the extents for a string of text. The extents describe a user-space
;;; rectangle that encloses the "inked" portion of the text, (as it would be
;;; drawn by cairo_show_text()). Additionally, the x_advance and y_advance
;;; values indicate the amount by which the current point would be advanced by
;;; cairo_show_text().
;;;
;;; Note that whitespace characters do not directly contribute to the size of
;;; the rectangle (extents.width and extents.height). They do contribute
;;; indirectly by changing the position of non-whitespace characters. In
;;; particular, trailing whitespace characters are likely to not affect the size
;;; of the rectangle, though they will affect the x_advance and y_advance
;;; values.
;;;
;;; cr :
;;;     a cairo_t
;;;
;;; utf8 :
;;;     a NUL-terminated string of text encoded in UTF-8, or NULL
;;;
;;; extents :
;;;     a cairo_text_extents_t object into which the results will be stored
;;;
;;; Since 1.0
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_text_extents" %cairo-text-extents) :void
  (cr (:pointer (:struct cairo-t)))
  (utf8 :string)
  (extents (:pointer (:struct cairo-text-extents-t))))

(defun cairo-text-extents (cr utf8)
  (with-foreign-object (extents '(:struct cairo-text-extents-t))
    (%cairo-text-extents cr utf8 extents)
    extents))

(export 'cairo-text-extents)

;;; ----------------------------------------------------------------------------
;;; cairo_glyph_extents ()
;;;
;;; void cairo_glyph_extents (cairo_t *cr,
;;;                           const cairo_glyph_t *glyphs,
;;;                           int num_glyphs,
;;;                           cairo_text_extents_t *extents);
;;;
;;; Gets the extents for an array of glyphs. The extents describe a user-space
;;; rectangle that encloses the "inked" portion of the glyphs, (as they would be
;;; drawn by cairo_show_glyphs()). Additionally, the x_advance and y_advance
;;; values indicate the amount by which the current point would be advanced by
;;; cairo_show_glyphs().
;;;
;;; Note that whitespace glyphs do not contribute to the size of the rectangle
;;; (extents.width and extents.height).
;;;
;;; cr :
;;;     a cairo_t
;;;
;;; glyphs :
;;;     an array of cairo_glyph_t objects
;;;
;;; num_glyphs :
;;;     the number of elements in glyphs
;;;
;;; extents :
;;;     a cairo_text_extents_t object into which the results will be stored
;;;
;;; Since 1.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_toy_font_face_create ()
;;;
;;; cairo_font_face_t * cairo_toy_font_face_create (const char *family,
;;;                                                 cairo_font_slant_t slant,
;;;                                                 cairo_font_weight_t weight);
;;;
;;; Creates a font face from a triplet of family, slant, and weight. These font
;;; faces are used in implementation of the the cairo_t "toy" font API.
;;;
;;; If family is the zero-length string "", the platform-specific default family
;;; is assumed. The default family then can be queried using
;;; cairo_toy_font_face_get_family().
;;;
;;; The cairo_select_font_face() function uses this to create font faces. See
;;; that function for limitations and other details of toy font faces.
;;;
;;; family :
;;;     a font family name, encoded in UTF-8
;;;
;;; slant :
;;;     the slant for the font
;;;
;;; weight :
;;;     the weight for the font
;;;
;;; Returns :
;;;     a newly created cairo_font_face_t. Free with cairo_font_face_destroy()
;;;     when you are done using it.
;;;
;;; Since 1.8
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_toy_font_face_get_family ()
;;;
;;; const char * cairo_toy_font_face_get_family (cairo_font_face_t *font_face);
;;;
;;; Gets the familly name of a toy font.
;;;
;;; font_face :
;;;     A toy font face
;;;
;;; Returns :
;;;     The family name. This string is owned by the font face and remains valid
;;;     as long as the font face is alive (referenced).
;;;
;;; Since 1.8
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_toy_font_face_get_slant ()
;;;
;;; cairo_font_slant_t  cairo_toy_font_face_get_slant
;;;                                               (cairo_font_face_t *font_face)
;;;
;;; Gets the slant a toy font.
;;;
;;; font_face :
;;;     A toy font face
;;;
;;; Returns :
;;;     The slant value
;;;
;;; Since 1.8
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_toy_font_face_get_weight ()
;;;
;;; cairo_font_weight_t cairo_toy_font_face_get_weight
;;;                                               (cairo_font_face_t *font_face)
;;;
;;; Gets the weight a toy font.
;;;
;;; font_face :
;;;     A toy font face
;;;
;;; Returns :
;;;     The weight value
;;;
;;; Since 1.8
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_glyph_allocate ()
;;;
;;; cairo_glyph_t * cairo_glyph_allocate (int num_glyphs);
;;;
;;; Allocates an array of cairo_glyph_t's. This function is only useful in
;;; implementations of cairo_user_scaled_font_text_to_glyphs_func_t where the
;;; user needs to allocate an array of glyphs that cairo will free. For all
;;; other uses, user can use their own allocation method for glyphs.
;;;
;;; This function returns NULL if num_glyphs is not positive, or if out of
;;; memory. That means, the NULL return value signals out-of-memory only if
;;; num_glyphs was positive.
;;;
;;; num_glyphs :
;;;     number of glyphs to allocate
;;;
;;; Returns :
;;;     the newly allocated array of glyphs that should be freed using
;;;     cairo_glyph_free()
;;;
;;; Since 1.8
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_glyph_free ()
;;;
;;; void cairo_glyph_free (cairo_glyph_t *glyphs);
;;;
;;; Frees an array of cairo_glyph_t's allocated using cairo_glyph_allocate().
;;; This function is only useful to free glyph array returned by
;;; cairo_scaled_font_text_to_glyphs() where cairo returns an array of glyphs
;;; that the user will free. For all other uses, user can use their own
;;; allocation method for glyphs.
;;;
;;; glyphs :
;;;     array of glyphs to free, or NULL
;;;
;;; Since 1.8
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_text_cluster_allocate ()
;;;
;;; cairo_text_cluster_t * cairo_text_cluster_allocate (int num_clusters);
;;;
;;; Allocates an array of cairo_text_cluster_t's. This function is only useful
;;; in implementations of cairo_user_scaled_font_text_to_glyphs_func_t where the
;;; user needs to allocate an array of text clusters that cairo will free. For
;;; all other uses, user can use their own allocation method for text clusters.
;;;
;;; This function returns NULL if num_clusters is not positive, or if out of
;;; memory. That means, the NULL return value signals out-of-memory only if
;;; num_clusters was positive.
;;;
;;; num_clusters :
;;;     number of text_clusters to allocate
;;;
;;; Returns :
;;;     the newly allocated array of text clusters that should be freed using
;;;     cairo_text_cluster_free()
;;;
;;; Since 1.8
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_text_cluster_free ()
;;;
;;; void cairo_text_cluster_free (cairo_text_cluster_t *clusters);
;;;
;;; Frees an array of cairo_text_cluster's allocated using
;;; cairo_text_cluster_allocate(). This function is only useful to free text
;;; cluster array returned by cairo_scaled_font_text_to_glyphs() where cairo
;;; returns an array of text clusters that the user will free. For all other
;;; uses, user can use their own allocation method for text clusters.
;;;
;;; clusters :
;;;     array of text clusters to free, or NULL
;;;
;;; Since 1.8
;;; ----------------------------------------------------------------------------

;;; --- End of file cairo.text.lisp --------------------------------------------
