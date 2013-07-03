;;; ----------------------------------------------------------------------------
;;; pango.glyph.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See <http://common-lisp.net/project/cl-gtk2/>.
;;;
;;; The documentation has been copied from the Pango Reference Manual
;;; for Pango 1.32.6. See <http://www.gtk.org>. The API documentation of the
;;; Lisp binding is available at <http://www.crategus.com/books/cl-cffi-gtk/>.
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
;;; Glyph Storage
;;;
;;; Structures for storing information about glyphs
;;;
;;; Synopsis
;;;
;;;     PANGO_SCALE
;;;     PANGO_PIXELS
;;;     PANGO_PIXELS_FLOOR
;;;     PANGO_PIXELS_CEIL
;;;     PANGO_UNITS_ROUND
;;;
;;;     pango_units_to_double
;;;     pango_units_from_double
;;;
;;;     PangoRectangle
;;;
;;;     PANGO_ASCENT
;;;     PANGO_DESCENT
;;;     PANGO_LBEARING
;;;     PANGO_RBEARING
;;;
;;;     pango_extents_to_pixels
;;;
;;;     PangoMatrix
;;;
;;;     PANGO_TYPE_MATRIX
;;;     PANGO_MATRIX_INIT
;;;
;;;     pango_matrix_copy
;;;     pango_matrix_free
;;;     pango_matrix_translate
;;;     pango_matrix_scale
;;;     pango_matrix_rotate
;;;     pango_matrix_concat
;;;     pango_matrix_transform_point
;;;     pango_matrix_transform_distance
;;;     pango_matrix_transform_rectangle
;;;     pango_matrix_transform_pixel_rectangle
;;;     pango_matrix_get_font_scale_factor
;;;
;;;     PangoGlyph
;;;
;;;     PANGO_GLYPH_EMPTY
;;;     PANGO_GLYPH_INVALID_INPUT
;;;     PANGO_GLYPH_UNKNOWN_FLAG
;;;     PANGO_GET_UNKNOWN_GLYPH
;;;
;;;     PangoGlyphInfo
;;;     PangoGlyphGeometry
;;;     PangoGlyphUnit
;;;     PangoGlyphVisAttr
;;;     PangoGlyphString
;;;     PangoGlyphItem
;;;     PangoGlyphItemIter
;;;
;;;     PANGO_TYPE_GLYPH_STRING
;;;
;;;     pango_glyph_string_new
;;;     pango_glyph_string_copy
;;;     pango_glyph_string_set_size
;;;     pango_glyph_string_free
;;;     pango_glyph_string_extents
;;;     pango_glyph_string_extents_range
;;;     pango_glyph_string_get_width
;;;     pango_glyph_string_index_to_x
;;;     pango_glyph_string_x_to_index
;;;     pango_glyph_string_get_logical_widths
;;;
;;;     PANGO_TYPE_GLYPH_ITEM
;;;
;;;     pango_glyph_item_copy
;;;     pango_glyph_item_free
;;;     pango_glyph_item_split
;;;     pango_glyph_item_apply_attrs
;;;     pango_glyph_item_letter_space
;;;     pango_glyph_item_get_logical_widths
;;;
;;;     PANGO_TYPE_GLYPH_ITEM_ITER
;;;
;;;     pango_glyph_item_iter_copy
;;;     pango_glyph_item_iter_free
;;;     pango_glyph_item_iter_init_start
;;;     pango_glyph_item_iter_init_end
;;;     pango_glyph_item_iter_next_cluster
;;;     pango_glyph_item_iter_prev_cluster
;;;
;;; Object Hierarchy
;;;
;;;   GBoxed
;;;    +----PangoMatrix
;;;
;;;   GBoxed
;;;    +----PangoGlyphString
;;;
;;;   GBoxed
;;;    +----PangoGlyphItem
;;;
;;;   GBoxed
;;;    +----PangoGlyphItemIter
;;;
;;; Description
;;;
;;; pango_shape() produces a string of glyphs which can be measured or drawn to
;;; the screen. The following structures are used to store information about
;;; glyphs.
;;; ----------------------------------------------------------------------------

(in-package :pango)

;;; ----------------------------------------------------------------------------
;;; PANGO_SCALE
;;;
;;; #define PANGO_SCALE 1024
;;;
;;; The PANGO_SCALE macro represents the scale between dimensions used for Pango
;;; distances and device units. (The definition of device units is dependent on
;;; the output device; it will typically be pixels for a screen, and points for
;;; a printer.) PANGO_SCALE is currently 1024, but this may be changed in the
;;; future.
;;;
;;; When setting font sizes, device units are always considered to be points (as
;;; in "12 point font"), rather than pixels.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; PANGO_PIXELS()
;;;
;;; #define PANGO_PIXELS(d) (((int)(d) + 512) >> 10)
;;;
;;; Converts a dimension to device units by rounding.
;;;
;;; d :
;;;     a dimension in Pango units.
;;;
;;; Returns :
;;;     rounded dimension in device units.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; PANGO_PIXELS_FLOOR()
;;;
;;; #define PANGO_PIXELS_FLOOR(d) (((int)(d)) >> 10)
;;;
;;; Converts a dimension to device units by flooring.
;;;
;;; d :
;;;     a dimension in Pango units.
;;;
;;; Returns :
;;;     floored dimension in device units.
;;;
;;; Since 1.14
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; PANGO_PIXELS_CEIL()
;;;
;;; #define PANGO_PIXELS_CEIL(d) (((int)(d) + 1023) >> 10)
;;;
;;; Converts a dimension to device units by ceiling.
;;;
;;; d :
;;;     a dimension in Pango units.
;;;
;;; Returns :
;;;     ceiled dimension in device units.
;;;
;;; Since 1.14
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; PANGO_UNITS_ROUND()
;;;
;;; #define PANGO_UNITS_ROUND(d)
;;;
;;; Rounds a dimension to whole device units, but does not convert it to device
;;; units.
;;;
;;; d :
;;;     a dimension in Pango units.
;;;
;;; Returns :
;;;     rounded dimension in Pango units.
;;;
;;; Since 1.18
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_units_to_double ()
;;;
;;; double pango_units_to_double (int i);
;;;
;;; Converts a number in Pango units to floating-point: divides it by
;;; PANGO_SCALE.
;;;
;;; i :
;;;     value in Pango units
;;;
;;; Returns :
;;;     the double value.
;;;
;;; Since 1.16
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_units_from_double ()
;;;
;;; int pango_units_from_double (double d);
;;;
;;; Converts a floating-point number to Pango units: multiplies it by
;;; PANGO_SCALE and rounds to nearest integer.
;;;
;;; d :
;;;     double floating-point value
;;;
;;; Returns :
;;;     the value in Pango units.
;;;
;;; Since 1.16
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; struct PangoRectangle
;;;
;;; struct PangoRectangle {
;;;   int x;
;;;   int y;
;;;   int width;
;;;   int height;
;;; };
;;;
;;; The PangoRectangle structure represents a rectangle. It is frequently used
;;; to represent the logical or ink extents of a single glyph or section of
;;; text. (See, for instance, pango_font_get_glyph_extents())
;;;
;;; int x;
;;;     X coordinate of the left side of the rectangle.
;;;
;;; int y;
;;;     Y coordinate of the the top side of the rectangle.
;;;
;;; int width;
;;;     width of the rectangle.
;;;
;;; int height;
;;;     height of the rectangle.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; PANGO_ASCENT()
;;;
;;; #define PANGO_ASCENT(rect) (-(rect).y)
;;;
;;; Extracts the ascent from a PangoRectangle representing glyph extents. The
;;; ascent is the distance from the baseline to the highest point of the
;;; character. This is positive if the glyph ascends above the baseline.
;;;
;;; rect :
;;;     a PangoRectangle
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; PANGO_DESCENT()
;;;
;;; #define PANGO_DESCENT(rect) ((rect).y + (rect).height)
;;;
;;; Extracts the descent from a PangoRectangle representing glyph extents. The
;;; descent is the distance from the baseline to the lowest point of the
;;; character. This is positive if the glyph descends below the baseline.
;;;
;;; rect :
;;;     a PangoRectangle
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; PANGO_LBEARING()
;;;
;;; #define PANGO_LBEARING(rect) ((rect).x)
;;;
;;; Extracts the left bearing from a PangoRectangle representing glyph extents.
;;; The left bearing is the distance from the horizontal origin to the farthest
;;; left point of the character. This is positive for characters drawn
;;; completely to the right of the glyph origin.
;;;
;;; rect :
;;;     a PangoRectangle
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; PANGO_RBEARING()
;;;
;;; #define PANGO_RBEARING(rect) ((rect).x + (rect).width)
;;;
;;; Extracts the right bearing from a PangoRectangle representing glyph extents.
;;; The right bearing is the distance from the horizontal origin to the farthest
;;; right point of the character. This is positive except for characters drawn
;;; completely to the left of the horizontal origin.
;;;
;;; rect :
;;;     a PangoRectangle
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_extents_to_pixels ()
;;;
;;; void pango_extents_to_pixels (PangoRectangle *inclusive,
;;;                               PangoRectangle *nearest);
;;;
;;; Converts extents from Pango units to device units, dividing by the
;;; PANGO_SCALE factor and performing rounding.
;;;
;;; The inclusive rectangle is converted by flooring the x/y coordinates and
;;; extending width/height, such that the final rectangle completely includes
;;; the original rectangle.
;;;
;;; The nearest rectangle is converted by rounding the coordinates of the
;;; rectangle to the nearest device unit (pixel).
;;;
;;; The rule to which argument to use is: if you want the resulting device-space
;;; rectangle to completely contain the original rectangle, pass it in as
;;; inclusive. If you want two touching-but-not-overlapping rectangles stay
;;; touching-but-not-overlapping after rounding to device units, pass them in as
;;; nearest.
;;;
;;; inclusive :
;;;     rectangle to round to pixels inclusively, or NULL
;;;
;;; nearest :
;;;     rectangle to round to nearest pixels, or NULL
;;;
;;; Since 1.16
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; struct PangoMatrix
;;; ----------------------------------------------------------------------------

(define-g-boxed-cstruct pango-matrix "PangoMatrix"
  (xx :double :initform 0.0)
  (xy :double :initform 0.0)
  (yx :double :initform 0.0)
  (yy :double :initform 0.0)
  (x0 :double :initform 0.0)
  (y0 :double :initform 0.0))

#+cl-cffi-gtk-documentation
(setf (gethash 'pango-matrix atdoc:*class-name-alias*) "CStruct"
      (documentation 'pango-matrix 'type)
 "@version{2013-6-29}
  @begin{short}
    A structure specifying a transformation between user-space coordinates and
    device coordinates.
  @end{short}
  The transformation is given by
  @begin{pre}
 x_device = x_user * matrix->xx + y_user * matrix->xy + matrix->x0;
 y_device = x_user * matrix->yx + y_user * matrix->yy + matrix->y0;
  @end{pre}
  @begin{pre}
(define-g-boxed-cstruct pango-matrix \"PangoMatrix\"
  (xx :double :initform 0.0)
  (xy :double :initform 0.0)
  (yx :double :initform 0.0)
  (yy :double :initform 0.0)
  (x0 :double :initform 0.0)
  (y0 :double :initform 0.0))
  @end{pre}
  @begin[code]{table}
    @entry[xx]{1st component of the transformation matrix.}
    @entry[xy]{2nd component of the transformation matrix.}
    @entry[yx]{3rd component of the transformation matrix.}
    @entry[yy]{4th component of the transformation matrix.}
    @entry[x0]{x translation.}
    @entry[y0]{ y translation.}
  @end{table}
  Since 1.6
  @see-constructor{copy-pango-matrix}
  @see-constructor{make-pango-matrix}
  @see-slot{pango-matrix-xx}
  @see-slot{pango-matrix-xy}
  @see-slot{pango-matrix-yx}
  @see-slot{pango-matrix-yy}
  @see-slot{pango-matrix-x0}
  @see-slot{pango-matrix-y0}")

(export (boxed-related-symbols 'pango-matrix))

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation 'make-pango-matrix 'function)
 "@version{2013-6-29}
  Returns a @class{pango-matrix} structure.")

#+cl-cffi-gtk-documentation
(setf (documentation 'copy-pango-matrix 'function)
 "@version{2013-6-29}
  @argument[instance]{a @class{pango-matrix} structure}
  Copy constructor of a @class{pango-matrix} structure.")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'pango-matrix-xx atdoc:*function-name-alias*) "Accessor"
      (documentation 'pango-matrix-xx 'function)
 "@version{2013-6-29}
  Accessor of the slot @code{xx} of the @class{pango-matrix} structure.")

#+cl-cffi-gtk-documentation
(setf (gethash 'pango-matrix-xy atdoc:*function-name-alias*) "Accessor"
      (documentation 'pango-matrix-xy 'function)
 "@version{2013-6-29}
  Accessor of the slot @code{xy} of the @class{pango-matrix} structure.")

#+cl-cffi-gtk-documentation
(setf (gethash 'pango-matrix-yx atdoc:*function-name-alias*) "Accessor"
      (documentation 'pango-matrix-yx 'function)
 "@version{2013-6-29}
  Accessor of the slot @code{yx} of the @class{pango-matrix} structure.")

#+cl-cffi-gtk-documentation
(setf (gethash 'pango-matrix-yy atdoc:*function-name-alias*) "Accessor"
      (documentation 'pango-matrix-yy 'function)
 "@version{2013-6-29}
  Accessor of the slot @code{yy} of the @class{pango-matrix} structure.")

#+cl-cffi-gtk-documentation
(setf (gethash 'pango-matrix-x0 atdoc:*function-name-alias*) "Accessor"
      (documentation 'pango-matrix-x0 'function)
 "@version{2013-6-29}
  Accessor of the slot @code{x0} of the @class{pango-matrix} structure.")

#+cl-cffi-gtk-documentation
(setf (gethash 'pango-matrix-y0 atdoc:*function-name-alias*) "Accessor"
      (documentation 'pango-matrix-y0 'function)
 "@version{2013-6-29}
  Accessor of the slot @code{y0} of the @class{pango-matrix} structure.")

;;; ----------------------------------------------------------------------------
;;; PANGO_TYPE_MATRIX
;;;
;;; #define PANGO_TYPE_MATRIX (pango_matrix_get_type ())
;;;
;;; The GObject type for PangoMatrix
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; PANGO_MATRIX_INIT
;;;
;;; #define PANGO_MATRIX_INIT { 1., 0., 0., 1., 0., 0. }
;;;
;;; Constant that can be used to initialize a PangoMatrix to the identity
;;; transform.
;;;
;;; PangoMatrix matrix = PANGO_MATRIX_INIT;
;;; pango_matrix_rotate (&matrix, 45.);
;;;
;;; Since 1.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_matrix_copy ()
;;;
;;; PangoMatrix * pango_matrix_copy (const PangoMatrix *matrix);
;;;
;;; Copies a PangoMatrix.
;;;
;;; matrix :
;;;     a PangoMatrix, may be NULL
;;;
;;; Returns :
;;;     the newly allocated PangoMatrix, which should be freed with
;;;     pango_matrix_free(), or NULL if matrix was NULL.
;;;
;;; Since 1.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_matrix_free ()
;;;
;;; void pango_matrix_free (PangoMatrix *matrix);
;;;
;;; Free a PangoMatrix created with pango_matrix_copy().
;;;
;;; matrix :
;;;     a PangoMatrix, may be NULL
;;;
;;; Since 1.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_matrix_translate ()
;;;
;;; void pango_matrix_translate (PangoMatrix *matrix,
;;;                              double tx,
;;;                              double ty);
;;;
;;; Changes the transformation represented by matrix to be the transformation
;;; given by first translating by (tx, ty) then applying the original
;;; transformation.
;;;
;;; matrix :
;;;     a PangoMatrix
;;;
;;; tx :
;;;     amount to translate in the X direction
;;;
;;; ty :
;;;     amount to translate in the Y direction
;;;
;;; Since 1.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_matrix_scale ()
;;;
;;; void pango_matrix_scale (PangoMatrix *matrix,
;;;                          double scale_x,
;;;                          double scale_y);
;;;
;;; Changes the transformation represented by matrix to be the transformation
;;; given by first scaling by sx in the X direction and sy in the Y direction
;;; then applying the original transformation.
;;;
;;; matrix :
;;;     a PangoMatrix
;;;
;;; scale_x :
;;;     amount to scale by in X direction
;;;
;;; scale_y :
;;;     amount to scale by in Y direction
;;;
;;; Since 1.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_matrix_rotate ()
;;;
;;; void pango_matrix_rotate (PangoMatrix *matrix, double degrees);
;;;
;;; Changes the transformation represented by matrix to be the transformation
;;; given by first rotating by degrees degrees counter-clockwise then applying
;;; the original transformation.
;;;
;;; matrix :
;;;     a PangoMatrix
;;;
;;; degrees :
;;;     degrees to rotate counter-clockwise
;;;
;;; Since 1.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_matrix_concat ()
;;;
;;; void pango_matrix_concat (PangoMatrix *matrix,
;;;                           const PangoMatrix *new_matrix);
;;;
;;; Changes the transformation represented by matrix to be the transformation
;;; given by first applying transformation given by new_matrix then applying the
;;; original transformation.
;;;
;;; matrix :
;;;     a PangoMatrix
;;;
;;; new_matrix :
;;;     a PangoMatrix
;;;
;;; Since 1.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_matrix_transform_point ()
;;;
;;; void pango_matrix_transform_point (const PangoMatrix *matrix,
;;;                                    double *x,
;;;                                    double *y);
;;;
;;; Transforms the point (x, y) by matrix.
;;;
;;; matrix :
;;;     a PangoMatrix, or NULL
;;;
;;; x :
;;;     in/out X position. [inout]
;;;
;;; y :
;;;     in/out Y position. [inout]
;;;
;;; Since 1.16
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_matrix_transform_distance ()
;;;
;;; void pango_matrix_transform_distance (const PangoMatrix *matrix,
;;;                                       double *dx,
;;;                                       double *dy);
;;;
;;; Transforms the distance vector (dx,dy) by matrix. This is similar to
;;; pango_matrix_transform_point() except that the translation components of the
;;; transformation are ignored. The calculation of the returned vector is as
;;; follows:
;;;
;;; dx2 = dx1 * xx + dy1 * xy;
;;; dy2 = dx1 * yx + dy1 * yy;
;;;
;;; Affine transformations are position invariant, so the same vector always
;;; transforms to the same vector. If (x1,y1) transforms to (x2,y2) then
;;; (x1+dx1,y1+dy1) will transform to (x1+dx2,y1+dy2) for all values of x1
;;; and x2.
;;;
;;; matrix :
;;;     a PangoMatrix, or NULL
;;;
;;; dx :
;;;     in/out X component of a distance vector. [inout]
;;;
;;; dy :
;;;     in/out Y component of a distance vector. [inout]
;;;
;;; Since 1.16
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_matrix_transform_rectangle ()
;;;
;;; void pango_matrix_transform_rectangle (const PangoMatrix *matrix,
;;;                                        PangoRectangle *rect);
;;;
;;; First transforms rect using matrix, then calculates the bounding box of the
;;; transformed rectangle. The rectangle should be in Pango units.
;;;
;;; This function is useful for example when you want to draw a rotated
;;; PangoLayout to an image buffer, and want to know how large the image should
;;; be and how much you should shift the layout when rendering.
;;;
;;; If you have a rectangle in device units (pixels), use
;;; pango_matrix_transform_pixel_rectangle().
;;;
;;; If you have the rectangle in Pango units and want to convert to transformed
;;; pixel bounding box, it is more accurate to transform it first (using this
;;; function) and pass the result to pango_extents_to_pixels(), first argument,
;;; for an inclusive rounded rectangle. However, there are valid reasons that
;;; you may want to convert to pixels first and then transform, for example when
;;; the transformed coordinates may overflow in Pango units (large matrix
;;; translation for example).
;;;
;;; matrix :
;;;     a PangoMatrix, or NULL
;;;
;;; rect :
;;;     in/out bounding box in Pango units, or NULL. [inout][allow-none]
;;;
;;; Since 1.16
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_matrix_transform_pixel_rectangle ()
;;;
;;; void pango_matrix_transform_pixel_rectangle (const PangoMatrix *matrix,
;;;                                              PangoRectangle *rect);
;;;
;;; First transforms the rect using matrix, then calculates the bounding box of
;;; the transformed rectangle. The rectangle should be in device units (pixels).
;;;
;;; This function is useful for example when you want to draw a rotated
;;; PangoLayout to an image buffer, and want to know how large the image should
;;; be and how much you should shift the layout when rendering.
;;;
;;; For better accuracy, you should use pango_matrix_transform_rectangle() on
;;; original rectangle in Pango units and convert to pixels afterward using
;;; pango_extents_to_pixels()'s first argument.
;;;
;;; matrix :
;;;     a PangoMatrix, or NULL
;;;
;;; rect :
;;;     in/out bounding box in device units, or NULL. [inout][allow-none]
;;;
;;; Since 1.16
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_matrix_get_font_scale_factor ()
;;;
;;; double pango_matrix_get_font_scale_factor (const PangoMatrix *matrix);
;;;
;;; Returns the scale factor of a matrix on the height of the font. That is, the
;;; scale factor in the direction perpendicular to the vector that the X
;;; coordinate is mapped to.
;;;
;;; matrix :
;;;     a PangoMatrix, may be NULL. [allow-none]
;;;
;;; Returns :
;;;     the scale factor of matrix on the height of the font, or 1.0 if matrix
;;;     is NULL.
;;;
;;; Since 1.12
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; PangoGlyph
;;;
;;; typedef guint32 PangoGlyph;
;;;
;;; A PangoGlyph represents a single glyph in the output form of a string.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; PANGO_GLYPH_EMPTY
;;;
;;; #define PANGO_GLYPH_EMPTY ((PangoGlyph)0x0FFFFFFF)
;;;
;;; The PANGO_GLYPH_EMPTY macro represents a PangoGlyph value that has a special
;;; meaning, which is a zero-width empty glyph. This is useful for example in
;;; shaper modules, to use as the glyph for various zero-width Unicode
;;; characters (those passing pango_is_zero_width()).
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; PANGO_GLYPH_INVALID_INPUT
;;;
;;; #define PANGO_GLYPH_INVALID_INPUT ((PangoGlyph)0xFFFFFFFF)
;;;
;;; The PANGO_GLYPH_EMPTY macro represents a PangoGlyph value that has a special
;;; meaning of invalid input. PangoLayout produces one such glyph per invalid
;;; input UTF-8 byte and such a glyph is rendered as a crossed box. Note that
;;; this value is defined such that it has the PANGO_GLYPH_UNKNOWN_FLAG on.
;;;
;;; Since 1.20
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; PANGO_GLYPH_UNKNOWN_FLAG
;;;
;;; #define PANGO_GLYPH_UNKNOWN_FLAG ((PangoGlyph)0x10000000)
;;;
;;; The PANGO_GLYPH_UNKNOWN_FLAG macro is a flag value that can be added to a
;;; gunichar value of a valid Unicode character, to produce a PangoGlyph value,
;;; representing an unknown-character glyph for the respective gunichar.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; PANGO_GET_UNKNOWN_GLYPH()
;;;
;;; #define PANGO_GET_UNKNOWN_GLYPH(wc)
;;;         ((PangoGlyph)(wc)|PANGO_GLYPH_UNKNOWN_FLAG)
;;;
;;; Returns a PangoGlyph value that means no glyph was found for wc. The way
;;; this unknown glyphs are rendered is backend specific. For example, a box
;;; with the hexadecimal Unicode code-point of the character written in it is
;;; what is done in the most common backends.
;;;
;;; wc :
;;;     a Unicode character
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; struct PangoGlyphInfo
;;;
;;; struct PangoGlyphInfo {
;;;   PangoGlyph    glyph;
;;;   PangoGlyphGeometry geometry;
;;;   PangoGlyphVisAttr  attr;
;;; };
;;;
;;; The PangoGlyphInfo structure represents a single glyph together with
;;; positioning information and visual attributes. It contains the following
;;; fields.
;;;
;;; PangoGlyph glyph;
;;;     the glyph itself.
;;;
;;; PangoGlyphGeometry geometry;
;;;     the positional information about the glyph.
;;;
;;; PangoGlyphVisAttr attr;
;;;     the visual attributes of the glyph.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; struct PangoGlyphGeometry
;;;
;;; struct PangoGlyphGeometry {
;;;   PangoGlyphUnit width;
;;;   PangoGlyphUnit x_offset;
;;;   PangoGlyphUnit y_offset;
;;; };
;;;
;;; The PangoGlyphGeometry structure contains width and positioning information
;;; for a single glyph.
;;;
;;; PangoGlyphUnit width;
;;;     the logical width to use for the the character.
;;;
;;; PangoGlyphUnit x_offset;
;;;     horizontal offset from nominal character position.
;;;
;;; PangoGlyphUnit y_offset;
;;;     vertical offset from nominal character position.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; PangoGlyphUnit
;;;
;;; typedef gint32 PangoGlyphUnit;
;;;
;;; The PangoGlyphUnit type is used to store dimensions within Pango. Dimensions
;;; are stored in 1/PANGO_SCALE of a device unit. (A device unit might be a
;;; pixel for screen display, or a point on a printer.) PANGO_SCALE is currently
;;; 1024, and may change in the future (unlikely though), but you should not
;;; depend on its exact value. The PANGO_PIXELS() macro can be used to convert
;;; from glyph units into device units with correct rounding.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; struct PangoGlyphVisAttr
;;;
;;; struct PangoGlyphVisAttr {
;;;   guint is_cluster_start : 1;
;;; };
;;;
;;; The PangoGlyphVisAttr is used to communicate information between the shaping
;;; phase and the rendering phase. More attributes may be added in the future.
;;;
;;; guint is_cluster_start : 1;
;;;     set for the first logical glyph in each cluster. (Clusters are stored in
;;;     visual order, within the cluster, glyphs are always ordered in logical
;;;     order, since visual order is meaningless; that is, in Arabic text,
;;;     accent glyphs follow the glyphs for the base character.)
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; struct PangoGlyphString
;;; ----------------------------------------------------------------------------

(define-g-boxed-opaque pango-glyph-string "PangoGlyphString"
  :alloc (%pango-glyph-string-new))

#+cl-cffi-gtk-documentation
(setf (gethash 'pango-glyph-string atdoc:*class-name-alias*) "CStruct"
      (documentation 'pango-glyph-string 'type)
 "@version{2013-6-29}
  @begin{short}
    The @sym{pango-glyph-string} structure is used to store strings of glyphs
    with geometry and visual attribute information.
  @end{short}
  The storage for the glyph information is owned by the structure which
  simplifies memory management.
  @begin{pre}
(define-g-boxed-opaque pango-glyph-string \"PangoGlyphString\"
  :alloc (%pango-glyph-string-new))
  @end{pre}")

(export (boxed-related-symbols 'pango-glyph-string))

;;; ----------------------------------------------------------------------------
;;; struct PangoGlyphItem
;;;
;;; struct PangoGlyphItem {
;;;   PangoItem        *item;
;;;   PangoGlyphString *glyphs;
;;; };
;;;
;;; A PangoGlyphItem is a pair of a PangoItem and the glyphs resulting from
;;; shaping the text corresponding to an item. As an example of the usage of
;;; PangoGlyphItem, the results of shaping text with PangoLayout is a list of
;;; PangoLayoutLine, each of which contains a list of PangoGlyphItem.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; struct PangoGlyphItemIter
;;;
;;; struct PangoGlyphItemIter {
;;;   PangoGlyphItem *glyph_item;
;;;   const gchar *text;
;;;
;;;   int start_glyph;
;;;   int start_index;
;;;   int start_char;
;;;
;;;   int end_glyph;
;;;   int end_index;
;;;   int end_char;
;;; };
;;;
;;; A PangoGlyphItemIter is an iterator over the clusters in a PangoGlyphItem.
;;; The forward direction of the iterator is the logical direction of text. That
;;; is, with increasing start_index and start_char values. If glyph_item is
;;; right-to-left (that is, if glyph_item->item->analysis.level is odd), then
;;; start_glyph decreases as the iterator moves forward. Moreover, in
;;; right-to-left cases, start_glyph is greater than end_glyph. An iterator
;;; should be initialized using either of pango_glyph_item_iter_init_start() and
;;; pango_glyph_item_iter_init_end(), for forward and backward iteration
;;; respectively, and walked over using any desired mixture of
;;; pango_glyph_item_iter_next_cluster() and
;;; pango_glyph_item_iter_prev_cluster(). A common idiom for doing a forward
;;; iteration over the clusters is:
;;;
;;; PangoGlyphItemIter cluster_iter;
;;;
;;; gboolean have_cluster;
;;; for (have_cluster = pango_glyph_item_iter_init_start (&cluster_iter,
;;;                                                       glyph_item, text);
;;;      have_cluster;
;;;      have_cluster = pango_glyph_item_iter_next_cluster (&cluster_iter))
;;; {
;;;   ...
;;; }
;;;
;;; Note that text is the start of the text for layout, which is then indexed by
;;; glyph_item->item->offset to get to the text of glyph_item. The start_index
;;; and end_index values can directly index into text. The start_glyph,
;;; end_glyph, start_char, and end_char values however are zero-based for the
;;; glyph_item. For each cluster, the item pointed at by the start variables is
;;; included in the cluster while the one pointed at by end variables is not.
;;; None of the members of a PangoGlyphItemIter should be modified manually.
;;;
;;; Since 1.22
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; PANGO_TYPE_GLYPH_STRING
;;;
;;; #define PANGO_TYPE_GLYPH_STRING (pango_glyph_string_get_type ())
;;;
;;; The GObject type for PangoGlyphString.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_glyph_string_new ()
;;;
;;; PangoGlyphString * pango_glyph_string_new (void);
;;;
;;; Create a new PangoGlyphString.
;;;
;;; Returns :
;;;     the newly allocated PangoGlyphString, which should be freed with
;;;     pango_glyph_string_free().
;;; ----------------------------------------------------------------------------

(defcfun ("pango_glyph_string_new" %pango-glyp-string-new) :pointer)

;;; ----------------------------------------------------------------------------
;;; pango_glyph_string_copy ()
;;;
;;; PangoGlyphString * pango_glyph_string_copy (PangoGlyphString *string);
;;;
;;; Copy a glyph string and associated storage.
;;;
;;; string :
;;;     a PangoGlyphString, may be NULL
;;;
;;; Returns :
;;;     the newly allocated PangoGlyphString, which should be freed with
;;;     pango_glyph_string_free(), or NULL if string was NULL.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_glyph_string_set_size ()
;;;
;;; void pango_glyph_string_set_size (PangoGlyphString *string, gint new_len);
;;;
;;; Resize a glyph string to the given length.
;;;
;;; string :
;;;     a PangoGlyphString.
;;;
;;; new_len :
;;;     the new length of the string.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_glyph_string_free ()
;;;
;;; void pango_glyph_string_free (PangoGlyphString *string);
;;;
;;; Free a glyph string and associated storage.
;;;
;;; string :
;;;     a PangoGlyphString, may be NULL
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_glyph_string_extents ()
;;;
;;; void pango_glyph_string_extents (PangoGlyphString *glyphs,
;;;                                  PangoFont *font,
;;;                                  PangoRectangle *ink_rect,
;;;                                  PangoRectangle *logical_rect);
;;;
;;; Compute the logical and ink extents of a glyph string. See the documentation
;;; for pango_font_get_glyph_extents() for details about the interpretation of
;;; the rectangles.
;;;
;;; glyphs :
;;;     a PangoGlyphString
;;;
;;; font :
;;;     a PangoFont
;;;
;;; ink_rect :
;;;     rectangle used to store the extents of the glyph string as drawn or NULL
;;;     to indicate that the result is not needed
;;;
;;; logical_rect :
;;;     rectangle used to store the logical extents of the glyph string or NULL
;;;     to indicate that the result is not needed
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_glyph_string_extents_range ()
;;;
;;; void pango_glyph_string_extents_range (PangoGlyphString *glyphs,
;;;                                        int start,
;;;                                        int end,
;;;                                        PangoFont *font,
;;;                                        PangoRectangle *ink_rect,
;;;                                        PangoRectangle *logical_rect);
;;;
;;; Computes the extents of a sub-portion of a glyph string. The extents are
;;; relative to the start of the glyph string range (the origin of their
;;; coordinate system is at the start of the range, not at the start of the
;;; entire glyph string).
;;;
;;; glyphs :
;;;     a PangoGlyphString
;;;
;;; start :
;;;     start index
;;;
;;; end :
;;;     end index (the range is the set of bytes with indices such that
;;;     start <= index < end)
;;;
;;; font :
;;;     a PangoFont
;;;
;;; ink_rect :
;;;     rectangle used to store the extents of the glyph string range as drawn
;;;     or NULL to indicate that the result is not needed
;;;
;;; logical_rect :
;;;     rectangle used to store the logical extents of the glyph string range or
;;;     NULL to indicate that the result is not needed
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_glyph_string_get_width ()
;;;
;;; int pango_glyph_string_get_width (PangoGlyphString *glyphs);
;;;
;;; Computes the logical width of the glyph string as can also be computed using
;;; pango_glyph_string_extents(). However, since this only computes the width,
;;; it's much faster. This is in fact only a convenience function that computes
;;; the sum of geometry.width for each glyph in the glyphs.
;;;
;;; glyphs :
;;;     a PangoGlyphString
;;;
;;; Returns :
;;;     the logical width of the glyph string.
;;;
;;; Since 1.14
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_glyph_string_index_to_x ()
;;;
;;; void pango_glyph_string_index_to_x (PangoGlyphString *glyphs,
;;;                                     char *text,
;;;                                     int length,
;;;                                     PangoAnalysis *analysis,
;;;                                     int index_,
;;;                                     gboolean trailing,
;;;                                     int *x_pos);
;;;
;;; Converts from character position to x position. (X position is measured from
;;; the left edge of the run). Character positions are computed by dividing up
;;; each cluster into equal portions.
;;;
;;; glyphs :
;;;     the glyphs return from pango_shape()
;;;
;;; text :
;;;     the text for the run
;;;
;;; length :
;;;     the number of bytes (not characters) in text.
;;;
;;; analysis :
;;;     the analysis information return from pango_itemize()
;;;
;;; index_ :
;;;     the byte index within text
;;;
;;; trailing :
;;;     whether we should compute the result for the beginning (FALSE) or end
;;;     (TRUE) of the character
;;;
;;; x_pos :
;;;     location to store result
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_glyph_string_x_to_index ()
;;;
;;; void pango_glyph_string_x_to_index (PangoGlyphString *glyphs,
;;;                                     char *text,
;;;                                     int length,
;;;                                     PangoAnalysis *analysis,
;;;                                     int x_pos,
;;;                                     int *index_,
;;;                                     int *trailing);
;;;
;;; Convert from x offset to character position. Character positions are
;;; computed by dividing up each cluster into equal portions. In scripts where
;;; positioning within a cluster is not allowed (such as Thai), the returned
;;; value may not be a valid cursor position; the caller must combine the result
;;; with the logical attributes for the text to compute the valid cursor
;;; position.
;;;
;;; glyphs :
;;;     the glyphs returned from pango_shape()
;;;
;;; text :
;;;     the text for the run
;;;
;;; length :
;;;     the number of bytes (not characters) in text.
;;;
;;; analysis :
;;;     the analysis information return from pango_itemize()
;;;
;;; x_pos :
;;;     the x offset (in Pango units)
;;;
;;; index_ :
;;;     location to store calculated byte index within text
;;;
;;; trailing :
;;;     location to store a boolean indicating whether the user clicked on the
;;;     leading or trailing edge of the character
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_glyph_string_get_logical_widths ()
;;;
;;; void pango_glyph_string_get_logical_widths (PangoGlyphString *glyphs,
;;;                                             const char *text,
;;;                                             int length,
;;;                                             int embedding_level,
;;;                                             int *logical_widths);
;;;
;;; Given a PangoGlyphString resulting from pango_shape() and the corresponding
;;; text, determine the screen width corresponding to each character. When
;;; multiple characters compose a single cluster, the width of the entire
;;; cluster is divided equally among the characters.
;;;
;;; See also pango_glyph_item_get_logical_widths().
;;;
;;; glyphs :
;;;     a PangoGlyphString
;;;
;;; text :
;;;     the text corresponding to the glyphs
;;;
;;; length :
;;;     the length of text, in bytes
;;;
;;; embedding_level :
;;;     the embedding level of the string
;;;
;;; logical_widths :
;;;     an array whose length is the number of characters in text (equal to
;;;     g_utf8_strlen (text, length) unless text has NUL bytes) to be filled in
;;;     with the resulting character widths
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; PANGO_TYPE_GLYPH_ITEM
;;;
;;; #define PANGO_TYPE_GLYPH_ITEM (pango_glyph_item_get_type ())
;;;
;;; The GObject type for PangoGlyphItem.
;;;
;;; Since 1.20
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_glyph_item_copy ()
;;;
;;; PangoGlyphItem * pango_glyph_item_copy (PangoGlyphItem *orig);
;;;
;;; Make a deep copy of an existing PangoGlyphItem structure.
;;;
;;; orig :
;;;     a PangoGlyphItem, may be NULL
;;;
;;; Returns :
;;;     the newly allocated PangoGlyphItem, which should be freed with
;;;     pango_glyph_item_free(), or NULL if orig was NULL.
;;;
;;; Since 1.20
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_glyph_item_free ()
;;;
;;; void pango_glyph_item_free (PangoGlyphItem *glyph_item);
;;;
;;; Frees a PangoGlyphItem and resources to which it points.
;;;
;;; glyph_item :
;;;     a PangoGlyphItem, may be NULL
;;;
;;; Since 1.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_glyph_item_split ()
;;;
;;; PangoGlyphItem * pango_glyph_item_split (PangoGlyphItem *orig,
;;;                                          const char *text,
;;;                                          int split_index);
;;;
;;; Modifies orig to cover only the text after split_index, and returns a new
;;; item that covers the text before split_index that used to be in orig. You
;;; can think of split_index as the length of the returned item. split_index may
;;; not be 0, and it may not be greater than or equal to the length of orig
;;; (that is, there must be at least one byte assigned to each item, you can't
;;; create a zero-length item).
;;;
;;; This function is similar in function to pango_item_split() (and uses it
;;; internally.)
;;;
;;; orig :
;;;     a PangoItem
;;;
;;; text :
;;;     text to which positions in orig apply
;;;
;;; split_index :
;;;     byte index of position to split item, relative to the start of the item
;;;
;;; Returns :
;;;     the newly allocated item representing text before split_index, which
;;;     should be freed with pango_glyph_item_free().
;;;
;;; Since 1.2
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_glyph_item_apply_attrs ()
;;;
;;; GSList * pango_glyph_item_apply_attrs (PangoGlyphItem *glyph_item,
;;;                                        const char *text,
;;;                                        PangoAttrList *list);
;;;
;;; Splits a shaped item (PangoGlyphItem) into multiple items based on an
;;; attribute list. The idea is that if you have attributes that don't affect
;;; shaping, such as color or underline, to avoid affecting shaping, you filter
;;; them out (pango_attr_list_filter()), apply the shaping process and then
;;; reapply them to the result using this function.
;;;
;;; All attributes that start or end inside a cluster are applied to that
;;; cluster; for instance, if half of a cluster is underlined and the other-half
;;; strikethrough, then the cluster will end up with both underline and
;;; strikethrough attributes. In these cases, it may happen that
;;; item->extra_attrs for some of the result items can have multiple attributes
;;; of the same type.
;;;
;;; This function takes ownership of glyph_item; it will be reused as one of the
;;; elements in the list.
;;;
;;; glyph_item :
;;;     a shaped item
;;;
;;; text :
;;;     text that list applies to
;;;
;;; list :
;;;     a PangoAttrList
;;;
;;; Returns :
;;;     A list of glyph items resulting from splitting glyph_item. Free the
;;;     elements using pango_glyph_item_free(), the list using g_slist_free().
;;;
;;; Since 1.2
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_glyph_item_letter_space ()
;;;
;;; void pango_glyph_item_letter_space (PangoGlyphItem *glyph_item,
;;;                                     const char *text,
;;;                                     PangoLogAttr *log_attrs,
;;;                                     int letter_spacing);
;;;
;;; Adds spacing between the graphemes of glyph_item to give the effect of
;;; typographic letter spacing.
;;;
;;; glyph_item :
;;;     a PangoGlyphItem
;;;
;;; text :
;;;     text that glyph_item corresponds to (glyph_item->item->offset is an
;;;     offset from the start of text)
;;;
;;; log_attrs :
;;;     logical attributes for the item (the first logical attribute refers to
;;;     the position before the first character in the item)
;;;
;;; letter_spacing :
;;;     amount of letter spacing to add in Pango units. May be negative, though
;;;     too large negative values will give ugly results.
;;;
;;; Since 1.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_glyph_item_get_logical_widths ()
;;;
;;; void pango_glyph_item_get_logical_widths (PangoGlyphItem *glyph_item,
;;;                                           const char *text,
;;;                                           int *logical_widths);
;;;
;;; Given a PangoGlyphItem and the corresponding text, determine the screen
;;; width corresponding to each character. When multiple characters compose a
;;; single cluster, the width of the entire cluster is divided equally among the
;;; characters.
;;;
;;; See also pango_glyph_string_get_logical_widths().
;;;
;;; glyph_item :
;;;     a PangoGlyphItem
;;;
;;; text :
;;;     text that glyph_item corresponds to (glyph_item->item->offset is an
;;;     offset from the start of text)
;;;
;;; logical_widths :
;;;     an array whose length is the number of characters in glyph_item (equal
;;;     to glyph_item->item->num_chars) to be filled in with the resulting
;;;     character widths
;;;
;;; Since 1.26
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; PANGO_TYPE_GLYPH_ITEM_ITER
;;;
;;; #define PANGO_TYPE_GLYPH_ITEM_ITER (pango_glyph_item_iter_get_type ())
;;;
;;; The GObject type for PangoGlyphItemIter.
;;;
;;; Since 1.22
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_glyph_item_iter_copy ()
;;;
;;; PangoGlyphItemIter * pango_glyph_item_iter_copy (PangoGlyphItemIter *orig);
;;;
;;; Make a shallow copy of an existing PangoGlyphItemIter structure.
;;;
;;; orig :
;;;     a PangoGlyphItemIter, may be NULL
;;;
;;; Returns :
;;;     the newly allocated PangoGlyphItemIter, which should be freed with
;;;     pango_glyph_item_iter_free(), or NULL if orig was NULL.
;;;
;;; Since 1.22
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_glyph_item_iter_free ()
;;;
;;; void pango_glyph_item_iter_free (PangoGlyphItemIter *iter);
;;;
;;; Frees a PangoGlyphItemIter created by pango_glyph_item_iter_copy().
;;;
;;; iter :
;;;     a PangoGlyphItemIter, may be NULL
;;;
;;; Since 1.22
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_glyph_item_iter_init_start ()
;;;
;;; gboolean pango_glyph_item_iter_init_start (PangoGlyphItemIter *iter,
;;;                                            PangoGlyphItem *glyph_item,
;;;                                            const char *text);
;;;
;;; Initializes a PangoGlyphItemIter structure to point to the first cluster in
;;; a glyph item. See PangoGlyphItemIter for details of cluster orders.
;;;
;;; iter :
;;;     a PangoGlyphItemIter
;;;
;;; glyph_item :
;;;     the glyph item to iterate over
;;;
;;; text :
;;;     text corresponding to the glyph item
;;;
;;; Returns :
;;;     FALSE if there are no clusters in the glyph item
;;;
;;; Since 1.22
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_glyph_item_iter_init_end ()
;;;
;;; gboolean pango_glyph_item_iter_init_end (PangoGlyphItemIter *iter,
;;;                                          PangoGlyphItem *glyph_item,
;;;                                          const char *text);
;;;
;;; Initializes a PangoGlyphItemIter structure to point to the last cluster in
;;; a glyph item. See PangoGlyphItemIter for details of cluster orders.
;;;
;;; iter :
;;;     a PangoGlyphItemIter
;;;
;;; glyph_item :
;;;     the glyph item to iterate over
;;;
;;; text :
;;;     text corresponding to the glyph item
;;;
;;; Returns :
;;;     FALSE if there are no clusters in the glyph item
;;;
;;; Since 1.22
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_glyph_item_iter_next_cluster ()
;;;
;;; gboolean pango_glyph_item_iter_next_cluster (PangoGlyphItemIter *iter);
;;;
;;; Advances the iterator to the next cluster in the glyph item. See
;;; PangoGlyphItemIter for details of cluster orders.
;;;
;;; iter :
;;;     a PangoGlyphItemIter
;;;
;;; Returns :
;;;     TRUE if the iterator was advanced, FALSE if we were already on the last
;;;     cluster.
;;;
;;; Since 1.22
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_glyph_item_iter_prev_cluster ()
;;;
;;; gboolean pango_glyph_item_iter_prev_cluster (PangoGlyphItemIter *iter);
;;;
;;; Moves the iterator to the preceding cluster in the glyph item. See
;;; PangoGlyphItemIter for details of cluster orders.
;;;
;;; iter :
;;;     a PangoGlyphItemIter
;;;
;;; Returns :
;;;     TRUE if the iterator was moved, FALSE if we were already on the first
;;;     cluster.
;;; ----------------------------------------------------------------------------

;;; --- End of file pango.glyph.lisp -------------------------------------------
