;;; ----------------------------------------------------------------------------
;;; cairo.matrix.lisp
;;;
;;; The documentation of this file is taken from the Cairo Reference Manual
;;; Version 1.12.16 and modified to document the Lisp binding to the Cairo
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
;;; cairo_matrix_t
;;;
;;; Generic matrix operations
;;;
;;; Synopsis
;;;
;;;     cairo_matrix_t
;;;
;;;     cairo_matrix_init
;;;     cairo_matrix_init_identity
;;;     cairo_matrix_init_translate
;;;     cairo_matrix_init_scale
;;;     cairo_matrix_init_rotate
;;;     cairo_matrix_translate
;;;     cairo_matrix_scale
;;;     cairo_matrix_rotate
;;;     cairo_matrix_invert
;;;     cairo_matrix_multiply
;;;     cairo_matrix_transform_distance
;;;     cairo_matrix_transform_point
;;;
;;; Description
;;;
;;; cairo_matrix_t is used throughout cairo to convert between different
;;; coordinate spaces. A cairo_matrix_t holds an affine transformation, such as
;;; a scale, rotation, shear, or a combination of these. The transformation of
;;; a point (x,y) is given by:
;;;
;;; x_new = xx * x + xy * y + x0;
;;; y_new = yx * x + yy * y + y0;
;;;
;;; The current transformation matrix of a cairo_t, represented as a
;;; cairo_matrix_t, defines the transformation from user-space coordinates to
;;; device-space coordinates. See cairo_get_matrix() and cairo_set_matrix().
;;; ----------------------------------------------------------------------------

(in-package :cairo)

;;; ----------------------------------------------------------------------------
;;; cairo_matrix_t
;;; ----------------------------------------------------------------------------

(defcstruct cairo-matrix-t
  (xx :double)
  (yx :double)
  (xy :double)
  (yy :double)
  (x0 :double)
  (y0 :double))

#+cl-cffi-gtk-documentation
(setf (gethash 'cairo-matrix-t atdoc:*symbol-name-alias*) "CStruct"
      (gethash 'cairo-matrix-t atdoc:*external-symbols*)
 "@version{2014-2-1}
  @begin{short}
    A @sym{cairo-matrix-t} holds an affine transformation, such as a scale,
    rotation, shear, or a combination of those.
  @end{short}
  The transformation of a point (x, y) is given by:
  @begin{pre}
 x_new = xx * x + xy * y + x0;
 y_new = yx * x + yy * y + y0;
  @end{pre}
  The current transformation matrix of a @symbol{cairo-t}, represented as a
  @sym{cairo-matrix-t}, defines the transformation from user-space coordinates
  to device-space coordinates. See the functions @fun{cairo-get-matrix} and
  @fun{cairo-set-matrix}.
  @begin{pre}
(defcstruct cairo-matrix-t
  (xx :double)
  (yx :double)
  (xy :double)
  (yy :double)
  (x0 :double)
  (y0 :double))
  @end{pre}
  @begin[code]{table}
    @entry[xx]{xx component of the affine transformation}
    @entry[yx]{yx component of the affine transformation}
    @entry[xy]{xy component of the affine transformation}
    @entry[yy]{yy component of the affine transformation}
    @entry[x0]{x translation component of the affine transformation}
    @entry[y0]{y translation component of the affine transformation}
  @end{table}
  Since 1.0
  @see-symbol{cairo-t}
  @see-function{cairo-get-matrix}
  @see-function{cairo-set-matrix}")

(export 'cairo-matrix-t)

;;; ----------------------------------------------------------------------------
;;; cairo_matrix_init ()
;;;
;;; void cairo_matrix_init (cairo_matrix_t *matrix,
;;;                         double xx,
;;;                         double yx,
;;;                         double xy,
;;;                         double yy,
;;;                         double x0,
;;;                         double y0);
;;;
;;; Sets matrix to be the affine transformation given by xx, yx, xy, yy, x0, y0.
;;; The transformation is given by:
;;;
;;;  x_new = xx * x + xy * y + x0;
;;;  y_new = yx * x + yy * y + y0;
;;;
;;; matrix :
;;;     a cairo_matrix_t
;;;
;;; xx :
;;;     xx component of the affine transformation
;;;
;;; yx :
;;;     yx component of the affine transformation
;;;
;;; xy :
;;;     xy component of the affine transformation
;;;
;;; yy :
;;;     yy component of the affine transformation
;;;
;;; x0 :
;;;     X translation component of the affine transformation
;;;
;;; y0 :
;;;     Y translation component of the affine transformation
;;;
;;; Since 1.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_matrix_init_identity ()
;;;
;;; void cairo_matrix_init_identity (cairo_matrix_t *matrix);
;;;
;;; Modifies matrix to be an identity transformation.
;;;
;;; matrix :
;;;     a cairo_matrix_t
;;;
;;; Since 1.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_matrix_init_translate ()
;;;
;;; void cairo_matrix_init_translate (cairo_matrix_t *matrix,
;;;                                   double tx,
;;;                                   double ty);
;;;
;;; Initializes matrix to a transformation that translates by tx and ty in the
;;; X and Y dimensions, respectively.
;;;
;;; matrix :
;;;     a cairo_matrix_t
;;;
;;; tx :
;;;     amount to translate in the X direction
;;;
;;; ty :
;;;     amount to translate in the Y direction
;;;
;;; Since 1.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_matrix_init_scale ()
;;;
;;; void cairo_matrix_init_scale (cairo_matrix_t *matrix,
;;;                               double sx,
;;;                               double sy);
;;;
;;; Initializes matrix to a transformation that scales by sx and sy in the X and
;;; Y dimensions, respectively.
;;;
;;; matrix :
;;;     a cairo_matrix_t
;;;
;;; sx :
;;;     scale factor in the X direction
;;;
;;; sy :
;;;     scale factor in the Y direction
;;;
;;; Since 1.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_matrix_init_rotate ()
;;;
;;; void cairo_matrix_init_rotate (cairo_matrix_t *matrix, double radians);
;;;
;;; Initialized matrix to a transformation that rotates by radians.
;;;
;;; matrix :
;;;     a cairo_matrix_t
;;;
;;; radians :
;;;     angle of rotation, in radians. The direction of rotation is defined such
;;;     that positive angles rotate in the direction from the positive X axis
;;;     toward the positive Y axis. With the default axis orientation of cairo,
;;;     positive angles rotate in a clockwise direction.
;;;
;;; Since 1.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_matrix_translate ()
;;;
;;; void cairo_matrix_translate (cairo_matrix_t *matrix,
;;;                              double tx,
;;;                              double ty);
;;;
;;; Applies a translation by tx, ty to the transformation in matrix. The effect
;;; of the new transformation is to first translate the coordinates by tx and
;;; ty, then apply the original transformation to the coordinates.
;;;
;;; matrix :
;;;     a cairo_matrix_t
;;;
;;; tx :
;;;     amount to translate in the X direction
;;;
;;; ty :
;;;     amount to translate in the Y direction
;;;
;;; Since 1.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_matrix_scale ()
;;;
;;; void cairo_matrix_scale (cairo_matrix_t *matrix, double sx, double sy);
;;;
;;; Applies scaling by sx, sy to the transformation in matrix. The effect of the
;;; new transformation is to first scale the coordinates by sx and sy, then
;;; apply the original transformation to the coordinates.
;;;
;;; matrix :
;;;     a cairo_matrix_t
;;;
;;; sx :
;;;     scale factor in the X direction
;;;
;;; sy :
;;;     scale factor in the Y direction
;;;
;;; Since 1.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_matrix_rotate ()
;;;
;;; void cairo_matrix_rotate (cairo_matrix_t *matrix, double radians);
;;;
;;; Applies rotation by radians to the transformation in matrix. The effect of
;;; the new transformation is to first rotate the coordinates by radians, then
;;; apply the original transformation to the coordinates.
;;;
;;; matrix :
;;;     a cairo_matrix_t
;;;
;;; radians :
;;;     angle of rotation, in radians. The direction of rotation is defined such
;;;     that positive angles rotate in the direction from the positive X axis
;;;     toward the positive Y axis. With the default axis orientation of cairo,
;;;     positive angles rotate in a clockwise direction.
;;;
;;; Since 1.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_matrix_invert ()
;;;
;;; cairo_status_t cairo_matrix_invert (cairo_matrix_t *matrix);
;;;
;;; Changes matrix to be the inverse of its original value. Not all
;;; transformation matrices have inverses; if the matrix collapses points
;;; together (it is degenerate), then it has no inverse and this function will
;;; fail.
;;;
;;; matrix :
;;;     a cairo_matrix_t
;;;
;;; Returns :
;;;     If matrix has an inverse, modifies matrix to be the inverse matrix and
;;;     returns CAIRO_STATUS_SUCCESS. Otherwise, returns
;;;     CAIRO_STATUS_INVALID_MATRIX.
;;;
;;; Since 1.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_matrix_multiply ()
;;;
;;; void cairo_matrix_multiply (cairo_matrix_t *result,
;;;                             const cairo_matrix_t *a,
;;;                             const cairo_matrix_t *b);
;;;
;;; Multiplies the affine transformations in a and b together and stores the
;;; result in result. The effect of the resulting transformation is to first
;;; apply the transformation in a to the coordinates and then apply the
;;; transformation in b to the coordinates.
;;;
;;; It is allowable for result to be identical to either a or b.
;;;
;;; result :
;;;     a cairo_matrix_t in which to store the result
;;;
;;; a :
;;;     a cairo_matrix_t
;;;
;;; b :
;;;     a cairo_matrix_t
;;;
;;; Since 1.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_matrix_transform_distance ()
;;;
;;; void cairo_matrix_transform_distance (const cairo_matrix_t *matrix,
;;;                                       double *dx,
;;;                                       double *dy);
;;;
;;; Transforms the distance vector (dx,dy) by matrix. This is similar to
;;; cairo_matrix_transform_point() except that the translation components of the
;;; transformation are ignored. The calculation of the returned vector is as
;;; follows:
;;;
;;; dx2 = dx1 * a + dy1 * c;
;;; dy2 = dx1 * b + dy1 * d;
;;;
;;; Affine transformations are position invariant, so the same vector always
;;; transforms to the same vector. If (x1,y1) transforms to (x2,y2) then
;;; (x1+dx1,y1+dy1) will transform to (x1+dx2,y1+dy2) for all values of x1 and
;;; x2.
;;;
;;; matrix :
;;;     a cairo_matrix_t
;;;
;;; dx :
;;;     X component of a distance vector. An in/out parameter
;;;
;;; dy :
;;;     Y component of a distance vector. An in/out parameter
;;;
;;; Since 1.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_matrix_transform_point ()
;;;
;;; void cairo_matrix_transform_point (const cairo_matrix_t *matrix,
;;;                                    double *x,
;;;                                    double *y);
;;;
;;; Transforms the point (x, y) by matrix.
;;;
;;; matrix :
;;;     a cairo_matrix_t
;;;
;;; x :
;;;     X position. An in/out parameter
;;;
;;; y :
;;;     Y position. An in/out parameter
;;;
;;; Since 1.0
;;; ----------------------------------------------------------------------------

;;; --- End of file cairo.matrix.lisp ------------------------------------------
