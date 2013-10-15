;;; ----------------------------------------------------------------------------
;;; cairo.paths.lisp
;;;
;;; The documentation has been copied from the Cairo Reference Manual
;;; for Cairo 1.12.2. See <http://cairographics.org>.
;;; The API documentation of the Lisp binding is available at
;;; <http://www.crategus.com/books/cl-cffi-gtk/>.
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
;;; Paths
;;;
;;; Creating paths and manipulating path data
;;;
;;; Synopsis
;;;
;;;     cairo_path_t
;;;     cairo_path_data_t
;;;     cairo_path_data_type_t
;;;
;;;     cairo_copy_path
;;;     cairo_copy_path_flat
;;;     cairo_path_destroy
;;;     cairo_append_path
;;;     cairo_has_current_point
;;;     cairo_get_current_point
;;;     cairo_new_path
;;;     cairo_new_sub_path
;;;     cairo_close_path
;;;     cairo_arc
;;;     cairo_arc_negative
;;;     cairo_curve_to
;;;     cairo_line_to
;;;     cairo_move_to
;;;     cairo_rectangle
;;;     cairo_glyph_path
;;;     cairo_text_path
;;;     cairo_rel_curve_to
;;;     cairo_rel_line_to
;;;     cairo_rel_move_to
;;;     cairo_path_extents
;;;
;;; Description
;;;
;;; Paths are the most basic drawing tools and are primarily used to implicitly
;;; generate simple masks.
;;; ----------------------------------------------------------------------------

(in-package :cairo)

;;; ----------------------------------------------------------------------------
;;; cairo_path_t
;;;
;;; typedef struct {
;;;     cairo_status_t status;
;;;     cairo_path_data_t *data;
;;;     int num_data;
;;; } cairo_path_t;
;;;
;;; A data structure for holding a path. This data structure serves as the
;;; return value for cairo_copy_path() and cairo_copy_path_flat() as well the
;;; input value for cairo_append_path().
;;;
;;; See cairo_path_data_t for hints on how to iterate over the actual data
;;; within the path.
;;;
;;; The num_data member gives the number of elements in the data array. This
;;; number is larger than the number of independent path portions (defined in
;;; cairo_path_data_type_t), since the data includes both headers and
;;; coordinates for each portion.
;;;
;;; cairo_status_t status;
;;;     the current error status
;;;
;;; cairo_path_data_t *data;
;;;     the elements in the path
;;;
;;; int num_data;
;;;     the number of elements in the data array
;;;
;;; Since 1.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; union cairo_path_data_t
;;;
;;; union _cairo_path_data_t {
;;;     struct {
;;;     cairo_path_data_type_t type;
;;;     int length;
;;;     } header;
;;;     struct {
;;;     double x, y;
;;;     } point;
;;; };
;;;
;;; cairo_path_data_t is used to represent the path data inside a cairo_path_t.
;;;
;;; The data structure is designed to try to balance the demands of efficiency
;;; and ease-of-use. A path is represented as an array of cairo_path_data_t,
;;; which is a union of headers and points.
;;;
;;; Each portion of the path is represented by one or more elements in the
;;; array, (one header followed by 0 or more points). The length value of the
;;; header is the number of array elements for the current portion including the
;;; header, (ie. length == 1 + # of points), and where the number of points for
;;; each element type is as follows:
;;;
;;;     %CAIRO_PATH_MOVE_TO:     1 point
;;;     %CAIRO_PATH_LINE_TO:     1 point
;;;     %CAIRO_PATH_CURVE_TO:    3 points
;;;     %CAIRO_PATH_CLOSE_PATH:  0 points
;;;
;;; The semantics and ordering of the coordinate values are consistent with
;;; cairo_move_to(), cairo_line_to(), cairo_curve_to(), and cairo_close_path().
;;;
;;; Here is sample code for iterating through a cairo_path_t:
;;;
;;; int i;
;;; cairo_path_t *path;
;;; cairo_path_data_t *data;
;;;
;;; path = cairo_copy_path (cr);
;;;
;;; for (i=0; i < path->num_data; i += path->data[i].header.length) {
;;;     data = &path->data[i];
;;;     switch (data->header.type) {
;;;     case CAIRO_PATH_MOVE_TO:
;;;         do_move_to_things (data[1].point.x, data[1].point.y);
;;;         break;
;;;     case CAIRO_PATH_LINE_TO:
;;;         do_line_to_things (data[1].point.x, data[1].point.y);
;;;         break;
;;;     case CAIRO_PATH_CURVE_TO:
;;;         do_curve_to_things (data[1].point.x, data[1].point.y,
;;;                             data[2].point.x, data[2].point.y,
;;;                             data[3].point.x, data[3].point.y);
;;;         break;
;;;     case CAIRO_PATH_CLOSE_PATH:
;;;         do_close_path_things ();
;;;         break;
;;;     }
;;; }
;;; cairo_path_destroy (path);
;;;
;;; As of cairo 1.4, cairo does not mind if there are more elements in a portion
;;; of the path than needed. Such elements can be used by users of the cairo API
;;; to hold extra values in the path data structure. For this reason, it is
;;; recommended that applications always use data->header.length to iterate over
;;; the path data, instead of hardcoding the number of elements for each element
;;; type.
;;;
;;; Since 1.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; enum cairo_path_data_type_t
;;;
;;; typedef enum {
;;;     CAIRO_PATH_MOVE_TO,
;;;     CAIRO_PATH_LINE_TO,
;;;     CAIRO_PATH_CURVE_TO,
;;;     CAIRO_PATH_CLOSE_PATH
;;; } cairo_path_data_type_t;
;;;
;;; cairo_path_data_t is used to describe the type of one portion of a path when
;;; represented as a cairo_path_t. See cairo_path_data_t for details.
;;;
;;; CAIRO_PATH_MOVE_TO
;;;     A move-to operation, since 1.0
;;;
;;; CAIRO_PATH_LINE_TO
;;;     A line-to operation, since 1.0
;;;
;;; CAIRO_PATH_CURVE_TO
;;;     A curve-to operation, since 1.0
;;;
;;; CAIRO_PATH_CLOSE_PATH
;;;     A close-path operation, since 1.0
;;;
;;; Since 1.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_copy_path ()
;;;
;;; cairo_path_t * cairo_copy_path (cairo_t *cr);
;;;
;;; Creates a copy of the current path and returns it to the user as a
;;; cairo_path_t. See cairo_path_data_t for hints on how to iterate over the
;;; returned data structure.
;;;
;;; This function will always return a valid pointer, but the result will have
;;; no data (data==NULL and num_data==0), if either of the following conditions
;;; hold:
;;;
;;;     If there is insufficient memory to copy the path. In this case
;;;     path->status will be set to CAIRO_STATUS_NO_MEMORY.
;;;
;;;     If cr is already in an error state. In this case path->status will
;;;     contain the same status that would be returned by cairo_status().
;;;
;;; cr :
;;;     a cairo context
;;;
;;; Returns :
;;;     the copy of the current path. The caller owns the returned object and
;;;     should call cairo_path_destroy() when finished with it.
;;;
;;; Since 1.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_copy_path_flat ()
;;;
;;; cairo_path_t * cairo_copy_path_flat (cairo_t *cr);
;;;
;;; Gets a flattened copy of the current path and returns it to the user as a
;;; cairo_path_t. See cairo_path_data_t for hints on how to iterate over the
;;; returned data structure.
;;;
;;; This function is like cairo_copy_path() except that any curves in the path
;;; will be approximated with piecewise-linear approximations, (accurate to
;;; within the current tolerance value). That is, the result is guaranteed to
;;; not have any elements of type CAIRO_PATH_CURVE_TO which will instead be
;;; replaced by a series of CAIRO_PATH_LINE_TO elements.
;;;
;;; This function will always return a valid pointer, but the result will have
;;; no data (data==NULL and num_data==0), if either of the following conditions
;;; hold:
;;;
;;;     If there is insufficient memory to copy the path. In this case
;;;     path->status will be set to CAIRO_STATUS_NO_MEMORY.
;;;
;;;     If cr is already in an error state. In this case path->status will
;;;     contain the same status that would be returned by cairo_status().
;;;
;;; cr :
;;;     a cairo context
;;;
;;; Returns :
;;;     the copy of the current path. The caller owns the returned object and
;;;     should call cairo_path_destroy() when finished with it.
;;;
;;; Since 1.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_path_destroy ()
;;;
;;; void cairo_path_destroy (cairo_path_t *path);
;;;
;;; Immediately releases all memory associated with path. After a call to
;;; cairo_path_destroy() the path pointer is no longer valid and should not be
;;; used further.
;;;
;;; Note: cairo_path_destroy() should only be called with a pointer to a
;;; cairo_path_t returned by a cairo function. Any path that is created manually
;;; (ie. outside of cairo) should be destroyed manually as well.
;;;
;;; path :
;;;     a path previously returned by either cairo_copy_path() or
;;;     cairo_copy_path_flat().
;;;
;;; Since 1.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_append_path ()
;;;
;;; void cairo_append_path (cairo_t *cr, const cairo_path_t *path);
;;;
;;; Append the path onto the current path. The path may be either the return
;;; value from one of cairo_copy_path() or cairo_copy_path_flat() or it may be
;;; constructed manually. See cairo_path_t for details on how the path data
;;; structure should be initialized, and note that path->status must be
;;; initialized to CAIRO_STATUS_SUCCESS.
;;;
;;; cr :
;;;     a cairo context
;;;
;;; path :
;;;     path to be appended
;;;
;;; Since 1.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_has_current_point ()
;;;
;;; cairo_bool_t cairo_has_current_point (cairo_t *cr);
;;;
;;; Returns whether a current point is defined on the current path. See
;;; cairo_get_current_point() for details on the current point.
;;;
;;; cr :
;;;     a cairo context
;;;
;;; Returns :
;;;     whether a current point is defined.
;;;
;;; Since 1.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_get_current_point ()
;;;
;;; void cairo_get_current_point (cairo_t *cr, double *x, double *y);
;;;
;;; Gets the current point of the current path, which is conceptually the final
;;; point reached by the path so far.
;;;
;;; The current point is returned in the user-space coordinate system. If there
;;; is no defined current point or if cr is in an error status, x and y will
;;; both be set to 0.0. It is possible to check this in advance with
;;; cairo_has_current_point().
;;;
;;; Most path construction functions alter the current point. See the following
;;; for details on how they affect the current point: cairo_new_path(),
;;; cairo_new_sub_path(), cairo_append_path(), cairo_close_path(),
;;; cairo_move_to(), cairo_line_to(), cairo_curve_to(), cairo_rel_move_to(),
;;; cairo_rel_line_to(), cairo_rel_curve_to(), cairo_arc(),
;;; cairo_arc_negative(), cairo_rectangle(), cairo_text_path(),
;;; cairo_glyph_path(), cairo_stroke_to_path().
;;;
;;; Some functions use and alter the current point but do not otherwise change
;;; current path: cairo_show_text().
;;;
;;; Some functions unset the current path and as a result, current point:
;;; cairo_fill(), cairo_stroke().
;;;
;;; cr :
;;;     a cairo context
;;;
;;; x :
;;;     return value for X coordinate of the current point
;;;
;;; y :
;;;     return value for Y coordinate of the current point
;;;
;;; Since 1.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_new_path ()
;;;
;;; void cairo_new_path (cairo_t *cr);
;;;
;;; Clears the current path. After this call there will be no path and no
;;; current point.
;;;
;;; cr :
;;;     a cairo context
;;;
;;; Since 1.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_new_sub_path ()
;;;
;;; void cairo_new_sub_path (cairo_t *cr);
;;;
;;; Begin a new sub-path. Note that the existing path is not affected. After
;;; this call there will be no current point.
;;;
;;; In many cases, this call is not needed since new sub-paths are frequently
;;; started with cairo_move_to().
;;;
;;; A call to cairo_new_sub_path() is particularly useful when beginning a new
;;; sub-path with one of the cairo_arc() calls. This makes things easier as it
;;; is no longer necessary to manually compute the arc's initial coordinates for
;;; a call to cairo_move_to().
;;;
;;; cr :
;;;     a cairo context
;;;
;;; Since 1.2
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_close_path ()
;;;
;;; void cairo_close_path (cairo_t *cr);
;;;
;;; Adds a line segment to the path from the current point to the beginning of
;;; the current sub-path, (the most recent point passed to cairo_move_to()), and
;;; closes this sub-path. After this call the current point will be at the
;;; joined endpoint of the sub-path.
;;;
;;; The behavior of cairo_close_path() is distinct from simply calling
;;; cairo_line_to() with the equivalent coordinate in the case of stroking. When
;;; a closed sub-path is stroked, there are no caps on the ends of the sub-path.
;;; Instead, there is a line join connecting the final and initial segments of
;;; the sub-path.
;;;
;;; If there is no current point before the call to cairo_close_path(), this
;;; function will have no effect.
;;;
;;; Note: As of cairo version 1.2.4 any call to cairo_close_path() will place an
;;; explicit MOVE_TO element into the path immediately after the CLOSE_PATH
;;; element, (which can be seen in cairo_copy_path() for example). This can
;;; simplify path processing in some cases as it may not be necessary to save
;;; the "last move_to point" during processing as the MOVE_TO immediately after
;;; the CLOSE_PATH will provide that point.
;;;
;;; cr :
;;;     a cairo context
;;;
;;; Since 1.0
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_close_path" cairo-close-path) :void
  (cr (:pointer (:struct cairo-t))))

(export 'cairo-close-path)

;;; ----------------------------------------------------------------------------
;;; cairo_arc ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_arc" %cairo-arc) :void
  (cr (:pointer (:struct cairo-t)))
  (xc :double)
  (yc :double)
  (radius :double)
  (angle1 :double)
  (angle2 :double))

(defun cairo-arc (cr xc yc radius angle1 angle2)
 #+cl-cffi-gtk-documentation
 "@version{2013-8-1}
  @argument[cr]{a cairo context}
  @argument[xc]{x position of the center of the arc}
  @argument[yc]{y position of the center of the arc}
  @argument[radius]{the radius of the arc}
  @argument[angle1]{the start angle, in radians}
  @argument[angle2]{the end angle, in radians}
  @begin{short}
    Adds a circular arc of the given @arg{radius} to the current path.
  @end{short}
  The arc is centered at @code{(@arg{xc}, @arg{yc})}, begins at @arg{angle1} and
  proceeds in the direction of increasing angles to end at @arg{angle2}. If
  @arg{angle2} is less than @arg{angle1} it will be progressively increased by
  @code{2*PI} until it is greater than @arg{angle1}.

  If there is a current point, an initial line segment will be added to the
  path to connect the current point to the beginning of the arc. If this
  initial line is undesired, it can be avoided by calling the function
  @fun{cairo-new-sub-path} before calling the function @sym{cairo-arc}.

  Angles are measured in radians. An angle of 0.0 is in the direction of the
  positive x axis (in user space). An angle of @code{PI/2.0} radians (90
  degrees) is in the direction of the positive y axis (in user space). Angles
  increase in the direction from the positive x axis toward the positive y axis.
  So with the default transformation matrix, angles increase in a clockwise
  direction.

  This function gives the arc in the direction of increasing angles; see the
  function @fun{cairo-arc-negative} to get the arc in the direction of
  decreasing angles.

  The arc is circular in user space. To achieve an elliptical arc, you can
  scale the current transformation matrix by different amounts in the x and y
  directions. For example, to draw an ellipse in the box given by @arg{x},
  @arg{y}, @arg{width}, @arg{height}:
  @begin{pre}
 cairo_save (cr);
 cairo_translate (cr, x + width / 2., y + height / 2.);
 cairo_scale (cr, width / 2., height / 2.);
 cairo_arc (cr, 0., 0., 1., 0., 2 * M_PI);
 cairo_restore (cr);
  @end{pre}
  Since 1.0
  @see-symbol{cairo-t}
  @see-function{cairo-new-sub-path}
  @see-function{cairo-arc-negative}"
  (%cairo-arc cr
              (coerce xc 'double-float)
              (coerce yc 'double-float)
              (coerce radius 'double-float)
              (coerce angle1 'double-float)
              (coerce angle2 'double-float)))

(export 'cairo-arc)

;;; ----------------------------------------------------------------------------
;;; cairo_arc_negative ()
;;;
;;; void cairo_arc_negative (cairo_t *cr,
;;;                          double xc,
;;;                          double yc,
;;;                          double radius,
;;;                          double angle1,
;;;                          double angle2);
;;;
;;; Adds a circular arc of the given radius to the current path. The arc is
;;; centered at (xc, yc), begins at angle1 and proceeds in the direction of
;;; decreasing angles to end at angle2. If angle2 is greater than angle1 it will
;;; be progressively decreased by 2*M_PI until it is less than angle1.
;;;
;;; See cairo_arc() for more details. This function differs only in the
;;; direction of the arc between the two angles.
;;;
;;; cr :
;;;     a cairo context
;;;
;;; xc :
;;;     X position of the center of the arc
;;;
;;; yc :
;;;     Y position of the center of the arc
;;;
;;; radius :
;;;     the radius of the arc
;;;
;;; angle1 :
;;;     the start angle, in radians
;;;
;;; angle2 :
;;;     the end angle, in radians
;;;
;;; Since 1.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_curve_to ()
;;;
;;; void cairo_curve_to (cairo_t *cr,
;;;                      double x1,
;;;                      double y1,
;;;                      double x2,
;;;                      double y2,
;;;                      double x3,
;;;                      double y3);
;;;
;;; Adds a cubic Bézier spline to the path from the current point to position
;;; (x3, y3) in user-space coordinates, using (x1, y1) and (x2, y2) as the
;;; control points. After this call the current point will be (x3, y3).
;;;
;;; If there is no current point before the call to cairo_curve_to() this
;;; function will behave as if preceded by a call to cairo_move_to(cr, x1, y1).
;;;
;;; cr :
;;;     a cairo context
;;;
;;; x1 :
;;;     the X coordinate of the first control point
;;;
;;; y1 :
;;;     the Y coordinate of the first control point
;;;
;;; x2 :
;;;     the X coordinate of the second control point
;;;
;;; y2 :
;;;     the Y coordinate of the second control point
;;;
;;; x3 :
;;;     the X coordinate of the end of the curve
;;;
;;; y3 :
;;;     the Y coordinate of the end of the curve
;;;
;;; Since 1.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_line_to ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_line_to" %cairo-line-to) :void
  (cr (:pointer (:struct cairo-t)))
  (x :double)
  (y :double))

(defun cairo-line-to (cr x y)
 #+cl-cffi-gtk-documentation
 "@version{2013-10-13}
  @argument[cr]{a cairo context}
  @argument[x]{the x coordinate of the end of the new line}
  @argument[y]{the y coordinate of the end of the new line}
  @begin{short}
    Adds a line to the path from the current point to position
   (@arg{x}, @arg{y}) in user-space coordinates. After this call the current
   point will be (@arg{x}, @arg{y}).
  @end{short}

  If there is no current point before the call to @sym{cairo-line-to} this
  function will behave as @code{(cairo-move-to cr x y)}.

  Since 1.0
  @see-symbol{cairo-t}
  @see-function{cairo-move-to}"
  (%cairo-line-to cr (coerce x 'double-float) (coerce y 'double-float)))

(export 'cairo-line-to)

;;; ----------------------------------------------------------------------------
;;; cairo_move_to ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_move_to" %cairo-move-to) :void
  (cr (:pointer (:struct cairo-t)))
  (x :double)
  (y :double))

(defun cairo-move-to (cr x y)
 #+cl-cffi-gtk-documentation
 "@version{2013-10-13}
  @argument[cr]{a cairo context}
  @argument[x]{the x coordinate of the new position}
  @argument[y]{the y coordinate of the new position}
  @begin{short}
    Begin a new sub-path. After this call the current point will be
    (@arg{x}, @arg{y}).
  @end{short}

  Since 1.0
  @see-symbol{cairo-t}
  @see-function{cairo-line-to}"
  (%cairo-move-to cr (coerce x 'double-float) (coerce y 'double-float)))

(export 'cairo-move-to)

;;; ----------------------------------------------------------------------------
;;; cairo_rectangle ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_rectangle" %cairo-rectangle) :void
  (cr (:pointer (:struct cairo-t)))
  (x :double)
  (y :double)
  (width :double)
  (height :double))

(defun cairo-rectangle (cr x y width height)
 #+cl-cffi-gtk-documentation
 "@version{2013-10-13}
  @argument[cr]{a cairo context}
  @argument[x]{the x coordinate of the top left corner of the rectangle}
  @argument[y]{the y coordinate to the top left corner of the rectangle}
  @argument[width]{the width of the rectangle}
  @argument[height]{the height of the rectangle}
  @begin{short}
    Adds a closed sub-path rectangle of the given size to the current path at
    position (@arg{x}, @arg{y}) in user-space coordinates.
  @end{short}

  This function is logically equivalent to:
  @begin{pre}
    (cairo-move-to cr x y)
    (cairo-rel-line-to cr width 0)
    (cairo-rel-line-to cr 0 height)
    (cairo-rel-line-to cr -width 0)
    (cairo-close-path cr)
  @end{pre}
  Since 1.0
  @see-symbol{cairo-t}
  @see-function{cairo-move-to}
  @see-function{cairo-rel-line-to}
  @see-function{cairo-close-path}"
  (%cairo-rectangle cr
                    (coerce x 'double-float)
                    (coerce y 'double-float)
                    (coerce width 'double-float)
                    (coerce height 'double-float)))

(export 'cairo-rectangle)

;;; ----------------------------------------------------------------------------
;;; cairo_glyph_path ()
;;;
;;; void cairo_glyph_path (cairo_t *cr,
;;;                        const cairo_glyph_t *glyphs,
;;;                        int num_glyphs);
;;;
;;; Adds closed paths for the glyphs to the current path. The generated path if
;;; filled, achieves an effect similar to that of cairo_show_glyphs().
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
;;; cairo_text_path ()
;;;
;;; void cairo_text_path (cairo_t *cr, const char *utf8);
;;;
;;; Adds closed paths for text to the current path. The generated path if
;;; filled, achieves an effect similar to that of cairo_show_text().
;;;
;;; Text conversion and positioning is done similar to cairo_show_text().
;;;
;;; Like cairo_show_text(), After this call the current point is moved to the
;;; origin of where the next glyph would be placed in this same progression.
;;; That is, the current point will be at the origin of the final glyph offset
;;; by its advance values. This allows for chaining multiple calls to to
;;; cairo_text_path() without having to set current point in between.
;;;
;;; Note: The cairo_text_path() function call is part of what the cairo
;;; designers call the "toy" text API. It is convenient for short demos and
;;; simple programs, but it is not expected to be adequate for serious
;;; text-using applications. See cairo_glyph_path() for the "real" text path API
;;; in cairo.
;;;
;;; cr :
;;;     a cairo context
;;;
;;; utf8 :
;;;     a NUL-terminated string of text encoded in UTF-8, or NULL
;;;
;;; Since 1.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_rel_curve_to ()
;;;
;;; void cairo_rel_curve_to (cairo_t *cr,
;;;                          double dx1,
;;;                          double dy1,
;;;                          double dx2,
;;;                          double dy2,
;;;                          double dx3,
;;;                          double dy3);
;;;
;;; Relative-coordinate version of cairo_curve_to(). All offsets are relative to
;;; the current point. Adds a cubic Bézier spline to the path from the current
;;; point to a point offset from the current point by (dx3, dy3), using points
;;; offset by (dx1, dy1) and (dx2, dy2) as the control points. After this call
;;; the current point will be offset by (dx3, dy3).
;;;
;;; Given a current point of (x, y),
;;; cairo_rel_curve_to(cr, dx1, dy1, dx2, dy2, dx3, dy3) is logically equivalent
;;; to cairo_curve_to(cr, x+dx1, y+dy1, x+dx2, y+dy2, x+dx3, y+dy3).
;;;
;;; It is an error to call this function with no current point. Doing so will
;;; cause cr to shutdown with a status of CAIRO_STATUS_NO_CURRENT_POINT.
;;;
;;; cr :
;;;     a cairo context
;;;
;;; dx1 :
;;;     the X offset to the first control point
;;;
;;; dy1 :
;;;     the Y offset to the first control point
;;;
;;; dx2 :
;;;     the X offset to the second control point
;;;
;;; dy2 :
;;;     the Y offset to the second control point
;;;
;;; dx3 :
;;;     the X offset to the end of the curve
;;;
;;; dy3 :
;;;     the Y offset to the end of the curve
;;;
;;; Since 1.0
;;; ----------------------------------------------------------------------------

;;; void cairo_rel_curve_to (cairo_t *cr,
;;;                          double dx1,
;;;                          double dy1,
;;;                          double dx2,
;;;                          double dy2,
;;;                          double dx3,
;;;                          double dy3);

(defcfun ("cairo_rel_curve_to" %cairo-rel-curve-to) :void
  (cr (:pointer (:struct cairo-t)))
  (dx1 :double)
  (dy1 :double)
  (dx2 :double)
  (dy2 :double)
  (dx3 :double)
  (dy3 :double))

(defun cairo-rel-curve-to (cr dx1 dy1 dx2 dy2 dx3 dy3)
  (%cairo-rel-curve-to cr
                       (coerce dx1 'double-float)
                       (coerce dy1 'double-float)
                       (coerce dx2 'double-float)
                       (coerce dy2 'double-float)
                       (coerce dx3 'double-float)
                       (coerce dy3 'double-float)))

(export 'cairo-rel-curve-to)

;;; ----------------------------------------------------------------------------
;;; cairo_rel_line_to ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_rel_line_to" %cairo-rel-line-to) :void
  (cr (:pointer (:struct cairo-t)))
  (dx :double)
  (dy :double))

(defun cairo-rel-line-to (cr dx dy)
 #+cl-cffi-gtk-documentation
 "@version{2013-10-14}
  @argument[cr]{a cairo context}
  @argument[dx]{the x offset to the end of the new line}
  @argument[dy]{the y offset to the end of the new line}
  @begin{short}
    Relative-coordinate version of the function @fun{cairo-line-to}.
  @end{short}
  Adds a line to the path from the current point to a point that is offset from
  the current point by (@arg{dx}, @arg{dy}) in user space. After this call the
  current point will be offset by (@arg{dx}, @arg{dy}).

  Given a current point of (x, y), @code{(cairo-rel-line-to cr dx dy)} is
  logically equivalent to @code{(cairo-line-to cr (+ x dx) (+ y dy))}.

  It is an error to call this function with no current point. Doing so will
  cause @arg{cr} to shutdown with a status of @code{:no-current-point}.

  Since 1.0
  @see-symbol{cairo-t}
  @see-function{cairo-line-to}"
  (%cairo-rel-line-to cr (coerce dx 'double-float) (coerce dy 'double-float)))

(export 'cairo-rel-line-to)

;;; ----------------------------------------------------------------------------
;;; cairo_rel_move_to ()
;;;
;;; void cairo_rel_move_to (cairo_t *cr, double dx, double dy);
;;;
;;; Begin a new sub-path. After this call the current point will offset by
;;; (x, y).
;;;
;;; Given a current point of (x, y), cairo_rel_move_to(cr, dx, dy) is logically
;;; equivalent to cairo_move_to(cr, x + dx, y + dy).
;;;
;;; It is an error to call this function with no current point. Doing so will
;;; cause cr to shutdown with a status of CAIRO_STATUS_NO_CURRENT_POINT.
;;;
;;; cr :
;;;     a cairo context
;;;
;;; dx :
;;;     the X offset
;;;
;;; dy :
;;;     the Y offset
;;;
;;; Since 1.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_path_extents ()
;;;
;;; void cairo_path_extents (cairo_t *cr,
;;;                          double *x1,
;;;                          double *y1,
;;;                          double *x2,
;;;                          double *y2);
;;;
;;; Computes a bounding box in user-space coordinates covering the points on the
;;; current path. If the current path is empty, returns an empty rectangle
;;; ((0,0), (0,0)). Stroke parameters, fill rule, surface dimensions and
;;; clipping are not taken into account.
;;;
;;; Contrast with cairo_fill_extents() and cairo_stroke_extents() which return
;;; the extents of only the area that would be "inked" by the corresponding
;;; drawing operations.
;;;
;;; The result of cairo_path_extents() is defined as equivalent to the limit of
;;; cairo_stroke_extents() with CAIRO_LINE_CAP_ROUND as the line width
;;; approaches 0.0, (but never reaching the empty-rectangle returned by
;;; cairo_stroke_extents() for a line width of 0.0).
;;;
;;; Specifically, this means that zero-area sub-paths such as cairo_move_to();
;;; cairo_line_to() segments, (even degenerate cases where the coordinates to
;;; both calls are identical), will be considered as contributing to the
;;; extents. However, a lone cairo_move_to() will not contribute to the results
;;; of cairo_path_extents().
;;;
;;; cr :
;;;     a cairo context
;;;
;;; x1 :
;;;     left of the resulting extents
;;;
;;; y1 :
;;;     top of the resulting extents
;;;
;;; x2 :
;;;     right of the resulting extents
;;;
;;; y2 :
;;;     bottom of the resulting extents
;;;
;;; Since 1.6
;;; ----------------------------------------------------------------------------

;;; --- End of file cairo.paths.lisp -------------------------------------------
