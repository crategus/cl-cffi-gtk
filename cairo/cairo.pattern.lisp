;;; ----------------------------------------------------------------------------
;;; cairo.pattern.lisp
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
;;; cairo_pattern_t
;;;
;;; Sources for drawing
;;;
;;; Synopsis
;;;
;;;     cairo_pattern_t
;;;
;;;     cairo_pattern_add_color_stop_rgb
;;;     cairo_pattern_add_color_stop_rgba
;;;     cairo_pattern_get_color_stop_count
;;;     cairo_pattern_get_color_stop_rgba
;;;     cairo_pattern_create_rgb
;;;     cairo_pattern_create_rgba
;;;     cairo_pattern_get_rgba
;;;     cairo_pattern_create_for_surface
;;;     cairo_pattern_get_surface
;;;     cairo_pattern_create_linear
;;;     cairo_pattern_get_linear_points
;;;     cairo_pattern_create_radial
;;;     cairo_pattern_get_radial_circles
;;;     cairo_pattern_create_mesh
;;;     cairo_mesh_pattern_begin_patch
;;;     cairo_mesh_pattern_end_patch
;;;     cairo_mesh_pattern_move_to
;;;     cairo_mesh_pattern_line_to
;;;     cairo_mesh_pattern_curve_to
;;;     cairo_mesh_pattern_set_control_point
;;;     cairo_mesh_pattern_set_corner_color_rgb
;;;     cairo_mesh_pattern_set_corner_color_rgba
;;;     cairo_mesh_pattern_get_patch_count
;;;     cairo_mesh_pattern_get_path
;;;     cairo_mesh_pattern_get_control_point
;;;     cairo_mesh_pattern_get_corner_color_rgba
;;;     cairo_pattern_reference
;;;     cairo_pattern_destroy
;;;     cairo_pattern_status
;;;
;;;     cairo_extend_t
;;;
;;;     cairo_pattern_set_extend
;;;     cairo_pattern_get_extend
;;;
;;;     cairo_filter_t
;;;
;;;     cairo_pattern_set_filter
;;;     cairo_pattern_get_filter
;;;     cairo_pattern_set_matrix
;;;     cairo_pattern_get_matrix
;;;
;;;     cairo_pattern_type_t
;;;
;;;     cairo_pattern_get_type
;;;     cairo_pattern_get_reference_count
;;;     cairo_pattern_set_user_data
;;;     cairo_pattern_get_user_data
;;;
;;; Description
;;;
;;; cairo_pattern_t is the paint with which cairo draws. The primary use of
;;; patterns is as the source for all cairo drawing operations, although they
;;; can also be used as masks, that is, as the brush too.
;;;
;;; A cairo pattern is created by using one of the many constructors, of the
;;; form cairo_pattern_create_type() or implicitly through
;;; cairo_set_source_type() functions.
;;; ----------------------------------------------------------------------------

(in-package :cairo)

;;; ----------------------------------------------------------------------------
;;; cairo_pattern_t
;;; ----------------------------------------------------------------------------

(defcstruct cairo-pattern-t)

#+cl-cffi-gtk-documentation
(setf (gethash 'cairo-pattern-t atdoc:*symbol-name-alias*) "CStruct"
      (gethash 'cairo-pattern-t atdoc:*external-symbols*)
 "@version{2013-10-13}
  @begin{short}
    A @sym{cairo-pattern-t} represents a source when drawing onto a surface.
    There are different subtypes of @sym{cairo-pattern-t}, for different types
    of sources; for example, @fun{cairo-pattern-create-rgb} creates a pattern
    for a solid opaque color.
  @end{short}

  Other than various @code{cairo-pattern-create-type} functions, some of the
  pattern types can be implicitly created using various
  @code{cairo-set-source-type} functions; for example the function
  @fun{cairo-set-source-rgb}.

  The type of a pattern can be queried with @code{cairo-pattern-get-type}.

  Memory management of @sym{cairo-pattern-t} is done with the functions
  @fun{cairo-pattern-reference} and @fun{cairo-pattern-destroy}.

  Since 1.0
  @see-function{cairo-pattern-create-rgb}
  @see-function{cairo-set-source-rgb}
  @see-function{cairo-pattern-reference}
  @see-function{cairo-pattern-destroy}")

(export 'cairo-pattern-t)

;;; ----------------------------------------------------------------------------
;;; cairo_pattern_add_color_stop_rgb ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_pattern_add_color_stop_rgb" %cairo-pattern-add-color-stop-rgb)
    :void
  (pattern (:pointer (:struct cairo-pattern-t)))
  (offset :double)
  (red :double)
  (green :double)
  (blue :double))

(defun cairo-pattern-add-color-stop-rgb (pattern offset red green blue)
 #+cl-cffi-gtk-documentation
 "@version{2013-10-14}
  @argument[pattern]{a @symbol{cairo-pattern-t}}
  @argument[offset]{an offset in the range [0.0 .. 1.0]}
  @argument[red]{red component of color}
  @argument[green]{green component of color}
  @argument[blue]{blue component of color}
  @begin{short}
    Adds an opaque color stop to a gradient pattern.
  @end{short}
  The offset specifies the location along the gradient's control vector. For
  example, a linear gradient's control vector is from (x0,y0) to (x1,y1) while
  a radial gradient's control vector is from any point on the start circle to
  the corresponding point on the end circle.

  The color is specified in the same way as in the function
  @fun{cairo-set-source-rgb}.

  If two (or more) stops are specified with identical offset values, they will
  be sorted according to the order in which the stops are added, (stops added
  earlier will compare less than stops added later). This can be useful for
  reliably making sharp color transitions instead of the typical blend.

  Note: If the pattern is not a gradient pattern, (e. g. a linear or radial
  pattern), then the pattern will be put into an error status with a status of
  @code{:pattern-type-mismatch}.

  Since 1.0"
  (%cairo-pattern-add-color-stop-rgb pattern
                                     (coerce offset 'double-float)
                                     (coerce red 'double-float)
                                     (coerce green 'double-float)
                                     (coerce blue 'double-float)))

(export 'cairo-pattern-add-color-stop-rgb)

;;; ----------------------------------------------------------------------------
;;; cairo_pattern_add_color_stop_rgba ()
;;;
;;; void cairo_pattern_add_color_stop_rgba (cairo_pattern_t *pattern,
;;;                                         double offset,
;;;                                         double red,
;;;                                         double green,
;;;                                         double blue,
;;;                                         double alpha);
;;;
;;; Adds a translucent color stop to a gradient pattern. The offset specifies
;;; the location along the gradient's control vector. For example, a linear
;;; gradient's control vector is from (x0,y0) to (x1,y1) while a radial
;;; gradient's control vector is from any point on the start circle to the
;;; corresponding point on the end circle.
;;;
;;; The color is specified in the same way as in cairo_set_source_rgba().
;;;
;;; If two (or more) stops are specified with identical offset values, they will
;;; be sorted according to the order in which the stops are added, (stops added
;;; earlier will compare less than stops added later). This can be useful for
;;; reliably making sharp color transitions instead of the typical blend.
;;;
;;; Note: If the pattern is not a gradient pattern, (eg. a linear or radial
;;; pattern), then the pattern will be put into an error status with a status of
;;; CAIRO_STATUS_PATTERN_TYPE_MISMATCH.
;;;
;;; pattern :
;;;     a cairo_pattern_t
;;;
;;; offset :
;;;     an offset in the range [0.0 .. 1.0]
;;;
;;; red :
;;;     red component of color
;;;
;;; green :
;;;     green component of color
;;;
;;; blue :
;;;     blue component of color
;;;
;;; alpha :
;;;     alpha component of color
;;;
;;; Since 1.0
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_pattern_add_color_stop_rgba"
          %cairo-pattern-add-color-stop-rgba) :void
  (pattern (:pointer (:struct cairo-pattern-t)))
  (offset :double)
  (red :double)
  (green :double)
  (blue :double)
  (alpha :double))

(defun cairo-pattern-add-color-stop-rgba (pattern offset red green blue alpha)
  (%cairo-pattern-add-color-stop-rgba pattern
                                      (coerce offset 'double-float)
                                      (coerce red 'double-float)
                                      (coerce green 'double-float)
                                      (coerce blue 'double-float)
                                      (coerce alpha 'double-float)))

(export 'cairo-pattern-add-color-stop-rgba)

;;; ----------------------------------------------------------------------------
;;; cairo_pattern_get_color_stop_count ()
;;;
;;; cairo_status_t cairo_pattern_get_color_stop_count (cairo_pattern_t *pattern,
;;;                                                    int *count);
;;;
;;; Gets the number of color stops specified in the given gradient pattern.
;;;
;;; pattern :
;;;     a cairo_pattern_t
;;;
;;; count :
;;;     return value for the number of color stops, or NULL
;;;
;;; Returns :
;;;     CAIRO_STATUS_SUCCESS, or CAIRO_STATUS_PATTERN_TYPE_MISMATCH if pattern
;;;     is not a gradient pattern.
;;;
;;; Since 1.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_pattern_get_color_stop_rgba ()
;;;
;;; cairo_status_t cairo_pattern_get_color_stop_rgba (cairo_pattern_t *pattern,
;;;                                                   int index,
;;;                                                   double *offset,
;;;                                                   double *red,
;;;                                                   double *green,
;;;                                                   double *blue,
;;;                                                   double *alpha);
;;;
;;; Gets the color and offset information at the given index for a gradient
;;; pattern. Values of index are 0 to 1 less than the number returned by
;;; cairo_pattern_get_color_stop_count().
;;;
;;; pattern :
;;;     a cairo_pattern_t
;;;
;;; index :
;;;     index of the stop to return data for
;;;
;;; offset :
;;;     return value for the offset of the stop, or NULL
;;;
;;; red :
;;;     return value for red component of color, or NULL
;;;
;;; green :
;;;     return value for green component of color, or NULL
;;;
;;; blue :
;;;     return value for blue component of color, or NULL
;;;
;;; alpha :
;;;     return value for alpha component of color, or NULL
;;;
;;; Returns :
;;;     CAIRO_STATUS_SUCCESS, or CAIRO_STATUS_INVALID_INDEX if index is not
;;;     valid for the given pattern. If the pattern is not a gradient pattern,
;;;     CAIRO_STATUS_PATTERN_TYPE_MISMATCH is returned.
;;;
;;; Since 1.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_pattern_create_rgb ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_pattern_create_rgb" cairo-pattern-create-rgb)
    (:pointer (:struct cairo-pattern-t))
 #+cl-cffi-gtk-documentation
 "@version{2013-11-20}
  @argument[red]{red component of the color}
  @argument[green]{green component of the color}
  @argument[blue]{blue component of the color}
  @begin{return}
    The newly created @sym{cairo-pattern-t} if successful, or an error pattern
    in case of no memory. The caller owns the returned object and should call
    the function @fun{cairo-pattern-destroy} when finished with it. This
    function will always return a valid pointer, but if an error occurred the
    pattern status will be set to an error. To inspect the status of a pattern
    use the function @fun{cairo-pattern-status}.
  @end{return}
  @begin{short}
    Creates a new @sym{cairo-pattern-t} corresponding to an opaque color.
  @end{short}
  The color components are floating point numbers in the range 0 to 1. If the
  values passed in are outside that range, they will be clamped.

  Since 1.0
  @see-symbol{cairo-pattern-t}
  @see-function{cairo-pattern-destroy}
  @see-fun{cairo-pattern-status}"
  (red :double)
  (green :double)
  (blue :double))

(export 'cairo-pattern-create-rgb)

;;; ----------------------------------------------------------------------------
;;; cairo_pattern_create_rgba ()
;;;
;;; cairo_pattern_t * cairo_pattern_create_rgba (double red,
;;;                                              double green,
;;;                                              double blue,
;;;                                              double alpha);
;;;
;;; Creates a new cairo_pattern_t corresponding to a translucent color. The
;;; color components are floating point numbers in the range 0 to 1. If the
;;; values passed in are outside that range, they will be clamped.
;;;
;;; red :
;;;     red component of the color
;;;
;;; green :
;;;     green component of the color
;;;
;;; blue :
;;;     blue component of the color
;;;
;;; alpha :
;;;     alpha component of the color
;;;
;;; Returns :
;;;     the newly created cairo_pattern_t if successful, or an error pattern in
;;;     case of no memory. The caller owns the returned object and should call
;;;     cairo_pattern_destroy() when finished with it. This function will always
;;;     return a valid pointer, but if an error occurred the pattern status will
;;;     be set to an error. To inspect the status of a pattern use
;;;     cairo_pattern_status().
;;;
;;; Since 1.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_pattern_get_rgba ()
;;;
;;; cairo_status_t cairo_pattern_get_rgba (cairo_pattern_t *pattern,
;;;                                                          double *red,
;;;                                                          double *green,
;;;                                                          double *blue,
;;;                                                          double *alpha);
;;;
;;; Gets the solid color for a solid color pattern.
;;;
;;; pattern :
;;;     a cairo_pattern_t
;;;
;;; red :
;;;     return value for red component of color, or NULL
;;;
;;; green :
;;;     return value for green component of color, or NULL
;;;
;;; blue :
;;;     return value for blue component of color, or NULL
;;;
;;; alpha :
;;;     return value for alpha component of color, or NULL
;;;
;;; Returns :
;;;     CAIRO_STATUS_SUCCESS, or CAIRO_STATUS_PATTERN_TYPE_MISMATCH if the
;;;     pattern is not a solid color pattern.
;;;
;;; Since 1.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_pattern_create_for_surface ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_pattern_create_for_surface" cairo-pattern-create-for-surface)
    (:pointer (:struct cairo-pattern-t))
 #+cl-cffi-gtk-documentation
 "@version{2013-11-24}
  @argument[surface]{the surface}
  @begin{return}
    The newly created @symbol{cairo-pattern-t} if successful, or an error
    pattern in case of no memory. The caller owns the returned object and should
    call the function @fun{cairo-pattern-destroy} when finished with it. This
    function will always return a valid pointer, but if an error occurred the
    pattern status will be set to an error. To inspect the status of a pattern
    use the function @fun{cairo-pattern-status}.
  @end{return}
  @begin{short}
    Create a new @symbol{cairo-pattern-t} for the given surface.
  @end{short}

  Since 1.0
  @see-symbol{cairo-pattern-t}
  @see-symbol{cairo-surface-t}
  @see-function{cairo-pattern-destroy}
  @see-function{cairo-pattern-status}"
  (surface (:pointer (:struct cairo-surface-t))))

(export 'cairo-pattern-create-for-surface)

;;; ----------------------------------------------------------------------------
;;; cairo_pattern_get_surface ()
;;;
;;; cairo_status_t cairo_pattern_get_surface (cairo_pattern_t *pattern,
;;;                                           cairo_surface_t **surface);
;;;
;;; Gets the surface of a surface pattern. The reference returned in surface is
;;; owned by the pattern; the caller should call cairo_surface_reference() if
;;; the surface is to be retained.
;;;
;;; pattern :
;;;     a cairo_pattern_t
;;;
;;; surface :
;;;     return value for surface of pattern, or NULL
;;;
;;; Returns :
;;;     CAIRO_STATUS_SUCCESS, or CAIRO_STATUS_PATTERN_TYPE_MISMATCH if the
;;;     pattern is not a surface pattern.
;;;
;;; Since 1.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_pattern_create_linear ()
;;;
;;; cairo_pattern_t * cairo_pattern_create_linear (double x0,
;;;                                                double y0,
;;;                                                double x1,
;;;                                                double y1);
;;;
;;; Create a new linear gradient cairo_pattern_t along the line defined by
;;; (x0, y0) and (x1, y1). Before using the gradient pattern, a number of color
;;; stops should be defined using cairo_pattern_add_color_stop_rgb() or
;;; cairo_pattern_add_color_stop_rgba().
;;;
;;; Note: The coordinates here are in pattern space. For a new pattern, pattern
;;; space is identical to user space, but the relationship between the spaces
;;; can be changed with cairo_pattern_set_matrix().
;;;
;;; x0 :
;;;     x coordinate of the start point
;;;
;;; y0 :
;;;     y coordinate of the start point
;;;
;;; x1 :
;;;     x coordinate of the end point
;;;
;;; y1 :
;;;     y coordinate of the end point
;;;
;;; Returns :
;;;     the newly created cairo_pattern_t if successful, or an error pattern in
;;;     case of no memory. The caller owns the returned object and should call
;;;     cairo_pattern_destroy() when finished with it. This function will always
;;;     return a valid pointer, but if an error occurred the pattern status will
;;;     be set to an error. To inspect the status of a pattern use
;;;     cairo_pattern_status().
;;;
;;; Since 1.0
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_pattern_create_linear" %cairo-pattern-create-linear)
    (:pointer (:struct cairo-pattern-t))
  (x0 :double)
  (y0 :double)
  (x1 :double)
  (y1 :double))

(defun cairo-pattern-create-linear (x0 y0 x1 y1)
  (%cairo-pattern-create-linear (coerce x0 'double-float)
                                (coerce y0 'double-float)
                                (coerce x1 'double-float)
                                (coerce y1 'double-float)))

(export 'cairo-pattern-create-linear)

;;; ----------------------------------------------------------------------------
;;; cairo_pattern_get_linear_points ()
;;;
;;; cairo_status_t cairo_pattern_get_linear_points (cairo_pattern_t *pattern,
;;;                                                 double *x0,
;;;                                                 double *y0,
;;;                                                 double *x1,
;;;                                                 double *y1);
;;;
;;; Gets the gradient endpoints for a linear gradient.
;;;
;;; pattern :
;;;     a cairo_pattern_t
;;;
;;; x0 :
;;;     return value for the x coordinate of the first point, or NULL
;;;
;;; y0 :
;;;     return value for the y coordinate of the first point, or NULL
;;;
;;; x1 :
;;;     return value for the x coordinate of the second point, or NULL
;;;
;;; y1 :
;;;     return value for the y coordinate of the second point, or NULL
;;;
;;; Returns :
;;;     CAIRO_STATUS_SUCCESS, or CAIRO_STATUS_PATTERN_TYPE_MISMATCH if pattern
;;;     is not a linear gradient pattern.
;;;
;;; Since 1.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_pattern_create_radial ()
;;;
;;; cairo_pattern_t * cairo_pattern_create_radial (double cx0,
;;;                                                double cy0,
;;;                                                double radius0,
;;;                                                double cx1,
;;;                                                double cy1,
;;;                                                double radius1);
;;;
;;; Creates a new radial gradient cairo_pattern_t between the two circles
;;; defined by (cx0, cy0, radius0) and (cx1, cy1, radius1). Before using the
;;; gradient pattern, a number of color stops should be defined using
;;; cairo_pattern_add_color_stop_rgb() or cairo_pattern_add_color_stop_rgba().
;;;
;;; Note: The coordinates here are in pattern space. For a new pattern, pattern
;;; space is identical to user space, but the relationship between the spaces
;;; can be changed with cairo_pattern_set_matrix().
;;;
;;; cx0 :
;;;     x coordinate for the center of the start circle
;;;
;;; cy0 :
;;;     y coordinate for the center of the start circle
;;;
;;; radius0 :
;;;     radius of the start circle
;;;
;;; cx1 :
;;;     x coordinate for the center of the end circle
;;;
;;; cy1 :
;;;     y coordinate for the center of the end circle
;;;
;;; radius1 :
;;;     radius of the end circle
;;;
;;; Returns :
;;;     the newly created cairo_pattern_t if successful, or an error pattern in
;;;     case of no memory. The caller owns the returned object and should call
;;;     cairo_pattern_destroy() when finished with it. This function will always
;;;     return a valid pointer, but if an error occurred the pattern status will
;;;     be set to an error. To inspect the status of a pattern use
;;;     cairo_pattern_status().
;;;
;;; Since 1.0
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_pattern_create_radial" %cairo-pattern-create-radial)
    (:pointer (:struct cairo-pattern-t))
  (cx0 :double)
  (cy0 :double)
  (radius0 :double)
  (cx1 :double)
  (cy1 :double)
  (radius1 :double))

(defun cairo-pattern-create-radial (cx0 cy0 radius0 cx1 cy1 radius1)
  (%cairo-pattern-create-radial (coerce cx0 'double-float)
                                (coerce cy0 'double-float)
                                (coerce radius0 'double-float)
                                (coerce cx1 'double-float)
                                (coerce cy1 'double-float)
                                (coerce radius1 'double-float)))

(export 'cairo-pattern-create-radial)

;;; ----------------------------------------------------------------------------
;;; cairo_pattern_get_radial_circles ()
;;;
;;; cairo_status_t cairo_pattern_get_radial_circles (cairo_pattern_t *pattern,
;;;                                                  double *x0,
;;;                                                  double *y0,
;;;                                                  double *r0,
;;;                                                  double *x1,
;;;                                                  double *y1,
;;;                                                  double *r1);
;;;
;;; Gets the gradient endpoint circles for a radial gradient, each specified as
;;; a center coordinate and a radius.
;;;
;;; pattern :
;;;     a cairo_pattern_t
;;;
;;; x0 :
;;;     return value for the x coordinate of the center of the first circle,
;;;     or NULL
;;;
;;; y0 :
;;;     return value for the y coordinate of the center of the first circle,
;;;     or NULL
;;;
;;; r0 :
;;;     return value for the radius of the first circle, or NULL
;;;
;;; x1 :
;;;     return value for the x coordinate of the center of the second circle,
;;;     or NULL
;;;
;;; y1 :
;;;     return value for the y coordinate of the center of the second circle,
;;;     or NULL
;;;
;;; r1 :
;;;     return value for the radius of the second circle, or NULL
;;;
;;; Returns :
;;;     CAIRO_STATUS_SUCCESS, or CAIRO_STATUS_PATTERN_TYPE_MISMATCH if pattern
;;;     is not a radial gradient pattern.
;;;
;;; Since 1.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_pattern_create_mesh ()
;;;
;;; cairo_pattern_t * cairo_pattern_create_mesh (void);
;;;
;;; Create a new mesh pattern.
;;;
;;; Mesh patterns are tensor-product patch meshes (type 7 shadings in PDF). Mesh
;;; patterns may also be used to create other types of shadings that are special
;;; cases of tensor-product patch meshes such as Coons patch meshes (type 6
;;; shading in PDF) and Gouraud-shaded triangle meshes (type 4 and 5 shadings in
;;; PDF).
;;;
;;; Mesh patterns consist of one or more tensor-product patches, which should be
;;; defined before using the mesh pattern. Using a mesh pattern with a partially
;;; defined patch as source or mask will put the context in an error status with
;;; a status of CAIRO_STATUS_INVALID_MESH_CONSTRUCTION.
;;;
;;; A tensor-product patch is defined by 4 Bézier curves (side 0, 1, 2, 3) and
;;; by 4 additional control points (P0, P1, P2, P3) that provide further control
;;; over the patch and complete the definition of the tensor-product patch. The
;;; corner C0 is the first point of the patch.
;;;
;;; Degenerate sides are permitted so straight lines may be used. A zero length
;;; line on one side may be used to create 3 sided patches.
;;;
;;;       C1     Side 1       C2
;;;        +---------------+
;;;        |               |
;;;        |  P1       P2  |
;;;        |               |
;;; Side 0 |               | Side 2
;;;        |               |
;;;        |               |
;;;        |  P0       P3  |
;;;        |               |
;;;        +---------------+
;;;      C0     Side 3        C3
;;;
;;; Each patch is constructed by first calling cairo_mesh_pattern_begin_patch(),
;;; then cairo_mesh_pattern_move_to() to specify the first point in the patch
;;; (C0). Then the sides are specified with calls to
;;; cairo_mesh_pattern_curve_to() and cairo_mesh_pattern_line_to().
;;;
;;; The four additional control points (P0, P1, P2, P3) in a patch can be
;;; specified with cairo_mesh_pattern_set_control_point().
;;;
;;; At each corner of the patch (C0, C1, C2, C3) a color may be specified with
;;; cairo_mesh_pattern_set_corner_color_rgb() or
;;; cairo_mesh_pattern_set_corner_color_rgba(). Any corner whose color is not
;;; explicitly specified defaults to transparent black.
;;;
;;; A Coons patch is a special case of the tensor-product patch where the
;;; control points are implicitly defined by the sides of the patch. The default
;;; value for any control point not specified is the implicit value for a Coons
;;; patch, i. e. if no control points are specified the patch is a Coons patch.
;;;
;;; A triangle is a special case of the tensor-product patch where the control
;;; points are implicitly defined by the sides of the patch, all the sides are
;;; lines and one of them has length 0, i.e. if the patch is specified using
;;; just 3 lines, it is a triangle. If the corners connected by the 0-length
;;; side have the same color, the patch is a Gouraud-shaded triangle.
;;;
;;; Patches may be oriented differently to the above diagram. For example the
;;; first point could be at the top left. The diagram only shows the
;;; relationship between the sides, corners and control points. Regardless of
;;; where the first point is located, when specifying colors, corner 0 will
;;; always be the first point, corner 1 the point between side 0 and side 1 etc.
;;;
;;; Calling cairo_mesh_pattern_end_patch() completes the current patch. If less
;;; than 4 sides have been defined, the first missing side is defined as a line
;;; from the current point to the first point of the patch (C0) and the other
;;; sides are degenerate lines from C0 to C0. The corners between the added
;;; sides will all be coincident with C0 of the patch and their color will be
;;; set to be the same as the color of C0.
;;;
;;; Additional patches may be added with additional calls to
;;; cairo_mesh_pattern_begin_patch()/cairo_mesh_pattern_end_patch().
;;;
;;; cairo_pattern_t *pattern = cairo_pattern_create_mesh ();
;;;
;;; /* Add a Coons patch */
;;; cairo_mesh_pattern_begin_patch (pattern);
;;; cairo_mesh_pattern_move_to (pattern, 0, 0);
;;; cairo_mesh_pattern_curve_to (pattern, 30, -30,  60,  30, 100, 0);
;;; cairo_mesh_pattern_curve_to (pattern, 60,  30, 130,  60, 100, 100);
;;; cairo_mesh_pattern_curve_to (pattern, 60,  70,  30, 130,   0, 100);
;;; cairo_mesh_pattern_curve_to (pattern, 30,  70, -30,  30,   0, 0);
;;; cairo_mesh_pattern_set_corner_color_rgb (pattern, 0, 1, 0, 0);
;;; cairo_mesh_pattern_set_corner_color_rgb (pattern, 1, 0, 1, 0);
;;; cairo_mesh_pattern_set_corner_color_rgb (pattern, 2, 0, 0, 1);
;;; cairo_mesh_pattern_set_corner_color_rgb (pattern, 3, 1, 1, 0);
;;; cairo_mesh_pattern_end_patch (pattern);
;;;
;;; /* Add a Gouraud-shaded triangle */
;;; cairo_mesh_pattern_begin_patch (pattern)
;;; cairo_mesh_pattern_move_to (pattern, 100, 100);
;;; cairo_mesh_pattern_line_to (pattern, 130, 130);
;;; cairo_mesh_pattern_line_to (pattern, 130,  70);
;;; cairo_mesh_pattern_set_corner_color_rgb (pattern, 0, 1, 0, 0);
;;; cairo_mesh_pattern_set_corner_color_rgb (pattern, 1, 0, 1, 0);
;;; cairo_mesh_pattern_set_corner_color_rgb (pattern, 2, 0, 0, 1);
;;; cairo_mesh_pattern_end_patch (pattern)
;;;
;;; When two patches overlap, the last one that has been added is drawn over
;;; the first one.
;;;
;;; When a patch folds over itself, points are sorted depending on their
;;; parameter coordinates inside the patch. The v coordinate ranges from 0 to 1
;;; when moving from side 3 to side 1; the u coordinate ranges from 0 to 1 when
;;; going from side 0 to side 2. Points with higher v coordinate hide points
;;; with lower v coordinate. When two points have the same v coordinate, the one
;;; with higher u coordinate is above. This means that points nearer to side 1
;;; are above points nearer to side 3; when this is not sufficient to decide
;;; which point is above (for example when both points belong to side 1 or side
;;; 3) points nearer to side 2 are above points nearer to side 0.
;;;
;;; For a complete definition of tensor-product patches, see the PDF
;;; specification (ISO32000), which describes the parametrization in detail.
;;;
;;; Note: The coordinates are always in pattern space. For a new pattern,
;;; pattern space is identical to user space, but the relationship between the
;;; spaces can be changed with cairo_pattern_set_matrix().
;;;
;;; Returns :
;;;     the newly created cairo_pattern_t if successful, or an error pattern in
;;;     case of no memory. The caller owns the returned object and should call
;;;     cairo_pattern_destroy() when finished with it. This function will always
;;;     return a valid pointer, but if an error occurred the pattern status will
;;;     be set to an error. To inspect the status of a pattern use
;;;     cairo_pattern_status().
;;;
;;; Since 1.12
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_mesh_pattern_begin_patch ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_mesh_pattern_begin_patch" cairo-mesh-pattern-begin-patch) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-11-18}
  @argument[pattern]{a @symbol{cairo-pattern-t}}
  @begin{short}
    Begin a patch in a mesh pattern.
  @end{short}

  After calling this function, the patch shape should be defined with the
  functions @fun{cairo-mesh-pattern-move-to}, @fun{cairo-mesh-pattern-line-to}
  and @fun{cairo-mesh-pattern-curve-to}.

  After defining the patch, @fun{cairo-mesh-pattern-end-patch} must be called
  before using pattern as a source or mask.

  @subheadin{Note}
  If pattern is not a mesh pattern then pattern will be put into an error status
  with a status of @code{:pattern-type-mismatch}. If pattern already has a
  current patch, it will be put into an error status with a status of
  @code{:invalid-mesh-contstruction}.

  Since 1.12
  @see-symbol{cairo-pattern-t}
  @see-symbol{cairo-status-t}
  @see-function{cairo-mesh-pattern-move-to}
  @see-function{cairo-mesh-pattern-line-to}
  @see-function{cairo-mesh-pattern-curve-to}"
  (pattern (:pointer (:struct cairo-pattern-t))))

(export 'cairo-mesh-patern-begin-patch)

;;; ----------------------------------------------------------------------------
;;; cairo_mesh_pattern_end_patch ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_mesh_pattern_end_patch" cairo-mesh-pattern-end-patch) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-11-18}
  @argument[pattern]{a @symbol{cairo-pattern-t}}
  @begin{short}
    Indicates the end of the current patch in a mesh pattern.
  @end{short}

  If the current patch has less than 4 sides, it is closed with a straight
  line from the current point to the first point of the patch as if the function
  @fun{cairo-mesh-pattern-line-to} was used.

  @subheading{Note}
  If pattern is not a mesh pattern then pattern will be put into an error status
  with a status of @code{:pattern-type-mismatch}. If pattern has no current
  patch or the current patch has no current point, pattern will be put into an
  error status with a status of @code{:invalid-mesh-construction}.

  Since 1.12
  @see-symbol{cairo-pattern-t}
  @see-symbol{cairo-status-t}
  @see-function{cairo-mesh-pattern-line-to}"
  (patten cairo-pattern-t))

(export 'cairo-mesh-pattern-end-patch)

;;; ----------------------------------------------------------------------------
;;; cairo_mesh_pattern_move_to ()
;;;
;;; void cairo_mesh_pattern_move_to (cairo_pattern_t *pattern,
;;;                                  double x,
;;;                                  double y);
;;;
;;; Define the first point of the current patch in a mesh pattern.
;;;
;;; After this call the current point will be (x, y).
;;;
;;; Note: If pattern is not a mesh pattern then pattern will be put into an
;;; error status with a status of CAIRO_STATUS_PATTERN_TYPE_MISMATCH. If pattern
;;; has no current patch or the current patch already has at least one side,
;;; pattern will be put into an error status with a status of
;;; CAIRO_STATUS_INVALID_MESH_CONSTRUCTION.
;;;
;;; pattern :
;;;     a cairo_pattern_t
;;;
;;; x :
;;;     the X coordinate of the new position
;;;
;;; y :
;;;     the Y coordinate of the new position
;;;
;;; Since 1.12
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_mesh_pattern_line_to ()
;;;
;;; void cairo_mesh_pattern_line_to (cairo_pattern_t *pattern,
;;;                                  double x,
;;;                                  double y);
;;;
;;; Adds a line to the current patch from the current point to position (x, y)
;;; in pattern-space coordinates.
;;;
;;; If there is no current point before the call to cairo_mesh_pattern_line_to()
;;; this function will behave as cairo_mesh_pattern_move_to(pattern, x, y).
;;;
;;; After this call the current point will be (x, y).
;;;
;;; Note: If pattern is not a mesh pattern then pattern will be put into an
;;; error status with a status of CAIRO_STATUS_PATTERN_TYPE_MISMATCH. If pattern
;;; has no current patch or the current patch already has 4 sides, pattern will
;;; be put into an error status with a status of
;;; CAIRO_STATUS_INVALID_MESH_CONSTRUCTION.
;;;
;;; pattern :
;;;     a cairo_pattern_t
;;;
;;; x :
;;;     the X coordinate of the end of the new line
;;;
;;; y :
;;;     the Y coordinate of the end of the new line
;;;
;;; Since 1.12
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_mesh_pattern_curve_to ()
;;;
;;; void cairo_mesh_pattern_curve_to (cairo_pattern_t *pattern,
;;;                                   double x1,
;;;                                   double y1,
;;;                                   double x2,
;;;                                   double y2,
;;;                                   double x3,
;;;                                   double y3);
;;;
;;; Adds a cubic Bézier spline to the current patch from the current point to
;;; position (x3, y3) in pattern-space coordinates, using (x1, y1) and (x2, y2)
;;; as the control points.
;;;
;;; If the current patch has no current point before the call to
;;; cairo_mesh_pattern_curve_to(), this function will behave as if preceded by a
;;; call to cairo_mesh_pattern_move_to(pattern, x1, y1).
;;;
;;; After this call the current point will be (x3, y3).
;;;
;;; Note: If pattern is not a mesh pattern then pattern will be put into an
;;; error status with a status of CAIRO_STATUS_PATTERN_TYPE_MISMATCH. If pattern
;;; has no current patch or the current patch already has 4 sides, pattern will
;;; be put into an error status with a status of
;;; CAIRO_STATUS_INVALID_MESH_CONSTRUCTION.
;;;
;;; pattern :
;;;     a cairo_pattern_t
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
;;; Since 1.12
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_mesh_pattern_set_control_point ()
;;;
;;; void cairo_mesh_pattern_set_control_point (cairo_pattern_t *pattern,
;;;                                            unsigned int point_num,
;;;                                            double x,
;;;                                            double y);
;;;
;;; Set an internal control point of the current patch.
;;;
;;; Valid values for point_num are from 0 to 3 and identify the control points
;;; as explained in cairo_pattern_create_mesh().
;;;
;;; Note: If pattern is not a mesh pattern then pattern will be put into an
;;; error status with a status of CAIRO_STATUS_PATTERN_TYPE_MISMATCH. If
;;; point_num is not valid, pattern will be put into an error status with a
;;; status of CAIRO_STATUS_INVALID_INDEX. If pattern has no current patch,
;;; pattern will be put into an error status with a status of
;;; CAIRO_STATUS_INVALID_MESH_CONSTRUCTION.
;;;
;;; pattern :
;;;     a cairo_pattern_t
;;;
;;; point_num :
;;;     the control point to set the position for
;;;
;;; x :
;;;     the X coordinate of the control point
;;;
;;; y :
;;;     the Y coordinate of the control point
;;;
;;; Since 1.12
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_mesh_pattern_set_corner_color_rgb ()
;;;
;;; void cairo_mesh_pattern_set_corner_color_rgb (cairo_pattern_t *pattern,
;;;                                               unsigned int corner_num,
;;;                                               double red,
;;;                                               double green,
;;;                                               double blue);
;;;
;;; Sets the color of a corner of the current patch in a mesh pattern.
;;;
;;; The color is specified in the same way as in cairo_set_source_rgb().
;;;
;;; Valid values for corner_num are from 0 to 3 and identify the corners as
;;; explained in cairo_pattern_create_mesh().
;;;
;;; Note: If pattern is not a mesh pattern then pattern will be put into an
;;; error status with a status of CAIRO_STATUS_PATTERN_TYPE_MISMATCH. If
;;; corner_num is not valid, pattern will be put into an error status with a
;;; status of CAIRO_STATUS_INVALID_INDEX. If pattern has no current patch,
;;; pattern will be put into an error status with a status of
;;; CAIRO_STATUS_INVALID_MESH_CONSTRUCTION.
;;;
;;; pattern :
;;;     a cairo_pattern_t
;;;
;;; corner_num :
;;;     the corner to set the color for
;;;
;;; red :
;;;     red component of color
;;;
;;; green :
;;;     green component of color
;;;
;;; blue :
;;;     blue component of color
;;;
;;; Since 1.12
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_mesh_pattern_set_corner_color_rgba ()
;;;
;;; void cairo_mesh_pattern_set_corner_color_rgba (cairo_pattern_t *pattern,
;;;                                                unsigned int corner_num,
;;;                                                double red,
;;;                                                double green,
;;;                                                double blue,
;;;                                                double alpha);
;;;
;;; Sets the color of a corner of the current patch in a mesh pattern.
;;;
;;; The color is specified in the same way as in cairo_set_source_rgba().
;;;
;;; Valid values for corner_num are from 0 to 3 and identify the corners as
;;; explained in cairo_pattern_create_mesh().
;;;
;;; Note: If pattern is not a mesh pattern then pattern will be put into an
;;; error status with a status of CAIRO_STATUS_PATTERN_TYPE_MISMATCH. If
;;; corner_num is not valid, pattern will be put into an error status with a
;;; status of CAIRO_STATUS_INVALID_INDEX. If pattern has no current patch,
;;; pattern will be put into an error status with a status of
;;; CAIRO_STATUS_INVALID_MESH_CONSTRUCTION.
;;;
;;; pattern :
;;;     a cairo_pattern_t
;;;
;;; corner_num :
;;;     the corner to set the color for
;;;
;;; red :
;;;     red component of color
;;;
;;; green :
;;;     green component of color
;;;
;;; blue :
;;;     blue component of color
;;;
;;; alpha :
;;;     alpha component of color
;;;
;;; Since 1.12
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_mesh_pattern_get_patch_count ()
;;;
;;; cairo_status_t cairo_mesh_pattern_get_patch_count (cairo_pattern_t *pattern,
;;;                                                    unsigned int *count);
;;;
;;; Gets the number of patches specified in the given mesh pattern.
;;;
;;; The number only includes patches which have been finished by calling
;;; cairo_mesh_pattern_end_patch(). For example it will be 0 during the
;;; definition of the first patch.
;;;
;;; pattern :
;;;     a cairo_pattern_t
;;;
;;; count :
;;;     return value for the number patches, or NULL
;;;
;;; Returns :
;;;     CAIRO_STATUS_SUCCESS, or CAIRO_STATUS_PATTERN_TYPE_MISMATCH if pattern
;;;     is not a mesh pattern.
;;;
;;; Since 1.12
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_mesh_pattern_get_path ()
;;;
;;; cairo_path_t * cairo_mesh_pattern_get_path (cairo_pattern_t *pattern,
;;;                                             unsigned int patch_num);
;;;
;;; Gets path defining the patch patch_num for a mesh pattern.
;;;
;;; patch_num can range 0 to 1 less than the number returned by
;;; cairo_mesh_pattern_get_patch_count().
;;;
;;; pattern :
;;;     a cairo_pattern_t
;;;
;;; patch_num :
;;;     the patch number to return data for
;;;
;;; Returns :
;;;     the path defining the patch, or a path with status
;;;     CAIRO_STATUS_INVALID_INDEX if patch_num or point_num is not valid for
;;;     pattern. If pattern is not a mesh pattern, a path with status
;;;     CAIRO_STATUS_PATTERN_TYPE_MISMATCH is returned.
;;;
;;; Since 1.12
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_mesh_pattern_get_control_point ()
;;;
;;; cairo_status_t cairo_mesh_pattern_get_control_point
;;;                                                   (cairo_pattern_t *pattern,
;;;                                                    unsigned int patch_num,
;;;                                                    unsigned int point_num,
;;;                                                    double *x,
;;;                                                    double *y);
;;;
;;; Gets the control point point_num of patch patch_num for a mesh pattern.
;;;
;;; patch_num can range 0 to 1 less than the number returned by
;;; cairo_mesh_pattern_get_patch_count().
;;;
;;; Valid values for point_num are from 0 to 3 and identify the control points
;;; as explained in cairo_pattern_create_mesh().
;;;
;;; pattern :
;;;     a cairo_pattern_t
;;;
;;; patch_num :
;;;     the patch number to return data for
;;;
;;; point_num :
;;;     the control point number to return data for
;;;
;;; x :
;;;     return value for the x coordinate of the control point, or NULL
;;;
;;; y :
;;;     return value for the y coordinate of the control point, or NULL
;;;
;;; Returns :
;;;     CAIRO_STATUS_SUCCESS, or CAIRO_STATUS_INVALID_INDEX if patch_num or
;;;     point_num is not valid for pattern. If pattern is not a mesh pattern,
;;;     CAIRO_STATUS_PATTERN_TYPE_MISMATCH is returned.
;;;
;;; Since 1.12
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_mesh_pattern_get_corner_color_rgba ()
;;;
;;; cairo_status_t cairo_mesh_pattern_get_corner_color_rgba
;;;                                                   (cairo_pattern_t *pattern,
;;;                                                    unsigned int patch_num,
;;;                                                    unsigned int corner_num,
;;;                                                    double *red,
;;;                                                    double *green,
;;;                                                    double *blue,
;;;                                                    double *alpha);
;;;
;;; Gets the color information in corner corner_num of patch patch_num for a
;;; mesh pattern.
;;;
;;; patch_num can range 0 to 1 less than the number returned by
;;; cairo_mesh_pattern_get_patch_count().
;;;
;;; Valid values for corner_num are from 0 to 3 and identify the corners as
;;; explained in cairo_pattern_create_mesh().
;;;
;;; pattern :
;;;     a cairo_pattern_t
;;;
;;; patch_num :
;;;     the patch number to return data for
;;;
;;; corner_num :
;;;     the corner number to return data for
;;;
;;; red :
;;;     return value for red component of color, or NULL
;;;
;;; green :
;;;     return value for green component of color, or NULL
;;;
;;; blue :
;;;     return value for blue component of color, or NULL
;;;
;;; alpha :
;;;     return value for alpha component of color, or NULL
;;;
;;; Returns :
;;;     CAIRO_STATUS_SUCCESS, or CAIRO_STATUS_INVALID_INDEX if patch_num or
;;;     corner_num is not valid for pattern. If pattern is not a mesh pattern,
;;;     CAIRO_STATUS_PATTERN_TYPE_MISMATCH is returned.
;;;
;;; Since 1.12
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_pattern_reference ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_pattern_reference" cairo-pattern-reference)
    (:pointer (:struct cairo-pattern-t))
 #+cl-cffi-gtk-documentation
 "@version{2013-11-20}
  @argument[pattern]{a @symbol{cairo-pattern-t}}
  @return{The referenced @symbol{cairo-pattern-t}.}
  @begin{short}
    Increases the reference count on @arg{pattern} by one.
  @end{short}
  This prevents pattern from being destroyed until a matching call to
  the function @fun{cairo-pattern-destroy} is made.

  The number of references to a @symbol{cairo-pattern-t} can be get using the
  function @fun{cairo-pattern-get-reference-count}.

  Since 1.0
  @see-symbol{cairo-pattern-t}
  @see-function{cairo-pattern-destroy}
  @see-function{cairo-pattern-get-reference-count}"
  (pattern (:pointer (:struct cairo-pattern-t))))

(export 'cairo-pattern-reference)

;;; ----------------------------------------------------------------------------
;;; cairo_pattern_destroy ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_pattern_destroy" cairo-pattern-destroy) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-12-2}
  @argument[pattern]{a @symbol{cairo-pattern-t}}
  @begin{short}
    Decreases the reference count on pattern by one.
  @end{short}
  If the result is zero, then pattern and all associated resources are freed.
  See the function @fun{cairo-pattern-reference}.

  Since 1.0
  @see-symbol{cairo-pattern-t}
  @see-function{cairo-pattern-reference}"
  (pattern (:pointer (:struct cairo-pattern-t))))

(export 'cairo-pattern-destroy)

;;; ----------------------------------------------------------------------------
;;; cairo_pattern_status ()
;;;
;;; cairo_status_t cairo_pattern_status (cairo_pattern_t *pattern);
;;;
;;; Checks whether an error has previously occurred for this pattern.
;;;
;;; pattern :
;;;     a cairo_pattern_t
;;;
;;; Returns :
;;;     CAIRO_STATUS_SUCCESS, CAIRO_STATUS_NO_MEMORY,
;;;     CAIRO_STATUS_INVALID_MATRIX, CAIRO_STATUS_PATTERN_TYPE_MISMATCH, or
;;;     CAIRO_STATUS_INVALID_MESH_CONSTRUCTION.
;;;
;;; Since 1.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; enum cairo_extend_t
;;; ----------------------------------------------------------------------------

(defcenum cairo-extend-t
  :none
  :repeat
  :reflect
  :pad)

#+cl-cffi-gtk-documentation
(setf (gethash 'cairo-extend-t atdoc:*symbol-name-alias*) "CEnum"
      (gethash 'cairo-extend-t atdoc:*external-symbols*)
 "@version{2013-11-11}
  @begin{short}
    @sym{cairo-extend-t} is used to describe how pattern color/alpha will be
    determined for areas \"outside\" the pattern's natural area, for example,
    outside the surface bounds or outside the gradient geometry.
  @end{short}

  Mesh patterns are not affected by the extend mode.

  The default extend mode is @code{:none} for surface patterns and @code{:pad}
  for gradient patterns.

  New entries may be added in future versions.
  @begin{pre}
(defcenum cairo-extend-t
  :none
  :repeat
  :reflect
  :pad)
  @end{pre}
  @begin[code]{table}
    @entry[:none]{Pixels outside of the source pattern are fully transparent.
      Since 1.0}
    @entry[:repeat]{The pattern is tiled by repeating. Since 1.0}
    @entry[:reflect]{The pattern is tiled by reflecting at the edges. Since 1.0;
      but only implemented for surface patterns since 1.6.}
    @entry[:pad]{Pixels outside of the pattern copy the closest pixel from the
      source. Since 1.2; but only implemented for surface patterns since 1.6.}
  @end{table}
  Since 1.0
  @symbol{cairo-pattern-t}")

(export 'cairo-extend-t)

;;; ----------------------------------------------------------------------------
;;; cairo_pattern_set_extend ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_pattern_set_extend" cairo-pattern-set-extend) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-11-11}
  @argument[pattern]{a @symbol{cairo-pattern-t}}
  @argument[extend]{a @symbol{cairo-extend-t} describing how the area outside of
    the pattern will be drawn}
  @begin{short}
    Sets the mode to be used for drawing outside the area of a pattern.
  @end{short}
  See @symbol{cairo-extend-t} for details on the semantics of each extend
  strategy.

  The default extend mode is @code{:none} for surface patterns and @code{:pad}
  for gradient patterns.

  Since 1.0
  @see-symbol{cairo-pattern-t}
  @see-symbol{cairo-extend-t}"
  (pattern (:pointer (:struct cairo-pattern-t)))
  (extend cairo-extend-t))

(export 'cairo-pattern-set-extend)

;;; ----------------------------------------------------------------------------
;;; cairo_pattern_get_extend ()
;;;
;;; cairo_extend_t cairo_pattern_get_extend (cairo_pattern_t *pattern);
;;;
;;; Gets the current extend mode for a pattern. See cairo_extend_t for details
;;; on the semantics of each extend strategy.
;;;
;;; pattern :
;;;     a cairo_pattern_t
;;;
;;; Returns :
;;;     the current extend strategy used for drawing the pattern.
;;;
;;; Since 1.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; enum cairo_filter_t
;;;
;;; typedef enum {
;;;     CAIRO_FILTER_FAST,
;;;     CAIRO_FILTER_GOOD,
;;;     CAIRO_FILTER_BEST,
;;;     CAIRO_FILTER_NEAREST,
;;;     CAIRO_FILTER_BILINEAR,
;;;     CAIRO_FILTER_GAUSSIAN
;;; } cairo_filter_t;
;;;
;;; cairo_filter_t is used to indicate what filtering should be applied when
;;; reading pixel values from patterns. See cairo_pattern_set_filter() for
;;; indicating the desired filter to be used with a particular pattern.
;;;
;;; CAIRO_FILTER_FAST
;;;     A high-performance filter, with quality similar to CAIRO_FILTER_NEAREST
;;;     (Since 1.0)
;;;
;;; CAIRO_FILTER_GOOD
;;;     A reasonable-performance filter, with quality similar to
;;;     CAIRO_FILTER_BILINEAR (Since 1.0)
;;;
;;; CAIRO_FILTER_BEST
;;;     The highest-quality available, performance may not be suitable for
;;;     interactive use. (Since 1.0)
;;;
;;; CAIRO_FILTER_NEAREST
;;;     Nearest-neighbor filtering (Since 1.0)
;;;
;;; CAIRO_FILTER_BILINEAR
;;;     Linear interpolation in two dimensions (Since 1.0)
;;;
;;; CAIRO_FILTER_GAUSSIAN
;;;     This filter value is currently unimplemented, and should not be used in
;;;     current code. (Since 1.0)
;;;
;;; Since 1.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_pattern_set_filter ()
;;;
;;; void cairo_pattern_set_filter (cairo_pattern_t *pattern,
;;;                                cairo_filter_t filter);
;;;
;;; Sets the filter to be used for resizing when using this pattern. See
;;; cairo_filter_t for details on each filter.
;;;
;;; * Note that you might want to control filtering even when you do not have an
;;; explicit cairo_pattern_t object, (for example when using
;;; cairo_set_source_surface()). In these cases, it is convenient to use
;;; cairo_get_source() to get access to the pattern that cairo creates
;;; implicitly. For example:
;;;
;;; cairo_set_source_surface (cr, image, x, y);
;;; cairo_pattern_set_filter (cairo_get_source (cr), CAIRO_FILTER_NEAREST);
;;;
;;; pattern :
;;;     a cairo_pattern_t
;;;
;;; filter :
;;;     a cairo_filter_t describing the filter to use for resizing the pattern
;;;
;;; Since 1.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_pattern_get_filter ()
;;;
;;; cairo_filter_t cairo_pattern_get_filter (cairo_pattern_t *pattern);
;;;
;;; Gets the current filter for a pattern. See cairo_filter_t for details on
;;; each filter.
;;;
;;; pattern :
;;;     a cairo_pattern_t
;;;
;;; Returns :
;;;     the current filter used for resizing the pattern.
;;;
;;; Since 1.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_pattern_set_matrix ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_pattern_set_matrix" cairo-pattern-set-matrix) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-12-2}
  @argument[pattern]{a @symbol{cairo-pattern-t}}
  @argument[matrix]{a @symbol{cairo-matrix-t}}
  @begin{short}
    Sets the pattern's transformation matrix to @arg{matrix}. This matrix is a
    transformation from user space to pattern space.
  @end{short}

  When a pattern is first created it always has the identity matrix for its
  transformation matrix, which means that pattern space is initially identical
  to user space.

  Important: Please note that the direction of this transformation matrix is
  from user space to pattern space. This means that if you imagine the flow
  from a pattern to user space, and on to device space, then coordinates in
  that flow will be transformed by the inverse of the pattern matrix.

  For example, if you want to make a pattern appear twice as large as it does
  by default the correct code to use is:
  @begin{pre}
 cairo_matrix_init_scale (&matrix, 0.5, 0.5);
 cairo_pattern_set_matrix (pattern, &matrix);
  @end{pre}
  Meanwhile, using values of 2.0 rather than 0.5 in the code above would cause
  the pattern to appear at half of its default size.

  Also, please note the discussion of the user-space locking semantics of the
  function @fun{cairo-set-source}.

  Since 1.0
  @see-symbol{cairo-pattern-t}
  @see-symbol{cairo-matrix-t}
  @see-function{cairo-set-source}"
  (pattern (:pointer (:struct cairo-pattern-t)))
  (matrix (:pointer (:struct cairo-matrix-t))))

(export 'cairo-pattern-set-matrix)

;;; ----------------------------------------------------------------------------
;;; cairo_pattern_get_matrix ()
;;;
;;; void cairo_pattern_get_matrix (cairo_pattern_t *pattern,
;;;                                cairo_matrix_t *matrix);
;;;
;;; Stores the pattern's transformation matrix into matrix.
;;;
;;; pattern :
;;;     a cairo_pattern_t
;;;
;;; matrix :
;;;     return value for the matrix
;;;
;;; Since 1.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; enum cairo_pattern_type_t
;;;
;;; typedef enum {
;;;     CAIRO_PATTERN_TYPE_SOLID,
;;;     CAIRO_PATTERN_TYPE_SURFACE,
;;;     CAIRO_PATTERN_TYPE_LINEAR,
;;;     CAIRO_PATTERN_TYPE_RADIAL,
;;;     CAIRO_PATTERN_TYPE_MESH,
;;;     CAIRO_PATTERN_TYPE_RASTER_SOURCE
;;; } cairo_pattern_type_t;
;;;
;;; cairo_pattern_type_t is used to describe the type of a given pattern.
;;;
;;; The type of a pattern is determined by the function used to create it. The
;;; cairo_pattern_create_rgb() and cairo_pattern_create_rgba() functions create
;;; SOLID patterns. The remaining cairo_pattern_create functions map to pattern
;;; types in obvious ways.
;;;
;;; The pattern type can be queried with cairo_pattern_get_type()
;;;
;;; Most cairo_pattern_t functions can be called with a pattern of any type,
;;; (though trying to change the extend or filter for a solid pattern will have
;;; no effect). A notable exception is cairo_pattern_add_color_stop_rgb() and
;;; cairo_pattern_add_color_stop_rgba() which must only be called with gradient
;;; patterns (either LINEAR or RADIAL). Otherwise the pattern will be shutdown
;;; and put into an error state.
;;;
;;; New entries may be added in future versions.
;;;
;;; CAIRO_PATTERN_TYPE_SOLID
;;;     The pattern is a solid (uniform) color. It may be opaque or translucent,
;;;     since 1.2.
;;;
;;; CAIRO_PATTERN_TYPE_SURFACE
;;;     The pattern is a based on a surface (an image), since 1.2.
;;;
;;; CAIRO_PATTERN_TYPE_LINEAR
;;;     The pattern is a linear gradient, since 1.2.
;;;
;;; CAIRO_PATTERN_TYPE_RADIAL
;;;     The pattern is a radial gradient, since 1.2.
;;;
;;; CAIRO_PATTERN_TYPE_MESH
;;;     The pattern is a mesh, since 1.12.
;;;
;;; CAIRO_PATTERN_TYPE_RASTER_SOURCE
;;;     The pattern is a user pattern providing raster data, since 1.12.
;;;
;;; Since 1.2
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_pattern_get_type ()
;;;
;;; cairo_pattern_type_t cairo_pattern_get_type (cairo_pattern_t *pattern);
;;;
;;; This function returns the type a pattern. See cairo_pattern_type_t for
;;; available types.
;;;
;;; pattern :
;;;     a cairo_pattern_t
;;;
;;; Returns :
;;;     The type of pattern.
;;;
;;; Since 1.2
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_pattern_get_reference_count ()
;;;
;;; unsigned int cairo_pattern_get_reference_count (cairo_pattern_t *pattern);
;;;
;;; Returns the current reference count of pattern.
;;;
;;; pattern :
;;;     a cairo_pattern_t
;;;
;;; Returns :
;;;     the current reference count of pattern. If the object is a nil object,
;;;     0 will be returned.
;;;
;;; Since 1.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_pattern_set_user_data ()
;;;
;;; cairo_status_t cairo_pattern_set_user_data (cairo_pattern_t *pattern,
;;;                                            const cairo_user_data_key_t *key,
;;;                                            void *user_data,
;;;                                            cairo_destroy_func_t destroy);
;;;
;;; Attach user data to pattern. To remove user data from a surface, call this
;;; function with the key that was used to set it and NULL for data.
;;;
;;; pattern :
;;;     a cairo_pattern_t
;;;
;;; key :
;;;     the address of a cairo_user_data_key_t to attach the user data to
;;;
;;; user_data :
;;;     the user data to attach to the cairo_pattern_t
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
;;; cairo_pattern_get_user_data ()
;;;
;;; void * cairo_pattern_get_user_data (cairo_pattern_t *pattern,
;;;                                     const cairo_user_data_key_t *key);
;;;
;;; Return user data previously attached to pattern using the specified key. If
;;; no user data has been attached with the given key this function returns
;;; NULL.
;;;
;;; pattern :
;;;     a cairo_pattern_t
;;;
;;; key :
;;;     the address of the cairo_user_data_key_t the user data was attached to
;;;
;;; Returns :
;;;     the user data previously attached or NULL.
;;;
;;; Since 1.4
;;; ----------------------------------------------------------------------------

;;; --- End of file cairo.pattern.lisp -----------------------------------------
