;;; ----------------------------------------------------------------------------
;;; cairo.paths.lisp
;;;
;;; The documentation of the file is taken from the Cairo Reference Manual
;;; Version 1.16 and modified to document the Lisp binding to the Cairo
;;; library. See <http://cairographics.org>. The API documentation of the
;;; Lisp binding is available at <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2012 - 2020 Dieter Kaiser
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
;;;     Creating paths and manipulating path data
;;;
;;; Types and Values
;;;
;;;     cairo_path_t
;;;     cairo_path_data_t
;;;     cairo_path_data_type_t
;;;
;;; Functions
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
;;; enum cairo_path_data_type_t
;;; ----------------------------------------------------------------------------

(defcenum cairo-path-data-type-t
  :move-to
  :line-to
  :curve-to
  :close-path)

#+cl-cffi-gtk-documentation
(setf (gethash 'cairo-path-data-type-t atdoc:*symbol-name-alias*)
       "Enum"
      (gethash 'cairo-path-data-type-t atdoc:*external-symbols*)
 "@version{2020-12-11}
  @begin{short}
    The @sym{cairo-path-data-type-t} enumeration is used to describe the type of
    one portion of a path when represented as a @symbol{cairo-path-t} structure.
  @end{short}
  See the @symbol{cairo-path-data-t} structure for details.
  @begin{pre}
(defcenum cairo-path-data-type-t
  :move-to
  :line-to
  :curve-to
  :close-path)
  @end{pre}
  @begin[code]{table}
    @entry[:move-to]{A move-to operation.}
    @entry[:line-to]{A line-to operation.}
    @entry[:curve-to]{A curve-to operation.}
    @entry[:close-path]{A close-path operation.}
  @end{table}
  @see-symbol{cairo-path-t}
  @see-symbol{cairo-path-data-t}")

(export 'cairo-path-data-type-t)

;;; ----------------------------------------------------------------------------
;;; union cairo_path_data_t
;;; ----------------------------------------------------------------------------

(defcstruct header-t
  (data-type cairo-path-data-type-t)
  (length :int))

(defcstruct point-t
  (x :double)
  (y :double))

(defcstruct cairo-path-data-t
  (header (:pointer (:struct header-t)))
  (point (:pointer (:struct point-t))))

#+cl-cffi-gtk-documentation
(setf (gethash 'cairo-path-data-t atdoc:*symbol-name-alias*)
       "CStruct"
      (gethash 'cairo-path-data-t atdoc:*external-symbols*)
 "@version{2020-12-11}
  @begin{short}
    The @sym{cairo-path-data-t} structure is used to represent the path data
    inside a @symbol{cairo-path-t}.
  @end{short}

  The data structure is designed to try to balance the demands of efficiency
  and ease-of-use. A path is represented as an array of @sym{cairo-path-data-t}
  structures, which is a union of headers and points.
  @begin{pre}
(defcstruct header-t
  (data-type cairo-path-data-type-t)
  (length :int))

(defcstruct point-t
  (x :double)
  (y :double))

(defcunion cairo-path-data-t
  (header (:pointer (:struct header-t)))
  (point (:pointer (:struct point-t))))
  @end{pre}
  Each portion of the path is represented by one or more elements in the array,
  (one header followed by 0 or more points). The length value of the header is
  the number of array elements for the current portion including the header,
  (i.e. length == 1 + # of points), and where the number of points for each
  element type is as follows:
  @begin{pre}
%CAIRO_PATH_MOVE_TO:     1 point
%CAIRO_PATH_LINE_TO:     1 point
%CAIRO_PATH_CURVE_TO:    3 points
%CAIRO_PATH_CLOSE_PATH:  0 points
  @end{pre}
  The semantics and ordering of the coordinate values are consistent with
  the functions @fun{cairo-move-to}, @fun{cairo-line-to}, @fun{cairo-curve-to},
  and @fun{cairo-close-path}.

  Here is sample code for iterating through a @symbol{cairo-path-t} structure:
  @begin{pre}
int i;
cairo_path_t *path;
cairo_path_data_t *data;

path = cairo_copy_path (cr);

for (i=0; i < path->num_data; i += path->data[i].header.length) {
    data = &path->data[i];
    switch (data->header.type) {
    case CAIRO_PATH_MOVE_TO:
        do_move_to_things (data[1].point.x, data[1].point.y);
        break;
    case CAIRO_PATH_LINE_TO:
        do_line_to_things (data[1].point.x, data[1].point.y);
        break;
    case CAIRO_PATH_CURVE_TO:
        do_curve_to_things (data[1].point.x, data[1].point.y,
                            data[2].point.x, data[2].point.y,
                            data[3].point.x, data[3].point.y);
        break;
    case CAIRO_PATH_CLOSE_PATH:
        do_close_path_things ();
        break;
    @}
@}
cairo_path_destroy (path);
  @end{pre}
  As of Cairo 1.4, Cairo does not mind if there are more elements in a portion
  of the path than needed. Such elements can be used by users of the Cairo API
  to hold extra values in the path data structure. For this reason, it is
  recommended that applications always use data->header.length to iterate over
  the path data, instead of hardcoding the number of elements for each element
  type.
  @see-symbol{cairo-path-t}
  @see-function{cairo-move-to}
  @see-function{cairo-line-to}
  @see-function{cairo-curve-to}
  @see-function{cairo-close-path}")

(export 'cairo-path-data-t)

;;; ----------------------------------------------------------------------------
;;; cairo_path_t
;;; ----------------------------------------------------------------------------

(defcstruct cairo-path-t
  (status cairo-status-t)
  (data :pointer)           ; (:pointer (:pointer (:struct cairo-path-data-t))))
  (num-data :int))

#+cl-cffi-gtk-documentation
(setf (gethash 'cairo-path-t atdoc:*symbol-name-alias*)
       "CStruct"
      (gethash 'cairo-path-t atdoc:*external-symbols*)
 "@version{2020-12-25}
  @begin{short}
    A data structure for holding a path.
  @end{short}
  This data structure serves as the return value for the functions
  @fun{cairo-copy-path} and @fun{cairo-copy-path-flat} as well the input value
  for the function @fun{cairo-append-path}.

  See the @symbol{cairo-path-data-t} structure for hints on how to iterate over
  the actual data within the path.

  The @arg{num-data} member gives the number of elements in the data array.
  This number is larger than the number of independent path portions (defined
  in the @symbol{cairo-path-data-type-t} structure), since the data includes
  both headers and coordinates for each portion.
  @begin{pre}
(defcstruct cairo-path-t
  (status cairo-status-t)
  (data (:pointer (:pointer (:struct cairo-path-data-t))))
  (num-data :int))
  @end{pre}
  @begin[code]{table}
    @entry[status]{The current @symbol{cairo-status-t} error status.}
    @entry[data]{The elements of type @symbol{cairo-path-data-t} in the path.}
    @entry[num-data]{An integer with the number of elements in the data array.}
  @end{table}
  @see-symbol{cairo-status-t}
  @see-symbol{cairo-path-data-t}
  @see-function{cairo-copy-path}
  @see-function{cairo-copy-path-flat}
  @see-function{cairo-append-path}")

(export 'cairo-path-t)

;;; ----------------------------------------------------------------------------
;;; cairo_copy_path ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_copy_path" cairo-copy-path) (:pointer (:struct cairo-path-t))
 #+cl-cffi-gtk-documentation
 "@version{2020-12-25}
  @argument[cr]{a @symbol{cairo-t} context}
  @begin{return}
    The copy of the @symbol{cairo-path-t} current path. The caller owns the
    returned object and should call the function @fun{cairo-path-destroy} when
    finished with it.
  @end{return}
  @begin{short}
    Creates a copy of the current path and returns it to the user as a
    @symbol{cairo-path-t} structure.
  @end{short}
  See the @symbol{cairo-path-data-t} structure for hints on how to iterate over
  the returned data structure.

  This function will always return a valid pointer, but the result will have
  no data, if either of the following conditions hold:
  @begin{itemize}
    @begin{item}
      If there is insufficient memory to copy the path. In this case the status
      of the path will be set to @code{:no-memory}.
    @end{item}
    @begin{item}
      If @arg{cr} is already in an error state. In this case path->status will
      contain the same status that would be returned by the function
      @fun{cairo-status}.
    @end{item}
  @end{itemize}
  @see-symbol{cairo-t}
  @see-symbol{cairo-path-t}
  @see-symbol{cairo-path-data-t}
  @see-function{cairo-status}
  @see-function{cairo-path-destroy}"
  (cr (:pointer (:struct cairo-t))))

(export 'cairo-copy-path)

;;; ----------------------------------------------------------------------------
;;; cairo_copy_path_flat ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_copy_path_flat" cairo-copy-path-flat)
    (:pointer (:struct cairo-path-t))
 #+cl-cffi-gtk-documentation
 "@version{2020-12-25}
  @argument[cr]{a @symbol{cairo-t} context}
  @begin{return}
    The copy of the @symbol{cairo-path-t} current path. The caller owns the
    returned object and should call the function @fun{cairo-path-destroy} when
    finished with it.
  @end{return}
  @begin{short}
    Gets a flattened copy of the current path and returns it to the user as a
    @symbol{cairo-path-t} structure.
  @end{short}
  See the @symbol{cairo-path-data-t} structure for hints on how to iterate over
  the returned data structure.

  This function is like the function @fun{cairo-copy-path} except that any
  curves in the path will be approximated with piecewise-linear approximations,
  (accurate to within the current tolerance value). That is, the result is
  guaranteed to not have any elements of type @code{:curve-to} which will
  instead be replaced by a series of @code{:line-to} elements.

  This function will always return a valid pointer, but the result will have
  no data, if either of the following conditions hold:
  @begin{itemize}
    @begin{item}
      If there is insufficient memory to copy the path. In this case the status
      of the path will be set to @code{:no-memory}.
    @end{item}
    @begin{item}
      If @arg{cr} is already in an error state. In this case path->status will
      contain the same status that would be returned by the function
      @fun{cairo-status}.
    @end{item}
  @end{itemize}
  @see-symbol{cairo-t}
  @see-symbol{cairo-path-t}
  @see-symbol{cairo-path-data-t}
  @see-function{cairo-status}
  @see-function{cairo-copy-path}
  @see-function{cairo-path-destroy}"
  (cr (:pointer (:struct cairo-t))))

(export 'cairo-copy-path-flat)

;;; ----------------------------------------------------------------------------
;;; cairo_path_destroy ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_path_destroy" cairo-path-destroy) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-12-25}
  @argument[path]{a @symbol{cairo-path-t} structure previously returned by
    either the function @fun{cairo-copy-path} or @fun{cairo-copy-path-flat}}
  @begin{short}
    Immediately releases all memory associated with @arg{path}.
  @end{short}
  After a call to the function @sym{cairo-path-destroy} the path pointer is no
  longer valid and should not be used further.
  @begin[Note]{dictionary}
    The function @sym{cairo-path-destroy} should only be called with a pointer
    to a @symbol{cairo-path-t} structure returned by a Cairo function. Any path
    that is created manually (i.e. outside of Cairo) should be destroyed
    manually as well.
  @end{dictionary}
  @see-symbol{cairo-path-t}
  @see-function{cairo-copy-path}
  @see-function{cairo-copy-path-flat}"
  (path (:pointer (:struct cairo-path-t))))

(export 'cairo-path-destroy)

;;; ----------------------------------------------------------------------------
;;; cairo_append_path ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_append_path" cairo-append-path) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-12-25}
  @argument[cr]{a @symbol{cairo-t} context}
  @argument[path]{a @symbol{cairo-path-t} structure to be appended}
  @begin{short}
    Append @arg{path} onto the current path.
  @end{short}
  The path may be either the return value from one of the functions
  @fun{cairo-copy-path} or @fun{cairo-copy-path-flat} or it may be constructed
  manually. See the @symbol{cairo-path-t} structure for details on how the path
  data structure should be initialized, and note that the status of the path
  must be initialized to @code{:success}.
  @see-symbol{cairo-t}
  @see-symbol{cairo-path-t}
  @see-function{cairo-copy-path}
  @see-function{cairo-copy-path-flat}"
  (cr (:pointer (:struct cairo-t)))
  (path (:pointer (:struct cairo-path-t))))

(export 'cairo-append-path)

;;; ----------------------------------------------------------------------------
;;; cairo_has_current_point ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_has_current_point" cairo-has-current-point) cairo-bool-t
 #+cl-cffi-gtk-documentation
 "@version{2020-12-11}
  @argument[cr]{a @symbol{cairo-t} context}
  @return{Whether a current point is defined.}
  @begin{short}
    Returns whether a current point is defined on the current path.
  @end{short}
  See the function @fun{cairo-get-current-point} for details on the current
  point.
  @see-symbol{cairo-t}
  @see-function{cairo-get-current-point}"
  (cr (:pointer (:struct cairo-t))))

(export 'cairo-has-current-point)

;;; ----------------------------------------------------------------------------
;;; cairo_get_current_point ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_get_current_point" %cairo-get-current-point) :void
  (cr (:pointer (:struct cairo-t)))
  (x (:pointer :double))
  (y (:pointer :double)))

(defun cairo-get-current-point (cr)
 #+cl-cffi-gtk-documentation
 "@version{2020-12-25}
  @argument[cr]{a @symbol{cairo-t} context}
  @begin{return}
    @code{x} -- a double float x coordinate of the current point @br{}
    @code{y} -- a double float y coordinate of the current point
  @end{return}
  @begin{short}
    Gets the current point of the current path, which is conceptually the final
    point reached by the path so far.
  @end{short}

  The current point is returned in the user-space coordinate system. If there
  is no defined current point or if @arg{cr} is in an error status, x and y will
  both be set to 0.0. It is possible to check this in advance with the function
  @fun{cairo-has-current-point}.

  Most path construction functions alter the current point. See the following
  functions for details on how they affect the current point:
  @begin{pre}
cairo-new-path           cairo-new-sub-path       cairo-append-path
cairo-close-path         cairo-move-to            cairo-line-to
cairo-curve-to           cairo-rel-move-to        cairo-rel-line-to
cairo-rel-curve-to       cairo-arc                cairo-arc-negative
cairo-rectangle          cairo-text-path          cairo-glyph-path
cairo-stroke-to-path
  @end{pre}
  Some functions use and alter the current point but do not otherwise change
  current path:
  @begin{pre}
cairo-show-text
  @end{pre}
  Some functions unset the current path and as a result, current point:
  @begin{pre}
cairo-fill               cairo-stroke
  @end{pre}
  @see-symbol{cairo-t}
  @see-function{cairo-has-current-point}"
  (with-foreign-objects ((x :double) (y :double))
    (%cairo-get-current-point cr x y)
    (values (mem-ref x :double)
            (mem-ref y :double))))

(export 'cairo-get-current-point)

;;; ----------------------------------------------------------------------------
;;; cairo_new_path ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_new_path" cairo-new-path) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-12-11}
  @argument[cr]{a @symbol{cairo-t} context}
  @begin{short}
    Clears the current path.
  @end{short}
  After this call there will be no path and no current point.
  @see-function{cairo-t}"
  (cr (:pointer (:struct cairo-t))))

(export 'cairo-new-path)

;;; ----------------------------------------------------------------------------
;;; cairo_new_sub_path ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_new_sub_path" cairo-new-sub-path) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-12-11}
  @argument[cr]{a @symbol{cairo-t} context}
  @begin{short}
    Begin a new sub-path.
  @end{short}
  Note that the existing path is not affected. After this call there will be no
  current point.

  In many cases, this call is not needed since new sub-paths are frequently
  started with the function @fun{cairo-move-to}.

  A call to the function @sym{cairo-new-sub-path} is particularly useful when
  beginning a new sub-path with one of the @code{cairo-arc} calls. This makes
  things easier as it is no longer necessary to manually compute the arc's
  initial coordinates for a call to the function @fun{cairo-move-to}.
  @see-symbol{cairo-t}
  @see-function{cairo-move-to}"
  (cr (:pointer (:struct cairo-t))))

(export 'cairo-new-sub-path)

;;; ----------------------------------------------------------------------------
;;; cairo_close_path ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_close_path" cairo-close-path) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-12-11}
  @argument[cr]{a @symbol{cairo-t} context}
  @begin{short}
    Adds a line segment to the path from the current point to the beginning of
    the current sub-path, the most recent point passed to the function
    @fun{cairo-move-to}, and closes this sub-path.
  @end{short}
  After this call the current point will be at the joined endpoint of the
  sub-path.

  The behavior of the funcion @sym{cairo-close-path} is distinct from simply
  calling the function @fun{cairo-line-to} with the equivalent coordinate in
  the case of stroking. When a closed sub-path is stroked, there are no caps on
  the ends of the sub-path. Instead, there is a line join connecting the final
  and initial segments of the sub-path.

  If there is no current point before the call to the function
  @sym{cairo-close-path}, this function will have no effect.
  @begin[Note]{dictionary}
    As of Cairo version 1.2.4 any call to the function @sym{cairo-close-path}
    will place an explicit @code{:move-to} element into the path immediately
    after the @code{:close-path} element, which can be seen in the function
    @fun{cairo-copy-path} for example. This can simplify path processing in some
    cases as it may not be necessary to save the \"last @code{:move-to} point\"
    during processing as the @code{:move-to} immediately after the
    @code{:close-path} will provide that point.
  @end{dictionary}
  @see-symbol{cairo-t}
  @see-function{cairo-move-to}
  @see-function{cairo-line-to}
  @see-function{cairo-copy-path}"
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
 "@version{2020-12-11}
  @argument[cr]{a @symbol{cairo-t} context}
  @argument[xc]{a double float x position of the center of the arc}
  @argument[yc]{a double float y position of the center of the arc}
  @argument[radius]{a double float with the radius of the arc}
  @argument[angle1]{a double float with the start angle, in radians}
  @argument[angle2]{a dobule float with the end angle, in radians}
  @begin{short}
    Adds a circular arc of the given @arg{radius} to the current path.
  @end{short}
  The arc is centered at @code{(@arg{xc}, @arg{yc})}, begins at @arg{angle1}
  and proceeds in the direction of increasing angles to end at @arg{angle2}. If
  @arg{angle2} is less than @arg{angle1} it will be progressively increased by
  2*PI until it is greater than @arg{angle1}.

  If there is a current point, an initial line segment will be added to the
  path to connect the current point to the beginning of the arc. If this
  initial line is undesired, it can be avoided by calling the function
  @fun{cairo-new-sub-path} before calling the function @sym{cairo-arc}.

  Angles are measured in radians. An angle of 0 is in the direction of the
  positive x axis (in user space). An angle of PI/2 radians (90 degrees) is in
  the direction of the positive y axis (in user space). Angles increase in the
  direction from the positive x axis toward the positive y axis. So with the
  default transformation matrix, angles increase in a clockwise direction.

  This function gives the arc in the direction of increasing angles. See the
  function @fun{cairo-arc-negative} to get the arc in the direction of
  decreasing angles.
  @begin[Example]{dictionary}
    The arc is circular in user space. To achieve an elliptical arc, you can
    scale the current transformation matrix by different amounts in the x and y
    directions. For example, to draw an ellipse in the box given by @arg{x},
    @arg{y}, @arg{width}, @arg{height}:
    @begin{pre}
(cairo-save cr)
(cairo-translate cr (+ x (/ width 2)) (+ y (/ height 2)))
(cairo-scale cr (/ width 2) (/ height 2))
(cairo-arc cr 0 0 1 0 (* 2 pi))
(cairo-restore cr)
    @end{pre}
  @end{dictionary}
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
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_arc_negative" %cairo-arc-negative) :void
  (cr (:pointer (:struct cairo-t)))
  (xc :double)
  (yc :double)
  (radius :double)
  (angle1 :double)
  (angle2 :double))

(defun cairo-arc-negative (cr xc yc radius angle1 angle2)
 #+cl-cffi-gtk-documentation
 "@version{2020-12-11}
  @argument[cr]{a @symbol{cairo-t} context}
  @argument[xc]{a double float x position of the center of the arc}
  @argument[yc]{a double float y position of the center of the arc}
  @argument[radius]{a double float with the radius of the arc}
  @argument[angle1]{a double float with the start angle, in radians}
  @argument[angle2]{a double float with the end angle, in radians}
  @begin{short}
    Adds a circular arc of the given @arg{radius} to the current path.
  @end{short}
  The arc is centered at (@arg{xc}, @arg{yc}), begins at @arg{angle1} and
  proceeds in the direction of decreasing angles to end at @arg{angle2}. If
  @arg{angle2} is greater than @arg{angle1} it will be progressively decreased
  by @code{2*PI} until it is less than @arg{angle1}.

  See the function @fun{cairo-arc} for more details. This function differs only
  in the direction of the arc between the two angles.
  @see-symbol{cairo-t}
  @see-function{cairo-arc}"
  (%cairo-arc-negative cr
                       (coerce xc 'double-float)
                       (coerce yc 'double-float)
                       (coerce radius 'double-float)
                       (coerce angle1 'double-float)
                       (coerce angle2 'double-float)))

(export 'cairo-arc-negative)

;;; ----------------------------------------------------------------------------
;;; cairo_curve_to ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_curve_to" %cairo-curve-to) :void
  (cr (:pointer (:struct cairo-t)))
  (x1 :double)
  (y1 :double)
  (x2 :double)
  (y2 :double)
  (x3 :double)
  (y3 :double))

(defun cairo-curve-to (cr x1 y1 x2 y2 x3 y3)
 #+cl-cffi-gtk-documentation
 "@version{2020-12-25}
  @argument[cr]{a @symbol{cairo-t} context}
  @argument[x1]{a double float x coordinate of the first control point}
  @argument[y1]{a double float y coordinate of the first control point}
  @argument[x2]{a double float x coordinate of the second control point}
  @argument[y2]{a double float y coordinate of the second control point}
  @argument[x3]{a double float x coordinate of the third control point}
  @argument[y3]{a double float x coordinate of the third control point}
  @begin{short}
    Adds a cubic Bezier spline to the path from the current point to position
    (@arg{x3}, @arg{y3}) in user-space coordinates, using (@arg{x1}, @arg{y1})
    and (@arg{x2}, @arg{y2}) as the control points.
  @end{short}
  After this call the current point will be (@arg{x3}, @arg{y3}).

  If there is no current point before the call to the function
  @sym{cairo-curve-to} this function will behave as if preceded by a call to:
  @begin{pre}
(cairo-move-to cr x1 y1)
  @end{pre}
  @see-class{cairo-t}
  @see-function{cairo-move-to}"
  (%cairo-curve-to cr
                   (coerce x1 'double-float)
                   (coerce y1 'double-float)
                   (coerce x2 'double-float)
                   (coerce y2 'double-float)
                   (coerce x3 'double-float)
                   (coerce y3 'double-float)))

(export 'cairo-curve-to)

;;; ----------------------------------------------------------------------------
;;; cairo_line_to ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_line_to" %cairo-line-to) :void
  (cr (:pointer (:struct cairo-t)))
  (x :double)
  (y :double))

(defun cairo-line-to (cr x y)
 #+cl-cffi-gtk-documentation
 "@version{2020-12-25}
  @argument[cr]{a @symbol{cairo-t} context}
  @argument[x]{a double float x coordinate of the end of the new line}
  @argument[y]{a double float y coordinate of the end of the new line}
  @begin{short}
    Adds a line to the path from the current point to position
   (@arg{x}, @arg{y}) in user-space coordinates.
  @end{short}
  After this call the current point will be (@arg{x}, @arg{y}).

  If there is no current point before the call to @sym{cairo-line-to} this
  function will behave as:
  @begin{pre}
(cairo-move-to cr x y)
  @end{pre}
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
 "@version{2020-12-11}
  @argument[cr]{a @symbol{cairo-t} context}
  @argument[x]{a double float x coordinate of the new position}
  @argument[y]{a double float y coordinate of the new position}
  @begin{short}
    Begin a new sub-path.
  @end{short}
  After this call the current point will be (@arg{x}, @arg{y}).
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
 "@version{2020-12-11}
  @argument[cr]{a @symbol{cairo-t} context}
  @argument[x]{a double float x coordinate of the top left corner of the
    rectangle}
  @argument[y]{a double float y coordinate to the top left corner of the
    rectangle}
  @argument[width]{a double float with the width of the rectangle}
  @argument[height]{a double float with the height of the rectangle}
  @begin{short}
    Adds a closed sub-path rectangle of the given size to the current path at
    position (@arg{x}, @arg{y}) in user-space coordinates.
  @end{short}

  This function is logically equivalent to:
  @begin{pre}
(cairo-move-to cr x y)
(cairo-rel-line-to cr width 0)
(cairo-rel-line-to cr 0 height)
(cairo-rel-line-to cr (- width) 0)
(cairo-close-path cr)
  @end{pre}
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
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_glyph_path" cairo-glyph-path) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-12-25}
  @argument[cr]{a @symbol{cairo-t} context}
  @argument[glyphs]{array of @symbol{cairo-glyphs-t} glyphs to show}
  @argument[num-glyphs]{an integer with the number of glyphs to show}
  @begin{short}
    Adds closed paths for the glyphs to the current path.
  @end{short}
  The generated path if filled, achieves an effect similar to that of the
  function @fun{cairo-show-glyphs}.
  @see-symbol{cairo-t}
  @see-symbol{cairo-glyph-t}
  @see-function{cairo-show-glyphs}"
  (cr (:pointer (:struct cairo-t)))
  (glyphs (:pointer (:pointer (:struct cairo-glyph-t))))
  (num-glyphs :int))

(export 'cairo-glyph-path)

;;; ----------------------------------------------------------------------------
;;; cairo_text_path ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_text_path" cairo-text-path) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-12-25}
  @argument[cr]{a @symbol{cairo-t} context}
  @argument[utf8]{a string of text encoded in UTF-8, or @code{nil}}
  @begin{short}
    Adds closed paths for text to the current path.
  @end{short}
  The generated path if filled, achieves an effect similar to that of the
  function @fun{cairo-show-text}.

  Text conversion and positioning is done similar to the function
  @fun{cairo-show-text}.

  Like the function @fun{cairo-show-text}, after this call the current point is
  moved to the origin of where the next glyph would be placed in this same
  progression. That is, the current point will be at the origin of the final
  glyph offset by its advance values. This allows for chaining multiple calls
  to to the function @sym{cairo-text-path} without having to set current point
  in between.
  @begin[Note]{dictionary}
    The function @sym{cairo-text-path} call is part of what the Cairo designers
    call the \"toy\" text API. It is convenient for short demos and simple
    programs, but it is not expected to be adequate for serious text-using
    applications. See the function @fun{cairo-glyph-path} for the \"real\" text
    path API in Cairo.
  @end{dictionary}
  @see-symbol{cario-t}
  @see-function{cairo-show-text}
  @see-function{cairo-glyph-path}"
  (cr (:pointer (:struct cairo-t)))
  (uft8 :string))

(export 'cairo-text-path)

;;; ----------------------------------------------------------------------------
;;; cairo_rel_curve_to ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_rel_curve_to" %cairo-rel-curve-to) :void
  (cr (:pointer (:struct cairo-t)))
  (dx1 :double)
  (dy1 :double)
  (dx2 :double)
  (dy2 :double)
  (dx3 :double)
  (dy3 :double))

(defun cairo-rel-curve-to (cr dx1 dy1 dx2 dy2 dx3 dy3)
 #+cl-cffi-gtk-documentation
 "@version{2020-12-11}
  @argument[cr]{a @symbol{cairo-t} context}
  @argument[dx1]{a double float x offset to the first control point}
  @argument[dy1]{a double float y offset to the first control point}
  @argument[dx2]{a double float x offset to the second control point}
  @argument[dy2]{a double float y offset to the second control point}
  @argument[dx3]{a double float x offset to the end of the curve}
  @argument[dy3]{a double float y offset to the end of the curve}
  @begin{short}
    Relative-coordinate version of the function @fun{cairo-curve-to}.
  @end{short}
  All offsets are relative to the current point. Adds a cubic Bézier spline to
  the path from the current point to a point offset from the current point by
  (@arg{dx3}, @arg{dy3}), using points offset by (@arg{dx1}, @arg{dy1}) and
  (@arg{dx2}, @arg{dy2}) as the control points. After this call the current
  point will be offset by (@arg{dx3}, @arg{dy3}).

  Given a current point of (@arg{x}, @arg{y}),
  @begin{pre}
(cairo-rel-curve-to cr dx1 dy1 dx2 dy2 dx3 dy3)
  @end{pre}
  is logically equivalent to
  @begin{pre}
(cairo-curve-to cr (+ x dx1) (+ y dy1)
                   (+ x dx2) (+ y dy2)
                   (+ x dx3) (+ y dy3))
  @end{pre}
  It is an error to call this function with no current point. Doing so will
  cause @arg{cr} to shutdown with a status of @code{:no-current-point}.
  @see-symbol{cairo-t}
  @see-function{cairo-curve-to}"
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
 "@version{2020-12-11}
  @argument[cr]{a @symbol{cairo-t} context}
  @argument[dx]{a double float x offset to the end of the new line}
  @argument[dy]{a double float y offset to the end of the new line}
  @begin{short}
    Relative-coordinate version of the function @fun{cairo-line-to}.
  @end{short}
  Adds a line to the path from the current point to a point that is offset from
  the current point by (@arg{dx}, @arg{dy}) in user space. After this call the
  current point will be offset by (@arg{dx}, @arg{dy}).

  Given a current point of (x, y),
  @begin{pre}
(cairo-rel-line-to cr dx dy)
  @end{pre}
  is logically equivalent to
  @begin{pre}
(cairo-line-to cr (+ x dx) (+ y dy))
  @end{pre}
  It is an error to call this function with no current point. Doing so will
  cause @arg{cr} to shutdown with a status of @code{:no-current-point}.
  @see-symbol{cairo-t}
  @see-function{cairo-line-to}"
  (%cairo-rel-line-to cr (coerce dx 'double-float) (coerce dy 'double-float)))

(export 'cairo-rel-line-to)

;;; ----------------------------------------------------------------------------
;;; cairo_rel_move_to ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_rel_move_to" %cairo-rel-move-to) :void
  (cr (:pointer (:struct cairo-t)))
  (dx :double)
  (dy :double))

(defun cairo-rel-move-to (cr dx dy)
 #+cl-cffi-gtk-documentation
 "@version{2020-12-11}
  @argument[cr]{a @symbol{cairo-t} context}
  @argument[dx]{a double float x offset}
  @argument[dy]{a double float y offset}
  @begin{short}
    Begin a new sub-path.
  @end{short}
  After this call the current point will offset by (x, y).

  Given a current point of (x, y),
  @begin{pre}
(cairo-rel-move-to cr dx dy)
  @end{pre}
  is logically equivalent to
  @begin{pre}
(cairo-move-to cr (+ x dx) (+ y dy))
  @end{pre}
  It is an error to call this function with no current point. Doing so will
  cause @arg{cr} to shutdown with a status of @code{:no-current-point}.
  @see-class{cairo-t}
  @see-function{cairo-move-to}"
  (%cairo-rel-move-to cr (coerce dx 'double-float) (coerce dy 'double-float)))

(export 'cairo-rel-move-to)

;;; ----------------------------------------------------------------------------
;;; cairo_path_extents ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_path_extents" %cairo-path-extents) :void
  (cr (:pointer (:struct cairo-t)))
  (x1 (:pointer :double))
  (y1 (:pointer :double))
  (x2 (:pointer :double))
  (y2 (:pointer :double)))

(defun cairo-path-extents (cr)
 #+cl-cffi-gtk-documentation
 "@version{2020-12-25}
  @argument[cr]{a @symbol{cairo-t} context}
  @begin{return}
    @code{x1} -- a double float for the left of the resulting extents @br{}
    @code{y1} -- a double float for the top of the resulting extents @br{}
    @code{x2} -- a double float for the right of the resulting extents @br{}
    @code{y2} -- a double float for the bottom of the resulting extents
  @end{return}
  @begin{short}
    Computes a bounding box in user-space coordinates covering the points on
    the current path.
  @end{short}
  If the current path is empty, returns an empty rectangle ((0,0), (0,0)).
  Stroke parameters, fill rule, surface dimensions and clipping are not taken
  into account.

  Contrast with the functions @fun{cairo-fill-extents} and
  @fun{cairo-stroke-extents} which return the extents of only the area that
  would be \"inked\" by the corresponding drawing operations.

  The result of the function @sym{cairo-path-extents} is defined as equivalent
  to the limit of the function @fun{cairo-stroke-extents} with @code{:round} as
  the line width approaches 0.0, but never reaching the empty-rectangle
  returned by the function @fun{cairo-stroke-extents} for a line width of 0.0.

  Specifically, this means that zero-area sub-paths such as @fun{cairo-move-to};
  @fun{cairo-line-to} segments, even degenerate cases where the coordinates to
  both calls are identical, will be considered as contributing to the extents.
  However, a lone @fun{cairo-move-to} will not contribute to the results of
  the function @sym{cairo-path-extents}.
  @see-symbol{cairo-t}
  @see-function{cairo-fill-extents}
  @see-function{cairo-stroke-extents}
  @see-function{cairo-move-to}
  @see-function{cairo-line-to}"
  (with-foreign-objects ((x1 :double) (y1 :double) (x2 :double) (y2 :double))
    (%cairo-path-extents cr x1 y1 x2 y2)
    (values (mem-ref x1 :double)
            (mem-ref y1 :double)
            (mem-ref x2 :double)
            (mem-ref y2 :double))))

(export 'cairo-path-extents)

;;; --- End of file cairo.paths.lisp -------------------------------------------
