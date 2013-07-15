;;; ----------------------------------------------------------------------------
;;; cairo.context.lisp
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
;;; cairo_t
;;;
;;; The cairo drawing context
;;;
;;; Synopsis
;;;
;;;     cairo_t
;;;
;;;     cairo_create
;;;     cairo_reference
;;;     cairo_destroy
;;;     cairo_status
;;;     cairo_save
;;;     cairo_restore
;;;     cairo_get_target
;;;     cairo_push_group
;;;     cairo_push_group_with_content
;;;     cairo_pop_group
;;;     cairo_pop_group_to_source
;;;     cairo_get_group_target
;;;     cairo_set_source_rgb
;;;     cairo_set_source_rgba
;;;     cairo_set_source
;;;     cairo_set_source_surface
;;;     cairo_get_source
;;;
;;;     cairo_antialias_t
;;;
;;;     cairo_set_antialias
;;;     cairo_get_antialias
;;;     cairo_set_dash
;;;     cairo_get_dash_count
;;;     cairo_get_dash
;;;
;;;     cairo_fill_rule_t
;;;
;;;     cairo_set_fill_rule
;;;     cairo_get_fill_rule
;;;
;;;     cairo_line_cap_t
;;;
;;;     cairo_set_line_cap
;;;     cairo_get_line_cap
;;;
;;;     cairo_line_join_t
;;;
;;;     cairo_set_line_join
;;;     cairo_get_line_join
;;;     cairo_set_line_width
;;;     cairo_get_line_width
;;;     cairo_set_miter_limit
;;;     cairo_get_miter_limit
;;;
;;;     cairo_operator_t
;;;
;;;     cairo_set_operator
;;;     cairo_get_operator
;;;     cairo_set_tolerance
;;;     cairo_get_tolerance
;;;     cairo_clip
;;;     cairo_clip_preserve
;;;     cairo_clip_extents
;;;     cairo_in_clip
;;;     cairo_reset_clip
;;;
;;;     cairo_rectangle_t
;;;     cairo_rectangle_list_t
;;;
;;;     cairo_rectangle_list_destroy
;;;     cairo_copy_clip_rectangle_list
;;;     cairo_fill
;;;     cairo_fill_preserve
;;;     cairo_fill_extents
;;;     cairo_in_fill
;;;     cairo_mask
;;;     cairo_mask_surface
;;;     cairo_paint
;;;     cairo_paint_with_alpha
;;;     cairo_stroke
;;;     cairo_stroke_preserve
;;;     cairo_stroke_extents
;;;     cairo_in_stroke
;;;     cairo_copy_page
;;;     cairo_show_page
;;;     cairo_get_reference_count
;;;     cairo_set_user_data
;;;     cairo_get_user_data
;;;
;;; Description
;;;
;;; cairo_t is the main object used when drawing with cairo. To draw with cairo,
;;; you create a cairo_t, set the target surface, and drawing options for the
;;; cairo_t, create shapes with functions like cairo_move_to() and
;;; cairo_line_to(), and then draw shapes with cairo_stroke() or cairo_fill().
;;;
;;; cairo_t's can be pushed to a stack via cairo_save(). They may then safely be
;;; changed, without losing the current state. Use cairo_restore() to restore to
;;; the saved state.
;;; ----------------------------------------------------------------------------

(in-package :cairo)

;;; ----------------------------------------------------------------------------
;;; cairo_t
;;; ----------------------------------------------------------------------------

(defcstruct cairo-t)

#+cl-cffi-gtk-documentation
(setf (gethash 'cairo-t atdoc:*symbol-name-alias*) "CStruct"
      (gethash 'cairo-t atdoc:*external-symbols*)
 "@version{2013-6-22}
  @begin{short}
    A @sym{cairo-t} contains the current state of the rendering device,
    including coordinates of yet to be drawn shapes.
  @end{short}

  Cairo contexts, as @sym{cairo-t} objects are named, are central to Cairo and
  all drawing with Cairo is always done to a @sym{cairo-t} object.

  Memory management of @sym{cairo-t} is done with the functions
  @fun{cairo-reference} and @fun{cairo-destroy}.

  Since 1.0
  @see-function{cairo-destroy}
  @see-function{cairo-reference}")

(export 'cairo-t)

;;; ----------------------------------------------------------------------------
;;; cairo_create ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_create" cairo-create) (:pointer (:struct cairo-t))
 #+cl-cffi-gtk-documentation
 "@version{2012-12-21}
  @argument[target]{target surface for the context}
  @begin{return}
    a newly allocated cairo_t with a reference count of 1. The initial
    reference count should be released with cairo_destroy() when you are
    done using the cairo_t. This function never returns NULL. If memory
    cannot be allocated, a special cairo_t object will be returned on which
    cairo_status() returns CAIRO_STATUS_NO_MEMORY. If you attempt to target
    a surface which does not support writing (such as cairo_mime_surface_t)
    then a CAIRO_STATUS_WRITE_ERROR will be raised. You can use this object
    normally, but no drawing will be done.
  @end{return}
  @begin{short}
    Creates a new cairo_t with all graphics state parameters set to default
    values and with target as a target surface.
  @end{short}
  The target surface should be
  constructed with a backend-specific function such as
  cairo_image_surface_create() (or any other cairo_backend_surface_create()
  variant).

  This function references target, so you can immediately call
  cairo_surface_destroy() on it if you don't need to maintain a separate
  reference to it.

  Since 1.0"
  (target (:pointer (:struct cairo-surface-t))))

(export 'cairo-create)

;;; ----------------------------------------------------------------------------
;;; cairo_reference ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_reference" cairo-reference) (:pointer (:struct cairo-t))
 #+cl-cffi-gtk-documentation
 "@version{2013-3-2}
  @argument[cr]{a cairo_t}
  @return{the referenced cairo_t.}
  @begin{short}
    Increases the reference count on cr by one. This prevents cr from being
    destroyed until a matching call to cairo_destroy() is made.
  @end{short}

  The number of references to a cairo_t can be get using
  cairo_get_reference_count().

  Since 1.0"
  (cr (:pointer (:struct cairo-t))))

(export 'cairo-reference)

;;; ----------------------------------------------------------------------------
;;; cairo_destroy ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_destroy" cairo-destroy) :void
 #+cl-cffi-gtk-documentation
 "@version{2012-12-21}
  @argument[cr]{a @symbol{cairo-t}}
  @begin{short}
    Decreases the reference count on @arg{cr} by one. If the result is zero,
    then @arg{cr} and all associated resources are freed. See
    @code{cairo_reference()}.
  @end{short}

  Since 1.0"
  (cr (:pointer (:struct cairo-t))))

(export 'cairo-destroy)

;;; ----------------------------------------------------------------------------
;;; cairo_status ()
;;;
;;; cairo_status_t cairo_status (cairo_t *cr);
;;;
;;; Checks whether an error has previously occurred for this context.
;;;
;;; cr :
;;;     a cairo context
;;;
;;; Returns :
;;;     the current status of this context, see cairo_status_t
;;;
;;; Since 1.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_save ()
;;;
;;; void cairo_save (cairo_t *cr);
;;;
;;; Makes a copy of the current state of cr and saves it on an internal stack of
;;; saved states for cr. When cairo_restore() is called, cr will be restored to
;;; the saved state. Multiple calls to cairo_save() and cairo_restore() can be
;;; nested; each call to cairo_restore() restores the state from the matching
;;; paired cairo_save().
;;;
;;; It isn't necessary to clear all saved states before a cairo_t is freed. If
;;; the reference count of a cairo_t drops to zero in response to a call to
;;; cairo_destroy(), any saved states will be freed along with the cairo_t.
;;;
;;; cr :
;;;     a cairo_t
;;;
;;; Since 1.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_restore ()
;;;
;;; void cairo_restore (cairo_t *cr);
;;;
;;; Restores cr to the state saved by a preceding call to cairo_save() and
;;; removes that state from the stack of saved states.
;;;
;;; cr :
;;;     a cairo_t
;;;
;;; Since 1.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_get_target ()
;;;
;;; cairo_surface_t * cairo_get_target (cairo_t *cr);
;;;
;;; Gets the target surface for the cairo context as passed to cairo_create().
;;;
;;; This function will always return a valid pointer, but the result can be a
;;; "nil" surface if cr is already in an error state, (ie.
;;; cairo_status() != CAIRO_STATUS_SUCCESS). A nil surface is indicated by
;;; cairo_surface_status() != CAIRO_STATUS_SUCCESS.
;;;
;;; cr :
;;;     a cairo context
;;;
;;; Returns :
;;;     the target surface. This object is owned by cairo. To keep a reference
;;;     to it, you must call cairo_surface_reference().
;;;
;;; Since 1.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_push_group ()
;;;
;;; void cairo_push_group (cairo_t *cr);
;;;
;;; Temporarily redirects drawing to an intermediate surface known as a group.
;;; The redirection lasts until the group is completed by a call to
;;; cairo_pop_group() or cairo_pop_group_to_source(). These calls provide the
;;; result of any drawing to the group as a pattern, (either as an explicit
;;; object, or set as the source pattern).
;;;
;;; This group functionality can be convenient for performing intermediate
;;; compositing. One common use of a group is to render objects as opaque within
;;; the group, (so that they occlude each other), and then blend the result with
;;; translucence onto the destination.
;;;
;;; Groups can be nested arbitrarily deep by making balanced calls to
;;; cairo_push_group()/cairo_pop_group(). Each call pushes/pops the new target
;;; group onto/from a stack.
;;;
;;; The cairo_push_group() function calls cairo_save() so that any changes to
;;; the graphics state will not be visible outside the group, (the pop_group
;;; functions call cairo_restore()).
;;;
;;; By default the intermediate group will have a content type of
;;; CAIRO_CONTENT_COLOR_ALPHA. Other content types can be chosen for the group
;;; by using cairo_push_group_with_content() instead.
;;;
;;; As an example, here is how one might fill and stroke a path with
;;; translucence, but without any portion of the fill being visible under the
;;; stroke:
;;;
;;; cairo_push_group (cr);
;;; cairo_set_source (cr, fill_pattern);
;;; cairo_fill_preserve (cr);
;;; cairo_set_source (cr, stroke_pattern);
;;; cairo_stroke (cr);
;;; cairo_pop_group_to_source (cr);
;;; cairo_paint_with_alpha (cr, alpha);
;;;
;;; cr :
;;;     a cairo context
;;;
;;; Since 1.2
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_push_group_with_content ()
;;;
;;; void cairo_push_group_with_content (cairo_t *cr, cairo_content_t content);
;;;
;;; Temporarily redirects drawing to an intermediate surface known as a group.
;;; The redirection lasts until the group is completed by a call to
;;; cairo_pop_group() or cairo_pop_group_to_source(). These calls provide the
;;; result of any drawing to the group as a pattern, (either as an explicit
;;; object, or set as the source pattern).
;;;
;;; The group will have a content type of content. The ability to control this
;;; content type is the only distinction between this function and
;;; cairo_push_group() which you should see for a more detailed description of
;;; group rendering.
;;;
;;; cr :
;;;     a cairo context
;;;
;;; content :
;;;     a cairo_content_t indicating the type of group that will be created
;;;
;;; Since 1.2
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_pop_group ()
;;;
;;; cairo_pattern_t * cairo_pop_group (cairo_t *cr);
;;;
;;; Terminates the redirection begun by a call to cairo_push_group() or
;;; cairo_push_group_with_content() and returns a new pattern containing the
;;; results of all drawing operations performed to the group.
;;;
;;; The cairo_pop_group() function calls cairo_restore(), (balancing a call to
;;; cairo_save() by the push_group function), so that any changes to the
;;; graphics state will not be visible outside the group.
;;;
;;; cr :
;;;     a cairo context
;;;
;;; Returns :
;;;     a newly created (surface) pattern containing the results of all drawing
;;;     operations performed to the group. The caller owns the returned object
;;;     and should call cairo_pattern_destroy() when finished with it.
;;;
;;; Since 1.2
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_pop_group_to_source ()
;;;
;;; void cairo_pop_group_to_source (cairo_t *cr);
;;;
;;; Terminates the redirection begun by a call to cairo_push_group() or
;;; cairo_push_group_with_content() and installs the resulting pattern as the
;;; source pattern in the given cairo context.
;;;
;;; The behavior of this function is equivalent to the sequence of operations:
;;;
;;; cairo_pattern_t *group = cairo_pop_group (cr);
;;; cairo_set_source (cr, group);
;;; cairo_pattern_destroy (group);
;;;
;;; but is more convenient as their is no need for a variable to store the
;;; short-lived pointer to the pattern.
;;;
;;; The cairo_pop_group() function calls cairo_restore(), (balancing a call to
;;; cairo_save() by the push_group function), so that any changes to the
;;; graphics state will not be visible outside the group.
;;;
;;; cr :
;;;     a cairo context
;;;
;;; Since 1.2
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_get_group_target ()
;;;
;;; cairo_surface_t * cairo_get_group_target (cairo_t *cr);
;;;
;;; Gets the current destination surface for the context. This is either the
;;; original target surface as passed to cairo_create() or the target surface
;;; for the current group as started by the most recent call to
;;; cairo_push_group() or cairo_push_group_with_content().
;;;
;;; This function will always return a valid pointer, but the result can be a
;;; "nil" surface if cr is already in an error state, (ie.
;;; cairo_status() != CAIRO_STATUS_SUCCESS). A nil surface is indicated by
;;; cairo_surface_status() != CAIRO_STATUS_SUCCESS.
;;;
;;; cr :
;;;     a cairo context
;;;
;;; Returns :
;;;     the target surface. This object is owned by cairo. To keep a reference
;;;     to it, you must call cairo_surface_reference().
;;;
;;; Since 1.2
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_set_source_rgb ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_set_source_rgb" cairo-set-source-rgb) :void
 #+cl-cffi-gtk-documentation
 "@version{2012-12-21}
  @argument[cr]{a cairo context}
  @argument[red]{red component of color}
  @argument[green]{green component of color}
  @argument[blue]{blue component of color}
  @begin{short}
    Sets the source pattern within @arg{cr} to an opaque color. This opaque
    color will then be used for any subsequent drawing operation until a new
    source pattern is set.
  @end{short}

  The color components are floating point numbers in the range @code{0.0} to
  @code{1.0}. If the values passed in are outside that range, they will be
  clamped.

  The default source pattern is opaque black, (that is, it is equivalent to
  @code{(cairo-set-source-rgb cr 0.0 0.0 0.0)}).

 Since 1.0"
  (cr (:pointer (:struct cairo-t)))
  (red :double)
  (green :double)
  (blue :double))

(export 'cairo-set-source-rgb)

;;; ----------------------------------------------------------------------------
;;; cairo_set_source_rgba ()
;;;
;;; void cairo_set_source_rgba (cairo_t *cr,
;;;                             double red,
;;;                             double green,
;;;                             double blue,
;;;                             double alpha);
;;;
;;; Sets the source pattern within cr to a translucent color. This color will
;;; then be used for any subsequent drawing operation until a new source pattern
;;; is set.
;;;
;;; The color and alpha components are floating point numbers in the range 0 to
;;; 1. If the values passed in are outside that range, they will be clamped.
;;;
;;; The default source pattern is opaque black, (that is, it is equivalent to
;;; cairo_set_source_rgba(cr, 0.0, 0.0, 0.0, 1.0)).
;;;
;;; cr :
;;;     a cairo context
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

;;; ----------------------------------------------------------------------------
;;; cairo_set_source ()
;;;
;;; void cairo_set_source (cairo_t *cr, cairo_pattern_t *source);
;;;
;;; Sets the source pattern within cr to source. This pattern will then be used
;;; for any subsequent drawing operation until a new source pattern is set.
;;;
;;; Note: The pattern's transformation matrix will be locked to the user space
;;; in effect at the time of cairo_set_source(). This means that further
;;; modifications of the current transformation matrix will not affect the
;;; source pattern. See cairo_pattern_set_matrix().
;;;
;;; The default source pattern is a solid pattern that is opaque black, (that
;;; is, it is equivalent to cairo_set_source_rgb(cr, 0.0, 0.0, 0.0)).
;;;
;;; cr :
;;;     a cairo context
;;;
;;; source :
;;;     a cairo_pattern_t to be used as the source for subsequent drawing
;;;     operations.
;;;
;;; Since 1.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_set_source_surface ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_set_source_surface" cairo-set-source-surface) :void
 #+cl-cffi-gtk-documentation
 "@version{2012-12-21}
  @argument[cr]{a cairo context}
  @argument[surface]{a surface to be used to set the source pattern}
  @argument[x]{User-space X coordinate for surface origin}
  @argument[y]{User-space Y coordinate for surface origin}
  @begin{short}
    This is a convenience function for creating a pattern from surface and
    setting it as the source in @arg{cr} with @code{cairo_set_source()}.
  @end{short}

  The @arg{x} and @arg{y} parameters give the user-space coordinate at which the
  surface origin should appear. (The surface origin is its upper-left corner
  before any transformation has been applied.) The @arg{x} and @arg{y}
  parameters are negated and then set as translation values in the pattern
  matrix.

  Other than the initial translation pattern matrix, as described above, all
  other pattern attributes, (such as its extend mode), are set to the default
  values as in @code{cairo_pattern_create_for_surface()}. The resulting pattern
  can be queried with @code{cairo_get_source()} so that these attributes can be
  modified if desired, (eg. to create a repeating pattern with
  @code{cairo_pattern_set_extend()}).

  Since 1.0"
  (cr (:pointer (:struct cairo-t)))
  (surface (:pointer (:struct cairo-surface-t)))
  (x :double)
  (y :double))

(export 'cairo-set-source-surface)

;;; ----------------------------------------------------------------------------
;;; cairo_get_source ()
;;;
;;; cairo_pattern_t * cairo_get_source (cairo_t *cr);
;;;
;;; Gets the current source pattern for cr.
;;;
;;; cr :
;;;     a cairo context
;;;
;;; Returns :
;;;     the current source pattern. This object is owned by cairo. To keep a
;;;     reference to it, you must call cairo_pattern_reference().
;;;
;;; Since 1.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; enum cairo_antialias_t
;;;
;;; typedef enum {
;;;     CAIRO_ANTIALIAS_DEFAULT,
;;;
;;;     /* method */
;;;     CAIRO_ANTIALIAS_NONE,
;;;     CAIRO_ANTIALIAS_GRAY,
;;;     CAIRO_ANTIALIAS_SUBPIXEL,
;;;
;;;     /* hints */
;;;     CAIRO_ANTIALIAS_FAST,
;;;     CAIRO_ANTIALIAS_GOOD,
;;;     CAIRO_ANTIALIAS_BEST
;;; } cairo_antialias_t;
;;;
;;; Specifies the type of antialiasing to do when rendering text or shapes.
;;;
;;; As it is not necessarily clear from the above what advantages a particular
;;; antialias method provides, since 1.12, there is also a set of hints:
;;;
;;; CAIRO_ANTIALIAS_FAST: Allow the backend to degrade raster quality for speed
;;; CAIRO_ANTIALIAS_GOOD: A balance between speed and quality
;;; CAIRO_ANTIALIAS_BEST: A high-fidelity, but potentially slow, raster mode
;;;
;;; These make no guarantee on how the backend will perform its rasterisation
;;; (if it even rasterises!), nor that they have any differing effect other than
;;; to enable some form of antialiasing. In the case of glyph rendering,
;;; CAIRO_ANTIALIAS_FAST and CAIRO_ANTIALIAS_GOOD will be mapped to
;;; CAIRO_ANTIALIAS_GRAY, with CAIRO_ANTALIAS_BEST being equivalent to
;;; CAIRO_ANTIALIAS_SUBPIXEL.
;;;
;;; The interpretation of CAIRO_ANTIALIAS_DEFAULT is left entirely up to the
;;; backend, typically this will be similar to CAIRO_ANTIALIAS_GOOD.
;;;
;;; CAIRO_ANTIALIAS_DEFAULT
;;;     Use the default antialiasing for the subsystem and target device, since
;;;     1.0
;;;
;;; CAIRO_ANTIALIAS_NONE
;;;     Use a bilevel alpha mask, since 1.0
;;;
;;; CAIRO_ANTIALIAS_GRAY
;;;     Perform single-color antialiasing (using shades of gray for black text
;;;     on a white background, for example), since 1.0
;;;
;;; CAIRO_ANTIALIAS_SUBPIXEL
;;;     Perform antialiasing by taking advantage of the order of subpixel
;;;     elements on devices such as LCD panels, since 1.0
;;;
;;; CAIRO_ANTIALIAS_FAST
;;;     Hint that the backend should perform some antialiasing but prefer speed
;;;     over quality, since 1.12
;;;
;;; CAIRO_ANTIALIAS_GOOD
;;;     The backend should balance quality against performance, since 1.12
;;;
;;; CAIRO_ANTIALIAS_BEST
;;;     Hint that the backend should render at the highest quality, sacrificing
;;;     speed if necessary, since 1.12
;;;
;;; Since 1.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_set_antialias ()
;;;
;;; void cairo_set_antialias (cairo_t *cr, cairo_antialias_t antialias);
;;;
;;; Set the antialiasing mode of the rasterizer used for drawing shapes. This
;;; value is a hint, and a particular backend may or may not support a
;;; particular value. At the current time, no backend supports
;;; CAIRO_ANTIALIAS_SUBPIXEL when drawing shapes.
;;;
;;; Note that this option does not affect text rendering, instead see
;;; cairo_font_options_set_antialias().
;;;
;;; cr :
;;;     a cairo_t
;;;
;;; antialias :
;;;     the new antialiasing mode
;;;
;;; Since 1.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_get_antialias ()
;;;
;;; cairo_antialias_t cairo_get_antialias (cairo_t *cr);
;;;
;;; Gets the current shape antialiasing mode, as set by cairo_set_antialias().
;;;
;;; cr :
;;;     a cairo context
;;;
;;; Returns :
;;;     the current shape antialiasing mode.
;;;
;;; Since 1.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_set_dash ()
;;;
;;; void cairo_set_dash (cairo_t *cr,
;;;                      const double *dashes,
;;;                      int num_dashes,
;;;                      double offset);
;;;
;;; Sets the dash pattern to be used by cairo_stroke(). A dash pattern is
;;; specified by dashes, an array of positive values. Each value provides the
;;; length of alternate "on" and "off" portions of the stroke. The offset
;;; specifies an offset into the pattern at which the stroke begins.
;;;
;;; Each "on" segment will have caps applied as if the segment were a separate
;;; sub-path. In particular, it is valid to use an "on" length of 0.0 with
;;; CAIRO_LINE_CAP_ROUND or CAIRO_LINE_CAP_SQUARE in order to distributed dots
;;; or squares along a path.
;;;
;;; Note: The length values are in user-space units as evaluated at the time of
;;; stroking. This is not necessarily the same as the user space at the time of
;;; cairo_set_dash().
;;;
;;; If num_dashes is 0 dashing is disabled.
;;;
;;; If num_dashes is 1 a symmetric pattern is assumed with alternating on and
;;; off portions of the size specified by the single value in dashes.
;;;
;;; If any value in dashes is negative, or if all values are 0, then cr will be
;;; put into an error state with a status of CAIRO_STATUS_INVALID_DASH.
;;;
;;; cr :
;;;     a cairo context
;;;
;;; dashes :
;;;     an array specifying alternate lengths of on and off stroke portions
;;;
;;; num_dashes :
;;;     the length of the dashes array
;;;
;;; offset :
;;;     an offset into the dash pattern at which the stroke should start
;;;
;;; Since 1.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_get_dash_count ()
;;;
;;; int cairo_get_dash_count (cairo_t *cr);
;;;
;;; This function returns the length of the dash array in cr (0 if dashing is
;;; not currently in effect).
;;;
;;; See also cairo_set_dash() and cairo_get_dash().
;;;
;;; cr :
;;;     a cairo_t
;;;
;;; Returns :
;;;     the length of the dash array, or 0 if no dash array set.
;;;
;;; Since 1.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_get_dash ()
;;;
;;; void cairo_get_dash (cairo_t *cr, double *dashes, double *offset);
;;;
;;; Gets the current dash array. If not NULL, dashes should be big enough to
;;; hold at least the number of values returned by cairo_get_dash_count().
;;;
;;; cr :
;;;     a cairo_t
;;;
;;; dashes :
;;;     return value for the dash array, or NULL
;;;
;;; offset :
;;;     return value for the current dash offset, or NULL
;;;
;;; Since 1.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; enum cairo_fill_rule_t
;;;
;;; typedef enum {
;;;     CAIRO_FILL_RULE_WINDING,
;;;     CAIRO_FILL_RULE_EVEN_ODD
;;; } cairo_fill_rule_t;
;;;
;;; cairo_fill_rule_t is used to select how paths are filled. For both fill
;;; rules, whether or not a point is included in the fill is determined by
;;; taking a ray from that point to infinity and looking at intersections with
;;; the path. The ray can be in any direction, as long as it doesn't pass
;;; through the end point of a segment or have a tricky intersection such as
;;; intersecting tangent to the path. (Note that filling is not actually
;;; implemented in this way. This is just a description of the rule that is
;;; applied.)
;;;
;;; The default fill rule is CAIRO_FILL_RULE_WINDING.
;;;
;;; New entries may be added in future versions.
;;;
;;; CAIRO_FILL_RULE_WINDING
;;;     If the path crosses the ray from left-to-right, counts +1. If the path
;;;     crosses the ray from right to left, counts -1. (Left and right are
;;;     determined from the perspective of looking along the ray from the
;;;     starting point.) If the total count is non-zero, the point will be
;;;     filled. (Since 1.0)
;;;
;;; CAIRO_FILL_RULE_EVEN_ODD
;;;     Counts the total number of intersections, without regard to the
;;;     orientation of the contour. If the total number of intersections is odd,
;;;     the point will be filled. (Since 1.0)
;;;
;;; Since 1.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_set_fill_rule ()
;;;
;;; void cairo_set_fill_rule (cairo_t *cr, cairo_fill_rule_t fill_rule);
;;;
;;; Set the current fill rule within the cairo context. The fill rule is used to
;;; determine which regions are inside or outside a complex (potentially
;;; self-intersecting) path. The current fill rule affects both cairo_fill() and
;;; cairo_clip(). See cairo_fill_rule_t for details on the semantics of each
;;; available fill rule.
;;;
;;; The default fill rule is CAIRO_FILL_RULE_WINDING.
;;;
;;; cr :
;;;     a cairo_t
;;;
;;; fill_rule :
;;;     a fill rule, specified as a cairo_fill_rule_t
;;;
;;; Since 1.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_get_fill_rule ()
;;;
;;; cairo_fill_rule_t cairo_get_fill_rule (cairo_t *cr);
;;;
;;; Gets the current fill rule, as set by cairo_set_fill_rule().
;;;
;;; cr :
;;;     a cairo context
;;;
;;; Returns :
;;;     the current fill rule.
;;;
;;; Since 1.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; enum cairo_line_cap_t
;;;
;;; typedef enum {
;;;     CAIRO_LINE_CAP_BUTT,
;;;     CAIRO_LINE_CAP_ROUND,
;;;     CAIRO_LINE_CAP_SQUARE
;;; } cairo_line_cap_t;
;;;
;;; Specifies how to render the endpoints of the path when stroking.
;;;
;;; The default line cap style is CAIRO_LINE_CAP_BUTT.
;;;
;;; CAIRO_LINE_CAP_BUTT
;;;     start(stop) the line exactly at the start(end) point (Since 1.0)
;;;
;;; CAIRO_LINE_CAP_ROUND
;;;     use a round ending, the center of the circle is the end point
;;;     (Since 1.0)
;;;
;;; CAIRO_LINE_CAP_SQUARE
;;;     use squared ending, the center of the square is the end point
;;;     (Since 1.0)
;;;
;;; Since 1.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_set_line_cap ()
;;;
;;; void cairo_set_line_cap (cairo_t *cr, cairo_line_cap_t line_cap);
;;;
;;; Sets the current line cap style within the cairo context. See
;;; cairo_line_cap_t for details about how the available line cap styles are
;;; drawn.
;;;
;;; As with the other stroke parameters, the current line cap style is examined
;;; by cairo_stroke(), cairo_stroke_extents(), and cairo_stroke_to_path(), but
;;; does not have any effect during path construction.
;;;
;;; The default line cap style is CAIRO_LINE_CAP_BUTT.
;;;
;;; cr :
;;;     a cairo context
;;;
;;; line_cap :
;;;     a line cap style
;;;
;;; Since 1.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_get_line_cap ()
;;;
;;; cairo_line_cap_t cairo_get_line_cap (cairo_t *cr);
;;;
;;; Gets the current line cap style, as set by cairo_set_line_cap().
;;;
;;; cr :
;;;     a cairo context
;;;
;;; Returns :
;;;     the current line cap style.
;;;
;;; Since 1.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; enum cairo_line_join_t
;;;
;;; typedef enum {
;;;     CAIRO_LINE_JOIN_MITER,
;;;     CAIRO_LINE_JOIN_ROUND,
;;;     CAIRO_LINE_JOIN_BEVEL
;;; } cairo_line_join_t;
;;;
;;; Specifies how to render the junction of two lines when stroking.
;;;
;;; The default line join style is CAIRO_LINE_JOIN_MITER.
;;;
;;; CAIRO_LINE_JOIN_MITER
;;;     use a sharp (angled) corner, see cairo_set_miter_limit() (Since 1.0)
;;;
;;; CAIRO_LINE_JOIN_ROUND
;;;     use a rounded join, the center of the circle is the joint point
;;;     (Since 1.0)
;;;
;;; CAIRO_LINE_JOIN_BEVEL
;;;     use a cut-off join, the join is cut off at half the line width from the
;;;     joint point (Since 1.0)
;;;
;;; Since 1.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_set_line_join ()
;;;
;;; void cairo_set_line_join (cairo_t *cr, cairo_line_join_t line_join);
;;;
;;; Sets the current line join style within the cairo context. See
;;; cairo_line_join_t for details about how the available line join styles are
;;; drawn.
;;;
;;; As with the other stroke parameters, the current line join style is examined
;;; by cairo_stroke(), cairo_stroke_extents(), and cairo_stroke_to_path(), but
;;; does not have any effect during path construction.
;;;
;;; The default line join style is CAIRO_LINE_JOIN_MITER.
;;;
;;; cr :
;;;     a cairo context
;;;
;;; line_join :
;;;     a line join style
;;;
;;; Since 1.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_get_line_join ()
;;;
;;; cairo_line_join_t cairo_get_line_join (cairo_t *cr);
;;;
;;; Gets the current line join style, as set by cairo_set_line_join().
;;;
;;; cr :
;;;     a cairo context
;;;
;;; Returns :
;;;     the current line join style.
;;;
;;; Since 1.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_set_line_width ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_set_line_width" %cairo-set-line-width) :void
  (cr (:pointer (:struct cairo-t)))
  (width :double))

(defun cairo-set-line-width (cr width)
 #+cl-cffi-gtk-documentation
 "@version{2013-3-2}
  @argument[cr]{a cairo_t}
  @argument[width]{a line width}
  @begin{short}
    Sets the current line width within the cairo context. The line width value
    specifies the diameter of a pen that is circular in user space, (though
    device-space pen may be an ellipse in general due to scaling/shear/rotation
    of the CTM).
  @end{short}

  Note: When the description above refers to user space and CTM it refers to
  the user space and CTM in effect at the time of the stroking operation, not
  the user space and CTM in effect at the time of the call to
  cairo_set_line_width(). The simplest usage makes both of these spaces
  identical. That is, if there is no change to the CTM between a call to
  cairo_set_line_width() and the stroking operation, then one can just pass
  user-space values to cairo_set_line_width() and ignore this note.

  As with the other stroke parameters, the current line width is examined by
  cairo_stroke(), cairo_stroke_extents(), and cairo_stroke_to_path(), but does
  not have any effect during path construction.

  The default line width value is 2.0.

  Since 1.0"
  (%cairo-set-line-width cr (coerce width 'double-float)))

(export 'cairo-set-line-width)

;;; ----------------------------------------------------------------------------
;;; cairo_get_line_width ()
;;;
;;; double cairo_get_line_width (cairo_t *cr);
;;;
;;; This function returns the current line width value exactly as set by
;;; cairo_set_line_width(). Note that the value is unchanged even if the CTM has
;;; changed between the calls to cairo_set_line_width() and
;;; cairo_get_line_width().
;;;
;;; cr :
;;;     a cairo context
;;;
;;; Returns :
;;;     the current line width.
;;;
;;; Since 1.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_set_miter_limit ()
;;;
;;; void cairo_set_miter_limit (cairo_t *cr, double limit);
;;;
;;; Sets the current miter limit within the cairo context.
;;;
;;; If the current line join style is set to CAIRO_LINE_JOIN_MITER (see
;;; cairo_set_line_join()), the miter limit is used to determine whether the
;;; lines should be joined with a bevel instead of a miter. Cairo divides the
;;; length of the miter by the line width. If the result is greater than the
;;; miter limit, the style is converted to a bevel.
;;;
;;; As with the other stroke parameters, the current line miter limit is
;;; examined by cairo_stroke(), cairo_stroke_extents(), and
;;; cairo_stroke_to_path(), but does not have any effect during path
;;; construction.
;;;
;;; The default miter limit value is 10.0, which will convert joins with
;;; interior angles less than 11 degrees to bevels instead of miters. For
;;; reference, a miter limit of 2.0 makes the miter cutoff at 60 degrees, and a
;;; miter limit of 1.414 makes the cutoff at 90 degrees.
;;;
;;; A miter limit for a desired angle can be computed as:
;;; miter limit = 1/sin(angle/2)
;;;
;;; cr :
;;;     a cairo context
;;;
;;; limit :
;;;     miter limit to set
;;;
;;; Since 1.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_get_miter_limit ()
;;;
;;; double cairo_get_miter_limit (cairo_t *cr);
;;;
;;; Gets the current miter limit, as set by cairo_set_miter_limit().
;;;
;;; cr :
;;;     a cairo context
;;;
;;; Returns :
;;;     the current miter limit.
;;;
;;; Since 1.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; enum cairo_operator_t
;;;
;;; typedef enum {
;;;     CAIRO_OPERATOR_CLEAR,
;;;
;;;     CAIRO_OPERATOR_SOURCE,
;;;     CAIRO_OPERATOR_OVER,
;;;     CAIRO_OPERATOR_IN,
;;;     CAIRO_OPERATOR_OUT,
;;;     CAIRO_OPERATOR_ATOP,
;;;
;;;     CAIRO_OPERATOR_DEST,
;;;     CAIRO_OPERATOR_DEST_OVER,
;;;     CAIRO_OPERATOR_DEST_IN,
;;;     CAIRO_OPERATOR_DEST_OUT,
;;;     CAIRO_OPERATOR_DEST_ATOP,
;;;
;;;     CAIRO_OPERATOR_XOR,
;;;     CAIRO_OPERATOR_ADD,
;;;     CAIRO_OPERATOR_SATURATE,
;;;
;;;     CAIRO_OPERATOR_MULTIPLY,
;;;     CAIRO_OPERATOR_SCREEN,
;;;     CAIRO_OPERATOR_OVERLAY,
;;;     CAIRO_OPERATOR_DARKEN,
;;;     CAIRO_OPERATOR_LIGHTEN,
;;;     CAIRO_OPERATOR_COLOR_DODGE,
;;;     CAIRO_OPERATOR_COLOR_BURN,
;;;     CAIRO_OPERATOR_HARD_LIGHT,
;;;     CAIRO_OPERATOR_SOFT_LIGHT,
;;;     CAIRO_OPERATOR_DIFFERENCE,
;;;     CAIRO_OPERATOR_EXCLUSION,
;;;     CAIRO_OPERATOR_HSL_HUE,
;;;     CAIRO_OPERATOR_HSL_SATURATION,
;;;     CAIRO_OPERATOR_HSL_COLOR,
;;;     CAIRO_OPERATOR_HSL_LUMINOSITY
;;; } cairo_operator_t;
;;;
;;; cairo_operator_t is used to set the compositing operator for all cairo
;;; drawing operations.
;;;
;;; The default operator is CAIRO_OPERATOR_OVER.
;;;
;;; The operators marked as unbounded modify their destination even outside of
;;; the mask layer (that is, their effect is not bound by the mask layer).
;;; However, their effect can still be limited by way of clipping.
;;;
;;; To keep things simple, the operator descriptions here document the behavior
;;; for when both source and destination are either fully transparent or fully
;;; opaque. The actual implementation works for translucent layers too. For a
;;; more detailed explanation of the effects of each operator, including the
;;; mathematical definitions, see http://cairographics.org/operators/.
;;;
;;; CAIRO_OPERATOR_CLEAR
;;;     clear destination layer (bounded) (Since 1.0)
;;;
;;; CAIRO_OPERATOR_SOURCE
;;;     replace destination layer (bounded) (Since 1.0)
;;;
;;; CAIRO_OPERATOR_OVER
;;;     draw source layer on top of destination layer (bounded) (Since 1.0)
;;;
;;; CAIRO_OPERATOR_IN
;;;     draw source where there was destination content (unbounded) (Since 1.0)
;;;
;;; CAIRO_OPERATOR_OUT
;;;     draw source where there was no destination content (unbounded)
;;;     (Since 1.0)
;;;
;;; CAIRO_OPERATOR_ATOP
;;;     draw source on top of destination content and only there (Since 1.0)
;;;
;;; CAIRO_OPERATOR_DEST
;;;     ignore the source (Since 1.0)
;;;
;;; CAIRO_OPERATOR_DEST_OVER
;;;     draw destination on top of source (Since 1.0)
;;;
;;; CAIRO_OPERATOR_DEST_IN
;;;     leave destination only where there was source content (unbounded)
;;;     (Since 1.0)
;;;
;;; CAIRO_OPERATOR_DEST_OUT
;;;     leave destination only where there was no source content (Since 1.0)
;;;
;;; CAIRO_OPERATOR_DEST_ATOP
;;;     leave destination on top of source content and only there (unbounded)
;;;     (Since 1.0)
;;;
;;; CAIRO_OPERATOR_XOR
;;;     source and destination are shown where there is only one of them
;;;     (Since 1.0)
;;;
;;; CAIRO_OPERATOR_ADD
;;;     source and destination layers are accumulated (Since 1.0)
;;;
;;; CAIRO_OPERATOR_SATURATE
;;;     like over, but assuming source and dest are disjoint geometries
;;;     (Since 1.0)
;;;
;;; CAIRO_OPERATOR_MULTIPLY
;;;     source and destination layers are multiplied. This causes the result to
;;;     be at least as dark as the darker inputs. (Since 1.10)
;;;
;;; CAIRO_OPERATOR_SCREEN
;;;     source and destination are complemented and multiplied. This causes the
;;;     result to be at least as light as the lighter inputs. (Since 1.10)
;;;
;;; CAIRO_OPERATOR_OVERLAY
;;;     multiplies or screens, depending on the lightness of the destination
;;;     color. (Since 1.10)
;;;
;;; CAIRO_OPERATOR_DARKEN
;;;     replaces the destination with the source if it is darker, otherwise
;;;     keeps the source. (Since 1.10)
;;;
;;; CAIRO_OPERATOR_LIGHTEN
;;;     replaces the destination with the source if it is lighter, otherwise
;;;     keeps the source. (Since 1.10)
;;;
;;; CAIRO_OPERATOR_COLOR_DODGE
;;;     brightens the destination color to reflect the source color.
;;;     (Since 1.10)
;;;
;;; CAIRO_OPERATOR_COLOR_BURN
;;;     darkens the destination color to reflect the source color. (Since 1.10)
;;;
;;; CAIRO_OPERATOR_HARD_LIGHT
;;;     Multiplies or screens, dependent on source color. (Since 1.10)
;;;
;;; CAIRO_OPERATOR_SOFT_LIGHT
;;;     Darkens or lightens, dependent on source color. (Since 1.10)
;;;
;;; CAIRO_OPERATOR_DIFFERENCE
;;;     Takes the difference of the source and destination color. (Since 1.10)
;;;
;;; CAIRO_OPERATOR_EXCLUSION
;;;     Produces an effect similar to difference, but with lower contrast.
;;;     (Since 1.10)
;;;
;;; CAIRO_OPERATOR_HSL_HUE
;;;     Creates a color with the hue of the source and the saturation and
;;;     luminosity of the target. (Since 1.10)
;;;
;;; CAIRO_OPERATOR_HSL_SATURATION
;;;     Creates a color with the saturation of the source and the hue and
;;;     luminosity of the target. Painting with this mode onto a gray area
;;;     produces no change. (Since 1.10)
;;;
;;; CAIRO_OPERATOR_HSL_COLOR
;;;     Creates a color with the hue and saturation of the source and the
;;;     luminosity of the target. This preserves the gray levels of the target
;;;     and is useful for coloring monochrome images or tinting color images.
;;;     (Since 1.10)
;;;
;;; CAIRO_OPERATOR_HSL_LUMINOSITY
;;;     Creates a color with the luminosity of the source and the hue and
;;;     saturation of the target. This produces an inverse effect to
;;;     CAIRO_OPERATOR_HSL_COLOR. (Since 1.10)
;;;
;;; Since 1.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_set_operator ()
;;;
;;; void cairo_set_operator (cairo_t *cr, cairo_operator_t op);
;;;
;;; Sets the compositing operator to be used for all drawing operations. See
;;; cairo_operator_t for details on the semantics of each available compositing
;;; operator.
;;;
;;; The default operator is CAIRO_OPERATOR_OVER.
;;;
;;; cr :
;;;     a cairo_t
;;;
;;; op :
;;;     a compositing operator, specified as a cairo_operator_t
;;;
;;; Since 1.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_get_operator ()
;;;
;;; cairo_operator_t cairo_get_operator (cairo_t *cr);
;;;
;;; Gets the current compositing operator for a cairo context.
;;;
;;; cr :
;;;     a cairo context
;;;
;;; Returns :
;;;     the current compositing operator.
;;;
;;; Since 1.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_set_tolerance ()
;;;
;;; void cairo_set_tolerance (cairo_t *cr, double tolerance);
;;;
;;; Sets the tolerance used when converting paths into trapezoids. Curved
;;; segments of the path will be subdivided until the maximum deviation between
;;; the original path and the polygonal approximation is less than tolerance.
;;; The default value is 0.1. A larger value will give better performance, a
;;; smaller value, better appearance. (Reducing the value from the default value
;;; of 0.1 is unlikely to improve appearance significantly.) The accuracy of
;;; paths within Cairo is limited by the precision of its internal arithmetic,
;;; and the prescribed tolerance is restricted to the smallest representable
;;; internal value.
;;;
;;; cr :
;;;     a cairo_t
;;;
;;; tolerance :
;;;     the tolerance, in device units (typically pixels)
;;;
;;; Since 1.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_get_tolerance ()
;;;
;;; double cairo_get_tolerance (cairo_t *cr);
;;;
;;; Gets the current tolerance value, as set by cairo_set_tolerance().
;;;
;;; cr :
;;;     a cairo context
;;;
;;; Returns :
;;;     the current tolerance value.
;;;
;;; Since 1.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_clip ()
;;;
;;; void cairo_clip (cairo_t *cr);
;;;
;;; Establishes a new clip region by intersecting the current clip region with
;;; the current path as it would be filled by cairo_fill() and according to the
;;; current fill rule (see cairo_set_fill_rule()).
;;;
;;; After cairo_clip(), the current path will be cleared from the cairo context.
;;;
;;; The current clip region affects all drawing operations by effectively
;;; masking out any changes to the surface that are outside the current clip
;;; region.
;;;
;;; Calling cairo_clip() can only make the clip region smaller, never larger.
;;; But the current clip is part of the graphics state, so a temporary
;;; restriction of the clip region can be achieved by calling cairo_clip()
;;; within a cairo_save()/cairo_restore() pair. The only other means of
;;; increasing the size of the clip region is cairo_reset_clip().
;;;
;;; cr :
;;;     a cairo context
;;;
;;; Since 1.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_clip_preserve ()
;;;
;;; void cairo_clip_preserve (cairo_t *cr);
;;;
;;; Establishes a new clip region by intersecting the current clip region with
;;; the current path as it would be filled by cairo_fill() and according to the
;;; current fill rule (see cairo_set_fill_rule()).
;;;
;;; Unlike cairo_clip(), cairo_clip_preserve() preserves the path within the
;;; cairo context.
;;;
;;; The current clip region affects all drawing operations by effectively
;;; masking out any changes to the surface that are outside the current clip
;;; region.
;;;
;;; Calling cairo_clip_preserve() can only make the clip region smaller, never
;;; larger. But the current clip is part of the graphics state, so a temporary
;;; restriction of the clip region can be achieved by calling
;;; cairo_clip_preserve() within a cairo_save()/cairo_restore() pair. The only
;;; other means of increasing the size of the clip region is cairo_reset_clip().
;;;
;;; cr :
;;;     a cairo context
;;;
;;; Since 1.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_clip_extents ()
;;;
;;; void cairo_clip_extents (cairo_t *cr,
;;;                          double *x1,
;;;                          double *y1,
;;;                          double *x2,
;;;                          double *y2);
;;;
;;; Computes a bounding box in user coordinates covering the area inside the
;;; current clip.
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
;;; Since 1.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_in_clip ()
;;;
;;; cairo_bool_t cairo_in_clip (cairo_t *cr, double x, double y);
;;;
;;; Tests whether the given point is inside the area that would be visible
;;; through the current clip, i.e. the area that would be filled by a
;;; cairo_paint() operation.
;;;
;;; See cairo_clip(), and cairo_clip_preserve().
;;;
;;; cr :
;;;     a cairo context
;;;
;;; x :
;;;     X coordinate of the point to test
;;;
;;; y :
;;;     Y coordinate of the point to test
;;;
;;; Returns :
;;;     A non-zero value if the point is inside, or zero if outside.
;;;
;;; Since 1.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_reset_clip ()
;;;
;;; void cairo_reset_clip (cairo_t *cr);
;;;
;;; Reset the current clip region to its original, unrestricted state. That is,
;;; set the clip region to an infinitely large shape containing the target
;;; surface. Equivalently, if infinity is too hard to grasp, one can imagine the
;;; clip region being reset to the exact bounds of the target surface.
;;;
;;; Note that code meant to be reusable should not call cairo_reset_clip() as it
;;; will cause results unexpected by higher-level code which calls cairo_clip().
;;; Consider using cairo_save() and cairo_restore() around cairo_clip() as a
;;; more robust means of temporarily restricting the clip region.
;;;
;;; cr :
;;;     a cairo context
;;;
;;; Since 1.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_rectangle_t
;;;
;;; typedef struct {
;;;     double x, y, width, height;
;;; } cairo_rectangle_t;
;;;
;;; A data structure for holding a rectangle.
;;;
;;; double x;
;;;     X coordinate of the left side of the rectangle
;;;
;;; double y;
;;;     Y coordinate of the the top side of the rectangle
;;;
;;; double width;
;;;     width of the rectangle
;;;
;;; double height;
;;;     height of the rectangle
;;;
;;; Since 1.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_rectangle_list_t
;;;
;;; typedef struct {
;;;     cairo_status_t     status;
;;;     cairo_rectangle_t *rectangles;
;;;     int                num_rectangles;
;;; } cairo_rectangle_list_t;
;;;
;;; A data structure for holding a dynamically allocated array of rectangles.
;;;
;;; cairo_status_t status;
;;;     Error status of the rectangle list
;;;
;;; cairo_rectangle_t *rectangles;
;;;     Array containing the rectangles
;;;
;;; int num_rectangles;
;;;     Number of rectangles in this list
;;;
;;; Since 1.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_rectangle_list_destroy ()
;;;
;;; void cairo_rectangle_list_destroy (cairo_rectangle_list_t *rectangle_list);
;;;
;;; Unconditionally frees rectangle_list and all associated references. After
;;; this call, the rectangle_list pointer must not be dereferenced.
;;;
;;; rectangle_list :
;;;     a rectangle list, as obtained from cairo_copy_clip_rectangle_list()
;;;
;;; Since 1.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_copy_clip_rectangle_list ()
;;;
;;; cairo_rectangle_list_t * cairo_copy_clip_rectangle_list (cairo_t *cr);
;;;
;;; Gets the current clip region as a list of rectangles in user coordinates.
;;; Never returns NULL.
;;;
;;; The status in the list may be CAIRO_STATUS_CLIP_NOT_REPRESENTABLE to
;;; indicate that the clip region cannot be represented as a list of user-space
;;; rectangles. The status may have other values to indicate other errors.
;;;
;;; cr :
;;;     a cairo context
;;;
;;; Returns :
;;;     the current clip region as a list of rectangles in user coordinates,
;;;     which should be destroyed using cairo_rectangle_list_destroy().
;;;
;;; Since 1.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_fill ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_fill" cairo-fill) :void
 #+cl-cffi-gtk-documentation
 "@version{2012-12-21}
  @argument[cr]{a cairo context}
  @begin{short}
    A drawing operator that fills the current path according to the current fill
    rule, (each sub-path is implicitly closed before being filled). After
    @sym{cairo-fill}, the current path will be cleared from the cairo context.
    See @code{cairo_set_fill_rule()} and @code{cairo_fill_preserve()}.
  @end{short}

  Since 1.0"
  (cr (:pointer (:struct cairo-t))))

(export 'cairo-fill)

;;; ----------------------------------------------------------------------------
;;; cairo_fill_preserve ()
;;;
;;; void cairo_fill_preserve (cairo_t *cr);
;;;
;;; A drawing operator that fills the current path according to the current fill
;;; rule, (each sub-path is implicitly closed before being filled). Unlike
;;; cairo_fill(), cairo_fill_preserve() preserves the path within the cairo
;;; context.
;;;
;;; See cairo_set_fill_rule() and cairo_fill().
;;;
;;; cr :
;;;     a cairo context
;;;
;;; Since 1.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_fill_extents ()
;;;
;;; void cairo_fill_extents (cairo_t *cr,
;;;                          double *x1,
;;;                          double *y1,
;;;                          double *x2,
;;;                          double *y2);
;;;
;;; Computes a bounding box in user coordinates covering the area that would be
;;; affected, (the "inked" area), by a cairo_fill() operation given the current
;;; path and fill parameters. If the current path is empty, returns an empty
;;; rectangle ((0,0), (0,0)). Surface dimensions and clipping are not taken into
;;; account.
;;;
;;; Contrast with cairo_path_extents(), which is similar, but returns non-zero
;;; extents for some paths with no inked area, (such as a simple line segment).
;;;
;;; Note that cairo_fill_extents() must necessarily do more work to compute the
;;; precise inked areas in light of the fill rule, so cairo_path_extents() may
;;; be more desirable for sake of performance if the non-inked path extents are
;;; desired.
;;;
;;; See cairo_fill(), cairo_set_fill_rule() and cairo_fill_preserve().
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
;;; Since 1.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_in_fill ()
;;;
;;; cairo_bool_t cairo_in_fill (cairo_t *cr, double x, double y);
;;;
;;; Tests whether the given point is inside the area that would be affected by a
;;; cairo_fill() operation given the current path and filling parameters.
;;; Surface dimensions and clipping are not taken into account.
;;;
;;; See cairo_fill(), cairo_set_fill_rule() and cairo_fill_preserve().
;;;
;;; cr :
;;;     a cairo context
;;;
;;; x :
;;;     X coordinate of the point to test
;;;
;;; y :
;;;     Y coordinate of the point to test
;;;
;;; Returns :
;;;     A non-zero value if the point is inside, or zero if outside.
;;;
;;; Since 1.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_mask ()
;;;
;;; void cairo_mask (cairo_t *cr, cairo_pattern_t *pattern);
;;;
;;; A drawing operator that paints the current source using the alpha channel of
;;; pattern as a mask. (Opaque areas of pattern are painted with the source,
;;; transparent areas are not painted.)
;;;
;;; cr :
;;;     a cairo context
;;;
;;; pattern :
;;;     a cairo_pattern_t
;;;
;;; Since 1.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_mask_surface ()
;;;
;;; void cairo_mask_surface (cairo_t *cr,
;;;                          cairo_surface_t *surface,
;;;                          double surface_x,
;;;                          double surface_y);
;;;
;;; A drawing operator that paints the current source using the alpha channel of
;;; surface as a mask. (Opaque areas of surface are painted with the source,
;;; transparent areas are not painted.)
;;;
;;; cr :
;;;     a cairo context
;;;
;;; surface :
;;;     a cairo_surface_t
;;;
;;; surface_x :
;;;     X coordinate at which to place the origin of surface
;;;
;;; surface_y :
;;;     Y coordinate at which to place the origin of surface
;;;
;;; Since 1.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_paint ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_paint" cairo-paint) :void
 #+cl-cffi-gtk-documentation
 "@version{2012-12-21}
  @argument[cr]{a cairo context}
  @begin{short}
    A drawing operator that paints the current source everywhere within the
    current clip region.
  @end{short}

  Since 1.0"
  (cr (:pointer (:struct cairo-t))))

(export 'cairo-paint)

;;; ----------------------------------------------------------------------------
;;; cairo_paint_with_alpha ()
;;;
;;; void cairo_paint_with_alpha (cairo_t *cr, double alpha);
;;;
;;; A drawing operator that paints the current source everywhere within the
;;; current clip region using a mask of constant alpha value alpha. The effect
;;; is similar to cairo_paint(), but the drawing is faded out using the alpha
;;; value.
;;;
;;; cr :
;;;     a cairo context
;;;
;;; alpha :
;;;     alpha value, between 0 (transparent) and 1 (opaque)
;;;
;;; Since 1.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_stroke ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_stroke" cairo-stroke) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-3-2}
  @argument[cr]{a cairo context}
  @begin{short}
    A drawing operator that strokes the current path according to the current
    line width, line join, line cap, and dash settings. After cairo_stroke(),
    the current path will be cleared from the cairo context.
  @end{short}
  See cairo_set_line_width(), cairo_set_line_join(), cairo_set_line_cap(),
  cairo_set_dash(), and cairo_stroke_preserve().

  Note: Degenerate segments and sub-paths are treated specially and provide a
  useful result. These can result in two different situations:
  @begin{enumerate}
    @begin{item}
      Zero-length \"on\" segments set in cairo_set_dash(). If the cap style is
      CAIRO_LINE_CAP_ROUND or CAIRO_LINE_CAP_SQUARE then these segments will be
      drawn as circular dots or squares respectively. In the case of
      CAIRO_LINE_CAP_SQUARE, the orientation of the squares is determined by
      the direction of the underlying path.
    @end{item}
    @begin{item}
      A sub-path created by cairo_move_to() followed by either a
      cairo_close_path() or one or more calls to cairo_line_to() to the same
      coordinate as the cairo_move_to(). If the cap style is
      CAIRO_LINE_CAP_ROUND then these sub-paths will be drawn as circular dots.
      Note that in the case of CAIRO_LINE_CAP_SQUARE a degenerate sub-path will
      not be drawn at all, (since the correct orientation is indeterminate).
    @end{item}
  @end{enumerate}
  In no case will a cap style of CAIRO_LINE_CAP_BUTT cause anything to be
  drawn in the case of either degenerate segments or sub-paths.

  Since 1.0"
  (cr (:pointer (:struct cairo-t))))

(export 'cairo-stroke)

;;; ----------------------------------------------------------------------------
;;; cairo_stroke_preserve ()
;;;
;;; void cairo_stroke_preserve (cairo_t *cr);
;;;
;;; A drawing operator that strokes the current path according to the current
;;; line width, line join, line cap, and dash settings. Unlike cairo_stroke(),
;;; cairo_stroke_preserve() preserves the path within the cairo context.
;;;
;;; See cairo_set_line_width(), cairo_set_line_join(), cairo_set_line_cap(),
;;; cairo_set_dash(), and cairo_stroke_preserve().
;;;
;;; cr :
;;;     a cairo context
;;;
;;; Since 1.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_stroke_extents ()
;;;
;;; void cairo_stroke_extents (cairo_t *cr,
;;;                            double *x1,
;;;                            double *y1,
;;;                            double *x2,
;;;                            double *y2);
;;;
;;; Computes a bounding box in user coordinates covering the area that would be
;;; affected, (the "inked" area), by a cairo_stroke() operation given the
;;; current path and stroke parameters. If the current path is empty, returns an
;;; empty rectangle ((0,0), (0,0)). Surface dimensions and clipping are not
;;; taken into account.
;;;
;;; Note that if the line width is set to exactly zero, then
;;; cairo_stroke_extents() will return an empty rectangle. Contrast with
;;; cairo_path_extents() which can be used to compute the non-empty bounds as
;;; the line width approaches zero.
;;;
;;; Note that cairo_stroke_extents() must necessarily do more work to compute
;;; the precise inked areas in light of the stroke parameters, so
;;; cairo_path_extents() may be more desirable for sake of performance if
;;; non-inked path extents are desired.
;;;
;;; See cairo_stroke(), cairo_set_line_width(), cairo_set_line_join(),
;;; cairo_set_line_cap(), cairo_set_dash(), and cairo_stroke_preserve().
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
;;; Since 1.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_in_stroke ()
;;;
;;; cairo_bool_t cairo_in_stroke (cairo_t *cr, double x, double y);
;;;
;;; Tests whether the given point is inside the area that would be affected by a
;;; cairo_stroke() operation given the current path and stroking parameters.
;;; Surface dimensions and clipping are not taken into account.
;;;
;;; See cairo_stroke(), cairo_set_line_width(), cairo_set_line_join(),
;;; cairo_set_line_cap(), cairo_set_dash(), and cairo_stroke_preserve().
;;;
;;; cr :
;;;     a cairo context
;;;
;;; x :
;;;     X coordinate of the point to test
;;;
;;; y :
;;;     Y coordinate of the point to test
;;;
;;; Returns :
;;;     A non-zero value if the point is inside, or zero if outside.
;;;
;;; Since 1.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_copy_page ()
;;;
;;; void cairo_copy_page (cairo_t *cr);
;;;
;;; Emits the current page for backends that support multiple pages, but doesn't
;;; clear it, so, the contents of the current page will be retained for the next
;;; page too. Use cairo_show_page() if you want to get an empty page after the
;;; emission.
;;;
;;; This is a convenience function that simply calls cairo_surface_copy_page()
;;; on cr's target.
;;;
;;; cr :
;;;     a cairo context
;;;
;;; Since 1.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_show_page ()
;;;
;;; void cairo_show_page (cairo_t *cr);
;;;
;;; Emits and clears the current page for backends that support multiple pages.
;;; Use cairo_copy_page() if you don't want to clear the page.
;;;
;;; This is a convenience function that simply calls cairo_surface_show_page()
;;; on cr's target.
;;;
;;; cr :
;;;     a cairo context
;;;
;;; Since 1.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_get_reference_count ()
;;;
;;; unsigned int cairo_get_reference_count (cairo_t *cr);
;;;
;;; Returns the current reference count of cr.
;;;
;;; cr :
;;;     a cairo_t
;;;
;;; Returns :
;;;     the current reference count of cr. If the object is a nil object, 0 will
;;;     be returned.
;;;
;;; Since 1.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_set_user_data ()
;;;
;;; cairo_status_t cairo_set_user_data (cairo_t *cr,
;;;                                     const cairo_user_data_key_t *key,
;;;                                     void *user_data,
;;;                                     cairo_destroy_func_t destroy);
;;;
;;; Attach user data to cr. To remove user data from a surface, call this
;;; function with the key that was used to set it and NULL for data.
;;;
;;; cr :
;;;     a cairo_t
;;;
;;; key :
;;;     the address of a cairo_user_data_key_t to attach the user data to
;;;
;;; user_data :
;;;     the user data to attach to the cairo_t
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
;;; cairo_get_user_data ()
;;;
;;; void * cairo_get_user_data (cairo_t *cr, const cairo_user_data_key_t *key);
;;;
;;; Return user data previously attached to cr using the specified key. If no
;;; user data has been attached with the given key this function returns NULL.
;;;
;;; cr :
;;;     a cairo_t
;;;
;;; key :
;;;     the address of the cairo_user_data_key_t the user data was attached to
;;;
;;; Returns :
;;;     the user data previously attached or NULL.
;;;
;;; Since 1.4
;;; ----------------------------------------------------------------------------

;;; --- End of file cairo.context.lisp -----------------------------------------
