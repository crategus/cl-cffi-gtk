;;; ----------------------------------------------------------------------------
;;; cairo.context.lisp
;;;
;;; The documentation of this file is taken from the Cairo Reference Manual
;;; Version 1.12.2 and modified to document the Lisp binding to the Cairo
;;; library. See <http://cairographics.org>. The API documentation of the Lisp
;;; binding is available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2012, 2013, 2014 Dieter Kaiser
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
 "@version{2014-1-23}
  @argument[target]{target surface of type @symbol{cairo-surface-t} for the
    context}
  @begin{return}
    A newly allocated @symbol{cairo-t} with a reference count of 1.
  @end{return}
  @begin{short}
    Creates a new @symbol{cairo-t} with all graphics state parameters set to
    default values and with @arg{target} as a target surface.
  @end{short}
  The target surface should be constructed with a backend-specific function such
  as the function @fun{cairo-image-surface-create}, or any other variant.

  The initial reference count should be released with the function
  @fun{cairo-destroy} when you are done using the @symbol{cairo-t}. This
  function never returns @code{NULL}. If memory cannot be allocated, a special
  @symbol{cairo-t} object will be returned on which the function
  @fun{cairo-status} returns @code{:no-memory}. If you attempt to target a
  surface which does not support writing, such as @symbol{cairo-mime-surface-t},
  then a @code{:write-error} will be raised. You can use this object normally,
  but no drawing will be done.

  This function references target, so you can immediately call the function
  @fun{cairo-surface-destroy} on it if you do not need to maintain a separate
  reference to it.

  Since 1.0
  @see-symbol{cairo-t}
  @see-symbol{cairo-surface-t}
  @see-symbol{cairo-mime-surface-t}
  @see-function{cairo-status}
  @see-function{cairo-image-surface-create}
  @see-function{cairo-surface-destroy}"
  (target (:pointer (:struct cairo-surface-t))))

(export 'cairo-create)

;;; ----------------------------------------------------------------------------
;;; cairo_reference ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_reference" cairo-reference) (:pointer (:struct cairo-t))
 #+cl-cffi-gtk-documentation
 "@version{2013-8-4}
  @argument[cr]{a @symbol{cairo-t}}
  @return{The referenced @symbol{cairo-t}.}
  @begin{short}
    Increases the reference count on @arg{cr} by one. This prevents @arg{cr}
    from being destroyed until a matching call to the function
    @fun{cairo-destroy} is made.
  @end{short}

  The number of references to a @symbol{cairo-t} can be get using the function
  @fun{cairo-get-reference-count}.

  Since 1.0
  @see-symbol{cairo-t}
  @see-function{cairo-destroy}
  @see-function{cairo-get-reference-count}"
  (cr (:pointer (:struct cairo-t))))

(export 'cairo-reference)

;;; ----------------------------------------------------------------------------
;;; cairo_destroy ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_destroy" cairo-destroy) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-8-4}
  @argument[cr]{a @symbol{cairo-t}}
  @begin{short}
    Decreases the reference count on @arg{cr} by one. If the result is zero,
    then @arg{cr} and all associated resources are freed.
  @end{short}
  See the function @fun{cairo-reference}.

  Since 1.0
  @see-symbol{cairo-t}
  @see-function{cairo-reference}"
  (cr (:pointer (:struct cairo-t))))

(export 'cairo-destroy)

;;; ----------------------------------------------------------------------------
;;; cairo_status ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_status" cairo-status) cairo-status-t
 #+cl-cffi-gtk-documentation
 "@version{2014-1-23}
  @argument[cr]{a cairo context}
  @return{The current status of type @symbol{cairo-status-t} of this context.}
  @begin{short}
    Checks whether an error has previously occurred for this context.
  @end{short}
  See the enumeration @symbol{cairo-status-t}.

  Since 1.0
  @see-symbol{cairo-t}
  @see-symbol{cairo-status-t}"
  (cr (:pointer (:struct cairo-t))))

(export 'cairo-status)

;;; ----------------------------------------------------------------------------
;;; cairo_save ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_save" cairo-save) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-10-5}
  @argument[cr]{a @symbol{cairo-t}}
  @begin{short}
    Makes a copy of the current state of @arg{cr} and saves it on an internal
    stack of saved states for @arg{cr}.
  @end{short}
  When the function @fun{cairo-restore} is called, @arg{cr} will be restored to
  the saved state. Multiple calls to the functions @sym{cairo-save} and
  @fun{cairo-restore} can be nested; each call to the function
  @fun{cairo-restore} restores the state from the matching paired
  @sym{cairo-save}.

  It is not necessary to clear all saved states before a @symbol{cairo-t} is
  freed. If the reference count of a @symbol{cairo-t} drops to zero in response
  to a call to the function @fun{cairo-destroy}, any saved states will be freed
  along with the @symbol{cairo-t}.

  Since 1.0
  @see-symbol{cairo-t}
  @see-function{cairo-restore}"
  (cr (:pointer (:struct cairo-t))))

(export 'cairo-save)

;;; ----------------------------------------------------------------------------
;;; cairo_restore ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_restore" cairo-restore) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-10-5}
  @argument[cr]{a @symbol{cairo-t}}
  @begin{short}
    Restores @arg{cr} to the state saved by a preceding call to the function
    @fun{cairo-save} and removes that state from the stack of saved states.
  @end{short}

  Since 1.0
  @see-symbol{cairo-t}
  @see-function{cairo-save}"
  (cr (:pointer (:struct cairo-t))))

(export 'cairo-restore)

;;; ----------------------------------------------------------------------------
;;; cairo_get_target ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_get_target" cairo-get-target)
    (:pointer (:struct cairo-surface-t))
 #+cl-cffi-gtk-documentation
 "@version{2014-1-23}
  @argument[cr]{a cairo context}
  @begin{return}
    The target surface of type @symbol{cairo-surface-t}. This object is
    owned by cairo.
  @end{return}
  @begin{short}
    Gets the target surface for the cairo context as passed to the function
    @fun{cairo-create}.
  @end{short}
  To keep a reference to it, you must call the function
  @fun{cairo-surface-reference}.

  This function will always return a valid pointer, but the result can be a
  @code{nil} surface if @arg{cr} is already in an error state.

  Since 1.0
  @see-symbol{cairo-t}
  @see-symbol{cairo-surface-t}
  @see-function{cairo-create}
  @see-function{cairo-surface-reference}"
  (cr (:pointer (:struct cairo-t))))

(export 'cairo-get-target)

;;; ----------------------------------------------------------------------------
;;; cairo_push_group ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_push_group" cairo-push-group) :void
 #+cl-cffi-gtk-documentation
"@version{2014-1-23}
  @argument[cr]{a cairo context}
  @begin{short}
    Temporarily redirects drawing to an intermediate surface known as a group.
  @end{short}
  The redirection lasts until the group is completed by a call to the functions
  @fun{cairo-pop-group} or @fun{cairo-pop-group-to-source}. These calls provide
  the result of any drawing to the group as a pattern, either as an explicit
  object, or set as the source pattern.

  This group functionality can be convenient for performing intermediate
  compositing. One common use of a group is to render objects as opaque within
  the group, so that they occlude each other, and then blend the result with
  translucence onto the destination.

  Groups can be nested arbitrarily deep by making balanced calls to
  the functions @sym{cairo-push-group}and @fun{cairo-pop-group}. Each call
  pushes and pops the new target group onto and from a stack.

  The function @sym{cairo-push-group} calls the function @fun{cairo-save} so
  that any changes to the graphics state will not be visible outside the group,
  the pop group functions call the function @fun{cairo-restore}.

  By default the intermediate group will have a content type of
  @code{:color-alpha} of type @symbol{cairo-content-t}. Other content types can
  be chosen for the group by using the function
  @fun{cairo-push-group-with-content} instead.

  @begin[Example]{dictionary}
    As an example, here is how one might fill and stroke a path with
    translucence, but without any portion of the fill being visible under the
    stroke:
    @begin{pre}
 (cairo-push-group cr)
 (cairo-set-source cr fill-pattern)
 (cairo-fill-preserve cr)
 (cairo-set-source cr stroke-pattern)
 (cairo-stroke cr)
 (cairo-pop-group-to-source cr)
 (cairo-paint-with-alpha cr alpha)
    @end{pre}
  @end{dictionary}
  Since 1.2
  @see-symbol{cairo-t}
  @see-symbol{cairo-content-t}
  @see-function{cairo-restore}
  @see-function{cairo-pop-group}
  @see-function{cairo-pop-group-to-source}
  @see-function{cairo-push-group-with-content}"
  (cr (:pointer (:struct cairo-t))))

(export 'cairo-push-group)

;;; ----------------------------------------------------------------------------
;;; cairo_push_group_with_content ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_push_group_with_content" cairo-push-group-with-content) :void
 #+cl-cffi-gtk-documentation
 "@version{2014-1-23}
  @argument[cr]{a cairo context}
  @argument[content]{a @symbol{cairo-content-t} indicating the type of group
    that will be created}
  @begin{short}
    Temporarily redirects drawing to an intermediate surface known as a group.
  @end{short}
  The redirection lasts until the group is completed by a call to the functions
  @fun{cairo-pop-group} or @fun{cairo-pop-group-to-source}. These calls provide
  the result of any drawing to the group as a pattern, either as an explicit
  object, or set as the source pattern.

  The group will have a content type of @arg{content}. The ability to control
  this content type is the only distinction between this function and the
  function @fun{cairo-push-group} which you should see for a more detailed
  description of group rendering.

  Since 1.2
  @see-symbol{cairo-t}
  @see-symbol{cairo-content-t}
  @see-function{cairo-pop-group}
  @see-function{cairo-push-group}
  @see-function{cairo-pop-group-to-source}"
  (cr (:pointer (:struct cairo-t)))
  (content cairo-content-t))

(export 'cairo-push-group-with-content)

;;; ----------------------------------------------------------------------------
;;; cairo_pop_group ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_pop_group" cairo-pop-group)
    (:pointer (:struct cairo-pattern-t))
 #+cl-cffi-gtk-documentation
 "@version{2014-1-23}
  @argument[cr]{a cairo context}
  @begin{return}
    A newly created (surface) pattern containing the results of all drawing
    operations performed to the group. The caller owns the returned object
    and should call the function @fun{cairo-pattern-destroy} when finished with
    it.
  @end{return}
  @begin{short}
    Terminates the redirection begun by a call to the functions
    @fun{cairo-push-group} or @fun{cairo-push-group-with-content} and returns a
    new pattern containing the results of all drawing operations performed to
    the group.
  @end{short}

  The function @sym{cairo-pop-group} calls the function @fun{cairo-restore},
  balancing a call to the function @fun{cairo-save} by the push group function,
  so that any changes to the graphics state will not be visible outside the
  group.

  Since 1.2
  @see-symbol{cairo-t}
  @see-function{cairo-pattern-destroy}
  @see-function{cairo-push-group}
  @see-function{cairo-push-group-with-content}
  @see-function{cairo-restore}
  @see-function{cairo-save}"
  (cr (:pointer (:struct cairo-pattern-t))))

(export 'cairo-pop-group)

;;; ----------------------------------------------------------------------------
;;; cairo_pop_group_to_source ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_pop_group_to_source" cairo-pop-group-to-source) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-12-5}
  @argument[cr]{a cairo context}
  @begin{short}
    Terminates the redirection begun by a call to the functions
    @fun{cairo-push-group} or @fun{cairo-push-group-with-content} and installs
    the resulting pattern as the source pattern in the given cairo context.
  @end{short}
  The behavior of this function is equivalent to the sequence of operations:
  @begin{pre}
 (let ((group (cairo-pop-group cr)))
   (cairo-set-source cr group)
   (cairo-pattern-destroy group))
  @end{pre}
  but is more convenient as their is no need for a variable to store the
  pattern.

  The function @fun{cairo-pop-group} calls the function @fun{cairo-restore},
  balancing a call to the function @fun{cairo-save} by the push group
  function, so that any changes to the graphics state will not be visible
  outside the group.

  Since 1.2
  @see-symbol{cairo-t}
  @see-function{cairo-push-group}
  @see-function{cairo-push-group-with-content}
  @see-function{cairo-pop-group}
  @see-function{cairo-save}
  @see-function{cairo-restore}"
  (cr (:pointer (:struct cairo-t))))

(export 'cairo-pop-group-to-source)

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

(defcfun ("cairo_set_source_rgb" %cairo-set-source-rgb) :void
  (cr (:pointer (:struct cairo-t)))
  (red :double)
  (green :double)
  (blue :double))

(defun cairo-set-source-rgb (cr red green blue)
 #+cl-cffi-gtk-documentation
 "@version{2014-1-23}
  @argument[cr]{a cairo context}
  @argument[red]{red component of color}
  @argument[green]{green component of color}
  @argument[blue]{blue component of color}
  @begin{short}
    Sets the source pattern within @arg{cr} to an opaque color. This opaque
    color will then be used for any subsequent drawing operation until a new
    source pattern is set.
  @end{short}

  The color components are floating point numbers in the range 0.0 to
  1.0. If the values passed in are outside that range, they will be
  clamped.

  The default source pattern is opaque black, that is, it is equivalent to
  @code{(cairo-set-source-rgb cr 0.0 0.0 0.0)}.

  Since 1.0
  @see-symbol{cairo-t}
  @see-function{cairo-set-source-rgba}"
  (%cairo-set-source-rgb cr
                         (coerce red 'double-float)
                         (coerce green 'double-float)
                         (coerce blue 'double-float)))

(export 'cairo-set-source-rgb)

;;; ----------------------------------------------------------------------------
;;; cairo_set_source_rgba ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_set_source_rgba" %cairo-set-source-rgba) :void
  (cr (:pointer (:struct cairo-t)))
  (red :double)
  (green :double)
  (blue :double)
  (alpha :double))

(defun cairo-set-source-rgba (cr red green blue alpha)
 #+cl-cffi-gtk-documentation
 "@version{2014-1-23}
  @argument[cr]{a cairo context}
  @argument[red]{red component of color}
  @argument[green]{green component of color}
  @argument[blue]{blue component of color}
  @argument[alpha]{alpha component of color}
  @begin{short}
    Sets the source pattern within @arg{cr} to a translucent color.
  @end{short}
  This color will then be used for any subsequent drawing operation until a new
  source pattern is set.

  The color and alpha components are floating point numbers in the range 0.0 to
  1.0. If the values passed in are outside that range, they will be clamped.

  The default source pattern is opaque black, that is, it is equivalent to
  @code{(cairo-set-source-rgba cr 0.0 0.0 0.0 1.0)}.

  Since 1.0
  @see-symbol{cairo-t}
  @see-function{cairo-set-source-rgb}"
  (%cairo-set-source-rgba cr
                          (coerce red 'double-float)
                          (coerce green 'double-float)
                          (coerce blue 'double-float)
                          (coerce alpha 'double-float)))

(export 'cairo-set-source-rgba)

;;; ----------------------------------------------------------------------------
;;; cairo_set_source ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_set_source" cairo-set-source) :void
 #+cl-cffi-gtk-documentation
 "@version{2014-1-23}
  @argument[cr]{a cairo context}
  @argument[source]{a @symbol{cairo-pattern-t} to be used as the source for
    subsequent drawing operations}
  @begin{short}
    Sets the source pattern within @arg{cr} to @arg{source}.
  @end{short}
  This pattern will then be used for any subsequent drawing operation until a
  new source pattern is set.

  The default source pattern is a solid pattern that is opaque black, that
  is, it is equivalent to @code{(cairo-set-source-rgb cr 0.0 0.0 0.0)}.
  @begin[Note]{dictionary}
    The pattern's transformation matrix will be locked to the user space
    in effect at the time of @sym{cairo-set-source}. This means that further
    modifications of the current transformation matrix will not affect the
    source pattern. See the function @fun{cairo-pattern-set-matrix}.
  @end{dictionary}

  Since 1.0
  @see-symbol{cairo-t}
  @see-symbol{cairo-pattern-t}
  @see-function{cairo-set-source-rgb}
  @see-function{cairo-pattern-set-matrix}"
  (cr (:pointer (:struct cairo-t)))
  (source (:pointer (:struct cairo-pattern-t))))

(export 'cairo-set-source)

;;; ----------------------------------------------------------------------------
;;; cairo_set_source_surface ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_set_source_surface" %cairo-set-source-surface) :void
  (cr (:pointer (:struct cairo-t)))
  (surface (:pointer (:struct cairo-surface-t)))
  (x :double)
  (y :double))

(defun cairo-set-source-surface (cr surface x y)
 #+cl-cffi-gtk-documentation
 "@version{2014-1-23}
  @argument[cr]{a cairo context}
  @argument[surface]{a surface of type @symbol{cairo-surface-t} to be used to
    set the source pattern}
  @argument[x]{user-space x coordinate for surface origin}
  @argument[y]{user-space y coordinate for surface origin}
  @begin{short}
    This is a convenience function for creating a pattern from @arg{surface}
    and setting it as the source in @arg{cr} with the function
    @fun{cairo-set-source}.
  @end{short}

  The @arg{x} and @arg{y} parameters give the user-space coordinate at which the
  surface origin should appear. The surface origin is its upper-left corner
  before any transformation has been applied. The @arg{x} and @arg{y}
  parameters are negated and then set as translation values in the pattern
  matrix.

  Other than the initial translation pattern matrix, as described above, all
  other pattern attributes, such as its extend mode, are set to the default
  values as in the function @fun{cairo-pattern-create-for-surface}. The
  resulting pattern can be queried with the function @fun{cairo-get-source} so
  that these attributes can be modified if desired, e. g. to create a repeating
  pattern with with the function @fun{cairo-pattern-set-extend}.

  Since 1.0
  @see-symbol{cairo-t}
  @see-symbol{cairo-surface-t}
  @see-function{cairo-set-source}
  @see-function{cairo-get-source}
  @see-function{cairo-pattern-create-for-surface}
  @see-function{cairo-pattern-set-extend}"
  (%cairo-set-source-surface cr
                             surface
                             (coerce x 'double-float)
                             (coerce y 'double-float)))

(export 'cairo-set-source-surface)

;;; ----------------------------------------------------------------------------
;;; cairo_get_source ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_get_source" cairo-get-source)
    (:pointer (:struct cairo-pattern-t))
 #+cl-cffi-gtk-documentation
 "@version{2014-1-23}
  @argument[cr]{a cairo context}
  @begin{return}
    The current source pattern.
  @end{return}
  @begin{short}
    Gets the current source pattern for @arg{cr}.
  @end{short}

  This object is owned by cairo. To keep a reference to it, you must call the
  function @fun{cairo-pattern-reference}.

  Since 1.0
  @see-symbol{cairo-t}
  @see-symbol{cairo-pattern-t}
  @see-function{cairo-pattern-reference}"
  (cr (:pointer (:struct cairo-t))))

(export 'cairo-get-source)

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
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_set_dash" %cairo-set-dash) :void
  (cr (:pointer (:struct cairo-t)))
  (dashes (:pointer :double))
  (num-dashes :int)
  (offset :double))

(defun cairo-set-dash (cr dashes offset)
 #+cl-cffi-gtk-documentation
 "@version{2014-2-8}
  @argument[cr]{a cairo context}
  @argument[dashes]{a list specifying alternate lengths of on and off stroke
    portions}
  @argument[offset]{an offset into the dash pattern at which the stroke should
    start}
  @begin{short}
    Sets the dash pattern to be used by the function @fun{cairo-stroke}.
  @end{short}
  A dash pattern is specified by dashes, a list of positive values. Each value
  provides the length of alternate \"on\" and \"off\" portions of the stroke.
  The offset specifies an offset into the pattern at which the stroke begins.

  Each \"on\" segment will have caps applied as if the segment were a separate
  sub-path. In particular, it is valid to use an \"on\" length of 0.0 with
  @code{:round} or @code{:square} in order to distributed dots or squares along
  a path.

  If @arg{dashes} is an empty list dashing is disabled.

  If @arg{dashes} has one list element a symmetric pattern is assumed with
  alternating on and off portions of the size specified by the single value in
  @arg{dashes}.

  If any value in @arg{dashes} is negative, or if all values are 0, then
  @arg{cr} will be put into an error state with a status of
  @code{:invalid-dash}.

  @begin[Note]{dictionary}
    The length values are in user-space units as evaluated at the time of
    stroking. This is not necessarily the same as the user space at the time of
    the @dym{cairo-set-dash}.
  @end{dictionary}
  Since 1.0
  @see-symbol{cairo-t}
  @see-function{cairo-stroke}"
  (let ((num-dashes (length dashes)))
    (with-foreign-object (dashes-array :double num-dashes)
      (let ((i 0))
        (map nil
             (lambda (x)
               (setf (mem-aref dashes-array :double i) (coerce x 'double-float))
               (incf i))
             dashes))
      (%cairo-set-dash cr
                       dashes-array
                       num-dashes
                       (coerce offset 'double-float)))))

(export 'cairo-set-dash)

;;; ----------------------------------------------------------------------------
;;; cairo_get_dash_count ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_get_dash_count" cairo-get-dash-count) :int
 #+cl-cffi-gtk-documentation
 "@version{2014-2-4}
  @argument[cr]{a cairo context}
  @begin{return}
    The length of the dash array, or 0 if no dash array set.
  @end{return}
  @begin{short}
    This function returns the length of the dash array in @arg{cr}, 0 if
    dashing is not currently in effect.
  @end{short}

  See also the functions @fun{cairo-set-dash} and @fun{cairo-get-dash}.

  Since 1.4
  @see-symbol{cairo-t}
  @see-function{cairo-get-dash}
  @see-function{cairo-set-dash}"
  (cr (:pointer (:struct cairo-t))))

(export 'cairo-get-dash-count)

;;; ----------------------------------------------------------------------------
;;; cairo_get_dash ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_get_dash" %cairo-get-dash) :void
  (cr (:pointer (:struct cairo-t)))
  (dashes (:pointer :double))
  (offset (:pointer :double)))

(defun cairo-get-dash (cr)
 #+cl-cffi-gtk-documentation
 "@version{2014-2-4}
  @argument[cr]{a cairo context}
  @begin{return}
    @code{dashes} -- return value for the dash array, or @code{nil} @br{}
    @code{offset} -- return value for the current dash offset, or @code{nil}
  @end{return}
  @begin{short}
    Gets the current dash list.
  @end{short}

  Since 1.4
  @see-symbol{cairo-t}
  @see-function{cairo-set-dash}"
  (let ((count (cairo-get-dash-count cr))
        (dashes '()))
    (with-foreign-objects ((dash-array :double count) (offset :double))
      (%cairo-get-dash cr dash-array offset)
      (dotimes (i count)
        (push (mem-aref dash-array :double i) dashes))
      (values (nreverse dashes)
              (mem-ref offset :double)))))

(export 'cairo-get-dash)

;;; ----------------------------------------------------------------------------
;;; enum cairo_fill_rule_t
;;; ----------------------------------------------------------------------------

(defcenum cairo-fill-rule-t
  :winding
  :even-odd)

#+cl-cffi-gtk-documentation
(setf (gethash 'cairo-fill-rule-t atdoc:*symbol-name-alias*) "CEnum"
      (gethash 'cairo-fill-rule-t atdoc:*external-symbols*)
 "@version{2014-1-23}
  @begin{short}
    The @sym{cairo-fill-rule-t} enumeration is used to select how paths are
    filled.
  @end{short}
  For both fill rules, whether or not a point is included in the fill is
  determined by taking a ray from that point to infinity and looking at
  intersections with the path. The ray can be in any direction, as long as it
  does not pass through the end point of a segment or have a tricky intersection
  such as intersecting tangent to the path. Note that filling is not actually
  implemented in this way. This is just a description of the rule that is
  applied.

  The default fill rule is @code{:winding}.

  New entries may be added in future versions.
  @begin{pre}
(defcenum cairo-fill-rule-t
  :winding
  :even-odd)
  @end{pre}
  @begin[code]{table}
    @entry[:winding]{If the path crosses the ray from left-to-right, counts +1.
      If the path crosses the ray from right to left, counts -1. Left and right
      are determined from the perspective of looking along the ray from the
      starting point. If the total count is non-zero, the point will be
      filled. Since 1.0}
    @entry[:even-odd]{Counts the total number of intersections, without regard
      to the orientation of the contour. If the total number of intersections
      is odd, the point will be filled. Since 1.0}
  @end{table}
  Since 1.0
  @see-function{cairo-get-fill-rule}
  @see-function{cairo-set-fille-rule}")

(export 'cairo-fill-rule-t)

;;; ----------------------------------------------------------------------------
;;; cairo_set_fill_rule ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_set_fill_rule" cairo-set-fill-rule) :void
 #+cl-cffi-gtk-documentation
 "@version{2014-1-24}
  @argument[cr]{a cairo context}
  @argument[fill-rule]{a fill rule, specified as a @symbol{cairo-fill-rule-t}}
  @begin{short}
    Set the current fill rule within the cairo context.
  @end{short}
  The fill rule is used to determine which regions are inside or outside a
  complex, potentially self-intersecting, path. The current fill rule affects
  both the functions @fun{cairo-fill} and @fun{cairo-clip}. See
  @symbol{cairo-fill-rule-t} for details on the semantics of each available
  fill rule.

  The default fill rule is @code{:winding}.

  Since 1.0
  @see-symbol{cairo-t}
  @see-symbol{cairo-fill-rule-t}
  @see-function{cairo-fill}
  @see-function{cairo-clip}
  @see-function{cairo-get-fill-rule}"
  (cr (:pointer (:struct cairo-t)))
  (fill-rule cairo-fill-rule-t))

(export 'cairo-set-fill-rule)

;;; ----------------------------------------------------------------------------
;;; cairo_get_fill_rule ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_get_fill_rule" cairo-get-fill-rule) cairo-fill-rule-t
 #+cl-cffi-gtk-documentation
 "@version{2014-1-24}
  @argument[cr]{a cairo context}
  @return{The current fill rule of type @symbol{cairo-fill-rule-t}.}
  @begin{short}
    Gets the current fill rule, as set by the function
    @fun{cairo-set-fill-rule}.
  @end{short}

  Since 1.0
  @see-symbol{cairo-t}
  @see-symbol{cairo-fill-rule-t}
  @see-function{cairo-set-fill-rule}"
  (cr (:pointer (:struct cairo-t))))

(export 'cairo-get-fill-rule)

;;; ----------------------------------------------------------------------------
;;; enum cairo_line_cap_t
;;; ----------------------------------------------------------------------------

(defcenum cairo-line-cap-t
  :butt
  :round
  :square)

#+cl-cffi-gtk-documentation
(setf (gethash 'cairo-line-cap-t atdoc:*symbol-name-alias*) "CEnum"
      (gethash 'cairo-line-cap-t atdoc:*external-symbols*)
 "@version{2013-11-12}
  @begin{short}
    Specifies how to render the endpoints of the path when stroking.
  @end{short}

  The default line cap style is @code{:butt}.
  @begin{pre}
(defcenum cairo-line-cap-t
  :butt
  :round
  :square)
  @end{pre}
  @begin[code]{table}
    @entry[:butt]{Start (stop) the line exactly at the start (end) point.
      Since 1.0}
    @entry[:round]{Use a round ending, the center of the circle is the end
      point. Since 1.0}
    @entry[:square]{Use squared ending, the center of the square is the end
      point. Since 1.0}
  @end{table}
  Since 1.0
  @see-function{cairo-get-line-cap}
  @see-function{cairo-set-line-cap}")

(export 'cairo-line-cap-t)

;;; ----------------------------------------------------------------------------
;;; cairo_set_line_cap ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_set_line_cap" cairo-set-line-cap) :void
 #+cl-cffi-gtk-documentation
 "@version{2014-1-24}
  @argument[cr]{a cairo context}
  @argument[line-cap]{a line cap style of type @symbol{cairo-line-cap-t}}
  @begin{short}
    Sets the current line cap style within the cairo context.
  @end{short}
  See the @symbol{cairo-line-cap-t} enumeration for details about how the
  available line cap styles are drawn.

  As with the other stroke parameters, the current line cap style is examined
  by the functions @fun{cairo-stroke} and @fun{cairo-stroke-extents}, but does
  not have any effect during path construction.

  The default line cap style is @code{:butt}.

  Since 1.0
  @see-symbol{cairo-t}
  @see-symbol{cairo-line-cap-t}
  @see-function{cairo-stroke}
  @see-function{cairo-stroke-extents}
  @see-function{cairo-get-line-cap}"
  (cr (:pointer (:struct cairo-t)))
  (line-cap cairo-line-cap-t))

(export 'cairo-set-line-cap)

;;; ----------------------------------------------------------------------------
;;; cairo_get_line_cap ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_get_line_cap" cairo-get-line-cap) cairo-line-cap-t
 #+cl-cffi-gtk-documentation
 "@version{2014-1-24}
  @argument[cr]{a cairo context}
  @return{The current line cap style of type @symbol{cairo-line-cap-t}.}
  @begin{short}
    Gets the current line cap style, as set by the function
    @fun{cairo-set-line-cap}.
  @end{short}

  Since 1.0
  @see-symbol{cairo-t}
  @see-symbol{cairo-line-cap-t}
  @see-function{cairo-set-line-cap}"
  (cr (:pointer (:struct cairo-t))))

(export 'cairo-get-line-cap)

;;; ----------------------------------------------------------------------------
;;; enum cairo_line_join_t
;;; ----------------------------------------------------------------------------

(defcenum cairo-line-join-t
  :miter
  :round
  :bevel)

#+cl-cffi-gtk-documentation
(setf (gethash 'cairo-line-join-t atdoc:*symbol-name-alias*) "CEnum"
      (gethash 'cairo-line-join-t atdoc:*external-symbols*)
 "@version{2013-11-12}
  @begin{short}
    Specifies how to render the junction of two lines when stroking.
  @end{short}

  The default line join style is @code{:miter}.
  @begin{pre}
(defcenum cairo-line-join-t
  :miter
  :round
  :bevel)
  @end{pre}
  @begin[code]{table}
    @entry[:miter]{Use a sharp (angled) corner, see the function
      @fun{cairo-set-miter-limit}. Since 1.0}
    @entry[:round]{Use a rounded join, the center of the circle is the joint
      point. Since 1.0}
    @entry[:bevel]{Use a cut-off join, the join is cut off at half the line
      width from the joint point. Since 1.0}
  @end{table}
  Since 1.0
  @see-function{cairo-set-miter-limit}
  @see-function{cairo-get-line-join}
  @see-function{cairo-set-line-join}")

(export 'cairo-line-join-t)

;;; ----------------------------------------------------------------------------
;;; cairo_set_line_join ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_set_line_join" cairo-set-line-join) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-11-12}
  @argument[cr]{a cairo context}
  @argument[line-join]{a line join style of type @symbol{cairo-line-join-t}}
  @begin{short}
    Sets the current line join style within the cairo context.
  @end{short}
  See the @symbol{cairo-line-join-t} enumeration for details about how the
  available line join styles are drawn.

  As with the other stroke parameters, the current line join style is examined
  by the functions @fun{cairo-stroke} and @fun{cairo-stroke-extents}, but does
  not have any effect during path construction.

  The default line join style is @code{:miter}.

  Since 1.0
  @see-symbol{cairo-t}
  @see-symbol{cairo-line-join-t}
  @see-function{cairo-stroke}
  @see-function{cairo-stroke-extents}
  @see-function{cairo-get-line-join}"
  (cr (:pointer (:struct cairo-t)))
  (line-join cairo-line-join-t))

(export 'cairo-set-line-join)

;;; ----------------------------------------------------------------------------
;;; cairo_get_line_join ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_get_line_join" cairo-get-line-join) cairo-line-join-t
 #+cl-cffi-gtk-documentation
 "@version{2014-1-24}
  @argument[cr]{a cairo context}
  @return{The current line join style of type @symbol{cairo-line-join-t}.}
  @begin{short}
    Gets the current line join style, as set by the function
    @fun{cairo-set-line-join}.
  @end{short}

  Since 1.0
  @see-symbol{cairo-t}
  @see-symbol{cairo-line-join-t}
  @see-function{cairo-set-line-join}"
  (cr (:pointer (:struct cairo-t))))

(export 'cairo-get-line-join)

;;; ----------------------------------------------------------------------------
;;; cairo_set_line_width ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_set_line_width" %cairo-set-line-width) :void
  (cr (:pointer (:struct cairo-t)))
  (width :double))

(defun cairo-set-line-width (cr width)
 #+cl-cffi-gtk-documentation
 "@version{2014-1-24}
  @argument[cr]{a cairo context}
  @argument[width]{a line width}
  @begin{short}
    Sets the current line width within the cairo context.
  @end{short}
  The line width value specifies the diameter of a pen that is circular in user
  space, though device-space pen may be an ellipse in general due to
  scaling/shear/rotation of the Coordinate Transformation Matrix (CTM).

  As with the other stroke parameters, the current line width is examined by
  the functions @fun{cairo-stroke} and @fun{cairo-stroke-extents}, but does not
  have any effect during path construction.

  The default line width value is 2.0.

  @begin[Note]{dictionary}
    When the description above refers to user space and CTM it refers to
    the user space and CTM in effect at the time of the stroking operation, not
    the user space and CTM in effect at the time of the call to the function
    @sym{cairo-set-line-width}. The simplest usage makes both of these spaces
    identical. That is, if there is no change to the CTM between a call to the
    function @sym{cairo-set-line-width} and the stroking operation, then one
    can just pass user-space values to the function @sym{cairo-set-line-width}
    and ignore this note.
  @end{dictionary}

  Since 1.0
  @see-symbol{cairo-t}
  @see-function{cairo-stroke}
  @see-function{cairo-stroke-extents}
  @see-function{cairo-get-line-width}"
  (%cairo-set-line-width cr (coerce width 'double-float)))

(export 'cairo-set-line-width)

;;; ----------------------------------------------------------------------------
;;; cairo_get_line_width ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_get_line_width" cairo-get-line-width) :double
 #+cl-cffi-gtk-documentation
 "@version{2014-1-24}
  @argument[cr]{a cairo context}
  @return{The current line width.}
  @begin{short}
    This function returns the current line width value exactly as set by
    the function @fun{cairo-set-line-width}.
  @end{short}
  Note that the value is unchanged even if the CTM has changed between the calls
  to the functions @fun{cairo-set-line-width} and @sym{cairo-get-line-width}.

  Since 1.0
  @see-symbol{cairo-t}
  @see-function{cairo-set-line-width}"
  (cr (:pointer (:struct cairo-t))))

(export 'cairo-get-line-width)

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
;;; ----------------------------------------------------------------------------

(defcenum cairo-operator-t
  :clear
  :source
  :over
  :in
  :out
  :atop
  :dest
  :dest-over
  :dest-in
  :dest-out
  :dest-atop
  :xor
  :add
  :saturate
  :multiply
  :screen
  :overlay
  :darken
  :lighten
  :color-dodge
  :color-burn
  :hard-light
  :soft-ligth
  :difference
  :exclusion
  :hsl-hue
  :hsl-saturation
  :hsl-color
  :hsl-luminosity)

#+cl-cffi-gtk-documentation
(setf (gethash 'cairo-operator-t atdoc:*symbol-name-alias*) "CEnum"
      (gethash 'cairo-operator-t atdoc:*external-symbols*)
 "@version{2014-1-24}
  @begin{short}
    A @sym{cairo-operator-t} enumeration is used to set the compositing
    operator for all cairo drawing operations.
  @end{short}

  The default operator is @code{:over}.

  The operators marked as unbounded modify their destination even outside of
  the mask layer, that is, their effect is not bound by the mask layer.
  However, their effect can still be limited by way of clipping.

  To keep things simple, the operator descriptions here document the behavior
  for when both source and destination are either fully transparent or fully
  opaque. The actual implementation works for translucent layers too. For a
  more detailed explanation of the effects of each operator, including the
  mathematical definitions, see http://cairographics.org/operators/.
  @begin{pre}
(defcenum cairo-operator-t
  :clear
  :source
  :over
  :in
  :out
  :atop
  :dest
  :dest-over
  :dest-in
  :dest-out
  :dest-atop
  :xor
  :add
  :saturate
  :multiply
  :screen
  :overlay
  :darken
  :lighten
  :color-dodge
  :color-burn
  :hard-light
  :soft-ligth
  :difference
  :exclusion
  :hsl-hue
  :hsl-saturation
  :hsl-color
  :hsl-luminosity)
  @end{pre}
  @begin[code]{table}
    @entry[:clear]{Clear destination layer (bounded). Since 1.0}
    @entry[:source]{Replace destination layer (bounded). Since 1.0}
    @entry[:over]{Draw source layer on top of destination layer (bounded).
      Since 1.0}
    @entry[:in]{Draw source where there was destination content (unbounded).
    Since 1.0}
    @entry[:out]{Draw source where there was no destination content (unbounded).
      Since 1.0}
    @entry[:atop]{Draw source on top of destination content and only there.
      Since 1.0}
    @entry[:dest]{Ignore the source. Since 1.0}
    @entry[:dest-over]{Draw destination on top of source. Since 1.0}
    @entry[:dest-in]{Leave destination only where there was source content
      (unbounded). Since 1.0}
    @entry[:dest-out]{Leave destination only where there was no source content.
      Since 1.0}
    @entry[:dest-atop]{Leave destination on top of source content and only
      there (unbounded). Since 1.0}
    @entry[:xor]{Source and destination are shown where there is only one of
      them. Since 1.0}
    @entry[:add]{Source and destination layers are accumulated. Since 1.0}
    @entry[:saturate]{Like over, but assuming source and dest are disjoint
      geometries. Since 1.0}
    @entry[:multiply]{Source and destination layers are multiplied. This causes
      the result to be at least as dark as the darker inputs. Since 1.10}
    @entry[:screen]{Source and destination are complemented and multiplied. This
      causes the result to be at least as light as the lighter inputs.
      Since 1.10}
    @entry[:overlay]{Multiplies or screens, depending on the lightness of the
      destination color. Since 1.10}
    @entry[:darken]{Replaces the destination with the source if it is darker,
      otherwise keeps the source. Since 1.10.}
    @entry[:lighten]{Replaces the destination with the source if it is lighter,
      otherwise keeps the source. Since 1.10}
    @entry[:dodge]{Brightens the destination color to reflect the source color.
      Since 1.10}
    @entry[:burn]{Darkens the destination color to reflect the source color.
      Since 1.10}
    @entry[:hard-light]{Multiplies or screens, dependent on source color.
      Since 1.10}
    @entry[:soft-light]{Darkens or lightens, dependent on source color.
      Since 1.10}
    @entry[:difference]{Takes the difference of the source and destination
      color. Since 1.10}
    @entry[:exclusion]{Produces an effect similar to difference, but with lower
      contrast. Since 1.10}
    @entry[:hsl-hue]{Creates a color with the hue of the source and the
      saturation and luminosity of the target. Since 1.10}
    @entry[:hsl-saturation]{Creates a color with the saturation of the source
      and the hue and luminosity of the target. Painting with this mode onto a
      gray area produces no change. Since 1.10}
    @entry[:hsl-color]{Creates a color with the hue and saturation of the source
      and the luminosity of the target. This preserves the gray levels of the
      target and is useful for coloring monochrome images or tinting color
      images. Since 1.10}
    @entry[:hsl-luinosity]{Creates a color with the luminosity of the source and
      the hue and saturation of the target. This produces an inverse effect to
      @code{:hsl-color}. Since 1.10}
  @end{table}
  Since 1.0")

(export 'cairo-operator-t)

;;; ----------------------------------------------------------------------------
;;; cairo_set_operator ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_set_operator" cairo-set-operator) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-8-4}
  @argument[cr]{a @symbol{cairo-t}}
  @argument[op]{a compositing operator, specified as a
    @symbol{cairo-operator-t}}
  @begin{short}
    Sets the compositing operator to be used for all drawing operations. See the
    @symbol{cairo-operator-t} enumeration for details on the semantics of each
    available compositing operator.
  @end{short}

  The default operator is @code{:over}.

  Since 1.0
  @see-symbol{cairo-t}
  @see-symbol{cairo-operator-t}
  @see-function{cairo-get-operator}"
  (cr (:pointer (:struct cairo-t)))
  (op cairo-operator-t))

(export 'cairo-set-operator)

;;; ----------------------------------------------------------------------------
;;; cairo_get_operator ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_get_operator" cairo-get-operator) cairo-operator-t
 #+cl-cffi-gtk-documentation
 "@version{2013-8-4}
  @argument[cr]{a cairo context}
  @return{The current compositing operator of type @symbol{cairo-operator-t}.}
  @begin{short}
    Gets the current compositing operator for a cairo context.
  @end{short}

  Since 1.0
  @see-symbol{cairo-t}
  @see-symbol{cairo-operator-t}
  @see-function{cairo-set-operator}"
  (cr (:pointer (:struct cairo-t))))

(export 'cairo-get-operator)

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
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_clip" cairo-clip) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-8-4}
  @argument[cr]{a cairo context}
  @begin{short}
    Establishes a new clip region by intersecting the current clip region with
    the current path as it would be filled by the function @fun{cairo-fill} and
    according to the current fill rule, see the function
    @fun{cairo-set-fill-rule}.
  @end{short}

  After a call of the function @sym{cairo-clip}, the current path will be
  cleared from the cairo context.

  The current clip region affects all drawing operations by effectively
  masking out any changes to the surface that are outside the current clip
  region.

  Calling the function @sym{cairo-clip} can only make the clip region smaller,
  never larger. But the current clip is part of the graphics state, so a
  temporary restriction of the clip region can be achieved by calling
  the function @sym{cairo-clip} within a @fun{cairo-save}/@fun{cairo-restore}
  pair. The only other means of increasing the size of the clip region is
  the function @fun{cairo-reset-clip}.

  Since 1.0
  @see-symbol{cairo-t}
  @see-function{cairo-fill}
  @see-function{cairo-set-fill-rule}
  @see-function{cairo-save}
  @see-function{cairo-restore}
  @see-function{cairo-reset-clip}"
  (cr (:pointer (:struct cairo-t))))

(export 'cairo-clip)

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
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_reset_clip" cairo-reset-clip) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-12-2}
  @argument[cr]{a cairo context}
  @begin{short}
    Reset the current clip region to its original, unrestricted state.
  @end{short}
  That is, set the clip region to an infinitely large shape containing the
  target surface. Equivalently, if infinity is too hard to grasp, one can
  imagine the clip region being reset to the exact bounds of the target surface.

  Note that code meant to be reusable should not call the function
  @sym{cairo-reset-clip} as it will cause results unexpected by higher-level
  code which calls the function @fun{cairo-clip}. Consider using the functions
  @fun{cairo-save} and @fun{cairo-restore} around the function @fun{cairo-clip}
  as a more robust means of temporarily restricting the clip region.

  Since 1.0
  @see-symbol{cairo-t}
  @see-function{cairo-clip}
  @see-function{cairo-save}
  @see-function{cairo-restore}"
  (cr (:pointer (:struct cairo-t))))

(export 'cairo-reset-clip)

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
 "@version{2013-8-4}
  @argument[cr]{a cairo context}
  @begin{short}
    A drawing operator that fills the current path according to the current fill
    rule, each sub-path is implicitly closed before being filled.
  @end{short}
  After the function @sym{cairo-fill}, the current path will be cleared from the
  cairo context. See the functions @fun{cairo-set-fill-rule} and
  @fun{cairo-fill-preserve}.

  Since 1.0
  @see-symbol{cairo-t}
  @see-function{cairo-set-fill-rule}
  @see-function{cairo-fill-preserve}"
  (cr (:pointer (:struct cairo-t))))

(export 'cairo-fill)

;;; ----------------------------------------------------------------------------
;;; cairo_fill_preserve ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_fill_preserve" cairo-fill-preserve) :void
 #+cl-cffi-gtk-documentation
 "@version{2014-1-24}
  @argument[cr]{a cairo context}
  @begin{short}
    A drawing operator that fills the current path according to the current
    fill rule, each sub-path is implicitly closed before being filled.
  @end{short}
  Unlike the function @fun{cairo-fill}, @sym{cairo-fill-preserve} preserves the
  path within the cairo context.

  See the functions @fun{cairo-set-fill-rule} and @fun{cairo-fill}.

  Since 1.0
  @see-symbol{cairo-t}
  @see-function{cairo-fill}
  @see-function{cairo-set-fill-rule}"
  (cr (:pointer (:struct cairo-t))))

(export 'cairo-fill-preserve)

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
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_mask" cairo-mask) :void
 #+cl-cffi-gtk-documentation
 "@version{2014-1-24}
  @argument[cr]{a cairo context}
  @argument[pattern]{a @symbol{cairo-pattern-t}}
  @begin{short}
    A drawing operator that paints the current source using the alpha channel of
    @arg{pattern} as a mask.
  @end{short}
  Opaque areas of @arg{pattern} are painted with the source, transparent areas
  are not painted.

  Since 1.0
  @see-symbol{cairo-t}
  @see-symbol{cairo-pattern-t}"
  (cr (:pointer (:struct cairo-t)))
  (pattern (:pointer (:struct cairo-pattern-t))))

(export 'cairo-mask)

;;; ----------------------------------------------------------------------------
;;; cairo_mask_surface ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_mask_surface" %cairo-mask-surface) :void
  (cr (:pointer (:struct cairo-t)))
  (surface (:pointer (:struct cairo-surface-t)))
  (surface-x :double)
  (surface-y :double))

(defun cairo-mask-surface (cr surface surface-x surface-y)
 #+cl-cffi-gtk-documentation
 "@version{2014-2-1}
  @argument[cr]{a cairo context}
  @argument[surface]{a @symbol{cairo-surface-t} structure}
  @argument[surface-x]{x coordinate at which to place the origin of
    @arg{surface}}
  @argument[surface-y]{y coordinate at which to place the origin of
    @arg{surface}}
  @begin{short}
    A drawing operator that paints the current source using the alpha channel of
    @arg{surface} as a mask.
  @end{short}
  Opaque areas of @arg{surface} are painted with the source, transparent areas
  are not painted.

  Since 1.0
  @see-symbol{cairo-t}
  @see-symbol{cairo-surface-t}"
  (%cairo-mask-surface cr
                       surface
                       (coerce surface-x 'double-float)
                       (coerce surface-y 'double-float)))

(export 'cairo-mask-surface)

;;; ----------------------------------------------------------------------------
;;; cairo_paint ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_paint" cairo-paint) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-8-4}
  @argument[cr]{a cairo context}
  @begin{short}
    A drawing operator that paints the current source everywhere within the
    current clip region.
  @end{short}

  Since 1.0
  @see-symbol{cairo-t}"
  (cr (:pointer (:struct cairo-t))))

(export 'cairo-paint)

;;; ----------------------------------------------------------------------------
;;; cairo_paint_with_alpha ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_paint_with_alpha" cairo-paint-with-alpha) :void
 #+cl-cffi-gtk-documentation
 "@version{2014-1-24}
  @argument[cr]{a cairo context}
  @argument[alpha]{alpha value, between 0 (transparent) and 1 (opaque)}
  @begin{short}
    A drawing operator that paints the current source everywhere within the
    current clip region using a mask of constant alpha value @arg{alpha}.
  @end{short}
  The effect is similar to the function @fun{cairo-paint}, but the drawing is
  faded out using the alpha value.

  Since 1.0
  @see-symbol{cairo-t}
  @see-function{cairo-paint}"
  (cr (:pointer (:struct cairo-t)))
  (alpha :double))

(export 'cairo-paint-with-alpha)

;;; ----------------------------------------------------------------------------
;;; cairo_stroke ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_stroke" cairo-stroke) :void
 #+cl-cffi-gtk-documentation
 "@version{2014-1-24}
  @argument[cr]{a cairo context}
  @begin{short}
    A drawing operator that strokes the current path according to the current
    line width, line join, line cap, and dash settings. After the function
    @sym{cairo-stroke}, the current path will be cleared from the cairo context.
  @end{short}
  See the functions @fun{cairo-set-line-width}, @fun{cairo-set-line-join},
  @fun{cairo-set-line-cap}, @fun{cairo-set-dash}, and
  @fun{cairo-stroke-preserve}.

  @begin[Note]{dictionary}
    Degenerate segments and sub-paths are treated specially and provide
    a useful result. These can result in two different situations:
    @begin{enumerate}
      @begin{item}
        Zero-length \"on\" segments set in the function @fun{cairo-set-dash}.
        If the cap style of type @symbol{cairo-line-cap-t} is @code{:round} or
        @code{:square} then these segments will be drawn as circular dots or
        squares respectively. In the case of @code{:square}, the orientation of
        the squares is determined by the direction of the underlying path.
      @end{item}
      @begin{item}
        A sub-path created by the function @fun{cairo-move-to} followed by
        either a call to the function @fun{cairo-close-path} or one or more
        calls to the function @fun{cairo-line-to} to the same coordinate as the
        the function @fun{cairo-move-to}. If the cap style is @code{:round}
        then these sub-paths will be drawn as circular dots. Note that in the
        case of @code{:square} a degenerate sub-path will not be drawn at all,
        since the correct orientation is indeterminate.
      @end{item}
    @end{enumerate}
    In no case will a cap style of @code{:butt} cause anything to be drawn in
    the case of either degenerate segments or sub-paths.
  @end{dictionary}

  Since 1.0
  @see-symbol{cairo-t}
  @see-symbol{cairo-line-cap-t}
  @see-function{cairo-set-line-width}
  @see-function{cairo-set-line-join}
  @see-function{cairo-set-line-cap}
  @see-function{cairo-set-dash}
  @see-function{cairo-stroke-preserve}
  @see-function{cairo-move-to}
  @see-function{cairo-close-path}
  @see-function{cairo-line-to}"
  (cr (:pointer (:struct cairo-t))))

(export 'cairo-stroke)

;;; ----------------------------------------------------------------------------
;;; cairo_stroke_preserve ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_stroke_preserve" cairo-stroke-preserve) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-11-11}
  @argument[cr]{a cairo context}
  @begin{short}
    A drawing operator that strokes the current path according to the current
    line width, line join, line cap, and dash settings.
  @end{short}
  Unlike the function @fun{cairo-stroke}, @sym{cairo-stroke-preserve} preserves
  the path within the cairo context.

  See the functions @fun{cairo-set-line-width}, @fun{cairo-set-line-join},
  @fun{cairo-set-line-cap}, and @fun{cairo-set-dash}.

  Since 1.0
  @see-symbol{cairo-t}
  @see-function{cairo-stroke}
  @see-function{cairo-set-line-width}
  @see-function{cairo-set-line-join}
  @see-function{cairo-set-line-cap}
  @see-function{cairo-set-dash}"
  (cr (:pointer (:struct cairo-t))))

(export 'cairo-stroke-preserve)

;;; ----------------------------------------------------------------------------
;;; cairo_stroke_extents ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_stroke_extents" cairo-stroke-extents) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-12-28}
  @argument[cr]{a cairo context}
  @argument[x1]{left of the resulting extents}
  @argument[y1]{top of the resulting extents}
  @argument[x2]{right of the resulting extents}
  @argument[y2]{bottom of the resulting extents}
  @begin{short}
    Computes a bounding box in user coordinates covering the area that would be
    affected, the \"inked\" area, by a @fun{cairo-stroke} operation given the
    current path and stroke parameters.
  @end{short}
  If the current path is empty, returns an empty rectangle ((0,0), (0,0)).
  Surface dimensions and clipping are not taken into account.

  Note that if the line width is set to exactly zero, then the function
  @sym{cairo-stroke-extents} will return an empty rectangle. Contrast with the
  function @fun{cairo-path-extents} which can be used to compute the non-empty
  bounds as the line width approaches zero.

  Note that the function @sym{cairo-stroke-extents} must necessarily do more
  work to compute the precise inked areas in light of the stroke parameters, so
  the function @fun{cairo-path-extents} may be more desirable for sake of
  performance if non-inked path extents are desired.

  See the functions @fun{cairo-stroke}, @fun{cairo-set-line-width},
  @fun{cairo-set-line-join}, @fun{cairo-set-line-cap}, @fun{cairo-set-dash},
  and @fun{cairo-stroke-preserve}.

  Since 1.0
  @see-symbol{cairo-t}
  @see-funcion{cairo-stroke}
  @see-function{cairo-path-extents}
  @see-function{cairo-set-line-width}
  @see-function{cairo-set-line-join}
  @see-function{cairo-set-line-cap}
  @see-function{cairo-set-dash}
  @see-function{cairo-stroke-preserve}"
  (cr (:pointer (:struct cairo-t)))
  (x1 :double)
  (y1 :double)
  (x2 :double)
  (y2 :double))

(export 'cairo-stroke-extents)

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
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_get_reference_count" cairo-get-reference-count) :uint
 #+cl-cffi-gtk-documentation
 "@version{2013-11-18}
  @argument[cr]{a @symbol{cairo-t}}
  @begin{return}
    The current reference count of @arg{cr}. If the object is a @code{nil}
    object, 0 will be returned.
  @end{return}
  @begin{short}
    Returns the current reference count of @arg{cr}.
  @end{short}

  Since 1.4
  @see-symbol{cairo-t}"
  (cr (:pointer (:struct cairo-t))))

(export 'cairo-get-reference-count)

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
