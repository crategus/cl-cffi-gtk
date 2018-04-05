;;; ----------------------------------------------------------------------------
;;; cairo.surface.lisp
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
;;; cairo_surface_t
;;;
;;; Base class for surfaces
;;;
;;; Synopsis
;;;
;;;     CAIRO_HAS_MIME_SURFACE
;;;     CAIRO_MIME_TYPE_JP2
;;;     CAIRO_MIME_TYPE_JPEG
;;;     CAIRO_MIME_TYPE_PNG
;;;     CAIRO_MIME_TYPE_URI
;;;     CAIRO_MIME_TYPE_UNIQUE_ID
;;;
;;;     cairo_surface_t
;;;     cairo_content_t
;;;     cairo_format_t     <--- cairo.image-surface.lisp
;;;
;;;     cairo_surface_create_similar
;;;     cairo_surface_create_similar_image
;;;     cairo_surface_create_for_rectangle
;;;     cairo_surface_reference
;;;     cairo_surface_destroy
;;;     cairo_surface_status
;;;     cairo_surface_finish
;;;     cairo_surface_flush
;;;     cairo_surface_get_device
;;;     cairo_surface_get_font_options
;;;     cairo_surface_get_content
;;;     cairo_surface_mark_dirty
;;;     cairo_surface_mark_dirty_rectangle
;;;     cairo_surface_set_device_offset
;;;     cairo_surface_get_device_offset
;;;     cairo_surface_set_fallback_resolution
;;;     cairo_surface_get_fallback_resolution
;;;
;;;     cairo_surface_type_t
;;;
;;;     cairo_surface_get_type
;;;     cairo_surface_get_reference_count
;;;     cairo_surface_set_user_data
;;;     cairo_surface_get_user_data
;;;     cairo_surface_copy_page
;;;     cairo_surface_show_page
;;;     cairo_surface_has_show_text_glyphs
;;;     cairo_surface_set_mime_data
;;;     cairo_surface_get_mime_data
;;;     cairo_surface_supports_mime_type
;;;     cairo_surface_map_to_image
;;;     cairo_surface_unmap_image
;;;
;;; Description
;;;
;;; cairo_surface_t is the abstract type representing all different drawing
;;; targets that cairo can render to. The actual drawings are performed using a
;;; cairo context.
;;;
;;; A cairo surface is created by using backend-specific constructors, typically
;;; of the form cairo_backend_surface_create().
;;;
;;; Most surface types allow accessing the surface without using Cairo
;;; functions. If you do this, keep in mind that it is mandatory that you call
;;; cairo_surface_flush() before reading from or writing to the surface and that
;;; you must use cairo_surface_mark_dirty() after modifying it.
;;;
;;; Example 1. Directly modifying an image surface
;;;
;;; void
;;; modify_image_surface (cairo_surface_t *surface)
;;; {
;;;   unsigned char *data;
;;;   int width, height, stride;
;;;
;;;   // flush to ensure all writing to the image was done
;;;   cairo_surface_flush (surface);
;;;
;;;   // modify the image
;;;   data = cairo_image_surface_get_data (surface);
;;;   width = cairo_image_surface_get_width (surface);
;;;   height = cairo_image_surface_get_height (surface);
;;;   stride = cairo_image_surface_get_stride (surface);
;;;   modify_image_data (data, width, height, stride);
;;;
;;;   // mark the image dirty so Cairo clears its caches.
;;;   cairo_surface_mark_dirty (surface);
;;; }
;;;
;;; Note that for other surface types it might be necessary to acquire the
;;; surface's device first. See cairo_device_acquire() for a discussion of
;;; devices.
;;; ----------------------------------------------------------------------------

(in-package :cairo)

;;; ----------------------------------------------------------------------------
;;; CAIRO_HAS_MIME_SURFACE
;;;
;;; #define CAIRO_HAS_MIME_SURFACE 1
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; CAIRO_MIME_TYPE_JP2
;;;
;;; #define CAIRO_MIME_TYPE_JP2 "image/jp2"
;;;
;;; The Joint Photographic Experts Group (JPEG) 2000 image coding standard
;;; (ISO/IEC 15444-1).
;;;
;;; Since 1.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; CAIRO_MIME_TYPE_JPEG
;;;
;;; #define CAIRO_MIME_TYPE_JPEG "image/jpeg"
;;;
;;; The Joint Photographic Experts Group (JPEG) image coding standard (ISO/IEC
;;; 10918-1).
;;;
;;; Since 1.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; CAIRO_MIME_TYPE_PNG
;;;
;;; #define CAIRO_MIME_TYPE_PNG "image/png"
;;;
;;; The Portable Network Graphics image file format (ISO/IEC 15948).
;;;
;;; Since 1.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; CAIRO_MIME_TYPE_URI
;;;
;;; #define CAIRO_MIME_TYPE_URI "text/x-uri"
;;;
;;; URI for an image file (unofficial MIME type).
;;;
;;; Since 1.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; CAIRO_MIME_TYPE_UNIQUE_ID
;;;
;;; #define CAIRO_MIME_TYPE_UNIQUE_ID "application/x-cairo.uuid"
;;;
;;; Unique identifier for a surface (cairo specific MIME type).
;;;
;;; Since 1.12
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_surface_t
;;; ----------------------------------------------------------------------------

(defcstruct cairo-surface-t)

#+cl-cffi-gtk-documentation
(setf (gethash 'cairo-surface-t atdoc:*symbol-name-alias*) "CStruct"
      (gethash 'cairo-surface-t atdoc:*external-symbols*)
 "@version{2014-1-23}
  @begin{short}
    A @sym{cairo-surface-t} structure represents an image, either as the
    destination of a drawing operation or as source when drawing onto another
    surface. To draw to a @sym{cairo-surface-t} structure, create a cairo
    context with the surface as the target, using the function
    @fun{cairo-create}.
  @end{short}

  There are different subtypes of a @sym{cairo-surface-t} structure for
  different drawing backends; for example, the function
  @fun{cairo-image-surface-create} creates a bitmap image in memory. The type of
  a surface can be queried with the function @fun{cairo-surface-get-type}.

  The initial contents of a surface after creation depend upon the manner of
  its creation. If cairo creates the surface and backing storage for the user,
  it will be initially cleared; for example, the functions
  @fun{cairo-image-surface-create} and @fun{cairo-surface-create-similar}.
  Alternatively, if the user passes in a reference to some backing storage and
  asks cairo to wrap that in a @sym{cairo-surface-t} structure, then the
  contents are not modified; for example, the function
  @fun{cairo-image-surface-create-for-data}.

  Memory management of a @sym{cairo-surface-t} structure is done with the
  functions @fun{cairo-surface-reference} and @fun{cairo-surface-destroy}.

  Since 1.0
  @see-function{cairo-create}
  @see-function{cairo-image-surface-create}
  @see-function{cairo-surface-create-similar}
  @see-function{cairo-surface-get-type}
  @see-function{cairo-image-surface-create-for-data}
  @see-function{cairo-surface-reference}
  @see-function{cairo-surface-destroy}")

(export 'cairo-surface-t)

;;; ----------------------------------------------------------------------------
;;; enum cairo_content_t
;;; ----------------------------------------------------------------------------

(defcenum cairo-content-t
  (:color #x1000)
  (:alpha #x2000)
  (:color-alpha #x3000))

#+cl-cffi-gtk-documentation
(setf (gethash 'cairo-content-t atdoc:*symbol-name-alias*) "CEnum"
      (gethash 'cairo-content-t atdoc:*external-symbols*)
 "@version{2013-10-20}
  @begin{short}
    @sym{cairo-content-t} is used to describe the content that a surface will
    contain, whether color information, alpha information (translucence vs.
    opacity), or both.
  @end{short}

  Note: The large values here are designed to keep @sym{cairo-content-t} values
  distinct from @symbol{cairo-format-t} values so that the implementation can
  detect the error if users confuse the two types.
  @begin{pre}
(defcenum cairo-content-t
  (:color #x1000)
  (:alpha #x2000)
  (:color-alpha #x3000))
  @end{pre}
  @begin[code]{table}
    @entry[:color]{The surface will hold color content only. (Since 1.0)}
    @entry[:alpha]{The surface will hold alpha content only. (Since 1.0)}
    @entry[:color-alpha]{The surface will hold color and alpha content.
      (Since 1.0)}
  @end{table}
  Since 1.0
  @see-symbol{cairo-format-t}")

(export 'cairo-content-t)

;;; ----------------------------------------------------------------------------
;;; enum cairo_format_t
;;; ----------------------------------------------------------------------------

(defcenum cairo-format-t
  (:invalid -1)
  (:argb32 0)
  (:rgb24 1)
  (:a8 2)
  (:a1 3)
  (:rgb16-565 4)
  (:rgb30 5))

#+cl-cffi-gtk-documentation
(setf (gethash 'cairo-format-t atdoc:*symbol-name-alias*) "CEnum"
      (gethash 'cairo-format-t atdoc:*external-symbols*)
 "@version{2013-12-28}
  @begin{short}
    @sym{cairo-format-t} is used to identify the memory format of image data.
  @end{short}

  New entries may be added in future versions.
  @begin{pre}
(defcenum cairo-format-t
  (:invalid -1)
  (:argb32 0)
  (:rgb24 1)
  (:a8 2)
  (:a1 3)
  (:rgb16-565 4)
  (:rgb30 5))
  @end{pre}
  @begin[code]{table}
    @entry[:invalid]{No such format exists or is supported.}
    @entry[:argb32]{Each pixel is a 32-bit quantity, with alpha in the upper
      8 bits, then red, then green, then blue. The 32-bit quantities are stored
      native-endian. Pre-multiplied alpha is used. That is, 50 % transparent
      red is 0x80800000, not 0x80ff0000. Since 1.0.}
    @entry[:rgb24]{Each pixel is a 32-bit quantity, with the upper 8 bits
      unused. Red, Green, and Blue are stored in the remaining 24 bits in that
      order. Since 1.0.}
    @entry[:a8]{Each pixel is a 8-bit quantity holding an alpha value.
      Since 1.0.}
    @entry[:a1]{Each pixel is a 1-bit quantity holding an alpha value. Pixels
      are packed together into 32-bit quantities. The ordering of the bits
      matches the endianess of the platform. On a big-endian machine, the first
      pixel is in the uppermost bit, on a little-endian machine the first pixel
      is in the least-significant bit. Since 1.0.}
    @entry[:rgb16-565]{Each pixel is a 16-bit quantity with red in the upper 5
      bits, then green in the middle 6 bits, and blue in the lower 5 bits.
      Since 1.2.}
    @entry[:rgb30]{Like @code{:rgb24} but with 10 bpc. Since 1.12.}
  @end{table}
  Since 1.0
  @see-symbol{cairo-surface-t}")

(export 'cairo-format-t)

;;; ----------------------------------------------------------------------------
;;; cairo_surface_create_similar ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_surface_create_similar" cairo-surface-create-similar)
    (:pointer (:struct cairo-surface-t))
 #+cl-cffi-gtk-documentation
 "@version{2013-10-20}
  @argument[other]{an existing surface used to select the backend of the new
    surface}
  @argument[content]{the content for the new surface}
  @argument[width]{width of the new surface, (in device-space units)}
  @argument[height]{height of the new surface (in device-space units)}
  @begin{return}
    A pointer to the newly allocated surface. The caller owns the surface
    and should call the function @fun{cairo-surface-destroy} when done with it.
    This function always returns a valid pointer, but it will return a pointer
    to a \"nil\" surface if other is already in an error state or any other
    error occurs.
  @end{return}
  @begin{short}
    Create a new surface that is as compatible as possible with an existing
    surface.
  @end{short}
  For example the new surface will have the same fallback resolution and font
  options as other. Generally, the new surface will also use the same backend as
  other, unless that is not possible for some reason. The type of the returned
  surface may be examined with the function @fun{cairo-surface-get-type}.

  Initially the surface contents are all 0; transparent if contents have
  transparency, black otherwise.

  Use the function @fun{cairo-surface-create-similar-image} if you need an image
  surface which can be painted quickly to the target surface.

  Since 1.0
  @see-symbol{cairo-surface-t}
  @see-symbol{cairo-content-t}
  @see-function{cairo-surface-destroy}
  @see-function{cairo-surface-create-similar-image}"
  (other (:pointer (:struct cairo-surface-t)))
  (content cairo-content-t)
  (width :int)
  (height :int))

(export 'cairo-surface-create-similar)

;;; ----------------------------------------------------------------------------
;;; cairo_surface_create_similar_image ()
;;; ----------------------------------------------------------------------------

#+cairo-1-12
(defcfun ("cairo_surface_create_similar_image"
           cairo-surface-create-similar-image)
    (:pointer (:struct cairo-surface-t))
 #+cl-cffi-gtk-documentation
 "@version{2013-11-11}
  @argument[other]{an existing surface used to select the preference of the new
    surface}
  @argument[format]{the format of type @symbol{cairo-format-t} for the new
    surface}
  @argument[width]{width of the new surface, in device-space units}
  @argument[height]{height of the new surface, in device-space units}
  @begin{return}
    A pointer to the newly allocated image surface. The caller owns the
    surface and should call the function @fun{cairo-surface-destroy} when done
    with it. This function always returns a valid pointer, but it will return a
    pointer to a \"nil\" surface if other is already in an error state or any
    other error occurs.
  @end{return}
  @begin{short}
    Create a new image surface that is as compatible as possible for uploading
    to and the use in conjunction with an existing surface.
  @end{short}
  However, this surface can still be used like any normal image surface.

  Initially the surface contents are all 0, transparent if contents have
  transparency, black otherwise.

  Use the function @fun{cairo-surface-create-similar} if you do not need an
  image surface.

  Since 1.12
  @see-symbol{cairo-surface-t}
  @see-symbol{cairo-format-t}
  @see-function{cairo-surface-destroy}
  @see-function{cairo-surface-create-similar}"
  (other (:pointer (:struct cairo-surface-t)))
  (format cairo-format-t)
  (width :int)
  (height :int))

#+cairo-1-12
(export 'cairo-surface-create-similar-image)

;;; ----------------------------------------------------------------------------
;;; cairo_surface_create_for_rectangle ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_surface_create_for_rectangle"
           cairo-surface-create-for-rectangle)
    (:pointer (:struct cairo-surface-t))
 #+cl-cffi-gtk-documentation
 "@version{2013-11-11}
  @argument[target]{an existing surface for which the sub-surface will point to}
  @argument[x]{the x-origin of the sub-surface from the top-left of the target
    surface, in device-space units}
  @argument[y]{the y-origin of the sub-surface from the top-left of the target
    surface, in device-space units}
  @argument[width]{width of the sub-surface, in device-space units}
  @argument[height]{height of the sub-surface, in device-space units}
  @begin{return}
    A pointer to the newly allocated surface. The caller owns the surface
    and should call the function @fun{cairo-surface-destroy} when done with it.
    This function always returns a valid pointer, but it will return a pointer
    to a \"nil\" surface if other is already in an error state or any other
    error occurs.
  @end{return}
  @begin{short}
    Create a new surface that is a rectangle within the target surface.
  @end{short}
  All operations drawn to this surface are then clipped and translated onto the
  target surface. Nothing drawn via this sub-surface outside of its bounds is
  drawn onto the target surface, making this a useful method for passing
  constrained child surfaces to library routines that draw directly onto the
  parent surface, i. e. with no further backend allocations, double buffering
  or copies.

  @subheading{Note}
    The semantics of subsurfaces have not been finalized yet unless the
    rectangle is in full device units, is contained within the extents of the
    target surface, and the target or subsurface's device transforms are not
    changed.

  Since 1.10
  @see-symbol{cairo-surface-t}
  @see-function{cairo-surface-destroy}"
  (target (:pointer (:struct cairo-surface-t)))
  (x :double)
  (y :double)
  (width :double)
  (height :double))

(export 'cairo-surface-create-for-rectangle)

;;; ----------------------------------------------------------------------------
;;; cairo_surface_reference ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_surface_reference" cairo-surface-reference)
    (:pointer (:struct cairo-surface-t))
 #+cl-cffi-gtk-documentation
 "@version{2013-10-20}
  @argument[surface]{a @symbol{cairo-surface-t}}
  @return{The referenced @symbol{cairo-surface-t}.}
  @begin{short}
    Increases the reference count on @arg{surface} by one.
  @end{short}
  This prevents @arg{surface} from being destroyed until a matching call to
  the function @fun{cairo-surface-destroy} is made.

  The number of references to a @symbol{cairo-surface-t} can be get using
  the function @fun{cairo-surface-get-reference-count}.

  Since 1.0
  @see-symbol{cairo-surface-t}
  @see-function{cairo-surface-destroy}
  @see-function{cairo-surface-get-reference-count}"
  (surface (:pointer (:struct cairo-surface-t))))

(export 'cairo-surface-reference)

;;; ----------------------------------------------------------------------------
;;; cairo_surface_destroy ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_surface_destroy" cairo-surface-destroy) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-3-3}
  @argument[surface]{a cairo_surface_t}
  @begin{short}
    Decreases the reference count on surface by one. If the result is zero,
    then surface and all associated resources are freed.
  @end{short}
  See cairo_surface_reference().

  Since 1.0"
  (surface (:pointer (:struct cairo-surface-t))))

(export 'cairo-surface-destroy)

;;; ----------------------------------------------------------------------------
;;; cairo_surface_status ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_surface_status" cairo-surface-status) cairo-status-t
 #+cl-cffi-gtk-documentation
 "@version{2013-11-14}
  @argument[surface]{a @symbol{cairo-surface-t}}
  @begin{return}
    @code{:success}, @code{:null-pointer}, @code{:no-memory},
    @code{:read-error}, @code{:invalid-content}, @code{:invalid-format}, or
    @code{:invalid-visual}.
  @end{return}
  @begin{short}
    Checks whether an error has previously occurred for this surface.
  @end{short}

  Since 1.0
  @see-symbol{cairo-surface-t}
  @see-symbol{cairo-status-t}"
  (surface (:pointer (:struct cairo-surface-t))))

(export 'cairo-surface-status)

;;; ----------------------------------------------------------------------------
;;; cairo_surface_finish ()
;;;
;;; void cairo_surface_finish (cairo_surface_t *surface);
;;;
;;; This function finishes the surface and drops all references to external
;;; resources. For example, for the Xlib backend it means that cairo will no
;;; longer access the drawable, which can be freed. After calling
;;; cairo_surface_finish() the only valid operations on a surface are getting
;;; and setting user, referencing and destroying, and flushing and finishing it.
;;; Further drawing to the surface will not affect the surface but will instead
;;; trigger a CAIRO_STATUS_SURFACE_FINISHED error.
;;;
;;; When the last call to cairo_surface_destroy() decreases the reference count
;;; to zero, cairo will call cairo_surface_finish() if it hasn't been called
;;; already, before freeing the resources associated with the surface.
;;;
;;; surface :
;;;     the cairo_surface_t to finish
;;;
;;; Since 1.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_surface_flush ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_surface_flush" cairo-surface-flush) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-10-20}
  @argument[surface]{a @symbol{cairo-surface-t}}
  @begin{short}
    Do any pending drawing for the surface and also restore any temporary
    modifications cairo has made to the surface's state.
  @end{short}
  This function must be called before switching from drawing on the surface with
  cairo to drawing on it directly with native APIs. If the surface does not
  support direct access, then this function does nothing.

  Since 1.0
  @see-symbol{cairo-surface-t}"
  (surface (:pointer (:struct cairo-surface-t))))

(export 'cairo-surface-flush)

;;; ----------------------------------------------------------------------------
;;; cairo_surface_get_device ()
;;;
;;; cairo_device_t * cairo_surface_get_device (cairo_surface_t *surface);
;;;
;;; This function returns the device for a surface. See cairo_device_t.
;;;
;;; surface :
;;;     a cairo_surface_t
;;;
;;; Returns :
;;;     The device for surface or NULL if the surface does not have an
;;;     associated device.
;;;
;;; Since 1.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_surface_get_font_options ()
;;;
;;; void cairo_surface_get_font_options (cairo_surface_t *surface,
;;;                                      cairo_font_options_t *options);
;;;
;;; Retrieves the default font rendering options for the surface. This allows
;;; display surfaces to report the correct subpixel order for rendering on them,
;;; print surfaces to disable hinting of metrics and so forth. The result can
;;; then be used with cairo_scaled_font_create().
;;;
;;; surface :
;;;     a cairo_surface_t
;;;
;;; options :
;;;     a cairo_font_options_t object into which to store the retrieved options.
;;;     All existing values are overwritten
;;;
;;; Since 1.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_surface_get_content ()
;;;
;;; cairo_content_t cairo_surface_get_content (cairo_surface_t *surface);
;;;
;;; This function returns the content type of surface which indicates whether
;;; the surface contains color and/or alpha information. See cairo_content_t.
;;;
;;; surface :
;;;     a cairo_surface_t
;;;
;;; Returns :
;;;     The content type of surface.
;;;
;;; Since 1.2
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_surface_mark_dirty ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_surface_mark_dirty" cairo-surface-mark-dirty) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-10-20}
  @argument[surface]{a @symbol{cairo-surface-t}}
  @begin{short}
    Tells cairo that drawing has been done to surface using means other than
    cairo, and that cairo should reread any cached areas.
  @end{short}
  Note that you must call the function @fun{cairo-surface-flush} before doing
  such drawing.

  Since 1.0
  @see-symbol{cairo-surface-t}
  @see-function{cairo-surface-flush}"
  (surface (:pointer (:struct cairo-surface-t))))

(export 'cairo-surface-mark-dirty)

;;; ----------------------------------------------------------------------------
;;; cairo_surface_mark_dirty_rectangle ()
;;;
;;; void cairo_surface_mark_dirty_rectangle (cairo_surface_t *surface,
;;;                                          int x,
;;;                                          int y,
;;;                                          int width,
;;;                                          int height);
;;;
;;; Like cairo_surface_mark_dirty(), but drawing has been done only to the
;;; specified rectangle, so that cairo can retain cached contents for other
;;; parts of the surface.
;;;
;;; Any cached clip set on the surface will be reset by this function, to make
;;; sure that future cairo calls have the clip set that they expect.
;;;
;;; surface :
;;;     a cairo_surface_t
;;;
;;; x :
;;;     X coordinate of dirty rectangle
;;;
;;; y :
;;;     Y coordinate of dirty rectangle
;;;
;;; width :
;;;     width of dirty rectangle
;;;
;;; height :
;;;     height of dirty rectangle
;;;
;;; Since 1.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_surface_set_device_offset ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_surface_set_device_offset" cairo-surface-set-device) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-1-23}
  @argument[surface]{a @symbol{cairo-surface-t}}
  @argument[x-offset]{the offset in the x direction, in device units}
  @argument[y-offset]{the offset in the y direction, in device units}
  @begin{short}
    Sets an offset that is added to the device coordinates determined by the
    CTM when drawing to surface.
  @end{short}
  One use case for this function is when we want to create a
  @symbol{cairo-surface-t} that redirects drawing for a portion of an onscreen
  surface to an offscreen surface in a way that is completely invisible to the
  user of the cairo API. Setting a transformation via the function
  @fun{cairo-translate} is not sufficient to do this, since functions like
  the function @fun{cairo-device-to-user} will expose the hidden offset.

  Note that the offset affects drawing to the surface as well as using the
  surface in a source pattern.

  Since 1.0
  @see-symbol{cairo-surface-t}
  @see-function{cario-translate}
  @see-function{cairo-device-to-user}"
  (surface (:pointer (:struct cairo-surface-t)))
  (x-offset :double)
  (y-offset :double))

(export 'cairo-surface-set-device-offset)

;;; ----------------------------------------------------------------------------
;;; cairo_surface_get_device_offset ()
;;;
;;; void cairo_surface_get_device_offset (cairo_surface_t *surface,
;;;                                       double *x_offset,
;;;                                       double *y_offset);
;;;
;;; This function returns the previous device offset set by
;;; cairo_surface_set_device_offset().
;;;
;;; surface :
;;;     a cairo_surface_t
;;;
;;; x_offset :
;;;     the offset in the X direction, in device units
;;;
;;; y_offset :
;;;     the offset in the Y direction, in device units
;;;
;;; Since 1.2
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_surface_set_fallback_resolution ()
;;;
;;; void cairo_surface_set_fallback_resolution (cairo_surface_t *surface,
;;;                                             double x_pixels_per_inch,
;;;                                             double y_pixels_per_inch);
;;;
;;; Set the horizontal and vertical resolution for image fallbacks.
;;;
;;; When certain operations aren't supported natively by a backend, cairo will
;;; fallback by rendering operations to an image and then overlaying that image
;;; onto the output. For backends that are natively vector-oriented, this
;;; function can be used to set the resolution used for these image fallbacks,
;;; (larger values will result in more detailed images, but also larger file
;;; sizes).
;;;
;;; Some examples of natively vector-oriented backends are the ps, pdf, and svg
;;; backends.
;;;
;;; For backends that are natively raster-oriented, image fallbacks are still
;;; possible, but they are always performed at the native device resolution. So
;;; this function has no effect on those backends.
;;;
;;; Note: The fallback resolution only takes effect at the time of completing a
;;; page (with cairo_show_page() or cairo_copy_page()) so there is currently no
;;; way to have more than one fallback resolution in effect on a single page.
;;;
;;; The default fallback resoultion is 300 pixels per inch in both dimensions.
;;;
;;; surface :
;;;     a cairo_surface_t
;;;
;;; x_pixels_per_inch :
;;;     horizontal setting for pixels per inch
;;;
;;; y_pixels_per_inch :
;;;     vertical setting for pixels per inch
;;;
;;; Since 1.2
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_surface_get_fallback_resolution ()
;;;
;;; void cairo_surface_get_fallback_resolution (cairo_surface_t *surface,
;;;                                             double *x_pixels_per_inch,
;;;                                             double *y_pixels_per_inch);
;;;
;;; This function returns the previous fallback resolution set by
;;; cairo_surface_set_fallback_resolution(), or default fallback resolution if
;;; never set.
;;;
;;; surface :
;;;     a cairo_surface_t
;;;
;;; x_pixels_per_inch :
;;;     horizontal pixels per inch
;;;
;;; y_pixels_per_inch :
;;;     vertical pixels per inch
;;;
;;; Since 1.8
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; enum cairo_surface_type_t
;;; ----------------------------------------------------------------------------

(defcenum cairo-surface-type-t
  :image
  :pdf
  :ps
  :xlib
  :xcb
  :glitz
  :quartz
  :win32
  :beos
  :directfb
  :svg
  :os2
  :win32-printing
  :quartz-image
  :script
  :qt
  :recording
  :vg
  :gl
  :drm
  :tee
  :xml
  :skia
  :subsurface
  :cogl)

#+cl-cffi-gtk-documentation
(setf (gethash 'cairo-surface-type-t atdoc:*symbol-name-alias*) "CEnum"
      (gethash 'cairo-surface-type-t atdoc:*external-symbols*)
 "@version{2013-10-20}
  @begin{short}
    @sym{cairo-surface-type-t} is used to describe the type of a given surface.
    The surface types are also known as \"backends\" or \"surface backends\"
    within cairo.
  @end{short}

  The type of a surface is determined by the function used to create it, which
  will generally be of the form @code{cairo-type-surface-create}, though see the
  function @fun{cairo-surface-create-similar} as well.

  The surface type can be queried with the function
  @fun{cairo-surface-get-type}.

  The various @symbol{cairo-surface-t} functions can be used with surfaces of
  any type, but some backends also provide type-specific functions that must
  only be called with a surface of the appropriate type. These functions have
  names that begin with @code{cairo-type-surface} such as the function
  @fun{cairo-image-surface-get-width}.

  The behavior of calling a type-specific function with a surface of the wrong
  type is undefined.

  New entries may be added in future versions.
  @begin{pre}
(defcenum cairo-surface-type-t
  :image
  :pdf
  :ps
  :xlib
  :xcb
  :glitz
  :quartz
  :win32
  :beos
  :directfb
  :svg
  :os2
  :win32-printing
  :quartz-image
  :script
  :qt
  :recording
  :vg
  :gl
  :drm
  :tee
  :xml
  :skia
  :subsurface
  :cogl)
  @end{pre}
  @begin[code]{table}
    @entry[:image]{The surface is of type image, since 1.2.}
    @entry[:pdf]{The surface is of type pdf, since 1.2.}
    @entry[:ps]{The surface is of type ps, since 1.2.}
    @entry[:xlib]{The surface is of type xlib, since 1.2.}
    @entry[:xcb]{The surface is of type xcb, since 1.2.}
    @entry[:glitz]{The surface is of type glitz, since 1.2.}
    @entry[:quartz]{The surface is of type quartz, since 1.2.}
    @entry[:win32]{The surface is of type win32, since 1.2.}
    @entry[:beos]{The surface is of type beos, since 1.2.}
    @entry[:directfb]{The surface is of type directfb, since 1.2.}
    @entry[:svg]{The surface is of type svg, since 1.2.}
    @entry[:os2]{The surface is of type os2, since 1.4.}
    @entry[:win32-printing]{The surface is a win32 printing surface, since 1.6.}
    @entry[:quartz-image]{The surface is of type quartz_image, since 1.6.}
    @entry[:script]{The surface is of type script, since 1.10.}
    @entry[:qt]{The surface is of type Qt, since 1.10.}
    @entry[:recording]{The surface is of type recording, since 1.10.}
    @entry[:vg]{The surface is a OpenVG surface, since 1.10.}
    @entry[:gl]{The surface is of type OpenGL, since 1.10.}
    @entry[:drm]{The surface is of type Direct Render Manager, since 1.10.}
    @entry[:tee]{The surface is of type 'tee' (a multiplexing surface),
      since 1.10.}
    @entry[:xml]{The surface is of type XML (for debugging), since 1.10.}
    @entry[:skia]{The surface is of type Skia, since 1.10.}
    @entry[:subsurface]{The surface is a subsurface created with the function
      @fun{cairo-surface-create-for-rectangle}, since 1.10.}
    @entry[:cogl]{This surface is of type Cogl, since 1.12.}
  @end{table}
  Since 1.2
  @see-symbol{cairo-surface-t}
  @see-function{cairo-surface-create-similar}
  @see-function{cairo-surface-get-type}
  @see-function{cairo-image-surface-get-width}
  @see-function{cairo-surface-create-for-rectangle}")

(export 'cairo-surface-type-t)

;;; ----------------------------------------------------------------------------
;;; cairo_surface_get_type ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_surface_get_type" cairo-surface-get-type) cairo-surface-type-t
 #+cl-cffi-gtk-documentation
 "@version{2013-10-20}
  @argument[surface]{a @symbol{cairo-surface-t}}
  @return{The type of surface.}
  @begin{short}
    This function returns the type of the backend used to create a surface. See
    the @symbol{cairo-surface-type-t} enumeration for available types.
  @end{short}

  Since 1.2
  @see-symbol{cairo-surface-t}
  @see-symbol{cairo-surface-type-t}"
  (surface (:pointer (:struct cairo-surface-t))))

(export 'cairo-surface-get-type)

;;; ----------------------------------------------------------------------------
;;; cairo_surface_get_reference_count ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_surface_get_reference_count" cairo-surface-get-reference-count)
    :uint
 #+cl-cffi-gtk-documentation
 "@version{2013-12-7}
  @argument[surface]{a @symbol{cairo-surface-t}}
  @begin{return}
    The current reference count of @arg{surface}. If the object is a nil object,
    0 will be returned.
  @end{return}
  @begin{short}
    Returns the current reference count of @arg{surface}.
  @end{short}

  Since 1.4
  @see-symbol{cairo-surface-t}"
  (surface (:pointer (:struct cairo-surface-t))))

(export 'cairo-surface-get-reference-count)

;;; ----------------------------------------------------------------------------
;;; cairo_surface_set_user_data ()
;;;
;;; cairo_status_t cairo_surface_set_user_data
;;;                                           (cairo_surface_t *surface,
;;;                                            const cairo_user_data_key_t *key,
;;;                                            void *user_data,
;;;                                            cairo_destroy_func_t destroy);
;;;
;;; Attach user data to surface. To remove user data from a surface, call this
;;; function with the key that was used to set it and NULL for data.
;;;
;;; surface :
;;;     a cairo_surface_t
;;;
;;; key :
;;;     the address of a cairo_user_data_key_t to attach the user data to
;;;
;;; user_data :
;;;     the user data to attach to the surface
;;;
;;; destroy :
;;;     a cairo_destroy_func_t which will be called when the surface is
;;;     destroyed or when new user data is attached using the same key.
;;;
;;; Returns :
;;;     CAIRO_STATUS_SUCCESS or CAIRO_STATUS_NO_MEMORY if a slot could not be
;;;     allocated for the user data.
;;;
;;; Since 1.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_surface_get_user_data ()
;;;
;;; void * cairo_surface_get_user_data (cairo_surface_t *surface,
;;;                                     const cairo_user_data_key_t *key);
;;;
;;; Return user data previously attached to surface using the specified key. If
;;; no user data has been attached with the given key this function returns
;;; NULL.
;;;
;;; surface :
;;;     a cairo_surface_t
;;;
;;; key :
;;;     the address of the cairo_user_data_key_t the user data was attached to
;;;
;;; Returns :
;;;     the user data previously attached or NULL.
;;;
;;; Since 1.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_surface_copy_page ()
;;;
;;; void cairo_surface_copy_page (cairo_surface_t *surface);
;;;
;;; Emits the current page for backends that support multiple pages, but doesn't
;;; clear it, so that the contents of the current page will be retained for the
;;; next page. Use cairo_surface_show_page() if you want to get an empty page
;;; after the emission.
;;;
;;; There is a convenience function for this that takes a cairo_t, namely
;;; cairo_copy_page().
;;;
;;; surface :
;;;     a cairo_surface_t
;;;
;;; Since 1.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_surface_show_page ()
;;;
;;; void cairo_surface_show_page (cairo_surface_t *surface);
;;;
;;; Emits and clears the current page for backends that support multiple pages.
;;; Use cairo_surface_copy_page() if you don't want to clear the page.
;;;
;;; There is a convenience function for this that takes a cairo_t, namely
;;; cairo_show_page().
;;;
;;; surface :
;;;     a cairo_Surface_t
;;;
;;; Since 1.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_surface_has_show_text_glyphs ()
;;;
;;; cairo_bool_t cairo_surface_has_show_text_glyphs (cairo_surface_t *surface);
;;;
;;; Returns whether the surface supports sophisticated cairo_show_text_glyphs()
;;; operations. That is, whether it actually uses the provided text and cluster
;;; data to a cairo_show_text_glyphs() call.
;;;
;;; Note: Even if this function returns FALSE, a cairo_show_text_glyphs()
;;; operation targeted at surface will still succeed. It just will act like a
;;; cairo_show_glyphs() operation. Users can use this function to avoid
;;; computing UTF-8 text and cluster mapping if the target surface does not use
;;; it.
;;;
;;; surface :
;;;     a cairo_surface_t
;;;
;;; Returns :
;;;     TRUE if surface supports cairo_show_text_glyphs(), FALSE otherwise
;;;
;;; Since 1.8
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_surface_set_mime_data ()
;;;
;;; cairo_status_t cairo_surface_set_mime_data (cairo_surface_t *surface,
;;;                                             const char *mime_type,
;;;                                             const unsigned char *data,
;;;                                             unsigned long  length,
;;;                                             cairo_destroy_func_t destroy,
;;;                                             void *closure);
;;;
;;; Attach an image in the format mime_type to surface. To remove the data from
;;; a surface, call this function with same mime type and NULL for data.
;;;
;;; The attached image (or filename) data can later be used by backends which
;;; support it (currently: PDF, PS, SVG and Win32 Printing surfaces) to emit
;;; this data instead of making a snapshot of the surface. This approach tends
;;; to be faster and requires less memory and disk space.
;;;
;;; The recognized MIME types are the following: CAIRO_MIME_TYPE_JPEG,
;;; CAIRO_MIME_TYPE_PNG, CAIRO_MIME_TYPE_JP2, CAIRO_MIME_TYPE_URI.
;;;
;;; See corresponding backend surface docs for details about which MIME types it
;;; can handle. Caution: the associated MIME data will be discarded if you draw
;;; on the surface afterwards. Use this function with care.
;;;
;;; surface :
;;;     a cairo_surface_t
;;;
;;; mime_type :
;;;     the MIME type of the image data
;;;
;;; data :
;;;     the image data to attach to the surface
;;;
;;; length :
;;;     the length of the image data
;;;
;;; destroy :
;;;     a cairo_destroy_func_t which will be called when the surface is
;;;     destroyed or when new image data is attached using the same mime type.
;;;
;;; closure :
;;;     the data to be passed to the destroy notifier
;;;
;;; Returns :
;;;     CAIRO_STATUS_SUCCESS or CAIRO_STATUS_NO_MEMORY if a slot could not be
;;;     allocated for the user data.
;;;
;;; Since 1.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_surface_get_mime_data ()
;;;
;;; void cairo_surface_get_mime_data (cairo_surface_t *surface,
;;;                                   const char *mime_type,
;;;                                   const unsigned char **data,
;;;                                   unsigned long *length);
;;;
;;; Return mime data previously attached to surface using the specified mime
;;; type. If no data has been attached with the given mime type, data is set
;;; NULL.
;;;
;;; surface :
;;;     a cairo_surface_t
;;;
;;; mime_type :
;;;     the mime type of the image data
;;;
;;; data :
;;;     the image data to attached to the surface
;;;
;;; length :
;;;     the length of the image data
;;;
;;; Since 1.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_surface_supports_mime_type ()
;;;
;;; cairo_bool_t cairo_surface_supports_mime_type (cairo_surface_t *surface,
;;;                                                const char *mime_type);
;;;
;;; Return whether surface supports mime_type.
;;;
;;; surface :
;;;     a cairo_surface_t
;;;
;;; mime_type :
;;;     the mime type
;;;
;;; Returns :
;;;     TRUE if surface supports mime_type, FALSE otherwise
;;;
;;; Since 1.12
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_surface_map_to_image ()
;;;
;;; cairo_surface_t * cairo_surface_map_to_image
;;;                                      (cairo_surface_t *surface,
;;;                                       const cairo_rectangle_int_t *extents);
;;;
;;; Returns an image surface that is the most efficient mechanism for modifying
;;; the backing store of the target surface. The region retrieved may be limited
;;; to the extents or NULL for the whole surface
;;;
;;; Note, the use of the original surface as a target or source whilst it is
;;; mapped is undefined. The result of mapping the surface multiple times is
;;; undefined. Calling cairo_surface_destroy() or cairo_surface_finish() on the
;;; resulting image surface results in undefined behavior.
;;;
;;; surface :
;;;     an existing surface used to extract the image from
;;;
;;; extents :
;;;     limit the extraction to an rectangular region
;;;
;;; Returns :
;;;     a pointer to the newly allocated image surface. The caller must use
;;;     cairo_surface_unmap_image() to destroy this image surface. This function
;;;     always returns a valid pointer, but it will return a pointer to a "nil"
;;;     surface if other is already in an error state or any other error occurs.
;;;
;;; Since 1.12
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_surface_unmap_image ()
;;;
;;; void cairo_surface_unmap_image (cairo_surface_t *surface,
;;;                                 cairo_surface_t *image);
;;;
;;; Unmaps the image surface as returned from #cairo_surface_map_to_image().
;;;
;;; The content of the image will be uploaded to the target surface. Afterwards,
;;; the image is destroyed.
;;;
;;; Using an image surface which wasn't returned by cairo_surface_map_to_image()
;;; results in undefined behavior.
;;;
;;; surface :
;;;     the surface passed to cairo_surface_map_to_image().
;;;
;;; image :
;;;     the currently mapped image
;;;
;;; Since 1.12
;;; ----------------------------------------------------------------------------

;;; --- End of file cairo.surface.lisp -----------------------------------------
