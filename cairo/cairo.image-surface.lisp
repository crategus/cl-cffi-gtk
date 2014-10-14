;;; ----------------------------------------------------------------------------
;;; cairo.image-surface.lisp
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
;;; Image Surfaces
;;;
;;; Rendering to memory buffers
;;;
;;; Synopsis
;;;
;;;     CAIRO_HAS_IMAGE_SURFACE
;;;
;;;     cairo_format_t --> cairo.surface.lisp
;;;
;;;     cairo_format_stride_for_width
;;;     cairo_image_surface_create
;;;     cairo_image_surface_create_for_data
;;;     cairo_image_surface_get_data
;;;     cairo_image_surface_get_format
;;;     cairo_image_surface_get_width
;;;     cairo_image_surface_get_height
;;;     cairo_image_surface_get_stride
;;;
;;; Description
;;;
;;; Image surfaces provide the ability to render to memory buffers either
;;; allocated by cairo or by the calling code. The supported image formats are
;;; those defined in cairo_format_t.
;;; ----------------------------------------------------------------------------

(in-package :cairo)

;;; ----------------------------------------------------------------------------
;;; CAIRO_HAS_IMAGE_SURFACE
;;;
;;; #define CAIRO_HAS_IMAGE_SURFACE 1
;;;
;;; Defined if the image surface backend is available. The image surface backend
;;; is always built in. This macro was added for completeness in cairo 1.8.
;;;
;;; Since 1.8
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_format_stride_for_width ()
;;;

(defcfun ("cairo_format_stride_for_width" cairo-format-stride-for-width)
    :int
  (format cairo-format-t)
  (width :int))

(export 'cairo-format-stride-for-width)

;;; int cairo_format_stride_for_width (cairo_format_t format, int width);
;;;
;;; This function provides a stride value that will respect all alignment
;;; requirements of the accelerated image-rendering code within cairo. Typical
;;; usage will be of the form:
;;;
;;; int stride;
;;; unsigned char *data;
;;; cairo_surface_t *surface;
;;;
;;; stride = cairo_format_stride_for_width (format, width);
;;; data = malloc (stride * height);
;;; surface = cairo_image_surface_create_for_data (data, format,
;;;                       width, height,
;;;                       stride);
;;;
;;; format :
;;;     A cairo_format_t value
;;;
;;; width :
;;;     The desired width of an image surface to be created.
;;;
;;; Returns :
;;;     the appropriate stride to use given the desired format and width, or -1
;;;     if either the format is invalid or the width too large.
;;;
;;; Since 1.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_image_surface_create ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_image_surface_create" cairo-image-surface-create)
    (:pointer (:struct cairo-surface-t))
 #+cl-cffi-gtk-documentation
 "@version{2014-2-2}
  @argument[format]{format of pixels of type @symbol{cairo-format-t} in the
    surface to create}
  @argument[width]{width of the surface, in pixels}
  @argument[height]{height of the surface, in pixels}
  @begin{return}
    A pointer to the newly created surface. The caller owns the surface and
    should call the function @fun{cairo-surface-destroy} when done with it.
  @end{return}
  @begin{short}
    Creates an image surface of the specified format and dimensions.
  @end{short}
  Initially the surface contents are all 0. Specifically, within each pixel,
  each color or alpha channel belonging to format will be 0. The contents of
  bits within a pixel, but not belonging to the given format are undefined.

  This function always returns a valid pointer, but it will return a pointer to
  a \"nil\" surface if an error such as out of memory occurs. You can use the
  function @fun{cairo-surface-status} to check for this.

  Since 1.0
  @see-symbol{cairo-surface-t}
  @see-symbol{cairo-format-t}
  @see-function{cairo-surface-destroy}
  @see-function{cairo-surface-status}"
  (format cairo-format-t)
  (width :int)
  (height :int))

(export 'cairo-image-surface-create)

;;; ----------------------------------------------------------------------------
;;; cairo_image_surface_create_for_data ()
;;;

(defcfun ("cairo_image_surface_create_for_data" cairo-image-surface-create-for-data)
    (:pointer (:struct cairo-surface-t))
  (data :pointer)
  (format cairo-format-t)
  (width :int)
  (height :int)
  (stride :int))

(export 'cairo-image-surface-create-for-data)


;;; cairo_surface_t * cairo_image_surface_create_for_data (unsigned char *data,
;;;                                                       cairo_format_t format,
;;;                                                        int width,
;;;                                                        int height,
;;;                                                        int stride);
;;;
;;; Creates an image surface for the provided pixel data. The output buffer must
;;; be kept around until the cairo_surface_t is destroyed or
;;; cairo_surface_finish() is called on the surface. The initial contents of
;;; data will be used as the initial image contents; you must explicitly clear
;;; the buffer, using, for example, cairo_rectangle() and cairo_fill() if you
;;; want it cleared.
;;;
;;; Note that the stride may be larger than width*bytes_per_pixel to provide
;;; proper alignment for each pixel and row. This alignment is required to allow
;;; high-performance rendering within cairo. The correct way to obtain a legal
;;; stride value is to call cairo_format_stride_for_width() with the desired
;;; format and maximum image width value, and then use the resulting stride
;;; value to allocate the data and to create the image surface. See
;;; cairo_format_stride_for_width() for example code.
;;;
;;; data :
;;;     a pointer to a buffer supplied by the application in which to write
;;;     contents. This pointer must be suitably aligned for any kind of
;;;     variable, (for example, a pointer returned by malloc).
;;;
;;; format :
;;;     the format of pixels in the buffer
;;;
;;; width :
;;;     the width of the image to be stored in the buffer
;;;
;;; height :
;;;     the height of the image to be stored in the buffer
;;;
;;; stride :
;;;     the number of bytes between the start of rows in the buffer as
;;;     allocated. This value should always be computed by
;;;     cairo_format_stride_for_width() before allocating the data buffer.
;;;
;;; Returns :
;;;     a pointer to the newly created surface. The caller owns the surface and
;;;     should call cairo_surface_destroy() when done with it. This function
;;;     always returns a valid pointer, but it will return a pointer to a "nil"
;;;     surface in the case of an error such as out of memory or an invalid
;;;     stride value. In case of invalid stride value the error status of the
;;;     returned surface will be CAIRO_STATUS_INVALID_STRIDE. You can use
;;;     cairo_surface_status() to check for this. See
;;;     cairo_surface_set_user_data() for a means of attaching a
;;;    destroy-notification fallback to the surface if necessary.
;;;
;;; Since 1.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_image_surface_get_data ()
;;;
;;; unsigned char * cairo_image_surface_get_data (cairo_surface_t *surface);
;;;
;;; Get a pointer to the data of the image surface, for direct inspection or
;;; modification.
;;;
;;; A call to cairo_surface_flush() is required before accessing the pixel data
;;; to ensure that all pending drawing operations are finished. A call to
;;; cairo_surface_mark_dirty() is required after the data is modified.
;;;
;;; surface :
;;;     a cairo_image_surface_t
;;;
;;; Returns :
;;;     a pointer to the image data of this surface or NULL if surface is not an
;;;     image surface, or if cairo_surface_finish() has been called.
;;;
;;; Since 1.2
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_image_surface_get_format ()
;;;
;;; cairo_format_t cairo_image_surface_get_format (cairo_surface_t *surface);
;;;
;;; Get the format of the surface.
;;;
;;; surface :
;;;     a cairo_image_surface_t
;;;
;;; Returns :
;;;     the format of the surface
;;;
;;; Since 1.2
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_image_surface_get_width ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_image_surface_get_width" cairo-image-surface-get-width) :int
 #+cl-cffi-gtk-documentation
 "@version{2014-2-7}
  @argument[surface]{a @symbol{cairo-surface-t}}
  @return{The width of the surface in pixels.}
  @begin{short}
    Get the width of the image surface in pixels.
  @end{short}

  Since 1.0
  @see-symbol{cairo-surface-t}
  @see-function{cairo-image-surface-get-height}"
  (surface (:pointer (:struct cairo-surface-t))))

(export 'cairo-image-surface-get-width)

;;; ----------------------------------------------------------------------------
;;; cairo_image_surface_get_height ()
;;;
;;; int cairo_image_surface_get_height (cairo_surface_t *surface);
;;;
;;; Get the height of the image surface in pixels.
;;;
;;; surface :
;;;     a cairo_image_surface_t
;;;
;;; Returns :
;;;     the height of the surface in pixels.
;;;
;;; Since 1.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_image_surface_get_stride ()
;;;
;;; int cairo_image_surface_get_stride (cairo_surface_t *surface);
;;;
;;; Get the stride of the image surface in bytes
;;;
;;; surface :
;;;     a cairo_image_surface_t
;;;
;;; Returns :
;;;     the stride of the image surface in bytes (or 0 if surface is not an
;;;     image surface). The stride is the distance in bytes from the beginning
;;;     of one row of the image data to the beginning of the next row.
;;;
;;; Since 1.2
;;; ----------------------------------------------------------------------------

;;; --- End of file cairo.image-surface.lisp -----------------------------------
