;;; ----------------------------------------------------------------------------
;;; cairo.image-surface.lisp
;;;
;;; The documentation of the file is taken from the Cairo Reference Manual
;;; Version 1.16 and modified to document the Lisp binding to the Cairo
;;; library. See <http://cairographics.org>. The API documentation of the
;;; Lisp binding is available at <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2013 - 2020 Dieter Kaiser
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
;;;     Rendering to memory buffers
;;;
;;; Types and Values
;;;
;;;     CAIRO_HAS_IMAGE_SURFACE
;;;
;;;     cairo_format_t --> cairo.surface.lisp
;;;
;;; Functions
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
;;; allocated by cairo or by the calling code. The supported image formats
;;; are those defined in cairo_format_t.
;;; ----------------------------------------------------------------------------

(in-package :cairo)

;;; ----------------------------------------------------------------------------
;;; CAIRO_HAS_IMAGE_SURFACE
;;;
;;; #define CAIRO_HAS_IMAGE_SURFACE 1
;;;
;;; Defined if the image surface backend is available. The image surface backend
;;; is always built in. This macro was added for completeness in Cairo 1.8.
;;;
;;; Since 1.8
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_format_stride_for_width ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_format_stride_for_width" cairo-format-stride-for-width) :int
 #+cl-cffi-gtk-documentation
 "@version{2020-12-21}
  @argument[format]{a @symbol{cairo-format-t} value}
  @argument[width]{An integer with the desired width of an image surface to
    be created.}
  @begin{return}
    The appropriate stride to use given the desired format and width, or -1
    if either the format is invalid or the width too large.
  @end{return}
  @begin{short}
    This function provides a stride value that will respect all alignment
    requirements of the accelerated image-rendering code within Cairo.
  @end{short}
  @begin[Example]{dictionary}
   Typical usage will be of the form:
   @begin{pre}
(let* ((height 150)
       (width 200)
       (stride (cairo-format-stride-for-width :argb32 width))
       (data (g-malloc (* height stride)))
       (surface (cairo-image-surface-create-for-data data
                                                     :argb32
                                                     width height stride)))
  ... )
    @end{pre}
  @end{dictionary}
  @see-symbol{cairo-format-t}"
  (format cairo-format-t)
  (width :int))

(export 'cairo-format-stride-for-width)

;;; ----------------------------------------------------------------------------
;;; cairo_image_surface_create ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_image_surface_create" cairo-image-surface-create)
    (:pointer (:struct cairo-surface-t))
 #+cl-cffi-gtk-documentation
 "@version{2020-12-21}
  @argument[format]{format of pixels of type @symbol{cairo-format-t} in the
    surface to create}
  @argument[width]{an integer with the width of the surface, in pixels}
  @argument[height]{an integer with the height of the surface, in pixels}
  @begin{return}
    A pointer to the newly created @symbol{cairo-surface-t} structure. The
    caller owns the surface and should call the function
    @fun{cairo-surface-destroy} when done with it.
  @end{return}
  @begin{short}
    Creates an image surface of the specified format and dimensions.
  @end{short}
  Initially the surface contents are all 0. Specifically, within each pixel,
  each color or alpha channel belonging to the format will be 0. The contents
  of bits within a pixel, but not belonging to the given format are undefined.

  This function always returns a valid pointer, but it will return a pointer
  to a \"nil\" surface if an error such as out of memory occurs. You can use
  the function @fun{cairo-surface-status} to check for this.
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
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_image_surface_create_for_data"
           cairo-image-surface-create-for-data)
    (:pointer (:struct cairo-surface-t))
 #+cl-cffi-gtk-documentation
 "@version{2020-12-21}
  @argument[data]{a pointer to a buffer supplied by the application in which to
    write contents, this pointer must be suitably aligned for any kind of
    variable, for example, a pointer returned by malloc}
  @argument[format]{a value of the @symbol{cairo-format-t} enumeration for the
    format of pixels in the buffer}
  @argument[width]{an integer with the width of the image to be stored in the
    buffer}
  @argument[height]{an integer with the height of the image to be stored in the
    buffer}
  @argument[stride]{an integer with the number of bytes between the start of
    rows in the buffer as allocated, this value should always be computed by
    the function @fun{cairo-format-stride-for-width} before allocating the data
    buffer}
  @begin{return}
    A pointer to the newly created surface. The caller owns the surface and
    should call the function @fun{cairo-surface-destroy} when done with it.
    This function always returns a valid pointer, but it will return a pointer
    to a \"nil\" surface in the case of an error such as out of memory or an
    invalid stride value. In case of invalid stride value the error status of
    the returned surface will be @code{:invalid-stride}. You can use the
    function @fun{cairo-surface-status} to check for this. See the function
    @fun{cairo-surface-set-user-data} for a means of attaching a
    destroy-notification fallback to the surface if necessary.
  @end{return}
  @begin{short}
    Creates an image surface for the provided pixel data.
  @end{short}
  The output buffer must be kept around until the Cairo surface is destroyed or
  the function @fun{cairo-surface-finish} is called on the surface. The initial
  contents of @arg{data} will be used as the initial image contents; you must
  explicitly clear the buffer, using, for example, the functions
  @fun{cairo-rectangle} and @fun{cairo-fill} if you want it cleared.

  Note that the stride may be larger than width*bytes per pixel to provide
  proper alignment for each pixel and row. This alignment is required to
  allow high-performance rendering within Cairo. The correct way to obtain a
  legal stride value is to call the function @fun{cairo-format-stride-for-width}
  with the desired format and maximum image width value, and then use the
  resulting stride value to allocate the data and to create the image surface.
  See the function @fun{cairo-format-stride-for-width} for example code.
  @see-symbol{cairo-surface-t}
  @see-function{cairo-format-stride-for-width}
  @see-function{cairo-surface-destroy}
  @see-function{cairo-surface-status}
  @see-function{cairo-surface-set-user-data}
  @see-function{cairo-surface-finish}
  @see-function{cairo-rectangle}
  @see-function{cairo-fill}"
  (data :pointer)
  (format cairo-format-t)
  (width :int)
  (height :int)
  (stride :int))

(export 'cairo-image-surface-create-for-data)

;;; ----------------------------------------------------------------------------
;;; cairo_image_surface_get_data () -> cairo-image-surface-data
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_image_surface_get_data" cairo-image-surface-data) :pointer
 #+cl-cffi-gtk-documentation
 "@version{2020-12-21}
  @argument[surface]{a @symbol{cairo-surface-t} instance}
  @begin{return}
    A pointer to the image data of this surface or NULL if @arg{surface} is not
    an image surface, or if the function @fun{cairo-surface-finish} has been
    called.
  @end{return}
  @begin{short}
    Get a pointer to the data of the image surface, for direct inspection or
    modification.
  @end{short}

  A call to the function @fun{cairo-surface-flush} is required before accessing
  the pixel data to ensure that all pending drawing operations are finished. A
  call to the function @fun{cairo-surface-mark-dirty} is required after the
  data is modified.
  @see-symbol{cairo-surface-t}
  @see-function{cairo-surface-finish}
  @see-function{cairo-surface-flush}
  @see-function{cairo-surface-mark-dirty}"
  (surface (:pointer (:struct cairo-surface-t))))

(export 'cairo-image-surface-data)

;;; ----------------------------------------------------------------------------
;;; cairo_image_surface_get_format () -> cairo-image-surface-format
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_image_surface_get_format" cairo-image-surface-format)
    cairo-format-t
 #+cl-cffi-gtk-documentation
 "@version{2020-12-21}
  @argument[surface]{a @symbol{cairo-surface-t} instance}
  @begin{return}
    A value of the @symbol{cairo-format-t} enumeration with the format of the
    surface.
  @end{return}
  @begin{short}
    Get the format of the image surface.
  @end{short}
  @see-symbol{cairo-surface-t}
  @see-symbol{cairo-format-t}"
  (surface (:pointer (:struct cairo-surface-t))))

(export 'cairo-image-surface-format)

;;; ----------------------------------------------------------------------------
;;; cairo_image_surface_get_width () cairo-image-surface-width
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_image_surface_get_width" cairo-image-surface-width) :int
 #+cl-cffi-gtk-documentation
 "@version{2020-12-21}
  @argument[surface]{a @symbol{cairo-surface-t} instance}
  @return{An integer with the width of the surface in pixels.}
  @begin{short}
    Gets the width of the image surface in pixels.
  @end{short}
  @see-symbol{cairo-surface-t}
  @see-function{cairo-image-surface-height}"
  (surface (:pointer (:struct cairo-surface-t))))

(export 'cairo-image-surface-width)

;;; ----------------------------------------------------------------------------
;;; cairo_image_surface_get_height () cairo-image-surface-height
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_image_surface_get_height" cairo-image-surface-height) :int
 #+cl-cffi-gtk-documentation
 "@version{2020-12-21}
  @argument[surface]{a @symbol{cairo-surface-t} instance}
  @return{An integer with the height of the surface in pixels.}
  @begin{short}
    Gets the height of the image surface in pixels.
  @end{short}
  @see-symbol{cairo-surface-t}
  @see-function{cairo-image-surface-width}"
  (surface (:pointer (:struct cairo-surface-t))))

(export 'cairo-image-surface-height)

;;; ----------------------------------------------------------------------------
;;; cairo_image_surface_get_stride () -> cairo-image-surface-stride
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_image_surface_get_stride" cairo-image-surface-stride) :int
 #+cl-cffi-gtk-documentation
 "@version{2020-12-21}
  @argument[surface]{a @symbol{cairo-surface-t} instance}
  @begin{return}
    The stride of the image surface in bytes, or 0 if @arg{surface} is not an
    image surface. The stride is the distance in bytes from the beginning of
    one row of the image data to the beginning of the next row.
  @end{return}
  @begin{short}
    Get the stride of the image surface in bytes.
  @end{short}
  @see-symbol{cairo-surface-t}
  @see-function{cairo-format-stride-for-width}"
  (surface (:pointer (:struct cairo-surface-t))))

(export 'cairo-image-surface-stride)

;;; --- End of file cairo.image-surface.lisp -----------------------------------
