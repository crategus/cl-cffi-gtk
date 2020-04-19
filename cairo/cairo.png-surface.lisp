;;; ----------------------------------------------------------------------------
;;; cairo.png-surface.lisp
;;;
;;; The documentation of this file is taken from the Cairo Reference Manual
;;; Version 1.12.16 and modified to document the Lisp binding to the Cairo
;;; library. See <http://cairographics.org>. The API documentation of the Lisp
;;; binding is available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2013, 2014 Dieter Kaiser
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
;;; PNG Support
;;;
;;; Reading and writing PNG images
;;;
;;; Synopsis
;;;
;;;     CAIRO_HAS_PNG_FUNCTIONS
;;;
;;;     cairo_image_surface_create_from_png
;;;     cairo_image_surface_create_from_png_stream
;;;     cairo_surface_write_to_png
;;;     cairo_surface_write_to_png_stream
;;; ----------------------------------------------------------------------------

(in-package :cairo)

;;; ----------------------------------------------------------------------------
;;; CAIRO_HAS_PNG_FUNCTIONS
;;;
;;; #define CAIRO_HAS_PNG_FUNCTIONS 1
;;;
;;; Defined if the PNG functions are available. This macro can be used to
;;; conditionally compile code using the cairo PNG functions.
;;;
;;; Since 1.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_image_surface_create_from_png ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_image_surface_create_from_png"
           cairo-image-surface-create-from-png)
    (:pointer (:struct cairo-surface-t))
 #+cl-cffi-gtk-documentation
 "@version{2014-2-1}
  @argument[filename]{name of PNG file to load}
  @begin{return}
    A new @symbol{cairo-surface-t} structure initialized with the contents of
    the PNG file, or a \"nil\" surface if any error occurred.
  @end{return}
  @begin{short}
    Creates a new image surface and initializes the contents to the given PNG
    file.
  @end{short}

  A nil surface can be checked for with the function @fun{cairo-surface-status}
  which may return one of the following values: @code{:no-memory},
  @code{:file-not-found}, or @code{:read-error}. Alternatively, you can allow
  errors to propagate through the drawing operations and check the status on
  the context upon completion using the function @fun{cairo-status}.

  Since 1.0
  @see-symbol{cairo-surface-t}
  @see-function{cairo-status}
  @see-function{cairo-surface-status}"
  (filename :string))

(export 'cairo-image-surface-create-from-png)

;;; ----------------------------------------------------------------------------
;;; cairo_read_func_t ()
;;;
;;; cairo_status_t (*cairo_read_func_t) (void *closure,
;;;                                      unsigned char *data,
;;;                                      unsigned int length);
;;;
;;; cairo_read_func_t is the type of function which is called when a backend
;;; needs to read data from an input stream. It is passed the closure which was
;;; specified by the user at the time the read function was registered, the
;;; buffer to read the data into and the length of the data in bytes. The read
;;; function should return CAIRO_STATUS_SUCCESS if all the data was successfully
;;; read, CAIRO_STATUS_READ_ERROR otherwise.
;;;
;;; closure :
;;;     the input closure
;;;
;;; data :
;;;     the buffer into which to read the data
;;;
;;; length :
;;;     the amount of data to read
;;;
;;; Returns :
;;;     the status code of the read operation
;;;
;;; Since 1.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_image_surface_create_from_png_stream ()
;;;
;;; cairo_surface_t * cairo_image_surface_create_from_png_stream
;;;                                                (cairo_read_func_t read_func,
;;;                                                 void *closure);
;;;
;;; Creates a new image surface from PNG data read incrementally via the
;;; read_func function.
;;;
;;; read_func :
;;;     function called to read the data of the file
;;;
;;; closure :
;;;     data to pass to read_func.
;;;
;;; Returns :
;;;     a new cairo_surface_t initialized with the contents of the PNG file or a
;;;     "nil" surface if the data read is not a valid PNG image or memory could
;;;     not be allocated for the operation. A nil surface can be checked for
;;;     with cairo_surface_status(surface) which may return one of the following
;;;     values: CAIRO_STATUS_NO_MEMORY CAIRO_STATUS_READ_ERROR Alternatively,
;;;     you can allow errors to propagate through the drawing operations and
;;;     check the status on the context upon completion using cairo_status().
;;;
;;; Since 1.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_surface_write_to_png ()
;;;
;;; cairo_status_t cairo_surface_write_to_png (cairo_surface_t *surface,
;;;                                            const char *filename);
;;;
;;; Writes the contents of surface to a new file filename as a PNG image.
;;;
;;; surface :
;;;     a cairo_surface_t with pixel contents
;;;
;;; filename :
;;;     the name of a file to write to
;;;
;;; Returns :
;;;     CAIRO_STATUS_SUCCESS if the PNG file was written successfully.
;;;     Otherwise, CAIRO_STATUS_NO_MEMORY if memory could not be allocated for
;;;     the operation or CAIRO_STATUS_SURFACE_TYPE_MISMATCH if the surface does
;;;     not have pixel contents, or CAIRO_STATUS_WRITE_ERROR if an I/O error
;;;     occurs while attempting to write the file.
;;;
;;; Since 1.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_write_func_t ()
;;;
;;; cairo_status_t (*cairo_write_func_t) (void *closure,
;;;                                       const unsigned char *data,
;;;                                       unsigned int length);
;;;
;;; cairo_write_func_t is the type of function which is called when a backend
;;; needs to write data to an output stream. It is passed the closure which was
;;; specified by the user at the time the write function was registered, the
;;; data to write and the length of the data in bytes. The write function should
;;; return CAIRO_STATUS_SUCCESS if all the data was successfully written,
;;; CAIRO_STATUS_WRITE_ERROR otherwise.
;;;
;;; closure :
;;;     the output closure
;;;
;;; data :
;;;     the buffer containing the data to write
;;;
;;; length :
;;;     the amount of data to write
;;;
;;; Returns :
;;;     the status code of the write operation
;;;
;;; Since 1.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_surface_write_to_png_stream ()
;;;
;;; cairo_status_t cairo_surface_write_to_png_stream
;;;                                              (cairo_surface_t *surface,
;;;                                               cairo_write_func_t write_func,
;;;                                               void *closure);
;;;
;;; Writes the image surface to the write function.
;;;
;;; surface :
;;;     a cairo_surface_t with pixel contents
;;;
;;; write_func :
;;;     a cairo_write_func_t
;;;
;;; closure :
;;;     closure data for the write function
;;;
;;; Returns :
;;;     CAIRO_STATUS_SUCCESS if the PNG file was written successfully.
;;;     Otherwise, CAIRO_STATUS_NO_MEMORY is returned if memory could not be
;;;     allocated for the operation, CAIRO_STATUS_SURFACE_TYPE_MISMATCH if the
;;;     surface does not have pixel contents.
;;;
;;; Since 1.0
;;; ----------------------------------------------------------------------------

;;; --- End of file cairo.png-surface.lisp -------------------------------------
