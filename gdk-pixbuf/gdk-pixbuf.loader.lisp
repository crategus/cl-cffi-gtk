;;; ----------------------------------------------------------------------------
;;; gdk-pixbuf.loader.lisp
;;;
;;; The documentation of this file is taken from the GDK-PixBuf Reference Manual
;;; Version 2.36 and modified to document the Lisp binding to the GDK-PixBuf
;;; library. See <http://www.gtk.org>. The API documentation of the Lisp
;;; binding is available from <http://www.crategus.com/books/cl-cffi-gtk/>.
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
;;; GdkPixbufLoader
;;;
;;;     Application-driven progressive image loading.
;;;
;;; Types and Values
;;;
;;;     GdkPixbufLoader
;;;
;;; Functions
;;;
;;;     gdk_pixbuf_loader_new
;;;     gdk_pixbuf_loader_new_with_type
;;;     gdk_pixbuf_loader_new_with_mime_type
;;;     gdk_pixbuf_loader_get_format
;;;     gdk_pixbuf_loader_write
;;;     gdk_pixbuf_loader_write_bytes
;;;     gdk_pixbuf_loader_set_size
;;;     gdk_pixbuf_loader_get_pixbuf
;;;     gdk_pixbuf_loader_get_animation
;;;     gdk_pixbuf_loader_close
;;;
;;; Signals
;;;
;;;     void    area-prepared    Run Last
;;;     void    area-updated     Run Last
;;;     void    closed           Run Last
;;;     void    size-prepared    Run Last
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GdkPixbufLoader
;;;
;;; ----------------------------------------------------------------------------

(in-package :gdk-pixbuf)

;;; ----------------------------------------------------------------------------
;;; struct GdkPixbufLoader
;;; ----------------------------------------------------------------------------

(define-g-object-class "GdkPixbufLoader" gdk-pixbuf-loader
  (:superclass g-object
   :export t
   :interfaces nil
   :type-initializer "gdk_pixbuf_loader_get_type")
  nil)

#+cl-cffi-gtk-documentation
(setf (documentation 'gdk-pixbuf-loader 'type)
 "@version{2020-11-22}
  @begin{short}
    The @sym{gdk-pixbuf-loader} class provides a way for applications to drive
    the process of loading an image, by letting them send the image data
    directly to the loader instead of having the loader read the data from a
    file.
  @end{short}
  Applications can use this functionality instead of the functions
  @fun{gdk-pixbuf-new-from-file} or @fun{gdk-pixbuf-animation-new-from-file}
  when they need to parse image data in small chunks. For example, it should
  be used when reading an image from a (potentially) slow network connection,
  or when loading an extremely large file.

  To use the @sym{gdk-pixbuf-loader} class to load an image, just create a new
  one, and call the function @fun{gdk-pixbuf-loader-write} to send the data to
  it. When done, the function @fun{gdk-pixbuf-loader-close} should be called to
  end the stream and finalize everything. The loader will emit three important
  signals throughout the process. The first, \"size-prepared\", will be called
  as soon as the image has enough information to determine the size of the image
  to be used. If you want to scale the image while loading it, you can call the
  function @fun{gdk-pixbuf-loader-set-size} in response to this signal.

  The second signal, \"area-prepared\", will be called as soon as the pixbuf of
  the desired has been allocated. You can obtain it by calling the function
  @fun{gdk-pixbuf-loader-pixbuf}. In addition, no actual information will be
  passed in yet, so the pixbuf can be safely filled with any temporary graphics
  (or an initial color) as needed. You can also call the function
  @fun{gdk-pixbuf-loader-pixbuf} later and get the same pixbuf.

  The last signal, \"area-updated\" gets called every time a region is updated.
  This way you can update a partially completed image. Note that you do not
  know anything about the completeness of an image from the area updated. For
  example, in an interlaced image, you need to make several passes before the
  image is done loading.

  @subheading{Loading an animation}
    Loading an animation is almost as easy as loading an image. Once the first
    \"area-prepared\" signal has been emitted, you can call the function
    @fun{gdk-pixbuf-loader-get-animation} to get the
    @class{gdk-pixbuf-animation} object and the function
    @fun{gdk-pixbuf-animation-get-iter} to get an
    @class{gdk-pixbuf-animation-iter} structure for displaying it.
  @begin[Signal Details]{dictionary}
    @subheading{The \"area-prepared\" signal}
      @begin{pre}
 lambda (loader)    :run-last
      @end{pre}
      The signal is emitted when the pixbuf loader has allocated the pixbuf
      in the desired size. After this signal is emitted, applications can call
      the @fun{gdk-pixbuf-loader-pixbuf} function to fetch the partially-loaded
      pixbuf.
      @begin[code]{table}
        @entry[loader]{The @sym{gdk-pixbuf-loader} object which received
          the signal.}
      @end{table}
    @subheading{The \"area-updated\" signal}
      @begin{pre}
  lambda (loader x y width height)    :run-last
      @end{pre}
      The signal is emitted when a significant area of the image being loaded
      has been updated. Normally it means that a complete scanline has been read
      in, but it could be a different area as well. Applications can use this
      signal to know when to repaint areas of an image that is being loaded.
      @begin[code]{table}
        @entry[loader]{The @sym{gdk-pixbuf-loader} object which received the
          signal.}
        @entry[x]{An integer with the x offset of upper-left corner of the
          updated area.}
        @entry[y]{An integer with the y offset of upper-left corner of the
          updated area.}
        @entry[width]{An integer with the width of updated area.}
        @entry[height]{An integer with the height of updated area.}
      @end{table}
    @subheading{The \"closed\" signal}
      @begin{pre}
   lambda (loader)    :run-last
      @end{pre}
      The signal is emitted when the @fun{gdk-pixbuf-loader-close} function is
      called. It can be used by different parts of an application to receive
      notification when an image loader is closed by the code that drives it.
      @begin[code]{table}
        @entry[loader]{The @sym{gdk-pixbuf-loader} object which received the
          signal.}
      @end{table}
    @subheading{The \"size-prepared\" signal}
      @begin{pre}
 lambda (loader width height)    :run-last
      @end{pre}
      The signal is emitted when the pixbuf loader has been fed the initial
      amount of data that is required to figure out the size of the image that
      it will create. Applications can call the @fun{gdk-pixbuf-loader-set-size}
      function in response to this signal to set the desired size to which the
      image should be scaled.
      @begin[code]{table}
        @entry[loader]{The @sym{gdk-pixbuf-loader} object which received the
          signal.}
        @entry[width]{An integer with the original width of the image.}
        @entry[height]{An integer with the original height of the image.}
      @end{table}
  @end{dictionary}
  @see-class{gdk-pixbuf-animation}
  @see-class{gdk-pixbuf-animation-iter}
  @see-function{gdk-pixbuf-new-from-file}
  @see-function{gdk-pixbuf-animation-new-from-file}
  @see-function{gdk-pixbuf-loader-write}
  @see-function{gdk-pixbuf-loader-close}
  @see-function{gdk-pixbuf-loader-set-size}
  @see-function{gdk-pixbuf-loader-pixbuf}
  @see-function{gdk-pixbuf-loader-get-animation}
  @see-function{gdk-pixbuf-animation-get-iter}")

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_loader_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gdk-pixbuf-loader-new))

(defun gdk-pixbuf-loader-new ()
 #+cl-cffi-gtk-documentation
 "@version{*2021-12-22}
  @return{A newly-created @class{gdk-pixbuf-loader} object.}
  @short{Creates a new pixbuf loader object.}
  @see-class{gdk-pixbuf-loader}"
  (make-instance 'gdk-pixbuf-loader))

(export 'gdk-pixbuf-loader-new)

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_loader_new_with_type ()
;;;
;;; GdkPixbufLoader * gdk_pixbuf_loader_new_with_type (const char *image_type,
;;;                                                    GError **error);
;;;
;;; Creates a new pixbuf loader object that always attempts to parse image data
;;; as if it were an image of type image_type, instead of identifying the type
;;; automatically. Useful if you want an error if the image isn't the expected
;;; type, for loading image formats that can't be reliably identified by looking
;;; at the data, or if the user manually forces a specific type.
;;;
;;; The list of supported image formats depends on what image loaders are
;;; installed, but typically "png", "jpeg", "gif", "tiff" and "xpm" are among
;;; the supported formats. To obtain the full list of supported image formats,
;;; call gdk_pixbuf_format_get_name() on each of the GdkPixbufFormat structs
;;; returned by gdk_pixbuf_get_formats().
;;;
;;; image_type :
;;;     name of the image format to be loaded with the image
;;;
;;; error :
;;;     return location for an allocated GError, or NULL to ignore errors
;;;
;;; Returns :
;;;     A newly-created pixbuf loader.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_loader_new_with_mime_type ()
;;;
;;; GdkPixbufLoader * gdk_pixbuf_loader_new_with_mime_type
;;;                                                      (const char *mime_type,
;;;                                                       GError **error);
;;;
;;; Creates a new pixbuf loader object that always attempts to parse image data
;;; as if it were an image of mime type mime_type, instead of identifying the
;;; type automatically. Useful if you want an error if the image isn't the
;;; expected mime type, for loading image formats that can't be reliably
;;; identified by looking at the data, or if the user manually forces a specific
;;; mime type.
;;;
;;; The list of supported mime types depends on what image loaders are
;;; installed, but typically "image/png", "image/jpeg", "image/gif",
;;; "image/tiff" and "image/x-xpixmap" are among the supported mime types. To
;;; obtain the full list of supported mime types, call
;;; gdk_pixbuf_format_get_mime_types() on each of the GdkPixbufFormat structs
;;; returned by gdk_pixbuf_get_formats().
;;;
;;; mime_type :
;;;     the mime type to be loaded
;;;
;;; error :
;;;     return location for an allocated GError, or NULL to ignore errors
;;;
;;; Returns :
;;;     A newly-created pixbuf loader.
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_loader_get_format ()
;;;
;;; GdkPixbufFormat * gdk_pixbuf_loader_get_format (GdkPixbufLoader *loader);
;;;
;;; Obtains the available information about the format of the currently loading
;;; image file.
;;;
;;; loader :
;;;     A pixbuf loader.
;;;
;;; Returns :
;;;     A GdkPixbufFormat or NULL. The return value is owned by GdkPixbuf and
;;;     should not be freed.
;;;
;;; Since 2.2
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_loader_write ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_pixbuf_loader_write" %gdk-pixbuf-loader-write) :boolean
  (loader (g-object gdk-pixbuf-loader))
  (buf (:pointer :uchar))
  (count g-size)
  (error :pointer))

(defun gdk-pixbuf-loader-write (loader buffer count)
 #+cl-cffi-gtk-documentation
 "@version{*2021-12-22}
  @argument[loader]{a @class{gdk-pixbuf-loader} object}
  @argument[buffer]{a Lisp array for image data}
  @argument[count]{an integer with the length of @arg{buffer} in bytes}
  @begin{return}
    @em{True} if the write was successful, or @em{false} if the loader cannot
    parse the buffer.
  @end{return}
  @begin{short}
    This will cause a pixbuf loader to parse the next count bytes of an image.
  @end{short}
  It will return @em{true} if the data was loaded successfully, and @em{false}
  if an error occurred.
  @begin[Example]{dictionary}
    A code fragment, which writes data into the pixbuf loader:
    @begin{pre}
;; Create the image stream and the GdkPixbufLoader
(setf stream
      (open (sys-path \"alphatest.png\")
            :element-type '(unsigned-byte 8)))
(setf loader (gdk-pixbuf-loader-new))

...

(let* ((buffer (make-array 128 :element-type '(unsigned-byte 8)))
       (len (read-sequence buffer stream)))
  ...
  ;; Load the buffer into GdkPixbufLoader
  (gdk-pixbuf-loader-write loader buffer 128)
  ... )
    @end{pre}
  @end{dictionary}
  @see-class{gdk-pixbuf-loader}"
  (with-g-error (err)
    (let ((buf (foreign-alloc :uchar :initial-contents buffer)))
      (%gdk-pixbuf-loader-write loader buf count err))))

(export 'gdk-pixbuf-loader-write)

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_loader_write_bytes ()
;;;
;;; gboolean
;;; gdk_pixbuf_loader_write_bytes (GdkPixbufLoader *loader,
;;;                                GBytes *buffer,
;;;                                GError **error);
;;;
;;; This will cause a pixbuf loader to parse a buffer inside a GBytes for an
;;; image. It will return TRUE if the data was loaded successfully, and FALSE if
;;; an error occurred. In the latter case, the loader will be closed, and will
;;; not accept further writes. If FALSE is returned, error will be set to an
;;; error from the GDK_PIXBUF_ERROR or G_FILE_ERROR domains.
;;;
;;; See also: gdk_pixbuf_loader_write()
;;;
;;; loader :
;;;     A pixbuf loader.
;;;
;;; buffer :
;;;     The image data as a GBytes
;;;
;;; error :
;;;     return location for errors
;;;
;;; Returns :
;;;     TRUE if the write was successful, or FALSE if the loader cannot parse
;;;     the buffer.
;;;
;;; Since 2.30
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_loader_set_size ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_pixbuf_loader_set_size" gdk-pixbuf-loader-set-size) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-11-22}
  @argument[loader]{a @class{gdk-pixbuf-loader} object}
  @argument[width]{an integer with the desired width of the image being loaded}
  @argument[height]{an integer with the desired height of the image being
    loaded}
  @begin{short}
    Causes the image to be scaled while it is loaded.
  @end{short}
  The desired image size can be determined relative to the original size of the
  image by calling the function @sym{gdk-pixbuf-loader-set-size} from a signal
  handler for the \"size-prepared\" signal.

  Attempts to set the desired image size are ignored after the emission of the
  \"size-prepared\" signal.
  @see-class{gdk-pixbuf-loader}"
  (loader (g-object gdk-pixbuf-loader))
  (width :int)
  (height :int))

(export 'gdk-pixbuf-loader-set-size)

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_loader_get_pixbuf () -> gdk-pixbuf-loader-pixbuf
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_pixbuf_loader_get_pixbuf" gdk-pixbuf-loader-pixbuf)
    (g-object gdk-pixbuf)
 #+cl-cffi-gtk-documentation
 "@version{*2021-12-22}
  @argument[loader]{a @class{gdk-pixbuf-loader} object}
  @begin{return}
    The @class{gdk-pixbuf} object that the loader is creating, or @code{nil} if
    not enough data has been read to determine how to create the image buffer.
  @end{return}
  @begin{short}
    Queries the @class{gdk-pixbuf} object that a pixbuf loader is currently
    creating.
  @end{short}
  In general it only makes sense to call this function after the
  \"area-prepared\" signal has been emitted by the loader. This means that
  enough data has been read to know the size of the image that will be
  allocated. If the loader has not received enough data via the
  @fun{gdk-pixbuf-loader-write} function, then this function returns @code{nil}.
  The returned pixbuf will be the same in all future calls to the loader.
  Additionally, if the loader is an animation, it will return the
  \"static image\" of the animation, see the
  @fun{gdk-pixbuf-animation-static-image} function.
  @see-class{gdk-pixbuf-loader}
  @see-class{gdk-pixbuf}
  @see-function{gdk-pixbuf-loader-write}
  @see-function{gdk-pixbuf-animation-static-image}"
  (loader (g-object gdk-pixbuf-loader)))

(export 'gdk-pixbuf-loader-pixbuf)

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_loader_get_animation () -> gdk-pixbuf-loader-animation
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_pixbuf_loader_get_animation" gdk-pixbuf-loader-animation)
    (g-object gdk-pixbuf-animation)
 #+cl-cffi-gtk-documentation
 "@version{2020-11-22}
  @argument[loader]{a @class{gdk-pixbuf-loader} object}
  @begin{return}
    The @class{gdk-pixbuf-animation} object that the loader is loading, or
    @code{nil} if not enough data has been read to determine the information.
  @end{return}
  @begin{short}
    Queries the @class{gdk-pixbuf-animation} object that a pixbuf loader is
    currently creating.
  @end{short}
  In general it only makes sense to call this function after the
  \"area-prepared\" signal has been emitted by the loader. If the loader does
  not have enough bytes yet, has not emitted the \"area-prepared\" signal, this
  function will return @code{nil}.
  @see-class{gdk-pixbuf-loader}
  @see-class{gdk-pixbuf-animation}"
  (load (g-object gdk-pixbuf-loader)))

(export 'gdk-pixbuf-loader-animation)

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_loader_close ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_pixbuf_loader_close" %gdk-pixbuf-loader-close) :boolean
  (loader (g-object gdk-pixbuf-loader))
  (err :pointer))

(defun gdk-pixbuf-loader-close (loader)
 #+cl-cffi-gtk-documentation
 "@version{*2021-12-22}
  @argument[loader]{a @class{gdk-pixbuf-loader} object}
  @begin{return}
    @em{True} if all image data written so far was successfully passed out via
    the \"update-area\" signal.
  @end{return}
  @begin{short}
    Informs a pixbuf loader that no further writes with the
    @fun{gdk-pixbuf-loader-write} function will occur, so that it can free its
    internal loading structures.
  @end{short}
  Also, tries to parse any data that has not yet been parsed.
  @see-class{gdk-pixbuf-loader}
  @see-function{gdk-pixbuf-loader-write}"
  (with-g-error (err)
    (%gdk-pixbuf-loader-close loader err)))

(export 'gdk-pixbuf-loader-close)

;;; --- End of file gdk-pixbuf.loader.lisp -------------------------------------
