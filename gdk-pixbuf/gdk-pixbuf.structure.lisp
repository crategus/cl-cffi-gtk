;;; ----------------------------------------------------------------------------
;;; gdk-pixbuf.structure.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See <http://common-lisp.net/project/cl-gtk2/>.
;;;
;;; The documentation of this file is taken from the GDK-PixBuf Reference Manual
;;; Version 2.26.1 and modified to document the Lisp binding to the GDK-PixBuf
;;; library. See <http://www.gtk.org>. The API documentation of the Lisp binding
;;; is available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2013 Dieter Kaiser
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
;;; The GdkPixbuf Structure
;;;
;;; Information that describes an image.
;;;
;;; Synopsis
;;;
;;;     GdkPixbufError
;;;
;;;     GDK_PIXBUF_ERROR
;;;
;;;     GdkColorspace
;;;     GdkPixbufAlphaMode
;;;     GdkPixbuf
;;;
;;;     gdk_pixbuf_get_colorspace
;;;     gdk_pixbuf_get_n_channels
;;;     gdk_pixbuf_get_has_alpha
;;;     gdk_pixbuf_get_bits_per_sample
;;;     gdk_pixbuf_get_pixels
;;;     gdk_pixbuf_get_pixels_with_length
;;;     gdk_pixbuf_get_width
;;;     gdk_pixbuf_get_height
;;;     gdk_pixbuf_get_rowstride
;;;     gdk_pixbuf_get_byte_length
;;;     gdk_pixbuf_get_option
;;; ----------------------------------------------------------------------------

(in-package :gdk-pixbuf)

;;; ----------------------------------------------------------------------------
;;; enum GdkPixbufError
;;;
;;; typedef enum {
;;;         /* image data hosed */
;;;         GDK_PIXBUF_ERROR_CORRUPT_IMAGE,
;;;         /* no mem to load image */
;;;         GDK_PIXBUF_ERROR_INSUFFICIENT_MEMORY,
;;;         /* bad option passed to save routine */
;;;         GDK_PIXBUF_ERROR_BAD_OPTION,
;;;         /* unsupported image type (sort of an ENOSYS) */
;;;         GDK_PIXBUF_ERROR_UNKNOWN_TYPE,
;;;         /* unsupported operation (load, save) for image type */
;;;         GDK_PIXBUF_ERROR_UNSUPPORTED_OPERATION,
;;;         GDK_PIXBUF_ERROR_FAILED
;;; } GdkPixbufError;
;;;
;;; An error code in the GDK_PIXBUF_ERROR domain. Many &gdk-pixbuf; operations
;;; can cause errors in this domain, or in the G_FILE_ERROR domain.
;;;
;;; GDK_PIXBUF_ERROR_CORRUPT_IMAGE
;;;     An image file was broken somehow.
;;;
;;; GDK_PIXBUF_ERROR_INSUFFICIENT_MEMORY
;;;     Not enough memory.
;;;
;;; GDK_PIXBUF_ERROR_BAD_OPTION
;;;     A bad option was passed to a pixbuf save module.
;;;
;;; GDK_PIXBUF_ERROR_UNKNOWN_TYPE
;;;     Unknown image type.
;;;
;;; GDK_PIXBUF_ERROR_UNSUPPORTED_OPERATION
;;;     Don't know how to perform the given operation on the type of image at
;;;     hand.
;;;
;;; GDK_PIXBUF_ERROR_FAILED
;;;     Generic failure code, something went wrong.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GDK_PIXBUF_ERROR
;;;
;;; #define GDK_PIXBUF_ERROR gdk_pixbuf_error_quark ()
;;;
;;; Error domain used for pixbuf operations. Indicates that the error code will
;;; be in the GdkPixbufError enumeration. See GError for information on error
;;; domains and error codes.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; enum GdkColorspace
;;; ----------------------------------------------------------------------------

(define-g-enum "GdkColorspace" gdk-colorspace
  (:export t
   :type-initializer "gdk_colorspace_get_type")
  :rgb)

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-colorspace atdoc:*symbol-name-alias*)
      "Enum"
      (gethash 'gdk-colorspace atdoc:*external-symbols*)
 "@version{2013-6-22}
  @begin{short}
    This enumeration defines the color spaces that are supported by the
    GDK-Pixbuf library. Currently only RGB is supported.
  @end{short}
  @begin{pre}
(define-g-enum \"GdkColorspace\" gdk-colorspace
  (:export t
   :type-initializer \"gdk_colorspace_get_type\")
  :rgb)
  @end{pre}
  @begin[code]{table}
    @entry[:rgb]{Indicates a red/green/blue additive color space.}
  @end{table}")

;;; ----------------------------------------------------------------------------
;;; enum GdkPixbufAlphaMode
;;; ----------------------------------------------------------------------------

(define-g-enum "GdkPixbufAlphaMode" gdk-pixbuf-alpha-mode
  (:export t
   :type-initializer "gdk_pixbuf_alpha_mode_get_type")
  (:bilevel 0)
  (:full 1))

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-pixbuf-alpha-mode atdoc:*symbol-name-alias*)
      "Enum"
      (gethash 'gdk-pixbuf-alpha-mode atdoc:*external-symbols*)
 "@version{2013-2-16}
  @begin{short}
    These values can be passed to @code{gdk_pixbuf_render_to_drawable_alpha()}
    to control how the alpha channel of an image should be handled.
  @end{short}
  This function can create a bilevel clipping mask (black and white) and use it
  while painting the image. In the future, when the X Window System gets an
  alpha channel extension, it will be possible to do full alpha compositing onto
  arbitrary drawables. For now both cases fall back to a bilevel clipping mask.
  @begin{pre}
(define-g-enum \"GdkPixbufAlphaMode\" gdk-pixbuf-alpha-mode
  (:export t
   :type-initializer \"gdk_pixbuf_alpha_mode_get_type\")
  (:bilevel 0)
  (:full 1))
  @end{pre}
  @begin[code]{table}
    @entry[:bilevel]{A bilevel clipping mask (black and white) will be created
      and used to draw the image. Pixels below 0.5 opacity will be considered
      fully transparent, and all others will be considered fully opaque.}
    @entry[:full]{For now falls back to @code{:bilevel}. In the future it will
      do full alpha compositing.}
  @end{table}")

;;; ----------------------------------------------------------------------------
;;; GdkPixbuf
;;; ----------------------------------------------------------------------------

(define-g-object-class "GdkPixbuf" gdk-pixbuf
  (:superclass g-object
   :export t
   :interfaces nil
   :type-initializer "gdk_pixbuf_get_type")
  ((bits-per-sample
    gdk-pixbuf-bits-per-sample
    "bits-per-sample" "gint" t nil)
   (colorspace
    gdk-pixbuf-colorspace
    "colorspace" "GdkColorspace" t nil)
   (has-alpha
    gdk-pixbuf-has-alpha
    "has-alpha" "gboolean" t nil)
   (height
    gdk-pixbuf-height
    "height" "gint" t nil)
   (n-channels
    gdk-pixbuf-n-channels
    "n-channels" "gint" t nil)
   (pixels
    gdk-pixbuf-pixels
    "pixels" "gpointer" t nil)
   (rowstride
    gdk-pixbuf-rowstride
    "rowstride" "gint" t nil)
   (width
    gdk-pixbuf-width
    "width" "gint" t nil)))

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation 'gdk-pixbuf 'type)
 "@version{2013-2-8}
  @begin{short}
    This is the main structure in the @sym{gdk-pixbuf} library. It is used to
    represent images. It contains information about the image's pixel data, its
    color space, bits per sample, width and height, and the rowstride (the
    number of bytes between the start of one row and the start of the next).
  @end{short}

  @heading{Image Data}
  Image data in a pixbuf is stored in memory in uncompressed, packed format.
  Rows in the image are stored top to bottom, and in each row pixels are
  stored from left to right. There may be padding at the end of a row. The
  \"rowstride\" value of a pixbuf, as returned by
  @fun{gdk-pixbuf-get-rowstride}, indicates the number of bytes between rows.

  @subheading{Example 1. put_pixel() example}
  The following code illustrates a simple @code{put_pixel()} function for RGB
  pixbufs with 8 bits per channel with an alpha channel. It is not included in
  the @sym{gdk-pixbuf} library for performance reasons; rather than making
  several function calls for each pixel, your own code can take shortcuts.
  @begin{pre}
 static void
 put_pixel (GdkPixbuf *pixbuf, int x, int y, guchar red, guchar green,
                                             guchar blue, guchar alpha)
 {
   int width, height, rowstride, n_channels;
   guchar *pixels, *p;

   n_channels = gdk_pixbuf_get_n_channels (pixbuf);

   g_assert (gdk_pixbuf_get_colorspace (pixbuf) == GDK_COLORSPACE_RGB);
   g_assert (gdk_pixbuf_get_bits_per_sample (pixbuf) == 8);
   g_assert (gdk_pixbuf_get_has_alpha (pixbuf));
   g_assert (n_channels == 4);

   width = gdk_pixbuf_get_width (pixbuf);
   height = gdk_pixbuf_get_height (pixbuf);

   g_assert (x >= 0 && x < width);
   g_assert (y >= 0 && y < height);

   rowstride = gdk_pixbuf_get_rowstride (pixbuf);
   pixels = gdk_pixbuf_get_pixels (pixbuf);

   p = pixels + y * rowstride + x * n_channels;
   p[0] = red;
   p[1] = green;
   p[2] = blue;
   p[3] = alpha;
  @}
  @end{pre}
  This function will not work for pixbufs with images that are other than
  8 bits per sample or channel, but it will work for most of the pixbufs that
  GTK+ uses.

  @heading{Note}
  If you are doing @code{memcpy()} of raw pixbuf data, note that the last row in
  the pixbuf may not be as wide as the full rowstride, but rather just as wide
  as the pixel data needs to be. That is, it is unsafe to do
  @code{memcpy (dest, pixels, rowstride * height)} to copy a whole pixbuf. Use
  @fun{gdk-pixbuf-copy} instead, or compute the width in bytes of the last row
  as @code{width * ((n_channels * bits_per_sample + 7) / 8)}.
  @see-slot{gdk-pixbuf-bits-per-sample}
  @see-slot{gdk-pixbuf-colorspace}
  @see-slot{gdk-pixbuf-has-alpha}
  @see-slot{gdk-pixbuf-height}
  @see-slot{gdk-pixbuf-n-channels}
  @see-slot{gdk-pixbuf-pixels}
  @see-slot{gdk-pixbuf-rowstride}
  @see-slot{gdk-pixbuf-width}")

;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "bits-per-sample" 'gdk-pixbuf) 't)
 "The @code{\"bits-per-sample\"} property of type @code{gint}
  (Read / Write / Construct Only)@br{}
  The number of bits per sample. Currently only 8 bit per sample are
  supported.@br{}
  Allowed values: @code{[1,16]}@br{}
  Default value: @code{8}")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "colorspace" 'gdk-pixbuf) 't)
 "The @code{\"colorspace\"} property of type @symbol{gdk-colorspace}
  (Read / Write / Construct Only)@br{}
  The colorspace in which the samples are interpreted.@br{}
  Default value: @code{:rgb}")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "has-alpha" 'gdk-pixbuf) 't)
 "The @code{\"has-alpha\"} property of type @code{gboolean}
  (Read / Write / Construct Only)@br{}
  Whether the pixbuf has an alpha channel.@br{}
  Default value: @code{nil}")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "height" 'gdk-pixbuf) 't)
 "The @code{\"height\"} property of type @code{gint}
  (Read / Write / Construct Only)@br{}
  The number of rows of the pixbuf.@br{}
  Allowed values: @code{>= 1}@br{}
  Default value: @code{1}")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "n-channels" 'gdk-pixbuf) 't)
 "The @code{\"n-channels\"} property of type @code{gint}
  (Read / Write / Construct Only)@br{}
  The number of samples per pixel. Currently, only 3 or 4 samples per pixel
  are supported.@br{}
  Allowed values: @code{>= 0}@br{}
  Default value: @code{3}")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "pixels" 'gdk-pixbuf) 't)
 "The @code{\"pixels\"} property of type @code{gpointer}
  (Read / Write / Construct Only)@br{}
  A pointer to the pixel data of the pixbuf.")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "rowstride" 'gdk-pixbuf) 't)
 "The @code{\"rowstride\"} property of type @code{gint}
  (Read / Write / Construct Only)@br{}
  The number of bytes between the start of a row and the start of the next
  row. This number must (obviously) be at least as large as the width of the
  pixbuf.@br{}
  Allowed values: @code{>= 1}@br{}
  Default value: @code{1}")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "width" 'gdk-pixbuf) 't)
 "The @code{\"width\"} property of type @code{gint}
  (Read / Write / Construct Only)@br{}
  The number of columns of the pixbuf.@br{}
  Allowed values: @code{>= 1}@br{}
  Default value: @code{1}")

;;; ----------------------------------------------------------------------------
;;;
;;; Accessors
;;;
;;; ----------------------------------------------------------------------------

;;; --- gdk-pixbuf-bits-per-sample ---------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-pixbuf-bits-per-sample atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-pixbuf-bits-per-sample 'function)
 "@version{2013-2-8}
  @begin{short}
    Accessor of the slot @code{\"bits-per-sample\"} of the @class{gdk-pixbuf}
    class.
  @end{short}")

;;; --- gdk-pixbuf-colorspace --------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-pixbuf-colorspace atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-pixbuf-colorspace 'function)
 "@version{2013-2-8}
  @begin{short}
    Accessor of the slot @code{\"colorspace\"} of the @class{gdk-pixbuf} class.
  @end{short}")

;;; --- gdk-pixbuf-has-alpha ---------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-pixbuf-has-alpha atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-pixbuf-has-alpha 'function)
 "@version{2013-2-8}
  @begin{short}
    Accessor of the slot @code{\"has-alpha\"} of the @class{gdk-pixbuf} class.
  @end{short}")

;;; --- gdk-pixbuf-height ------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-pixbuf-height atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-pixbuf-height 'function)
 "@version{2013-2-8}
  @begin{short}
    Accessor of the slot @code{\"height\"} of the @class{gdk-pixbuf} class.
  @end{short}")

;;; --- gdk-pixbuf-n-channels --------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-pixbuf-n-channels atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-pixbuf-n-channels 'function)
 "@version{2013-2-8}
  @begin{short}
    Accessor of the slot @code{\"n-channels\"} of the @class{gdk-pixbuf} class.
  @end{short}")

;;; --- gdk-pixbuf-pixels ------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-pixbuf-pixels atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-pixbuf-pixels 'function)
 "@version{2013-2-8}
  @begin{short}
    Accessor of the slot @code{\"pixels\"} of the @class{gdk-pixbuf} class.
  @end{short}")

;;; --- gdk-pixbuf-rowstride ---------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-pixbuf-rowstride atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-pixbuf-rowstride 'function)
 "@version{2013-2-8}
  @begin{short}
    Accessor of the slot @code{\"rowstride\"} of the @class{gdk-pixbuf} class.
  @end{short}")

;;; --- gdk-pixbuf-width -------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-pixbuf-width atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-pixbuf-width 'function)
 "@version{2013-2-8}
  @begin{short}
    Accessor of the slot @code{\"width\"} of the @class{gdk-pixbuf} class.
  @end{short}")

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_get_colorspace ()
;;; ----------------------------------------------------------------------------

(declaim (inline gdk-pixbuf-get-colorspace))

(defun gdk-pixbuf-get-colorspace (pixbuf)
 #+cl-cffi-gtk-documentation
 "@version{2013-2-16}
  @argument[pixbuf]{a pixbuf.}
  @return{Color space.}
  @short{Queries the color space of a pixbuf.}"
  (gdk-pixbuf-colorspace pixbuf))

(export 'gdk-pixbuf-get-colorspace)

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_get_n_channels ()
;;; ----------------------------------------------------------------------------

(declaim (inline gdk-pixbuf-get-n-channels))

(defun gdk-pixbuf-get-n-channels (pixbuf)
 #+cl-cffi-gtk-documentation
 "@version{2013-2-16}
  @argument{pixbuf]{a pixbuf.}
  @return{Number of channels.}
  @short{Queries the number of channels of a pixbuf.}"
  (gdk-pixbuf-n-channels pixbuf))

(export 'gdk-pixbuf-get-n-channels)

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_get_has_alpha ()
;;; ----------------------------------------------------------------------------

(declaim (inline gdk-pixbuf-get-has-alpha))

(defun gdk-pixbuf-get-has-alpha (pixbuf)
 #+cl-cffi-gtk-documentation
 "@version{2013-2-16}
  @argument[pixbuf]{a pixbuf.}
  @return{@arg{true} if it has an alpha channel, @code{nil} otherwise.}
  @short{Queries whether a pixbuf has an alpha channel (opacity information).}"
  (gdk-pixbuf-has-alpha pixbuf))

(export 'gdk-pixbuf-get-has-alpha)

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_get_bits_per_sample ()
;;; ----------------------------------------------------------------------------

(declaim (inline gdk-pixbuf-get-bits-per-sample))

(defun gdk-pixbuf-get-bits-per-sample (pixbuf)
 #+cl-cffi-gtk-documentation
 "@version{2013-2-16}
  @argument[pixbuf]{a pixbuf.}
  @return{Number of bits per color sample.}
  @short{Queries the number of bits per color sample in a pixbuf.}"
  (gdk-pixbuf-bits-per-sample pixbuf))

(export 'gdk-pixbuf-get-bits-per-sample)

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_get_pixels ()
;;; ----------------------------------------------------------------------------

(declaim (inline gdk-pixbuf-get-pixels))

(defun gdk-pixbuf-get-pixels (pixbuf)
 #+cl-cffi-gtk-documentation
 "@version{2013-2-16}
  @argument[pixbuf]{a pixbuf.}
  @return{A pointer to the pixbuf's pixel data. Please see the section called
    \"Image Data\" for information about how the pixel data is stored in
    memory.}
  @short{Queries a pointer to the pixel data of a pixbuf.}"
  (gdk-pixbuf-pixels pixbuf))

(export 'gdk-pixbuf-get-pixels)

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_get_pixels_with_length ()
;;;
;;; guchar * gdk_pixbuf_get_pixels_with_length (const GdkPixbuf *pixbuf,
;;;                                             guint *length);
;;;
;;; Queries a pointer to the pixel data of a pixbuf.
;;;
;;; pixbuf :
;;;     A pixbuf.
;;;
;;; length :
;;;     The length of the binary data
;;;
;;; Returns :
;;;     A pointer to the pixbuf's pixel data. Please see the section called
;;;     “Image Data” for information about how the pixel data is stored in
;;;     memory. Rename to: gdk_pixbuf_get_pixels.
;;;
;;; Since 2.26
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_get_width ()
;;; ----------------------------------------------------------------------------

(declaim (inline gdk-pixbuf-get-width))

(defun gdk-pixbuf-get-width (pixbuf)
 #+cl-cffi-gtk-documentation
 "@version{2013-2-16}
  @argument[pixbuf]{a pixbuf.}
  @return{Width in pixels.}
  @short{Queries the width of a pixbuf.}"
  (gdk-pixbuf-width pixbuf))

(export 'gdk-pixbuf-get-width)

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_get_height ()
;;; ----------------------------------------------------------------------------

(declaim (inline gdk-pixbuf-get-height))

(defun gdk-pixbuf-get-height (pixbuf)
 #+cl-cffi-gtk-documentation
 "@version{2013-2-16}
  @argument[pixbuf]{a pixbuf.}
  @return{Height in pixels.}
  @short{Queries the height of a pixbuf.}"
  (gdk-pixbuf-height pixbuf))

(export 'gdk-pixbuf-get-height)

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_get_rowstride ()
;;; ----------------------------------------------------------------------------

(declaim (inline gdk-pixbuf-get-rowstride))

(defun gdk-pixbuf-get-rowstride (pixbuf)
 #+cl-cffi-gtk-documentation
 "@version{2013-2-16}
  @argument[pixbuf]{a pixbuf.}
  @return{Distance between row starts.}
  @begin{short}
    Queries the rowstride of a pixbuf, which is the number of bytes between
    the start of a row and the start of the next row.
  @end{short}"
(gdk-pixbuf-rowstride pixbuf))

(export 'gdk-pixbuf-get-rowstride)

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_get_byte_length ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_pixbuf_get_byte_length" gdk-pixbuf-get-byte-length) g-size
 #+cl-cffi-gtk-documentation
 "@version{2013-2-16}
  @argument[pixbuf]{a pixbuf}
  @return{The length of the pixel data.}
  @short{Returns the length of the pixel data, in bytes.}

  Since 2.26"
  (pixbuf (g-object gdk-pixbuf)))

(export 'gdk-pixbuf-get-byte-length)

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_get_option ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_pixbuf_get_option" gdk-pixbuf-get-option) :string
 #+cl-cffi-gtk-documentation
 "@version{2013-2-16}
  @argument[pixbuf]{a @class{gdk-pixbuf object}}
  @argument[key]{a string.}
  @return{The value associated with @arg{key}. This is a string or @code{nil} if
    key was not found.}
  @begin{short}
    Looks up key in the list of options that may have been attached to the
    pixbuf when it was loaded, or that may have been attached by another
    function using @code{gdk_pixbuf_set_option()}.
  @end{short}
  For instance, the ANI loader provides \"Title\" and \"Artist\" options. The
  ICO, XBM, and XPM loaders provide \"x_hot\" and \"y_hot\" hot-spot options for
  cursor definitions. The PNG loader provides the tEXt ancillary chunk key/value
  pairs as options. Since 2.12, the TIFF and JPEG loaders return an
  \"orientation\" option string that corresponds to the embedded TIFF/Exif
  orientation tag (if present)."
  (pixbuf (g-object gdk-pixbuf))
  (key :string))

(export 'gdk-pixbuf-get-option)

(defcfun ("gdk_pixbuf_get_options" gdk-pixbuf-get-options) (g-hash-table :string :string)
  (pixbuf (g-object gdk-pixbuf)))

(export 'gdk-pixbuf-get-options)

;;; --- End of file gdk-pixbuf.structure.lisp ----------------------------------
