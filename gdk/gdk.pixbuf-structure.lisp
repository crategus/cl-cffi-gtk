;;; ----------------------------------------------------------------------------
;;; gdk.pixbuf-structure.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See http://common-lisp.net/project/cl-gtk2/
;;;
;;; The documentation has been copied from the GDK 2 Reference Manual
;;; Version 2.24.10. See http://www.gtk.org.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2012 Dieter Kaiser
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
;;;     GDK_PIXBUF_ERROR
;;;     GdkColorspace
;;;     GdkPixbufAlphaMode
;;;     GdkPixbuf
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
;;; 
;;; Object Hierarchy
;;; 
;;;   GObject
;;;    +----GdkPixbuf
;;; 
;;; Implemented Interfaces
;;; 
;;; GdkPixbuf implements GIcon.
;;; Properties
;;; 
;;;   "bits-per-sample"    gint                : Read / Write / Construct Only
;;;   "colorspace"         GdkColorspace       : Read / Write / Construct Only
;;;   "has-alpha"          gboolean            : Read / Write / Construct Only
;;;   "height"             gint                : Read / Write / Construct Only
;;;   "n-channels"         gint                : Read / Write / Construct Only
;;;   "pixels"             gpointer            : Read / Write / Construct Only
;;;   "rowstride"          gint                : Read / Write / Construct Only
;;;   "width"              gint                : Read / Write / Construct Only
;;; 
;;; Description
;;; 
;;; The GdkPixbuf structure contains information that describes an image in
;;; memory.
;;; 
;;; Image Data
;;; 
;;; Image data in a pixbuf is stored in memory in uncompressed, packed format.
;;; Rows in the image are stored top to bottom, and in each row pixels are
;;; stored from left to right. There may be padding at the end of a row. The
;;; "rowstride" value of a pixbuf, as returned by gdk_pixbuf_get_rowstride(),
;;; indicates the number of bytes between rows.
;;; 
;;; Example 1. put_pixel() example
;;; 
;;; The following code illustrates a simple put_pixel() function for RGB pixbufs
;;; with 8 bits per channel with an alpha channel. It is not included in the
;;; gdk-pixbuf library for performance reasons; rather than making several
;;; function calls for each pixel, your own code can take shortcuts.
;;; 
;;; static void
;;; put_pixel (GdkPixbuf *pixbuf, int x, int y, guchar red, guchar green,
;;;                                             guchar blue, guchar alpha)
;;; {
;;;   int width, height, rowstride, n_channels;
;;;   guchar *pixels, *p;
;;; 
;;;   n_channels = gdk_pixbuf_get_n_channels (pixbuf);
;;; 
;;;   g_assert (gdk_pixbuf_get_colorspace (pixbuf) == GDK_COLORSPACE_RGB);
;;;   g_assert (gdk_pixbuf_get_bits_per_sample (pixbuf) == 8);
;;;   g_assert (gdk_pixbuf_get_has_alpha (pixbuf));
;;;   g_assert (n_channels == 4);
;;; 
;;;   width = gdk_pixbuf_get_width (pixbuf);
;;;   height = gdk_pixbuf_get_height (pixbuf);
;;; 
;;;   g_assert (x >= 0 && x < width);
;;;   g_assert (y >= 0 && y < height);
;;; 
;;;   rowstride = gdk_pixbuf_get_rowstride (pixbuf);
;;;   pixels = gdk_pixbuf_get_pixels (pixbuf);
;;; 
;;;   p = pixels + y * rowstride + x * n_channels;
;;;   p[0] = red;
;;;   p[1] = green;
;;;   p[2] = blue;
;;;   p[3] = alpha;
;;; }
;;; 
;;; This function will not work for pixbufs with images that are other than
;;; 8 bits per sample or channel, but it will work for most of the pixbufs that
;;; GTK+ uses.
;;;  
;;; Note
;;;
;;; If you are doing memcpy() of raw pixbuf data, note that the last row in the
;;; pixbuf may not be as wide as the full rowstride, but rather just as wide as
;;; the pixel data needs to be. That is, it is unsafe to do memcpy (dest,
;;; pixels, rowstride * height) to copy a whole pixbuf. Use gdk_pixbuf_copy()
;;; instead, or compute the width in bytes of the last row as
;;; width * ((n_channels * bits_per_sample + 7) / 8).
;;; ----------------------------------------------------------------------------

(in-package :gdk)

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
;;; 
;;; typedef enum {
;;;     GDK_COLORSPACE_RGB
;;; } GdkColorspace;
;;; 
;;; This enumeration defines the color spaces that are supported by the
;;; &gdk-pixbuf; library. Currently only RGB is supported.
;;; 
;;; GDK_COLORSPACE_RGB
;;;     Indicates a red/green/blue additive color space.
;;; ----------------------------------------------------------------------------

(define-g-enum "GdkColorspace" gdk-colorspace
  ()
  :rgb)

;;; ----------------------------------------------------------------------------
;;; enum GdkPixbufAlphaMode
;;; 
;;; typedef enum {
;;;         GDK_PIXBUF_ALPHA_BILEVEL,
;;;         GDK_PIXBUF_ALPHA_FULL
;;; } GdkPixbufAlphaMode;
;;; 
;;; These values can be passed to gdk_pixbuf_render_to_drawable_alpha() to
;;; control how the alpha channel of an image should be handled. This function
;;; can create a bilevel clipping mask (black and white) and use it while
;;; painting the image. In the future, when the X Window System gets an alpha
;;; channel extension, it will be possible to do full alpha compositing onto
;;; arbitrary drawables. For now both cases fall back to a bilevel clipping
;;; mask.
;;; 
;;; GDK_PIXBUF_ALPHA_BILEVEL
;;;     A bilevel clipping mask (black and white) will be created and used to
;;;     draw the image. Pixels below 0.5 opacity will be considered fully
;;;     transparent, and all others will be considered fully opaque.
;;; 
;;; GDK_PIXBUF_ALPHA_FULL
;;;     For now falls back to GDK_PIXBUF_ALPHA_BILEVEL. In the future it will
;;;     do full alpha compositing.
;;; ----------------------------------------------------------------------------

(define-g-enum "GdkPixbufAlphaMode" gdk-pixbuf-alpha-mode
  (:export t
   :type-initializer "gdk_pixbuf_alpha_mode_get_type")
  (:bilevel 0)
  (:full 1))

;;; ----------------------------------------------------------------------------
;;; GdkPixbuf
;;; 
;;; typedef struct _GdkPixbuf GdkPixbuf;
;;; 
;;; This is the main structure in the &gdk-pixbuf; library. It is used to
;;; represent images. It contains information about the image's pixel data, its
;;; color space, bits per sample, width and height, and the rowstride (the
;;; number of bytes between the start of one row and the start of the next).
;;; ----------------------------------------------------------------------------

(define-g-object-class "GdkPixbuf" gdk-pixbuf
  (:type-initializer "gdk_pixbuf_get_type")
  ((colorspace gdk-pixbuf-colorspace "colorspace" "GdkColorspace" t nil)
   (n-channels gdk-pixbuf-n-channels "n-channels" "gint" t nil)
   (has-alpha gdk-pixbuf-has-alpha "has-alpha" "gboolean" t nil)
   (bits-per-sample gdk-pixbuf-bits-per-sample "bits-per-sample" "gint" t nil)
   (width gdk-pixbuf-width "width" "gint" t nil)
   (height gdk-pixbuf-height "height" "gint" t nil)
   (rowstride gdk-pixbuf-rowstride "rowstride" "gint" t nil)
   (pixels gdk-pixbuf-pixels "pixels" "gpointer" t nil)))

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_get_colorspace ()
;;; 
;;; GdkColorspace gdk_pixbuf_get_colorspace (const GdkPixbuf *pixbuf);
;;; 
;;; Queries the color space of a pixbuf.
;;; 
;;; pixbuf :
;;;     A pixbuf.
;;; 
;;; Returns :
;;;     Color space.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_get_n_channels ()
;;; 
;;; int gdk_pixbuf_get_n_channels (const GdkPixbuf *pixbuf);
;;; 
;;; Queries the number of channels of a pixbuf.
;;; 
;;; pixbuf :
;;;     A pixbuf.
;;; 
;;; Returns :
;;;     Number of channels.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_get_has_alpha ()
;;; 
;;; gboolean gdk_pixbuf_get_has_alpha (const GdkPixbuf *pixbuf);
;;; 
;;; Queries whether a pixbuf has an alpha channel (opacity information).
;;; 
;;; pixbuf :
;;;     A pixbuf.
;;; 
;;; Returns :
;;;     TRUE if it has an alpha channel, FALSE otherwise.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_get_bits_per_sample ()
;;; 
;;; int gdk_pixbuf_get_bits_per_sample (const GdkPixbuf *pixbuf);
;;; 
;;; Queries the number of bits per color sample in a pixbuf.
;;; 
;;; pixbuf :
;;;     A pixbuf.
;;; 
;;; Returns :
;;;     Number of bits per color sample.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_get_pixels ()
;;; 
;;; guchar * gdk_pixbuf_get_pixels (const GdkPixbuf *pixbuf);
;;; 
;;; Queries a pointer to the pixel data of a pixbuf.
;;; 
;;; pixbuf :
;;;     A pixbuf.
;;; 
;;; Returns :
;;;     A pointer to the pixbuf's pixel data. Please see the section called
;;;     “Image Data” for information about how the pixel data is stored in
;;;     memory.
;;; ----------------------------------------------------------------------------

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
;;;     The length of the binary data. [out]
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
;;; 
;;; int gdk_pixbuf_get_width (const GdkPixbuf *pixbuf);
;;; 
;;; Queries the width of a pixbuf.
;;; 
;;; pixbuf :
;;;     A pixbuf.
;;; 
;;; Returns :
;;;     Width in pixels.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_get_height ()
;;; 
;;; int gdk_pixbuf_get_height (const GdkPixbuf *pixbuf);
;;; 
;;; Queries the height of a pixbuf.
;;; 
;;; pixbuf :
;;;     A pixbuf.
;;; 
;;; Returns :
;;;     Height in pixels.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_get_rowstride ()
;;; 
;;; int gdk_pixbuf_get_rowstride (const GdkPixbuf *pixbuf);
;;; 
;;; Queries the rowstride of a pixbuf, which is the number of bytes between
;;; the start of a row and the start of the next row.
;;; 
;;; pixbuf :
;;;     A pixbuf.
;;; 
;;; Returns :
;;;     Distance between row starts.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_get_byte_length ()
;;; 
;;; gsize gdk_pixbuf_get_byte_length (const GdkPixbuf *pixbuf);
;;; 
;;; Returns the length of the pixel data, in bytes.
;;; 
;;; pixbuf :
;;;     A pixbuf
;;; 
;;; Returns :
;;;     The length of the pixel data.
;;; 
;;; Since 2.26
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_get_option ()
;;; 
;;; const gchar * gdk_pixbuf_get_option (GdkPixbuf *pixbuf, const gchar *key);
;;; 
;;; Looks up key in the list of options that may have been attached to the
;;; pixbuf when it was loaded, or that may have been attached by another
;;; function using gdk_pixbuf_set_option().
;;; 
;;; For instance, the ANI loader provides "Title" and "Artist" options. The ICO,
;;; XBM, and XPM loaders provide "x_hot" and "y_hot" hot-spot options for cursor
;;; definitions. The PNG loader provides the tEXt ancillary chunk key/value
;;; pairs as options. Since 2.12, the TIFF and JPEG loaders return an
;;; "orientation" option string that corresponds to the embedded TIFF/Exif
;;; orientation tag (if present).
;;; 
;;; pixbuf :
;;;     a GdkPixbuf
;;; 
;;; key :
;;;     a nul-terminated string.
;;; 
;;; Returns :
;;;     the value associated with key. This is a nul-terminated string that
;;;     should not be freed or NULL if key was not found.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "bits-per-sample" property
;;; 
;;;   "bits-per-sample" gint                  : Read / Write / Construct Only
;;; 
;;; The number of bits per sample. Currently only 8 bit per sample are
;;; supported.
;;; 
;;; Allowed values: [1,16]
;;; 
;;; Default value: 8
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; The "colorspace" property
;;; 
;;;   "colorspace" GdkColorspace         : Read / Write / Construct Only
;;; 
;;; The colorspace in which the samples are interpreted.
;;; 
;;; Default value: GDK_COLORSPACE_RGB
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; The "has-alpha" property
;;; 
;;;   "has-alpha" gboolean              : Read / Write / Construct Only
;;; 
;;; Whether the pixbuf has an alpha channel.
;;; 
;;; Default value: FALSE
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; The "height" property
;;; 
;;;   "height" gint                  : Read / Write / Construct Only
;;; 
;;; The number of rows of the pixbuf.
;;; 
;;; Allowed values: >= 1
;;; 
;;; Default value: 1
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; The "n-channels" property
;;; 
;;;   "n-channels" gint                  : Read / Write / Construct Only
;;; 
;;; The number of samples per pixel. Currently, only 3 or 4 samples per pixel
;;; are supported.
;;; 
;;; Allowed values: >= 0
;;; 
;;; Default value: 3
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; The "pixels" property
;;; 
;;;   "pixels" gpointer              : Read / Write / Construct Only
;;; 
;;; A pointer to the pixel data of the pixbuf.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; The "rowstride" property
;;; 
;;;   "rowstride" gint                  : Read / Write / Construct Only
;;; 
;;; The number of bytes between the start of a row and the start of the next
;;; row. This number must (obviously) be at least as large as the width of the
;;; pixbuf.
;;; 
;;; Allowed values: >= 1
;;; 
;;; Default value: 1
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; The "width" property
;;; 
;;;   "width" gint                  : Read / Write / Construct Only
;;; 
;;; The number of columns of the pixbuf.
;;; 
;;; Allowed values: >= 1
;;; 
;;; Default value: 1
;;; ----------------------------------------------------------------------------

;;; --- End of file gdk.pixbuf-structure.lisp ----------------------------------
