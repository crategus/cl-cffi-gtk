;;; ----------------------------------------------------------------------------
;;; gdk-pixbuf.structure.lisp
;;;
;;; The documentation of this file is taken from the GDK-PixBuf Reference Manual
;;; Version 2.36 and modified to document the Lisp binding to the GDK-PixBuf
;;; library. See <http://www.gtk.org>. The API documentation of the Lisp
;;; binding is available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2020 Dieter Kaiser
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
;;;     Information that describes an image.
;;;
;;; Types and Values
;;;
;;;     GdkPixbufError
;;;     GDK_PIXBUF_ERROR
;;;     GdkColorspace
;;;     GdkPixbufAlphaMode
;;;     GdkPixbuf
;;;
;;; Functions
;;;
;;;     gdk_pixbuf_get_colorspace                          Accessor
;;;     gdk_pixbuf_get_n_channels                          Accessor
;;;     gdk_pixbuf_get_has_alpha                           Accessor
;;;     gdk_pixbuf_get_bits_per_sample                     Accessor
;;;     gdk_pixbuf_get_pixels                              Accessor
;;;     gdk_pixbuf_get_pixels_with_length
;;;     gdk_pixbuf_get_width                               Accessor
;;;     gdk_pixbuf_get_height                              Accessor
;;;     gdk_pixbuf_get_rowstride                           Accessor
;;;     gdk_pixbuf_get_byte_length
;;;     gdk_pixbuf_get_option
;;;     gdk_pixbuf_set_option
;;;     gdk_pixbuf_remove_option
;;;     gdk_pixbuf_get_options
;;;     gdk_pixbuf_copy_options
;;;     gdk_pixbuf_read_pixels
;;;
;;; Properties
;;;
;;;          gint    bits-per-sample    Read / Write / Construct Only
;;; GdkColorspace    colorspace         Read / Write / Construct Only
;;;      gboolean    has-alpha          Read / Write / Construct Only
;;;          gint    height             Read / Write / Construct Only
;;;          gint    n-channels         Read / Write / Construct Only
;;;        GBytes*   pixel-bytes        Read / Write / Construct Only
;;;      gpointer    pixels             Read / Write / Construct Only
;;;          gint    rowstride          Read / Write / Construct Only
;;;          gint    width              Read / Write / Construct Only
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GdkPixbuf
;;;
;;; Implemented Interfaces
;;;
;;;     GdkPixbuf implements GIcon and GLoadableIcon.
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
(setf (gethash 'gdk-colorspace atdoc:*symbol-name-alias*) "Enum"
      (gethash 'gdk-colorspace atdoc:*external-symbols*)
 "@version{2020-11-20}
  @begin{short}
    This enumeration defines the color spaces that are supported by the
    GDK-Pixbuf library.
  @end{short}
  Currently only RGB is supported.
  @begin{pre}
(define-g-enum \"GdkColorspace\" gdk-colorspace
  (:export t
   :type-initializer \"gdk_colorspace_get_type\")
  :rgb)
  @end{pre}
  @begin[code]{table}
    @entry[:rgb]{Indicates a red/green/blue additive color space.}
  @end{table}
  @see-class{gdk-pixbuf}")

;;; ----------------------------------------------------------------------------
;;; enum GdkPixbufAlphaMode                                not exported
;;; ----------------------------------------------------------------------------

;; Only neededd for deprecated functionality

(define-g-enum "GdkPixbufAlphaMode" gdk-pixbuf-alpha-mode
  (:export nil
   :type-initializer "gdk_pixbuf_alpha_mode_get_type")
  (:bilevel 0)
  (:full 1))

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-pixbuf-alpha-mode atdoc:*symbol-name-alias*) "Enum"
      (gethash 'gdk-pixbuf-alpha-mode atdoc:*external-symbols*)
 "@version{2020-11-20}
  @begin{short}
    These values can be passed to the function
    @code{gdk_pixbuf_render_to_drawable_alpha()} to control how the alpha
    channel of an image should be handled.
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
  @end{table}
  @see-class{gdk-pixbuf}")

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
   ;; TODO: GBytes is not implemented
   (pixel-bytes
    gdk-pixbuf-pixel-bytes
    "pixel-bytes" "GBytes" t t)
   (pixels
    gdk-pixbuf-pixels
    "pixels" "gpointer" t nil)
   (rowstride
    gdk-pixbuf-rowstride
    "rowstride" "gint" t nil)
   (width
    gdk-pixbuf-width
    "width" "gint" t nil)))

#+cl-cffi-gtk-documentation
(setf (documentation 'gdk-pixbuf 'type)
 "@version{2020-11-21}
  @begin{short}
    The @sym{gdk-pixbuf} structure contains information that describes an image
    in memory.
  @end{short}
  It contains information about the image's pixel data, its color space, bits
  per sample, width and height, and the rowstride (the number of bytes between
  the start of one row and the start of the next).
  @begin[Image Data]{dictionary}
    Image data in a pixbuf is stored in memory in uncompressed, packed format.
    Rows in the image are stored top to bottom, and in each row pixels are
    stored from left to right. There may be padding at the end of a row. The
    \"rowstride\" value of a pixbuf, as returned by the function
    @fun{gdk-pixbuf-rowstride}, indicates the number of bytes between rows.
  @end{dictionary}
  @begin[Examples]{dictionary}
    The following code illustrates a simple function @code{put-pixel} for RGB
    pixbufs with 8 bits per channel with an alpha channel. It is not included
    in the @sym{gdk-pixbuf} library for performance reasons. Rather than making
    several function calls for each pixel, your own code can take shortcuts.
    @begin{pre}
(defun put-pixel (pixbuf x y red green blue alpha)
  (let ((n-channels (gdk-pixbuf-n-channels pixbuf))
        (rowstride (gdk-pixbuf-rowstride pixbuf))
        (pixels (gdk-pixbuf-pixels pixbuf)))
    ;; Add offset to the pointer pixels into the pixbuf
    (incf-pointer pixels (+ (* y rowstride) (* x n-channels)))
    ;; Set the color of the point and the alpha value
    (setf (mem-aref pixels :uchar 0) red)
    (setf (mem-aref pixels :uchar 1) green)
    (setf (mem-aref pixels :uchar 2) blue)
    (setf (mem-aref pixels :uchar 3) alpha)))
    @end{pre}
    This function will not work for pixbufs with images that are other than
    8 bits per sample or channel, but it will work for most of the pixbufs that
    GTK+ uses.
  @end{dictionary}
  @begin[Note]{dictionary}
    If you are doing @code{memcpy()} of raw pixbuf data, note that the last row
    in the pixbuf may not be as wide as the full rowstride, but rather just as
    wide as the pixel data needs to be. That is, it is unsafe to do
    @code{memcpy (dest, pixels, rowstride * height)} to copy a whole pixbuf. Use
    the function @fun{gdk-pixbuf-copy} instead, or compute the width in bytes of
    the last row as @code{width * ((n_channels * bits_per_sample + 7) / 8)}.
  @end{dictionary}
  @see-slot{gdk-pixbuf-bits-per-sample}
  @see-slot{gdk-pixbuf-colorspace}
  @see-slot{gdk-pixbuf-has-alpha}
  @see-slot{gdk-pixbuf-height}
  @see-slot{gdk-pixbuf-n-channels}
  @see-slot{gdk-pixbuf-pixel-bytes}
  @see-slot{gdk-pixbuf-pixels}
  @see-slot{gdk-pixbuf-rowstride}
  @see-slot{gdk-pixbuf-width}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gdk-pixbuf-bits-per-sample ---------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "bits-per-sample"
                                               'gdk-pixbuf) 't)
 "The @code{bits-per-sample} property of type @code{:int}
  (Read / Write / Construct Only) @br{}
  The number of bits per sample. Currently only 8 bit per sample are
  supported. @br{}
  Allowed values: [1,16] @br{}
  Default value: 8")

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-pixbuf-bits-per-sample atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-pixbuf-bits-per-sample 'function)
 "@version{2020-11-20}
  @syntax[]{(gdk-pixbuf-bits-per-sample object) => bits-per-sample}
  @argument[object]{a @class{gdk-pixbuf} object}
  @argument[bits-per-sample]{an integer with the number of bits per sample}
  @begin{short}
    Accessor of the @slot[gdk-pixbuf]{bits-per-sample} slot of the
    @class{gdk-pixbuf} class.
  @end{short}

  Queries the number of bits per color sample in a pixbuf. Currently only 8 bit
  per sample are supported.
  @see-class{gdk-pixbuf}")

;;; --- gdk-pixbuf-colorspace --------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "colorspace" 'gdk-pixbuf) 't)
 "The @code{colorspace} property of type @symbol{gdk-colorspace}
  (Read / Write / Construct Only) @br{}
  The colorspace in which the samples are interpreted. @br{}
  Default value: @code{:rgb}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-pixbuf-colorspace atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-pixbuf-colorspace 'function)
 "@version{2020-11-20}
  @syntax[]{(gdk-pixbuf-colorspace object) => colorspace}
  @argument[object]{a @class{gdk-pixbuf} object}
  @argument[colorspace]{a @symbol{gdk-colorspace} value}
  @begin{short}
    Accessor of the @slot[gdk-pixbuf]{colorspace} slot of the
    @class{gdk-pixbuf} class.
  @end{short}

  Querries the colorspace in which the samples are interpreted.
  @see-class{gdk-pixbuf}
  @see-symbol{gdk-colorspace}")

;;; --- gdk-pixbuf-has-alpha ---------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "has-alpha" 'gdk-pixbuf) 't)
 "The @code{has-alpha} property of type @code{:boolean}
  (Read / Write / Construct Only) @br{}
  Whether the pixbuf has an alpha channel. @br{}
  Default value: @em{false}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-pixbuf-has-alpha atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-pixbuf-has-alpha 'function)
 "@version{2020-11-20}
  @syntax[]{(gdk-pixbuf-has-alpha object) => has-alpha}
  @argument[object]{a @class{gdk-pixbuf} object}
  @argument[has-alpha]{a boolean whether the pixbuf has an alpha channel}
  @begin{short}
    Accessor of the @slot[gdk-pixbuf]{has-alpha} slot of the @class{gdk-pixbuf}
    class.
  @end{short}

  Queries whether a pixbuf has an alpha channel (opacity information).
  @see-class{gdk-pixbuf}")

;;; --- gdk-pixbuf-height ------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "height" 'gdk-pixbuf) 't)
 "The @code{height} property of type @code{:int} (Read / Write / Construct Only)
  @br{}
  The number of rows of the pixbuf. @br{}
  Allowed values: >= 1 @br{}
  Default value: 1")

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-pixbuf-height atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-pixbuf-height 'function)
 "@version{2020-11-20}
  @syntax[]{(gdk-pixbuf-height object) => height}
  @argument[object]{a @class{gdk-pixbuf} object}
  @argument[height]{an integer with the number of rows of the pixbuf}
  @begin{short}
    Accessor of the @slot[gdk-pixbuf]{height} slot of the @class{gdk-pixbuf}
    class.
  @end{short}

  Queries the height of a pixbuf.
  @see-class{gdk-pixbuf}")

;;; --- gdk-pixbuf-n-channels --------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "n-channels" 'gdk-pixbuf) 't)
 "The @code{n-channels} property of type @code{:int}
  (Read / Write / Construct Only) @br{}
  The number of samples per pixel. Currently, only 3 or 4 samples per pixel
  are supported. @br{}
  Allowed values: >= 0 @br{}
  Default value: 3")

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-pixbuf-n-channels atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-pixbuf-n-channels 'function)
 "@version{2020-11-21}
  @syntax[]{(gdk-pixbuf-n-channels object) => n-channels}
  @argument[object]{a @class{gdk-pixbuf} object}
  @argument[n-channels]{an integer with the number of channels}
  @begin{short}
    Accessor of the @slot[gdk-pixbuf]{n-channels} slot of the
    @class{gdk-pixbuf} class.
  @end{short}

  Queries the number of channels of a pixbuf.
  @see-class{gdk-pixbuf}")

;;; --- gdk-pixbuf-pixel-bytes -------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "pixel-bytes" 'gdk-pixbuf) 't)
 "The @code{pixel-bytes} property of type @code{GBytes}
  (Read / Write / Construct Only) @br{}
  Readonly pixel data.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-pixbuf-pixel-bytes atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-pixbuf-pixel-bytes 'function)
 "@version{2020-11-24}
  @syntax[]{(gdk-pixbuf-pixel-bytes object) => pixel-bytes}
  @argument[object]{a @class{gdk-pixbuf} object}
  @argument[pixel-bytes]{pixel data of type @code{GByts}}
  @begin{short}
    Accessor of the @slot[gdk-pixbuf]{pixel-bytes} slot of the
    @class{gdk-pixbuf} class.
  @end{short}

  Querries the readonly pixel data.
  @begin[Note]{dictionary}
    At this time the GBytes structure is not implemented. This function
    signals an error.
  @end{dictionary}
  @see-class{gdk-pixbuf}")

;;; --- gdk-pixbuf-pixels ------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "pixels" 'gdk-pixbuf) 't)
 "The @code{pixels} property of type @code{:pointer}
  (Read / Write / Construct Only) @br{}
  A pointer to the pixel data of the pixbuf.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-pixbuf-pixels atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-pixbuf-pixels 'function)
 "@version{2020-11-24}
  @syntax[]{(gdk-pixbuf-pixels object) => pixels}
  @argument[object]{a @class{gdk-pixbuf} object}
  @argument[pixels]{a pointer to the pixel data of @arg{pixbuf}}
  @begin{short}
    Accessor of the @slot[gdk-pixbuf]{pixels} slot of the @class{gdk-pixbuf}
    class.
  @end{short}

  Queries a pointer to the pixel data of a pixbuf. Please see the section called
  \"Image Data\" in the @class{gdk-pixbuf} documentation for information about
  how the pixel data is stored in memory. This function will cause an implicit
  copy of the pixbuf data if the pixbuf was created from read-only data.
  @see-class{gdk-pixbuf}")

;;; --- gdk-pixbuf-rowstride ---------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "rowstride" 'gdk-pixbuf) 't)
 "The @code{rowstride} property of type @code{:int}
  (Read / Write / Construct Only) @br{}
  The number of bytes between the start of a row and the start of the next
  row. This number must be at least as large as the width of the pixbuf. @br{}
  Allowed values: >= 1 @br{}
  Default value: 1")

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-pixbuf-rowstride atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-pixbuf-rowstride 'function)
 "@version{2020-11-21}
  @syntax[]{(gdk-pixbuf-rowstride object) => rowstride}
  @argument[object]{a @class{gdk-pixbuf} object}
  @argument[rowstride]{an integer with the distance between row starts}
  @begin{short}
    Accessor of the @slot[gdk-pixbuf]{rowstride} slot of the @class{gdk-pixbuf}
    class.
  @end{short}

  Queries the rowstride of a pixbuf, which is the number of bytes between
  the start of a row and the start of the next row.
  @see-class{gdk-pixbuf}")

;;; --- gdk-pixbuf-width -------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "width" 'gdk-pixbuf) 't)
 "The @code{width} property of type @code{:int} (Read / Write / Construct Only)
  @br{}
  The number of columns of the pixbuf. @br{}
  Allowed values: >= 1 @br{}
  Default value: 1")

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-pixbuf-width atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-pixbuf-width 'function)
 "@version{2020-11-21}
  @syntax[]{(gdk-pixbuf-width object) => width}
  @argument[object]{a @class{gdk-pixbuf} object}
  @argument[width]{an integer with the width of the pixbuf}
  @begin{short}
    Accessor of the @slot[gdk-pixbuf]{width} slot of the @class{gdk-pixbuf}
    class.
  @end{short}

  Queries the width of a pixbuf.
  @see-class{gdk-pixbuf}")

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_get_pixels_with_length () -> gdk-pixbuf-pixels-with-length
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_pixbuf_get_pixels_with_length" gdk-pixbuf-pixels-with-length)
    (:pointer :uchar)
 #+cl-cffi-gtk-documentation
 "@version{2020-11-21}
  @argument[pixbuf]{a @class{gdk-pixbuf} object}
  @argument[length]{an unsigned integer with the length of the binary data}
  @return{A pointer to the pixbuf's pixel data.}
  @begin{short}
    Queries a pointer to the pixel data of a pixbuf.
  @end{short}

  Please see the section on image data in the @class{gdk-pixbuf} documentation
  for information about how the pixel data is stored in memory. This function
  will cause an implicit copy of the pixbuf data if the pixbuf was created from
  read-only data.
  @see-class{gdk-pixbuf}
  @see-function{gdk-pixbuf-pixels}"
  (pixbuf (g-object gdk-pixbuf))
  (length :uint))

(export 'gdk-pixbuf-pixels-with-length)

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_get_byte_length () -> gdk-pixbuf-byte-length
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_pixbuf_get_byte_length" gdk-pixbuf-byte-length) g-size
 #+cl-cffi-gtk-documentation
 "@version{2020-11-21}
  @argument[pixbuf]{a @class{gdk-pixbuf} object}
  @return{An integer with the length of the pixel data.}
  @short{Returns the length of the pixel data, in bytes.}
  @see-class{gdk-pixbuf}"
  (pixbuf (g-object gdk-pixbuf)))

(export 'gdk-pixbuf-byte-length)

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_get_option ()
;;; gdk_pixbuf_set_option () -> gdk-pixbuf-option
;;; ----------------------------------------------------------------------------

(defun (setf gdk-pixbuf-option) (value pixbuf key)
  (when (foreign-funcall "gdk_pixbuf_set_option"
                         (g-object gdk-pixbuf) pixbuf
                         :string key
                         :string value
                         :boolean)
    value))

(defcfun ("gdk_pixbuf_get_option" gdk-pixbuf-option) :string
 #+cl-cffi-gtk-documentation
 "@version{2020-11-21}
  @syntax[]{(gdk-pixbuf-option pixbuf key) => value}
  @syntax[]{(setf (gdk-pixbuf-option pixbuf key) value)}
  @argument[pixbuf]{a @class{gdk-pixbuf} object}
  @argument[key]{a string with a key}
  @argument[value]{a string with a value}
  @begin{short}
    Accessor of an option that may have been attached to the pixbuf.
  @end{short}

  The function @sym{gdk-pixbuf-option} looks up @arg{key} in the list of
  options that may have been attached to the pixbuf when it was loaded, or that
  may have been attached. The function @sym{(setf gdk-pixbuf-option)} attaches
  a key/value pair as an option to a pixbuf. If @arg{key} already exists in the
  list of options attached to pixbuf, the new value is ignored.

  For instance, the ANI loader provides \"Title\" and \"Artist\" options. The
  ICO, XBM, and XPM loaders provide \"x_hot\" and \"y_hot\" hot-spot options
  for cursor definitions. The PNG loader provides the tEXt ancillary chunk
  key/value pairs as options. Since 2.12, the TIFF and JPEG loaders return an
  \"orientation\" option string that corresponds to the embedded TIFF/Exif
  orientation tag (if present). Since 2.32, the TIFF loader sets the
  \"multipage\" option string to \"yes\" when a multi-page TIFF is loaded.
  Since 2.32 the JPEG and PNG loaders set \"x-dpi\" and \"y-dpi\" if the file
  contains image density information in dots per inch.
  @see-class{gdk-pixbuf}"
  (pixbuf (g-object gdk-pixbuf))
  (key :string))

(export 'gdk-pixbuf-option)

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_remove_option ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_pixbuf_remove_option" gdk-pixbuf-remove-option) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2020-11-21}
  @argument[pixbuf]{a @class{gdk-pixbuf} oject}
  @argument[key]{a string representing the key to remove}
  @return{@em{True} if an option was removed, @em{false} if not.}
  @begin{short}
    Remove the key/value pair option attached to a pixbuf.
  @end{short}

  Since 2.36
  @see-class{gdk-pixbuf}
  @see-function{gdk-pixbuf-option}"
  (pixbuf (g-object gdk-pixbuf))
  (key :string))

(export 'gdk-pixbuf-remove-option)

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_get_options ()
;;;
;;; GHashTable *
;;; gdk_pixbuf_get_options (GdkPixbuf *pixbuf);
;;;
;;; Returns a GHashTable with a list of all the options that may have been
;;; attached to the pixbuf when it was loaded, or that may have been attached
;;; by another function using gdk_pixbuf_set_option().
;;;
;;; See gdk_pixbuf_get_option() for more details.
;;;
;;; pixbuf :
;;;     a GdkPixbuf
;;;
;;; Returns :
;;;     a GHashTable of key/values.
;;;
;;; Since 2.32
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_copy_options ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_pixbuf_copy_options" gdk-pixbuf-copy-options) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2020-11-21}
  @argument[src-pixbuf]{a @class{gdk-pixbuf} to copy options from}
  @argument[dest-pixbuf]{a @class{gdk-pixbuf} to copy options to}
  @return{@em{True} on sucess.}
  @begin{short}
    Copy the key/value pair options attached to a pixbuf to another.
  @end{short}
  This is useful to keep original metadata after having manipulated a file.
  However be careful to remove metadata which you have already applied, such as
  the \"orientation\" option after rotating the image.

  Since 2.36
  @see-class{gdk-pixbuf}
  @see-function{gdk-pixbuf-option}"
  (src-pixbuf (g-object gdk-pixbuf))
  (dest-pixbuf (g-object gdk-pixbuf)))

(export 'gdk-pixbuf-copy-options)

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_read_pixels ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_pixbuf_read_pixels" gdk-pixbuf-read-pixels) (:pointer :uint8)
 #+cl-cffi-gtk-documentation
 "@version{2020-11-21}
  @argument[pixbuf]{a @class{gdk-pixbuf} object}
  @return{A pointer to the pixel data.}
  @begin{short}
    Returns a read-only pointer to the raw pixel data.
  @end{short}
  Must not be modified. This function allows skipping the implicit copy that
  must be made if the function @fun{gdk-pixbuf-pixels} is called on a read-only
  pixbuf.

  Since 2.32
  @see-class{gdk-pixbuf}
  @see-function{gdk-pixbuf-pixels}"
  (pixbuf (g-object gdk-pixbuf)))

(export 'gdk-pixbuf-read-pixels)

;;; --- End of file gdk-pixbuf.structure.lisp ----------------------------------
