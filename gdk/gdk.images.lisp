;;; ----------------------------------------------------------------------------
;;; gdk.images.lisp
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
;;; Images
;;; 
;;; A client-side area for bit-mapped graphics
;;; 
;;; Synopsis
;;;
;;;     GdkImage
;;;     GdkImageType
;;;
;;;     gdk_image_new
;;;     gdk_image_new_bitmap
;;;     gdk_image_get
;;;     gdk_image_ref
;;;     gdk_image_unref
;;;     gdk_image_destroy
;;;     gdk_image_get_colormap
;;;     gdk_image_set_colormap
;;;     gdk_image_get_bits_per_pixel
;;;     gdk_image_get_bytes_per_pixel
;;;     gdk_image_get_bytes_per_line
;;;     gdk_image_get_byte_order
;;;     gdk_image_get_depth
;;;     gdk_image_get_height
;;;     gdk_image_get_image_type
;;;     gdk_image_get_visual
;;;     gdk_image_get_width
;;;     gdk_image_get_pixels
;;;     gdk_image_put_pixel
;;;     gdk_image_get_pixel
;;; 
;;; Description
;;; 
;;; The GdkImage type represents an area for drawing graphics. It has now been
;;; superceded to a large extent by the much more flexible GdkRGB functions.
;;; 
;;; To create an empty GdkImage use gdk_image_new(). To create a GdkImage from
;;; bitmap data use gdk_image_new_bitmap(). To create an image from part of a
;;; GdkWindow use gdk_drawable_get_image().
;;; 
;;; The image can be manipulated with gdk_image_get_pixel() and
;;; gdk_image_put_pixel(), or alternatively by changing the actual pixel data.
;;; Though manipulating the pixel data requires complicated code to cope with
;;; the different formats that may be used.
;;; 
;;; To draw a GdkImage in a GdkWindow or GdkPixmap use gdk_draw_image().
;;; 
;;; To destroy a GdkImage use gdk_image_destroy().
;;; ----------------------------------------------------------------------------

(in-package :gdk)

;;; ----------------------------------------------------------------------------
;;; enum GdkImageType
;;; 
;;; typedef enum
;;; {
;;;   GDK_IMAGE_NORMAL,
;;;   GDK_IMAGE_SHARED,
;;;   GDK_IMAGE_FASTEST
;;; } GdkImageType;
;;; 
;;; Specifies the type of a GdkImage.
;;; 
;;; GDK_IMAGE_NORMAL
;;;     The original X image type, which is quite slow since the image has to
;;;     be transferred from the client to the server to display it.
;;; 
;;; GDK_IMAGE_SHARED
;;;     A faster image type, which uses shared memory to transfer the image data
;;;     between client and server. However this will only be available if client
;;;     and server are on the same machine and the shared memory extension is
;;;     supported by the server.
;;; 
;;; GDK_IMAGE_FASTEST
;;;     Specifies that GDK_IMAGE_SHARED should be tried first, and if that fails
;;;     then GDK_IMAGE_NORMAL will be used.
;;; ----------------------------------------------------------------------------

(define-g-enum "GdkImageType"
    gdk-image-type
    (:export t :type-initializer "gdk_image_type_get_type")
  (:normal 0)
  (:shared 1)
  (:fastest 2))

;;; ----------------------------------------------------------------------------
;;; GdkImage
;;; 
;;; typedef struct {
;;;   GObject parent_instance;
;;; 
;;;   
;;;   GdkImageType GSEAL (type);       /* read only. */
;;;   GdkVisual    *GSEAL (visual);    /* read only visual to create the image*/
;;;   GdkByteOrder GSEAL (byte_order); /* read only. */
;;;   gint     GSEAL (width);          /* read only. */
;;;   gint     GSEAL (height);         /* read only. */
;;;   guint16  GSEAL (depth);          /* read only. */
;;;   guint16  GSEAL (bpp);            /* read only. bytes per pixel */
;;;   guint16  GSEAL (bpl);            /* read only. bytes per line */
;;;   guint16  GSEAL (bits_per_pixel); /* read only. bits per pixel */
;;;   gpointer GSEAL (mem);
;;; 
;;;   GdkColormap  *GSEAL (colormap);  /* read only. */
;;; } GdkImage;
;;; 
;;; The GdkImage struct contains information on the image and the pixel data.
;;; 
;;; GObject parent_instance;
;;;     the parent instance
;;; ----------------------------------------------------------------------------

(define-g-object-class "GdkImage" gdk-image
  (:superclass g-object
   :export t
   :interfaces nil
   :type-initializer "gdk_image_get_type")
  ((:cffi colormap
          gdk-image-colormap (g-object gdk-colormap)
     "gdk_image_get_colormap" "gdk_image_set_colormap")))

;;; ----------------------------------------------------------------------------
;;; gdk_image_new ()
;;; 
;;; GdkImage * gdk_image_new (GdkImageType type,
;;;                           GdkVisual *visual,
;;;                           gint width,
;;;                           gint height);
;;; 
;;; Warning
;;; 
;;; gdk_image_new is deprecated and should not be used in newly-written code.
;;; 
;;; Creates a new GdkImage.
;;; 
;;; type :
;;;     the type of the GdkImage, one of GDK_IMAGE_NORMAL, GDK_IMAGE_SHARED and
;;;     GDK_IMAGE_FASTEST. GDK_IMAGE_FASTEST is probably the best choice, since
;;;     it will try creating a GDK_IMAGE_SHARED image first and if that fails
;;;     it will then use GDK_IMAGE_NORMAL.
;;; 
;;; visual :
;;;     the GdkVisual to use for the image.
;;; 
;;; width :
;;;     the width of the image in pixels.
;;; 
;;; height :
;;;     the height of the image in pixels.
;;; 
;;; Returns :
;;;     a new GdkImage, or NULL if the image could not be created.
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_image_new" gdk-image-new)
    (g-object gdk-image :already-referenced)
  (type gdk-image-type)
  (visual (g-object gdk-visual))
  (width :int)
  (height :int))

(export 'gdk-image-new)

;;; ----------------------------------------------------------------------------
;;; gdk_image_new_bitmap ()
;;; 
;;; GdkImage * gdk_image_new_bitmap (GdkVisual *visual,
;;;                                  gpointer data,
;;;                                  gint width,
;;;                                  gint height);
;;; 
;;; Warning
;;; 
;;; gdk_image_new_bitmap is deprecated and should not be used in newly-written
;;; code.
;;; 
;;; Creates a new GdkImage with a depth of 1 from the given data.
;;; Warning
;;; 
;;; THIS FUNCTION IS INCREDIBLY BROKEN. The passed-in data must be allocated by
;;; malloc() (NOT g_malloc()) and will be freed when the image is freed.
;;; 
;;; visual :
;;;     the GdkVisual to use for the image.
;;; 
;;; data :
;;;     the pixel data.
;;; 
;;; width :
;;;     the width of the image in pixels.
;;; 
;;; height :
;;;     the height of the image in pixels.
;;; 
;;; Returns :
;;;     a new GdkImage.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_image_get ()
;;; 
;;; GdkImage * gdk_image_get (GdkDrawable *drawable,
;;;                           gint x,
;;;                           gint y,
;;;                           gint width,
;;;                           gint height);
;;; 
;;; Warning
;;; 
;;; gdk_image_get is deprecated and should not be used in newly-written code.
;;; 
;;; This is a deprecated wrapper for gdk_drawable_get_image();
;;; gdk_drawable_get_image() should be used instead. Or even better: in most
;;; cases gdk_pixbuf_get_from_drawable() is the most convenient choice.
;;; 
;;; drawable :
;;;     a GdkDrawable
;;; 
;;; x :
;;;     x coordinate in window
;;; 
;;; y :
;;;     y coordinate in window
;;; 
;;; width :
;;;     width of area in window
;;; 
;;; height :
;;;     height of area in window
;;; 
;;; Returns :
;;;     a new GdkImage or NULL
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_image_ref ()
;;; 
;;; GdkImage * gdk_image_ref (GdkImage *image);
;;; 
;;; Warning
;;; 
;;; gdk_image_ref has been deprecated since version 2.0 and should not be used
;;; in newly-written code. Use g_object_ref() instead.
;;; 
;;; Deprecated function; use g_object_ref() instead.
;;; 
;;; image :
;;;     a GdkImage
;;; 
;;; Returns :
;;;     the image
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_image_unref ()
;;; 
;;; void gdk_image_unref (GdkImage *image);
;;; 
;;; Warning
;;; 
;;; gdk_image_unref has been deprecated since version 2.0 and should not be
;;; used in newly-written code. Use g_object_unref() instead.
;;; 
;;; Deprecated function; use g_object_unref() instead.
;;; 
;;; image :
;;;     a GdkImage
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_image_destroy
;;; 
;;; #define gdk_image_destroy g_object_unref
;;; 
;;; Warning
;;; 
;;; gdk_image_destroy is deprecated and should not be used in newly-written
;;; code.
;;; 
;;; Destroys a GdkImage, freeing any resources allocated for it.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_image_get_colormap ()
;;; 
;;; GdkColormap * gdk_image_get_colormap (GdkImage *image);
;;; 
;;; Warning
;;; 
;;; gdk_image_get_colormap has been deprecated since version 2.22 and should
;;; not be used in newly-written code. GdkImage should not be used anymore.
;;; 
;;; Retrieves the colormap for a given image, if it exists. An image will have
;;; a colormap if the drawable from which it was created has a colormap, or if
;;; a colormap was set explicitely with gdk_image_set_colormap().
;;; 
;;; image :
;;;     a GdkImage
;;; 
;;; Returns :
;;;     colormap for the image
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_image_set_colormap ()
;;; 
;;; void gdk_image_set_colormap (GdkImage *image, GdkColormap *colormap);
;;; 
;;; Warning
;;; 
;;; gdk_image_set_colormap has been deprecated since version 2.22 and should
;;; not be used in newly-written code. GdkImage should not be used anymore.
;;; 
;;; Sets the colormap for the image to the given colormap. Normally there's no
;;; need to use this function, images are created with the correct colormap if
;;; you get the image from a drawable. If you create the image from scratch,
;;; use the colormap of the drawable you intend to render the image to.
;;; 
;;; image :
;;;     a GdkImage
;;; 
;;; colormap :
;;;     a GdkColormap
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_image_get_bits_per_pixel ()
;;; 
;;; guint16 gdk_image_get_bits_per_pixel (GdkImage *image);
;;; 
;;; Warning
;;; 
;;; gdk_image_get_bits_per_pixel has been deprecated since version 2.22 and
;;; should not be used in newly-written code. GdkImage should not be used
;;; anymore.
;;; 
;;; Determines the number of bits per pixel of the image.
;;; 
;;; image :
;;;     a GdkImage
;;; 
;;; Returns :
;;;     the bits per pixel
;;; 
;;; Since 2.22
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_image_get_bytes_per_pixel ()
;;; 
;;; guint16 gdk_image_get_bytes_per_pixel (GdkImage *image);
;;; 
;;; Warning
;;; 
;;; gdk_image_get_bytes_per_pixel has been deprecated since version 2.22 and
;;; should not be used in newly-written code. GdkImage should not be used
;;; anymore.
;;; 
;;; Determines the number of bytes per pixel of the image.
;;; 
;;; image :
;;;     a GdkImage
;;; 
;;; Returns :
;;;     the bytes per pixel
;;; 
;;; Since 2.22
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_image_get_bytes_per_line ()
;;; 
;;; guint16 gdk_image_get_bytes_per_line (GdkImage *image);
;;; 
;;; Warning
;;; 
;;; gdk_image_get_bytes_per_line has been deprecated since version 2.22 and
;;; should not be used in newly-written code. GdkImage should not be used
;;; anymore.
;;; 
;;; Determines the number of bytes per line of the image.
;;; 
;;; image :
;;;     a GdkImage
;;; 
;;; Returns :
;;;     the bytes per line
;;; 
;;; Since 2.22
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_image_get_byte_order ()
;;; 
;;; GdkByteOrder gdk_image_get_byte_order (GdkImage *image);
;;; 
;;; Warning
;;; 
;;; gdk_image_get_byte_order has been deprecated since version 2.22 and should
;;; not be used in newly-written code. GdkImage should not be used anymore.
;;; 
;;; Determines the byte order of the image.
;;; 
;;; image :
;;;     a GdkImage
;;; 
;;; Returns :
;;;     a GdkVisual
;;; 
;;; Since 2.22
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_image_get_depth ()
;;; 
;;; guint16 gdk_image_get_depth (GdkImage *image);
;;; 
;;; Warning
;;; 
;;; gdk_image_get_depth has been deprecated since version 2.22 and should not
;;; be used in newly-written code. GdkImage should not be used anymore.
;;; 
;;; Determines the depth of the image.
;;; 
;;; image :
;;;     a GdkImage
;;; 
;;; Returns :
;;;     the depth
;;; 
;;; Since 2.22
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_image_get_height ()
;;; 
;;; gint gdk_image_get_height (GdkImage *image);
;;; 
;;; Warning
;;; 
;;; gdk_image_get_height has been deprecated since version 2.22 and should not
;;; be used in newly-written code. GdkImage should not be used anymore.
;;; 
;;; Determines the height of the image.
;;; 
;;; image :
;;;     a GdkImage
;;; 
;;; Returns :
;;;     the height
;;; 
;;; Since 2.22
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_image_get_image_type ()
;;; 
;;; GdkImageType gdk_image_get_image_type (GdkImage *image);
;;; 
;;; Warning
;;; 
;;; gdk_image_get_image_type has been deprecated since version 2.22 and should
;;; not be used in newly-written code. GdkImage should not be used anymore.
;;; 
;;; Determines the type of a given image.
;;; 
;;; image :
;;;     a GdkImage
;;; 
;;; Returns :
;;;     the GdkImageType of the image
;;; 
;;; Since 2.22
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_image_get_visual ()
;;; 
;;; GdkVisual * gdk_image_get_visual (GdkImage *image);
;;; 
;;; Warning
;;; 
;;; gdk_image_get_visual has been deprecated since version 2.22 and should not
;;; be used in newly-written code. GdkImage should not be used anymore.
;;; 
;;; Determines the visual that was used to create the image.
;;; 
;;; image :
;;;     a GdkImage
;;; 
;;; Returns :
;;;     a GdkVisual
;;; 
;;; Since 2.22
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_image_get_width ()
;;; 
;;; gint gdk_image_get_width (GdkImage *image);
;;; 
;;; Warning
;;; 
;;; gdk_image_get_width has been deprecated since version 2.22 and should not
;;; be used in newly-written code. GdkImage should not be used anymore.
;;; 
;;; Determines the width of the image.
;;; 
;;; image :
;;;     a GdkImage
;;; 
;;; Returns :
;;;     the width
;;; 
;;; Since 2.22
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_image_get_pixels ()
;;; 
;;; gpointer gdk_image_get_pixels (GdkImage *image);
;;; 
;;; Warning
;;; 
;;; gdk_image_get_pixels has been deprecated since version 2.22 and should not
;;; be used in newly-written code. GdkImage should not be used anymore.
;;; 
;;; Returns a pointer to the pixel data of the image.
;;; 
;;; image :
;;;     a GdkImage
;;; 
;;; Returns :
;;;     the pixel data of the image
;;; 
;;; Since 2.22
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_image_get_pixel" gdk-image-get-pixel) :uint32
  (image (g-object gdk-image))
  (x :int)
  (y :int))

(export 'gdk-image-get-pixel)

;;; ----------------------------------------------------------------------------
;;; gdk_image_put_pixel ()
;;; 
;;; void gdk_image_put_pixel (GdkImage *image, gint x, gint y, guint32 pixel);
;;; 
;;; Warning
;;; 
;;; gdk_image_put_pixel is deprecated and should not be used in newly-written
;;; code.
;;; 
;;; Sets a pixel in a GdkImage to a given pixel value.
;;; 
;;; image :
;;;     a GdkImage.
;;; 
;;; x :
;;;     the x coordinate of the pixel to set.
;;; 
;;; y :
;;;     the y coordinate of the pixel to set.
;;; 
;;; pixel :
;;;     the pixel value to set.
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_image_put_pixel" gdk-image-put-pixel) :void
  (image (g-object gdk-image))
  (x :int)
  (y :int)
  (pixel :uint32))

(export 'gdk-image-put-pixel)

;;; ----------------------------------------------------------------------------
;;; gdk_image_get_pixel ()
;;; 
;;; guint32 gdk_image_get_pixel (GdkImage *image, gint x, gint y);
;;; 
;;; Warning
;;; 
;;; gdk_image_get_pixel is deprecated and should not be used in newly-written
;;; code.
;;; 
;;; Gets a pixel value at a specified position in a GdkImage.
;;; 
;;; image :
;;;     a GdkImage.
;;; 
;;; x :
;;;     the x coordinate of the pixel to get.
;;; 
;;; y :
;;;     the y coordinate of the pixel to get.
;;; 
;;; Returns :
;;;     the pixel value at the given position.
;;; ----------------------------------------------------------------------------

;;; --- End of file gdk.image.lisp ---------------------------------------------

