;;; ----------------------------------------------------------------------------
;;; gdk-pixbuf.memory.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See <http://common-lisp.net/project/cl-gtk2/>.
;;;
;;; The documentation of this file is taken from the GDK-PixBuf Reference Manual
;;; Version 2.28.1 and modified to document the Lisp binding to the GDK-PixBuf
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
;;; Image Data in Memory
;;;
;;; Creating a pixbuf from image data that is already in memory.
;;;
;;; Synopsis
;;;
;;;     gdk_pixbuf_new
;;;     gdk_pixbuf_new_from_data
;;;     gdk_pixbuf_new_from_xpm_data
;;;     gdk_pixbuf_new_from_inline
;;;     gdk_pixbuf_new_subpixbuf
;;;     gdk_pixbuf_copy
;;;
;;; Description
;;;
;;; The most basic way to create a pixbuf is to wrap an existing pixel buffer
;;; with a GdkPixbuf structure. You can use the gdk_pixbuf_new_from_data()
;;; function to do this. You need to specify the destroy notification function
;;; that will be called when the data buffer needs to be freed; this will happen
;;; when a GdkPixbuf is finalized by the reference counting functions If you
;;; have a chunk of static data compiled into your application, you can pass in
;;; NULL as the destroy notification function so that the data will not be
;;; freed.
;;;
;;; The gdk_pixbuf_new() function can be used as a convenience to create a
;;; pixbuf with an empty buffer. This is equivalent to allocating a data buffer
;;; using malloc() and then wrapping it with gdk_pixbuf_new_from_data(). The
;;; gdk_pixbuf_new() function will compute an optimal rowstride so that
;;; rendering can be performed with an efficient algorithm.
;;;
;;; As a special case, you can use the gdk_pixbuf_new_from_xpm_data() function
;;; to create a pixbuf from inline XPM image data.
;;;
;;; You can also copy an existing pixbuf with the gdk_pixbuf_copy() function.
;;; This is not the same as just doing a g_object_ref() on the old pixbuf; the
;;; copy function will actually duplicate the pixel data in memory and create a
;;; new GdkPixbuf structure for it.
;;; ----------------------------------------------------------------------------

(in-package :gdk-pixbuf)

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_new ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_pixbuf_new" gdk-pixbuf-new) (g-object gdk-pixbuf)
 #+cl-cffi-gtk-documentation
 "@version{2013-6-22}
  @argument[colorspace]{color space for image}
  @argument[has-alpha]{whether the image should have transparency information}
  @argument[bits-per-sample]{number of bits per color sample}
  @argument[width]{width of image in pixels, must be > 0}
  @argument[height]{height of image in pixels, must be > 0}
  @begin{return}
    A newly-created @class{gdk-pixbuf} with a reference count of 1, or
    @code{nil} if not enough memory could be allocated for the image buffer.
  @end{return}
  Creates a new @class{gdk-pixbuf} structure and allocates a buffer for it. The
  buffer has an optimal rowstride. Note that the buffer is not cleared; you will
  have to fill it completely yourself."
  (colorspace gdk-colorspace)
  (has-alpha :boolean)
  (bits-per-sample :int)
  (width :int)
  (height :int))

(export 'gdk-pixbuf-new)

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_new_from_data ()
;;;
;;; GdkPixbuf * gdk_pixbuf_new_from_data (const guchar *data,
;;;                                       GdkColorspace colorspace,
;;;                                       gboolean has_alpha,
;;;                                       int bits_per_sample,
;;;                                       int width,
;;;                                       int height,
;;;                                       int rowstride,
;;;                                       GdkPixbufDestroyNotify destroy_fn,
;;;                                       gpointer destroy_fn_data);
;;;
;;; Creates a new GdkPixbuf out of in-memory image data. Currently only RGB
;;; images with 8 bits per sample are supported.
;;;
;;; data :
;;;     Image data in 8-bit/sample packed format.
;;;
;;; colorspace :
;;;     Colorspace for the image data
;;;
;;; has_alpha :
;;;     Whether the data has an opacity channel
;;;
;;; bits_per_sample :
;;;     Number of bits per sample
;;;
;;; width :
;;;     Width of the image in pixels, must be > 0
;;;
;;; height :
;;;     Height of the image in pixels, must be > 0
;;;
;;; rowstride :
;;;     Distance in bytes between row starts
;;;
;;; destroy_fn :
;;;     Function used to free the data when the pixbuf's reference count drops
;;;     to zero, or NULL if the data should not be freed
;;;
;;; destroy_fn_data :
;;;     Closure data to pass to the destroy notification function
;;;
;;; Returns :
;;;     A newly-created GdkPixbuf structure with a reference count of 1.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_new_from_xpm_data ()
;;;
;;; GdkPixbuf * gdk_pixbuf_new_from_xpm_data (const char **data);
;;;
;;; Creates a new pixbuf by parsing XPM data in memory. This data is commonly
;;; the result of including an XPM file into a program's C source.
;;;
;;; data :
;;;     pointer to inline XPM data
;;;
;;; Returns :
;;;     A newly-created pixbuf with a reference count of 1.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_new_from_inline ()
;;;
;;; GdkPixbuf * gdk_pixbuf_new_from_inline (gint data_length,
;;;                                         const guint8 *data,
;;;                                         gboolean copy_pixels,
;;;                                         GError **error);
;;;
;;; Create a GdkPixbuf from a flat representation that is suitable for storing
;;; as inline data in a program. This is useful if you want to ship a program
;;; with images, but don't want to depend on any external files.
;;;
;;; gdk-pixbuf ships with a program called gdk-pixbuf-csource which allows for
;;; conversion of GdkPixbufs into such a inline representation. In almost all
;;; cases, you should pass the --raw flag to gdk-pixbuf-csource. A sample
;;; invocation would be:
;;;
;;;     gdk-pixbuf-csource --raw --name=myimage_inline myimage.png
;;;
;;; For the typical case where the inline pixbuf is read-only static data, you
;;; don't need to copy the pixel data unless you intend to write to it, so you
;;; can pass FALSE for copy_pixels. (If you pass --rle to gdk-pixbuf-csource, a
;;; copy will be made even if copy_pixels is FALSE, so using this option is
;;; generally a bad idea.)
;;;
;;; If you create a pixbuf from const inline data compiled into your program,
;;; it's probably safe to ignore errors and disable length checks, since things
;;; will always succeed:
;;;
;;;     pixbuf = gdk_pixbuf_new_from_inline (-1, myimage_inline, FALSE, NULL);
;;;
;;; For non-const inline data, you could get out of memory. For untrusted inline
;;; data located at runtime, you could have corrupt inline data in addition.
;;;
;;; data_length :
;;;     Length in bytes of the data argument or -1 to disable length checks
;;;
;;; data :
;;;     Byte data containing a serialized GdkPixdata structure
;;;
;;; copy_pixels :
;;;     Whether to copy the pixel data, or use direct pointers data for the
;;;     resulting pixbuf
;;;
;;; error :
;;;     GError return location, may be NULL to ignore errors
;;;
;;; Returns :
;;;     A newly-created GdkPixbuf structure with a reference, count of 1, or
;;;     NULL if an error occurred.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_new_subpixbuf ()
;;;
;;; GdkPixbuf * gdk_pixbuf_new_subpixbuf (GdkPixbuf *src_pixbuf,
;;;                                       int src_x,
;;;                                       int src_y,
;;;                                       int width,
;;;                                       int height);
;;;
;;; Creates a new pixbuf which represents a sub-region of src_pixbuf. The new
;;; pixbuf shares its pixels with the original pixbuf, so writing to one affects
;;; both. The new pixbuf holds a reference to src_pixbuf, so src_pixbuf will not
;;; be finalized until the new pixbuf is finalized.
;;;
;;; src_pixbuf :
;;;     a GdkPixbuf
;;;
;;; src_x :
;;;     X coord in src_pixbuf
;;;
;;; src_y :
;;;     Y coord in src_pixbuf
;;;
;;; width :
;;;     width of region in src_pixbuf
;;;
;;; height :
;;;     height of region in src_pixbuf
;;;
;;; Returns :
;;;     a new pixbuf
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_copy ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_pixbuf_copy" gdk-pixbuf-copy) (g-object gdk-pixbuf)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-21}
  @argument[pixbuf]{a @class{gdk-pixbuf} object}
  @begin{return}
    A newly created pixbuf with a reference count of 1, or @code{nil} if not
    enough memory could be allocated.
  @end{return}
  Creates a new @class{gdk-pixbuf} object with a copy of the information in the
  specified @arg{pixbuf}."
  (pixbuf (g-object gdk-pixbuf)))

(export 'gdk-pixbuf-copy)

;;; --- End of file gdk-pixbuf.memory.lisp -------------------------------------
