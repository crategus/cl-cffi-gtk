;;; ----------------------------------------------------------------------------
;;; gdk.pixmap.lisp
;;; 
;;; This file contains code from a fork of cl-gtk2.
;;; See http://common-lisp.net/project/cl-gtk2/
;;; 
;;; The documentation has been copied from the GDK 2 Reference Manual
;;; Version 2.24.10. See http://www.gtk.org.
;;; 
;;; Copyright (C) 2009, 2011 Kalyanov Dmitry
;;; Copyright (C) 2011, 2012 Dr. Dieter Kaiser
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
;;; Bitmaps and Pixmaps
;;; 
;;; Offscreen drawables
;;; 
;;; Synopsis
;;; 
;;;     GdkPixmap
;;;     
;;;     gdk_pixmap_new
;;;     gdk_pixmap_create_from_data
;;;     gdk_pixmap_create_from_xpm
;;;     gdk_pixmap_colormap_create_from_xpm
;;;     gdk_pixmap_create_from_xpm_d
;;;     gdk_pixmap_colormap_create_from_xpm_d
;;;     gdk_pixmap_get_size
;;;     
;;;     GdkBitmap
;;;     
;;;     gdk_bitmap_create_from_data
;;; 
;;; Object Hierarchy
;;; 
;;;   GObject
;;;    +----GdkDrawable
;;;          +----GdkPixmap
;;; 
;;; Description
;;; 
;;; Pixmaps are offscreen drawables. They can be drawn upon with the standard
;;; drawing primitives, then copied to another drawable (such as a GdkWindow)
;;; with gdk_pixmap_draw(). The depth of a pixmap is the number of bits per
;;; pixels. Bitmaps are simply pixmaps with a depth of 1. (That is, they are
;;; monochrome bitmaps - each pixel can be either on or off).
;;; 
;;; GTK 3 will remove GdkPixmap and GdkBitmap. You should use cairo surfaces
;;; instead. However, because a lot of functions still use these types, they
;;; are not deprecated.
;;; ----------------------------------------------------------------------------

(in-package :gdk)

;;; ----------------------------------------------------------------------------
;;; struct GdkPixmap
;;; 
;;; struct GdkPixmap;
;;; 
;;; An opaque structure representing an offscreen drawable. Pointers to
;;; structures of type GdkPixmap, GdkBitmap, and GdkWindow, can often be used
;;; interchangeably. The type GdkDrawable refers generically to any of these
;;; types.
;;; ----------------------------------------------------------------------------

(define-g-object-class "GdkPixmap" gdk-pixmap
  (:superclass gdk-drawable
   :type-initializer "gdk_pixmap_get_type")
  nil)

;;; ----------------------------------------------------------------------------
;;; gdk_pixmap_new ()
;;; 
;;; GdkPixmap * gdk_pixmap_new (GdkDrawable *drawable,
;;;                             gint width,
;;;                             gint height,
;;;                             gint depth);
;;; 
;;; Create a new pixmap with a given size and depth.
;;; 
;;; Please use gdk_window_create_similar_surface() instead of this function if
;;; you're not forced to use a GdkPixmap. It is intended as the replacement
;;; function for this function in GTK 3.
;;; 
;;; drawable :
;;;     A GdkDrawable, used to determine default values for the new pixmap.
;;;     Can be NULL if depth is specified.
;;; 
;;; width :
;;;     The width of the new pixmap in pixels.
;;; 
;;; height :
;;;     The height of the new pixmap in pixels.
;;; 
;;; depth :
;;;     The depth (number of bits per pixel) of the new pixmap. If -1, and
;;;     drawable is not NULL, the depth of the new pixmap will be equal to
;;;     that of drawable.
;;; 
;;; Returns :
;;;     the GdkPixmap
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_pixmap_new" gdk-pixmap-new)
    (g-object gdk-pixmap :already-referenced)
  (drawable (g-object gdk-drawable))
  (width :int)
  (height :int)
  (depth :int))

(export 'gdk-pixmap-new)

;;; ----------------------------------------------------------------------------
;;; gdk_pixmap_create_from_data ()
;;; 
;;; GdkPixmap * gdk_pixmap_create_from_data (GdkDrawable *drawable,
;;;                                          const gchar *data,
;;;                                          gint width,
;;;                                          gint height,
;;;                                          gint depth,
;;;                                          const GdkColor *fg,
;;;                                          const GdkColor *bg);
;;; 
;;; Warning
;;; 
;;; gdk_pixmap_create_from_data has been deprecated since version 2.22 and
;;; should not be used in newly-written code. If you must replicate the
;;; functionality of this function, create a pixmap using gdk_pixmap_new(),
;;; cairo_paint() it with the background color and then create a Cairo image
;;; surface as pointed out in the docs to gdk_bitmap_create_from_data() and
;;; use this surface with cairo_mask_surface() to paint the foreground color.
;;; 
;;; Create a two-color pixmap from data in XBM data.
;;; 
;;; drawable :
;;;     a GdkDrawable, used to determine default values for the new pixmap.
;;;     Can be NULL, if the depth is given.
;;; 
;;; data :
;;;     a pointer to the data
;;; 
;;; width :
;;;     the width of the new pixmap in pixels
;;; 
;;; height :
;;;     the height of the new pixmap in pixels
;;; 
;;; depth :
;;;     the depth (number of bits per pixel) of the new pixmap
;;; 
;;; fg :
;;;     the foreground color
;;; 
;;; bg :
;;;     the background color
;;; 
;;; Returns :
;;;     the GdkPixmap
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_pixmap_create_from_data" gdk-pixmap-create-from-data)
    (g-object gdk-pixmap :already-referenced)
  (drawable (g-object gdk-drawable))
  (data :pointer)
  (width :int)
  (height :int)
  (depth :int)
  (fg (g-boxed-foreign gdk-color))
  (bg (g-boxed-foreign gdk-color)))

(export 'gdk-pixmap-create-from-data)

;;; ----------------------------------------------------------------------------
;;; gdk_pixmap_create_from_xpm ()
;;; 
;;; GdkPixmap * gdk_pixmap_create_from_xpm (GdkDrawable *drawable,
;;;                                         GdkBitmap **mask,
;;;                                         const GdkColor *transparent_color,
;;;                                         const gchar *filename);
;;; 
;;; Warning
;;; 
;;; gdk_pixmap_create_from_xpm has been deprecated since version 2.22 and
;;; should not be used in newly-written code. Use a GdkPixbuf instead. You can
;;; use gdk_pixbuf_new_from_file() to create it. If you must use a pixmap, use
;;; gdk_pixmap_new() to create it and Cairo to draw the pixbuf onto it.
;;; 
;;; Create a pixmap from a XPM file.
;;; 
;;; drawable :
;;;     a GdkDrawable, used to determine default values for the new pixmap
;;; 
;;; mask :
;;;     (out) a pointer to a place to store a bitmap representing the
;;;     transparency mask of the XPM file. Can be NULL, in which case
;;;     transparency will be ignored.
;;; 
;;; transparent_color :
;;;     the color to be used for the pixels that are transparent in the input
;;;     file. Can be NULL, in which case a default color will be used.
;;; 
;;; filename :
;;;     the filename of a file containing XPM data
;;; 
;;; Returns :
;;;     the GdkPixmap
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_pixmap_create_from_xpm" %gdk-pixmap-create-from-xpm)
    (g-object gdk-pixmap :already-referenced)
  (drawable (g-object gdk-drawable))
  (mask :pointer)
  (transparent-color (g-boxed-foreign gdk-color))
  (filename :string))

(defun gdk-pixmap-create-from-xpm (drawable transparent-color
                                            &key (colormap nil colormap-p)
                                                 (filename nil filename-p)
                                                 (xpm-data nil xpm-p))
  (unless (or filename-p xpm-p)
    (error "FILENAME or XPM-DATA must be specified"))
  (when (and filename-p xpm-p)
    (error "FILENAME and XPM-DATA may not be specified at the same time"))
  (with-foreign-object (mask-ptr :pointer)
    (let ((pixmap (if filename-p
                      (if colormap-p
                          (gdk-pixmap-colormap-create-from-xpm drawable
                                                               colormap
                                                               mask-ptr
                                                               transparent-color
                                                               filename)
                          (%gdk-pixmap-create-from-xpm drawable
                                                       mask-ptr
                                                       transparent-color
                                                       filename))
                      (if colormap-p
                          (gdk-pixmap-colormap-create-from-xpm-d drawable
                                                                 colormap
                                                                 mask-ptr
                                                                 transparent-color
                                                                 xpm-data)
                          (gdk-pixmap-create-from-xpm-d drawable
                                                        mask-ptr
                                                        transparent-color
                                                        xpm-data)))))
      (values pixmap
              (convert-from-foreign mask-ptr 
                                    '(g-object gdk-pixmap
                                               :already-referenced))))))

(export 'pixmap-create-from-xpm)

;;; ----------------------------------------------------------------------------
;;; gdk_pixmap_colormap_create_from_xpm ()
;;; 
;;; GdkPixmap * gdk_pixmap_colormap_create_from_xpm
;;;                                          (GdkDrawable *drawable,
;;;                                           GdkColormap *colormap,
;;;                                           GdkBitmap **mask,
;;;                                           const GdkColor *transparent_color,
;;;                                           const gchar *filename);
;;; 
;;; Warning
;;; 
;;; gdk_pixmap_colormap_create_from_xpm has been deprecated since version 2.22
;;; and should not be used in newly-written code. Use a GdkPixbuf instead. You
;;; can use gdk_pixbuf_new_from_file() to create it. If you must use a pixmap,
;;; use gdk_pixmap_new() to create it and Cairo to draw the pixbuf onto it.
;;; 
;;; Create a pixmap from a XPM file using a particular colormap.
;;; 
;;; drawable :
;;;     a GdkDrawable, used to determine default values for the new pixmap.
;;;     Can be NULL if colormap is given.
;;; 
;;; colormap :
;;;     the GdkColormap that the new pixmap will be use. If omitted, the
;;;     colormap for window will be used.
;;; 
;;; mask :
;;;     a pointer to a place to store a bitmap representing the transparency
;;;     mask of the XPM file. Can be NULL, in which case transparency will be
;;;     ignored.
;;; 
;;; transparent_color :
;;;     the color to be used for the pixels that are transparent in the input
;;;     file. Can be NULL, in which case a default color will be used.
;;; 
;;; filename :
;;;     the filename of a file containing XPM data
;;; 
;;; Returns :
;;;     the GdkPixmap
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_pixmap_colormap_create_from_xpm"
          gdk-pixmap-colormap-create-from-xpm)
    (g-object gdk-pixmap :already-referenced)
  (drawable (g-object gdk-drawable))
  (colormap (g-object colormap))
  (mask :pointer)
  (transparent-color (g-boxed-foreign gdk-color))
  (filename :string))

(export 'gdk-pixmap-colormap-create-from-xpm)

;;; ----------------------------------------------------------------------------
;;; gdk_pixmap_create_from_xpm_d ()
;;; 
;;; GdkPixmap * gdk_pixmap_create_from_xpm_d (GdkDrawable *drawable,
;;;                                           GdkBitmap **mask,
;;;                                           const GdkColor *transparent_color,
;;;                                           gchar **data);
;;; 
;;; Warning
;;; 
;;; gdk_pixmap_create_from_xpm_d has been deprecated since version 2.22 and
;;; should not be used in newly-written code. Use a GdkPixbuf instead. You can
;;; use gdk_pixbuf_new_from_xpm_data() to create it. If you must use a pixmap,
;;; use gdk_pixmap_new() to create it and Cairo to draw the pixbuf onto it.
;;; 
;;; Create a pixmap from data in XPM format.
;;; 
;;; drawable :
;;;     a GdkDrawable, used to determine default values for the new pixmap
;;; 
;;; mask :
;;;     Pointer to a place to store a bitmap representing the transparency mask
;;;     of the XPM file. Can be NULL, in which case transparency will be
;;;     ignored.
;;; 
;;; transparent_color :
;;;     This color will be used for the pixels that are transparent in the
;;;     input file. Can be NULL in which case a default color will be used.
;;; 
;;; data :
;;;     Pointer to a string containing the XPM data.
;;; 
;;; Returns :
;;;     the GdkPixmap
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_pixmap_create_from_xpm_d" %gdk-pixmap-create-from-xpm-d)
    (g-object gdk-pixmap :already-referenced)
  (drawable (g-object gdk-drawable))
  (mask :pointer)
  (transparent-color (g-boxed-foreign gdk-color))
  (data (:pointer :pointer)))

(defun gdk-pixmap-create-from-xpm-d (drawable mask transparent-color data)
  (let ((n (length data)))
    (with-foreign-object (data-ptr :pointer n)
      (let ((i 0))
        (map nil
             (lambda (str)
               (setf (mem-aref data-ptr :pointer i)
                     (cffi:foreign-string-alloc str))
               (incf i))
             data))
      (%gdk-pixmap-create-from-xpm-d drawable
                                     mask
                                     transparent-color
                                     data-ptr))))

(export 'gdk-pixmap-create-from-xpm-d)

;;; ----------------------------------------------------------------------------
;;; gdk_pixmap_colormap_create_from_xpm_d ()
;;; 
;;; GdkPixmap * gdk_pixmap_colormap_create_from_xpm_d
;;;                                          (GdkDrawable *drawable,
;;;                                           GdkColormap *colormap,
;;;                                           GdkBitmap **mask,
;;;                                           const GdkColor *transparent_color,
;;;                                           gchar **data);
;;; 
;;; Warning
;;; 
;;; gdk_pixmap_colormap_create_from_xpm_d has been deprecated since version
;;; 2.22 and should not be used in newly-written code. Use a GdkPixbuf instead.
;;; You can use gdk_pixbuf_new_from_xpm_data() to create it. If you must use a
;;; pixmap, use gdk_pixmap_new() to create it and Cairo to draw the pixbuf onto
;;; it.
;;; 
;;; Create a pixmap from data in XPM format using a particular colormap.
;;; 
;;; drawable :
;;;     a GdkDrawable, used to determine default values for the new pixmap.
;;;     Can be NULL if colormap is given.
;;; 
;;; colormap :
;;;     the GdkColormap that the new pixmap will be use. If omitted, the
;;;     colormap for window will be used.
;;; 
;;; mask :
;;;     a pointer to a place to store a bitmap representing the transparency
;;;     mask of the XPM file. Can be NULL, in which case transparency will be
;;;     ignored.
;;; 
;;; transparent_color :
;;;     the color to be used for the pixels that are transparent in the input
;;;     file. Can be NULL, in which case a default color will be used.
;;; 
;;; data :
;;;     Pointer to a string containing the XPM data.
;;; 
;;; Returns :
;;;     the GdkPixmap
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_pixmap_colormap_create_from_xpm_d"
          %gdk-pixmap-colormap-create-from-xpm-d)
    (g-object gdk-pixmap :already-referenced)
  (drawable (g-object gdk-drawable))
  (colormap (g-object colormap))
  (mask :pointer)
  (transparent-color (g-boxed-foreign gdk-color))
  (data (:pointer :pointer)))

(defun gdk-pixmap-colormap-create-from-xpm-d (drawable colormap mask
                                                       transparent-color data)
  (let ((n (length data)))
    (with-foreign-object (data-ptr :pointer n)
      (let ((i 0))
        (map nil
             (lambda (str)
               (setf (mem-aref data-ptr :pointer i)
                     (cffi:foreign-string-alloc str))
               (incf i))
             data))
      (%gdk-pixmap-colormap-create-from-xpm-d drawable
                                              colormap
                                              mask
                                              transparent-color
                                              data-ptr))))

(export 'gdk-pixmap-colormap-create-from-xpm-d)

;;; ----------------------------------------------------------------------------
;;; gdk_pixmap_get_size ()
;;; 
;;; void gdk_pixmap_get_size (GdkPixmap *pixmap, gint *width, gint *height);
;;; 
;;; This function is purely to make it possible to query the size of pixmaps
;;; even when compiling without deprecated symbols and you must use pixmaps.
;;; It is identical to gdk_drawable_get_size(), but for pixmaps.
;;; 
;;; pixmap :
;;;     a GdkPixmap
;;; 
;;; width :
;;;     location to store pixmap's width, or NULL
;;; 
;;; height :
;;;     location to store pixmap's height, or NULL
;;; 
;;; Since 2.24
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GdkBitmap
;;; 
;;; typedef struct _GdkDrawable GdkBitmap;
;;; 
;;; An opaque structure representing an offscreen drawable of depth 1. Pointers
;;; to structures of type GdkPixmap, GdkBitmap, and GdkWindow, can often be
;;; used interchangeably. The type GdkDrawable refers generically to any of
;;; these types.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_bitmap_create_from_data ()
;;; 
;;; GdkBitmap * gdk_bitmap_create_from_data (GdkDrawable *drawable,
;;;                                          const gchar *data,
;;;                                          gint width,
;;;                                          gint height);
;;; 
;;; Warning
;;; 
;;; gdk_bitmap_create_from_data has been deprecated since version 2.22 and
;;; should not be used in newly-written code. You can create a Cairo image
;;; surface using cairo_image_surface_create_for_data() instead. Specify
;;; CAIRO_FORMAT_A1 as the format to get a bitmap. Keep in mind that Cairo
;;; images must have a rowstride of 4 bytes, so you will need to align your
;;; data properly. If you must use a pixmap, use gdk_pixmap_new() with a depth
;;; of 1 to create a bitmap and then use gdk_cairo_create(),
;;; cairo_set_source_surface() and cairo_paint() to draw the image surface to
;;; the bitmap.
;;; 
;;; Creates a new bitmap from data in XBM format.
;;; 
;;; drawable :
;;;     a GdkDrawable, used to determine default values for the new pixmap.
;;;     Can be NULL, in which case the root window is used.
;;; 
;;; data :
;;;     a pointer to the XBM data
;;; 
;;; width :
;;;     the width of the new pixmap in pixels
;;; 
;;; height :
;;;     the height of the new pixmap in pixels
;;; 
;;; Returns :
;;;     the GdkBitmap
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_bitmap_create_from_data" gdk-bitmap-create-from-data)
    (g-object gdk-pixmap :already-referenced)
  (drawable (g-object gdk-drawable))
  (data :pointer)
  (width :int)
  (height :int))

(export 'gdk-bitmap-create-from-data)

;;; --- End of file gdk.pixmap.lisp --------------------------------------------
