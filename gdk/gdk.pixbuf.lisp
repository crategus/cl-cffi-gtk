;;; ----------------------------------------------------------------------------
;;; gdk.pixbuf.lisp
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
;;; Pixbufs
;;; 
;;; Functions for rendering pixbufs on drawables
;;; 
;;; Synopsis
;;; 
;;;     gdk_pixbuf_render_threshold_alpha
;;;     gdk_pixbuf_render_to_drawable
;;;     gdk_pixbuf_render_to_drawable_alpha
;;;     gdk_pixbuf_render_pixmap_and_mask
;;;     gdk_pixbuf_render_pixmap_and_mask_for_colormap
;;;     gdk_pixbuf_get_from_drawable
;;;     gdk_pixbuf_get_from_image
;;; 
;;; Description
;;; 
;;; These functions allow to render pixbufs on drawables. Pixbufs are
;;; client-side images. For details on how to create and manipulate pixbufs,
;;; see the GdkPixbuf API documentation.
;;; ----------------------------------------------------------------------------

(in-package :gdk)

;; An opaque struct representing an animation. 

(define-g-object-class "GdkPixbufAnimation" gdk-pixbuf-animation
  (:type-initializer "gdk_pixbuf_animation_get_type")
    nil)

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_render_threshold_alpha ()
;;; 
;;; void gdk_pixbuf_render_threshold_alpha (GdkPixbuf *pixbuf,
;;;                                         GdkBitmap *bitmap,
;;;                                         int src_x,
;;;                                         int src_y,
;;;                                         int dest_x,
;;;                                         int dest_y,
;;;                                         int width,
;;;                                         int height,
;;;                                         int alpha_threshold);
;;; 
;;; Takes the opacity values in a rectangular portion of a pixbuf and
;;; thresholds them to produce a bi-level alpha mask that can be used as a
;;; clipping mask for a drawable.
;;; 
;;; pixbuf :
;;;     A pixbuf.
;;; 
;;; bitmap :
;;;     Bitmap where the bilevel mask will be painted to.
;;; 
;;; src_x :
;;;     Source X coordinate.
;;; 
;;; src_y :
;;;     source Y coordinate.
;;; 
;;; dest_x :
;;;     Destination X coordinate.
;;; 
;;; dest_y :
;;;     Destination Y coordinate.
;;; 
;;; width :
;;;     Width of region to threshold, or -1 to use pixbuf width
;;; 
;;; height :
;;;     Height of region to threshold, or -1 to use pixbuf height
;;; 
;;; alpha_threshold :
;;;     Opacity values below this will be painted as zero; all other values
;;;     will be painted as one.
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_pixbuf_render_threshold_alpha" gdk-pixbuf-render-threshold-alpha)
    :void
  (pixbuf (g-object gdk-pixbuf))
  (bitmap (g-object bitmap))
  (src-x :int)
  (src-y :int)
  (dest-x :int)
  (dest-y :int)
  (width :int)
  (height :int)
  (alpha-threshold :int))

(export 'gdk-pixbuf-render-threshold-alpha)

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_render_to_drawable ()
;;; 
;;; void gdk_pixbuf_render_to_drawable (GdkPixbuf *pixbuf,
;;;                                     GdkDrawable *drawable,
;;;                                     GdkGC *gc,
;;;                                     int src_x,
;;;                                     int src_y,
;;;                                     int dest_x,
;;;                                     int dest_y,
;;;                                     int width,
;;;                                     int height,
;;;                                     GdkRgbDither dither,
;;;                                     int x_dither,
;;;                                     int y_dither);
;;; 
;;; Warning
;;; 
;;; gdk_pixbuf_render_to_drawable has been deprecated since version 2.4 and
;;; should not be used in newly-written code. This function is obsolete.
;;; Use gdk_draw_pixbuf() instead.
;;; 
;;; Renders a rectangular portion of a pixbuf to a drawable while using the
;;; specified GC. This is done using GdkRGB, so the specified drawable must
;;; have the GdkRGB visual and colormap. Note that this function will ignore
;;; the opacity information for images with an alpha channel; the GC must
;;; already have the clipping mask set if you want transparent regions to show
;;; through.
;;; 
;;; For an explanation of dither offsets, see the GdkRGB documentation. In
;;; brief, the dither offset is important when re-rendering partial regions of
;;; an image to a rendered version of the full image, or for when the offsets
;;; to a base position change, as in scrolling. The dither matrix has to be
;;; shifted for consistent visual results. If you do not have any of these
;;; cases, the dither offsets can be both zero.
;;; 
;;; pixbuf :
;;;     A pixbuf.
;;; 
;;; drawable :
;;;     Destination drawable.
;;; 
;;; gc :
;;;     GC used for rendering.
;;; 
;;; src_x :
;;;     Source X coordinate within pixbuf.
;;; 
;;; src_y :
;;;     Source Y coordinate within pixbuf.
;;; 
;;; dest_x :
;;;     Destination X coordinate within drawable.
;;; 
;;; dest_y :
;;;     Destination Y coordinate within drawable.
;;; 
;;; width :
;;;     Width of region to render, in pixels, or -1 to use pixbuf width
;;; 
;;; height :
;;;     Height of region to render, in pixels, or -1 to use pixbuf height
;;; 
;;; dither :
;;;     Dithering mode for GdkRGB.
;;; 
;;; x_dither :
;;;     X offset for dither.
;;; 
;;; y_dither :
;;;     Y offset for dither.
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_pixbuf_render_to_drawable" gdk-pixbuf-render-to-drawable) :void
  (pixbuf (g-object gdk-pixbuf))
  (drawable (g-object gdk-drawable))
  (gc (g-object gdk-gc))
  (src-x :int)
  (src-y :int)
  (dest-x :int)
  (dest-y :int)
  (width :int)
  (height :int)
  (dither gdk-rgb-dither)
  (x-dither :int)
  (y-dither :int))

(export 'gdk-pixbuf-render-to-drawable)

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_render_to_drawable_alpha ()
;;; 
;;; void gdk_pixbuf_render_to_drawable_alpha (GdkPixbuf *pixbuf,
;;;                                           GdkDrawable *drawable,
;;;                                           int src_x,
;;;                                           int src_y,
;;;                                           int dest_x,
;;;                                           int dest_y,
;;;                                           int width,
;;;                                           int height,
;;;                                           GdkPixbufAlphaMode alpha_mode,
;;;                                           int alpha_threshold,
;;;                                           GdkRgbDither dither,
;;;                                           int x_dither,
;;;                                           int y_dither);
;;; 
;;; Warning
;;; 
;;; gdk_pixbuf_render_to_drawable_alpha has been deprecated since version 2.4
;;; and should not be used in newly-written code. This function is obsolete.
;;; Use gdk_draw_pixbuf() instead.
;;; 
;;; Renders a rectangular portion of a pixbuf to a drawable. The destination
;;; drawable must have a colormap. All windows have a colormap, however,
;;; pixmaps only have colormap by default if they were created with a non-NULL
;;; window argument. Otherwise a colormap must be set on them with
;;; gdk_drawable_set_colormap.
;;; 
;;; On older X servers, rendering pixbufs with an alpha channel involves round
;;; trips to the X server, and may be somewhat slow.
;;; 
;;; pixbuf :
;;;     A pixbuf.
;;; 
;;; drawable :
;;;     Destination drawable.
;;; 
;;; src_x :
;;;     Source X coordinate within pixbuf.
;;; 
;;; src_y :
;;;     Source Y coordinates within pixbuf.
;;; 
;;; dest_x :
;;;     Destination X coordinate within drawable.
;;; 
;;; dest_y :
;;;     Destination Y coordinate within drawable.
;;; 
;;; width :
;;;     Width of region to render, in pixels, or -1 to use pixbuf width.
;;; 
;;; height :
;;;     Height of region to render, in pixels, or -1 to use pixbuf height.
;;; 
;;; alpha_mode :
;;;     Ignored. Present for backwards compatibility.
;;; 
;;; alpha_threshold :
;;;     Ignored. Present for backwards compatibility
;;; 
;;; dither :
;;;     Dithering mode for GdkRGB.
;;; 
;;; x_dither :
;;;     X offset for dither.
;;; 
;;; y_dither :
;;;     Y offset for dither.
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_pixbuf_render_to_drawable_alpha"
          gdk-pixbuf-render-to-drawable-alpha) :void
  (pixbuf (g-object gdk-pixbuf))
  (drawable (g-object gdk-drawable))
  (src-x :int)
  (src-y :int)
  (dest-x :int)
  (dest-y :int)
  (width :int)
  (height :int)
  (alpha-mode gdk-pixbuf-alpha-mode)
  (alpha-threshold :int)
  (dither gdk-rgb-dither)
  (x-dither :int)
  (y-dither :int))

(export 'gdk-pixbuf-render-to-drawable-alpha)

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_render_pixmap_and_mask ()
;;; 
;;; void gdk_pixbuf_render_pixmap_and_mask (GdkPixbuf *pixbuf,
;;;                                         GdkPixmap **pixmap_return,
;;;                                         GdkBitmap **mask_return,
;;;                                         int alpha_threshold);
;;; 
;;; Creates a pixmap and a mask bitmap which are returned in the pixmap_return
;;; and mask_return arguments, respectively, and renders a pixbuf and its
;;; corresponding thresholded alpha mask to them. This is merely a convenience
;;; function; applications that need to render pixbufs with dither offsets or
;;; to given drawables should use gdk_draw_pixbuf() and
;;; gdk_pixbuf_render_threshold_alpha().
;;; 
;;; The pixmap that is created is created for the colormap returned by
;;; gdk_rgb_get_colormap(). You normally will want to instead use the actual
;;; colormap for a widget, and use
;;; gdk_pixbuf_render_pixmap_and_mask_for_colormap().
;;; 
;;; If the pixbuf does not have an alpha channel, then *mask_return will be set
;;; to NULL.
;;; 
;;; pixbuf :
;;;     A pixbuf.
;;; 
;;; pixmap_return :
;;;     Location to store a pointer to the created pixmap, or NULL if the
;;;     pixmap is not needed.
;;; 
;;; mask_return :
;;;     Location to store a pointer to the created mask, or NULL if the mask
;;;     is not needed.
;;; 
;;; alpha_threshold :
;;;     Threshold value for opacity values.
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_pixbuf_render_pixmap_and_mask"
          %gdk-pixbuf-render-pixmap-and-mask) :void
  (pixbuf (g-object gdk-pixbuf))
  (pixmap-return :pointer)
  (mask-return :pointer)
  (alpha-threshold :int))

(defun gdk-pixbuf-render-pixmap-and-mask (pixbuf alpha-threshold)
  (with-foreign-objects ((pixmap-return :pointer) (mask-return :pointer))
    (%gdk-pixbuf-render-pixmap-and-mask pixbuf
                                        pixmap-return
                                        mask-return
                                        alpha-threshold)
    (values (convert-from-foreign (mem-ref pixmap-return :pointer)
                                  '(g-object pixmap :already-referenced))
            (convert-from-foreign (mem-ref mask-return :pointer) 
                                  '(g-object pixmap :already-referenced)))))

(export 'gdk-pixbuf-render-pixmap-and-mask)

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_render_pixmap_and_mask_for_colormap ()
;;; 
;;; void gdk_pixbuf_render_pixmap_and_mask_for_colormap
;;;                                                  (GdkPixbuf *pixbuf,
;;;                                                   GdkColormap *colormap,
;;;                                                   GdkPixmap **pixmap_return,
;;;                                                   GdkBitmap **mask_return,
;;;                                                   int alpha_threshold);
;;; 
;;; Creates a pixmap and a mask bitmap which are returned in the pixmap_return
;;; and mask_return arguments, respectively, and renders a pixbuf and its
;;; corresponding tresholded alpha mask to them. This is merely a convenience
;;; function; applications that need to render pixbufs with dither offsets or
;;; to given drawables should use gdk_draw_pixbuf(), and
;;; gdk_pixbuf_render_threshold_alpha().
;;; 
;;; The pixmap that is created uses the GdkColormap specified by colormap. This
;;; colormap must match the colormap of the window where the pixmap will
;;; eventually be used or an error will result.
;;; 
;;; If the pixbuf does not have an alpha channel, then *mask_return will be set
;;; to NULL.
;;; 
;;; pixbuf :
;;;     A pixbuf.
;;; 
;;; colormap :
;;;     A GdkColormap
;;; 
;;; pixmap_return :
;;;     Location to store a pointer to the created pixmap, or NULL if the
;;;     pixmap is not needed.
;;; 
;;; mask_return :
;;;     Location to store a pointer to the created mask, or NULL if the mask
;;;     is not needed.
;;; 
;;; alpha_threshold :
;;;     Threshold value for opacity values.
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_pixbuf_render_pixmap_and_mask_for_colormap"
          %gdk-pixbuf-render-pixmap-and-mask-for-colormap) :void
  (pixbuf (g-object gdk-pixbuf))
  (colormap (g-object colormap))
  (pixmap-return :pointer)
  (mask-return :pointer)
  (alpha-threshold :int))

(defun gdk-pixbuf-render-pixmap-and-mask-for-colormap (pixbuf
                                                       colormap
                                                       alpha-threshold)
  (with-foreign-objects ((pixmap-return :pointer) (mask-return :pointer))
    (%gdk-pixbuf-render-pixmap-and-mask-for-colormap pixbuf
                                                     colormap
                                                     pixmap-return
                                                     mask-return
                                                     alpha-threshold)
    (values (convert-from-foreign (mem-ref pixmap-return :pointer)
                                  '(g-object pixmap :already-referenced))
            (convert-from-foreign (mem-ref mask-return :pointer)
                                  '(g-object pixmap :already-referenced)))))

(export 'gdk-pixbuf-render-pixmap-and-mask-for-colormap)

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_get_from_drawable ()
;;; 
;;; GdkPixbuf * gdk_pixbuf_get_from_drawable (GdkPixbuf *dest,
;;;                                           GdkDrawable *src,
;;;                                           GdkColormap *cmap,
;;;                                           int src_x,
;;;                                           int src_y,
;;;                                           int dest_x,
;;;                                           int dest_y,
;;;                                           int width,
;;;                                           int height);
;;; 
;;; Transfers image data from a GdkDrawable and converts it to an RGB(A)
;;; representation inside a GdkPixbuf. In other words, copies image data from a
;;; server-side drawable to a client-side RGB(A) buffer. This allows you to
;;; efficiently read individual pixels on the client side.
;;; 
;;; If the drawable src has no colormap (gdk_drawable_get_colormap() returns
;;; NULL), then a suitable colormap must be specified. Typically a GdkWindow or
;;; a pixmap created by passing a GdkWindow to gdk_pixmap_new() will already
;;; have a colormap associated with it. If the drawable has a colormap, the
;;; cmap argument will be ignored. If the drawable is a bitmap (1 bit per pixel
;;; pixmap), then a colormap is not required; pixels with a value of 1 are
;;; assumed to be white, and pixels with a value of 0 are assumed to be black.
;;; For taking screenshots, gdk_colormap_get_system() returns the correct
;;; colormap to use.
;;; 
;;; If the specified destination pixbuf dest is NULL, then this function will
;;; create an RGB pixbuf with 8 bits per channel and no alpha, with the same
;;; size specified by the width and height arguments. In this case, the dest_x
;;; and dest_y arguments must be specified as 0. If the specified destination
;;; pixbuf is not NULL and it contains alpha information, then the filled pixels
;;; will be set to full opacity (alpha = 255).
;;; 
;;; If the specified drawable is a pixmap, then the requested source rectangle
;;; must be completely contained within the pixmap, otherwise the function will
;;; return NULL. For pixmaps only (not for windows) passing -1 for width or
;;; height is allowed to mean the full width or height of the pixmap.
;;; 
;;; If the specified drawable is a window, and the window is off the screen,
;;; then there is no image data in the obscured/offscreen regions to be placed
;;; in the pixbuf. The contents of portions of the pixbuf corresponding to the
;;; offscreen region are undefined.
;;; 
;;; If the window you're obtaining data from is partially obscured by other
;;; windows, then the contents of the pixbuf areas corresponding to the
;;; obscured regions are undefined.
;;; 
;;; If the target drawable is not mapped (typically because it's
;;; iconified/minimized or not on the current workspace), then NULL will be
;;; returned.
;;; 
;;; If memory can't be allocated for the return value, NULL will be returned
;;; instead.
;;; 
;;; (In short, there are several ways this function can fail, and if it fails
;;; it returns NULL; so check the return value.)
;;; 
;;; This function calls gdk_drawable_get_image() internally and converts the
;;; resulting image to a GdkPixbuf, so the documentation for
;;; gdk_drawable_get_image() may also be relevant.
;;; 
;;; dest :
;;;     Destination pixbuf, or NULL if a new pixbuf should be created.
;;; 
;;; src :
;;;     Source drawable.
;;; 
;;; cmap :
;;;     A colormap if src doesn't have one set.
;;; 
;;; src_x :
;;;     Source X coordinate within drawable.
;;; 
;;; src_y :
;;;     Source Y coordinate within drawable.
;;; 
;;; dest_x :
;;;     Destination X coordinate in pixbuf, or 0 if dest is NULL.
;;; 
;;; dest_y :
;;;     Destination Y coordinate in pixbuf, or 0 if dest is NULL.
;;; 
;;; width :
;;;     Width in pixels of region to get.
;;; 
;;; height :
;;;     Height in pixels of region to get.
;;; 
;;; Returns :
;;;     The same pixbuf as dest if it was non-NULL, or a newly-created pixbuf
;;;     with a reference count of 1 if no destination pixbuf was specified,
;;;     or NULL on error
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_pixbuf_get_from_drawable" %gdk-pixbuf-get-from-drawable)
    (g-object gdk-pixbuf :already-referenced)
  (dest (g-object gdk-pixbuf))
  (src (g-object gdk-drawable))
  (colormap :pointer)
  (src-x :int)
  (src-y :int)
  (dest-x :int)
  (dest-y :int)
  (width :int)
  (height :int))

(defun gdk-pixbuf-get-from-drawable (pixbuf drawable
                                            &key (src-x 0)  (src-y 0)
                                                 (dest-x 0) (dest-y 0)
                                                 (width -1) (height -1))
  (%gdk-pixbuf-get-from-drawable pixbuf
                                 drawable
                                 (null-pointer)
                                 src-x src-y
                                 dest-x dest-y
                                 width height))

(export 'gdk-pixbuf-get-from-drawable)

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_get_from_image ()
;;; 
;;; GdkPixbuf * gdk_pixbuf_get_from_image (GdkPixbuf *dest,
;;;                                        GdkImage *src,
;;;                                        GdkColormap *cmap,
;;;                                        int src_x,
;;;                                        int src_y,
;;;                                        int dest_x,
;;;                                        int dest_y,
;;;                                        int width,
;;;                                        int height);
;;; 
;;; Same as gdk_pixbuf_get_from_drawable() but gets the pixbuf from an image.
;;; 
;;; dest :
;;;     Destination pixbuf, or NULL if a new pixbuf should be created.
;;; 
;;; src :
;;;     Source GdkImage.
;;; 
;;; cmap :
;;;     A colormap, or NULL to use the one for src. [allow-none]
;;; 
;;; src_x :
;;;     Source X coordinate within drawable.
;;; 
;;; src_y :
;;;     Source Y coordinate within drawable.
;;; 
;;; dest_x :
;;;     Destination X coordinate in pixbuf, or 0 if dest is NULL.
;;; 
;;; dest_y :
;;;     Destination Y coordinate in pixbuf, or 0 if dest is NULL.
;;; 
;;; width :
;;;     Width in pixels of region to get.
;;; 
;;; height :
;;;     Height in pixels of region to get.
;;; 
;;; Returns :
;;;     dest, newly-created pixbuf if dest was NULL, NULL on error
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_pixbuf_get_from_image" %gdk-pixbuf-get-from-image)
    (g-object gdk-pixbuf :already-referenced)
  (dest (g-object gdk-pixbuf))
  (src (g-object gdk-image))
  (colormap :pointer)
  (src-x :int)
  (src-y :int)
  (dest-x :int)
  (dest-y :int)
  (width :int)
  (height :int))

(defun gdk-pixbuf-get-from-image (pixbuf image
                                         &key (src-x 0) (src-y 0)
                                              (dest-x 0) (dest-y 0)
                                              (width -1) (height -1))
  (%gdk-pixbuf-get-from-image pixbuf
                              image
                              (null-pointer)
                              src-x src-y
                              dest-x dest-y
                              width height))

(export 'gdk-pixbuf-get-from-image)

;;; --- End of file gdk.pixbuf.lisp --------------------------------------------
