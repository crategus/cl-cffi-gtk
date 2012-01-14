;;; ----------------------------------------------------------------------------
;;; gdk.rgb.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See http://common-lisp.net/project/cl-gtk2/
;;;
;;; The documentation has been copied from the GDK 2 Reference Manual
;;; See http://www.gtk.org
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
;;; GdkRGB
;;; 
;;; Renders RGB, grayscale, or indexed image data to a GdkDrawable
;;; 	
;;; Synopsis
;;; 
;;;     gdk_rgb_init
;;;     gdk_draw_rgb_image
;;;     gdk_draw_rgb_image_dithalign
;;;     gdk_draw_indexed_image
;;;     gdk_draw_gray_image
;;;     gdk_draw_rgb_32_image
;;;     gdk_draw_rgb_32_image_dithalign
;;;
;;;     GdkRgbDither
;;;
;;;     gdk_rgb_cmap_new
;;;     gdk_rgb_cmap_free
;;;     gdk_rgb_gc_set_foreground
;;;     gdk_rgb_gc_set_background
;;;     gdk_rgb_xpixel_from_rgb
;;;     gdk_rgb_find_color
;;;     gdk_rgb_set_install
;;;     gdk_rgb_set_min_colors
;;;     gdk_rgb_get_visual
;;;     gdk_rgb_get_colormap
;;;     gdk_rgb_get_cmap
;;;     gdk_rgb_ditherable
;;;     gdk_rgb_colormap_ditherable
;;;     gdk_rgb_set_verbose
;;; 
;;; Description
;;; 
;;; GdkRGB is a low-level module which renders RGB, grayscale, and indexed
;;; colormap images to a GdkDrawable. It does this as efficiently as possible,
;;; handling issues such as colormaps, visuals, dithering, temporary buffers,
;;; and so on. Most code should use the higher-level GdkPixbuf features in
;;; place of this module; for example, gdk_draw_pixbuf() uses GdkRGB in its
;;; implementation.
;;; 
;;; GdkRGB allocates a color cube to use when rendering images. You can set the
;;; threshold for installing colormaps with gdk_rgb_set_min_colors(). The
;;; default is 5x5x5 (125). If a colorcube of this size or larger can be
;;; allocated in the default colormap, then that's done. Otherwise, GdkRGB
;;; creates its own private colormap. Setting it to 0 means that it always
;;; tries to use the default colormap, and setting it to 216 means that it
;;; always creates a private one if it cannot allocate the 6x6x6 colormap in
;;; the default. If you always want a private colormap (to avoid consuming too
;;; many colormap entries for other apps, say), you can use
;;; gdk_rgb_set_install(TRUE). Setting the value greater than 216 exercises a
;;; bug in older versions of GdkRGB. Note, however, that setting it to 0
;;; doesn't let you get away with ignoring the colormap and visual - a colormap
;;; is always created in grayscale and direct color modes, and the visual is
;;; changed in cases where a "better" visual than the default is available.
;;; 
;;; If GDK is built with the Sun mediaLib library, the GdkRGB functions are
;;; accelerated using mediaLib, which provides hardware acceleration on Intel,
;;; AMD, and Sparc chipsets. If desired, mediaLib support can be turned off by
;;; setting the GDK_DISABLE_MEDIALIB environment variable.
;;; 
;;; Example 4. A simple example program using GdkRGB
;;; 
;;; #include <gtk/gtk.h>
;;; #define IMAGE_WIDTH 256
;;; #define IMAGE_HEIGHT    256
;;; guchar rgbbuf[IMAGE_WIDTH * IMAGE_HEIGHT * 3];
;;; gboolean on_darea_expose (GtkWidget *widget,
;;;               GdkEventExpose *event,
;;;               gpointer user_data);
;;; int
;;; main (int argc, char *argv[])
;;; {
;;;   GtkWidget *window, *darea;
;;;   gint x, y;
;;;   guchar *pos;
;;;   gtk_init (&argc, &argv);
;;;   window = gtk_window_new (GTK_WINDOW_TOPLEVEL);
;;;   darea = gtk_drawing_area_new ();
;;;   gtk_widget_set_size_request (darea, IMAGE_WIDTH, IMAGE_HEIGHT);
;;;   gtk_container_add (GTK_CONTAINER (window), darea);
;;;   gtk_signal_connect (GTK_OBJECT (darea), "expose-event",
;;;                       GTK_SIGNAL_FUNC (on_darea_expose), NULL);
;;;   gtk_widget_show_all (window);
;;;   /* Set up the RGB buffer. */
;;;   pos = rgbbuf;
;;;   for (y = 0; y < IMAGE_HEIGHT; y++)
;;;     {
;;;       for (x = 0; x < IMAGE_WIDTH; x++)
;;;     {
;;;       *pos++ = x - x % 32;          /* Red. */
;;;       *pos++ = (x / 32) * 4 + y - y % 32;   /* Green. */
;;;       *pos++ = y - y % 32;          /* Blue. */
;;;     }
;;;     }
;;;   gtk_main ();
;;;   return 0;
;;; }
;;; gboolean
;;; on_darea_expose (GtkWidget *widget,
;;;          GdkEventExpose *event,
;;;          gpointer user_data)
;;; {
;;;   gdk_draw_rgb_image (widget->window, widget->style->fg_gc[GTK_STATE_NORMAL],
;;;               0, 0, IMAGE_WIDTH, IMAGE_HEIGHT,
;;;               GDK_RGB_DITHER_MAX, rgbbuf, IMAGE_WIDTH * 3);
;;;   return TRUE;
;;; }
;;; ----------------------------------------------------------------------------

(in-package :gdk)

;;; ----------------------------------------------------------------------------
;;; enum GdkRgbDither
;;; 
;;; typedef enum
;;; {
;;;   GDK_RGB_DITHER_NONE,
;;;   GDK_RGB_DITHER_NORMAL,
;;;   GDK_RGB_DITHER_MAX
;;; } GdkRgbDither;
;;; 
;;; Selects whether or not GdkRGB applies dithering to the image on display.
;;; 
;;; Since GdkRGB currently only handles images with 8 bits per component,
;;; dithering on 24 bit per pixel displays is a moot point.
;;; 
;;; GDK_RGB_DITHER_NONE
;;; 	Never use dithering.
;;; 
;;; GDK_RGB_DITHER_NORMAL
;;; 	Use dithering in 8 bits per pixel (and below) only.
;;; 
;;; GDK_RGB_DITHER_MAX
;;; 	Use dithering in 16 bits per pixel and below.
;;; ----------------------------------------------------------------------------

(define-g-enum "GdkRgbDither" gdk-rgb-dither
  (:export t
   :type-initializer "gdk_rgb_dither_get_type")
  (:none 0)
  (:normal 1)
  (:max 2))

;;; ----------------------------------------------------------------------------
;;; gdk_rgb_init ()
;;; 
;;; void gdk_rgb_init (void);
;;; 
;;; Warning
;;; 
;;; gdk_rgb_init is deprecated and should not be used in newly-written code.
;;; 
;;; This function no longer does anything at all. It's completely useless
;;; (and harmless).
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_draw_rgb_image ()
;;; 
;;; void gdk_draw_rgb_image (GdkDrawable *drawable,
;;;                          GdkGC *gc,
;;;                          gint x,
;;;                          gint y,
;;;                          gint width,
;;;                          gint height,
;;;                          GdkRgbDither dith,
;;;                          const guchar *rgb_buf,
;;;                          gint rowstride);
;;; 
;;; Warning
;;; 
;;; gdk_draw_rgb_image is deprecated and should not be used in newly-written
;;; code.
;;; 
;;; Draws an RGB image in the drawable. This is the core GdkRGB function, and
;;; likely the only one you will need to use.
;;; 
;;; The rowstride parameter allows for lines to be aligned more flexibly. For
;;; example, lines may be allocated to begin on 32-bit boundaries, even if the
;;; width of the rectangle is odd. Rowstride is also useful when drawing a
;;; subrectangle of a larger image in memory. Finally, to replicate the same
;;; line a number of times, the trick of setting rowstride to 0 is allowed.
;;; 
;;; In general, for 0 <= i < width and 0 <= j < height, the pixel (x + i, y + j)
;;; is colored with red value rgb_buf[j * rowstride + i * 3], green value
;;; rgb_buf[j * rowstride + i * 3 + 1], and blue value rgb_buf[j * rowstride
;;; + i * 3 + 2].
;;; 
;;; drawable :
;;; 	The GdkDrawable to draw in (usually a GdkWindow).
;;; 
;;; gc :
;;; 	The graphics context (all GDK drawing operations require one; its
;;;     contents are ignored).
;;; 
;;; x :
;;; 	The x coordinate of the top-left corner in the drawable.
;;; 
;;; y :
;;; 	The y coordinate of the top-left corner in the drawable.
;;; 
;;; width :
;;; 	The width of the rectangle to be drawn.
;;; 
;;; height :
;;; 	The height of the rectangle to be drawn.
;;; 
;;; dith :
;;; 	A GdkRgbDither value, selecting the desired dither mode.
;;; 
;;; rgb_buf :
;;; 	The pixel data, represented as packed 24-bit data.
;;; 
;;; rowstride :
;;; 	The number of bytes from the start of one row in rgb_buf to the start
;;;     of the next.
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_draw_rgb_image" gdk-draw-rgb-image) :void
  (drawable (g-object gdk-drawable))
  (gc (g-object gdk-gc))
  (x :int)
  (y :int)
  (width :int)
  (height :int)
  (dither gdk-rgb-dither)
  (rgb-buf :pointer)
  (rowstride :int))

(export 'gdk-draw-rgb-image)

;;; ----------------------------------------------------------------------------
;;; gdk_draw_rgb_image_dithalign ()
;;; 
;;; void gdk_draw_rgb_image_dithalign (GdkDrawable *drawable,
;;;                                    GdkGC *gc,
;;;                                    gint x,
;;;                                    gint y,
;;;                                    gint width,
;;;                                    gint height,
;;;                                    GdkRgbDither dith,
;;;                                    const guchar *rgb_buf,
;;;                                    gint rowstride,
;;;                                    gint xdith,
;;;                                    gint ydith);
;;; 
;;; Warning
;;; 
;;; gdk_draw_rgb_image_dithalign is deprecated and should not be used in
;;; newly-written code.
;;; 
;;; Draws an RGB image in the drawable, with an adjustment for dither alignment.
;;; 
;;; This function is useful when drawing dithered images into a window that may
;;; be scrolled. Pixel (x, y) will be drawn dithered as if its actual location
;;; is (x + xdith, y + ydith). Thus, if you draw an image into a window using
;;; zero dither alignment, then scroll up one pixel, subsequent draws to the
;;; window should have ydith = 1.
;;; 
;;; Setting the dither alignment correctly allows updating of small parts of
;;; the screen while avoiding visible "seams" between the different dither
;;; textures.
;;; 
;;; drawable :
;;; 	The GdkDrawable to draw in (usually a GdkWindow).
;;; 
;;; gc :
;;; 	The graphics context.
;;; 
;;; x :
;;; 	The x coordinate of the top-left corner in the drawable.
;;; 
;;; y :
;;; 	The y coordinate of the top-left corner in the drawable.
;;; 
;;; width :
;;; 	The width of the rectangle to be drawn.
;;; 
;;; height :
;;; 	The height of the rectangle to be drawn.
;;; 
;;; dith :
;;; 	A GdkRgbDither value, selecting the desired dither mode.
;;; 
;;; rgb_buf :
;;; 	The pixel data, represented as packed 24-bit data.
;;; 
;;; rowstride :
;;; 	The number of bytes from the start of one row in rgb_buf to the start of the next.
;;; 
;;; xdith :
;;; 	An x offset for dither alignment.
;;; 
;;; ydith :
;;; 	A y offset for dither alignment.
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_draw_rgb_image_dithalign" gdk-draw-rgb-image-dithalign) :void
  (drawable (g-object gdk-drawable))
  (gc (g-object gdk-gc))
  (x :int)
  (y :int)
  (width :int)
  (height :int)
  (dither gdk-rgb-dither)
  (rgb-buf :pointer)
  (rowstride :int)
  (x-dith :int)
  (y-dith :int))

(export 'gdk-draw-rgb-image-dithalign)

;;; ----------------------------------------------------------------------------
;;; struct GdkRgbCmap
;;; 
;;; struct GdkRgbCmap {
;;;   guint32 colors[256];
;;;   gint n_colors;
;;; };
;;; 
;;; Warning
;;; 
;;; GdkRgbCmap is deprecated and should not be used in newly-written code.
;;; 
;;; A private data structure which maps color indices to actual RGB colors.
;;; This is used only for gdk_draw_indexed_image().
;;; 
;;; guint32 colors[256];
;;; 	The colors, represented as 0xRRGGBB integer values.
;;; 
;;; gint n_colors;
;;; 	The number of colors in the cmap.
;;; ----------------------------------------------------------------------------

(define-g-boxed-cstruct gdk-rgb-cmap nil
  (colors :uint32 
          :count 256
          :initform (make-array 256
                                :element-type '(unsigned-byte 32)
                                :initial-element 0))
  (n-colors :int :initform 0))

;;; ----------------------------------------------------------------------------
;;; gdk_draw_indexed_image ()
;;; 
;;; void gdk_draw_indexed_image (GdkDrawable *drawable,
;;;                              GdkGC *gc,
;;;                              gint x,
;;;                              gint y,
;;;                              gint width,
;;;                              gint height,
;;;                              GdkRgbDither dith,
;;;                              const guchar *buf,
;;;                              gint rowstride,
;;;                              GdkRgbCmap *cmap);
;;; 
;;; Warning
;;; 
;;; gdk_draw_indexed_image is deprecated and should not be used in
;;; newly-written code.
;;; 
;;; Draws an indexed image in the drawable, using a GdkRgbCmap to assign actual
;;; colors to the color indices.
;;; 
;;; drawable :
;;; 	The GdkDrawable to draw in (usually a GdkWindow).
;;; 
;;; gc :
;;; 	The graphics context.
;;; 
;;; x :
;;; 	The x coordinate of the top-left corner in the drawable.
;;; 
;;; y :
;;; 	The y coordinate of the top-left corner in the drawable.
;;; 
;;; width :
;;; 	The width of the rectangle to be drawn.
;;; 
;;; height :
;;; 	The height of the rectangle to be drawn.
;;; 
;;; dith :
;;; 	A GdkRgbDither value, selecting the desired dither mode.
;;; 
;;; buf :
;;; 	The pixel data, represented as 8-bit color indices.
;;; 
;;; rowstride :
;;; 	The number of bytes from the start of one row in buf to the start of the next.
;;; 
;;; cmap :
;;; 	The GdkRgbCmap used to assign colors to the color indices.
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_draw_indexed_image" gdk-draw-indexed-image) :void
  (drawable (g-object gdk-drawable))
  (gc (g-object gdk-gc))
  (x :int)
  (y :int)
  (width :int)
  (height :int)
  (dither gdk-rgb-dither)
  (buf :pointer)
  (rowstring :int)
  (cmap (g-boxed-foreign gdk-rgb-cmap)))

(export 'gdk-draw-indexed-image)

;;; ----------------------------------------------------------------------------
;;; gdk_draw_gray_image ()
;;; 
;;; void gdk_draw_gray_image (GdkDrawable *drawable,
;;;                           GdkGC *gc,
;;;                           gint x,
;;;                           gint y,
;;;                           gint width,
;;;                           gint height,
;;;                           GdkRgbDither dith,
;;;                           const guchar *buf,
;;;                           gint rowstride);
;;; 
;;; Warning
;;; 
;;; gdk_draw_gray_image is deprecated and should not be used in newly-written
;;; code.
;;; 
;;; Draws a grayscale image in the drawable.
;;; 
;;; drawable :
;;; 	The GdkDrawable to draw in (usually a GdkWindow).
;;; 
;;; gc :
;;; 	The graphics context.
;;; 
;;; x :
;;; 	The x coordinate of the top-left corner in the drawable.
;;; 
;;; y :
;;; 	The y coordinate of the top-left corner in the drawable.
;;; 
;;; width :
;;; 	The width of the rectangle to be drawn.
;;; 
;;; height :
;;; 	The height of the rectangle to be drawn.
;;; 
;;; dith :
;;; 	A GdkRgbDither value, selecting the desired dither mode.
;;; 
;;; buf :
;;; 	The pixel data, represented as 8-bit gray values.
;;; 
;;; rowstride :
;;; 	The number of bytes from the start of one row in buf to the start of
;;;     the next.
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_draw_gray_image" gdk-draw-gray-image) :void
  (drawable (g-object gdk-drawable))
  (gc (g-object gdk-gc))
  (x :int)
  (y :int)
  (width :int)
  (height :int)
  (dith gdk-rgb-dither)
  (buf :pointer)
  (rowstride :int))

(export 'gdk-draw-gray-image)

;;; ----------------------------------------------------------------------------
;;; gdk_draw_rgb_32_image ()
;;; 
;;; void gdk_draw_rgb_32_image (GdkDrawable *drawable,
;;;                             GdkGC *gc,
;;;                             gint x,
;;;                             gint y,
;;;                             gint width,
;;;                             gint height,
;;;                             GdkRgbDither dith,
;;;                             const guchar *buf,
;;;                             gint rowstride);
;;; 
;;; Warning
;;; 
;;; gdk_draw_rgb_32_image is deprecated and should not be used in newly-written
;;; code.
;;; 
;;; Draws a padded RGB image in the drawable. The image is stored as one pixel
;;; per 32-bit word. It is laid out as a red byte, a green byte, a blue byte,
;;; and a padding byte.
;;; 
;;; It's unlikely that this function will give significant performance gains in
;;; practice. In my experience, the performance gain from having pixels aligned
;;; to 32-bit boundaries is cancelled out by the increased memory bandwidth.
;;; 
;;; drawable :
;;; 	The GdkDrawable to draw in (usually a GdkWindow).
;;; 
;;; gc :
;;; 	The graphics context.
;;; 
;;; x :
;;; 	The x coordinate of the top-left corner in the drawable.
;;; 
;;; y :
;;; 	The y coordinate of the top-left corner in the drawable.
;;; 
;;; width :
;;; 	The width of the rectangle to be drawn.
;;; 
;;; height :
;;; 	The height of the rectangle to be drawn.
;;; 
;;; dith :
;;; 	A GdkRgbDither value, selecting the desired dither mode.
;;; 
;;; buf :
;;; 	The pixel data, represented as padded 32-bit data.
;;; 
;;; rowstride :
;;; 	The number of bytes from the start of one row in buf to the start of
;;;     the next.
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_draw_rgb_32_image" gdk-draw-rgb-32-image) :void
  (drawable (g-object gdk-drawable))
  (gc (g-object gdk-gc))
  (x :int)
  (y :int)
  (width :int)
  (height :int)
  (dither gdk-rgb-dither)
  (buf :pointer)
  (rowstride :int))

(export 'gdk-draw-rgb-32-image)

;;; ----------------------------------------------------------------------------
;;; gdk_draw_rgb_32_image_dithalign ()
;;; 
;;; void gdk_draw_rgb_32_image_dithalign (GdkDrawable *drawable,
;;;                                       GdkGC *gc,
;;;                                       gint x,
;;;                                       gint y,
;;;                                       gint width,
;;;                                       gint height,
;;;                                       GdkRgbDither dith,
;;;                                       const guchar *buf,
;;;                                       gint rowstride,
;;;                                       gint xdith,
;;;                                       gint ydith);
;;; Warning
;;; 
;;; gdk_draw_rgb_32_image_dithalign has been deprecated since version 2.22 and
;;; should not be used in newly-written code. Cairo handles colors
;;; automatically.
;;; 
;;; Like gdk_draw_rgb_32_image(), but allows you to specify the dither offsets.
;;; See gdk_draw_rgb_image_dithalign() for more details.
;;; 
;;; drawable :
;;; 	a GdkDrawable
;;; 
;;; gc :
;;; 	a GdkGC
;;; 
;;; x :
;;; 	X coordinate on drawable where image should go
;;; 
;;; y :
;;; 	Y coordinate on drawable where image should go
;;; 
;;; width :
;;; 	width of area of image to draw
;;; 
;;; height :
;;; 	height of area of image to draw
;;; 
;;; dith :
;;; 	dithering mode
;;; 
;;; buf :
;;; 	RGB image data
;;; 
;;; rowstride :
;;; 	rowstride of RGB image data
;;; 
;;; xdith :
;;; 	X dither offset
;;; 
;;; ydith :
;;; 	Y dither offset
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_draw_rgb_32_image_dithalign" gdk-draw-rgb-32-image-dithalign)
    :void
  (drawable (g-object gdk-drawable))
  (gc (g-object gdk-gc))
  (x :int)
  (y :int)
  (width :int)
  (height :int)
  (dither gdk-rgb-dither)
  (buf :pointer)
  (rowstride :int)
  (xdith :int)
  (ydith :int))

(export 'gdk-draw-rgb-32-image-dithalign)

;;; ----------------------------------------------------------------------------
;;; gdk_rgb_cmap_new ()
;;; 
;;; GdkRgbCmap * gdk_rgb_cmap_new (guint32 *colors, gint n_colors);
;;; 
;;; Warning
;;; 
;;; gdk_rgb_cmap_new is deprecated and should not be used in newly-written code.
;;; 
;;; Creates a new GdkRgbCmap structure. The cmap maps color indexes to RGB
;;; colors. If n_colors is less than 256, then images containing color values
;;; greater than or equal to n_colors will produce undefined results, including
;;; possibly segfaults.
;;; 
;;; colors :
;;; 	The colors, represented as 0xRRGGBB integer values.
;;; 
;;; n_colors :
;;; 	The number of colors in the cmap.
;;; 
;;; Returns :
;;; 	The newly created GdkRgbCmap
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_rgb_cmap_free ()
;;; 
;;; void gdk_rgb_cmap_free (GdkRgbCmap *cmap);
;;; 
;;; Warning
;;; 
;;; gdk_rgb_cmap_free is deprecated and should not be used in newly-written
;;; code.
;;; 
;;; Frees the memory associated with a GdkRgbCmap created by gdk_rgb_cmap_new().
;;; 
;;; cmap :
;;; 	The GdkRgbCmap to free.
;;; ----------------------------------------------------------------------------


;;; ----------------------------------------------------------------------------
;;; gdk_rgb_gc_set_foreground ()
;;; 
;;; void gdk_rgb_gc_set_foreground (GdkGC *gc, guint32 rgb);
;;; 
;;; Warning
;;; 
;;; gdk_rgb_gc_set_foreground is deprecated and should not be used in
;;; newly-written code.
;;; 
;;; Sets the foreground color in gc to the specified color (or the closest
;;; approximation, in the case of limited visuals).
;;; 
;;; gc :
;;; 	The GdkGC to modify.
;;; 
;;; rgb :
;;; 	The color, represented as a 0xRRGGBB integer value.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_rgb_gc_set_background ()
;;; 
;;; void gdk_rgb_gc_set_background (GdkGC *gc, guint32 rgb);
;;; 
;;; Warning
;;; 
;;; gdk_rgb_gc_set_background is deprecated and should not be used in
;;; newly-written code.
;;; 
;;; Sets the background color in gc to the specified color (or the closest
;;; approximation, in the case of limited visuals).
;;; 
;;; gc :
;;; 	The GdkGC to modify.
;;; 
;;; rgb :
;;; 	The color, represented as a 0xRRGGBB integer value.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_rgb_xpixel_from_rgb ()
;;; 
;;; gulong gdk_rgb_xpixel_from_rgb (guint32 rgb);
;;; 
;;; Warning
;;; 
;;; gdk_rgb_xpixel_from_rgb is deprecated and should not be used in
;;; newly-written code.
;;; 
;;; Finds the X pixel closest in color to the rgb color specified. This value
;;; may be used to set the pixel field of a GdkColor struct.
;;; 
;;; rgb :
;;; 	The color, represented as a 0xRRGGBB integer value.
;;; 
;;; Returns :
;;; 	The X pixel value.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_rgb_find_color ()
;;; 
;;; void gdk_rgb_find_color (GdkColormap *colormap, GdkColor *color);
;;; 
;;; Warning
;;; 
;;; gdk_rgb_find_color has been deprecated since version 2.22 and should not be
;;; used in newly-written code. Cairo handles colors automatically.
;;; 
;;; colormap should be the colormap for the graphics context and drawable
;;; you're using to draw. If you're drawing to a GtkWidget, call
;;; gtk_widget_get_colormap().
;;; 
;;; color should have its red, green, and blue fields initialized;
;;; gdk_rgb_find_color() will fill in the pixel field with the best matching
;;; pixel from a color cube. The color is then ready to be used for drawing,
;;; e.g. you can call gdk_gc_set_foreground() which expects pixel to be
;;; initialized.
;;; 
;;; In many cases, you can avoid this whole issue by calling
;;; gdk_gc_set_rgb_fg_color() or gdk_gc_set_rgb_bg_color(), which do not expect
;;; pixel to be initialized in advance. If you use those functions, there's no
;;; need for gdk_rgb_find_color().
;;; 
;;; colormap :
;;; 	a GdkColormap
;;; 
;;; color :
;;; 	a GdkColor
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_rgb_find_color" gdk-rgb-find-color) :void
  (colormap (g-object gdk-colormap))
  (color (g-boxed-foreign gdk-color)))

(export 'gdk-rgb-find-color)

;;; ----------------------------------------------------------------------------
;;; gdk_rgb_set_install ()
;;; 
;;; void gdk_rgb_set_install (gboolean install);
;;; 
;;; Warning
;;; 
;;; gdk_rgb_set_install is deprecated and should not be used in newly-written
;;; code.
;;; 
;;; If install is TRUE, directs GdkRGB to always install a new "private"
;;; colormap rather than trying to find a best fit with the colors already
;;; allocated. Ordinarily, GdkRGB will install a colormap only if a sufficient
;;; cube cannot be allocated.
;;; 
;;; A private colormap has more colors, leading to better quality display, but
;;; also leads to the dreaded "colormap flashing" effect.
;;; 
;;; install :
;;; 	TRUE to set install mode.
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_rgb_set_install" gdk-rgb-set-install) :void
  (install :boolean))

(export 'gdk-rgb-set-install)

;;; ----------------------------------------------------------------------------
;;; gdk_rgb_set_min_colors ()
;;; 
;;; void gdk_rgb_set_min_colors (gint min_colors);
;;; 
;;; Warning
;;; 
;;; gdk_rgb_set_min_colors is deprecated and should not be used in
;;; newly-written code.
;;; 
;;; Sets the minimum number of colors for the color cube. Generally, GdkRGB
;;; tries to allocate the largest color cube it can. If it can't allocate a
;;; color cube at least as large as min_colors, it installs a private colormap.
;;; 
;;; min_colors :
;;; 	The minimum number of colors accepted.
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_rgb_set_min_colors" gdk-rgb-set-min-colors) :void
  (min-colors :int))

(export 'gdk-rgb-set-min-colors)

;;; ----------------------------------------------------------------------------
;;; gdk_rgb_get_visual ()
;;; 
;;; GdkVisual * gdk_rgb_get_visual (void);
;;; 
;;; Warning
;;; 
;;; gdk_rgb_get_visual has been deprecated since version 2.22 and should not be
;;; used in newly-written code. Use gdk_visual_get_system_visual
;;; (gdk_screen_get_default()) instead.
;;; 
;;; Gets a "preferred visual" chosen by GdkRGB for rendering image data on the
;;; default screen. In previous versions of GDK, this was the only visual
;;; GdkRGB could use for rendering. In current versions, it's simply the visual
;;; GdkRGB would have chosen as the optimal one in those previous versions.
;;; GdkRGB can now render to drawables with any visual.
;;; 
;;; Returns :
;;; 	The GdkVisual chosen by GdkRGB. [transfer none]
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_rgb_get_visual" gdk-rgb-get-visual) (g-object gdk-visual))

(export 'gdk-rgb-get-visual)

;;; ----------------------------------------------------------------------------
;;; gdk_rgb_get_colormap ()
;;; 
;;; GdkColormap * gdk_rgb_get_colormap (void);
;;; 
;;; Warning
;;; 
;;; gdk_rgb_get_colormap has been deprecated since version 2.22 and should not
;;; be used in newly-written code. Use gdk_screen_get_system_colormap
;;; (gdk_screen_get_default()) instead.
;;; 
;;; Get the preferred colormap for rendering image data. Not a very useful
;;; function; historically, GDK could only render RGB image data to one
;;; colormap and visual, but in the current version it can render to any
;;; colormap and visual. So there's no need to call this function.
;;; 
;;; Returns :
;;; 	the preferred colormap. [transfer none]
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_rgb_get_colormap" gdk-rgb-get-colormap) (g-object gdk-colormap))

(export 'gdk-rgb-get-colormap)

;;; ----------------------------------------------------------------------------
;;; gdk_rgb_get_cmap
;;; 
;;; #define gdk_rgb_get_cmap gdk_rgb_get_colormap
;;; 
;;; Warning
;;; 
;;; gdk_rgb_get_cmap is deprecated and should not be used in newly-written code.
;;; 
;;; Gets the colormap set by GdkRGB. This colormap and the corresponding visual
;;; should be used when creating windows that will be drawn in by GdkRGB.
;;; 
;;; Returns :
;;; 	The GdkColormap set by GdkRGB.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_rgb_ditherable ()
;;; 
;;; gboolean gdk_rgb_ditherable (void);
;;; 
;;; Warning
;;; 
;;; gdk_rgb_ditherable is deprecated and should not be used in newly-written
;;; code.
;;; 
;;; Determines whether the preferred visual is ditherable. This function may be
;;; useful for presenting a user interface choice to the user about which
;;; dither mode is desired; if the display is not ditherable, it may make sense
;;; to gray out or hide the corresponding UI widget.
;;; 
;;; Returns :
;;; 	TRUE if the preferred visual is ditherable.
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_rgb_ditherable" gdk-rgb-ditherable) :boolean)

(export 'gdk-rgb-ditherable)

;;; ----------------------------------------------------------------------------
;;; gdk_rgb_colormap_ditherable ()
;;; 
;;; gboolean gdk_rgb_colormap_ditherable (GdkColormap *cmap);
;;; 
;;; Warning
;;; 
;;; gdk_rgb_colormap_ditherable is deprecated and should not be used in
;;; newly-written code.
;;; 
;;; Determines whether the visual associated with cmap is ditherable. This
;;; function may be useful for presenting a user interface choice to the user
;;; about which dither mode is desired; if the display is not ditherable, it
;;; may make sense to gray out or hide the corresponding UI widget.
;;; 
;;; cmap :
;;; 	a GdkColormap
;;; 
;;; Returns :
;;; 	TRUE if the visual associated with cmap is ditherable.
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_rgb_colormap_ditherable" gdk-rgb-colormap-ditherable) :boolean
  (colormap (g-object gdk-colormap)))

(export 'gdk-rgb-colormap-ditherable)

;;; ----------------------------------------------------------------------------
;;; gdk_rgb_set_verbose ()
;;; 
;;; void gdk_rgb_set_verbose (gboolean verbose);
;;; 
;;; Warning
;;; 
;;; gdk_rgb_set_verbose is deprecated and should not be used in newly-written
;;; code.
;;; 
;;; Sets the "verbose" flag. This is generally only useful for debugging.
;;; 
;;; verbose :
;;; 	TRUE if verbose messages are desired.
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_rgb_set_verbose" gdk-rgb-set-verbose) :void
  (verbose :boolean))

(export 'gdk-rgb-set-verbose)

;;; --- End of file gdk.rgb.lisp -----------------------------------------------
