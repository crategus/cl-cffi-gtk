;;; ----------------------------------------------------------------------------
;;; gdk.pixbuf-package.lisp
;;;
;;; The documentation has been copied from the GDK-PixBuf Reference Manual
;;; Version 2.26.1. See <http://www.gtk.org>. The API documentation of the
;;; Lisp binding is available at <http://www.crategus.com/books/cl-cffi-gtk/>.
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

(declaim #+sbcl(sb-ext:muffle-conditions style-warning))

(defpackage :gdk-pixbuf
  (:use :cl :gobject :glib :cffi :iter))

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (find-package :gdk-pixbuf) t)
 "GDK-Pixbuf is a library for image loading and manipulation. The GDK-Pixbuf
  documentation contains both the programmer's guide and the API reference.
  This is the API documentation of a Lisp binding to GDK-Pixbuf.
  @begin[Library version numbers]{section}
    These macros and variables let you check the version of GDK-Pixbuf you are
    linking against.

    @about-symbol{+gdk-pixbuf-version+}
    @about-symbol{+gdk-pixbuf-major-version+}
    @about-symbol{+gdk-pixbuf-minor-version+}
    @about-symbol{+gdk-pixbuf-micro-version+}
  @end{section}
  @begin[The GdkPixbuf Structure]{section}
    Information that describes an image.

    @about-symbol{GdkPixbufError}
    @about-symbol{GDK_PIXBUF_ERROR}
    @about-symbol{gdk-colorspace}
    @about-symbol{gdk-pixbuf-alpha-mode}
    @about-class{gdk-pixbuf}
    @about-function{gdk-pixbuf-get-colorspace}
    @about-function{gdk-pixbuf-get-n-channels}
    @about-function{gdk-pixbuf-get-has-alpha}
    @about-function{gdk-pixbuf-get-bits-per-sample}
    @about-function{gdk-pixbuf-get-pixels}
    @about-function{gdk-pixbuf-get-pixels-with-length}
    @about-function{gdk-pixbuf-get-width}
    @about-function{gdk-pixbuf-get-height}
    @about-function{gdk-pixbuf-get-rowstride}
    @about-function{gdk-pixbuf-get-byte-length}
    @about-function{gdk-pixbuf-get-option}
  @end{section}
  @begin[File Loading]{section}
    Loading a pixbuf from a file.

    The GDK-Pixbuf library provides a simple mechanism for loading an image
    from a file in synchronous fashion. This means that the library takes
    control of the application while the file is being loaded; from the user's
    point of view, the application will block until the image is done loading.

    This interface can be used by applications in which blocking is acceptable
    while an image is being loaded. It can also be used to load small images in
    general. Applications that need progressive loading can use the
    @code{GdkPixbufLoader} functionality instead.

    @about-function{gdk-pixbuf-new-from-file}
    @about-function{gdk-pixbuf-new-from-file-at-size}
    @about-function{gdk-pixbuf-new-from-file-at-scale}
    @about-function{gdk-pixbuf-get-file-info}
    @about-function{gdk-pixbuf-new-from-stream}
    @about-function{gdk-pixbuf-new-from-stream-at-scale}
  @end{section}
  @begin[File Saving]{section}
    Saving a pixbuf to a file.

    These functions allow to save a @class{gdk-pixbuf} object in a number of
    file formats. The formatted data can be written to a file or to a memory
    buffer. @class{gdk-pixbuf} can also call a user-defined callback on the
    data, which allows to e. g. write the image to a socket or store it in a
    database.

    @about-function{gdk-pixbuf-savev}
    @about-function{gdk-pixbuf-save}
    @about-function{gdk-pixbuf-save-to-callback}
    @about-function{gdk-pixbuf-save-to-callbackv}
    @about-function{gdk-pixbuf-save-to-buffer}
    @about-function{gdk-pixbuf-save-to-bufferv}
    @about-function{gdk-pixbuf-save-to-stream}
  @end{section}
  @begin[Image Data in Memory]{section}
    Creating a pixbuf from image data that is already in memory.

    The most basic way to create a pixbuf is to wrap an existing pixel buffer
    with a @class{gdk-pixbuf} structure. You can use the function
    @fun{gdk-pixbuf-new-from-data} to do this. You need to specify the destroy
    notification function that will be called when the data buffer needs to be
    freed; this will happen when a @class{gdk-pixbuf} is finalized by the
    reference counting functions If you have a chunk of static data compiled
    into your application, you can pass in @code{nil} as the destroy
    notification function so that the data will not be freed.

    The function @fun{gdk-pixbuf-new} can be used as a convenience to create a
    pixbuf with an empty buffer. This is equivalent to allocating a data buffer
    using @code{malloc()} and then wrapping it with the function
    @fun{gdk-pixbuf-new-from-data}. The function @fun{gdk-pixbuf-new} will
    compute an optimal rowstride so that rendering can be performed with an
    efficient algorithm.

    As a special case, you can use the function
    @fun{gdk-pixbuf-new-from-xpm_data} to create a pixbuf from inline XPM image
    data.

    You can also copy an existing pixbuf with the function
    @fun{gdk-pixbuf-copy}. This is not the same as just doing a
    @fun{g-object-ref} on the old pixbuf; the copy function will actually
    duplicate the pixel data in memory and create a new @class{gdk-pixbuf}
    structure for it.

    @about-function{gdk-pixbuf-new}
    @about-function{gdk-pixbuf-from-data}
    @about-function{gdk-pixbuf-new-from-xpm-data}
    @about-function{gdk-pixbuf-new-from-inline}
    @about-function{gdk-pixbuf-new-subpixbuf}
    @about-function{gdk-pixbuf-copy}
  @end{section}
  @begin[Scaling]{section}
    Scaling pixbufs and scaling and compositing pixbufs.

    The @class{gdk-pixbuf} contains functions to scale pixbufs, to scale pixbufs
    and composite against an existing image, and to scale pixbufs and composite
    against a solid color or checkerboard. Compositing a checkerboard is a
    common way to show an image with an alpha channel in image-viewing and
    editing software.

    Since the full-featured functions (@fun{gdk-pixbuf-scale},
    @fun{gdk-pixbuf-composite}, and @fun{gdk-pixbuf-composite-color}) are rather
    complex to use and have many arguments, two simple convenience functions are
    provided, @fun{gdk-pixbuf-scale-simple} and
    @fun{gdk-pixbuf-composite-color-simple} which create a new pixbuf of a given
    size, scale an original image to fit, and then return the new pixbuf.

    Scaling and compositing functions take advantage of MMX hardware
    acceleration on systems where MMX is supported. If @class{gdk-pixbuf} is
    built with the Sun mediaLib library, these functions are instead accelerated
    using mediaLib, which provides hardware acceleration on Intel, AMD, and
    Sparc chipsets. If desired, mediaLib support can be turned off by setting
    the @code{GDK_DISABLE_MEDIALIB} environment variable.

    The following example demonstrates handling an expose event by rendering the
    appropriate area of a source image (which is scaled to fit the widget) onto
    the widget's window. The source image is rendered against a checkerboard,
    which provides a visual representation of the alpha channel if the image has
    one. If the image does not have an alpha channel, calling
    @fun{gdk-pixbuf-composite-color} function has exactly the same effect as
    calling the function @fun{gdk-pixbuf-scale}.

    @b{Example:} Handling an expose event.
    @begin{pre}
 gboolean
 expose_cb (GtkWidget *widget, GdkEventExpose *event, gpointer data)
 {
   GdkPixbuf *dest;

   dest = gdk_pixbuf_new (GDK_COLORSPACE_RGB, FALSE, 8, event->area.width, event->area.height);

   gdk_pixbuf_composite_color (pixbuf, dest,
                               0, 0, event->area.width, event->area.height,
                               -event->area.x, -event->area.y,
                               (double) widget->allocation.width / gdk_pixbuf_get_width (pixbuf),
                               (double) widget->allocation.height / gdk_pixbuf_get_height (pixbuf),
                               GDK_INTERP_BILINEAR, 255,
                               event->area.x, event->area.y, 16, 0xaaaaaa, 0x555555);

   gdk_draw_pixbuf (widget->window, widget->style->fg_gc[GTK_STATE_NORMAL], dest,
                    0, 0, event->area.x, event->area.y,
                    event->area.width, event->area.height,
                    GDK_RGB_DITHER_NORMAL, event->area.x, event->area.y);

   gdk_pixbuf_unref (dest);

   return TRUE;
 @}
    @end{pre}
    @about-symbol{gdk-interp-type}
    @about-function{gdk-pixbuf-scale-simple}
    @about-function{gdk-pixbuf-scale}
    @about-function{gdk-pixbuf-composite-color-simple}
    @about-function{gdk-pixbuf-composite}
    @about-function{gdk-pixbuf-composite-color}
    @about-symbol{gdk-pixbuf-rotation}
    @about-function{gdk-pixbuf-rotate-simple}
    @about-function{gdk-pixbuf-flip}
  @end{section}
  @begin[Utilities]{section}
    Utility and miscellaneous convenience functions.

    These functions provide miscellaneous utilities for manipulating pixbufs.
    The pixel data in pixbufs may of course be manipulated directly by
    applications, but several common operations can be performed by these
    functions instead.

    @about-function{gdk-pixbuf-add-alpha}
    @about-function{gdk-pixbuf-copy-area}
    @about-function{gdk-pixbuf-saturate-and-pixelate}
    @about-function{gdk-pixbuf-apply-embedded-orientation}
    @about-function{gdk-pixbuf-fill}
  @end{section}")

;;; --- End of file gdk.pixbuf-package.lisp ------------------------------------
