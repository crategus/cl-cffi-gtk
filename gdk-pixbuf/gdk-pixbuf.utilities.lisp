;;; ----------------------------------------------------------------------------
;;; gdk-pixbuf.utilities.lisp
;;;
;;; The documentation of this file is taken from the GDK-PixBuf Reference Manual
;;; Version 2.36 and modified to document the Lisp binding to the GDK-PixBuf
;;; library. See <http://www.gtk.org>. The API documentation of the Lisp
;;; binding is available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2013 - 2021 Dieter Kaiser
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
;;; Utilities
;;;
;;;     Utility and miscellaneous convenience functions.
;;;
;;; Functions
;;;
;;;     gdk_pixbuf_add_alpha
;;;     gdk_pixbuf_copy_area
;;;     gdk_pixbuf_saturate_and_pixelate
;;;     gdk_pixbuf_apply_embedded_orientation
;;;     gdk_pixbuf_fill
;;;
;;; Description
;;;
;;; These functions provide miscellaneous utilities for manipulating pixbufs.
;;; The pixel data in pixbufs may of course be manipulated directly by
;;; applications, but several common operations can be performed by these
;;; functions instead.
;;; ----------------------------------------------------------------------------

(in-package :gdk-pixbuf)

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_add_alpha ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_pixbuf_add_alpha" gdk-pixbuf-add-alpha) (g-object gdk-pixbuf)
 #+cl-cffi-gtk-documentation
 "@version{2021-12-12}
  @argument[pixbuf]{a @class{gdk-pixbuf} object}
  @argument[substitute]{a boolean whether to set a color to zero opacity, if
    this is @em{false}, then the (@arg{red}, @arg{green}, @arg{blue}) arguments
    will be ignored}
  @argument[red]{an unsigned char with the red value to substitute}
  @argument[green]{an unsigned char with the green value to substitute}
  @argument[blue]{an unsigned char with the blue value to substitute}
  @return{A newly created pixbuf with a reference count of 1.}
  @begin{short}
    Takes an existing @arg{pixbuf} and adds an alpha channel to it.
  @end{short}
  If the existing @arg{pixbuf} already had an alpha channel, the channel values
  are copied from the original; otherwise, the alpha channel is initialized to
  255 (full opacity).

  If the @arg{substitute} argument is @em{true}, then the color specified by
  (@arg{red}, @arg{green}, @arg{blue}) will be assigned zero opacity. That is,
  if you pass (255, 255, 255) for the substitute color, all white pixels will
  become fully transparent.
  @see-class{gdk-pixbuf}"
  (pixbuf (g-object gdk-pixbuf))
  (substitute :boolean)
  (red :uchar)
  (green :uchar)
  (blue :uchar))

(export 'gdk-pixbuf-add-alpha)

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_copy_area ()
;;; ----------------------------------------------------------------------------

;; TODO: Change the implementation to return the dest pixbuf!?

(defcfun ("gdk_pixbuf_copy_area" gdk-pixbuf-copy-area) :void
 #+cl-cffi-gtk-documentation
 "@version{*2021-12-12}
  @argument[src]{a @class{gdk-pixbuf} object}
  @argument[xsrc]{an integer with the source x coordinate within @arg{src}}
  @argument[ysrc]{an integer with the source y coordinate within @arg{src}}
  @argument[width]{an integer with the width of the area to copy}
  @argument[height]{an integer with the height of the area to copy}
  @argument[dest]{a @class{gdk-pixbuf} destination object}
  @argument[xdest]{an integer with the x coordinate within @arg{dest}}
  @argument[ydest]{an integer with the y coordinate within @arg{dest}}
  @begin{short}
    Copies a rectangular area from @arg{src} to @arg{dest}.
  @end{short}
  Conversion of pixbuf formats is done automatically.

  If the source rectangle overlaps the destination rectangle on the same pixbuf,
  it will be overwritten during the copy operation. Therefore, you can not use
  this function to scroll a pixbuf.
  @see-class{gdk-pixbuf}"
  (src (g-object gdk-pixbuf))
  (xsrc :int)
  (ysrc :int)
  (width :int)
  (height :int)
  (dest (g-object gdk-pixbuf))
  (xdest :int)
  (ydest :int))

(export 'gdk-pixbuf-copy-area)

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_saturate_and_pixelate ()
;;;
;;; void gdk_pixbuf_saturate_and_pixelate (const GdkPixbuf *src,
;;;                                        GdkPixbuf *dest,
;;;                                        gfloat saturation,
;;;                                        gboolean pixelate);
;;;
;;; Modifies saturation and optionally pixelates src, placing the result in
;;; dest. src and dest may be the same pixbuf with no ill effects. If saturation
;;; is 1.0 then saturation is not changed. If it's less than 1.0, saturation is
;;; reduced (the image turns toward grayscale); if greater than 1.0, saturation
;;; is increased (the image gets more vivid colors). If pixelate is TRUE, then
;;; pixels are faded in a checkerboard pattern to create a pixelated image. src
;;; and dest must have the same image format, size, and rowstride.
;;;
;;; src :
;;;     source image
;;;
;;; dest :
;;;     place to write modified version of src
;;;
;;; saturation :
;;;     saturation factor
;;;
;;; pixelate :
;;;     whether to pixelate
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_apply_embedded_orientation ()
;;;
;;; GdkPixbuf * gdk_pixbuf_apply_embedded_orientation (GdkPixbuf *src);
;;;
;;; Takes an existing pixbuf and checks for the presence of an associated
;;; "orientation" option, which may be provided by the jpeg loader (which reads
;;; the exif orientation tag) or the tiff loader (which reads the tiff
;;; orientation tag, and compensates it for the partial transforms performed by
;;; libtiff). If an orientation option/tag is present, the appropriate transform
;;; will be performed so that the pixbuf is oriented correctly.
;;;
;;; src :
;;;     A GdkPixbuf.
;;;
;;; Returns :
;;;     A newly-created pixbuf, or a reference to the input pixbuf (with an
;;;     increased reference count). [transfer full]
;;;
;;; Since 2.12
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_fill ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_pixbuf_fill" gdk-pixbuf-fill) :void
 #+cl-cffi-gtk-documentation
 "@version{*2021-12-12}
  @argument[pixbuf]{a @class{gdk-pixbuf} object}
  @argument[pixel]{an unsigned integer with the RGBA pixel to clear to,
    @code{#xffffffff} is opaque white, @code{#x00000000} transparent black}
  @begin{short}
    Clears a pixbuf to the given RGBA value, converting the RGBA value
    into the pixel format of the pixbuf.
  @end{short}
  The alpha will be ignored if the pixbuf does not have an alpha channel.
  @see-class{gdk-pixbuf}"
  (pixbuf (g-object gdk-pixbuf))
  (pixel :uint32))

(export 'gdk-pixbuf-fill)

;;; --- End of file gdk-pixbuf.utilities.lisp ----------------------------------
