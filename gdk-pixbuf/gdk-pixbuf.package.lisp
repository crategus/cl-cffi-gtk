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

    @about-function{gdk-pixbuf-new}
    @about-function{gdk-pixbuf-from-data}
    @about-function{gdk-pixbuf-new-from-xpm-data}
    @about-function{gdk-pixbuf-new-from-inline}
    @about-function{gdk-pixbuf-new-subpixbuf}
    @about-function{gdk-pixbuf-copy}
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
