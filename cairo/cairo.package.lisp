;;; ----------------------------------------------------------------------------
;;; cairo.package.lisp
;;;
;;; The documentation has been copied from the Cairo Reference Manual
;;; for Cairo 1.12.2. See <http://cairographics.org>.
;;; The API documentation of the Lisp binding is available at
;;; <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2012, 2013 Dieter Kaiser
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

(defpackage :cairo
  (:use :cl :cffi :glib))

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (find-package :cairo) t)
 "Cairo is a software library used to provide a vector graphics-based,
  device-independent API for software developers. It is designed to provide
  primitives for 2-dimensional drawing across a number of different backends.
  Cairo is designed to use hardware acceleration when available.

  This is the API documentation of a Lisp binding to Cairo. At this time only a
  few types and functions are implemented, which are needed to compile the Lisp
  bindung to GTK+.
  @begin[Version Information]{section}
    Cairo provides the ability to examine the version at either compile-time or
    run-time and in both a human-readable form as well as an encoded form
    suitable for direct comparison. Cairo also provides the function
    @fun{cairo-version-encode} to perform the encoding.

    @about-variable{+cairo-version-major+}
    @about-variable{+cairo-version-minor+}
    @about-variable{+cairo-version-micro+}
    @about-variable{+cairo-version-string+}
    @about-variable{+cairo-version+}
    @about-function{cairo-version-encode}
    @about-function{cairo-version}
    @about-function{cairo-version-string}
  @end{section}
  @begin[Drawing]{section}
    The Cairo drawing context.

    @symbol{cairo-t} is the main object used when drawing with Cairo. To
    draw with Cairo, you create a @symbol{cairo-t}, set the target surface,
    and drawing options for the @symbol{cairo-t}, create shapes with functions
    like @code{cairo_move_to()} and @code{cairo_line_to()}, and then draw shapes
    with @code{cairo_stroke()} or @fun{cairo-fill}.
 
    @symbol{cairo-t}'s can be pushed to a stack via @code{cairo_save()}. They
    may then safely be changed, without losing the current state. Use
    @code{cairo_restore()} to restore to the saved state.
  
    @about-symbol{cairo-t}
    @about-function{cairo-create}
    @about-function{cairo-reference}
    @about-function{cairo-destroy}
    @about-function{cairo-status}
    @about-function{cairo-save}
    @about-function{cairo-restore}
    @about-function{cairo-get-target}
    @about-function{cairo-push-group}
    @about-function{cairo-push-group-with-content}
    @about-function{cairo-pop-group}
    @about-function{cairo-pop-group-to-source}
    @about-function{cairo-get-group-target}
    @about-function{cairo-set-source-rgb}
    @about-function{cairo-set-source-rgba}
    @about-function{cairo-set-source}
    @about-function{cairo-set-source-surface}
    @about-function{cairo-get-source}
    @about-symbol{cairo-antialias-t}
    @about-function{cairo-set-antialias}
    @about-function{cairo-get-antialias}
    @about-function{cairo-set-dash}
    @about-function{cairo-get-dash-count}
    @about-function{cairo-get-dash}
    @about-symbol{cairo-fill-rule-t}
    @about-function{cairo-set-fill-rule}
    @about-function{cairo-get-fill-rule}
    @about-symbol{cairo-line-cap-t}
    @about-function{cairo-set-line-cap}
    @about-function{cairo-get-line-cap}
    @about-symbol{cairo-line-join-t}
    @about-function{cairo-set-line-join}
    @about-function{cairo-get-line-join}
    @about-function{cairo-set-line-width}
    @about-function{cairo-get-line-width}
    @about-function{cairo-set-miter-limit}
    @about-function{cairo-get-miter-limit}
    @about-symbol{cairo-operator-t}
    @about-function{cairo-set-operator}
    @about-function{cairo-get-operator}
    @about-function{cairo-set-tolerance}
    @about-function{cairo-get-tolerance}
    @about-function{cairo-clip}
    @about-function{cairo-clip-preserve}
    @about-function{cairo-clip-extents}
    @about-function{cairo-in-clip}
    @about-function{cairo-reset-clip}
    @about-function{cairo-rectangle-t}
    @about-function{cairo-rectangle-list-t}
    @about-function{cairo-rectangle-list-destroy}
    @about-function{cairo-copy-clip-rectangle-list}
    @about-function{cairo-fill}
    @about-function{cairo-fill-preserve}
    @about-function{cairo-fill-extents}
    @about-function{cairo-in-fill}
    @about-function{cairo-mask}
    @about-function{cairo-mask-surface}
    @about-function{cairo-paint}
    @about-function{cairo-paint-with-alpha}
    @about-function{cairo-stroke}
    @about-function{cairo-stroke-preserve}
    @about-function{cairo-stroke-extents}
    @about-function{cairo-in-stroke}
    @about-function{cairo-copy-page}
    @about-function{cairo-show-page}
    @about-function{cairo-get-reference-count}
    @about-function{cairo-set-user-data}
    @about-function{cairo-get-user-data}
  @end{section}
  @begin[Paths]{section}
    Creating paths and manipulating path data.

    Paths are the most basic drawing tools and are primarily used to implicitly
    generate simple masks.

    @about-symbol{cairo-path-t}
    @about-symbol{cairo-data-t}
    @about-symbol{cairo-data-type-t}
    @about-function{cairo-copy-path}
    @about-function{cairo-copy-path-flat}
    @about-function{cairo-path-destroy}
    @about-function{cairo-append-path}
    @about-function{cairo-has-current-point}
    @about-function{cairo-get-current-point}
    @about-function{cairo-new-path}
    @about-function{cairo-new-sub-path}
    @about-function{cairo-close-path}
    @about-function{cairo-arc}
    @about-function{cairo-arc-negative}
    @about-function{cairo-curve-to}
    @about-function{cairo-line-to}
    @about-function{cairo-move-to}
    @about-function{cairo-rectangle}
    @about-function{cairo-glyph-path}
    @about-function{cairo-text-path}
    @about-function{cairo-rel-curve-to}
    @about-function{cairo-rel-line-to}
    @about-function{cairo-rel-move-to}
    @about-function{cairo-path-extents}
  @end{section}
  @begin[Regions]{section}
    Representing a pixel-aligned area.

    Regions are a simple graphical data type representing an area of
    integer-aligned rectangles. They are often used on raster surfaces to track
    areas of interest, such as change or clip areas.

    @about-symbol{cairo-region-t}
    @about-function{cairo-region-create}
    @about-function{cairo-region-create-rectangle}
    @about-function{cairo-region-create-rectangles}
    @about-function{cairo-region-copy}
    @about-function{cairo-region-reference}
    @about-function{cairo-region-destroy}
    @about-function{cairo-region-status}
    @about-function{cairo-region-get-extents}
    @about-function{cairo-region-num-rectangles}
    @about-function{cairo-region-get-rectangle}
    @about-function{cairo-region-is-empty}
    @about-function{cairo-region-contains-point}
    @about-symbol{cairo-region-overlap-t}
    @about-function{cairo-region-contains-rectangle}
    @about-function{cairo-region-equal}
    @about-function{cairo-region-translate}
    @about-function{cairo-region-intersect}
    @about-function{cairo-region-intersect-rectangle}
    @about-function{cairo-region-subtract}
    @about-function{cairo-region-subtract-rectangle}
    @about-function{cairo-region-union}
    @about-function{cairo-region-union-rectangle}
    @about-function{cairo-region-xor}
    @about-function{cairo-region-xor-rectangle}
  @end{section}
  @begin[Surfaces]{section}
    Base class for surfaces

    cairo_surface_t is the abstract type representing all different drawing
    targets that cairo can render to. The actual drawings are performed using a
    cairo context.

    A cairo surface is created by using backend-specific constructors, typically
    of the form cairo_backend_surface_create().

    Most surface types allow accessing the surface without using Cairo
    functions. If you do this, keep in mind that it is mandatory that you call
    cairo_surface_flush() before reading from or writing to the surface and that
    you must use cairo_surface_mark_dirty() after modifying it.
    
    Example 1. Directly modifying an image surface
    @begin{pre}
 void
 modify_image_surface (cairo_surface_t *surface)
 {
   unsigned char *data;
   int width, height, stride;

   // flush to ensure all writing to the image was done
   cairo_surface_flush (surface);
 
   // modify the image
   data = cairo_image_surface_get_data (surface);
   width = cairo_image_surface_get_width (surface);
   height = cairo_image_surface_get_height (surface);
   stride = cairo_image_surface_get_stride (surface);
   modify_image_data (data, width, height, stride);

   // mark the image dirty so Cairo clears its caches.
   cairo_surface_mark_dirty (surface);
 @}
    @end{pre}
    Note that for other surface types it might be necessary to acquire the
    surface's device first. See cairo_device_acquire() for a discussion of
    devices.

    @about-symbol{CAIRO_HAS_MIME_SURFACE}
    @about-symbol{CAIRO_MIME_TYPE_JP2}
    @about-symbol{CAIRO_MIME_TYPE_JPEG}
    @about-symbol{CAIRO_MIME_TYPE_PNG}
    @about-symbol{CAIRO_MIME_TYPE_URI}
    @about-symbol{CAIRO_MIME_TYPE_UNIQUE_ID}
    @about-symbol{cairo-surface-t}
    @about-symbol{cairo-content-t}
    @about-function{cairo-surface-create-similar}
    @about-function{cairo-surface-create-similar-image}
    @about-function{cairo-surface-create-for-rectangle}
    @about-function{cairo-surface-reference}
    @about-function{cairo-surface-destroy}
    @about-function{cairo-surface-status}
    @about-function{cairo-surface-finish}
    @about-function{cairo-surface-flush}
    @about-function{cairo-surface-get-device}
    @about-function{cairo-surface-get-font-options}
    @about-function{cairo-surface-get-content}
    @about-function{cairo-surface-mark-dirty}
    @about-function{cairo-surface-mark-dirty-rectangle}
    @about-function{cairo-surface-set-device-offset}
    @about-function{cairo-surface-get-device-offset}
    @about-function{cairo-surface-set-fallback-resolution}
    @about-function{cairo-surface-get-fallback-resolution}
    @about-symbol{cairo-surface-type-t}
    @about-function{cairo-surface-get-type}
    @about-function{cairo-surface-get-reference-count}
    @about-function{cairo-surface-set-user-data}
    @about-function{cairo-surface-get-user-data}
    @about-function{cairo-surface-copy-page}
    @about-function{cairo-surface-show-page}
    @about-function{cairo-surface-has-show-text-glyphs}
    @about-function{cairo-surface-set-mime-data}
    @about-function{cairo-surface-get-mime-data}
    @about-function{cairo-surface-supports-mime-type}
    @about-function{cairo-surface-map-to-image}
    @about-function{cairo-surface-unmap-image}
  @end{section}
")

;;; --- End of file cairo.package.lisp -----------------------------------------
