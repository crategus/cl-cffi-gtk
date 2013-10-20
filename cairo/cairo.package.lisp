;;; ----------------------------------------------------------------------------
;;; cairo.package.lisp
;;;
;;; The documentation of this file is taken from the Cairo Reference Manual
;;; Version 1.12.2 and modified to document the Lisp binding to the Cairo
;;; library. See <http://cairographics.org>. The API documentation of the Lisp
;;; binding is available from <http://www.crategus.com/books/cl-cffi-gtk/>.
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

#+cl-cffi-gtk-documentation
(setf (documentation (find-package :cairo) t)
 "Cairo is a software library used to provide a vector graphics-based,
  device-independent API for software developers. It is designed to provide
  primitives for 2-dimensional drawing across a number of different backends.
  Cairo is designed to use hardware acceleration when available.

  This is the API documentation of a Lisp binding to Cairo. At this time only a
  few types and functions are implemented, which are needed to compile the Lisp
  bindung to GTK+.

  @begin[Drawing]{section}
    The Cairo drawing context.

    @symbol{cairo-t} is the main object used when drawing with Cairo. To
    draw with Cairo, you create a @symbol{cairo-t}, set the target surface,
    and drawing options for the @symbol{cairo-t}, create shapes with functions
    like @fun{cairo-move-to} and @fun{cairo-line-to}, and then draw shapes
    with the functions @fun{cairo-stroke} or @fun{cairo-fill}.

    @symbol{cairo-t}'s can be pushed to a stack via the function
    @fun{cairo-save}. They may then safely be changed, without losing the
    current state. Use the function @fun{cairo-restore} to restore to the saved
    state.

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
  @begin[Transformations]{section}
    Manipulating the current transformation matrix.

    @about-function{cairo-translate}
    @about-function{cairo-scale}
    @about-function{cairo-rotate}
    @about-function{cairo-transform}
    @about-function{cairo-set-matrix}
    @about-function{cairo-get-matrix}
    @about-function{cairo-identity-matrix}
    @about-function{cairo-user-to-device}
    @about-function{cairo-user-to-device-distance}
    @about-function{cairo-device-to-user}
    @about-function{cairo-device-to-user-distance}
  @end{section}
  @begin[Fonts]{section}
    @begin[Font Options]{subsection}
      How a font should be rendered.

      The font options specify how fonts should be rendered. Most of the time
      the font options implied by a surface are just right and do not need any
      changes, but for pixel-based targets tweaking font options may result in
      superior output on a particular display.

      @about-symbol{cairo-font-options-t}
      @about-function{cairo-font-options-create}
      @about-function{cairo-font-options-copy}
      @about-function{cairo-font-options-destroy}
      @about-function{cairo-font-options-status}
      @about-function{cairo-font-options-merge}
      @about-function{cairo-font-options-hash}
      @about-function{cairo-font-options-equal}
      @about-function{cairo-font-options-set_antialias}
      @about-function{cairo-font-options-get_antialias}
      @about-symbol{cairo-subpixel-order-t}
      @about-function{cairo-font-options-set-subpixel-order}
      @about-function{cairo-font-options-get-subpixel-order}
      @about-symbol{cairo-hint-style-t}
      @about-function{cairo-font-options-set-hint-style}
      @about-function{cairo-font-options-get-hint-style}
      @about-symbol{cairo-hint-metrics-t}
      @about-function{cairo-font-options-set-hint-metrics}
      @about-function{cairo-font-options-get-hint-metrics}
    @end{subsection}
  @end{section}
  @begin[Surfaces]{section}
    Base class for surfaces.

    @symbol{cairo-surface-t} is the abstract type representing all different
    drawing targets that cairo can render to. The actual drawings are performed
    using a cairo context.

    A cairo surface is created by using backend-specific constructors, typically
    of the form @code{cairo-backend-surface-create}.

    Most surface types allow accessing the surface without using Cairo
    functions. If you do this, keep in mind that it is mandatory that you call
    the function @fun{cairo-surface-flush} before reading from or writing to
    the surface and that you must use the function
    @fun{cairo-surface-mark-dirty} after modifying it.

    @b{Example 1.} Directly modifying an image surface
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
    surface's device first. See the function @fun{cairo-device-acquire} for a
    discussion of devices.

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
  @begin[Utilities]{section}
    @begin[Error handling]{subsection}
      Decoding cairo's status.

      Cairo uses a single status type to represent all kinds of errors. A
      status value of @code{:success} represents no error and has an integer
      value of zero. All other status values represent an error.

      Cairo's error handling is designed to be easy to use and safe. All major
      cairo objects retain an error status internally which can be queried
      anytime by the users using @code{cairo*-status} calls. In the mean time,
      it is safe to call all cairo functions normally even if the underlying
      object is in an error status. This means that no error handling code is
      required before or after each individual cairo function call.

      @about-symbol{cairo-status-t}
      @about-function{cairo-status-to-string}
      @about-function{cairo-debug-reset-static-data}
    @end{subsection}
    @begin[Types]{subsection}
      This section lists generic data types used in the cairo API.

      @about-symbol{cairo-bool-t}
      @about-symbol{cairo-user-data-key-t}
      @about-symbol{cairo-rectangle-int-t}
    @end{subsection}
    @begin[Version Information]{subsection}
      Cairo provides the ability to examine the version at either compile-time
      or run-time and in both a human-readable form as well as an encoded form
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
    @end{subsection}
  @end{section}")

;;; --- End of file cairo.package.lisp -----------------------------------------
