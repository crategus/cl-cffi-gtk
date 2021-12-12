;;; ----------------------------------------------------------------------------
;;; cairo.package.lisp
;;;
;;; The documentation of this file is taken from the Cairo Reference Manual
;;; Version 1.12.2 and modified to document the Lisp binding to the Cairo
;;; library. See <http://cairographics.org>. The API documentation of the Lisp
;;; binding is available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2012 - 2021 Dieter Kaiser
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
  (:use :cl :cffi :glib :glib-init))

#+cl-cffi-gtk-documentation
(setf (documentation (find-package :cairo) t)
 "Cairo is a software library used to provide a vector graphics-based,
  device-independent API for software developers. It is designed to provide
  primitives for 2-dimensional drawing across a number of different backends.
  Cairo is designed to use hardware acceleration when available.

  This is the API documentation of a Lisp binding to Cairo. At this time only a
  few types and functions are implemented, which are needed to compile the Lisp
  bindung to GTK.

  @begin[Drawing]{section}
    @begin[The Cairo drawing context]{subsection}
      A @symbol{cairo-t} context is the main object used when drawing with
      Cairo. To draw with Cairo, you create a @symbol{cairo-t} context, set the
      target surface, and drawing options for the @symbol{cairo-t} context,
      create shapes with functions like the @fun{cairo-move-to} and
      @fun{cairo-line-to} functions, and then draw shapes with the
      @fun{cairo-stroke} or @fun{cairo-fill} functions.

      The @symbol{cairo-t} context can be pushed to a stack via the
      @fun{cairo-save} function. They may then safely be changed, without losing
      the current state. Use the @fun{cairo-restore} function to restore to the
      saved state.

      @about-symbol{cairo-antialias-t}
      @about-symbol{cairo-fill-rule-t}
      @about-symbol{cairo-line-cap-t}
      @about-symbol{cairo-line-join-t}
      @about-symbol{cairo-operator-t}
      @about-symbol{cairo-rectangle-t}
      @about-symbol{cairo-rectangle-list-t}
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
      @about-function{cairo-set-antialias}
      @about-function{cairo-get-antialias}
      @about-function{cairo-set-dash}
      @about-function{cairo-get-dash-count}
      @about-function{cairo-get-dash}
      @about-function{cairo-set-fill-rule}
      @about-function{cairo-get-fill-rule}
      @about-function{cairo-set-line-cap}
      @about-function{cairo-get-line-cap}
      @about-function{cairo-set-line-join}
      @about-function{cairo-get-line-join}
      @about-function{cairo-set-line-width}
      @about-function{cairo-get-line-width}
      @about-function{cairo-set-miter-limit}
      @about-function{cairo-get-miter-limit}
      @about-function{cairo-set-operator}
      @about-function{cairo-get-operator}
      @about-function{cairo-set-tolerance}
      @about-function{cairo-get-tolerance}
      @about-function{cairo-clip}
      @about-function{cairo-clip-preserve}
      @about-function{cairo-clip-extents}
      @about-function{cairo-in-clip}
      @about-function{cairo-reset-clip}
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
    @end{subsection}
    @begin[Paths]{subsection}
      Creating paths and manipulating path data.

      Paths are the most basic drawing tools and are primarily used to
      implicitly generate simple masks.

      @about-symbol{cairo-path-data-type-t}
      @about-symbol{cairo-path-data-t}
      @about-symbol{cairo-path-t}
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
    @end{subsection}
    @begin[Pattern]{subsection}
      Sources for drawing.

      The @symbol{cairo-pattern-t} structure is the paint with which Cairo
      draws. The primary use of patterns is as the source for all Cairo drawing
      operations, although they can also be used as masks, that is, as the
      brush too.

      A Cairo pattern is created by using one of the many constructors, of the
      form @sym{cairo-pattern-create-type} or implicitly through the
      @sym{cairo-set-source-type} functions.

      @about-symbol{cairo-pattern-t}
      @about-symbol{cairo-extend-t}
      @about-symbol{cairo-filter-t}
      @about-symbol{cairo-pattern-type-t}
      @about-function{cairo-pattern-add-color-stop-rgb}
      @about-function{cairo-pattern-add-color-stop-rgba}
      @about-function{cairo-pattern-get-color-stop-count}
      @about-function{cairo-pattern-get-color-stop-rgba}
      @about-function{cairo-pattern-create-rgb}
      @about-function{cairo-pattern-create-rgba}
      @about-function{cairo-pattern-get-rgba}
      @about-function{cairo-pattern-create-for-surface}
      @about-function{cairo-pattern-get-surface}
      @about-function{cairo-pattern-create-linear}
      @about-function{cairo-pattern-get-linear-points}
      @about-function{cairo-pattern-create-radial}
      @about-function{cairo-pattern-get-radial-circles}
      @about-function{cairo-pattern-create-mesh}
      @about-function{cairo-mesh-pattern-begin-patch}
      @about-function{cairo-mesh-pattern-end-patch}
      @about-function{cairo-mesh-pattern-move-to}
      @about-function{cairo-mesh-pattern-line-to}
      @about-function{cairo-mesh-pattern-curve-to}
      @about-function{cairo-mesh-pattern-set-control-point}
      @about-function{cairo-mesh-pattern-set-corner-color-rgb}
      @about-function{cairo-mesh-pattern-set-corner-color-rgba}
      @about-function{cairo-mesh-pattern-get-patch-count}
      @about-function{cairo-mesh-pattern-get-path}
      @about-function{cairo-mesh-pattern-get-control-point}
      @about-function{cairo-mesh-pattern-get-corner-color-rgba}
      @about-function{cairo-pattern-reference}
      @about-function{cairo-pattern-destroy}
      @about-function{cairo-pattern-status}
      @about-function{cairo-pattern-set-extend}
      @about-function{cairo-pattern-get-extend}
      @about-function{cairo-pattern-set-filter}
      @about-function{cairo-pattern-get-filter}
      @about-function{cairo-pattern-set-matrix}
      @about-function{cairo-pattern-get-matrix}
      @about-function{cairo-pattern-get-type}
      @about-function{cairo-pattern-get-reference-count}
      @about-function{cairo-pattern-set-user-data}
      @about-function{cairo-pattern-get-user-data}
    @end{subsection}
    @begin[Regions]{subsection}
      Representing a pixel-aligned area.

      Regions are a simple graphical data type representing an area of
      integer-aligned rectangles. They are often used on raster surfaces to
      track areas of interest, such as change or clip areas.

      @about-symbol{cairo-region-overlap-t}
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
    @end{subsection}
    @begin[Transformations]{subsection}
      Manipulating the current transformation matrix.

      The current transformation matrix, CTM, is a two-dimensional affine
      transformation that maps all coordinates and other drawing instruments
      from the user space into the surface's canonical coordinate system, also
      known as the device space.

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
    @end{subsection}
    @begin[Text]{subsection}
      Rendering text and glyphs.

      The functions with text in their name form Cairo's toy text API. The toy
      API takes UTF-8 encoded text and is limited in its functionality to
      rendering simple left-to-right text with no advanced features. That means
      for example that most complex scripts like Hebrew, Arabic, and Indic
      scripts are out of question. No kerning or correct positioning of
      diacritical marks either. The font selection is pretty limited too and
      does not handle the case that the selected font does not cover the
      characters in the text. This set of functions are really that, a toy text
      API, for testing and demonstration purposes. Any serious application
      should avoid them.

      The functions with glyphs in their name form Cairo's low-level text API.
      The low-level API relies on the user to convert text to a set of glyph
      indexes and positions. This is a very hard problem and is best handled by
      external libraries, like the @code{pangocairo} library that is part of
      the Pango text layout and rendering library. Pango is available from the
      @url[http://www.pango.org/]{Pango library}.

      @about-symbol{cairo-glyph-t}
      @about-symbol{cairo-font-slant-t}
      @about-symbol{cairo-font-weight-t}
      @about-symbol{cairo-text-cluster-t}
      @about-symbol{cairo-text-cluster-flags-t}
      @about-function{cairo-select-font-face}
      @about-function{cairo-set-font-size}
      @about-function{cairo-set-font-matrix}
      @about-function{cairo-get-font-matrix}
      @about-function{cairo-font-options}
      @about-function{cairo-set-font-face}
      @about-function{cairo-get-font-face}
      @about-function{cairo-set-scaled-font}
      @about-function{cairo-get-scaled-font}
      @about-function{cairo-show-text}
      @about-function{cairo-show-glyphs}
      @about-function{cairo-show-text-glyphs}
      @about-function{cairo-font-extents}
      @about-function{cairo-text-extents}
      @about-function{cairo-glyph-extents}
      @about-function{cairo-toy-font-face-create}
      @about-function{cairo-toy-font-face-get-family}
      @about-function{cairo-toy-font-face-get-slant}
      @about-function{cairo-toy-font-face-get-weight}
      @about-function{cairo-glyph-allocate}
      @about-function{cairo-glyph-free}
      @about-function{cairo-text-cluster-allocate}
      @about-function{cairo-text-cluster-free}
    @end{subsection}
    @begin[Raster Source]{subsection}
      Supplying arbitrary image data.

      @about-function{cairo-pattern-create-raster-source}
      @about-function{cairo-raster-source-pattern-set-callback-data}
      @about-function{cairo-raster-source-pattern-get-callback-data}
      @about-function{cairo-raster-source-pattern-set-acquire}
      @about-function{cairo-raster-source-pattern-get-acquire}
      @about-function{cairo-raster-source-pattern-set-snapshot}
      @about-function{cairo-raster-source-pattern-get-snapshot}
      @about-function{cairo-raster-source-pattern-set-copy}
      @about-function{cairo-raster-source-pattern-get-copy}
      @about-function{cairo-raster-source-pattern-set-finish}
      @about-function{cairo-raster-source-pattern-get-finish}
    @end{subsection}
    @begin[Tags and Links]{subsection}
      Hyperlinks and document structure

      @see-symbol{CAIRO_TAG_DEST}
      @see-symbol{CAIRO_TAG_LINK}
      @see-function{cairo-tag-begin}
      @see-function{cairo-tag-end}
    @end{subsection}
  @end{section}
  @begin[Fonts]{section}
    @begin[Font Faces]{subsection}
      Base class for font faces.

      The @symbol{cairo-font-face-t} structure represents a particular font at
      a particular weight, slant, and other characteristic but no size,
      transformation, or size.

      Font faces are created using font-backend-specific constructors, typically
      of the form @code{cairo-backend-font-face-create}, or implicitly using the
      toy text API by way of the @fun{cairo-select-font-face} function. The
      resulting face can be accessed using the @fun{cairo-get-font-face}
      function.

      @about-symbol{cairo-font-face-t}
      @about-symbol{cairo-font-type-t}
      @about-function{cairo-font-face-reference}
      @about-function{cairo-font-face-destroy}
      @about-function{cairo-font-face-status}
      @about-function{cairo-font-face-get-type}
      @about-function{cairo-font-face-get-reference-count}
      @about-function{cairo-font-face-set-user-data}
      @about-function{cairo-font-face-get-user-data}
    @end{subsection}
    @begin[Scaled Fonts]{subsection}
      Font face at particular size and options.

      The @symbol{cairo-scaled-font-t} structure represents a realization of a
      font face at a particular size and transformation and a certain set of
      font options.

      @about-symbol{cairo-scaled-font-t}
      @about-symbol{cairo-font-extents-t}
      @about-function{cairo-font-extents-ascent}
      @about-function{cairo-font-extents-descent}
      @about-function{cairo-font-extents-height}
      @about-function{cairo-font-extents-max-x-advance}
      @about-function{cairo-font-extents-max-y-advance}
      @about-symbol{cairo-text-extents-t}
      @about-function{cairo-text-extents-x-bearing}
      @about-function{cairo-text-extents-y-bearing}
      @about-function{cairo-text-extents-width}
      @about-function{cairo-text-extents-height}
      @about-function{cairo-text-extents-x-advance}
      @about-function{cairo-text-extents-y-advance}
      @about-function{cairo-scaled-font-create}
      @about-function{cairo-scaled-font-reference}
      @about-function{cairo-scaled-font-destroy}
      @about-function{cairo-scaled-font-status}
      @about-function{cairo-scaled-font-extents}
      @about-function{cairo-scaled-font-text-extents}
      @about-function{cairo-scaled-font-glyph-extents}
      @about-function{cairo-scaled-font-text-to-glyphs}
      @about-function{cairo-scaled-font-get-font-face}
      @about-function{cairo-scaled-font-get-font-options}
      @about-function{cairo-scaled-font-get-font-matrix}
      @about-function{cairo-scaled-font-get-ctm}
      @about-function{cairo-scaled-font-get-scale-matrix}
      @about-function{cairo-scaled-font-get-type}
      @about-function{cairo-scaled-font-get-reference-count}
      @about-function{cairo-scaled-font-set-user-data}
      @about-function{cairo-scaled-font-get-user-data}
    @end{subsection}
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
      @about-function{cairo-font-options-antialias}
      @about-symbol{cairo-subpixel-order-t}
      @about-function{cairo-font-options-subpixel-order}
      @about-symbol{cairo-hint-style-t}
      @about-function{cairo-font-options-hint-style}
      @about-symbol{cairo-hint-metrics-t}
      @about-function{cairo-font-options-hint-metrics}
      @about-function{cairo-font-options-variations}
    @end{subsection}
    @begin[FreeType Fonts]{subsection}
      Font support for FreeType
    @end{subsection}
    @begin[Win32 Fonts]{subsection}
      Font support for Microsoft Windows
    @end{subsection}
    @begin[Quartz Fonts]{subsection}
      Font support via CGFont on OS X
    @end{subsection}
    @begin[User Fonts]{subsection}
      Font support with font data provided by the user
    @end{subsection}
  @end{section}
  @begin[Surfaces]{section}
    @begin[cairo_device_t]{subsection}
        Interface to underlying rendering system.

        @about-symbol{cairo-device-t}
        @about-symbol{cairo-device-type-t}
        @about-function{cairo-device-reference}
        @about-function{cairo-device-destroy}
        @about-function{cairo-device-status}
        @about-function{cairo-device-finish}
        @about-function{cairo-device-flush}
        @about-function{cairo-device-get-type}
        @about-function{cairo-device-get-reference-count}
        @about-function{cairo-device-set-user-data}
        @about-function{cairo-device-get-user-data}
        @about-function{cairo-device-acquire}
        @about-function{cairo-device-release}
        @about-function{cairo-device-observer-elapsed}
        @about-function{cairo-device-observer-fill-elapsed}
        @about-function{cairo-device-observer-glyphs-elapsed}
        @about-function{cairo-device-observer-mask-elapsed}
        @about-function{cairo-device-observer-paint-elapsed}
        @about-function{cairo-device-observer-print}
        @about-function{cairo-device-observer-stroke-elapsed}
    @end{subsection}
    @begin[Cairo surfaces]{subsection}
      Base class for surfaces.

      A @symbol{cairo-surface-t} structure is the abstract type representing all
      different drawing targets that cairo can render to. The actual drawings
      are performed using a Cairo context.

      A cairo surface is created by using backend-specific constructors,
      typically of the form @code{cairo-backend-surface-create}.

      Most surface types allow accessing the surface without using Cairo
      functions. If you do this, keep in mind that it is mandatory that you call
      the @fun{cairo-surface-flush} function before reading from or writing to
      the surface and that you must use the @fun{cairo-surface-mark-dirty}
      function after modifying it.

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
      surface's device first. See the @fun{cairo-device-acquire} function for a
      discussion of devices.

      @about-symbol{CAIRO_HAS_MIME_SURFACE}
      @about-symbol{CAIRO_MIME_TYPE_JP2}
      @about-symbol{CAIRO_MIME_TYPE_JPEG}
      @about-symbol{CAIRO_MIME_TYPE_PNG}
      @about-symbol{CAIRO_MIME_TYPE_URI}
      @about-symbol{CAIRO_MIME_TYPE_UNIQUE_ID}
      @about-symbol{cairo-surface-t}
      @about-symbol{cairo-content-t}
      @about-symbol{cairo-surface-type-t}
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
      @about-function{cairo-surface-get-device-scale}
      @about-function{cairo-surface-set-device-scale}
      @about-function{cairo-surface-set-fallback-resolution}
      @about-function{cairo-surface-get-fallback-resolution}
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
    @end{subsection}
    @begin[Image Surfaces]{subsection}
      Rendering to memory buffers.

      Image surfaces provide the ability to render to memory buffers either
      allocated by Cairo or by the calling code. The supported image formats
      are those defined in the @symbol{cairo-format-t} enumeration.

      @about-symbol{cairo-format-t}
      @about-function{cairo-format-stride-for-width}
      @about-function{cairo-image-surface-create}
      @about-function{cairo-image-surface-create-for-data}
      @about-function{cairo-image-surface-data}
      @about-function{cairo-image-surface-format}
      @about-function{cairo-image-surface-width}
      @about-function{cairo-image-surface-height}
      @about-function{cairo-image-surface-stride}
    @end{subsection}
    @begin[PDF Surfaces]{subsection}
      Rendering PDF documents

      @about-symbol{CAIRO_HAS_PDF_SURFACE}
      @about-symbol{CAIRO_PDF_OUTLINE_ROOT}
      @about-symbol{cairo-pdf-outline-flags-t}
      @about-symbol{cairo-pdf-metadata-t}
      @about-symbol{cairo-pdf-version-t}
      @about-function{cairo-pdf-surface-create}
      @about-function{cairo-pdf-surface-create-for-stream}
      @about-function{cairo-pdf-surface-restrict-to-version}
      @about-function{cairo-pdf-get-versions}
      @about-function{cairo-pdf-version-to-string}
      @about-function{cairo-pdf-surface-set-size}
      @about-function{cairo-pdf-surface-add-outline}
      @about-function{cairo-pdf-surface-set-metadata}
      @about-function{cairo-pdf-surface-set-page-label}
      @about-function{cairo-pdf-surface-set-thumbnail-size}
    @end{subsection}
    @begin[PNG Support]{subsection}
      Reading and writing PNG images.

      The PNG functions allow reading PNG images into image surfaces, and
      writing any surface to a PNG file.

      It is a toy API. It only offers very simple support for reading and
      writing PNG files, which is sufficient for testing and demonstration
      purposes. Applications which need more control over the generated PNG
      file should access the pixel data directly, using the
      @fun{cairo-image-surface-data} function or a backend-specific access
      function, and process it with another library, e.g. GdkPixbuf or
      @code{libpng}.

      @about-symbol{CAIRO_HAS_PNG_FUNCTIONS}
      @about-function{cairo-image-surface-create-from-png}
      @about-function{cairo-image-surface-create-from-png-stream}
      @about-function{cairo-surface-write-to-png}
      @about-function{cairo-surface-write-to-png-stream}
    @end{subsection}
    @begin[PostScript Surfaces]{subsection}
      Rendering PostScript documents
    @end{subsection}
    @begin[Recording Surfaces]{subsection}
      Records all drawing operations
    @end{subsection}
    @begin[Win32 Surfaces]{subsection}
      Microsoft Windows surface support
    @end{subsection}
    @begin[SVG Surfaces]{subsection}
      Rendering SVG documents
    @end{subsection}
    @begin[Quartz Surfaces]{subsection}
      Rendering to Quartz surfaces
    @end{subsection}
    @begin[XCB Surfaces]{subsection}
      X Window System rendering using the XCB library
    @end{subsection}
    @begin[XLib Surfaces]{subsection}
      X Window System rendering using XLib
    @end{subsection}
    @begin[XLib-XRender Backend]{subsection}
      X Window System rendering using the X Render extension
    @end{subsection}
    @begin[Script Surfaces]{subsection}
      Rendering to replayable scripts
    @end{subsection}
  @end{section}
  @begin[Utilities]{section}
    @begin[Generic matrix operations]{subsection}
      Generic matrix operations.

      @about-symbol{cairo-matrix-t}
      @about-function{cairo-matrix-init}
      @about-function{cairo-matrix-init-identity}
      @about-function{cairo-matrix-init-translate}
      @about-function{cairo-matrix-init-scale}
      @about-function{cairo-matrix-init-rotate}
      @about-function{cairo-matrix-translate}
      @about-function{cairo-matrix-scale}
      @about-function{cairo-matrix-rotate}
      @about-function{cairo-matrix-invert}
      @about-function{cairo-matrix-multiply}
      @about-function{cairo-matrix-transform-distance}
      @about-function{cairo-matrix-transform-point}
    @end{subsection}
    @begin[Error handling]{subsection}
      Decoding Cairo's status.

      Cairo uses a single status type to represent all kinds of errors. A
      status value of @code{:success} represents no error and has an integer
      value of zero. All other status values represent an error.

      Cairo's error handling is designed to be easy to use and safe. All major
      Cairo objects retain an error status internally which can be queried
      anytime by the users using @code{cairo*-status} calls. In the mean time,
      it is safe to call all Cairo functions normally even if the underlying
      object is in an error status. This means that no error handling code is
      required before or after each individual Cairo function call.

      @about-symbol{cairo-status-t}
      @about-function{cairo-status-to-string}
      @about-function{cairo-debug-reset-static-data}
    @end{subsection}
    @begin[Version Information]{subsection}
      Cairo provides the ability to examine the version at either compile-time
      or run-time and in both a human readable form as well as an encoded form
      suitable for direct comparison. Cairo also provides the
      @fun{cairo-version-encode} function to perform the encoding.

      @about-variable{+cairo-version+}
      @about-variable{+cairo-version-major+}
      @about-variable{+cairo-version-minor+}
      @about-variable{+cairo-version-micro+}
      @about-variable{+cairo-version-string+}
      @about-function{cairo-version-encode}
      @about-function{cairo-version-stringize}
      @about-function{cairo-version}
      @about-function{cairo-version-string}
    @end{subsection}
    @begin[Types]{subsection}
      This section lists generic data types used in the cairo API.

      @about-symbol{cairo-bool-t}
      @about-symbol{cairo-user-data-key-t}
      @about-symbol{cairo-rectangle-int-t}
    @end{subsection}
  @end{section}")

;;; --- End of file cairo.package.lisp -----------------------------------------
