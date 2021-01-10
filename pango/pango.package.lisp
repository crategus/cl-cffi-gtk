;;; ----------------------------------------------------------------------------
;;; pango.package.lisp
;;;
;;; The documentation of this file is taken from the Pango Reference Manual
;;; Version 1.32.6 and modified to document the Lisp binding to the Pango
;;; library. See <http://www.gtk.org>. The API documentation of the Lisp binding
;;; is available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2020 Dieter Kaiser
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

(defpackage :pango
  (:use :cl :iter :cffi :gobject :glib :glib-init :cairo))

;;; ----------------------------------------------------------------------------

(setf (documentation (find-package :pango) t)
 "Pango is a text layout and shaping library. Pango facilitates the
  layout and shaping of multi-language text. Full-function rendering of text
  and cross-platform support is had when Pango is used with platform APIs or
  3rd party libraries, such as Uniscribe and FreeType, as text rendering
  backends. Pango-processed text will appear similar under different operating
  systems.
  This is the API documentation of a Lisp binding to Pango.
  @begin[Basic Pango Interfaces]{section}
    @begin[Rendering]{subsection}
      Functions to run the rendering pipeline.

      The Pango rendering pipeline takes a string of Unicode characters and
      converts it into glyphs. The functions described in this section
      accomplish various steps of this process.

      @about-symbol{pango-item}
      @about-symbol{pango-analysis}
      @about-symbol{PANGO_ANALYSIS_FLAG_CENTERED_BASELINE}
      @about-symbol{PANGO_ANALYSIS_FLAG_IS_ELLIPSIS}
      @about-symbol{PANGO_ANALYSIS_FLAG_NEED_HYPHEN}
      @about-symbol{pango-log-attr}
      @about-symbol{pango-shape-flags}
      @about-function{pango-itemize}
      @about-function{pango-itemize-with-base-dir}
      @about-function{pango-item-free}
      @about-function{pango-item-copy}
      @about-function{pango-item-new}
      @about-function{pango-item-split}
      @about-function{pango-item-apply-attrs}
      @about-function{pango-reorder-items}
      @about-function{pango-break}
      @about-function{pango-get-log-attrs}
      @about-function{pango-find-paragraph-boundary}
      @about-function{pango-default-break}
      @about-function{pango-tailor-break}
      @about-function{pango-shape}
      @about-function{pango-shape-full}
      @about-function{pango-shape-with-flags}
    @end{subsection}
    @begin[Fonts]{subsection}
      Structures representing abstract fonts.

      Pango supports a flexible architecture where a particular rendering
      architecture can supply an implementation of fonts. The
      @class{pango-font} class represents an abstract
      rendering-system-independent font. Pango provides routines to list
      available fonts, and to load a font of a given description.

      @about-class{pango-font-description}
      @about-symbol{pango-style}
      @about-symbol{pango-weight}
      @about-symbol{pango-variant}
      @about-symbol{pango-stretch}
      @about-symbol{pango-font-mask}
      @about-class{pango-font-metrics}
      @about-class{pango-font}
      @about-class{pango-font-family}
      @about-class{pango-font-face}
      @about-class{pango-font-map}
      @about-class{pango-font-map-class}
      @about-class{pango-fontset}
      @about-class{pango-fontset-class}
      @about-function{pango-font-description-new}
      @about-function{pango-font-description-copy}
      @about-function{pango-font-description-copy-static}
      @about-function{pango-font-description-hash}
      @about-function{pango-font-description-equal}
      @about-function{pango-font-description-free}
      @about-function{pango-font-descriptions-free}
      @about-function{pango-font-description-family}
      @about-function{pango-font-description-set-family-static}
      @about-function{pango-font-description-style}
      @about-function{pango-font-description-variant}
      @about-function{pango-font-description-weight}
      @about-function{pango-font-description-stretch}
      @about-function{pango-font-description-size}
      @about-function{pango-font-description-set-absolute-size}
      @about-function{pango-font-description-size-is-absolute}
      @about-function{pango-font-description-gravity}
      @about-function{pango-font-description-variations}
      @about-function{pango-font-description-set-variations-static}
      @about-function{pango-font-description-set-fields}
      @about-function{pango-font-description-unset-fields}
      @about-function{pango-font-description-merge}
      @about-function{pango-font-description-merge-static}
      @about-function{pango-font-description-better-match}
      @about-function{pango-font-description-from-string}
      @about-function{pango-font-description-to-string}
      @about-function{pango-font-description-to-filename}
      @about-function{pango-font-metrics-ref}
      @about-function{pango-font-metrics-unref}
      @about-function{pango-font-metrics-ascent}
      @about-function{pango-font-metrics-descent}
      @about-function{pango-font-metrics-height}
      @about-function{pango-font-metrics-approximate-char-width}
      @about-function{pango-font-metrics-approximate-digit-width}
      @about-function{pango-font-metrics-underline-thickness}
      @about-function{pango-font-metrics-underline-position}
      @about-function{pango-font-metrics-strikethrough-thickness}
      @about-function{pango-font-metrics-strikethrough-position}
      @about-function{pango-font-find-shaper}
      @about-function{pango-font-describe}
      @about-function{pango-font-describe-with-absolute-size}
      @about-function{pango-font-face}
      @about-function{pango-font-coverage}
      @about-function{pango-font-has-char}
      @about-function{pango-font-glyph-extents}
      @about-function{pango-font-metrics}
      @about-function{pango-font-font-map}
      @about-function{pango-font-features}
      @about-function{pango-font-hb-font}
      @about-function{pango-font-family-name}
      @about-function{pango-font-family-is-monospace}
      @about-function{pango-font-family-is-variable}
      @about-function{pango-font-family-list-faces}
      @about-function{pango-font-family-face}
      @about-function{pango-font-face-face-name}
      @about-function{pango-font-face-list-sizes}
      @about-function{pango-font-face-describe}
      @about-function{pango-font-face-is-synthesized}
      @about-function{pango-font-face-family}
      @about-function{pango-font-map-create-context}
      @about-function{pango-font-map-load-font}
      @about-function{pango-font-map-load-fontset}
      @about-function{pango-font-map-list-families}
      @about-function{pango-font-map-family}
      @about-function{pango-font-map-serial}
      @about-function{pango-font-map-changed}
      @about-function{pango-font-map-shape-engine-type}
      @about-function{pango-fontset-font}
      @about-function{pango-fontset-metrics}
      @about-symbol{PangoFontsetForeachFunc}
      @about-function{pango-fontset-foreach}
    @end{subsection}
    @begin[Glyph Storage]{subsection}
      Structures for storing information about glyphs.

      the function @fun{pango-shape} produces a string of glyphs which can be
      measured or drawn to the screen. The following structures are used to
      store information about glyphs.

      @about-variable{+pango-scale+}
      @about-symbol{pango-rectangle}
      @about-struct{pango-matrix}
      @about-symbol{pango-glyph}
      @about-function{PANGO_GLYPH_EMPTY}
      @about-function{PANGO_GLYPH_INVALID_INPUT}
      @about-function{PANGO_GLYPH_UNKNOWN_FLAG}
      @about-class{pango-glyph-info}
      @about-class{pango-glyph-geometry}
      @about-class{pango-glyph-unit}
      @about-class{pango-glyph-vis-attr}
      @about-class{pango-glyph-string}
      @about-class{pango-glyph-item}
      @about-class{pango-glyph-item-iter}
      @about-function{PANGO_TYPE_GLYPH_STRING}
      @about-function{pango-pixels}
      @about-function{PANGO_PIXELS_FLOOR}
      @about-function{PANGO_PIXELS_CEIL}
      @about-function{PANGO_UNITS_ROUND}
      @about-function{pango-units-to-double}
      @about-function{pango-units-from-double}
      @about-function{PANGO_ASCENT}
      @about-function{PANGO_DESCENT}
      @about-function{PANGO_LBEARING}
      @about-function{PANGO_RBEARING}
      @about-function{pango-extents-to-pixels}
      @about-function{pango-matrix-init}
      @about-function{pango-matrix-copy}
      @about-function{pango-matrix-free}
      @about-function{pango-matrix-translate}
      @about-function{pango-matrix-scale}
      @about-function{pango-matrix-rotate}
      @about-function{pango-matrix-concat}
      @about-function{pango-matrix-transform-point}
      @about-function{pango-matrix-transform-distance}
      @about-function{pango-matrix-transform-rectangle}
      @about-function{pango-matrix-transform-pixel-rectangle}
      @about-function{pango-matrix-font-scale-factor}
      @about-function{pango-matrix-font-scale-factors}
      @about-function{PANGO_GET_UNKNOWN_GLYPH}
      @about-function{pango-glyph-string-new}
      @about-function{pango-glyph-string-copy}
      @about-function{pango-glyph-string-set-size}
      @about-function{pango-glyph-string-free}
      @about-function{pango-glyph-string-extents}
      @about-function{pango-glyph-string-extents-range}
      @about-function{pango-glyph-string-get-width}
      @about-function{pango-glyph-string-index-to-x}
      @about-function{pango-glyph-string-x-to-index}
      @about-function{pango-glyph-string-get-logical-widths}
      @about-function{pango-glyph-item-copy}
      @about-function{pango-glyph-item-free}
      @about-function{pango-glyph-item-split}
      @about-function{pango-glyph-item-apply-attrs}
      @about-function{pango-glyph-item-letter-space}
      @about-function{pango-glyph-item-get-logical-widths}
      @about-function{pango-glyph-item-iter-copy}
      @about-function{pango-glyph-item-iter-free}
      @about-function{pango-glyph-item-iter-init-start}
      @about-function{pango-glyph-item-iter-init-end}
      @about-function{pango-glyph-item-iter-next-cluster}
      @about-function{pango-glyph-item-iter-prev-cluster}
    @end{subsection}
    @begin[Text Attributes]{subsection}
      Font and other attributes for annotating text.

      Attributed text is used in a number of places in Pango. It is used as the
      input to the itemization process and also when creating a PangoLayout.
      The data types and functions in this section are used to represent and
      manipulate sets of attributes applied to a portion of text.

      @about-symbol{pango-attr-type}
      @about-symbol{pango-attr-class}
      @about-symbol{pango-attribute}
      @about-symbol{PANGO_ATTR_INDEX_FROM_TEXT_BEGINNING}
      @about-symbol{PANGO_ATTR_INDEX_TO_TEXT_END}
      @about-symbol{pango-attr-string}
      @about-symbol{pango-attr-language}
      @about-symbol{pango-attr-color}
      @about-symbol{pango-attr-int}
      @about-symbol{pango-attr-float}
      @about-symbol{pango-attr-font-desc}
      @about-symbol{pango-attr-shape}
      @about-symbol{pango-attr-size}
      @about-symbol{PangoAttrFontFeatures}
      @about-symbol{pango-underline}
      @about-symbol{pango-overline}
      @about-symbol{PANGO_SCALE_XX_SMALL}
      @about-symbol{PANGO_SCALE_X_SMALL}
      @about-symbol{PANGO_SCALE_SMALL}
      @about-symbol{PANGO_SCALE_MEDIUM}
      @about-symbol{PANGO_SCALE_LARGE}
      @about-symbol{PANGO_SCALE_X_LARGE}
      @about-symbol{PANGO_SCALE_XX_LARGE}
      @about-symbol{PangoShowFlags}
      @about-symbol{PangoColor}
      @about-class{pango-attr-list}
      @about-symbol{PangoAttrIterator}
      @about-function{pango-attr-type-register}
      @about-function{pango-attr-type-get-name}
      @about-function{pango-attribute-init}
      @about-function{pango-attribute-copy}
      @about-function{pango-attribute-equal}
      @about-function{pango-attribute-destroy}
      @about-function{pango-attr-language-new}
      @about-function{pango-attr-family-new}
      @about-function{pango-attr-style-new}
      @about-function{pango-attr-variant-new}
      @about-function{pango-attr-stretch-new}
      @about-function{pango-attr-weight-new}
      @about-function{pango-attr-size-new}
      @about-function{pango-attr-size-new-absolute}
      @about-function{pango-attr-font-desc-new}
      @about-function{pango-attr-foreground-new}
      @about-function{pango-attr-background-new}
      @about-function{pango-attr-strikethrough-new}
      @about-function{pango-attr-strikethrough-color-new}
      @about-function{pango-attr-underline-new}
      @about-function{pango-attr-underline-color-new}
      @about-function{pango-attr-overline-new}
      @about-function{pango-attr-overline-color-new}
      @about-function{pango-attr-shape-new}
      @about-function{pango-attr-shape-new-with-data}
      @about-function{PangoAttrDataCopyFunc}
      @about-function{pango-attr-scale-new}
      @about-function{pango-attr-rise-new}
      @about-function{pango-attr-letter-spacing-new}
      @about-function{pango-attr-fallback-new}
      @about-function{pango-attr-gravity-new}
      @about-function{pango-attr-gravity-hint-new}
      @about-function{pango-attr-font-features-new}
      @about-function{pango-attr-foreground-alpha-new}
      @about-function{pango-attr-background-alpha-new}
      @about-function{pango-attr-allow-breaks-new}
      @about-function{pango-attr-insert-hyphens-new}
      @about-function{pango-attr-show-new}
      @about-function{pango-color-parse}
      @about-function{pango-color-parse-with-alpha}
      @about-function{pango-color-copy}
      @about-function{pango-color-free}
      @about-function{pango-color-to-string}
      @about-function{pango-attr-list-new}
      @about-function{pango-attr-list-ref}
      @about-function{pango-attr-list-unref}
      @about-function{pango-attr-list-copy}
      @about-function{pango-attr-list-insert}
      @about-function{pango-attr-list-insert-before}
      @about-function{pango-attr-list-change}
      @about-function{pango-attr-list-splice}
      @about-function{pango-attr-list-filter}
      @about-function{pango-attr-list-update}
      @about-function{PangoAttrFilterFunc}
      @about-function{pango-attr-list-get-attributes}
      @about-function{pango-attr-list-equal}
      @about-function{pango-attr-list-get-iterator}
      @about-function{pango-attr-iterator-copy}
      @about-function{pango-attr-iterator-next}
      @about-function{pango-attr-iterator-range}
      @about-function{pango-attr-iterator-get}
      @about-function{pango-attr-iterator-get-font}
      @about-function{pango-attr-iterator-get-attrs}
      @about-function{pango-attr-iterator-destroy}
    @end{subsection}
    @begin[Pango Markup]{subsection}
      Simple markup language for text with attributes.

      @about-function{pango-parse-markup}
      @about-function{pango-markup-parser-new}
      @about-function{pango-markup-parser-finish}
    @end{subsection}
    @begin[Layout Objects]{subsection}
      High-level layout driver objects.

      While complete access to the layout capabilities of Pango is provided
      using the detailed interfaces for itemization and shaping, using that
      functionality directly involves writing a fairly large amount of code.
      The objects and functions in this section provide a high-level driver
      for formatting entire paragraphs of text at once. This includes
      paragraph-level functionality such as line-breaking, justification,
      alignment and ellipsization.

      @about-symbol{pango-wrap-mode}
      @about-symbol{pango-ellipsize-mode}
      @about-symbol{pango-alignment}
      @about-class{pango-layout-line}
      @about-symbol{pango-layout-run}
      @about-class{pango-layout}
      @about-class{pango-layout-iter}
      @about-function{pango-layout-new}
      @about-function{pango-layout-copy}
      @about-function{pango-layout-context}
      @about-function{pango-layout-context-changed}
      @about-function{pango-layout-serial}
      @about-function{pango-layout-text}
      @about-function{pango-layout-character-count}
      @about-function{pango-layout-set-markup}
      @about-function{pango-layout-set-markup-with-accel}
      @about-function{pango-layout-attributes}
      @about-function{pango-layout-font-description}
      @about-function{pango-layout-width}
      @about-function{pango-layout-height}
      @about-function{pango-layout-wrap}
      @about-function{pango-layout-is-wrapped}
      @about-function{pango-layout-ellipsize}
      @about-function{pango-layout-is-ellipsized}
      @about-function{pango-layout-indent}
      @about-function{pango-layout-spacing}
      @about-function{pango-layout-line-spacing}
      @about-function{pango-layout-justify}
      @about-function{pango-layout-auto-dir}
      @about-function{pango-layout-direction}
      @about-function{pango-layout-alignment}
      @about-function{pango-layout-tabs}
      @about-function{pango-layout-single-paragraph-mode}
      @about-function{pango-layout-unknown-glyphs-count}
      @about-function{pango-layout-log-attrs}
      @about-function{pango-layout-log-attrs-readonly}
      @about-function{pango-layout-index-to-pos}
      @about-function{pango-layout-index-to-line-x}
      @about-function{pango-layout-xy-to-index}
      @about-function{pango-layout-cursor-pos}
      @about-function{pango-layout-move-cursor-visually}
      @about-function{pango-layout-extents}
      @about-function{pango-layout-pixel-extents}
      @about-function{pango-layout-size}
      @about-function{pango-layout-pixel-size}
      @about-function{pango-layout-baseline}
      @about-function{pango-layout-line-count}
      @about-function{pango-layout-line}
      @about-function{pango-layout-line-readonly}
      @about-function{pango-layout-lines}
      @about-function{pango-layout-lines-readonly}
      @about-function{pango-layout-iter}
      @about-function{pango-layout-iter-copy}
      @about-function{pango-layout-iter-free}
      @about-function{pango-layout-iter-next-run}
      @about-function{pango-layout-iter-next-char}
      @about-function{pango-layout-iter-next-cluster}
      @about-function{pango-layout-iter-next-line}
      @about-function{pango-layout-iter-at_last-line}
      @about-function{pango-layout-iter-index}
      @about-function{pango-layout-iter-baseline}
      @about-function{pango-layout-iter-run}
      @about-function{pango-layout-iter-run-readonly}
      @about-function{pango-layout-iter-line}
      @about-function{pango-layout-iter-line-readonly}
      @about-function{pango-layout-iter-layout}
      @about-function{pango-layout-iter-char-extents}
      @about-function{pango-layout-iter-cluster-extents}
      @about-function{pango-layout-iter-run-extents}
      @about-function{pango-layout-iter-line-yrange}
      @about-function{pango-layout-iter-line-extents}
      @about-function{pango-layout-iter-layout-extents}
      @about-function{pango-layout-line-ref}
      @about-function{pango-layout-line-unref}
      @about-function{pango-layout-line-extents}
      @about-function{pango-layout-line-pixel-extents}
      @about-function{pango-layout-line-index-to-x}
      @about-function{pango-layout-line-x-to-index}
      @about-function{pango-layout-line-x-ranges}
      @about-function{pango-layout-line-height}
    @end{subsection}
    @begin[Scripts and Languages]{subsection}
      Identifying writing systems and languages.

      @about-symbol{pango-script}
      @about-symbol{pango-script-iter}
      @about-function{pango-script-for-unichar}
      @about-function{pango-script-sample-language}
      @about-function{pango-script-iter-new}
      @about-function{pango-script-iter-get-range}
      @about-function{pango-script-iter-next}
      @about-function{pango-script-iter-free}
      @about-class{pango-language}
      @about-function{pango-language-from-string}
      @about-function{pango-language-to-string}
      @about-function{pango-language-matches}
      @about-function{pango-language-includes-script}
      @about-function{pango-language-scripts}
      @about-function{pango-language-default}
      @about-function{pango-language-preferred}
      @about-function{pango-language-sample-string}
    @end{subsection}
    @begin[Bidirectional Text]{subsection}
      Types and functions to help with handling bidirectional text.

      Pango supports bidirectional text (like Arabic and Hebrew) automatically.
      Some applications however, need some help to correctly handle
      bidirectional text.

      The @symbol{pango-direction} enumeration can be used with the function
      @fun{pango-context-base-dir} to instruct Pango about direction of text,
      though in most cases Pango detects that correctly and automatically.
      The rest of the facilities in this section are used internally by Pango
      already, and are provided to help applications that need more direct
      control over bidirectional setting of text.

      @about-symbol{pango-direction}
      @about-symbol{pango-bidi-type}
      @about-function{pango-unichar-direction}
      @about-function{pango-find-base-dir}
      @about-function{pango-pango-get-mirror-char}
      @about-function{pango-bidi-type-for-unichar}
    @end{subsection}
    @begin[Vertical Text]{subsection}
      Laying text out in vertical directions.

      @about-symbol{pango-gravity}
      @about-symbol{pango-gravity-hint}
      @about-function{PANGO_GRAVITY_IS_IMPROPER}
      @about-function{PANGO_GRAVITY_IS_VERTICAL}
      @about-function{pango-gravity-for-matrix}
      @about-function{pango-gravity-for-script}
      @about-function{pango-gravity-for-script-and-width}
      @about-function{pango-gravity-to-rotation}
    @end{subsection}
  @end{section}
  @begin[Rendering with Pango]{section}
    @begin[Cairo Rendering]{subsection}
      Font handling and rendering with Cairo.

      @about-class{pango-cairo-font}
      @about-class{pango-cairo-font-map}
      @about-function{pango-cairo-font-map-default}
      @about-function{pango-cairo-font-map-new}
      @about-function{pango-cairo-font-map-new-for-font-type}
      @about-function{pango-cairo-font-map-font-type}
      @about-function{pango-cairo-font-map-resolution}
      @about-function{pango-cairo-font-map-create-context}
      @about-function{pango-cairo-font-scaled-font}
      @about-function{pango-cairo-context-resolution}
      @about-function{pango-cairo-context-font-options}
      @about-symbol{PangoCairoShapeRendererFunc}
      @about-function{pango-cairo-context-shape-renderer}
      @about-function{pango-cairo-create-context}
      @about-function{pango-cairo-update-context}
      @about-function{pango-cairo-create-layout}
      @about-function{pango-cairo-update-layout}
      @about-function{pango-cairo-show-glyph-string}
      @about-function{pango-cairo-show-glyph-item}
      @about-function{pango-cairo-show-layout-line}
      @about-function{pango-cairo-show-layout}
      @about-function{pango-cairo-show-error-underline}
      @about-function{pango-cairo-glyph-string-path}
      @about-function{pango-cairo-layout-line-path}
      @about-function{pango-cairo-layout-path}
      @about-function{pango-cairo-error-underline-path}
    @end{subsection}
    @begin[Win32 Fonts and Rendering]{subsection}
      not implemented
    @end{subsection}
    @begin[CoreText Fonts]{subsection}
      not implemented
    @end{subsection}
    @begin[FreeType Fonts and Rendering]{subsection}
      not implemented
    @end{subsection}
    @begin[Xft Fonts and Rendering]{subsection}
      not implemented
    @end{subsection}
  @end{section}
  @begin[Low Level Functionality]{section}
    @begin[Contexts]{subsection}
      Global context object.

      @about-class{pango-context}
      @about-function{pango-context-new}
      @about-function{pango-context-changed}
      @about-function{pango-context-serial}
      @about-function{pango-context-font-map}
      @about-function{pango-context-font-description}
      @about-function{pango-context-language}
      @about-function{pango-context-base-dir}
      @about-function{pango-context-base-gravity}
      @about-function{pango-context-gravity}
      @about-function{pango-context-gravity-hint}
      @about-function{pango-context-matrix}
      @about-function{pango-context-round-glyph-positions}
      @about-function{pango-context-load-font}
      @about-function{pango-context-load-fontset}
      @about-function{pango-context-metrics}
      @about-function{pango-context-list-families}
    @end{subsection}
    @begin[Tab Stops]{subsection}
      Structures for storing tab stops.

      @about-class{pango-tab-array}
      @about-symbol{pango-tab-align}
      @about-function{pango-tab-array-new}
      @about-function{pango-tab-array-new-with-positions}
      @about-function{pango-tab-array-copy}
      @about-function{pango-tab-array-free}
      @about-function{pango-tab-array-get-size}
      @about-function{pango-tab-array-resize}
      @about-function{pango-tab-array-set-tab}
      @about-function{pango-tab-array-get-tab}
      @about-function{pango-tab-array-get-tabs}
      @about-function{pango-tab-array-get-positions-in-pixels}
    @end{subsection}
    @begin[Coverage Maps]{subsection}
      Unicode character range coverage storage.

      @about-symbol{pango-coverage-level}
      @about-symbol{PANGO_TYPE_COVERAGE_LEVEL}
      @about-class{pango-coverage}
      @about-function{pango-coverage-new}
      @about-function{pango-coverage-ref}
      @about-function{pango-coverage-unref}
      @about-function{pango-coverage-copy}
      @about-function{pango-coverage-get}
      @about-function{pango-coverage-max}
      @about-function{pango-coverage-set}
      @about-function{pango-coverage-to-bytes}
      @about-function{pango-coverage-from-bytes}
    @end{subsection}
    @begin[PangoRenderer]{subsection}
      Rendering driver base class.

      @about-class{pango-renderer}
      @about-symbol{pango-render-part}
      @about-function{pango-type-render-part}
      @about-class{pango-renderer-class}
      @about-function{pango-renderer-draw-layout}
      @about-function{pango-renderer-draw-layout-line}
      @about-function{pango-renderer-draw-glyphs}
      @about-function{pango-renderer-draw-glyph-item}
      @about-function{pango-renderer-draw-rectangle}
      @about-function{pango-renderer-draw-error-underline}
      @about-function{pango-renderer-draw-trapezoid}
      @about-function{pango-renderer-draw-glyph}
      @about-function{pango-renderer-activate}
      @about-function{pango-renderer-deactivate}
      @about-function{pango-renderer-part-changed}
      @about-function{pango-renderer-set-color}
      @about-function{pango-renderer-get-color}
      @about-function{pango-renderer-set-alpha}
      @about-function{pango-renderer-get-alpha}
      @about-function{pango-renderer-set-matrix}
      @about-function{pango-renderer-get-matrix}
      @about-function{pango-renderer-get-layout}
      @about-function{pango-renderer-get-layout-line}
    @end{subsection}
    @begin[PangoFcFontMap]{subsection}
      not implemented
    @end{subsection}
    @begin[PangoFcFont]{subsection}
      not implemented
    @end{subsection}
    @begin[PangoFcDecoder]{subsection}
      not implemented
    @end{subsection}
    @begin[Miscellaneous Utilities]{subsection}
      not implemented
    @end{subsection}
    @begin[Version Information]{subsection}
      Tools for checking Pango version at compile- and run-time.

      @about-function{PANGO_VERSION_ENCODE}
      @about-function{PANGO_VERSION}
      @about-function{PANGO_VERSION_MAJOR}
      @about-function{PANGO_VERSION_MINOR}
      @about-function{PANGO_VERSION_MICRO}
      @about-function{PANGO_VERSION_STRING}
      @about-function{PANGO_VERSION_CHECK}
      @about-function{pango-version}
      @about-function{pango-version-string}
      @about-function{pango-version-check}
    @end{subsection}
  @end{section}
  @begin[Deprecated APIs]{section}
    @begin[OpenType Font Handling]{subsection}
      not implemented
    @end{subsection}
    @begin[Engines]{subsection}
      not implemented
    @end{subsection}
    @begin[PangoEngineLang]{subsection}
      not implemented
    @end{subsection}
    @begin[PangoEngineShape]{subsection}
      not implemented
    @end{subsection}
    @begin[Modules]{subsection}
      not implemented
    @end{subsection}
  @end{section}")

;;; --- End of file pango.package.lisp -----------------------------------------
