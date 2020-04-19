;;; ----------------------------------------------------------------------------
;;; pango.package.lisp
;;;
;;; The documentation of this file is taken from the Pango Reference Manual
;;; Version 1.32.6 and modified to document the Lisp binding to the Pango
;;; library. See <http://www.gtk.org>. The API documentation of the Lisp binding
;;; is available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2019 Dieter Kaiser
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
  layout and shaping of multi-language text. Full-function rendering of text and
  cross-platform support is had when Pango is used with platform APIs or
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

      @about-class{pango-context}
      @about-class{pango-item}
      @about-class{pango-analysis}
      @about-function{PANGO_ANALYSIS_FLAG_CENTERED_BASELINE}
      @about-function{PANGO_TYPE_DIRECTION}
      @about-function{pango-itemize}
      @about-function{pango-itemize-with-base-dir}
      @about-function{pango-item-free}
      @about-function{pango-item-copy}
      @about-function{pango-item-new}
      @about-function{pango-item-split}
      @about-function{pango-reorder-items}
      @about-function{pango-context-new}
      @about-function{pango-context-changed}
      @about-function{pango-context-get-serial}
      @about-function{pango-context-set-font-map}
      @about-function{pango-context-get-font-map}
      @about-function{pango-context-get-font-description}
      @about-function{pango-context-set-font-description}
      @about-function{pango-context-get-language}
      @about-function{pango-context-set-language}
      @about-function{pango-context-get-base-dir}
      @about-function{pango-context-set-base-dir}
      @about-function{pango-context-get-base-gravity}
      @about-function{pango-context-set-base-gravity}
      @about-function{pango-context-get-gravity}
      @about-function{pango-context-get-gravity-hint}
      @about-function{pango-context-set-gravity-hint}
      @about-function{pango-context-get-matrix}
      @about-function{pango-context-set-matrix}
      @about-function{pango-context-load-font}
      @about-function{pango-context-load-fontset}
      @about-function{pango-context-get-metrics}
      @about-function{pango-context-list-families}
      @about-function{pango-break}
      @about-function{pango-get-log-attrs}
      @about-function{pango-find-paragraph-boundary}
      @about-function{pango-pango-default-break}
      @about-symbol{pango-log-attr}
      @about-function{pango-shape}
      @about-function{pango-shape-full}
    @end{subsection}
    @begin[Glyph Storage]{subsection}
      Structures for storing information about glyphs.

      @about-variable{+pango-scale+}
      @about-function{pango-pixels}
      @about-function{PANGO_PIXELS_FLOOR}
      @about-function{PANGO_PIXELS_CEIL}
      @about-function{PANGO_UNITS_ROUND}
      @about-function{pango-units-to-double}
      @about-function{pango-units-from-double}
      @about-struct{pango-rectangle}
      @about-function{PANGO_ASCENT}
      @about-function{PANGO_DESCENT}
      @about-function{PANGO_LBEARING}
      @about-function{PANGO_RBEARING}
      @about-function{pango-extents-to-pixels}
      @about-struct{pango-matrix}
      @about-function{PANGO_TYPE_MATRIX}
      @about-function{PANGO_MATRIX_INIT}
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
      @about-function{pango-matrix-get-font-scale-factor}
      @about-struct{pango-glyph}
      @about-function{PANGO_GLYPH_EMPTY}
      @about-function{PANGO_GLYPH_INVALID_INPUT}
      @about-function{PANGO_GLYPH_UNKNOWN_FLAG}
      @about-function{PANGO_GET_UNKNOWN_GLYPH}
      @about-class{pango-glyph-info}
      @about-class{pango-glyph-geometry}
      @about-class{pango-glyph-unit}
      @about-class{pango-glyph-vis-attr}
      @about-class{pango-glyph-string}
      @about-class{pango-glyph-item}
      @about-class{pango-glyph-item-iter}
      @about-function{PANGO_TYPE_GLYPH_STRING}
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
      @about-function{PANGO_TYPE_GLYPH_ITEM}
      @about-function{pango-glyph-item-copy}
      @about-function{pango-glyph-item-free}
      @about-function{pango-glyph-item-split}
      @about-function{pango-glyph-item-apply-attrs}
      @about-function{pango-glyph-item-letter-space}
      @about-function{pango-glyph-item-get-logical-widths}
      @about-function{PANGO_TYPE_GLYPH_ITEM_ITER}
      @about-function{pango-glyph-item-iter-copy}
      @about-function{pango-glyph-item-iter-free}
      @about-function{pango-glyph-item-iter-init-start}
      @about-function{pango-glyph-item-iter-init-end}
      @about-function{pango-glyph-item-iter-next-cluster}
      @about-function{pango-glyph-item-iter-prev-cluster}
    @end{subsection}
    @begin[Fonts]{subsection}
      Structures representing abstract fonts.

      @about-class{pango-font-description}
      @about-symbol{pango-style}
      @about-symbol{pango-weight}
      @about-symbol{pango-variant}
      @about-symbol{pango-stretch}
      @about-symbol{pango-font-mask}
      @about-function{pango-font-description-new}
      @about-function{pango-font-description-copy}
      @about-function{pango-font-description-copy-static}
      @about-function{pango-font-description-hash}
      @about-function{pango-font-description-equal}
      @about-function{pango-font-description-free}
      @about-function{pango-font-description-free}
      @about-function{pango-font-description-set-family}
      @about-function{pango-font-description-set-family-static}
      @about-function{pango-font-description-get-family}
      @about-function{pango-font-description-set-style}
      @about-function{pango-font-description-get-style}
      @about-function{pango-font-description-set-variant}
      @about-function{pango-font-description-get-variant}
      @about-function{pango-font-description-set-weight}
      @about-function{pango-font-description-get-weight}
      @about-function{pango-font-description-set-stretch}
      @about-function{pango-font-description-get-stretch}
      @about-function{pango-font-description-set-size}
      @about-function{pango-font-description-get-size}
      @about-function{pango-font-description-set-absolute-size}
      @about-function{pango-font-description-get-size-is-absolute}
      @about-function{pango-font-description-set-gravity}
      @about-function{pango-font-description-get-gravity}
      @about-function{pango-font-description-get-set-fields}
      @about-function{pango-font-description-unset-fields}
      @about-function{pango-font-description-merge}
      @about-function{pango-font-description-merge-static}
      @about-function{pango-font-description-better-match}
      @about-function{pango-font-description-from-string}
      @about-function{pango-font-description-to-string}
      @about-function{pango-font-description-to-filename}
      @about-symbol{pango-font-metrics}
      @about-function{pango-font-metrics-ref}
      @about-function{pango-font-metrics-unref}
      @about-function{pango-font-metrics-get-ascent}
      @about-function{pango-font-metrics-get-descent}
      @about-function{pango-font-metrics-get-approximate-char-width}
      @about-function{pango-font-metrics-get-approximate-digit-width}
      @about-function{pango-font-metrics-get-underline-thickness}
      @about-function{pango-font-metrics-get-underline-position}
      @about-function{pango-font-metrics-get-strikethrough-thickness}
      @about-function{pango-font-metrics-get-strikethrough-position}
      @about-class{pango-font}
      @about-function{pango-font-find-shaper}
      @about-function{pango-font-describe}
      @about-function{pango-font-describe-with-absolute-size}
      @about-function{pango-font-get-coverage}
      @about-function{pango-font-get-glyph-extents}
      @about-function{pango-font-get-metrics}
      @about-function{pango-font-get-font-map}
      @about-class{pango-font-family}
      @about-function{pango-font-family-get-name}
      @about-function{pango-font-family-is-monospace}
      @about-function{pango-font-family-list-faces}
      @about-class{pango-font-face}
      @about-function{pango-font-face-get-face-name}
      @about-function{pango-font-face-list-sizes}
      @about-function{pango-font-face-describe}
      @about-function{pango-font-face-is-synthesized}
      @about-class{pango-font-map}
      @about-class{pango-font-map-class}
      @about-function{pango-font-map-create-context}
      @about-function{pango-font-map-load-font}
      @about-function{pango-font-map-load-fontset}
      @about-function{pango-font-map-list-families}
      @about-function{pango-font-map-get-shape-engine-type}
      @about-class{pango-fontset}
      @about-class{pango-fontset-class}
      @about-function{pango-fontset-get-font}
      @about-function{pango-fontset-get-metrics}
      @about-function{pango-fontset-foreach}
      @about-class{pango-fontset-simple}
      @about-function{pango-fontset-simple-new}
      @about-function{pango-fontset-simple-append}
      @about-function{pango-fontset-simple-size}
    @end{subsection}
    @begin[Text Attributes]{subsection}
      Font and other attributes for annotating text.

      Attributed text is used in a number of places in Pango. It is used as the
      input to the itemization process and also when creating a PangoLayout. The
      data types and functions in this section are used to represent and
      manipulate sets of attributes applied to a portion of text.

      @about-symbol{pango-attr-type}
      @about-symbol{pango-attr-class}
      @about-symbol{pango-attribute}
      @about-symbol{pango-attr-string}
      @about-symbol{pango-attr-language}
      @about-symbol{pango-attr-color}
      @about-symbol{pango-attr-int}
      @about-symbol{pango-attr-float}
      @about-symbol{pango-attr-font-desc}
      @about-symbol{pango-attr-shape}
      @about-symbol{pango-attr-size}
      @about-function{pango-parse-markup}
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
      @about-symbol{pango-underline}
      @about-function{pango-attr-shape-new}
      @about-function{pango-attr-shape-new-with-data}
      @about-function{pango-attr-scale-new}
      @about-function{PANGO-SCALE-XX-SMALL}
      @about-function{PANGO-SCALE-X-SMALL}
      @about-function{PANGO-SCALE-SMALL}
      @about-function{PANGO-SCALE-MEDIUM}
      @about-function{PANGO-SCALE-LARGE}
      @about-function{PANGO-SCALE-X-LARGE}
      @about-function{PANGO-SCALE-XX-LARGE}
      @about-function{pango-attr-rise-new}
      @about-function{pango-attr-letter-spacing-new}
      @about-function{pango-attr-fallback-new}
      @about-function{pango-attr-gravity-new}
      @about-function{pango-attr-gravity-hint-new}
      @about-symbol{pango-color}
      @about-function{pango-color-parse}
      @about-function{pango-color-copy}
      @about-function{pango-color-free}
      @about-function{pango-color-to-string}
      @about-class{pango-attr-list}
      @about-function{pango-attr-list-new}
      @about-function{pango-attr-list-ref}
      @about-function{pango-attr-list-unref}
      @about-function{pango-attr-list-copy}
      @about-function{pango-attr-list-insert}
      @about-function{pango-attr-list-insert-before}
      @about-function{pango-attr-list-change}
      @about-function{pango-attr-list-splice}
      @about-function{pango-attr-list-filter}
      @about-function{pango-attr-list-get-iterator}
      @about-symbol{pango-attr-iterator}
      @about-function{pango-attr-iterator-copy}
      @about-function{pango-attr-iterator-next}
      @about-function{pango-attr-iterator-range}
      @about-function{pango-attr-iterator-get}
      @about-function{pango-attr-iterator-get-font}
      @about-function{pango-attr-iterator-get-attrs}
      @about-function{pango-attr-iterator-destroy}
    @end{subsection}
    @begin[Tab Stops]{subsection}
      Structures for storing tab stops.

      @about-class{pango-tab-array}
      @about-function{PANGO_TYPE_TAB_ARRAY}
      @about-symbol{pango-tab-align}
      @about-function{PANGO_TYPE_TAB_ALIGN}
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
    @begin[Text Attribute Markup]{subsection}
      not implemented
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
      @about-function{pango-layout-get-context}
      @about-function{pango-layout-context-changed}
      @about-function{pango-layout-get-serial}

      @about-function{pango-layout-text}

      @about-function{pango-layout-get-character-count}
      @about-function{pango-layout-set-markup}
      @about-function{pango-layout-set-markup-with-accel}

      @about-function{pango-layout-attributes}
      @about-function{pango-layout-font-description}
      @about-function{pango-layout-width}
      @about-function{pango-layout-height}

      @about-function{pango-layout-set-wrap}
      @about-function{pango-layout-get-wrap}
      @about-function{pango-layout-is-wrapped}
      @about-function{pango-layout-set-ellipsize}
      @about-function{pango-layout-get-ellipsize}
      @about-function{pango-layout-is-ellipsized}
      @about-function{pango-layout-set-indent}
      @about-function{pango-layout-get-indent}
      @about-function{pango-layout-get-spacing}
      @about-function{pango-layout-set-spacing}
      @about-function{pango-layout-set-line-spacing}
      @about-function{pango-layout-get-line-spacing}
      @about-function{pango-layout-set-justify}
      @about-function{pango-layout-get-justify}
      @about-function{pango-layout-set-auto-dir}
      @about-function{pango-layout-get-auto-dir}

      @about-function{pango-layout-alignment}

      @about-function{pango-layout-set-tabs}
      @about-function{pango-layout-get-tabs}
      @about-function{pango-layout-set-single-paragraph-mode}
      @about-function{pango-layout-get-single-paragraph-mode}
      @about-function{pango-layout-get-unknown-glyphs-count}
      @about-function{pango-layout-get-log-attrs}
      @about-function{pango-layout-get-log-attrs-readonly}
      @about-function{pango-layout-index-to-pos}
      @about-function{pango-layout-index-to-line-x}
      @about-function{pango-layout-xy-to-index}
      @about-function{pango-layout-get-cursor-pos}
      @about-function{pango-layout-move-cursor-visually}
      @about-function{pango-layout-get-extents}
      @about-function{pango-layout-get-pixel-extents}
      @about-function{pango-layout-get-size}
      @about-function{pango-layout-get-pixel-size}
      @about-function{pango-layout-get-baseline}
      @about-function{pango-layout-get-line-count}
      @about-function{pango-layout-get-line}
      @about-function{pango-layout-get-line-readonly}
      @about-function{pango-layout-get-lines}
      @about-function{pango-layout-get-lines-readonly}
      @about-function{pango-layout-get-iter}
      @about-function{pango-layout-iter-copy}
      @about-function{pango-layout-iter-free}
      @about-function{pango-layout-iter-next-run}
      @about-function{pango-layout-iter-next-char}
      @about-function{pango-layout-iter-next-cluster}
      @about-function{pango-layout-iter-next-line}
      @about-function{pango-layout-iter-at_last-line}
      @about-function{pango-layout-iter-get-index}
      @about-function{pango-layout-iter-get-baseline}
      @about-function{pango-layout-iter-get-run}
      @about-function{pango-layout-iter-get-run-readonly}
      @about-function{pango-layout-iter-get-line}
      @about-function{pango-layout-iter-get-line-readonly}
      @about-function{pango-layout-iter-get-layout}
      @about-function{pango-layout-iter-get-char-extents}
      @about-function{pango-layout-iter-get-cluster-extents}
      @about-function{pango-layout-iter-get-run-extents}
      @about-function{pango-layout-iter-get-line-yrange}
      @about-function{pango-layout-iter-get-line-extents}
      @about-function{pango-layout-iter-get-layout-extents}
      @about-function{pango-layout-line-ref}
      @about-function{pango-layout-line-unref}
      @about-function{pango-layout-line-get-extents}
      @about-function{pango-layout-line-get-pixel-extents}
      @about-function{pango-layout-line-index-to-x}
      @about-function{pango-layout-line-x-to-index}
      @about-function{pango-layout-line-get-x-ranges}
      @about-function{pango-layout-line-get-height}
    @end{subsection}
    @begin[Scripts and Languages]{subsection}
      Identifying writing systems and languages

      @about-symbol{pango-script}
      @about-function{pango-type-script}
      @about-symbol{pango-script-iter}
      @about-function{pango-script-for-unichar}
      @about-function{pango-script-get-sample-language}
      @about-function{pango-script-iter-new}
      @about-function{pango-script-iter-get-range}
      @about-function{pango-script-iter-next}
      @about-function{pango-script-iter-free}
      @about-class{pango-language}
      @about-function{pango-type-language}
      @about-function{pango-language-from-string}
      @about-function{pango-language-to-string}
      @about-function{pango-language-matches}
      @about-function{pango-language-includes-script}
      @about-function{pango-language-get-scripts}
      @about-function{pango-language-get-default}
      @about-function{pango-language-get-sample-string}
    @end{subsection}
    @begin[Bidirectional Text]{subsection}
      Types and functions to help with handling bidirectional text.

      Pango supports bidirectional text (like Arabic and Hebrew) automatically.
      Some applications however, need some help to correctly handle
      bidirectional text.

      The @symbol{pango-direction} type can be used with the function
      @fun{pango-context-set-base-dir} to instruct Pango about direction of
      text, though in most cases Pango detects that correctly and automatically.
      The rest of the facilities in this section are used internally by Pango
      already, and are provided to help applications that need more direct
      control over bidirectional setting of text.

      @about-symbol{pango-direction}
      @about-function{pango-unichar-direction}
      @about-function{pango-find-base-dir}
      @about-function{pango-pango-get-mirror-char}
      @about-symbol{pango-bidi-type}
      @about-function{pango-bidi-type-for-unichar}
    @end{subsection}
    @begin[Vertical Text]{subsection}
      not implemented
    @end{subsection}
  @end{section}
  @begin[Rendering with Pango]{section}
    @begin[Win32 Fonts and Rendering]{subsection}
      not implemented
    @end{subsection}
    @begin[FreeType Fonts and Rendering]{subsection}
      not implemented
    @end{subsection}
    @begin[Xft Fonts and Rendering]{subsection}
      not implemented
    @end{subsection}
    @begin[Cairo Rendering]{subsection}
      not implemented
    @end{subsection}
    @begin[CoreText Fonts]{subsection}
      not implemented
    @end{subsection}
  @end{section}
  @begin[Low Level Functionality]{section}
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
    @begin[OpenType Font Handling]{subsection}
      not implemented
    @end{subsection}
    @begin[Coverage Maps]{subsection}
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
  @end{section}")

;;; --- End of file pango.package.lisp -----------------------------------------
