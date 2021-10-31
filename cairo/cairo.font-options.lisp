;;; ----------------------------------------------------------------------------
;;; cairo.font-options.lisp
;;;
;;; The documentation of the file is taken from the Cairo Reference Manual
;;; Version 1.16 and modified to document the Lisp binding to the Cairo
;;; library. See <http://cairographics.org>. The API documentation of the
;;; Lisp binding is available at <http://www.crategus.com/books/cl-cffi-gtk/>.
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
;;; cairo_font_options_t
;;;
;;;     How a font should be rendered
;;;
;;; Types and Values
;;;
;;;     cairo_font_options_t
;;;     cairo_subpixel_order_t
;;;     cairo_hint_style_t
;;;     cairo_hint_metrics_t
;;;
;;; Functions
;;;
;;;     cairo_font_options_create
;;;     cairo_font_options_copy
;;;     cairo_font_options_destroy
;;;     cairo_font_options_status
;;;     cairo_font_options_merge
;;;     cairo_font_options_hash
;;;     cairo_font_options_equal
;;;     cairo_font_options_set_antialias
;;;     cairo_font_options_get_antialias
;;;     cairo_font_options_set_subpixel_order
;;;     cairo_font_options_get_subpixel_order
;;;     cairo_font_options_set_hint_style
;;;     cairo_font_options_get_hint_style
;;;     cairo_font_options_set_hint_metrics
;;;     cairo_font_options_get_hint_metrics
;;;     cairo_font_options_get_variations
;;;     cairo_font_options_set_variations
;;;
;;; Description
;;;
;;; The font options specify how fonts should be rendered. Most of the time the
;;; font options implied by a surface are just right and do not need any
;;; changes, but for pixel-based targets tweaking font options may result in
;;; superior output on a particular display.
;;; ----------------------------------------------------------------------------

(in-package :cairo)

;;; ----------------------------------------------------------------------------
;;; cairo_font_options_t
;;; ----------------------------------------------------------------------------

(defcstruct cairo-font-options-t)

#+cl-cffi-gtk-documentation
(setf (gethash 'cairo-font-options-t atdoc:*symbol-name-alias*)
      "CStruct"
      (gethash 'cairo-font-options-t atdoc:*external-symbols*)
 #+cl-cffi-gtk-documentation
 "@version{2021-10-27}
  @begin{short}
    An opaque structure holding all options that are used when rendering fonts.
  @end{short}

  Individual features of a @sym{cairo-font-options-t} instance can be set or
  accessed using accessor functions named
  @code{cairo-font-options-feature-name}, like the
  @fun{cairo-font-options-antialias} function.

  New features may be added to a @sym{cairo-font-options-t} structure in the
  future. For this reason, the @fun{cairo-font-options-copy},
  @fun{cairo-font-options-equal}, @fun{cairo-font-options-merge}, and
  @fun{cairo-font-options-hash} functions should be used to copy, check for
  equality, merge, or compute a hash value of @sym{cairo-font-options-t}
  instances.
  @see-function{cairo-font-options-copy}
  @see-function{cairo-font-options-equal}
  @see-function{cairo-font-options-merge}
  @see-function{cairo-font-options-hash}
  @see-function{cairo-font-options-antialias}")

(export 'cairo-font-options-t)

;;; ----------------------------------------------------------------------------
;;; enum cairo_subpixel_order_t
;;; ----------------------------------------------------------------------------

(defcenum cairo-subpixel-order-t
  :default
  :rgb
  :bgr
  :vrgb
  :vbgr)

#+cl-cffi-gtk-documentation
(setf (gethash 'cairo-subpixel-order-t atdoc:*symbol-name-alias*)
      "CEnum"
      (gethash 'cairo-subpixel-order-t atdoc:*external-symbols*)
 "@version{2021-10-27}
  @begin{short}
    The subpixel order specifies the order of color elements within each pixel
    on the display device when rendering with an antialiasing mode of
    @code{:subpixel}.
  @end{short}
  @begin{pre}
(defcenum cairo-subpixel-order-t
  :default
  :rgb
  :bgr
  :vrgb
  :vbgr)
  @end{pre}
  @begin[code]{table}
    @entry[:default]{Use the default subpixel order for the target device.}
    @entry[:rgb]{Subpixel elements are arranged horizontally with red at the
      left.}
    @entry[:bgr]{Subpixel elements are arranged horizontally with blue at the
      left.}
    @entry[:vrgb]{Subpixel elements are arranged vertically with red at the
      top.}
    @entry[:vbgr]{Subpixel elements are arranged vertically with blue at the
      top.}
  @end{table}
  @see-symbol{cairo-font-options-t}
  @see-symbol{cairo-antialias-t}")

(export 'cairo-subpixel-order-t)

;;; ----------------------------------------------------------------------------
;;; enum cairo_hint_style_t
;;; ----------------------------------------------------------------------------

(defcenum cairo-hint-style-t
  :default
  :none
  :slight
  :medium
  :full)

#+cl-cffi-gtk-documentation
(setf (gethash 'cairo-hint-style-t atdoc:*symbol-name-alias*)
      "CEnum"
      (gethash 'cairo-hint-style-t atdoc:*external-symbols*)
 "@version{2021-10-27}
  @begin{short}
    Specifies the type of hinting to do on font outlines.
  @end{short}
  Hinting is the process of fitting outlines to the pixel grid in order to
  improve the appearance of the result. Since hinting outlines involves
  distorting them, it also reduces the faithfulness to the original outline
  shapes. Not all of the outline hinting styles are supported by all font
  backends.

  New entries may be added in future versions.
  @begin{pre}
(defcenum cairo-hint-style-t
  :default
  :none
  :slight
  :medium
  :full)
  @end{pre}
  @begin[code]{table}
    @entry[:default]{Use the default hint style for font backend and target
      device.}
    @entry[:none]{Do not hint outlines.}
    @entry[:sligth]{Hint outlines slightly to improve contrast while retaining
      good fidelity to the original shapes.}
    @entry[:medium]{Hint outlines with medium strength giving a compromise
      between fidelity to the original shapes and contrast.}
    @entry[:full]{Hint outlines to maximize contrast.}
  @end{table}
  @see-symbol{cairo-font-options-t}")

(export 'cairo-hint-style-t)

;;; ----------------------------------------------------------------------------
;;; enum cairo_hint_metrics_t
;;; ----------------------------------------------------------------------------

(defcenum cairo-hint-metrics-t
  :default
  :off
  :on)

#+cl-cffi-gtk-documentation
(setf (gethash 'cairo-hint-metrics-t atdoc:*symbol-name-alias*)
      "CEnum"
      (gethash 'cairo-hint-metrics-t atdoc:*external-symbols*)
 "@version{2021-10-27}
  @begin{short}
    Specifies whether to hint font metrics.
  @end{short}
  Hinting font metrics means quantizing them so that they are integer values in
  device space. Doing this improves the consistency of letter and line spacing,
  however it also means that text will be laid out differently at different
  zoom factors.
  @begin{pre}
(defcenum cairo-hint-metrics-t
  :default
  :off
  :on)
  @end{pre}
  @begin[code]{table}
    @entry[:default]{Hint metrics in the default manner for the font backend
      and target device.}
    @entry[:off]{Do not hint font metrics.}
    @entry[:on]{Hint font metrics.}
  @end{table}
  @see-symbol{cairo-font-options-t}")

(export 'cairo-hint-metrics-t)

;;; ----------------------------------------------------------------------------
;;; cairo_font_options_create ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_font_options_create" cairo-font-options-create)
    (:pointer (:struct cairo-font-options-t))
 #+cl-cffi-gtk-documentation
 "@version{2021-10-27}
  @return{A newly allocated @symbol{cairo-font-options-t} instance.}
  @begin{short}
    Allocates a new font options object with all options initialized to default
    values.
  @end{short}
  Free with the @fun{cairo-font-options-destroy} function. This function always
  returns a valid pointer. If memory cannot be allocated, then a special error
  object is returned where all operations on the object do nothing. You can
  check for this with the @fun{cairo-font-options-status} function.
  @see-symbol{cairo-font-options-t}
  @see-function{cairo-font-options-destroy}
  @see-function{cairo-font-options-status}")

(export 'cairo-font-options-create)

;;; ----------------------------------------------------------------------------
;;; cairo_font_options_copy ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_font_options_copy" cairo-font-options-copy)
    (:pointer (:struct cairo-font-options-t))
 #+cl-cffi-gtk-documentation
 "@version{2021-10-27}
  @argument[options]{a @symbol{cairo-font-options-t} instance}
  @return{A newly allocated @symbol{cairo-font-options-t} instance.}
  @begin{short}
    Allocates a new font options object copying the option values from original.
  @end{short}
  Free with the @fun{cairo-font-options-destroy} function. This function always
  returns a valid pointer. If memory cannot be allocated, then a special error
  object is returned where all operations on the object do nothing. You can
  check for this with the @fun{cairo-font-options-status} function.
  @see-symbol{cairo-font-options-t}
  @see-function{cairo-font-options-destroy}
  @see-function{cairo-font-options-status}"
  (options (:pointer (:struct cairo-font-options-t))))

(export 'cairo-font-options-copy)

;;; ----------------------------------------------------------------------------
;;; cairo_font_options_destroy ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_font_options_destroy" cairo-font-options-destroy) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-10-27}
  @argument[options]{a @symbol{cairo-font-options-t} instance}
  @begin{short}
    Destroys a @symbol{cairo-font-options-t} instance created with the
    @fun{cairo-font-options-create} or @fun{cairo-font-options-copy} function.
  @end{short}
  @see-class{cairo-font-options-t}
  @see-function{cairo-font-options-create}
  @see-function{cairo-font-options-copy}"
  (options (:pointer (:struct cairo-font-options-t))))

(export 'cairo-font-options-destroy)

;;; ----------------------------------------------------------------------------
;;; cairo_font_options_status ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_font_options_status" cairo-font-options-status) cairo-status-t
 #+cl-cffi-gtk-documentation
 "@version{2021-10-27}
  @argument[options]{a @symbol{cairo-font-options-t} instance}
  @return{The @code{:success} or @code{:no-memory} values of the
    @symbol{cairo-status-t} enumeration.}
  @begin{short}
    Checks whether an error has previously occurred for this font options
    instance.
  @end{short}
  @see-symbol{cairo-font-options-t}
  @see-symbol{cairo-status-t}"
  (options (:pointer (:struct cairo-font-options-t))))

(export 'cairo-font-options-status)

;;; ----------------------------------------------------------------------------
;;; cairo_font_options_merge ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_font_options_merge" cairo-font-options-merge) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-10-27}
  @argument[options]{a @symbol{cairo-font-options-t} instance}
  @argument[other]{another @symbol{cairo-font-options-t} instance}
  @begin{short}
    Merges non-default options from @arg{other} into @arg{options}, replacing
    existing values.
  @end{short}
  This operation can be thought of as somewhat similar to compositing
  @arg{other} onto @arg{options} with the operation of @code{:over}.
  @see-symbol{cairo-font-options-t}
  @see-symbol{cairo-operator-t}"
  (options (:pointer (:struct cairo-font-options-t)))
  (other (:pointer (:struct cairo-font-options-t))))

(export 'cairo-font-options-merge)

;;; ----------------------------------------------------------------------------
;;; cairo_font_options_hash ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_font_options_hash" cairo-font-options-hash) :ulong
 #+cl-cffi-gtk-documentation
 "@version{2021-10-27}
  @argument[options]{a @symbol{cairo-font-options-t} instance}
  @return{An unsigned long integer with the hash value for the font options
    object.}
  @begin{short}
    Compute a hash for the font options instance.
  @end{short}
  This value will be useful when storing an object containing a
  @symbol{cairo-font-options-t} instance in a hash table.
  @see-symbol{cairo-font-options-t}"
  (options (:pointer (:struct cairo-font-options-t))))

(export 'cairo-font-options-hash)

;;; ----------------------------------------------------------------------------
;;; cairo_font_options_equal ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_font_options_equal" cairo-font-options-equal) cairo-bool-t
 #+cl-cffi-gtk-documentation
 "@version{2021-10-27}
  @argument[options]{a @symbol{cairo-font-options-t} instance}
  @argument[other]{another @symbol{cairo-font-options-t} instance}
  @return{@em{True} if all fields of the two font options instances match. Note
    that this function will return @em{false} if either instance is in error.}
  @begin{short}
    Compares two font options instances for equality.
  @end{short}
  @see-symbol{cairo-font-options-t}"
  (options (:pointer (:struct cairo-font-options-t)))
  (other (:pointer (:struct cairo-font-options-t))))

(export 'cairo-font-options-equal)

;;; ----------------------------------------------------------------------------
;;; cairo_font_options_get_antialias ()
;;; cairo_font_options_set_antialias () -> cairo-font-options-antialias
;;; ----------------------------------------------------------------------------

(defun (setf cairo-font-options-antialias) (antialias options)
  (foreign-funcall "cairo_font_options_antialias"
                   (:pointer (:struct cairo-font-options-t)) options
                   cairo-antialias-t antialias
                   :void)
  antialias)

(defcfun ("cairo_font_options_get_antialias" cairo-font-options-antialias)
    cairo-antialias-t
 #+cl-cffi-gtk-documentation
 "@version{2021-10-27}
  @syntax[]{(cairo-font-options-antialias options) => antialias}
  @syntax[]{(setf (cairo-font-options-antialias options) antialias)}
  @argument[options]{a @symbol{cairo-font-options-t} instance}
  @argument[antialias]{a value of the @symbol{cairo-antialias-t} enumeration}
  @begin{short}
    The @sym{cairo-font-options-antialias} function gets the antialiasing mode
    for the font options instance.
  @end{short}
  The @sym{(setf cairo-font-options-antialias)} function sets the antialiasing
  mode. This specifies the type of antialiasing to do when rendering text.
  @see-symbol{cairo-font-options-t}
  @see-symbol{cairo-antialias-t}"
  (options (:pointer (:struct cairo-font-options-t))))

(export 'cairo-font-options-antialias)

;;; ----------------------------------------------------------------------------
;;; cairo_font_options_get_subpixel_order ()
;;; cairo_font_options_set_subpixel_order ()
;;; -> cairo-font-options-subpixel-order
;;; ----------------------------------------------------------------------------

(defun (setf cairo-font-options-subpixel-order) (order options)
  (foreign-funcall "cairo_font_options_set_subpixel_order"
                   (:pointer (:struct cairo-font-options-t)) options
                   cairo-subpixel-order-t order
                   :void)
  order)

(defcfun ("cairo_font_options_get_subpixel_order"
           cairo-font-options-subpixel-order) cairo-subpixel-order-t
 #+cl-cffi-gtk-documentation
 "@version{2021-10-28}
  @syntax[]{(cairo-font-options-subpixel-order options) => order}
  @syntax[]{(setf (cairo-font-options-subpixel-order options) order)}
  @argument[options]{a @symbol{cairo-font-options-t} instance}
  @argument[order]{a value of the @symbol{cairo-subpixel-order-t} enumeration}
  @begin{short}
    The @sym{cairo-font-options-subpixel-order} function gets the subpixel order
    for the font options instance.
  @end{short}
  The @sym{(setf cairo-font-options-subpixel-order)} function sets the subpixel
  order. The subpixel order specifies the order of color elements within each
  pixel on the display device when rendering with an antialiasing mode of
  @code{:subpixel}. See the documentation for the
  @symbol{cairo-subpixel-order-t} enumeration for full details.
  @see-symbol{cairo-font-options-t}
  @see-symbol{cairo-subpixel-order-t}"
  (options (:pointer (:struct cairo-font-options-t))))

(export 'cairo-font-options-subpixel-order)

;;; ----------------------------------------------------------------------------
;;; cairo_font_options_get_hint_style ()
;;; cairo_font_options_set_hint_style () -> cairo-font-options-hint-style
;;; ----------------------------------------------------------------------------

(defun (setf cairo-font-options-hint-style) (style options)
  (foreign-funcall "cairo_font_options_set_hint_style"
                   (:pointer (:struct cairo-font-options-t)) options
                   cairo-hint-style-t style
                   :void)
  style)

(defcfun ("cairo_font_options_get_hint_style"
           cairo-font-options-hint-style) cairo-hint-style-t
 #+cl-cffi-gtk-documentation
 "@version{2021-10-28}
  @syntax[]{(cairo-font-options-hint-style options) => style}
  @syntax[]{(setf (cairo-font-options-hint-style options) style)}
  @argument[options]{a @symbol{cairo-font-options-t} instance}
  @argument[style]{a value of the @symbol{cairo-hint-style-t} enumeration}
  @begin{short}
    The @sym{cairo-font-options-hint-style} function gets the hint style for
    font outlines for the font options instance.
  @end{short}
  The @sym{(setf cairo-font-options-hint-style)} function sets the hint style
  for font outlines for the font options instance. This controls whether to fit
  font outlines to the pixel grid, and if so, whether to optimize for fidelity
  or contrast. See the documentation for the @symbol{cairo-hint-style-t}
  enumeration for full details.
  @see-symbol{cairo-font-options-t}
  @see-symbol{cairo-hint-style-t}"
  (options (:pointer (:struct cairo-font-options-t))))

(export 'cairo-font-options-hint-style)

;;; ----------------------------------------------------------------------------
;;; cairo_font_options_get_hint_metrics ()
;;; cairo_font_options_set_hint_metrics () -> cairo-font-options-hint-metrics
;;; ----------------------------------------------------------------------------

(defun (setf cairo-font-options-hint-metrics) (metrics options)
  (foreign-funcall "cairo_font_options_set_hint_metrics"
                   (:pointer (:struct cairo-font-options-t)) options
                   cairo-hint-metrics-t metrics
                   :void)
  metrics)

(defcfun ("cairo_font_options_get_hint_metrics"
           cairo-font-options-hint-metrics) cairo-hint-metrics-t
 #+cl-cffi-gtk-documentation
 "@version{2021-10-28}
  @syntax[]{(cairo-font-options-hint-metrics options) => metrics}
  @syntax[]{(setf (cairo-font-options-hint-metrics options) metrics)}
  @argument[options]{a @symbol{cairo-font-options-t} instance}
  @argument[metrics]{a value of the @symbol{cairo-hint-metrics-t} enumeration}
  @begin{short}
    The @sym{cairo-font-options-hint-metrics} function gets the metrics hinting
    mode for the font options instance.
  @end{short}
  The @sym{(setf cairo-font-options-hint-metrics)} function sets the metrics
  hinting mode for the font options instance. This controls whether metrics are
  quantized to integer values in device units. See the documentation for
  the @symbol{cairo-hint-metrics-t} enumeration for full details.
  @see-symbol{cairo-font-options-t}
  @see-symbol{cairo-hint-metrics-t}"
  (options (:pointer (:struct cairo-font-options-t))))

(export 'cairo-font-options-hint-metrics)

;;; ----------------------------------------------------------------------------
;;; cairo_font_options_get_variations ()
;;; cairo_font_options_set_variations () -> cairo-font-options-variations
;;; ----------------------------------------------------------------------------

(defun (setf cairo-font-options-variations) (variations options)
  (foreign-funcall "cairo_font_options_set_variations"
                   (:pointer (:struct cairo-font-options-t)) options
                   :string (if variations variations (null-pointer))
                   :void)
  variations)

(defcfun ("cairo_font_options_get_variations"
           cairo-font-options-variations) :string
 #+cl-cffi-gtk-documentation
 "@version{2021-10-28}
  @syntax[]{(cairo-font-options-variations options) => variations}
  @syntax[]{(setf (cairo-font-options-variations options) variations)}
  @argument[options]{a @symbol{cairo-font-options-t} instance}
  @argument[variations]{a string with the font variations, or @code{nil}}
  @begin{short}
    The @sym{cairo-font-options-variations} function gets the OpenType font
    variations for the font options instance.
  @end{short}
  The @sym{(setf cairo-font-options-variations)} function sets the OpenType
  font variations for the font options instance. The returned string belongs to
  the font options instance and must not be modified. It is valid until either
  the font options instance is destroyed or the font variations in the font
  options instance is modified.

  Font variations are specified as a string with a format that is similar to the
  CSS font-variation-settings. The string contains a comma-separated list of
  axis assignments, which each assignment consists of a 4-character axis name
  and a value, separated by whitespace and optional equals sign.
  @begin[Examples]{dictionary}
    @begin{pre}
      wght=200, wdth=140.5
      wght 200, wdth 140.5
    @end{pre}
  @end{dictionary}
  @see-symbol{cairo-font-options-t}"
  (options (:pointer (:struct cairo-font-options-t))))

(export 'cairo-font-options-variations)

;;; --- cairo.font-options.lisp ------------------------------------------------
