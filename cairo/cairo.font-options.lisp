;;; ----------------------------------------------------------------------------
;;; cairo.font-options.lisp
;;;
;;; The documentation of this file is taken from the Cairo Reference Manual
;;; Version 1.12.14 and modified to document the Lisp binding to the Cairo
;;; library. See <http://cairographics.org>. The API documentation of the Lisp
;;; binding is available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2013, 2014 Dieter Kaiser
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
;;; How a font should be rendered
;;;
;;; Synopsis
;;;
;;;     cairo_font_options_t
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
;;;
;;;     cairo_subpixel_order_t
;;;
;;;     cairo_font_options_set_subpixel_order
;;;     cairo_font_options_get_subpixel_order
;;;
;;;     cairo_hint_style_t
;;;
;;;     cairo_font_options_set_hint_style
;;;     cairo_font_options_get_hint_style
;;;
;;;     cairo_hint_metrics_t
;;;
;;;     cairo_font_options_set_hint_metrics
;;;     cairo_font_options_get_hint_metrics
;;; ----------------------------------------------------------------------------

(in-package :cairo)

;;; ----------------------------------------------------------------------------
;;; cairo_font_options_t
;;; ----------------------------------------------------------------------------

(defcstruct cairo-font-options-t)

#+cl-cffi-gtk-documentation
(setf (gethash 'cairo-font-options-t atdoc:*symbol-name-alias*) "CStruct"
      (gethash 'cairo-font-options-t atdoc:*external-symbols*)
 #+cl-cffi-gtk-documentation
 "@version{2014-2-2}
  @begin{short}
    An opaque structure holding all options that are used when rendering fonts.
  @end{short}

  Individual features of a @sym{cairo-font-options-t} can be set or accessed
  using functions named @code{cairo-font-options-set-feature-name} and
  @code{cairo-font-options-get-feature-name}, like the functions
  @fun{cairo-font-options-set-antialias} and
  @fun{cairo-font-options-get-antialias}.

  New features may be added to a @sym{cairo-font-options-t} in the future.
  For this reason, the functions @fun{cairo-font-options-copy},
  @fun{cairo-font-options-equal}, @fun{cairo-font-options-merge}, and
  @fun{cairo-font-options-hash} should be used to copy, check for equality,
  merge, or compute a hash value of @sym{cairo-font-options-t} structures.

  Since 1.0
  @see-function{cairo-font-options-set-antialias}
  @see-function{cairo-font-options-get-antialias}
  @see-function{cairo-font-options-copy}
  @see-function{cairo-font-options-hash}")

(export 'cairo-font-options-t)

;;; ----------------------------------------------------------------------------
;;; cairo_font_options_create ()
;;;
;;; cairo_font_options_t * cairo_font_options_create (void);
;;;
;;; Allocates a new font options object with all options initialized to default
;;; values.
;;;
;;; Returns :
;;;     a newly allocated cairo_font_options_t. Free with
;;;     cairo_font_options_destroy(). This function always returns a valid
;;;     pointer; if memory cannot be allocated, then a special error object is
;;;     returned where all operations on the object do nothing. You can check
;;;     for this with cairo_font_options_status().
;;;
;;; Since 1.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_font_options_copy ()
;;;
;;; cairo_font_options_t * cairo_font_options_copy
;;;                                      (const cairo_font_options_t *original);
;;;
;;; Allocates a new font options object copying the option values from original.
;;;
;;; original :
;;;     a cairo_font_options_t
;;;
;;; Returns :
;;;     a newly allocated cairo_font_options_t. Free with
;;;     cairo_font_options_destroy(). This function always returns a valid
;;;     pointer; if memory cannot be allocated, then a special error object is
;;;     returned where all operations on the object do nothing. You can check
;;;     for this with cairo_font_options_status().
;;;
;;; Since 1.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_font_options_destroy ()
;;;
;;; void cairo_font_options_destroy (cairo_font_options_t *options);
;;;
;;; Destroys a cairo_font_options_t object created with
;;; cairo_font_options_create() or cairo_font_options_copy().
;;;
;;; options :
;;;     a cairo_font_options_t
;;;
;;; Since 1.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_font_options_status ()
;;;
;;; cairo_status_t cairo_font_options_status (cairo_font_options_t *options);
;;;
;;; Checks whether an error has previously occurred for this font options object.
;;;
;;; options :
;;;     a cairo_font_options_t
;;;
;;; Returns :
;;;     CAIRO_STATUS_SUCCESS or CAIRO_STATUS_NO_MEMORY
;;;
;;; Since 1.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_font_options_merge ()
;;;
;;; void cairo_font_options_merge (cairo_font_options_t *options,
;;;                                const cairo_font_options_t *other);
;;;
;;; Merges non-default options from other into options, replacing existing
;;; values. This operation can be thought of as somewhat similar to compositing
;;; other onto options with the operation of CAIRO_OPERATOR_OVER.
;;;
;;; options :
;;;     a cairo_font_options_t
;;;
;;; other :
;;;     another cairo_font_options_t
;;;
;;; Since 1.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_font_options_hash ()
;;;
;;; unsigned long cairo_font_options_hash (const cairo_font_options_t *options);
;;;
;;; Compute a hash for the font options object; this value will be useful when
;;; storing an object containing a cairo_font_options_t in a hash table.
;;;
;;; options :
;;;     a cairo_font_options_t
;;;
;;; Returns :
;;;     The hash value for the font options object. The return value can be cast
;;;     to a 32-bit type if a 32-bit hash value is needed.
;;;
;;; Since 1.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_font_options_equal ()
;;;
;;; cairo_bool_t cairo_font_options_equal (const cairo_font_options_t *options,
;;;                                        const cairo_font_options_t *other);
;;;
;;; Compares two font options objects for equality.
;;;
;;; options :
;;;     a cairo_font_options_t
;;;
;;; other :
;;;     another cairo_font_options_t
;;;
;;; Returns :
;;;     TRUE if all fields of the two font options objects match. Note that this
;;;     function will return FALSE if either object is in error.
;;;
;;; Since 1.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_font_options_set_antialias ()
;;;
;;; void cairo_font_options_set_antialias (cairo_font_options_t *options,
;;;                                        cairo_antialias_t antialias);
;;;
;;; Sets the antialiasing mode for the font options object. This specifies the
;;; type of antialiasing to do when rendering text.
;;;
;;; options :
;;;     a cairo_font_options_t
;;;
;;; antialias :
;;;     the new antialiasing mode
;;;
;;; Since 1.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_font_options_get_antialias ()
;;;
;;; cairo_antialias_t cairo_font_options_get_antialias
;;;                                       (const cairo_font_options_t *options);
;;;
;;; Gets the antialiasing mode for the font options object.
;;;
;;; options :
;;;     a cairo_font_options_t
;;;
;;; Returns :
;;;     the antialiasing mode
;;;
;;; Since 1.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; enum cairo_subpixel_order_t
;;;
;;; typedef enum {
;;;     CAIRO_SUBPIXEL_ORDER_DEFAULT,
;;;     CAIRO_SUBPIXEL_ORDER_RGB,
;;;     CAIRO_SUBPIXEL_ORDER_BGR,
;;;     CAIRO_SUBPIXEL_ORDER_VRGB,
;;;     CAIRO_SUBPIXEL_ORDER_VBGR
;;; } cairo_subpixel_order_t;
;;;
;;; The subpixel order specifies the order of color elements within each pixel
;;; on the display device when rendering with an antialiasing mode of
;;; CAIRO_ANTIALIAS_SUBPIXEL.
;;;
;;; CAIRO_SUBPIXEL_ORDER_DEFAULT
;;;     Use the default subpixel order for for the target device.
;;;
;;; CAIRO_SUBPIXEL_ORDER_RGB
;;;     Subpixel elements are arranged horizontally with red at the left.
;;;
;;; CAIRO_SUBPIXEL_ORDER_BGR
;;;     Subpixel elements are arranged horizontally with blue at the left.
;;;
;;; CAIRO_SUBPIXEL_ORDER_VRGB
;;;     Subpixel elements are arranged vertically with red at the top.
;;;
;;; CAIRO_SUBPIXEL_ORDER_VBGR
;;;     Subpixel elements are arranged vertically with blue at the top.
;;;
;;; Since 1.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_font_options_set_subpixel_order ()
;;;
;;; void cairo_font_options_set_subpixel_order
;;;                                     (cairo_font_options_t *options,
;;;                                      cairo_subpixel_order_t subpixel_order);
;;;
;;; Sets the subpixel order for the font options object. The subpixel order
;;; specifies the order of color elements within each pixel on the display
;;; device when rendering with an antialiasing mode of CAIRO_ANTIALIAS_SUBPIXEL.
;;; See the documentation for cairo_subpixel_order_t for full details.
;;;
;;; options :
;;;     a cairo_font_options_t
;;;
;;; subpixel_order :
;;;     the new subpixel order
;;;
;;; Since 1.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_font_options_get_subpixel_order ()
;;;
;;; cairo_subpixel_order_t cairo_font_options_get_subpixel_order
;;;                                       (const cairo_font_options_t *options);
;;;
;;; Gets the subpixel order for the font options object. See the documentation
;;; for cairo_subpixel_order_t for full details.
;;;
;;; options :
;;;     a cairo_font_options_t
;;;
;;; Returns :
;;;     the subpixel order for the font options object
;;;
;;; Since 1.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; enum cairo_hint_style_t
;;;
;;; typedef enum {
;;;     CAIRO_HINT_STYLE_DEFAULT,
;;;     CAIRO_HINT_STYLE_NONE,
;;;     CAIRO_HINT_STYLE_SLIGHT,
;;;     CAIRO_HINT_STYLE_MEDIUM,
;;;     CAIRO_HINT_STYLE_FULL
;;; } cairo_hint_style_t;
;;;
;;; Specifies the type of hinting to do on font outlines. Hinting is the process
;;; of fitting outlines to the pixel grid in order to improve the appearance of
;;; the result. Since hinting outlines involves distorting them, it also reduces
;;; the faithfulness to the original outline shapes. Not all of the outline
;;; hinting styles are supported by all font backends.
;;;
;;; New entries may be added in future versions.
;;;
;;; CAIRO_HINT_STYLE_DEFAULT
;;;     Use the default hint style for font backend and target device.
;;;
;;; CAIRO_HINT_STYLE_NONE
;;;     Do not hint outlines.
;;;
;;; CAIRO_HINT_STYLE_SLIGHT
;;;     Hint outlines slightly to improve contrast while retaining good fidelity
;;;     to the original shapes.
;;;
;;; CAIRO_HINT_STYLE_MEDIUM
;;;     Hint outlines with medium strength giving a compromise between fidelity
;;;     to the original shapes and contrast.
;;;
;;; CAIRO_HINT_STYLE_FULL
;;;     Hint outlines to maximize contrast.
;;;
;;; Since 1.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_font_options_set_hint_style ()
;;;
;;; void cairo_font_options_set_hint_style (cairo_font_options_t *options,
;;;                                         cairo_hint_style_t hint_style);
;;;
;;; Sets the hint style for font outlines for the font options object. This
;;; controls whether to fit font outlines to the pixel grid, and if so, whether
;;; to optimize for fidelity or contrast. See the documentation for
;;; cairo_hint_style_t for full details.
;;;
;;; options :
;;;     a cairo_font_options_t
;;;
;;; hint_style :
;;;     the new hint style
;;;
;;; Since 1.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_font_options_get_hint_style ()
;;;
;;; cairo_hint_style_t cairo_font_options_get_hint_style
;;;                                       (const cairo_font_options_t *options);
;;;
;;; Gets the hint style for font outlines for the font options object. See the
;;; documentation for cairo_hint_style_t for full details.
;;;
;;; options :
;;;     a cairo_font_options_t
;;;
;;; Returns :
;;;     the hint style for the font options object
;;;
;;; Since 1.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; enum cairo_hint_metrics_t
;;;
;;; typedef enum {
;;;     CAIRO_HINT_METRICS_DEFAULT,
;;;     CAIRO_HINT_METRICS_OFF,
;;;     CAIRO_HINT_METRICS_ON
;;; } cairo_hint_metrics_t;
;;;
;;; Specifies whether to hint font metrics; hinting font metrics means
;;; quantizing them so that they are integer values in device space. Doing this
;;; improves the consistency of letter and line spacing, however it also means
;;; that text will be laid out differently at different zoom factors.
;;;
;;; CAIRO_HINT_METRICS_DEFAULT
;;;     Hint metrics in the default manner for the font backend and target
;;;     device.
;;;
;;; CAIRO_HINT_METRICS_OFF
;;;     Do not hint font metrics.
;;;
;;; CAIRO_HINT_METRICS_ON
;;;     Hint font metrics.
;;;
;;; Since 1.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_font_options_set_hint_metrics ()
;;;
;;; void cairo_font_options_set_hint_metrics (cairo_font_options_t *options,
;;;                                           cairo_hint_metrics_t hint_metrics)
;;;
;;; Sets the metrics hinting mode for the font options object. This controls
;;; whether metrics are quantized to integer values in device units. See the
;;; documentation for cairo_hint_metrics_t for full details.
;;;
;;; options :
;;;     a cairo_font_options_t
;;;
;;; hint_metrics :
;;;     the new metrics hinting mode
;;;
;;; Since 1.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_font_options_get_hint_metrics ()
;;;
;;; cairo_hint_metrics_t cairo_font_options_get_hint_metrics
;;;                                       (const cairo_font_options_t *options);
;;;
;;; Gets the metrics hinting mode for the font options object. See the
;;; documentation for cairo_hint_metrics_t for full details.
;;;
;;; options :
;;;     a cairo_font_options_t
;;;
;;; Returns :
;;;     the metrics hinting mode for the font options object
;;;
;;; Since 1.0
;;; ----------------------------------------------------------------------------

;;; --- cairo.font-options.lisp ------------------------------------------------
