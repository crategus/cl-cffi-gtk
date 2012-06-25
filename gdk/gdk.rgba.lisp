;;; ----------------------------------------------------------------------------
;;; gdk.rgba.lisp
;;; 
;;; The documentation has been copied from the GDK 3 Reference Manual
;;; Version 3.4.3. See http://www.gtk.org.
;;; 
;;; Copyright (C) 2012 Dieter Kaiser
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
;;; RGBA Colors
;;; 
;;; RGBA colors
;;;     
;;; Synopsis
;;; 
;;;     GdkRGBA
;;;     
;;;     gdk_rgba_copy
;;;     gdk_rgba_free
;;;     gdk_rgba_parse
;;;     gdk_rgba_equal
;;;     gdk_rgba_hash
;;;     gdk_rgba_to_string
;;;     
;;; Description
;;; 
;;; The GdkRGBA struct is a convenient way to pass rgba colors around. It's
;;; based on cairo's way to deal with colors and mirrors its behavior. All
;;; values are in the range from 0.0 to 1.0 inclusive. So the color
;;; (0.0, 0.0, 0.0, 0.0) represents transparent black and (1.0, 1.0, 1.0, 1.0)
;;; is opaque white. Other values will be clamped to this range when drawing.
;;; ----------------------------------------------------------------------------

(in-package :gdk)

;;; ----------------------------------------------------------------------------
;;; GdkRGBA
;;; 
;;; typedef struct {
;;;   gdouble red;
;;;   gdouble green;
;;;   gdouble blue;
;;;   gdouble alpha;
;;; } GdkRGBA;
;;; 
;;; The GdkRGBA structure is used to represent a (possibly translucent) color,
;;; in a way that is compatible with cairos notion of color.
;;; 
;;; gdouble red;
;;;     The intensity of the red channel from 0.0 to 1.0 inclusive
;;; 
;;; gdouble green;
;;;     The intensity of the green channel from 0.0 to 1.0 inclusive
;;; 
;;; gdouble blue;
;;;     The intensity of the blue channel from 0.0 to 1.0 inclusive
;;; 
;;; gdouble alpha;
;;;     The opacity of the color from 0.0 for completely translucent to 1.0 for
;;;     opaque
;;; ----------------------------------------------------------------------------

(define-g-boxed-cstruct gdk-rgba "GdkRGBA"
  (red :double :initform 0.0d0)
  (green :double :initform 0.0d0)
  (blue :double :initform 0.0d0)
  (alpha :double :initform 0.0d0))

(export (boxed-related-symbols 'gdk-rgba))

;;; ----------------------------------------------------------------------------
;;; gdk_rgba_copy ()
;;; 
;;; GdkRGBA * gdk_rgba_copy (const GdkRGBA *rgba);
;;; 
;;; Makes a copy of a GdkRGBA structure.
;;; 
;;; The result must be freed through gdk_rgba_free().
;;; 
;;; rgba :
;;;     a GdkRGBA
;;; 
;;; Returns :
;;;     A newly allocated GdkRGBA, with the same contents as rgba
;;; 
;;; Since 3.0
;;; ----------------------------------------------------------------------------

(declaim (inline gdk-rgba-copy))

(defun gdk-rgba-copy (rgba)
  (copy-gdk-rgba rgba))

(export 'gdk-rgba-copy)

;;; ----------------------------------------------------------------------------
;;; gdk_rgba_free ()
;;; 
;;; void gdk_rgba_free (GdkRGBA *rgba);
;;; 
;;; Frees a GdkRGBA struct created with gdk_rgba_copy()
;;; 
;;; rgba :
;;;     a GdkRGBA
;;; 
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_rgba_parse ()
;;; 
;;; gboolean gdk_rgba_parse (GdkRGBA *rgba, const gchar *spec);
;;; 
;;; Parses a textual representation of a color, filling in the red, green, blue
;;; and alpha fields of the rgba struct.
;;; 
;;; The string can be either one of:
;;; 
;;;     A standard name (Taken from the X11 rgb.txt file).
;;;     A hex value in the form 'rgb' 'rrggbb' 'rrrgggbbb' or 'rrrrggggbbbb'
;;;     A RGB color in the form 'rgb(r,g,b)' (In this case the color will have
;;;       full opacity)
;;;     A RGBA color in the form 'rgba(r,g,b,a)'
;;; 
;;; Where 'r', 'g', 'b' and 'a' are respectively the red, green, blue and alpha
;;; color values. In the last two cases, r g and b are either integers in the
;;; range 0 to 255 or precentage values in the range 0% to 100%, and a is a
;;; floating point value in the range 0 to 1.
;;; 
;;; rgba :
;;;     the GdkRGBA struct to fill in
;;; 
;;; spec :
;;;     the string specifying the color
;;; 
;;; Returns :
;;;     TRUE if the parsing succeeded
;;; 
;;; Since 3.0
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_rgba_parse" %gdk-rgba-parse) :boolean
  (rgba (g-boxed-foreign gdk-rgba))
  (spec :string))

(defun gdk-rgba-parse (spec)
  (let ((rgba (make-gdk-rgba)))
    (when (%gdk-rgba-parse rgba spec)
      rgba)))

(export 'gdk-rgba-parse)

;;; ----------------------------------------------------------------------------
;;; gdk_rgba_equal ()
;;; 
;;; gboolean gdk_rgba_equal (gconstpointer p1, gconstpointer p2);
;;; 
;;; Compares two RGBA colors.
;;; 
;;; p1 :
;;;     a GdkRGBA pointer
;;; 
;;; p2 :
;;;     another GdkRGBA pointer
;;; 
;;; Returns :
;;;     TRUE if the two colors compare equal
;;; 
;;; Since 3.0
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_rgba_equal" gdk-rgba-equal) :boolean
  (color1 (g-boxed-foreign gdk-rgba))
  (color2 (g-boxed-foreign gdk-rgba)))

(export 'gdk-rgba-equal)

;;; ----------------------------------------------------------------------------
;;; gdk_rgba_hash ()
;;; 
;;; guint gdk_rgba_hash (gconstpointer p);
;;; 
;;; A hash function suitable for using for a hash table that stores GdkRGBAs.
;;; 
;;; p :
;;;     a GdkRGBA pointer
;;; 
;;; Returns :
;;;     The hash value for p
;;; 
;;; Since 3.0
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_rgba_hash" gdk-rgba-hash) :uint
  (p (g-boxed-foreign gdk-rgba)))

(export 'gdk-rgba-hash)

;;; ----------------------------------------------------------------------------
;;; gdk_rgba_to_string ()
;;; 
;;; gchar * gdk_rgba_to_string (const GdkRGBA *rgba);
;;; 
;;; Returns a textual specification of rgba in the form rgb (r, g, b) or
;;; rgba (r, g, b, a), where 'r', 'g', 'b' and 'a' represent the red, green,
;;; blue and alpha values respectively. r, g, and b are represented as integers
;;; in the range 0 to 255, and a is represented as floating point value in the
;;; range 0 to 1.
;;; 
;;; These string forms are string forms those supported by the CSS3 colors
;;; module, and can be parsed by gdk_rgba_parse().
;;; 
;;; Note that this string representation may loose some precision, since r, g
;;; and b are represented as 8-bit integers. If this is a concern, you should
;;; use a different representation.
;;; 
;;; rgba :
;;;     a GdkRGBA
;;; 
;;; Returns :
;;;     A newly allocated text string
;;; 
;;; Since 3.0
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_rgba_to_string" gdk-rgba-to-string) :string
  (rgba (g-boxed-foreign gdk-rgba)))

(export 'gdk-rgba-to-string)

;;; --- End of file gdk.rgba.lisp ----------------------------------------------
