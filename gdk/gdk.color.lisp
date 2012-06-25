;;; ----------------------------------------------------------------------------
;;; gdk.color.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See http://common-lisp.net/project/cl-gtk2/
;;;
;;; The documentation has been copied from the GDK 3 Reference Manual
;;; Version 3.4.3. See http://www.gtk.org.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2012 Dieter Kaiser
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
;;; Colors
;;;
;;; Manipulation of colors
;;;
;;; Synopsis
;;;
;;;     GdkColor
;;;
;;;     gdk_color_copy
;;;     gdk_color_free
;;;     gdk_color_parse
;;;     gdk_color_equal
;;;     gdk_color_hash
;;;     gdk_color_to_string
;;;
;;; Description
;;;
;;; A GdkColor represents a color.
;;;
;;; When working with cairo, it is often more convenient to use a GdkRGBA
;;; instead.
;;; ----------------------------------------------------------------------------

(in-package :gdk)

;;; ----------------------------------------------------------------------------
;;; GdkColor
;;;
;;; typedef struct {
;;;   guint32 pixel;
;;;   guint16 red;
;;;   guint16 green;
;;;   guint16 blue;
;;; } GdkColor;
;;;
;;; The GdkColor structure is used to describe a color, similar to the XColor
;;; struct used in the X11 drawing API.
;;;
;;; guint32 pixel;
;;;     For allocated colors, the pixel value used to draw this color on the
;;;     screen. Not used anymore.
;;;
;;; guint16 red;
;;;     The red component of the color. This is a value between 0 and 65535,
;;;     with 65535 indicating full intensity
;;;
;;; guint16 green;
;;;     The green component of the color
;;;
;;; guint16 blue;
;;;     The blue component of the color
;;; ----------------------------------------------------------------------------

(define-g-boxed-cstruct gdk-color "GdkColor"
  (pixel :uint32 :initform 0)
  (red :uint16 :initform 0)
  (green :uint16 :initform 0)
  (blue :uint16 :initform 0))

(export (boxed-related-symbols 'gdk-color))

;;; ----------------------------------------------------------------------------
;;; gdk_color_copy ()
;;;
;;; GdkColor * gdk_color_copy (const GdkColor *color);
;;;
;;; Makes a copy of a color structure.
;;;
;;; The result must be freed using gdk_color_free().
;;;
;;; color :
;;;     a GdkColor
;;;
;;; Returns :
;;;     a copy of color
;;; ----------------------------------------------------------------------------

(declaim (inline gdk-color-copy))

(defun gdk-color-copy (color)
  (copy-gdk-color color))

(export 'gdk-color-copy)

;;; ----------------------------------------------------------------------------
;;; gdk_color_free ()
;;;
;;; void gdk_color_free (GdkColor *color);
;;;
;;; Frees a color structure created with gdk_color_copy().
;;;
;;; color :
;;;     a GdkColor
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_color_parse ()
;;;
;;; gboolean gdk_color_parse (const gchar *spec, GdkColor *color);
;;;
;;; Parses a textual specification of a color and fill in the red, green, and
;;; blue fields of a GdkColor structure.
;;;
;;; The string can either one of a large set of standard names (taken from the
;;; X11 rgb.txt file), or it can be a hex value in the form '#rgb' '#rrggbb'
;;; '#rrrgggbbb' or '#rrrrggggbbbb' where 'r', 'g' and 'b' are hex digits of the
;;; red, green, and blue components of the color, respectively. (White in the
;;; four forms is '#fff', '#ffffff', '#fffffffff' and '#ffffffffffff').
;;;
;;; spec :
;;;     the string specifying the color
;;;
;;; color :
;;;     the GdkColor to fill in
;;;
;;; Returns :
;;;     TRUE if the parsing succeeded
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_color_parse" %gdk-color-parse) :boolean
  (spec :string)
  (color (g-boxed-foreign gdk-color)))

(defun gdk-color-parse (color-spec)
  (let ((color (make-gdk-color)))
    (when (%gdk-color-parse color-spec color)
      color)))

(export 'gdk-color-parse)

;;; ----------------------------------------------------------------------------
;;; gdk_color_equal ()
;;;
;;; gboolean gdk_color_equal (const GdkColor *colora, const GdkColor *colorb);
;;;
;;; Compares two colors.
;;;
;;; color1 :
;;;     a GdkColor
;;;
;;; color2 :
;;;     another GdkColor
;;;
;;; Returns :
;;;     TRUE if the two colors compare equal
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_color_equal" gdk-color-equal) :boolean
  (color1 (g-boxed-foreign gdk-color))
  (color2 (g-boxed-foreign gdk-color)))

(export 'gdk-color-equal)

;;; ----------------------------------------------------------------------------
;;; gdk_color_hash ()
;;;
;;; guint gdk_color_hash (const GdkColor *color);
;;;
;;; A hash function suitable for using for a hash table that stores GdkColors.
;;;
;;; color :
;;;     a GdkColor
;;;
;;; Returns :
;;;     The hash function applied to color
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_color_hash" gdk-color-hash) :uint
  (color (g-boxed-foreign gdk-color)))

(export 'gdk-color-hash)

;;; ----------------------------------------------------------------------------
;;; gdk_color_to_string ()
;;;
;;; gchar * gdk_color_to_string (const GdkColor *color);
;;;
;;; Returns a textual specification of color in the hexadecimal form
;;; #rrrrggggbbbb, where r, g and b are hex digits representing the red, green
;;; and blue components respectively.
;;;
;;; The returned string can be parsed by gdk_color_parse().
;;;
;;; color :
;;;     a GdkColor
;;;
;;; Returns :
;;;     a newly-allocated text string
;;;
;;; Since 2.12
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_color_to_string" gdk-color-to-string)
    (g-string :free-from-foreign t)
  (color (g-boxed-foreign gdk-color)))

(export 'gdk-color-to-string)

;;; --- End of file gdk.color.lisp ---------------------------------------------
