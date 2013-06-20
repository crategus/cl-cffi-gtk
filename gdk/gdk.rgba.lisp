;;; ----------------------------------------------------------------------------
;;; gdk.rgba.lisp
;;; 
;;; The documentation has been copied from the GDK 3 Reference Manual
;;; Version 3.4.3. See http://www.gtk.org. See <http://www.gtk.org>.
;;; The API  documentation of the Lisp binding is available at
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
;;; ----------------------------------------------------------------------------

(define-g-boxed-cstruct gdk-rgba "GdkRGBA"
  (red :double :initform 0.0d0)
  (green :double :initform 0.0d0)
  (blue :double :initform 0.0d0)
  (alpha :double :initform 0.0d0))

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-rgba atdoc:*class-name-alias*) "CStruct"
      (documentation 'gdk-rgba 'type)
 "@version{2013-4-5}
  @begin{short}
    The GdkRGBA structure is used to represent a (possibly translucent) color,
    in a way that is compatible with cairos notion of color.
  @end{short}
  @begin{pre}
(define-g-boxed-cstruct gdk-rgba \"GdkRGBA\"
  (red :double :initform 0.0d0)
  (green :double :initform 0.0d0)
  (blue :double :initform 0.0d0)
  (alpha :double :initform 0.0d0))
  @end{pre}
  @begin[code]{table}
    @entry[red]{The intensity of the red channel from 0.0 to 1.0 inclusive.}
    @entry[green]{The intensity of the green channel from 0.0 to 1.0 inclusive.}
    @entry[blue]{The intensity of the blue channel from 0.0 to 1.0 inclusive.}
    @entry[alpha]{The opacity of the color from 0.0 for completely translucent
      to 1.0 for opaque.}
  @end{table}
  @see-constructor{make-gdk-rgba}
  @see-constructor{copy-gdk-rgba}
  @see-slot{red}
  @see-slot{green}
  @see-slot{blue}
  @see-slot{alpha}")

(export (boxed-related-symbols 'gdk-rgba))

;;; --- copy-gdk-rgba ----------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation 'copy-gdk-rgba 'function)
 "@version{2013-4-5}
  @argument[instance]{a @class{gdk-rgba} struct}
  Copy constructor of a @class{gdk-rgba} struct.")

;;; --- make-gdk-rgba ----------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation 'make-gdk-rgba 'function)
 "@version{2013-4-5}
  @argument[red]{The intensity of the red channel from 0.0 to 1.0 inclusive.}
  @argument[green]{The intensity of the green channel from 0.0 to 1.0
    inclusive.}
  @argument[blue]{The intensity of the blue channel from 0.0 to 1.0 inclusive.}
  @argument[alpha]{The opacity of the color from 0.0 for completely translucent
    to 1.0 for opaque.}
  @begin{short}
    Creates a @class{gdk-rgba} struct.
  @end{short}")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-rgba-red atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-rgba-red 'function)
 "@version{2013-4-5}
  Accessor of the slot @code{red} of the @class{gdk-rgba} struct.")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-rgba-green atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-rgba-green 'function)
 "@version{2013-4-5}
  Accessor of the slot @code{green} of the @class{gdk-rgba} struct.")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-rgba-blue atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-rgba-blue 'function)
 "@version{2013-4-5}
  Accessor of the slot @code{blue} of the @class{gdk-rgba} struct.")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-rgba-alpha atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-rgba-alpha 'function)
 "@version{2013-4-5}
  Accessor of the slot @code{alpha} of the @class{gdk-rgba} struct.")

;;; ----------------------------------------------------------------------------
;;; gdk_rgba_copy ()
;;; ----------------------------------------------------------------------------

(declaim (inline gdk-rgba-copy))

(defun gdk-rgba-copy (rgba)
 #+cl-cffi-gtk-documentation
 "@version{2013-6-14}
  @argument[rgba]{a @class{gdk-rgba} structure}
  @return{A newly allocated @class{gdk-rgba}, with the same contents
    as @arg{rgba}.}
  @short{Makes a copy of a @class{gdk-rgba} structure.}

  Since 3.0"
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
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_rgba_parse" %gdk-rgba-parse) :boolean
  (rgba (g-boxed-foreign gdk-rgba))
  (spec :string))

(defun gdk-rgba-parse (spec)
 #+cl-cffi-gtk-documentation
 "@version{2013-6-14}
  @argument[spec]{the string specifying the color}
  @return{The @class{gdk-rgba} structure to fill in.}
  @begin{short}
    Parses a textual representation of a color, and returns a rgba structure
    filling in the red, green, blue and alpha fields.
  @end{short}

  The string can be either one of:
  @begin{itemize}
    @item{A standard name (Taken from the X11 rgb.txt file).}
    @item{A hex value in the form 'rgb' 'rrggbb' 'rrrgggbbb' or 'rrrrggggbbbb'.}
    @item{A RGB color in the form 'rgb(r,g,b)' (In this case the color will have
      full opacity).}
    @item{A RGBA color in the form 'rgba(r,g,b,a)'.}
  @end{itemize}

  Where 'r', 'g', 'b' and 'a' are respectively the red, green, blue and alpha
  color values. In the last two cases, r g and b are either integers in the
  range 0 to 255 or precentage values in the range 0% to 100%, and a is a
  floating point value in the range 0 to 1.

  Since 3.0"
  (let ((rgba (make-gdk-rgba)))
    (when (%gdk-rgba-parse rgba spec)
      rgba)))

(export 'gdk-rgba-parse)

;;; ----------------------------------------------------------------------------
;;; gdk_rgba_equal ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_rgba_equal" gdk-rgba-equal) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-6-14}
  @argument[p1]{a @class{gdk-rgba} structure}
  @argument[p2]{another @class{gdk-rgba} structure}
  @return{@em{True} if the two colors compare equal.}
  @short{Compares two RGBA colors.}

  Since 3.0"
  (color1 (g-boxed-foreign gdk-rgba))
  (color2 (g-boxed-foreign gdk-rgba)))

(export 'gdk-rgba-equal)

;;; ----------------------------------------------------------------------------
;;; gdk_rgba_hash ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_rgba_hash" gdk-rgba-hash) :uint
 #+cl-cffi-gtk-documentation
 "@version{2013-6-14}
  @argument[p]{a @class{gdk-rgba} structure}
  @return{The hash value for @arg{p}.}
  @begin{short}
    A hash function suitable for using for a hash table that stores
    @class{gdk-rgba}'s.
  @end{short}

  Since 3.0"
  (p (g-boxed-foreign gdk-rgba)))

(export 'gdk-rgba-hash)

;;; ----------------------------------------------------------------------------
;;; gdk_rgba_to_string ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_rgba_to_string" gdk-rgba-to-string) :string
 #+cl-cffi-gtk-documentation
 "@version{2013-6-14}
  @argument[rgba]{a @class{gdk-rgba} structure}
  @return{A newly allocated text string.}
  @begin{short}
    Returns a textual specification of rgba in the form rgb (r, g, b) or
    rgba (r, g, b, a), where 'r', 'g', 'b' and 'a' represent the red, green,
    blue and alpha values respectively. r, g, and b are represented as integers
    in the range 0 to 255, and a is represented as floating point value in the
    range 0 to 1.
  @end{short}

  These string forms are string forms those supported by the CSS3 colors
  module, and can be parsed by the function @fun{gdk-rgba-parse}.

  Note that this string representation may loose some precision, since r, g
  and b are represented as 8-bit integers. If this is a concern, you should
  use a different representation.

  Since 3.0
  @see-function{gdk-rgba-parse}"
  (rgba (g-boxed-foreign gdk-rgba)))

(export 'gdk-rgba-to-string)

;;; --- End of file gdk.rgba.lisp ----------------------------------------------
