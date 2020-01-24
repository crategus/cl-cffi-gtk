;;; ----------------------------------------------------------------------------
;;; gdk.rgba.lisp
;;;
;;; The documentation of this file is taken from the GDK 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GDK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2012 - 2019 Dieter Kaiser
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
;;;     RGBA colors
;;;
;;; Types and Values
;;;
;;;     GdkRGBA
;;;
;;; Functions
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

(eval-when (:execute :load-toplevel :compile-toplevel)
  (foreign-funcall "gdk_rgba_get_type" g-type))

(define-g-boxed-cstruct gdk-rgba "GdkRGBA"
  (red :double :initform 0.0d0)
  (green :double :initform 0.0d0)
  (blue :double :initform 0.0d0)
  (alpha :double :initform 0.0d0))

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-rgba atdoc:*class-name-alias*) "CStruct"
      (documentation 'gdk-rgba 'type)
 "@version{2013-8-17}
  @begin{short}
    The @sym{gdk-rgba} structure is used to represent a (possibly translucent)
    color, in a way that is compatible with cairos notion of color.
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
  @see-slot{gdk-rgba-red}
  @see-slot{gdk-rgba-green}
  @see-slot{gdk-rgba-blue}
  @see-slot{gdk-rgba-alpha}")

(export (boxed-related-symbols 'gdk-rgba))

;;; ----------------------------------------------------------------------------
;;;
;;;  Contstructors
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation 'copy-gdk-rgba 'function)
 "@version{2013-8-17}
  @argument[instance]{a @class{gdk-rgba} structure}
  Copy constructor of a @class{gdk-rgba} structure.
  @see-class{gdk-rgba}")

#+cl-cffi-gtk-documentation
(setf (documentation 'make-gdk-rgba 'function)
 "@version{2013-8-17}
  @argument[red]{The intensity of the red channel from 0.0 to 1.0 inclusive.}
  @argument[green]{The intensity of the green channel from 0.0 to 1.0
    inclusive.}
  @argument[blue]{The intensity of the blue channel from 0.0 to 1.0 inclusive.}
  @argument[alpha]{The opacity of the color from 0.0 for completely translucent
    to 1.0 for opaque.}
  Creates a @class{gdk-rgba} structure.
  @see-class{gdk-rgba}")

;;; ----------------------------------------------------------------------------
;;;
;;; Slot Accessors
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-rgba-red atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-rgba-red 'function)
 "@version{2013-8-17}
  Accessor of the slot @code{red} of the @class{gdk-rgba} structure.
  @see-class{gdk-rgba}
  @see-function{gdk-rgba-green}
  @see-function{gdk-rgba-blue}
  @see-function{gdk-rgba-alpha}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-rgba-green atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-rgba-green 'function)
 "@version{2013-8-17}
  Accessor of the slot @code{green} of the @class{gdk-rgba} structure.
  @see-class{gdk-rgba}
  @see-function{gdk-rgba-red}
  @see-function{gdk-rgba-blue}
  @see-function{gdk-rgba-alpha}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-rgba-blue atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-rgba-blue 'function)
 "@version{2013-8-17}
  Accessor of the slot @code{blue} of the @class{gdk-rgba} structure.
  @see-class{gdk-rgba}
  @see-function{gdk-rgba-red}
  @see-function{gdk-rgba-green}
  @see-function{gdk-rgba-alpha}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-rgba-alpha atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-rgba-alpha 'function)
 "@version{2013-8-17}
  Accessor of the slot @code{alpha} of the @class{gdk-rgba} structure.
  @see-class{gdk-rgba}
  @see-function{gdk-rgba-red}
  @see-function{gdk-rgba-green}
  @see-function{gdk-rgba-blue}")

;;; ----------------------------------------------------------------------------
;;; gdk_rgba_copy ()
;;; ----------------------------------------------------------------------------

(declaim (inline gdk-rgba-copy))

(defun gdk-rgba-copy (rgba)
 #+cl-cffi-gtk-documentation
 "@version{2013-8-17}
  @argument[rgba]{a @class{gdk-rgba} structure}
  @return{A newly allocated @class{gdk-rgba} structure, with the same contents
    as @arg{rgba}.}
  @short{Makes a copy of a @class{gdk-rgba} structure.}
  @begin[Note]{dictionary}
    In the Lisp implementation this function is implemented as a call of the
    constructor @fun{copy-gdk-rgba}.
  @end{dictionary}

  Since 3.0
  @see-class{gdk-rgba}
  @see-function{copy-gdk-rgba}"
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

;; not needed

;;; ----------------------------------------------------------------------------
;;; gdk_rgba_parse ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_rgba_parse" %gdk-rgba-parse) :boolean
  (rgba (g-boxed-foreign gdk-rgba))
  (spec :string))

(defun gdk-rgba-parse (spec)
 #+cl-cffi-gtk-documentation
 "@version{2020-1-24}
  @argument[spec]{the string specifying the color}
  @return{A @class{gdk-rgba} structure with the filled in values.}
  @begin{short}
    Parses a textual representation of a color, and returns a rgba structure
    filling in the red, green, blue and alpha fields.
  @end{short}

  The string can be either one of:
  @begin{itemize}
    @item{A standard name taken from the X11 @code{rgb.txt} file.}
    @item{A hex value in the form @code{rgb}, @code{rrggbb}, @code{rrrgggbbb}
      or @code{rrrrggggbbbb}.}
    @item{A RGB color in the form @code{rgb(r,g,b)}. In this case the color
      will have full opacity.}
    @item{A RGBA color in the form @code{rgba(r,g,b,a)}.}
  @end{itemize}
  Where @code{r}, @code{g}, @code{b} and @code{a} are respectively the red,
  green, blue and alpha color values. In the last two cases, @code{r}, @code{g}
  and @code{b} are either integers in the range 0 to 255 or precentage values
  in the range 0% to 100%, and @code{a} is a floating point value in the range
  0 to 1.
  @begin[Example]{dictionary}
    @begin{pre}
 (gdk-rgba-parse \"LightGreen\")
=> #S(GDK-RGBA
      :RED 0.5647058823529412d0
      :GREEN 0.9333333333333333d0
      :BLUE 0.5647058823529412d0
      :ALPHA 1.0d0)
 (gdk-rgba-parse \"#90ee90\")
=> #S(GDK-RGBA
      :RED 0.5647058823529412d0
      :GREEN 0.9333333333333333d0
      :BLUE 0.5647058823529412d0
      :ALPHA 1.0d0)
    @end{pre}
  @end{dictionary}
  @see-class{gdk-rgba}
  @see-function{gdk-rgba-to-string}"
  (let ((rgba (make-gdk-rgba)))
    (when (%gdk-rgba-parse rgba spec)
      rgba)))

(export 'gdk-rgba-parse)

;;; ----------------------------------------------------------------------------
;;; gdk_rgba_equal ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_rgba_equal" gdk-rgba-equal) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-8-17}
  @argument[p1]{a @class{gdk-rgba} structure}
  @argument[p2]{another @class{gdk-rgba} structure}
  @return{@em{True} if the two colors compare equal.}
  @short{Compares two RGBA colors.}

  Since 3.0
  @see-class{gdk-rgba}"
  (color1 (g-boxed-foreign gdk-rgba))
  (color2 (g-boxed-foreign gdk-rgba)))

(export 'gdk-rgba-equal)

;;; ----------------------------------------------------------------------------
;;; gdk_rgba_hash ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_rgba_hash" gdk-rgba-hash) :uint
 #+cl-cffi-gtk-documentation
 "@version{2013-8-17}
  @argument[p]{a @class{gdk-rgba} structure}
  @return{The hash value for @arg{p}.}
  @begin{short}
    A hash function suitable for using for a hash table that stores
    @class{gdk-rgba}'s.
  @end{short}

  Since 3.0
  @see-class{gdk-rgba}"
  (p (g-boxed-foreign gdk-rgba)))

(export 'gdk-rgba-hash)

;;; ----------------------------------------------------------------------------
;;; gdk_rgba_to_string ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_rgba_to_string" gdk-rgba-to-string) :string
 #+cl-cffi-gtk-documentation
 "@version{2013-8-15}
  @argument[rgba]{a @class{gdk-rgba} structure}
  @return{A newly allocated text string.}
  @begin{short}
    Returns a textual specification of rgba in the form @code{rgb (r, g, b)} or
    @code{rgba (r, g, b, a)}, where @code{r}, @code{g}, @code{b} and @code{a}
    represent the red, green, blue and alpha values respectively.
  @end{short}
  @code{r}, @code{g}, and @code{b} are represented as integers in the range 0
  to 255, and @code{a} is represented as floating point value in the range
  0 to 1.

  These string forms are string forms those supported by the CSS3 colors
  module, and can be parsed by the function @fun{gdk-rgba-parse}.

  Note that this string representation may loose some precision, since @code{r},
  @code{g} and @code{b} are represented as 8-bit integers. If this is a concern,
  you should use a different representation.

  Since 3.0
  @see-class{gdk-rgba}
  @see-function{gdk-rgba-parse}"
  (rgba (g-boxed-foreign gdk-rgba)))

(export 'gdk-rgba-to-string)

;;; --- End of file gdk.rgba.lisp ----------------------------------------------
