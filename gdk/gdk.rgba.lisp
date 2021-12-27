;;; ----------------------------------------------------------------------------
;;; gdk.rgba.lisp
;;;
;;; The documentation of this file is taken from the GDK 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GDK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
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
;;; ----------------------------------------------------------------------------

(in-package :gdk)

;;; ----------------------------------------------------------------------------
;;; GdkRGBA
;;; ----------------------------------------------------------------------------

(eval-when (:execute :load-toplevel :compile-toplevel)
  (foreign-funcall "gdk_rgba_get_type" g-size))

(define-g-boxed-cstruct gdk-rgba "GdkRGBA"
  (red :double :initform 0.0d0)
  (green :double :initform 0.0d0)
  (blue :double :initform 0.0d0)
  (alpha :double :initform 0.0d0))

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-rgba atdoc:*class-name-alias*)
      "Boxed CStruct"
      (documentation 'gdk-rgba 'type)
 "@version{2021-1-22}
  @begin{short}
    The @sym{gdk-rgba} structure is used to represent a (possibly translucent)
    color, in a way that is compatible with Cairo's notion of color.
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
  @see-slot{gdk-rgba-red}
  @see-slot{gdk-rgba-green}
  @see-slot{gdk-rgba-blue}
  @see-slot{gdk-rgba-alpha}")

(export (boxed-related-symbols 'gdk-rgba))

;;; ----------------------------------------------------------------------------
;;; Accessors
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-rgba-red atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-rgba-red 'function)
 "@version{*2021-1-22}
  @syntax[]{(gdk-rgba-red instance) => red}
  @syntax[]{(setf (gdk-rgba-red instance) red)}
  @argument[instance]{a @struct{gdk-rgba} color}
  @argument[red]{a double float intensity of the red channel from 0.0 to 1.0}
  @begin{short}
    Accessor of the @code{red} slot of the @struct{gdk-rgba} color.
  @end{short}
  @see-struct{gdk-rgba}
  @see-function{gdk-rgba-green}
  @see-function{gdk-rgba-blue}
  @see-function{gdk-rgba-alpha}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-rgba-green atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-rgba-green 'function)
 "@version{*2021-1-22}
  @syntax[]{(gdk-rgba-green instance) => green}
  @syntax[]{(setf (gdk-rgba-green instance) green)}
  @argument[instance]{a @struct{gdk-rgba} color}
  @argument[green]{a double float intensity of the green channel from 0.0
    to 1.0}
  @begin{short}
    Accessor of the @code{green} slot of the @struct{gdk-rgba} color.
  @end{short}
  @see-struct{gdk-rgba}
  @see-function{gdk-rgba-red}
  @see-function{gdk-rgba-blue}
  @see-function{gdk-rgba-alpha}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-rgba-blue atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-rgba-blue 'function)
 "@version{*2021-1-22}
  @syntax[]{(gdk-rgba-blue instance) => blue}
  @syntax[]{(setf (gdk-rgba-blue instance) blue)}
  @argument[instance]{a @struct{gdk-rgba} color}
  @argument[blue]{a double float intensity of the blue channel from 0.0 to 1.0}
  @begin{short}
    Accessor of the @code{blue} slot of the @struct{gdk-rgba} color.
  @end{short}
  @see-struct{gdk-rgba}
  @see-function{gdk-rgba-red}
  @see-function{gdk-rgba-green}
  @see-function{gdk-rgba-alpha}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-rgba-alpha atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-rgba-alpha 'function)
 "@version{2021-1-21}
  @syntax[]{(gdk-rgba-alpha instance) => alpha}
  @syntax[]{(setf (gdk-rgba-alpha instance) alpha)}
  @argument[instance]{a @struct{gdk-rgba} color}
  @argument[alpha]{a double float opacity of the color from 0.0 for completely
    translucent to 1.0 for opaque}
  @begin{short}
    Accessor of the @code{alpha} slot of the @struct{gdk-rgba} color.
  @end{short}
  @see-struct{gdk-rgba}
  @see-function{gdk-rgba-red}
  @see-function{gdk-rgba-green}
  @see-function{gdk-rgba-blue}")

;;; ----------------------------------------------------------------------------
;;; gdk-rgba-new
;;; ----------------------------------------------------------------------------

(defun gdk-rgba-new (&key (red 0.0d0) (green 0.0d0) (blue 0.0d0) (alpha 0.0d0))
 "@version{2021-1-22}
  @argument[red]{the double float intensity of the red channel from 0.0
    to 1.0 inclusive}
  @argument[green]{the double float intensity of the green channel
    from 0.0 to 1.0 inclusive}
  @argument[blue]{the double float intensity of the blue channel from 0.0
    to 1.0 inclusive}
  @argument[alpha]{the double float opacity of the color from 0.0 for
    completely translucent to 1.0 for opaque}
  @begin{short}
    Creates a @struct{gdk-rgba} color.
  @end{short}
  @see-struct{gdk-rgba}
  @see-function{gdk-rgba-copy}"
  (make-gdk-rgba :red (coerce red 'double-float)
                 :green (coerce green 'double-float)
                 :blue (coerce blue 'double-float)
                 :alpha (coerce alpha 'double-float)))

(export 'gdk-rgba-new)

;;; ----------------------------------------------------------------------------
;;; gdk_rgba_copy ()
;;; ----------------------------------------------------------------------------

(defun gdk-rgba-copy (rgba)
 #+cl-cffi-gtk-documentation
 "@version{2021-1-22}
  @argument[rgba]{a @struct{gdk-rgba} color}
  @return{A newly allocated @struct{gdk-rgba} color, with the same contents
    as @arg{rgba}.}
  @short{Makes a copy of a @struct{gdk-rgba} color.}
  @see-struct{gdk-rgba}
  @see-function{gdk-rgba-new}"
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
;;; ----------------------------------------------------------------------------

;; not needed

;;; ----------------------------------------------------------------------------
;;; gdk_rgba_parse ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_rgba_parse" %gdk-rgba-parse) :boolean
  (rgba (g-boxed-foreign gdk-rgba))
  (str :string))

(defun gdk-rgba-parse (str)
 #+cl-cffi-gtk-documentation
 "@version{*2021-12-15}
  @argument[str]{a string specifying the color}
  @return{A @struct{gdk-rgba} color with the filled in values.}
  @begin{short}
    Parses a textual representation of a color, and returns a RGBA instance
    filling in the @code{red}, @code{green}, @code{blue} and @code{alpha}
    fields.
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
  0.0 to 1.0.
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
  @see-struct{gdk-rgba}
  @see-function{gdk-rgba-to-string}"
  (let ((rgba (make-gdk-rgba)))
    (when (%gdk-rgba-parse rgba str)
      rgba)))

(export 'gdk-rgba-parse)

;;; ----------------------------------------------------------------------------
;;; gdk_rgba_equal ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_rgba_equal" gdk-rgba-equal) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2021-1-22}
  @argument[color-11]{a @struct{gdk-rgba} color}
  @argument[color-2]{another @struct{gdk-rgba} color}
  @return{@em{True} if the two colors compare equal.}
  @short{Compares two RGBA colors.}
  @see-struct{gdk-rgba}"
  (color-1 (g-boxed-foreign gdk-rgba))
  (color-2 (g-boxed-foreign gdk-rgba)))

(export 'gdk-rgba-equal)

;;; ----------------------------------------------------------------------------
;;; gdk_rgba_hash ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_rgba_hash" gdk-rgba-hash) :uint
 #+cl-cffi-gtk-documentation
 "@version{2021-1-21}
  @argument[color]{a @struct{gdk-rgba} color}
  @return{An unsigned integer with the hash value for @arg{color}.}
  @begin{short}
    A hash function suitable for using for a hash table that stores
    RGBA colors.
  @end{short}
  @see-struct{gdk-rgba}"
  (color (g-boxed-foreign gdk-rgba)))

(export 'gdk-rgba-hash)

;;; ----------------------------------------------------------------------------
;;; gdk_rgba_to_string ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_rgba_to_string" gdk-rgba-to-string) :string
 #+cl-cffi-gtk-documentation
 "@version{*2021-1-24}
  @argument[color]{a @struct{gdk-rgba} color}
  @return{A string with the textual specification of @arg{color}.}
  @begin{short}
    Returns a textual specification of @arg{color} in the form @code{rgb(r,g,b)}
    or @code{rgba(r,g,b,a)}, where @code{r}, @code{g}, @code{b} and @code{a}
    represent the red, green, blue and alpha values respectively.
  @end{short}
  @code{r}, @code{g}, and @code{b} are represented as integers in the range 0
  to 255, and @code{a} is represented as a floating point value in the range
  0.0 to 1.0.

  These string forms are supported by the CSS3 colors module, and can be parsed
  by the function @fun{gdk-rgba-parse}.

  Note that this string representation may loose some precision, since @code{r},
  @code{g} and @code{b} are represented as 8-bit integers. If this is a concern,
  you should use a different representation.
  @begin[Example]{dictionary}
    @begin{pre}
(gdk-rgba-to-string (gdk-rgba-new :red 1.0))
=> \"rgba(255,0,0,0)\"
(gdk-rgba-parse *)
=> #S(GDK-RGBA :RED 1.0d0 :GREEN 0.0d0 :BLUE 0.0d0 :ALPHA 0.0d0)
    @end{pre}
  @end{dictionary}
  @see-struct{gdk-rgba}
  @see-function{gdk-rgba-parse}"
  (color (g-boxed-foreign gdk-rgba)))

(export 'gdk-rgba-to-string)

;;; --- End of file gdk.rgba.lisp ----------------------------------------------
