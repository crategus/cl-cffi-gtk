;;; ----------------------------------------------------------------------------
;;; gdk.color.lisp
;;;
;;; The documentation of this file is taken from the GDK 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GDK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2021 Dieter Kaiser
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
;;;     Manipulation of colors
;;;
;;; Types and Values
;;;
;;;     GdkColor
;;;
;;; Functons
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
;;;     A GdkColor represents a color.
;;;
;;;     When working with Cairo, it is often more convenient to use a GdkRGBA
;;;     instead, and GdkColor has been deprecated in favor of GdkRGBA.
;;; ----------------------------------------------------------------------------

(in-package :gdk)

;;; ----------------------------------------------------------------------------
;;; GdkColor
;;; ----------------------------------------------------------------------------

(define-g-boxed-cstruct gdk-color "GdkColor"
  (pixel :uint32 :initform 0)
  (red :uint16 :initform 0)
  (green :uint16 :initform 0)
  (blue :uint16 :initform 0))

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-color atdoc:*class-name-alias*)
      "GBoxed"
      (documentation 'gdk-color 'type)
 "@version{2021-12-11}
  @begin{short}
    The @sym{gdk-color} structure is used to describe a color, similar to the
    XColor structure used in the X11 drawing API.
  @end{short}
  @begin{pre}
(define-g-boxed-cstruct gdk-color \"GdkColor\"
  (pixel :uint32 :initform 0)
  (red :uint16 :initform 0)
  (green :uint16 :initform 0)
  (blue :uint16 :initform 0))
  @end{pre}
  @begin[code]{table}
    @entry[pixel]{For allocated colors, the pixel value used to draw this
      color on the screen. Not used anymore.}
    @entry[red]{The red component of the color. This is a value between
      0 and 65535, with 65535 indicating full intensity.}
    @entry[green]{The green component of the color. This is a value between
      0 and 65535, with 65535 indicating full intensity.}
    @entry[blue]{The blue component of the color. This is a value between
      0 and 65535, with 65535 indicating full intensity.}
  @end{table}
  @begin[Warning]{dictionary}
    The @sym{gdk-color} color has been deprecated since version 3.14 and
    should not be used in newly written code. Use the @class{gdk-rgba} color.
  @end{dictionary}
  @see-slot{gdk-color-pixel}
  @see-slot{gdk-color-red}
  @see-slot{gdk-color-green}
  @see-slot{gdk-color-blue}
  @see-class{gdk-rgba}")

(export (boxed-related-symbols 'gdk-color))

;;; ----------------------------------------------------------------------------
;;; Constructors for GdkColor structure
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation 'copy-gdk-color 'function)
 "@version{2013-12-25}
  @argument[instance]{a @class{gdk-color} structure}
  Copy constructor of a @class{gdk-color} structure.
  @see-class{gdk-color}")

#+cl-cffi-gtk-documentation
(setf (documentation 'make-gdk-color 'function)
 "@version{2013-12-25}
  @argument[pixel]{For allocated colors, the pixel value used to draw this
      color on the screen. Not used anymore.}
  @argument[red]{The red component of the color. This is a value between
      0 and 65535, with 65535 indicating full intensity.}
  @argument[green]{The green component of the color.}
  @argument[blue]{The blue component of the color.}
  Creates a @class{gdk-color} structure.
  @see-class{gdk-color}")

;;; ----------------------------------------------------------------------------
;;; Accessors of the GdkColor structure
;;; ----------------------------------------------------------------------------

;;; --- gdk-color-pixel --------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-color-pixel atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-color-pixel 'function)
 "@version{2021-12-11}
  @syntax[]{(gdk-color-pixel instance) => pixel}
  @syntax[]{(setf (gdk-color-pixel instance) pixel)}
  @begin{short}
    Accessor of the @code{pixel} slot of the @class{gdk-color} color.
  @end{short}

  For allocated colors, the pixel value used to draw this color on the screen.
  Not used anymore.
  @begin[Warning]{dictionary}
    The @sym{gdk-color-pixel} function has been deprecated since version 3.14
    and should not be used in newly written code. Use the @class{gdk-rgba}
    color.
  @end{dictionary}
  @see-class{gdk-color}
  @see-class{gdk-rgba}")

;;; --- gdk-color-red ----------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-color-red atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-color-red 'function)
 "@version{2021-12-11}
  @syntax[]{(gdk-color-red instance) => red}
  @syntax[]{(setf (gdk-color-red instance) red)}
  @begin{short}
    Accessor of the @code{red} slot of the @class{gdk-color} color.
  @end{short}

  The red component of the color. This is a value between 0 and 65535, with
  65535 indicating full intensity.
  @begin[Warning]{dictionary}
    The @sym{gdk-color-red} function has been deprecated since version 3.14
    and should not be used in newly written code. Use the @class{gdk-rgba}
    color.
  @end{dictionary}
  @see-class{gdk-color}
  @see-class{gdk-rgba}
  @see-function{gdk-color-green}
  @see-function{gdk-color-blue}")

;;; --- gdk-color-green --------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-color-green atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-color-green 'function)
 "@version{2021-12-11}
  @syntax[]{(gdk-color-green instance) => green}
  @syntax[]{(setf (gdk-color-green instance) green)}
  @begin{short}
    Accessor of the @code{green} slot of the @class{gdk-color} color.
  @end{short}

  The green component of the color. This is a value between 0 and 65535, with
  65535 indicating full intensity.
  @begin[Warning]{dictionary}
    The @sym{gdk-color-green} function has been deprecated since version 3.14
    and should not be used in newly written code. Use the @class{gdk-rgba}
    color.
  @end{dictionary}
  @see-class{gdk-color}
  @see-class{rgba}
  @see-function{gdk-color-red}
  @see-function{gdk-color-blue}")

;;; --- gdk-color-blue ---------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-color-blue atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-color-blue 'function)
 "@version{2021-12-11}
  @syntax[]{(gdk-color-blue instance) => blue}
  @syntax[]{(setf (gdk-color-blue instance) blue)}
  @begin{short}
    Accessor of the @code{blue} slot of the @class{gdk-color} color.
  @end{short}

  The blue component of the color. This is a value between 0 and 65535, with
  65535 indicating full intensity.
  @begin[Warning]{dictionary}
    The @sym{gdk-color-blue} function has been deprecated since version 3.14
    and should not be used in newly written code. Use the @class{gdk-rgba}
    color.
  @end{dictionary}
  @see-class{gdk-color}
  @see-class{gdk-rgba}
  @see-function{gdk-color-red}
  @see-function{gdk-color-green}")

;;; ----------------------------------------------------------------------------
;;; gdk-color-new
;;; ----------------------------------------------------------------------------

(declaim (inline gdk-color-new))

(defun gdk-color-new (&key (pixel 0) (red 0) (green 0) (blue 0))
 #+cl-cffi-gtk-documentation
 "@version{2021-12-11}
  @argument[pixel]{an unsigned integer with the pixel value used to draw this
    color on the screen, not used anymore}
  @argument[red]{an unsigned integer with the red component of the color, this
    is a value between 0 and 65535, with 65535 indicating full intensity}
  @argument[green]{an unsigned integer with the green component of the color}
  @argument[blue]{an unsigned integer with the blue component of the color}
  @begin{short}
    Creates a new @class{gdk-color} color.
  @end{short}
  @begin[Warning]{dictionary}
    The @sym{gdk-color-new} function has been deprecated since version 3.14
    and should not be used in newly written code. Use the @class{gdk-rgba}
    color.
  @end{dictionary}
  @see-class{gdk-color}"
  (make-gdk-color :pixel pixel :red red :green green :blue blue))

(export 'gdk-color-new)

;;; ----------------------------------------------------------------------------
;;; gdk_color_copy ()
;;; ----------------------------------------------------------------------------

(declaim (inline gdk-color-copy))

(defun gdk-color-copy (color)
 #+cl-cffi-gtk-documentation
 "@version{2021-12-11}
  @argument[color]{a @class{gdk-color} color}
  @return{A copy of @arg{color}.}
  @begin{short}
    Makes a copy of a color.
  @end{short}
  @begin[Warning]{dictionary}
    The @sym{gdk-color-copy} function has been deprecated since version 3.14
    and should not be used in newly written code. Use the @class{gdk-rgba}
    color.
  @end{dictionary}
  @see-class{gdk-color}
  @see-class{gdk-rgba}"
  (copy-gdk-color color))

(export 'gdk-color-copy)

;;; ----------------------------------------------------------------------------
;;; gdk_color_free ()
;;;
;;; void gdk_color_free (GdkColor *color);
;;;
;;; Frees a color structure created with gdk_color_copy().
;;;
;;; gdk_color_free has been deprecated since version 3.14 and should not be
;;; used in newly written code. Use GdkRGBA
;;;
;;; color :
;;;     a GdkColor
;;; ----------------------------------------------------------------------------

;; not needed

;;; ----------------------------------------------------------------------------
;;; gdk_color_parse ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_color_parse" %gdk-color-parse) :boolean
  (spec :string)
  (color (g-boxed-foreign gdk-color)))

(defun gdk-color-parse (spec)
 #+cl-cffi-gtk-documentation
 "@version{2021-12-11}
  @argument[spec]{a string specifying the color}
  @return{The @class{gdk-color} color or @code{nil} if the parsing did not
    succeed.}
  @begin{short}
    Parses a textual specification of a color and fill in the red, green, and
    blue fields of a @class{gdk-color} color.
  @end{short}

  The string can either one of a large set of standard names taken from the
  X11 @file{rgb.txt} file, or it can be a hex value in the form @code{#rgb},
  @code{#rrggbb}, @code{#rrrgggbbb} or @code{#rrrrggggbbbb} where @code{r},
  @code{g} and @code{b} are hex digits of the red, green, and blue components
  of the color, respectively. White in the four forms is @code{#fff},
  @code{#ffffff}, @code{#fffffffff} and @code{#ffffffffffff}.
  @begin[Warning]{dictionary}
    The @sym{gdk-color-parse} function has been deprecated since version 3.14
    and should not be used in newly written code. Use the @class{gdk-rgba}
    color.
  @end{dictionary}
  @see-class{gdk-color}
  @see-class{gdk-rgba}
  @see-function{gdk-color-to-string}"
  (let ((color (gdk-color-new)))
    (when (%gdk-color-parse spec color)
      color)))

(export 'gdk-color-parse)

;;; ----------------------------------------------------------------------------
;;; gdk_color_equal ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_color_equal" gdk-color-equal) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2021-12-11}
  @argument[color1]{a @class{gdk-color} color}
  @argument[color2]{another @class{gdk-color} color}
  @return{@em{True} if the two colors compare equal.}
  @begin{short}
    Compares two colors.
  @end{short}
  @begin[Warning]{dictionary}
    The @sym{gdk-color-equal} function has been deprecated since version 3.14
    and should not be used in newly written code. Use the @class{gdk-rgba}
    color.
  @end{dictionary}
  @see-class{gdk-color}
  @see-class{gdk-rgba}"
  (color1 (g-boxed-foreign gdk-color))
  (color2 (g-boxed-foreign gdk-color)))

(export 'gdk-color-equal)

;;; ----------------------------------------------------------------------------
;;; gdk_color_hash ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_color_hash" gdk-color-hash) :uint
 #+cl-cffi-gtk-documentation
 "@version{2021-12-11}
  @argument[color]{a @class{gdk-color} color}
  @return{The hash function applied to @arg{color}.}
  @begin{short}
    A hash function suitable for using for a hash table that stores
    @class{gdk-color} colors.
  @end{short}
  @begin[Warning]{dictionary}
    The @sym{gdk-color-hash} function has been deprecated since version 3.14
    and should not be used in newly written code. Use the @class{gdk-rgba}
    color.
  @end{dictionary}
  @see-class{gdk-color}
  @see-class{gdk-rgba}"
  (color (g-boxed-foreign gdk-color)))

(export 'gdk-color-hash)

;;; ----------------------------------------------------------------------------
;;; gdk_color_to_string ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_color_to_string" gdk-color-to-string)
    (g-string :free-from-foreign t)
 #+cl-cffi-gtk-documentation
 "@version{2021-12-11}
  @argument[color]{a @class{gdk-color} color}
  @return{A text string representing @arg{color}.}
  @begin{short}
    Returns a textual specification of @arg{color} in the hexadecimal form
    @code{#rrrrggggbbbb}, where @code{r}, @code{g} and @code{b} are hex digits
    representing the red, green and blue components respectively.
  @end{short}

  The returned string can be parsed by the @fun{gdk-color-parse} function.
  @begin[Warning]{dictionary}
    The @sym{gdk-color-to-string} function has been deprecated since version
    3.14 and should not be used in newly written code. Use the @class{gdk-rgba}
    color.
  @end{dictionary}
  @see-class{gdk-color}
  @see-class{gdk-rgba}
  @see-function{gdk-color-parse}"
  (color (g-boxed-foreign gdk-color)))

(export 'gdk-color-to-string)

;;; --- End of file gdk.color.lisp ---------------------------------------------
