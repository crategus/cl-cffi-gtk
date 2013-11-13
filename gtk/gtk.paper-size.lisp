;;; ----------------------------------------------------------------------------
;;; gtk.paper-size.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See <http://common-lisp.net/project/cl-gtk2/>.
;;;
;;; The documentation of this file is taken from the GTK+ 3 Reference Manual
;;; Version 3.6.4 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2013 Dieter Kaiser
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
;;; GtkPaperSize
;;;
;;; Support for named paper sizes
;;;
;;; Synopsis
;;;
;;;     GtkPaperSize
;;;     GtkUnit
;;;
;;;     GTK_PAPER_NAME_A3
;;;     GTK_PAPER_NAME_A4
;;;     GTK_PAPER_NAME_A5
;;;     GTK_PAPER_NAME_B5
;;;     GTK_PAPER_NAME_LETTER
;;;     GTK_PAPER_NAME_EXECUTIVE
;;;     GTK_PAPER_NAME_LEGAL
;;;
;;;     gtk_paper_size_new
;;;     gtk_paper_size_new_from_ppd
;;;     gtk_paper_size_new_custom
;;;     gtk_paper_size_copy
;;;     gtk_paper_size_free
;;;     gtk_paper_size_is_equal
;;;     gtk_paper_size_get_paper_sizes
;;;     gtk_paper_size_get_name
;;;     gtk_paper_size_get_display_name
;;;     gtk_paper_size_get_ppd_name
;;;     gtk_paper_size_get_width
;;;     gtk_paper_size_get_height
;;;     gtk_paper_size_is_custom
;;;     gtk_paper_size_set_size
;;;     gtk_paper_size_get_default_top_margin
;;;     gtk_paper_size_get_default_bottom_margin
;;;     gtk_paper_size_get_default_left_margin
;;;     gtk_paper_size_get_default_right_margin
;;;     gtk_paper_size_get_default
;;;
;;;     gtk_paper_size_new_from_key_file
;;;     gtk_paper_size_to_key_file
;;;
;;; Object Hierarchy
;;;
;;;   GBoxed
;;;    +----GtkPaperSize
;;;
;;; Description
;;;

;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkPaperSize
;;; ----------------------------------------------------------------------------

(glib::at-init () (foreign-funcall "gtk_paper_size_get_type" :int))

(define-g-boxed-opaque gtk-paper-size "GtkPaperSize"
  :alloc (%gtk-paper-size-new (null-pointer)))

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-paper-size atdoc:*class-name-alias*) "CStruct"
      (documentation 'gtk-paper-size 'type)
 "@version{2013-11-12}
  @begin{short}
    @sym{gtk-paper-size} handles paper sizes. It uses the standard called
    \"PWG 5101.1-2002 PWG: Standard for Media Standardized Names\" to name the
    paper sizes and to get the data for the page sizes. In addition to
    standard paper sizes, @symbol{gtk-paper-size} allows to construct custom
    paper sizes with arbitrary dimensions.
  @end{short}

  The @symbol{gtk-paper-size} object stores not only the dimensions (width and
  height) of a paper size and its name, it also provides default print margins.

  Printing support has been added in GTK+ 2.10.
  @begin{pre}
(define-g-boxed-opaque gtk-paper-size \"GtkPaperSize\"
  :alloc (%gtk-paper-size-new (null-pointer)))
  @end{pre}")

(export 'gtk-paper-size)

;;; ----------------------------------------------------------------------------
;;; enum GtkUnit
;;; ----------------------------------------------------------------------------

(define-g-enum "GtkUnit" gtk-unit
  (:export t
   :type-initializer "gtk_unit_get_type")
  (:none 0)
  (:points 1)
  (:inch 2)
  (:mm 3))

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-unit atdoc:*symbol-name-alias*) "Enum"
      (gethash 'gtk-unit atdoc:*external-symbols*)
 "@version{2013-7-31}
  @short{}
  @begin{pre}
(define-g-enum \"GtkUnit\" gtk-unit
  (:export t
   :type-initializer \"gtk_unit_get_type\")
  (:none 0)
  (:points 1)
  (:inch 2)
  (:mm 3))
  @end{pre}")

;;; ----------------------------------------------------------------------------
;;; GTK_PAPER_NAME_A3
;;;
;;; #define GTK_PAPER_NAME_A3 "iso_a3"
;;;
;;; Name for the A4 paper size.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_PAPER_NAME_A4
;;;
;;; #define GTK_PAPER_NAME_A4 "iso_a4"
;;;
;;; Name for the A4 paper size.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_PAPER_NAME_A5
;;;
;;; #define GTK_PAPER_NAME_A5 "iso_a5"
;;;
;;; Name for the A5 paper size.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_PAPER_NAME_B5
;;;
;;; #define GTK_PAPER_NAME_B5 "iso_b5"
;;;
;;; Name for the B5 paper size.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_PAPER_NAME_LETTER
;;;
;;; #define GTK_PAPER_NAME_LETTER "na_letter"
;;;
;;; Name for the Letter paper size.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_PAPER_NAME_EXECUTIVE
;;;
;;; #define GTK_PAPER_NAME_EXECUTIVE "na_executive"
;;;
;;; Name for the Executive paper size.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_PAPER_NAME_LEGAL
;;;
;;; #define GTK_PAPER_NAME_LEGAL "na_legal"
;;;
;;; Name for the Legal paper size.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_paper_size_new ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_paper_size_new" %gtk-paper-size-new) :pointer
  (name :string))

(defcfun ("gtk_paper_size_new" gtk-paper-size-new)
    (g-boxed-foreign gtk-paper-size)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-12}
  @argument[name]{a paper size name, or @code{nil}}
  @return{A new @class{gtk-paper-size}.}
  @begin{short}
    Creates a new @class{gtk-paper-size} object by parsing a PWG 5101.1-2002
    paper name.
  @end{short}

  If name is @code{nil}, the default paper size is returned, see the function
  @fun{gtk-paper-size-get-default}.

  Since 2.10
  @see-class{gtk-paper-size}
  @see-function{gtk-paper-size-get-default}"
  (name :string))

(export 'gtk-paper-size-new)

;;; ----------------------------------------------------------------------------
;;; gtk_paper_size_new_from_ppd ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_paper_size_new_from_ppd" gtk-paper-size-new-from-ppd)
    (g-boxed-foreign gtk-paper-size)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-13}
  @argument[ppd-name]{a PPD paper name}
  @argument[ppd-display-name]{the corresponding human-readable name}
  @argument[width]{the paper width, in points}
  @argument[height]{the paper height in points}
  @begin{return}
    A new @class{gtk-paper-size}.
  @end{return}
  @begin{short}
    Creates a new @class{gtk-paper-size} object by using PPD information.
  @end{short}

  If @arg{ppd-name} is not a recognized PPD paper name, @arg{ppd-display-name},
  @arg{width} and @arg{height} are used to construct a custom
  @class{gtk-paper-size} object.

  Since 2.10
  @see-class{gtk-paper-size}"
  (ppd-name :string)
  (ppd-display-name :string)
  (width :double)
  (height :double))

(export 'gtk-paper-size-new-from-ppd)

;;; ----------------------------------------------------------------------------
;;; gtk_paper_size_new_custom ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_paper_size_new_custom" gtk-paper-size-new-custom)
    (g-boxed-foreign gtk-paper-size)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-12}
  @argument[name]{the paper name}
  @argument[display-name]{the human-readable name}
  @argument[width]{the paper width, in units of unit}
  @argument[height]{the paper height, in units of unit}
  @argument[unit]{the unit for @arg{width} and @arg{height}, not @code{:none}}
  @return{A new @class{gtk-paper-size} object.}
  @begin{short}
    Creates a new @class{gtk-paper-size} object with the given parameters.
  @end{short}

  Since 2.10
  @see-class{gtk-paper-size}
  @see-symbol{gtk-unit}"
  (name :string)
  (display-name :string)
  (width :double)
  (height :double)
  (unit gtk-unit))

(export 'gtk-paper-size-new-custom)

;;; ----------------------------------------------------------------------------
;;; gtk_paper_size_copy ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_paper_size_copy" gtk-paper-size-copy)
    (g-boxed-foreign gtk-paper-size)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-13}
  @argument[other]{a @class{gtk-paper-size} object}
  @return{A copy of @arg{other}.}
  @begin{short}
    Copies an existing @class{gtk-paper-size}.
  @end{short}

  Since 2.10
  @see-class{gtk-paper-size}"
  (other (g-boxed-foreign gtk-paper-size)))

(export 'gtk-paper-size-copy)

;;; ----------------------------------------------------------------------------
;;; gtk_paper_size_free ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_paper_size_free" gtk-paper-size-free) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-11-13}
  @argument[size]{a @class{gtk-paper-size} object}
  @begin{short}
    Free the given @class{gtk-paper-size} object.
  @end{short}

  Since 2.10
  @see-class{gtk-paper-size}"
  (size (g-boxed-foreign gtk-paper-size)))

(export 'gtk-paper-size-free)

;;; ----------------------------------------------------------------------------
;;; gtk_paper_size_is_equal ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_paper_size_is_equal" gtk-paper-size-is-equal) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-11-12}
  @argument[size1]{a @class{gtk-paper-size} object}
  @argument[size2]{another @class{gtk-paper-size} object}
  @begin{return}
    @em{True}, if @arg{size1} and @arg{size2} represent the same paper size.
  @end{return}
  @begin{short}
    Compares two @class{gtk-paper-size} objects.
  @end{short}

  Since 2.10
  @see-class{gtk-paper-size}"
  (size1 (g-boxed-foreign gtk-paper-size))
  (size2 (g-boxed-foreign gtk-paper-size)))

(export 'gtk-paper-size-is-equal)

;;; ----------------------------------------------------------------------------
;;; gtk_paper_size_get_paper_sizes ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_paper_size_get_paper_sizes" gtk-paper-size-get-paper-sizes)
    (g-list (g-boxed-foreign gtk-paper-size))
 #+cl-cffi-gtk-documentation
 "@version{2013-11-13}
  @argument[include-custom]{whether to include custom paper sizes as defined in
    the page setup dialog}
  @begin{return}
    A newly allocated list of newly allocated @class{gtk-paper-size} objects.
  @end{return}
  @begin{short}
    Creates a list of known paper sizes.
  @end{short}

  Since 2.12
  @see-class{gtk-paper-size}"
  (include-custom :boolean))

(export 'gtk-paper-size-get-paper-sizes)

;;; ----------------------------------------------------------------------------
;;; gtk_paper_size_get_name ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_paper_size_get_name" gtk-paper-size-get-name) :string
 #+cl-cffi-gtk-documentation
 "@version{2013-11-12}
  @argument[size]{a @class{gtk-paper-size} object}
  @return{The name of @arg{size}.}
  @begin{short}
    Gets the name of the @class{gtk-paper-size}.
  @end{short}

  Since 2.10
  @see-class{gtk-paper-size}"
  (size (g-boxed-foreign gtk-paper-size)))

(export 'gtk-paper-size-get-name)

;;; ----------------------------------------------------------------------------
;;; gtk_paper_size_get_display_name ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_paper_size_get_display_name" gtk-paper-size-get-display-name)
    :string
 #+cl-cffi-gtk-documentation
 "@version{2013-11-12}
  @argument[size]{a @class{gtk-paper-size} object}
  @return{The human-readable name of size.}
  @begin{short}
    Gets the human-readable name of the @class{gtk-paper-size}.
  @end{short}

  Since 2.10
  @see-class{gtk-paper-size}"
  (size (g-boxed-foreign gtk-paper-size)))

(export 'gtk-paper-size-get-display-name)

;;; ----------------------------------------------------------------------------
;;; gtk_paper_size_get_ppd_name ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_paper_size_get_ppd_name" gtk-paper-size-get-ppd-name) :string
 #+cl-cffi-gtk-documentation
 "@version{2013-11-12}
  @argument[size]{a @class{gtk-paper-size} object}
  @return{The PPD name of size.}
  @begin{short}
    Gets the PPD name of the @class{gtk-paper-size}, which may be @code{nil}.
  @end{short}

  Since 2.10
  @see-class{gtk-paper-size}"
  (size (g-boxed-foreign gtk-paper-size)))

(export 'gtk-paper-size-get-ppd-name)

;;; ----------------------------------------------------------------------------
;;; gtk_paper_size_get_width ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_paper_size_get_width" gtk-paper-size-get-width) :double
 #+cl-cffi-gtk-documentation
 "@version{2013-11-12}
  @argument[size]{a @class{gtk-paper-size} object}
  @argument[unit]{the unit for the return value, not @code{:none}}
  @return{The paper width.}
  @begin{short}
    Gets the paper width of the @class{gtk-paper-size}, in units of @arg{unit}.
  @end{short}

  Since 2.10
  @see-class{gtk-paper-size}
  @see-symbol{gtk-unit}
  @see-function{gtk-paper-size-get-height}"
  (size (g-boxed-foreign gtk-paper-size))
  (unit gtk-unit))

(export 'gtk-paper-size-get-width)

;;; ----------------------------------------------------------------------------
;;; gtk_paper_size_get_height ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_paper_size_get_height" gtk-paper-size-get-height) :double
 #+cl-cffi-gtk-documentation
 "@version{2013-11-12}
  @argument[size]{a @class{gtk-paper-size} object}
  @argument[unit]{the unit for the return value, not @code{:none}}
  @return{The paper height.}
  @begin{short}
    Gets the paper height of the @class{gtk-paper-size}, in units of @arg{unit}.
  @end{short}

  Since 2.10
  @see-class{gtk-paper-size}
  @see-symbol{gtk-unit}"
  (size (g-boxed-foreign gtk-paper-size))
  (unit gtk-unit))

(export 'gtk-paper-size-get-height)

;;; ----------------------------------------------------------------------------
;;; gtk_paper_size_is_custom ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_paper_size_is_custom" gtk-paper-size-is-custom) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-11-12}
  @argument[size]{a @class{gtk-paper-size} object}
  @return{Whether size is a custom paper size.}
  @short{Returns @em{true} if @arg{size} is not a standard paper size.}
  @see-class{gtk-paper-size}"
  (size (g-boxed-foreign gtk-paper-size)))

(export 'gtk-paper-size-is-custom)

;;; ----------------------------------------------------------------------------
;;; gtk_paper_size_set_size ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_paper_size_set_size" gtk-paper-size-set-size) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-11-12}
  @argument[size]{a custom @class{gtk-paper-size} object}
  @argument[width]{the new width in units of @arg{unit}}
  @argument[height]{the new height in units of @arg{unit}}
  @argument[unit]{the unit for @arg{width} and @arg{height}}
  @begin{short}
    Changes the dimensions of a paper size to @arg{width} x @arg{height}.
  @end{short}

  Since 2.10
  @see-class{gtk-paper-size}
  @see-symbol{gtk-unit}"
  (size (g-boxed-foreign gtk-paper-size))
  (width :double)
  (height :double)
  (unit gtk-unit))

(export 'gtk-paper-size-set-size)

;;; ----------------------------------------------------------------------------
;;; gtk_paper_size_get_default_top_margin ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_paper_size_get_default_top_margin"
           gtk-paper-size-get-default-top-margin) :double
 #+cl-cffi-gtk-documentation
 "@version{2013-11-12}
  @argument[size]{a @class{gtk-paper-size} object}
  @argument[unit]{the unit of type @symbol{gtk-unit} for the return value,
    not @code{:none}}
  @return{The default top margin.}
  @begin{short}
    Gets the default top margin for the @class{gtk-paper-size}.
  @end{short}

  Since 2.10
  @see-class{gtk-paper-size}
  @see-symbol{gtk-unit}
  @see-function{gtk-paper-size-get-default-bottom-margin}"
  (size (g-boxed-foreign gtk-paper-size))
  (unit gtk-unit))

(export 'gtk-paper-size-get-default-top-margin)

;;; ----------------------------------------------------------------------------
;;; gtk_paper_size_get_default_bottom_margin ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_paper_size_get_default_bottom_margin"
           gtk-paper-size-get-default-bottom-margin) :double
 #+cl-cffi-gtk-documentation
 "@version{2013-11-12}
  @argument[size]{a @class{gtk-paper-size} object}
  @argument[unit]{the unit of type @symbol{gtk-unit} for the return value,
    not @code{:none}}
  @return{The default bottom margin.}
  @begin{short}
    Gets the default bottom margin for the @class{gtk-paper-size}.
  @end{short}

  Since 2.10
  @see-class{gtk-paper-size}
  @see-symbol{gtk-unit}
  @see-function{gtk-paper-size-get-default-top-margin}"
  (size (g-boxed-foreign gtk-paper-size))
  (unit gtk-unit))

(export 'gtk-paper-size-get-default-bottom-margin)

;;; ----------------------------------------------------------------------------
;;; gtk_paper_size_get_default_left_margin ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_paper_size_get_default_left_margin"
           gtk-paper-size-get-default-left-margin) :double
 #+cl-cffi-gtk-documentation
 "@version{2013-11-12}
  @argument[size]{a @class{gtk-paper-size} object}
  @argument[unit]{the unit of type @symbol{gtk-unit} for the return value,
    not @code{:none}}
  @return{The default left margin.}
  @begin{short}
    Gets the default left margin for the @class{gtk-paper-size}.
  @end{short}

  Since 2.10
  @see-class{gtk-paper-size}
  @see-symbol{gtk-unit}
  @see-function{gtk-paper-size-get-default-right-margin}"
  (size (g-boxed-foreign gtk-paper-size))
  (unit gtk-unit))

(export 'gtk-paper-size-get-default-left-margin)

;;; ----------------------------------------------------------------------------
;;; gtk_paper_size_get_default_right_margin ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_paper_size_get_default_right_margin"
           gtk-paper-size-get-default-right-margin) :double
 #+cl-cffi-gtk-documentation
 "@version{2013-11-12}
  @argument[size]{a @class{gtk-paper-size} object}
  @argument[unit]{the unit of type @symbol{gtk-unit} for the return value,
    not @code{:none}}
  @return{The default right margin.}
  @begin{short}
    Gets the default right margin for the @class{gtk-paper-size}.
  @end{short}

  Since 2.10
  @see-class{gtk-paper-size}
  @see-symbol{gtk-unit}
  @see-function{gtk-paper-size-get-default-left-margin}"
  (size (g-boxed-foreign gtk-paper-size))
  (unit gtk-unit))

(export 'gtk-paper-size-get-default-right-margin)

;;; ----------------------------------------------------------------------------
;;; gtk_paper_size_get_default ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_paper_size_get_default" gtk-paper-size-get-default) :string
 #+cl-cffi-gtk-documentation
 "@version{2013-11-12}
  @begin{return}
    The name of the default paper size. The string is owned by GTK+ and
    should not be modified.
  @end{return}
  @begin{short}
    Returns the name of the default paper size, which depends on the current
    locale.
  @end{short}

  Since 2.10
  @see-class{gtk-paper-size}")

(export 'gtk-paper-size-get-default)

;;; ----------------------------------------------------------------------------
;;; gtk_paper_size_new_from_key_file ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_paper_size_new_from_key_file" %gtk-paper-size-new-from-key-file)
    (g-boxed-foreign gtk-paper-size)
  (key-file (:pointer (:struct g-key-file)))
  (group-name :string)
  (error :pointer))

(defun gtk-paper-size-new-from-key-file (key-file group-name)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-13}
  @argument[key-file]{the @class{g-key-file} to retrieve the paper size from}
  @argument[group-name]{the name of the group in the key file to read, or nil to
    read the first group}
  @begin{return}
    A new @class{gtk-paper-size} object with the restored paper size, or
    @code{nil} if an error occurred.
  @end{return}
  @begin{short}
    Reads a paper size from the group @arg{group-name} in the key file
    @arg{key-file}.
  @end{short}

  Since 2.12
  @see-class{gtk-paper-size}
  @see-symbol{g-key-file}"
  (with-g-error (err)
    (%gtk-paper-size-new-from-key-file key-file group-name err)))

(export 'gtk-paper-size-new-from-key-file)

;;; ----------------------------------------------------------------------------
;;; gtk_paper_size_to_key_file ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_paper_size_to_key_file" gtk-paper-size-to-key-file) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-11-13}
  @argument[size]{a @class{gtk-paper-size} object}
  @argument[key-file]{the @symbol{g-key-file}] to save the paper size to}
  @argument[group-name]{the group to add the settings to in @arg{key-file}}
  @begin{short}
    This function adds the paper size from size to @arg{key-file}.
  @end{short}

  Since 2.12
  @see-class{gtk-paper-size}
  @see-symbol{g-key-file}"
  (size (g-boxed-foreign gtk-paper-size))
  (key-file (:pointer (:struct g-key-file)))
  (group-name :string))

(export 'gtk-paper-size-to-key-file)

;;; --- End of file gtk.paper-size.lisp ----------------------------------------
