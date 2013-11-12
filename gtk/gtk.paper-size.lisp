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
;;;
;;; GtkPaperSize * gtk_paper_size_new_from_ppd (const gchar *ppd_name,
;;;                                             const gchar *ppd_display_name,
;;;                                             gdouble width,
;;;                                             gdouble height);
;;;
;;; Creates a new GtkPaperSize object by using PPD information.
;;;
;;; If ppd_name is not a recognized PPD paper name, ppd_display_name, width and
;;; height are used to construct a custom GtkPaperSize object.
;;;
;;; ppd_name :
;;;     a PPD paper name
;;;
;;; ppd_display_name :
;;;     the corresponding human-readable name
;;;
;;; width :
;;;     the paper width, in points
;;;
;;; height :
;;;     the paper height in points
;;;
;;; Returns :
;;;     a new GtkPaperSize, use gtk_paper_size_free() to free it
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_paper_size_new_custom ()
;;;
;;; GtkPaperSize * gtk_paper_size_new_custom (const gchar *name,
;;;                                           const gchar *display_name,
;;;                                           gdouble width,
;;;                                           gdouble height,
;;;                                           GtkUnit unit);
;;;
;;; Creates a new GtkPaperSize object with the given parameters.
;;;
;;; name :
;;;     the paper name
;;;
;;; display_name :
;;;     the human-readable name
;;;
;;; width :
;;;     the paper width, in units of unit
;;;
;;; height :
;;;     the paper height, in units of unit
;;;
;;; unit :
;;;     the unit for width and height. not GTK_UNIT_NONE.
;;;
;;; Returns :
;;;     a new GtkPaperSize object, use gtk_paper_size_free() to free it
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_paper_size_copy ()
;;;
;;; GtkPaperSize * gtk_paper_size_copy (GtkPaperSize *other);
;;;
;;; Copies an existing GtkPaperSize.
;;;
;;; other :
;;;     a GtkPaperSize
;;;
;;; Returns :
;;;     a copy of other
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_paper_size_free ()
;;;
;;; void gtk_paper_size_free (GtkPaperSize *size);
;;;
;;; Free the given GtkPaperSize object.
;;;
;;; size :
;;;     a GtkPaperSize
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_paper_size_is_equal ()
;;;
;;; gboolean gtk_paper_size_is_equal (GtkPaperSize *size1, GtkPaperSize *size2);
;;;
;;; Compares two GtkPaperSize objects.
;;;
;;; size1 :
;;;     a GtkPaperSize object
;;;
;;; size2 :
;;;     another GtkPaperSize object
;;;
;;; Returns :
;;;     TRUE, if size1 and size2 represent the same paper size
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_paper_size_get_paper_sizes ()
;;;
;;; GList * gtk_paper_size_get_paper_sizes (gboolean include_custom);
;;;
;;; Creates a list of known paper sizes.
;;;
;;; include_custom :
;;;     whether to include custom paper sizes as defined in the page setup
;;;     dialog
;;;
;;; Returns :
;;;     A newly allocated list of newly allocated GtkPaperSize objects.
;;;
;;; Since 2.12
;;; ----------------------------------------------------------------------------

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
  @argument[width]{the new width in units of unit}
  @argument[height]{the new height in units of unit}
  @argument[unit]{the unit for @arg{width} and @arg{height}}
  @begin{short}
    Changes the dimensions of a size to @arg{width} x @arg{height}.
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

;;; ----------------------------------------------------------------------------
;;; gtk_paper_size_new_from_key_file ()
;;;
;;; GtkPaperSize * gtk_paper_size_new_from_key_file (GKeyFile *key_file,
;;;                                                  const gchar *group_name,
;;;                                                  GError **error);
;;;
;;; Reads a paper size from the group group_name in the key file key_file.
;;;
;;; key_file :
;;;     the GKeyFile to retrieve the papersize from
;;;
;;; group_name :
;;;     the name ofthe group in the key file to read, or NULL to read the first
;;;     group
;;;
;;; error :
;;;     return location for an error, or NULL. [allow-none]
;;;
;;; Returns :
;;;     A new GtkPaperSize object with the restored paper size, or NULL if an
;;;     error occurred.
;;;
;;; Since 2.12
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_paper_size_to_key_file ()
;;;
;;; void gtk_paper_size_to_key_file (GtkPaperSize *size,
;;;                                  GKeyFile *key_file,
;;;                                  const gchar *group_name);
;;;
;;; This function adds the paper size from size to key_file.
;;;
;;; size :
;;;     a GtkPaperSize
;;;
;;; key_file :
;;;     the GKeyFile to save the paper size to
;;;
;;; group_name :
;;;     the group to add the settings to in key_file
;;;
;;; Since 2.12
;;; ----------------------------------------------------------------------------

;;; --- End of file gtk.paper-size.lisp ----------------------------------------
