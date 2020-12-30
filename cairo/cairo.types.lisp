;;; ----------------------------------------------------------------------------
;;; cairo.types.lisp
;;;
;;; The documentation of the file is taken from the Cairo Reference Manual
;;; Version 1.16 and modified to document the Lisp binding to the Cairo
;;; library. See <http://cairographics.org>. The API documentation of the
;;; Lisp binding is available at <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2013 - 2020 Dieter Kaiser
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
;;; Types
;;;
;;;     Generic data types
;;;
;;; Types and Values
;;;
;;;     cairo_bool_t
;;;     cairo_user_data_key_t
;;;     cairo_rectangle_int_t
;;;
;;; Functions
;;;
;;;     cairo-destroy-func-t
;;;
;;; Description
;;;
;;; This section lists generic data types used in the cairo API.
;;; ----------------------------------------------------------------------------

(in-package :cairo)

;;; ----------------------------------------------------------------------------
;;; cairo_bool_t
;;;
;;; typedef int cairo_bool_t;
;;;
;;; cairo_bool_t is used for boolean values. Returns of type cairo_bool_t will
;;; always be either 0 or 1, but testing against these values explicitly is not
;;; encouraged; just use the value as a boolean condition.
;;;
;;; if (cairo_in_stroke (cr, x, y)) {
;;;     /* do something */
;;; }
;;;
;;; Since 1.0
;;; ----------------------------------------------------------------------------

(defctype cairo-bool-t :int)

(export 'cairo-bool-t)

;;; ----------------------------------------------------------------------------
;;; cairo_user_data_key_t
;;;
;;; typedef struct {
;;;     int unused;
;;; } cairo_user_data_key_t;
;;;
;;; cairo_user_data_key_t is used for attaching user data to cairo data
;;; structures. The actual contents of the struct is never used, and there is no
;;; need to initialize the object; only the unique address of a cairo_data_key_t
;;; object is used. Typically, you would just use the address of a static
;;; cairo_data_key_t object.
;;;
;;; int unused;
;;;     not used; ignore.
;;;
;;; Since 1.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_rectangle_int_t
;;; ----------------------------------------------------------------------------

(defcstruct cairo-rectangle-int-t
  (x :int)
  (y :int)
  (width :int)
  (height :int))

#+cl-cffi-gtk-documentation
(setf (gethash 'cairo-rectangle-int-t atdoc:*symbol-name-alias*)
      "CStruct"
      (gethash 'cairo-rectangle-int-t atdoc:*external-symbols*)
 "@version{2020-12-5}
  @begin{short}
    A data structure for holding a rectangle with integer coordinates.
  @end{short}
  @begin{pre}
(defcstruct cairo-rectangle-int-t
  (x :int)
  (y :int)
  (width :int)
  (height :int))
  @end{pre}
  @begin[code]{table}
    @entry[x]{An integer x coordinate of the left side of the rectangle.}
    @entry[y]{An integer y coordinate of the the top side of the rectangle.}
    @entry[width]{An integer with the width of the rectangle.}
    @entry[height]{An integer with the height of the rectangle.}
  @end{table}
  @see-class{gdk-rectangle}")

(export 'cairo-rectangle-int-t)

;;; ----------------------------------------------------------------------------
;;; cairo_destroy_func_t ()
;;;
;;; void (*cairo_destroy_func_t) (void *data);
;;;
;;; cairo_destroy_func_t the type of function which is called when a data
;;; element is destroyed. It is passed the pointer to the data element and
;;; should free any memory and resources allocated for it.
;;;
;;; data :
;;;     The data element being destroyed.
;;;
;;; Since 1.0
;;; ----------------------------------------------------------------------------

;;; --- End of file cairo.types.lisp -------------------------------------------
