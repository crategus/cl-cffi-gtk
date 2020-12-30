;;; ----------------------------------------------------------------------------
;;; cairo.version.lisp
;;;
;;; The documentation of the file is taken from the Cairo Reference Manual
;;; Version 1.16 and modified to document the Lisp binding to the Cairo
;;; library. See <http://cairographics.org>. The API documentation of the
;;; Lisp binding is available at <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2012 - 2020 Dieter Kaiser
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
;;; Version Information
;;;
;;;     Compile and run time version checks
;;;
;;; Types and Values
;;;
;;;     CAIRO_VERSION
;;;     CAIRO_VERSION_MAJOR
;;;     CAIRO_VERSION_MINOR
;;;     CAIRO_VERSION_MICRO
;;;     CAIRO_VERSION_STRING
;;;
;;; Functions
;;;
;;;     CAIRO_VERSION_ENCODE
;;;     CAIRO_VERSION_STRINGIZE
;;;
;;;     cairo_version
;;;     cairo_version_string
;;; ----------------------------------------------------------------------------

(in-package :cairo)

;;; ----------------------------------------------------------------------------
;;; CAIRO_VERSION                                          not implemented
;;; ----------------------------------------------------------------------------

;(defconstant +cairo-version+
;             #.(cairo-version-encode +cairo-version-major+
;                                     +cairo-version-minor+
;                                     +cairo-version-micro+)
; #+cl-cffi-gtk-documentation
; "@version{2013-3-2}
;  The version of Cairo available at compile-time, encoded using
;  @fun{cairo-version-encode}")

;#+cl-cffi-gtk-documentation
;(setf (gethash '+cairo-version+ atdoc:*variable-name-alias*) "Constant")

;;; ----------------------------------------------------------------------------
;;; CAIRO_VERSION_MAJOR                                    not implemented
;;; ----------------------------------------------------------------------------

;(eval-when (:load-toplevel :compile-toplevel :execute)
;  (defconstant +cairo-version-major+ 1
;   #+cl-cffi-gtk-documentation
;   "@version{2013-3-2}
;    @begin{short}
;      The major component of the version of Cairo available at compile-time.
;    @end{short}"))

;#+cl-cffi-gtk-documentation
;(setf (gethash '+cairo-version-major+ atdoc:*variable-name-alias*) "Constant")

;;; ----------------------------------------------------------------------------
;;; CAIRO_VERSION_MINOR                                    not implemented
;;; ----------------------------------------------------------------------------

;(eval-when (:load-toplevel :compile-toplevel :execute)
;  (defconstant +cairo-version-minor+ 12
;   #+cl-cffi-gtk-documentation
;   "@version{2013-3-2}
;    @begin{short}
;      The minor component of the version of Cairo available at compile-time.
;    @end{short}"))

;#+cl-cffi-gtk-documentation
;(setf (gethash '+cairo-version-minor+ atdoc:*variable-name-alias*) "Constant")

;;; ----------------------------------------------------------------------------
;;; CAIRO_VERSION_MICRO                                    not implemented
;;; ----------------------------------------------------------------------------

;(eval-when (:load-toplevel :compile-toplevel :execute)
;  (defconstant +cairo-version-micro+ 2
;   #+cl-cffi-gtk-documentation
;   "@version{2013-3-2}
;    @begin{short}
;      The micro component of the version of Cairo available at compile-time.
;    @end{short}"))

;#+cl-cffi-gtk-documentation
;(setf (gethash '+cairo-version-micro+ atdoc:*variable-name-alias*) "Constant")

;;; ----------------------------------------------------------------------------
;;; CAIRO_VERSION_STRING                                   not implemented
;;; ----------------------------------------------------------------------------

;(defvar +cairo-version-string+
;        #.(format nil "~D.~D.~D" +cairo-version-major+
;                                 +cairo-version-minor+
;                                 +cairo-version-micro+)
; #+cl-cffi-gtk-documentation
; "@version{2013-3-2}
;  @begin{short}
;    A human-readable string literal containing the version of Cairo available at
;    compile-time, in the form of \"x.y.z\".
;  @end{short}")

;#+cl-cffi-gtk-documentation
;(setf (gethash '+cairo-version-string+ atdoc:*variable-name-alias*) "Constant")

;;; ----------------------------------------------------------------------------
;;; CAIRO_VERSION_ENCODE
;;; ----------------------------------------------------------------------------

(defun cairo-version-encode (major minor micro)
 #+cl-cffi-gtk-documentation
 "@version{2020-12-5}
  @argument[major]{an integer with the major component of the version number}
  @argument[minor]{an integer with the minor component of the version number}
  @argument[micro]{an integer with the micro component of the version number}
  @return{An integer with the encoded version.}
  @begin{short}
    This function encodes the given Cairo version into an integer.
  @end{short}
  Two encoded version numbers can be compared as integers. The encoding
  ensures that later versions compare greater than earlier versions.
  @see-function{cairo-version}"
  (parse-integer (format nil "~D~2,'0D~2,'0D" major minor micro)))

(export 'cairo-version-encode)

;;; ----------------------------------------------------------------------------
;;; CAIRO_VERSION_STRINGIZE()
;;;
;;; #define CAIRO_VERSION_STRINGIZE(major, minor, micro)
;;;
;;; This macro encodes the given cairo version into an string. The numbers
;;; returned by CAIRO_VERSION_STRING and cairo_version_string() are encoded
;;; using this macro. The parameters to this macro must expand to numerical
;;; literals.
;;;
;;; major :
;;;     the major component of the version number
;;;
;;; minor :
;;;     the minor component of the version number
;;;
;;; micro :
;;;     the micro component of the version number
;;;
;;; Returns :
;;;     a string literal containing the version.
;;;
;;; Since 1.8
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_version ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_version" cairo-version) :int
 #+cl-cffi-gtk-documentation
 "@version{2020-12-5}
  @return{An integer with the encoded version.}
  @begin{short}
    Returns the version of the Cairo library encoded in a single integer.
  @end{short}
  The encoding ensures that later versions compare greater than earlier
  versions.

  A run-time comparison to check that Cairo's version is greater than or
  equal to version x.y.z could be performed as follows:
  @code{(>= (cairo-version) (cairo-version-encode x y z))}

  See also the function @fun{cairo-version-string}.
  @see-function{cairo-version-string}")

(export 'cairo-version)

;;; ----------------------------------------------------------------------------
;;; cairo_version_string ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_version_string" cairo-version-string) :string
 #+cl-cffi-gtk-documentation
 "@version{2020-12-5}
  @return{A string containing the Cairo version.}
  @begin{short}
    Returns the version of the Cairo library as a human-readable string of the
    form \"x.y.z\".
  @end{short}
  See also the function @fun{cairo-version}.
  @see-function{cairo-version}")

(export 'cairo-version-string)

;;; ----------------------------------------------------------------------------

(glib-init::push-library-version-features cairo
    (truncate (/ (cairo-version) 10000))
    (- (truncate (/ (cairo-version) 100))
       (* 100 (truncate (/ (cairo-version) 10000))))
    1 16)

(glib-init::require-library-version "Cairo" 1 16
    (truncate (/ (cairo-version) 10000))
    (- (truncate (/ (cairo-version) 100))
       (* 100 (truncate (/ (cairo-version) 10000)))))

;;; --- End of file cairo.version.lisp -----------------------------------------
