;;; ----------------------------------------------------------------------------
;;; cairo.version.lisp
;;;
;;; The documentation has been copied from the Cairo Reference Manual
;;; for Cairo 1.12.2. See <http://cairographics.org>.
;;; The API documentation of the Lisp binding is available at
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
;;; Version Information
;;; 
;;; Compile and run time version checks
;;;     
;;; Synopsis
;;; 
;;;     CAIRO_VERSION_MAJOR
;;;     CAIRO_VERSION_MINOR
;;;     CAIRO_VERSION_MICRO
;;;     CAIRO_VERSION_STRING
;;;     CAIRO_VERSION_ENCODE
;;;     CAIRO_VERSION
;;;
;;;     cairo_version
;;;     cairo_version_string
;;; ----------------------------------------------------------------------------

(in-package :cairo)

;;; ----------------------------------------------------------------------------
;;; CAIRO_VERSION_MAJOR
;;; ----------------------------------------------------------------------------

(defconstant +cairo-version-major+ 1
 #+cl-cffi-gtk-documentation
 "@version{2013-3-2}
  @begin{short}
    The major component of the version of Cairo available at compile-time.
  @end{short}")

#+cl-cffi-gtk-documentation
(setf (gethash '+cairo-version-major+ atdoc:*variable-name-alias*) "Constant")

(export '+cairo-version-major+)

;;; ----------------------------------------------------------------------------
;;; CAIRO_VERSION_MINOR
;;; ----------------------------------------------------------------------------

(defconstant +cairo-version-minor+ 12
 #+cl-cffi-gtk-documentation
 "@version{2013-3-2}
  @begin{short}
    The minor component of the version of Cairo available at compile-time.
  @end{short}")

#+cl-cffi-gtk-documentation
(setf (gethash '+cairo-version-minor+ atdoc:*variable-name-alias*) "Constant")

(export '+cairo-version-minor+)

;;; ----------------------------------------------------------------------------
;;; CAIRO_VERSION_MICRO
;;; ----------------------------------------------------------------------------

(defconstant +cairo-version-micro+ 2
 #+cl-cffi-gtk-documentation
 "@version{2013-3-2}
  @begin{short}
    The micro component of the version of Cairo available at compile-time.
  @end{short}")

#+cl-cffi-gtk-documentation
(setf (gethash '+cairo-version-micro+ atdoc:*variable-name-alias*) "Constant")

(export '+cairo-version-micro+)

;;; ----------------------------------------------------------------------------
;;; CAIRO_VERSION_STRING
;;; ----------------------------------------------------------------------------

(defvar +cairo-version-string+
        #.(format nil "~D.~D.~D" +cairo-version-major+
                                 +cairo-version-minor+
                                 +cairo-version-micro+)
 #+cl-cffi-gtk-documentation
 "@version{2013-3-2}
  @begin{short}
    A human-readable string literal containing the version of Cairo available at
    compile-time, in the form of \"x.y.z\".
  @end{short}")

#+cl-cffi-gtk-documentation
(setf (gethash '+cairo-version-string+ atdoc:*variable-name-alias*) "Constant")

(export '+cairo-version-string+)

;;; ----------------------------------------------------------------------------
;;; CAIRO_VERSION_ENCODE
;;; ----------------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun cairo-version-encode (major minor micro)
   #+cl-cffi-gtk-documentation
   "@version{2013-3-2}
    @argument[major]{the major component of the version number}
    @argument[minor]{the minor component of the version number}
    @argument[micro]{the micro component of the version number}
    @return{The encoded version.}
    @begin{short}
      This function encodes the given Cairo version into an integer.
    @end{short}
    The numbers returned by @var{+cairo-version+} and @fun{cairo-version} are
    encoded using this function. Two encoded version numbers can be compared as
    integers. The encoding ensures that later versions compare greater than
    earlier versions.
    @see-variable{+cairo-version+}
    @see-function{cairo-version}"
    (parse-integer (format nil "~D~2,'0D~2,'0D" major minor micro))))

(export 'cairo-version-encode)

;;; ----------------------------------------------------------------------------
;;; CAIRO_VERSION
;;; ----------------------------------------------------------------------------

(defconstant +cairo-version+
             #.(cairo-version-encode +cairo-version-major+
                                     +cairo-version-minor+
                                     +cairo-version-micro+)
 #+cl-cffi-gtk-documentation
 "@version{2013-3-2}
  The version of Cairo available at compile-time, encoded using
  @fun{cairo-version-encode}")

#+cl-cffi-gtk-documentation
(setf (gethash '+cairo-version+ atdoc:*variable-name-alias*) "Constant")

(export '+cairo-version+)

;;; ----------------------------------------------------------------------------
;;; cairo_version ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_version" cairo-version) :int
 #+cl-cffi-gtk-documentation
 "@version{2013-3-2}
  @return{The encoded version.}
  @begin{short}
    Returns the version of the Cairo library encoded in a single integer.
    The encoding ensures that later versions compare greater than earlier
    versions.
  @end{short}

  A run-time comparison to check that Cairo's version is greater than or
  equal to version x.y.z could be performed as follows:
  @code{(>= (cairo-version) (cairo-version-encode x y z))}

  See also @fun{cairo-version-string} as well as the compile-time equivalents
  @var{+cairo-version+} and @var{+cairo-version-string+}.
  @see-variable{+cairo-version+}
  @see-variable{+cairo-version-string+}
  @see-function{cairo-version-string}")

(export 'cairo-version)

;;; ----------------------------------------------------------------------------
;;; cairo_version_string ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_version_string" cairo-version-string) :string
 #+cl-cffi-gtk-documentation
 "@version{2013-3-2}
  @return{A string containing the version.}
  @begin{short}
    Returns the version of the Cairo library as a human-readable string of the
    form \"x.y.z\".
  @end{short}

  See also @fun{cairo-version} as well as the compile-time equivalents
  @var{+cairo-version-string+} and @var{+cairo-version+}.
  @see-function{cairo-version}
  @see-variable{+cairo-version-string+}
  @see-variable{+cairo-version+}")

(export 'cairo-version-string)

;;; --- End of file cairo.version.lisp -----------------------------------------
