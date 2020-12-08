;;; ----------------------------------------------------------------------------
;;; pango.version.lisp
;;;
;;; The documentation of this file is taken from the Pango Reference Manual
;;; for Pango 1.48 and modified to document the Lisp binding to the Pango
;;; library. See <http://www.gtk.org>. The API documentation of the Lisp
;;; binding is available at <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2011 - 2020 Dieter Kaiser
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
;;; Version Checking
;;;
;;;     Tools for checking Pango version at compile- and run-time.
;;;
;;; Types and Values
;;;
;;;     PANGO_VERSION_ENCODE
;;;     PANGO_VERSION
;;;     PANGO_VERSION_MAJOR
;;;     PANGO_VERSION_MINOR
;;;     PANGO_VERSION_MICRO
;;;     PANGO_VERSION_STRING
;;;     PANGO_VERSION_CHECK
;;;
;;; Functions
;;;
;;;     pango_version
;;;     pango_version_string
;;;     pango_version_check
;;;
;;; Description
;;;
;;; The capital-letter macros defined here can be used to check the version of
;;; Pango at compile-time, and to encode Pango versions into integers. The
;;; functions can be used to check the version of the linked Pango library at
;;; run-time.
;;; ----------------------------------------------------------------------------

(in-package :pango)

;;; ----------------------------------------------------------------------------
;;; PANGO_VERSION_ENCODE()
;;;
;;; #define PANGO_VERSION_ENCODE(major, minor, micro)
;;;
;;; This macro encodes the given Pango version into an integer. The numbers
;;; returned by PANGO_VERSION and pango_version() are encoded using this macro.
;;; Two encoded version numbers can be compared as integers.
;;;
;;; major :
;;;     the major component of the version number
;;;
;;; minor :
;;;     the minor component of the version number
;;;
;;; micro :
;;;     the micro component of the version number
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; PANGO_VERSION
;;;
;;; #define PANGO_VERSION
;;;
;;; The version of Pango available at compile-time, encoded using
;;; PANGO_VERSION_ENCODE().
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; PANGO_VERSION_MAJOR
;;;
;;; #define PANGO_VERSION_MAJOR 1
;;;
;;; The major component of the version of Pango available at compile-time.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; PANGO_VERSION_MINOR
;;;
;;; #define PANGO_VERSION_MINOR 30
;;;
;;; The minor component of the version of Pango available at compile-time.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; PANGO_VERSION_MICRO
;;;
;;; #define PANGO_VERSION_MICRO 0
;;;
;;; The micro component of the version of Pango available at compile-time.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; PANGO_VERSION_STRING
;;;
;;; #define PANGO_VERSION_STRING "1.30.0"
;;;
;;; A string literal containing the version of Pango available at compile-time.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; PANGO_VERSION_CHECK()
;;;
;;; #define PANGO_VERSION_CHECK(major,minor,micro)
;;;
;;; Checks that the version of Pango available at compile-time is not older
;;; than the provided version number.
;;;
;;; major :
;;;     the major component of the version number
;;;
;;; minor :
;;;     the minor component of the version number
;;;
;;; micro :
;;;     the micro component of the version number
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_version ()
;;; ----------------------------------------------------------------------------

(defcfun ("pango_version" pango-version) :int
 #+cl-cffi-gtk-documentation
 "@version{2020-12-1}
  @return{The encoded version of the Pango library available at run time.}
  @begin{short}
    Returns the encoded version of Pango available at run-time.
  @end{short}
  @begin[Example]{dictionary}
    @begin{pre}
(pango-version) => 14602
    @end{pre}
  @end{dictionary}
  @see-function{pango-version-string}")

(export 'pango-version)

;;; ----------------------------------------------------------------------------
;;; pango_version_string ()
;;; ----------------------------------------------------------------------------

(defcfun ("pango_version_string" pango-version-string) :string
 #+cl-cffi-gtk-documentation
 "@version{2020-12-1}
  @begin{return}
    A string containing the version of the Pango library available at run
    time.
  @end{return}
  @begin{short}
    Returns the version of Pango available at run-time
  @end{short}
  @begin[Example]{dictionary}
    @begin{pre}
(pango-version-string) => \"1.46.2\"
    @end{pre}
  @end{dictionary}
  @see-function{pango-version}")

(export 'pango-version-string)

;;; ----------------------------------------------------------------------------
;;; pango_version_check ()
;;; ----------------------------------------------------------------------------

(defcfun ("pango_version_check" pango-version-check) :string
 #+cl-cffi-gtk-documentation
 "@version{2020-12-1}
  @argument[required-major]{an integer with the required major version}
  @argument[required-minor]{an integer with the required minor version}
  @argument[required-micro]{an integer with the required major version}
  @begin{return}
    Returns @code{nil} if the Pango library is compatible with the given
    version, or a string describing the version mismatch.
  @end{return}
  @begin{short}
    Checks that the Pango library in use is compatible with the given version.
  @end{short}

  Compatibility is defined by two things: first the version of the running
  library is newer than the version
  @arg{required-major}.@arg{required-minor}.@arg{required-micro}. Second the
  running library must be binary compatible with the version
  @arg{required-major}.@arg{required-minor}.@arg{required-micro} (same major
  version.)
  @begin[Example]{dictionary}
    @begin{pre}
(pango-version-check 1 46 0) => NIL
(pango-version-check 1 48 0) => \"Pango version too old (micro mismatch)\"
    @end{pre}
  @end{dictionary}
  @see-function{pango-version}
  @see-function{pango-version-string}"
  (required-major :int)
  (required-minor :int)
  (required-micro :int))

(export 'pango-version-check)

;;; --- End of file pango.version.lisp -----------------------------------------
