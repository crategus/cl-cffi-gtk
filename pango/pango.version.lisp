;;; ----------------------------------------------------------------------------
;;; pango.version.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See http://common-lisp.net/project/cl-gtk2/
;;;
;;; The documentation has been copied from the Pango Reference Manual
;;; for Pango 1.30.0. See http://www.gtk.org.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2012 Dieter Kaiser
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
;;; Tools for checking Pango version at compile- and run-time.
;;;     
;;; Synopsis
;;; 
;;;     PANGO_VERSION_ENCODE
;;;     PANGO_VERSION
;;;     PANGO_VERSION_MAJOR
;;;     PANGO_VERSION_MINOR
;;;     PANGO_VERSION_MICRO
;;;     PANGO_VERSION_STRING
;;;     PANGO_VERSION_CHECK
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
;;; 
;;; int pango_version (void);
;;; 
;;; This is similar to the macro PANGO_VERSION except that it returns the
;;; encoded version of Pango available at run-time, as opposed to the version
;;; available at compile-time.
;;; 
;;; A version number can be encoded into an integer using
;;; PANGO_VERSION_ENCODE().
;;; 
;;; Returns :
;;;     The encoded version of Pango library available at run time.
;;; 
;;; Since 1.16
;;; ----------------------------------------------------------------------------

(defcfun ("pango_version" pango-version) :int)

(export 'pango-version)

;;; ----------------------------------------------------------------------------
;;; pango_version_string ()
;;; 
;;; const char * pango_version_string (void);
;;; 
;;; This is similar to the macro PANGO_VERSION_STRING except that it returns
;;; the version of Pango available at run-time, as opposed to the version
;;; available at compile-time.
;;; 
;;; Returns :
;;;     A string containing the version of Pango library available at run time.
;;;     The returned string is owned by Pango and should not be modified or
;;;     freed.
;;; 
;;; Since 1.16
;;; ----------------------------------------------------------------------------

(defcfun ("pango_version_string" pango-version-string) :string)

(export 'pango-version-string)

;;; ----------------------------------------------------------------------------
;;; pango_version_check ()
;;; 
;;; const char * pango_version_check (int required_major,
;;;                                   int required_minor,
;;;                                   int required_micro);
;;; 
;;; Checks that the Pango library in use is compatible with the given version.
;;; Generally you would pass in the constants PANGO_VERSION_MAJOR,
;;; PANGO_VERSION_MINOR, PANGO_VERSION_MICRO as the three arguments to this
;;; function; that produces a check that the library in use at run-time is
;;; compatible with the version of Pango the application or module was compiled
;;; against.
;;; 
;;; Compatibility is defined by two things: first the version of the running
;;; library is newer than the version
;;; required_major.required_minor.required_micro. Second the running library
;;; must be binary compatible with the version
;;; required_major.required_minor.required_micro (same major version.)
;;; 
;;; For compile-time version checking use PANGO_VERSION_CHECK().
;;; 
;;; required_major :
;;;     the required major version
;;; 
;;; required_minor :
;;;     the required minor version
;;; 
;;; required_micro :
;;;     the required major version
;;; 
;;; Returns :
;;;     NULL if the Pango library is compatible with the given version, or a
;;;     string describing the version mismatch. The returned string is owned by
;;;     Pango and should not be modified or freed.
;;; 
;;; Since 1.16
;;; ----------------------------------------------------------------------------

(defcfun ("pango_version_check" pango-version-check) :string
  (required-major :int)
  (required-minor :int)
  (required-micro :int))

(export 'pango-version-check)

;;; --- End of file pango.version.lisp -----------------------------------------
