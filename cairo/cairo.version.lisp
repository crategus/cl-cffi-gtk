;;; ----------------------------------------------------------------------------
;;; cairo.version.lisp
;;;
;;; The documentation has been copied from the Cairo Reference Manual
;;; for Cairo 1.12.2. See http://cairographics.org
;;;
;;; Copyright (C) 2012 Dieter Kaiser
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
;;;     CAIRO_VERSION
;;;     CAIRO_VERSION_ENCODE
;;;     cairo_version
;;;     cairo_version_string
;;; 
;;; ----------------------------------------------------------------------------

(in-package :cairo)

;;; ----------------------------------------------------------------------------
;;; cairo_version ()
;;; 
;;; int cairo_version (void);
;;; 
;;; Returns the version of the cairo library encoded in a single integer as per
;;; CAIRO_VERSION_ENCODE. The encoding ensures that later versions compare
;;; greater than earlier versions.
;;; 
;;; A run-time comparison to check that cairo's version is greater than or
;;; equal to version X.Y.Z could be performed as follows:
;;; 
;;; if (cairo_version() >= CAIRO_VERSION_ENCODE(X,Y,Z)) {...}
;;; 
;;; See also cairo_version_string() as well as the compile-time equivalents
;;; CAIRO_VERSION and CAIRO_VERSION_STRING.
;;; 
;;; Returns :
;;;     the encoded version
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_version" cairo-version) :int)

(export 'cairo-version)

;;; ----------------------------------------------------------------------------
;;; cairo_version_string ()
;;; 
;;; const char* cairo_version_string (void);
;;; 
;;; Returns the version of the cairo library as a human-readable string of the
;;; form "X.Y.Z".
;;; 
;;; See also cairo_version() as well as the compile-time equivalents
;;; CAIRO_VERSION_STRING and CAIRO_VERSION.
;;; 
;;; Returns :
;;;     a string containing the version
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_version_string" cairo-version-string) :string)

(export 'cairo-version-string)

;;; --- End of file cairo.version.lisp -----------------------------------------
