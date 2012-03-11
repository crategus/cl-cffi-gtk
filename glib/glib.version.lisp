;;; ----------------------------------------------------------------------------
;;; glib.version.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See http://common-lisp.net/project/cl-gtk2/
;;;
;;; The documentation of this file has been copied from the
;;; GLib 2.30.2 Reference Manual.  See http://www.gtk.org.
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
;;; Version Information
;;; 
;;; Variables and functions to check the GLib version
;;; 
;;; Synopsis
;;; 
;;;     *glib-major-version*
;;;     *glib-minor-version*
;;;     *glib-micro-version*
;;;     *glib-binary-age*
;;;     *glib-interface-age*
;;;
;;;     glib-check-version (required-major required-minor required-micro)
;;; 
;;; Description
;;; 
;;; GLib provides version information, primarily useful in configure checks for
;;; builds that have a configure script. Applications will not typically use
;;; the features described here.
;;; ----------------------------------------------------------------------------

(in-package :glib)

;;; ----------------------------------------------------------------------------
;;; *glib-major-version*
;;; 
;;; extern const guint glib_major_version;
;;; ----------------------------------------------------------------------------

(defcvar ("glib_major_version" *glib-major-version* :read-only t) :uint)

(export '*glib-major-version*)

;;; ---------------------------------------------------------------------------- 
;;; *glib-minor-version*
;;; 
;;; extern const guint glib_minor_version;
;;; ----------------------------------------------------------------------------

(defcvar ("glib_minor_version" *glib-minor-version* :read-only t) :uint)

(export '*glib-minor-version*)

;;; ----------------------------------------------------------------------------
;;; *glib-micro-version*
;;; 
;;; extern const guint glib_micro_version;
;;; ----------------------------------------------------------------------------

(defcvar ("glib_micro_version" *glib-micro-version* :read-only t) :uint)

(export '*glib-micro-version*)

;;; ----------------------------------------------------------------------------
;;; *glib-binary-age*
;;; 
;;; extern const guint glib_binary_age;
;;; ----------------------------------------------------------------------------

(defcvar ("glib_binary_age" *glib-binary-age* :read-only t) :uint)

(export '*glib-binary-age*)

;;; ---------------------------------------------------------------------------- 
;;; *glib-interface-age*
;;; 
;;; extern const guint glib_interface_age;
;;; ----------------------------------------------------------------------------

(defcvar ("glib_interface_age" *glib-interface-age* :read-only t) :uint)

(export '*glib-interface-age*)

;;; ----------------------------------------------------------------------------
;;; glib-check-version (required-major required-minor required-micro)
;;; 
;;; const gchar * glib_check_version (guint required_major,
;;;                                   guint required_minor,
;;;                                   quint required_micro)
;;; 
;;; Checks that the GLib library in use is compatible with the given version.
;;; Generally you would pass in the constants GLIB_MAJOR_VERSION,
;;; GLIB_MINOR_VERSION, GLIB_MICRO_VERSION as the three arguments to this
;;; function; that produces a check that the library in use is compatible with
;;; the version of GLib the application or module was compiled against.
;;; 
;;; Compatibility is defined by two things: first the version of the running
;;; library is newer than the version
;;; required_major.required_minor.required_micro. Second the running library
;;; must be binary compatible with the version
;;; required_major.required_minor.required_micro (same major version.)
;;; 
;;; required-major :
;;;     the required major version.
;;; 
;;; required-minor :
;;;     the required minor version.
;;; 
;;; required-micro :
;;;     the required micro version.
;;; 
;;; Returns :
;;;     NULL if the GLib library is compatible with the given version, or a
;;;     string describing the version mismatch. The returned string is owned
;;;     by GLib and must not be modified or freed.
;;; 
;;; Since 2.6
;;; ----------------------------------------------------------------------------

(defcfun ("glib_check_version" glib-check-version ) :string
  (required-major :uint)
  (required-minor :uint)
  (required-micro :uint))

(export 'glib-check-version)

;;; --- End of file glib.version.lisp ------------------------------------------
