;;; ----------------------------------------------------------------------------
;;; gtk.version.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See http://common-lisp.net/project/cl-gtk2/
;;;
;;; The documentation has been copied from the GTK+ 3 Reference Manual
;;; Version 3.2.3. See http://www.gtk.org.
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
;;; Variables and functions to check the GTK+ version
;;; 
;;; Synopsis
;;; 
;;;     GTK_MAJOR_VERSION
;;;     GTK_MINOR_VERSION
;;;     GTK_MICRO_VERSION
;;;     GTK_BINARY_AGE
;;;     GTK_INTERFACE_AGE
;;;     GTK_CHECK_VERSION
;;;
;;;     gtk_get_major_version
;;;     gtk_get_minor_version
;;;     gtk_get_micro_version
;;;     gtk_get_binary_age
;;;     gtk_get_interface_age
;;;     gtk_check_version
;;; 
;;; Description
;;; 
;;; GTK+ provides version information, primarily useful in configure checks for
;;; builds that have a configure script. Applications will not typically use
;;; the features described here.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GTK_MAJOR_VERSION
;;; 
;;; #define GTK_MAJOR_VERSION (3)
;;; 
;;; Like gtk_get_major_version(), but from the headers used at application
;;; compile time, rather than from the library linked against at application
;;; run time.
;;; ----------------------------------------------------------------------------

(defcvar (*gtk-major-version* "gtk_major_version" :read-only t) :uint)

(export '*gtk-major-version*)

;;; ----------------------------------------------------------------------------
;;; GTK_MINOR_VERSION
;;; 
;;; #define GTK_MINOR_VERSION (2)
;;; 
;;; Like gtk_get_minor_version(), but from the headers used at application
;;; compile time, rather than from the library linked against at application
;;; run time.
;;; ----------------------------------------------------------------------------

(defcvar (*gtk-minor-version* "gtk_minor_version" :read-only t) :uint)

(export '*gtk-minor-version*)

;;; ----------------------------------------------------------------------------
;;; GTK_MICRO_VERSION
;;; 
;;; #define GTK_MICRO_VERSION (3)
;;; 
;;; Like gtk_get_micro_version(), but from the headers used at application
;;; compile time, rather than from the library linked against at application
;;; run time.
;;; ----------------------------------------------------------------------------

(defcvar (*gtk-micro-version* "gtk_micro_version" :read-only t) :uint)

(export 'gtk-micro-version*)

;;; ----------------------------------------------------------------------------
;;; GTK_BINARY_AGE
;;; 
;;; #define GTK_BINARY_AGE    (203)
;;; 
;;; Like gtk_get_binary_age(), but from the headers used at application compile
;;; time, rather than from the library linked against at application run time.
;;; ----------------------------------------------------------------------------

(defcvar (*gtk-binary-age* "gtk_binary_age" :read-only t) :uint)

(export 'gtk-binary-age*)

;;; ----------------------------------------------------------------------------
;;; GTK_INTERFACE_AGE
;;; 
;;; #define GTK_INTERFACE_AGE (3)
;;; 
;;; Like gtk_get_interface_age(), but from the headers used at application
;;; compile time, rather than from the library linked against at application
;;; run time.
;;; ----------------------------------------------------------------------------

(defcvar (*gtk-interface-age* "gtk_interface_age" :read-only t) :uint)

(export 'gtk-interface-age*)

;;; ----------------------------------------------------------------------------
;;; GTK_CHECK_VERSION()
;;; 
;;; #define GTK_CHECK_VERSION(major,minor,micro)
;;; 
;;; major :
;;;     major version (e.g. 1 for version 1.2.5)
;;; 
;;; minor :
;;;     minor version (e.g. 2 for version 1.2.5)
;;; 
;;; micro :
;;;     micro version (e.g. 5 for version 1.2.5)
;;; 
;;; Returns :
;;;     TRUE if the version of the GTK+ header files is the same as or newer
;;;     than the passed-in version.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_get_major_version ()
;;; 
;;; guint gtk_get_major_version (void);
;;; 
;;; Returns the major version number of the GTK+ library. (e.g. in GTK+ version
;;; 3.1.5 this is 3.)
;;; 
;;; This function is in the library, so it represents the GTK+ library your
;;; code is running against. Contrast with the GTK_MAJOR_VERSION macro, which
;;; represents the major version of the GTK+ headers you have included when
;;; compiling your code.
;;; 
;;; Returns :
;;;     the major version number of the GTK+ library
;;; 
;;; Since 3.0
;;; ----------------------------------------------------------------------------

(defun gtk-get-major-version ()
  *gtk-major-version*)

(export 'gtk-get-major-version)

;;; ----------------------------------------------------------------------------
;;; gtk_get_minor_version ()
;;; 
;;; guint gtk_get_minor_version (void);
;;; 
;;; Returns the minor version number of the GTK+ library. (e.g. in GTK+ version
;;; 3.1.5 this is 1.)
;;; 
;;; This function is in the library, so it represents the GTK+ library your
;;; code is are running against. Contrast with the GTK_MINOR_VERSION macro,
;;; which represents the minor version of the GTK+ headers you have included
;;; when compiling your code.
;;; 
;;; Returns :
;;;     the minor version number of the GTK+ library
;;; 
;;; Since 3.0
;;; ----------------------------------------------------------------------------

(defun gtk-get-minor-version ()
  *gtk-minor-version*)

(export 'gtk-get-minor-version)

;;; ----------------------------------------------------------------------------
;;; gtk_get_micro_version ()
;;; 
;;; guint gtk_get_micro_version (void);
;;; 
;;; Returns the micro version number of the GTK+ library. (e.g. in GTK+ version
;;; 3.1.5 this is 5.)
;;; 
;;; This function is in the library, so it represents the GTK+ library your
;;; code is are running against. Contrast with the GTK_MICRO_VERSION macro,
;;; which represents the micro version of the GTK+ headers you have included
;;; when compiling your code.
;;; 
;;; Returns :
;;;     the micro version number of the GTK+ library
;;; 
;;; Since 3.0
;;; ----------------------------------------------------------------------------

(defun gtk-get-micro-version ()
  *gtk-micro-version*)

(export 'gtk-get-micro-version)

;;; ----------------------------------------------------------------------------
;;; gtk_get_binary_age ()
;;; 
;;; guint gtk_get_binary_age (void);
;;; 
;;; Returns the binary age as passed to libtool when building the GTK+ library
;;; the process is running against. If libtool means nothing to you, don't
;;; worry about it.
;;; 
;;; Returns :
;;;     the binary age of the GTK+ library
;;; 
;;; Since 3.0
;;; ----------------------------------------------------------------------------

(defun gtk-get-binary-age ()
  *gtk-binary-age*)

(export 'gtk-get-binary-age)

;;; ----------------------------------------------------------------------------
;;; gtk_get_interface_age ()
;;; 
;;; guint gtk_get_interface_age (void);
;;; 
;;; Returns the interface age as passed to libtool when building the GTK+
;;; library the process is running against. If libtool means nothing to you,
;;; don't worry about it.
;;; 
;;; Returns :
;;;     the interface age of the GTK+ library
;;; 
;;; Since 3.0
;;; ----------------------------------------------------------------------------

(defun gtk-get-interface-age ()
  *gtk-interface-age*)

(export 'gtk-get-interface-age)

;;; ----------------------------------------------------------------------------
;;; gtk_check_version ()
;;; 
;;; const gchar * gtk_check_version (guint required_major,
;;;                                  guint required_minor,
;;;                                  guint required_micro);
;;; 
;;; Checks that the GTK+ library in use is compatible with the given version.
;;; Generally you would pass in the constants GTK_MAJOR_VERSION,
;;; GTK_MINOR_VERSION, GTK_MICRO_VERSION as the three arguments to this
;;; function; that produces a check that the library in use is compatible with
;;; the version of GTK+ the application or module was compiled against.
;;; 
;;; Compatibility is defined by two things: first the version of the running
;;; library is newer than the version
;;; required_major.required_minor.required_micro. Second the running library
;;; must be binary compatible with the version
;;; required_major.required_minor.required_micro (same major version.)
;;; 
;;; This function is primarily for GTK+ modules; the module can call this
;;; function to check that it wasn't loaded into an incompatible version of
;;; GTK+. However, such a check isn't completely reliable, since the module may
;;; be linked against an old version of GTK+ and calling the old version of
;;; gtk_check_version(), but still get loaded into an application using a newer
;;; version of GTK+.
;;; 
;;; required_major :
;;;     the required major version
;;; 
;;; required_minor :
;;;     the required minor version
;;; 
;;; required_micro :
;;;     the required micro version
;;; 
;;; Returns :
;;;     NULL if the GTK+ library is compatible with the given version, or a
;;; string describing the version mismatch. The returned string is owned by
;;; GTK+ and should not be modified or freed.
;;; ----------------------------------------------------------------------------

(defcfun (check-version "gtk_check_version") :string
  (required_major :uint)
  (required_minor :uint)
  (required_micro :uint))

;;; ----------------------------------------------------------------------------

;;; More Lisp support

(defun cl-cffi-gtk-build-info ()
  (format t "~%cl-cffi-gtk version: ~a~%" *cl-cffi-gtk-version*)
  (format t "cl-cffi-gtk build date: ~a:~a ~a/~a/~a~%"
          (third cl-user:*cl-cffi-gtk-build-time*)
          (second cl-user:*cl-cffi-gtk-build-time*)
          (fifth cl-user:*cl-cffi-gtk-build-time*)
          (fourth cl-user:*cl-cffi-gtk-build-time*)
          (sixth cl-user:*cl-cffi-gtk-build-time*))
  (format t "GTK+ version: ~a.~a.~a~%"
          (gtk-get-major-version)
          (gtk-get-minor-version)
          (gtk-get-micro-version))
  (format t "Machine type: ~a~%" (machine-type))
  (format t "Machine version: ~a~%" (machine-version))
  (format t "Software type: ~a~%" (software-type))
  (format t "Software version: ~A~%" (software-version))
  (format t "Lisp implementation type: ~a~%" (lisp-implementation-type))
  (format t "Lisp implementation version: ~a~%~%" (lisp-implementation-version))
  nil)

(glib:push-library-version-features gtk
                                    *gtk-major-version*
                                    *gtk-minor-version*
  2 2
  2 4
  2 6
  2 8
  2 10
  2 12
  2 14
  2 16
  2 18)

(glib:require-library-version "Gtk+"
                              2 16
                              *gtk-major-version*
                              *gtk-minor-version*)

;;; --- End of file gtk.version.lisp -------------------------------------------
