;;; ----------------------------------------------------------------------------
;;; glib.version.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See <http://common-lisp.net/project/cl-gtk2/>.
;;;
;;; The documentation of this file has been copied from the
;;; GLib 2.34.3 Reference Manual. See <http://www.gtk.org>.
;;; The API documentation of the Lisp binding is available at
;;; <http://www.crategus.com/books/cl-cffi-gtk/>.
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
;;; Version Information
;;;
;;; Variables and functions to check the GLib version
;;;
;;; Synopsis
;;;
;;;     glib_major_version
;;;     glib_minor_version
;;;     glib_micro_version
;;;     glib_binary_age
;;;     glib_interface_age
;;;     glib_check_version
;;;
;;;     GLIB_MAJOR_VERSION
;;;     GLIB_MINOR_VERSION
;;;     GLIB_MICRO_VERSION
;;;     GLIB_CHECK_VERSION
;;;
;;;     GLIB_VERSION_2_26
;;;     GLIB_VERSION_2_28
;;;     GLIB_VERSION_2_30
;;;     GLIB_VERSION_2_32
;;;     GLIB_VERSION_MIN_REQUIRED
;;;     GLIB_VERSION_MAX_ALLOWED
;;;
;;; Description
;;;
;;; GLib provides version information, primarily useful in configure checks for
;;; builds that have a configure script. Applications will not typically use the
;;; features described here.
;;;
;;; The GLib headers annotate deprecated APIs in a way that produces compiler
;;; warnings if these deprecated APIs are used. The warnings can be turned off
;;; by defining the macro GLIB_DISABLE_DEPRECATION_WARNINGS before including the
;;; glib.h header.
;;;
;;; GLib also provides support for building applications against defined subsets
;;; of deprecated or new GLib APIs. Define the macro GLIB_VERSION_MIN_REQUIRED
;;; to specify up to what version of GLib you want to receive warnings about
;;; deprecated APIs. Define the macro GLIB_VERSION_MAX_ALLOWED to specify the
;;; newest version of GLib whose API you want to use.
;;; ----------------------------------------------------------------------------

(in-package :glib)

;;; ----------------------------------------------------------------------------
;;; glib_major_version
;;; ----------------------------------------------------------------------------

(defcvar ("glib_major_version" glib-major-version :read-only t) :uint)

(export 'glib-major-version)

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'glib-major-version atdoc:*symbol-name-alias*) "Variable"
      (gethash 'glib-major-version atdoc:*external-symbols*)
 "@version{2013-1-15}
  @short{The major version number of the GLib library.}")

;;; ----------------------------------------------------------------------------
;;; glib_minor_version
;;; ----------------------------------------------------------------------------

(defcvar ("glib_minor_version" glib-minor-version :read-only t) :uint)

(export 'glib-minor-version)

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'glib-minor-version atdoc:*symbol-name-alias*) "Variable"
      (gethash 'glib-minor-version atdoc:*external-symbols*)
 "@version{2013-1-15}
  @short{The minor version number of the GLib library.}")

;;; ----------------------------------------------------------------------------
;;; glib_micro_version
;;; ----------------------------------------------------------------------------

(defcvar ("glib_micro_version" glib-micro-version :read-only t) :uint)

(export 'glib-micro-version)

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'glib-micro-version atdoc:*symbol-name-alias*) "Variable"
      (gethash 'glib-micro-version atdoc:*external-symbols*)
 "@version{2013-1-15}
  @short{The micro version number of the GLib library.}")

;;; ----------------------------------------------------------------------------
;;; glib_binary_age
;;; ----------------------------------------------------------------------------

(defcvar ("glib_binary_age" glib-binary-age :read-only t) :uint)

(export 'glib-binary-age)

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'glib-binary-age atdoc:*symbol-name-alias*) "Variable"
      (gethash 'glib-binary-age atdoc:*external-symbols*)
 "@version{2013-1-15}
  @short{The binary age of the GLib library.}")

;;; ----------------------------------------------------------------------------
;;; glib_interface_age
;;; ----------------------------------------------------------------------------

(defcvar ("glib_interface_age" glib-interface-age :read-only t) :uint)

(export 'glib-interface-age)

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'glib-interface-age atdoc:*symbol-name-alias*) "Variable"
      (gethash 'glib-interface-age atdoc:*external-symbols*)
 "@version{2013-1-15}
  @short{The interface age of the GLib library.}")

;;; ----------------------------------------------------------------------------
;;; glib_check_version ()
;;; ----------------------------------------------------------------------------

(defcfun ("glib_check_version" glib-check-version ) :string
 #+cl-cffi-gtk-documentation
 "@version{2013-1-15}
  @argument[required-major]{the required major version.}
  @argument[required-minor]{the required minor version.}
  @argument[required-micro]{the required micro version.}
  @return{@code{nil} if the GLib library is compatible with the given version,
    or a string describing the version mismatch. The returned string is owned by
    GLib and must not be modified or freed.}
  @short{Checks that the GLib library in use is compatible with the given
    version.}

  Since 2.6
  @see-symbol{glib-major-version}
  @see-symbol{glib-minor-version}
  @see-symbol{glib-micro-version}
  @see-symbol{glib-binary-age}
  @see-symbol{glib-interface-age}"
  (required-major :uint)
  (required-minor :uint)
  (required-micro :uint))

(export 'glib-check-version)

;;; ----------------------------------------------------------------------------
;;; GLIB_MAJOR_VERSION
;;;
;;; #define GLIB_MAJOR_VERSION 2
;;;
;;; The major version number of the GLib library.
;;;
;;; Like glib_major_version, but from the headers used at application compile
;;; time, rather than from the library linked against at application run time.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GLIB_MINOR_VERSION
;;;
;;; #define GLIB_MINOR_VERSION 34
;;;
;;; The minor version number of the GLib library.
;;;
;;; Like gtk_minor_version, but from the headers used at application compile
;;; time, rather than from the library linked against at application run time.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GLIB_MICRO_VERSION
;;;
;;; #define GLIB_MICRO_VERSION 3
;;;
;;; The micro version number of the GLib library.
;;;
;;; Like gtk_micro_version, but from the headers used at application compile
;;; time, rather than from the library linked against at application run time.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GLIB_CHECK_VERSION()
;;;
;;; #define GLIB_CHECK_VERSION(major,minor,micro)
;;;
;;; Checks the version of the GLib library that is being compiled against.
;;;
;;; Example 1. Checking the version of the GLib library
;;;
;;; if (!GLIB_CHECK_VERSION (1, 2, 0))
;;;   g_error ("GLib version 1.2.0 or above is needed");
;;;
;;; See glib_check_version() for a runtime check.
;;;
;;; major :
;;;     the major version to check for
;;;
;;; minor :
;;;     the minor version to check for
;;;
;;; micro :
;;;     the micro version to check for
;;;
;;; Returns :
;;;     TRUE if the version of the GLib header files is the same as or newer
;;;     than the passed-in version.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GLIB_VERSION_2_26
;;;
;;; #define GLIB_VERSION_2_26 (G_ENCODE_VERSION (2, 26))
;;;
;;; A macro that evaluates to the 2.26 version of GLib, in a format that can be
;;; used by the C pre-processor.
;;;
;;; Since 2.32
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GLIB_VERSION_2_28
;;;
;;; #define GLIB_VERSION_2_28 (G_ENCODE_VERSION (2, 28))
;;;
;;; A macro that evaluates to the 2.28 version of GLib, in a format that can be
;;; used by the C pre-processor.
;;;
;;; Since 2.32
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GLIB_VERSION_2_30
;;;
;;; #define GLIB_VERSION_2_30 (G_ENCODE_VERSION (2, 30))
;;;
;;; A macro that evaluates to the 2.30 version of GLib, in a format that can be
;;; used by the C pre-processor.
;;;
;;; Since 2.32
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GLIB_VERSION_2_32
;;;
;;; #define GLIB_VERSION_2_32 (G_ENCODE_VERSION (2, 32))
;;;
;;; A macro that evaluates to the 2.32 version of GLib, in a format that can be
;;; used by the C pre-processor.
;;;
;;; Since 2.32
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GLIB_VERSION_MIN_REQUIRED
;;;
;;; #define GLIB_VERSION_MIN_REQUIRED (GLIB_VERSION_PREV_STABLE)
;;;
;;; A macro that should be defined by the user prior to including the glib.h
;;; header. The definition should be one of the predefined GLib version macros:
;;; GLIB_VERSION_2_26, GLIB_VERSION_2_28,...
;;;
;;; This macro defines the lower bound for the GLib API to use.
;;;
;;; If a function has been deprecated in a newer version of GLib, it is possible
;;; to use this symbol to avoid the compiler warnings without disabling warning
;;; for every deprecated function.
;;;
;;; Since 2.32
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GLIB_VERSION_MAX_ALLOWED
;;;
;;; #define GLIB_VERSION_MAX_ALLOWED GLIB_VERSION_MIN_REQUIRED
;;;
;;; A macro that should be defined by the user prior to including the
;;; glib.h header. The definition should be one of the predefined GLib version
;;; macros: GLIB_VERSION_2_26, GLIB_VERSION_2_28,...
;;;
;;; This macro defines the upper bound for the GLib API to use.
;;;
;;; If a function has been introduced in a newer version of GLib, it is possible
;;; to use this symbol to get compiler warnings when trying to use that
;;; function.
;;;
;;; Since 2.32
;;; ----------------------------------------------------------------------------


;;; Lisp support to check the library version

(defmacro push-library-version-features (library-name
                                         major-version-var
                                         minor-version-var
                                         &body versions)
  `(eval-when (:load-toplevel :execute)
     ,@(iter (for (major minor) on versions by #'cddr)
             (collect
                 `(when (or (and (= ,major-version-var ,major)
                                 (>= ,minor-version-var ,minor))
                            (> ,major-version-var ,major))
                    (pushnew ,(intern (format nil "~A-~A.~A"
                                              (string library-name)
                                              major minor)
                                      (find-package :keyword))
                             *features*))))))

(define-condition foreign-library-minimum-version-mismatch (error)
  ((library :initarg :library :reader .library)
   (minimum-version :initarg :minimum-version :reader .minimum-version)
   (actual-version :initarg :actual-version :reader .actual-version))
  (:report (lambda (c s)
             (format s
                     "Library ~A has too old version: it is ~A but required ~
                      to be at least ~A"
                     (.library c)
                     (.actual-version c)
                     (.minimum-version c)))))

(defun require-library-version (library min-major-version
                                        min-minor-version
                                        major-version
                                        minor-version)
  (unless (or (> major-version min-major-version)
              (and (= major-version min-major-version)
                   (>= minor-version min-minor-version)))
    (restart-case
        (error 'foreign-library-minimum-version-mismatch
               :library library
               :minimum-version (format nil "~A.~A"
                                        min-major-version min-minor-version)
               :actual-version (format nil "~A.~A"
                                       major-version minor-version))
      (ignore () :report "Ignore version requirement" nil))))

(push-library-version-features glib
  glib-major-version glib-micro-version
  2 32
  2 34)

(require-library-version "GLib" 2 32 glib-major-version glib-minor-version)

;;; --- End of file glib.version.lisp ------------------------------------------
