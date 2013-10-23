;;; ----------------------------------------------------------------------------
;;; glib.version.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See <http://common-lisp.net/project/cl-gtk2/>.
;;;
;;; The documentation of this file is taken from the GLib 2.36.3 Reference
;;; Manual and modified to document the Lisp binding to the GLib library.
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
;;;     GLIB_VERSION_2_34
;;;     GLIB_VERSION_2_36
;;;     GLIB_VERSION_MIN_REQUIRED
;;;     GLIB_VERSION_MAX_ALLOWED
;;;     GLIB_DISABLE_DEPRECATION_WARNINGS
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

#+cl-cffi-gtk-documentation
(setf (gethash 'glib-major-version atdoc:*symbol-name-alias*) "Constant"
      (gethash 'glib-major-version atdoc:*external-symbols*)
 "@version{2013-7-22}
  The major version number of the GLib C library.
  @see-symbol{glib-minor-version}
  @see-symbol{glib-micro-version}
  @see-symbol{glib-binary-age}
  @see-symbol{glib-interface-age}
  @see-variable{+glib-major-version+}
  @see-variable{+glib-minor-version+}
  @see-variable{+glib-micro-version+}
  @see-function{glib-check-version}")

(export 'glib-major-version)

;;; ----------------------------------------------------------------------------
;;; glib_minor_version
;;; ----------------------------------------------------------------------------

(defcvar ("glib_minor_version" glib-minor-version :read-only t) :uint)

#+cl-cffi-gtk-documentation
(setf (gethash 'glib-minor-version atdoc:*symbol-name-alias*) "Constant"
      (gethash 'glib-minor-version atdoc:*external-symbols*)
 "@version{2013-7-22}
  The minor version number of the GLib C library.
  @see-symbol{glib-major-version}
  @see-symbol{glib-micro-version}
  @see-symbol{glib-binary-age}
  @see-symbol{glib-interface-age}
  @see-variable{+glib-major-version+}
  @see-variable{+glib-minor-version+}
  @see-variable{+glib-micro-version+}
  @see-function{glib-check-version}")

(export 'glib-minor-version)

;;; ----------------------------------------------------------------------------
;;; glib_micro_version
;;; ----------------------------------------------------------------------------

(defcvar ("glib_micro_version" glib-micro-version :read-only t) :uint)

#+cl-cffi-gtk-documentation
(setf (gethash 'glib-micro-version atdoc:*symbol-name-alias*) "Constant"
      (gethash 'glib-micro-version atdoc:*external-symbols*)
 "@version{2013-7-22}
  The micro version number of the GLib C library.
  @see-symbol{glib-major-version}
  @see-symbol{glib-minor-version}
  @see-symbol{glib-binary-age}
  @see-symbol{glib-interface-age}
  @see-variable{+glib-major-version+}
  @see-variable{+glib-minor-version+}
  @see-variable{+glib-micro-version+}
  @see-function{glib-check-version}")

(export 'glib-micro-version)

;;; ----------------------------------------------------------------------------
;;; glib_binary_age
;;; ----------------------------------------------------------------------------

(defcvar ("glib_binary_age" glib-binary-age :read-only t) :uint)

#+cl-cffi-gtk-documentation
(setf (gethash 'glib-binary-age atdoc:*symbol-name-alias*) "Constant"
      (gethash 'glib-binary-age atdoc:*external-symbols*)
 "@version{2013-7-21}
  The binary age of the GLib C library.
  @see-symbol{glib-major-version}
  @see-symbol{glib-minor-version}
  @see-symbol{glib-micro-version}
  @see-symbol{glib-interface-age}
  @see-variable{+glib-major-version+}
  @see-variable{+glib-minor-version+}
  @see-variable{+glib-micro-version+}
  @see-function{glib-check-version}")

(export 'glib-binary-age)

;;; ----------------------------------------------------------------------------
;;; glib_interface_age
;;; ----------------------------------------------------------------------------

(defcvar ("glib_interface_age" glib-interface-age :read-only t) :uint)

#+cl-cffi-gtk-documentation
(setf (gethash 'glib-interface-age atdoc:*symbol-name-alias*) "Constant"
      (gethash 'glib-interface-age atdoc:*external-symbols*)
 "@version{2013-7-22}
  The interface age of the GLib C library.
  @see-symbol{glib-major-version}
  @see-symbol{glib-minor-version}
  @see-symbol{glib-micro-version}
  @see-symbol{glib-binary-age}
  @see-variable{+glib-major-version+}
  @see-variable{+glib-minor-version+}
  @see-variable{+glib-micro-version+}
  @see-function{glib-check-version}")

(export 'glib-interface-age)

;;; ----------------------------------------------------------------------------
;;; glib_check_version ()
;;; ----------------------------------------------------------------------------

(defcfun ("glib_check_version" glib-check-version )
   (:string :free-from-foreign nil)
 #+cl-cffi-gtk-documentation
 "@version{2013-7-22}
  @argument[required-major]{the required major version}
  @argument[required-minor]{the required minor version}
  @argument[required-micro]{the required micro version}
  @begin{return}
    @code{nil} if the GLib C library is compatible with the given version,
    or a string describing the version mismatch.
  @end{return}
  @begin{short}
    Checks that the GLib C library in use is compatible with the given version.
  @end{short}

  Since 2.6
  @begin[Examples]{dictionary}
    @begin{pre}
 (glib-check-version 2 34 0)
=> NIL
 (glib-check-version 2 36 0)
=> \"GLib version too old (micro mismatch)\"
    @end{pre}
  @end{dictionary}
  @see-symbol{glib-major-version}
  @see-symbol{glib-minor-version}
  @see-symbol{glib-micro-version}
  @see-variable{+glib-major-version+}
  @see-variable{+glib-minor-version+}
  @see-variable{+glib-micro-version+}"
  (required-major :uint)
  (required-minor :uint)
  (required-micro :uint))

(export 'glib-check-version)

;;; ----------------------------------------------------------------------------
;;; GLIB_MAJOR_VERSION
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash '+glib-major-version+ atdoc:*variable-name-alias*) "Constant")

(defconstant +glib-major-version+ 2
 #+cl-cffi-gtk-documentation
 "@version{2013-4-8}
  @variable-value{2}
  @short{The major version number of the GLib Lisp library.}

  Like @symbol{glib-major-version}, but from the Lisp library used at
  application compile time, rather than from the C library loaded at
  application run time.
  @see-symbol{glib-major-version}")

(export '+glib-major-version+)

;;; ----------------------------------------------------------------------------
;;; GLIB_MINOR_VERSION
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash '+glib-minor-version+ atdoc:*variable-name-alias*) "Constant")

(defconstant +glib-minor-version+ 36
 #+cl-cffi-gtk-documentation
 "@version{2013-4-8}
  @variable-value{34}
  @short{The minor version number of the GLib library.}

  Like @symbol{glib-minor-version}, but from the Lisp library used at
  application compile time, rather than from the C library loaded at
  application run time.
  @see-symbol{glib-minor-version}")

(export '+glib-minor-version+)

;;; ----------------------------------------------------------------------------
;;; GLIB_MICRO_VERSION
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash '+glib-micro-version+ atdoc:*variable-name-alias*) "Constant")

;; The documentation is taken from the GDK Reference manual for version 2.36.3,
;; but we implement the micro version number to 0. This way we match all
;; versions with a minor version number of 36.

(defconstant +glib-micro-version+ 0
 #+cl-cffi-gtk-documentation
 "@version{2013-4-8}
  @variable-value{0}
  @short{The micro version number of the GLib library.}

  Like @symbol{glib-micro-version}, but from the Lisp library used at
  application compile time, rather than from the C library loaded at
  application run time.
  @see-symbol{glib-micro-version}")

(export '+glib-micro-version+)

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
;;; GLIB_VERSION_2_34
;;;
;;; #define GLIB_VERSION_2_34 (G_ENCODE_VERSION (2, 34))
;;;
;;; A macro that evaluates to the 2.34 version of GLib, in a format that can be
;;; used by the C pre-processor.
;;;
;;; Since 2.34
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GLIB_VERSION_2_36
;;;
;;; #define GLIB_VERSION_2_36 (G_ENCODE_VERSION (2, 36))
;;;
;;; A macro that evaluates to the 2.36 version of GLib, in a format that can be
;;; used by the C pre-processor.
;;;
;;; Since 2.36
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

;;; ----------------------------------------------------------------------------
;;; GLIB_DISABLE_DEPRECATION_WARNINGS
;;;
;;; #ifdef GLIB_DISABLE_DEPRECATION_WARNINGS
;;;
;;; A macro that should be defined before including the glib.h header. If it is
;;; defined, no compiler warnings will be produced for uses of deprecated GLib
;;; APIs.
;;; ----------------------------------------------------------------------------

(push-library-version-features glib
  glib-major-version glib-minor-version
  2 36
  2 34
  2 32
  2 30)

(require-library-version "GLib" 2 32 glib-major-version glib-minor-version)

;;; --- End of file glib.version.lisp ------------------------------------------
