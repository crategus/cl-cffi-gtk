;;; ----------------------------------------------------------------------------
;;; gtk.version.lisp
;;;
;;; The documentation of this file is taken from the GTK+ 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GTK+ library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
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
;;; Version Information
;;;
;;;     Variables and functions to check the GTK+ version
;;;
;;; Synopsis
;;;
;;;     gtk_get_major_version
;;;     gtk_get_minor_version
;;;     gtk_get_micro_version
;;;     gtk_get_binary_age
;;;     gtk_get_interface_age
;;;     gtk_check_version
;;;
;;;     GTK_MAJOR_VERSION                        * not implemented *
;;;     GTK_MINOR_VERSION                        * not implemented *
;;;     GTK_MICRO_VERSION                        * not implemented *
;;;     GTK_BINARY_AGE                           * not implemented *
;;;     GTK_INTERFACE_AGE                        * not implemented *
;;;     GTK_CHECK_VERSION                        * not implemented *
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; gtk_get_major_version () -> gtk-major-version
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_get_major_version" gtk-major-version) :int
 #+cl-cffi-gtk-documentation
 "@version{2020-8-23}
  @return{The major version number of the GTK+ library.}
  @begin{short}
    Returns the major version number of the GTK+ library.
  @end{short}

  This function is in the library, so it represents the GTK+ library your code
  is running against.
  @see-function{gtk-check-version}")

(export 'gtk-major-version)

;;; ----------------------------------------------------------------------------
;;; gtk_get_minor_version () -> gtk-minor-version
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_get_minor_version" gtk-minor-version) :int
 #+cl-cffi-gtk-documentation
 "@version{2020-8-23}
  @return{The minor version number of the GTK+ library.}
  @begin{short}
    Returns the minor version number of the GTK+ library.
  @end{short}

  This function is in the library, so it represents the GTK+ library your code
  is are running against.
  @see-function{gtk-check-version}")

(export 'gtk-minor-version)

;;; ----------------------------------------------------------------------------
;;; gtk_get_micro_version () -> gtk-micro-version
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_get_micro_version" gtk-micro-version) :int
 #+cl-cffi-gtk-documentation
 "@version{2020-8-23}
  @return{The micro version number of the GTK+ library.}
  @begin{short}
    Returns the micro version number of the GTK+ library.
  @end{short}

  This function is in the library, so it represents the GTK+ library your code
  is are running against.
  @see-function{gtk-check-version}")

(export 'gtk-micro-version)

;;; ----------------------------------------------------------------------------
;;; gtk_get_binary_age () -> gtk-binary-age
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_get_binary_age" gtk-binary-age) :int
 #+cl-cffi-gtk-documentation
 "@version{2020-8-23}
  @return{The binary age of the GTK+ library.}
  @begin{short}
    Returns the binary age as passed to @code{libtool} when building the GTK+
    library the process is running against.
  @end{short}
  If @code{libtool} means nothing to you, do not worry about it.
  @see-function{gtk-interface-age}")

(export 'gtk-binary-age)

;;; ----------------------------------------------------------------------------
;;; gtk_get_interface_age () -> gtk-interface-age
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_get_interface_age" gtk-interface-age) :int
 #+cl-cffi-gtk-documentation
 "@version{2020-8-23}
  @return{The interface age of the GTK+ library.}
  @begin{short}
    Returns the interface age as passed to @code{libtool} when building the
    GTK+ library the process is running against.
  @end{short}
  If @code{libtool} means nothing to you, do not worry about it.
  @see-function{gtk-binary-age}")

(export 'gtk-interface-age)

;;; ----------------------------------------------------------------------------
;;; gtk_check_version ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_check_version" gtk-check-version)
    (:string :free-from-foreign nil)
 #+cl-cffi-gtk-documentation
 "@version{2020-8-23}
  @argument[major]{the required major version}
  @argument[minor]{the required minor version}
  @argument[micro]{the required micro version}
  @begin{return}
    @code{Nil} if the GTK+ library is compatible with the given version,
    or a string describing the version mismatch.
  @end{return}
  @begin{short}
    Checks that the GTK+ library in use is compatible with the given version.
  @end{short}

  Compatibility is defined by two things: first the version of the running
  library is newer than the version @code{major}.@code{minor}.@code{micro}.
  Second the running library must be binary compatible with the version
  @code{major}.@code{minor}.@code{micro} (same major version).
  @see-function{gtk-major-version}
  @see-function{gtk-minor-version}
  @see-function{gtk-micro-version}"
  (major :uint)
  (minor :uint)
  (micro :uint))

(export 'gtk-check-version)

;;; ----------------------------------------------------------------------------
;;; GTK_MAJOR_VERSION
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_MINOR_VERSION
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_MICRO_VERSION
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_BINARY_AGE
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_INTERFACE_AGE
;;; ----------------------------------------------------------------------------

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

;;; Lisp functions

(defun cl-cffi-gtk-build-info ()
 #+cl-cffi-gtk-documentation
 "@version{2020-8-23}
  @begin{short}
    Provides informations about the installation and the versions of the
    loaded libraries.
  @end{short}
  @begin[Example]{dictionary}
    @begin{pre}
* (cl-cffi-gtk-build-info)

cl-cffi-gtk version: 1.0.0
cl-cffi-gtk build date: 16:2 8/23/2020
GTK+ version: 3.24.20
GLIB version: 2.64.3
GDK-Pixbuf version: 2.40.0
Pango version: 1.44.7
Cairo version: 1.16.0
Machine type: X86-64
Machine version: Intel(R) Core(TM) i5-4210U CPU @@ 1.70GHz
Software type: Linux
Software version: 5.4.0-42-generic
Lisp implementation type: SBCL
Lisp implementation version: 2.0.1.debian

NIL
    @end{pre}
  @end{dictionary}
  @see-function{gtk-major-version}
  @see-function{gtk-minor-version}
  @see-function{gtk-micro-version}
  @see-symbol{+glib-major-version+}
  @see-symbol{+glib-minor-version+}
  @see-symbol{+glib-micro-version+}
  @see-symbol{+gdk-pixbuf-version+}
  @see-function{pango-version-string}
  @see-function{cairo-version-string}"
  (format t "~%cl-cffi-gtk version: ~a~%" *cl-cffi-gtk-version*)
  (format t "cl-cffi-gtk build date: ~a:~a ~a/~a/~a~%"
          (third cl-user:*cl-cffi-gtk-build-time*)
          (second cl-user:*cl-cffi-gtk-build-time*)
          (fifth cl-user:*cl-cffi-gtk-build-time*)
          (fourth cl-user:*cl-cffi-gtk-build-time*)
          (sixth cl-user:*cl-cffi-gtk-build-time*))
  (format t "GTK+ version: ~a.~a.~a~%"
          (gtk-major-version)
          (gtk-minor-version)
          (gtk-micro-version))
  (format t "GLIB version: ~a.~a.~a~%"
          +glib-major-version+
          +glib-minor-version+
          +glib-micro-version+)
  (format t "GDK-Pixbuf version: ~a~%" +gdk-pixbuf-version+)
  (format t "Pango version: ~a~%" (pango-version-string))
  (format t "Cairo version: ~a~%" (cairo-version-string))
  (format t "Machine type: ~a~%" (machine-type))
  (format t "Machine version: ~a~%" (machine-version))
  (format t "Software type: ~a~%" (software-type))
  (format t "Software version: ~A~%" (software-version))
  (format t "Lisp implementation type: ~a~%" (lisp-implementation-type))
  (format t "Lisp implementation version: ~a~%~%" (lisp-implementation-version))
  nil)

;;; --- End of file gtk.version.lisp -------------------------------------------
