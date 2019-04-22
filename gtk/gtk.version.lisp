;;; ----------------------------------------------------------------------------
;;; gtk.version.lisp
;;;
;;; The documentation of this file is taken from the GTK+ 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2019 Dieter Kaiser
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
;;; gtk_get_major_version ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_get_major_version" gtk-get-major-version) :int
 #+cl-cffi-gtk-documentation
 "@version{2014-1-16}
  @return{The major version number of the GTK+ library.}
  @begin{short}
    Returns the major version number of the GTK+ library.
  @end{short}

  This function is in the library, so it represents the GTK+ library your code
  is running against.
  @see-function{gtk-check-version}")

(export 'gtk-get-major-version)

;;; ----------------------------------------------------------------------------
;;; gtk_get_minor_version ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_get_minor_version" gtk-get-minor-version) :int
 #+cl-cffi-gtk-documentation
 "@version{2014-1-16}
  @return{The minor version number of the GTK+ library.}
  @begin{short}
    Returns the minor version number of the GTK+ library.
  @end{short}

  This function is in the library, so it represents the GTK+ library your code
  is are running against.
  @see-function{gtk-check-version}")

(export 'gtk-get-minor-version)

;;; ----------------------------------------------------------------------------
;;; gtk_get_micro_version ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_get_micro_version" gtk-get-micro-version) :int
 #+cl-cffi-gtk-documentation
 "@version{2014-1-16}
  @return{The micro version number of the GTK+ library.}
  @begin{short}
    Returns the micro version number of the GTK+ library.
  @end{short}

  This function is in the library, so it represents the GTK+ library your code
  is are running against.
  @see-function{gtk-check-version}")

(export 'gtk-get-micro-version)

;;; ----------------------------------------------------------------------------
;;; gtk_get_binary_age ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_get_binary_age" gtk-get-binary-age) :int
 #+cl-cffi-gtk-documentation
 "@version{2014-1-16}
  @return{The binary age of the GTK+ library.}
  @begin{short}
    Returns the binary age as passed to @code{libtool} when building the GTK+
    library the process is running against.
  @end{short}
  If @code{libtool} means nothing to you, do not worry about it.
  @see-function{gtk-get-interface-age}")

(export 'gtk-get-binary-age)

;;; ----------------------------------------------------------------------------
;;; gtk_get_interface_age ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_get_interface_age" gtk-get-interface-age) :int
 #+cl-cffi-gtk-documentation
 "@version{2014-1-16}
  @return{The interface age of the GTK+ library.}
  @begin{short}
    Returns the interface age as passed to @code{libtool} when building the GTK+
    library the process is running against.
  @end{short}
  If @code{libtool} means nothing to you, do not worry about it.
  @see-function{gtk-get-binary-age}")

(export 'gtk-get-interface-age)

;;; ----------------------------------------------------------------------------
;;; gtk_check_version ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_check_version" gtk-check-version)
    (:string :free-from-foreign nil)
 #+cl-cffi-gtk-documentation
 "@version{2014-1-16}
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
  @see-function{gtk-get-major-version}
  @see-function{gtk-get-minor-version}
  @see-function{gtk-get-micro-version}"
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
 "@version{2013-2-15}
  @begin{short}
    Provides informations about the installation and the versions of the
    loaded libraries.
  @end{short}
  @begin[Example]{dictionary}
    @begin{pre}
* (cl-cffi-gtk-build-info)

cl-cffi-gtk version: 1.0.0
cl-cffi-gtk build date: 20:18 2/16/2013
GTK+ version: 3.6.0
GLIB version: 2.34.1
GDK-Pixbuf version: 2.26.4
Pango version: 1.30.1
Cairo version: 1.12.2
Machine type: X86
Machine version: Intel(R) Core(TM)2 Duo CPU T7250 @@ 2.00GHz
Software type: Linux
Software version: 3.5.0-24-generic
Lisp implementation type: SBCL
Lisp implementation version: 1.0.57

NIL
    @end{pre}
  @end{dictionary}
  @see-function{gtk-get-major-version}
  @see-function{gtk-get-minor-version}
  @see-function{gtk-get-micro-version}
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
          (gtk-get-major-version)
          (gtk-get-minor-version)
          (gtk-get-micro-version))
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
