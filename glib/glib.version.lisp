;;; ----------------------------------------------------------------------------
;;; glib.version.lisp
;;;
;;; The documentation of this file is taken from the GLib 2.68 Reference
;;; Manual and modified to document the Lisp binding to the GLib library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2021 Dieter Kaiser
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
;;;     Variables and functions to check the GLib version
;;;
;;; Types and Values
;;;
;;;     glib_major_version
;;;     glib_minor_version
;;;     glib_micro_version
;;;     glib_binary_age
;;;     glib_interface_age
;;;
;;; Functions
;;;
;;;     glib_check_version
;;; ----------------------------------------------------------------------------

(in-package :glib)

;;; ----------------------------------------------------------------------------
;;; glib_major_version
;;; ----------------------------------------------------------------------------

(defcvar ("glib_major_version" +glib-major-version+ :read-only t) :uint)

#+cl-cffi-gtk-documentation
(setf (gethash '+glib-major-version+ atdoc:*symbol-name-alias*)
      "Constant"
      (gethash '+glib-major-version+ atdoc:*external-symbols*)
 "@version{2021-4-9}
  @begin{short}
    The major version number of the Glib C library the Lisp binding is running
    against.
  @end{short}
  @see-function{glib-check-version}")

(export '+glib-major-version+)

;;; ----------------------------------------------------------------------------
;;; glib_minor_version
;;; ----------------------------------------------------------------------------

(defcvar ("glib_minor_version" +glib-minor-version+ :read-only t) :uint)

#+cl-cffi-gtk-documentation
(setf (gethash '+glib-minor-version+ atdoc:*symbol-name-alias*)
      "Constant"
      (gethash '+glib-minor-version+ atdoc:*external-symbols*)
 "@version{2021-4-9}
  @begin{short}
    The minor version number of the GLib C library the Lisp binding is running
    against.
  @end{short}
  @see-function{glib-check-version}")

(export '+glib-minor-version+)

;;; ----------------------------------------------------------------------------
;;; glib_micro_version
;;; ----------------------------------------------------------------------------

(defcvar ("glib_micro_version" +glib-micro-version+ :read-only t) :uint)

#+cl-cffi-gtk-documentation
(setf (gethash '+glib-micro-version+ atdoc:*symbol-name-alias*)
      "Constant"
      (gethash '+glib-micro-version+ atdoc:*external-symbols*)
 "@version{2021-4-9}
  @begin{short}
    The micro version number of the GLib C library the Lisp binding is running
    against.
  @end{short}
  @see-function{glib-check-version}")

(export '+glib-micro-version+)

;;; ----------------------------------------------------------------------------
;;; glib_binary_age
;;; ----------------------------------------------------------------------------

(defcvar ("glib_binary_age" +glib-binary-age+ :read-only t) :uint)

#+cl-cffi-gtk-documentation
(setf (gethash '+glib-binary-age+ atdoc:*symbol-name-alias*)
      "Constant"
      (gethash '+glib-binary-age+ atdoc:*external-symbols*)
 "@version{2021-4-9}
  @begin{short}
    The binary age of the GLib C library the Lisp binding is running
    against.
  @end{short}
  @see-function{glib-check-version}")

(export '+glib-binary-age+)

;;; ----------------------------------------------------------------------------
;;; glib_interface_age
;;; ----------------------------------------------------------------------------

(defcvar ("glib_interface_age" +glib-interface-age+ :read-only t) :uint)

#+cl-cffi-gtk-documentation
(setf (gethash '+glib-interface-age+ atdoc:*symbol-name-alias*)
      "Constant"
      (gethash '+glib-interface-age+ atdoc:*external-symbols*)
 "@version{2021-4-9}
  @begin{short}
    The interface age of the GLib C library the Lisp binding is running
    against.
  @end{short}
  @see-function{glib-check-version}")

(export '+glib-interface-age+)

;;; ----------------------------------------------------------------------------
;;; glib_check_version ()
;;; ----------------------------------------------------------------------------

(defcfun ("glib_check_version" glib-check-version)
   (:string :free-from-foreign nil)
 #+cl-cffi-gtk-documentation
 "@version{2021-4-9}
  @argument[major]{an unsigned integer with the required major version}
  @argument[minor]{an unsigned integer with the required minor version}
  @argument[micro]{an unsigned integer with the required micro version}
  @begin{return}
    @code{nil} if the GLib C library the Lisp binding is running against is
    compatible with the given version, or a string describing the version
    mismatch.
  @end{return}
  @begin{short}
    Checks that the GLib C library in use is compatible with the given version.
  @end{short}
  @begin[Examples]{dictionary}
    Suppose the Glib library version 2.66.1 is installed. Then the following
    results are returned:
    @begin{pre}
(glib-check-version 2 66 1) => NIL
(glib-check-version 2 66 2) => \"GLib version too old (micro mismatch)\"
    @end{pre}
  @end{dictionary}
  @see-function{cl-cffi-gtk-build-info}"
  (major :uint)
  (minor :uint)
  (micro :uint))

(export 'glib-check-version)

;;; --- End of file glib.version.lisp ------------------------------------------
