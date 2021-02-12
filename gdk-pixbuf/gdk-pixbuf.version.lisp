;;; ----------------------------------------------------------------------------
;;; gdk-pixbuf.version.lisp
;;;
;;; The documentation of this file is taken from the GDK-PixBuf Reference Manual
;;; Version 2.36 and modified to document the Lisp binding to the GDK-PixBuf
;;; library. See <http://www.gtk.org>. The API documentation of the Lisp
;;; binding is available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2012 - 2020 Dieter Kaiser
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
;;; Initialization and Versions
;;;
;;; Library version numbers.
;;;
;;; Synopsis
;;;
;;;     gdk_pixbuf_version
;;;     gdk_pixbuf_major_version
;;;     gdk_pixbuf_minor_version
;;;     gdk_pixbuf_micro_version
;;;
;;;     GDK_PIXBUF_VERSION
;;;     GDK_PIXBUF_MAJOR
;;;     GDK_PIXBUF_MINOR
;;;     GDK_PIXBUF_MICRO
;;; ----------------------------------------------------------------------------

(in-package :gdk-pixbuf)

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_version
;;; ----------------------------------------------------------------------------

(defcvar ("gdk_pixbuf_version" +gdk-pixbuf-version+ :read-only t) :string)

#+cl-cffi-gtk-documentation
(setf (gethash '+gdk-pixbuf-version+ atdoc:*symbol-name-alias*)
      "Constant"
      (gethash '+gdk-pixbuf-version+ atdoc:*external-symbols*)
 "@version{2020-11-20}
  @begin{short}
    Contains the full version of the GDK-Pixbuf library as a string.
  @end{short}
  This is the version currently in use by a running program.
  @see-symbol{+gdk-pixbuf-major-version+}
  @see-symbol{+gdk-pixbuf-minor-version+}
  @see-symbol{+gdk-pixbuf-micro-version+}")

(export '+gdk-pixbuf-version+)

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_major_version
;;; ----------------------------------------------------------------------------

(defcvar ("gdk_pixbuf_major_version"
          +gdk-pixbuf-major-version+ :read-only t) :uint)

#+cl-cffi-gtk-documentation
(setf (gethash '+gdk-pixbuf-major-version+ atdoc:*symbol-name-alias*)
      "Constant"
      (gethash '+gdk-pixbuf-major-version+ atdoc:*external-symbols*)
 "@version{2020-11-20}
  @begin{short}
    The major version number of the GDK-Pixbuf library.
  @end{short}
  E.g. in GDK-Pixbuf version 1.2.5 this is 1. This variable is in the library,
  so represents the GDK-Pixbuf library you have loaded.
  @see-symbol{+gdk-pixbuf-version+}
  @see-symbol{+gdk-pixbuf-minor-version+}
  @see-symbol{+gdk-pixbuf-micro-version+}")

(export '+gdk-pixbuf-major-version+)

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_minor_version
;;; ----------------------------------------------------------------------------

(defcvar ("gdk_pixbuf_minor_version"
          +gdk-pixbuf-minor-version+ :read-only t) :uint)

#+cl-cffi-gtk-documentation
(setf (gethash '+gdk-pixbuf-minor-version+ atdoc:*symbol-name-alias*)
      "Constant"
      (gethash '+gdk-pixbuf-minor-version+ atdoc:*external-symbols*)
 "@version{2020-11-20}
  @begin{short}
    The minor version number of the GDK-Pixbuf library.
  @end{short}
  E.g. in GDK-Pixbuf version 1.2.5 this is 2. This variable is in the library,
  so represents the GDK-Pixbuf library you have loaded.
  @see-symbol{+gdk-pixbuf-version+}
  @see-symbol{+gdk-pixbuf-major-version+}
  @see-symbol{+gdk-pixbuf-micro-version+}")

(export '+gdk-pixbuf-minor-version+)

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_micro_version
;;; ----------------------------------------------------------------------------

(defcvar ("gdk_pixbuf_micro_version"
          +gdk-pixbuf-micro-version+ :read-only t) :uint)

#+cl-cffi-gtk-documentation
(setf (gethash '+gdk-pixbuf-micro-version+ atdoc:*symbol-name-alias*)
      "Constant"
      (gethash '+gdk-pixbuf-micro-version+ atdoc:*external-symbols*)
 "@version{2020-11-20}
  @begin{short}
    The micro version number of the GDK-Pixbuf library.
  @end{short}
  E.g. in GDK-Pixbuf version 1.2.5 this is 5. This variable is in the library,
  so represents the GDK-Pixbuf library you have loaded.
  @see-symbol{+gdk-pixbuf-version+}
  @see-symbol{+gdk-pixbuf-major-version+}
  @see-symbol{+gdk-pixbuf-minor-version+}")

(export '+gdk-pixbuf-micro-version+)

;;; ----------------------------------------------------------------------------
;;; GDK_PIXBUF_VERSION
;;;
;;; #define GDK_PIXBUF_VERSION "2.26.1"
;;;
;;; Contains the full version of the gdk-pixbuf header as a string. This is the
;;; version being compiled against; contrast with gdk_pixbuf_version.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GDK_PIXBUF_MAJOR
;;;
;;; #define GDK_PIXBUF_MAJOR (2)
;;;
;;; Major version of gdk-pixbuf library, that is the first "0" in "0.8.0" for
;;; example.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GDK_PIXBUF_MINOR
;;;
;;; #define GDK_PIXBUF_MINOR (26)
;;;
;;; Minor version of gdk-pixbuf library, that is the "8" in "0.8.0" for example.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GDK_PIXBUF_MICRO
;;;
;;; #define GDK_PIXBUF_MICRO (1)
;;;
;;; Micro version of gdk-pixbuf library, that is the last "0" in "0.8.0" for
;;; example.
;;; ----------------------------------------------------------------------------

;;; --- End of file gdk-pixbuf.version.lisp ------------------------------------
