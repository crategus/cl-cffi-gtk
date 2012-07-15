;;; ----------------------------------------------------------------------------
;;; gdk-pixbuf.version.lisp
;;;
;;; The documentation has been copied from the GDK-PixBuf Reference Manual
;;; Version 2.26.1. See http://www.gtk.org.
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
;;;
;;; Description
;;;
;;; These macros and variables let you check the version of gdk-pixbuf you're
;;; linking against.
;;; ----------------------------------------------------------------------------

(in-package :gdk-pixbuf)

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_version
;;;
;;; extern const char *gdk_pixbuf_version;
;;;
;;; Contains the full version of the gdk-pixbuf library as a string. This is
;;; the version currently in use by a running program.
;;; ----------------------------------------------------------------------------

(defcvar ("gdk_pixbuf_version" *gdk-pixbuf-version* :read-only t) :string)

(export '*gdk-pixbuf-version*)

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_major_version
;;;
;;; extern const guint gdk_pixbuf_major_version;
;;;
;;; The major version number of the gdk-pixbuf library. (e.g. in gdk-pixbuf
;;; version 1.2.5 this is 1.)
;;;
;;; This variable is in the library, so represents the gdk-pixbuf library you
;;; have linked against. Contrast with the GDK_PIXBUF_MAJOR macro, which
;;; represents the major version of the gdk-pixbuf headers you have included.
;;; ----------------------------------------------------------------------------

(defcvar ("gdk_pixbuf_major_version"
          *gdk-pixbuf-major-version* :read-only t) :uint)

(export '*gdk-pixbuf-major-version*)

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_minor_version
;;;
;;; extern const guint gdk_pixbuf_minor_version;
;;;
;;; The minor version number of the gdk-pixbuf library. (e.g. in gdk-pixbuf
;;; version 1.2.5 this is 2.)
;;;
;;; This variable is in the library, so represents the gdk-pixbuf library you
;;; have linked against. Contrast with the GDK_PIXBUF_MINOR macro, which
;;; represents the minor version of the &gdk-pixbuf; headers you have included.
;;; ----------------------------------------------------------------------------

(defcvar ("gdk_pixbuf_minor_version"
          *gdk-pixbuf-minor-version* :read-only t) :uint)

(export '*gdk-pixbuf-minor-version*)

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_micro_version
;;;
;;; extern const guint gdk_pixbuf_micro_version;
;;;
;;; The micro version number of the gdk-pixbuf library. (e.g. in gdk-pixbuf
;;; version 1.2.5 this is 5.)
;;;
;;; This variable is in the library, so represents the gdk-pixbuf library you
;;; have linked against. Contrast with the GDK_PIXBUF_MICRO macro, which
;;; represents the micro version of the &gdk-pixbuf; headers you have included.
;;; ----------------------------------------------------------------------------

(defcvar ("gdk_pixbuf_micro_version"
          *gdk-pixbuf-micro-version* :read-only t) :uint)

(export '*gdk-pixbuf-micro-version*)

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
