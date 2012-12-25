;;; ----------------------------------------------------------------------------
;;; atdoc-gtk.lisp
;;;
;;; Documentation strings for the library GTK.
;;;
;;; The documentation of this file has been copied from the
;;; GLib 2.32.3 Reference Manual. See http://www.gtk.org.
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

(in-package :gtk)

(load "../gtk/atdoc/src/atdoc-gtk.package.lisp")
(load "../gtk/atdoc/src/atdoc-gtk.main-loop.lisp")
(load "../gtk/atdoc/src/atdoc-gtk.selections.lisp")
(load "../gtk/atdoc/src/atdoc-gtk.dialog.lisp")
(load "../gtk/atdoc/src/atdoc-gtk.about-dialog.lisp")
(load "../gtk/atdoc/src/atdoc-gtk.widget.lisp")

;;; ----------------------------------------------------------------------------

(in-package :gdk)

(load "../gdk/atdoc/src/atdoc-gdk.package.lisp")

;;; ----------------------------------------------------------------------------

(in-package :gdk-pixbuf)

(load "../gdk-pixbuf/atdoc/src/atdoc-gdk-pixbuf.package.lisp")

;;; ----------------------------------------------------------------------------

(in-package :gobject)

(load "../gobject/atdoc/src/atdoc-gobject.package.lisp")
(load "../gobject/atdoc/src/atdoc-gobject.type-info.lisp")

;;; ----------------------------------------------------------------------------

(in-package :glib)

(load "../glib/atdoc/src/atdoc-glib.package.lisp")
(load "../glib/atdoc/src/atdoc-glib.version.lisp")
(load "../glib/atdoc/src/atdoc-glib.misc.lisp")
(load "../glib/atdoc/src/atdoc-glib.threads.lisp")

;;; ----------------------------------------------------------------------------

(in-package :gio)

(load "../gio/atdoc/src/atdoc-gio.package.lisp")

;;; ----------------------------------------------------------------------------

(in-package :pango)

(load "../pango/atdoc/src/atdoc-pango.package.lisp")

;;; ----------------------------------------------------------------------------

(in-package :cairo)

(load "../cairo/atdoc/src/atdoc-cairo.package.lisp")
(load "../cairo/atdoc/src/atdoc-cairo.context.lisp")

;;; --- End of file atdoc-gtk.lisp ---------------------------------------------
