;;; ----------------------------------------------------------------------------
;;; atdoc-gtk.lisp
;;;
;;; Documentation strings for the library GTK.
;;;
;;; The documentation of this file has been copied from the
;;; GLib 2.32.3 Reference Manual. See http://www.gtk.org.
;;;
;;; Copyright (C) 2012, 2013 Dieter Kaiser
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
(load "../gtk/atdoc/src/atdoc-gtk.widget.lisp")
(load "../gtk/atdoc/src/atdoc-gtk.window.lisp")
(load "../gtk/atdoc/src/atdoc-gtk.enumerations.lisp")
(load "../gtk/atdoc/src/atdoc-gtk.container.lisp")
(load "../gtk/atdoc/src/atdoc-gtk.misc.lisp")
(load "../gtk/atdoc/src/atdoc-gtk.grid.lisp")
(load "../gtk/atdoc/src/atdoc-gtk.style.lisp")
(load "../gtk/atdoc/src/atdoc-gtk.tooltip.lisp")
(load "../gtk/atdoc/src/atdoc-gtk.button.lisp")
(load "../gtk/atdoc/src/atdoc-gtk.orientable.lisp")
(load "../gtk/atdoc/src/atdoc-gtk.table.lisp")
(load "../gtk/atdoc/src/atdoc-gtk.label.lisp")
(load "../gtk/atdoc/src/atdoc-gtk.calendar.lisp")
(load "../gtk/atdoc/src/atdoc-gtk.frame.lisp")
(load "../gtk/atdoc/src/atdoc-gtk.separator.lisp")
(load "../gtk/atdoc/src/atdoc-gtk.invisible.lisp")
(load "../gtk/atdoc/src/atdoc-gtk.message-dialog.lisp")
(load "../gtk/atdoc/src/atdoc-gtk.window-group.lisp")
(load "../gtk/atdoc/src/atdoc-gtk.offscreen-window.lisp")
(load "../gtk/atdoc/src/atdoc-gtk.image.lisp")
(load "../gtk/atdoc/src/atdoc-gtk.progress-bar.lisp")
(load "../gtk/atdoc/src/atdoc-gtk.statusbar.lisp")
(load "../gtk/atdoc/src/atdoc-gtk.info-bar.lisp")
(load "../gtk/atdoc/src/atdoc-gtk.status-icon.lisp")

;;; ----------------------------------------------------------------------------

(in-package :gdk)

(load "../gdk/atdoc/src/atdoc-gdk.package.lisp")
(load "../gdk/atdoc/src/atdoc-gdk.event-structures.lisp")
(load "../gdk/atdoc/src/atdoc-gdk.window.lisp")
(load "../gdk/atdoc/src/atdoc-gdk.color.lisp")
(load "../gdk/atdoc/src/atdoc-gdk.rectangle.lisp")

;;; ----------------------------------------------------------------------------

(in-package :gdk-pixbuf)

(load "../gdk-pixbuf/atdoc/src/atdoc-gdk-pixbuf.package.lisp")

;;; ----------------------------------------------------------------------------

(in-package :gobject)

(load "../gobject/atdoc/src/atdoc-gobject.package.lisp")

(load "../gobject/atdoc/src/atdoc-gobject.base.lisp")
(load "../gobject/atdoc/src/atdoc-gobject.signals.lisp")

;;; ----------------------------------------------------------------------------

(in-package :glib)

(load "../glib/atdoc/src/atdoc-glib.package.lisp")
(load "../glib/atdoc/src/atdoc-glib.version.lisp")
(load "../glib/atdoc/src/atdoc-glib.misc.lisp")
(load "../glib/atdoc/src/atdoc-glib.threads.lisp")
(load "../glib/atdoc/src/atdoc-glib.main-loop.lisp")
(load "../glib/atdoc/src/atdoc-glib.utils.lisp")
(load "../glib/atdoc/src/atdoc-glib.quark.lisp")
(load "../glib/atdoc/src/atdoc-glib.error.lisp")

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
