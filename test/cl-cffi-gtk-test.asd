;;; ----------------------------------------------------------------------------
;;; cl-cffi-gtk-test.asd
;;;
;;; Copyright (C) 2018 Sébastien Villemot
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

(defsystem "cl-cffi-gtk-test"
  :depends-on ("cl-cffi-gtk" "fiveam")
  :components ((:file "gtk-testsuite")
               (:file "rtest-glib")
               (:file "rtest-glib-main-loop")
               (:file "rtest-glib-misc")
               (:file "rtest-glib-option-group")
               (:file "rtest-glib-quark")
               (:file "rtest-glib-stable-pointer")
               (:file "rtest-glib-utils")
               (:file "rtest-glib-variant")
               (:file "rtest-glib-variant-type")
               (:file "rtest-glib-version")
               (:file "rtest-gobject")
               (:file "rtest-gobject-base")
               (:file "rtest-gobject-enumeration")
               (:file "rtest-gobject-gc")
               (:file "rtest-gobject-g-value")
               (:file "rtest-gobject-param")
               (:file "rtest-gobject-signals")
               (:file "rtest-gobject-type-info")
               (:file "rtest-gobject-utils")
               (:file "rtest-gdk")
               (:file "rtest-gdk-color")
               (:file "rtest-gdk-display-manager")
               (:file "rtest-gdk-events")
               (:file "rtest-gdk-general")
               (:file "rtest-gdk-key-values")
               (:file "rtest-gdk-pixbuf")
               (:file "rtest-gdk-seat")
               (:file "rtest-gdk-selections")
               (:file "rtest-gdk-visual")
               (:file "rtest-gtk")
               (:file "rtest-gtk-accel-group")
               (:file "rtest-gtk-accel-map")
               (:file "rtest-gtk-action")
               (:file "rtest-gtk-action-group")
               (:file "rtest-gtk-app-chooser")
               (:file "rtest-gtk-container")
               (:file "rtest-gtk-dialog")
               (:file "rtest-gtk-entry-buffer")
               (:file "rtest-gtk-frame")
               (:file "rtest-gtk-list-store")
               (:file "rtest-gtk-main-loop")
               (:file "rtest-gtk-paper-size")
               (:file "rtest-gtk-print-settings")
               (:file "rtest-gtk-selections")
               (:file "rtest-gtk-text-buffer")
               (:file "rtest-gtk-text-iter")
               (:file "rtest-gtk-tool-palette")
               (:file "rtest-gtk-widget")
               (:file "rtest-gio")
               (:file "rtest-gio-action")
               (:file "rtest-gio-content-type")
               (:file "rtest-gio-icon")
               (:file "rtest-gio-simple-action-group")
               (:file "rtest-gio-simple-action")
               (:file "rtest-gio-themed-icon")
               (:file "rtest-pango")
               (:file "rtest-cairo")
               (:file "rtest-cairo-context")
               (:file "rtest-cairo-image-surface"))
  :perform (asdf:test-op (op c) (uiop:symbol-call :gtk-testsuite :run! (uiop:find-symbol* :gtk-testsuite :gtk-testsuite))))
