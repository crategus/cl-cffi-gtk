;;; ----------------------------------------------------------------------------
;;; cl-cffi-gtk-gio.asd
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See http://common-lisp.net/project/cl-gtk2/
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

(defsystem :cl-cffi-gtk-gio
  :name :cl-cffi-gtk-gio
  :version "2.32.3"                  ; Version of GIO
  :author "Dieter Kaiser"
  :license "LLGPL"
  :serial t
  :components
  ((:file "gio.package")
   (:file "gio.init")
   (:file "gio.action")              ; An action interface
   (:file "gio.action-group")        ; A group of actions
   (:file "gio.action-map")          ; Interface for action containers
   (:file "gio.simple-action")       ; Simple GAction implementation
   (:file "gio.simple-action-group") ; Simple GActionGroup implementation
   (:file "gio.application")         ; Core application class
  )
  :depends-on (:cl-cffi-gtk-glib
               :cl-cffi-gtk-gobject))

;;; --- End of file cl-cffi-gtk-gio.asd ----------------------------------------
