;;; ----------------------------------------------------------------------------
;;; cl-cffi-gtk-init.asd
;;;
;;; Copyright (C) 2019 Dieter Kaiser
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

(defsystem :cl-cffi-gtk-init
  :name :cl-cffi-gtk-init
  :version "0.1.0"
  :author "Dieter Kaiser"
  :license "LLGPL"
  :serial t
  :components (
               (:file "gtk.init-features")           ; Libraray Initialization
              )
  :depends-on (:cl-cffi-gtk-glib-init
               :cl-cffi-gtk-gdk-init
               :cffi
               :iterate
               :trivial-features))

;;; --- End of file cl-cffi-gtk-init.asd ---------------------------------------
