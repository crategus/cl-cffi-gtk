;;; ----------------------------------------------------------------------------
;;; cl-gtk-cairo.asd
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

(defsystem :cl-gtk-cairo
  :name :cl-gtk-cairo
  :version "0.0.0"
  :author  "Dieter Kaiser"
  :license "LLGPL"
  :serial t
  :components ((:file "cairo.package")
               (:file "cairo.init")
               (:file "cairo.regions")
;               (:file "cairo")
;               (:file "cairo.demo")
              )
  :depends-on (:cl-gtk-glib
               :cl-gtk-gobject
               :cffi
               :iterate))

;;; --- End of file cl-gtk-cairo.asd -------------------------------------------
