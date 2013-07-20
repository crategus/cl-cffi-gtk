;;; ----------------------------------------------------------------------------
;;; cl-cffi-gtk-glib.asd
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See <http://common-lisp.net/project/cl-gtk2/>.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2013 Dieter Kaiser
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

(defsystem :cl-cffi-gtk-glib
  :name :cl-cffi-gtk-glib
  :version "2.36.0"                          ; Version of the GLib Library
  :author "Dieter Kaiser"
  :license "LLGPL"
  :serial t
  :components ((:file "glib.package")
               (:file "glib.init")           ; Lisp Initialization
               (:file "glib.stable-pointer") ; Stable Pointers for callbacks

               (:file "glib.version")        ; Glib Version information
               (:file "glib.misc")           ; Various Glib Types and Functions
               (:file "glib.threads")        ; Thread abstraction
               (:file "glib.main-loop")      ; The Main Event Loop
               (:file "glib.quark")          ; Association between string and id
               (:file "glib.error")          ; Error Reporting
               (:file "glib.utils")          ; Miscellaneous Utility Functions
               (:file "glib.option-group")   ; Parses commandline options)
              )
  :depends-on (:cffi
               :iterate
               :trivial-features))

;;; --- End of file cl-cffi-gtk-glib.asd ---------------------------------------
