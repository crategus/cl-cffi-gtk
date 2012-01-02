;;; ----------------------------------------------------------------------------
;;; cl-gtk-glib.asd
;;;
;;; Copyright (C) 2009, 2011 Kalyanov Dmitry
;;; Copyright (C) 2011, 2012 Dr. Dieter Kaiser
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See http://common-lisp.net/project/cl-gtk2/
;;; ----------------------------------------------------------------------------
;;;
;;; License
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;;; ----------------------------------------------------------------------------

(defsystem :cl-gtk-glib
  :name :cl-gtk-glib
  :version "0.0.0"
  :author "Dr. Dieter Kaiser"
  :license "LLGPL"
  :serial t
  :components ((:file "glib.package")
               (:file "glib.init")        ; Lisp Initialization

               (:file "glib.misc")        ; Different Glib Types and Functions
               (:file "glib.version")     ; Glib Version information
               (:file "glib.threads")     ; Thread abstraction
               (:file "glib.main-loop")   ; The Main Event Loop
               (:file "glib.quark")       ; Association between string and id
               (:file "glib.error")       ; Error Reporting
               (:file "glib.utils")       ; Miscellaneous Utility Functions
               
               (:file "glib.lisp"))       ; More Lisp support
  :depends-on (:cffi
               :trivial-garbage
               :iterate
               :bordeaux-threads
               :iterate
               :closer-mop))

;;; --- End of file cl-gtk-glib.asd --------------------------------------------
