;;; ----------------------------------------------------------------------------
;;; glib.package.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See http://common-lisp.net/project/cl-gtk2/
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2012 Dieter Kaiser
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

(defpackage :glib
  (:use :cl :cffi :iter)
  (:export ;; Symbols from glib.init.lisp
           #:at-init
           #:at-finalize
           
           ;; Symbols from glib.stable-pointer.lisp
           #:allocate-stable-pointer
           #:free-stable-pointer
           #:get-stable-pointer-value
           #:with-stable-pointer
           #:stable-pointer-destroy-notify-cb
           
           ;; Symbols from glib.version.lisp
           #:push-library-version-features
           #:require-library-version
           
           ;; Symbols from glib.error.lisp
           #:with-catching-to-g-error
           #:with-g-error))

;;; --- End of file glib.package.lisp ------------------------------------------