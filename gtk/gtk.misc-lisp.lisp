;;; ----------------------------------------------------------------------------
;;; gtk.misc-lisp.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See <http://common-lisp.net/project/cl-gtk2/>.
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

(in-package :gtk)

(defcallback call-from-main-loop-callback :boolean
    ((data :pointer))
  (restart-case
      (progn (funcall (glib::get-stable-pointer-value data))
             nil)
    (return-from-callback () nil)))

(defun call-from-gtk-main-loop (func &key (priority g-priority-default-idle))
  (glib::%g-idle-add-full priority
                          (callback call-from-main-loop-callback)
                          (glib:allocate-stable-pointer func)
                          (callback glib:stable-pointer-destroy-notify-cb))
  (ensure-gtk-main))

(defcallback call-timeout-from-main-loop-callback :boolean
    ((data :pointer))
  (restart-case
      (progn (funcall (glib::get-stable-pointer-value data)))
    (return-from-callback () nil)))

(defmacro within-main-loop (&body body)
 #+cl-cffi-gtk-documentation
 "@version{2013-7-30}
  The macro @sym{within-main-loop} is a wrapper around a GTK+ program. The
  functionality of the macro corresponds to the C functions @code{gtk_init()}
  and @code{gtk_main()} which initialize and start a GTK+ program. Both
  functions have corresponding Lisp functions. The function
  @code{gtk_main()} is exported as the Lisp function @fun{gtk-main}. The
  corresponding Lisp function to @code{gtk_init()} is called internally when
  loading the library, but is not exported.
  @see-function{gtk-main}"
  `(call-from-gtk-main-loop (lambda () ,@body)))

(export 'within-main-loop)

#+thread-support
(defmacro with-main-loop (&body body)
  `(progn
     (ensure-gtk-main)
     (within-main-loop ,@body)))

#-thread-support
(defmacro with-main-loop (&body body)
  `(progn
     ,@body
     (ensure-gtk-main)))

;;; --- End of file gtk.misc-lisp.lisp -----------------------------------------
