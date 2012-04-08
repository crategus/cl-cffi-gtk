;;; ----------------------------------------------------------------------------
;;; gtk.misc-lisp.lisp
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

(in-package :gtk)

(defcallback stable-pointer-free-destroy-notify-cb :void ((data :pointer))
  (free-stable-pointer data))

(defcallback call-from-main-loop-callback :boolean
    ((data :pointer))
  (restart-case
      (progn (funcall (get-stable-pointer-value data))
             nil)
    (return-from-callback () nil)))

(defun call-from-gtk-main-loop (func &key (priority +g-priority-default-idle+))
  (g-idle-add-full priority
                   (callback call-from-main-loop-callback)
                   (allocate-stable-pointer func)
                   (callback stable-pointer-free-destroy-notify-cb))
  (ensure-gtk-main))

(export 'call-from-gtk-main-loop)

(defcallback call-timeout-from-main-loop-callback :boolean
    ((data :pointer))
  (restart-case
      (progn (funcall (get-stable-pointer-value data)))
    (return-from-callback () nil)))

(defun gtk-main-add-timeout (millisec func &key (priority +g-priority-default+))
  (g-timeout-add-full priority
                      millisec
                      (callback call-timeout-from-main-loop-callback)
                      (allocate-stable-pointer func)
                      (callback stable-pointer-free-destroy-notify-cb)))

(export 'gtk-main-add-timeout)

(defmacro within-main-loop (&body body)
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

(export 'with-main-loop)

;;; --- End of file gtk.misc-lisp.lisp -----------------------------------------
