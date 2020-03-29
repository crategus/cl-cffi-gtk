;;; ----------------------------------------------------------------------------
;;; glib.stable-pointer.lisp
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

(in-package :glib)

;; Allocates the stable pointer for thing. Stable pointer is an integer that
;; can be dereferenced with get-stable-pointer-value and freed with
;; free-stable-pointer. Stable pointers are used to pass references to lisp
;; objects to foreign code. thing is any object. The return value is an integer.

(let ((stable-pointers (make-array 0 :adjustable t :fill-pointer t))
      (sp-mutex (bt:make-lock "stable-pointers lock")))

  (defun allocate-stable-pointer (thing)
    (flet ((find-fresh-id ()
             (or ;; Search a free place for the pointer
                 (position nil stable-pointers)
                 ;; Add a place for the pointer
                 (vector-push-extend nil stable-pointers))))
      (bt:with-lock-held (sp-mutex)
        (let ((id (find-fresh-id)))
          (setf (aref stable-pointers id) thing)
          (make-pointer id)))))

  ;; Frees the stable pointer previously allocated by allocate-stable-pointer

  (defun free-stable-pointer (stable-pointer)
    (bt:with-lock-held (sp-mutex)
      (setf (aref stable-pointers (pointer-address stable-pointer))
            nil)))

  ;; Returns the objects that is referenced by stable pointer previously
  ;; allocated by allocate-stable-pointer. May be called any number of times.

  (defun get-stable-pointer-value (stable-pointer)
    (bt:with-lock-held (sp-mutex)
      (let ((ptr-id (pointer-address stable-pointer)))
        (when (<= 0 ptr-id (1- (length stable-pointers)))
          (aref stable-pointers ptr-id)))))

  (defun set-stable-pointer-value (stable-pointer data)
    (bt:with-lock-held (sp-mutex)
      (let ((ptr-id (pointer-address stable-pointer)))
        (when (<= 0 ptr-id (1- (length stable-pointers)))
          (setf (aref stable-pointers ptr-id) data)))))
)

;; Executes body with ptr bound to the stable pointer to result of evaluating
;; expr. ptr is a symbol naming the variable which will hold the stable pointer
;; value and expr is an expression

(defmacro with-stable-pointer ((ptr expr) &body body)
  `(let ((,ptr (glib::allocate-stable-pointer ,expr)))
     (unwind-protect
         (progn ,@body)
       (free-stable-pointer ,ptr))))

;; Callback function to free a pointer

(defcallback stable-pointer-destroy-notify-cb :void ((data :pointer))
  (free-stable-pointer data))

;;; --- End of file glib.stable-pointer.lisp -----------------------------------
