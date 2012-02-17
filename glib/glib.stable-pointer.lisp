;;; ----------------------------------------------------------------------------
;;; gobject.stable-pointer.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See http://common-lisp.net/project/cl-gtk2/
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2012 Dr. Dieter Kaiser
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

(defvar *registered-stable-pointers*
        (make-array 0 :adjustable t :fill-pointer t))

;; Allocates the stable pointer for thing. Stable pointer is an integer that
;; can be dereferenced with get-stable-pointer-value and freed with
;; free-stable-pointer. Stable pointers are used to pass references to lisp
;; objects to foreign code. thing is any object. The return value is an integer.

(defun allocate-stable-pointer (thing)
  (let ((id (find-fresh-id)))
    (setf (aref *registered-stable-pointers* id) thing)
    (make-pointer id)))

;; Frees the stable pointer previously allocated by allocate-stable-pointer

(defun free-stable-pointer (stable-pointer)
  (setf (aref *registered-stable-pointers* (pointer-address stable-pointer))
        nil))

;; Returns the objects that is referenced by stable pointer previously
;; allocated by allocate-stable-pointer. May be called any number of times.

(defun get-stable-pointer-value (stable-pointer)
  (when (<= 0 (pointer-address stable-pointer)
              (length *registered-stable-pointers*))
    (aref *registered-stable-pointers* (pointer-address stable-pointer))))

(defun set-stable-pointer-value (stable-pointer value)
  (when (<= 0 (pointer-address stable-pointer)
              (length *registered-stable-pointers*))
    (setf (aref *registered-stable-pointers* (pointer-address stable-pointer))
          value)))

(defun stable-pointer-value (stable-pointer)
  (get-stable-pointer-value stable-pointer))

(defun (setf stable-pointer-value) (new-value stable-pointer)
  (set-stable-pointer-value stable-pointer new-value))

(defun find-fresh-id ()
  (or (position nil *registered-stable-pointers*)
      (progn
        (vector-push-extend nil *registered-stable-pointers*)
        (1- (length *registered-stable-pointers*)))))

;; Executes body with ptr bound to the stable pointer to result of evaluating
;; expr. ptr is a symbol naming the variable which will hold the stable pointer
;; value and expr is an expression

(defmacro with-stable-pointer ((ptr expr) &body body)
  `(let ((,ptr (allocate-stable-pointer ,expr)))
     (unwind-protect
          (progn ,@body)
       (free-stable-pointer ,ptr))))

;; Callback function to free a pointer

(defcallback stable-pointer-free-destroy-notify-cb :void ((data :pointer))
  (free-stable-pointer data))

;;; --- End of file gobject.stable-pointer.lisp --------------------------------
