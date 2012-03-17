;;; ----------------------------------------------------------------------------
;;; gobject.init.lisp
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

(defpackage :cl-gtk2-init
  (:use :cl :glib))

(in-package :cl-gtk2-init)

(at-init ()
 (eval-when (:compile-toplevel :load-toplevel :execute)
   (cffi:define-foreign-library gobject
     ((:and :unix (:not :darwin))
      (:or "libgobject-2.0.so.0" "libgobject-2.0.so"))
     (:darwin (:or "libgobject-2.0.0.dylib" "libgobject-2.0.dylib"))
     (:windows "libgobject-2.0-0.dll")
     (t "libgobject-2.0")))

 (cffi:use-foreign-library gobject))

(in-package :gobject)

(defvar *lisp-name-package* nil)
(defvar *generated-types* nil)

(defvar *gobject-debug* nil)
(defvar *debug-gc* nil)
(defvar *debug-subclass* nil)
(defvar *debug-stream* t)

(defmacro log-for (categories control-string &rest args)
  (let ((vars (iter (for sym in (if (listp categories)
                                    categories
                                    (list categories)))
                    (collect (intern (format nil "*DEBUG-~A*"
                                             (symbol-name sym))
                                     (find-package :gobject))))))
    `(progn
       (when (or ,@vars)
         (format *debug-stream* ,control-string ,@args))
       nil)))

(defmacro ev-case (keyform &body clauses)
  "Macro that is an analogue of CASE except that it evaluates keyforms"
  (let ((value (gensym)))
    `(let ((,value ,keyform))
       (cond
         ,@(loop
              for (key . forms) in clauses
              collect
                (if (eq key t)
                    `(t ,@forms)
                    `((equalp ,key ,value) ,@forms)))))))

(defmacro with-unwind ((var expr unwind-function) &body body)
  `(let ((,var ,expr))
     (unwind-protect (progn ,@body)
       (,unwind-function ,var))))

;;; ----------------------------------------------------------------------------

;; Manually frees the Lisp reference to the object. Probably should not be
;; called. object is an instance of g-object

(defgeneric release (object))

(defmethod release ((object null)))

;; Calls release on all objects in objects.
;; objects is a list of instances of g-object

(defun release* (&rest objects)
  (declare (dynamic-extent objects))
  (loop
     for object in objects
     do (release object)))

(defmacro using ((var &optional (expr var)) &body body)
  `(let ((,var ,expr))
     (unwind-protect
          (progn ,@body)
       (release ,var))))

(defun using-expand (bindings body)
  (if bindings
      (destructuring-bind (var &optional (expr var))
          (ensure-list (first bindings))
       `(let ((,var ,expr))
          (unwind-protect
               ,(using-expand (rest bindings) body)
            (release ,var))))
      `(progn ,@body)))

(defmacro using* ((&rest bindings) &body body)
  (using-expand bindings body))

;;; --- End of file gobject.init.lisp ------------------------------------------
