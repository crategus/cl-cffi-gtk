;;; ----------------------------------------------------------------------------
;;; gobject.object-function.lisp
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2020 Dieter Kaiser
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

(in-package :gobject)

(defcstruct object-func-ref
  (:object :pointer)
  (:fn-id :int))

(defmacro define-cb-methods (name return-type (&rest args))
  (flet ((make-name (control-string)
           (format-symbol (symbol-package name) control-string name)))
    (let ((call-cb (make-name "~A-CB"))
          (destroy-cb (make-name "~A-DESTROY-NOTIFY"))
          (object (gensym "OBJECT"))
          (fn-id (gensym "FN-ID"))
          (fn (gensym "FN"))
          (data (gensym "DATA"))
          (arg-names (mapcar #'first args)))
      `(progn
         (defcallback ,call-cb ,return-type (,@args (,data :pointer))
           (let* ((,object (convert-from-foreign
                               (foreign-slot-value ,data
                                                   '(:struct object-func-ref)
                                                   :object)
                               'g-object))
                  (,fn-id (foreign-slot-value ,data
                                              '(:struct object-func-ref)
                                              :fn-id))
                  (,fn (retrieve-handler-from-object ,object ,fn-id)))
             (funcall ,fn ,@arg-names)))
         (defcallback ,destroy-cb :void ((,data :pointer))
           (let* ((,object (convert-from-foreign
                               (foreign-slot-value ,data
                                                   '(:struct object-func-ref)
                                                   :object)
                               'g-object))
                  (,fn-id (foreign-slot-value ,data
                                              '(:struct object-func-ref)
                                              :fn-id)))
             (delete-handler-from-object ,object ,fn-id))
           (foreign-free ,data))))))

(defun create-fn-ref (object function)
  (let ((ref (foreign-alloc '(:struct object-func-ref)))
        (fn-id (save-handler-to-object object function)))
    (setf (foreign-slot-value ref '(:struct object-func-ref) :object)
          (pointer object)
          (foreign-slot-value ref '(:struct object-func-ref) :fn-id)
          fn-id)
    ref))

;;; --- End of file gobject.object-function.lisp -------------------------------
