;;; ----------------------------------------------------------------------------
;;; gtk-child-properties.lisp
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

(defun container-child-property-info (type property-name)
  (let ((class (g-type-class-ref type)))
    (unwind-protect
         (let ((g-param-spec (gtk-container-class-find-child-property class property-name)))
           (parse-g-param-spec g-param-spec))
      (g-type-class-unref class))))

(export 'container-child-property-info)

(defun container-call-get-property (container child property-name type)
  (with-foreign-object (gvalue 'g-value)
    (g-value-zero gvalue)
    (g-value-init gvalue (gtype type))
    (%gtk-container-child-get-property container child property-name gvalue)
    (prog1 (parse-g-value gvalue)
      (g-value-unset gvalue))))

(defun container-call-set-property (container child property-name new-value type)
  (with-foreign-object (gvalue 'g-value)
    (set-g-value gvalue new-value (gtype type) :zero-g-value t)
    (%gtk-container-child-set-property container child property-name gvalue)
    (g-value-unset gvalue)
    (values)))

(export '(container-call-get-property container-call-set-property))

(defmacro define-child-property (container-type
                                  property-name property-gname
                                  property-type readable writable export)
  (when (stringp container-type)
    (setf container-type (registered-object-type-by-name container-type)))
  `(progn
     ,@(when readable
             (list `(defun ,property-name (container child)
                      (assert (typep container ',container-type))
                      (container-call-get-property container
                                                   child
                                                   ,property-gname
                                                   ,property-type))))
     ,@(when writable
             (list `(defun (setf ,property-name) (new-value container child)
                      (assert (typep container ',container-type))
                      (container-call-set-property container
                                                   child
                                                   ,property-gname
                                                   new-value
                                                   ,property-type))))
     ,@(when export
             (list `(export ',property-name)))))

(defun container-class-child-properties (type)
  (setf type (gtype type))
  (let ((class (g-type-class-ref type)))
    (unwind-protect
      (with-foreign-object (n-props :uint)
        (let ((params (gtk-container-class-list-child-properties class n-props)))
          (unwind-protect
            (loop
              for i from 0 below (mem-ref n-props :uint)
              for param = (mem-aref params :pointer i)
              collect (parse-g-param-spec param))
            (g-free params))))
      (g-type-class-unref class))))

(defun child-property-name (type-name property-name package-name)
  (intern (format nil "~A-CHILD-~A"
                  (symbol-name (registered-object-type-by-name type-name))
                  (string-upcase property-name))
          (find-package package-name)))

(defun generate-child-properties (&optional (type-root "GtkContainer") (package-name "GTK"))
  (setf type-root (gtype type-root))
  (append (loop
             for property in (container-class-child-properties type-root)
             collect
               `(define-child-property
                    ,(gtype-name type-root)
                    ,(child-property-name (gtype-name type-root)
                                          (param-spec-name property)
                                          package-name)
                  ,(param-spec-name property)
                  ,(gtype-name (param-spec-type property))
                  ,(param-spec-readable property)
                  ,(param-spec-writable property)
                  t))
          (loop
             for subclass in (g-type-children type-root)
           appending (generate-child-properties subclass package-name))))

