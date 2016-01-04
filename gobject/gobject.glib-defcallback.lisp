;;; ----------------------------------------------------------------------------
;;; gobject.glib-defcallback.lisp
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

(in-package :gobject)

(defun wrap-body-with-boxed-translations (args body)
  (if (null args)
      body
      (let ((arg (first args)))
        (destructuring-bind (arg-name arg-type) arg
          (if (and (listp arg-type)
                   (eq 'g-boxed-foreign (first arg-type)))
              (let ((var (gensym))
                    (cffi-type (cffi::parse-type arg-type)))
                `((let ((,var ,arg-name)
                        (,arg-name (translate-from-foreign ,arg-name
                                                           ,cffi-type)))
                    (unwind-protect
                      (progn
                        ,@(wrap-body-with-boxed-translations (rest args) body))
                      (cleanup-translated-object-for-callback ,cffi-type
                                                              ,arg-name
                                                              ,var)))))
              (wrap-body-with-boxed-translations (rest args) body))))))

(defmacro glib-defcallback (name-and-options return-type args &body body)
  (let* ((c-args (iter (for arg in args)
                       (for (name type) = arg)
                       (if (and (listp type)
                                (eq 'g-boxed-foreign (first type)))
                           (collect `(,name :pointer))
                           (collect arg))))
         (c-body (wrap-body-with-boxed-translations args body)))
    `(defcallback ,name-and-options ,return-type ,c-args ,@c-body)))

;;; --- End of file gobject.glib-defcallback.lisp ------------------------------
