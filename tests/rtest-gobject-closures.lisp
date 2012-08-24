;;; ----------------------------------------------------------------------------
;;; rtest-gobject-closures.lisp
;;;
;;; Copyright (C) 2012 Dieter Kaiser
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

(in-package :gobject-tests)

(define-test g-object-closures
 ;; Create a simple closure and check the slots
 (let* ((closure (g-closure-new-simple (foreign-type-size 'lisp-closure)
                                                          (null-pointer)))
        (parent-instance (foreign-slot-value closure
                                             'lisp-closure
                                             :parent-instance)))
   (assert-true
     (pointerp (foreign-slot-value closure 'lisp-closure :parent-instance)))
   (assert-true
     (null-pointer-p
       (foreign-slot-value closure 'lisp-closure :object)))
   (assert-eql 0
     (foreign-slot-value closure 'lisp-closure :function-id))
   (assert-true
     (integerp
       (foreign-slot-value parent-instance 'g-closure :private-data)))
   (assert-true
     (null-pointer-p
       (foreign-slot-value parent-instance 'g-closure :marshal)))
   (assert-true
     (null-pointer-p
       (foreign-slot-value parent-instance 'g-closure :data)))
   (assert-true
     (null-pointer-p
       (foreign-slot-value parent-instance 'g-closure :notifiers)))
 )
 ;; Create a closure with the Lisp function create-closure
 (let* ((button (make-instance 'gtk-button))
        (func #'(lambda (x) x))
        (closure (gobject::create-closure button func))
        (parent-instance (foreign-slot-value closure
                                             'lisp-closure
                                             :parent-instance)))
   (assert-true
     (pointerp (foreign-slot-value closure 'lisp-closure :parent-instance)))
   (assert-equal "GtkButton"
     (g-type-name
       (g-type-from-instance
         (foreign-slot-value closure 'lisp-closure :object))))
   (assert-eql 0
     (foreign-slot-value closure 'lisp-closure :function-id))
   (assert-true
     (integerp
       (foreign-slot-value parent-instance 'g-closure :private-data)))
   (assert-true
     (pointerp (foreign-slot-value parent-instance 'g-closure :marshal)))
   (assert-true
     (null-pointer-p
       (foreign-slot-value parent-instance 'g-closure :data)))
   (assert-true
     (pointerp (foreign-slot-value parent-instance 'g-closure :notifiers)))


 )
)

