;;; ----------------------------------------------------------------------------
;;; rtest-gobject-base.lisp
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

(define-test %g-object
  (let* (;; Create a label as an example for an instance of an object
         (label (make-instance 'gtk-label))
         ;; Get the pointer to the instance of the object
         (ptr (pointer label)))
    ;; Access the slot values of the structure %g-object
    (assert-true  (pointerp (foreign-slot-value ptr 'gobject::%g-object :type-instance)))
    (assert-eql 1 (foreign-slot-value ptr 'gobject::%g-object :ref-count))
    (assert-true  (pointerp (foreign-slot-value ptr 'gobject::%g-object :data)))
    
    ;; Increase and decrease the ref-count with the functions
    ;; g-object-ref and g-object-unref
    (assert-true (pointerp (g-object-ref ptr)))
    (assert-eql 2 (foreign-slot-value ptr 'gobject::%g-object :ref-count))
    (g-object-unref ptr)
    (assert-eql 1 (foreign-slot-value ptr 'gobject::%g-object :ref-count))
    
    ;; ref-count takes an object or an pointer as an argument and
    ;; access the slot value of the structure %g-object to get the ref-count
    (assert-eql 1 (gobject::ref-count ptr))
    (assert-eql 1 (gobject::ref-count label))))

(define-test g-object-class
  (let ((class nil))
    (setq class (find-class 'g-object))
    (assert-eq 'g-object (class-name class))
    (assert-eq 'standard-class (type-of class))
    (assert-eq (find-class 'standard-class) (class-of class))
    
    (setq class (find-class 'gobject-class))
    (assert-eq 'gobject-class (class-name class))
    (assert-eq 'standard-class (type-of class))
    (assert-eq (find-class 'standard-class) (class-of class))
    
    (setq class (find-class 'g-initially-unowned))
    (assert-eq 'g-initially-unowned (class-name class))
    (assert-eq 'gobject-class (type-of class))
    (assert-eq (find-class 'gobject-class) (class-of class))
  ))

(define-test g-object-base
  (assert-true (g-type-is-object (gtype "GtkButton")))
  (assert-equal "GtkButton" (g-type-name (g-object-type (make-instance 'gtk-button))))
  (assert-equal "GtkButton" (g-object-type-name (make-instance 'gtk-button)))
  (assert-equal "GtkButton" (g-type-name (g-object-class-type (g-type-class-ref (gtype "GtkButton")))))
  (assert-equal "GtkButton" (g-object-class-name (g-type-class-ref (gtype "GtkButton"))))
  ;; g-object-class-find-property
  (let ((param (g-object-class-find-property (g-type-class-ref (gtype "GtkButton")) "label")))
    (assert-true (pointerp (foreign-slot-value param 'g-param-spec :type-instance)))
    (assert-equal "label" (foreign-slot-value param 'g-param-spec :name))
    (assert-equal '(:READABLE :WRITABLE :CONSTRUCT :STATIC-NAME :STATIC-NICK :STATIC-BLURB)
                  (foreign-slot-value param 'g-param-spec :flags))
    (assert-equal "gchararray" (g-type-name (foreign-slot-value param 'g-param-spec :value-type)))
    (assert-equal "GtkButton" (g-type-name (foreign-slot-value param 'g-param-spec :owner-type)))
  )
)

;;; --- End of file rtest-gobject-base.lisp ------------------------------------
