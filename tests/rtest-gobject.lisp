;;; ----------------------------------------------------------------------------
;;; rtest-gobject.lisp
;;;
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

#-lisp-unit
(progn
  (format t "~&Loading of package lisp-unit ...~%")
  (asdf:operate 'asdf:load-op :lisp-unit)
  (format t "Loading of package lisp-unit is finished.~%"))

#-gtk
(progn
  (format t "~&Loading of package cl-cffi-gtk ...~%")
  (asdf:operate 'asdf:load-op :cl-cffi-gtk)
  (format t "Loading is finished.~%"))

(defpackage :gobject-tests
  (:use :gtk :gdk :gobject :glib :cffi :common-lisp :lisp-unit))

(in-package :gobject-tests)

;;; ----------------------------------------------------------------------------

(define-test %g-type-name
  (assert-equal "void"       (g::%g-type-name +g-type-void+))
  (assert-equal "GInterface" (g::%g-type-name +g-type-interface+))
  (assert-equal "gchar"      (g::%g-type-name +g-type-char+))
  (assert-equal "guchar"     (g::%g-type-name +g-type-uchar+))
  (assert-equal "gboolean"   (g::%g-type-name +g-type-boolean+))
  (assert-equal "gint"       (g::%g-type-name +g-type-int+))
  (assert-equal "guint"      (g::%g-type-name +g-type-uint+))
  (assert-equal "glong"      (g::%g-type-name +g-type-long+))
  (assert-equal "gulong"     (g::%g-type-name +g-type-ulong+))
  (assert-equal "gint64"     (g::%g-type-name +g-type-int64+))
  (assert-equal "guint64"    (g::%g-type-name +g-type-uint64+))
  (assert-equal "GEnum"      (g::%g-type-name +g-type-enum+))
  (assert-equal "GFlags"     (g::%g-type-name +g-type-flags+))
  (assert-equal "gfloat"     (g::%g-type-name +g-type-float+))
  (assert-equal "gdouble"    (g::%g-type-name +g-type-double+))
  (assert-equal "gchararray" (g::%g-type-name +g-type-string+))
  (assert-equal "gpointer"   (g::%g-type-name +g-type-pointer+))
  (assert-equal "GBoxed"     (g::%g-type-name +g-type-boxed+))
  (assert-equal "GParam"     (g::%g-type-name +g-type-param+))
  (assert-equal "GObject"    (g::%g-type-name +g-type-object+)))

(define-test g-type-is-value-type
  (assert-false (g-type-is-value-type (gtype +g-type-void+)))
  (assert-false (g-type-is-value-type (gtype +g-type-interface+)))
  (assert-true  (g-type-is-value-type (gtype +g-type-char+)))
  (assert-true  (g-type-is-value-type (gtype +g-type-uchar+)))
  (assert-true  (g-type-is-value-type (gtype +g-type-boolean+)))
  (assert-true  (g-type-is-value-type (gtype +g-type-int+)))
  (assert-true  (g-type-is-value-type (gtype +g-type-uint+)))
  (assert-true  (g-type-is-value-type (gtype +g-type-long+)))
  (assert-true  (g-type-is-value-type (gtype +g-type-ulong+)))
  (assert-true  (g-type-is-value-type (gtype +g-type-int64+)))
  (assert-true  (g-type-is-value-type (gtype +g-type-uint64+)))
  (assert-false (g-type-is-value-type (gtype +g-type-enum+)))
  (assert-false (g-type-is-value-type (gtype +g-type-flags+)))
  (assert-true  (g-type-is-value-type (gtype +g-type-float+)))
  (assert-true  (g-type-is-value-type (gtype +g-type-double+)))
  (assert-true  (g-type-is-value-type (gtype +g-type-string+)))
  (assert-true  (g-type-is-value-type (gtype +g-type-pointer+)))
  (assert-false (g-type-is-value-type (gtype +g-type-boxed+)))
  (assert-true  (g-type-is-value-type (gtype +g-type-param+)))
  (assert-true  (g-type-is-value-type (gtype +g-type-object+))))

(define-test g-type-depth
  ;; The depth of a fundamental type is always 1. The examples show the
  ;; different possibilieties to pass an argument to a foreign function.
  (assert-eql 1 (g-type-depth +g-type-void+))         ; an ID as argument
  (assert-eql 1 (g-type-depth "GInterface"))          ; a string as an argument
  (assert-eql 1 (g-type-depth (gtype +g-type-char+))) ; a GType as an argument
  )

(define-test g-type-is-a
  ;; Again different methods to pass an argument
  (assert-true  (g-type-is-a +g-type-long+ (gtype +g-type-long+)))
  (assert-false (g-type-is-a +g-type-long+ (gtype +g-type-string+)))
  (assert-true  (g-type-is-a "gdouble" +g-type-double+)))

(define-test %g-object
  (let* (;; Create a label as an example for an instance of an object
         (label (make-instance 'gtk-label))
         ;; Get the pointer to the instance of the object
         (ptr (pointer label)))
    ;; Access the slot values of the structure %g-object
    (assert-true  (pointerp (foreign-slot-value ptr '%g-object :type-instance)))
    (assert-eql 1 (foreign-slot-value ptr '%g-object :ref-count))
    (assert-true  (pointerp (foreign-slot-value ptr '%g-object :data)))
    
    ;; Increase and decrease the ref-count with the functions
    ;; g-object-ref and g-object-unref
    (assert-true (pointerp (g-object-ref ptr)))
    (assert-eql 2 (foreign-slot-value ptr '%g-object :ref-count)) 
    (g-object-unref ptr)
    (assert-eql 1 (foreign-slot-value ptr '%g-object :ref-count))
    
    ;; ref-count takes an object or an pointer as an argument and
    ;; access the slot value of the structure %g-object to get the ref-count
    (assert-eql 1 (gobject::ref-count ptr))
    (assert-eql 1 (gobject::ref-count label))
))

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

;;; ----------------------------------------------------------------------------

(load "rtest-gobject-closures.lisp")
(load "rtest-gobject-param-spec.lisp")
(load "rtest-gobject-signals.lisp")
(load "rtest-gobject-type-info.lisp")

;;; ----------------------------------------------------------------------------

(format t "~&-----------------------------------------------------------------")
(run-all-tests :gobject-tests)

;;; --- End of file rtest-gobject.lisp -----------------------------------------
