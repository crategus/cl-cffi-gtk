;;; ----------------------------------------------------------------------------
;;; rtest-glib.lisp
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

(defpackage :glib-tests
  (:use :glib :cffi :common-lisp :lisp-unit))

(in-package :glib-tests)

;;; ----------------------------------------------------------------------------

(define-test glib-stable-pointer
  (let* ((func (lambda() 88))
         (ptr (glib::allocate-stable-pointer func)))
    (assert-eql 88 (funcall (glib::get-stable-pointer-value ptr)))
    (assert-false (glib::free-stable-pointer ptr))
    (assert-false (glib::get-stable-pointer-value ptr)))
  (let ((func (lambda () 99)))
    (with-stable-pointer (ptr func)
      (assert-eql 99 (funcall (glib::get-stable-pointer-value ptr))))))

;;; ----------------------------------------------------------------------------

#+sbcl
(define-test glib-version
  (assert-equal    2 glib-major-version)
  (assert-equal   34 glib-minor-version)
  (assert-equal    1 glib-micro-version)
  (assert-equal 3401 glib-binary-age)
  (assert-equal    1 glib-interface-age)
  (assert-false (glib-check-version 2 24 0))
  (assert-equal "GLib version too old (micro mismatch)"
                (glib-check-version 2 36 0)))

#+windows
(define-test glib-version
  (assert-equal    2 glib-major-version)
  (assert-equal   32 glib-minor-version)
  (assert-equal    3 glib-micro-version)
  (assert-equal 3203 glib-binary-age)
  (assert-equal    3 glib-interface-age)
  (assert-false (glib-check-version 2 24 0))
  (assert-equal "GLib version too old (micro mismatch)"
                (glib-check-version 2 36 0)))

;;; ----------------------------------------------------------------------------

(define-test glib-misc
  ;; Check the types g-size, g-ssize, and g-offset
  (assert-eq :unsigned-long (cffi::canonicalize-foreign-type 'g-size))
  (assert-eq :long (cffi::canonicalize-foreign-type 'g-ssize))
  (assert-eq :unsigned-long-long (cffi::canonicalize-foreign-type 'g-offset))

  ;; g-malloc and g-free
  (let ((mem nil))
    (assert-true (pointerp (setq mem (g-malloc 10))))
    (g-free mem)
    (assert-true (null-pointer-p (g-malloc 0))))

  ;; g-time-val CStruct
  (assert-eq :pointer (cffi::canonicalize-foreign-type 'g-time-val))
  (with-foreign-object (ptr 'g-time-val)
    ;; Write values into the slots.
    ;; The CStruct is exported, but not the slots.
    (setf (foreign-slot-value ptr 'g-time-val 'glib::tv-sec) 412776
          (foreign-slot-value ptr 'g-time-val 'glib::tv-usec) 132423)
    ;; Read the slots.
    (assert-equal
      (list 412776 132423)
      (with-foreign-slots ((glib::tv-sec glib::tv-usec) ptr g-time-val)
        (list glib::tv-sec glib::tv-usec))))

  ;; g-get-current-time returns a g-time-val CStruct
  (with-foreign-object (result 'g-time-val)
    (setf result (g-get-current-time))
    (with-foreign-slots ((glib::tv-sec glib::tv-usec) result g-time-val)
      (assert-true (integerp glib::tv-sec))
      (assert-true (integerp glib::tv-usec))))

  ;; g-get-monotonic-time
  (assert-true (integerp (g-get-monotonic-time)))

  ;; g-get-real-time
  (assert-true (integerp (g-get-real-time)))

  ;; g-string
  (assert-eq :pointer (cffi::canonicalize-foreign-type 'g-string))
  (let ((ptr (convert-to-foreign "Hello" 'g-string)))
    (assert-true (pointerp ptr))
    (assert-equal "Hello" (convert-from-foreign ptr 'g-string)))

  ;; g-strv
  (assert-eq :pointer (cffi::canonicalize-foreign-type 'g-strv))
  (let ((ptr (convert-to-foreign (list "Hello" "World") 'g-strv)))
    (assert-true (pointerp ptr))
    (assert-equal '("Hello" "World") (convert-from-foreign ptr 'g-strv))))

;;; ----------------------------------------------------------------------------

  (defvar *main-loop* nil)
  (defvar *main-thread* nil)
  (defvar *main-thread-level* nil)
  (defvar *main-thread-lock* (bt:make-lock "*main-thread* lock"))

(define-test glib-main-loop
  (bt:with-lock-held (*main-thread-lock*)
    (when (and *main-thread* (not (bt:thread-alive-p *main-thread*)))
      (setf *main-thread* nil))
    (unless *main-thread*
      (setf *main-thread*
            (bt:make-thread (lambda ()
                              ;; instead of gtk-main
                              (setf *main-loop*
                                    (g-main-loop-new (null-pointer) nil))
                              (g-main-loop-run *main-loop*))
                            :name "rtest-glib-thread")
            *main-thread-level* 0))
    (incf *main-thread-level*))

    (sleep 1)
    (assert-true (bt:thread-alive-p *main-thread*))
    (assert-eql 1 *main-thread-level*)
    (assert-true (pointerp *main-loop*))
    (assert-true (g-main-loop-is-running *main-loop*))
    (assert-eql 0 (g-main-depth))
    (assert-true (pointer-eq (g-main-context-default)
                             (g-main-loop-get-context *main-loop*)))
    (assert-false (g-main-context-is-owner (g-main-context-default)))

    (bt:with-lock-held (*main-thread-lock*)
      (decf *main-thread-level*)
      (when (zerop *main-thread-level*)
        ;; instead of gtk-main-quit
        (g-main-loop-quit *main-loop*)))

    (sleep 1)
    (assert-false (bt:thread-alive-p *main-thread*))
    (assert-eql 0 *main-thread-level*)
    (assert-true (pointerp *main-loop*))
    (assert-false (g-main-loop-is-running *main-loop*)))

;;; ----------------------------------------------------------------------------

#+nil
(define-test glib-threads
  (let* ((func (lambda () (sleep 2)))
         (start (get-universal-time))
         (thread nil))
  (assert-true (pointerp (setq thread (g-thread-new "GTK+" func))))
  (assert-true (pointerp (g-thread-self)))
  (g-thread-join thread)
  (assert-eql 2 (- (get-universal-time) start))))

;;; ----------------------------------------------------------------------------

(defvar *started* nil)

#+nil
(define-test glib-utils
  (when (not *started*)
    (setq *started* t)
    (g-set-application-name "GTK Application")
    (g-set-prgname "GTK Program"))
  (assert-equal "GTK Application" (g-get-application-name))
  (assert-equal "GTK Program" (g-get-prgname)))

;;; ----------------------------------------------------------------------------

(format t "~&-----------------------------------------------------------------")
(run-all-tests :glib-tests)

;;; --- End of file rtest-glib.lisp --------------------------------------------
