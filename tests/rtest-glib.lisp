;;; ----------------------------------------------------------------------------
;;; rtest-glib.lisp
;;;
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

(in-package :glib-tests)

(defvar *main-thread* nil)
(defvar *main-thread-level* nil)
(defvar *main-thread-lock* (bt:make-lock "*main-thread* lock"))

(defvar *main-loop* (g-main-loop-new (null-pointer) nil))

(at-finalize ()
  (when (and *main-thread*
             (bt:thread-alive-p *main-thread*))
    (bt:destroy-thread *main-thread*)
    (setf *main-thread* nil)))

(defun ensure-main-thread ()
  (bt:with-lock-held (*main-thread-lock*)
    (when (and *main-thread*
               (not (bt:thread-alive-p *main-thread*)))
      (setf *main-thread* nil))
    (unless *main-thread*
      (setf *main-thread*
            (bt:make-thread (lambda ()
                              (setq *main-loop*
                                    (g-main-loop-new (null-pointer) nil))
                              (g-main-loop-run *main-loop*))
                            :name "cl-gtk main thread")
            *main-thread-level* 0))
    (incf *main-thread-level*))
  (values))

;;; ----------------------------------------------------------------------------

(define-test glib-misc
  (assert-eq :unsigned-long (cffi::canonicalize-foreign-type 'g-size))
  (assert-eq :long (cffi::canonicalize-foreign-type 'g-ssize))
  (assert-eq :UNSIGNED-LONG-LONG (cffi::canonicalize-foreign-type 'g-offset)))

(define-test glib-version
  (assert-equal    2 *glib-major-version*)
  (assert-equal   24 *glib-minor-version*)
  (assert-equal    1 *glib-micro-version*)
  (assert-equal 2401 *glib-binary-age*)
  (assert-equal    1 *glib-interface-age*)
  (assert-false (glib-check-version 2 0 0))
  (assert-equal "GLib version too old (micro mismatch)"
                (glib-check-version 2 28 0)))

(define-test glib-threads
  (assert-true (g-thread-get-initialized))
  (assert-true (g-thread-self)))

(defvar *started* nil)

(define-test glib-utils
  (when (not *started*)
    (setq *started* t)
    (g-set-application-name "GTK Application")
    (g-set-prgname "GTK Program"))
  (assert-equal "GTK Application" (g-get-application-name))
  (assert-equal "GTK Program" (g-get-prgname)))

(define-test glib-main-loop
  (ensure-main-thread)
  (do ()
      ((and *main-loop*
            (g-main-loop-is-running *main-loop*))))
  (assert-true (bt:thread-alive-p *main-thread*))
  (assert-true (g-main-loop-is-running *main-loop*)))

;;; --- End of file rtest-glib.lisp --------------------------------------------
