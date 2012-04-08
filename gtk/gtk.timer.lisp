;;; ----------------------------------------------------------------------------
;;; gtk.timer.lisp
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

(in-package :gtk)

(defclass timer ()
  ((fn :initform nil :initarg :fn :accessor timer-fn)
   (interval-msec :initform 100 :initarg :interval-msec :accessor timer-interval-msec)
   (source-id :initform nil)))

(defun timer-enabled-p (timer)
  (not (null (slot-value timer 'source-id))))

(defun (setf timer-enabled-p) (new-value timer)
  (unless (eq new-value (timer-enabled-p timer))
    (if new-value
        (start-timer timer)
        (stop-timer timer))))

(defmethod (setf timer-interval-msec) :after (new-value (timer timer))
  (when (timer-enabled-p timer)
    (stop-timer timer)
    (start-timer timer)))

(defun start-timer (timer)
  (unless (slot-value timer 'source-id)
    (setf (slot-value timer 'source-id)
          (gtk-main-add-timeout (timer-interval-msec timer) (lambda () (funcall (timer-fn timer)) t)))))

(defun stop-timer (timer)
  (when (slot-value timer 'source-id)
    (glib:g-source-remove (slot-value timer 'source-id))
    (setf (slot-value timer 'source-id) nil)))

(export '(timer timer-fn timer-interval-msec timer-enabled-p timer-interval-msec start-timer stop-timer))
