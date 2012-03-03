;;; ----------------------------------------------------------------------------
;;; gtk.high-level.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See http://common-lisp.net/project/cl-gtk2/
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2012 Dieter Kaiser
;;;.
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

(define-condition gtk-call-aborted (error)
  ((condition :initarg :condition :reader gtk-call-aborted-condition))
  (:report (lambda (c stream)
             (format stream
                     "Call within main loop aborted because of error:~%~A"
                     (gtk-call-aborted-condition c)))))

(defun call-within-main-loop-and-wait (fn)
  (let ((lock (bt:make-lock))
        (cv (bt:make-condition-variable))
        error
        result)
    (bt:with-lock-held (lock)
      (within-main-loop
        (handler-case
            (setf result (multiple-value-list (funcall fn)))
          (error (e) (setf error e)))
        (bt:with-lock-held (lock)
          (bt:condition-notify cv)))
      (bt:condition-wait cv lock)
      (if error
          (error 'gtk-call-aborted :condition error)
          (values-list result)))))

(export 'call-within-main-loop-and-wait)

(defmacro within-main-loop-and-wait (&body body)
  `(call-within-main-loop-and-wait (lambda () ,@body)))

(export 'within-main-loop-and-wait)

(defstruct progress-display
  parent
  name
  count
  bar
  time-started
  current)

(export 'progress-display)
(export 'progress-display-parent)
(export 'progress-display-name)
(export 'progress-display-count)
(export 'progress-display-bar)
(export 'progress-display-time-started)
(export 'progress-display-current)

(defstruct (progress-window (:include progress-display))
  window
  box)

(export 'progress-window)
(export 'progress-window-window)
(export 'progress-window-box)

(defun create-progress-window (name count)
  (within-main-loop-and-wait
    (let* ((window (make-instance 'gtk-window
                                  :type :toplevel
                                  :title name
                                  :window-position :center))
           (box (make-instance 'gtk-vbox))
           (bar (make-instance 'progress-bar :text name)))
      (gtk-container-add window box)
      (gtk-box-pack-start box bar :expand nil)
      (gtk-widget-show window)
      (make-progress-window :parent nil
                            :name name
                            :count count
                            :bar bar
                            :window window
                            :box box
                            :time-started (get-internal-real-time)
                            :current 0))))

(defun progress-display-root (progress)
  (if (progress-display-parent progress)
      (progress-display-root (progress-display-parent progress))
      progress))

(defun create-progress-bar (parent name count)
  (assert name) (assert count)
  (if parent
      (within-main-loop-and-wait
        (let* ((root (progress-display-root parent))
               (bar (make-instance 'progress-bar :text name)))
          (gtk-box-pack-start (progress-window-box root) bar :expand nil)
          (gtk-widget-show bar)
          (make-progress-display :parent parent
                                 :name name
                                 :count count
                                 :bar bar
                                 :time-started (get-internal-real-time)
                                 :current 0)))
      (create-progress-window name count)))

(export 'create-progress-window)

(defgeneric delete-progress-bar (bar))

(export 'delete-progress-bar)

(defmethod delete-progress-bar ((bar progress-window))
  (within-main-loop-and-wait (gtk-widget-destroy (progress-window-window bar))))

(defmethod delete-progress-bar ((bar progress-display))
  (let ((root (progress-display-root bar)))
    (within-main-loop-and-wait
      (gtk-container-remove (progress-window-box root)
                            (progress-display-bar bar)))))

(defun format-duration (stream seconds colon-modifier-p at-sign-modifier-p)
  (declare (ignore colon-modifier-p at-sign-modifier-p))
  (let ((seconds (mod (truncate seconds) 60))
        (minutes (mod (truncate seconds 60) 60))
        (hours (truncate seconds 3600)))
    (format stream "~2,'0D:~2,'0D:~2,'0D" hours minutes seconds)))

(defun update-progress-bar-text (bar &optional (lower-frac 0.0))
  (let* ((elapsed (coerce (/ (- (get-internal-real-time)
                                (progress-display-time-started bar))
                             internal-time-units-per-second)
                          'double-float))
         (process-rate (coerce (/ elapsed
                                  (+ lower-frac
                                     (progress-display-current bar)))
                               'double-float))
         (total-time (coerce (* (progress-display-count bar) process-rate)
                             'double-float)))
    (setf (gtk-progress-bar-text (progress-display-bar bar))
          (format nil
                  "~A (~/gtk::format-duration/; ETA ~/gtk::format-duration/)"
                  (progress-display-name bar) elapsed total-time))))

(defun update-progress-bar-texts (bar &optional (lower-frac 0.0))
  (when bar
    (update-progress-bar-text bar lower-frac)
    (update-progress-bar-texts (progress-display-parent bar)
                               (coerce (/ (progress-display-current bar)
                                          (progress-display-count bar))
                                       'double-float))))

(defun tick-progress-bar (bar)
  (when bar
    (within-main-loop-and-wait
      (incf (gtk-progress-bar-fraction (progress-display-bar bar))
            (coerce (/ (progress-display-count bar)) 'double-float))
      (incf (progress-display-current bar))
      (update-progress-bar-text bar))))

(export 'tick-progress-bar)

(defvar *current-progress-bar* nil)

(defmacro with-progress-bar ((name count) &body body)
  (let ((bar (gensym)))
    `(let* ((,bar (create-progress-bar *current-progress-bar* ,name ,count))
            (*current-progress-bar* ,bar))
       (unwind-protect
            (progn ,@body)
         (delete-progress-bar ,bar)))))

(export 'with-progress-bar)

(defmacro with-progress-bar-action (&body body)
  `(multiple-value-prog1 (progn ,@body)
     (tick-progress-bar *current-progress-bar*)))

(export 'with-progress-bar-action)

(defun test-progress ()
  (with-progress-bar ("Snowball" 10)
    (loop
       repeat 10
       do (with-progress-bar-action
            (with-progress-bar ("Texts" 10)
              (loop
                 repeat 10
                 do (with-progress-bar-action (sleep 1))))))))

;;; --- End of file gtk.high-level.lisp ----------------------------------------
