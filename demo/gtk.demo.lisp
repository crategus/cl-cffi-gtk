;;; ----------------------------------------------------------------------------
;;; gtk.demo.lisp
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2012 Dr. Dieter Kaiser
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See http://common-lisp.net/project/cl-gtk2/
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

(asdf:operate 'asdf:load-op :cl-gtk-gtk)

(defpackage :gtk-demo
  (:use :cl :gtk :gdk :gobject :iter)
  (:export #:demo))

(in-package :gtk-demo)

;;; ----------------------------------------------------------------------------

;; Test various gdk primitives

(defun gdk-expose (window)
  (let* ((gc (gdk-gc-new window)))
    (multiple-value-bind (w h) (gdk-drawable-get-size window)
      (gdk-gc-set-rgb-bg-color gc (make-gdk-color :red 0 :green 0 :blue 0))
      (gdk-draw-polygon window
                        gc
                        t
                        (list (make-gdk-point :x 0 :y 0)
                              (make-gdk-point :x (truncate w 2) :y 0)
                              (make-gdk-point :x w :y (truncate h 2))
                              (make-gdk-point :x w :y h)
                              (make-gdk-point :x (truncate w 2) :y h)
                              (make-gdk-point :x 0 :y (truncate h 2))))
      (gdk-gc-set-rgb-fg-color gc (make-gdk-color :red 65535 :green 0 :blue 0))
      (gdk-draw-point window gc 20 10)
      (gdk-gc-set-rgb-fg-color gc (make-gdk-color :red 0 :green 65535 :blue 0))
      (gdk-draw-points window
                       gc
                       (list (make-gdk-point :x 15 :y 20)
                             (make-gdk-point :x 35 :y 40)))
      (gdk-gc-set-rgb-fg-color gc (make-gdk-color :red 0 :green 0 :blue 65535))
      (gdk-draw-line window gc 60 30 40 50)
      (gdk-gc-set-rgb-fg-color gc (make-gdk-color :red 65535
                                                  :green 65535
                                                  :blue 0))
      (gdk-draw-lines window
                  gc
                  (list (make-gdk-point :x 10 :y 30)
                        (make-gdk-point :x 15 :y 40)
                        (make-gdk-point :x 15 :y 50)
                        (make-gdk-point :x 10 :y 56)))
      (gdk-gc-set-rgb-fg-color gc (make-gdk-color :red 0
                                                  :green 65535
                                                  :blue 65535))
      (gdk-draw-segments window
                         gc
                         (list (make-gdk-segment :x1 35 :y1 35 :x2 55 :y2 35)
                               (make-gdk-segment :x1 65 :y1 35 :x2 43 :y2 17)))
      (gdk-gc-set-rgb-fg-color gc (make-gdk-color :red 65535
                                                  :green 0
                                                  :blue 65535))
      (gdk-gc-set-rgb-bg-color gc (make-gdk-color :red 32767
                                                  :green 0
                                                  :blue 32767))
      (gdk-draw-arc window gc nil 70 30 75 50 (* 64 75) (* 64 200))
      (gdk-draw-polygon window
                        gc
                        nil
                        (list (make-gdk-point :x 20 :y 40)
                              (make-gdk-point :x 30 :y 50)
                              (make-gdk-point :x 40 :y 70)
                              (make-gdk-point :x 30 :y 80)
                              (make-gdk-point :x 10 :y 55)))
      (gdk-gc-set-rgb-fg-color gc (make-gdk-color :red 16384
                                                  :green 16384
                                                  :blue 65535))
      (gdk-draw-trapezoids window
                           gc
                           (list (make-gdk-trapezoid :y1  50.0d0
                                                     :y2 70.0d0
                                                     :x11 30.0d0
                                                     :x12 45.0d0
                                                     :x21 70.0d0
                                                     :x22 50.0d0))))))

(defun demo-gdk ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :title "Demo GDK Primitives"
                                 :type :toplevel
                                 :default-width 400
                                 :default-height 300
                                 :app-paintable t)))
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (gtk-main-quit)))
      (g-signal-connect window "expose-event"
                        (lambda (widget event)
                          (declare (ignore widget event))
                          (gdk-expose (gtk-widget-window window))))
      (g-signal-connect window "configure-event"
                        (lambda (widget event)
                          (declare (ignore widget event))
                          (gtk-widget-queue-draw window)))
      (gtk-widget-show window)
      (push :pointer-motion-mask
            (gdk-window-events (gtk-widget-window window))))))

;;; ----------------------------------------------------------------------------

;; A simple test of 'on-expose' event

(defun demo-expose-event ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :title "Demo 'on-expose' Event"
                                 :type :toplevel
                                 :default-width 400
                                 :default-height 300))
          (area   (make-instance 'gtk-drawing-area))
          (x 0.0)
          (y 0.0))
      (gtk-container-add window area)
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (gtk-main-quit)))
      (g-signal-connect area "motion-notify-event"
                        (lambda (widget event)
                          (declare (ignore widget))
                          (setf x (event-motion-x event)
                                y (event-motion-y event))
                          (gtk-widget-queue-draw window)))
      (g-signal-connect area "expose-event"
                        (lambda (widget event)
                          (declare (ignore widget event))
                          (let* ((window (gtk-widget-window area))
                                 (gc (gdk-gc-new window))
                                 (layout (gtk-widget-create-pango-layout area
                                              (format nil "X: ~F~%Y: ~F" x y))))
                            (gdk-draw-layout window gc 0 0 layout)
                            (gdk-gc-set-rgb-fg-color gc
                                                     (make-gdk-color :red 65535
                                                                     :green 0
                                                                     :blue 0))
                            (multiple-value-bind (x y)
                                (gdk-drawable-get-size window)
                              (gdk-draw-line window gc 0 0 x y)))))
      (g-signal-connect area "realize"
                        (lambda (widget)
                          (declare (ignore widget))
                          (pushnew :pointer-motion-mask
                                   (gdk-window-events
                                     (gtk-widget-window area)))))
      (g-signal-connect area "configure-event"
                        (lambda (widget event)
                          (declare (ignore widget event))
                          (gtk-widget-queue-draw area)))
      (gtk-widget-show window))))

;;; ----------------------------------------------------------------------------

;; Testing GtkTextEntry

(defun demo-entry ()
  (within-main-loop
    (let* ((window        (make-instance 'gtk-window
                                         :type :toplevel
                                         :title "Demo Entry"
                                         :default-width 250
                                         :border-width 10))
           (box           (make-instance 'gtk-v-box))
           (entry         (make-instance 'gtk-entry))
           (button        (make-instance 'gtk-button
                                         :label "OK"))
           (text-buffer   (make-instance 'gtk-text-buffer))
           (text-view     (make-instance 'gtk-text-view
                                         :buffer text-buffer))
           (button-select (make-instance 'gtk-button
                                         :label "Select"))
           (button-insert (make-instance 'gtk-button
                                         :label "Insert"))
           (label         (make-instance 'gtk-label
                                         :use-markup t
                                         :label
                                         "Enter <b>anything</b> you wish:")))
      (gtk-box-pack-start box label :expand nil)
      (gtk-box-pack-start box entry :expand nil)
      (gtk-box-pack-start box button :expand nil)
      (gtk-box-pack-start box button-select :expand nil)
      (gtk-box-pack-start box button-insert :expand nil)
      (let ((w (make-instance 'gtk-scrolled-window)))
        (gtk-box-pack-start box w)
        (gtk-container-add w text-view))
      (gtk-container-add window box)
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (gtk-main-quit)))
      (g-signal-connect window "delete-event"
                        (lambda (widget event)
                          (declare (ignore widget event))
                          (let ((dlg (make-instance 'gtk-message-dialog
                                                    :title "Confirm Exit"
                                                    :text "Are you sure?"
                                                    :buttons :yes-no)))
                            (let ((response (gtk-dialog-run dlg)))
                              (gtk-widget-destroy dlg)
                              (not (eq :yes response))))))
      (g-signal-connect button "clicked"
                        (lambda (button)
                          (declare (ignore button))
                          (setf (gtk-text-buffer-text text-buffer)
                                (format nil "~A~%~A"
                                        (gtk-text-buffer-text text-buffer)
                                        (gtk-entry-text entry))
                                (gtk-entry-text entry)
                                "")))
      (g-signal-connect button-select "clicked"
                        (lambda (button)
                          (declare (ignore button))
                          (gtk-editable-select-region entry 5 10)))
      (g-signal-connect button-insert "clicked"
                        (lambda (button)
                          (declare (ignore button))
                          (gtk-editable-insert-text entry "hello" 2)))
      (gtk-widget-show window))))

;;; ----------------------------------------------------------------------------

;; Simple test of packing widgets into GtkTable

(defun demo-table-packing ()
  (within-main-loop
    (let* ((window   (make-instance 'gtk-window
                                    :type :toplevel
                                    :title "Demo Table packing"
                                    :default-width 300
                                    :border-width 20))
           (table    (make-instance 'gtk-table
                                    :n-rows 2
                                    :n-columns 2
                                    :homogeneous t))
           (button-1 (make-instance 'gtk-button :label "Button 1"))
           (button-2 (make-instance 'gtk-button :label "Button 2"))
           (button-q (make-instance 'gtk-button :label "Quit")))
      (gtk-container-add window table)
      (gtk-table-attach table button-1 0 1 0 1)
      (gtk-table-attach table button-2 1 2 0 1)
      (gtk-table-attach table button-q 0 2 1 2)
      (g-signal-connect window "destroy"
                        (lambda (w)
                          (declare (ignore w))
                          (gtk-main-quit)))
      (g-signal-connect button-q "clicked"
                        (lambda (b)
                          (declare (ignore b))
                          (gtk-widget-destroy window)))
      (gtk-widget-show window))))

;;; ----------------------------------------------------------------------------
