;;; ----------------------------------------------------------------------------
;;; tutorial.lisp
;;;
;;; Examples from the offical GTK+ 2.0 Tutorial translated to Lisp
;;;
;;; Copyright (C) 2011 - 2012 Dr. Dieter Kaiser
;;;
;;; ----------------------------------------------------------------------------
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

(defpackage :gtk-tutorial
  (:use :gtk :gobject :common-lisp)
  (:export 
    #:example-1
    #:example-1-1
    #:example-2
    #:example-3
    #:example-4
    ))

(in-package :gtk-tutorial)

;;; Chapter 3. Getting started

(defun example-1 ()
  (within-main-loop
    (let ((window (gtk-window-new :toplevel)))
      (gtk-widget-show window))))

(defun example-1-1 ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :title "Getting started"
                                 :type :toplevel
                                 :window-position :center
                                 :default-width 400
                                 :default-height 300)))
      (gtk-widget-show window))))

;;; Hello World in GTK

(defun example-2 ()
  (within-main-loop
    (let (;; Create a new window.
          (window (make-instance 'gtk-window
                                 :title "Hello World in GTK"
                                 :type :toplevel
                                 :window-position :center
                                 :default-width  400
                                 :default-height 300
                                 :border-width    10))
          ;; Create a new button with a text.
          (button (make-instance 'gtk-button :label "Hello World")))
      (g-signal-connect window "delete-event"
                        (lambda (window event)
                          (declare (ignore window event))
                          (format t "Delete Event Occured.~%")
                          t))
      (g-signal-connect window "destroy-event"
                        (lambda (window event)
                          (declare (ignore window event))
                          (gtk-main-quit)))
      (g-signal-connect button "clicked"
                        (lambda (button)
                          (declare (ignore button))
                          (format t "Hello world.~%")
                          (gtk-widget-destroy window)))
      (gtk-container-add window button)
      (gtk-widget-show button)
      (gtk-widget-show window))))

;;; An Upgraded Hello World

(defun example-3 ()
  (within-main-loop
    (let ((window (gtk-window-new :toplevel))
          (button (gtk-button-new-with-label "Button 1"))
          (box    (gtk-h-box-new nil 0)))
      (gtk-window-set-title window "Hello Buttons")
      (g-signal-connect window "delete_event"
                        (lambda (window event)
                          (declare (ignore window event))
                          (gtk-main-quit)))
      (gtk-container-set-border-width window 10)
      (gtk-container-add window box)
      (g-signal-connect button "clicked"
                        (lambda (button)
                          (declare (ignore button))
                          (format t "Hello again - Button 1 was pressed.~%")))
      (gtk-box-pack-start box button :expand t :fill t :padding 0)
      (gtk-widget-show button)
      (setq button (gtk-button-new-with-label "Button 2"))
      (g-signal-connect button "clicked"
                        (lambda (button)
                          (declare (ignore button))
                          (format t "Hello again - Button 2 was pressed.~%")))
      (gtk-box-pack-start box button :expand t :fill t :padding 0)
      (gtk-widget-show button)
      (gtk-widget-show box)
      (gtk-widget-show window))))

;;; Chapter 5. Packing Widgets

;;; Packing Demonstrations Program

(defun make-box (homogeneous spacing expand fill padding)
  (let ((box (make-instance 'gtk-h-box
                            :homogeneous homogeneous
                            :spacing spacing))
        button)
    (setq button (gtk-button-new-with-label "gtk-box-pack"))
    (gtk-box-pack-start box button :expand expand :fill fill :padding padding)
    (gtk-widget-show button)

    (setq button (gtk-button-new-with-label "(box"))
    (gtk-box-pack-start box button :expand expand :fill fill :padding padding)
    (gtk-widget-show button)
         
    (setq button (gtk-button-new-with-label "button"))
    (gtk-box-pack-start box button :expand expand :fill fill :padding padding)
    (gtk-widget-show button)

    (if expand
        (setq button (gtk-button-new-with-label "TRUE"))
        (setq button (gtk-button-new-with-label "FALSE")))
    (gtk-box-pack-start box button :expand expand :fill fill :padding padding)
    (gtk-widget-show button)

    (if fill
        (setq button (gtk-button-new-with-label "TRUE"))
        (setq button (gtk-button-new-with-label "FALSE")))
    (gtk-box-pack-start box button :expand expand :fill fill :padding padding)
    (gtk-widget-show button)
    
    (setq button (gtk-button-new-with-label (format nil "~A)" padding)))
    (gtk-box-pack-start box button :expand expand :fill fill :padding padding)
    (gtk-widget-show button)
    ;; Return the box.
    box))

(defun example-4 ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :title "Example 4"
                                 :type :toplevel
                                 :border-width 10
                                 :default-height 200
                                 :default-width 300))
          (vbox   (make-instance 'gtk-v-box
                                 :homogeneous nil
                                 :spacing 0))
          label
          button
          separator
          box
          quitbox)
      (g-signal-connect window "delete_event"
                        (lambda (window event)
                          (declare (ignore window event))
                          (gtk-main-quit)))
      ;; Create a new label
      (setq label
            (make-instance 'gtk-label
                           :label "GtkHBox :homogenous nil :spacing 0"))
      ;; Align the label to the left side
      (gtk-misc-set-alignment label 0 0)
      
      ;; Pack the label into the vertical box and show the label
      (gtk-box-pack-start vbox label :expand nil :fill nil :padding 0)
      (gtk-widget-show label)
         
      ;; Call the make-box function
      (setq box (make-box nil 0 nil nil 0))
      (gtk-box-pack-start vbox box :expand nil :fill nil :padding 0)
      (gtk-widget-show box)
         
      ;; Call the make-box function
      (setq box (make-box nil 0 t nil 0))
      (gtk-box-pack-start vbox box :expand nil :fill nil :padding 0)
      (gtk-widget-show box)
      
      ;; Create a horizontal separator
      (setq separator (make-instance 'gtk-h-separator))
      (gtk-box-pack-start vbox separator :expand nil :fill t :padding 0)
      (gtk-widget-show separator)
         
      ;; Create another label
      (setq label
            (make-instance 'gtk-label
                           :label "GtkHbox :homogenous t :spacing 0"))
      (gtk-misc-set-alignment label 0 0)
      (gtk-box-pack-start vbox label :expand nil :fill nil :padding 0)
      (gtk-widget-show label)
      
      ;; Call the make-box function
      (setq box (make-box t 0 t nil 0))
      (gtk-box-pack-start vbox box :expand nil :fill nil :padding 0)
      (gtk-widget-show box)
      
      ;; Call the make-box function
      (setq box (make-box t 0 t t 0))
      (gtk-box-pack-start vbox box :expand nil :fill nil :padding 0)
      (gtk-widget-show box)
      
      ;; Another separator
      (setq separator (make-instance 'gtk-h-separator))
      (gtk-box-pack-start vbox separator :expand nil :fill t :padding 5)
      (gtk-widget-show separator)
      
      (setq quitbox (gtk-h-box-new nil 0))
      (setq button  (gtk-button-new-with-label "Quit"))
      (gtk-box-pack-start quitbox button :expand nil :fill nil :padding 0)
      (gtk-box-pack-start vbox quitbox :expand nil :fill nil :padding 0)
      
      (g-signal-connect button "clicked"
                        (lambda (button)
                          (declare (ignore button))
                          (gtk-widget-destroy window)))
      (gtk-container-add window vbox)
      (gtk-widget-show window))))
