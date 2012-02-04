;;; ----------------------------------------------------------------------------
;;; tutorial.lisp
;;;
;;; Examples from the offical GTK+ 2.0 Tutorial translated to Lisp
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

(asdf:operate 'asdf:load-op :cl-gtk-gtk)

(defpackage :gtk-tutorial
  (:use :gtk :gdk :gobject :common-lisp))

(in-package :gtk-tutorial)

;;; Chapter 3. Getting started

(defun example-simple-window ()
  (within-main-loop
    (let ((window (gtk-window-new :toplevel)))
      (gtk-widget-show window))))

(defun example-simple-window-2 ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :type :toplevel
                                 :title "Getting started"
                                 :default-width 250)))
      (gtk-widget-show window))))

;;; Hello World in GTK

(defun example-hello-world ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :type :toplevel
                                 :title "Hello World"
                                 :default-width 250
                                 :border-width 10))
          (button (make-instance 'gtk-button :label "Hello World")))
      (g-signal-connect button "clicked"
                        (lambda (button)
                          (declare (ignore button))
                          (format t "Hello world.~%")
                          (gtk-widget-destroy window)))
      (g-signal-connect window "delete-event"
                        (lambda (window event)
                          (declare (ignore window event))
                          (format t "Delete Event Occured.~%")
                          t))
      (g-signal-connect window "destroy-event"
                        (lambda (window event)
                          (declare (ignore window event))
                          (gtk-main-quit)))
      (gtk-container-add window button)
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

;;; Table Packing Example

(defun example-5 ()
  (within-main-loop
    (let ((window  (make-instance 'gtk-window
                                  :type :toplevel
                                  :title "Table Packing Example"
                                  :border-width 20
                                  :default-width 300
                                  :default-heigt 200))
          (table   (make-instance 'gtk-table
                                  :n-columns 2
                                  :n-rows 2
                                  :homogeneous t))
          (button1 (make-instance 'gtk-button
                                  :label "Button 1"))
          (button2 (make-instance 'gtk-button
                                  :label "Button 2"))
          (quit    (make-instance 'gtk-button
                                  :label "Quit")))
      
      (g-signal-connect quit "clicked"
                        (lambda (button)
                          (declare (ignore button))
                          (gtk-widget-destroy window)))
      
      (gtk-container-add window table)
      
      (gtk-table-attach-defaults table button1 0 1 0 1)
      (gtk-table-attach-defaults table button2 1 2 0 1)
      (gtk-table-attach-defaults table quit    0 2 1 2)
      
      (gtk-widget-show window))))

(defun example-5-1 ()
  (within-main-loop
    (let ((window  (make-instance 'gtk-window
                                  :type :toplevel
                                  :title "Table Packing Example"
                                  :border-width 20
                                  :default-width 300
                                  :default-heigt 200))
          (table   (make-instance 'gtk-table
                                  :n-columns 2
                                  :n-rows 2
                                  :homogeneous t))
          (button1 (make-instance 'gtk-button
                                  :label "More Row Spacing"))
          (button2 (make-instance 'gtk-button
                                  :label "More Col Spacing"))
          (quit    (make-instance 'gtk-button
                                  :label "Quit")))
      
      (g-signal-connect button1 "clicked"
                        (lambda (button1)
                          (declare (ignore button1))
                          (gtk-table-set-row-spacings table 15)))
      (g-signal-connect button2 "clicked"
                        (lambda (button2)
                          (declare (ignore button2))
                          (gtk-table-set-col-spacings table 15)))
      (g-signal-connect quit "clicked"
                        (lambda (button)
                          (declare (ignore button))
                          (gtk-widget-destroy window)))
            
      (gtk-container-add window table)
      
      (gtk-table-attach-defaults table button1 0 1 0 1)
      (gtk-table-attach-defaults table button2 1 2 0 1)
      (gtk-table-attach-defaults table quit    0 2 1 2)
      
      (gtk-widget-show window))))

;;; Chapter 7. The Button Widget

(defun xpm-label-box (filename text)
  (let ((box (make-instance 'gtk-h-box
                            :homogeneous nil
                            :spacing 0
                            :border-width 2))
        (label (make-instance 'gtk-label
                              :label text))
        (image (gtk-image-new-from-file filename)))
    (gtk-box-pack-start box image :expand nil :fill nil :padding 2)
    (gtk-box-pack-start box label :expand nil :fill nil :padding 2)
    box))

(defun example-6 ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :title "Example Cool Button"
                                 :type :toplevel
                                 :border-width 10))
          (button (make-instance 'gtk-button))
          (box (xpm-label-box "save.png" "Save to File")))
      (gtk-container-add button box)
      (gtk-container-add window button)
      (gtk-widget-show window))))

;;; Radio Buttons

(defun example-7 ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :title "Example Radio Buttons"
                                 :type :toplevel
                                 :border-width 0))
          (vbox1  (make-instance 'gtk-v-box
                                 :homogeneous nil
                                 :spacing 0))
          (vbox2  (make-instance 'gtk-v-box
                                 :homogeneous nil
                                 :spacing 10
                                 :border-width 10))
          (vbox3  (make-instance 'gtk-v-box
                                 :homogeneous nil
                                 :spacing 10
                                 :border-width 10))
          (separator (make-instance 'gtk-h-separator))
          (button nil)
          (group nil))
      (gtk-container-add window vbox1)
      (gtk-box-pack-start vbox1 vbox2 :expand t :fill t :padding 0)
      
      (setq button (gtk-radio-button-new-with-label group "Button 1"))
      (gtk-box-pack-start vbox2 button :expand t :fill t :padding 0)
      
      (setq group (gtk-radio-button-get-group button))
      (setq button (gtk-radio-button-new-with-label group "Button 2"))
      (gtk-toggle-button-set-active button t)
      (gtk-box-pack-start vbox2 button :expand t :fill t :padding 0)
      
      (setq group (gtk-radio-button-get-group button))
      (setq button (gtk-radio-button-new-with-label group "Button 3"))
      (setq group (gtk-radio-button-get-group button))
      (gtk-box-pack-start vbox2 button :expand t :fill t :padding 0)
      
      (gtk-box-pack-start vbox1 separator :expand nil :fill nil :padding 0)
      (gtk-box-pack-start vbox1 vbox3 :expand nil :fill t :padding 0)
      
      (setq button (make-instance 'gtk-button :label "Close"))
      (gtk-box-pack-start vbox3 button)
      
      (g-signal-connect button "clicked"
                        (lambda (button)
                          (declare (ignore button))
                          (gtk-widget-destroy window)))
      
      (gtk-widget-show window))))

;; The same with the function gtk-radio-button-new-with-label-from-widget.
;; For this example it is not necessary to have the list group.

(defun example-7-1 ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :title "Example Radio Buttons"
                                 :type :toplevel
                                 :border-width 0))
          (vbox1  (make-instance 'gtk-v-box
                                 :homogeneous nil
                                 :spacing 0))
          (vbox2  (make-instance 'gtk-v-box
                                 :homogeneous nil
                                 :spacing 10
                                 :border-width 10))
          (vbox3  (make-instance 'gtk-v-box
                                 :homogeneous nil
                                 :spacing 10
                                 :border-width 10))
          (separator (make-instance 'gtk-h-separator))
          (button nil))
      (gtk-container-add window vbox1)
      (gtk-box-pack-start vbox1 vbox2 :expand t :fill t :padding 0)
      
      (setq button (gtk-radio-button-new-with-label nil "Button 1"))
      (gtk-box-pack-start vbox2 button :expand t :fill t :padding 0)
      
      (setq button
            (gtk-radio-button-new-with-label-from-widget button "Button 2"))
      (gtk-toggle-button-set-active button t)
      (gtk-box-pack-start vbox2 button :expand t :fill t :padding 0)
      
      (setq button
            (gtk-radio-button-new-with-label-from-widget button "Button 3"))
      (gtk-box-pack-start vbox2 button :expand t :fill t :padding 0)
      
      (gtk-box-pack-start vbox1 separator :expand nil :fill nil :padding 0)
      (gtk-box-pack-start vbox1 vbox3 :expand nil :fill t :padding 0)
      
      (setq button (make-instance 'gtk-button :label "Close"))
      (gtk-box-pack-start vbox3 button)
      
      (g-signal-connect button "clicked"
                        (lambda (button)
                          (declare (ignore button))
                          (gtk-widget-destroy window)))
      
      (gtk-widget-show window))))

;;; Chapter 9. Range Widgets

(defun example-8 ()
  (within-main-loop
    (let* ((window   (make-instance 'gtk-window
                                    :type :toplevel
                                    :title "Range Controls"))
           (box1     (make-instance 'gtk-v-box
                                    :homogeneous nil
                                    :spacing 0))
           (box2     (make-instance 'gtk-h-box
                                    :homogeneous nil
                                    :spacing 10
                                    :border-width 10))
           (box3     (make-instance 'gtk-v-box
                                    :homogeneous nil
                                    :spacing 10))
           (adj      (make-instance 'gtk-adjustment
                                    :value 0.0
                                    :lower 0.0
                                    :upper 101.0
                                    :step-increment 0.1
                                    :page-increment 1.0
                                    :page-size 1.0))
           (adj2     (make-instance 'gtk-adjustment
                                    :value 1.0
                                    :lower 0.0
                                    :upper 5.0
                                    :step-increment 1.0
                                    :page-increment 1.0
                                    :page-size 0.0))
           (vscale   (make-instance 'gtk-v-scale
                                    :update-policy :continuous
                                    :digits 1
                                    :value-pos :top
                                    :draw-value t
                                    :adjustement adj))
           (hscale   (make-instance 'gtk-h-scale
                                    :update-policy :continuous
                                    :digits 1
                                    :value-pos :top
                                    :draw-value t
                                    :width-request 200
                                    :height-request -1
                                    :adjustement adj))
           (scale    (make-instance 'gtk-h-scale
                                    :digits 0
                                    :adjustement adj2))
           (scrollbar (make-instance 'gtk-h-scrollbar
                                     :update-policy :continuous
                                     :adjustement adj))
           (button    (make-instance 'gtk-check-button
                                     :label "Display value on scale widget"
                                     :active t))
           (label     (make-instance 'gtk-label
                                     :label "Scale value position"))
; TODO: GtkOptionMenu is not implemented           
;           (opt       (make-instance 'gtk-option-menu))
                                     
           )
      (gtk-box-pack-start box2 vscale :expand t :fill t :padding 0)

      (gtk-box-pack-start box3 hscale :expand t :fill t :padding 0)
      (gtk-box-pack-start box3 scrollbar :expand t :fill t :padding 0)
      
      (gtk-box-pack-start box2 box3 :expand t :fill t :padding 0)
      (gtk-box-pack-start box1 box2 :expand t :fill t :padding 0)
            
      (gtk-container-add window box1)
      
      (setq box2 (make-instance 'gtk-h-box
                                :homogeneous nil
                                :spacing 10
                                :border-width 10))
      
      (gtk-box-pack-start box1 box2 :expand t :fill t :padding 0)
      
      (g-signal-connect button "toggled"
                        (lambda (button)
                          (setf (gtk-scale-draw-value hscale)
                                (gtk-toggle-button-active button))
                          (setf (gtk-scale-draw-value vscale)
                                (gtk-toggle-button-active button))))
      
      (gtk-box-pack-start box2 button :expand t :fill t :padding 0)
      
      (setq box2 (make-instance 'gtk-h-box
                                :homogeneous nil
                                :spacing 10
                                :border-width 10))
      
      (gtk-box-pack-start box2 label :expand nil :fill nil :padding 0)
      
      ;; At this place the code for a GtkOptionMenu is missing
      
      (gtk-box-pack-start box1 box2 :expand t :fill t :padding 0)
      
      (setq box2 (make-instance 'gtk-h-box
                                :homogeneous nil
                                :spacing 10
                                :border-width 10))
      
      (setq label (make-instance 'gtk-label :label "Scale Update Policy"))
      (gtk-box-pack-start box2 label :expand nil :fill nil :padding 0)
      
      ;; At this place the code for a GtkOptionMenu is missing
      
      (gtk-box-pack-start box1 box2 :expand t :fill t :padding 0)
      
      (setq box2 (make-instance 'gtk-h-box
                                :homogeneous nil
                                :spacing 10
                                :border-width 10))
      
      (setq label (make-instance 'gtk-label :label "Scale Digits:"))
      (gtk-box-pack-start box2 label :expand nil :fill nil :padding 0)
      
      (g-signal-connect adj2 "value_changed"
                        (lambda (adjustment)
                          (setf (gtk-scale-digits hscale)
                                (gtk-adjustment-value adjustment))
                          (setf (gtk-scale-digits vscale)
                                (gtk-adjustment-value adjustment))))
      
      (gtk-box-pack-start box2 scale :expand t :fill t :padding 0)
      (gtk-box-pack-start box1 box2 :expand t :fill t :padding 0)
      
      (setq box2 (make-instance 'gtk-h-box
                                :homogeneous nil
                                :spacing 10
                                :border-width 10))
      
      (setq label (make-instance 'gtk-label :label "Scrollbar Page Size:"))
      (gtk-box-pack-start box2 label :expand nil :fill nil :padding 0)
      
      (setq adj2 (make-instance 'gtk-adjustment
                                :value 1.0
                                :lower 1.0
                                :upper 101.0
                                :step-increment 1.0
                                :page-increment 1.0
                                :page-size 0.0))
      
      (g-signal-connect adj2 "value_changed"
                        (lambda (adjustment)
                          (setf (gtk-adjustment-page-size adj)
                                (gtk-adjustment-page-size adjustment))
                          (setf (gtk-adjustment-page-increment adj)
                                (gtk-adjustment-page-increment adjustment))))
      
      (setq scale (make-instance 'gtk-h-scale
                                 :digits 0
                                 :adjustement adj2))
      (gtk-box-pack-start box2 scale :expand t :fill t :padding 0)
      (gtk-box-pack-start box1 box2 :expand t :fill t :padding 0)
      
      (gtk-box-pack-start box1
                          (make-instance 'gtk-h-separator)
                          :expand nil :fill t :padding 0)
      
      (setq box2 (make-instance 'gtk-v-box
                                :homogeneous nil
                                :spacing 10
                                :border-width 10))
      
      (gtk-box-pack-start box1 box2 :expand nil :fill t :padding 0)
      
      (setq button (make-instance 'gtk-button :label "Quit"))
      
      (g-signal-connect button "clicked"
                        (lambda (button)
                          (declare (ignore button))
                          (gtk-widget-destroy window)))
      
      (gtk-box-pack-start box2 button :expand t :fill t :padding 0)
      
      ; GTK_WIDGET_SET_FLAGS (button, GTK_CAN_DEFAULT)
      ; gtk_widget_grab_default (button)
      
      (gtk-widget-show window)
      )))

;;; [...]

;;; Chapter 10. Miscellaneous Widgets

;; Label

(defun example-label ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :type :toplevel
                                 :title "Demo Label"
                                 :border-width 5))
          (vbox (make-instance 'gtk-v-box
                               :homogeneous nil
                               :spacing 5))
          (hbox (make-instance 'gtk-h-box
                               :homogeneous nil
                               :spacing 5))
          (frame (make-instance 'gtk-frame
                                :title "Normal Label"))
          (label (make-instance 'gtk-label
                                :label "This is a Normal label")))
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (gtk-main-quit)))
      (gtk-container-add window hbox)
      (gtk-box-pack-start hbox vbox :expand nil :fill nil :padding 0)
      (gtk-container-add frame label)
      (gtk-box-pack-start vbox frame :expand nil :fill nil :padding 0)
      
      (setq frame (make-instance 'gtk-frame
                                 :label "Multi-line Label"))
      (setq label (make-instance 'gtk-label
                                 :label
"This is a Multi-line label.
Second line.
Third line"))
      (gtk-container-add frame label)
      (gtk-box-pack-start vbox frame :expand nil :fill nil :padding 0)
      
      (setq frame (make-instance 'gtk-frame
                                 :label "Left Justified Label"))
      (setq label (make-instance 'gtk-label
                                 :justify :left
                                 :label
"This is a Left Justified
Multi-line label.
Third      line"))
      (gtk-container-add frame label)
      (gtk-box-pack-start vbox frame :expand nil :fill nil :padding 0)
      
      (setq frame (make-instance 'gtk-frame
                                 :label "Right Justified Label"))
      (setq label (make-instance 'gtk-label
                                 :justify :right
                                 :label
"This is a Right Justified
Multi-line label.
Third      line"))
      (gtk-container-add frame label)
      (gtk-box-pack-start vbox frame :expand nil :fill nil :padding 0)
      
      (setq vbox (make-instance 'gtk-v-box
                                :homogeneous nil
                                :spacing 5))
      (gtk-box-pack-start hbox vbox :expand nil :fill nil :padding 0)
      (setq frame (make-instance 'gtk-frame
                                 :label "Line wrapped label"))
      (setq label (make-instance 'gtk-label
                                 :wrap t
                                 :label
"This is an example of a line-wrapped label.  It should not be taking up the entire               width allocated to it, but automatically wraps the words to fit.  The time has come, for all good men, to come to the aid of their party. The sixth sheik's six sheep's sick.  It supports multiple paragraphs correctly, and  correctly   adds many          extra  spaces. "))
      (gtk-container-add frame label)
      (gtk-box-pack-start vbox frame :expand nil :fill nil :padding 0)
      
      (setq frame (make-instance 'gtk-frame
                                 :label "Filled and wrapped label"))
      (setq label (make-instance 'gtk-label
                                 :wrap t
                                 :justify :fill
                                 :label
"This is an example of a line-wrapped, filled label.  It should be taking up the entire             width allocated to it.  Here is a sentence to prove my point.  Here is another sentence.  Here comes the sun, do de do de do.    This is a new aragraph.    This is another newer, longer, better paragraph.  It is coming to an end, unfortunately."))
      (gtk-container-add frame label)
      (gtk-box-pack-start vbox frame :expand nil :fill nil :padding 0)
      
      (setq frame (make-instance 'gtk-frame
                                 :label "Underlined label"))
      (setq label (make-instance 'gtk-label
                                 :justify :left
                                 :use-underline t
                                 :pattern
"_________________________ _ _________ _ ______     __ _______ ___"
                                 :label
"This label is underlined!
This one is underlined in quite a funky fashion"))
      
      (gtk-container-add frame label)
      (gtk-box-pack-start vbox frame :expand nil :fill nil :padding 0)
      
      (gtk-widget-show window))))

;; Arrows

(defun create-button (arrow-type shadow-type)
  (let ((button (make-instance 'gtk-button))
        (arrow (make-instance 'gtk-arrow
                              :arrow-type arrow-type
                              :shadow-type shadow-type)))
    (gtk-container-add button arrow)
    button))

(defun example-arrow ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :type :toplevel
                                 :title "Arrow Buttons"
                                 :default-width 250
                                 :border-width 10))
          (button (make-instance 'gtk-button))
          (box (make-instance 'gtk-h-box
                              :homogeneous nil
                              :spacing 0
                              :border-width 2)))
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (gtk-main-quit)))
      (gtk-container-add window box)
      
      (setq button (create-button :up :in))
      (gtk-box-pack-start box button :expand nil :fill nil :padding 3)
      
      (setq button (create-button :down :out))
      (gtk-box-pack-start box button :expand nil :fill nil :padding 3)
      
      (setq button (create-button :left :etched-in))
      (gtk-box-pack-start box button :expand nil :fill nil :padding 3)
      
      (setq button (create-button :right :etched-out))
      (gtk-box-pack-start box button :expand nil :fill nil :padding 3)
      
      (gtk-widget-show window))))

;;; [...]

;;; Chapter 11. Container Widgets

;; The Event Box

(defun example-event-box ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :type :toplevel
                                 :title "Example Event Box"
                                 :border-width 10))
          (eventbox (make-instance 'gtk-event-box))
          (label    (make-instance 'gtk-label
                                   :label
                                   "Click here to quit, and more text, more")))
      (g-signal-connect window "destroy"
                        (lambda (window)
                          (declare (ignore window))
                          (gtk-main-quit)))
      
      (gtk-container-add window eventbox)
      (gtk-container-add eventbox label)
      
      (gtk-widget-size-request label
                               (make-gtk-requisition :width 60 :height 20))
      (gtk-widget-set-events eventbox 256)
      
      (g-signal-connect eventbox "button-press-event"
                        (lambda (eventbox event)
                          (declare (ignore eventbox event))
                          (gtk-widget-destroy window)))
      
      (gtk-widget-realize eventbox)
      (gdk-window-set-cursor (gtk-widget-window eventbox)
                             (gdk-cursor-new :hand1))
      
      (gtk-widget-show window))))


(defvar x 50)
(defvar y 50)

(defun move-button (button fixed)
  (setq x (+ x 30))
  (setq y (+ y 50))
  (gtk-fixed-move fixed button x y))

(defun example-fixed ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :type :toplevel
                                 :title "Example Fixed Container"
                                 :default-width 400
                                 :default-height 300
                                 :border-width 10))
          (fixed  (make-instance 'gtk-fixed))
          (button nil))
      (g-signal-connect window "destroy"
                        (lambda (window)
                          (declare (ignore window))
                          (gtk-main-quit)))
      (gtk-container-add window fixed)
      (dotimes (i 3)
        (setq button (gtk-button-new-with-label "Press me"))
        (g-signal-connect button "clicked"
                          (lambda (button)
                            (move-button button fixed)))
        (gtk-fixed-put fixed button (+ 50 (* i 50)) (+ 50 (* i 50))))
      (gtk-widget-show window))))

(defun example-frame ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :type :toplevel
                                 :title "Example Frame"
                                 :default-width 300
                                 :default-height 300
                                 :border-width 10))
          (frame  (make-instance 'gtk-frame
                                 :label "Gtk Frame Widget"
                                 :label-xalign 1.0
                                 :label-yalign 0.5
                                 :shadow-type :etched-in)))
      (g-signal-connect window "destroy"
                        (lambda (window)
                          (declare (ignore window))
                          (gtk-main-quit)))
      (gtk-container-add window frame)
      (gtk-widget-show window))))
