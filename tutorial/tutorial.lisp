;;; ----------------------------------------------------------------------------
;;; tutorial.lisp
;;;
;;; Examples from the offical GTK+ 2.0 Tutorial translated to Lisp
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

(asdf:operate 'asdf:load-op :cl-gtk)

(defpackage :gtk-tutorial
  (:use :gtk :gdk :gobject :glib :common-lisp))

(in-package :gtk-tutorial)

;;; ----------------------------------------------------------------------------
;;;
;;; Chapter 3. Getting started
;;;
;;; ----------------------------------------------------------------------------

(defun example-simple-window ()
  (within-main-loop
    (let (;; Create a toplevel window.
          (window (gtk-window-new :toplevel)))
      ;; Show the window.
      (gtk-widget-show window))))

(defun example-simple-window-2 ()
  (within-main-loop
    (let (;; Create a toplevel window with a title and a default width.
          (window (make-instance 'gtk-window
                                 :type :toplevel
                                 :title "Getting started"
                                 :default-width 250)))
      ;; Show the window.
      (gtk-widget-show window))))

;;; ----------------------------------------------------------------------------

;;; Hello World in GTK

(defun example-hello-world ()
  (within-main-loop
    (let (;; Create a toplevel window, set a border width.
          (window (make-instance 'gtk-window
                                 :type :toplevel
                                 :title "Hello World"
                                 :default-width 250
                                 :border-width 12))
          ;; Create a button with a label.
          (button (make-instance 'gtk-button :label "Hello World")))
      ;; Signal handler for the button to handle the signal "clicked".
      (g-signal-connect button "clicked"
                        (lambda (widget)
                          (declare (ignore widget))
                          (format t "Hello world.~%")
                          (gtk-widget-destroy window)))
      ;; Signal handler for the window to handle the signal "destroy".
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (gtk-main-quit)))
      ;; Signal handler for the window to handle the signal "delete-event".
      (g-signal-connect window "delete-event"
                        (lambda (widget event)
                          (declare (ignore widget event))
                          (format t "Delete Event Occured.~%")
                          t))
      ;; Put the button into the window.
      (gtk-container-add window button)
      ;; Show the window and the button.
      (gtk-widget-show window))))

;;; ----------------------------------------------------------------------------

;;; An Upgraded Hello World

(defun example-upgraded-hello-world ()
  (within-main-loop
    (let ((window (gtk-window-new :toplevel))
          (box (gtk-hbox-new nil 6))
          (button  nil))
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (gtk-main-quit)))
      (gtk-window-set-title window "Hello Buttons")
      (gtk-window-set-default-size window 250 75)
      (gtk-container-set-border-width window 12)
      (setq button (gtk-button-new-with-label "Button 1"))
      (g-signal-connect button "clicked"
                        (lambda (widget)
                          (declare (ignore widget))
                          (format t "Button 1 was pressed.~%")))
      (gtk-box-pack-start box button :expand t :fill t :padding 0)
      (gtk-widget-show button)
      
      (setq button (gtk-button-new-with-label "Button 2"))
      (g-signal-connect button "clicked"
                        (lambda (widget)
                          (declare (ignore widget))
                          (format t "Button 2 was pressed.~%")))
      (gtk-box-pack-start box button :expand t :fill t :padding 0)
      (gtk-container-add window box)
      (gtk-widget-show window))))

;;; ----------------------------------------------------------------------------

(defun example-upgraded-hello-world-2 ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window 
                                 :type :toplevel
                                 :title "Hello Buttons"
                                 :default-width 250
                                 :default-height 75
                                 :border-width 12))
          (box (make-instance 'gtk-hbox
                              :homogeneous nil
                              :spacing 6)))
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (gtk-main-quit)))
      (let ((button (gtk-button-new-with-label "Button 1")))
        (g-signal-connect button "clicked"
                          (lambda (widget)
                            (declare (ignore widget))
                            (format t "Button 1 was pressed.~%")))
        (gtk-box-pack-start box button :expand t :fill t :padding 0))
      (let ((button (gtk-button-new-with-label "Button 2")))
        (g-signal-connect button "clicked"
                          (lambda (widget)
                            (declare (ignore widget))
                            (format t "Button 2 was pressed.~%")))
        (gtk-box-pack-start box button :expand t :fill t :padding 0))
      (gtk-container-add window box)
      (gtk-widget-show window))))

;;; ----------------------------------------------------------------------------
;;;
;;; Chapter 5. Packing Widgets
;;;
;;; ----------------------------------------------------------------------------

;;; Packing Demonstrations Program

(defun make-box (homogeneous spacing expand fill padding)
  (let ((box (make-instance 'gtk-hbox
                            :homogeneous homogeneous
                            :spacing spacing)))
    (gtk-box-pack-start box
                        (gtk-button-new-with-label "gtk-box-pack")
                        :expand expand
                        :fill fill
                        :padding padding)
    (gtk-box-pack-start box
                        (gtk-button-new-with-label "box")
                        :expand expand
                        :fill fill
                        :padding padding)
    (gtk-box-pack-start box
                        (gtk-button-new-with-label "button")
                        :expand expand
                        :fill fill
                        :padding padding)
    (gtk-box-pack-start box
                        (if expand
                            (gtk-button-new-with-label "T")
                            (gtk-button-new-with-label "NIL"))
                        :expand expand
                        :fill fill
                        :padding padding)
    (gtk-box-pack-start box
                        (if fill
                            (gtk-button-new-with-label "T")
                            (gtk-button-new-with-label "NIL"))
                        :expand expand
                        :fill fill
                        :padding padding)
    (gtk-box-pack-start box
                        (gtk-button-new-with-label (format nil "~A" padding))
                        :expand expand
                        :fill fill
                        :padding padding)
    box))

(defun example-packing-boxes (&optional (spacing 0))
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :title "Example Packing Boxes"
                                 :type :toplevel
                                 :border-width 12
                                 :default-height 200
                                 :default-width 300))
          (vbox (make-instance 'gtk-vbox
                               :homogeneous nil
                               :spacing 6))
          (button (make-instance 'gtk-button
                                 :label "Quit"))
          (quitbox (make-instance 'gtk-hbox
                                  :homogeneous nil
                                  :spacing 0)))
      (g-signal-connect button "clicked"
                        (lambda (widget)
                          (declare (ignore widget))
                          (gtk-widget-destroy window)))
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (gtk-main-quit)))
      (gtk-box-pack-start vbox
                          (make-instance 'gtk-label
                                         :label
                                         (format nil
                                          "GtkHBox homogeneous nil spacing ~A"
                                                 spacing)
                                         :xalign 0
                                         :yalign 0)
                          :expand nil
                          :fill nil
                          :padding 0)
      (gtk-box-pack-start vbox
                          (make-instance 'gtk-h-separator)
                          :expand nil
                          :fill t
                          :padding 0)
      (gtk-box-pack-start vbox
                          (make-box nil spacing nil nil 0)
                          :expand nil
                          :fill nil
                          :padding 0)
      (gtk-box-pack-start vbox
                          (make-box nil spacing t nil 0)
                          :expand nil
                          :fill nil
                          :padding 0)
      (gtk-box-pack-start vbox
                          (make-box nil spacing t t 0)
                          :expand nil
                          :fill nil
                          :padding 0)
      (gtk-box-pack-start vbox
                          (make-instance 'gtk-h-separator)
                          :expand nil
                          :fill t
                          :padding 0)
      (gtk-box-pack-start vbox
                          (make-instance 'gtk-label
                                         :label
                                         (format nil
                                          "GtkHBox homogeneous t spacing ~A"
                                                 spacing)
                                         :xalign 0
                                         :yalign 0)
                          :expand nil
                          :fill nil
                          :padding 5)
      (gtk-box-pack-start vbox
                          (make-instance 'gtk-h-separator)
                          :expand nil
                          :fill t
                          :padding 0)
      (gtk-box-pack-start vbox
                          (make-box t spacing t nil 0)
                          :expand nil
                          :fill nil
                          :padding 0)
      (gtk-box-pack-start vbox
                          (make-box t spacing t t 0)
                          :expand nil
                          :fill nil
                          :padding 0)
      (gtk-box-pack-start vbox
                          (make-instance 'gtk-h-separator)
                          :expand nil
                          :fill t
                          :padding 5)
      (gtk-box-pack-start quitbox button :expand nil :fill nil :padding 0)
      (gtk-box-pack-start vbox quitbox :expand nil :fill nil :padding 0)
      (gtk-container-add window vbox)
      (gtk-widget-show window))))

;;; ----------------------------------------------------------------------------

;;; Table Packing Example

(defun example-table-packing ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :type :toplevel
                                 :title "Example Table Packing"
                                 :border-width 12
                                 :default-width 300))
          (table (make-instance 'gtk-table
                                :n-columns 2
                                :n-rows 2
                                :homogeneous t))
          (button1 (make-instance 'gtk-button
                                  :label "Button 1"))
          (button2 (make-instance 'gtk-button
                                  :label "Button 2"))
          (quit (make-instance 'gtk-button
                               :label "Quit")))
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (gtk-main-quit)))
      (g-signal-connect quit "clicked"
                        (lambda (widget)
                          (declare (ignore widget))
                          (gtk-widget-destroy window)))
      (gtk-table-attach table button1 0 1 0 1)
      (gtk-table-attach table button2 1 2 0 1)
      (gtk-table-attach table quit    0 2 1 2)
      (gtk-container-add window table)
      (gtk-widget-show window))))

(defun example-table-packing-2 ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :type :toplevel
                                 :title "Example Table Packing"
                                 :border-width 12
                                 :default-width 300))
          (table (make-instance 'gtk-table
                                :n-columns 2
                                :n-rows 2
                                :homogeneous t))
          (button1 (make-instance 'gtk-toggle-button
                                  :label "More Row Spacing"))
          (button2 (make-instance 'gtk-toggle-button
                                  :label "More Col Spacing"))
          (quit (make-instance 'gtk-button
                               :label "Quit")))
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (gtk-main-quit)))
      (g-signal-connect button1 "toggled"
         (lambda (widget)
           (if (gtk-toggle-button-get-active widget)
               (progn
                 (gtk-table-set-row-spacings table 12)
                 (gtk-button-set-label widget "Less Row Spacing"))
               (progn
                 (gtk-table-set-row-spacings table 0)
                 (gtk-button-set-label widget "More Row Spacing")))))
      (g-signal-connect button2 "toggled"
         (lambda (widget)
           (if (gtk-toggle-button-get-active widget)
               (progn
                 (gtk-table-set-col-spacings table 12)
                 (gtk-button-set-label widget "Less Col Spacing"))
               (progn
                 (gtk-table-set-col-spacings table 0)
                 (gtk-button-set-label widget "More Col Spacing")))))
      (g-signal-connect quit "clicked"
                        (lambda (widget)
                          (declare (ignore widget))
                          (gtk-widget-destroy window)))
      (gtk-table-attach table button1 0 1 0 1)
      (gtk-table-attach table button2 1 2 0 1)
      (gtk-table-attach table quit    0 2 1 2)
      (gtk-container-add window table)
      (gtk-widget-show window))))

;;; ----------------------------------------------------------------------------
;;;
;;; Chapter 7. The Button Widget
;;;
;;; ----------------------------------------------------------------------------

;;; Normal Buttons

(defun xpm-label-box (filename text)
  (let ((box (make-instance 'gtk-hbox
                            :homogeneous nil
                            :spacing 0
                            :border-width 2))
        (label (make-instance 'gtk-label
                              :label text))
        (image (gtk-image-new-from-file filename)))
    (gtk-box-pack-start box image :expand nil :fill nil :padding 2)
    (gtk-box-pack-start box label :expand nil :fill nil :padding 2)
    box))

(defun example-button ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :title "Example Cool Button"
                                 :type :toplevel
                                 :border-width 10))
          (button (make-instance 'gtk-button))
          (box (xpm-label-box "save.png" "Save to File")))
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (gtk-main-quit)))
      (gtk-container-add button box)
      (gtk-container-add window button)
      (gtk-widget-show window))))

;;; ----------------------------------------------------------------------------

(defun example-buttons ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :title "Example Buttons"
                                 :type :toplevel
                                 :default-width 250
                                 :border-width 10))
          (vbox1 (make-instance 'gtk-vbox :spacing 5))
          (vbox2 (make-instance 'gtk-vbox :spacing 5))
          (hbox  (make-instance 'gtk-hbox :spacing 5)))
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (gtk-main-quit)))
      ;; Set gtk-button-images to T. This allows buttons with text and image.
      (setf (gtk-settings-gtk-button-images (gtk-settings-get-default)) t)
      ;; These are the standard functions to create a button.
      (gtk-box-pack-start vbox1
                          (gtk-button-new-with-label "Label"))
      (gtk-box-pack-start vbox1
                          (gtk-button-new-with-mnemonic "_Mnemonic"))
      (gtk-box-pack-start vbox1
                          (gtk-button-new-from-stock "gtk-apply"))
      ;; Create some buttons with make-instance.
      (gtk-box-pack-start vbox2
                          (make-instance 'gtk-button
                                         :image-position :right
                                         :image
                                         (gtk-image-new-from-stock "gtk-edit"
                                                                   :button)
                                         :label "gtk-edit"
                                         :use-stock t))
      (gtk-box-pack-start vbox2
                          (make-instance 'gtk-button
                                         :image-position :top
                                         :image
                                         (gtk-image-new-from-stock "gtk-cut"
                                                                   :button)
                                         :label "gtk-cut"
                                         :use-stock t))
      (gtk-box-pack-start vbox2
                          (make-instance 'gtk-button
                                         :image-position :bottom
                                         :image
                                         (gtk-image-new-from-stock "gtk-cancel"
                                                                   :button)
                                         :label "gtk-cancel"
                                         :use-stock t))
      (gtk-box-pack-start hbox vbox1)
      (gtk-box-pack-start hbox vbox2)
      (gtk-container-add window hbox)
      (gtk-widget-show window))))

;;; ----------------------------------------------------------------------------

;;; Radio Buttons

(defun example-radio-buttons ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :title "Example Radio Buttons"
                                 :type :toplevel
                                 :border-width 0))
          (vbox1 (make-instance 'gtk-vbox
                                :homogeneous nil
                                :spacing 0))
          (vbox2 (make-instance 'gtk-vbox
                                :homogeneous nil
                                :spacing 10
                                :border-width 10))
          (vbox3 (make-instance 'gtk-vbox
                                :homogeneous nil
                                :spacing 10
                                :border-width 10))
          (button nil))
      (gtk-container-add window vbox1)
      (gtk-box-pack-start vbox1 vbox2 :expand t :fill t :padding 0)
      
      (setq button (gtk-radio-button-new-with-label nil "Button 1"))
      (gtk-box-pack-start vbox2 button :expand t :fill t :padding 0)
      
      (setq button
            (gtk-radio-button-new-with-label (gtk-radio-button-get-group button)
                                             "Button 2"))
      (gtk-toggle-button-set-active button t)
      (gtk-box-pack-start vbox2 button :expand t :fill t :padding 0)
      
      (setq button
            (gtk-radio-button-new-with-mnemonic
                                             (gtk-radio-button-get-group button)
                                             "_Button 3"))
      (gtk-box-pack-start vbox2 button :expand t :fill t :padding 0)
      
      (gtk-box-pack-start vbox1
                          (make-instance 'gtk-h-separator)
                          :expand nil :fill nil :padding 0)
      (gtk-box-pack-start vbox1 vbox3 :expand nil :fill t :padding 0)
      
      (gtk-box-pack-start vbox3
                          (setq button
                                (make-instance 'gtk-button :label "Close")))
      
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (gtk-main-quit)))
      (g-signal-connect button "clicked"
                        (lambda (button)
                          (declare (ignore button))
                          (gtk-widget-destroy window)))
      
      (gtk-widget-show window))))

;;; ----------------------------------------------------------------------------
;;;
;;; Chapter 9. Range Widgets
;;;
;;; ----------------------------------------------------------------------------

(defun example-8 ()
  (within-main-loop
    (let* ((window   (make-instance 'gtk-window
                                    :type :toplevel
                                    :title "Range Controls"))
           (box1     (make-instance 'gtk-vbox
                                    :homogeneous nil
                                    :spacing 0))
           (box2     (make-instance 'gtk-hbox
                                    :homogeneous nil
                                    :spacing 10
                                    :border-width 10))
           (box3     (make-instance 'gtk-vbox
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
      
      (setq box2 (make-instance 'gtk-hbox
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
      
      (setq box2 (make-instance 'gtk-hbox
                                :homogeneous nil
                                :spacing 10
                                :border-width 10))
      
      (gtk-box-pack-start box2 label :expand nil :fill nil :padding 0)
      
      ;; At this place the code for a GtkOptionMenu is missing
      
      (gtk-box-pack-start box1 box2 :expand t :fill t :padding 0)
      
      (setq box2 (make-instance 'gtk-hbox
                                :homogeneous nil
                                :spacing 10
                                :border-width 10))
      
      (setq label (make-instance 'gtk-label :label "Scale Update Policy"))
      (gtk-box-pack-start box2 label :expand nil :fill nil :padding 0)
      
      ;; At this place the code for a GtkOptionMenu is missing
      
      (gtk-box-pack-start box1 box2 :expand t :fill t :padding 0)
      
      (setq box2 (make-instance 'gtk-hbox
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
      
      (setq box2 (make-instance 'gtk-hbox
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
      
      (setq box2 (make-instance 'gtk-vbox
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

;;; ----------------------------------------------------------------------------
;;;
;;; Chapter 10. Miscellaneous Widgets
;;;
;;; ----------------------------------------------------------------------------

;; Label

(defun example-labels ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :type :toplevel
                                 :title "Example Labels"
                                 :border-width 5))
          (vbox (make-instance 'gtk-vbox
                               :homogeneous nil
                               :spacing 5))
          (hbox (make-instance 'gtk-hbox
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
                                 (format nil "This is a Multi-line label.~%~
                                              Second line.~%~
                                              Third line.")))
      (gtk-container-add frame label)
      (gtk-box-pack-start vbox frame :expand nil :fill nil :padding 0)
      
      (setq frame (make-instance 'gtk-frame
                                 :label "Left Justified Label"))
      (setq label (make-instance 'gtk-label
                                 :justify :left
                                 :label
                                 (format nil
                                         "This is a Left Justified~%~
                                          Multi-line label.~%~
                                          Third     line.")))
      (gtk-container-add frame label)
      (gtk-box-pack-start vbox frame :expand nil :fill nil :padding 0)
      
      (setq frame (make-instance 'gtk-frame
                                 :label "Right Justified Label"))
      (setq label (make-instance 'gtk-label
                                 :justify :right
                                 :label
                                 (format nil
                                         "This is a Right Justified~%~
                                          Multi-line label.~%~
                                          Third     line.")))
      (gtk-container-add frame label)
      (gtk-box-pack-start vbox frame :expand nil :fill nil :padding 0)
      
      (setq vbox (make-instance 'gtk-vbox
                                :homogeneous nil
                                :spacing 5))
      (gtk-box-pack-start hbox vbox :expand nil :fill nil :padding 0)
      (setq frame (make-instance 'gtk-frame
                                 :label "Line wrapped label"))
      (setq label (make-instance 'gtk-label
                                 :wrap t
                                 :label
                                 (format nil
                                         "This is an example of a           ~
                                          line-wrapped label. It should not ~
                                          be taking up the entire width     ~
                                          allocated to it, but              ~
                                          automatically wraps the words to  ~
                                          fit.  The time  has come, for all ~
                                          good men, to come  to the aid of  ~
                                          their party. The sixth            ~
                                          sheik's six sheep's sick.  It     ~
                                          supports multiple paragraphs      ~
                                          correctly, and  correctly   adds  ~
                                          many          extra  spaces.")))
      (gtk-container-add frame label)
      (gtk-box-pack-start vbox frame :expand nil :fill nil :padding 0)
      
      (setq frame (make-instance 'gtk-frame
                                 :label "Filled and wrapped label"))
      (setq label (make-instance 'gtk-label
                                 :wrap t
                                 :justify :fill
                                 :label
                                 (format nil
                                         "This is an example of a           ~
                                          line-wrapped, filled label.  It   ~
                                          should be taking up the entire    ~
                                          width allocated to it.  Here is a ~
                                          sentence to prove my point.  Here ~
                                          is another sentence.  Here comes  ~
                                          the sun, do de do de do.    This  ~
                                          is a new paragraph.      This is  ~
                                          another newer, longer, better     ~
                                          paragraph.  It is coming to an    ~
                                          end, unfortunately.")))
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
                                 (format nil
                                         "This label is underlined!~%~
                                          This one is underlined in quite a ~
                                          funky fashion")))
      
      (gtk-container-add frame label)
      (gtk-box-pack-start vbox frame :expand nil :fill nil :padding 0)
      
      (gtk-widget-show window))))

;;; ----------------------------------------------------------------------------

(defun example-more-labels ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :type :toplevel
                                 :title "Example More Labels"
                                 :default-width 300
                                 :border-width 5))
          (vbox1 (make-instance 'gtk-vbox
                                :homogeneous nil
                                :spacing 5))
          (vbox2 (make-instance 'gtk-vbox
                                :homogeneous nil
                                :spacing 5))
          (hbox (make-instance 'gtk-hbox
                               :homogeneous nil
                               :spacing 5)))
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (gtk-main-quit)))
      (gtk-box-pack-start hbox
                          (make-instance 'gtk-label
                                         :label "Angle 90°"
                                         :angle 90))
      (gtk-box-pack-start vbox1
                          (make-instance 'gtk-label
                                         :label "Angel 45°"
                                         :angle 45))
      (gtk-box-pack-start vbox1
                          (make-instance 'gtk-label
                                         :label "Angel 315°"
                                         :angle 315))
      (gtk-box-pack-start hbox vbox1)
      (gtk-box-pack-start hbox
                          (make-instance 'gtk-label
                                         :label "Angel 270°"
                                         :angle 270))
      (gtk-box-pack-start vbox2 hbox)
      (gtk-box-pack-start vbox2
                          (make-instance 'gtk-h-separator))
      (gtk-box-pack-start vbox2
                          (gtk-label-new "Normal Label"))
      (gtk-box-pack-start vbox2
                          (gtk-label-new-with-mnemonic "With _Mnemonic"))
      (gtk-box-pack-start vbox2
                          (make-instance 'gtk-label
                                         :label "This Label is Selectable"
                                         :selectable t))
      (gtk-container-add window vbox2)
      (gtk-widget-show window))))

;;; ----------------------------------------------------------------------------

;;; Arrows

(defun create-button (arrow-type shadow-type)
  (let ((button (make-instance 'gtk-button)))
    (gtk-container-add button
                       (make-instance 'gtk-arrow
                                      :arrow-type arrow-type
                                      :shadow-type shadow-type))
    button))

(defun example-arrows ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :type :toplevel
                                 :title "Arrow Buttons"
                                 :default-width 250
                                 :border-width 10))
          (box (make-instance 'gtk-hbox
                              :homogeneous t
                              :spacing 0
                              :border-width 5)))
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (gtk-main-quit)))
      (gtk-box-pack-start box
                          (create-button :up :in)
                          :expand nil :fill nil :padding 3)
      (gtk-box-pack-start box
                          (create-button :down :out)
                          :expand nil :fill nil :padding 3)
      (gtk-box-pack-start box
                          (create-button :left :etched-in)
                          :expand nil :fill nil :padding 3)
      (gtk-box-pack-start box
                          (create-button :right :etched-out) 
                          :expand nil :fill nil :padding 3)
      (gtk-container-add window box)
      (gtk-widget-show window))))

;;; ----------------------------------------------------------------------------

;;; The Tooltip Object

(defun create-button (arrow-type shadow-type)
  (let ((button (make-instance 'gtk-button)))
    (gtk-container-add button
                       (make-instance 'gtk-arrow
                                      :arrow-type arrow-type
                                      :shadow-type shadow-type))
    (gtk-widget-set-tooltip-text button
                                 (format nil
                                         "Arrow of type ~A"
                                         (symbol-name arrow-type)))
    button))

(defun example-tooltips ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :type :toplevel
                                 :title "Arrow Buttons"
                                 :default-width 250
                                 :border-width 10))
          (box (make-instance 'gtk-hbox
                              :homogeneous t
                              :spacing 0
                              :border-width 5)))
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (gtk-main-quit)))
      (gtk-box-pack-start box
                          (create-button :up :in)
                          :expand nil :fill nil :padding 3)
      (gtk-box-pack-start box
                          (create-button :down :out)
                          :expand nil :fill nil :padding 3)
      (gtk-box-pack-start box
                          (create-button :left :etched-in)
                          :expand nil :fill nil :padding 3)
      (gtk-box-pack-start box
                          (create-button :right :etched-out) 
                          :expand nil :fill nil :padding 3)
      (gtk-container-add window box)
      (gtk-widget-show window))))

;;; ----------------------------------------------------------------------------

;;; Progress Bars

(defstruct pbar-data
  pbar
  timer
  mode)

(defun progress-bar-timeout (pdata)
  (if (pbar-data-mode pdata)
      (gtk-progress-bar-pulse (pbar-data-pbar pdata))
      (let ((val (+ (gtk-progress-bar-get-fraction (pbar-data-pbar pdata))
                    0.01)))
        (when (> val 1.0) (setq val 0.0))
        (gtk-progress-bar-set-fraction (pbar-data-pbar pdata) val)))
  t)

(defun example-progress-bar ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :type :toplevel
                                 :title "Example Progress Bar"))
          (pdata (make-pbar-data :pbar (make-instance 'gtk-progress-bar)
                                 :mode nil))
          (vbox (make-instance 'gtk-vbox
                               :border-width 10))
          (align (gtk-alignment-new 0.5 0.5 0.0 0.0))
          (table (gtk-table-new 2 3 nil)))
      (setf (pbar-data-timer pdata)
            (g-timeout-add 100
                           (lambda ()
                             (progress-bar-timeout pdata))))
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (g-source-remove (pbar-data-timer pdata))
                          (setf (pbar-data-timer pdata) 0)
                          (gtk-main-quit)))
      (gtk-box-pack-start vbox align)
      (gtk-container-add align (pbar-data-pbar pdata))
      (gtk-box-pack-start vbox (make-instance 'gtk-h-separator))
      (gtk-box-pack-start vbox table)
      (let ((check (gtk-check-button-new-with-mnemonic "_Show text")))
        (g-signal-connect check "clicked"
           (lambda (widget)
             (declare (ignore widget))
             (let ((text (gtk-progress-bar-get-text (pbar-data-pbar pdata))))
               (if (or (null text) (zerop (length text)))
                   (gtk-progress-bar-set-text (pbar-data-pbar pdata)
                                              "Some text")
                   (gtk-progress-bar-set-text (pbar-data-pbar pdata)
                                              "")))))
        (gtk-table-attach table check 0 1 0 1))
      (let ((check (gtk-check-button-new-with-label "Activity mode")))
        (g-signal-connect check "clicked"
           (lambda (widget)
             (declare (ignore widget))
             (setf (pbar-data-mode pdata)
                   (not (pbar-data-mode pdata)))
             (if (pbar-data-mode pdata)
                 (gtk-progress-bar-pulse (pbar-data-pbar pdata))
                 (gtk-progress-bar-set-fraction (pbar-data-pbar pdata)
                                                0.0))))
        (gtk-table-attach table check 0 1 1 2))
      (let ((check (gtk-check-button-new-with-label "Rigth to left")))
        (g-signal-connect check "clicked"
           (lambda (widget)
             (declare (ignore widget))
             (case (gtk-progress-bar-orientation (pbar-data-pbar pdata))
               (:left-to-right
                 (setf (gtk-progress-bar-orientation (pbar-data-pbar pdata))
                       :right-to-left))
               (:right-to-left
                 (setf (gtk-progress-bar-orientation (pbar-data-pbar pdata))
                       :left-to-right)))))
        (gtk-table-attach table check 0 1 2 3))
      (let ((button (gtk-button-new-with-label "Close")))
        (g-signal-connect button "clicked"
                          (lambda (widget)
                            (declare (ignore widget))
                            (gtk-widget-destroy window)))
        (gtk-box-pack-start vbox button))
      (gtk-container-add window vbox)
      (gtk-widget-show window))))

;;; ----------------------------------------------------------------------------

;;; Status Bar

(defun example-statusbar ()
  (within-main-loop
    (let* ((window (make-instance 'gtk-window
                                  :type :toplevel
                                  :title "Example Status Bar"
                                  :default-width 300
                                  :border-width 10))
           (vbox (make-instance 'gtk-vbox
                                :homogeneous nil
                                :spacing 1))
           (statusbar (make-instance 'gtk-statusbar))
           (id (gtk-statusbar-get-context-id statusbar "Example Status Bar"))
           (count 0))
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (gtk-main-quit)))
      (gtk-box-pack-start vbox statusbar)
      (let ((button (gtk-button-new-with-label "Push Item")))
        (g-signal-connect button "clicked"
           (lambda (widget)
             (declare (ignore widget))
             (setq count (+ 1 count))
             (gtk-statusbar-push statusbar id (format nil "Item ~A" count))))
        (gtk-box-pack-start vbox button :expand t :fill t :padding 2))
      (let ((button (gtk-button-new-with-label "Pop Item")))
        (g-signal-connect button "clicked"
           (lambda (widget)
             (declare (ignore widget))
             (gtk-statusbar-pop statusbar id)))
        (gtk-box-pack-start vbox button :expand t :fill t :padding 2))
      (gtk-container-add window vbox)
      (gtk-widget-show window))))

;;; ----------------------------------------------------------------------------

;;; Text Entry

(defun example-text-entry ()
  (within-main-loop
    (let* ((window (make-instance 'gtk-window
                                  :type :toplevel
                                  :title "Example Text Entry"
                                  :default-width 250))
           (vbox (make-instance 'gtk-vbox))
           (hbox (make-instance 'gtk-hbox))
           (entry (make-instance 'gtk-entry
                                 :text "Hello"
                                 :max-length 50))
           (pos (gtk-entry-get-text-length entry)))
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (gtk-main-quit)))
      (g-signal-connect entry "activate"
                        (lambda (widget)
                          (declare (ignore widget))
                          (format t "Entry contents: ~A"
                                  (gtk-entry-get-text entry))))
      (gtk-editable-insert-text entry " world" pos)
      (gtk-editable-select-region entry 0 (gtk-entry-get-text-length entry))
      (gtk-box-pack-start vbox entry :expand t :fill t :padding 0)
      (let ((check (gtk-check-button-new-with-label "Editable")))
        (g-signal-connect check "toggled"
           (lambda (widget)
             (declare (ignore widget))
             (gtk-editable-set-editable entry
                                        (gtk-toggle-button-get-active check))))
        (gtk-box-pack-start hbox check))
      (let ((check (gtk-check-button-new-with-label "Visible")))
        (gtk-toggle-button-set-active check t)
        (g-signal-connect check "toggled"
           (lambda (widget)
             (declare (ignore widget))
             (gtk-entry-set-visibility entry
                                       (gtk-toggle-button-get-active check))))
        (gtk-box-pack-start hbox check))
      (gtk-box-pack-start vbox hbox)
      (let ((button (gtk-button-new-from-stock "gtk-close")))
        (g-signal-connect button "clicked"
                          (lambda (widget)
                            (declare (ignore widget))
                            (gtk-widget-destroy window)))
        (gtk-box-pack-start vbox button))
      (gtk-container-add window vbox)
      (gtk-widget-show window))))

;;; ----------------------------------------------------------------------------

;;; Spin Button

(defun example-spin-button ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :type :toplevel
                                 :title "Example Spin Button"
                                 :default-width 300))
          (vbox (make-instance 'gtk-vbox
                               :homogeneous nil
                               :spacing 5
                               :border-width 10))
          (vbox1 (make-instance 'gtk-vbox
                                :homogeneous nil
                                :spacing 0
                                :border-width 5))
          (vbox2 (make-instance 'gtk-vbox
                                :homogeneous nil
                                :spacing 0
                                :boder-width 5))
          (hbox (make-instance 'gtk-hbox))
          (frame1 (make-instance 'gtk-frame
                                 :label "Not accelerated"))
          (frame2 (make-instance 'gtk-frame
                                 :label "Accelerated"))
          (label (make-instance 'gtk-label
                                :label "0")))
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (gtk-main-quit)))
      (let ((vbox (make-instance 'gtk-vbox))
            (spinner (make-instance 'gtk-spin-button
                                    :adjustment
                                    (make-instance 'gtk-adjustment
                                                   :value 1.0
                                                   :lower 1.0
                                                   :upper 31.0
                                                   :step-increment 1.0
                                                   :page-increment 5.0
                                                   :page-size 0.0)
                                    :climb-rate 0
                                    :digits 0
                                    :wrap t)))
        (gtk-box-pack-start vbox
                            (make-instance 'gtk-label
                                           :label "Day :"
                                           :xalign 0
                                           :yalign 0.5)
                            :expand nil
                            :fill t
                            :padding 0)
        (gtk-box-pack-start vbox spinner :expand nil :fill t :padding 0)
        (gtk-box-pack-start hbox vbox :expand t :fill t :padding 5))
      (let ((vbox (make-instance 'gtk-vbox))
            (spinner (make-instance 'gtk-spin-button
                                    :adjustment
                                    (make-instance 'gtk-adjustment
                                                   :value 1.0
                                                   :lower 1.0
                                                   :upper 12.0
                                                   :step-increment 1.0
                                                   :page-increment 5.0
                                                   :page-size 0.0)
                                    :climb-rate 0
                                    :digits 0
                                    :wrap t)))
        (gtk-box-pack-start vbox
                            (make-instance 'gtk-label
                                           :label "Month :"
                                           :xalign 0
                                           :yalign 0.5)
                            :expand nil
                            :fill t
                            :padding 0)
        (gtk-box-pack-start vbox spinner :expand nil :fill t :padding 0)
        (gtk-box-pack-start hbox vbox :expand t :fill t :padding 5))
      (let ((vbox (make-instance 'gtk-vbox))
            (spinner (make-instance 'gtk-spin-button
                                    :adjustment
                                    (make-instance 'gtk-adjustment
                                                   :value 1.0
                                                   :lower 1998.0
                                                   :upper 2100.0
                                                   :step-increment 1.0
                                                   :page-increment 100.0
                                                   :page-size 0.0)
                                    :climb-rate 0
                                    :digits 0
                                    :wrap t)))
        (gtk-box-pack-start vbox
                            (make-instance 'gtk-label
                                           :label "Year :"
                                           :xalign 0
                                           :yalign 0.5)
                            :expand nil
                            :fill t
                            :padding 0)
        (gtk-box-pack-start vbox spinner :expand nil :fill t :padding 0)
        (gtk-box-pack-start hbox vbox :expand t :fill t :padding 5))
      (gtk-box-pack-start vbox1 hbox :expand t :fill t :padding 5)
      (gtk-container-add frame1 vbox1)
      (gtk-box-pack-start vbox frame1 :expand t :fill t :padding 0)
      (setq hbox (make-instance 'gtk-hbox))
      (let ((vbox (make-instance 'gtk-vbox))
            (spinner1 (make-instance 'gtk-spin-button
                                     :adjustment
                                     (make-instance 'gtk-adjustment
                                                    :value 1.0
                                                    :lower -10000.0
                                                    :upper  10000.0
                                                    :step-increment 0.5
                                                    :page-increment 100.0
                                                    :page-size 0.0)
                                     :climb-rate 1.0
                                     :digits 2
                                     :wrap t))
            (spinner2 (make-instance 'gtk-spin-button
                                     :adjustment
                                     (make-instance 'gtk-adjustment
                                                    :value 2
                                                    :lower 1
                                                    :upper 5
                                                    :step-increment 1
                                                    :page-increment 1
                                                    :page-size 0)
                                     :climb-rate 0.0
                                     :digits 0
                                     :wrap t)))
        (gtk-box-pack-start vbox
                            (make-instance 'gtk-label
                                           :label "Value :"
                                           :xalign 0
                                           :yalign 0.5)
                            :expand nil
                            :fill t
                            :padding 0)
        (gtk-box-pack-start vbox spinner1 :expand nil :fill t :padding 0)
        (gtk-box-pack-start hbox vbox :expand t :fill t :padding 5)
        (g-signal-connect spinner2 "value-changed"
           (lambda (widget)
             (declare (ignore widget))
             (gtk-spin-button-set-digits
                                  spinner1
                                  (gtk-spin-button-get-value-as-int spinner2))))
        (setq vbox (make-instance 'gtk-vbox))
        (gtk-box-pack-start vbox
                            (make-instance 'gtk-label
                                           :label "Digits :"
                                           :xalign 0
                                           :yalign 0.5)
                            :expand nil
                            :fill t
                            :padding 0)
        (gtk-box-pack-start vbox spinner2 :expand nil :fill t :padding 0)
        (gtk-box-pack-start hbox vbox :expand t :fill t :padding 5)
        (gtk-box-pack-start vbox2 hbox :expand t :fill t :padding 5)
        (let ((check (make-instance 'gtk-check-button
                                    :label "Snap to 0.5-ticks"
                                    :active t)))
          (g-signal-connect check "clicked"
             (lambda (widget)
               (gtk-spin-button-set-snap-to-ticks
                                        spinner1
                                        (gtk-toggle-button-get-active widget))))
          (gtk-box-pack-start vbox2 check :expand t :fill t :padding 0))
        (let ((check (make-instance 'gtk-check-button
                                    :label "Numeric only input mode"
                                    :active t)))
          (g-signal-connect check "clicked"
             (lambda (widget)
               (gtk-spin-button-set-numeric
                                        spinner1
                                        (gtk-toggle-button-get-active widget))))
          (gtk-box-pack-start vbox2 check :expand t :fill t :padding 0))
        (gtk-container-add frame2 vbox2)
        (setq hbox (make-instance 'gtk-hbox))
        (let ((button (gtk-button-new-with-label "Value as Int")))
          (g-signal-connect button "clicked"
             (lambda (widget)
               (declare (ignore widget))
               (gtk-label-set-text
                         label
                         (format nil "~A"
                                 (gtk-spin-button-get-value-as-int spinner1)))))
            (gtk-box-pack-start hbox button))
        (let ((button (gtk-button-new-with-label "Value as Float")))
          (g-signal-connect button "clicked"
             (lambda (widget)
               (declare (ignore widget))
               (gtk-label-set-text
                                label
                                (format nil "~A"
                                        (gtk-spin-button-get-value spinner1)))))
          (gtk-box-pack-start hbox button))
        (gtk-box-pack-start vbox2 hbox)
        (gtk-box-pack-start vbox2 label))
      (gtk-box-pack-start vbox frame2 :expand t :fill t :padding 0)
      (let ((button (make-instance 'gtk-button
                                   :label "Close")))
        (g-signal-connect button "clicked"
                          (lambda (widget)
                            (declare (ignore widget))
                            (gtk-widget-destroy window)))
        (gtk-box-pack-start vbox button))
      (gtk-container-add window vbox)
      (gtk-widget-show window))))

;;; ----------------------------------------------------------------------------

;;; [...]

;;; Color Selection Widget

(let ((color (make-gdk-color :red 0
                             :blue 65535
                             :green 0)))

  (defun drawing-area-event (widget event area)
    (declare (ignore widget))
    (let ((handled nil))
      (when (eql (gdk-event-type event) :button-press)
        (let* ((colorseldlg (make-instance 'gtk-color-selection-dialog
                                           :title "Select Background Color"))
               (colorsel
                 (gtk-color-selection-dialog-color-selection colorseldlg)))
          (setq handled t)
          (gtk-color-selection-set-previous-color colorsel color)
          (gtk-color-selection-set-current-color colorsel color)
          (gtk-color-selection-set-has-palette colorsel t)
          (g-signal-connect colorsel "color-changed"
             (lambda (widget)
               (declare (ignore widget))
               (let ((color (gtk-color-selection-get-current-color colorsel)))
                 (gtk-widget-modify-bg area :normal color))))
          (let ((response (gtk-dialog-run colorseldlg)))
            (gtk-widget-destroy colorseldlg)
            (if (eql response :ok)
                (setq color (gtk-color-selection-get-current-color colorsel))
                (gtk-widget-modify-bg area :normal color)))))
      handled))

  (defun example-color-selection ()
    (within-main-loop
      (let ((window (make-instance 'gtk-window
                                   :title "Example Color Selection"
                                   :default-width 300))
            (area (make-instance 'gtk-drawing-area)))
        (g-signal-connect window "destroy"
                          (lambda (widget)
                            (declare (ignore widget))
                            (gtk-widget-destroy window)))
        (gtk-widget-modify-bg area :normal color)
        (gtk-widget-set-events area :button-press-mask)
        (g-signal-connect area "event"
                          (lambda (widget event)
                            (drawing-area-event widget event area)))
        (gtk-container-add window area)
        (gtk-widget-show window))))
)

;;; [...]

;;; ----------------------------------------------------------------------------
;;;
;;; Chapter 11. Container Widgets
;;;
;;; ----------------------------------------------------------------------------

;;; The Event Box

(defun example-event-box ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :type :toplevel
                                 :title "Example Event Box"
                                 :border-width 10))
          (eventbox (make-instance 'gtk-event-box))
          (label (make-instance 'gtk-label
                                :width-request 120
                                :height-request 20
                                :label
                                "Click here to quit, and more text, more")))
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (gtk-main-quit)))
      (gtk-container-add window eventbox)
      (gtk-container-add eventbox label)
      (gtk-widget-set-events eventbox :button-press-mask)
      (g-signal-connect eventbox "button-press-event"
                        (lambda (widget event)
                          (declare (ignore widget event))
                          (gtk-widget-destroy window)))
      (gtk-widget-realize eventbox)
      (gdk-window-set-cursor (gtk-widget-window eventbox)
                             (gdk-cursor-new :hand1))
      (gtk-widget-show window))))

;;; ----------------------------------------------------------------------------

;;; The Alignment widget

(defun example-alignment ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :type :toplevel
                                 :title "Example Alignment"
                                 :default-width 300
                                 :default-height 200
                                 :border-width 10))
          (button (make-instance 'gtk-button
                                 :label "Quit"))
          (alignment (make-instance 'gtk-alignment
                                    :xalign 0.25
                                    :yalign 0.25
                                    :xscale 0.75
                                    :yscale 0.50)))
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (gtk-widget-destroy window)))
      (gtk-container-add alignment button)
      (gtk-container-add window alignment)
      (gtk-widget-show window))))

;;; ----------------------------------------------------------------------------

;;; Fixed Container

(defun move-button (button fixed)
  (let* ((allocation (gtk-widget-get-allocation fixed))
         (width (- (gdk-rectangle-width allocation) 20))
         (height (- (gdk-rectangle-height allocation) 10)))
;    (format t "Position of button is: (~A ~A)~%"
;            (gtk-fixed-child-x fixed button)
;            (gtk-fixed-child-y fixed button))
    (gtk-fixed-move fixed button (random width) (random height))))

(defun example-fixed ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :type :toplevel
                                 :title "Example Fixed Container"
                                 :default-width 300
                                 :default-height 200
                                 :border-width 10))
          (fixed (make-instance 'gtk-fixed)))
      (g-signal-connect window "destroy"
                        (lambda (window)
                          (declare (ignore window))
                          (gtk-main-quit)))
      (gtk-container-add window fixed)
      (dotimes (i 3)
        (let ((button (gtk-button-new-with-label "Press me")))
          (g-signal-connect button "clicked"
                            (lambda (widget)
                              (move-button widget fixed)))
          (gtk-fixed-put fixed button (random 300) (random 200))))
      (gtk-widget-show window))))

;;; ----------------------------------------------------------------------------

;;; Layout Container

(defun move-button (button layout)
  (let* ((allocation (gtk-widget-get-allocation layout))
         (width (- (gdk-rectangle-width allocation) 20))
         (height (- (gdk-rectangle-height allocation) 10)))
    (gtk-layout-move layout button (random width) (random height))))

(defun example-layout ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :type :toplevel
                                 :title "Example Layout Container"
                                 :default-width 300
                                 :default-height 200
                                 :border-width 10))
          (layout (make-instance 'gtk-layout)))
      (g-signal-connect window "destroy"
                        (lambda (window)
                          (declare (ignore window))
                          (gtk-main-quit)))
      (gtk-container-add window layout)
      (dotimes (i 3)
        (let ((button (gtk-button-new-with-label "Press me")))
          (g-signal-connect button "clicked"
                            (lambda (widget)
                              (move-button widget layout)))
          (gtk-layout-put layout button (random 300) (random 200))))
      (gtk-widget-show window))))

;;; ----------------------------------------------------------------------------

;;; Frames

(defun example-frame ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :type :toplevel
                                 :title "Example Frame"
                                 :default-width 250
                                 :default-height 200
                                 :border-width 10))
          (frame (make-instance 'gtk-frame
                                :label "Gtk Frame Widget"
                                :label-xalign 1.0
                                :label-yalign 0.5
                                :shadow-type :etched-in)))
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (gtk-main-quit)))
      (gtk-container-add window frame)
      (gtk-widget-show window))))

;;; ----------------------------------------------------------------------------

;;; Aspect Frames

(defun example-aspect-frame ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :type :toplevel
                                 :title "Example Aspect Frame"
                                 :default-width 300
                                 :default-height 250
                                 :border-width 10))
          (frame (make-instance 'gtk-aspect-frame
                                :label "2 x 1"
                                :xalign 0.5
                                :yalign 0.5
                                :ratio 2
                                :obey-child nil))
          (area (make-instance 'gtk-drawing-area
                               :width-request 200
                               :hight-request 200)))
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (gtk-main-quit)))
      (gtk-container-add window frame)
      (gtk-container-add frame area)
      (gtk-widget-show window))))

;;; ----------------------------------------------------------------------------

;;; Paned Window Widgets

(defun example-paned-window ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :type :toplevel
                                 :title "Example Paned Window"
                                 :border-width 10))
          (vpaned (make-instance 'gtk-v-paned))
          (frame1 (make-instance 'gtk-frame :label "Frame 1"))
          (frame2 (make-instance 'gtk-frame :label "Frame 2")))
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (gtk-main-quit)))
      
      (gtk-widget-set-size-request window 450 400)
      (gtk-container-add window vpaned)
      
      (gtk-paned-add1 vpaned frame1)
      (gtk-paned-add2 vpaned frame2)
      
      (gtk-widget-show window))))

;;; ----------------------------------------------------------------------------

;;; Scrolled Windows

(defun example-scrolled-window ()
  (within-main-loop
    (let ((window (make-instance 'gtk-dialog
                                 :type :toplevel
                                 :title "Example Scrolled Window"
                                 :border-width 0
                                 :width-request 350
                                 :height-request 300))
          (scrolled (make-instance 'gtk-scrolled-window
                                   :border-width 10
                                   :hscrollbar-policy :automatic
                                   :vscrollbar-policy :always))
          (table (make-instance 'gtk-table
                                :n-rows 10
                                :n-columns 10
                                :row-spacing 10
                                :column-spacing 10
                                :homogeneous nil)))
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (gtk-main-quit)))
      
      (gtk-box-pack-start (gtk-dialog-get-content-area window)
                          scrolled
                          :expand t :fill t :padding 0)
      (gtk-scrolled-window-add-with-viewport scrolled table)
      
      (dotimes (i 10)
        (dotimes (j 10)
          (gtk-table-attach table
                            (make-instance 'gtk-button
                                           :label
                                           (format nil "(~d, ~d)" i j))
                            i (+ i 1) j (+ j 1))))
      
      (let ((button (make-instance 'gtk-button
                                   :label "Close"
                                   :can-default t)))
        (g-signal-connect button "clicked"
                          (lambda (widget)
                            (declare (ignore widget))
                            (gtk-widget-destroy window)))
        (gtk-box-pack-start (gtk-dialog-get-action-area window)
                            button
                            :expand t :fill t :padding 0)
        (gtk-widget-grab-default button))
      
      (gtk-widget-show window))))

;;; ----------------------------------------------------------------------------
