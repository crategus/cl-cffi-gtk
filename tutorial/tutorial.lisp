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
                          (make-instance 'gtk-hseparator)
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
                          (make-instance 'gtk-hseparator)
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
                          (make-instance 'gtk-hseparator)
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
                          (make-instance 'gtk-hseparator)
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

(defun image-label-box (filename text)
  (let ((box (make-instance 'gtk-hbox
                            :homogeneous nil
                            :spacing 0
                            :border-width 3))
        (label (make-instance 'gtk-label
                              :label text))
        (image (gtk-image-new-from-file filename)))
    (gtk-box-pack-start box image :expand nil :fill nil :padding 3)
    (gtk-box-pack-start box label :expand nil :fill nil :padding 3)
    box))

(defun example-button ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :title "Example Cool Button"
                                 :type :toplevel
                                 :border-width 12))
          (button (make-instance 'gtk-button))
          (box (image-label-box "save.png" "Save to File")))
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
                                 :border-width 12))
          (vbox1 (make-instance 'gtk-vbox :spacing 6))
          (vbox2 (make-instance 'gtk-vbox :spacing 6))
          (hbox  (make-instance 'gtk-hbox :spacing 6)))
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

;;; Toggle Buttons

(defun example-toggle-buttons ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :title "Example Toggle Buttons"
                                 :type :toplevel))
          (vbox (make-instance 'gtk-vbox
                               :homogeneous nil
                               :spacing 0))
          (hbox (make-instance 'gtk-hbox
                               :homogenous nil
                               :spacing 0)))
      ;; Handler for the signal "destroy"
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (gtk-main-quit)))
      ;; Create three radio buttons and put the buttons in a vbox      
      (let ((vbox (make-instance 'gtk-vbox
                                 :homogenous nil
                                 :spacing 12
                                 :border-width 12))
            (button (gtk-radio-button-new-with-label nil "Radio Button 1")))
        (gtk-box-pack-start vbox button :expand t :fill t)
        (setq button
              (gtk-radio-button-new-with-label
                                          (gtk-radio-button-get-group button)
                                          "Radio Button 2"))
        (gtk-toggle-button-set-active button t)
        (gtk-box-pack-start vbox button :expand t :fill t)
        (setq button
              (gtk-radio-button-new-with-mnemonic
                                          (gtk-radio-button-get-group button)
                                          "_Radio Button 3"))
        (gtk-box-pack-start vbox button :expand t :fill t)
        ;; Put the vbox with the radio buttons in a hbox
        (gtk-box-pack-start hbox vbox :expand nil :fill nil))
      ;; Create three check buttons and put the buttons in a vbox
      (let ((vbox (make-instance 'gtk-vbox
                                 :homogenous nil
                                 :spacing 12
                                 :border-width 12)))
        (gtk-box-pack-start vbox
                            (gtk-check-button-new-with-label "Check Button 1")
                            :expand t :fill t :padding 0)
        (gtk-box-pack-start vbox
                            (gtk-check-button-new-with-label "Check Button 2")
                            :expand t :fill t :padding 0)
        (gtk-box-pack-start vbox
                            (gtk-check-button-new-with-label "Check Button 3")
                            :expand t :fill t :padding 0)
        ;; Put the vbox with the buttons in a hbox
        (gtk-box-pack-start hbox vbox :expand nil :fill nil))
      ;; Put the hbox in a vbox
      (gtk-box-pack-start vbox hbox :expand nil :fill nil)
      ;; Add a separator to the vbox
      (gtk-box-pack-start vbox
                          (make-instance 'gtk-hseparator)
                          :expand nil :fill nil :padding 0)
      ;; Add a quit button to the vbox
      (let ((vbox-quit (make-instance 'gtk-vbox
                                      :homogeneous nil
                                      :spacing 12
                                      :border-width 12))
            (button (make-instance 'gtk-button :label "Close")))
        (gtk-box-pack-start vbox-quit button :expand nil :fill nil)
        (gtk-box-pack-start vbox vbox-quit :expand nil :fill t)
        (g-signal-connect button "clicked"
                          (lambda (button)
                            (declare (ignore button))
                            (gtk-widget-destroy window))))
      ;; Put the vbox in the window widget                     
      (gtk-container-add window vbox)
      (gtk-widget-show window))))

;;; ----------------------------------------------------------------------------
;;;
;;; Chapter 9. Range Widgets
;;;
;;; ----------------------------------------------------------------------------

(defun example-range-widgets ()
  (within-main-loop
    (let* ((window   (make-instance 'gtk-window
                                    :type :toplevel
                                    :title "Example Range Widgets"))
           (box1     (make-instance 'gtk-vbox
                                    :homogeneous nil
                                    :spacing 0))
           (box2     (make-instance 'gtk-hbox
                                    :homogeneous nil
                                    :spacing 12
                                    :border-width 12))
           (box3     (make-instance 'gtk-vbox
                                    :homogeneous nil
                                    :spacing 12))
           (adj1     (make-instance 'gtk-adjustment
                                    :value 0.0
                                    :lower 0.0
                                    :upper 101.0
                                    :step-increment 0.1
                                    :page-increment 1.0
                                    :page-size 1.0))
           (vscale   (make-instance 'gtk-v-scale
                                    :update-policy :continuous
                                    :digits 1
                                    :value-pos :top
                                    :draw-value t
                                    :adjustment adj1))
           (hscale   (make-instance 'gtk-h-scale
                                    :update-policy :continuous
                                    :digits 1
                                    :value-pos :top
                                    :draw-value t
                                    :width-request 200
                                    :height-request -1
                                    :adjustment adj1))
           (scrollbar (make-instance 'gtk-h-scrollbar
                                     :update-policy :continuous
                                     :adjustment adj1)))
      ;; Connect handler for the signal "destroy" to the main window.
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (gtk-main-quit)))
      ;; Packing of the global widgets hscale, vscale, and scrollbar
      (gtk-container-add window box1)
      (gtk-box-pack-start box1 box2 :expand t :fill t :padding 0)
      (gtk-box-pack-start box2 vscale :expand t :fill t :padding 0)
      (gtk-box-pack-start box2 box3 :expand t :fill t :padding 0)
      (gtk-box-pack-start box3 hscale :expand t :fill t :padding 0)
      (gtk-box-pack-start box3 scrollbar :expand t :fill t :padding 0)
      ;; A check button to control whether the value is displayed or not.
      (let ((box (make-instance 'gtk-hbox
                                :homogeneous nil
                                :spacing 12
                                :border-width 12))
            (button (make-instance 'gtk-check-button
                                   :label "Display value on scale widget"
                                   :active t)))
        (g-signal-connect button "toggled"
                          (lambda (widget)
                            (setf (gtk-scale-draw-value hscale)
                                  (gtk-toggle-button-active widget))
                            (setf (gtk-scale-draw-value vscale)
                                  (gtk-toggle-button-active widget))))
        (gtk-box-pack-start box button :expand t :fill t :padding 0)
        (gtk-box-pack-start box1 box :expand t :fill t :padding 0))
      ;; A ComboBox to change the position of the value.
      (let ((box (make-instance 'gtk-hbox
                                :homogeneous nil
                                :spacing 12
                                :border-width 12))
            (combo (make-instance 'gtk-combo-box-text)))
        (gtk-combo-box-text-append-text combo "TOP")
        (gtk-combo-box-text-append-text combo "BOTTOM")
        (gtk-combo-box-text-append-text combo "LEFT")
        (gtk-combo-box-text-append-text combo "RIGHT")
        (gtk-combo-box-set-active combo 0)
        (g-signal-connect combo "changed"
           (lambda (widget)
             (let ((pos (intern (gtk-combo-box-get-active-text widget)
                                :keyword)))
               (gtk-scale-set-value-pos hscale pos)
               (gtk-scale-set-value-pos vscale pos))))
        (gtk-box-pack-start box
                            (make-instance 'gtk-label
                                           :label "Scale value position")
                            :expand nil :fill nil :padding 0)
        (gtk-box-pack-start box combo :expand t :fill t :padding 0)
        (gtk-box-pack-start box1 box :expand t :fill t :padding 0))
      ;; Another ComboBox for the update policy of the scale widgets.
      (let ((box (make-instance 'gtk-hbox
                                :homogeneous nil
                                :spacing 12
                                :border-width 12))
            (combo (make-instance 'gtk-combo-box-text)))
        (gtk-combo-box-text-append-text combo "CONTINUOUS")
        (gtk-combo-box-text-append-text combo "DISCONTINUOUS")
        (gtk-combo-box-text-append-text combo "DELAYED")
        (gtk-combo-box-set-active combo 0)
        (g-signal-connect combo "changed"
           (lambda (widget)
             (let ((policy (intern (gtk-combo-box-get-active-text widget)
                                   :keyword)))
               (setf (gtk-range-update-policy hscale) policy)
               (setf (gtk-range-update-policy vscale) policy))))
        (gtk-box-pack-start box
                            (make-instance 'gtk-label
                                           :label "Scale Update Policy")
                            :expand nil :fill nil :padding 0)
        (gtk-box-pack-start box combo :expand t :fill t :padding 0)
        (gtk-box-pack-start box1 box :expand t :fill t :padding 0))
      ;; Create a scale to change the digits of hscale and vscale.
      (let* ((box (make-instance 'gtk-hbox
                                 :homogeneous nil
                                 :spacing 12
                                 :border-width 12))
             (adj (make-instance 'gtk-adjustment
                                 :value 1.0
                                 :lower 0.0
                                 :upper 5.0
                                 :step-increment 1.0
                                 :page-increment 1.0
                                 :page-size 0.0))
             (scale (make-instance 'gtk-h-scale
                                   :digits 0
                                   :adjustment adj)))
        (g-signal-connect adj "value-changed"
           (lambda (adjustment)
             (setf (gtk-scale-digits hscale)
                   (truncate (gtk-adjustment-value adjustment)))
             (setf (gtk-scale-digits vscale)
                   (truncate (gtk-adjustment-value adjustment)))))
        (gtk-box-pack-start box
                            (make-instance 'gtk-label
                                           :label "Scale Digits:")
                            :expand nil :fill nil :padding 0)
        (gtk-box-pack-start box scale :expand t :fill t :padding 0)
        (gtk-box-pack-start box1 box :expand t :fill t :padding 0))
      ;; Another hscale for adjusting the page size of the scrollbar
      (let* ((box (make-instance 'gtk-hbox
                                 :homogeneous nil
                                 :spacing 12
                                 :border-width 12))
             (adj (make-instance 'gtk-adjustment
                                 :value 1.0
                                 :lower 1.0
                                 :upper 101.0
                                 :step-increment 1.0
                                 :page-increment 1.0
                                 :page-size 0.0))
             (scale (make-instance 'gtk-h-scale
                                   :digits 0
                                   :adjustment adj)))
        (g-signal-connect adj "value-changed"
           (lambda (adjustment)
             (setf (gtk-adjustment-page-size adj1)
                   (gtk-adjustment-page-size adjustment))
             (setf (gtk-adjustment-page-increment adj1)
                   (gtk-adjustment-page-increment adjustment))))
        (gtk-box-pack-start box
                            (make-instance 'gtk-label
                                           :label "Scrollbar Page Size:") 
                            :expand nil :fill nil :padding 0)
        (gtk-box-pack-start box scale :expand t :fill t :padding 0)
        (gtk-box-pack-start box1 box :expand t :fill t :padding 0))
      ;; Add a separator
      (gtk-box-pack-start box1
                          (make-instance 'gtk-hseparator)
                          :expand nil :fill t :padding 0)
      ;; Create the quit button.
      (let ((box (make-instance 'gtk-vbox
                                :homogeneous nil
                                :spacing 12
                                :border-width 12))
            (button (make-instance 'gtk-button :label "Quit")))
        (g-signal-connect button "clicked"
                          (lambda (button)
                            (declare (ignore button))
                            (gtk-widget-destroy window)))
        (gtk-box-pack-start box button :expand t :fill t :padding 0)
        (gtk-box-pack-start box1 box :expand nil :fill t :padding 0))
      (gtk-widget-show window))))

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
                                 :border-width 6))
          (vbox1 (make-instance 'gtk-vbox
                                :homogeneous nil
                                :spacing 6))
          (vbox2 (make-instance 'gtk-vbox
                                :homogenous nil
                                :spacing 6))
          (hbox (make-instance 'gtk-hbox
                               :homogeneous nil
                               :spacing 6)))
      ;; Connect a handler for the signal "destroy" to window.
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (gtk-main-quit)))
      ;; Create a Normal Label
      (let ((frame (make-instance 'gtk-frame
                                  :label "Normal Label"))
            (label (make-instance 'gtk-label
                                  :label "This is a Normal Label")))
        (gtk-container-add frame label)
        (gtk-box-pack-start vbox1 frame :expand nil :fill nil))
      ;; Create a Multi-line Label
      (let ((frame (make-instance 'gtk-frame
                                  :label "Multi-line label"))
            (label (make-instance 'gtk-label
                                  :label
                                  (format nil "This is a Multi-line label~%~
                                               Second line~%~
                                               Third line"))))
        (gtk-container-add frame label)
        (gtk-box-pack-start vbox1 frame :expand nil :fill nil))
      ;; Create a Left Justified Label
      (let ((frame (make-instance 'gtk-frame
                                  :label "Left Justified Label"))
            (label (make-instance 'gtk-label
                                  :justify :left
                                  :label
                                  (format nil
                                          "This is a Left Justified~%~
                                           Multi-line label~%~
                                           Third line"))))
        (gtk-container-add frame label)
        (gtk-box-pack-start vbox1 frame :expand nil :fill nil))
      ;; Create a Right Justified Label
      (let ((frame (make-instance 'gtk-frame
                                  :label "Right Justified Label"))
            (label (make-instance 'gtk-label
                                  :justify :right
                                  :label
                                  (format nil
                                          "This is a Right Justified~%~
                                           Multi-line label~%~
                                           Third line"))))
        (gtk-container-add frame label)
        (gtk-box-pack-start vbox1 frame :expand nil :fill nil))
      ;; Create a Line wrapped label
      (let ((frame (make-instance 'gtk-frame
                                  :label "Line wrapped label"))
                                  
            (label (make-instance 'gtk-label
                                  :wrap t
                                  :label
                                  (format nil
                                          "This is an example of a ~
                                           line-wrapped label.  It should ~
                                           not be taking up the entire width ~
                                           allocated to it, but ~
                                           automatically wraps the words to ~
                                           fit.  The time has come, for all ~
                                           good men, to come to the aid of ~
                                           their party.  The sixth sheik's ~
                                           six sheep's sick.  It supports ~
                                           multiple paragraphs correctly, ~
                                           and correctly adds many extra ~
                                           spaces."))))
        (gtk-container-add frame label)
        (gtk-box-pack-start vbox2 frame :expand nil :fill nil))
      ;; Create a Filled and wrapped label
      (let ((frame (make-instance 'gtk-frame
                                  :label "Filled and wrapped label"))
            (label (make-instance 'gtk-label
                                  :wrap t
                                  :justify :fill
                                  :label
                                  (format nil
                                          "This is an example of a ~
                                           line-wrapped, filled label.  It ~
                                           should be taking up the entire ~
                                           width allocated to it.  Here is a ~
                                           sentence to prove my point.  Here ~
                                           is another sentence.  Here comes ~
                                           the sun, do de do de do.  This ~
                                           is a new paragraph.  This is ~
                                           another newer, longer, better ~
                                           paragraph.  It is coming to an ~
                                           end, unfortunately."))))
        (gtk-container-add frame label)
        (gtk-box-pack-start vbox2 frame :expand nil :fill nil))
      ;; Create an underlined label
      (let ((frame (make-instance 'gtk-frame
                                  :label "Underlined label"))
            (label (make-instance 'gtk-label
                                  :justify :left
                                  :use-underline t
                                  :pattern
             "_________________________ _ _________ _ ______     __ _______ ___"
                                  :label
                                  (format nil
                                          "This label is underlined!~%~
                                           This one is underlined in quite a ~
                                           funky fashion"))))
        (gtk-container-add frame label)
        (gtk-box-pack-start vbox2 frame :expand nil :fill nil))
      ;; Put the boxes into the window and show the window
      (gtk-box-pack-start hbox vbox1 :expand nil :fill nil)
      (gtk-box-pack-start hbox vbox2 :expand nil :fill nil)
      (gtk-container-add window hbox)
      (gtk-widget-show window))))

;;; ----------------------------------------------------------------------------

(defun example-more-labels ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :type :toplevel
                                 :title "Example More Labels"
                                 :default-width 300
                                 :border-width 6))
          (vbox1 (make-instance 'gtk-vbox
                                :homogeneous nil
                                :spacing 6))
          (vbox2 (make-instance 'gtk-vbox
                                :homogeneous nil
                                :spacing 6))
          (hbox (make-instance 'gtk-hbox
                               :homogeneous nil
                               :spacing 6)))
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
                          (make-instance 'gtk-hseparator))
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

(defun create-arrow-button (arrow-type shadow-type)
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

(defun example-arrows ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :type :toplevel
                                 :title "Arrow Buttons"
                                 :default-width 250
                                 :border-width 12))
          (box (make-instance 'gtk-hbox
                              :homogeneous t
                              :spacing 0
                              :border-width 6)))
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (gtk-main-quit)))
      (gtk-box-pack-start box
                          (create-arrow-button :up :in)
                          :expand nil :fill nil :padding 3)
      (gtk-box-pack-start box
                          (create-arrow-button :down :out)
                          :expand nil :fill nil :padding 3)
      (gtk-box-pack-start box
                          (create-arrow-button :left :etched-in)
                          :expand nil :fill nil :padding 3)
      (gtk-box-pack-start box
                          (create-arrow-button :right :etched-out) 
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
                                 :title "Example Progress Bar"
                                 :default-width 300))
          (pdata (make-pbar-data :pbar (make-instance 'gtk-progress-bar)
                                 :mode nil))
          (vbox (make-instance 'gtk-vbox
                               :border-width 12))
          (align (gtk-alignment-new 0.1 0.9 1.0 0.0))
          (table (gtk-table-new 2 3 t)))
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
      (gtk-box-pack-start vbox (make-instance 'gtk-hseparator))
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

;; Dialogs

(defun license-text ()
  (format nil
          "This program is free software: you can redistribute it and/or ~
          modify it under the terms of the GNU Lesser General Public ~
          License for Lisp as published by the Free Software Foundation, ~
          either version 3 of the License, or (at your option) any later ~
          version and with a preamble to the GNU Lesser General Public ~
          License that clarifies the terms for use with Lisp programs and ~
          is referred as the LLGPL.~%~% ~
          This program is distributed in the hope that it will be useful, ~
          but WITHOUT ANY WARRANTY; without even the implied warranty of ~
          MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the ~
          GNU Lesser General Public License for more details. ~%~% ~
          You should have received a copy of the GNU Lesser General Public ~
          License along with this program and the preamble to the Gnu ~
          Lesser General Public License.  If not, see ~
          <http://www.gnu.org/licenses/> and ~
          <http://opensource.franz.com/preamble.html>."))
          
(defun create-dialog ()
  (let ((dialog (make-instance 'gtk-dialog
                               :title "Dialog Window"
                               :has-separator t)))
    ;; Add a border width to the vbox of the content area
    (gtk-container-set-border-width (gtk-dialog-get-content-area dialog) 12)
    ;; Add a label widget with text to the content area
    (let ((vbox (make-instance 'gtk-vbox :border-width 12))
          (label (make-instance 'gtk-label
                                :wrap t
                                :label
                                (format nil
                                        "The content area is the place to ~
                                         put in the widgets.~%~% ~
                                         The action area is separated from ~
                                         the content area with a horizontal ~
                                         line."))))
      (gtk-box-pack-start vbox label)
      (gtk-box-pack-start (gtk-dialog-get-content-area dialog) vbox)
      ;; Show the content area of the dialog
      (gtk-widget-show (gtk-dialog-get-content-area dialog)))
    ;; Add buttons with a stock id to the action area
    (gtk-dialog-add-button dialog "gtk-yes" :yes)
    (gtk-dialog-add-button dialog "gtk-no" :no)
    (gtk-dialog-add-button dialog "gtk-cancel" :cancel)
    (gtk-dialog-set-default-response dialog :cancel)
    ;; Change the order of the buttons
    (gtk-dialog-set-alternative-button-order dialog
                                             (list :yes :cancel :no))
    ;; Run the dialog and print the message on the console
    (format t "Response was: ~S~%" (gtk-dialog-run dialog))
    ;; Destroy the dialog
    (gtk-widget-destroy dialog)))

(defun create-message-dialog ()
  (let ((dialog (make-instance 'gtk-message-dialog
                               :message-type :info
                               :buttons :ok
                               :text "Info Message Dialog"
                               :secondary-text
                               (format nil
                                       "This is a message dialog of type ~
                                        :info with a secondary text."))))
    ;; Run the message dialog    
    (gtk-dialog-run dialog)
    ;; Destroy the message dialog
    (gtk-widget-destroy dialog)))

(defun create-about-dialog ()
  (let ((dialog (make-instance 'gtk-about-dialog
                               :program-name "Example Dialog"
                               :version "0.00"
                               :copyright "(c) Dieter Kaiser"
                               :website
                               "github.com/crategus/cl-cffi-gtk"
                               :website-label "Project web site"
                               :license (license-text)
                               :authors '("Kalyanov Dmitry"
                                          "Dieter Kaiser")
                               :documenters '("Dieter Kaiser")
                               :artists '("None")
                               :logo-icon-name
                               "applications-development"
                               :wrap-license t)))
    ;; Run the about dialog
    (gtk-dialog-run dialog)
    ;; Destroy the about dialog
    (gtk-widget-destroy dialog)))

(defun example-dialog ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :type :toplevel
                                 :title "Example Dialog"
                                 :default-width 250
                                 :border-width 12))
          (vbox (make-instance 'gtk-vbox
                               :spacing 6)))
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget)) 
                          (gtk-main-quit)))
      (gtk-container-add window vbox)
      (let ((button (make-instance 'gtk-button
                                   :label "Open a Dialog Window")))
        (gtk-box-pack-start vbox button)
        (g-signal-connect button "clicked"
           (lambda (widget)
             (declare (ignore widget))
             ;; Create and show the dialog
             (create-dialog))))
      (let ((button (make-instance 'gtk-button
                                   :label "Open a Message Dialog")))
        (gtk-box-pack-start vbox button)
        (g-signal-connect button "clicked"
           (lambda (widget)
             (declare (ignore widget))
             ;; Create and show the message dialog
             (create-message-dialog))))
      (let ((button (make-instance 'gtk-button
                                   :label "Open an About Dialog")))
        (gtk-box-pack-start vbox button)
        (g-signal-connect button "clicked"
           (lambda (widget)
             (declare (ignore widget))
             ;; Create and show the about dialog
             (create-about-dialog))))
      (gtk-box-pack-start vbox
                          (make-instance 'gtk-hseparator))
      ;; Create a quit button
      (let ((button (make-instance 'gtk-button
                                   :label "Quit")))
        (g-signal-connect button "clicked"
                          (lambda (widget)
                            (declare (ignore widget))
                            (gtk-widget-destroy window)))
        (gtk-box-pack-start vbox button))
      (gtk-widget-show window))))

;;; ----------------------------------------------------------------------------

;;; Status Bar

(defun example-statusbar ()
  (within-main-loop
    (let* ((window (make-instance 'gtk-window
                                  :type :toplevel
                                  :title "Example Status Bar"
                                  :default-width 300
                                  :border-width 12))
           (vbox (make-instance 'gtk-vbox
                                :homogeneous nil
                                :spacing 3))
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
        (gtk-box-pack-start vbox button :expand t :fill t :padding 3))
      (let ((button (gtk-button-new-with-label "Pop Item")))
        (g-signal-connect button "clicked"
           (lambda (widget)
             (declare (ignore widget))
             (gtk-statusbar-pop statusbar id)))
        (gtk-box-pack-start vbox button :expand t :fill t :padding 3))
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
                               :spacing 6
                               :border-width 12))
          (vbox1 (make-instance 'gtk-vbox
                                :homogeneous nil
                                :spacing 0
                                :border-width 6))
          (vbox2 (make-instance 'gtk-vbox
                                :homogeneous nil
                                :spacing 0
                                :boder-width 6))
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
                            :expand nil)
        (gtk-box-pack-start vbox spinner :expand nil)
        (gtk-box-pack-start hbox vbox :padding 6))
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
                            :expand nil)
        (gtk-box-pack-start vbox spinner :expand nil)
        (gtk-box-pack-start hbox vbox :padding 6))
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
                            :expand nil)
        (gtk-box-pack-start vbox spinner :expand nil :fill t)
        (gtk-box-pack-start hbox vbox :padding 6))
      (gtk-box-pack-start vbox1 hbox :padding 6)
      (gtk-container-add frame1 vbox1)
      (gtk-box-pack-start vbox frame1)
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
                            :fill t)
        (gtk-box-pack-start vbox spinner1 :expand nil)
        (gtk-box-pack-start hbox vbox :padding 6)
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
                            :expand nil)
        (gtk-box-pack-start vbox spinner2 :expand nil)
        (gtk-box-pack-start hbox vbox :padding 6)
        (gtk-box-pack-start vbox2 hbox :padding 6)
        (let ((check (make-instance 'gtk-check-button
                                    :label "Snap to 0.5-ticks"
                                    :active t)))
          (g-signal-connect check "clicked"
             (lambda (widget)
               (gtk-spin-button-set-snap-to-ticks
                                     spinner1
                                     (gtk-toggle-button-get-active widget))))
          (gtk-box-pack-start vbox2 check))
        (let ((check (make-instance 'gtk-check-button
                                    :label "Numeric only input mode"
                                    :active t)))
          (g-signal-connect check "clicked"
             (lambda (widget)
               (gtk-spin-button-set-numeric
                                     spinner1
                                     (gtk-toggle-button-get-active widget))))
          (gtk-box-pack-start vbox2 check))
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
      (gtk-box-pack-start vbox frame2)
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

;; Combo Box

(defstruct tvi
  title
  value)

(defun example-combo-box ()
  (within-main-loop
    (let* ((window (make-instance 'gtk-window
                                  :type :toplevel
                                  :border-width 12
                                  :title "Example Combo Box"))
           (model (make-instance 'array-list-store))
           (combo-box (make-instance 'gtk-combo-box :model model))
           (title-label (make-instance 'gtk-label :label "Title:"))
           (value-label (make-instance 'gtk-label :label "Value:"))
           (title-entry (make-instance 'gtk-entry))
           (value-entry (make-instance 'gtk-entry))
           (button (make-instance 'gtk-button :label "Add"))
           (table (make-instance 'gtk-table
                                 :n-rows 3
                                 :n-columns 3)))
      ;; Define two columns
      (store-add-column model "gchararray" #'tvi-title)
      (store-add-column model "gint" #'tvi-value)
      ;; Fill in data into the columns
      (store-add-item model (make-tvi :title "Monday" :value 1))
      (store-add-item model (make-tvi :title "Tuesday" :value 2))
      (store-add-item model (make-tvi :title "Wednesday" :value 3))
      (store-add-item model (make-tvi :title "Thursday" :value 4))
      (store-add-item model (make-tvi :title "Friday" :value 5))
      (store-add-item model (make-tvi :title "Saturday" :value 6))
      (store-add-item model (make-tvi :title "Sunday" :value 7))
      ;; Set the first entry to active
      (gtk-combo-box-set-active combo-box 0)
      ;; Define the signal handlers
      (g-signal-connect window "destroy"
                        (lambda (w)
                          (declare (ignore w))
                          (gtk-main-quit)))
      (g-signal-connect button "clicked"
         (lambda (widget)
           (declare (ignore widget))
           (store-add-item model
                           (make-tvi :title
                                     (gtk-entry-text title-entry)
                                     :value
                                     (or (parse-integer
                                           (gtk-entry-text value-entry)
                                           :junk-allowed t)
                                         0)))))
      (g-signal-connect combo-box "changed"
         (lambda (widget)
           (declare (ignore widget))
           (show-message (format nil "You clicked on row ~A~%"
                                 (gtk-combo-box-get-active combo-box)))))
      ;; Create renderers for the cells
      (let ((renderer (make-instance 'gtk-cell-renderer-text
                                     :text "A text")))
        (gtk-cell-layout-pack-start combo-box renderer :expand t)
        (gtk-cell-layout-add-attribute combo-box renderer "text" 0))
      (let ((renderer (make-instance 'gtk-cell-renderer-text
                                     :text "A number")))
        (gtk-cell-layout-pack-start combo-box renderer :expand nil)
        (gtk-cell-layout-add-attribute combo-box renderer "text" 1))
      ;; Align the labels
      (gtk-misc-set-alignment title-label 0.0 0.0)
      (gtk-misc-set-alignment value-label 0.0 0.0)
      ;; Put the widgets into the table
      (gtk-table-attach table title-label 0 1 0 1)
      (gtk-table-attach table value-label 1 2 0 1)
      (gtk-table-attach table title-entry 0 1 1 2)
      (gtk-table-attach table value-entry 1 2 1 2)
      (gtk-table-attach table button      2 3 1 2)
      (gtk-table-attach table combo-box   0 3 2 3)
      ;; Put the table into the window
      (gtk-container-add window table)
      ;; Show the window
      (gtk-widget-show window))))

;;; ----------------------------------------------------------------------------

;;; Combo Box Text

(defun example-combo-box-text ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :border-width 12
                                 :title "Example Combo Box Text"))
          (combo (make-instance 'gtk-combo-box-text)))
      (gtk-combo-box-text-append-text combo "First entry")
      (gtk-combo-box-text-append-text combo "Second entry")
      (gtk-combo-box-text-append-text combo "Third entry")
      (gtk-combo-box-set-active combo 0)
      (gtk-container-add window combo)
      (gtk-widget-show window))))

;;; ----------------------------------------------------------------------------

;; Calendar

(defun calendar-detail (calendar year month day)
  (declare (ignore calendar year month))
  (when (= day 23)
    "!"))

(defun example-calendar ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :title "Example Calendar"
                                 :type :toplevel
                                 :default-width 250
                                 :default-height 100))
          (calendar (make-instance 'gtk-calendar
                                   :detail-function #'calendar-detail)))
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (gtk-main-quit)))
      (g-signal-connect calendar "day-selected"
                        (lambda (widget)
                          (declare (ignore widget))
                          (format t "selected: year ~A month ~A day ~A~%"
                                  (gtk-calendar-year calendar)
                                  (gtk-calendar-month calendar)
                                  (gtk-calendar-day calendar))))
      (gtk-container-add window calendar)
      (gtk-widget-show window))))

;;; ----------------------------------------------------------------------------

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

;;; ----------------------------------------------------------------------------

;;; File Chooser Button

(defun example-file-chooser-button ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :title "Example File Chooser Button"
                                 :type :toplevel
                                 :border-width 12
                                 :default-width 300
                                 :default-height 100))
          (button (make-instance 'gtk-file-chooser-button
                                 :action :open)))
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (gtk-main-quit)))
      (g-signal-connect button "file-set"
                        (lambda (widget)
                          (declare (ignore widget))
                          (format t "File set: ~A~%"
                                  (gtk-file-chooser-filename button))))
      (gtk-container-add window button)
      (gtk-widget-show window))))
      
;;; ----------------------------------------------------------------------------

;;; File Chooser Dialog

(defun example-file-chooser-dialog ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :title "Example File Chooser Dialog"
                                 :type :toplevel
                                 :border-width 12
                                 :default-width 300
                                 :default-height 100))
          (button (make-instance 'gtk-button
                                 :label "Select a file for save ..."
                                 :image
                                 (gtk-image-new-from-stock "gtk-save"
                                                           :button))))
      ;; Handle the signal "destroy" for the window.
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (gtk-main-quit)))
      ;; Handle the signal "clicked" for the button.
      (g-signal-connect button "clicked"
         (lambda (widget)
           (declare (ignore widget))
           (let ((dialog (make-instance 'gtk-file-chooser-dialog
                                        :action :save
                                        :title "Choose file to save")))
             (gtk-dialog-add-button dialog "gtk-save" :accept)
             (gtk-dialog-add-button dialog "gtk-cancel" :cancel)
             (when (eq (gtk-dialog-run dialog) :accept)
               (format t "saved to file ~A~%"
                       (gtk-file-chooser-filename dialog)))
             (gtk-widget-destroy dialog))))
      (gtk-container-add window button)
      (gtk-widget-show window))))

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
                                 :default-width 250
                                 :border-width 12))
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

;;; Alignment widget

(defun example-alignment ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :type :toplevel
                                 :title "Example Alignment"
                                 :default-width 300
                                 :default-height 200
                                 :border-width 12))
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
    (gtk-fixed-move fixed button (random width) (random height))))

(defun example-fixed ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :type :toplevel
                                 :title "Example Fixed Container"
                                 :default-width 300
                                 :default-height 200
                                 :border-width 12))
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

;;; Frames

(defun example-frame ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :type :toplevel
                                 :title "Example Frame"
                                 :default-width 250
                                 :default-height 200
                                 :border-width 12))
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
                                 :border-width 12))
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
                                 :border-width 12))
          (vpaned (make-instance 'gtk-v-paned))
          (frame1 (make-instance 'gtk-frame :label "Frame 1"))
          (frame2 (make-instance 'gtk-frame :label "Frame 2")))
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (gtk-main-quit)))
      (gtk-widget-set-size-request window 350 300)
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
                                   :border-width 12
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
      (gtk-box-pack-start (gtk-dialog-get-content-area window) scrolled)
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
        (gtk-box-pack-start (gtk-dialog-get-action-area window) button)
        (gtk-widget-grab-default button))
      (gtk-widget-show window))))

;;; ----------------------------------------------------------------------------

;;; Button Boxes

(defun create-bbox (horizontal title spacing layout)
  (let ((frame (make-instance 'gtk-frame
                              :label title))
        (bbox (make-instance (if horizontal
                                 'gtk-hbutton-box
                                 'gtk-vbutton-box)
                             :border-width 6
                             :layout-style layout
                             :spacing spacing)))
  (gtk-container-add bbox (gtk-button-new-from-stock "gtk-ok"))
  (gtk-container-add bbox (gtk-button-new-from-stock "gtk-cancel"))
  (gtk-container-add bbox (gtk-button-new-from-stock "gtk-help"))
  (gtk-container-add frame bbox)
  frame))

(defun example-button-box ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :type :toplevel
                                 :title "Example Button Box"
                                 :border-width 12))
          (vbox1 (make-instance 'gtk-vbox
                                :homogeneous nil
                                :spacing 0))
          (vbox2 (make-instance 'gtk-vbox
                                :homogeneous nil
                                :spacing 0))
          (hbox (make-instance 'gtk-hbox
                               :homogeneous nil
                               :spacing 0)))
      ;; Set gtk-button-images to T. This allows buttons with text and image.
      (setf (gtk-settings-gtk-button-images (gtk-settings-get-default)) t)      
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (gtk-main-quit)))
      ;; Create Horizontal Button Boxes
      (gtk-box-pack-start vbox1
                          (make-instance 'gtk-label
                                         :ypad 6
                                         :xalign 0
                                         :use-markup t
                                         :label
                                         "<b>Horizontal Button Boxes</b>")
                          :expand nil
                          :fill nil)
      ;; Create the first Horizontal Box
      (gtk-box-pack-start vbox2
                          (create-bbox t "Spread (spacing 12)" 12 :spread))
      ;; Create the second Horizontal Box
      (gtk-box-pack-start vbox2
                          (create-bbox t "Edge (spacing 12)" 12 :edge))
      ;; Create the third Horizontal Box
      (gtk-box-pack-start vbox2
                          (create-bbox t "Start (spacing 6)" 6 :start))
      ;; Create the fourth Horizontal Box
      (gtk-box-pack-start vbox2
                          (create-bbox t "End (spacing 6)" 6 :end))
      (gtk-box-pack-start vbox1 vbox2)
      ;; Create Vertical Button Boxes
      (gtk-box-pack-start vbox1
                          (make-instance 'gtk-label
                                         :ypad 12
                                         :xalign 0
                                         :use-markup t
                                         :label
                                         "<b>Vertical Button Boxes</b>")
                          :expand nil
                          :fill nil)
      ;; Create the first Vertical Box
      (gtk-box-pack-start hbox
                          (create-bbox nil "Spread (spacing 12)" 12 :spread))
      ;; Create the second Vertical Box
      (gtk-box-pack-start hbox
                          (create-bbox nil "Edge (spacing 12)" 12 :edge))
      ;; Create the third Vertical Box
      (gtk-box-pack-start hbox
                          (create-bbox nil "Start (spacing 6)" 6 :start))
      ;; Create the fourth Vertical Box
      (gtk-box-pack-start hbox
                          (create-bbox nil "End (spacing 6)" 6 :end))
      (gtk-box-pack-start vbox1 hbox)
      (gtk-container-add window vbox1)
      (gtk-widget-show window))))

;;; ----------------------------------------------------------------------------

;;; Toolbar - Example is not finished

(defun example-toolbar ()
  (within-main-loop
    (let ((dialog (make-instance 'gtk-dialog
                                 :title "Example GtkToolbar"
                                 :width-request 600
                                 :height-request 300
                                 :allow-shring t))
          (handle-box (make-instance 'gtk-handle-box))
          (toolbar (make-instance 'gtk-toolbar
                                  :toolbar-style :both
                                  :border-width 6)))
      (g-signal-connect dialog "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (gtk-main-quit)))
      (gtk-widget-realize dialog)
      (gtk-box-pack-start (gtk-dialog-get-content-area dialog)
                          handle-box
                          :expand nil
                          :fill nil
                          :padding 6)
      (gtk-container-add handle-box toolbar)
      (gtk-toolbar-insert toolbar
                          (make-instance 'gtk-tool-button
                                         :label "Close"
                                         :icon-widget
                                         (gtk-image-new-from-file "gtk-logo.png"))
                          0)
      
      (gtk-widget-show dialog))))

;;; ----------------------------------------------------------------------------
;;;
;;; Menu Widget
;;;
;;; ----------------------------------------------------------------------------

(defun example-menu ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :type :toplevel
                                 :default-width 200
                                 :default-height 200
                                 :title "Example Menu Widget"))
          (vbox (make-instance 'gtk-vbox)))
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (gtk-main-quit)))
      ;; Create the menu-bar and the items of the menu-bar.
      (let ((menubar (make-instance 'gtk-menu-bar
                                    :visible t
                                    :can-focus nil))
            ;; Item file of the menu-bar.
            (item-file (make-instance 'gtk-menu-item
                                      :label "_Datei"
                                      :use-underline t))
            ;; Item edit of the menu-bar.
            (item-edit (make-instance 'gtk-menu-item
                                      :label "_Bearbeiten"
                                      :use-underline t))
            ;; Item help of the menu-bar.
            (item-help (make-instance 'gtk-menu-item
                                      :label "_Hilfe"
                                      :use-underline t)))
        ;; Create submenu for the item file.
        (let ((submenu (make-instance 'gtk-menu
                                      :visible t
                                      :can-focus nil))
              (item-file-new (make-instance 'gtk-image-menu-item
                                       :label "gtk-new"
                                       :use-underline t
                                       :use-stock t))
              (item-file-open (make-instance 'gtk-image-menu-item
                                        :label "gtk-open"
                                        :use-underline t
                                        :use-stock t))
              (item-file-save (make-instance 'gtk-image-menu-item
                                        :label "gtk-save"
                                        :user-underline t
                                        :use-stock t))
              (item-file-save-as (make-instance 'gtk-image-menu-item
                                                :label "gtk-save-as"
                                                :user-underline t
                                                :use-stock t))
              (item-file-quit (make-instance 'gtk-image-menu-item
                                             :label "gtk-quit"
                                             :user-underline t
                                             :use-stock t)))
          ;; Add the items to to the submenu.
          (gtk-menu-shell-append submenu item-file-new)
          (gtk-menu-shell-append submenu item-file-open)
          (gtk-menu-shell-append submenu item-file-save)
          (gtk-menu-shell-append submenu item-file-save-as)
          ;; Insert a GtkSeparatorMenuItem.
          (gtk-menu-shell-append submenu (gtk-separator-menu-item-new))
          ;; Add the item file quit to the submenu
          (gtk-menu-shell-append submenu item-file-quit)
          ;; Set the submenu of the item file.
          (gtk-menu-item-set-submenu item-file submenu))
        ;; Create submenu for the item edit.
        (let ((submenu (make-instance 'gtk-menu
                                      :visible t
                                      :can-focus nil))
              (item-edit-cut (make-instance 'gtk-image-menu-item
                                            :label "gtk-cut"
                                            :use-underline t
                                            :use-stock t))
              (item-edit-copy (make-instance 'gtk-image-menu-item
                                             :label "gtk-copy"
                                             :use-underline t
                                             :use-stock t))
              (item-edit-paste (make-instance 'gtk-image-menu-item
                                              :label "gtk-paste"
                                              :user-underline t
                                              :use-stock t))
              (item-edit-delete (make-instance 'gtk-image-menu-item
                                               :label "gtk-delete"
                                               :user-underline t
                                               :use-stock t)))
          ;; Add the items to to the submenu.
          (gtk-menu-shell-append submenu item-edit-cut)
          (gtk-menu-shell-append submenu item-edit-copy)
          (gtk-menu-shell-append submenu item-edit-paste)
          (gtk-menu-shell-append submenu item-edit-delete)
          ;; Set the submenu of the item edit.
          (gtk-menu-item-set-submenu item-edit submenu))
        ;; Create submenu for the item help.
        (let ((submenu (make-instance 'gtk-menu
                                      :visible t
                                      :can-focus nil))
              (item-help-about (make-instance 'gtk-image-menu-item
                                              :label "gtk-about"
                                              :use-underline t
                                              :use-stock t)))
          ;; Add the items to to the submenu.
          (gtk-menu-shell-append submenu item-help-about)
          ;; Set the submenu of the item help.
          (gtk-menu-item-set-submenu item-help submenu))
        ;; Add the items file, edit, and help into the menubar.
        (gtk-menu-shell-append menubar item-file)
        (gtk-menu-shell-append menubar item-edit)
        (gtk-menu-shell-append menubar item-help)
        ;; Pack the menubar into the vbox.
        (gtk-box-pack-start vbox menubar))
      ;; Pack the vbox into the window.
      (gtk-container-add window vbox)
      ;; Show the window.
      (gtk-widget-show window))))

(defun example-menu-builder ()
  (within-main-loop
    (let ((builder (make-instance 'gtk-builder)))
      (gtk-builder-add-from-file builder "example-menu-builder.ui")
      (g-signal-connect (gtk-builder-get-object builder "window") "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (gtk-main-quit)))
      (gtk-widget-show (gtk-builder-get-object builder "window")))))

;;; ----------------------------------------------------------------------------

;;; Multiline Text Editing Widget

(defun example-1 ()
  (within-main-loop
    (let* ((window (make-instance 'gtk-window
                                  :title "Simple Multiline Text Input"
                                  :default-width 350
                                  :default-height 200))
           (vbox (make-instance 'gtk-vbox
                                :homogeneous nil
                                :spacing 3))
           (text-view (make-instance 'gtk-text-view))
           (buffer (gtk-text-view-get-buffer text-view))
           (button (make-instance 'gtk-button
                                  :label "Close")))
      (g-signal-connect window "destroy"
                        (lambda (window)
                          (declare (ignore window))
                          (gtk-main-quit)))
      (g-signal-connect button "clicked"
         (lambda (button)
           (declare (ignore button))
           (let* ((start (gtk-text-buffer-get-start-iter buffer))
                  (end   (gtk-text-buffer-get-end-iter buffer))
                  (text  (gtk-text-buffer-get-text buffer start end)))
             (format t "~A~%" text)
             (gtk-widget-destroy window))))
      (gtk-container-add window vbox)
      (gtk-box-pack-start vbox text-view)
      (gtk-text-buffer-set-text buffer "Hello Text View")
      (gtk-box-pack-start vbox button :expand nil :fill nil)
      (gtk-widget-show window))))

;;; ----------------------------------------------------------------------------

(defun on-button-clicked (buffer tag)
  (multiple-value-bind (start end)
      (gtk-text-buffer-get-selection-bounds buffer)
    (gtk-text-buffer-apply-tag-by-name buffer tag start end)))
    
(defun example-2 ()
  (within-main-loop
    (let* ((window (make-instance 'gtk-window
                                  :title "Multiline Text Input"
                                  :type :toplevel
                                  :default-width 200
                                  :default-height 200))
           (vbox (make-instance 'gtk-vbox))
           (bbox (make-instance 'gtk-hbutton-box))
           (text-view (make-instance 'gtk-text-view))
           (buffer (gtk-text-view-get-buffer text-view)))
      (g-signal-connect window "destroy"
                               (lambda (widget)
                                 (declare (ignore widget))
                                 (gtk-main-quit)))
      (gtk-box-pack-start vbox bbox)
      (gtk-box-pack-start vbox text-view)
      (gtk-text-buffer-set-text buffer "Hello World Text View")
      ;; Create tags associated with the buffer.
      (gtk-text-tag-table-add (gtk-text-buffer-get-tag-table buffer)
                              (make-instance 'gtk-text-tag
                                             :name "bold"
                                             :weight +pango-weight-bold+))
      (gtk-text-tag-table-add (gtk-text-buffer-get-tag-table buffer)
                              (make-instance 'gtk-text-tag
                                             :name "italic"
                                             :style :pango-style-italic))
      (gtk-text-tag-table-add (gtk-text-buffer-get-tag-table buffer)
                              (make-instance 'gtk-text-tag
                                             :name "font"
                                             :font "fixed"))
      ;; Create button for bold.
      (let ((button (make-instance 'gtk-button :label "Bold")))
        (g-signal-connect button "clicked"
           (lambda (widget)
             (declare (ignore widget))
             (on-button-clicked buffer "bold")))
        (gtk-container-add bbox button))
      ;; Create button for italic.
      (let ((button (make-instance 'gtk-button :label "Italic")))
        (g-signal-connect button "clicked"
           (lambda (widget)
             (declare (ignore widget))
             (on-button-clicked buffer "italic")))
        (gtk-container-add bbox button))
      ;; Create button for fixed font.
      (let ((button (make-instance 'gtk-button :label "Font Fixed")))
        (g-signal-connect button "clicked"
           (lambda (widget)
             (declare (ignore widget))
             (on-button-clicked buffer "font")))
        (gtk-container-add bbox button))
      ;; Create the close button.   
      (let ((button (make-instance 'gtk-button :label "Close")))
        (g-signal-connect button "clicked"
                          (lambda (widget)
                            (declare (ignore widget))
                            (gtk-widget-destroy window)))
        (gtk-box-pack-start vbox button))
      (gtk-container-add window vbox)
      (gtk-widget-show window))))

;;; ----------------------------------------------------------------------------

(defun example-3 ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :title "Multiline Text Search"
                                 :type :toplevel))
          (entry (make-instance 'gtk-entry))
          (button (make-instance 'gtk-button
                                 :label "Search"))
          (scrolled (make-instance 'gtk-scrolled-window))
          (text-view (make-instance 'gtk-text-view))
          (vbox (make-instance 'gtk-vbox))
          (hbox (make-instance 'gtk-hbox)))
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (gtk-main-quit)))
      (g-signal-connect button "clicked"
         (lambda (widget)
           (declare (ignore widget))
           (let* ((text (gtk-entry-get-text entry))
                  (buffer (gtk-text-view-get-buffer text-view))
                  (iter (gtk-text-buffer-get-start-iter buffer)))
             (multiple-value-bind (found start end)
                 (gtk-text-iter-search iter text)
               (when found
                 (gtk-text-buffer-select-range buffer start end))))))
      (gtk-container-add scrolled text-view)
      (gtk-box-pack-start hbox entry)
      (gtk-box-pack-start hbox button)
      (gtk-box-pack-start vbox hbox)
      (gtk-box-pack-start vbox scrolled)
      (gtk-container-add window vbox)
      (gtk-widget-show window))))
      
;;; ----------------------------------------------------------------------------

(defun find-text (text-view text iter)
  (let ((buffer (gtk-text-view-get-buffer text-view)))
    (multiple-value-bind (found start end)
        (gtk-text-iter-search iter text)
      (when found
        (gtk-text-buffer-select-range buffer start end)
        (let ((last-pos (gtk-text-buffer-create-mark buffer "last-pos" end)))
          (gtk-text-view-scroll-mark-onscreen text-view last-pos))))))

(defun example-4 ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :title "Multiline Text Search"
                                 :type :toplevel))
          (entry (make-instance 'gtk-entry))
          (button-search (make-instance 'gtk-button
                                        :label "Search"))
          (button-next (make-instance 'gtk-button
                                      :label "Next"))
          (scrolled (make-instance 'gtk-scrolled-window))
          (text-view (make-instance 'gtk-text-view))
          (vbox (make-instance 'gtk-vbox))
          (hbox (make-instance 'gtk-hbox)))
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (gtk-main-quit)))
      (g-signal-connect button-search "clicked"
         (lambda (widget)
           (declare (ignore widget))
           (let* ((text (gtk-entry-get-text entry))
                  (buffer (gtk-text-view-get-buffer text-view))
                  (iter (gtk-text-buffer-get-start-iter buffer)))
             (find-text text-view text iter))))
      (g-signal-connect button-next "clicked"
         (lambda (widget)
           (declare (ignore widget))
           (let* ((text (gtk-entry-get-text entry))
                  (buffer (gtk-text-view-get-buffer text-view))
                  (last-pos (gtk-text-buffer-get-mark buffer "last-pos")))
             (when last-pos
               (find-text text-view
                          text
                          (gtk-text-buffer-get-iter-at-mark buffer
                                                            last-pos))))))
               
      
      (gtk-container-add scrolled text-view)
      (gtk-box-pack-start hbox entry)
      (gtk-box-pack-start hbox button-search)
      (gtk-box-pack-start hbox button-next)
      (gtk-box-pack-start vbox hbox)
      (gtk-box-pack-start vbox scrolled)
      (gtk-container-add window vbox)
      (gtk-widget-show window))))

;;; ----------------------------------------------------------------------------
