;;; ----------------------------------------------------------------------------
;;; tutorial.lisp
;;;
;;; Copyright (C) 2011 - 2021 Dieter Kaiser
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

(defpackage :gtk-tutorial
  (:use :gtk :gdk :gdk-pixbuf :gobject
        :glib :gio :pango :cairo :cffi :common-lisp)
  (:export ;; Chapter Multiline Text Widget
           #:example-text-view-attributes
           #:example-text-view-find-next
           #:example-text-view-insert
           #:example-text-view-insert-image
           #:example-text-view-insert-widget
           #:example-text-view-search
           #:example-text-view-simple
           #:example-text-view-tags
           #:example-text-view-tooltip
           ;; Chapter Tree and List Widgets
           #:example-cell-renderer-properties
           #:example-tree-view-content-type
           #:example-tree-view-context-menu
           #:example-tree-view-drag-and-drop
           #:example-tree-view-dump-model
           #:example-tree-view-editable
           #:example-tree-view-example
           #:example-tree-view-path
           #:example-tree-view-simple
           #:example-tree-view-sortable
           #:example-icon-view
           ;; Chapter Selecting, Colors, Files and Fonts
           #:example-color-button-label
           #:example-color-chooser-dialog
           #:example-file-chooser-button
           #:example-font-button-label
  ))

(in-package :gtk-tutorial)

;; Create a sample string without line breaks
(defvar *some-text*
        (format nil "One of the important things to remember about text in ~
                     GTK+ is that it is in the UTF-8 encoding. This means that ~
                     one character can be encoded as multiple bytes. Character ~
                     counts are usually referred to as offsets, while byte ~
                     counts are called indexes. If you confuse these two, ~
                     things will work fine with ASCII, but as soon as your ~
                     buffer contains multibyte characters, bad things will ~
                     happen."))

(defvar *lorem-ipsum-short*
        (format nil "Lorem ipsum dolor sit amet, consectetur adipiscing elit. ~
Nunc scelerisque aliquam dui id ullamcorper. Sed placerat felis sed aliquam ~
sodales. Cras et ultricies nulla. Nullam ipsum ante, gravida vel malesuada ac, ~
sollicitudin eu diam. Morbi pellentesque elit et odio hendrerit dignissim. ~
Maecenas sagittis auctor leo a dictum. Sed at auctor."))

(defvar *lorem-ipsum-long*
        (format nil "Lorem ipsum dolor sit amet, consectetur adipiscing elit. ~
Morbi vitae condimentum ligula, vitae bibendum urna. Praesent vitae nisi ~
hendrerit lorem malesuada interdum vitae vitae massa. Integer elementum justo ~
nibh, non euismod odio tincidunt et. Praesent lobortis molestie mi quis ~
rhoncus. Interdum et malesuada fames ac ante ipsum primis in faucibus. ~
Curabitur luctus, tortor vel ornare aliquet, erat nulla tempus orci, ac ~
pulvinar velit turpis ac nulla. Orci varius natoque penatibus et magnis dis ~
parturient montes, nascetur ridiculus mus. Nam efficitur scelerisque erat. ~
Nunc nec viverra magna, eget consequat dui. Vestibulum vitae porttitor quam. ~
Fusce leo enim, molestie non sollicitudin sollicitudin, porta vel libero.

In hac habitasse platea dictumst. In ultricies nulla vel massa varius, eu ~
tempor metus condimentum. Duis nisl tortor, vestibulum ut auctor eu, tristique ~
lobortis libero. Nam congue volutpat leo a hendrerit. In ut purus ac risus ~
aliquet commodo in sit amet ante. Aenean sed tempus dolor. Aliquam a sagittis ~
metus. Donec eget urna eu justo fringilla tincidunt id et diam. Maecenas ~
ultrices pellentesque augue vitae rhoncus. Integer aliquet venenatis elit sed ~
lacinia. Praesent dui libero, aliquet imperdiet blandit ut, sollicitudin id ~
ipsum. Pellentesque venenatis vitae sem non fermentum. Ut orci libero, ~
interdum a pharetra at, mollis a mi.

Integer tempus cursus fringilla. Donec ornare fermentum nulla sed aliquet. ~
Mauris in velit metus. Quisque in diam id diam bibendum eleifend vitae id ~
tortor. Nulla condimentum ultricies ultrices. Nunc tincidunt, justo at blandit ~
condimentum, leo purus mollis orci, sed mollis dui metus eget eros. Mauris ~
quam nibh, laoreet eget arcu in, accumsan lacinia purus. Morbi aliquet nibh id ~
sem venenatis, vitae ultricies arcu laoreet."))

;;; Lisp utility functions for the examples

;; Get the absolute filename of a file for a ASDF loadable package

(defun system-path (filename package)
  (let ((system-path (asdf:system-source-directory package)))
    (princ-to-string (merge-pathnames filename system-path))))

;; Recursivly apply CSS to a widget an all child widgets

(defun apply-css-to-widget (widget provider)
  (gtk-style-context-add-provider (gtk-widget-style-context widget)
                                  provider
                                  +gtk-style-provider-priority-user+)
  (when (g-type-is-a (g-type-from-instance widget) "GtkContainer")
    (gtk-container-forall widget
                          (lambda (widget)
                            (apply-css-to-widget widget provider)))))

;;; ----------------------------------------------------------------------------
;;;
;;; Chapter 2. Getting started
;;;
;;; ----------------------------------------------------------------------------

(defun example-simple-window ()
  (within-main-loop
    (let (;; Create a toplevel window.
          (window (gtk-window-new :toplevel)))
      ;; Signal handler for the window to handle the signal "destroy".
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))
      ;; Show the window.
      (gtk-widget-show-all window))))

(defun example-simple-window-2 ()
  (within-main-loop
    (let (;; Create a toplevel window.
          (window (gtk-window-new :toplevel)))
      ;; Signal handler for the window to handle the signal "destroy".
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))
      ;; Show the window.
      (gtk-widget-show-all window)))
  (join-gtk-main))

(defun example-simple-message ()
  (let ((response))
    (within-main-loop
      (let ((dialog (make-instance 'gtk-message-dialog
                                   :message-type :info
                                   :buttons :ok
                                   :text "Info Message Dialog"
                                   :secondary-text
                                   (format nil
                                           "This is a message dialog of type ~
                                            :info with a secondary text."))))
        ;; Signal handler for the dialog to handle the signal "destroy".
        (g-signal-connect dialog "destroy"
                          (lambda (widget)
                            (declare (ignore widget))
                            (leave-gtk-main)))
        ;; Signal handler for the dialog to handle the signal "response".
        (g-signal-connect dialog "response"
                          (lambda (dialog response-id)
                            (setf response response-id)
                            (gtk-widget-destroy dialog)))
        (gtk-widget-show dialog)))
    (join-gtk-main)
    (format t "Back from message dialog with response-id ~A~%" response)))

(defun example-getting-started ()
  (within-main-loop
    (let (;; Create a toplevel window with a title and a default width.
          (window (make-instance 'gtk-window
                                 :type :toplevel
                                 :title "Getting started"
                                 :default-width 250)))
      ;; Signal handler for the window to handle the signal "destroy".
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))
      ;; Show the window.
      (gtk-widget-show-all window))))

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
                          (leave-gtk-main)))
      ;; Signal handler for the window to handle the signal "delete-event".
      (g-signal-connect window "delete-event"
                        (lambda (widget event)
                          (declare (ignore widget event))
                          (format t "Delete Event Occured.~%")
                          +gdk-event-stop+))
      ;; Put the button into the window.
      (gtk-container-add window button)
      ;; Show the window and the button.
      (gtk-widget-show-all window))))

;;; ----------------------------------------------------------------------------

;;; An Upgraded Hello World

(defun example-upgraded-hello-world ()
  (within-main-loop
    (let ((window (gtk-window-new :toplevel))
          (box (gtk-box-new :horizontal 6))
          (button  nil))
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))
      (setf (gtk-window-title window) "Hello Buttons")
      (setf (gtk-window-default-size window) '(250 75))
      (setf (gtk-container-border-width window) 12)
      (setf button (gtk-button-new-with-label "Button 1"))
      (g-signal-connect button "clicked"
                        (lambda (widget)
                          (declare (ignore widget))
                          (format t "Button 1 was pressed.~%")))
      (gtk-box-pack-start box button :expand t :fill t :padding 0)
      (setf button (gtk-button-new-with-label "Button 2"))
      (g-signal-connect button "clicked"
                        (lambda (widget)
                          (declare (ignore widget))
                          (format t "Button 2 was pressed.~%")))
      (gtk-box-pack-start box button :expand t :fill t :padding 0)
      (gtk-container-add window box)
      (gtk-widget-show-all window))))

;;; ----------------------------------------------------------------------------

(defun example-upgraded-hello-world-2 ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :type :toplevel
                                 :title "Hello Buttons"
                                 :default-width 250
                                 :default-height 75
                                 :border-width 12))
          (box (make-instance 'gtk-box
                              :orientation :horizontal
                              :spacing 6)))
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))
      (let ((button (gtk-button-new-with-label "Button 1")))
        (g-signal-connect button "clicked"
                          (lambda (widget)
                            (declare (ignore widget))
                            (format t "Button 1 was pressed.~%")))
        (gtk-box-pack-start box button))
      (let ((button (gtk-button-new-with-label "Button 2")))
        (g-signal-connect button "clicked"
                          (lambda (widget)
                            (declare (ignore widget))
                            (format t "Button 2 was pressed.~%")))
        (gtk-box-pack-start box button))
      (gtk-container-add window box)
      (gtk-widget-show-all window))))

;;; ----------------------------------------------------------------------------

;; Drawing in response to input

(let ((surface nil))
  (defun example-drawing ()
    (within-main-loop
      (let ((window (make-instance 'gtk-window
                                   :type :toplevel
                                   :title "Example Drawing"))
            (frame (make-instance 'gtk-frame
                                  :shadow-type :in))
            (area (make-instance 'gtk-drawing-area
                                 :width-request 250
                                 :height-request 200)))
        (g-signal-connect window "destroy"
                          (lambda (widget)
                            (declare (ignore widget))
                            (leave-gtk-main)))
        ;; Signals used to handle the backing surface
        (g-signal-connect area "draw"
           (lambda (widget cr)
             (declare (ignore widget))
             (let ((cr (pointer cr)))
               (cairo-set-source-surface cr surface 0.0 0.0)
               (cairo-paint cr)
               (cairo-destroy cr)
               +gdk-event-propagate+)))
        (g-signal-connect area "configure-event"
           (lambda (widget event)
             (declare (ignore event))
             (when surface
               (cairo-surface-destroy surface))
             (setf surface
                   (gdk-window-create-similar-surface
                                   (gtk-widget-window widget)
                                   :color
                                   (gtk-widget-allocated-width widget)
                                   (gtk-widget-allocated-height widget)))
             ;; Clear surface
             (let ((cr (cairo-create surface)))
               (cairo-set-source-rgb cr 1.0 1.0 1.0)
               (cairo-paint cr)
               (cairo-destroy cr))
             (format t "leave event 'configure-event'~%")
             +gdk-event-stop+))
        ;; Event signals
        (g-signal-connect area "motion-notify-event"
           (lambda (widget event)
             (format t "MOTION-NOTIFY-EVENT ~A~%" event)
             (when (member :button1-mask (gdk-event-motion-state event))
               (let ((cr (cairo-create surface))
                     (x (gdk-event-motion-x event))
                     (y (gdk-event-motion-y event)))
                 (cairo-rectangle cr (- x 3.0) (- y 3.0) 6.0 6.0)
                 (cairo-fill cr)
                 (cairo-destroy cr)
                 (gtk-widget-queue-draw-area widget
                                             (truncate (- x 3.0))
                                             (truncate (- y 3.0))
                                             6
                                             6)))
             ;; We have handled the event, stop processing
             +gdk-event-stop+))
        (g-signal-connect area "button-press-event"
           (lambda (widget event)
             (format t "BUTTON-PRESS-EVENT ~A~%" event)
             (if (eql 1 (gdk-event-button-button event))
                 (let ((cr (cairo-create surface))
                       (x (gdk-event-button-x event))
                       (y (gdk-event-button-y event)))
                   (cairo-rectangle cr (- x 3.0) (- y 3.0) 6.0 6.0)
                   (cairo-fill cr)
                   (cairo-destroy cr)
                   (gtk-widget-queue-draw-area widget
                                               (truncate (- x 3.0))
                                               (truncate (- y 3.0))
                                               6
                                               6))
                 ;; Clear surface
                 (let ((cr (cairo-create surface)))
                   (cairo-set-source-rgb cr 1.0 1.0 1.0)
                   (cairo-paint cr)
                   (cairo-destroy cr)
                   (gtk-widget-queue-draw widget)))))
        (gtk-widget-add-events area
                               '(:button-press-mask
                                 :pointer-motion-mask))
        (gtk-container-add frame area)
        (gtk-container-add window frame)
        (gtk-widget-show-all window)))))

;;; ----------------------------------------------------------------------------
;;;
;;; A second approach to get started
;;;
;;; ----------------------------------------------------------------------------

(defun example-simple-app ()
  (within-main-loop
    (let ((app (gtk-application-new "com.crategus.example-simple-app" :none)))
      (g-signal-connect app "activate"
                            (lambda (app)
                              (let ((window (gtk-application-window-new app)))
                                (setf (gtk-window-title window) "Window")
                                (setf (gtk-window-default-size window)
                                      '(200 200))
                                (gtk-widget-show-all window))))
      (g-application-run app nil))))

;;; ----------------------------------------------------------------------------
;;;
;;; Chapter 3. Packing Widgets
;;;
;;; ----------------------------------------------------------------------------

;;; Packing Demonstrations Program

(defun make-box (homogeneous spacing expand fill padding)
  (let ((box (make-instance 'gtk-box
                            :orientation :horizontal
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

(defun example-box-packing (&optional (spacing 0))
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :title "Example Box Packing"
                                 :type :toplevel
                                 :border-width 12))
          (vbox (make-instance 'gtk-box
                               :orientation :vertical
                               :spacing 6))
          (button (make-instance 'gtk-button
                                 :label "Quit"))
          (quitbox (make-instance 'gtk-box
                                  :orientation :horizontal)))
      (g-signal-connect button "clicked"
                        (lambda (widget)
                          (declare (ignore widget))
                          (gtk-widget-destroy window)))
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))
      (gtk-box-pack-start vbox
                          (make-instance 'gtk-label
                                         :label
                                         (format nil
                                          "GtkBox   ~
                                           :orientation :horizontal   ~
                                           :homogeneous nil   ~
                                           :spacing ~A"
                                          spacing)
                                         :xalign 0)
                          :expand nil)
      (gtk-box-pack-start vbox
                          (make-box nil spacing nil nil 0)
                          :expand nil)
      (gtk-box-pack-start vbox
                          (make-box nil spacing t nil 0)
                          :expand nil)
      (gtk-box-pack-start vbox
                          (make-box nil spacing t t 0)
                          :expand nil)
      (gtk-box-pack-start vbox
                          (make-instance 'gtk-label
                                         :label
                                         (format nil
                                            "GtkBox   ~
                                             :orientation :horizontal   ~
                                             :homogeneous t   ~
                                             :spacing ~A"
                                            spacing)
                                         :xalign 0)
                          :expand nil
                          :padding 6)
      (gtk-box-pack-start vbox
                          (make-box t spacing t nil 0)
                          :expand nil)
      (gtk-box-pack-start vbox
                          (make-box t spacing t t 0)
                          :expand nil)
      (gtk-box-pack-start vbox
                          (gtk-separator-new :horizontal)
                          :expand nil
                          :padding 6)
      ;; Align the quit-button on the right side
      (gtk-box-pack-end quitbox button :expand nil)
      (gtk-box-pack-start vbox quitbox :expand nil)
      (gtk-container-add window vbox)
      (gtk-widget-show-all window))))

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
                          (leave-gtk-main)))
      (g-signal-connect quit "clicked"
                        (lambda (widget)
                          (declare (ignore widget))
                          (gtk-widget-destroy window)))
      (gtk-table-attach table button1 0 1 0 1)
      (gtk-table-attach table button2 1 2 0 1)
      (gtk-table-attach table quit    0 2 1 2)
      (gtk-container-add window table)
      (gtk-widget-show-all window))))

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
                          (leave-gtk-main)))
      (g-signal-connect button1 "toggled"
         (lambda (widget)
           (if (gtk-toggle-button-active widget)
               (progn
                 (gtk-table-set-row-spacings table 12)
                 (setf (gtk-button-label widget) "Less Row Spacing"))
               (progn
                 (gtk-table-set-row-spacings table 0)
                 (setf (gtk-button-label widget) "More Row Spacing")))))
      (g-signal-connect button2 "toggled"
         (lambda (widget)
           (if (gtk-toggle-button-active widget)
               (progn
                 (gtk-table-set-col-spacings table 12)
                 (setf (gtk-button-label widget) "Less Col Spacing"))
               (progn
                 (gtk-table-set-col-spacings table 0)
                 (setf (gtk-button-label widget) "More Col Spacing")))))
      (g-signal-connect quit "clicked"
                        (lambda (widget)
                          (declare (ignore widget))
                          (gtk-widget-destroy window)))
      (gtk-table-attach table button1 0 1 0 1)
      (gtk-table-attach table button2 1 2 0 1)
      (gtk-table-attach table quit    0 2 1 2)
      (gtk-container-add window table)
      (gtk-widget-show-all window))))

;;; ----------------------------------------------------------------------------

;;; Grid Packing Example

(defun make-grid (homogeneous spacing expand align margin)
  (let ((box (make-instance 'gtk-grid
                            :orientation :horizontal
                            :column-homogeneous homogeneous
                            :column-spacing spacing)))
    (gtk-container-add box
                       (make-instance 'gtk-button
                                      :label "gtk-container-add"
                                      :hexpand expand
                                      :halgin align
                                      :margin margin))
    (gtk-container-add box
                       (make-instance 'gtk-button
                                      :label "box"
                                      :hexpand expand
                                      :halign align
                                      :margin margin))
    (gtk-container-add box
                       (make-instance 'gtk-button
                                      :label "button"
                                      :hexpand expand
                                      :halign align
                                      :margin margin))
    (gtk-container-add box
                       (make-instance 'gtk-button
                                      :label (if expand "T" "NIL")
                                      :hexpand expand
                                      :halign align
                                      :margin margin))
    (gtk-container-add box
                       (make-instance 'gtk-button
                                      :label (format nil "~A" align)
                                      :hexpand expand
                                      :halign align
                                      :margin margin))
    (gtk-container-add box
                       (make-instance 'gtk-button
                                      :label (format nil "~A" margin)
                                      :hexpand expand
                                      :halign align
                                      :margin margin))
    box))

(defun example-grid-packing (&optional (spacing 0))
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :title "Example Grid Packing"
                                 :type :toplevel
                                 :border-width 12
                                 :default-height 200
                                 :default-width 300))
          (vbox (make-instance 'gtk-grid
                               :orientation :vertical
                               :row-spacing 6))
          (button (make-instance 'gtk-button
                                 :label "Quit"))
          (quitbox (make-instance 'gtk-box
                                  :orientation :horizontal)))
      (g-signal-connect button "clicked"
                        (lambda (widget)
                          (declare (ignore widget))
                          (gtk-widget-destroy window)))
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))
      (gtk-container-add vbox
                         (make-instance 'gtk-label
                                        :label
                                        (format nil
                                         "GtkGrid homogeneous nil spacing ~A"
                                         spacing)
                                        :xalign 0
                                        :yalign 0
                                        :vexpand nil
                                        :valign :start))
      (gtk-container-add vbox (gtk-separator-new :horizontal))
      (gtk-container-add vbox (make-grid nil spacing nil :center 0))
      (gtk-container-add vbox (make-grid nil spacing t :center 0))
      (gtk-container-add vbox (make-grid nil spacing t :fill 0))
      (gtk-container-add vbox (gtk-separator-new :horizontal))
      (gtk-container-add vbox
                         (make-instance 'gtk-label
                                        :label
                                        (format nil
                                           "GtkGrid homogeneous t spacing ~A"
                                           spacing)
                                         :xalign 0
                                         :yalign 0
                                         :vexpand nil
                                         :valign :start
                                         :margin 6))
      (gtk-container-add vbox (gtk-separator-new :horizontal))
      (gtk-container-add vbox (make-grid t spacing t :center 0))
      (gtk-container-add vbox (make-grid t spacing t :fill 0))
      (gtk-container-add vbox (gtk-separator-new :horizontal))
      (gtk-container-add quitbox button)
      (gtk-container-add vbox quitbox)
      (gtk-container-add window vbox)
      (gtk-widget-show-all window))))

;;; ----------------------------------------------------------------------------

(defun example-grid-packing-2 ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :type :toplevel
                                 :title "Example Grid Packing"
                                 :border-width 12
                                 :default-width 300))
          (grid (make-instance 'gtk-grid
                                :column-homogeneous t
                                :row-homogeneous t))
          (button1 (make-instance 'gtk-button
                                  :label "Button 1"))
          (button2 (make-instance 'gtk-button
                                  :label "Button 2"))
          (quit (make-instance 'gtk-button
                               :label "Quit")))
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))
      (g-signal-connect quit "clicked"
                        (lambda (widget)
                          (declare (ignore widget))
                          (gtk-widget-destroy window)))
      (gtk-grid-attach grid button1 0 1 1 1)
      (gtk-grid-attach grid button2 1 1 1 1)
      (gtk-grid-attach grid quit    0 2 2 1)
      (gtk-container-add window grid)
      (gtk-widget-show-all window))))

;;; ----------------------------------------------------------------------------
;;;
;;; Chapter 4. Button Widgets
;;;
;;; ----------------------------------------------------------------------------

;;; Normal Buttons

(defun image-label-box (filename text)
  (let ((box (make-instance 'gtk-box
                            :orientation :horizontal
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
                          (leave-gtk-main)))
      (gtk-container-add button box)
      (gtk-container-add window button)
      (gtk-widget-show-all window))))

;;; ----------------------------------------------------------------------------

(defun example-more-buttons ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :title "Example Buttons"
                                 :type :toplevel
                                 :default-width 250
                                 :border-width 12))
          (vbox1 (make-instance 'gtk-box
                                :orientation :vertical
                                :spacing 6))
          (vbox2 (make-instance 'gtk-box
                                :orientation :vertical
                                :spacing 6))
          (hbox  (make-instance 'gtk-box
                                :orientation :horizontal
                                :spacing 6)))
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))
      ;; These are the standard functions to create a button.
      (gtk-box-pack-start vbox1
                          (gtk-button-new-with-label "Label"))
      (gtk-box-pack-start vbox1
                          (gtk-button-new-with-mnemonic "_Mnemonic"))
      (gtk-box-pack-start vbox1
                          (gtk-button-new-from-icon-name "gtk-apply" :button))
      ;; Create some buttons with make-instance.
      (gtk-box-pack-start vbox2
                          (make-instance 'gtk-button
                                         :image-position :left
                                         :always-show-image t
                                         :image
                                         (make-instance 'gtk-image
                                                        :icon-name "gtk-edit")
                                         :label "Bearbeiten"))
      (gtk-box-pack-start vbox2
                          (make-instance 'gtk-button
                                         :image-position :top
                                         :always-show-image t
                                         :image
                                         (make-instance 'gtk-image
                                                        :icon-name "gtk-cut")
                                         :label "Ausschneiden"))
      (gtk-box-pack-start vbox2
                          (make-instance 'gtk-button
                                         :image-position :bottom
                                         :always-show-image t
                                         :image
                                         (make-instance 'gtk-image
                                                        :icon-name "gtk-cancel")
                                         :label "Abbrechen"))
      (gtk-box-pack-start hbox vbox1)
      (gtk-box-pack-start hbox vbox2)
      (gtk-container-add window hbox)
      (gtk-widget-show-all window))))

;;; ----------------------------------------------------------------------------

;;; Toggle Buttons

(defun example-toggle-buttons ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :title "Example Toggle Buttons"
                                 :type :toplevel))
          (vbox (make-instance 'gtk-box
                               :orientation :vertical))
          (hbox (make-instance 'gtk-box
                               :orientation :horizontal)))
      ;; Handler for the signal "destroy"
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))
      ;; Create three radio buttons and put the buttons in a vbox
      (let ((vbox (make-instance 'gtk-box
                                 :orientation :vertical
                                 :spacing 12
                                 :border-width 12))
            (button (gtk-radio-button-new-with-label nil "Radio Button 1")))
        (gtk-box-pack-start vbox button)
        (setq button
              (gtk-radio-button-new-with-label
                                          (gtk-radio-button-get-group button)
                                          "Radio Button 2"))
        (setf (gtk-toggle-button-active button) t)
        (gtk-box-pack-start vbox button)
        (setq button
              (gtk-radio-button-new-with-mnemonic
                                          (gtk-radio-button-get-group button)
                                          "_Radio Button 3"))
        (gtk-box-pack-start vbox button)
        ;; Put the vbox with the radio buttons in a hbox
        (gtk-box-pack-start hbox vbox :expand nil :fill nil))
      ;; Create three check buttons and put the buttons in a vbox
      (let ((vbox (make-instance 'gtk-box
                                 :orientation :vertical
                                 :homogeneous nil
                                 :spacing 12
                                 :border-width 12)))
        (gtk-box-pack-start vbox
                            (gtk-check-button-new-with-label "Check Button 1"))
        (gtk-box-pack-start vbox
                            (gtk-check-button-new-with-label "Check Button 2"))
        (gtk-box-pack-start vbox
                            (gtk-check-button-new-with-label "Check Button 3"))
        ;; Put the vbox with the buttons in a hbox
        (gtk-box-pack-start hbox vbox :expand nil :fill nil))
      ;; Put the hbox in a vbox
      (gtk-box-pack-start vbox hbox :expand nil :fill nil)
      ;; Add a separator to the vbox
      (gtk-box-pack-start vbox
                          (make-instance 'gtk-separator
                                         :orientation :horizontal)
                          :expand nil :fill nil)
      ;; Add a quit button to the vbox
      (let ((vbox-quit (make-instance 'gtk-box
                                      :orientation :vertical
                                      :spacing 12
                                      :border-width 12))
            (button (make-instance 'gtk-button :label "Close")))
        (gtk-box-pack-start vbox-quit button :expand nil :fill nil)
        (gtk-box-pack-start vbox vbox-quit :expand nil)
        (g-signal-connect button "clicked"
                          (lambda (button)
                            (declare (ignore button))
                            (gtk-widget-destroy window))))
      ;; Put the vbox in the window widget
      (gtk-container-add window vbox)
      (gtk-widget-show-all window))))

;;; ----------------------------------------------------------------------------

;;; Link button

(defun example-link-button ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :type :toplevel
                                 :title "Example Link Button"
                                 :default-width 270
                                 :border-width 12))
          (grid (make-instance 'gtk-grid
                               :orientation :vertical
                               :row-spacing 6
                               :column-homogeneous t)))
      (g-signal-connect window "destroy"
                               (lambda (widget)
                                 (declare (ignore widget))
                                 (leave-gtk-main)))
      (gtk-container-add grid
                         (make-instance 'gtk-label
                                        :use-markup t
                                        :label
                                        "<b>Link Button with url</b>"))
      (gtk-container-add grid
                         (gtk-link-button-new "http://www.gtk.org/"))
      (gtk-container-add grid
                         (make-instance 'gtk-label
                                        :use-markup t
                                        :label
                                        "<b>Link Button with Label</b>"))
      (gtk-container-add grid
                         (gtk-link-button-new-with-label
                                                        "http://www.gtk.org/"
                                                        "Project WebSite"))
      (gtk-container-add window grid)
      (gtk-widget-show-all window))))

;;; ----------------------------------------------------------------------------

;; Switch

(defun example-switch ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :type :toplevel
                                 :title "Example Switch"
                                 :default-width 230
                                 :border-width 12))
          (switch (make-instance 'gtk-switch
                                 :active t))
          (label (make-instance 'gtk-label
                                :label "The Switch is ON"))
          (grid (make-instance 'gtk-grid
                               :orientation :vertical
                               :row-spacing 6
                               :column-homogeneous t)))
      (g-signal-connect window "destroy"
                               (lambda (widget)
                                 (declare (ignore widget))
                                 (leave-gtk-main)))
      (g-signal-connect switch "notify::active"
         (lambda (widget param)
           (declare (ignore param))
           (if (gtk-switch-active widget)
               (setf (gtk-label-label label) "The Switch is ON")
               (setf (gtk-label-label label) "The Switch is OFF"))))
      (gtk-container-add grid switch)
      (gtk-container-add grid label)
      (gtk-container-add window grid)
      (gtk-widget-show-all window))))

;;; ----------------------------------------------------------------------------
;;;
;;; Chapter 5. Display Widgets
;;;
;;; ----------------------------------------------------------------------------

;; Labels

(defun make-heading (text)
  (make-instance 'gtk-label
                 :xalign 0
                 :use-markup t
                 :label (format nil "<b>~A</b>" text)))

(defun example-labels ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :type :toplevel
                                 :title "Example Labels"
                                 :default-width 250
                                 :border-width 12))
          (vbox1 (make-instance 'gtk-box
                                :orientation :vertical
                                :spacing 6))
          (vbox2 (make-instance 'gtk-box
                                :orientation :vertical
                                :spacing 6))
          (hbox (make-instance 'gtk-box
                               :orientation :horizontal
                               :spacing 12)))
      ;; Connect a handler for the signal "destroy" to window.
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))
      ;; Create a Normal Label
      (gtk-box-pack-start vbox1
                          (make-heading "Normal Label:")
                          :expand nil)
      (gtk-box-pack-start vbox1
                          (make-instance 'gtk-label
                                         :label "This is a Normal Label")
                          :expand nil)
      ;; Create a Multi-line Label
      (gtk-box-pack-start vbox1
                          (make-heading "Multi-line Label:")
                          :expand nil)
      (gtk-box-pack-start vbox1
                          (make-instance 'gtk-label
                                         :label
                                         (format nil
                                               "This is a Multi-line label~%~
                                                Second line~%~
                                                Third line"))
                          :expand nil)
      ;; Create a Left Justified Label
      (gtk-box-pack-start vbox1
                          (make-heading "Left Justified Label:")
                          :expand nil)
      (gtk-box-pack-start vbox1
                          (make-instance 'gtk-label
                                         :justify :left
                                         :label
                                         (format nil
                                                 "This is a Left Justified~%~
                                                  Multi-line label~%~
                                                  Third line"))
                          :expand nil)
      ;; Create a Right Justified Label
      (gtk-box-pack-start vbox1
                          (make-heading "Right Justified Label:")
                          :expand nil)
      (gtk-box-pack-start vbox1
                          (make-instance 'gtk-label
                                         :justify :right
                                         :label
                                         (format nil
                                                "This is a Right Justified~%~
                                                 Multi-line label~%~
                                                 Third line"))
                          :expand nil)
      ;; Create a Line wrapped label
      (gtk-box-pack-start vbox2
                          (make-heading "Line Wrapped Label:")
                          :expand nil)
      (gtk-box-pack-start vbox2
                          (make-instance 'gtk-label
                                         :wrap t
                                         :label
                                         (format nil
                                          "This is an example of a ~
                                           line-wrapped label.  It should ~
                                           not be taking up the entire ~
                                           width allocated to it, but ~
                                           automatically wraps the words to ~
                                           fit.  The time has come, for all ~
                                           good men, to come to the aid of ~
                                           their party.  The sixth sheik's ~
                                           six sheep's sick.  It supports ~
                                           multiple paragraphs correctly, ~
                                           and correctly adds many extra ~
                                           spaces."))
                          :expand nil)
      ;; Create a Filled and wrapped label
      (gtk-box-pack-start vbox2
                          (make-heading "Filled and Wrapped Label:")
                          :expand nil)
      (gtk-box-pack-start vbox2
                          (make-instance 'gtk-label
                                         :wrap t
                                         :justify :fill
                                         :label
                                         (format nil
                                          "This is an example of a ~
                                           line-wrapped, filled label.  It ~
                                           should be taking up the entire ~
                                           width allocated to it.  Here is ~
                                           a sentence to prove my point.  ~
                                           Here is another sentence.  Here ~
                                           comes the sun, do de do de do.  ~
                                           This  is a new paragraph.  This ~
                                           is  another newer, longer, ~
                                           better  paragraph.  It is coming ~
                                           to an end, unfortunately."))
                          :expand nil)
      ;; Create an underlined label
      (gtk-box-pack-start vbox2
                          (make-heading "Underlined Label:")
                          :expand nil)
      (gtk-box-pack-start vbox2
                          (make-instance 'gtk-label
                                         :justify :left
                                         :use-underline t
                                         :pattern
          "_________________________ _ _________ _ ______     __ _______ ___"
                                         :label
                                         (format nil
                                          "This label is underlined!~%~
                                           This one is underlined in quite ~
                                           a funky fashion"))
                          :expand nil)
      ;; Put the boxes into the window and show the window
      (gtk-box-pack-start hbox vbox1 :expand nil)
      (gtk-box-pack-start hbox (gtk-separator-new :vertical))
      (gtk-box-pack-start hbox vbox2 :expand nil)
      (gtk-container-add window hbox)
      (gtk-widget-show-all window))))

;;; ----------------------------------------------------------------------------

(defun example-more-labels ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :type :toplevel
                                 :title "GTK+ 3.4 Example More Labels"
                                 :default-width 300
                                 :border-width 6))
          (vbox1 (make-instance 'gtk-box
                                :orientation :vertical
                                :homogeneous nil
                                :spacing 6))
          (vbox2 (make-instance 'gtk-box
                                :orientation :vertical
                                :homogeneous nil
                                :spacing 6))
          (hbox (make-instance 'gtk-box
                               :orientation :horizontal
                               :homogeneous nil
                               :spacing 6)))
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))
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
                          (make-instance 'gtk-separator
                                         :orientation :horizontal))
      (gtk-box-pack-start vbox2
                          (gtk-label-new "Normal Label"))
      (gtk-box-pack-start vbox2
                          (gtk-label-new-with-mnemonic "With _Mnemonic"))
      (gtk-box-pack-start vbox2
                          (make-instance 'gtk-label
                                         :label "This Label is Selectable"
                                         :selectable t))
      (gtk-box-pack-start vbox2
                          (make-instance 'gtk-label
                                         :label
                                         "<small>Small text</small>"
                                          :use-markup t))
      (gtk-box-pack-start vbox2
                          (make-instance 'gtk-label
                                         :label
                                         "<b>Bold text</b>"
                                          :use-markup t))
      (gtk-box-pack-start vbox2
                          (make-instance 'gtk-label
                                         :use-markup t
                                         :label
                                         (format nil
                                         "Go to the ~
                                         <a href=\"http://gtk.org/\">~
                                         GTK+ Website</a> for more ...")))
      (gtk-container-add window vbox2)
      (gtk-widget-show-all window))))

;;; ----------------------------------------------------------------------------

;;; Progress Bar

(defstruct pbar-data
  pbar
  timer
  mode)

(defun progress-bar-timeout (pdata)
  (if (pbar-data-mode pdata)
      (gtk-progress-bar-pulse (pbar-data-pbar pdata))
      (let ((val (+ (gtk-progress-bar-fraction (pbar-data-pbar pdata))
                    0.01)))
        (when (> val 1.0) (setq val 0.0))
        (setf (gtk-progress-bar-fraction (pbar-data-pbar pdata)) val)))
  t)

(defun example-progress-bar ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :type :toplevel
                                 :title "Example Progress Bar"
                                 :default-width 300))
          (pdata (make-pbar-data :pbar (make-instance 'gtk-progress-bar)))
          (vbox (make-instance 'gtk-box
                               :orientation :vertical
                               :border-width 12
                               :spacing 12))
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
                          (leave-gtk-main)))
      (gtk-box-pack-start vbox align)
      (gtk-container-add align (pbar-data-pbar pdata))
      (gtk-box-pack-start vbox table)
      (let ((check (gtk-check-button-new-with-mnemonic "_Show text")))
        (g-signal-connect check "clicked"
           (lambda (widget)
             (declare (ignore widget))
             (let ((text (gtk-progress-bar-text (pbar-data-pbar pdata))))
               (if (or (null text) (zerop (length text)))
                   (setf (gtk-progress-bar-text (pbar-data-pbar pdata))
                         "Some text")
                   (setf (gtk-progress-bar-text (pbar-data-pbar pdata)) ""))
               (setf (gtk-progress-bar-show-text (pbar-data-pbar pdata))
                     (gtk-toggle-button-active check)))))
        (gtk-table-attach table check 0 1 0 1))
      (let ((check (gtk-check-button-new-with-label "Activity mode")))
        (g-signal-connect check "clicked"
           (lambda (widget)
             (declare (ignore widget))
             (setf (pbar-data-mode pdata)
                   (not (pbar-data-mode pdata)))
             (if (pbar-data-mode pdata)
                 (gtk-progress-bar-pulse (pbar-data-pbar pdata))
                 (setf (gtk-progress-bar-fraction (pbar-data-pbar pdata))
                       0.0))))
        (gtk-table-attach table check 0 1 1 2))
      (let ((check (gtk-check-button-new-with-label "Inverted")))
        (g-signal-connect check "clicked"
           (lambda (widget)
             (declare (ignore widget))
             (setf (gtk-progress-bar-inverted (pbar-data-pbar pdata))
                   (gtk-toggle-button-active check))))
        (gtk-table-attach table check 0 1 2 3))
      (let ((button (gtk-button-new-with-label "Close")))
        (g-signal-connect button "clicked"
                          (lambda (widget)
                            (declare (ignore widget))
                            (gtk-widget-destroy window)))
        (gtk-box-pack-start vbox button))
      (gtk-container-add window vbox)
      (gtk-widget-show-all window))))

;;; ----------------------------------------------------------------------------

;;; Status Bar

(defun example-statusbar ()
  (within-main-loop
    (let* ((window (make-instance 'gtk-window
                                  :type :toplevel
                                  :title "Example Status Bar"
                                  :default-width 300
                                  :border-width 12))
           (vbox (make-instance 'gtk-box
                                :orientation :vertical
                                :homogeneous nil
                                :spacing 3))
           (statusbar (make-instance 'gtk-statusbar))
           (id (gtk-statusbar-context-id statusbar "Example Status Bar"))
           (count 0))
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))
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
      (gtk-widget-show-all window))))


;;; ----------------------------------------------------------------------------

;;; Info Bar

(defun example-info-bar ()
  (within-main-loop
    (let* ((window (make-instance 'gtk-window
                                  :type :toplevel
                                  :title "Example Info bar"
                                  :border-width 12
                                  :default-width 250))
           (grid (make-instance 'gtk-grid))
           (info-bar (make-instance 'gtk-info-bar
                                    :no-show-all t))
           (message (make-instance 'gtk-label
                                   :label ""))
           (content (gtk-info-bar-content-area info-bar)))
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))
      (gtk-widget-show message)
      ;; Add a label to the content area of the info bar
      (gtk-container-add content message)
      ;; Add a button OK to the action area
      (gtk-info-bar-add-button info-bar "gtk-ok" 1)
      ;; Add two more buttons to the action area
      (gtk-info-bar-add-buttons info-bar "gtk-cancel" 2
                                         "gtk-no" 3)
      ;; Connect a handler for the "response" signal of the info bar
      (g-signal-connect info-bar "response"
         (lambda (widget response-id)
           (declare (ignore widget))
           (format t "response-id is ~A~%" response-id)
           (gtk-widget-hide info-bar)))
      (gtk-grid-attach grid info-bar 0 2 1 1)
      ;; Show the info bar
      (setf (gtk-label-text message) "An Info Message in the content area.")
      (setf (gtk-info-bar-message-type info-bar) :info)
      (gtk-widget-show info-bar)
      ;; Add the container grid to the window and show all
      (gtk-container-add window grid)
      (gtk-widget-show-all window))))

;;; ----------------------------------------------------------------------------

;;;   Accel Labels

;;; Example 49. Creating a simple menu item with an accelerator key.

(defun example-accel-label()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :type :toplevel))
          (menu (gtk-menu-new))
          (accel-group (gtk-accel-group-new))
          (save-item (gtk-menu-item-new-with-label "Save"))
          )
      (gtk-window-add-accel-group window accel-group)
      (gtk-container-add menu save-item)
      (gtk-widget-add-accelerator save-item
                                  "activate"
                                  accel-group
                                  (gdk-keyval-from-name "s")
                                  :control-mask
                                  :visible)
      (gtk-widget-show-all window))))

;;; GtkWidget *save_item;
;;; GtkAccelGroup *accel_group;
;;;
;;; /* Create a GtkAccelGroup and add it to the window. */
;;; accel_group = gtk_accel_group_new ();
;;; gtk_window_add_accel_group (GTK_WINDOW (window), accel_group);
;;;
;;; /* Create the menu item using the convenience function. */
;;; save_item = gtk_menu_item_new_with_label ("Save");
;;; gtk_widget_show (save_item);
;;; gtk_container_add (GTK_CONTAINER (menu), save_item);
;;;
;;; /* Now add the accelerator to the GtkMenuItem. Note that since we called
;;;    gtk_menu_item_new_with_label() to create the GtkMenuItem the
;;;    GtkAccelLabel is automatically set up to display the GtkMenuItem
;;;    accelerators. We just need to make sure we use GTK_ACCEL_VISIBLE here. */
;;; gtk_widget_add_accelerator (save_item, "activate", accel_group,
;;;                             GDK_KEY_s, GDK_CONTROL_MASK, GTK_ACCEL_VISIBLE);


;;; ----------------------------------------------------------------------------
;;;
;;; Chapter 7. Range Widgets
;;;
;;; ----------------------------------------------------------------------------

(defun example-range-widgets ()
  (within-main-loop
    (let* ((window (make-instance 'gtk-window
                                  :type :toplevel
                                  :title "Example Range Widgets"))
           (box1 (make-instance 'gtk-box
                                :orientation :vertical
                                :homogeneous nil
                                :spacing 0))
           (box2 (make-instance 'gtk-box
                                :orientation :horizontal
                                :homogeneous nil
                                :spacing 12
                                :border-width 12))
           (box3 (make-instance 'gtk-box
                                :orientation :vertical
                                :homogeneous nil
                                :spacing 12))
           (adj1 (make-instance 'gtk-adjustment
                                :value 0.0
                                :lower 0.0
                                :upper 101.0
                                :step-increment 0.1
                                :page-increment 1.0
                                :page-size 1.0))
           (vscale (make-instance 'gtk-scale
                                  :orientation :vertical
                                  :digits 1
                                  :value-pos :top
                                  :draw-value t
                                  :adjustment adj1))
           (hscale (make-instance 'gtk-scale
                                   :orientation :horizontal
                                   :digits 1
                                   :value-pos :top
                                   :draw-value t
                                   :width-request 200
                                   :height-request -1
                                   :adjustment adj1))
           (scrollbar (make-instance 'gtk-scrollbar
                                     :orientation :horizontal
                                     :adjustment adj1)))
      ;; Connect a handler for the signal "destroy" to the main window.
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))
      ;; Packing of the global widgets hscale, vscale, and scrollbar
      (gtk-container-add window box1)
      (gtk-box-pack-start box1 box2)
      (gtk-box-pack-start box2 vscale)
      (gtk-box-pack-start box2 box3)
      (gtk-box-pack-start box3 hscale)
      (gtk-box-pack-start box3 scrollbar)
      ;; A check button to control whether the value is displayed or not.
      (let ((box (make-instance 'gtk-box
                                :orientation :horizontal
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
        (gtk-box-pack-start box button)
        (gtk-box-pack-start box1 box))
      ;; A ComboBox to change the position of the value.
      (let ((box (make-instance 'gtk-box
                                :orientation :horizontal
                                :homogeneous nil
                                :spacing 12
                                :border-width 12))
            (combo (make-instance 'gtk-combo-box-text)))
        (gtk-combo-box-text-append-text combo "TOP")
        (gtk-combo-box-text-append-text combo "BOTTOM")
        (gtk-combo-box-text-append-text combo "LEFT")
        (gtk-combo-box-text-append-text combo "RIGHT")
        (setf (gtk-combo-box-active combo) 0)
        (g-signal-connect combo "changed"
           (lambda (widget)
             (let ((pos (gtk-combo-box-text-active-text widget)))
               (format t "type      : ~A~%"
                         (g-type-from-instance (pointer widget)))
               (format t "active is : ~A~%"
                         (gtk-combo-box-active widget))
               (setq pos (if pos (intern pos :keyword) :top))
               (setf (gtk-scale-value-pos hscale) pos)
               (setf (gtk-scale-value-pos vscale) pos))))
        (gtk-box-pack-start box
                            (make-instance 'gtk-label
                                           :label "Scale value position")
                            :expand nil :fill nil :padding 0)
        (gtk-box-pack-start box combo)
        (gtk-box-pack-start box1 box))
      ;; Create a scale to change the digits of hscale and vscale.
      (let* ((box (make-instance 'gtk-box
                                 :orientation :horizontal
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
             (scale (make-instance 'gtk-scale
                                   :orientation :horizontal
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
                            :expand nil :fill nil)
        (gtk-box-pack-start box scale)
        (gtk-box-pack-start box1 box))
      ;; Another hscale for adjusting the page size of the scrollbar
      (let* ((box (make-instance 'gtk-box
                                 :orientation :horizontal
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
             (scale (make-instance 'gtk-scale
                                   :orientation :horizontal
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
                            :expand nil :fill nil)
        (gtk-box-pack-start box scale)
        (gtk-box-pack-start box1 box))
      ;; Add a separator
      (gtk-box-pack-start box1
                          (make-instance 'gtk-separator
                                         :orientation :horizontal)
                          :expand nil :fill t)
      ;; Create the quit button.
      (let ((box (make-instance 'gtk-box
                                :orientation :vertical
                                :homogeneous nil
                                :spacing 12
                                :border-width 12))
            (button (make-instance 'gtk-button :label "Quit")))
        (g-signal-connect button "clicked"
                          (lambda (button)
                            (declare (ignore button))
                            (gtk-widget-destroy window)))
        (gtk-box-pack-start box button)
        (gtk-box-pack-start box1 box :expand nil))
      (gtk-widget-show-all window))))

;;; ----------------------------------------------------------------------------
;;;
;;; Chapter 8. Layout Widgets
;;;
;;; ----------------------------------------------------------------------------

;;; Alignment widget

(defun example-alignment ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :type :toplevel
                                 :title "Example Alignment"
                                 :border-width 12
                                 :width-request 300
                                 :height-request 300))
          (grid (make-instance 'gtk-grid
                                :column-spacing 12
                                :column-homogeneous t
                                :row-spacing 12
                                :row-homogeneous t)))
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))
      (let ((frame (make-instance 'gtk-frame
                                  :label "xalign: 0, yalign: 0"))
            (button (make-instance 'gtk-button
                                   :label "Button"))
            (alignment (make-instance 'gtk-alignment
                                      :xalign 0.00
                                      :yalign 0.00
                                      :xscale 0.50
                                      :yscale 0.25)))
        (gtk-alignment-set-padding alignment 6 6 6 6)
        (gtk-container-add alignment button)
        (gtk-container-add frame alignment)
        (gtk-grid-attach grid frame 0 1 1 1))
      (let ((frame (make-instance 'gtk-frame
                                  :label "xalign: 0, yalign: 1"))
            (button (make-instance 'gtk-button
                                   :label "Button"))
            (alignment (make-instance 'gtk-alignment
                                      :xalign 0.00
                                      :yalign 1.00
                                      :xscale 0.50
                                      :yscale 0.25)))
        (gtk-alignment-set-padding alignment 6 6 6 6)
        (gtk-container-add alignment button)
        (gtk-container-add frame alignment)
        (gtk-grid-attach grid frame 1 1 1 1))
      (let ((frame (make-instance 'gtk-frame
                                  :label "xalign: 1, yalign: 0"))
            (button (make-instance 'gtk-button
                                   :label "Button"))
            (alignment (make-instance 'gtk-alignment
                                      :xalign 1.00
                                      :yalign 0.00
                                      :xscale 0.50
                                      :yscale 0.25)))
        (gtk-alignment-set-padding alignment 6 6 6 6)
        (gtk-container-add alignment button)
        (gtk-container-add frame alignment)
        (gtk-grid-attach grid frame 0 2 1 1))
      (let ((frame (make-instance 'gtk-frame
                                  :label "xalign: 1, yalign: 1"))
            (button (make-instance 'gtk-button
                                   :label "Button"))
            (alignment (make-instance 'gtk-alignment
                                      :xalign 1.00
                                      :yalign 1.00
                                      :xscale 0.50
                                      :yscale 0.25)))
        (gtk-alignment-set-padding alignment 6 6 6 6)
        (gtk-container-add alignment button)
        (gtk-container-add frame alignment)
        (gtk-grid-attach grid frame 1 2 1 1))
      (gtk-container-add window grid)
      (gtk-widget-show-all window))))

;;; ----------------------------------------------------------------------------

;; Similiar to example-alignment, but using the child properties "margin",
;; "valign", and "halign" of the button widget.  In distinction to
;; example-alignment the scaling of the button is not implemented.

(defun example-alignment-2 ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :type :toplevel
                                 :title "Example Alignment"
                                 :border-width 12
                                 :width-request 300
                                 :height-request 300))
          (grid (make-instance 'gtk-grid
                                :column-spacing 12
                                :column-homogeneous t
                                :row-spacing 12
                                :row-homogeneous t)))
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))
      (let ((frame (make-instance 'gtk-frame
                                  :label "halign: start, valign: start"))
            (button (make-instance 'gtk-button
                                   :label "Button"
                                   :margin 6
                                   :halign :start
                                   :valign :start)))
        (gtk-container-add frame button)
        (gtk-grid-attach grid frame 0 1 1 1))
      (let ((frame (make-instance 'gtk-frame
                                  :label "halign: start, valign: end"))
            (button (make-instance 'gtk-button
                                   :label "Button"
                                   :margin 6
                                   :halign :start
                                   :valign :end)))
        (gtk-container-add frame button)
        (gtk-grid-attach grid frame 1 1 1 1))
      (let ((frame (make-instance 'gtk-frame
                                  :label "halign: end, valign: start"))
            (button (make-instance 'gtk-button
                                   :label "Button"
                                   :margin 6
                                   :halign :end
                                   :valign :start)))
        (gtk-container-add frame button)
        (gtk-grid-attach grid frame 0 2 1 1))
      (let ((frame (make-instance 'gtk-frame
                                  :label "halign: end, valign: end"))
            (button (make-instance 'gtk-button
                                   :label "Button"
                                   :margin 6
                                   :halign :end
                                   :valign :end)))
        (gtk-container-add frame button)
        (gtk-grid-attach grid frame 1 2 1 1))
      (gtk-container-add window grid)
      (gtk-widget-show-all window))))

;;; ----------------------------------------------------------------------------

;;; Fixed Container

(defun move-button (button fixed)
  (let* ((allocation (gtk-widget-allocation fixed))
         (width (- (gdk-rectangle-width allocation) 50))
         (height (- (gdk-rectangle-height allocation) 25)))
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
                          (leave-gtk-main)))
      (gtk-container-add window fixed)
      (dotimes (i 3)
        (let ((button (gtk-button-new-with-label "Press me")))
          (g-signal-connect button "clicked"
                            (lambda (widget)
                              (move-button widget fixed)))
          (gtk-fixed-put fixed button (random 250) (random 200))))
      (gtk-widget-show-all window))))

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
                          (leave-gtk-main)))
      (gtk-container-add window frame)
      (gtk-widget-show-all window))))

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
                                :label "Ratio 2 x 1"
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
                          (leave-gtk-main)))
      (gtk-container-add window frame)
      (gtk-container-add frame area)
      (gtk-widget-show-all window))))

;;; ----------------------------------------------------------------------------

;;; Paned Window Widgets

(defun example-paned-window ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :type :toplevel
                                 :title "Example Paned Window"
                                 :border-width 12))
          (paned (make-instance 'gtk-paned
                                :orientation :vertical))
          (frame1 (make-instance 'gtk-frame :label "Window 1"))
          (frame2 (make-instance 'gtk-frame :label "Window 2")))
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))
      (setf (gtk-widget-size-request window) '(300 250))
      (gtk-container-add window paned)
      (gtk-paned-add1 paned frame1)
      (gtk-paned-add2 paned frame2)
      (gtk-widget-show-all window))))

;;; ----------------------------------------------------------------------------

;;; Button Boxes

(defun create-bbox (orientation title spacing layout)
  (let ((frame (make-instance 'gtk-frame
                              :label title))
        (bbox (make-instance 'gtk-button-box
                             :orientation orientation
                             :border-width 6
                             :layout-style layout
                             :spacing spacing)))
    (gtk-container-add bbox
                       (make-instance 'gtk-button
                                      :label "OK"
                                      :always-show-image t
                                      :image
                                      (make-instance 'gtk-image
                                                     :icon-name "gtk-ok")))
  (gtk-container-add bbox
                     (make-instance 'gtk-button
                                    :label "Cancel"
                                    :always-show-image t
                                    :image
                                    (make-instance 'gtk-image
                                                   :icon-name "gtk-cancel")))
  (gtk-container-add bbox
                     (make-instance 'gtk-button
                                    :label "Help"
                                    :always-show-image t
                                    :image
                                    (make-instance 'gtk-image
                                                   :icon-name "gtk-help")))
  (gtk-container-add frame bbox)
  frame))

(defun example-button-box ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :type :toplevel
                                 :title "Example Button Box"
                                 :border-width 12))
          (vbox1 (make-instance 'gtk-box
                                :orientation :vertical
                                :homogeneous nil
                                :spacing 12))
          (vbox2 (make-instance 'gtk-box
                                :orientation :vertical
                                :homogeneous nil
                                :spacing 12))
          (hbox (make-instance 'gtk-box
                               :orientation :horizontal
                               :homogeneous nil
                               :spacing 12)))
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))
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
                          (create-bbox :horizontal
                                       "Spread (spacing 12)"
                                       12
                                       :spread))
      ;; Create the second Horizontal Box
      (gtk-box-pack-start vbox2
                          (create-bbox :horizontal
                                       "Edge (spacing 12)"
                                       12
                                       :edge))
      ;; Create the third Horizontal Box
      (gtk-box-pack-start vbox2
                          (create-bbox :horizontal
                                       "Start (spacing 6)"
                                       6
                                       :start))
      ;; Create the fourth Horizontal Box
      (gtk-box-pack-start vbox2
                          (create-bbox :horizontal
                                       "End (spacing 6)"
                                       6
                                       :end))
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
                          (create-bbox :vertical
                                       "Spread (spacing 12)"
                                       12
                                       :spread))
      ;; Create the second Vertical Box
      (gtk-box-pack-start hbox
                          (create-bbox :vertical
                                       "Edge (spacing 12)"
                                       12
                                       :edge))
      ;; Create the third Vertical Box
      (gtk-box-pack-start hbox
                          (create-bbox :vertical
                                       "Start (spacing 6)"
                                       6
                                       :start))
      ;; Create the fourth Vertical Box
      (gtk-box-pack-start hbox
                          (create-bbox :vertical
                                       "End (spacing 6)"
                                       6
                                       :end))
      (gtk-box-pack-start vbox1 hbox)
      (gtk-container-add window vbox1)
      (gtk-widget-show-all window))))

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
                          (leave-gtk-main)))
      (gtk-widget-realize dialog)
      (gtk-box-pack-start (gtk-dialog-content-area dialog)
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

      (gtk-widget-show-all dialog))))

;;; ----------------------------------------------------------------------------

;; Example of GtkNotebook

(defun example-notebook ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :title "Example Notebook"
                                 :type :toplevel
                                 :default-width 250
                                 :default-height 200))
          (expander (make-instance 'gtk-expander
                                   :expanded t
                                   :label "Notebook"))
          (notebook (make-instance 'gtk-notebook
                                   :enable-popup t)))
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))
      (dotimes (i 5)
        (let ((page (make-instance 'gtk-label
                                   :label
                                   (format nil
                                           "Text for page ~A" i)))
              (tab-label (make-instance 'gtk-label
                                        :label (format nil "Tab ~A" i)))
              (tab-button (make-instance 'gtk-button
                                         :image
                                         (make-instance 'gtk-image
                                                        :stock
                                                        "gtk-close"
                                                        :icon-size 1)
                                         :relief :none)))
          (g-signal-connect tab-button "clicked"
             (let ((page page))
               (lambda (button)
                 (declare (ignore button))
                 (format t "Removing page ~A~%" page)
                 (gtk-notebook-remove-page notebook page))))
          (let ((tab-hbox (make-instance 'gtk-box
                                         :orientation :horizontal)))
            (gtk-box-pack-start tab-hbox tab-label)
            (gtk-box-pack-start tab-hbox tab-button)
            (gtk-widget-show-all tab-hbox)
            (gtk-notebook-add-page notebook page tab-hbox))))
      (gtk-container-add expander notebook)
      (gtk-container-add window expander)
      (gtk-widget-show-all window))))

;;; ----------------------------------------------------------------------------
;;;
;;; Chapter 9. Multiline Text Editor
;;;
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;;
;;; Chapter 10. Tree and List Widgets
;;;
;;; ----------------------------------------------------------------------------

;;; Retrieving Row Data

(defun foreach-func (model path iter)
  (let ((first-name (gtk-tree-model-value model iter 0))
        (last-name (gtk-tree-model-value model iter 1))
        (age (gtk-tree-model-value model iter 2))
        (tree-path (gtk-tree-path-to-string path)))
    (format t "Row ~A: ~A ~A, age ~A~%" tree-path first-name last-name age)))

(defun create-and-fill-and-dump-model ()
  (let ((model (make-instance 'gtk-list-store
                              :column-types
                              '("gchararray" "gchararray" "guint"))))
    ;; Fill the model with data
    (gtk-list-store-set model (gtk-list-store-append model)
                              "Klaus-Dieter" "Mustermann" 51)
    (gtk-list-store-set model (gtk-list-store-append model)
                              "Ulrike" "Langhals" 23)
    (gtk-list-store-set model (gtk-list-store-append model)
                              "Marius" "Kalinowski" 91)
    ;; Now traverse the list
    (gtk-tree-model-foreach model #'foreach-func)))

;;; ----------------------------------------------------------------------------

;;; Removing multiple rows

(let ((rowref-list nil))
  (defun foreach-func-1 (model path iter)
    (let ((age (gtk-tree-model-value model iter 2)))
      (when (> age 30)
        (let ((rowref (gtk-tree-row-reference-new model path)))
          (setf rowref-list (cons rowref rowref-list))))
      nil))

  (defun remove-people-older-than (model)
    (setf rowref-list nil)
    (gtk-tree-model-foreach model #'foreach-func-1)
    (format t "rowrefs : ~A~%" rowref-list)
    (dolist (rowref rowref-list)
      (let ((path (gtk-tree-row-reference-path rowref)))
      (when path
        (let ((iter (gtk-tree-model-iter model path)))
          (when iter
            (gtk-list-store-remove model iter))))))))

(defun create-and-fill-and-dump-model-1 ()
  (let ((model (make-instance 'gtk-list-store
                              :column-types
                              '("gchararray" "gchararray" "guint"))))
    ;; Fill the model with data
    (gtk-list-store-set model (gtk-list-store-append model)
                              "Klaus-Dieter" "Mustermann" 51)
    (gtk-list-store-set model (gtk-list-store-append model)
                              "Ulrike" "Langhals" 23)
    (gtk-list-store-set model (gtk-list-store-append model)
                              "Marius" "Kalinowski" 91)
    ;; Remove some entries
    (remove-people-older-than model)
    ;; Now traverse the list
    (gtk-tree-model-foreach model #'foreach-func)))

;;; ----------------------------------------------------------------------------
;;;
;;; Chapter 11. Selecting Colors, Files and Fonts
;;;
;;; ----------------------------------------------------------------------------

;;; Color Button

(let ((color (gdk-rgba-parse "Blue")))
  (defun example-color-button ()
    (within-main-loop
      (let ((window (make-instance 'gtk-window
                                   :title "Example Color Button"
                                   :border-width 12
                                   :default-width 250
                                   :default-height 200))
            (button (make-instance 'gtk-color-button
                                   :rgba color)))
        (g-signal-connect window "destroy"
                          (lambda (widget)
                            (declare (ignore widget))
                            (leave-gtk-main)))
        (g-signal-connect button "color-set"
           (lambda (widget)
             (let ((rgba (gtk-color-chooser-rgba widget)))
               (format t "Selected color is ~A~%" (gdk-rgba-to-string rgba)))))
        (gtk-container-add window button)
        (gtk-widget-show-all window)))))

;;; ----------------------------------------------------------------------------

;;; Color Chooser Dialog

(let ((message "Click to change the background color.")
      (bg-color (gdk-rgba-parse "White"))
      ;; Color palette with 4 RGBA colors
      (palette1 (list (gdk-rgba-parse "Red")
                      (gdk-rgba-parse "Yellow")
                      (gdk-rgba-parse "Blue")
                      (gdk-rgba-parse "Green")))
      ;; Gray palette with 3 RGBA grays
      (palette2 (list (gdk-rgba-parse "White")
                      (gdk-rgba-parse "Gray")
                      (gdk-rgba-parse "Black"))))

  (defun drawing-area-event (widget event)
    (declare (ignore widget))
    (when (eq (gdk-event-type event) :button-press)
      (let ((dialog (make-instance 'gtk-color-chooser-dialog
                                   :color bg-color
                                   :use-alpha nil)))
        ;; Add a custom palette to the dialog
        (gtk-color-chooser-add-palette dialog :vertical 1 palette1)
        ;; Add a second coustom palette to the dialog
        (gtk-color-chooser-add-palette dialog :vertical 1 palette2)
        ;; Run the color chooser dialog
        (let ((response (gtk-dialog-run dialog)))
          (when (eq response :ok)
            ;; Change the background color for the drawing area
            (setf bg-color (gtk-color-chooser-rgba dialog)))
          ;; Destroy the color chooser dialog
          (gtk-widget-destroy dialog)))))

  (defun example-color-chooser-dialog ()
    (within-main-loop
      (let ((window (make-instance 'gtk-window
                                   :title "Example Color Chooser Dialog"
                                   :default-width 400))
            (area (make-instance 'gtk-drawing-area)))
        (g-signal-connect window "destroy"
                          (lambda (widget)
                            (declare (ignore widget))
                            (leave-gtk-main)))
        (g-signal-connect area "draw"
                          (lambda (widget cr)
                            (declare (ignore widget))
                            (let ((cr (pointer cr))
                                  (red (gdk-rgba-red bg-color))
                                  (green (gdk-rgba-green bg-color))
                                  (blue (gdk-rgba-blue bg-color)))
                              ;; Paint the current color on the drawing area
                              (cairo-set-source-rgb cr red green blue)
                              (cairo-paint cr)
                              ;; Print a hint on the drawing area
                              (cairo-set-source-rgb cr
                                                    (- 1.0d0 red)
                                                    (- 1.0d0 green)
                                                    (- 1.0d0 blue))
                              (cairo-select-font-face cr "Sans")
                              (cairo-set-font-size cr 12)
                              (cairo-move-to cr 12 24)
                              (cairo-show-text cr message)
                              (cairo-destroy cr))))
        (g-signal-connect area "event"
                          (lambda (widget event)
                            (drawing-area-event widget event)))
        ;; Set the event mask for the drawing area
        (setf (gtk-widget-events area) :button-press-mask)
        ;; Add the drawing area to the window
        (gtk-container-add window area)
        (gtk-widget-show-all window)))))

;;; ----------------------------------------------------------------------------

;;; Color Selection Dialog (deprecated)

(let ((color (gdk-color-new :red 0
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
          (setf (gtk-color-selection-current-color colorsel) color)
          (setf (gtk-color-selection-has-palette colorsel) t)
          (g-signal-connect colorsel "color-changed"
             (lambda (widget)
               (declare (ignore widget))
               (let ((color (gtk-color-selection-current-color colorsel)))
                 (gtk-widget-modify-bg area :normal color))))
          (let ((response (gtk-dialog-run colorseldlg)))
            (gtk-widget-destroy colorseldlg)
            (if (eq response :ok)
                (setq color (gtk-color-selection-current-color colorsel))
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
                            (leave-gtk-main)))
        (gtk-widget-modify-bg area :normal color)
        (setf (gtk-widget-events area) :button-press-mask)
        (g-signal-connect area "event"
                          (lambda (widget event)
                            (drawing-area-event widget event area)))
        (gtk-container-add window area)
        (gtk-widget-show-all window)))))

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
                                 :label "Select a file for save ...")))
      ;; Handle the signal "destroy" for the window.
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))
      ;; Handle the signal "clicked" for the button.
      (g-signal-connect button "clicked"
         (lambda (widget)
           (declare (ignore widget))
           (let ((dialog (gtk-file-chooser-dialog-new "Speichern"
                                                      nil
                                                      :save
                                                      "gtk-save" :accept
                                                      "gtk-cancel" :cancel)))
             (when (eq (gtk-dialog-run dialog) :accept)
               (format t "Saved to file ~A~%"
                         (gtk-file-chooser-filename dialog)))
             (gtk-widget-destroy dialog))))
      (gtk-container-add window button)
      (gtk-widget-show-all window))))

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
                          (leave-gtk-main)))
      (g-signal-connect button "file-set"
                        (lambda (widget)
                          (declare (ignore widget))
                          (format t "File set: ~a~%"
                                    (gtk-file-chooser-filename button))))
      (gtk-container-add window button)
      (gtk-widget-show-all window))))

;;; ----------------------------------------------------------------------------

;;; Font Chooser Button

(defun font-filter (family face)
  (declare (ignore face))
  (member (pango-font-family-name family)
          '("Sans" "Serif")
          :test #'equal))

(defun example-font-button ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :title "Example Font Chooser Button"
                                 :type :toplevel
                                 :border-width 12
                                 :default-width 300
                                 :default-height 100))
          (button (make-instance 'gtk-font-button)))
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))
      ;; Set a filter function to select fonts for the font chooser
      (gtk-font-chooser-set-filter-func button #'font-filter)
      (g-signal-connect button "font-set"
         (lambda (widget)
           (declare (ignore widget))
           (format t "Font is set:~%")
           (format t "   Font name   : ~A~%"
                     (gtk-font-chooser-font button))
           (format t "   Font family : ~A~%"
                     (pango-font-family-name
                       (gtk-font-chooser-font-family button)))
           (format t "   Font face   : ~A~%"
                     (pango-font-face-face-name
                       (gtk-font-chooser-font-face button)))
           (format t "   Font size   : ~A~%"
                     (gtk-font-chooser-font-size button))))
      (gtk-container-add window button)
      (gtk-widget-show-all window))))

;;; ----------------------------------------------------------------------------
;;;
;;; Chapter 12. Miscellaneous Widgets
;;;
;;; ----------------------------------------------------------------------------

;;; Arrows

(defun create-arrow-button (arrow-type shadow-type)
  (let (;; Create a button
        (button (make-instance 'gtk-button
                               ;; Add a small margin around the button
                               :margin 3
                               ;; Make big buttons of size 75 x 75
                               :width-request 75
                               :height-request 75)))
    ;; Add an arrow to the button
    (gtk-container-add button
                       (make-instance 'gtk-arrow
                                      :arrow-type arrow-type
                                      :shadow-type shadow-type))
    ;; Add a tooltip to the button
    (setf (gtk-widget-tooltip-text button)
          (format nil "Arrow of type ~A" (symbol-name arrow-type)))
    button))

(defun example-arrows ()
  (within-main-loop
    (let ((;; Create the main window
           window (make-instance 'gtk-window
                                 :type :toplevel
                                 :title "Example Arrow Buttons"
                                 :default-width 275
                                 :default-height 125
                                 :border-width 12))
          ;; Create a grid for the buttons
          (grid (make-instance 'gtk-grid
                               :orientation :horizontal
                               :column-homogeneous t)))
      ;; Connect a signal handler to the window
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))
      ;; Create buttons with an arrow and add the buttons to the grid
      (gtk-container-add grid (create-arrow-button :up :in))
      (gtk-container-add grid (create-arrow-button :down :out))
      (gtk-container-add grid (create-arrow-button :left :etched-in))
      (gtk-container-add grid (create-arrow-button :right :etched-out))
      ;; Add the grid to the window
      (gtk-container-add window grid)
      ;; Show the window
      (gtk-widget-show-all window))))

;;; ----------------------------------------------------------------------------

;; Calendar

(defun example-calendar ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :title "Example Calendar"
                                 :type :toplevel
                                 :border-width 24
                                 :default-width 250
                                 :default-height 100))
          (frame (make-instance 'gtk-frame))
          (calendar (make-instance 'gtk-calendar
                                   :show-details nil)))
      ;; Connect a signal handler to the window
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))
      ;; Connect a signal handler to print the selected day
      (g-signal-connect calendar "day-selected"
                        (lambda (widget)
                          (declare (ignore widget))
                          (format t "selected: year ~A month ~A day ~A~%"
                                  (gtk-calendar-year calendar)
                                  (gtk-calendar-month calendar)
                                  (gtk-calendar-day calendar))))
      ;; Install a calendar detail function
      (gtk-calendar-set-detail-func calendar
                                    (lambda (calendar year month day)
                                      (declare (ignore calendar year month))
                                      (when (= day 12)
                                        "This day has a tooltip.")))
      ;; Mark a day
      (gtk-calendar-mark-day calendar 6)
      ;; Put the calendar into the frame and the frame into the window.
      (gtk-container-add frame calendar)
      (gtk-container-add window frame)
      (gtk-widget-show-all window))))

;;; ----------------------------------------------------------------------------

;;; The Event Box

(defun example-event-box ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :type :toplevel
                                 :title "Example Event Box"
                                 :default-height 150
                                 :border-width 24))
          (eventbox (make-instance 'gtk-event-box))
          (label (make-instance 'gtk-label
                                :ellipsize :end
                                :label
                                "Click here to quit this Example Event Box.")))
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))
      ;; Set the available events for the event box
      (setf (gtk-widget-events eventbox) :button-press-mask)
      ;; Connect a signal handler to the eventbox
      (g-signal-connect eventbox "button-press-event"
                        (lambda (widget event)
                          (declare (ignore widget event))
                          (gtk-widget-destroy window)))
      ;; Add the label to the event box and the event box to the window
      (gtk-container-add eventbox label)
      (gtk-container-add window eventbox)
      ;; Realize the event box
      (gtk-widget-realize eventbox)
      ;; Set a new cursor for the event box
      (setf (gdk-window-cursor (gtk-widget-window eventbox))
            (gdk-cursor-new-from-name (gdk-display-default) "pointer"))
      ;; Show the window
      (gtk-widget-show-all window))))

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
    (setf (gtk-container-border-width (gtk-dialog-content-area dialog)) 12)
    ;; Add a label widget with text to the content area
    (let ((vbox (make-instance 'gtk-box
                               :orientation :vertical
                               :border-width 12))
          (label (make-instance 'gtk-label
                                :wrap t
                                :label
                                (format nil
                                        "The content area is the place to ~
                                         put in the widgets.~%~% ~
                                         The action area contains ~
                                         the buttons."))))
      (gtk-box-pack-start vbox label)
      (gtk-box-pack-start (gtk-dialog-content-area dialog) vbox)
      ;; Show the content area of the dialog
      (gtk-widget-show-all (gtk-dialog-content-area dialog)))
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

#+nil
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
          (vbox (make-instance 'gtk-box
                               :orientation :vertical
                               :spacing 6)))
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))
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
                          (make-instance 'gtk-separator
                                         :orientation :horizontal))
      ;; Create a quit button
      (let ((button (make-instance 'gtk-button
                                   :label "Quit")))
        (g-signal-connect button "clicked"
                          (lambda (widget)
                            (declare (ignore widget))
                            (gtk-widget-destroy window)))
        (gtk-box-pack-start vbox button))
      (gtk-widget-show-all window))))

;;; ----------------------------------------------------------------------------

;;; Text Entry

(defun example-text-entry ()
  (within-main-loop
    (let* ((window (make-instance 'gtk-window
                                  :type :toplevel
                                  :title "Example Text Entry"
                                  :default-width 250))
           (vbox (make-instance 'gtk-box :orientation :vertical))
           (hbox (make-instance 'gtk-box :orientation :horizontal))
           (entry (make-instance 'gtk-entry
                                 :text "Hello"
                                 :max-length 50))
           (pos (gtk-entry-text-length entry)))
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))
      (g-signal-connect entry "activate"
                        (lambda (widget)
                          (declare (ignore widget))
                          (format t "Entry contents: ~A"
                                  (gtk-entry-text entry))))
      (gtk-editable-insert-text entry " world" pos)
      (gtk-editable-select-region entry 0 (gtk-entry-text-length entry))
      (gtk-box-pack-start vbox entry :expand t :fill t :padding 0)
      (let ((check (gtk-check-button-new-with-label "Editable")))
        (g-signal-connect check "toggled"
           (lambda (widget)
             (declare (ignore widget))
             (setf (gtk-editable-editable entry)
                   (gtk-toggle-button-active check))))
        (gtk-box-pack-start hbox check))
      (let ((check (gtk-check-button-new-with-label "Visible")))
        (setf (gtk-toggle-button-active check) t)
        (g-signal-connect check "toggled"
           (lambda (widget)
             (declare (ignore widget))
             (setf (gtk-entry-visibility entry)
                   (gtk-toggle-button-active check))))
        (gtk-box-pack-start hbox check))
      (gtk-box-pack-start vbox hbox)
      (let ((button (make-instance 'gtk-button
                                   :label "Close"
                                   :always-show-image t
                                   :image
                                   (make-instance 'gtk-image
                                                  :icon-name "gtk-close"))))
        (g-signal-connect button "clicked"
                          (lambda (widget)
                            (declare (ignore widget))
                            (gtk-widget-destroy window)))
        (gtk-box-pack-start vbox button))
      (gtk-container-add window vbox)
      (gtk-widget-show-all window))))

;;; ----------------------------------------------------------------------------

;;; Spin Button

(defun example-spin-button ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :type :toplevel
                                 :title "Example Spin Button"
                                 :default-width 300))
          (vbox (make-instance 'gtk-box
                               :orientation :vertical
                               :homogeneous nil
                               :spacing 6
                               :border-width 12))
          (vbox1 (make-instance 'gtk-box
                                :orientation :vertical
                                :homogeneous nil
                                :spacing 0
                                :border-width 6))
          (vbox2 (make-instance 'gtk-box
                                :orientation :vertical
                                :homogeneous nil
                                :spacing 0
                                :boder-width 6))
          (hbox (make-instance 'gtk-box :orientatin :horizontal))
          (frame1 (make-instance 'gtk-frame
                                 :label "Not accelerated"))
          (frame2 (make-instance 'gtk-frame
                                 :label "Accelerated"))
          (label (make-instance 'gtk-label
                                :label "0")))
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))
      (let ((vbox (make-instance 'gtk-box :orientation :vertical))
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
      (let ((vbox (make-instance 'gtk-box :orientation :vertical))
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
      (let ((vbox (make-instance 'gtk-box :orientation :vertical))
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
      (setq hbox (make-instance 'gtk-box :orientation :horizontal))
      (let ((vbox (make-instance 'gtk-box :orientation :vertical))
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
             (setf (gtk-spin-button-digits spinner1)
                   (gtk-spin-button-value-as-int spinner2))))
        (setq vbox (make-instance 'gtk-box :orientation :vertical))
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
               (setf (gtk-spin-button-snap-to-ticks spinner1)
                     (gtk-toggle-button-active widget))))
          (gtk-box-pack-start vbox2 check))
        (let ((check (make-instance 'gtk-check-button
                                    :label "Numeric only input mode"
                                    :active t)))
          (g-signal-connect check "clicked"
             (lambda (widget)
               (setf (gtk-spin-button-numeric spinner1)
                     (gtk-toggle-button-active widget))))
          (gtk-box-pack-start vbox2 check))
        (gtk-container-add frame2 vbox2)
        (setq hbox (make-instance 'gtk-box :orientation :horizontal))
        (let ((button (gtk-button-new-with-label "Value as Int")))
          (g-signal-connect button "clicked"
             (lambda (widget)
               (declare (ignore widget))
               (setf (gtk-label-text label)
                     (format nil "~A"
                             (gtk-spin-button-value-as-int spinner1)))))
            (gtk-box-pack-start hbox button))
        (let ((button (gtk-button-new-with-label "Value as Float")))
          (g-signal-connect button "clicked"
             (lambda (widget)
               (declare (ignore widget))
               (setf (gtk-label-text label)
                     (format nil "~A"
                             (gtk-spin-button-value spinner1)))))
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
      (gtk-widget-show-all window))))

;;; ----------------------------------------------------------------------------
;;;
;;; 14. Menus and Toolbars
;;;
;;; ----------------------------------------------------------------------------

(defun example-menus-by-hand ()
  (within-main-loop
    ;; We set the "gtk-shell-shows-menubar" property to NIL to display the
    ;; menubar by the application itself and not by the desktop environment.
    (setf (gtk-settings-gtk-shell-shows-menubar (gtk-settings-default))
          nil)
    (let ((window (make-instance 'gtk-window
                                 :type :toplevel
                                 :default-width 425
                                 :default-height 250
                                 :title "Example Menus by Hand"))
          ;; A vbox to put a menu in
          (vbox (gtk-box-new :vertical 0)))
      ;; Create a menu bar and the menu items for the menu bar
      (let ((menu-bar (gtk-menu-bar-new))
            (file-item (gtk-menu-item-new-with-label "File"))
            (view-item (gtk-menu-item-new-with-label "View"))
            (tools-item (gtk-menu-item-new-with-label "Tools"))
            (help-item (gtk-menu-item-new-with-label "Help")))
        ;; Add the menu bar to the main container
        (gtk-container-add vbox menu-bar)
        ;; Add the menu items to the menu bar
        (gtk-menu-shell-append menu-bar file-item)
        (gtk-menu-shell-append menu-bar view-item)
        (gtk-menu-shell-append menu-bar tools-item)
        (gtk-menu-shell-append menu-bar help-item)
        ;; Create the menus for the menu items in the menu bar
        (let ((file-menu (gtk-menu-new))
              (view-menu (gtk-menu-new))
              (tools-menu (gtk-menu-new))
              (help-menu (gtk-menu-new)))
          ;; Attach the submenus to the items of the menu bar
          (setf (gtk-menu-item-submenu file-item) file-menu)
          (setf (gtk-menu-item-submenu view-item) view-menu)
          (setf (gtk-menu-item-submenu tools-item) tools-menu)
          (setf (gtk-menu-item-submenu help-item) help-menu)
          ;; Create items to put into the File menu
          (let ((open-item (gtk-menu-item-new-with-label "Open"))
                (close-item (gtk-menu-item-new-with-label "Close"))
                (exit-item (gtk-menu-item-new-with-label "Exit")))
            ;; Append the items to the File menu
            (gtk-menu-shell-append file-menu open-item)
            (gtk-menu-shell-append file-menu close-item)
            (gtk-menu-shell-append file-menu (gtk-separator-menu-item-new))
            (gtk-menu-shell-append file-menu exit-item)
            ;; Add a signal handler for exit-item
            (g-signal-connect exit-item "activate"
                              (lambda (widget)
                                (declare (ignore widget))
                                (gtk-widget-destroy window))))

          ;; The view and tools menus will be empty for now

          ;; Create items to put into the Help menu
          (let ((query-item (gtk-menu-item-new-with-label "What's this?"))
                (about-help-item (gtk-menu-item-new-with-label "Info")))
            ;; Append the items to the About Help Menu
            (gtk-menu-shell-append help-menu query-item)
            (gtk-menu-shell-append help-menu (gtk-separator-menu-item-new))
            (gtk-menu-shell-append help-menu about-help-item)
            ;; Create a submenu and items for about-help-item
            (let ((about-help-menu (gtk-menu-new))
                  (about-tool-item (gtk-menu-item-new-with-label "About This"))
                  (about-more-item (gtk-menu-item-new-with-label "About That")))
              ;; Attach the submenu to the about-help-item
              (setf (gtk-menu-item-submenu about-help-item) about-help-menu)
              ;; Append the items to the about-help-menu
              (gtk-menu-shell-append about-help-menu about-tool-item)
              (gtk-menu-shell-append about-help-menu about-more-item)))))
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))
      (gtk-container-add window vbox)
      (gtk-widget-show-all window))))

;;; ----------------------------------------------------------------------------

(defun example-menu-popup ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :type :toplevel
                                 :default-width 250
                                 :default-height 150
                                 :title "Example Popup Menu"))
          (button (gtk-button-new-with-label "Click me")))
      ;; Create pop-up menu for button
      (let ((popup-menu (gtk-menu-new))
            (big-item (gtk-menu-item-new-with-label "Larger"))
            (small-item (gtk-menu-item-new-with-label "Smaller")))
        (gtk-menu-shell-append popup-menu big-item)
        (gtk-menu-shell-append popup-menu small-item)
        (gtk-widget-show-all popup-menu)
        ;; Signal handler to pop up the menu
        (g-signal-connect button "button-press-event"
           (lambda (widget event)
             (declare (ignore widget))
             (gtk-menu-popup popup-menu
                             :button (gdk-event-button-button event)
                             :activate-time (gdk-event-button-time event))
             t)))
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))
      (gtk-container-add window button)
      (gtk-widget-show-all window))))

;;; ----------------------------------------------------------------------------

(defun example-toolbar-by-hand ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :type :toplevel
                                 :default-width 250
                                 :default-height 150
                                 :title "Example Toolbar"))
          ;; A vbox to put a menu and a button in
          (vbox (gtk-box-new :vertical 0)))
      (let ((toolbar (gtk-toolbar-new))
            (new-button (gtk-tool-button-new-from-stock "gtk-new"))
            (open-button (gtk-tool-button-new-from-stock "gtk-open"))
            (save-button (gtk-tool-button-new-from-stock "gtk-save"))
            (quit-button (gtk-tool-button-new-from-stock "gtk-quit"))
            (separator (make-instance 'gtk-separator-tool-item
                                      :draw nil)))
        (gtk-toolbar-insert toolbar new-button -1)
        (gtk-toolbar-insert toolbar open-button -1)
        (gtk-toolbar-insert toolbar save-button -1)
        (gtk-toolbar-insert toolbar separator -1)
        (gtk-toolbar-insert toolbar quit-button -1)
        (setf (gtk-tool-item-expand separator) t)
        (gtk-box-pack-start vbox toolbar :fill nil :expand nil :padding 3)
        ;; Connect a signal handler to the quit button
        (g-signal-connect quit-button "clicked"
                          (lambda (widget)
                            (declare (ignore widget))
                            (gtk-widget-destroy window))))
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))
      (gtk-container-add window vbox)
      (gtk-widget-show-all window))))

;;; ----------------------------------------------------------------------------

;; GtkUIManager

(defparameter ui-constant
 "<ui>
    <menubar name='MainMenu'>
      <menu action='FileMenu'>
        <placeholder name='OpenClose'/>
        <separator/>
        <menuitem action='Exit'/>
      </menu>
      <menu action='ViewMenu'>
        <menuitem action='ZoomIn'/>
        <menuitem action='ZoomOut'/>
        <separator/>
        <menuitem action='FullScreen'/>
        <separator/>
        <menuitemaction='JustifyLeft'/>
        <menuitemaction='JustifyCenter'/>
        <menuitemaction='JustifyRight'/>
      </menu>
    </menubar>
  </ui>")



;;; ----------------------------------------------------------------------------

(defun example-menu ()
  (within-main-loop
    (setf (gtk-settings-gtk-shell-shows-app-menu (gtk-settings-default))
          nil)
    (setf (gtk-settings-gtk-shell-shows-menubar (gtk-settings-default))
          nil)
    (let ((window (make-instance 'gtk-window
                                 :type :toplevel
                                 :default-width 250
                                 :default-height 200
                                 :title "Example Menu Widget"))
          (vbox (make-instance 'gtk-box
                               :orientation :vertical)))
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))
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
          (setf (gtk-menu-item-submenu item-file) submenu))
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
          (setf (gtk-menu-item-submenu item-edit) submenu))
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
          (setf (gtk-menu-item-submenu item-help) submenu))
        ;; Add the items file, edit, and help into the menubar.
        (gtk-menu-shell-append menubar item-file)
        (gtk-menu-shell-append menubar item-edit)
        (gtk-menu-shell-append menubar item-help)
        ;; Pack the menubar into the vbox.
        (gtk-box-pack-start vbox menubar :expand nil))
        ;; Pack a text view into the vbox
        (gtk-box-pack-start vbox (make-instance 'gtk-text-view))
      ;; Pack the vbox into the window.
      (gtk-container-add window vbox)
      ;; Show the window.
      (gtk-widget-show-all window))))

;;; ----------------------------------------------------------------------------

(defun example-menu-builder ()
  (within-main-loop
    (let ((builder (make-instance 'gtk-builder)))
      (gtk-builder-add-from-file builder "example-menu-builder.ui")
      (g-signal-connect (gtk-builder-object builder "window") "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))
      (gtk-widget-show-all (gtk-builder-object builder "window")))))

;;; ----------------------------------------------------------------------------

(defun example-8 ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :title "Example TreeView"
                                 :type :toplevel
                                 :default-width 200
                                 :default-height 200))
                                 )
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))
      (gtk-container-add window (make-instance 'gtk-tree-view))
      (gtk-widget-show-all window))))

;;; ----------------------------------------------------------------------------

;; Subclassing a GTK widget

(defclass custom-window (gtk-window)
  ((label :initform (make-instance 'gtk-label
                                   :label
                                   "Click the button to show the run time")
          :reader custom-window-label)
   (button :initform (make-instance 'gtk-button :label "Show run time")
           :reader custom-window-button))
  (:metaclass gobject-class)
  (:default-initargs :title "Example Custom Window"
                     :default-width 320
                     :default-height 240))

(defmethod initialize-instance :after
    ((window custom-window) &key &allow-other-keys)
  (let ((box (make-instance 'gtk-box
                            :orientation :vertical)))
    (gtk-box-pack-start box (custom-window-label window))
    (gtk-box-pack-start box (custom-window-button window) :expand nil)
    (gtk-container-add window box))
  (g-signal-connect (custom-window-button window) "clicked"
     (lambda (widget)
       (declare (ignore widget))
       (setf (gtk-label-label (custom-window-label window))
             (format nil "Internal run time is ~A" (get-internal-run-time))))))

(defun example-custom-window ()
  (within-main-loop
    (let ((window (make-instance 'custom-window)))
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))
      (gtk-widget-show-all window))))

;;; ----------------------------------------------------------------------------
;;;
;;; 14. Simple Application
;;;
;;; ----------------------------------------------------------------------------

(defclass bloat-pad (gtk-application)
  ()
  (:metaclass gobject-class)
  (:g-type-name . "BloatPad"))

(register-object-type-implementation "BloatPad"
                                     bloat-pad
                                     "GtkApplication"
                                     nil
                                     nil)

(defun new-window (application file)
  (declare (ignore file))
    (let (;; Create the application window
          (window (make-instance 'gtk-application-window
                                 :application application
                                 :title "Bloatpad"
                                 :border-width 12
                                 :default-width 500
                                 :default-height 400))
          (grid (make-instance 'gtk-grid))
          (toolbar (make-instance 'gtk-toolbar)))

      ;; Connect signal "destroy" to the application window
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)
                          (if (zerop gtk::*main-thread-level*)
                              (g-application-quit application))))

      ;; Add action "copy" to the application window
      (let ((action (g-simple-action-new "copy" nil)))
        (g-action-map-add-action window action)
        (g-signal-connect action "activate"
           (lambda (action parameter)
             (declare (ignore action parameter))
             (let ((view (gobject::get-g-object-for-pointer
                           (g-object-data window "bloatpad-text"))))
               (gtk-text-buffer-copy-clipboard
                                  (gtk-text-view-buffer view)
                                  (gtk-widget-clipboard view "CLIPBOARD"))))))

      ;; Add action "paste" to the application window
      (let ((action (g-simple-action-new "paste" nil)))
        (g-action-map-add-action window action)
        (g-signal-connect action "activate"
           (lambda (action parameter)
             (declare (ignore action parameter))
             (let ((view (gobject::get-g-object-for-pointer
                           (g-object-data window "bloatpad-text"))))
               (gtk-text-buffer-paste-clipboard
                                       (gtk-text-view-buffer view)
                                       (gtk-widget-clipboard view "CLIPBOARD")
                                       :editable t)))))

      ;; Add action "fullscreen" to the application window
      (let ((action (g-simple-action-new-stateful
                                               "fullscreen"
                                               nil
                                               (g-variant-new-boolean nil))))
        (g-action-map-add-action window action)
        (g-signal-connect action "activate"
           (lambda (action parameter)
             (declare (ignore parameter))
             (let* ((state (g-action-state action))
                    (value (g-variant-boolean state)))
               (g-action-change-state action
                                      (g-variant-new-boolean (not value))))))
        (g-signal-connect action "change-state"
           (lambda (action parameter)
             (if (g-variant-boolean parameter)
                 (gtk-window-fullscreen window)
                 (gtk-window-unfullscreen window))
             (setf (g-simple-action-state action) parameter))))

      ;; Add action "justify" to the application window
      (let ((action (g-simple-action-new-stateful
                                             "justify"
                                             (g-variant-type-new "s")
                                             (g-variant-new-string "left"))))
        (g-action-map-add-action window action)
        (g-signal-connect action "activate"
           (lambda (action parameter)
             (g-action-change-state action parameter)))
        (g-signal-connect action "change-state"
           (lambda (action parameter)
             (let ((view (gobject::get-g-object-for-pointer
                           (g-object-data window "bloatpad-text")))
                   (str (g-variant-string parameter)))
               (cond ((equal str "left")
                      (setf (gtk-text-view-justification view) :left))
                     ((equal str "center")
                      (setf (gtk-text-view-justification view) :center))
                     (t
                      (setf (gtk-text-view-justification view) :right)))
               (setf (g-simple-action-state action) parameter)))))

      (let ((button (make-instance 'gtk-toggle-tool-button
                                   :stock-id "gtk-justify-left")))
        (gtk-actionable-set-detailed-action-name button "win.justify::left")
        (gtk-container-add toolbar button))
      (let ((button (make-instance 'gtk-toggle-tool-button
                                   :stock-id "gtk-justify-center")))
        (gtk-actionable-set-detailed-action-name button "win.justify::center")
        (gtk-container-add toolbar button))
      (let ((button (make-instance 'gtk-toggle-tool-button
                                   :stock-id "gtk-justify-right")))
        (gtk-actionable-set-detailed-action-name button "win.justify::right")
        (gtk-container-add toolbar button))
      (let ((button (make-instance 'gtk-separator-tool-item
                                   :draw nil)))
        (setf (gtk-tool-item-expand button) t)
        (gtk-container-add toolbar button))
      (let ((button (make-instance 'gtk-tool-item))
            (box (make-instance 'gtk-box
                                :orientation :horizontal
                                :spacing 6))
            (label (make-instance 'gtk-label
                                  :label "Fullscreen:"))
            (switch (make-instance 'gtk-switch)))
        (setf (gtk-actionable-action-name switch) "win.fullscreen")
        (gtk-container-add box label)
        (gtk-container-add box switch)
        (gtk-container-add button box)
        (gtk-container-add toolbar button))
      (gtk-grid-attach grid toolbar 0 0 1 1)
      (let ((scrolled (make-instance 'gtk-scrolled-window
                                     :hexpand t
                                     :vexpand t))
            (view (make-instance 'gtk-text-view)))
        (setf (g-object-data window "bloatpad-text") (pointer view))
        (gtk-container-add scrolled view)
        (gtk-grid-attach grid scrolled 0 1 1 1))
      (gtk-container-add window grid)
      (gtk-widget-show-all window)))

(defun bloat-pad-activate (application)
  ;; Start a main loop and create an application window
  (within-main-loop
    (new-window application nil))
  ;; Wait until the main loop has finished
  (join-gtk-main))

(defun create-about-dialog ()
  (let (;; Create an about dialog
        (dialog (make-instance 'gtk-about-dialog
                               :program-name "Example Dialog"
                               :version "0.00"
                               :copyright "(c) Dieter Kaiser"
                               :website
                               "github.com/crategus/cl-cffi-gtk"
                               :website-label "Project web site"
                               :license "LLGPL"
                               :authors '("Dieter Kaiser")
                               :documenters '("Dieter Kaiser")
                               :artists '("None")
                               :logo-icon-name
                               "applications-development"
                               :wrap-license t)))
    ;; Run the about dialog
    (gtk-dialog-run dialog)
    ;; Destroy the about dialog
    (gtk-widget-destroy dialog)))

(defvar *menu*
  "<interface>
    <menu id='app-menu'>
     <section>
      <item>
       <attribute name='label' translatable='yes'>_New Window</attribute>
       <attribute name='action'>app.new</attribute>
       <attribute name='accel'>&lt;Primary&gt;n</attribute>
      </item>
     </section>
     <section>
      <item>
       <attribute name='label' translatable='yes'>_About Bloatpad</attribute>
       <attribute name='action'>app.about</attribute>
      </item>
     </section>
     <section>
      <item>
       <attribute name='label' translatable='yes'>_Quit</attribute>
       <attribute name='action'>app.quit</attribute>
       <attribute name='accel'>&lt;Primary&gt;q</attribute>
      </item>
     </section>
     </menu>
    <menu id='menubar'>
     <submenu>
      <attribute name='label' translatable='yes'>_Edit</attribute>
      <section>
       <item>
        <attribute name='label' translatable='yes'>_Copy</attribute>
        <attribute name='action'>win.copy</attribute>
        <attribute name='accel'>&lt;Primary&gt;c</attribute>
       </item>
       <item>
        <attribute name='label' translatable='yes'>_Paste</attribute>
        <attribute name='action'>win.paste</attribute>
        <attribute name='accel'>&lt;Primary&gt;v</attribute>
       </item>
      </section>
     </submenu>
     <submenu>
      <attribute name='label' translatable='yes'>_View</attribute>
      <section>
       <item>
        <attribute name='label' translatable='yes'>_Fullscreen</attribute>
        <attribute name='action'>win.fullscreen</attribute>
        <attribute name='accel'>F11</attribute>
       </item>
      </section>
     </submenu>
    </menu>
   </interface>")

(defun bloat-pad-startup (application)
  ;; Add action "new" to the application
  (let ((action (g-simple-action-new "new" nil)))
    ;; Connect a handler to the signal "activate"
    (g-signal-connect action "activate"
       (lambda (action parameter)
         (declare (ignore action parameter))
         ;; ensure-gtk-main increases the thread level for the new window
         (ensure-gtk-main)
         (new-window application nil)))
    ;; Add the action to the action map of the application
    (g-action-map-add-action application action))

  ;; Add action "about" to the application
  (let ((action (g-simple-action-new "about" nil)))
    ;; Connect a handler to the signal "activate"
    (g-signal-connect action "activate"
       (lambda (action parameter)
         (declare (ignore action parameter))
         (create-about-dialog)))
    ;; Add the action to the action map of the application
    (g-action-map-add-action application action))

  ;; Add action "quit" to the application
  (let ((action (g-simple-action-new "quit" nil)))
    ;; Connect a handler to the signal activate
    (g-signal-connect action "activate"
       (lambda (action parameter)
         (declare (ignore action parameter))
         ;; Destroy all windows of the application
         (dolist (window (gtk-application-windows application))
           (gtk-widget-destroy window))
         ;; Quit the main loop
         (leave-gtk-main)
         ;; Quit the application
         (g-application-quit application)))
    ;; Add the action to action map of the application
    (g-action-map-add-action application action))

  ;; Intitialize the application menu and the menubar
  (let ((builder (make-instance 'gtk-builder)))
    ;; Read the menus from a string
    (gtk-builder-add-from-string builder *menu*)
    ;; Set the application menu
    (setf (gtk-application-app-menu application)
          (gtk-builder-object builder "app-menu"))
    ;; Set the menubar
    (setf (gtk-application-menubar application)
          (gtk-builder-object builder "menubar"))))

(defun bloat-pad-open (application)
  (declare (ignore application))
  ;; Executed when the application is opened
  nil)

(defun bloat-pad-shutdown (application)
  (declare (ignore application))
  ;; Executed when the application is shut down
  nil)

(defmethod initialize-instance :after
    ((app bloat-pad) &key &allow-other-keys)
  (g-signal-connect app "activate" #'bloat-pad-activate)
  (g-signal-connect app "startup" #'bloat-pad-startup)
  (g-signal-connect app "open" #'bloat-pad-open)
  (g-signal-connect app "shutdown" #'bloat-pad-shutdown))

(defun bloat-pad-new ()
  (setf (g-application-name) "Bloatpad")
  (setf (gtk-settings-gtk-shell-shows-app-menu (gtk-settings-default))
        nil)
  (setf (gtk-settings-gtk-shell-shows-menubar (gtk-settings-default))
        nil)
  (make-instance 'bloat-pad
                 :application-id "org.gtk.Test.bloatpad"
                 :flags :handles-open
                 :inactivity-timeout 30000
                 :register-session t))

(defun example-application (&optional (argv nil))
  (let (;; Create an instance of the application Bloat Pad
        (bloat-pad (bloat-pad-new)))
    (format t "call G-APPLICATION-RUN.~%")
    ;; Run the application
    (g-application-run bloat-pad argv)
    (format t "back from G-APPLICATION-RUN.~%")))

;;; ----------------------------------------------------------------------------

