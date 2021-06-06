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
  (:export
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

(defun apply-css-to-widget (provider widget)
  (gtk-style-context-add-provider (gtk-widget-style-context widget)
                                  provider
                                  +gtk-style-provider-priority-user+)
  (when (g-type-is-a (g-type-from-instance widget) "GtkContainer")
    (gtk-container-forall widget
                          (lambda (widget)
                            (apply-css-to-widget provider widget)))))

;;; ----------------------------------------------------------------------------
;;;
;;; Chapter 2. Getting started
;;;
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;;
;;; Chapter 3. Packing Widgets
;;;
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;;
;;; Chapter 5. Display Widgets
;;;
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;;
;;; Chapter 7. Range Widgets
;;;
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;;
;;; Chapter 8. Layout Widgets
;;;
;;; ----------------------------------------------------------------------------

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

