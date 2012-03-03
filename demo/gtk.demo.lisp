;;; ----------------------------------------------------------------------------
;;; gtk.demo.lisp
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2012 Dieter Kaiser
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

(asdf:operate 'asdf:load-op :cl-gtk)

(defpackage :gtk-demo
  (:use :cl :gtk :gdk :gobject :iter))

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
          (area (make-instance 'gtk-drawing-area))
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
    (let* ((window (make-instance 'gtk-window
                                  :type :toplevel
                                  :title "Demo Entry"
                                  :default-width 250
                                  :border-width 10))
           (box (make-instance 'gtk-vbox))
           (entry (make-instance 'gtk-entry))
           (button (make-instance 'gtk-button :label "OK"))
           (text-buffer (make-instance 'gtk-text-buffer))
           (text-view (make-instance 'gtk-text-view :buffer text-buffer))
           (button-select (make-instance 'gtk-button :label "Select"))
           (button-insert (make-instance 'gtk-button :label "Insert"))
           (label (make-instance 'gtk-label
                                 :use-markup t
                                 :label "Enter <b>anything</b> you wish:")))
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
    (let* ((window (make-instance 'gtk-window
                                  :type :toplevel
                                  :title "Demo Table packing"
                                  :default-width 300
                                  :border-width 20))
           (table (make-instance 'gtk-table
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

;; Using GtkImage with stock icon

(defun demo-image ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :title "Test images"
                                 :default-width 300
                                 :default-height 200))
          (image (make-instance 'gtk-image
                                :icon-name "weather-storm"
                                :icon-size 6)))
      (gtk-container-add window image)
      (g-signal-connect window "destroy"
                        (lambda (w)
                          (declare (ignore w))
                          (gtk-main-quit)))
      (gtk-widget-show window))))

;;; ----------------------------------------------------------------------------

;; Testing progress-bar

(defun demo-progress-bar ()
  "Testing progress-bar"
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :title "Demo Progress Bar"
                                 :default-width 250))
          (v-box (make-instance 'gtkv-box))
          (p-bar (make-instance 'gtk-progress-bar :test "A process"))
          (button-pulse (make-instance 'gtk-button :label "Pulse"))
          (button-set (make-instance 'gtk-button :label "Set"))
          (entry (make-instance 'gtk-entry)))
      (gtk-container-add window v-box)
      (gtk-box-pack-start v-box p-bar)
      (gtk-box-pack-start v-box button-pulse)
      (gtk-box-pack-start v-box button-set)
      (gtk-box-pack-start v-box entry)
      (g-signal-connect window "destroy"
                        (lambda (w)
                          (declare (ignore w))
                          (gtk-main-quit)))
      (g-signal-connect button-pulse "clicked"
                        (lambda (w)
                         (declare (ignore w))
                          (gtk-progress-bar-pulse p-bar)))
      (g-signal-connect button-set "clicked"
                        (lambda (w)
                          (declare (ignore w))
                          (setf (gtk-progress-bar-fraction p-bar)
                                (coerce (read-from-string (gtk-entry-text entry))
                                        'real))))
      (gtk-widget-show window))))

;;; ----------------------------------------------------------------------------

;; Demo of Status Bar

(defun demo-statusbar ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :title "Demo Status Bar"
                                 :default-width 300))
          (v-box (make-instance 'gtk-vbox))
          (h-box (make-instance 'gtk-hbox))
          (label (make-instance 'gtk-label
                                :label "Push or Pop text on the statusbar" 
                                :xalign 0.5
                                :yalign 0.5))
          (statusbar (make-instance 'gtk-statusbar :has-resize-grip t))
          (button-push (make-instance 'gtk-button :label "Push"))
          (button-pop (make-instance 'gtk-button :label "Pop"))
          (entry (make-instance 'gtk-entry))
          (icon (make-instance 'gtk-status-icon
                               :icon-name "applications-development")))
      (gtk-status-icon-set-tooltip-text icon "Demo Status Bar Program")
      (gtk-status-icon-set-screen icon (gtk-window-screen window))
      (g-signal-connect window "destroy"
                        (lambda (w)
                          (declare (ignore w))
                          (gtk-status-icon-set-visible icon nil)
                          (gtk-main-quit)))
      (g-signal-connect button-push "clicked"
                        (lambda (button)
                          (declare (ignore button))
                          (gtk-statusbar-push statusbar
                                              "lisp-prog"
                                              (gtk-entry-text entry))))
      (g-signal-connect button-pop "clicked"
                        (lambda (button)
                          (declare (ignore button))
                          (gtk-statusbar-pop statusbar "lisp-prog")))
      (g-signal-connect icon "activate"
                        (lambda (icon)
                          (declare (ignore icon))
                          (let ((dlg (make-instance 'gtk-message-dialog
                                                    :buttons :ok
                                                    :text "Clicked on icon!")))
                            (gtk-dialog-run dlg)
                            (gtk-widget-destroy dlg))))
      (gtk-container-add window v-box)
      (gtk-box-pack-start v-box h-box :expand nil)
      (gtk-box-pack-start h-box entry)
      (gtk-box-pack-start h-box button-push :expand nil)
      (gtk-box-pack-start h-box button-pop :expand nil)
      (gtk-box-pack-start v-box label)
      (gtk-box-pack-start v-box statusbar :expand nil)
      (gtk-widget-show window))))

;;; ----------------------------------------------------------------------------

;;  Demo of scale button with icons

(defun demo-scale-button ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :type :toplevel
                                 :title "Demo Scale Button"
                                 :default-width 250
                                 :default-height 400))
          (button (make-instance 'gtk-scale-button
                                 :icons (list "media-seek-backward"
                                              "media-seek-forward"
                                              "media-playback-stop"
                                              "media-playback-start"
                                              "media-playback-pause"
                                              "media-record"
                                              "media-skip-backward"
                                              "media-skip-forward")
                                 :adjustment (make-instance 'gtk-adjustment
                                                            :lower -40
                                                            :upper 50
                                                            :value 20))))
      (g-signal-connect window "destroy"
                        (lambda (w)
                          (declare (ignore w))
                          (gtk-main-quit)))
      (gtk-container-add window button)
      (gtk-widget-show window))))

;;; ----------------------------------------------------------------------------

;; Demo of GtkTextView

(defun demo-text-view ()
  (within-main-loop
    (let* ((window (make-instance 'gtk-window
                                  :type :toplevel
                                  :title "Demo Text View"
                                  :width-request 400
                                  :height-request 300))
           (button (make-instance 'gtk-button :label "Do"))
           (button-insert (make-instance 'gtk-button
                                         :label "Insert a button!"))
           (button-bold (make-instance 'gtk-button :label "Bold"))
           (buffer (make-instance 'gtk-text-buffer
                                  :text
                                  "Some text buffer with some text inside"))
           (view (make-instance 'gtk-text-view
                                :buffer buffer
                                :wrap-mode :word))
           (box (make-instance 'gtk-vbox))
           (scrolled (make-instance 'gtk-scrolled-window
                                    :hscrollbar-policy :automatic
                                    :vscrollbar-policy :automatic)))
      (g-signal-connect window "destroy"
                        (lambda (window)
                          (declare (ignore window))
                          (gtk-main-quit)))
      (g-signal-connect button "clicked"
         (lambda (button)
           (declare (ignore button))
           (multiple-value-bind (i1 i2)
               (gtk-text-buffer-get-selection-bounds buffer)
             (when (and i1 i2)
               (let* ((i1 i1)
                      (i2 i2)
                      (dialog (make-instance 'gtk-message-dialog
                                             :buttons :ok)))
                 (setf (gtk-message-dialog-text dialog)
                       (format nil
                               "selection: from (~A,~A) to (~A,~A)"
                               (gtk-text-iter-line i1)
                               (gtk-text-iter-line-offset i1)
                               (gtk-text-iter-line i2)
                               (gtk-text-iter-line-offset i2)))
                 (gtk-dialog-run dialog)
                 (gtk-widget-destroy dialog))))))
      (g-signal-connect button-bold "clicked"
         (lambda (button)
           (declare (ignore button))
           (multiple-value-bind (start end)
               (gtk-text-buffer-get-selection-bounds buffer)
             (when (and start end)
               (let* ((start start)
                      (end end)
                      (tag (gtk-text-tag-table-lookup
                             (gtk-text-buffer-tag-table buffer)
                             "bold")))
                 (if (gtk-text-iter-has-tag start tag)
                     (gtk-text-buffer-remove-tag buffer tag start end)
                     (gtk-text-buffer-apply-tag buffer tag start end)))))))
      (g-signal-connect button-insert "clicked"
         (lambda (button)
           (declare (ignore button))
           (let* ((iter (gtk-text-buffer-get-iter-at-mark
                           buffer
                           (gtk-text-buffer-get-mark buffer "insert")))
                  (anchor (gtk-text-buffer-insert-child-anchor buffer iter))
                  (button (make-instance 'gtk-button :label "A button!")))
             (gtk-widget-show button)
             (gtk-text-view-add-child-at-anchor view button anchor))))
      (let ((tag (make-instance 'gtk-text-tag :name "bold" :weight 700)))
        (gtk-text-tag-table-add (gtk-text-buffer-tag-table buffer) tag)
        (g-signal-connect tag "event"
           (lambda (tag object event iter)
             (declare (ignore tag object iter))
             (when (eq (gdk-event-type event) :button-release)
               (let ((dlg (make-instance 'gtk-message-dialog
                                         :text "You clicked on bold text."
                                         :buttons :ok)))
                 (gtk-dialog-run dlg)
                 (gtk-widget-destroy dlg))))))
      (gtk-container-add window box)
      (gtk-container-add scrolled view)
      (gtk-box-pack-start box button :expand nil)
      (gtk-box-pack-start box button-insert :expand nil)
      (gtk-box-pack-start box button-bold :expand nil)
      (gtk-box-pack-start box scrolled)
      (gtk-widget-show window))))

;;; ----------------------------------------------------------------------------

;; Demo of treeview with CL-GTK2-GTK:ARRAY-LIST-STORE"

(defstruct tvi
  title
  value)
 
(defun demo-treeview-list ()
  (within-main-loop
    (let* ((window (make-instance 'gtk-window
                                  :type :toplevel
                                  :title "Treeview (list)"))
           (model (make-instance 'array-list-store))
           (scroll (make-instance 'gtk-scrolled-window
                                  :hscrollbar-policy :automatic
                                  :vscrollbar-policy :automatic))
           (tv (make-instance 'gtk-tree-view
                              :headers-visible t
                              :width-request 100
                              :height-request 400
                              :rules-hint t))
           (h-box (make-instance 'gtk-hbox))
           (v-box (make-instance 'gtk-vbox))
           (title-entry (make-instance 'gtk-entry))
           (value-entry (make-instance 'gtk-entry))
           (button (make-instance 'gtk-button :label "Add")))
      (store-add-column model "gchararray" #'tvi-title)
      (store-add-column model "gint" #'tvi-value)
      (store-add-item model (make-tvi :title "Monday" :value 1))
      (store-add-item model (make-tvi :title "Tuesday" :value 2))
      (store-add-item model (make-tvi :title "Wednesday" :value 3))
      (store-add-item model (make-tvi :title "Thursday" :value 4))
      (store-add-item model (make-tvi :title "Friday" :value 5))
      (store-add-item model (make-tvi :title "Saturday" :value 6))
      (store-add-item model (make-tvi :title "Sunday" :value 7))
      (setf (gtk-tree-view-model tv) model
            (gtk-tree-view-tooltip-column tv) 0)
      (g-signal-connect window "destroy"
                        (lambda (w)
                          (declare (ignore w))
                          (gtk-main-quit)))
      (g-signal-connect button "clicked"
         (lambda (b)
           (declare (ignore b))
           (store-add-item model
                           (make-tvi :title
                                     (gtk-entry-text title-entry)
                                     :value
                                     (or (parse-integer
                                           (gtk-entry-text value-entry) 
                                           :junk-allowed t)
                                         0)))))
      (g-signal-connect tv "row-activated"
         (lambda (tv path column)
           (declare (ignore tv column))
           (show-message (format nil "You clicked on row ~A"
                                 (gtk-tree-path-get-indices path)))))
      (gtk-container-add window v-box)
      (gtk-box-pack-start v-box h-box :expand nil)
      (gtk-box-pack-start h-box title-entry :expand nil)
      (gtk-box-pack-start h-box value-entry :expand nil)
      (gtk-box-pack-start h-box button :expand nil)
      (gtk-box-pack-start v-box scroll)
      (gtk-container-add scroll tv)
      (let ((column (make-instance 'gtk-tree-view-column
                                   :title "Title"
                                   :sort-column-id 0))
            (renderer (make-instance 'gtk-cell-renderer-text :text "A text")))
        (gtk-tree-view-column-pack-start column renderer)
        (gtk-tree-view-column-add-attribute column renderer "text" 0)
        (gtk-tree-view-append-column tv column)
        (print (gtk-tree-view-column-tree-view column))
        (print (gtk-tree-view-column-cell-renderers column)))
      (let ((column (make-instance 'gtk-tree-view-column :title "Value"))
            (renderer (make-instance 'gtk-cell-renderer-text :text "A text")))
        (gtk-tree-view-column-pack-start column renderer)
        (gtk-tree-view-column-add-attribute column renderer "text" 1)
        (gtk-tree-view-append-column tv column)
        (print (gtk-tree-view-column-tree-view column))
        (print (gtk-tree-view-column-cell-renderers column)))
      (gtk-widget-show window))))

;;; ----------------------------------------------------------------------------

;; Advanced demo: show s-expression tree structure

(defun make-tree-from-sexp (l)
  (setf l (if (listp l) l (list l)))
  (let ((node (make-tree-node :item
                              (make-tvi :title
                                        (format nil "~S" (first l))
                                        :value
                                        (format nil "~S"
                                                (class-of (first l)))))))
    (iter (for child in (rest l))
          (tree-node-insert-at node
                               (make-tree-from-sexp child)
                               (length (tree-node-children node))))
    node))

(defun demo-treeview-tree ()
  (within-main-loop
    (let* ((window (make-instance 'gtk-window
                                  :type :toplevel
                                  :title "Treeview (tree)"))
           (model (make-instance 'tree-lisp-store))
           (scroll (make-instance 'gtk-scrolled-window
                                  :hscrollbar-policy :automatic
                                  :vscrollbar-policy :automatic))
           (tree-view (make-instance 'gtk-tree-view
                                     :headers-visible t
                                     :width-request 300
                                     :height-request 400
                                     :rules-hint t))
           (h-box (make-instance 'gtk-hbox))
           (v-box (make-instance 'gtk-vbox))
           (entry (make-instance 'gtk-entry))
           (button (make-instance 'gtk-button :label "Display")))
      (tree-lisp-store-add-column model "gchararray" #'tvi-title)
      (tree-lisp-store-add-column model "gchararray" #'tvi-value)
      (tree-node-insert-at
        (tree-lisp-store-root model)
        (make-tree-from-sexp
          '(lambda (object &rest initargs &key &allow-other-keys)
              (* 1 2)
              (- 3 4)))
        0)
      (setf (gtk-tree-view-model tree-view) model
            (gtk-tree-view-tooltip-column tree-view) 0)
      (g-signal-connect tree-view "row-activated"
         (lambda (tv path column)
           (declare (ignore tv column))
           (show-message (format nil "You clicked on row ~A"
                                 (gtk-tree-path-get-indices path)))))
      (g-signal-connect button "clicked"
         (lambda (b)
           (declare (ignore b))
           (let ((object (read-from-string (gtk-entry-text entry))))
             (tree-node-remove-at (tree-lisp-store-root model) 0)
             (tree-node-insert-at (tree-lisp-store-root model)
                                  (make-tree-from-sexp object)
                                  0))))
      (gtk-container-add window v-box)
      (gtk-box-pack-start v-box h-box :expand nil)
      (gtk-box-pack-start h-box entry)
      (gtk-box-pack-start h-box button :expand nil)
      (gtk-box-pack-start v-box scroll)
      (gtk-container-add scroll tree-view)
      (let ((column (make-instance 'gtk-tree-view-column
                                   :title "Value"
                                   :sort-column-id 0))
            (renderer (make-instance 'gtk-cell-renderer-text :text "A text")))
        (gtk-tree-view-column-pack-start column renderer)
        (gtk-tree-view-column-add-attribute column renderer "text" 0)
        (gtk-tree-view-append-column tree-view column)
        (print (gtk-tree-view-column-tree-view column))
        (print (gtk-tree-view-column-cell-renderers column)))
      (let ((column (make-instance 'gtk-tree-view-column :title "Type"))
            (renderer (make-instance 'gtk-cell-renderer-text :text "A text")))
        (gtk-tree-view-column-pack-start column renderer)
        (gtk-tree-view-column-add-attribute column renderer "text" 1)
        (gtk-tree-view-append-column tree-view column)
        (print (gtk-tree-view-column-tree-view column))
        (print (gtk-tree-view-column-cell-renderers column)))
      (g-signal-connect window "destroy"
                        (lambda (w)
                          (declare (ignore w))
                          (leave-gtk-main)))
      (gtk-widget-show window))))

;;; ----------------------------------------------------------------------------

;; Demo of GtkComboBox

(defun demo-combo-box ()
  (within-main-loop
    (let* ((window (make-instance 'gtk-window
                                  :type :toplevel
                                  :title "Combo Box"))
           (model (make-instance 'array-list-store))
           (combo-box (make-instance 'gtk-combo-box :model model))
           (h-box (make-instance 'gtk-hbox))
           (v-box (make-instance 'gtk-vbox))
           (title-entry (make-instance 'gtk-entry))
           (value-entry (make-instance 'gtk-entry))
           (button (make-instance 'gtk-button :label "Add")))
      (store-add-column model "gchararray" #'tvi-title)
      (store-add-column model "gint" #'tvi-value)
      (store-add-item model (make-tvi :title "Monday" :value 1))
      (store-add-item model (make-tvi :title "Tuesday" :value 2))
      (store-add-item model (make-tvi :title "Wednesday" :value 3))
      (store-add-item model (make-tvi :title "Thursday" :value 4))
      (store-add-item model (make-tvi :title "Friday" :value 5))
      (store-add-item model (make-tvi :title "Saturday" :value 6))
      (store-add-item model (make-tvi :title "Sunday" :value 7))
      (g-signal-connect window "destroy"
                        (lambda (w)
                          (declare (ignore w))
                          (gtk-main-quit)))
      (g-signal-connect button "clicked"
         (lambda (b)
           (declare (ignore b))
           (store-add-item model
                           (make-tvi :title
                                     (gtk-entry-text title-entry)
                                     :value
                                     (or (parse-integer
                                           (gtk-entry-text value-entry)
                                           :junk-allowed t)
                                         0)))))
      (g-signal-connect combo-box "changed"
         (lambda (c)
           (declare (ignore c))
           (show-message (format nil "You clicked on row ~A~%"
                                 (gtk-combo-box-active combo-box)))))
      (gtk-container-add window v-box)
      (gtk-box-pack-start v-box h-box :expand nil)
      (gtk-box-pack-start h-box title-entry :expand nil)
      (gtk-box-pack-start h-box value-entry :expand nil)
      (gtk-box-pack-start h-box button :expand nil)
      (gtk-box-pack-start v-box combo-box)
      (let ((renderer (make-instance 'gtk-cell-renderer-text :text "A text")))
        (gtk-cell-layout-pack-start combo-box renderer :expand t)
        (gtk-cell-layout-add-attribute combo-box renderer "text" 0))
      (let ((renderer (make-instance 'gtk-cell-renderer-text :text "A number")))
        (gtk-cell-layout-pack-start combo-box renderer :expand nil)
        (gtk-cell-layout-add-attribute combo-box renderer "text" 1))
      (gtk-widget-show window))))

;;; ----------------------------------------------------------------------------

;; Demo of GtkUIManager

(defun demo-ui-manager ()
  (within-main-loop
    (let* ((window (make-instance 'gtk-window
                                  :type :toplevel
                                  :title "UI Manager"
                                  :default-width 200
                                  :default-height 100
                                  :window-position :center))
           (ui-manager (make-instance 'gtk-ui-manager))
           (print-confirmation t))
      (gtk-ui-manager-add-ui-from-string ui-manager
"<ui>
  <toolbar action='toolbar1'>
      <separator/>
      <toolitem name='Left' action='justify-left'/>
      <toolitem name='Center' action='justify-center'/>
      <toolitem name='Right' action='justify-right'/>
      <toolitem name='Zoom in' action='zoom-in' />
      <toolitem name='print-confirm' action='print-confirm' />
      <separator/>
  </toolbar>
</ui>")
      (g-signal-connect window "destroy"
                        (lambda (w)
                          (declare (ignore w))
                          (gtk-main-quit)))
      (iter (with fn = (lambda (action)
                         (when print-confirmation
                           (format t "Action ~A with name ~A activated~%"
                                   action
                                   (gtk-action-name action)))))
            (with action-group = (make-instance 'gtk-action-group
                                                :name "Actions"))
            (finally (let ((a (make-instance 'gtk-toggle-action
                                             :name "print-confirm"
                                             :label "Print"
                                             :stock-id "gtk-print-report"
                                             :active t)))
                       (g-signal-connect a "toggled"
                          (lambda (action)
                            (setf print-confirmation
                                  (gtk-toggle-action-active action))))
                       (gtk-action-group-add-action action-group a))
                     (gtk-ui-manager-insert-action-group ui-manager
                                                         action-group
                                                         0))
            (for (name stock-id) in '(("justify-left" "gtk-justify-left")
                                      ("justify-center" "gtk-justify-center")
                                      ("justify-right" "gtk-justify-right")
                                      ("zoom-in" "gtk-zoom-in")))
            (for action = (make-instance 'gtk-action
                                         :name name
                                         :stock-id stock-id))
            (g-signal-connect action "activate" fn)
            (gtk-action-group-add-action action-group action))
      (let ((widget (gtk-ui-manager-widget ui-manager "/toolbar1")))
        (when widget
          (gtk-container-add window widget)))
      (gtk-widget-show window))))

;;; ----------------------------------------------------------------------------

;; Demo of GtkColorButton

(defun demo-color-button ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :title "Demo Color Button"
                                 :type :toplevel
                                 :window-position :center
                                 :default-width 250
                                 :default-height 200))
          (button (make-instance 'gtk-color-button :title "Color button")))
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (gtk-main-quit)))
      (g-signal-connect button "color-set"
         (lambda (widget)
           (declare (ignore widget))
           (show-message (format nil "Chose color ~A"
                                 (gtk-color-button-get-color button)))))
      (gtk-container-add window button)
      (gtk-widget-show window))))

;;; ----------------------------------------------------------------------------

;; Test of GtkColorSelection

(defun demo-color-selection ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :title "Color selection"
                                 :type :toplevel
                                 :window-position :center))
          (selection (make-instance 'gtk-color-selection
                                    :has-opacity-control t)))
      (g-signal-connect window "destroy"
                        (lambda (w)
                          (declare (ignore w))
                          (gtk-main-quit)))
      (g-signal-connect selection "color-changed"
         (lambda (s)
           (declare (ignore s))
           (unless (gtk-color-selection-is-adjusting selection)
             (format t "color: ~A~%"
                     (gtk-color-selection-current-color selection)))))
      (gtk-container-add window selection)
      (gtk-widget-show window))))

;;; ----------------------------------------------------------------------------

;; Demo of GtkNotebook

(defun demo-notebook ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :title "Notebook"
                                 :type :toplevel
                                 :window-position :center
                                 :default-width 100
                                 :default-height 100))
          (expander (make-instance 'gtk-expander :expanded t :label "notebook"))
          (notebook (make-instance 'gtk-notebook :enable-popup t)))
      (g-signal-connect window "destroy"
                        (lambda (w)
                          (declare (ignore w))
                          (gtk-main-quit)))
      (iter (for i from 0 to 5)
            (for page = (make-instance 'gtk-label
                                       :label (format nil
                                                      "Label for page ~A" i)))
            (for tab-label = (make-instance 'gtk-label
                                            :label (format nil "Tab ~A" i)))
            (for tab-button = (make-instance 'gtk-button
                                             :image
                                             (make-instance 'gtk-image
                                                            :stock "gtk-close"
                                                            :icon-size 1)
                                             :relief :none))
            (g-signal-connect tab-button "clicked"
                              (let ((page page))
                                (lambda (button)
                                  (declare (ignore button))
                                  (format t "Removing page ~A~%" page)
                                  (gtk-notebook-remove-page notebook page))))
            (for tab-hbox = (make-instance 'gtk-hbox))
            (gtk-box-pack-start tab-hbox tab-label)
            (gtk-box-pack-start tab-hbox tab-button)
            (gtk-widget-show tab-hbox)
            (gtk-notebook-add-page notebook page tab-hbox))
      (gtk-container-add window expander)
      (gtk-container-add expander notebook)
      (gtk-widget-show window))))

;;; ----------------------------------------------------------------------------

;; Demo of GtkCalendar

(defun calendar-detail (calendar year month day)
  (declare (ignore calendar year month))
  (when (= day 23)
    "!"))

(defun demo-calendar ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :title "Calendar"
                                 :type :toplevel
                                 :default-width 250
                                 :default-height 100))
          (calendar (make-instance 'gtk-calendar
                                   :detail-function #'calendar-detail)))
      (g-signal-connect window "destroy"
                        (lambda (w)
                          (declare (ignore w))
                          (gtk-main-quit)))
      (g-signal-connect calendar "day-selected"
                        (lambda (c)
                          (declare (ignore c))
                          (format t "selected: year ~A month ~A day ~A~%"
                                  (gtk-calendar-year calendar)
                                  (gtk-calendar-month calendar)
                                  (gtk-calendar-day calendar))))
      (gtk-container-add window calendar)
      (gtk-widget-show window))))

;;; ----------------------------------------------------------------------------

;; Demo of GtkFileChooser

(defun demo-file-chooser ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :title "Demo File Chooser"
                                 :type :toplevel
                                 :window-position :center
                                 :default-width 250
                                 :default-height 100))
          (v-box (make-instance 'gtk-vbox))
          (button (make-instance 'gtk-file-chooser-button :action :open))
          (b (make-instance 'gtk-button
                            :label "Choose for save"
                            :stock-id "gtk-save")))
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (gtk-main-quit)))
      (g-signal-connect button "file-set"
                        (lambda (widget)
                          (declare (ignore widget))
                          (format t "File set: ~A~%"
                                  (gtk-file-chooser-filename button))))
      (g-signal-connect b "clicked"
                        (lambda (widget)
                          (declare (ignore widget))
                          (let ((d (make-instance 'gtk-file-chooser-dialog
                                                  :action :save
                                                  :title
                                                  "Choose file to save")))
                          (gtk-dialog-add-button d "gtk-save" :accept)
                          (gtk-dialog-add-button d "gtk-cancel" :cancel)
                          (when (eq (gtk-dialog-run d) :accept)
                            (format t "saved to file ~A~%"
                                    (gtk-file-chooser-filename d)))
                          (gtk-widget-destroy d))))
      (gtk-container-add window v-box)
      (gtk-box-pack-start v-box button)
      (gtk-box-pack-start v-box b)
      (gtk-widget-show window))))

;;; ----------------------------------------------------------------------------

;; Demo of GtkFontChooser

(defun demo-font-chooser ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :title "Demo Font Chooser"
                                 :type :toplevel
                                 :window-position :center
                                 :default-width 250
                                 :default-height 100))
          (v-box (make-instance 'gtk-vbox))
          (button (make-instance 'gtk-font-button
                                 :title "Choose font"
                                 :font-name "Sans 10")))
      (g-signal-connect window "destroy"
                        (lambda (w)
                          (declare (ignore w))
                          (gtk-main-quit)))
      (g-signal-connect button "font-set"
                        (lambda (b)
                          (declare (ignore b))
                          (format t "Chose font ~A~%"
                                  (gtk-font-button-font-name button))))
      (gtk-container-add window v-box)
      (gtk-box-pack-start v-box button)
      (gtk-widget-show window))))

;;; ----------------------------------------------------------------------------

;; Demo of child-property usage

(defun demo-box-child-property ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :title " Demo Box Child Property"
                                 :type :toplevel
                                 :window-position :center
                                 :width-request 250
                                 :height-request 200))
          (box (make-instance 'gtk-hbox))
          (button (make-instance 'gtk-toggle-button
                                 :active t
                                 :label "Expand")))
      (g-signal-connect window "destroy"
                        (lambda (w)
                          (declare (ignore w))
                          (gtk-main-quit)))
      (g-signal-connect button "toggled"
                        (lambda (b)
                          (declare (ignore b))
                          (setf (gtk-box-child-expand box button)
                                (gtk-toggle-button-active button))))
      (gtk-container-add window box)
      (gtk-box-pack-start box button)
      (gtk-widget-show window))))

;;; ----------------------------------------------------------------------------

;; Demo of list store

(defun demo-list-store ()
  (within-main-loop
    (let-ui
      (gtk-window :type :toplevel
                  :title "GtkListStore"
                  :default-width 600
                  :default-height 400
                  :var w
                  (gtk-vbox
                    (gtk-label :label "A GtkListStore")
                               :expand nil
                               (gtk-scrolled-window :hscrollbar-policy
                                                    :automatic
                                                    :vscrollbar-policy
                                                    :automatic
                                                    (gtk-tree-view :var tv))))
      (let ((l (make-instance 'gtk-list-store
                              :column-types '("gint" "gchararray"))))
      (iter (for i from 0 below 100)
            (for n = (random 10000000))
            (for s = (format nil "~R" n))
            (gtk-list-store-insert-with-values l i n s))
      (setf (gtk-tree-view-model tv) l)
      (let ((column (make-instance 'gtk-tree-view-column
                                   :title "Number"
                                   :sort-column-id 0))
           (renderer (make-instance 'gtk-cell-renderer-text :text "A text")))
        (gtk-tree-view-column-pack-start column renderer)
        (gtk-tree-view-column-add-attribute column renderer "text" 0)
        (gtk-tree-view-append-column tv column))
      (let ((column (make-instance 'gtk-tree-view-column
                                   :title "As string"
                                   :sort-column-id 1))
            (renderer (make-instance 'gtk-cell-renderer-text :text "A text")))
        (gtk-tree-view-column-pack-start column renderer)
        (gtk-tree-view-column-add-attribute column renderer "text" 1)
        (gtk-tree-view-append-column tv column))
      (g-signal-connect tv "row-activated"
         (lambda (w path column)
           (declare (ignore w column))
           (let* ((iter (gtk-tree-model-get-iter l path))
                  (n (gtk-tree-model-get-value l iter 0))
                  (dialog (make-instance 'gtk-message-dialog
                                         :title "Clicked"
                                         :text
                                         (format nil "Number ~A was clicked" n)
                                         :buttons :ok)))
             (gtk-dialog-run dialog)
             (gtk-widget-destroy dialog)))))
      (g-signal-connect w "destroy"
                        (lambda (w)
                          (declare (ignore w))
                          (gtk-main-quit)))
     (gtk-widget-show w))))

;;; ----------------------------------------------------------------------------

;; Demo of tree store

(defun demo-tree-store ()
  (within-main-loop
    (let-ui
      (gtk-window :type :toplevel
                  :title "GtkListStore"
                  :default-width 600
                  :default-height 400
                  :var w
                  (gtk-vbox
                    (gtk-label :label "A GtkListStore")
                               :expand nil
                               (gtk-scrolled-window :hscrollbar-policy
                                                    :automatic
                                                    :vscrollbar-policy
                                                    :automatic
                                                    (gtk-tree-view :var tv))))
      (let ((l (make-instance 'gtk-tree-store
                              :column-types '("gint" "gchararray"))))
        (iter (for i from 0 below 100)
              (for n = (random 10000000))
              (for s = (format nil "~R" n))
              (for it = (gtk-tree-store-insert-with-values l nil i n s))
              (iter (for j from 0 below 10)
                    (for n2 = (random 10000000))
                    (for s2 = (format nil "~R" n2))
                    (gtk-tree-store-insert-with-values l it j n2 s2)))
        (setf (gtk-tree-view-model tv) l)
        (let ((column (make-instance 'gtk-tree-view-column
                                     :title "Number"
                                     :sort-column-id 0))
              (renderer (make-instance 'gtk-cell-renderer-text :text "A text")))
          (gtk-tree-view-column-pack-start column renderer)
          (gtk-tree-view-column-add-attribute column renderer "text" 0)
          (gtk-tree-view-append-column tv column))
        (let ((column (make-instance 'gtk-tree-view-column
                                     :title "As string"
                                     :sort-column-id 1))
              (renderer (make-instance 'gtk-cell-renderer-text :text "A text")))
          (gtk-tree-view-column-pack-start column renderer)
          (gtk-tree-view-column-add-attribute column renderer "text" 1)
          (gtk-tree-view-append-column tv column))
        (g-signal-connect tv "row-activated"
           (lambda (w path column)
             (declare (ignore w column))
             (let* ((iter (gtk-tree-model-get-iter l path))
                    (n (gtk-tree-model-get-value l iter 0))
                    (dlg (make-instance 'gtk-message-dialog
                                        :title "Clicked"
                                        :text
                                        (format nil "Number ~A was clicked" n)
                                        :buttons :ok)))
                              (gtk-dialog-run dlg)
                              (gtk-widget-destroy dlg)))))
      (g-signal-connect w "destroy"
                        (lambda (w)
                          (declare (ignore w))
                          (gtk-main-quit)))
            (gtk-widget-show w))))

;;; ----------------------------------------------------------------------------

;; Demo of UI Markup

(defun demo-ui-markup ()
  (within-main-loop
    (let ((label (make-instance 'gtk-label :label "Hello!")))
      (let-ui
        (gtk-window :type :toplevel
                    :position :center
                    :title "Hello, world!"
                    :default-width 300
                    :default-height 400
                    :var w
          (gtk-vbox (:expr label)
                    :expand nil
                    (gtk-scrolled-window :hscrollbar-policy :automatic
                                         :vscrollbar-policy :automatic
                                         :shadow-type :etched-in
                                         (gtk-text-view :var tv))
                    (gtk-hbox (gtk-label :label "Insert:")
                               :expand nil
                               (gtk-entry :var entry)
                               (gtk-button :label "gtk-ok"
                                           :use-stock t
                                           :var btn)
                               :expand nil)
                     :expand nil
                     (gtk-label :label "Table packing")
                     :expand nil
                     (gtk-table :n-columns 2
                                :n-rows 2
                                (gtk-label :label "2 x 1")
                                :left 0
                                :right 2
                                :top 0
                                :bottom 1
                                (gtk-label :label "1 x 1")
                                :left 0
                                :right 1
                                :top 1
                                :bottom 2
                                (gtk-label :label "1 x 1")
                                :left 1
                                :right 2
                                :top 1
                                :bottom 2)))
        (g-signal-connect w "destroy"
                          (lambda (w)
                            (declare (ignore w))
                            (gtk-main-quit)))
        (g-signal-connect btn "clicked"
                          (lambda (b)
                            (declare (ignore b))
                            (gtk-text-buffer-insert (gtk-text-view-buffer tv)
                                                    (gtk-entry-text entry))))
              (gtk-widget-show w)))))

;;; ----------------------------------------------------------------------------

;; Demo of GtkAssistant

(defun demo-assistant ()
  (within-main-loop
    (let ((output *standard-output*)
          (d (make-instance 'gtk-assistant :title "Username wizard"))
          (p-1 (make-instance 'gtk-hbox))
          (entry (make-instance 'gtk-entry))
          (p-2 (make-instance 'gtk-label
                              :label "Click Apply to close this wizard")))
      (gtk-box-pack-start p-1 (make-instance 'gtk-label
                                             :label "Enter your name:")
                          :expand nil)
      (gtk-box-pack-start p-1 entry)
      (gtk-assistant-append-page d p-1)
      (gtk-assistant-append-page d p-2)
      (setf (gtk-assistant-child-title d p-1) "Username wizard"
            (gtk-assistant-child-title d p-2) "Username wizard"
            (gtk-assistant-child-complete d p-1) nil
            (gtk-assistant-child-complete d p-2) t
            (gtk-assistant-child-page-type d p-1) :intro
            (gtk-assistant-child-page-type d p-2) :confirm)
      (gtk-assistant-set-forward-page-func d
           (lambda (i)
             (format output "(assistant-forward-page-function ~A)~%" i)
             (ecase i
               (0 1)
               (1 -1))))
      (g-signal-connect entry "notify::text"
         (lambda (object pspec)
           (declare (ignore object pspec))
           (setf (gtk-assistant-child-complete d p-1)
                 (plusp (length (gtk-entry-text entry))))))
      (let ((w (make-instance 'gtk-label :label "A label in action area")))
        (gtk-widget-show w)
        (gtk-assistant-add-action-widget d w))
      (g-signal-connect d "destroy"
                        (lambda (w)
                          (declare (ignore w))
                          (leave-gtk-main)))
      (g-signal-connect d "cancel"
                        (lambda (assistant)
                          (declare (ignore assistant))
                          (gtk-widget-destroy d)
                          (show-message "Canceled")))
      (g-signal-connect d "close"
                        (lambda (assistant)
                          (declare (ignore assistant))
                          (gtk-widget-destroy d)
                          (show-message (format nil "Thank you, ~A!"
                                                (gtk-entry-text entry)))))
      (g-signal-connect d "prepare"
         (lambda (assistant page-widget)
           (declare (ignore assistant page-widget))
           (format output
                   "Assistant ~A has ~A pages and is on ~Ath page~%"
                   d
                   (gtk-assistant-n-pages d)
                   (gtk-assistant-current-page d))))
      (gtk-widget-show d))))

;;; ----------------------------------------------------------------------------

;; Demo of GtkBuilder

(defun demo-builder ()
  (within-main-loop
    (let ((builder (make-instance 'gtk-builder)))
      (gtk-builder-add-from-file builder "demo-builder.ui")
      (let ((text-view (gtk-builder-get-object builder "textview1"))
            (c 0))
        (gtk-builder-connect-signals-simple builder
         `(("toolbutton1_clicked_cb"
            ,(lambda (b)
               (declare (ignore b))
               (setf (gtk-text-buffer-text (gtk-text-view-buffer text-view))
                     (format nil
                             "Clicked ~A times~%"
                             (incf c)))
               (gtk-statusbar-pop (gtk-builder-get-object builder "statusbar1")
                                  "times")
               (gtk-statusbar-push (gtk-builder-get-object builder "statusbar1")
                                   "times"
                                   (format nil "~A times" c))))
           ("quit_cb" ,(lambda (&rest args)
                         (print args)
                         (gtk-widget-destroy
                           (gtk-builder-get-object builder "window1"))))
           ("about_cb" ,(lambda (&rest args)
                          (print args)
                          (let ((d (make-instance 'gtk-about-dialog
                                                  :program-name
                                                  "GtkBuilder text"
                                                  :version "0.00001"
                                                  :authors '("Dmitry Kalyanov")
                                                  :logo-icon-name "gtk-apply")))
                            (gtk-dialog-run d)
                            (gtk-widget-destroy d)))))))
      (g-signal-connect (gtk-builder-get-object builder "window1") "destroy"
                        (lambda (w)
                          (declare (ignore w))
                          (gtk-main-quit)))
      (gtk-statusbar-push (gtk-builder-get-object builder "statusbar1")
                          "times" "0 times")
      (gtk-widget-show (gtk-builder-get-object builder "window1")))))

;;; ----------------------------------------------------------------------------

;; More advanced example: text editor with ability to evaluate lisp expressions

(defun read-text-file (file-name)
  (with-output-to-string (str)
    (with-open-file (file file-name)
      (loop
         for line = (read-line file nil nil)
         while line
         do (fresh-line str)
         do (write-string line str)))))

(defun demo-text-editor ()
  (within-main-loop
    (let* ((builder (let ((builder (make-instance 'gtk-builder)))
                      (gtk-builder-add-from-file builder "demo-text-editor.ui")
                      builder))
           (window (gtk-builder-get-object builder "window1"))
           (text-view (gtk-builder-get-object builder "textview1"))
           (statusbar (gtk-builder-get-object builder "statusbar1"))
           (file-name nil)
           (modified-p t))
      (gtk-statusbar-push statusbar "filename" "Untitled *")
      (labels ((set-properties ()
                 (gtk-statusbar-pop statusbar "filename")
                 (gtk-statusbar-push statusbar
                                 "filename"
                                 (format nil "~A~:[~; *~]"
                                         (or file-name "Untitled")
                                         modified-p)))
               (new (&rest args)
                 (declare (ignore args))
                 (setf file-name nil
                       modified-p t
                       (gtk-text-buffer-text (gtk-text-view-buffer text-view))
                       "")
                 (set-properties))
               (cb-open (&rest args)
                 (declare (ignore args))
                 (let ((d (make-instance 'gtk-file-chooser-dialog
                                         :action :open
                                         :title "Open file")))
                   (when file-name
                     (setf (gtk-file-chooser-filename d) file-name))
                   (gtk-dialog-add-button d "gtk-open" :accept)
                   (gtk-dialog-add-button d "gtk-cancel" :cancel)
                   (when (eq :accept (gtk-dialog-run d))
                     (setf file-name (gtk-file-chooser-filename d)
                           (gtk-text-buffer-text
                             (gtk-text-view-buffer text-view))
                           (read-text-file file-name)
                           modified-p nil)
                     (set-properties))
                   (gtk-widget-destroy d)))
               (save (&rest args)
                 (declare (ignore args))
                 (if file-name
                     (progn
                       (with-open-file (file file-name :direction :output
                                                       :if-exists :supersede)
                         (write-string
                           (gtk-text-buffer-text
                             (gtk-text-view-buffer text-view))
                           file))
                       (setf modified-p nil)
                       (set-properties))
                       (save-as)))
               (save-as (&rest args)
                 (declare (ignore args))
                 (let ((d (make-instance 'gtk-file-chooser-dialog
                                         :action :save
                                         :title "Save file")))
                   (when file-name
                     (setf (gtk-file-chooser-filename d) file-name))
                   (gtk-dialog-add-button d "gtk-save" :accept)
                   (gtk-dialog-add-button d "gtk-cancel" :cancel)
                   (if (eq :accept (gtk-dialog-run d))
                       (progn
                         (setf file-name (gtk-file-chooser-filename d))
                         (gtk-widget-destroy d)
                         (save))
                       (gtk-widget-destroy d))))
               (cut (&rest args)
                 (declare (ignore args))
                    (gtk-text-buffer-cut-clipboard
                                                (gtk-text-view-buffer text-view)
                                                (gtk-clipboard-get "CLIPBOARD")
                                                t))
               (copy (&rest args)
                 (declare (ignore args))
                 (gtk-text-buffer-copy-clipboard
                                               (gtk-text-view-buffer text-view)
                                               (gtk-clipboard-get "CLIPBOARD")))
               (paste (&rest args)
                 (declare (ignore args))
                 (gtk-text-buffer-paste-clipboard
                                               (gtk-text-view-buffer text-view)
                                               (gtk-clipboard-get "CLIPBOARD")))
               (cb-delete (&rest args)
                 (declare (ignore args))
                 (let ((buffer (gtk-text-view-buffer text-view)))
                   (multiple-value-bind (i1 i2)
                       (gtk-text-buffer-get-selection-bounds buffer)
                     (when (and i1 i2)
                       (gtk-text-buffer-delete buffer i1 i2)))))
               (about (&rest args)
                 (declare (ignore args))
                 (let ((d (make-instance 'gtk-about-dialog
                                         :program-name
                                         "Lisp Gtk+ Binding Demo Text Editor"
                                         :version
                                         (format nil "0.0.0.1 ~A"
                                                 #\GREEK_SMALL_LETTER_ALPHA)
                                         :authors '("Kalyanov Dmitry")
                                         :license "Public Domain"
                                         :logo-icon-name
                                         "accessories-text-editor")))
                   (gtk-dialog-run d)
                   (gtk-widget-destroy d)))
               (quit (&rest args)
                 (declare (ignore args))
                 (gtk-widget-destroy window))
               (cb-eval (&rest args)
                 (declare (ignore args))
                 (let ((buffer (gtk-text-view-buffer text-view)))
                   (multiple-value-bind (i1 i2)
                       (gtk-text-buffer-get-selection-bounds buffer)
                     (when (and i1 i2)
                       (with-gtk-message-error-handler
                         (let* ((text (gtk-text-buffer-slice buffer i1 i2))
                                (value (eval (read-from-string text)))
                                (value-str (format nil "~A" value))
                                (pos (max (gtk-text-iter-offset i1)
                                          (gtk-text-iter-offset i2))))
                           (gtk-text-buffer-insert
                                buffer
                                " => "
                                :position
                                (gtk-text-buffer-get-iter-at-offset buffer pos))
                           (incf pos (length " => "))
                           (gtk-text-buffer-insert
                                buffer
                                value-str
                                :position
                                (gtk-text-buffer-get-iter-at-offset buffer
                                                                    pos)))))))))
        (gtk-builder-connect-signals-simple builder
                                            `(("new" ,#'new)
                                              ("open" ,#'cb-open)
                                              ("save" ,#'save)
                                              ("save-as" ,#'save-as)
                                              ("cut" ,#'cut)
                                              ("copy" ,#'copy)
                                              ("paste" ,#'paste)
                                              ("delete" ,#'cb-delete)
                                              ("about" ,#'about)
                                              ("quit" ,#'quit)
                                              ("eval" ,#'cb-eval)))
        (g-signal-connect window "destroy"
                          (lambda (w)
                            (declare (ignore w))
                            (gtk-main-quit)))
        (g-signal-connect (gtk-text-view-buffer text-view) "changed"
                          (lambda (b)
                            (declare (ignore b))
                            (setf modified-p t)
                            (set-properties)))
        (gtk-widget-show window)))))

;;; ----------------------------------------------------------------------------

;; Show slots of a class

(defun demo-class-browser ()
  (within-main-loop
    (let* ((output *standard-output*)
           (window (make-instance 'gtk-window
                                  :window-position :center
                                  :title "Class Browser"
                                  :default-width 400
                                  :default-height 600))
           (search-entry (make-instance 'gtk-entry))
           (search-button (make-instance 'gtk-button :label "Search"))
           (scroll (make-instance 'gtk-scrolled-window
                                  :hscrollbar-policy :automatic
                                  :vscrollbar-policy :automatic))
           (slots-model (make-instance 'array-list-store))
           (slots-list (make-instance 'gtk-tree-view :model slots-model)))
      (let ((v-box (make-instance 'gtk-vbox))
            (search-box (make-instance 'gtk-hbox)))
        (gtk-container-add window v-box)
        (gtk-box-pack-start v-box search-box :expand nil)
        (gtk-box-pack-start search-box search-entry)
        (gtk-box-pack-start search-box search-button :expand nil)
        (gtk-box-pack-start v-box scroll)
        (gtk-container-add scroll slots-list))
      (store-add-column slots-model "gchararray"
                        (lambda (slot)
                          (format nil "~S"
                                  (closer-mop:slot-definition-name slot))))
      (let ((col (make-instance 'gtk-tree-view-column :title "Slot name"))
            (cr (make-instance 'gtk-cell-renderer-text)))
        (gtk-tree-view-column-pack-start col cr)
        (gtk-tree-view-column-add-attribute col cr "text" 0)
        (gtk-tree-view-append-column slots-list col))
      (labels ((display-class-slots (class)
                 (format output "Displaying ~A~%" class)
                 (loop
                    repeat (store-items-count slots-model)
                    do (store-remove-item slots-model (store-item slots-model 0)))
                 (closer-mop:finalize-inheritance class)
                 (loop
                    for slot in (closer-mop:class-slots class)
                    do (store-add-item slots-model slot)))
               (on-search-clicked (button)
                                  (declare (ignore button))
                 (in-package :gtk)
                 (with-gtk-message-error-handler
                   (let* ((class-name (read-from-string
                                        (gtk-entry-text search-entry)))
                          (class (find-class class-name)))
                     (display-class-slots class)))))
        (g-signal-connect search-button "clicked" #'on-search-clicked))
      (g-signal-connect window "destroy"
                        (lambda (w)
                          (declare (ignore w))
                          (gtk-main-quit)))
      (gtk-widget-show window))))

;;; ----------------------------------------------------------------------------

;; Simple test of non-GObject subclass of GtkWindow

(defclass custom-window (gtk-window)
  ((label :initform (make-instance 'gtk-label
                                   :label "A label text")
          :reader custom-window-label)
   (button :initform (make-instance 'gtk-button :label "Click me!")
           :reader custom-window-button))
  (:metaclass gobject-class)
  (:default-initargs :title "Custom window with default initargs"
                     :default-width 320
                     :default-height 240))

(defun custom-window-label-text (w)
  (gtk-label-label (custom-window-label w)))

(defun (setf custom-window-label-text) (new-value w)
  (setf (gtk-label-label (custom-window-label w)) new-value))

(defun custom-window-button-clicked (w)
  (setf (custom-window-label-text w)
        (format nil "Now is: ~A~%" (get-internal-run-time))))

(defmethod initialize-instance :after ((w custom-window) &key &allow-other-keys)
  (let ((box (make-instance 'gtk-vbox)))
    (gtk-box-pack-start box (custom-window-label w))
    (gtk-box-pack-start box (custom-window-button w) :expand nil)
    (gtk-container-add w box))
  (g-signal-connect (custom-window-button w) "clicked"
                    (lambda (b)
                      (declare (ignore b))
                      (custom-window-button-clicked w))))
 
(defun demo-custom-window ()
  (within-main-loop
    (let ((w (make-instance 'custom-window)))
      (g-signal-connect w "destroy"
                        (lambda (w)
                          (declare (ignore w))
                          (gtk-main-quit)))
      (gtk-widget-show w))))

;;; ----------------------------------------------------------------------------

;; Demo Dialog

(defun demo-dialog ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :type :toplevel
                                 :title "Testing dialogs"
                                 :default-width 250))
          (v-box (make-instance 'gtk-vbox)))
      (g-signal-connect window
                        "destroy"
                        (lambda (w)
                          (declare (ignore w)) 
                          (gtk-main-quit)))
      (gtk-container-add window v-box)
      (let ((button (make-instance 'gtk-button :label "Dialog 1")))
        (gtk-box-pack-start v-box button)
        (g-signal-connect button "clicked"
           (lambda (b)
             (declare (ignore b))
             (let ((dialog (make-instance 'gtk-dialog
                                          :title "A Dialog Window")))
               (gtk-dialog-add-button dialog "OK" :ok)
               (gtk-dialog-add-button dialog "Yes" :yes)
               (gtk-dialog-add-button dialog "Cancel" :cancel)
               (setf (gtk-dialog-default-response dialog) :cancel)
               (gtk-dialog-set-alternative-button-order dialog 
                                                        (list :yes :cancel :ok))
               (format t "Response was: ~S~%" (gtk-dialog-run dialog))
               (gtk-widget-destroy dialog)))))
      (let ((button (make-instance 'gtk-button :label "About")))
        (gtk-box-pack-start v-box button)
        (g-signal-connect button "clicked"
           (lambda (b)
             (declare (ignore b))
             (let ((dialog (make-instance 'gtk-about-dialog
                                          :program-name "Dialogs examples"
                                          :version "0.01"
                                          :copyright "(c) Kalyanov Dmitry"
                                          :website
                                          "http://common-lisp.net/project/cl-gtk+"
                                          :website-label "Project web site"
                                          :license "LLGPL"
                                          :authors '("Kalyanov Dmitry")
                                          :documenters '("Kalyanov Dmitry")
                                          :artists '("None")
                                          :logo-icon-name
                                          "applications-development"
                                          :wrap-license t)))
               (format t "Response was: ~S~%" (gtk-dialog-run dialog))
               (gtk-widget-destroy dialog)))))
      (gtk-widget-show window))))

;;; ----------------------------------------------------------------------------
