;;;; Dialogs

(in-package #:gtk-demo)

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
                               :transient-for *demo-window*
                               :has-separator t)))
    (setf (gtk-window-transient-for dialog) *demo-window*)
    ;; Add a border width to the vbox of the content area
    (setf (gtk-container-border-width (gtk-dialog-get-content-area dialog)) 12)
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
      (gtk-box-pack-start (gtk-dialog-get-content-area dialog) vbox)
      ;; Show the content area of the dialog
      (gtk-widget-show-all (gtk-dialog-get-content-area dialog)))
    ;; Add buttons with a stock id to the action area
    (gtk-dialog-add-button dialog "gtk-yes" :yes)
    (gtk-dialog-add-button dialog "gtk-no" :no)
    (gtk-dialog-add-button dialog "gtk-cancel" :cancel)
    (gtk-dialog-add-button dialog "_Custom" 1)
    (gtk-dialog-set-default-response dialog :cancel)
    ;; Change the order of the buttons
    (gtk-dialog-set-alternative-button-order dialog
                                             (list 1 :yes :cancel :no))
    ;; Run the dialog and print the message on the console
    (format t "Response was: ~S~%" (gtk-dialog-run dialog))
    ;; Destroy the dialog
    (gtk-widget-destroy dialog)))

(defun create-message-dialog ()
  (let ((dialog (make-instance 'gtk-message-dialog
                               :message-type :info
                               :buttons :ok
                               :text "Info Message Dialog"
                               :transient-for *demo-window*
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
                               :transient-for *demo-window*
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
                          (make-instance 'gtk-hseparator))
      ;; Create a quit button
      (let ((button (make-instance 'gtk-button
                                   :label "Quit")))
        (g-signal-connect button "clicked"
                          (lambda (widget)
                            (declare (ignore widget))
                            (gtk-widget-destroy window)))
        (gtk-box-pack-start vbox button))
      (gtk-widget-show-all window))))

