;;;; Example Dialog Windows - 2021-12-3

(in-package :gtk-example)

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

(defun create-dialog (&optional (headerbar-p -1))
  (let ((dialog (make-instance 'gtk-dialog
                               :use-header-bar headerbar-p
                               :title "Dialog Window")))
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
                                         put in the widgets."))))
      (gtk-box-pack-start vbox label)
      (gtk-box-pack-start (gtk-dialog-content-area dialog) vbox)
      ;; Show the content area of the dialog
      (gtk-widget-show-all (gtk-dialog-content-area dialog)))
    ;; Add buttons with a stock id to the action area
    (gtk-dialog-add-button dialog "gtk-yes" :yes)
    (gtk-dialog-add-button dialog "gtk-cancel" :cancel)
    (gtk-dialog-set-default-response dialog :cancel)
    ;; Run the dialog and print the message on the console
    (format t "Response is: ~s~%" (gtk-dialog-run dialog))
    ;; Destroy the dialog
    (gtk-widget-destroy dialog)))

(defun create-message-dialog (&optional (mtype :info))
  (let ((dialog (make-instance 'gtk-message-dialog
                               :message-type mtype
                               :buttons :cancel
                               :text "Message Dialog"
                               :secondary-text
                               (format nil
                                       "This is a message dialog of type ~a."
                                       mtype))))
    ;; Run the message dialog
    (gtk-dialog-run dialog)
    ;; Destroy the message dialog
    (gtk-widget-destroy dialog)))

(defun create-about-dialog (&optional (headerbar-p 1))
  (let ((dialog (make-instance 'gtk-about-dialog
                               :use-header-bar headerbar-p
                               :program-name "GTK Demo"
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

(defun example-dialogs (&optional application)
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :application application
                                 :type :toplevel
                                 :title "Example Dialogs"
                                 :default-width 270
                                 :border-width 12))
          (vbox (make-instance 'gtk-box
                               :orientation :vertical
                               :spacing 6))
          (check (make-instance 'gtk-check-button
                                :margin-bottom 18
                                :label "Show Dialog with Header Bar"))
          (radio (gtk-radio-button-new-with-label nil "Info")))
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))
      ;; Show dialog
      (let ((button (make-instance 'gtk-button
                                   :label "Show Dialog")))
        (gtk-box-pack-start vbox button)
        (g-signal-connect button "clicked"
           (lambda (widget)
             (declare (ignore widget))
             ;; Create and show the dialog
             (create-dialog (if (gtk-toggle-button-active check)
                                1
                                -1)))))
      ;; Show about dialog
      (let ((button (make-instance 'gtk-button
                                   :label "Show About Dialog")))
        (gtk-box-pack-start vbox button)
        (g-signal-connect button "clicked"
           (lambda (widget)
             (declare (ignore widget))
             ;; Create and show the about dialog
             (create-about-dialog (if (gtk-toggle-button-active check)
                                      1
                                      -1)))))
      ;; Add the check button to the vertical box
      (gtk-box-pack-start vbox check)
      ;; Show message dialog
      (let ((mtype :info)
            (button (make-instance 'gtk-button
                                   :label "Show Message Dialog")))
        (gtk-box-pack-start vbox button)
        (g-signal-connect button "clicked"
           (lambda (widget)
             (declare (ignore widget))
             ;; Select the active radio button
             (dolist (radio (gtk-radio-button-get-group radio))
               (when (gtk-toggle-button-active radio)
                 (setf mtype
                       (cdr (assoc (gtk-button-label radio)
                                   '(("Info" . :info)
                                     ("Warning" . :warning)
                                     ("Question" . :question)
                                     ("Error" . :error))
                                   :test #'string=)))))
             ;; Create and show the message dialog
             (create-message-dialog mtype))))
      ;; Add the radio buttons to select the message type
      (let ((hbox (make-instance 'gtk-box
                                 :orientation :horizontal)))
        (gtk-box-pack-start hbox radio)
        (setf radio
              (gtk-radio-button-new-with-label
                                          (gtk-radio-button-get-group radio)
                                          "Warning"))
        (gtk-box-pack-start hbox radio)
        (setf radio
              (gtk-radio-button-new-with-label
                                          (gtk-radio-button-get-group radio)
                                          "Question"))
        (gtk-box-pack-start hbox radio)
        (setf radio
              (gtk-radio-button-new-with-label
                                          (gtk-radio-button-get-group radio)
                                          "Error"))
        (gtk-box-pack-start hbox radio)
        (gtk-container-add vbox hbox))
      ;; Pack and show the widgets
      (gtk-container-add window vbox)
      (gtk-widget-show-all window))))
