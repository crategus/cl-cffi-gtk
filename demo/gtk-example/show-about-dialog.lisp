
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
          
(defun example-show-about-dialog ()
  (within-main-loop
    (let (;; Create a toplevel window
          (window (gtk-window-new :toplevel))
          (button (make-instance 'gtk-button :label "Show About Dialog")))
      ;; Signal handler for the window to handle the signal "destroy"
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))
      (g-signal-connect button "clicked"
         (lambda (widget)
           (declare (ignore widget))
           ;; Create and show an about dialog
           (gtk-show-about-dialog window :title "About Dialog"
                                         :program-name "ExampleDialog"
                                         :version "0.00"
                                         :copyright "(c) Dieter Kaiser"
                                         :website
                                         "http:\\github.com/crategus/cl-cffi-gtk"
                                         :website-label "Project web site"
                                         :license (license-text)
                                         :authors '("Kalyanov Dmitry"
                                                    "Dieter Kaiser")
                                         :documenters '("Dieter Kaiser")
                                         :artists '("None")
                                         :logo-icon-name
                                         "applications-development"
                                         :wrap-license t)))
      ;; Add the button to the window
      (gtk-container-add window button)      
      ;; Show the window
      (gtk-widget-show-all window))))

      
      
