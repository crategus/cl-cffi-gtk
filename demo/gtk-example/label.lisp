;;;; Example Labels - 2021-12-21

(in-package :gtk-example)

(defparameter *label-text-1* "This is a normal label.")
(defparameter *label-text-2*
              (format nil "This is a multiline label.~%~
                           Second line.~%~
                           Third line."))
(defparameter *label-text-3*
              (format nil
                      "This is a center justified~%~
                       multiline label.~%~
                       Third line."))
(defparameter *label-text-4*
              (format nil
                      "This is a right justified~%~
                       multiline label.~%~
                       Third line."))
(defparameter *label-text-5*
              (format nil
                      "This label is underlined.~%~%~
                       This label is underlined~%~
                       in quite a funky fashion."))
(defvar *lorem-ipsum-short*
        (format nil "Lorem ipsum dolor sit amet, consectetur adipiscing elit. ~
Nunc scelerisque aliquam dui id ullamcorper. Sed placerat felis sed aliquam ~
sodales. Cras et ultricies nulla. Nullam ipsum ante, gravida vel malesuada ac, ~
sollicitudin eu diam. Morbi pellentesque elit et odio hendrerit dignissim. ~
Maecenas sagittis auctor leo a dictum. Sed at auctor."))

(defparameter *label-pattern*
  "_________________________ _ _________ _ ______     __ _______ ___")


(defun make-heading (text)
  (make-instance 'gtk-label
                 :xalign 0
                 :margin-top 6
                 :use-markup t
                 :label (format nil "<b>~A</b>" text)))

(defun example-label (&optional application)
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :type :toplevel
                                 :application application
                                 :title "Example Labels"
                                 :default-width 250
                                 :border-width 18))
          (vbox1 (make-instance 'gtk-box
                                :orientation :vertical
                                :spacing 6))
          (vbox2 (make-instance 'gtk-box
                                :orientation :vertical
                                :spacing 6))
          (hbox (make-instance 'gtk-box
                               :orientation :horizontal
                               :spacing 24)))
      ;; Connect a handler for the signal "destroy" to window.
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))
      ;; Create a Normal Label
      (gtk-box-pack-start vbox1
                          (make-heading "Normal Label")
                          :expand nil)
      (gtk-box-pack-start vbox1
                          (make-instance 'gtk-label
                                         :label *label-text-1*)
                          :expand nil)
      ;; Create a Multi-line Label
      (gtk-box-pack-start vbox1
                          (make-heading "Multiline Label")
                          :expand nil)
      (gtk-box-pack-start vbox1
                          (make-instance 'gtk-label
                                         :label *label-text-2*)
                          :expand nil)
      ;; Create a Left Justified Label
      (gtk-box-pack-start vbox1
                          (make-heading "Center Justified Label")
                          :expand nil)
      (gtk-box-pack-start vbox1
                          (make-instance 'gtk-label
                                         :justify :center
                                         :label *label-text-3*)
                          :expand nil)
      ;; Create a Right Justified Label
      (gtk-box-pack-start vbox1
                          (make-heading "Right Justified Label")
                          :expand nil)
      (gtk-box-pack-start vbox1
                          (make-instance 'gtk-label
                                         :justify :right
                                         :label *label-text-4*)
                          :expand nil)
      ;; Create an underlined label
      (gtk-box-pack-start vbox1
                          (make-heading "Underlined Label")
                          :expand nil)
      (gtk-box-pack-start vbox1
                          (make-instance 'gtk-label
                                         :justify :left
                                         :use-underline t
                                         :pattern *label-pattern*
                                         :label *label-text-5*)
                          :expand nil)

      ;; Create a Line wrapped label
      (gtk-box-pack-start vbox2
                          (make-heading "Line Wrapped Label")
                          :expand nil)
      (gtk-box-pack-start vbox2
                          (make-instance 'gtk-label
                                         :wrap t
                                         :label *lorem-ipsum-short*)
                          :expand nil)
      ;; Create a Filled and wrapped label
      (gtk-box-pack-start vbox2
                          (make-heading "Filled and Wrapped Label")
                          :expand nil)
      (gtk-box-pack-start vbox2
                          (make-instance 'gtk-label
                                         :wrap t
                                         :justify :fill
                                         :label *lorem-ipsum-short*)
                          :expand nil)

      ;; Put the boxes into the window and show the window
      (gtk-box-pack-start hbox vbox1 :expand nil)
      (gtk-box-pack-start hbox (gtk-separator-new :vertical))
      (gtk-box-pack-start hbox vbox2 :expand nil)
      (gtk-container-add window hbox)
      (gtk-widget-show-all window))))
