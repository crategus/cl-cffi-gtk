;;;; Labels

(in-package #:gtk-demo)

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

