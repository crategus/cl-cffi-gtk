;;;; Reveaker

;(in-package #:gtk-demo)

(defun example-revealer ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :type :toplevel
                                 :title "Example Revealer"
                                 :default-width 300
                                 :default-height 300
                                 :border-width 12))
          (grid   (make-instance 'gtk-grid)))

      (g-signal-connect window "destroy"         
                               (lambda (widget)
                                 (declare (ignore widget))
                                 (leave-gtk-main)))

      (gtk-grid-attach grid
                       (make-instance 'gtk-label
                                      :label (format nil
                                                   "Some filler text to avoid~%~
                                                    resizing of the window")
                                      :margin-top 10
                                      :margin-bottom 10
                                      :margin-start 10
                                      :margin-end 10)
                       1 1 1 1)

      (gtk-grid-attach grid
                       (make-instance 'gtk-label
                                      :label (format nil
                                                   "Some filler text to avoid~%~
                                                    resizing of the window")
                                      :margin-top 10
                                      :margin-bottom 10
                                      :margin-start 10
                                      :margin-end 20)
                       3 3 1 1)
                                              
      (let ((toggle   (make-instance 'gtk-toggle-button
                                     :label "None"))             
            (revealer (make-instance 'gtk-revealer
                                     :halign :start
                                     :valign :start
                                     :transition-type :none
                                     :transition-duration 2000))
            (entry    (make-instance 'gtk-entry
                                     :text "00000")))
        (gtk-container-add revealer entry)
        (g-object-bind-property toggle "active" 
                                revealer "reveal-child" 
                                :default)
        (gtk-grid-attach grid toggle 0 0 1 1)
        (gtk-grid-attach grid revealer 1 0 1 1)
      )
      
      (let ((toggle   (make-instance 'gtk-toggle-button
                                     :label "Fade"))             
            (revealer (make-instance 'gtk-revealer
                                     :halign :end
                                     :valign :end
                                     :transition-type :crossfade
                                     :transition-duration 2000))
            (entry    (make-instance 'gtk-entry
                                     :text "00000")))
        (gtk-container-add revealer entry)
        (g-object-bind-property toggle "active" 
                                revealer "reveal-child" 
                                :default)
        (gtk-grid-attach grid toggle 4 4 1 1)
        (gtk-grid-attach grid revealer 3 4 1 1)
      )
      
      (let ((toggle   (make-instance 'gtk-toggle-button
                                     :label "Right"))             
            (revealer (make-instance 'gtk-revealer
                                     :halign :start
                                     :hexpand t
                                     :transition-type :slide-right
                                     :transition-duration 2000))
            (entry    (make-instance 'gtk-entry
                                     :text "12345")))
        (gtk-container-add revealer entry)
        (g-object-bind-property toggle "active" 
                                revealer "reveal-child" 
                                :default)
        (gtk-grid-attach grid toggle 0 2 1 1)
        (gtk-grid-attach grid revealer 1 2 1 1)
      )
      
      (let ((toggle   (make-instance 'gtk-toggle-button
                                     :label "Down"))   
            (revealer (make-instance 'gtk-revealer
                                     :valign :start
                                     :vexpand t
                                     :transition-type :slide-down
                                     :transition-duration 2000))
            (entry    (make-instance 'gtk-entry
                                     :text "23456")))
        (gtk-container-add revealer entry)
        (g-object-bind-property toggle "active" 
                                revealer "reveal-child" 
                                :default)
        (gtk-grid-attach grid toggle 2 0 1 1)
        (gtk-grid-attach grid revealer 2 1 1 1)
      )
      
      (let ((toggle   (make-instance 'gtk-toggle-button
                                     :label "Left"))   
            (revealer (make-instance 'gtk-revealer
                                     :halign :end
                                     :hexpand t
                                     :transition-type :slide-left
                                     :transition-duration 2000))
            (entry    (make-instance 'gtk-entry
                                     :text "34567")))
        (gtk-container-add revealer entry)
        (g-object-bind-property toggle "active" 
                                revealer "reveal-child"
                                :default)
        (gtk-grid-attach grid toggle 4 2 1 1)
        (gtk-grid-attach grid revealer 3 2 1 1)
      )
      
      (let ((toggle   (make-instance 'gtk-toggle-button
                                     :label "Up"))
            (revealer (make-instance 'gtk-revealer
                                     :valign :end
                                     :vexpand t
                                     :transition-type :slide-up
                                     :transition-duration 2000))
            (entry    (make-instance 'gtk-entry
                                     :text "45678")))
        (gtk-container-add revealer entry)
        (g-object-bind-property toggle "active" 
                                revealer "reveal-child"
                                :default)
        (gtk-grid-attach grid toggle 2 4 1 1)
        (gtk-grid-attach grid revealer 2 3 1 1)
      )
      (gtk-container-add window grid)
      (gtk-widget-show-all window))))
      
