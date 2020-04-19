(in-package :gtk-example)

(defun create-level-bar (orientation)
  (let* ((level-bar (make-instance 'gtk-level-bar
                                   :orientation orientation)))
    ;; This changes the value of the default low offset
    (gtk-level-bar-add-offset-value level-bar "low" 0.10d0)

    ;; This adds a new offset to the bar. The application will
    ;; be able to change its color by using the following selector,
    ;; either by adding it to its CSS file or using the functions
    ;; gtk-css-provider-load-from-data and gtk-style-context-add-provider
    ;;
    ;; .level-bar.fill-block.level-my-offset {
    ;;   background-color: green;
    ;;   border-style: solid;
    ;;   border-color: black;
    ;;   border-style: 1px;
    ;; }
    (gtk-level-bar-add-offset-value level-bar "my-offset" 0.60d0)

    ;; Return the new level bar
    level-bar))

(defun example-level-bar ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :type :toplevel
                                 :title "Example Level bar"
                                 :border-width 12
                                 :default-width 420))
          (level-bar (create-level-bar :horizontal))
          (box (make-instance 'gtk-box :orientation :vertical :spacing 12)))
      ;; Signal handler for the window to handle the signal "destroy".
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))
      (setf (gtk-level-bar-value level-bar) 0.50)
      ;; Add the info bar to the grid and the grid to the window
      (gtk-container-add box level-bar)
      (gtk-container-add box (make-instance 'gtk-label :label "Level bar"))
      (gtk-container-add window box)
      ;; Show the window.
      (gtk-widget-show-all window))))

