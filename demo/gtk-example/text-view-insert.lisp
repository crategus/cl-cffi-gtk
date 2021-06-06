;;;; Text View Insert (2021-6-4)

(in-package :gtk-example)

(defvar *text-for-example-text-view-insert*
"<html>
    <head><title>Title</title></head>
    <body>
    <h1>Heading</h1>
")

(defun get-this-tag (iter buffer)
  (let* ((start-tag (gtk-text-iter-copy iter))
         end-tag)
    (and (gtk-text-iter-find-char start-tag #'alpha-char-p)
         (setq end-tag (gtk-text-iter-copy start-tag))
         (gtk-text-iter-find-char end-tag
                                  (lambda (ch) (not (alphanumericp ch))))
         (gtk-text-buffer-get-text buffer start-tag end-tag nil))))

(defun closing-tag-p (iter)
  (let ((slash (gtk-text-iter-copy iter)))
    (gtk-text-iter-forward-char slash)
    (eql (gtk-text-iter-char slash) #\/)))

(defun example-text-view-insert ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :title "Example Text View Insert"
                                 :type :toplevel
                                 :default-width 350
                                 :default-height 200))
          (textview (make-instance 'gtk-text-view
                                   :top-margin 6
                                   :left-margin 6
                                   :right-margin 6))
          (button-make-item (make-instance 'gtk-button
                                           :label "Make List Item"
                                           :margin 3))
          (button-close-tag (make-instance 'gtk-button
                                           :label "Insert Close Tag"
                                           :margin 3))
          (hbox (make-instance 'gtk-box
                               :orientation :horizontal))
          (vbox (make-instance 'gtk-box
                                :orientation :vertical)))
    (g-signal-connect window "destroy"
                      (lambda (widget)
                        (declare (ignore widget))
                        (leave-gtk-main)))
    (g-signal-connect button-make-item "clicked"
        (lambda (widget)
          (declare (ignore widget))
          (let* ((buffer (gtk-text-view-buffer textview))
                 (cursor (gtk-text-buffer-mark buffer "insert"))
                 (bound (gtk-text-buffer-mark buffer "selection_bound")))
            (if (gtk-text-buffer-has-selection buffer)
                ;; Insert text before and after the selection
                (progn
                  ;; Insert start tag
                  (let ((iter (gtk-text-buffer-iter-at-mark buffer cursor)))
                    (gtk-text-buffer-insert buffer "<li>" :position iter))
                  ;; Insert end tag
                  (let ((mark (gtk-text-mark-new nil t))
                        (iter (gtk-text-buffer-iter-at-mark buffer bound)))
                    ;; Add the anonymous mark with right gravity
                    (gtk-text-buffer-add-mark buffer mark iter)
                    ;; Do the insertion at the position of the mark
                    (let ((end (gtk-text-buffer-iter-at-mark buffer mark)))
                      (gtk-text-buffer-insert buffer "</li>" :position end))
                    ;; Reselect the previous selection
                    (let ((iter1 (gtk-text-buffer-iter-at-mark buffer cursor))
                          (iter2 (gtk-text-buffer-iter-at-mark buffer mark)))
                      (gtk-text-buffer-select-range buffer iter1 iter2))
                    ;; Delete the mark
                    (gtk-text-buffer-delete-mark buffer mark)))
                ;; Insert at start and end of current line
                (let ((iter (gtk-text-buffer-iter-at-mark buffer cursor)))
                  ;; Move to the start of the line
                  (setf (gtk-text-iter-line-offset iter) 0)
                  (gtk-text-buffer-insert buffer "<li>" :position iter)
                  ;; Move to the end of the line
                  (gtk-text-iter-forward-to-line-end iter)
                  (gtk-text-buffer-insert buffer "</li>" :position iter)
                  ;; Place cursor and selection and the end of the line
                  (gtk-text-iter-forward-to-line-end iter)
                  (gtk-text-buffer-select-range buffer iter iter))))))
      (g-signal-connect button-close-tag "clicked"
          (lambda (widget)
            (declare (ignore widget))
            (let* ((buffer (gtk-text-view-buffer textview))
                   (cursor (gtk-text-buffer-mark buffer "insert"))
                   (iter (gtk-text-buffer-iter-at-mark buffer cursor)))
              (do ((stack '()))
                  ((not (gtk-text-iter-find-char iter
                                                (lambda (ch) (eq ch #\<))
                                                :direction :backward)))
                (let ((tag (get-this-tag iter buffer)))
                  (if (closing-tag-p iter)
                      (push tag stack)
                      (let ((tag-in-stack (pop stack)))
                        (when (not tag-in-stack)
                          (gtk-text-buffer-insert buffer
                                                  (format nil "</~a>" tag))
                          (return)))))))))
   (setf (gtk-text-buffer-text (gtk-text-view-buffer textview))
         *text-for-example-text-view-insert*)
   ;; Pack and show the widgets
   (gtk-box-pack-start vbox textview)
   (gtk-box-pack-start hbox button-make-item)
   (gtk-box-pack-start hbox button-close-tag)
   (gtk-box-pack-start vbox hbox :expand nil)
   (gtk-container-add window vbox)
   (gtk-widget-show-all window))))
