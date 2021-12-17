;;;; Example Expander - 2021-12-7
;;;;
;;;; GtkExpander allows to provide additional content that is initially hidden.
;;;; This is also known as "disclosure triangle". This example also shows how
;;;; to make the window resizable only if the expander is expanded.

(in-package :gtk-example)

(defun create-expander-dialog ()
  (let* ((dialog (make-instance 'gtk-message-dialog
                                :message-type :info
                                :buttons :close
                                :default-width 320
                                :text
                                "Example Expander in a Message Dialog"
                                :secondary-text
                                "See Details for more Information."))
        (area (gtk-message-dialog-message-area dialog))
        (box (gtk-widget-parent area))
        (expander (make-instance 'gtk-expander
                                 :label "Details"))
        (scrolled (make-instance 'gtk-scrolled-window
                                 :margin-top 12
                                 :min-content-height 200
                                 :shadow-type :in
                                 :hscroll-policy :never
                                 :vscroll-policy :automatic))
        (view (make-instance 'gtk-text-view
                             :editable nil
                             :margin 6
                             :wrap-mode :word))
        (buffer (gtk-text-view-buffer view)))
    (g-signal-connect expander "notify::expanded"
        (lambda (expander param)
          (declare (ignore param))
          (setf (gtk-window-resizable dialog)
                (gtk-expander-expanded expander))))
    (setf (gtk-box-child-expand (gtk-widget-parent box) box) t)
    (setf (gtk-box-child-fill (gtk-widget-parent box) box) t)
    (gtk-container-foreach area
        (lambda (widget)
          (gtk-container-child-set (gtk-widget-parent widget)
                                   widget
                                   "expand" nil
                                   "fill" nil)))
    (setf (gtk-text-buffer-text buffer) *some-text*)
    (gtk-container-add scrolled view)
    (gtk-container-add expander scrolled)
    (gtk-box-pack-end area expander :expand t :fill t)
    (gtk-widget-show-all expander)
    ;; Run the message dialog
    (gtk-dialog-run dialog)
    ;; Destroy the message dialog
    (gtk-widget-destroy dialog)))
