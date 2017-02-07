(in-package #:gtk-demo)

(defclass custom-window (gtk-window)
  ()
  (:metaclass gobject-class)
  (:g-type-name . "CustomWindow"))

(defun gtk-window-set-focus-impl (window focus)
  (format T "Hello, from set-focus, ~A ~A.~%" window focus)
  (let ((super (foreign-slot-value (g-type-class-peek (g-type-from-name "GtkWindow")) '(:struct gtk-window-class) 'gtk::set-focus)))
    (foreign-funcall-pointer super NIL (g-object gtk-window) window (g-object gtk-widget) focus)))

(gobject::glib-defcallback c-set-focus :void ((window (g-object custom-window)) (focus (g-object gtk-widget)))
  (restart-case (gtk-window-set-focus-impl window focus)
    (return-from-method-implementation ()
      :report "Return from method implementation")))

(defun custom-window-class-init (foreign-class)
  (format T "custom-window-class-init ~A~%" foreign-class)
  (setf (foreign-slot-value foreign-class '(:struct gtk-window-class) 'gtk::set-focus)
        (callback c-set-focus)))

(register-object-type-implementation
 "CustomWindow" custom-window "GtkWindow" NIL NIL 'custom-window-class-init)

(cffi:load-foreign-library (asdf:system-relative-pathname :cl-cffi-gtk-demo-gtk "subclass.so"))

(define-g-object-class "GtkAnotherCustomWindow" gtk-another-custom-window
  (:superclass custom-window
   :export NIL
   :type-initializer "gtk_another_custom_window_get_type")
  ())

(defun custom-window-demo ()
  (within-main-loop
    (let ((custom (make-instance 'gtk-another-custom-window))
          (button (make-instance 'gtk-button :label "Button")))
      (gtk-container-add custom button)
      (gtk-widget-show-all custom)
      (gtk-window-set-focus custom button)
      (g-signal-connect custom "destroy" (lambda (widget)
                                           (declare (ignore widget))
                                           (leave-gtk-main))))))
