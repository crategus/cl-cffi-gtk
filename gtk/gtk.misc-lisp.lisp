(in-package :gtk)

(defcallback stable-pointer-free-destroy-notify-callback :void ((data :pointer))
  (free-stable-pointer data))

(defcfun (get-clipboard "gtk_clipboard_get") g-object
  (selection gdk-atom-as-string))

(export 'get-clipboard)

(defcallback call-from-main-loop-callback :boolean
    ((data :pointer))
  (restart-case
      (progn (funcall (get-stable-pointer-value data))
             nil)
    (return-from-callback () nil)))

(defun call-from-gtk-main-loop (function &key (priority +g-priority-default-idle+))
  (g-idle-add-full priority
                   (callback call-from-main-loop-callback)
                   (allocate-stable-pointer function)
                   (callback stable-pointer-free-destroy-notify-callback))
  (ensure-gtk-main))

(export 'call-from-gtk-main-loop)

(defcallback call-timeout-from-main-loop-callback :boolean
    ((data :pointer))
  (restart-case
      (progn (funcall (get-stable-pointer-value data)))
    (return-from-callback () nil)))

(defun gtk-main-add-timeout (milliseconds function &key (priority +g-priority-default+))
  (g-timeout-add-full priority milliseconds
                      (callback call-timeout-from-main-loop-callback)
                      (allocate-stable-pointer function)
                      (callback stable-pointer-free-destroy-notify-callback)))

(export 'gtk-main-add-timeout)

(defmacro within-main-loop (&body body)
  `(call-from-gtk-main-loop (lambda () ,@body)))

(export 'within-main-loop)

#+thread-support
(defmacro with-main-loop (&body body)
  `(progn
     (ensure-gtk-main)
     (within-main-loop ,@body)))

#-thread-support
(defmacro with-main-loop (&body body)
  `(progn
     ,@body
     (ensure-gtk-main)))

(export 'with-main-loop)
