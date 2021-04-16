;;; Example Application launch for URI

(in-package :gdk-example)

(defun example-app-launch ()
  (let* ((display (gdk-display-default))
         (screen (gdk-display-default-screen display))
         (context (gdk-display-app-launch-context display)))

    (gdk-app-launch-context-set-screen context screen)
    (gdk-app-launch-context-set-timestamp context +gdk-current-time+)

    (g-app-info-launch-default-for-uri "http://www.gtk.org" context)))

;;; 2021-4-14
