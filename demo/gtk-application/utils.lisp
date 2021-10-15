;;;; Utility functions - 2021-10-13

(in-package :gtk-application)

(defun sys-path (filename)
  (let ((system-path (asdf:system-source-directory :gtk-application)))
    (princ-to-string (merge-pathnames filename system-path))))

(defun clear-buffer (buffer)
  (multiple-value-bind (start end)
      (gtk-text-buffer-bounds buffer)
    (gtk-text-buffer-delete buffer start end)))

(defun load-file-into-buffer (buffer filename)
  (with-open-file (stream filename)
    (clear-buffer buffer)
    (do ((line (read-line stream nil)
               (read-line stream nil)))
        ((null line))
      (gtk-text-buffer-insert buffer line)
      (gtk-text-buffer-insert buffer (format nil "~%")))))
