;;;; A simple console application that will create a PNG image for data.

(in-package :cairo-example)

(defun example-png-image-from-png ()
  (let* (;; Create an image surface from PNG file and a Cairo context.
         (filename (rel-path "ducky.png"))
         (surface (cairo-image-surface-create-from-png filename))
         (cr (cairo-create surface)))
    (when (not (eq :success (cairo-surface-status surface)))
      (error "Error loading the file ~a.~%" filename))
    ;; Draw in black ink.
    (cairo-set-source-rgba cr 0.0 0.0 0.0 1.0)
    ;; Choose a font type and set its size.
    (cairo-select-font-face cr "Sans" :normal :normal)
    (cairo-set-font-size cr 18.0)
    ;; Move to a position within the image and draw the text.
    (cairo-move-to cr 10.0 50.0)
    (cairo-show-text cr "Cario drawing to an image surface from PNG file.")
    ;; Create and save the PNG image.
    (cairo-surface-write-to-png surface "image.png")
    ;; Clean the resources.
    (cairo-destroy cr)
    (cairo-surface-destroy surface)))

;;; 2020-12-21
