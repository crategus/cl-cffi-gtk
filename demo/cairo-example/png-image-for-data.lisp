;;;; A simple console application that will create a PNG image for data.

(in-package :cairo-example)

(defun example-png-image-for-data ()
  (let* ((height 300)
         (width 500)
         (stride (cairo-format-stride-for-width :argb32 width))
         (data (g-malloc (* height stride)))
         ;; Create an image surface for data and a Cairo context.
         (surface (cairo-image-surface-create-for-data data
                                                       :argb32
                                                       width height stride))
         (cr (cairo-create surface)))
    ;; Clear surface
    (cairo-set-source-rgb cr 1.0 1.0 1.0)
    (cairo-paint cr)
    ;; Draw in black ink.
    (cairo-set-source-rgba cr 0.0 0.0 0.0 1.0)
    ;; Choose a font type and set its size.
    (cairo-select-font-face cr "Sans")
    (cairo-set-font-size cr 20.0)
    ;; Move to a position within the image and draw the text.
    (cairo-move-to cr 10.0 50.0)
    (cairo-show-text cr "Cario drawing to an image surface for data.")
    ;; Create and save the PNG image.
    (cairo-surface-write-to-png surface "image.png")
    ;; Clean the resources.
    (cairo-destroy cr)
    (cairo-surface-destroy surface)
    (g-free data)))

;;; 2021-1-25
