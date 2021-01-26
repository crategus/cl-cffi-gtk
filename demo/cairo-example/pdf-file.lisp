;;;; Use the Cairo library to create a simple PDF file.

(in-package :cairo-example)

(defun example-pdf-file ()
  (let* (;; Create a PDF surface and a Cairo context.
         (surface (cairo-pdf-surface-create "pdffile.pdf" 504.0 648.0))
         (cr (cairo-create surface)))
    ;; Draw in blue ink.
    (cairo-set-source-rgba cr 0 0 1.0 1.0)
    ;; Choose a font type and set its size.
    (cairo-select-font-face cr "Sans")
    (cairo-set-font-size cr 20.0)
    ;; Move to a position within the image and draw the text.
    (cairo-move-to cr 20.0 70.0)
    (cairo-show-text cr "Cario drawing to a PDF surface.")
    ;; Finish rendering of the PDF file.
    (cairo-show-page cr)
    ;; Clean the resources.
    (cairo-surface-destroy surface)
    (cairo-destroy cr)))

;;; 2021-1-25
