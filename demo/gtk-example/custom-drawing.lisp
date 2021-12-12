;;;; Example Custom Drawing - 2021-11-30
;;;;
;;;; Many applications cannot use GTK widgets, for a variety of reasons, but
;;;; still want their user interface to appear integrated with the rest of the
;;;; desktop, and follow GTK themes. This demo shows how to use GtkStyleContext
;;;; and the gtk-render APIs to achieve this.
;;;;
;;;; Note that this is a simple, non-interactive example.

(in-package :gtk-example)

(defun append-element (path selector)
  (let* ((next (position-if (lambda (x) (member x '(#\# #\. #\:))) selector))
         (name (subseq selector 0 next)))
    (cond ((upper-case-p (elt name 0))
           (let ((gtype (g-type-from-name name)))
             (gtk-widget-path-append-type path gtype)))
          (t
           (gtk-widget-path-append-type path +g-type-none+)
           (setf (gtk-widget-path-iter-object-name path -1) name)))
    (do* ((mark (if next (elt selector next)) (if next (elt selector next)))
          (selector (if next (subseq selector (1+ next)))
                    (if next (subseq selector (1+ next))))
          (next (position-if (lambda (x) (member x '(#\# #\. #\:))) selector)
                (position-if (lambda (x) (member x '(#\# #\. #\:))) selector)))
         ((not selector))
      (setf name (subseq selector 0 next))
      (cond ((eq mark #\.)
             (gtk-widget-path-iter-add-class path -1 name))
            ((eq mark #\#)
             (setf (gtk-widget-path-iter-name path -1) name))
            ((eq mark #\:)
             (let ((flags (cdr (assoc name
                                      '(("active" . :active)
                                        ("hover" . :prelight)
                                        ("selected" . :selected)
                                        ("disabled" . :insensitive)
                                        ("indeterminate" . :inconsistent)
                                        ("focus" . :focused)
                                        ("backdrop" . :backdrop)
                                        ("dir(ltr)" . :dir-ltr)
                                        ("dir(rtl)" . :dir-rtl)
                                        ("link" . :link)
                                        ("visited" . :visited)
                                        ("checked" . :checked)
                                        ("drop(active)" . :drop-active))
                                       :test #'string=))))
             (setf (gtk-widget-path-iter-state path -1)
                   (union (list flags)
                          (gtk-widget-path-iter-state path -1)))))))
    path))

(defun get-style-context (parent selector)
  (let ((context (gtk-style-context-new))
        (path (if parent
                  (gtk-widget-path-copy (gtk-style-context-path parent))
                  (gtk-widget-path-new))))
    (setf path (append-element path selector))
    (setf (gtk-style-context-path context) path)
    (setf (gtk-style-context-parent context) parent)
    ;; We have to explicitly set the state again here for it to take effect
    (setf (gtk-style-context-state context)
          (gtk-widget-path-iter-state path -1))
    context))

(defun query-size (context width height)
  (let* ((state (gtk-style-context-state context))
         (min-width (gtk-style-context-property context "min-width" state))
         (min-height (gtk-style-context-property context "min-height" state))
         (margin (gtk-style-context-margin context state))
         (border (gtk-style-context-border context state))
         (padding (gtk-style-context-padding context state)))
    (setf min-width
          (+ min-width
             (gtk-border-left margin)
             (gtk-border-right margin)
             (gtk-border-left border)
             (gtk-border-right border)
             (gtk-border-left padding)
             (gtk-border-right padding)))
    (setf min-height
          (+ min-height
             (gtk-border-top margin)
             (gtk-border-bottom margin)
             (gtk-border-top border)
             (gtk-border-bottom border)
             (gtk-border-top padding)
             (gtk-border-bottom padding)))
    (values (max width min-width)
            (max height min-height))))

(defun draw-common (context cr x y width height)
  (let* ((state (gtk-style-context-state context))
         (margin (gtk-style-context-margin context state))
         (border (gtk-style-context-border context state))
         (padding (gtk-style-context-padding context state))
         (min-width (gtk-style-context-property context "min-width" state))
         (min-height (gtk-style-context-property context "min-height" state)))
    ;; Recalculate to include a margin
    (setf x (+ x (gtk-border-left margin)))
    (setf y (+ y (gtk-border-top margin)))
    (setf width (- width (gtk-border-left margin) (gtk-border-right margin)))
    (setf height (- height (gtk-border-top margin) (gtk-border-bottom margin)))
    ;; Check the width and height
    (setf width (max width min-width))
    (setf height (max height min-height))
    ;; Render background and frame
    (gtk-render-background context cr x y width height)
    (gtk-render-frame context cr x y width height)
    ;; Return new values for x, y, width, and height
    (values (+ x (gtk-border-left border)
                 (gtk-border-left padding))
            (+ y (gtk-border-top border)
                 (gtk-border-top padding))
            (- width (gtk-border-left border)
                     (gtk-border-right border)
                     (gtk-border-left padding)
                     (gtk-border-right padding))
            (- height (gtk-border-top border)
                      (gtk-border-bottom border)
                      (gtk-border-top padding)
                      (gtk-border-bottom padding)))))

(defun draw-text (widget cr x y text)
  (let ((width (gtk-widget-allocated-width widget))
        (height (gtk-widget-allocated-height widget)))
    (declare (ignorable width height))
    ;; Set the color.
    (cairo-set-source-rgb cr 0.1 0.1 0.1)
    ;; Select the font face
    (cairo-select-font-face cr "Monospace" :weight :bold)
    ;; Specify the font size
    (cairo-set-font-size cr 14)
    (let* ((extents (cairo-text-extents cr text))
           (text-height (cairo-text-extents-height extents)))
      ;; Display text on the drawing area
      (cairo-move-to cr x (+ y text-height))
      (cairo-show-text cr text)
      text-height)))

(defun draw-label (widget cr x y width height text state)
  (let* ((cxtlabel (get-style-context nil "label.view"))
         (cxtselection (get-style-context cxtlabel "selection"))
         (layout (gtk-widget-create-pango-layout widget text)))
    (setf (gtk-style-context-state cxtlabel) state)
    (let ((context (if (eq :selected state) cxtselection cxtlabel)))
      (gtk-render-background context cr x y width height)
      (gtk-render-frame context cr x y width height)
      (gtk-render-layout context cr x y layout))))

(defun draw-scrollbar (widget cr x y width position state)
  (declare (ignore widget))
  (let* ((dummy 0)
         (height 0)
         (cxtscrollbar (get-style-context nil "scrollbar.horizontal.bottom"))
         (cxtcontents (get-style-context cxtscrollbar "contents"))
         (cxttrough (get-style-context cxtcontents "trough"))
         (cxtslider (get-style-context cxttrough "slider"))
         (sliderwidth (gtk-style-context-property cxtslider "min-width" state)))
    (setf (gtk-style-context-state cxtscrollbar) state)
    (setf (gtk-style-context-state cxtcontents) state)
    (setf (gtk-style-context-state cxttrough) state)
    (setf (gtk-style-context-state cxtslider) state)
    (setf (values dummy height)
          (query-size cxtscrollbar dummy height))
    (setf (values dummy height)
          (query-size cxtcontents dummy height))
    (setf (values dummy height)
          (query-size cxttrough dummy height))
    (setf (values dummy height)
          (query-size cxtslider dummy height))
    (draw-common cxtscrollbar cr x y width height)
    (draw-common cxtcontents cr x y width height)
    (draw-common cxttrough cr x y width height)
    (draw-common cxtslider cr (+ x position) y sliderwidth height)
    height))

(defun draw-check (widget cr x y state)
  (declare (ignore widget))
  (let* ((width 0)
         (height 0)
         (cxtbutton (get-style-context nil "checkbutton"))
         (cxtcheck (get-style-context cxtbutton "check")))
    (setf (gtk-style-context-state cxtcheck) state)
    (setf (values width height) (query-size cxtbutton width height))
    (setf (values width height) (query-size cxtcheck width height))
    (draw-common cxtbutton cr x y width height)
    (multiple-value-bind (contents-x contents-y contents-width contents-height)
        (draw-common cxtcheck cr x y width height)
      (gtk-render-check cxtcheck cr contents-x
                                    contents-y
                                    contents-width
                                    contents-height))
    (values width height)))

(defun draw-radio (widget cr x y state)
  (declare (ignore widget))
  (let* ((width 0)
         (height 0)
         (cxtbutton (get-style-context nil "radiobutton"))
         (cxtcheck (get-style-context cxtbutton "radio")))
    (setf (gtk-style-context-state cxtcheck) state)
    (setf (values width height) (query-size cxtbutton width height))
    (setf (values width height) (query-size cxtcheck width height))
    (draw-common cxtbutton cr x y width height)
    (multiple-value-bind (contents-x contents-y contents-width contents-height)
        (draw-common cxtcheck cr x y width height)
      (gtk-render-check cxtcheck cr contents-x
                                    contents-y
                                    contents-width
                                    contents-height))
    (values width height)))

(defun draw-progress (widget cr x y width position)
  (declare (ignorable widget))
  (let* ((dummy 0)
         (height 0)
         (cxtbar (get-style-context nil "progressbar.horizontal"))
         (cxttrough (get-style-context cxtbar "trough"))
         (cxtprogress (get-style-context cxttrough "progress.left")))
    (setf (values dummy height) (query-size cxtbar dummy height))
    (setf (values dummy height) (query-size cxttrough dummy height))
    (setf (values dummy height) (query-size cxtprogress dummy height))
    (draw-common cxtbar cr x y width height)
    (draw-common cxttrough cr x y width height)
    (draw-common cxtprogress cr x y position height)
    height))

(defun draw-scale (widget cr x y width position)
  (declare (ignore widget))
  (let* ((height 0) (dummy 0)
         (contents-x 0) (contents-y 0)
         (contents-width 0) (contents-height 0)
         (trough-height 0)
         (slider-height 0)
         (cxtscale (get-style-context nil "scale.horizontal"))
         (cxtcontents (get-style-context cxtscale "contents"))
         (cxttrough (get-style-context cxtcontents "trough"))
         (cxtslider (get-style-context cxttrough "slider"))
         (cxthighlight (get-style-context cxttrough "higlight.top")))
    (setf (values dummy height) (query-size cxtscale dummy height))
    (setf (values dummy height) (query-size cxtcontents dummy height))
    (setf (values dummy height) (query-size cxttrough dummy height))
    (setf (values dummy height) (query-size cxtslider dummy height))
    (setf (values dummy height) (query-size cxthighlight dummy height))
    (setf (values contents-x contents-y contents-width contents-height)
          (draw-common cxtscale cr x y width height))
    (setf (values contents-x contents-y contents-width contents-height)
          (draw-common cxtcontents cr contents-x
                                      contents-y
                                      contents-width
                                      contents-height))
    ;; Scale trough defines its size querying slider and highlight
    (setf (values dummy trough-height)
          (query-size cxttrough dummy trough-height))
    (setf (values dummy slider-height)
          (query-size cxtslider dummy slider-height))
    (setf (values dummy slider-height)
          (query-size cxthighlight dummy slider-height))
    (setf trough-height (+ trough-height slider-height))
    (setf (values contents-x contents-y contents-width contents-height)
          (draw-common cxttrough cr contents-x
                                    contents-y
                                    contents-width
                                    trough-height))
    (draw-common cxthighlight cr contents-x
                                 contents-y
                                 (/ contents-width 2)
                                 contents-height)
    (draw-common cxtslider cr (+ contents-x  position)
                              contents-y
                              contents-height
                              contents-height)
    (max height trough-height slider-height)))

(defun draw-notebook (widget cr x y width height)
  (declare (ignorable widget))
  (let* ((dummy 0)
         (header-height 0)
         (contents-x 0) (contents-y 0) (contents-width 0) (contents-height 0)
         (cxtnotebook (get-style-context nil "notebook.frame"))
         (cxtheader (get-style-context cxtnotebook "header.top"))
         (cxttabs (get-style-context cxtheader "tabs"))
         (cxttab1 (get-style-context cxttabs "tab:checked"))
         (cxttab2 (get-style-context cxttabs "tab:hover"))
         (cxtstack (get-style-context cxtnotebook "stack")))
    (setf (values dummy header-height)
          (query-size cxtnotebook dummy header-height))
    (setf (values dummy header-height)
          (query-size cxtheader dummy header-height))
    (setf (values dummy header-height)
          (query-size cxttabs dummy header-height))
    (setf (values dummy header-height)
          (query-size cxttab1 dummy header-height))
    (setf (values dummy header-height)
          (query-size cxttab2 dummy header-height))
    (draw-common cxtnotebook cr x y width height)
    (draw-common cxtheader cr x y width height)
    (draw-common cxttabs cr x y width height)
    (setf (values contents-x contents-y contents-width contents-height)
          (draw-common cxttab1 cr x y (/ width 2) header-height))
    (draw-common cxttab2 cr (+ x (/ width 2)) y (/ width 2) header-height)
    (draw-common cxtstack cr x
                             (+ y header-height)
                             width
                             (- height header-height))))

(defun draw-callback (widget cr)
  (let* ((cr (pointer cr))
         (x 0) (y 0)
         (width (gtk-widget-allocated-width widget))
         (height (gtk-widget-allocated-height widget))
         (panewidth (/ width 2)))
    ;; Clear the drawing area
    (cairo-rectangle cr 0 0 width height)
    (cairo-set-source-rgb cr 0.9 0.9 0.9)
    (cairo-fill cr)
    ;; Draw label
    (setf x 12)
    (setf y 12)
    (setf height (draw-text widget cr x y "Draw Label"))
    (setf x 18)
    (setf y (+ y height 12))
    (draw-label widget cr x y (- panewidth 20) 20 "Not selected" :normal)
    (setf y (+ y 20 12))
    (draw-label widget cr x y (- panewidth 20) 20 "Selected" :selected)
    ;; Draw horizontal scrollbar
    (setf x 12)
    (setf y (+ y 20 18))
    (setf height (draw-text widget cr x y "Draw Scrollbar"))
    (setf x 18)
    (setf y (+ y height 12))
    (setf height
          (draw-scrollbar widget cr x y (- panewidth 20) 30 :normal))
    (setf y (+ y height 12))
    (setf height
          (draw-scrollbar widget cr x y (- panewidth 20) 40 :prelight))
    (setf y (+ y height 12))
    (setf height
          (draw-scrollbar widget cr x y (- panewidth 20) 50 :active))
    ;; Draw check button
    (setf x 12)
    (setf y (+ y height 18))
    (setf height (draw-text widget cr x y "Draw Check/Radio Button"))
    (setf x 18)
    (setf y (+ y height 12))
    (setf (values width height) (draw-check widget cr x y :normal))
    (setf x (+ x width 12))
    (setf (values width height) (draw-check widget cr x y :checked))
    (setf x (+ x width 12))
    ;; Draw radio button
    (setf x (+ x 12))
    (setf (values width height) (draw-radio widget cr x y :normal))
    (setf x (+ x width 12))
    (setf (values width height) (draw-radio widget cr x y :checked))
    ;; Draw progress bar
    (setf x 12)
    (setf y (+ y height 18))
    (setf height (draw-text widget cr x y "Draw Progressbar"))
    (setf x 18)
    (setf y (+ y height 12))
    (setf height
         (draw-progress widget cr x y (- panewidth 20) 50))
    ;; Draw scale
    (setf x (+ panewidth 12))
    (setf y 12)
    (setf height (draw-text widget cr x y "Draw Scale"))
    (setf x (+ panewidth 18))
    (setf y (+ y height 12))
    (setf height
          (draw-scale widget cr x y (- panewidth 20) 75))
    ;; Draw notebook
    (setf x (+ panewidth 12))
    (setf y (+ y height 18))
    (setf height (draw-text widget cr x y "Draw Notebook"))
    (setf x (+ panewidth 18))
    (setf y (+ y height 12))
    (setf height
          (draw-notebook widget cr x y (- panewidth 32) 192))
    (cairo-destroy cr)))

(defun example-custom-drawing (&optional application)
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :type :toplevel
                                 :application application
                                 :title "Example Custom Drawing"
                                 :width-request 420
                                 :height-request 408))
          (area (make-instance 'gtk-drawing-area
                               :app-paintable t
                               :hexpand t
                               :vexpand t)))
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))
      (g-signal-connect area "draw" #'draw-callback)
      (gtk-container-add window area)
      (gtk-widget-show-all window))))
