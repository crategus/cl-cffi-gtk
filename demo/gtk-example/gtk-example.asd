;;;; gtk-example.asd

(asdf:defsystem :gtk-example
  :author "Dieter Kaiser"
  :license "LLGPL"
  :serial t
  :depends-on (:cl-cffi-gtk :split-sequence)
  :components ((:file "gtk-example")
               (:file "application-simple")
               (:file "bloat-pad")
               (:file "box-packing")                   ; Packing Widgets
               (:file "box-simple")                    ; Packing Widgets
               (:file "button-box")
               (:file "clipboard")
               (:file "combo-box")
               (:file "combo-box-text")
               (:file "custom-window")
               (:file "dialog")
               (:file "drag-and-drop")
               (:file "drag-and-drop-simple")
               (:file "drawing-area")
               (:file "drawing-area-input")            ; Getting started
               (:file "event-handler")
               (:file "getting-started")               ; Getting started
               (:file "grab")
               (:file "grid-packing")                  ; Packing Widgets
               (:file "grid-simple")                   ; Packing Widgets
               (:file "grid-spacing")                  ; Packing Widgets
               (:file "hello-world")                   ; Getting started
               (:file "hello-world-upgraded")          ; Getting started
               (:file "hello-world-upgraded-2")        ; Getting started
               (:file "image-button-press")
               (:file "image-menu-item")
               (:file "info-bar")
               (:file "layout")
               (:file "level-bar")
;               (:file "list-box")
;               (:file "list-store")
               (:file "menu-builder")
               (:file "message-dialog-new")
               (:file "page-setup-dialog")
               (:file "pointer-device")
               (:file "progress-bar")
               (:file "print-dialog")
               (:file "print-operation")
               (:file "query-settings")
               (:file "radio-button")
               (:file "revealer")
               (:file "revealer-icon")
               (:file "scale-button")
               (:file "scrolled-window")
               (:file "show-about-dialog")
               (:file "tool-palette")
               (:file "widget-pointer")
               (:file "window-simple")                 ; Getting started
              ))

;;; 2021-5-14
