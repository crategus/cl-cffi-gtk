(defsystem :cl-gtk-gdk
  :name :cl-gtk-gdk
  :version "0.0.0"
  :author "Dr. Dieter Kaiser"
  :license "LLGPL"
  :serial t
  :components ((:file "gdk.package")
               (:file "gdk.init")
               
               (:file "gdk.region")           ; Points, Rectangles and Regions
               (:file "gdk.threads")          ; Using GDK with threads
               (:file "gdk.properties")       ; Manipulate properties on windows
               (:file "gdk.event-structures") ; Data structures for events
               
               (:file "gdk.device")           ; Handling extended input devices
               (:file "gdk.screen")           ; Representing a physical screen
               (:file "gdk.visual")           ; Low-level display information
               (:file "gdk.color")            ; Colormaps and Colors
               (:file "gdk.cursor")           ; Standard and pixmap cursors
               
               (:file "gdk.font")             ; Loading and manipulating fonts
               (:file "gdk.gc")               ; Encapsulate drawing properties
               (:file "gdk.rgb")              ; Renders RGBto a GdkDrawable
               (:file "gdk.drawable")         ; Drawing Primitives
               (:file "gdk.pixmap")           ; Offscreen drawables
               (:file "gdk.display")          ; Controls the keyboard/mouse
               (:file "gdk.manager")          ; Maintains a list GdkDisplays
               (:file "gdk.pixbuf-structure") ; Implementation of PixBuf
               (:file "gdk.pixbuf-file")      ; Loading and saving PixBuf
               (:file "gdk.pixbuf")           ; Gdk functions for PixBuf
               
               (:file "gdk.window")           ; Onscreen display areas
               (:file "gdk.events")           ; Functions for handling events
               (:file "gdk.general")          ; Library initialization
               
               (:file "gdk.key-values")       ; Manipulating keyboard codes
               (:file "gdk.selections")       ; Transfering data
               (:file "gdk.drag-and-drop")    ; Drag and drop handling
               (:file "gdk.app")              ; Notification for applications
               (:file "gdk.pango")            ; Using Pango in GDK
               )
  :depends-on (:cl-gtk-gobject
               :cl-gtk-glib
               :cffi
               :cl-gtk-pango))
