(def-suite gobject-utils :in gobject-suite)
(in-suite gobject-utils)

;; Load Gdk to have examples for flags and enum definitions.
(asdf:load-system :cl-cffi-gtk-gdk)

(test get-enum-items
  (is (equal '("GDK_DRAG_PROTO_NONE" "GDK_DRAG_PROTO_MOTIF"
               "GDK_DRAG_PROTO_XDND" "GDK_DRAG_PROTO_ROOTWIN"
               "GDK_DRAG_PROTO_WIN32_DROPFILES" "GDK_DRAG_PROTO_OLE2"
               "GDK_DRAG_PROTO_LOCAL" "GDK_DRAG_PROTO_WAYLAND")
             (mapcar #'enum-item-name
                     (get-enum-items "GdkDragProtocol"))))
  (is (equal '(0 1 2 3 4 5 6 7)
             (mapcar #'enum-item-value
                     (get-enum-items "GdkDragProtocol"))))
  (is (equal '("none" "motif" "xdnd" "rootwin" "win32-dropfiles" "ole2" "local"
               "wayland")
             (mapcar #'enum-item-nick
                     (get-enum-items "GdkDragProtocol")))))

(test get-g-enum-definition
  (is (equal '(DEFINE-G-ENUM "GdkDragProtocol"
                             GDK-DRAG-PROTOCOL
                             (:EXPORT T
                              :TYPE-INITIALIZER "gdk_drag_protocol_get_type")
                             (:NONE 0)
                             (:MOTIF 1)
                             (:XDND 2)
                             (:ROOTWIN 3)
                             (:WIN32-DROPFILES 4)
                             (:OLE2 5)
                             (:LOCAL 6)
                             (:WAYLAND 7))
             (get-g-enum-definition "GdkDragProtocol"))))

(test get-flags-items
  (is (equal '("G_APPLICATION_FLAGS_NONE" "G_APPLICATION_IS_SERVICE"
               "G_APPLICATION_IS_LAUNCHER" "G_APPLICATION_HANDLES_OPEN"
               "G_APPLICATION_HANDLES_COMMAND_LINE"
               "G_APPLICATION_SEND_ENVIRONMENT" "G_APPLICATION_NON_UNIQUE"
               "G_APPLICATION_CAN_OVERRIDE_APP_ID"
               "G_APPLICATION_ALLOW_REPLACEMENT" "G_APPLICATION_REPLACE")
             (mapcar #'flags-item-name
                     (get-flags-items "GApplicationFlags"))))
  (is (equal '(0 1 2 4 8 16 32 64 128 256)
             (mapcar #'flags-item-value
                     (get-flags-items "GApplicationFlags"))))
  (is (equal '("flags-none" "is-service" "is-launcher" "handles-open"
               "handles-command-line" "send-environment" "non-unique"
               "can-override-app-id" "allow-replacement" "replace")
             (mapcar #'flags-item-nick
                     (get-flags-items "GApplicationFlags")))))

(test get-g-flags-definition
 (is (equal '(DEFINE-G-FLAGS "GApplicationFlags"
                              G-APPLICATION-FLAGS
                             (:EXPORT T)
                             (:FLAGS-NONE 0)
                             (:IS-SERVICE 1)
                             (:IS-LAUNCHER 2)
                             (:HANDLES-OPEN 4)
                             (:HANDLES-COMMAND-LINE 8)
                             (:SEND-ENVIRONMENT 16)
                             (:NON-UNIQUE 32)
                             (:CAN-OVERRIDE-APP-ID 64)
                             (:ALLOW-REPLACEMENT 128)
                             (:REPLACE 256))
            (get-g-flags-definition "GApplicationFlags"))))

;;; 2020-11-10
