(in-package :gtk-testsuite)

(def-suite gobject-utils :in gobject-suite)
(in-suite gobject-utils)

(test get-enum-items
  (is (subsetp '("GDK_DRAG_PROTO_NONE"
                 "GDK_DRAG_PROTO_MOTIF"
                 "GDK_DRAG_PROTO_XDND"
                 "GDK_DRAG_PROTO_ROOTWIN"
                 "GDK_DRAG_PROTO_WIN32_DROPFILES"
                 "GDK_DRAG_PROTO_OLE2"
                 "GDK_DRAG_PROTO_LOCAL")
               (mapcar #'gobject::enum-item-name
                       (gobject::get-enum-items "GdkDragProtocol"))
               :test #'string=))
  (is (subsetp '(0 1 2 3 4 5 6)
               (mapcar #'gobject::enum-item-value
                       (gobject::get-enum-items "GdkDragProtocol"))))
  (is (subsetp '("none" "motif" "xdnd" "rootwin" "win32-dropfiles" "ole2" "local")
               (mapcar #'gobject::enum-item-nick
                       (gobject::get-enum-items "GdkDragProtocol"))
               :test #'string=)))

(test get-g-enum-definition
  (is (equal '(DEFINE-G-ENUM "GdkDragProtocol" GDK-DRAG-PROTOCOL
                (:EXPORT T :TYPE-INITIALIZER "gdk_drag_protocol_get_type")
                (:NONE 0)
                (:MOTIF 1)
                (:XDND 2)
                (:ROOTWIN 3)
                (:WIN32-DROPFILES 4)
                (:OLE2 5)
                (:LOCAL 6)
                (:WAYLAND 7))
             (gobject::get-g-enum-definition "GdkDragProtocol"))))

(test get-flags-items
  (is (subsetp '("G_APPLICATION_FLAGS_NONE"
                 "G_APPLICATION_IS_SERVICE"
                 "G_APPLICATION_IS_LAUNCHER"
                 "G_APPLICATION_HANDLES_OPEN"
                 "G_APPLICATION_HANDLES_COMMAND_LINE"
                 "G_APPLICATION_SEND_ENVIRONMENT"
                 "G_APPLICATION_NON_UNIQUE")
               (mapcar #'gobject::flags-item-name
                       (gobject::get-flags-items "GApplicationFlags"))
               :test #'string=))
  (is (subsetp '(0 1 2 4 8 16 32)
               (mapcar #'gobject::flags-item-value
                       (gobject::get-flags-items "GApplicationFlags"))))
  (is (subsetp '("flags-none" "is-service" "is-launcher" "handles-open"
                 "handles-command-line" "send-environment" "non-unique")
               (mapcar #'gobject::flags-item-nick
                       (gobject::get-flags-items "GApplicationFlags"))
               :test #'string=)))

(test get-g-flags-definition
 (is (equal '(DEFINE-G-FLAGS "GApplicationFlags" G-APPLICATION-FLAGS
               (:EXPORT T)
               (:FLAGS-NONE 0)
               (:IS-SERVICE 1)
               (:IS-LAUNCHER 2)
               (:HANDLES-OPEN 4)
               (:HANDLES-COMMAND-LINE 8)
               (:SEND-ENVIRONMENT 16)
               (:NON-UNIQUE 32)
               (:CAN-OVERRIDE-APP-ID 64))
            (gobject::get-g-flags-definition "GApplicationFlags"))))


