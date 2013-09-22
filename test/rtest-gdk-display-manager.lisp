(def-suite gdk-display-manager :in gdk-suite)
(in-suite gdk-display-manager)

;;;   GdkDisplayManager

(test gdk-display-manager-class
  ;; Type checks
  (is-true  (g-type-is-object "GdkDisplayManager"))
  (is-false (g-type-is-abstract "GdkDisplayManager"))
  (is-true  (g-type-is-derived "GdkDisplayManager"))
  (is-false (g-type-is-fundamental "GdkDisplayManager"))
  (is-true  (g-type-is-value-type "GdkDisplayManager"))
  (is-true  (g-type-has-value-table "GdkDisplayManager"))
  (is-true  (g-type-is-classed "GdkDisplayManager"))
  (is-true  (g-type-is-instantiatable "GdkDisplayManager"))
  (is-true  (g-type-is-derivable "GdkDisplayManager"))
  (is-true  (g-type-is-deep-derivable "GdkDisplayManager"))
  (is-false (g-type-is-interface "GdkDisplayManager"))

  ;; Check the registered name
  (is (eq 'gdk-display-manager
          (registered-object-type-by-name "GdkDisplayManager")))

  ;; Check infos about the C class implementation
  (let ((class (g-type-class-ref (gtype "GdkDisplayManager"))))
    (is (equal (gtype "GdkDisplayManager") (g-type-from-class class)))
    (is (equal (gtype "GdkDisplayManager") (g-object-class-type class)))
    (is (equal "GdkDisplayManager" (g-object-class-name class)))
    (is (equal (gtype "GdkDisplayManager")
               (g-type-from-class  (g-type-class-peek "GdkDisplayManager"))))
    (is (equal (gtype "GdkDisplayManager")
               (g-type-from-class (g-type-class-peek-static "GdkDisplayManager"))))
    (g-type-class-unref class))

  ;; Check infos about the Lisp class implementation
  (let ((class (find-class 'gdk-display-manager)))
    ;; Check the class name and type of the class
    (is (eq 'gdk-display-manager (class-name class)))
    (is (eq 'gobject-class (type-of class)))
    (is (eq (find-class 'gobject-class) (class-of class)))
    ;; Properties of the metaclass gobject-class
    (is (equal "GdkDisplayManager" (gobject-class-g-type-name class)))
    (is (equal "GdkDisplayManager" (gobject-class-direct-g-type-name class)))
    (is (equal "gdk_display_manager_get_type"
               (gobject-class-g-type-initializer class)))
    (is-false (gobject-class-interface-p class)))

  ;; Check some more GType information
  (is (equal (gtype "GObject") (g-type-parent "GdkDisplayManager")))
  (is (= 2 (g-type-depth "GdkDisplayManager")))
  (is (equal (gtype "GdkDisplayManager")
             (g-type-next-base "GdkDisplayManager" "GObject")))
  (is-true  (g-type-is-a "GdkDisplayManager" "GObject"))
  (is-false (g-type-is-a "GdkDisplayManager" "GtkWidget"))
  (is-false (g-type-is-a "GdkDisplayManager" "gboolean"))

  ;; Check the children
  #-windows
  (is (equal '("GdkX11DisplayManager")
             (mapcar #'gtype-name (g-type-children "GdkDisplayManager"))))
  #+windows
  (is (equal '("GdkWin32DisplayManager")
             (mapcar #'gtype-name (g-type-children "GdkDisplayManager"))))

  ;; Check the interfaces
  (is (equal '()
             (mapcar #'gtype-name (g-type-interfaces "GdkDisplayManager"))))

  ;; Query infos about the class
  (with-foreign-object (query '(:struct g-type-query))
    (g-type-query "GdkDisplayManager" query)
    (is (equal (gtype "GdkDisplayManager")
               (foreign-slot-value query '(:struct g-type-query) :type)))
    (is (equal "GdkDisplayManager"
               (foreign-slot-value query '(:struct g-type-query) :type-name)))
    (is (= 108 (foreign-slot-value query '(:struct g-type-query) :class-size)))
    (is (=  12 (foreign-slot-value query '(:struct g-type-query) :instance-size))))

  ;; Get the names of the class properties.
  (is (equal '("name" "parameter-type" "enabled" "state-type" "state")
             (mapcar #'param-spec-name
                     (g-object-class-list-properties "GSimpleAction"))))

  ;; No style properties
  ;; No child properties

  ;; Get the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GdkDisplayManager" GDK-DISPLAY-MANAGER
                       (:SUPERCLASS G-OBJECT :EXPORT T :INTERFACES NIL
                        :TYPE-INITIALIZER "gdk_display_manager_get_type")
                       ((DEFAULT-DISPLAY GDK-DISPLAY-MANAGER-DEFAULT-DISPLAY
                         "default-display" "GdkDisplay" T T)))
             (get-g-type-definition "GdkDisplayManager"))))

;;;   gdk_display_manager_get

(test gdk-display-manager-get
  (is (eq 'gdk-display-manager (type-of (gdk-display-manager-get)))))

;;;   gdk_display_manager_get_default_display

(test gdk-display-manager-get-default-display
  (let ((display-manager (gdk-display-manager-get)))
    (is (eq 'gdk-display
            (type-of (gdk-display-manager-get-default-display display-manager))))))

;;;   gdk_display_manager_set_default_display

(test gdk-display-manager-set-default-display
  (let* ((display-manager (gdk-display-manager-get))
         (display (gdk-display-manager-get-default-display display-manager)))
    (gdk-display-manager-set-default-display display-manager display)
    (is (eq display
            (gdk-display-manager-get-default-display display-manager)))))

;;;   gdk_display_manager_list_displays

(test gdk-display-manager-list-displays
  (let ((display-manager (gdk-display-manager-get)))
    (is-true (listp (gdk-display-manager-list-displays display-manager)))
    (is (eq 'gdk-display
            (type-of (first (gdk-display-manager-list-displays display-manager)))))))

;;;     gdk_display_manager_open_display

(test gdk-display-manager-open-display
  (let* ((display-manager (gdk-display-manager-get))
         (display-name (gdk-display-get-name (gdk-display-manager-get-default-display display-manager))))
    (is-true (gdk-display-manager-open-display display-manager display-name))))

