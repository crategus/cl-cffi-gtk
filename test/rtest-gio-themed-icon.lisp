(def-suite gio-themed-icon :in gio-suite)
(in-suite gio-themed-icon)

;;;   GThemedIcon

(test gio-themed-icon-class
  ;; Type checks
  (is-true  (g-type-is-object "GThemedIcon"))
  (is-false (g-type-is-abstract "GThemedIcon"))
  (is-true  (g-type-is-derived "GThemedIcon"))
  (is-false (g-type-is-fundamental "GThemedIcon"))
  (is-true  (g-type-is-value-type "GThemedIcon"))
  (is-true  (g-type-has-value-table "GThemedIcon"))
  (is-true  (g-type-is-classed "GThemedIcon"))
  (is-true  (g-type-is-instantiatable "GThemedIcon"))
  (is-true  (g-type-is-derivable "GThemedIcon"))
  (is-true  (g-type-is-deep-derivable "GThemedIcon"))
  (is-false (g-type-is-interface "GThemedIcon"))

  ;; Check the registered name
  (is (eq 'g-themed-icon
          (registered-object-type-by-name "GThemedIcon")))

  ;; Check infos about the C class implementation
  (let ((class (g-type-class-ref (gtype "GThemedIcon"))))
    (is (equal (gtype "GThemedIcon") (g-type-from-class class)))
    (is (equal (gtype "GThemedIcon") (g-object-class-type class)))
    (is (equal "GThemedIcon" (g-object-class-name class)))
    (is (equal (gtype "GThemedIcon") (g-type-from-class  (g-type-class-peek "GThemedIcon"))))
    (is (equal (gtype "GThemedIcon") (g-type-from-class  (g-type-class-peek-static "GThemedIcon"))))
    (g-type-class-unref class))

  ;; Check infos about the Lisp class implementation
  (let ((class (find-class 'g-themed-icon)))
    ;; Check the class name and type of the class
    (is (eq 'g-themed-icon (class-name class)))
    (is (eq 'gobject-class (type-of class)))
    (is (eq (find-class 'gobject-class) (class-of class)))
    ;; Properties of the metaclass gobject-class
    (is (equal "GThemedIcon" (gobject-class-g-type-name class)))
    (is (equal "GThemedIcon" (gobject-class-direct-g-type-name class)))
    (is (equal "g_themed_icon_get_type"
               (gobject-class-g-type-initializer class)))
    (is-false (gobject-class-interface-p class)))

  ;; Check some more GType information
  (is (equal (gtype "GObject") (g-type-parent "GThemedIcon")))
  (is (= 2 (g-type-depth "GThemedIcon")))
  (is (equal (gtype "GThemedIcon")
             (g-type-next-base "GThemedIcon" "GObject")))
  (is-true  (g-type-is-a "GThemedIcon" "GObject"))
  (is-false (g-type-is-a "GThemedIcon" "GtkWidget"))
  (is-false (g-type-is-a "GThemedIcon" "gboolean"))

  ;; Check the children
  (is (equal '()
             (mapcar #'gtype-name (g-type-children "GThemedIcon"))))
  ;; Check the interfaces
  (is (equal '("GIcon")
             (mapcar #'gtype-name (g-type-interfaces "GThemedIcon"))))

  ;; Query infos about the class
  (with-foreign-object (query '(:struct g-type-query))
    (g-type-query "GThemedIcon" query)
    (is (equal (gtype "GThemedIcon")
               (foreign-slot-value query '(:struct g-type-query) :type)))
    (is (equal "GThemedIcon"
               (foreign-slot-value query '(:struct g-type-query) :type-name)))
    (is (= 136
           (foreign-slot-value query '(:struct g-type-query) :class-size)))
    (is (= 48
           (foreign-slot-value query '(:struct g-type-query) :instance-size))))

  ;; Get the names of the class properties.
  (is (equal '("name" "names" "use-default-fallbacks")
             (mapcar #'param-spec-name (g-object-class-list-properties "GThemedIcon"))))

  ;; No style properties
  ;; No child properties

  ;; Get the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GThemedIcon" G-THEMED-ICON
                       (:SUPERCLASS G-OBJECT :EXPORT T :INTERFACES ("GIcon"))
                       ((NAME G-THEMED-ICON-NAME "name" "gchararray" NIL NIL)
                        (NAMES G-THEMED-ICON-NAMES "names" "GStrv" T NIL)
                        (USE-DEFAULT-FALLBACKS
                         G-THEMED-ICON-USE-DEFAULT-FALLBACKS
                         "use-default-fallbacks" "gboolean" T NIL)))
             (get-g-type-definition "GThemedIcon"))))

(test g-themed-icon-properties
  (let ((object (g-themed-icon-new-with-default-fallbacks "gnome-dev-cdrom-audio")))
;    (is-false (g-themed-icon-name object)) ; Not Readable
    (is (equal '("gnome-dev-cdrom-audio")
               (g-themed-icon-names object)))
    (is-true  (g-themed-icon-use-default-fallbacks object))))

;;;   g_themed_icon_new
;;;   g_themed_icon_append_name
;;;   g_themed_icon_prepend_name
;;;   g_themed_icon_get_names

(test g-themed-icon-new
  (let ((icon (g-themed-icon-new "gnome-dev-cdrom")))
    (is (equal '("gnome-dev-cdrom")
               (g-themed-icon-names icon)))
    (g-themed-icon-append-name icon "gnome-dev")
    (is (equal '("gnome-dev-cdrom" "gnome-dev")
               (g-themed-icon-names icon)))
    (g-themed-icon-prepend-name icon "gnome-dev-cdrom-audio")
    (is (equal '("gnome-dev-cdrom-audio" "gnome-dev-cdrom" "gnome-dev")
               (g-themed-icon-names icon)))))

;;;   g_themed_icon_new_from_names
;;;   g_themed_icon_get_names

(test g-themed-icon-new-from-names
  (let ((icon (g-themed-icon-new-from-names "gnome-dev-cdrom-audio"
                                            "gnome-dev-cdrom"
                                            "gnome-dev"
                                            "gnome")))
    (is (equal '("gnome-dev-cdrom-audio" "gnome-dev-cdrom" "gnome-dev"
                 "gnome")
               (g-themed-icon-names icon)))))

;;;   g_themed_icon_new_with_default_fallbacks

(test g-themed-icon-new-with-default-fallbacks
  (let* ((names (list "gnome-dev-cdrom-audio" "gnome-dev-cdrom" "gnome-dev" "gnome"))
         (icon1 (g-themed-icon-new-from-names names))
         (icon2 (g-themed-icon-new-with-default-fallbacks "gnome-dev-cdrom-audio")))
;    (is (equal (g-themed-icon-names icon1)
;               (g-themed-icon-names icon2)))
    (is (= 2604122446 (g-icon-hash icon1)))
    (is (= 2604122446 (g-icon-hash icon2)))
;    (is (= (g-icon-hash icon1) (g-icon-hash icon2)))
    (is-true (g-icon-equal icon1 icon2))
))

;;;     g_themed_icon_prepend_name
;;;     g_themed_icon_append_name
;;;     g_themed_icon_get_names

