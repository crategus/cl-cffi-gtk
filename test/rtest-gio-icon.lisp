
(def-suite gio-icon :in gio-suite)
(in-suite gio-icon)

;;;   GIcon

(test gio-icon-interface
  ;; Type checks
  (is-false (g-type-is-object "GIcon"))
  (is-false (g-type-is-abstract "GIcon"))
  (is-true  (g-type-is-derived "GIcon"))
  (is-false (g-type-is-fundamental "GIcon"))
  (is-true  (g-type-is-value-type "GIcon"))
  (is-true  (g-type-has-value-table "GIcon"))
  (is-false (g-type-is-classed "GIcon"))
  (is-false (g-type-is-instantiatable "GIcon"))
  (is-true  (g-type-is-derivable "GIcon"))
  (is-false (g-type-is-deep-derivable "GIcon"))
  (is-true  (g-type-is-interface "GIcon"))

  ;; Check the registered name
  (is (eq 'g-icon
          (registered-object-type-by-name "GIcon")))

  ;; Check infos about the C interface implementation
  (let ((class (g-type-default-interface-ref (gtype "GIcon"))))
    (is (equal (gtype "GIcon") (g-type-from-interface class)))
    (g-type-default-interface-unref class))

  ;; Check infos about the Lisp class implementation
  (let ((class (find-class 'g-icon)))
    ;; Check the class name and type of the class
    (is (eq 'g-icon (class-name class)))
    (is (eq 'gobject-class (type-of class)))
    (is (eq (find-class 'gobject-class) (class-of class)))
    ;; Properties of the metaclass gobject-class
    (is (equal "GIcon" (gobject-class-g-type-name class)))
    (is (equal "GIcon" (gobject-class-direct-g-type-name class)))
    (is (equal "g_icon_get_type"
               (gobject-class-g-type-initializer class)))
    (is-true (gobject-class-interface-p class)))

  ;; Check some more GType information
  (is (equal (gtype "GInterface") (g-type-parent "GIcon")))
  (is (= 2 (g-type-depth "GIcon")))
  (is (equal (gtype "GIcon")
             (g-type-next-base "GIcon" "GInterface")))
  (is-true  (g-type-is-a "GIcon" "GInterface"))
  (is-false (g-type-is-a "GIcon" "GtkWidget"))
  (is-false (g-type-is-a "GIcon" "gboolean"))
  (is-false (g-type-is-a "GIcon" "GtkWindow"))

  ;; Check the children
  (is (equal '()
             (mapcar #'gtype-name (g-type-children "GIcon"))))

  ;; Get the names of the interface properties.
  (is (equal '()
             (mapcar #'param-spec-name (g-object-interface-list-properties "GIcon"))))

  ;; Get the interface definition
  (is (equal '(DEFINE-G-INTERFACE "GIcon" G-ICON
                (:EXPORT T))
                (get-g-type-definition "GIcon"))))

;;;     GIconIface

;;;   g_icon_hash

(test g-icon-hash
  (let* ((names (list "gnome-dev-cdrom-audio"))
         (icon1 (g-themed-icon-new-from-names names))
         (icon2 (g-themed-icon-new-with-default-fallbacks "gnome-dev-cdrom-audio")))
;    (is (equal (g-themed-icon-names icon1)
;               (g-themed-icon-names icon2)))
    (is (= 1275220063 (g-icon-hash icon1)))
    (is (= 2604122446 (g-icon-hash icon2)))
;    (is (= (g-icon-hash icon1) (g-icon-hash icon2)))
))

;;;   g_icon_equal

(test g-icon-equal
  (let* ((names (list "gnome-dev-cdrom-audio" "gnome-dev-cdrom" "gnome-dev" "gnome"))
         (icon1 (g-themed-icon-new-from-names names))
         (icon2 (g-themed-icon-new-with-default-fallbacks "gnome-dev-cdrom-audio")))
;    (is (equal (g-themed-icon-names icon1)
;               (g-themed-icon-names icon2)))
    (is-true (g-icon-equal icon1 icon2))))

;;;   g_icon_to_string

(test g-icon-to-string
  (let* ((names (list "gnome-dev-cdrom-audio" "gnome-dev-cdrom" "gnome-dev" "gnome"))
         (icon1 (g-themed-icon-new "gnome-dev-cdrom-audio"))
         (icon2 (g-themed-icon-new-from-names names))
         (icon3 (g-themed-icon-new-with-default-fallbacks "gnome-dev-cdrom-audio")))
    (is (equal "gnome-dev-cdrom-audio"
               (g-icon-to-string icon1)))
    (is (equal ". GThemedIcon gnome-dev-cdrom-audio gnome-dev-cdrom gnome-dev gnome gnome-dev-cdrom-audio-symbolic gnome-dev-cdrom-symbolic gnome-dev-symbolic gnome-symbolic"
               (g-icon-to-string icon2)))
    (is (equal ". GThemedIcon gnome-dev-cdrom-audio gnome-dev-cdrom gnome-dev gnome gnome-dev-cdrom-audio-symbolic gnome-dev-cdrom-symbolic gnome-dev-symbolic gnome-symbolic"
               (g-icon-to-string icon3)))))

;;;   g_icon_new_for_string

(test g-icon-new-for-string
  (let ((icon1 (g-icon-new-for-string "gnome-dev-cdrom-audio"))
        (icon2 (g-icon-new-for-string ". GThemedIcon gnome-dev-cdrom-audio gnome-dev-cdrom gnome-dev gnome"))
        (icon3 (g-themed-icon-new-with-default-fallbacks "gnome-dev-cdrom-audio")))
    (is (equal "gnome-dev-cdrom-audio"
               (g-icon-to-string icon1)))
    (is (equal ". GThemedIcon gnome-dev-cdrom-audio gnome-dev-cdrom gnome-dev gnome gnome-dev-cdrom-audio-symbolic gnome-dev-cdrom-symbolic gnome-dev-symbolic gnome-symbolic"
               (g-icon-to-string icon2)))
    (is (equal ". GThemedIcon gnome-dev-cdrom-audio gnome-dev-cdrom gnome-dev gnome gnome-dev-cdrom-audio-symbolic gnome-dev-cdrom-symbolic gnome-dev-symbolic gnome-symbolic"
               (g-icon-to-string icon3)))))

