(def-suite gio-icon :in gio-suite)
(in-suite gio-icon)

;;;     GIcon

(test g-icon-interface
  ;; Type check
  (is (g-type-is-interface "GIcon"))
  ;; Check the registered name
  (is (eq 'g-icon
          (registered-object-type-by-name "GIcon")))
  ;; Check the type initializer
  (is (eq (gtype "GIcon")
          (gtype (foreign-funcall "g_icon_get_type" g-size))))
  ;; Get the names of the interface properties.
  (is (equal '()
             (mapcar #'g-param-spec-name
                     (g-object-interface-list-properties "GIcon"))))
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

;;; 2020-11-13
