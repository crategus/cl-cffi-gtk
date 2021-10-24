(def-suite gio-themed-icon :in gio-suite)
(in-suite gio-themed-icon)

;;; --- Types and Values -------------------------------------------------------

;;;     GThemedIcon

(test g-themed-icon-class
  ;; Type check
  (is (g-type-is-object "GThemedIcon"))
  ;; Check the registered name
  (is (eq 'g-themed-icon
          (registered-object-type-by-name "GThemedIcon")))
  ;; Check the type initializer
  (is (eq (gtype "GThemedIcon")
          (gtype (foreign-funcall "g_themed_icon_get_type" g-size))))
  ;; Check the parent
  (is (eq (gtype "GObject") (g-type-parent "GThemedIcon")))
  ;; Check the children
  (is (equal '()
             (mapcar #'g-type-name (g-type-children "GThemedIcon"))))
  ;; Check the interfaces
  (is (equal '("GIcon")
             (mapcar #'g-type-name (g-type-interfaces "GThemedIcon"))))
  ;; Check the class properties
  (is (equal '("name" "names" "use-default-fallbacks")
             (stable-sort (mapcar #'g-param-spec-name
                                  (g-object-class-list-properties "GThemedIcon"))
                          #'string-lessp)))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GThemedIcon" G-THEMED-ICON
                       (:SUPERCLASS G-OBJECT :EXPORT T :INTERFACES ("GIcon"))
                       ((NAME G-THEMED-ICON-NAME "name" "gchararray" NIL NIL)
                        (NAMES G-THEMED-ICON-NAMES "names" "GStrv" T NIL)
                        (USE-DEFAULT-FALLBACKS
                         G-THEMED-ICON-USE-DEFAULT-FALLBACKS
                         "use-default-fallbacks" "gboolean" T NIL)))
             (get-g-type-definition "GThemedIcon"))))

;;; --- Functions --------------------------------------------------------------

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

;;; 2021-10-18
