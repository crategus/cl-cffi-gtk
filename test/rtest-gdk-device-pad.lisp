(def-suite gdk-device-pad :in gdk-suite)
(in-suite gdk-device-pad)

;;; --- Types and Values -------------------------------------------------------

;;;     GdkDevicePadFeature

(test gdk-device-pad-feature
  ;; Check the type
  (is (g-type-is-enum "GdkDevicePadFeature"))
  ;; Check the type initializer
  (is (eq (gtype "GdkDevicePadFeature")
          (gtype (foreign-funcall "gdk_device_pad_feature_get_type" g-size))))
  ;; Check the registered name
  (is (eq 'gdk-device-pad-feature
          (registered-enum-type "GdkDevicePadFeature")))
  ;; Check the names
  (is (equal '("GDK_DEVICE_PAD_FEATURE_BUTTON" "GDK_DEVICE_PAD_FEATURE_RING"
               "GDK_DEVICE_PAD_FEATURE_STRIP")
             (mapcar #'enum-item-name
                     (get-enum-items "GdkDevicePadFeature"))))
  ;; Check the values
  (is (equal '(0 1 2)
             (mapcar #'enum-item-value
                     (get-enum-items "GdkDevicePadFeature"))))
  ;; Check the nick names
  (is (equal '("button" "ring" "strip")
             (mapcar #'enum-item-nick
                     (get-enum-items "GdkDevicePadFeature"))))
  ;; Check the enum definition
  (is (equal '(DEFINE-G-ENUM "GdkDevicePadFeature"
                             GDK-DEVICE-PAD-FEATURE
                             (:EXPORT T
                              :TYPE-INITIALIZER "gdk_device_pad_feature_get_type")
                             (:BUTTON 0)
                             (:RING 1)
                             (:STRIP 2))
             (get-g-type-definition "GdkDevicePadFeature"))))

;;;     GdkDevicePad

(test gdk-device-pad-interface
  ;; Type check
  (is (g-type-is-interface "GdkDevicePad"))
  ;; Check the registered name
  (is (eq 'gdk-device-pad
          (registered-object-type-by-name "GdkDevicePad")))
  ;; Check the type initializer
  (is (eq (gtype "GdkDevicePad")
          (gtype (foreign-funcall "gdk_device_pad_get_type" g-size))))
  ;; Get the names of the interface properties.
  (is (equal '()
             (mapcar #'g-param-spec-name
                     (g-object-interface-list-properties "GdkDevicePad"))))
  ;; Get the interface definition
  (is (equal '(DEFINE-G-INTERFACE "GdkDevicePad"
                                  GDK-DEVICE-PAD
                                  (:EXPORT T
                                   :TYPE-INITIALIZER "gdk_device_pad_get_type"))
             (get-g-type-definition "GdkDevicePad"))))

;;; --- Functions --------------------------------------------------------------

;;;     gdk_device_pad_get_n_groups
;;;     gdk_device_pad_get_group_n_modes
;;;     gdk_device_pad_get_n_features
;;;     gdk_device_pad_get_feature_group

;;; 2020-11-9
