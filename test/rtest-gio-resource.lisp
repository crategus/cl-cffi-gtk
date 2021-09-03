(def-suite g-resource :in gio-suite)
(in-suite g-resource)

;;; --- Types and Values -------------------------------------------------------

;;;     GResource

(test g-resource-boxed
  ;; Type check
  (is (g-type-is-a (gtype "GResource") +g-type-boxed+))
  ;; Check the type initializer
  (is (eq (gtype "GResource")
          (gtype (foreign-funcall "g_resource_get_type" g-size)))))

;;;     GResourceFlags

(test g-resource-flags
  ;; Check the type
  (is (g-type-is-flags "GResourceFlags"))
  ;; Check the registered name
  (is (eq 'g-resource-flags
          (registered-flags-type "GResourceFlags")))
  ;; Check the type initializer
  (is (eq (gtype "GResourceFlags")
          (gtype (foreign-funcall "g_resource_flags_get_type" g-size))))
  ;; Check the names
  (is (equal '("G_RESOURCE_FLAGS_NONE" "G_RESOURCE_FLAGS_COMPRESSED")
             (mapcar #'flags-item-name
                     (get-flags-items "GResourceFlags"))))
  ;; Check the values
  (is (equal '(0 1)
             (mapcar #'flags-item-value
                     (get-flags-items "GResourceFlags"))))
  ;; Check the nick names
  (is (equal '("none" "compressed")
             (mapcar #'flags-item-nick
                     (get-flags-items "GResourceFlags"))))
  ;; Check the flags definition
  (is (equal '(DEFINE-G-FLAGS "GResourceFlags"
                              G-RESOURCE-FLAGS
                              (:EXPORT T)
                              (:NONE 0)
                              (:COMPRESSED 1))
             (get-g-type-definition "GResourceFlags"))))

;;;     GResourceLookupFlags

(test g-resource-lookup-flags
  ;; Check the type
  (is (g-type-is-flags "GResourceLookupFlags"))
  ;; Check the registered name
  (is (eq 'g-resource-lookup-flags
          (registered-flags-type "GResourceLookupFlags")))
  ;; Check the type initializer
  (is (eq (gtype "GResourceLookupFlags")
          (gtype (foreign-funcall "g_resource_lookup_flags_get_type" g-size))))
  ;; Check the names
  (is (equal '("G_RESOURCE_LOOKUP_FLAGS_NONE")
             (mapcar #'flags-item-name
                     (get-flags-items "GResourceLookupFlags"))))
  ;; Check the values
  (is (equal '(0)
             (mapcar #'flags-item-value
                     (get-flags-items "GResourceLookupFlags"))))
  ;; Check the nick names
  (is (equal '("none")
             (mapcar #'flags-item-nick
                     (get-flags-items "GResourceLookupFlags"))))
  ;; Check the flags definition
  (is (equal '(DEFINE-G-FLAGS "GResourceLookupFlags"
                              G-RESOURCE-LOOKUP-FLAGS
                              (:EXPORT T)
                              (:NONE 0))
             (get-g-type-definition "GResourceLookupFlags"))))

;;;     GStaticResource
;;;     G_RESOURCE_ERROR
;;;     GResourceError

;;; --- Functions --------------------------------------------------------------

;;;     g-resource-load

(test g-resource-load.1
  (let ((resource (g-resource-load "rtest-gio-resource.gresource")))
    (is (typep resource 'g-resource))))

(test g-resource-load.2
  (let ((resource (g-resource-load "unknown")))
    ;; Ignoring error when loading, the return value is a G-RESOURCE instance
    (is (typep resource 'g-resource))))

;;;     g_resource_new_from_data
;;;     g_resource_ref
;;;     g_resource_unref

;;;     g_resource_lookup_data

(test g-resource-lookup-data
  (let ((resource (g-resource-load "rtest-gio-resource.gresource")))
    (is-false (g-resources-register resource))
    (is (pointerp (g-resource-lookup-data resource
                                          "/com/crategus/test/ducky.png"
                                          :none)))
    (is (pointerp (g-resource-lookup-data resource
                                          "/com/crategus/test/rtest-dialog.ui"
                                          :none)))
    (is-false (g-resources-unregister resource))))

;;;     g_resource_open_stream

;;;     g_resource_enumerate_children

(test g-resource-enumerate-children
  (let ((resource (g-resource-load "rtest-gio-resource.gresource")))
    (is-false (g-resources-register resource))
    (is (equal '("ducky.png" "floppybuddy.gif" "gtk-logo-24.png"
                 "rtest-application.ui" "rtest-dialog.ui" "rtest-dialog2.ui")
               (sort (g-resource-enumerate-children resource
                                                    "/com/crategus/test"
                                                    :none)
                     #'string<)))
    (is-false (g-resources-unregister resource))))

;;;     g_resource_get_info

(test g-resource-info
  (let ((resource (g-resource-load "rtest-gio-resource.gresource")))
    (is-false (g-resources-register resource))
    (is (equal '(248546 0)
               (multiple-value-list
                   (g-resource-info resource
                                    "/com/crategus/test/ducky.png"
                                    :none))))
    (is (equal '(5216 0)
               (multiple-value-list
                   (g-resource-info resource
                                    "/com/crategus/test/floppybuddy.gif"
                                    :none))))
    (is (equal '(1703 0)
               (multiple-value-list
                   (g-resource-info resource
                                    "/com/crategus/test/rtest-application.ui"
                                    :none))))
    (is-false (g-resources-unregister resource))))

;;;     g_static_resource_init
;;;     g_static_resource_fini
;;;     g_static_resource_get_resource

;;;     g_resources_register
;;;     g_resources_unregister

(test g-resources-register
  (let ((image nil)
        (resource (g-resource-load "rtest-gio-resource.gresource")))
    (is (typep resource 'g-resource))
    ;; Register the resource
    (is-false (g-resources-register resource))
    (is (typep (setf image
                     (gtk-image-new-from-resource "/com/crategus/test/ducky.png"))
               'gtk-image))
    ;; Pixbuf is loaded
    (is (typep (gtk-image-pixbuf image) 'gdk-pixbuf))
    ;; Unregister the resource
    (is-false (g-resources-unregister resource))
    (is (typep (setf image
                     (gtk-image-new-from-resource "/com/crategus/test/ducky.png"))
               'gtk-image))
    ;; Pixbuf is not loaded
    (is-false (gtk-image-pixbuf image) 'gdk-pixbuf)))

;;;     g_resources_lookup_data

(test g-resources-lookup-data
  (let ((resource (g-resource-load "rtest-gio-resource.gresource")))
    (is-false (g-resources-register resource))
    (is (pointerp (g-resources-lookup-data "/com/crategus/test/ducky.png"
                                           :none)))
    (is (pointerp (g-resources-lookup-data "/com/crategus/test/rtest-dialog.ui"
                                           :none)))
    (is-false (g-resources-unregister resource))))

;;;     g_resources_open_stream

;;;     g_resources_enumerate_children

(test g-resources-enumerate-children
  (let ((resource (g-resource-load "rtest-gio-resource.gresource")))
    (is-false (g-resources-register resource))
    (is (equal '("ducky.png" "floppybuddy.gif" "gtk-logo-24.png"
                 "rtest-application.ui" "rtest-dialog.ui" "rtest-dialog2.ui")
               (sort (g-resources-enumerate-children "/com/crategus/test"
                                                     :none)
                     #'string<)))
    (is-false (g-resources-unregister resource))))

;;;     g_resources_get_info

(test g-resources-info
  (let ((resource (g-resource-load "rtest-gio-resource.gresource")))
    (is-false (g-resources-register resource))
    (is (equal '(248546 0)
               (multiple-value-list
                   (g-resources-info "/com/crategus/test/ducky.png"
                                     :none))))
    (is (equal '(5216 0)
               (multiple-value-list
                   (g-resources-info "/com/crategus/test/floppybuddy.gif"
                                     :none))))
    (is (equal '(1703 0)
               (multiple-value-list
                   (g-resources-info "/com/crategus/test/rtest-application.ui"
                                     :none))))
    (is-false (g-resources-unregister resource))))

;;; 2021-8-16
