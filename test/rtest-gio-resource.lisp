(def-suite gio-resource :in gio-suite)
(in-suite gio-resource)

;;;     GResource


;;;     GResourceFlags
;;;     GResourceLookupFlags
;;;     GStaticResource
;;;
;;;     G_RESOURCE_ERROR
;;;     GResourceError


;;;     g_resource_load ()

(test g-resource-load
  (let ((resource (g-resource-load "rtest-gio-resource.gresource")))

    (is (eq 'g-resource  (type-of resource)))

))

;;;     g_resource_new_from_data ()
;;;     g_resource_ref ()
;;;     g_resource_unref ()
;;;     g_resource_lookup_data ()
;;;     g_resource_open_stream ()
;;;     g_resource_enumerate_children ()
;;;     g_resource_get_info ()
;;;     g_static_resource_init ()
;;;     g_static_resource_fini ()
;;;     g_static_resource_get_resource ()
;;;     g_resources_register ()
;;;     g_resources_unregister ()
;;;     g_resources_lookup_data ()
;;;     g_resources_open_stream ()
;;;     g_resources_enumerate_children ()
;;;     g_resources_get_info ()
