(def-suite gio-file :in gio-suite)
(in-suite gio-file)

;;; --- Types and Values -------------------------------------------------------

;;;     GFile
;;;     GFileIface

;;;     GFileQueryInfoFlags
;;;     GFileCreateFlags
;;;     GFileCopyFlags
;;;     GFileMonitorFlags
;;;     GFileMeasureFlags
;;;     GFilesystemPreviewType

;;; --- Functions --------------------------------------------------------------

;;;     GFileProgressCallback
;;;     GFileReadMoreCallback
;;;     GFileMeasureProgressCallback
;;;
;;;     g_file_new_for_path

(test g-file-new-for-path
  (is (typep (g-file-new-for-path "path") 'g-object)))

;;;     g_file_new_for_uri

(test g-file-new-for-uri
  (is (typep (g-file-new-for-uri "http://crategus.com") 'g-object)))

;;;     g_file_new_for_commandline_arg
;;;     g_file_new_for_commandline_arg_and_cwd
;;;     g_file_new_tmp
;;;     g_file_parse_name
;;;     g_file_new_build_filename
;;;     g_file_dup
;;;     g_file_hash
;;;     g_file_equal
;;;     g_file_get_basename
;;;     g_file_get_path
;;;     g_file_peek_path
;;;     g_file_get_uri
;;;     g_file_get_parse_name
;;;     g_file_get_parent
;;;     g_file_has_parent
;;;     g_file_get_child
;;;     g_file_get_child_for_display_name
;;;     g_file_has_prefix
;;;     g_file_get_relative_path
;;;     g_file_resolve_relative_path
;;;     g_file_is_native
;;;     g_file_has_uri_scheme
;;;     g_file_get_uri_scheme
;;;     g_file_read
;;;     g_file_read_async
;;;     g_file_read_finish
;;;     g_file_append_to
;;;     g_file_create
;;;     g_file_replace
;;;     g_file_append_to_async
;;;     g_file_append_to_finish
;;;     g_file_create_async
;;;     g_file_create_finish
;;;     g_file_replace_async
;;;     g_file_replace_finish
;;;     g_file_query_info
;;;     g_file_query_info_async
;;;     g_file_query_info_finish
;;;     g_file_query_exists
;;;     g_file_query_file_type
;;;     g_file_query_filesystem_info
;;;     g_file_query_filesystem_info_async
;;;     g_file_query_filesystem_info_finish
;;;     g_file_query_default_handler
;;;     g_file_query_default_handler_async
;;;     g_file_query_default_handler_finish
;;;     g_file_measure_disk_usage
;;;     g_file_measure_disk_usage_async
;;;     g_file_measure_disk_usage_finish
;;;     g_file_find_enclosing_mount
;;;     g_file_find_enclosing_mount_async
;;;     g_file_find_enclosing_mount_finish
;;;     g_file_enumerate_children
;;;     g_file_enumerate_children_async
;;;     g_file_enumerate_children_finish
;;;     g_file_set_display_name
;;;     g_file_set_display_name_async
;;;     g_file_set_display_name_finish
;;;     g_file_delete
;;;     g_file_delete_async
;;;     g_file_delete_finish
;;;     g_file_trash
;;;     g_file_trash_async
;;;     g_file_trash_finish
;;;     g_file_copy
;;;     g_file_copy_async
;;;     g_file_copy_finish
;;;     g_file_move
;;;     g_file_make_directory
;;;     g_file_make_directory_async
;;;     g_file_make_directory_finish
;;;     g_file_make_directory_with_parents
;;;     g_file_make_symbolic_link
;;;     g_file_query_settable_attributes
;;;     g_file_query_writable_namespaces
;;;     g_file_set_attribute
;;;     g_file_set_attributes_from_info
;;;     g_file_set_attributes_async
;;;     g_file_set_attributes_finish
;;;     g_file_set_attribute_string
;;;     g_file_set_attribute_byte_string
;;;     g_file_set_attribute_uint32
;;;     g_file_set_attribute_int32
;;;     g_file_set_attribute_uint64
;;;     g_file_set_attribute_int64
;;;     g_file_mount_mountable
;;;     g_file_mount_mountable_finish
;;;     g_file_unmount_mountable
;;;     g_file_unmount_mountable_finish
;;;     g_file_unmount_mountable_with_operation
;;;     g_file_unmount_mountable_with_operation_finish
;;;     g_file_eject_mountable
;;;     g_file_eject_mountable_finish
;;;     g_file_eject_mountable_with_operation
;;;     g_file_eject_mountable_with_operation_finish
;;;     g_file_start_mountable
;;;     g_file_start_mountable_finish
;;;     g_file_stop_mountable
;;;     g_file_stop_mountable_finish
;;;     g_file_poll_mountable
;;;     g_file_poll_mountable_finish
;;;     g_file_mount_enclosing_volume
;;;     g_file_mount_enclosing_volume_finish
;;;     g_file_monitor_directory
;;;     g_file_monitor_file
;;;     g_file_monitor
;;;     g_file_load_bytes
;;;     g_file_load_bytes_async
;;;     g_file_load_bytes_finish
;;;     g_file_load_contents
;;;     g_file_load_contents_async
;;;     g_file_load_contents_finish
;;;     g_file_load_partial_contents_async
;;;     g_file_load_partial_contents_finish
;;;     g_file_replace_contents
;;;     g_file_replace_contents_async
;;;     g_file_replace_contents_bytes_async
;;;     g_file_replace_contents_finish
;;;     g_file_copy_attributes
;;;     g_file_create_readwrite
;;;     g_file_create_readwrite_async
;;;     g_file_create_readwrite_finish
;;;     g_file_open_readwrite
;;;     g_file_open_readwrite_async
;;;     g_file_open_readwrite_finish
;;;     g_file_replace_readwrite
;;;     g_file_replace_readwrite_async
;;;     g_file_replace_readwrite_finish
;;;     g_file_supports_thread_contexts

;;; 2020-12-9
