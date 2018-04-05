﻿;;; ----------------------------------------------------------------------------
;;; gio.file.lisp
;;;
;;; The documentation has been copied from the GIO Reference Manual
;;; for GIO 2.32.3. The latest version of this documentation can be found
;;; on-line at http://library.gnome.org/devel/gio/unstable/.
;;;
;;; Copyright (C) 2012 Dieter Kaiser
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU Lesser General Public License for Lisp
;;; as published by the Free Software Foundation, either version 3 of the
;;; License, or (at your option) any later version and with a preamble to
;;; the GNU Lesser General Public License that clarifies the terms for use
;;; with Lisp programs and is referred as the LLGPL.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU Lesser General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with this program and the preamble to the Gnu Lesser
;;; General Public License.  If not, see <http://www.gnu.org/licenses/>
;;; and <http://opensource.franz.com/preamble.html>.
;;; ----------------------------------------------------------------------------
;;;
;;; GFile
;;;
;;; File and Directory Handling
;;;
;;; Synopsis
;;;
;;;     GFile
;;;     GFileIface
;;;     GFileQueryInfoFlags
;;;     GFileCreateFlags
;;;     GFileCopyFlags
;;;     GFileMonitorFlags
;;;     GFilesystemPreviewType
;;;
;;;     g_file_new_for_path
;;;     g_file_new_for_uri
;;;     g_file_new_for_commandline_arg
;;;     g_file_new_tmp
;;;     g_file_parse_name
;;;     g_file_dup
;;;     g_file_hash
;;;     g_file_equal
;;;     g_file_get_basename
;;;     g_file_get_path
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
;;;     g_file_trash
;;;     g_file_copy
;;;     g_file_copy_async
;;;     g_file_copy_finish
;;;     g_file_move
;;;     g_file_make_directory
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
;;;     g_file_load_contents
;;;     g_file_load_contents_async
;;;     g_file_load_contents_finish
;;;     g_file_load_partial_contents_async
;;;     g_file_load_partial_contents_finish
;;;     g_file_replace_contents
;;;     g_file_replace_contents_async
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
;;;
;;; Object Hierarchy
;;;
;;;   GInterface
;;;    +----GFile
;;;
;;; Prerequisites
;;;
;;; GFile requires GObject.
;;;
;;; Description
;;;
;;; GFile is a high level abstraction for manipulating files on a virtual file
;;; system. GFiles are lightweight, immutable objects that do no I/O upon
;;; creation. It is necessary to understand that GFile objects do not represent
;;; files, merely an identifier for a file. All file content I/O is implemented
;;; as streaming operations (see GInputStream and GOutputStream).
;;;
;;; To construct a GFile, you can use: g_file_new_for_path() if you have a path.
;;; g_file_new_for_uri() if you have a URI. g_file_new_for_commandline_arg() for
;;; a command line argument. g_file_new_tmp() to create a temporary file from a
;;; template. g_file_parse_name() from a utf8 string gotten from
;;; g_file_get_parse_name().
;;;
;;; One way to think of a GFile is as an abstraction of a pathname. For normal
;;; files the system pathname is what is stored internally, but as GFiles are
;;; extensible it could also be something else that corresponds to a pathname in
;;; a userspace implementation of a filesystem.
;;;
;;; GFiles make up hierarchies of directories and files that correspond to the
;;; files on a filesystem. You can move through the file system with GFile using
;;; g_file_get_parent() to get an identifier for the parent directory,
;;; g_file_get_child() to get a child within a directory,
;;; g_file_resolve_relative_path() to resolve a relative path between two
;;; GFiles. There can be multiple hierarchies, so you may not end up at the same
;;; root if you repeatedly call g_file_get_parent() on two different files.
;;;
;;; All GFiles have a basename (get with g_file_get_basename()). These names are
;;; byte strings that are used to identify the file on the filesystem (relative
;;; to its parent directory) and there is no guarantees that they have any
;;; particular charset encoding or even make any sense at all. If you want to
;;; use filenames in a user interface you should use the display name that you
;;; can get by requesting the G_FILE_ATTRIBUTE_STANDARD_DISPLAY_NAME attribute
;;; with g_file_query_info(). This is guaranteed to be in utf8 and can be used
;;; in a user interface. But always store the real basename or the GFile to use
;;; to actually access the file, because there is no way to go from a display
;;; name to the actual name.
;;;
;;; Using GFile as an identifier has the same weaknesses as using a path in that
;;; there may be multiple aliases for the same file. For instance, hard or soft
;;; links may cause two different GFiles to refer to the same file. Other
;;; possible causes for aliases are: case insensitive filesystems, short and
;;; long names on Fat/NTFS, or bind mounts in Linux. If you want to check if two
;;; GFiles point to the same file you can query for the G_FILE_ATTRIBUTE_ID_FILE
;;; attribute. Note that GFile does some trivial canonicalization of pathnames
;;; passed in, so that trivial differences in the path string used at creation
;;; (duplicated slashes, slash at end of path, "." or ".." path segments, etc)
;;; does not create different GFiles.
;;;
;;; Many GFile operations have both synchronous and asynchronous versions to
;;; suit your application. Asynchronous versions of synchronous functions simply
;;; have _async() appended to their function names. The asynchronous I/O
;;; functions call a GAsyncReadyCallback which is then used to finalize the
;;; operation, producing a GAsyncResult which is then passed to the function's
;;; matching _finish() operation.
;;;
;;; Some GFile operations do not have synchronous analogs, as they may take a
;;; very long time to finish, and blocking may leave an application unusable.
;;; Notable cases include: g_file_mount_mountable() to mount a mountable file.
;;; g_file_unmount_mountable_with_operation() to unmount a mountable file.
;;; g_file_eject_mountable_with_operation() to eject a mountable file.
;;;
;;; One notable feature of GFiles are entity tags, or "etags" for short. Entity
;;; tags are somewhat like a more abstract version of the traditional mtime, and
;;; can be used to quickly determine if the file has been modified from the
;;; version on the file system. See the HTTP 1.1 specification for HTTP Etag
;;; headers, which are a very similar concept.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GFile
;;;
;;; typedef struct _GFile GFile;
;;;
;;; A handle to an object implementing the GFileIface interface. Generally
;;; stores a location within the file system. Handles do not necessarily
;;; represent files or directories that currently exist.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; struct GFileIface
;;;
;;; struct GFileIface {
;;;   GTypeInterface g_iface;
;;;
;;;   /* Virtual Table */
;;;
;;;   GFile *        (* dup)                        (GFile       *file);
;;;   guint          (* hash)                       (GFile       *file);
;;;   gboolean       (* equal)                      (GFile       *file1,
;;;                                                  GFile       *file2);
;;;   gboolean       (* is_native)                  (GFile       *file);
;;;   gboolean       (* has_uri_scheme)             (GFile       *file,
;;;                                                  const char  *uri_scheme);
;;;   char *         (* get_uri_scheme)             (GFile       *file);
;;;   char *         (* get_basename)               (GFile       *file);
;;;   char *         (* get_path)                   (GFile       *file);
;;;   char *         (* get_uri)                    (GFile       *file);
;;;   char *         (* get_parse_name)             (GFile       *file);
;;;   GFile *        (* get_parent)                 (GFile       *file);
;;;   gboolean       (* prefix_matches)             (GFile       *prefix,
;;;                                                  GFile       *file);
;;;   char *         (* get_relative_path)          (GFile       *parent,
;;;                                                  GFile       *descendant);
;;;   GFile *        (* resolve_relative_path)      (GFile      *file,
;;;                                                  const char *relative_path);
;;;   GFile *        (* get_child_for_display_name) (GFile      *file,
;;;                                                  const char *display_name,
;;;                                                  GError    **error);
;;;
;;;   GFileEnumerator * (* enumerate_children)      (GFile                *file,
;;;                                                  const char           *attributes,
;;;                                                  GFileQueryInfoFlags   flags,
;;;                                                  GCancellable         *cancellable,
;;;                                                  GError              **error);
;;;   void           (* enumerate_children_async)   (GFile                *file,
;;;                                                  const char           *attributes,
;;;                                                  GFileQueryInfoFlags   flags,
;;;                                                  int                   io_priority,
;;;                                                  GCancellable         *cancellable,
;;;                                                  GAsyncReadyCallback   callback,
;;;                                                  gpointer              user_data);
;;;   GFileEnumerator * (* enumerate_children_finish)
;;;                                                 (GFile                *file,
;;;                                                        GAsyncResult         *res,
;;;                                                        GError              **error);
;;;
;;;   GFileInfo *         (* query_info)                  (GFile                *file,
;;;                                                        const char           *attributes,
;;;                                                        GFileQueryInfoFlags   flags,
;;;                                                        GCancellable         *cancellable,
;;;                                                        GError              **error);
;;;   void                (* query_info_async)            (GFile                *file,
;;;                                                        const char           *attributes,
;;;                                                        GFileQueryInfoFlags   flags,
;;;                                                        int                   io_priority,
;;;                                                        GCancellable         *cancellable,
;;;                                                        GAsyncReadyCallback   callback,
;;;                                                        gpointer              user_data);
;;;   GFileInfo *         (* query_info_finish)           (GFile                *file,
;;;                                                        GAsyncResult         *res,
;;;                                                        GError              **error);
;;;
;;;   GFileInfo *         (* query_filesystem_info)       (GFile                *file,
;;;                                                        const char           *attributes,
;;;                                                        GCancellable         *cancellable,
;;;                                                        GError              **error);
;;;   void                (* query_filesystem_info_async) (GFile                *file,
;;;                                                        const char           *attributes,
;;;                                                        int                   io_priority,
;;;                                                        GCancellable         *cancellable,
;;;                                                        GAsyncReadyCallback   callback,
;;;                                                        gpointer              user_data);
;;;   GFileInfo *         (* query_filesystem_info_finish)(GFile                *file,
;;;                                                        GAsyncResult         *res,
;;;                                                        GError              **error);
;;;
;;;   GMount *            (* find_enclosing_mount)        (GFile                *file,
;;;                                                        GCancellable         *cancellable,
;;;                                                        GError              **error);
;;;   void                (* find_enclosing_mount_async)  (GFile                *file,
;;;                                                        int                   io_priority,
;;;                                                        GCancellable         *cancellable,
;;;                                                        GAsyncReadyCallback   callback,
;;;                                                        gpointer              user_data);
;;;   GMount *            (* find_enclosing_mount_finish) (GFile                *file,
;;;                                                        GAsyncResult         *res,
;;;                                                        GError              **error);
;;;
;;;   GFile *             (* set_display_name)            (GFile                *file,
;;;                                                        const char           *display_name,
;;;                                                        GCancellable         *cancellable,
;;;                                                        GError              **error);
;;;   void                (* set_display_name_async)      (GFile                *file,
;;;                                                        const char           *display_name,
;;;                                                        int                   io_priority,
;;;                                                        GCancellable         *cancellable,
;;;                                                        GAsyncReadyCallback   callback,
;;;                                                        gpointer              user_data);
;;;   GFile *             (* set_display_name_finish)     (GFile                *file,
;;;                                                        GAsyncResult         *res,
;;;                                                        GError              **error);
;;;
;;;   GFileAttributeInfoList * (* query_settable_attributes)    (GFile          *file,
;;;                                                              GCancellable   *cancellable,
;;;                                                              GError        **error);
;;;   void                (* _query_settable_attributes_async)  (void);
;;;   void                (* _query_settable_attributes_finish) (void);
;;;
;;;   GFileAttributeInfoList * (* query_writable_namespaces)    (GFile          *file,
;;;                                                              GCancellable   *cancellable,
;;;                                                              GError        **error);
;;;   void                (* _query_writable_namespaces_async)  (void);
;;;   void                (* _query_writable_namespaces_finish) (void);
;;;
;;;   gboolean            (* set_attribute)               (GFile                *file,
;;;                                                        const char           *attribute,
;;;                                                        GFileAttributeType    type,
;;;                                                        gpointer              value_p,
;;;                                                        GFileQueryInfoFlags   flags,
;;;                                                        GCancellable         *cancellable,
;;;                                                        GError              **error);
;;;   gboolean            (* set_attributes_from_info)    (GFile                *file,
;;;                                                        GFileInfo            *info,
;;;                                                        GFileQueryInfoFlags   flags,
;;;                                                        GCancellable         *cancellable,
;;;                                                        GError              **error);
;;;   void                (* set_attributes_async)        (GFile                *file,
;;;                                                        GFileInfo            *info,
;;;                                                        GFileQueryInfoFlags   flags,
;;;                                                        int                   io_priority,
;;;                                                        GCancellable         *cancellable,
;;;                                                        GAsyncReadyCallback   callback,
;;;                                                        gpointer              user_data);
;;;   gboolean            (* set_attributes_finish)       (GFile                *file,
;;;                                                        GAsyncResult         *result,
;;;                                                        GFileInfo           **info,
;;;                                                        GError              **error);
;;;
;;;   GFileInputStream *  (* read_fn)                     (GFile                *file,
;;;                                                        GCancellable         *cancellable,
;;;                                                        GError              **error);
;;;   void                (* read_async)                  (GFile                *file,
;;;                                                        int                   io_priority,
;;;                                                        GCancellable         *cancellable,
;;;                                                        GAsyncReadyCallback   callback,
;;;                                                        gpointer              user_data);
;;;   GFileInputStream *  (* read_finish)                 (GFile                *file,
;;;                                                        GAsyncResult         *res,
;;;                                                        GError              **error);
;;;
;;;   GFileOutputStream * (* append_to)                   (GFile                *file,
;;;                                                        GFileCreateFlags      flags,
;;;                                                        GCancellable         *cancellable,
;;;                                                        GError              **error);
;;;   void                (* append_to_async)             (GFile                *file,
;;;                                                        GFileCreateFlags      flags,
;;;                                                        int                   io_priority,
;;;                                                        GCancellable         *cancellable,
;;;                                                        GAsyncReadyCallback   callback,
;;;                                                        gpointer              user_data);
;;;   GFileOutputStream * (* append_to_finish)            (GFile                *file,
;;;                                                        GAsyncResult         *res,
;;;                                                        GError              **error);
;;;
;;;   GFileOutputStream * (* create)                      (GFile                *file,
;;;                                                        GFileCreateFlags      flags,
;;;                                                        GCancellable         *cancellable,
;;;                                                        GError              **error);
;;;   void                (* create_async)                (GFile                *file,
;;;                                                        GFileCreateFlags      flags,
;;;                                                        int                   io_priority,
;;;                                                        GCancellable         *cancellable,
;;;                                                        GAsyncReadyCallback   callback,
;;;                                                        gpointer              user_data);
;;;   GFileOutputStream * (* create_finish)               (GFile                *file,
;;;                                                        GAsyncResult         *res,
;;;                                                        GError              **error);
;;;
;;;   GFileOutputStream * (* replace)                     (GFile                *file,
;;;                                                        const char           *etag,
;;;                                                        gboolean              make_backup,
;;;                                                        GFileCreateFlags      flags,
;;;                                                        GCancellable         *cancellable,
;;;                                                        GError              **error);
;;;   void                (* replace_async)               (GFile                *file,
;;;                                                        const char           *etag,
;;;                                                        gboolean              make_backup,
;;;                                                        GFileCreateFlags      flags,
;;;                                                        int                   io_priority,
;;;                                                        GCancellable         *cancellable,
;;;                                                        GAsyncReadyCallback   callback,
;;;                                                        gpointer              user_data);
;;;   GFileOutputStream * (* replace_finish)              (GFile                *file,
;;;                                                        GAsyncResult         *res,
;;;                                                        GError              **error);
;;;
;;;   gboolean            (* delete_file)                 (GFile                *file,
;;;                                                        GCancellable         *cancellable,
;;;                                                        GError              **error);
;;;   void                (* _delete_file_async)          (void);
;;;   void                (* _delete_file_finish)         (void);
;;;
;;;   gboolean            (* trash)                       (GFile                *file,
;;;                                                        GCancellable         *cancellable,
;;;                                                        GError              **error);
;;;   void                (* _trash_async)                (void);
;;;   void                (* _trash_finish)               (void);
;;;
;;;   gboolean            (* make_directory)              (GFile                *file,
;;;                                                        GCancellable         *cancellable,
;;;                                                        GError              **error);
;;;   void                (* _make_directory_async)       (void);
;;;   void                (* _make_directory_finish)      (void);
;;;
;;;   gboolean            (* make_symbolic_link)          (GFile                *file,
;;;                                                        const char           *symlink_value,
;;;                                                        GCancellable         *cancellable,
;;;                                                        GError              **error);
;;;   void                (* _make_symbolic_link_async)   (void);
;;;   void                (* _make_symbolic_link_finish)  (void);
;;;
;;;   gboolean            (* copy)                        (GFile                *source,
;;;                                                        GFile                *destination,
;;;                                                        GFileCopyFlags        flags,
;;;                                                        GCancellable         *cancellable,
;;;                                                        GFileProgressCallback progress_callback,
;;;                                                        gpointer              progress_callback_data,
;;;                                                        GError              **error);
;;;   void                (* copy_async)                  (GFile                *source,
;;;                                                        GFile                *destination,
;;;                                                        GFileCopyFlags        flags,
;;;                                                        int                   io_priority,
;;;                                                        GCancellable         *cancellable,
;;;                                                        GFileProgressCallback progress_callback,
;;;                                                        gpointer              progress_callback_data,
;;;                                                        GAsyncReadyCallback   callback,
;;;                                                        gpointer              user_data);
;;;   gboolean            (* copy_finish)                 (GFile                *file,
;;;                                                        GAsyncResult         *res,
;;;                                                        GError              **error);
;;;
;;;   gboolean            (* move)                        (GFile                *source,
;;;                                                        GFile                *destination,
;;;                                                        GFileCopyFlags        flags,
;;;                                                        GCancellable         *cancellable,
;;;                                                        GFileProgressCallback progress_callback,
;;;                                                        gpointer              progress_callback_data,
;;;                                                        GError              **error);
;;;   void                (* _move_async)                 (void);
;;;   void                (* _move_finish)                (void);
;;;
;;;   void                (* mount_mountable)             (GFile                *file,
;;;                                                        GMountMountFlags      flags,
;;;                                                        GMountOperation      *mount_operation,
;;;                                                        GCancellable         *cancellable,
;;;                                                        GAsyncReadyCallback   callback,
;;;                                                        gpointer              user_data);
;;;   GFile *             (* mount_mountable_finish)      (GFile                *file,
;;;                                                        GAsyncResult         *result,
;;;                                                        GError              **error);
;;;
;;;   void                (* unmount_mountable)           (GFile                *file,
;;;                                                        GMountUnmountFlags    flags,
;;;                                                        GCancellable         *cancellable,
;;;                                                        GAsyncReadyCallback   callback,
;;;                                                        gpointer              user_data);
;;;   gboolean            (* unmount_mountable_finish)    (GFile                *file,
;;;                                                        GAsyncResult         *result,
;;;                                                        GError              **error);
;;;
;;;   void                (* eject_mountable)             (GFile                *file,
;;;                                                        GMountUnmountFlags    flags,
;;;                                                        GCancellable         *cancellable,
;;;                                                        GAsyncReadyCallback   callback,
;;;                                                        gpointer              user_data);
;;;   gboolean            (* eject_mountable_finish)      (GFile                *file,
;;;                                                        GAsyncResult         *result,
;;;                                                        GError              **error);
;;;
;;;   void                (* mount_enclosing_volume)      (GFile                *location,
;;;                                                        GMountMountFlags      flags,
;;;                                                        GMountOperation      *mount_operation,
;;;                                                        GCancellable         *cancellable,
;;;                                                        GAsyncReadyCallback   callback,
;;;                                                        gpointer              user_data);
;;;   gboolean         (* mount_enclosing_volume_finish)  (GFile                *location,
;;;                                                        GAsyncResult         *result,
;;;                                                        GError              **error);
;;;
;;;   GFileMonitor *      (* monitor_dir)                 (GFile                *file,
;;;                                                        GFileMonitorFlags     flags,
;;;                                                        GCancellable         *cancellable,
;;;                                                        GError              **error);
;;;   GFileMonitor *      (* monitor_file)                (GFile                *file,
;;;                                                        GFileMonitorFlags     flags,
;;;                                                        GCancellable         *cancellable,
;;;                                                        GError              **error);
;;;
;;;   GFileIOStream *     (* open_readwrite)              (GFile                *file,
;;;                                                        GCancellable         *cancellable,
;;;                                                        GError              **error);
;;;   void                (* open_readwrite_async)        (GFile                *file,
;;;                                                        int                   io_priority,
;;;                                                        GCancellable         *cancellable,
;;;                                                        GAsyncReadyCallback   callback,
;;;                                                        gpointer              user_data);
;;;   GFileIOStream *     (* open_readwrite_finish)       (GFile                *file,
;;;                                                        GAsyncResult         *res,
;;;                                                        GError              **error);
;;;   GFileIOStream *     (* create_readwrite)            (GFile                *file,
;;;                                GFileCreateFlags      flags,
;;;                                                        GCancellable         *cancellable,
;;;                                                        GError              **error);
;;;   void                (* create_readwrite_async)      (GFile                *file,
;;;                                GFileCreateFlags      flags,
;;;                                                        int                   io_priority,
;;;                                                        GCancellable         *cancellable,
;;;                                                        GAsyncReadyCallback   callback,
;;;                                                        gpointer              user_data);
;;;   GFileIOStream *     (* create_readwrite_finish)      (GFile                *file,
;;;                                                        GAsyncResult         *res,
;;;                                                        GError              **error);
;;;   GFileIOStream *     (* replace_readwrite)           (GFile                *file,
;;;                                                        const char           *etag,
;;;                                                        gboolean              make_backup,
;;;                                                        GFileCreateFlags      flags,
;;;                                                        GCancellable         *cancellable,
;;;                                                        GError              **error);
;;;   void                (* replace_readwrite_async)     (GFile                *file,
;;;                                                        const char           *etag,
;;;                                                        gboolean              make_backup,
;;;                                                        GFileCreateFlags      flags,
;;;                                                        int                   io_priority,
;;;                                                        GCancellable         *cancellable,
;;;                                                        GAsyncReadyCallback   callback,
;;;                                                        gpointer              user_data);
;;;   GFileIOStream *     (* replace_readwrite_finish)    (GFile                *file,
;;;                                                        GAsyncResult         *res,
;;;                                                        GError              **error);
;;;
;;;   void                (* start_mountable)             (GFile                *file,
;;;                                                        GDriveStartFlags      flags,
;;;                                                        GMountOperation      *start_operation,
;;;                                                        GCancellable         *cancellable,
;;;                                                        GAsyncReadyCallback   callback,
;;;                                                        gpointer              user_data);
;;;   gboolean            (* start_mountable_finish)      (GFile                *file,
;;;                                                        GAsyncResult         *result,
;;;                                                        GError              **error);
;;;
;;;   void                (* stop_mountable)              (GFile                *file,
;;;                                                        GMountUnmountFlags    flags,
;;;                                                        GMountOperation      *mount_operation,
;;;                                                        GCancellable         *cancellable,
;;;                                                        GAsyncReadyCallback   callback,
;;;                                                        gpointer              user_data);
;;;   gboolean            (* stop_mountable_finish)       (GFile                *file,
;;;                                                        GAsyncResult         *result,
;;;                                                        GError              **error);
;;;
;;;   gboolean            supports_thread_contexts;
;;;
;;;   void                (* unmount_mountable_with_operation) (GFile           *file,
;;;                                                        GMountUnmountFlags    flags,
;;;                                                        GMountOperation      *mount_operation,
;;;                                                        GCancellable         *cancellable,
;;;                                                        GAsyncReadyCallback   callback,
;;;                                                        gpointer              user_data);
;;;   gboolean            (* unmount_mountable_with_operation_finish) (GFile    *file,
;;;                                                        GAsyncResult         *result,
;;;                                                        GError              **error);
;;;
;;;   void                (* eject_mountable_with_operation) (GFile             *file,
;;;                                                        GMountUnmountFlags    flags,
;;;                                                        GMountOperation      *mount_operation,
;;;                                                        GCancellable         *cancellable,
;;;                                                        GAsyncReadyCallback   callback,
;;;                                                        gpointer              user_data);
;;;   gboolean            (* eject_mountable_with_operation_finish) (GFile      *file,
;;;                                                        GAsyncResult         *result,
;;;                                                        GError              **error);
;;;
;;;   void                (* poll_mountable)              (GFile                *file,
;;;                                                        GCancellable         *cancellable,
;;;                                                        GAsyncReadyCallback   callback,
;;;                                                        gpointer              user_data);
;;;   gboolean            (* poll_mountable_finish)       (GFile                *file,
;;;                                                        GAsyncResult         *result,
;;;                                                        GError              **error);
;;; };
;;;
;;; An interface for writing VFS file handles.
;;;
;;; GTypeInterface g_iface;
;;;     The parent interface.
;;;
;;; dup ()
;;;     Duplicates a GFile.
;;;
;;; hash ()
;;;     Creates a hash of a GFile.
;;;
;;; equal ()
;;;     Checks equality of two given GFiles.
;;;
;;; is_native ()
;;;     Checks to see if a file is native to the system.
;;;
;;; has_uri_scheme ()
;;;     Checks to see if a GFile has a given URI scheme.
;;;
;;; get_uri_scheme ()
;;;     Gets the URI scheme for a GFile.
;;;
;;; get_basename ()
;;;     Gets the basename for a given GFile.
;;;
;;; get_path ()
;;;     Gets the current path within a GFile.
;;;
;;; get_uri ()
;;;     Gets a URI for the path within a GFile.
;;;
;;; get_parse_name ()
;;;     Gets the parsed name for the GFile.
;;;
;;; get_parent ()
;;;     Gets the parent directory for the GFile.
;;;
;;; prefix_matches ()
;;;     Checks whether a GFile contains a specified file.
;;;
;;; get_relative_path ()
;;;     Gets the path for a GFile relative to a given path.
;;;
;;; resolve_relative_path ()
;;;     Resolves a relative path for a GFile to an absolute path.
;;;
;;; get_child_for_display_name ()
;;;     Gets the child GFile for a given display name.
;;;
;;; enumerate_children ()
;;;     Gets a GFileEnumerator with the children of a GFile.
;;;
;;; enumerate_children_async ()
;;;     Asynchronously gets a GFileEnumerator with the children of a GFile.
;;;
;;; enumerate_children_finish ()
;;;     Finishes asynchronously enumerating the children.
;;;
;;; query_info ()
;;;     Gets the GFileInfo for a GFile.
;;;
;;; query_info_async ()
;;;     Asynchronously gets the GFileInfo for a GFile.
;;;
;;; query_info_finish ()
;;;     Finishes an asynchronous query info operation.
;;;
;;; query_filesystem_info ()
;;;     Gets a GFileInfo for the file system GFile is on.
;;;
;;; query_filesystem_info_async ()
;;;     Asynchronously gets a GFileInfo for the file system GFile is on.
;;;
;;; query_filesystem_info_finish ()
;;;     Finishes asynchronously getting the file system info.
;;;
;;; find_enclosing_mount ()
;;;     Gets a GMount for the GFile.
;;;
;;; find_enclosing_mount_async ()
;;;     Asynchronously gets the GMount for a GFile.
;;;
;;; find_enclosing_mount_finish ()
;;;     Finishes asynchronously getting the volume.
;;;
;;; set_display_name ()
;;;     Sets the display name for a GFile.
;;;
;;; set_display_name_async ()
;;;     Asynchronously sets a GFile's display name.
;;;
;;; set_display_name_finish ()
;;;     Finishes asynchronously setting a GFile's display name.
;;;
;;; query_settable_attributes ()
;;;     Returns a list of GFileAttributes that can be set.
;;;
;;; _query_settable_attributes_async ()
;;;     Asynchronously gets a list of GFileAttributes that can be set.
;;;
;;; _query_settable_attributes_finish ()
;;;     Finishes asynchronously querying settable attributes.
;;;
;;; query_writable_namespaces ()
;;;     Returns a list of GFileAttribute namespaces that are writable.
;;;
;;; _query_writable_namespaces_async ()
;;;     Asynchronously gets a list of GFileAttribute namespaces that are writable.
;;;
;;; _query_writable_namespaces_finish ()
;;;     Finishes asynchronously querying the writable namespaces.
;;;
;;; set_attribute ()
;;;     Sets a GFileAttribute.
;;;
;;; set_attributes_from_info ()
;;;     Sets a GFileAttribute with information from a GFileInfo.
;;;
;;; set_attributes_async ()
;;;     Asynchronously sets a file's attributes.
;;;
;;; set_attributes_finish ()
;;;     Finishes setting a file's attributes asynchronously.
;;;
;;; read_fn ()
;;;     Reads a file asynchronously.
;;;
;;; read_async ()
;;;     Asynchronously reads a file.
;;;
;;; read_finish ()
;;;     Finishes asynchronously reading a file.
;;;
;;; append_to ()
;;;     Writes to the end of a file.
;;;
;;; append_to_async ()
;;;     Asynchronously writes to the end of a file.
;;;
;;; append_to_finish ()
;;;     Finishes an asynchronous file append operation.
;;;
;;; create ()
;;;     Creates a new file.
;;;
;;; create_async ()
;;;     Asynchronously creates a file.
;;;
;;; create_finish ()
;;;     Finishes asynchronously creating a file.
;;;
;;; replace ()
;;;     Replaces the contents of a file.
;;;
;;; replace_async ()
;;;     Asynchronously replaces the contents of a file.
;;;
;;; replace_finish ()
;;;     Finishes asynchronously replacing a file.
;;;
;;; delete_file ()
;;;     Deletes a file.
;;;
;;; _delete_file_async ()
;;;     Asynchronously deletes a file.
;;;
;;; _delete_file_finish ()
;;;     Finishes an asynchronous delete.
;;;
;;; trash ()
;;;     Sends a GFile to the Trash location.
;;;
;;; _trash_async ()
;;;     Asynchronously sends a GFile to the Trash location.
;;;
;;; _trash_finish ()
;;;     Finishes an asynchronous file trashing operation.
;;;
;;; make_directory ()
;;;     Makes a directory.
;;;
;;; _make_directory_async ()
;;;     Asynchronously makes a directory.
;;;
;;; _make_directory_finish ()
;;;     Finishes making a directory asynchronously.
;;;
;;; make_symbolic_link ()
;;;     Makes a symbolic link.
;;;
;;; _make_symbolic_link_async ()
;;;     Asynchronously makes a symbolic link
;;;
;;; _make_symbolic_link_finish ()
;;;     Finishes making a symbolic link asynchronously.
;;;
;;; copy ()
;;;     Copies a file.
;;;
;;; copy_async ()
;;;     Asynchronously copies a file.
;;;
;;; copy_finish ()
;;;     Finishes an asynchronous copy operation.
;;;
;;; move ()
;;;     Moves a file.
;;;
;;; _move_async ()
;;;     Asynchronously moves a file.
;;;
;;; _move_finish ()
;;;     Finishes an asynchronous move operation.
;;;
;;; mount_mountable ()
;;;     Mounts a mountable object.
;;;
;;; mount_mountable_finish ()
;;;     Finishes a mounting operation.
;;;
;;; unmount_mountable ()
;;;     Unmounts a mountable object.
;;;
;;; unmount_mountable_finish ()
;;;     Finishes an unmount operation.
;;;
;;; eject_mountable ()
;;;     Ejects a mountable.
;;;
;;; eject_mountable_finish ()
;;;     Finishes an eject operation.
;;;
;;; mount_enclosing_volume ()
;;;     Mounts a specified location.
;;;
;;; mount_enclosing_volume_finish ()
;;;     Finishes mounting a specified location.
;;;
;;; monitor_dir ()
;;;     Creates a GFileMonitor for the location.
;;;
;;; monitor_file ()
;;;     Creates a GFileMonitor for the location.
;;;
;;; open_readwrite ()
;;;     Open file read/write. Since 2.22.
;;;
;;; open_readwrite_async ()
;;;     Asynchronously opens file read/write. Since 2.22.
;;;
;;; open_readwrite_finish ()
;;;     Finishes an asynchronous open read/write. Since 2.22.
;;;
;;; create_readwrite ()
;;;     Creates file read/write. Since 2.22.
;;;
;;; create_readwrite_async ()
;;;     Asynchronously creates file read/write. Since 2.22.
;;;
;;; create_readwrite_finish ()
;;;     Finishes an asynchronous creates read/write. Since 2.22.
;;;
;;; replace_readwrite ()
;;;     Replaces file read/write. Since 2.22.
;;;
;;; replace_readwrite_async ()
;;;     Asynchronously replaces file read/write. Since 2.22.
;;;
;;; replace_readwrite_finish ()
;;;     Finishes an asynchronous replace read/write. Since 2.22.
;;;
;;; start_mountable ()
;;;     Starts a mountable object. Since 2.22.
;;;
;;; start_mountable_finish ()
;;;     Finishes an start operation. Since 2.22.
;;;
;;; stop_mountable ()
;;;     Stops a mountable. Since 2.22.
;;;
;;; stop_mountable_finish ()
;;;     Finishes an stop operation. Since 2.22.
;;;
;;; gboolean supports_thread_contexts;
;;;     a boolean that indicates whether the GFile implementation supports
;;;     thread-default contexts. Since 2.22.
;;;
;;; unmount_mountable_with_operation ()
;;;     Unmounts a mountable object using a GMountOperation. Since 2.22.
;;;
;;; unmount_mountable_with_operation_finish ()
;;;     Finishes an unmount operation using a GMountOperation. Since 2.22.
;;;
;;; eject_mountable_with_operation ()
;;;     Ejects a mountable object using a GMountOperation. Since 2.22.
;;;
;;; eject_mountable_with_operation_finish ()
;;;     Finishes an eject operation using a GMountOperation. Since 2.22.
;;;
;;; poll_mountable ()
;;;     Polls a mountable object for media changes. Since 2.22.
;;;
;;; poll_mountable_finish ()
;;;     Finishes an poll operation for media changes. Since 2.22.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; enum GFileQueryInfoFlags
;;;
;;; typedef enum {
;;;   G_FILE_QUERY_INFO_NONE              = 0,
;;;   G_FILE_QUERY_INFO_NOFOLLOW_SYMLINKS = (1 << 0)   /*< nick=nofollow-symlinks >*/
;;; } GFileQueryInfoFlags;
;;;
;;; Flags used when querying a GFileInfo.
;;;
;;; G_FILE_QUERY_INFO_NONE
;;;     No flags set.
;;;
;;; G_FILE_QUERY_INFO_NOFOLLOW_SYMLINKS
;;;     Don't follow symlinks.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; enum GFileCreateFlags
;;;
;;; typedef enum {
;;;   G_FILE_CREATE_NONE    = 0,
;;;   G_FILE_CREATE_PRIVATE = (1 << 0),
;;;   G_FILE_CREATE_REPLACE_DESTINATION = (1 << 1)
;;; } GFileCreateFlags;
;;;
;;; Flags used when an operation may create a file.
;;;
;;; G_FILE_CREATE_NONE
;;;     No flags set.
;;;
;;; G_FILE_CREATE_PRIVATE
;;;     Create a file that can only be accessed by the current user.
;;;
;;; G_FILE_CREATE_REPLACE_DESTINATION
;;;     Replace the destination as if it didn't exist before. Don't try to keep
;;;     any old permissions, replace instead of following links. This is
;;;     generally useful if you're doing a "copy over" rather than a "save new
;;;     version of" replace operation. You can think of it as "unlink
;;;     destination" before writing to it, although the implementation may not
;;;     be exactly like that. Since 2.20
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; enum GFileCopyFlags
;;;
;;; typedef enum {
;;;   G_FILE_COPY_NONE                 = 0,          /*< nick=none >*/
;;;   G_FILE_COPY_OVERWRITE            = (1 << 0),
;;;   G_FILE_COPY_BACKUP               = (1 << 1),
;;;   G_FILE_COPY_NOFOLLOW_SYMLINKS    = (1 << 2),
;;;   G_FILE_COPY_ALL_METADATA         = (1 << 3),
;;;   G_FILE_COPY_NO_FALLBACK_FOR_MOVE = (1 << 4),
;;;   G_FILE_COPY_TARGET_DEFAULT_PERMS = (1 << 5)
;;; } GFileCopyFlags;
;;;
;;; Flags used when copying or moving files.
;;;
;;; G_FILE_COPY_NONE
;;;     No flags set.
;;;
;;; G_FILE_COPY_OVERWRITE
;;;     Overwrite any existing files
;;;
;;; G_FILE_COPY_BACKUP
;;;     Make a backup of any existing files.
;;;
;;; G_FILE_COPY_NOFOLLOW_SYMLINKS
;;;     Don't follow symlinks.
;;;
;;; G_FILE_COPY_ALL_METADATA
;;;     Copy all file metadata instead of just default set used for copy (see
;;;     GFileInfo).
;;;
;;; G_FILE_COPY_NO_FALLBACK_FOR_MOVE
;;;     Don't use copy and delete fallback if native move not supported.
;;;
;;; G_FILE_COPY_TARGET_DEFAULT_PERMS
;;;     Leaves target file with default perms, instead of setting the source
;;;     file perms.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; enum GFileMonitorFlags
;;;
;;; typedef enum {
;;;   G_FILE_MONITOR_NONE         = 0,
;;;   G_FILE_MONITOR_WATCH_MOUNTS = (1 << 0),
;;;   G_FILE_MONITOR_SEND_MOVED   = (1 << 1)
;;; } GFileMonitorFlags;
;;;
;;; Flags used to set what a GFileMonitor will watch for.
;;;
;;; G_FILE_MONITOR_NONE
;;;     No flags set.
;;;
;;; G_FILE_MONITOR_WATCH_MOUNTS
;;;     Watch for mount events.
;;;
;;; G_FILE_MONITOR_SEND_MOVED
;;;     Pair DELETED and CREATED events caused by file renames (moves) and send
;;;     a single G_FILE_MONITOR_EVENT_MOVED event instead (NB: not supported on
;;;     all backends; the default behaviour -without specifying this flag- is to
;;;     send single DELETED and CREATED events).
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; enum GFilesystemPreviewType
;;; ----------------------------------------------------------------------------

(define-g-enum "GFileSystemPreviewType" g-file-system-preview-type
  (:export t
   :type-initializer "g_file_system_preview_type_get_type")
  (:if-always 0)
  (:if-local 1)
  (:never 2))

#+cl-cffi-gtk-documentation
(setf (gethash 'g-file-system-preview-type atdoc:*symbol-name-alias*) "Enum"
      (gethash 'g-file-system-preview-type atdoc:*external-symbols*)
 "@version{2013-7-12}
  @begin{short}
    Indicates a hint from the file system whether files should be previewed in a
    file manager. Returned as the value of the key
    @code{G_FILE_ATTRIBUTE_FILESYSTEM_USE_PREVIEW}.
  @end{short}
  @begin{pre}
(define-g-enum \"GFileSystemPreviewType\" g-file-system-preview-type
  (:export t
   :type-initializer \"g_file_system_preview_type_get_type\")
  (:if-always 0)
  (:if-local 1)
  (:never 2))
  @end{pre}
  @begin[code]{table}
    @entry[:if-always]{Only preview files if user has explicitly requested it.}
    @entry[:if-local]{Preview files if user has requested preview of
      \"local\" files.}
    @entry[:never]{Never preview files.}
  @end{table}")

;;; ----------------------------------------------------------------------------
;;; GFileProgressCallback ()
;;;
;;; void                (*GFileProgressCallback)            (goffset current_num_bytes,
;;;                                                          goffset total_num_bytes,
;;;                                                          gpointer user_data);
;;;
;;; When doing file operations that may take a while, such as moving a file or copying a file, a progress callback is used to pass how far along that operation is to the application.
;;;
;;; current_num_bytes :
;;;     the current number of bytes in the operation.
;;;
;;; total_num_bytes :
;;;     the total number of bytes in the operation.
;;;
;;; user_data :
;;;     user data passed to the callback.
;;; GFileReadMoreCallback ()
;;;
;;; gboolean            (*GFileReadMoreCallback)            (const char *file_contents,
;;;                                                          goffset file_size,
;;;                                                          gpointer callback_data);
;;;
;;; When loading the partial contents of a file with g_file_load_partial_contents_async(), it may become necessary to determine if any more data from the file should be loaded. A GFileReadMoreCallback function facilitates this by returning TRUE if more data should be read, or FALSE otherwise.
;;;
;;; file_contents :
;;;     the data as currently read.
;;;
;;; file_size :
;;;     the size of the data currently read.
;;;
;;; callback_data :
;;;     data passed to the callback.
;;;
;;; Returns :
;;;     TRUE if more data should be read back. FALSE otherwise.
;;; g_file_new_for_path ()
;;;
;;; GFile *             g_file_new_for_path                 (const char *path);
;;;
;;; Constructs a GFile for a given path. This operation never fails, but the returned object might not support any I/O operation if path is malformed.
;;;
;;; path :
;;;     a string containing a relative or absolute path. The string must be encoded in the glib filename encoding.
;;;
;;; Returns :
;;;     a new GFile for the given path. Free the returned object with g_object_unref(). [transfer full]
;;; g_file_new_for_uri ()
;;;
;;; GFile *             g_file_new_for_uri                  (const char *uri);
;;;
;;; Constructs a GFile for a given URI. This operation never fails, but the returned object might not support any I/O operation if uri is malformed or if the uri type is not supported.
;;;
;;; uri :
;;;     a UTF8 string containing a URI.
;;;
;;; Returns :
;;;     a new GFile for the given uri. Free the returned object with g_object_unref(). [transfer full]
;;; g_file_new_for_commandline_arg ()
;;;
;;; GFile *             g_file_new_for_commandline_arg      (const char *arg);
;;;
;;; Creates a GFile with the given argument from the command line. The value of arg can be either a URI, an absolute path or a relative path resolved relative to the current working directory. This operation never fails, but the returned object might not support any I/O operation if arg points to a malformed path.
;;;
;;; arg :
;;;     a command line string.
;;;
;;; Returns :
;;;     a new GFile. Free the returned object with g_object_unref(). [transfer full]
;;; g_file_new_tmp ()
;;;
;;; GFile *             g_file_new_tmp                      (const char *tmpl,
;;;                                                          GFileIOStream **iostream,
;;;                                                          GError **error);
;;;
;;; Opens a file in the preferred directory for temporary files (as returned by g_get_tmp_dir()) and returns a GFile and GFileIOStream pointing to it.
;;;
;;; tmpl should be a string in the GLib file name encoding containing a sequence of six 'X' characters, and containing no directory components. If it is NULL, a default template is used.
;;;
;;; Unlike the other GFile constructors, this will return NULL if a temporary file could not be created.
;;;
;;; tmpl :
;;;     Template for the file name, as in g_file_open_tmp(), or NULL for a default template. [type filename][allow-none]
;;;
;;; iostream :
;;;     on return, a GFileIOStream for the created file. [out]
;;;
;;; error :
;;;     a GError, or NULL
;;;
;;; Returns :
;;;     a new GFile. Free the returned object with g_object_unref(). [transfer full]
;;;
;;; Since 2.32
;;; g_file_parse_name ()
;;;
;;; GFile *             g_file_parse_name                   (const char *parse_name);
;;;
;;; Constructs a GFile with the given parse_name (i.e. something given by g_file_get_parse_name()). This operation never fails, but the returned object might not support any I/O operation if the parse_name cannot be parsed.
;;;
;;; parse_name :
;;;     a file name or path to be parsed.
;;;
;;; Returns :
;;;     a new GFile. [transfer full]
;;; g_file_dup ()
;;;
;;; GFile *             g_file_dup                          (GFile *file);
;;;
;;; Duplicates a GFile handle. This operation does not duplicate the actual file or directory represented by the GFile; see g_file_copy() if attempting to copy a file.
;;;
;;; This call does no blocking i/o.
;;;
;;; file :
;;;     input GFile.
;;;
;;; Returns :
;;;     a new GFile that is a duplicate of the given GFile. [transfer full]
;;; g_file_hash ()
;;;
;;; guint               g_file_hash                         (gconstpointer file);
;;;
;;; Creates a hash value for a GFile.
;;;
;;; This call does no blocking i/o.
;;;
;;; Virtual: hash
;;;
;;; file :
;;;     gconstpointer to a GFile. [type GFile]
;;;
;;; Returns :
;;;     0 if file is not a valid GFile, otherwise an integer that can be used as hash value for the GFile. This function is intended for easily hashing a GFile to add to a GHashTable or similar data structure.
;;; g_file_equal ()
;;;
;;; gboolean            g_file_equal                        (GFile *file1,
;;;                                                          GFile *file2);
;;;
;;; Checks equality of two given GFiles. Note that two GFiles that differ can still refer to the same file on the filesystem due to various forms of filename aliasing.
;;;
;;; This call does no blocking i/o.
;;;
;;; file1 :
;;;     the first GFile.
;;;
;;; file2 :
;;;     the second GFile.
;;;
;;; Returns :
;;;     TRUE if file1 and file2 are equal. FALSE if either is not a GFile.
;;; g_file_get_basename ()
;;;
;;; char *              g_file_get_basename                 (GFile *file);
;;;
;;; Gets the base name (the last component of the path) for a given GFile.
;;;
;;; If called for the top level of a system (such as the filesystem root or a uri like sftp://host/) it will return a single directory separator (and on Windows, possibly a drive letter).
;;;
;;; The base name is a byte string (*not* UTF-8). It has no defined encoding or rules other than it may not contain zero bytes. If you want to use filenames in a user interface you should use the display name that you can get by requesting the G_FILE_ATTRIBUTE_STANDARD_DISPLAY_NAME attribute with g_file_query_info().
;;;
;;; This call does no blocking i/o.
;;;
;;; file :
;;;     input GFile.
;;;
;;; Returns :
;;;     string containing the GFile's base name, or NULL if given GFile is invalid. The returned string should be freed with g_free() when no longer needed.
;;; g_file_get_path ()
;;;
;;; char *              g_file_get_path                     (GFile *file);
;;;
;;; Gets the local pathname for GFile, if one exists.
;;;
;;; This call does no blocking i/o.
;;;
;;; file :
;;;     input GFile.
;;;
;;; Returns :
;;;     string containing the GFile's path, or NULL if no such path exists. The returned string should be freed with g_free() when no longer needed.
;;; g_file_get_uri ()
;;;
;;; char *              g_file_get_uri                      (GFile *file);
;;;
;;; Gets the URI for the file.
;;;
;;; This call does no blocking i/o.
;;;
;;; file :
;;;     input GFile.
;;;
;;; Returns :
;;;     a string containing the GFile's URI. The returned string should be freed with g_free() when no longer needed.
;;; g_file_get_parse_name ()
;;;
;;; char *              g_file_get_parse_name               (GFile *file);
;;;
;;; Gets the parse name of the file. A parse name is a UTF-8 string that describes the file such that one can get the GFile back using g_file_parse_name().
;;;
;;; This is generally used to show the GFile as a nice full-pathname kind of string in a user interface, like in a location entry.
;;;
;;; For local files with names that can safely be converted to UTF8 the pathname is used, otherwise the IRI is used (a form of URI that allows UTF8 characters unescaped).
;;;
;;; This call does no blocking i/o.
;;;
;;; file :
;;;     input GFile.
;;;
;;; Returns :
;;;     a string containing the GFile's parse name. The returned string should be freed with g_free() when no longer needed.
;;; g_file_get_parent ()
;;;
;;; GFile *             g_file_get_parent                   (GFile *file);
;;;
;;; Gets the parent directory for the file. If the file represents the root directory of the file system, then NULL will be returned.
;;;
;;; This call does no blocking i/o.
;;;
;;; file :
;;;     input GFile.
;;;
;;; Returns :
;;;     a GFile structure to the parent of the given GFile or NULL if there is no parent. Free the returned object with g_object_unref(). [transfer full]
;;; g_file_has_parent ()
;;;
;;; gboolean            g_file_has_parent                   (GFile *file,
;;;                                                          GFile *parent);
;;;
;;; Checks if file has a parent, and optionally, if it is parent.
;;;
;;; If parent is NULL then this function returns TRUE if file has any parent at all. If parent is non-NULL then TRUE is only returned if file is a child of parent.
;;;
;;; file :
;;;     input GFile
;;;
;;; parent :
;;;     the parent to check for, or NULL. [allow-none]
;;;
;;; Returns :
;;;     TRUE if file is a child of parent (or any parent in the case that parent is NULL).
;;;
;;; Since 2.24
;;; g_file_get_child ()
;;;
;;; GFile *             g_file_get_child                    (GFile *file,
;;;                                                          const char *name);
;;;
;;; Gets a child of file with basename equal to name.
;;;
;;; Note that the file with that specific name might not exist, but you can still have a GFile that points to it. You can use this for instance to create that file.
;;;
;;; This call does no blocking i/o.
;;;
;;; file :
;;;     input GFile.
;;;
;;; name :
;;;     string containing the child's basename.
;;;
;;; Returns :
;;;     a GFile to a child specified by name. Free the returned object with g_object_unref(). [transfer full]
;;; g_file_get_child_for_display_name ()
;;;
;;; GFile *             g_file_get_child_for_display_name   (GFile *file,
;;;                                                          const char *display_name,
;;;                                                          GError **error);
;;;
;;; Gets the child of file for a given display_name (i.e. a UTF8 version of the name). If this function fails, it returns NULL and error will be set. This is very useful when constructing a GFile for a new file and the user entered the filename in the user interface, for instance when you select a directory and type a filename in the file selector.
;;;
;;; This call does no blocking i/o.
;;;
;;; file :
;;;     input GFile.
;;;
;;; display_name :
;;;     string to a possible child.
;;;
;;; error :
;;;     GError.
;;;
;;; Returns :
;;;     a GFile to the specified child, or NULL if the display name couldn't be converted. Free the returned object with g_object_unref(). [transfer full]
;;; g_file_has_prefix ()
;;;
;;; gboolean            g_file_has_prefix                   (GFile *file,
;;;                                                          GFile *prefix);
;;;
;;; Checks whether file has the prefix specified by prefix. In other word, if the names of initial elements of files pathname match prefix. Only full pathname elements are matched, so a path like /foo is not considered a prefix of /foobar, only of /foo/bar.
;;;
;;; This call does no i/o, as it works purely on names. As such it can sometimes return FALSE even if file is inside a prefix (from a filesystem point of view), because the prefix of file is an alias of prefix.
;;;
;;; Virtual: prefix_matches
;;;
;;; file :
;;;     input GFile.
;;;
;;; prefix :
;;;     input GFile.
;;;
;;; Returns :
;;;     TRUE if the files's parent, grandparent, etc is prefix. FALSE otherwise.
;;; g_file_get_relative_path ()
;;;
;;; char *              g_file_get_relative_path            (GFile *parent,
;;;                                                          GFile *descendant);
;;;
;;; Gets the path for descendant relative to parent.
;;;
;;; This call does no blocking i/o.
;;;
;;; parent :
;;;     input GFile.
;;;
;;; descendant :
;;;     input GFile.
;;;
;;; Returns :
;;;     string with the relative path from descendant to parent, or NULL if descendant doesn't have parent as prefix. The returned string should be freed with g_free() when no longer needed.
;;; g_file_resolve_relative_path ()
;;;
;;; GFile *             g_file_resolve_relative_path        (GFile *file,
;;;                                                          const char *relative_path);
;;;
;;; Resolves a relative path for file to an absolute path.
;;;
;;; This call does no blocking i/o.
;;;
;;; file :
;;;     input GFile.
;;;
;;; relative_path :
;;;     a given relative path string.
;;;
;;; Returns :
;;;     GFile to the resolved path. NULL if relative_path is NULL or if file is invalid. Free the returned object with g_object_unref(). [transfer full]
;;; g_file_is_native ()
;;;
;;; gboolean            g_file_is_native                    (GFile *file);
;;;
;;; Checks to see if a file is native to the platform.
;;;
;;; A native file s one expressed in the platform-native filename format, e.g. "C:\Windows" or "/usr/bin/". This does not mean the file is local, as it might be on a locally mounted remote filesystem.
;;;
;;; On some systems non-native files may be available using the native filesystem via a userspace filesystem (FUSE), in these cases this call will return FALSE, but g_file_get_path() will still return a native path.
;;;
;;; This call does no blocking i/o.
;;;
;;; file :
;;;     input GFile.
;;;
;;; Returns :
;;;     TRUE if file is native.
;;; g_file_has_uri_scheme ()
;;;
;;; gboolean            g_file_has_uri_scheme               (GFile *file,
;;;                                                          const char *uri_scheme);
;;;
;;; Checks to see if a GFile has a given URI scheme.
;;;
;;; This call does no blocking i/o.
;;;
;;; file :
;;;     input GFile.
;;;
;;; uri_scheme :
;;;     a string containing a URI scheme.
;;;
;;; Returns :
;;;     TRUE if GFile's backend supports the given URI scheme, FALSE if URI scheme is NULL, not supported, or GFile is invalid.
;;; g_file_get_uri_scheme ()
;;;
;;; char *              g_file_get_uri_scheme               (GFile *file);
;;;
;;; Gets the URI scheme for a GFile. RFC 3986 decodes the scheme as:
;;;
;;; URI = scheme ":" hier-part [ "?" query ] [ "#" fragment ]
;;;
;;; Common schemes include "file", "http", "ftp", etc.
;;;
;;; This call does no blocking i/o.
;;;
;;; file :
;;;     input GFile.
;;;
;;; Returns :
;;;     a string containing the URI scheme for the given GFile. The returned string should be freed with g_free() when no longer needed.
;;; g_file_read ()
;;;
;;; GFileInputStream *  g_file_read                         (GFile *file,
;;;                                                          GCancellable *cancellable,
;;;                                                          GError **error);
;;;
;;; Opens a file for reading. The result is a GFileInputStream that can be used to read the contents of the file.
;;;
;;; If cancellable is not NULL, then the operation can be cancelled by triggering the cancellable object from another thread. If the operation was cancelled, the error G_IO_ERROR_CANCELLED will be returned.
;;;
;;; If the file does not exist, the G_IO_ERROR_NOT_FOUND error will be returned. If the file is a directory, the G_IO_ERROR_IS_DIRECTORY error will be returned. Other errors are possible too, and depend on what kind of filesystem the file is on.
;;;
;;; Virtual: read_fn
;;;
;;; file :
;;;     GFile to read.
;;;
;;; cancellable :
;;;     a GCancellable. [allow-none]
;;;
;;; error :
;;;     a GError, or NULL
;;;
;;; Returns :
;;;     GFileInputStream or NULL on error. Free the returned object with g_object_unref(). [transfer full]
;;; g_file_read_async ()
;;;
;;; void                g_file_read_async                   (GFile *file,
;;;                                                          int io_priority,
;;;                                                          GCancellable *cancellable,
;;;                                                          GAsyncReadyCallback callback,
;;;                                                          gpointer user_data);
;;;
;;; Asynchronously opens file for reading.
;;;
;;; For more details, see g_file_read() which is the synchronous version of this call.
;;;
;;; When the operation is finished, callback will be called. You can then call g_file_read_finish() to get the result of the operation.
;;;
;;; file :
;;;     input GFile
;;;
;;; io_priority :
;;;     the I/O priority of the request.
;;;
;;; cancellable :
;;;     optional GCancellable object, NULL to ignore. [allow-none]
;;;
;;; callback :
;;;     a GAsyncReadyCallback to call when the request is satisfied. [scope async]
;;;
;;; user_data :
;;;     the data to pass to callback function. [closure]
;;; g_file_read_finish ()
;;;
;;; GFileInputStream *  g_file_read_finish                  (GFile *file,
;;;                                                          GAsyncResult *res,
;;;                                                          GError **error);
;;;
;;; Finishes an asynchronous file read operation started with g_file_read_async().
;;;
;;; file :
;;;     input GFile.
;;;
;;; res :
;;;     a GAsyncResult.
;;;
;;; error :
;;;     a GError, or NULL
;;;
;;; Returns :
;;;     a GFileInputStream or NULL on error. Free the returned object with g_object_unref(). [transfer full]
;;; g_file_append_to ()
;;;
;;; GFileOutputStream * g_file_append_to                    (GFile *file,
;;;                                                          GFileCreateFlags flags,
;;;                                                          GCancellable *cancellable,
;;;                                                          GError **error);
;;;
;;; Gets an output stream for appending data to the file. If the file doesn't already exist it is created.
;;;
;;; By default files created are generally readable by everyone, but if you pass G_FILE_CREATE_PRIVATE in flags the file will be made readable only to the current user, to the level that is supported on the target filesystem.
;;;
;;; If cancellable is not NULL, then the operation can be cancelled by triggering the cancellable object from another thread. If the operation was cancelled, the error G_IO_ERROR_CANCELLED will be returned.
;;;
;;; Some file systems don't allow all file names, and may return an G_IO_ERROR_INVALID_FILENAME error. If the file is a directory the G_IO_ERROR_IS_DIRECTORY error will be returned. Other errors are possible too, and depend on what kind of filesystem the file is on.
;;;
;;; file :
;;;     input GFile.
;;;
;;; flags :
;;;     a set of GFileCreateFlags.
;;;
;;; cancellable :
;;;     optional GCancellable object, NULL to ignore. [allow-none]
;;;
;;; error :
;;;     a GError, or NULL
;;;
;;; Returns :
;;;     a GFileOutputStream, or NULL on error. Free the returned object with g_object_unref(). [transfer full]
;;; g_file_create ()
;;;
;;; GFileOutputStream * g_file_create                       (GFile *file,
;;;                                                          GFileCreateFlags flags,
;;;                                                          GCancellable *cancellable,
;;;                                                          GError **error);
;;;
;;; Creates a new file and returns an output stream for writing to it. The file must not already exist.
;;;
;;; By default files created are generally readable by everyone, but if you pass G_FILE_CREATE_PRIVATE in flags the file will be made readable only to the current user, to the level that is supported on the target filesystem.
;;;
;;; If cancellable is not NULL, then the operation can be cancelled by triggering the cancellable object from another thread. If the operation was cancelled, the error G_IO_ERROR_CANCELLED will be returned.
;;;
;;; If a file or directory with this name already exists the G_IO_ERROR_EXISTS error will be returned. Some file systems don't allow all file names, and may return an G_IO_ERROR_INVALID_FILENAME error, and if the name is to long G_IO_ERROR_FILENAME_TOO_LONG will be returned. Other errors are possible too, and depend on what kind of filesystem the file is on.
;;;
;;; file :
;;;     input GFile.
;;;
;;; flags :
;;;     a set of GFileCreateFlags.
;;;
;;; cancellable :
;;;     optional GCancellable object, NULL to ignore. [allow-none]
;;;
;;; error :
;;;     a GError, or NULL
;;;
;;; Returns :
;;;     a GFileOutputStream for the newly created file, or NULL on error. Free the returned object with g_object_unref(). [transfer full]
;;; g_file_replace ()
;;;
;;; GFileOutputStream * g_file_replace                      (GFile *file,
;;;                                                          const char *etag,
;;;                                                          gboolean make_backup,
;;;                                                          GFileCreateFlags flags,
;;;                                                          GCancellable *cancellable,
;;;                                                          GError **error);
;;;
;;; Returns an output stream for overwriting the file, possibly creating a backup copy of the file first. If the file doesn't exist, it will be created.
;;;
;;; This will try to replace the file in the safest way possible so that any errors during the writing will not affect an already existing copy of the file. For instance, for local files it may write to a temporary file and then atomically rename over the destination when the stream is closed.
;;;
;;; By default files created are generally readable by everyone, but if you pass G_FILE_CREATE_PRIVATE in flags the file will be made readable only to the current user, to the level that is supported on the target filesystem.
;;;
;;; If cancellable is not NULL, then the operation can be cancelled by triggering the cancellable object from another thread. If the operation was cancelled, the error G_IO_ERROR_CANCELLED will be returned.
;;;
;;; If you pass in a non-NULL etag value, then this value is compared to the current entity tag of the file, and if they differ an G_IO_ERROR_WRONG_ETAG error is returned. This generally means that the file has been changed since you last read it. You can get the new etag from g_file_output_stream_get_etag() after you've finished writing and closed the GFileOutputStream. When you load a new file you can use g_file_input_stream_query_info() to get the etag of the file.
;;;
;;; If make_backup is TRUE, this function will attempt to make a backup of the current file before overwriting it. If this fails a G_IO_ERROR_CANT_CREATE_BACKUP error will be returned. If you want to replace anyway, try again with make_backup set to FALSE.
;;;
;;; If the file is a directory the G_IO_ERROR_IS_DIRECTORY error will be returned, and if the file is some other form of non-regular file then a G_IO_ERROR_NOT_REGULAR_FILE error will be returned. Some file systems don't allow all file names, and may return an G_IO_ERROR_INVALID_FILENAME error, and if the name is to long G_IO_ERROR_FILENAME_TOO_LONG will be returned. Other errors are possible too, and depend on what kind of filesystem the file is on.
;;;
;;; file :
;;;     input GFile.
;;;
;;; etag :
;;;     an optional entity tag for the current GFile, or NULL to ignore. [allow-none]
;;;
;;; make_backup :
;;;     TRUE if a backup should be created.
;;;
;;; flags :
;;;     a set of GFileCreateFlags.
;;;
;;; cancellable :
;;;     optional GCancellable object, NULL to ignore. [allow-none]
;;;
;;; error :
;;;     a GError, or NULL
;;;
;;; Returns :
;;;     a GFileOutputStream or NULL on error. Free the returned object with g_object_unref(). [transfer full]
;;; g_file_append_to_async ()
;;;
;;; void                g_file_append_to_async              (GFile *file,
;;;                                                          GFileCreateFlags flags,
;;;                                                          int io_priority,
;;;                                                          GCancellable *cancellable,
;;;                                                          GAsyncReadyCallback callback,
;;;                                                          gpointer user_data);
;;;
;;; Asynchronously opens file for appending.
;;;
;;; For more details, see g_file_append_to() which is the synchronous version of this call.
;;;
;;; When the operation is finished, callback will be called. You can then call g_file_append_to_finish() to get the result of the operation.
;;;
;;; file :
;;;     input GFile.
;;;
;;; flags :
;;;     a set of GFileCreateFlags.
;;;
;;; io_priority :
;;;     the I/O priority of the request.
;;;
;;; cancellable :
;;;     optional GCancellable object, NULL to ignore. [allow-none]
;;;
;;; callback :
;;;     a GAsyncReadyCallback to call when the request is satisfied. [scope async]
;;;
;;; user_data :
;;;     the data to pass to callback function. [closure]
;;; g_file_append_to_finish ()
;;;
;;; GFileOutputStream * g_file_append_to_finish             (GFile *file,
;;;                                                          GAsyncResult *res,
;;;                                                          GError **error);
;;;
;;; Finishes an asynchronous file append operation started with g_file_append_to_async().
;;;
;;; file :
;;;     input GFile.
;;;
;;; res :
;;;     GAsyncResult
;;;
;;; error :
;;;     a GError, or NULL
;;;
;;; Returns :
;;;     a valid GFileOutputStream or NULL on error. Free the returned object with g_object_unref(). [transfer full]
;;; g_file_create_async ()
;;;
;;; void                g_file_create_async                 (GFile *file,
;;;                                                          GFileCreateFlags flags,
;;;                                                          int io_priority,
;;;                                                          GCancellable *cancellable,
;;;                                                          GAsyncReadyCallback callback,
;;;                                                          gpointer user_data);
;;;
;;; Asynchronously creates a new file and returns an output stream for writing to it. The file must not already exist.
;;;
;;; For more details, see g_file_create() which is the synchronous version of this call.
;;;
;;; When the operation is finished, callback will be called. You can then call g_file_create_finish() to get the result of the operation.
;;;
;;; file :
;;;     input GFile.
;;;
;;; flags :
;;;     a set of GFileCreateFlags.
;;;
;;; io_priority :
;;;     the I/O priority of the request.
;;;
;;; cancellable :
;;;     optional GCancellable object, NULL to ignore. [allow-none]
;;;
;;; callback :
;;;     a GAsyncReadyCallback to call when the request is satisfied. [scope async]
;;;
;;; user_data :
;;;     the data to pass to callback function. [closure]
;;; g_file_create_finish ()
;;;
;;; GFileOutputStream * g_file_create_finish                (GFile *file,
;;;                                                          GAsyncResult *res,
;;;                                                          GError **error);
;;;
;;; Finishes an asynchronous file create operation started with g_file_create_async().
;;;
;;; file :
;;;     input GFile.
;;;
;;; res :
;;;     a GAsyncResult.
;;;
;;; error :
;;;     a GError, or NULL
;;;
;;; Returns :
;;;     a GFileOutputStream or NULL on error. Free the returned object with g_object_unref(). [transfer full]
;;; g_file_replace_async ()
;;;
;;; void                g_file_replace_async                (GFile *file,
;;;                                                          const char *etag,
;;;                                                          gboolean make_backup,
;;;                                                          GFileCreateFlags flags,
;;;                                                          int io_priority,
;;;                                                          GCancellable *cancellable,
;;;                                                          GAsyncReadyCallback callback,
;;;                                                          gpointer user_data);
;;;
;;; Asynchronously overwrites the file, replacing the contents, possibly creating a backup copy of the file first.
;;;
;;; For more details, see g_file_replace() which is the synchronous version of this call.
;;;
;;; When the operation is finished, callback will be called. You can then call g_file_replace_finish() to get the result of the operation.
;;;
;;; file :
;;;     input GFile.
;;;
;;; etag :
;;;     an entity tag for the current GFile, or NULL to ignore. [allow-none]
;;;
;;; make_backup :
;;;     TRUE if a backup should be created.
;;;
;;; flags :
;;;     a set of GFileCreateFlags.
;;;
;;; io_priority :
;;;     the I/O priority of the request.
;;;
;;; cancellable :
;;;     optional GCancellable object, NULL to ignore. [allow-none]
;;;
;;; callback :
;;;     a GAsyncReadyCallback to call when the request is satisfied. [scope async]
;;;
;;; user_data :
;;;     the data to pass to callback function. [closure]
;;; g_file_replace_finish ()
;;;
;;; GFileOutputStream * g_file_replace_finish               (GFile *file,
;;;                                                          GAsyncResult *res,
;;;                                                          GError **error);
;;;
;;; Finishes an asynchronous file replace operation started with g_file_replace_async().
;;;
;;; file :
;;;     input GFile.
;;;
;;; res :
;;;     a GAsyncResult.
;;;
;;; error :
;;;     a GError, or NULL
;;;
;;; Returns :
;;;     a GFileOutputStream, or NULL on error. Free the returned object with g_object_unref(). [transfer full]
;;; g_file_query_info ()
;;;
;;; GFileInfo *         g_file_query_info                   (GFile *file,
;;;                                                          const char *attributes,
;;;                                                          GFileQueryInfoFlags flags,
;;;                                                          GCancellable *cancellable,
;;;                                                          GError **error);
;;;
;;; Gets the requested information about specified file. The result is a GFileInfo object that contains key-value attributes (such as the type or size of the file).
;;;
;;; The attributes value is a string that specifies the file attributes that should be gathered. It is not an error if it's not possible to read a particular requested attribute from a file - it just won't be set. attributes should be a comma-separated list of attributes or attribute wildcards. The wildcard "*" means all attributes, and a wildcard like "standard::*" means all attributes in the standard namespace. An example attribute query be "standard::*,owner::user". The standard attributes are available as defines, like G_FILE_ATTRIBUTE_STANDARD_NAME.
;;;
;;; If cancellable is not NULL, then the operation can be cancelled by triggering the cancellable object from another thread. If the operation was cancelled, the error G_IO_ERROR_CANCELLED will be returned.
;;;
;;; For symlinks, normally the information about the target of the symlink is returned, rather than information about the symlink itself. However if you pass G_FILE_QUERY_INFO_NOFOLLOW_SYMLINKS in flags the information about the symlink itself will be returned. Also, for symlinks that point to non-existing files the information about the symlink itself will be returned.
;;;
;;; If the file does not exist, the G_IO_ERROR_NOT_FOUND error will be returned. Other errors are possible too, and depend on what kind of filesystem the file is on.
;;;
;;; file :
;;;     input GFile.
;;;
;;; attributes :
;;;     an attribute query string.
;;;
;;; flags :
;;;     a set of GFileQueryInfoFlags.
;;;
;;; cancellable :
;;;     optional GCancellable object, NULL to ignore. [allow-none]
;;;
;;; error :
;;;     a GError.
;;;
;;; Returns :
;;;     a GFileInfo for the given file, or NULL on error. Free the returned object with g_object_unref(). [transfer full]
;;; g_file_query_info_async ()
;;;
;;; void                g_file_query_info_async             (GFile *file,
;;;                                                          const char *attributes,
;;;                                                          GFileQueryInfoFlags flags,
;;;                                                          int io_priority,
;;;                                                          GCancellable *cancellable,
;;;                                                          GAsyncReadyCallback callback,
;;;                                                          gpointer user_data);
;;;
;;; Asynchronously gets the requested information about specified file. The result is a GFileInfo object that contains key-value attributes (such as type or size for the file).
;;;
;;; For more details, see g_file_query_info() which is the synchronous version of this call.
;;;
;;; When the operation is finished, callback will be called. You can then call g_file_query_info_finish() to get the result of the operation.
;;;
;;; file :
;;;     input GFile.
;;;
;;; attributes :
;;;     an attribute query string.
;;;
;;; flags :
;;;     a set of GFileQueryInfoFlags.
;;;
;;; io_priority :
;;;     the I/O priority of the request.
;;;
;;; cancellable :
;;;     optional GCancellable object, NULL to ignore. [allow-none]
;;;
;;; callback :
;;;     a GAsyncReadyCallback to call when the request is satisfied. [scope async]
;;;
;;; user_data :
;;;     the data to pass to callback function. [closure]
;;; g_file_query_info_finish ()
;;;
;;; GFileInfo *         g_file_query_info_finish            (GFile *file,
;;;                                                          GAsyncResult *res,
;;;                                                          GError **error);
;;;
;;; Finishes an asynchronous file info query. See g_file_query_info_async().
;;;
;;; file :
;;;     input GFile.
;;;
;;; res :
;;;     a GAsyncResult.
;;;
;;; error :
;;;     a GError.
;;;
;;; Returns :
;;;     GFileInfo for given file or NULL on error. Free the returned object with g_object_unref(). [transfer full]
;;; g_file_query_exists ()
;;;
;;; gboolean            g_file_query_exists                 (GFile *file,
;;;                                                          GCancellable *cancellable);
;;;
;;; Utility function to check if a particular file exists. This is implemented using g_file_query_info() and as such does blocking I/O.
;;;
;;; Note that in many cases it is racy to first check for file existence and then execute something based on the outcome of that, because the file might have been created or removed in between the operations. The general approach to handling that is to not check, but just do the operation and handle the errors as they come.
;;;
;;; As an example of race-free checking, take the case of reading a file, and if it doesn't exist, creating it. There are two racy versions: read it, and on error create it; and: check if it exists, if not create it. These can both result in two processes creating the file (with perhaps a partially written file as the result). The correct approach is to always try to create the file with g_file_create() which will either atomically create the file or fail with a G_IO_ERROR_EXISTS error.
;;;
;;; However, in many cases an existence check is useful in a user interface, for instance to make a menu item sensitive/insensitive, so that you don't have to fool users that something is possible and then just show and error dialog. If you do this, you should make sure to also handle the errors that can happen due to races when you execute the operation.
;;;
;;; file :
;;;     input GFile.
;;;
;;; cancellable :
;;;     optional GCancellable object, NULL to ignore. [allow-none]
;;;
;;; Returns :
;;;     TRUE if the file exists (and can be detected without error), FALSE otherwise (or if cancelled).
;;; g_file_query_file_type ()
;;;
;;; GFileType           g_file_query_file_type              (GFile *file,
;;;                                                          GFileQueryInfoFlags flags,
;;;                                                          GCancellable *cancellable);
;;;
;;; Utility function to inspect the GFileType of a file. This is implemented using g_file_query_info() and as such does blocking I/O.
;;;
;;; The primary use case of this method is to check if a file is a regular file, directory, or symlink.
;;;
;;; file :
;;;     input GFile.
;;;
;;; flags :
;;;     a set of GFileQueryInfoFlags passed to g_file_query_info().
;;;
;;; cancellable :
;;;     optional GCancellable object, NULL to ignore. [allow-none]
;;;
;;; Returns :
;;;     The GFileType of the file and G_FILE_TYPE_UNKNOWN if the file does not exist
;;;
;;; Since 2.18
;;; g_file_query_filesystem_info ()
;;;
;;; GFileInfo *         g_file_query_filesystem_info        (GFile *file,
;;;                                                          const char *attributes,
;;;                                                          GCancellable *cancellable,
;;;                                                          GError **error);
;;;
;;; Similar to g_file_query_info(), but obtains information about the filesystem the file is on, rather than the file itself. For instance the amount of space available and the type of the filesystem.
;;;
;;; The attributes value is a string that specifies the file attributes that should be gathered. It is not an error if it's not possible to read a particular requested attribute from a file - it just won't be set. attributes should be a comma-separated list of attributes or attribute wildcards. The wildcard "*" means all attributes, and a wildcard like "filesystem::*" means all attributes in the filesystem namespace. The standard namespace for filesystem attributes is "filesystem". Common attributes of interest are G_FILE_ATTRIBUTE_FILESYSTEM_SIZE (the total size of the filesystem in bytes), G_FILE_ATTRIBUTE_FILESYSTEM_FREE (number of bytes available), and G_FILE_ATTRIBUTE_FILESYSTEM_TYPE (type of the filesystem).
;;;
;;; If cancellable is not NULL, then the operation can be cancelled by triggering the cancellable object from another thread. If the operation was cancelled, the error G_IO_ERROR_CANCELLED will be returned.
;;;
;;; If the file does not exist, the G_IO_ERROR_NOT_FOUND error will be returned. Other errors are possible too, and depend on what kind of filesystem the file is on.
;;;
;;; file :
;;;     input GFile.
;;;
;;; attributes :
;;;     an attribute query string.
;;;
;;; cancellable :
;;;     optional GCancellable object, NULL to ignore. [allow-none]
;;;
;;; error :
;;;     a GError.
;;;
;;; Returns :
;;;     a GFileInfo or NULL if there was an error. Free the returned object with g_object_unref(). [transfer full]
;;; g_file_query_filesystem_info_async ()
;;;
;;; void                g_file_query_filesystem_info_async  (GFile *file,
;;;                                                          const char *attributes,
;;;                                                          int io_priority,
;;;                                                          GCancellable *cancellable,
;;;                                                          GAsyncReadyCallback callback,
;;;                                                          gpointer user_data);
;;;
;;; Asynchronously gets the requested information about the filesystem that the specified file is on. The result is a GFileInfo object that contains key-value attributes (such as type or size for the file).
;;;
;;; For more details, see g_file_query_filesystem_info() which is the synchronous version of this call.
;;;
;;; When the operation is finished, callback will be called. You can then call g_file_query_info_finish() to get the result of the operation.
;;;
;;; file :
;;;     input GFile.
;;;
;;; attributes :
;;;     an attribute query string.
;;;
;;; io_priority :
;;;     the I/O priority of the request.
;;;
;;; cancellable :
;;;     optional GCancellable object, NULL to ignore. [allow-none]
;;;
;;; callback :
;;;     a GAsyncReadyCallback to call when the request is satisfied. [scope async]
;;;
;;; user_data :
;;;     the data to pass to callback function. [closure]
;;; g_file_query_filesystem_info_finish ()
;;;
;;; GFileInfo *         g_file_query_filesystem_info_finish (GFile *file,
;;;                                                          GAsyncResult *res,
;;;                                                          GError **error);
;;;
;;; Finishes an asynchronous filesystem info query. See g_file_query_filesystem_info_async().
;;;
;;; file :
;;;     input GFile.
;;;
;;; res :
;;;     a GAsyncResult.
;;;
;;; error :
;;;     a GError.
;;;
;;; Returns :
;;;     GFileInfo for given file or NULL on error. Free the returned object with g_object_unref(). [transfer full]
;;; g_file_query_default_handler ()
;;;
;;; GAppInfo *          g_file_query_default_handler        (GFile *file,
;;;                                                          GCancellable *cancellable,
;;;                                                          GError **error);
;;;
;;; Returns the GAppInfo that is registered as the default application to handle the file specified by file.
;;;
;;; If cancellable is not NULL, then the operation can be cancelled by triggering the cancellable object from another thread. If the operation was cancelled, the error G_IO_ERROR_CANCELLED will be returned.
;;;
;;; file :
;;;     a GFile to open.
;;;
;;; cancellable :
;;;     optional GCancellable object, NULL to ignore.
;;;
;;; error :
;;;     a GError, or NULL
;;;
;;; Returns :
;;;     a GAppInfo if the handle was found, NULL if there were errors. When you are done with it, release it with g_object_unref(). [transfer full]
;;; g_file_find_enclosing_mount ()
;;;
;;; GMount *            g_file_find_enclosing_mount         (GFile *file,
;;;                                                          GCancellable *cancellable,
;;;                                                          GError **error);
;;;
;;; Gets a GMount for the GFile.
;;;
;;; If the GFileIface for file does not have a mount (e.g. possibly a remote share), error will be set to G_IO_ERROR_NOT_FOUND and NULL will be returned.
;;;
;;; If cancellable is not NULL, then the operation can be cancelled by triggering the cancellable object from another thread. If the operation was cancelled, the error G_IO_ERROR_CANCELLED will be returned.
;;;
;;; file :
;;;     input GFile.
;;;
;;; cancellable :
;;;     optional GCancellable object, NULL to ignore. [allow-none]
;;;
;;; error :
;;;     a GError.
;;;
;;; Returns :
;;;     a GMount where the file is located or NULL on error. Free the returned object with g_object_unref(). [transfer full]
;;; g_file_find_enclosing_mount_async ()
;;;
;;; void                g_file_find_enclosing_mount_async   (GFile *file,
;;;                                                          int io_priority,
;;;                                                          GCancellable *cancellable,
;;;                                                          GAsyncReadyCallback callback,
;;;                                                          gpointer user_data);
;;;
;;; Asynchronously gets the mount for the file.
;;;
;;; For more details, see g_file_find_enclosing_mount() which is the synchronous version of this call.
;;;
;;; When the operation is finished, callback will be called. You can then call g_file_find_enclosing_mount_finish() to get the result of the operation.
;;;
;;; file :
;;;     a GFile
;;;
;;; io_priority :
;;;     the I/O priority of the request.
;;;
;;; cancellable :
;;;     optional GCancellable object, NULL to ignore. [allow-none]
;;;
;;; callback :
;;;     a GAsyncReadyCallback to call when the request is satisfied. [scope async]
;;;
;;; user_data :
;;;     the data to pass to callback function. [closure]
;;; g_file_find_enclosing_mount_finish ()
;;;
;;; GMount *            g_file_find_enclosing_mount_finish  (GFile *file,
;;;                                                          GAsyncResult *res,
;;;                                                          GError **error);
;;;
;;; Finishes an asynchronous find mount request. See g_file_find_enclosing_mount_async().
;;;
;;; file :
;;;     a GFile
;;;
;;; res :
;;;     a GAsyncResult
;;;
;;; error :
;;;     a GError
;;;
;;; Returns :
;;;     GMount for given file or NULL on error. Free the returned object with g_object_unref(). [transfer full]
;;; g_file_enumerate_children ()
;;;
;;; GFileEnumerator *   g_file_enumerate_children           (GFile *file,
;;;                                                          const char *attributes,
;;;                                                          GFileQueryInfoFlags flags,
;;;                                                          GCancellable *cancellable,
;;;                                                          GError **error);
;;;
;;; Gets the requested information about the files in a directory. The result is a GFileEnumerator object that will give out GFileInfo objects for all the files in the directory.
;;;
;;; The attributes value is a string that specifies the file attributes that should be gathered. It is not an error if it's not possible to read a particular requested attribute from a file - it just won't be set. attributes should be a comma-separated list of attributes or attribute wildcards. The wildcard "*" means all attributes, and a wildcard like "standard::*" means all attributes in the standard namespace. An example attribute query be "standard::*,owner::user". The standard attributes are available as defines, like G_FILE_ATTRIBUTE_STANDARD_NAME.
;;;
;;; If cancellable is not NULL, then the operation can be cancelled by triggering the cancellable object from another thread. If the operation was cancelled, the error G_IO_ERROR_CANCELLED will be returned.
;;;
;;; If the file does not exist, the G_IO_ERROR_NOT_FOUND error will be returned. If the file is not a directory, the G_FILE_ERROR_NOTDIR error will be returned. Other errors are possible too.
;;;
;;; file :
;;;     input GFile.
;;;
;;; attributes :
;;;     an attribute query string.
;;;
;;; flags :
;;;     a set of GFileQueryInfoFlags.
;;;
;;; cancellable :
;;;     optional GCancellable object, NULL to ignore. [allow-none]
;;;
;;; error :
;;;     GError for error reporting.
;;;
;;; Returns :
;;;     A GFileEnumerator if successful, NULL on error. Free the returned object with g_object_unref(). [transfer full]
;;; g_file_enumerate_children_async ()
;;;
;;; void                g_file_enumerate_children_async     (GFile *file,
;;;                                                          const char *attributes,
;;;                                                          GFileQueryInfoFlags flags,
;;;                                                          int io_priority,
;;;                                                          GCancellable *cancellable,
;;;                                                          GAsyncReadyCallback callback,
;;;                                                          gpointer user_data);
;;;
;;; Asynchronously gets the requested information about the files in a directory. The result is a GFileEnumerator object that will give out GFileInfo objects for all the files in the directory.
;;;
;;; For more details, see g_file_enumerate_children() which is the synchronous version of this call.
;;;
;;; When the operation is finished, callback will be called. You can then call g_file_enumerate_children_finish() to get the result of the operation.
;;;
;;; file :
;;;     input GFile.
;;;
;;; attributes :
;;;     an attribute query string.
;;;
;;; flags :
;;;     a set of GFileQueryInfoFlags.
;;;
;;; io_priority :
;;;     the I/O priority of the request.
;;;
;;; cancellable :
;;;     optional GCancellable object, NULL to ignore. [allow-none]
;;;
;;; callback :
;;;     (scope async) : a GAsyncReadyCallback to call when the request is satisfied
;;;
;;; user_data :
;;;     the data to pass to callback function. [closure]
;;; g_file_enumerate_children_finish ()
;;;
;;; GFileEnumerator *   g_file_enumerate_children_finish    (GFile *file,
;;;                                                          GAsyncResult *res,
;;;                                                          GError **error);
;;;
;;; Finishes an async enumerate children operation. See g_file_enumerate_children_async().
;;;
;;; file :
;;;     input GFile.
;;;
;;; res :
;;;     a GAsyncResult.
;;;
;;; error :
;;;     a GError.
;;;
;;; Returns :
;;;     a GFileEnumerator or NULL if an error occurred. Free the returned object with g_object_unref(). [transfer full]
;;; g_file_set_display_name ()
;;;
;;; GFile *             g_file_set_display_name             (GFile *file,
;;;                                                          const char *display_name,
;;;                                                          GCancellable *cancellable,
;;;                                                          GError **error);
;;;
;;; Renames file to the specified display name.
;;;
;;; The display name is converted from UTF8 to the correct encoding for the target filesystem if possible and the file is renamed to this.
;;;
;;; If you want to implement a rename operation in the user interface the edit name (G_FILE_ATTRIBUTE_STANDARD_EDIT_NAME) should be used as the initial value in the rename widget, and then the result after editing should be passed to g_file_set_display_name().
;;;
;;; On success the resulting converted filename is returned.
;;;
;;; If cancellable is not NULL, then the operation can be cancelled by triggering the cancellable object from another thread. If the operation was cancelled, the error G_IO_ERROR_CANCELLED will be returned.
;;;
;;; file :
;;;     input GFile.
;;;
;;; display_name :
;;;     a string.
;;;
;;; cancellable :
;;;     optional GCancellable object, NULL to ignore. [allow-none]
;;;
;;; error :
;;;     a GError, or NULL
;;;
;;; Returns :
;;;     a GFile specifying what file was renamed to, or NULL if there was an error. Free the returned object with g_object_unref(). [transfer full]
;;; g_file_set_display_name_async ()
;;;
;;; void                g_file_set_display_name_async       (GFile *file,
;;;                                                          const char *display_name,
;;;                                                          int io_priority,
;;;                                                          GCancellable *cancellable,
;;;                                                          GAsyncReadyCallback callback,
;;;                                                          gpointer user_data);
;;;
;;; Asynchronously sets the display name for a given GFile.
;;;
;;; For more details, see g_file_set_display_name() which is the synchronous version of this call.
;;;
;;; When the operation is finished, callback will be called. You can then call g_file_set_display_name_finish() to get the result of the operation.
;;;
;;; file :
;;;     input GFile.
;;;
;;; display_name :
;;;     a string.
;;;
;;; io_priority :
;;;     the I/O priority of the request.
;;;
;;; cancellable :
;;;     optional GCancellable object, NULL to ignore. [allow-none]
;;;
;;; callback :
;;;     a GAsyncReadyCallback to call when the request is satisfied. [scope async]
;;;
;;; user_data :
;;;     the data to pass to callback function. [closure]
;;; g_file_set_display_name_finish ()
;;;
;;; GFile *             g_file_set_display_name_finish      (GFile *file,
;;;                                                          GAsyncResult *res,
;;;                                                          GError **error);
;;;
;;; Finishes setting a display name started with g_file_set_display_name_async().
;;;
;;; file :
;;;     input GFile.
;;;
;;; res :
;;;     a GAsyncResult.
;;;
;;; error :
;;;     a GError, or NULL
;;;
;;; Returns :
;;;     a GFile or NULL on error. Free the returned object with g_object_unref(). [transfer full]
;;; g_file_delete ()
;;;
;;; gboolean            g_file_delete                       (GFile *file,
;;;                                                          GCancellable *cancellable,
;;;                                                          GError **error);
;;;
;;; Deletes a file. If the file is a directory, it will only be deleted if it is empty.
;;;
;;; If cancellable is not NULL, then the operation can be cancelled by triggering the cancellable object from another thread. If the operation was cancelled, the error G_IO_ERROR_CANCELLED will be returned.
;;;
;;; Virtual: delete_file
;;;
;;; file :
;;;     input GFile.
;;;
;;; cancellable :
;;;     optional GCancellable object, NULL to ignore. [allow-none]
;;;
;;; error :
;;;     a GError, or NULL
;;;
;;; Returns :
;;;     TRUE if the file was deleted. FALSE otherwise.
;;; g_file_trash ()
;;;
;;; gboolean            g_file_trash                        (GFile *file,
;;;                                                          GCancellable *cancellable,
;;;                                                          GError **error);
;;;
;;; Sends file to the "Trashcan", if possible. This is similar to deleting it, but the user can recover it before emptying the trashcan. Not all file systems support trashing, so this call can return the G_IO_ERROR_NOT_SUPPORTED error.
;;;
;;; If cancellable is not NULL, then the operation can be cancelled by triggering the cancellable object from another thread. If the operation was cancelled, the error G_IO_ERROR_CANCELLED will be returned.
;;;
;;; file :
;;;     GFile to send to trash.
;;;
;;; cancellable :
;;;     optional GCancellable object, NULL to ignore. [allow-none]
;;;
;;; error :
;;;     a GError, or NULL
;;;
;;; Returns :
;;;     TRUE on successful trash, FALSE otherwise.
;;; g_file_copy ()
;;;
;;; gboolean            g_file_copy                         (GFile *source,
;;;                                                          GFile *destination,
;;;                                                          GFileCopyFlags flags,
;;;                                                          GCancellable *cancellable,
;;;                                                          GFileProgressCallback progress_callback,
;;;                                                          gpointer progress_callback_data,
;;;                                                          GError **error);
;;;
;;; Copies the file source to the location specified by destination. Can not handle recursive copies of directories.
;;;
;;; If the flag G_FILE_COPY_OVERWRITE is specified an already existing destination file is overwritten.
;;;
;;; If the flag G_FILE_COPY_NOFOLLOW_SYMLINKS is specified then symlinks will be copied as symlinks, otherwise the target of the source symlink will be copied.
;;;
;;; If cancellable is not NULL, then the operation can be cancelled by triggering the cancellable object from another thread. If the operation was cancelled, the error G_IO_ERROR_CANCELLED will be returned.
;;;
;;; If progress_callback is not NULL, then the operation can be monitored by setting this to a GFileProgressCallback function. progress_callback_data will be passed to this function. It is guaranteed that this callback will be called after all data has been transferred with the total number of bytes copied during the operation.
;;;
;;; If the source file does not exist then the G_IO_ERROR_NOT_FOUND error is returned, independent on the status of the destination.
;;;
;;; If G_FILE_COPY_OVERWRITE is not specified and the target exists, then the error G_IO_ERROR_EXISTS is returned.
;;;
;;; If trying to overwrite a file over a directory the G_IO_ERROR_IS_DIRECTORY error is returned. If trying to overwrite a directory with a directory the G_IO_ERROR_WOULD_MERGE error is returned.
;;;
;;; If the source is a directory and the target does not exist, or G_FILE_COPY_OVERWRITE is specified and the target is a file, then the G_IO_ERROR_WOULD_RECURSE error is returned.
;;;
;;; If you are interested in copying the GFile object itself (not the on-disk file), see g_file_dup().
;;;
;;; source :
;;;     input GFile.
;;;
;;; destination :
;;;     destination GFile
;;;
;;; flags :
;;;     set of GFileCopyFlags
;;;
;;; cancellable :
;;;     optional GCancellable object, NULL to ignore. [allow-none]
;;;
;;; progress_callback :
;;;     function to callback with progress information, or NULL if progress information is not needed. [allow-none][scope call]
;;;
;;; progress_callback_data :
;;;     user data to pass to progress_callback. [closure]
;;;
;;; error :
;;;     GError to set on error, or NULL
;;;
;;; Returns :
;;;     TRUE on success, FALSE otherwise.
;;; g_file_copy_async ()
;;;
;;; void                g_file_copy_async                   (GFile *source,
;;;                                                          GFile *destination,
;;;                                                          GFileCopyFlags flags,
;;;                                                          int io_priority,
;;;                                                          GCancellable *cancellable,
;;;                                                          GFileProgressCallback progress_callback,
;;;                                                          gpointer progress_callback_data,
;;;                                                          GAsyncReadyCallback callback,
;;;                                                          gpointer user_data);
;;;
;;; Copies the file source to the location specified by destination asynchronously. For details of the behaviour, see g_file_copy().
;;;
;;; If progress_callback is not NULL, then that function that will be called just like in g_file_copy(), however the callback will run in the main loop, not in the thread that is doing the I/O operation.
;;;
;;; When the operation is finished, callback will be called. You can then call g_file_copy_finish() to get the result of the operation.
;;;
;;; source :
;;;     input GFile.
;;;
;;; destination :
;;;     destination GFile
;;;
;;; flags :
;;;     set of GFileCopyFlags
;;;
;;; io_priority :
;;;     the I/O priority of the request
;;;
;;; cancellable :
;;;     optional GCancellable object, NULL to ignore. [allow-none]
;;;
;;; progress_callback :
;;;     function to callback with progress information, or NULL if progress information is not needed. [allow-none]
;;;
;;; progress_callback_data :
;;;     user data to pass to progress_callback. [closure]
;;;
;;; callback :
;;;     a GAsyncReadyCallback to call when the request is satisfied
;;;
;;; user_data :
;;;     the data to pass to callback function
;;; g_file_copy_finish ()
;;;
;;; gboolean            g_file_copy_finish                  (GFile *file,
;;;                                                          GAsyncResult *res,
;;;                                                          GError **error);
;;;
;;; Finishes copying the file started with g_file_copy_async().
;;;
;;; file :
;;;     input GFile.
;;;
;;; res :
;;;     a GAsyncResult.
;;;
;;; error :
;;;     a GError, or NULL
;;;
;;; Returns :
;;;     a TRUE on success, FALSE on error.
;;; g_file_move ()
;;;
;;; gboolean            g_file_move                         (GFile *source,
;;;                                                          GFile *destination,
;;;                                                          GFileCopyFlags flags,
;;;                                                          GCancellable *cancellable,
;;;                                                          GFileProgressCallback progress_callback,
;;;                                                          gpointer progress_callback_data,
;;;                                                          GError **error);
;;;
;;; Tries to move the file or directory source to the location specified by destination. If native move operations are supported then this is used, otherwise a copy + delete fallback is used. The native implementation may support moving directories (for instance on moves inside the same filesystem), but the fallback code does not.
;;;
;;; If the flag G_FILE_COPY_OVERWRITE is specified an already existing destination file is overwritten.
;;;
;;; If the flag G_FILE_COPY_NOFOLLOW_SYMLINKS is specified then symlinks will be copied as symlinks, otherwise the target of the source symlink will be copied.
;;;
;;; If cancellable is not NULL, then the operation can be cancelled by triggering the cancellable object from another thread. If the operation was cancelled, the error G_IO_ERROR_CANCELLED will be returned.
;;;
;;; If progress_callback is not NULL, then the operation can be monitored by setting this to a GFileProgressCallback function. progress_callback_data will be passed to this function. It is guaranteed that this callback will be called after all data has been transferred with the total number of bytes copied during the operation.
;;;
;;; If the source file does not exist then the G_IO_ERROR_NOT_FOUND error is returned, independent on the status of the destination.
;;;
;;; If G_FILE_COPY_OVERWRITE is not specified and the target exists, then the error G_IO_ERROR_EXISTS is returned.
;;;
;;; If trying to overwrite a file over a directory the G_IO_ERROR_IS_DIRECTORY error is returned. If trying to overwrite a directory with a directory the G_IO_ERROR_WOULD_MERGE error is returned.
;;;
;;; If the source is a directory and the target does not exist, or G_FILE_COPY_OVERWRITE is specified and the target is a file, then the G_IO_ERROR_WOULD_RECURSE error may be returned (if the native move operation isn't available).
;;;
;;; source :
;;;     GFile pointing to the source location.
;;;
;;; destination :
;;;     GFile pointing to the destination location.
;;;
;;; flags :
;;;     set of GFileCopyFlags.
;;;
;;; cancellable :
;;;     optional GCancellable object, NULL to ignore. [allow-none]
;;;
;;; progress_callback :
;;;     GFileProgressCallback function for updates. [allow-none][scope call]
;;;
;;; progress_callback_data :
;;;     gpointer to user data for the callback function. [closure]
;;;
;;; error :
;;;     GError for returning error conditions, or NULL
;;;
;;; Returns :
;;;     TRUE on successful move, FALSE otherwise.
;;; g_file_make_directory ()
;;;
;;; gboolean            g_file_make_directory               (GFile *file,
;;;                                                          GCancellable *cancellable,
;;;                                                          GError **error);
;;;
;;; Creates a directory. Note that this will only create a child directory of the immediate parent directory of the path or URI given by the GFile. To recursively create directories, see g_file_make_directory_with_parents(). This function will fail if the parent directory does not exist, setting error to G_IO_ERROR_NOT_FOUND. If the file system doesn't support creating directories, this function will fail, setting error to G_IO_ERROR_NOT_SUPPORTED.
;;;
;;; For a local GFile the newly created directory will have the default (current) ownership and permissions of the current process.
;;;
;;; If cancellable is not NULL, then the operation can be cancelled by triggering the cancellable object from another thread. If the operation was cancelled, the error G_IO_ERROR_CANCELLED will be returned.
;;;
;;; file :
;;;     input GFile.
;;;
;;; cancellable :
;;;     optional GCancellable object, NULL to ignore. [allow-none]
;;;
;;; error :
;;;     a GError, or NULL
;;;
;;; Returns :
;;;     TRUE on successful creation, FALSE otherwise.
;;; g_file_make_directory_with_parents ()
;;;
;;; gboolean            g_file_make_directory_with_parents  (GFile *file,
;;;                                                          GCancellable *cancellable,
;;;                                                          GError **error);
;;;
;;; Creates a directory and any parent directories that may not exist similar to 'mkdir -p'. If the file system does not support creating directories, this function will fail, setting error to G_IO_ERROR_NOT_SUPPORTED. If the directory itself already exists, this function will fail setting error to G_IO_ERROR_EXISTS, unlike the similar g_mkdir_with_parents().
;;;
;;; For a local GFile the newly created directories will have the default (current) ownership and permissions of the current process.
;;;
;;; If cancellable is not NULL, then the operation can be cancelled by triggering the cancellable object from another thread. If the operation was cancelled, the error G_IO_ERROR_CANCELLED will be returned.
;;;
;;; file :
;;;     input GFile.
;;;
;;; cancellable :
;;;     optional GCancellable object, NULL to ignore. [allow-none]
;;;
;;; error :
;;;     a GError, or NULL
;;;
;;; Returns :
;;;     TRUE if all directories have been successfully created, FALSE otherwise.
;;;
;;; Since 2.18
;;; g_file_make_symbolic_link ()
;;;
;;; gboolean            g_file_make_symbolic_link           (GFile *file,
;;;                                                          const char *symlink_value,
;;;                                                          GCancellable *cancellable,
;;;                                                          GError **error);
;;;
;;; Creates a symbolic link named file which contains the string symlink_value.
;;;
;;; If cancellable is not NULL, then the operation can be cancelled by triggering the cancellable object from another thread. If the operation was cancelled, the error G_IO_ERROR_CANCELLED will be returned.
;;;
;;; file :
;;;     a GFile with the name of the symlink to create
;;;
;;; symlink_value :
;;;     a string with the path for the target of the new symlink
;;;
;;; cancellable :
;;;     optional GCancellable object, NULL to ignore. [allow-none]
;;;
;;; error :
;;;     a GError.
;;;
;;; Returns :
;;;     TRUE on the creation of a new symlink, FALSE otherwise.
;;; g_file_query_settable_attributes ()
;;;
;;; GFileAttributeInfoList * g_file_query_settable_attributes
;;;                                                         (GFile *file,
;;;                                                          GCancellable *cancellable,
;;;                                                          GError **error);
;;;
;;; Obtain the list of settable attributes for the file.
;;;
;;; Returns the type and full attribute name of all the attributes that can be set on this file. This doesn't mean setting it will always succeed though, you might get an access failure, or some specific file may not support a specific attribute.
;;;
;;; If cancellable is not NULL, then the operation can be cancelled by triggering the cancellable object from another thread. If the operation was cancelled, the error G_IO_ERROR_CANCELLED will be returned.
;;;
;;; file :
;;;     input GFile.
;;;
;;; cancellable :
;;;     optional GCancellable object, NULL to ignore. [allow-none]
;;;
;;; error :
;;;     a GError, or NULL
;;;
;;; Returns :
;;;     a GFileAttributeInfoList describing the settable attributes. When you are done with it, release it with g_file_attribute_info_list_unref()
;;; g_file_query_writable_namespaces ()
;;;
;;; GFileAttributeInfoList * g_file_query_writable_namespaces
;;;                                                         (GFile *file,
;;;                                                          GCancellable *cancellable,
;;;                                                          GError **error);
;;;
;;; Obtain the list of attribute namespaces where new attributes can be created by a user. An example of this is extended attributes (in the "xattr" namespace).
;;;
;;; If cancellable is not NULL, then the operation can be cancelled by triggering the cancellable object from another thread. If the operation was cancelled, the error G_IO_ERROR_CANCELLED will be returned.
;;;
;;; file :
;;;     input GFile.
;;;
;;; cancellable :
;;;     optional GCancellable object, NULL to ignore. [allow-none]
;;;
;;; error :
;;;     a GError, or NULL
;;;
;;; Returns :
;;;     a GFileAttributeInfoList describing the writable namespaces. When you are done with it, release it with g_file_attribute_info_list_unref()
;;; g_file_set_attribute ()
;;;
;;; gboolean            g_file_set_attribute                (GFile *file,
;;;                                                          const char *attribute,
;;;                                                          GFileAttributeType type,
;;;                                                          gpointer value_p,
;;;                                                          GFileQueryInfoFlags flags,
;;;                                                          GCancellable *cancellable,
;;;                                                          GError **error);
;;;
;;; Sets an attribute in the file with attribute name attribute to value.
;;;
;;; Some attributes can be unset by setting attribute to G_FILE_ATTRIBUTE_TYPE_INVALID and value_p to NULL.
;;;
;;; If cancellable is not NULL, then the operation can be cancelled by triggering the cancellable object from another thread. If the operation was cancelled, the error G_IO_ERROR_CANCELLED will be returned.
;;;
;;; file :
;;;     input GFile.
;;;
;;; attribute :
;;;     a string containing the attribute's name.
;;;
;;; type :
;;;     The type of the attribute
;;;
;;; value_p :
;;;     a pointer to the value (or the pointer itself if the type is a pointer type). [allow-none]
;;;
;;; flags :
;;;     a set of GFileQueryInfoFlags.
;;;
;;; cancellable :
;;;     optional GCancellable object, NULL to ignore. [allow-none]
;;;
;;; error :
;;;     a GError, or NULL
;;;
;;; Returns :
;;;     TRUE if the attribute was set, FALSE otherwise.
;;; g_file_set_attributes_from_info ()
;;;
;;; gboolean            g_file_set_attributes_from_info     (GFile *file,
;;;                                                          GFileInfo *info,
;;;                                                          GFileQueryInfoFlags flags,
;;;                                                          GCancellable *cancellable,
;;;                                                          GError **error);
;;;
;;; Tries to set all attributes in the GFileInfo on the target values, not stopping on the first error.
;;;
;;; If there is any error during this operation then error will be set to the first error. Error on particular fields are flagged by setting the "status" field in the attribute value to G_FILE_ATTRIBUTE_STATUS_ERROR_SETTING, which means you can also detect further errors.
;;;
;;; If cancellable is not NULL, then the operation can be cancelled by triggering the cancellable object from another thread. If the operation was cancelled, the error G_IO_ERROR_CANCELLED will be returned.
;;;
;;; file :
;;;     input GFile.
;;;
;;; info :
;;;     a GFileInfo.
;;;
;;; flags :
;;;     GFileQueryInfoFlags
;;;
;;; cancellable :
;;;     optional GCancellable object, NULL to ignore. [allow-none]
;;;
;;; error :
;;;     a GError, or NULL
;;;
;;; Returns :
;;;     TRUE if there was any error, FALSE otherwise.
;;; g_file_set_attributes_async ()
;;;
;;; void                g_file_set_attributes_async         (GFile *file,
;;;                                                          GFileInfo *info,
;;;                                                          GFileQueryInfoFlags flags,
;;;                                                          int io_priority,
;;;                                                          GCancellable *cancellable,
;;;                                                          GAsyncReadyCallback callback,
;;;                                                          gpointer user_data);
;;;
;;; Asynchronously sets the attributes of file with info.
;;;
;;; For more details, see g_file_set_attributes_from_info() which is the synchronous version of this call.
;;;
;;; When the operation is finished, callback will be called. You can then call g_file_set_attributes_finish() to get the result of the operation.
;;;
;;; file :
;;;     input GFile.
;;;
;;; info :
;;;     a GFileInfo.
;;;
;;; flags :
;;;     a GFileQueryInfoFlags.
;;;
;;; io_priority :
;;;     the I/O priority of the request.
;;;
;;; cancellable :
;;;     optional GCancellable object, NULL to ignore. [allow-none]
;;;
;;; callback :
;;;     a GAsyncReadyCallback. [scope async]
;;;
;;; user_data :
;;;     a gpointer. [closure]
;;; g_file_set_attributes_finish ()
;;;
;;; gboolean            g_file_set_attributes_finish        (GFile *file,
;;;                                                          GAsyncResult *result,
;;;                                                          GFileInfo **info,
;;;                                                          GError **error);
;;;
;;; Finishes setting an attribute started in g_file_set_attributes_async().
;;;
;;; file :
;;;     input GFile.
;;;
;;; result :
;;;     a GAsyncResult.
;;;
;;; info :
;;;     a GFileInfo. [out][transfer full]
;;;
;;; error :
;;;     a GError, or NULL
;;;
;;; Returns :
;;;     TRUE if the attributes were set correctly, FALSE otherwise.
;;; g_file_set_attribute_string ()
;;;
;;; gboolean            g_file_set_attribute_string         (GFile *file,
;;;                                                          const char *attribute,
;;;                                                          const char *value,
;;;                                                          GFileQueryInfoFlags flags,
;;;                                                          GCancellable *cancellable,
;;;                                                          GError **error);
;;;
;;; Sets attribute of type G_FILE_ATTRIBUTE_TYPE_STRING to value. If attribute is of a different type, this operation will fail.
;;;
;;; If cancellable is not NULL, then the operation can be cancelled by triggering the cancellable object from another thread. If the operation was cancelled, the error G_IO_ERROR_CANCELLED will be returned.
;;;
;;; file :
;;;     input GFile.
;;;
;;; attribute :
;;;     a string containing the attribute's name.
;;;
;;; value :
;;;     a string containing the attribute's value.
;;;
;;; flags :
;;;     GFileQueryInfoFlags.
;;;
;;; cancellable :
;;;     optional GCancellable object, NULL to ignore. [allow-none]
;;;
;;; error :
;;;     a GError, or NULL
;;;
;;; Returns :
;;;     TRUE if the attribute was successfully set, FALSE otherwise.
;;; g_file_set_attribute_byte_string ()
;;;
;;; gboolean            g_file_set_attribute_byte_string    (GFile *file,
;;;                                                          const char *attribute,
;;;                                                          const char *value,
;;;                                                          GFileQueryInfoFlags flags,
;;;                                                          GCancellable *cancellable,
;;;                                                          GError **error);
;;;
;;; Sets attribute of type G_FILE_ATTRIBUTE_TYPE_BYTE_STRING to value. If attribute is of a different type, this operation will fail, returning FALSE.
;;;
;;; If cancellable is not NULL, then the operation can be cancelled by triggering the cancellable object from another thread. If the operation was cancelled, the error G_IO_ERROR_CANCELLED will be returned.
;;;
;;; file :
;;;     input GFile.
;;;
;;; attribute :
;;;     a string containing the attribute's name.
;;;
;;; value :
;;;     a string containing the attribute's new value.
;;;
;;; flags :
;;;     a GFileQueryInfoFlags.
;;;
;;; cancellable :
;;;     optional GCancellable object, NULL to ignore. [allow-none]
;;;
;;; error :
;;;     a GError, or NULL
;;;
;;; Returns :
;;;     TRUE if the attribute was successfully set to value in the file, FALSE otherwise.
;;; g_file_set_attribute_uint32 ()
;;;
;;; gboolean            g_file_set_attribute_uint32         (GFile *file,
;;;                                                          const char *attribute,
;;;                                                          guint32 value,
;;;                                                          GFileQueryInfoFlags flags,
;;;                                                          GCancellable *cancellable,
;;;                                                          GError **error);
;;;
;;; Sets attribute of type G_FILE_ATTRIBUTE_TYPE_UINT32 to value. If attribute is of a different type, this operation will fail.
;;;
;;; If cancellable is not NULL, then the operation can be cancelled by triggering the cancellable object from another thread. If the operation was cancelled, the error G_IO_ERROR_CANCELLED will be returned.
;;;
;;; file :
;;;     input GFile.
;;;
;;; attribute :
;;;     a string containing the attribute's name.
;;;
;;; value :
;;;     a guint32 containing the attribute's new value.
;;;
;;; flags :
;;;     a GFileQueryInfoFlags.
;;;
;;; cancellable :
;;;     optional GCancellable object, NULL to ignore. [allow-none]
;;;
;;; error :
;;;     a GError, or NULL
;;;
;;; Returns :
;;;     TRUE if the attribute was successfully set to value in the file, FALSE otherwise.
;;; g_file_set_attribute_int32 ()
;;;
;;; gboolean            g_file_set_attribute_int32          (GFile *file,
;;;                                                          const char *attribute,
;;;                                                          gint32 value,
;;;                                                          GFileQueryInfoFlags flags,
;;;                                                          GCancellable *cancellable,
;;;                                                          GError **error);
;;;
;;; Sets attribute of type G_FILE_ATTRIBUTE_TYPE_INT32 to value. If attribute is of a different type, this operation will fail.
;;;
;;; If cancellable is not NULL, then the operation can be cancelled by triggering the cancellable object from another thread. If the operation was cancelled, the error G_IO_ERROR_CANCELLED will be returned.
;;;
;;; file :
;;;     input GFile.
;;;
;;; attribute :
;;;     a string containing the attribute's name.
;;;
;;; value :
;;;     a gint32 containing the attribute's new value.
;;;
;;; flags :
;;;     a GFileQueryInfoFlags.
;;;
;;; cancellable :
;;;     optional GCancellable object, NULL to ignore. [allow-none]
;;;
;;; error :
;;;     a GError, or NULL
;;;
;;; Returns :
;;;     TRUE if the attribute was successfully set to value in the file, FALSE otherwise.
;;; g_file_set_attribute_uint64 ()
;;;
;;; gboolean            g_file_set_attribute_uint64         (GFile *file,
;;;                                                          const char *attribute,
;;;                                                          guint64 value,
;;;                                                          GFileQueryInfoFlags flags,
;;;                                                          GCancellable *cancellable,
;;;                                                          GError **error);
;;;
;;; Sets attribute of type G_FILE_ATTRIBUTE_TYPE_UINT64 to value. If attribute is of a different type, this operation will fail.
;;;
;;; If cancellable is not NULL, then the operation can be cancelled by triggering the cancellable object from another thread. If the operation was cancelled, the error G_IO_ERROR_CANCELLED will be returned.
;;;
;;; file :
;;;     input GFile.
;;;
;;; attribute :
;;;     a string containing the attribute's name.
;;;
;;; value :
;;;     a guint64 containing the attribute's new value.
;;;
;;; flags :
;;;     a GFileQueryInfoFlags.
;;;
;;; cancellable :
;;;     optional GCancellable object, NULL to ignore. [allow-none]
;;;
;;; error :
;;;     a GError, or NULL
;;;
;;; Returns :
;;;     TRUE if the attribute was successfully set to value in the file, FALSE otherwise.
;;; g_file_set_attribute_int64 ()
;;;
;;; gboolean            g_file_set_attribute_int64          (GFile *file,
;;;                                                          const char *attribute,
;;;                                                          gint64 value,
;;;                                                          GFileQueryInfoFlags flags,
;;;                                                          GCancellable *cancellable,
;;;                                                          GError **error);
;;;
;;; Sets attribute of type G_FILE_ATTRIBUTE_TYPE_INT64 to value. If attribute is of a different type, this operation will fail.
;;;
;;; If cancellable is not NULL, then the operation can be cancelled by triggering the cancellable object from another thread. If the operation was cancelled, the error G_IO_ERROR_CANCELLED will be returned.
;;;
;;; file :
;;;     input GFile.
;;;
;;; attribute :
;;;     a string containing the attribute's name.
;;;
;;; value :
;;;     a guint64 containing the attribute's new value.
;;;
;;; flags :
;;;     a GFileQueryInfoFlags.
;;;
;;; cancellable :
;;;     optional GCancellable object, NULL to ignore. [allow-none]
;;;
;;; error :
;;;     a GError, or NULL
;;;
;;; Returns :
;;;     TRUE if the attribute was successfully set, FALSE otherwise.
;;; g_file_mount_mountable ()
;;;
;;; void                g_file_mount_mountable              (GFile *file,
;;;                                                          GMountMountFlags flags,
;;;                                                          GMountOperation *mount_operation,
;;;                                                          GCancellable *cancellable,
;;;                                                          GAsyncReadyCallback callback,
;;;                                                          gpointer user_data);
;;;
;;; Mounts a file of type G_FILE_TYPE_MOUNTABLE. Using mount_operation, you can request callbacks when, for instance, passwords are needed during authentication.
;;;
;;; If cancellable is not NULL, then the operation can be cancelled by triggering the cancellable object from another thread. If the operation was cancelled, the error G_IO_ERROR_CANCELLED will be returned.
;;;
;;; When the operation is finished, callback will be called. You can then call g_file_mount_mountable_finish() to get the result of the operation.
;;;
;;; file :
;;;     input GFile.
;;;
;;; flags :
;;;     flags affecting the operation
;;;
;;; mount_operation :
;;;     a GMountOperation, or NULL to avoid user interaction. [allow-none]
;;;
;;; cancellable :
;;;     optional GCancellable object, NULL to ignore. [allow-none]
;;;
;;; callback :
;;;     a GAsyncReadyCallback to call when the request is satisfied, or NULL. [scope async][allow-none]
;;;
;;; user_data :
;;;     the data to pass to callback function. [closure]
;;; g_file_mount_mountable_finish ()
;;;
;;; GFile *             g_file_mount_mountable_finish       (GFile *file,
;;;                                                          GAsyncResult *result,
;;;                                                          GError **error);
;;;
;;; Finishes a mount operation. See g_file_mount_mountable() for details.
;;;
;;; Finish an asynchronous mount operation that was started with g_file_mount_mountable().
;;;
;;; file :
;;;     input GFile.
;;;
;;; result :
;;;     a GAsyncResult.
;;;
;;; error :
;;;     a GError, or NULL
;;;
;;; Returns :
;;;     a GFile or NULL on error. Free the returned object with g_object_unref(). [transfer full]
;;; g_file_unmount_mountable ()
;;;
;;; void                g_file_unmount_mountable            (GFile *file,
;;;                                                          GMountUnmountFlags flags,
;;;                                                          GCancellable *cancellable,
;;;                                                          GAsyncReadyCallback callback,
;;;                                                          gpointer user_data);
;;;
;;; Warning
;;;
;;; g_file_unmount_mountable has been deprecated since version 2.22 and should not be used in newly-written code. Use g_file_unmount_mountable_with_operation() instead.
;;;
;;; Unmounts a file of type G_FILE_TYPE_MOUNTABLE.
;;;
;;; If cancellable is not NULL, then the operation can be cancelled by triggering the cancellable object from another thread. If the operation was cancelled, the error G_IO_ERROR_CANCELLED will be returned.
;;;
;;; When the operation is finished, callback will be called. You can then call g_file_unmount_mountable_finish() to get the result of the operation.
;;;
;;; file :
;;;     input GFile.
;;;
;;; flags :
;;;     flags affecting the operation
;;;
;;; cancellable :
;;;     optional GCancellable object, NULL to ignore. [allow-none]
;;;
;;; callback :
;;;     a GAsyncReadyCallback to call when the request is satisfied, or NULL. [scope async][allow-none]
;;;
;;; user_data :
;;;     the data to pass to callback function. [closure]
;;; g_file_unmount_mountable_finish ()
;;;
;;; gboolean            g_file_unmount_mountable_finish     (GFile *file,
;;;                                                          GAsyncResult *result,
;;;                                                          GError **error);
;;;
;;; Warning
;;;
;;; g_file_unmount_mountable_finish has been deprecated since version 2.22 and should not be used in newly-written code. Use g_file_unmount_mountable_with_operation_finish() instead.
;;;
;;; Finishes an unmount operation, see g_file_unmount_mountable() for details.
;;;
;;; Finish an asynchronous unmount operation that was started with g_file_unmount_mountable().
;;;
;;; file :
;;;     input GFile.
;;;
;;; result :
;;;     a GAsyncResult.
;;;
;;; error :
;;;     a GError, or NULL
;;;
;;; Returns :
;;;     TRUE if the operation finished successfully. FALSE otherwise.
;;; g_file_unmount_mountable_with_operation ()
;;;
;;; void                g_file_unmount_mountable_with_operation
;;;                                                         (GFile *file,
;;;                                                          GMountUnmountFlags flags,
;;;                                                          GMountOperation *mount_operation,
;;;                                                          GCancellable *cancellable,
;;;                                                          GAsyncReadyCallback callback,
;;;                                                          gpointer user_data);
;;;
;;; Unmounts a file of type G_FILE_TYPE_MOUNTABLE.
;;;
;;; If cancellable is not NULL, then the operation can be cancelled by triggering the cancellable object from another thread. If the operation was cancelled, the error G_IO_ERROR_CANCELLED will be returned.
;;;
;;; When the operation is finished, callback will be called. You can then call g_file_unmount_mountable_finish() to get the result of the operation.
;;;
;;; file :
;;;     input GFile.
;;;
;;; flags :
;;;     flags affecting the operation
;;;
;;; mount_operation :
;;;     a GMountOperation, or NULL to avoid user interaction. [allow-none]
;;;
;;; cancellable :
;;;     optional GCancellable object, NULL to ignore. [allow-none]
;;;
;;; callback :
;;;     a GAsyncReadyCallback to call when the request is satisfied, or NULL. [scope async][allow-none]
;;;
;;; user_data :
;;;     the data to pass to callback function. [closure]
;;;
;;; Since 2.22
;;; g_file_unmount_mountable_with_operation_finish ()
;;;
;;; gboolean            g_file_unmount_mountable_with_operation_finish
;;;                                                         (GFile *file,
;;;                                                          GAsyncResult *result,
;;;                                                          GError **error);
;;;
;;; Finishes an unmount operation, see g_file_unmount_mountable_with_operation() for details.
;;;
;;; Finish an asynchronous unmount operation that was started with g_file_unmount_mountable_with_operation().
;;;
;;; file :
;;;     input GFile.
;;;
;;; result :
;;;     a GAsyncResult.
;;;
;;; error :
;;;     a GError, or NULL
;;;
;;; Returns :
;;;     TRUE if the operation finished successfully. FALSE otherwise.
;;;
;;; Since 2.22
;;; g_file_eject_mountable ()
;;;
;;; void                g_file_eject_mountable              (GFile *file,
;;;                                                          GMountUnmountFlags flags,
;;;                                                          GCancellable *cancellable,
;;;                                                          GAsyncReadyCallback callback,
;;;                                                          gpointer user_data);
;;;
;;; Warning
;;;
;;; g_file_eject_mountable has been deprecated since version 2.22 and should not be used in newly-written code. Use g_file_eject_mountable_with_operation() instead.
;;;
;;; Starts an asynchronous eject on a mountable. When this operation has completed, callback will be called with user_user data, and the operation can be finalized with g_file_eject_mountable_finish().
;;;
;;; If cancellable is not NULL, then the operation can be cancelled by triggering the cancellable object from another thread. If the operation was cancelled, the error G_IO_ERROR_CANCELLED will be returned.
;;;
;;; file :
;;;     input GFile.
;;;
;;; flags :
;;;     flags affecting the operation
;;;
;;; cancellable :
;;;     optional GCancellable object, NULL to ignore. [allow-none]
;;;
;;; callback :
;;;     a GAsyncReadyCallback to call when the request is satisfied, or NULL. [scope async][allow-none]
;;;
;;; user_data :
;;;     the data to pass to callback function. [closure]
;;; g_file_eject_mountable_finish ()
;;;
;;; gboolean            g_file_eject_mountable_finish       (GFile *file,
;;;                                                          GAsyncResult *result,
;;;                                                          GError **error);
;;;
;;; Warning
;;;
;;; g_file_eject_mountable_finish has been deprecated since version 2.22 and should not be used in newly-written code. Use g_file_eject_mountable_with_operation_finish() instead.
;;;
;;; Finishes an asynchronous eject operation started by g_file_eject_mountable().
;;;
;;; file :
;;;     input GFile.
;;;
;;; result :
;;;     a GAsyncResult.
;;;
;;; error :
;;;     a GError, or NULL
;;;
;;; Returns :
;;;     TRUE if the file was ejected successfully. FALSE otherwise.
;;; g_file_eject_mountable_with_operation ()
;;;
;;; void                g_file_eject_mountable_with_operation
;;;                                                         (GFile *file,
;;;                                                          GMountUnmountFlags flags,
;;;                                                          GMountOperation *mount_operation,
;;;                                                          GCancellable *cancellable,
;;;                                                          GAsyncReadyCallback callback,
;;;                                                          gpointer user_data);
;;;
;;; Starts an asynchronous eject on a mountable. When this operation has completed, callback will be called with user_user data, and the operation can be finalized with g_file_eject_mountable_with_operation_finish().
;;;
;;; If cancellable is not NULL, then the operation can be cancelled by triggering the cancellable object from another thread. If the operation was cancelled, the error G_IO_ERROR_CANCELLED will be returned.
;;;
;;; file :
;;;     input GFile.
;;;
;;; flags :
;;;     flags affecting the operation
;;;
;;; mount_operation :
;;;     a GMountOperation, or NULL to avoid user interaction. [allow-none]
;;;
;;; cancellable :
;;;     optional GCancellable object, NULL to ignore. [allow-none]
;;;
;;; callback :
;;;     a GAsyncReadyCallback to call when the request is satisfied, or NULL. [scope async][allow-none]
;;;
;;; user_data :
;;;     the data to pass to callback function. [closure]
;;;
;;; Since 2.22
;;; g_file_eject_mountable_with_operation_finish ()
;;;
;;; gboolean            g_file_eject_mountable_with_operation_finish
;;;                                                         (GFile *file,
;;;                                                          GAsyncResult *result,
;;;                                                          GError **error);
;;;
;;; Finishes an asynchronous eject operation started by g_file_eject_mountable_with_operation().
;;;
;;; file :
;;;     input GFile.
;;;
;;; result :
;;;     a GAsyncResult.
;;;
;;; error :
;;;     a GError, or NULL
;;;
;;; Returns :
;;;     TRUE if the file was ejected successfully. FALSE otherwise.
;;;
;;; Since 2.22
;;; g_file_start_mountable ()
;;;
;;; void                g_file_start_mountable              (GFile *file,
;;;                                                          GDriveStartFlags flags,
;;;                                                          GMountOperation *start_operation,
;;;                                                          GCancellable *cancellable,
;;;                                                          GAsyncReadyCallback callback,
;;;                                                          gpointer user_data);
;;;
;;; Starts a file of type G_FILE_TYPE_MOUNTABLE. Using start_operation, you can request callbacks when, for instance, passwords are needed during authentication.
;;;
;;; If cancellable is not NULL, then the operation can be cancelled by triggering the cancellable object from another thread. If the operation was cancelled, the error G_IO_ERROR_CANCELLED will be returned.
;;;
;;; When the operation is finished, callback will be called. You can then call g_file_mount_mountable_finish() to get the result of the operation.
;;;
;;; file :
;;;     input GFile.
;;;
;;; flags :
;;;     flags affecting the operation
;;;
;;; start_operation :
;;;     a GMountOperation, or NULL to avoid user interaction. [allow-none]
;;;
;;; cancellable :
;;;     optional GCancellable object, NULL to ignore. [allow-none]
;;;
;;; callback :
;;;     a GAsyncReadyCallback to call when the request is satisfied, or NULL. [allow-none]
;;;
;;; user_data :
;;;     the data to pass to callback function
;;;
;;; Since 2.22
;;; g_file_start_mountable_finish ()
;;;
;;; gboolean            g_file_start_mountable_finish       (GFile *file,
;;;                                                          GAsyncResult *result,
;;;                                                          GError **error);
;;;
;;; Finishes a start operation. See g_file_start_mountable() for details.
;;;
;;; Finish an asynchronous start operation that was started with g_file_start_mountable().
;;;
;;; file :
;;;     input GFile.
;;;
;;; result :
;;;     a GAsyncResult.
;;;
;;; error :
;;;     a GError, or NULL
;;;
;;; Returns :
;;;     TRUE if the operation finished successfully. FALSE otherwise.
;;;
;;; Since 2.22
;;; g_file_stop_mountable ()
;;;
;;; void                g_file_stop_mountable               (GFile *file,
;;;                                                          GMountUnmountFlags flags,
;;;                                                          GMountOperation *mount_operation,
;;;                                                          GCancellable *cancellable,
;;;                                                          GAsyncReadyCallback callback,
;;;                                                          gpointer user_data);
;;;
;;; Stops a file of type G_FILE_TYPE_MOUNTABLE.
;;;
;;; If cancellable is not NULL, then the operation can be cancelled by triggering the cancellable object from another thread. If the operation was cancelled, the error G_IO_ERROR_CANCELLED will be returned.
;;;
;;; When the operation is finished, callback will be called. You can then call g_file_stop_mountable_finish() to get the result of the operation.
;;;
;;; file :
;;;     input GFile.
;;;
;;; flags :
;;;     flags affecting the operation
;;;
;;; mount_operation :
;;;     a GMountOperation, or NULL to avoid user interaction. [allow-none]
;;;
;;; cancellable :
;;;     optional GCancellable object, NULL to ignore. [allow-none]
;;;
;;; callback :
;;;     a GAsyncReadyCallback to call when the request is satisfied, or NULL. [allow-none]
;;;
;;; user_data :
;;;     the data to pass to callback function
;;;
;;; Since 2.22
;;; g_file_stop_mountable_finish ()
;;;
;;; gboolean            g_file_stop_mountable_finish        (GFile *file,
;;;                                                          GAsyncResult *result,
;;;                                                          GError **error);
;;;
;;; Finishes an stop operation, see g_file_stop_mountable() for details.
;;;
;;; Finish an asynchronous stop operation that was started with g_file_stop_mountable().
;;;
;;; file :
;;;     input GFile.
;;;
;;; result :
;;;     a GAsyncResult.
;;;
;;; error :
;;;     a GError, or NULL
;;;
;;; Returns :
;;;     TRUE if the operation finished successfully. FALSE otherwise.
;;;
;;; Since 2.22
;;; g_file_poll_mountable ()
;;;
;;; void                g_file_poll_mountable               (GFile *file,
;;;                                                          GCancellable *cancellable,
;;;                                                          GAsyncReadyCallback callback,
;;;                                                          gpointer user_data);
;;;
;;; Polls a file of type G_FILE_TYPE_MOUNTABLE.
;;;
;;; If cancellable is not NULL, then the operation can be cancelled by triggering the cancellable object from another thread. If the operation was cancelled, the error G_IO_ERROR_CANCELLED will be returned.
;;;
;;; When the operation is finished, callback will be called. You can then call g_file_mount_mountable_finish() to get the result of the operation.
;;;
;;; file :
;;;     input GFile.
;;;
;;; cancellable :
;;;     optional GCancellable object, NULL to ignore.
;;;
;;; callback :
;;;     a GAsyncReadyCallback to call when the request is satisfied, or NULL. [allow-none]
;;;
;;; user_data :
;;;     the data to pass to callback function
;;;
;;; Since 2.22
;;; g_file_poll_mountable_finish ()
;;;
;;; gboolean            g_file_poll_mountable_finish        (GFile *file,
;;;                                                          GAsyncResult *result,
;;;                                                          GError **error);
;;;
;;; Finishes a poll operation. See g_file_poll_mountable() for details.
;;;
;;; Finish an asynchronous poll operation that was polled with g_file_poll_mountable().
;;;
;;; file :
;;;     input GFile.
;;;
;;; result :
;;;     a GAsyncResult.
;;;
;;; error :
;;;     a GError, or NULL
;;;
;;; Returns :
;;;     TRUE if the operation finished successfully. FALSE otherwise.
;;;
;;; Since 2.22
;;; g_file_mount_enclosing_volume ()
;;;
;;; void                g_file_mount_enclosing_volume       (GFile *location,
;;;                                                          GMountMountFlags flags,
;;;                                                          GMountOperation *mount_operation,
;;;                                                          GCancellable *cancellable,
;;;                                                          GAsyncReadyCallback callback,
;;;                                                          gpointer user_data);
;;;
;;; Starts a mount_operation, mounting the volume that contains the file location.
;;;
;;; When this operation has completed, callback will be called with user_user data, and the operation can be finalized with g_file_mount_enclosing_volume_finish().
;;;
;;; If cancellable is not NULL, then the operation can be cancelled by triggering the cancellable object from another thread. If the operation was cancelled, the error G_IO_ERROR_CANCELLED will be returned.
;;;
;;; location :
;;;     input GFile.
;;;
;;; flags :
;;;     flags affecting the operation
;;;
;;; mount_operation :
;;;     a GMountOperation or NULL to avoid user interaction. [allow-none]
;;;
;;; cancellable :
;;;     optional GCancellable object, NULL to ignore. [allow-none]
;;;
;;; callback :
;;;     a GAsyncReadyCallback to call when the request is satisfied, or NULL. [allow-none]
;;;
;;; user_data :
;;;     the data to pass to callback function
;;; g_file_mount_enclosing_volume_finish ()
;;;
;;; gboolean            g_file_mount_enclosing_volume_finish
;;;                                                         (GFile *location,
;;;                                                          GAsyncResult *result,
;;;                                                          GError **error);
;;;
;;; Finishes a mount operation started by g_file_mount_enclosing_volume().
;;;
;;; location :
;;;     input GFile.
;;;
;;; result :
;;;     a GAsyncResult.
;;;
;;; error :
;;;     a GError, or NULL
;;;
;;; Returns :
;;;     TRUE if successful. If an error has occurred, this function will return FALSE and set error appropriately if present.
;;; g_file_monitor_directory ()
;;;
;;; GFileMonitor *      g_file_monitor_directory            (GFile *file,
;;;                                                          GFileMonitorFlags flags,
;;;                                                          GCancellable *cancellable,
;;;                                                          GError **error);
;;;
;;; Obtains a directory monitor for the given file. This may fail if directory monitoring is not supported.
;;;
;;; If cancellable is not NULL, then the operation can be cancelled by triggering the cancellable object from another thread. If the operation was cancelled, the error G_IO_ERROR_CANCELLED will be returned.
;;;
;;; Virtual: monitor_dir
;;;
;;; file :
;;;     input GFile.
;;;
;;; flags :
;;;     a set of GFileMonitorFlags.
;;;
;;; cancellable :
;;;     optional GCancellable object, NULL to ignore. [allow-none]
;;;
;;; error :
;;;     a GError, or NULL.
;;;
;;; Returns :
;;;     a GFileMonitor for the given file, or NULL on error. Free the returned object with g_object_unref(). [transfer full]
;;; g_file_monitor_file ()
;;;
;;; GFileMonitor *      g_file_monitor_file                 (GFile *file,
;;;                                                          GFileMonitorFlags flags,
;;;                                                          GCancellable *cancellable,
;;;                                                          GError **error);
;;;
;;; Obtains a file monitor for the given file. If no file notification mechanism exists, then regular polling of the file is used.
;;;
;;; If cancellable is not NULL, then the operation can be cancelled by triggering the cancellable object from another thread. If the operation was cancelled, the error G_IO_ERROR_CANCELLED will be returned.
;;;
;;; file :
;;;     input GFile.
;;;
;;; flags :
;;;     a set of GFileMonitorFlags.
;;;
;;; cancellable :
;;;     optional GCancellable object, NULL to ignore. [allow-none]
;;;
;;; error :
;;;     a GError, or NULL.
;;;
;;; Returns :
;;;     a GFileMonitor for the given file, or NULL on error. Free the returned object with g_object_unref(). [transfer full]
;;; g_file_monitor ()
;;;
;;; GFileMonitor *      g_file_monitor                      (GFile *file,
;;;                                                          GFileMonitorFlags flags,
;;;                                                          GCancellable *cancellable,
;;;                                                          GError **error);
;;;
;;; Obtains a file or directory monitor for the given file, depending on the type of the file.
;;;
;;; If cancellable is not NULL, then the operation can be cancelled by triggering the cancellable object from another thread. If the operation was cancelled, the error G_IO_ERROR_CANCELLED will be returned.
;;;
;;; file :
;;;     input GFile
;;;
;;; flags :
;;;     a set of GFileMonitorFlags
;;;
;;; cancellable :
;;;     optional GCancellable object, NULL to ignore. [allow-none]
;;;
;;; error :
;;;     a GError, or NULL
;;;
;;; Returns :
;;;     a GFileMonitor for the given file, or NULL on error. Free the returned object with g_object_unref(). [transfer full]
;;;
;;; Since 2.18
;;; g_file_load_contents ()
;;;
;;; gboolean            g_file_load_contents                (GFile *file,
;;;                                                          GCancellable *cancellable,
;;;                                                          char **contents,
;;;                                                          gsize *length,
;;;                                                          char **etag_out,
;;;                                                          GError **error);
;;;
;;; Loads the content of the file into memory. The data is always zero-terminated, but this is not included in the resultant length. The returned content should be freed with g_free() when no longer needed.
;;;
;;; If cancellable is not NULL, then the operation can be cancelled by triggering the cancellable object from another thread. If the operation was cancelled, the error G_IO_ERROR_CANCELLED will be returned.
;;;
;;; file :
;;;     input GFile.
;;;
;;; cancellable :
;;;     optional GCancellable object, NULL to ignore.
;;;
;;; contents :
;;;     a location to place the contents of the file. [out][transfer full][element-type guint8][array length=length]
;;;
;;; length :
;;;     a location to place the length of the contents of the file, or NULL if the length is not needed. [out][allow-none]
;;;
;;; etag_out :
;;;     a location to place the current entity tag for the file, or NULL if the entity tag is not needed. [out][allow-none]
;;;
;;; error :
;;;     a GError, or NULL
;;;
;;; Returns :
;;;     TRUE if the file's contents were successfully loaded. FALSE if there were errors.
;;; g_file_load_contents_async ()
;;;
;;; void                g_file_load_contents_async          (GFile *file,
;;;                                                          GCancellable *cancellable,
;;;                                                          GAsyncReadyCallback callback,
;;;                                                          gpointer user_data);
;;;
;;; Starts an asynchronous load of the file's contents.
;;;
;;; For more details, see g_file_load_contents() which is the synchronous version of this call.
;;;
;;; When the load operation has completed, callback will be called with user data. To finish the operation, call g_file_load_contents_finish() with the GAsyncResult returned by the callback.
;;;
;;; If cancellable is not NULL, then the operation can be cancelled by triggering the cancellable object from another thread. If the operation was cancelled, the error G_IO_ERROR_CANCELLED will be returned.
;;;
;;; file :
;;;     input GFile.
;;;
;;; cancellable :
;;;     optional GCancellable object, NULL to ignore.
;;;
;;; callback :
;;;     a GAsyncReadyCallback to call when the request is satisfied
;;;
;;; user_data :
;;;     the data to pass to callback function
;;; g_file_load_contents_finish ()
;;;
;;; gboolean            g_file_load_contents_finish         (GFile *file,
;;;                                                          GAsyncResult *res,
;;;                                                          char **contents,
;;;                                                          gsize *length,
;;;                                                          char **etag_out,
;;;                                                          GError **error);
;;;
;;; Finishes an asynchronous load of the file's contents. The contents are placed in contents, and length is set to the size of the contents string. The content should be freed with g_free() when no longer needed. If etag_out is present, it will be set to the new entity tag for the file.
;;;
;;; file :
;;;     input GFile.
;;;
;;; res :
;;;     a GAsyncResult.
;;;
;;; contents :
;;;     a location to place the contents of the file. [out][transfer full][element-type guint8][array length=length]
;;;
;;; length :
;;;     a location to place the length of the contents of the file, or NULL if the length is not needed. [out][allow-none]
;;;
;;; etag_out :
;;;     a location to place the current entity tag for the file, or NULL if the entity tag is not needed. [out][allow-none]
;;;
;;; error :
;;;     a GError, or NULL
;;;
;;; Returns :
;;;     TRUE if the load was successful. If FALSE and error is present, it will be set appropriately.
;;; g_file_load_partial_contents_async ()
;;;
;;; void                g_file_load_partial_contents_async  (GFile *file,
;;;                                                          GCancellable *cancellable,
;;;                                                          GFileReadMoreCallback read_more_callback,
;;;                                                          GAsyncReadyCallback callback,
;;;                                                          gpointer user_data);
;;;
;;; Reads the partial contents of a file. A GFileReadMoreCallback should be used to stop reading from the file when appropriate, else this function will behave exactly as g_file_load_contents_async(). This operation can be finished by g_file_load_partial_contents_finish().
;;;
;;; Users of this function should be aware that user_data is passed to both the read_more_callback and the callback.
;;;
;;; If cancellable is not NULL, then the operation can be cancelled by triggering the cancellable object from another thread. If the operation was cancelled, the error G_IO_ERROR_CANCELLED will be returned.
;;;
;;; file :
;;;     input GFile.
;;;
;;; cancellable :
;;;     optional GCancellable object, NULL to ignore.
;;;
;;; read_more_callback :
;;;     a GFileReadMoreCallback to receive partial data and to specify whether further data should be read.
;;;
;;; callback :
;;;     a GAsyncReadyCallback to call when the request is satisfied
;;;
;;; user_data :
;;;     the data to pass to the callback functions.
;;; g_file_load_partial_contents_finish ()
;;;
;;; gboolean            g_file_load_partial_contents_finish (GFile *file,
;;;                                                          GAsyncResult *res,
;;;                                                          char **contents,
;;;                                                          gsize *length,
;;;                                                          char **etag_out,
;;;                                                          GError **error);
;;;
;;; Finishes an asynchronous partial load operation that was started with g_file_load_partial_contents_async(). The data is always zero-terminated, but this is not included in the resultant length. The returned content should be freed with g_free() when no longer needed.
;;;
;;; file :
;;;     input GFile.
;;;
;;; res :
;;;     a GAsyncResult.
;;;
;;; contents :
;;;     a location to place the contents of the file. [out][transfer full][element-type guint8][array length=length]
;;;
;;; length :
;;;     a location to place the length of the contents of the file, or NULL if the length is not needed. [out][allow-none]
;;;
;;; etag_out :
;;;     a location to place the current entity tag for the file, or NULL if the entity tag is not needed. [out][allow-none]
;;;
;;; error :
;;;     a GError, or NULL
;;;
;;; Returns :
;;;     TRUE if the load was successful. If FALSE and error is present, it will be set appropriately.
;;; g_file_replace_contents ()
;;;
;;; gboolean            g_file_replace_contents             (GFile *file,
;;;                                                          const char *contents,
;;;                                                          gsize length,
;;;                                                          const char *etag,
;;;                                                          gboolean make_backup,
;;;                                                          GFileCreateFlags flags,
;;;                                                          char **new_etag,
;;;                                                          GCancellable *cancellable,
;;;                                                          GError **error);
;;;
;;; Replaces the contents of file with contents of length bytes. If etag is specified (not NULL) any existing file must have that etag, or the error G_IO_ERROR_WRONG_ETAG will be returned.
;;;
;;; If make_backup is TRUE, this function will attempt to make a backup of file.
;;;
;;; If cancellable is not NULL, then the operation can be cancelled by triggering the cancellable object from another thread. If the operation was cancelled, the error G_IO_ERROR_CANCELLED will be returned.
;;;
;;; The returned new_etag can be used to verify that the file hasn't changed the next time it is saved over.
;;;
;;; file :
;;;     input GFile.
;;;
;;; contents :
;;;     a string containing the new contents for file. [element-type guint8][array length=length]
;;;
;;; length :
;;;     the length of contents in bytes.
;;;
;;; etag :
;;;     the old entity tag for the document, or NULL. [allow-none]
;;;
;;; make_backup :
;;;     TRUE if a backup should be created.
;;;
;;; flags :
;;;     a set of GFileCreateFlags.
;;;
;;; new_etag :
;;;     a location to a new entity tag for the document. This should be freed with g_free() when no longer needed, or NULL. [allow-none][out]
;;;
;;; cancellable :
;;;     optional GCancellable object, NULL to ignore.
;;;
;;; error :
;;;     a GError, or NULL
;;;
;;; Returns :
;;;     TRUE if successful. If an error has occurred, this function will return FALSE and set error appropriately if present.
;;; g_file_replace_contents_async ()
;;;
;;; void                g_file_replace_contents_async       (GFile *file,
;;;                                                          const char *contents,
;;;                                                          gsize length,
;;;                                                          const char *etag,
;;;                                                          gboolean make_backup,
;;;                                                          GFileCreateFlags flags,
;;;                                                          GCancellable *cancellable,
;;;                                                          GAsyncReadyCallback callback,
;;;                                                          gpointer user_data);
;;;
;;; Starts an asynchronous replacement of file with the given contents of length bytes. etag will replace the document's current entity tag.
;;;
;;; When this operation has completed, callback will be called with user_user data, and the operation can be finalized with g_file_replace_contents_finish().
;;;
;;; If cancellable is not NULL, then the operation can be cancelled by triggering the cancellable object from another thread. If the operation was cancelled, the error G_IO_ERROR_CANCELLED will be returned.
;;;
;;; If make_backup is TRUE, this function will attempt to make a backup of file.
;;;
;;; file :
;;;     input GFile.
;;;
;;; contents :
;;;     string of contents to replace the file with. [element-type guint8][array length=length]
;;;
;;; length :
;;;     the length of contents in bytes.
;;;
;;; etag :
;;;     a new entity tag for the file, or NULL. [allow-none]
;;;
;;; make_backup :
;;;     TRUE if a backup should be created.
;;;
;;; flags :
;;;     a set of GFileCreateFlags.
;;;
;;; cancellable :
;;;     optional GCancellable object, NULL to ignore.
;;;
;;; callback :
;;;     a GAsyncReadyCallback to call when the request is satisfied
;;;
;;; user_data :
;;;     the data to pass to callback function
;;; g_file_replace_contents_finish ()
;;;
;;; gboolean            g_file_replace_contents_finish      (GFile *file,
;;;                                                          GAsyncResult *res,
;;;                                                          char **new_etag,
;;;                                                          GError **error);
;;;
;;; Finishes an asynchronous replace of the given file. See g_file_replace_contents_async(). Sets new_etag to the new entity tag for the document, if present.
;;;
;;; file :
;;;     input GFile.
;;;
;;; res :
;;;     a GAsyncResult.
;;;
;;; new_etag :
;;;     a location of a new entity tag for the document. This should be freed with g_free() when it is no longer needed, or NULL. [out][allow-none]
;;;
;;; error :
;;;     a GError, or NULL
;;;
;;; Returns :
;;;     TRUE on success, FALSE on failure.
;;; g_file_copy_attributes ()
;;;
;;; gboolean            g_file_copy_attributes              (GFile *source,
;;;                                                          GFile *destination,
;;;                                                          GFileCopyFlags flags,
;;;                                                          GCancellable *cancellable,
;;;                                                          GError **error);
;;;
;;; Copies the file attributes from source to destination.
;;;
;;; Normally only a subset of the file attributes are copied, those that are copies in a normal file copy operation (which for instance does not include e.g. owner). However if G_FILE_COPY_ALL_METADATA is specified in flags, then all the metadata that is possible to copy is copied. This is useful when implementing move by copy + delete source.
;;;
;;; source :
;;;     a GFile with attributes.
;;;
;;; destination :
;;;     a GFile to copy attributes to.
;;;
;;; flags :
;;;     a set of GFileCopyFlags.
;;;
;;; cancellable :
;;;     optional GCancellable object, NULL to ignore. [allow-none]
;;;
;;; error :
;;;     a GError, NULL to ignore.
;;;
;;; Returns :
;;;     TRUE if the attributes were copied successfully, FALSE otherwise.
;;; g_file_create_readwrite ()
;;;
;;; GFileIOStream *     g_file_create_readwrite             (GFile *file,
;;;                                                          GFileCreateFlags flags,
;;;                                                          GCancellable *cancellable,
;;;                                                          GError **error);
;;;
;;; Creates a new file and returns a stream for reading and writing to it. The file must not already exist.
;;;
;;; By default files created are generally readable by everyone, but if you pass G_FILE_CREATE_PRIVATE in flags the file will be made readable only to the current user, to the level that is supported on the target filesystem.
;;;
;;; If cancellable is not NULL, then the operation can be cancelled by triggering the cancellable object from another thread. If the operation was cancelled, the error G_IO_ERROR_CANCELLED will be returned.
;;;
;;; If a file or directory with this name already exists the G_IO_ERROR_EXISTS error will be returned. Some file systems don't allow all file names, and may return an G_IO_ERROR_INVALID_FILENAME error, and if the name is too long, G_IO_ERROR_FILENAME_TOO_LONG will be returned. Other errors are possible too, and depend on what kind of filesystem the file is on.
;;;
;;; Note that in many non-local file cases read and write streams are not supported, so make sure you really need to do read and write streaming, rather than just opening for reading or writing.
;;;
;;; file :
;;;     a GFile
;;;
;;; flags :
;;;     a set of GFileCreateFlags
;;;
;;; cancellable :
;;;     optional GCancellable object, NULL to ignore. [allow-none]
;;;
;;; error :
;;;     return location for a GError, or NULL
;;;
;;; Returns :
;;;     a GFileIOStream for the newly created file, or NULL on error. Free the returned object with g_object_unref(). [transfer full]
;;;
;;; Since 2.22
;;; g_file_create_readwrite_async ()
;;;
;;; void                g_file_create_readwrite_async       (GFile *file,
;;;                                                          GFileCreateFlags flags,
;;;                                                          int io_priority,
;;;                                                          GCancellable *cancellable,
;;;                                                          GAsyncReadyCallback callback,
;;;                                                          gpointer user_data);
;;;
;;; Asynchronously creates a new file and returns a stream for reading and writing to it. The file must not already exist.
;;;
;;; For more details, see g_file_create_readwrite() which is the synchronous version of this call.
;;;
;;; When the operation is finished, callback will be called. You can then call g_file_create_readwrite_finish() to get the result of the operation.
;;;
;;; file :
;;;     input GFile
;;;
;;; flags :
;;;     a set of GFileCreateFlags
;;;
;;; io_priority :
;;;     the I/O priority of the request
;;;
;;; cancellable :
;;;     optional GCancellable object, NULL to ignore. [allow-none]
;;;
;;; callback :
;;;     a GAsyncReadyCallback to call when the request is satisfied. [scope async]
;;;
;;; user_data :
;;;     the data to pass to callback function. [closure]
;;;
;;; Since 2.22
;;; g_file_create_readwrite_finish ()
;;;
;;; GFileIOStream *     g_file_create_readwrite_finish      (GFile *file,
;;;                                                          GAsyncResult *res,
;;;                                                          GError **error);
;;;
;;; Finishes an asynchronous file create operation started with g_file_create_readwrite_async().
;;;
;;; file :
;;;     input GFile
;;;
;;; res :
;;;     a GAsyncResult
;;;
;;; error :
;;;     a GError, or NULL
;;;
;;; Returns :
;;;     a GFileIOStream or NULL on error. Free the returned object with g_object_unref(). [transfer full]
;;;
;;; Since 2.22
;;; g_file_open_readwrite ()
;;;
;;; GFileIOStream *     g_file_open_readwrite               (GFile *file,
;;;                                                          GCancellable *cancellable,
;;;                                                          GError **error);
;;;
;;; Opens an existing file for reading and writing. The result is a GFileIOStream that can be used to read and write the contents of the file.
;;;
;;; If cancellable is not NULL, then the operation can be cancelled by triggering the cancellable object from another thread. If the operation was cancelled, the error G_IO_ERROR_CANCELLED will be returned.
;;;
;;; If the file does not exist, the G_IO_ERROR_NOT_FOUND error will be returned. If the file is a directory, the G_IO_ERROR_IS_DIRECTORY error will be returned. Other errors are possible too, and depend on what kind of filesystem the file is on. Note that in many non-local file cases read and write streams are not supported, so make sure you really need to do read and write streaming, rather than just opening for reading or writing.
;;;
;;; file :
;;;     GFile to open
;;;
;;; cancellable :
;;;     a GCancellable. [allow-none]
;;;
;;; error :
;;;     a GError, or NULL
;;;
;;; Returns :
;;;     GFileIOStream or NULL on error. Free the returned object with g_object_unref(). [transfer full]
;;;
;;; Since 2.22
;;; g_file_open_readwrite_async ()
;;;
;;; void                g_file_open_readwrite_async         (GFile *file,
;;;                                                          int io_priority,
;;;                                                          GCancellable *cancellable,
;;;                                                          GAsyncReadyCallback callback,
;;;                                                          gpointer user_data);
;;;
;;; Asynchronously opens file for reading and writing.
;;;
;;; For more details, see g_file_open_readwrite() which is the synchronous version of this call.
;;;
;;; When the operation is finished, callback will be called. You can then call g_file_open_readwrite_finish() to get the result of the operation.
;;;
;;; file :
;;;     input GFile.
;;;
;;; io_priority :
;;;     the I/O priority of the request.
;;;
;;; cancellable :
;;;     optional GCancellable object, NULL to ignore. [allow-none]
;;;
;;; callback :
;;;     a GAsyncReadyCallback to call when the request is satisfied. [scope async]
;;;
;;; user_data :
;;;     the data to pass to callback function. [closure]
;;;
;;; Since 2.22
;;; g_file_open_readwrite_finish ()
;;;
;;; GFileIOStream *     g_file_open_readwrite_finish        (GFile *file,
;;;                                                          GAsyncResult *res,
;;;                                                          GError **error);
;;;
;;; Finishes an asynchronous file read operation started with g_file_open_readwrite_async().
;;;
;;; file :
;;;     input GFile.
;;;
;;; res :
;;;     a GAsyncResult.
;;;
;;; error :
;;;     a GError, or NULL
;;;
;;; Returns :
;;;     a GFileIOStream or NULL on error. Free the returned object with g_object_unref(). [transfer full]
;;;
;;; Since 2.22
;;; g_file_replace_readwrite ()
;;;
;;; GFileIOStream *     g_file_replace_readwrite            (GFile *file,
;;;                                                          const char *etag,
;;;                                                          gboolean make_backup,
;;;                                                          GFileCreateFlags flags,
;;;                                                          GCancellable *cancellable,
;;;                                                          GError **error);
;;;
;;; Returns an output stream for overwriting the file in readwrite mode, possibly creating a backup copy of the file first. If the file doesn't exist, it will be created.
;;;
;;; For details about the behaviour, see g_file_replace() which does the same thing but returns an output stream only.
;;;
;;; Note that in many non-local file cases read and write streams are not supported, so make sure you really need to do read and write streaming, rather than just opening for reading or writing.
;;;
;;; file :
;;;     a GFile
;;;
;;; etag :
;;;     an optional entity tag for the current GFile, or NULL to ignore. [allow-none]
;;;
;;; make_backup :
;;;     TRUE if a backup should be created
;;;
;;; flags :
;;;     a set of GFileCreateFlags
;;;
;;; cancellable :
;;;     optional GCancellable object, NULL to ignore. [allow-none]
;;;
;;; error :
;;;     return location for a GError, or NULL
;;;
;;; Returns :
;;;     a GFileIOStream or NULL on error. Free the returned object with g_object_unref(). [transfer full]
;;;
;;; Since 2.22
;;; g_file_replace_readwrite_async ()
;;;
;;; void                g_file_replace_readwrite_async      (GFile *file,
;;;                                                          const char *etag,
;;;                                                          gboolean make_backup,
;;;                                                          GFileCreateFlags flags,
;;;                                                          int io_priority,
;;;                                                          GCancellable *cancellable,
;;;                                                          GAsyncReadyCallback callback,
;;;                                                          gpointer user_data);
;;;
;;; Asynchronously overwrites the file in read-write mode, replacing the contents, possibly creating a backup copy of the file first.
;;;
;;; For more details, see g_file_replace_readwrite() which is the synchronous version of this call.
;;;
;;; When the operation is finished, callback will be called. You can then call g_file_replace_readwrite_finish() to get the result of the operation.
;;;
;;; file :
;;;     input GFile.
;;;
;;; etag :
;;;     an entity tag for the current GFile, or NULL to ignore. [allow-none]
;;;
;;; make_backup :
;;;     TRUE if a backup should be created.
;;;
;;; flags :
;;;     a set of GFileCreateFlags.
;;;
;;; io_priority :
;;;     the I/O priority of the request.
;;;
;;; cancellable :
;;;     optional GCancellable object, NULL to ignore. [allow-none]
;;;
;;; callback :
;;;     a GAsyncReadyCallback to call when the request is satisfied. [scope async]
;;;
;;; user_data :
;;;     the data to pass to callback function. [closure]
;;;
;;; Since 2.22
;;; g_file_replace_readwrite_finish ()
;;;
;;; GFileIOStream *     g_file_replace_readwrite_finish     (GFile *file,
;;;                                                          GAsyncResult *res,
;;;                                                          GError **error);
;;;
;;; Finishes an asynchronous file replace operation started with g_file_replace_readwrite_async().
;;;
;;; file :
;;;     input GFile.
;;;
;;; res :
;;;     a GAsyncResult.
;;;
;;; error :
;;;     a GError, or NULL
;;;
;;; Returns :
;;;     a GFileIOStream, or NULL on error. Free the returned object with g_object_unref(). [transfer full]
;;;
;;; Since 2.22
;;; g_file_supports_thread_contexts ()
;;;
;;; gboolean            g_file_supports_thread_contexts     (GFile *file);
;;;
;;; Checks if file supports thread-default contexts. If this returns FALSE, you cannot perform asynchronous operations on file in a thread that has a thread-default context.
;;;
;;; file :
;;;     a GFile.
;;;
;;; Returns :
;;;     Whether or not file supports thread-default contexts.
;;;
;;; Since 2.22
;;; ----------------------------------------------------------------------------

;;; --- End of file gio.file.lisp ----------------------------------------------
