;;; ----------------------------------------------------------------------------
;;; glib.utils.lisp
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2012 Dr. Dieter Kaiser
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See http://common-lisp.net/project/cl-gtk2/
;;;
;;; The documentation of this file has been copied from the
;;; GLib 2.30.2 Reference Manual.  See http://www.gtk.org.
;;;
;;; ----------------------------------------------------------------------------
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
;;; Miscellaneous Utility Functions
;;; 
;;; A selection of portable utility functions
;;; 	
;;; Synopsis
;;; 
;;;     g-get-application-name ()
;;;     g-set-application-name (application-name)
;;;     g-get-prgname ()
;;;     g-set-prgname (prgname)
;;;
;;;     g_getenv (variable)
;;;     g_setenv (variable value overwrite)
;;;
;;;     g-listenv ()
;;;     g-get-user-name ()
;;;     g-get-real-name ()
;;;     g-get-user-cache-dir ()
;;;     g-get-user-data-dir ()
;;;     g-get-user-config-dir ()
;;;
;;;     g-user-directory
;;;     g-get-user-special-dir (directory)
;;;     g-get-system-data-dirs ()
;;;     g-get-system-config-dirs ()
;;;
;;;     g_get_host_name ()
;;;     g_get_home_dir ()
;;;     g_get_tmp_dir ()
;;;     g_get_current_dir ()
;;;
;;;     g-path-is-absolute (file-name)
;;;
;;;     g-build-filename (first-element ...)
;;;     g-build-filenamev (args)
;;;
;;; The following symbols are not implemented:
;;;
;;;     g_get_environ ()
;;;     g_unsetenv (variable)
;;;     g_get_user_runtime_dir()
;;;     g_reload_user_special_dirs_cache ()
;;;     g_basename (file_name)
;;;     g_dirname
;;;     g_path_skip_root (file_name)
;;;     g_path_get_basename (file_name)
;;;     g_path_get_dirname (file_name)
;;;     g_build_path (separator first_element ...)
;;;     g_build_pathv (separator args)
;;;     g_format_size (size)
;;;     GFormatSizeFlags
;;;     g_format_size_full (size flags)
;;;     g_format_size_for_display (size)
;;;     g_find_program_in_path (program)
;;;     g_bit_nth_lsf (mask nth_bit)
;;;     g_bit_nth_msf (mask nth_bit)
;;;     g_bit_storage (number)
;;;     g_spaced_primes_closest (num)
;;;     g_atexit (func)
;;;     g_parse_debug_string (string keys nkeys)
;;;     GDebugKey
;;;     g_qsort_with_data (pbase total_elems size compare_func user_data)
;;;     g_nullify_pointer (nullify_location)
;;; 
;;; Description
;;; 
;;; These are portable utility functions.
;;; ----------------------------------------------------------------------------

(in-package :glib)

;;; ----------------------------------------------------------------------------
;;; g_get_application_name ()
;;; 
;;; const gchar * g_get_application_name (void)
;;; 
;;; Gets a human-readable name for the application, as set by
;;; g_set_application_name(). This name should be localized if possible, and is
;;; intended for display to the user. Contrast with g_get_prgname(), which gets
;;; a non-localized name. If g_set_application_name() has not been called,
;;; returns the result of g_get_prgname() (which may be NULL if g_set_prgname()
;;; has also not been called).
;;; 
;;; Returns :
;;; 	human-readable application name. may return NULL
;;; 
;;; Since 2.2
;;; ----------------------------------------------------------------------------

(defcfun ("g_get_application_name" g-get-application-name) :string)

(export 'g-get-application-name)

;;; ----------------------------------------------------------------------------
;;; g_set_application_name ()
;;; 
;;; void g_set_application_name (const gchar *application_name)
;;; 
;;; Sets a human-readable name for the application. This name should be
;;; localized if possible, and is intended for display to the user. Contrast
;;; with g_set_prgname(), which sets a non-localized name. g_set_prgname() will
;;; be called automatically by gtk_init(), but g_set_application_name() will
;;; not.
;;; 
;;; Note that for thread safety reasons, this function can only be called once.
;;; 
;;; The application name will be used in contexts such as error messages, or
;;; when displaying an application's name in the task list.
;;; 
;;; application_name :
;;; 	localized name of the application
;;; 
;;; Since 2.2
;;; ----------------------------------------------------------------------------

(defcfun ("g_set_application_name" g-set-application-name) :void
    (application_name :string))

(export 'g-set-application-name)

;;; ----------------------------------------------------------------------------
;;; g_get_prgname ()
;;; 
;;; gchar * g_get_prgname (void)
;;; 
;;; Gets the name of the program. This name should not be localized, contrast
;;; with g_get_application_name(). (If you are using GDK or GTK+ the program
;;; name is set in gdk_init(), which is called by gtk_init(). The program name
;;; is found by taking the last component of argv[0].)
;;; 
;;; Returns :
;;; 	the name of the program. The returned string belongs to GLib and must
;;;     not be modified or freed.
;;; ----------------------------------------------------------------------------

(defcfun ("g_get_prgname" g-get-prgname) :string)

(export 'g-get-prgname)

;;; ----------------------------------------------------------------------------
;;; g_set_prgname ()
;;; 
;;; void g_set_prgname (const gchar *prgname);
;;; 
;;; Sets the name of the program. This name should not be localized, contrast
;;; with g_set_application_name(). Note that for thread-safety reasons this
;;; function can only be called once.
;;; 
;;; prgname :
;;; 	the name of the program.
;;; ----------------------------------------------------------------------------

(defcfun ("g_set_prgname" g-set-prgname) :void
  (prgname :string))

(export 'g-set-prgname)

;;; ----------------------------------------------------------------------------
;;; g_get_environ ()
;;; 
;;; gchar ** g_get_environ (void);
;;; 
;;; Gets the list of environment variables for the current process. The list is
;;; NULL terminated and each item in the list is of the form 'NAME=VALUE'.
;;; 
;;; This is equivalent to direct access to the 'environ' global variable,
;;; except portable.
;;; 
;;; The return value is freshly allocated and it should be freed with
;;; g_strfreev() when it is no longer needed.
;;; 
;;; Returns :
;;; 	the list of environment variables.
;;; 
;;; Since 2.28
;;; ----------------------------------------------------------------------------

;;; *** NOT IMPLEMENTED ***

;;; ----------------------------------------------------------------------------
;;; g_getenv ()
;;; 
;;; const gchar * g_getenv (const gchar *variable);
;;; 
;;; Returns the value of an environment variable. The name and value are in the
;;; GLib file name encoding. On UNIX, this means the actual bytes which might or
;;; might not be in some consistent character set and encoding. On Windows, it
;;; is in UTF-8. On Windows, in case the environment variable's value contains
;;; references to other environment variables, they are expanded.
;;; 
;;; variable :
;;; 	the environment variable to get, in the GLib file name encoding.
;;; 
;;; Returns :
;;; 	the value of the environment variable, or NULL if the environment
;;;     variable is not found. The returned string may be overwritten by the
;;;     next call to g_getenv(), g_setenv() or g_unsetenv().
;;; ----------------------------------------------------------------------------

(defcfun ("g_getenv" g-getenv) :string
  (variable :string))

(export 'g-getenv)

;;; ----------------------------------------------------------------------------
;;; g_setenv ()
;;; 
;;; gboolean g_setenv (const gchar *variable,
;;;                    const gchar *value,
;;;                    gboolean overwrite);
;;; 
;;; Sets an environment variable. Both the variable's name and value should be
;;; in the GLib file name encoding. On UNIX, this means that they can be any
;;; sequence of bytes. On Windows, they should be in UTF-8.
;;; 
;;; Note that on some systems, when variables are overwritten, the memory used
;;; for the previous variables and its value isn't reclaimed.
;;; 
;;; variable :
;;; 	the environment variable to set, must not contain '='.
;;; 
;;; value :
;;; 	the value for to set the variable to.
;;; 
;;; overwrite :
;;; 	whether to change the variable if it already exists.
;;; 
;;; Returns :
;;; 	FALSE if the environment variable couldn't be set.
;;; 
;;; Since 2.4
;;; ----------------------------------------------------------------------------

(defcfun ("g_setenv" g-setenv) :boolean
  (variable :string)
  (value :string)
  (overwrite :boolean))

(export 'g-setenv)

;;; ----------------------------------------------------------------------------
;;; g_unsetenv ()
;;; 
;;; void g_unsetenv (const gchar *variable);
;;; 
;;; Removes an environment variable from the environment.
;;; 
;;; Note that on some systems, when variables are overwritten, the memory used
;;; for the previous variables and its value isn't reclaimed. Furthermore, this
;;; function can't be guaranteed to operate in a threadsafe way.
;;; 
;;; variable :
;;; 	the environment variable to remove, must not contain '='.
;;; 
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; *** NOT IMPLEMENTED ***

;;; ----------------------------------------------------------------------------
;;; g_listenv ()
;;; 
;;; gchar ** g_listenv (void);
;;; 
;;; Gets the names of all variables set in the environment.
;;; 
;;; Returns :
;;; 	a NULL-terminated list of strings which must be freed with g_strfreev().
;;;     Programs that want to be portable to Windows should typically use this
;;;     function and g_getenv() instead of using the environ array from the C
;;;     library directly. On Windows, the strings in the environ array are in
;;;     system codepage encoding, while in most of the typical use cases for
;;;     environment variables in GLib-using programs you want the UTF-8 encoding
;;;     that this function and g_getenv() provide.
;;; 
;;; Since 2.8
;;; ----------------------------------------------------------------------------

(defcfun ("g_listenv" g-listenv) g-strv)

(export 'g-listenv)

;;; ----------------------------------------------------------------------------
;;; g_get_user_name ()
;;; 
;;; const gchar * g_get_user_name (void);
;;; 
;;; Gets the user name of the current user. The encoding of the returned string
;;; is system-defined. On UNIX, it might be the preferred file name encoding, or
;;; something else, and there is no guarantee that it is even consistent on a
;;; machine. On Windows, it is always UTF-8.
;;; 
;;; Returns :
;;; 	the user name of the current user.
;;; ----------------------------------------------------------------------------

(defcfun ("g_get_user_name" g-get-user-name) :string)

(export 'g-get-user-name)

;;; ----------------------------------------------------------------------------
;;; g_get_real_name ()
;;; 
;;; const gchar * g_get_real_name (void);
;;; 
;;; Gets the real name of the user. This usually comes from the user's entry
;;; in the passwd file. The encoding of the returned string is system-defined.
;;; (On Windows, it is, however, always UTF-8.) If the real user name cannot be
;;; determined, the string "Unknown" is returned.
;;; 
;;; Returns :
;;; 	the user's real name.
;;; ----------------------------------------------------------------------------

(defcfun ("g_get_real_name" g-get-real-name) :string)

(export 'g-get-real-name)

;;; ----------------------------------------------------------------------------
;;; g_get_user_cache_dir ()
;;; 
;;; const gchar * g_get_user_cache_dir (void)
;;; 
;;; Returns a base directory in which to store non-essential, cached data
;;; specific to particular user.
;;; 
;;; On UNIX platforms this is determined using the mechanisms described in the
;;; XDG Base Directory Specification. In this case the directory retrieved will
;;; be XDG_CACHE_HOME.
;;; 
;;; On Windows is the directory that serves as a common repository for temporary
;;; Internet files. A typical path is C:\Documents and Settings\username\Local
;;; Settings\Temporary Internet Files. See documentation for
;;; CSIDL_INTERNET_CACHE.
;;; 
;;; Returns :
;;; 	a string owned by GLib that must not be modified or freed.
;;; 
;;; Since 2.6
;;; ----------------------------------------------------------------------------

(defcfun ("g_get_user_cache_dir" g-get-user-cache-dir) :string)

(export 'g-get-user-cache-dir)

;;; ----------------------------------------------------------------------------
;;; g_get_user_data_dir ()
;;; 
;;; const gchar * g_get_user_data_dir (void);
;;; 
;;; Returns a base directory in which to access application data such as icons
;;; that is customized for a particular user.
;;; 
;;; On UNIX platforms this is determined using the mechanisms described in the
;;; XDG Base Directory Specification. In this case the directory retrieved will
;;; be XDG_DATA_HOME.
;;; 
;;; On Windows this is the folder to use for local (as opposed to roaming)
;;; application data. See documentation for CSIDL_LOCAL_APPDATA. Note that on
;;; Windows it thus is the same as what g_get_user_config_dir() returns.
;;; 
;;; Returns :
;;; 	a string owned by GLib that must not be modified or freed.
;;; 
;;; Since 2.6
;;; ----------------------------------------------------------------------------

(defcfun ("g_get_user_data_dir" g-get-user-data-dir) :string)

(export 'g-get-user-data-dir)

;;; ----------------------------------------------------------------------------
;;; g_get_user_config_dir ()
;;; 
;;; const gchar * g_get_user_config_dir (void)
;;; 
;;; Returns a base directory in which to store user-specific application
;;; configuration information such as user preferences and settings.
;;; 
;;; On UNIX platforms this is determined using the mechanisms described in the
;;; XDG Base Directory Specification. In this case the directory retrieved will
;;; be XDG_CONFIG_HOME.
;;; 
;;; On Windows this is the folder to use for local (as opposed to roaming)
;;; application data. See documentation for CSIDL_LOCAL_APPDATA. Note that on
;;; Windows it thus is the same as what g_get_user_data_dir() returns.
;;; 
;;; Returns :
;;; 	a string owned by GLib that must not be modified or freed.
;;; 
;;; Since 2.6
;;; ----------------------------------------------------------------------------

(defcfun ("g_get_user_config_dir" g-get-user-config-dir) :string)

(export 'g-get-user-config-dir)

;;; ----------------------------------------------------------------------------
;;; g_get_user_runtime_dir ()
;;; 
;;; const gchar * g_get_user_runtime_dir (void);
;;; 
;;; Returns a directory that is unique to the current user on the local system.
;;; 
;;; On UNIX platforms this is determined using the mechanisms described in the
;;; XDG Base Directory Specification. This is the directory specified in the
;;; XDG_RUNTIME_DIR environment variable. In the case that this variable is not
;;; set, GLib will issue a warning message to stderr and return the value of
;;; g_get_user_cache_dir().
;;; 
;;; On Windows this is the folder to use for local (as opposed to roaming)
;;; application data. See documentation for CSIDL_LOCAL_APPDATA. Note that on
;;; Windows it thus is the same as what g_get_user_config_dir() returns.
;;; 
;;; Returns :
;;; 	a string owned by GLib that must not be modified or freed.
;;; 
;;; Since 2.28
;;; ----------------------------------------------------------------------------

;;; *** NOT IMPLEMENTED ***

;;; ----------------------------------------------------------------------------
;;; enum GUserDirectory
;;; 
;;; typedef enum {
;;;   G_USER_DIRECTORY_DESKTOP,
;;;   G_USER_DIRECTORY_DOCUMENTS,
;;;   G_USER_DIRECTORY_DOWNLOAD,
;;;   G_USER_DIRECTORY_MUSIC,
;;;   G_USER_DIRECTORY_PICTURES,
;;;   G_USER_DIRECTORY_PUBLIC_SHARE,
;;;   G_USER_DIRECTORY_TEMPLATES,
;;;   G_USER_DIRECTORY_VIDEOS,
;;; 
;;;   G_USER_N_DIRECTORIES
;;; } GUserDirectory;
;;; 
;;; These are logical ids for special directories which are defined depending
;;; on the platform used. You should use g_get_user_special_dir() to retrieve
;;; the full path associated to the logical id.
;;; 
;;; The GUserDirectory enumeration can be extended at later date. Not every
;;; platform has a directory for every logical id in this enumeration.
;;; 
;;; G_USER_DIRECTORY_DESKTOP
;;; 	the user's Desktop directory
;;; 
;;; G_USER_DIRECTORY_DOCUMENTS
;;; 	the user's Documents directory
;;; 
;;; G_USER_DIRECTORY_DOWNLOAD
;;; 	the user's Downloads directory
;;; 
;;; G_USER_DIRECTORY_MUSIC
;;; 	the user's Music directory
;;; 
;;; G_USER_DIRECTORY_PICTURES
;;; 	the user's Pictures directory
;;; 
;;; G_USER_DIRECTORY_PUBLIC_SHARE
;;; 	the user's shared directory
;;; 
;;; G_USER_DIRECTORY_TEMPLATES
;;; 	the user's Templates directory
;;; 
;;; G_USER_DIRECTORY_VIDEOS
;;; 	the user's Movies directory
;;; 
;;; G_USER_N_DIRECTORIES
;;; 	the number of enum values
;;; 
;;; Since 2.14
;;; ----------------------------------------------------------------------------

(defcenum g-user-directory
  :desktop
  :documents
  :download
  :music
  :pictures
  :public-share
  :templates
  :videos
  :n-directories)

(export 'g-user-directory)

;;; ----------------------------------------------------------------------------
;;; g_get_user_special_dir ()
;;; 
;;; const gchar * g_get_user_special_dir (GUserDirectory directory);
;;; 
;;; Returns the full path of a special directory using its logical id.
;;; 
;;; On Unix this is done using the XDG special user directories. For
;;; compatibility with existing practise, G_USER_DIRECTORY_DESKTOP falls back
;;; to $HOME/Desktop when XDG special user directories have not been set up.
;;; 
;;; Depending on the platform, the user might be able to change the path of the
;;; special directory without requiring the session to restart; GLib will not
;;; reflect any change once the special directories are loaded.
;;; 
;;; directory :
;;; 	the logical id of special directory
;;; 
;;; Returns :
;;; 	the path to the specified special directory, or NULL if the logical id
;;;     was not found. The returned string is owned by GLib and should not be
;;;     modified or freed.
;;; 
;;; Since 2.14
;;; ----------------------------------------------------------------------------

(defcfun ("g_get_user_special_dir" g-get-user-special-dir) :string
  (directory g-user-directory))

(export 'g-get-user-special-dir)

;;; ----------------------------------------------------------------------------
;;; g_get_system_data_dirs ()
;;; 
;;; const gchar * const * g_get_system_data_dirs (void);
;;; 
;;; Returns an ordered list of base directories in which to access system-wide
;;; application data.
;;; 
;;; On UNIX platforms this is determined using the mechanisms described in the
;;; XDG Base Directory Specification In this case the list of directories
;;; retrieved will be XDG_DATA_DIRS.
;;; 
;;; On Windows the first elements in the list are the Application Data and
;;; Documents folders for All Users. (These can be determined only on Windows
;;; 2000 or later and are not present in the list on other Windows versions.)
;;; See documentation for CSIDL_COMMON_APPDATA and CSIDL_COMMON_DOCUMENTS.
;;; 
;;; Then follows the "share" subfolder in the installation folder for the
;;; package containing the DLL that calls this function, if it can be
;;; determined.
;;; 
;;; Finally the list contains the "share" subfolder in the installation folder
;;; for GLib, and in the installation folder for the package the application's
;;; .exe file belongs to.
;;; 
;;; The installation folders above are determined by looking up the folder
;;; where the module (DLL or EXE) in question is located. If the folder's name
;;; is "bin", its parent is used, otherwise the folder itself.
;;; 
;;; Note that on Windows the returned list can vary depending on where this
;;; function is called.
;;; 
;;; Returns :
;;; 	a NULL-terminated array of strings owned by GLib that must not be
;;;     modified or freed.
;;; 
;;; Since 2.6
;;; ----------------------------------------------------------------------------

(defcfun ("g_get_system_data_dirs" g-get-system-data-dirs) g-strv)

(export 'g-get-system-data-dirs)

;;; ----------------------------------------------------------------------------
;;; g_get_system_config_dirs ()
;;; 
;;; const gchar * const * g_get_system_config_dirs (void);
;;; 
;;; Returns an ordered list of base directories in which to access system-wide
;;; configuration information.
;;; 
;;; On UNIX platforms this is determined using the mechanisms described in the
;;; XDG Base Directory Specification. In this case the list of directories
;;; retrieved will be XDG_CONFIG_DIRS.
;;; 
;;; On Windows is the directory that contains application data for all users. A
;;; typical path is C:\Documents and Settings\All Users\Application Data. This
;;; folder is used for application data that is not user specific. For example,
;;; an application can store a spell-check dictionary, a database of clip art,
;;; or a log file in the CSIDL_COMMON_APPDATA folder. This information will not
;;; roam and is available to anyone using the computer.
;;; 
;;; Returns :
;;; 	a NULL-terminated array of strings owned by GLib that must not be
;;;     modified or freed.
;;; 
;;; Since 2.6
;;; ----------------------------------------------------------------------------

(defcfun ("g_get_system_config_dirs" g-get-system-config-dirs) g-strv)

(export 'g-get-system-config-dirs)

;;; ----------------------------------------------------------------------------
;;; g_reload_user_special_dirs_cache ()
;;; 
;;; void g_reload_user_special_dirs_cache (void);
;;; 
;;; Resets the cache used for g_get_user_special_dir(), so that the latest
;;; on-disk version is used. Call this only if you just changed the data on disk
;;; yourself.
;;; 
;;; Due to threadsafety issues this may cause leaking of strings that were
;;; previously returned from g_get_user_special_dir() that can't be freed. We
;;; ensure to only leak the data for the directories that actually changed
;;; value though.
;;; 
;;; Since 2.22
;;; ----------------------------------------------------------------------------

;;; *** NOT IMPLEMENTED ***

;;; ----------------------------------------------------------------------------
;;; g_get_host_name ()
;;; 
;;; const gchar * g_get_host_name (void);
;;; 
;;; Return a name for the machine.
;;; 
;;; The returned name is not necessarily a fully-qualified domain name, or even
;;; present in DNS or some other name service at all. It need not even be unique
;;; on your local network or site, but usually it is. Callers should not rely on
;;; the return value having any specific properties like uniqueness for security
;;; purposes. Even if the name of the machine is changed while an application is
;;; running, the return value from this function does not change. The returned
;;; string is owned by GLib and should not be modified or freed. If no name
;;; can be determined, a default fixed string "localhost" is returned.
;;; 
;;; Returns :
;;; 	the host name of the machine.
;;; 
;;; Since 2.8
;;; ----------------------------------------------------------------------------

(defcfun ("g_get_host_name" g-get-host-name) :string)

(export 'g-get-host-name)

;;; ----------------------------------------------------------------------------
;;; g_get_home_dir ()
;;; 
;;; const gchar * g_get_home_dir (void);
;;; 
;;; Gets the current user's home directory as defined in the password database.
;;; 
;;; Note that in contrast to traditional UNIX tools, this function prefers
;;; passwd entries over the HOME environment variable.
;;; 
;;; One of the reasons for this decision is that applications in many cases
;;; need special handling to deal with the case where HOME is
;;;
;;; Not owned by the user
;;; Not writeable
;;; Not even readable
;;; 
;;; Since applications are in general not written to deal with these situations
;;; it was considered better to make g_get_home_dir() not pay attention to HOME
;;; and to return the real home directory for the user. If applications want to
;;; pay attention to HOME, they can do:
;;; 
;;;  1 const char *homedir = g_getenv ("HOME");
;;;  2 if (!homedir)
;;;  3     homedir = g_get_home_dir ();
;;; 
;;; Returns :
;;; 	the current user's home directory
;;; ----------------------------------------------------------------------------

(defcfun ("g_get_home_dir" g-get-home-dir) :string)

(export 'g-get-home-dir)

;;; ----------------------------------------------------------------------------
;;; g_get_tmp_dir ()
;;; 
;;; const gchar * g_get_tmp_dir (void);
;;; 
;;; Gets the directory to use for temporary files. This is found from inspecting
;;; the environment variables TMPDIR, TMP, and TEMP in that order. If none of
;;; those are defined "/tmp" is returned on UNIX and "C:\" on Windows. The
;;; encoding of the returned string is system-defined. On Windows, it is always
;;; UTF-8. The return value is never NULL or the empty string.
;;; 
;;; Returns :
;;; 	the directory to use for temporary files.
;;; ----------------------------------------------------------------------------

(defcfun ("g_get_tmp_dir" g-get-tmp-dir) :string)

(export 'g-get-tmp-dir)

;;; ----------------------------------------------------------------------------
;;; g_get_current_dir ()
;;; 
;;; gchar * g_get_current_dir (void);
;;; 
;;; Gets the current directory. The returned string should be freed when no
;;; longer needed. The encoding of the returned string is system defined. On
;;; Windows, it is always UTF-8.
;;; 
;;; Returns :
;;; 	the current directory.
;;; ----------------------------------------------------------------------------

(defcfun ("g_get_current_dir" g-get-current-dir) :string)

(export 'g-get-current-dir)

;;; ----------------------------------------------------------------------------
;;; g_basename ()
;;; 
;;; const gchar * g_basename (const gchar *file_name);
;;; 
;;; Warning
;;; 
;;; g_basename has been deprecated since version 2.2 and should not be used
;;; in newly-written code. Use g_path_get_basename() instead, but notice that
;;; g_path_get_basename() allocates new memory for the returned string, unlike
;;; this function which returns a pointer into the argument.
;;; 
;;; Gets the name of the file without any leading directory components. It
;;; returns a pointer into the given file name string.
;;; 
;;; file_name :
;;; 	the name of the file.
;;; 
;;; Returns :
;;; 	the name of the file without any leading directory components.
;;; ----------------------------------------------------------------------------

;;; *** NOT IMPLEMENTED ***

;;; ----------------------------------------------------------------------------
;;; g_dirname
;;; 
;;; #define g_dirname
;;; 
;;; Warning
;;; 
;;; g_dirname is deprecated and should not be used in newly-written code. use
;;; g_path_get_dirname() instead
;;; 
;;; Gets the directory components of a file name. If the file name has no
;;; directory components "." is returned. The returned string should be freed
;;; when no longer needed.
;;; 
;;; file_name :
;;; 	the name of the file
;;; 
;;; Returns :
;;; 	the directory components of the file
;;; ----------------------------------------------------------------------------

;;; *** NOT IMPLEMENTED ***

;;; ----------------------------------------------------------------------------
;;; g_path_is_absolute ()
;;; 
;;; gboolean g_path_is_absolute (const gchar *file_name);
;;; 
;;; Returns TRUE if the given file_name is an absolute file name. Note that
;;; this is a somewhat vague concept on Windows.
;;; 
;;; On POSIX systems, an absolute file name is well-defined. It always starts
;;; from the single root directory. For example "/usr/local".
;;; 
;;; On Windows, the concepts of current drive and drive-specific current
;;; directory introduce vagueness. This function interprets as an absolute file
;;; name one that either begins with a directory separator such as "\Users\tml"
;;; or begins with the root on a drive, for example "C:\Windows". The first case
;;; also includes UNC paths such as "\\myserver\docs\foo". In all cases, either
;;; slashes or backslashes are accepted.
;;; 
;;; Note that a file name relative to the current drive root does not truly
;;; specify a file uniquely over time and across processes, as the current drive
;;; is a per-process value and can be changed.
;;; 
;;; File names relative the current directory on some specific drive, such as
;;; "D:foo/bar", are not interpreted as absolute by this function, but they
;;; obviously are not relative to the normal current directory as returned by
;;; getcwd() or g_get_current_dir() either. Such paths should be avoided, or
;;; need to be handled using Windows-specific code.
;;; 
;;; file_name :
;;; 	a file name.
;;; 
;;; Returns :
;;; 	TRUE if file_name is absolute.
;;; ----------------------------------------------------------------------------

(defcfun ("g_path_is_absolute" g-path-is-absolute) :boolean
  (file-name :string))

(export 'g-path-is-absolute)

;;; ----------------------------------------------------------------------------
;;; g_path_skip_root ()
;;; 
;;; const gchar * g_path_skip_root (const gchar *file_name);
;;; 
;;; Returns a pointer into file_name after the root component, i.e. after the
;;; "/" in UNIX or "C:\" under Windows. If file_name is not an absolute path
;;; it returns NULL.
;;; 
;;; file_name :
;;; 	a file name.
;;; 
;;; Returns :
;;; 	a pointer into file_name after the root component.
;;; ----------------------------------------------------------------------------

;;; *** NOT IMPLEMENTED ***

;;; ----------------------------------------------------------------------------
;;; g_path_get_basename ()
;;; 
;;; gchar * g_path_get_basename (const gchar *file_name);
;;; 
;;; Gets the last component of the filename. If file_name ends with a directory
;;; separator it gets the component before the last slash. If file_name consists
;;; only of directory separators (and on Windows, possibly a drive letter), a
;;; single separator is returned. If file_name is empty, it gets ".".
;;; 
;;; file_name :
;;; 	the name of the file.
;;; 
;;; Returns :
;;; 	a newly allocated string containing the last component of the filename.
;;; ----------------------------------------------------------------------------

;;; *** NOT IMPLEMENTED ***

;;; ----------------------------------------------------------------------------
;;; g_path_get_dirname ()
;;; 
;;; gchar * g_path_get_dirname (const gchar *file_name);
;;; 
;;; Gets the directory components of a file name. If the file name has no
;;; directory components "." is returned. The returned string should be freed
;;; when no longer needed.
;;; 
;;; file_name :
;;; 	the name of the file.
;;; 
;;; Returns :
;;; 	the directory components of the file.
;;; ----------------------------------------------------------------------------

;;; *** NOT IMPLEMENTED ***

;;; ----------------------------------------------------------------------------
;;; g_build_filename ()
;;; 
;;; gchar * g_build_filename (const gchar *first_element, ...)
;;; 
;;; Creates a filename from a series of elements using the correct separator
;;; for filenames.
;;; 
;;; On Unix, this function behaves identically to
;;; g_build_path (G_DIR_SEPARATOR_S, first_element, ....).
;;; 
;;; On Windows, it takes into account that either the backslash (\ or slash (/)
;;; can be used as separator in filenames, but otherwise behaves as on Unix.
;;; When file pathname separators need to be inserted, the one that last
;;; previously occurred in the parameters (reading from left to right) is used.
;;; 
;;; No attempt is made to force the resulting filename to be an absolute path.
;;; If the first element is a relative path, the result will be a relative path.
;;; 
;;; first_element :
;;; 	the first element in the path
;;; 
;;; ... :
;;; 	remaining elements in path, terminated by NULL
;;; 
;;; Returns :
;;; 	a newly-allocated string that must be freed with g_free().
;;; ----------------------------------------------------------------------------

(defun g-build-filename (&rest args)
  (let* ((n (length args))
         (arr (g-malloc (* (1+ n) (foreign-type-size :pointer)))))
    (iter (for i from 0)
          (for arg in args)
          (setf (mem-aref arr :pointer i) (g-strdup arg)))
    (setf (mem-aref arr :pointer n) (null-pointer))
    (prog1
      (g-build-filenamev arr)
      (iter (for i from 0)
            (for str-ptr = (mem-aref arr :pointer i))
            (until (null-pointer-p str-ptr))
            (g-free str-ptr))
      (g-free arr))))

(export 'g-build-filename)

;;; ----------------------------------------------------------------------------
;;; g_build_filenamev ()
;;; 
;;; gchar * g_build_filenamev (gchar **args)
;;; 
;;; Behaves exactly like g_build_filename(), but takes the path elements as a
;;; string array, instead of varargs. This function is mainly meant for
;;; language bindings.
;;; 
;;; args :
;;; 	NULL-terminated array of strings containing the path elements.
;;; 
;;; Returns :
;;; 	a newly-allocated string that must be freed with g_free().
;;; 
;;; Since 2.8
;;; ----------------------------------------------------------------------------

(defcfun ("g_build_filenamev" g-build-filenamev) (:string :free-from-foreign t)
  (args :pointer))

(export 'g-build-filenamev)

;;; ----------------------------------------------------------------------------
;;; g_build_path ()
;;; 
;;; gchar * g_build_path (const gchar *separator,
;;;                       const gchar *first_element,
;;;                       ...);
;;; 
;;; Creates a path from a series of elements using separator as the separator
;;; between elements. At the boundary between two elements, any trailing
;;; occurrences of separator in the first element, or leading occurrences of
;;; separator in the second element are removed and exactly one copy of the
;;; separator is inserted.
;;; 
;;; Empty elements are ignored.
;;; 
;;; The number of leading copies of the separator on the result is the same as
;;; the number of leading copies of the separator on the first non-empty
;;; element.
;;; 
;;; The number of trailing copies of the separator on the result is the same as
;;; the number of trailing copies of the separator on the last non-empty
;;; element. (Determination of the number of trailing copies is done without
;;; stripping leading copies, so if the separator is ABA, ABABA has 1 trailing
;;; copy.)
;;; 
;;; However, if there is only a single non-empty element, and there are no
;;; characters in that element not part of the leading or trailing separators,
;;; then the result is exactly the original value of that element.
;;; 
;;; Other than for determination of the number of leading and trailing copies
;;; of the separator, elements consisting only of copies of the separator are
;;; ignored.
;;; 
;;; separator :
;;; 	a string used to separator the elements of the path.
;;; 
;;; first_element :
;;; 	the first element in the path
;;; 
;;; ... :
;;; 	remaining elements in path, terminated by NULL
;;; 
;;; Returns :
;;; 	a newly-allocated string that must be freed with g_free().
;;; ----------------------------------------------------------------------------

;;; *** NOT IMPLEMENTED ***

;;; ----------------------------------------------------------------------------
;;; g_build_pathv ()
;;; 
;;; gchar * g_build_pathv (const gchar *separator, gchar **args)
;;; 
;;; Behaves exactly like g_build_path(), but takes the path elements as a
;;; string array, instead of varargs. This function is mainly meant for language
;;; bindings.
;;; 
;;; separator :
;;; 	a string used to separator the elements of the path.
;;; 
;;; args :
;;; 	NULL-terminated array of strings containing the path elements.
;;; 
;;; Returns :
;;; 	a newly-allocated string that must be freed with g_free().
;;; 
;;; Since 2.8
;;; ----------------------------------------------------------------------------

;;; *** NOT IMPLEMENTED ***

;;; ----------------------------------------------------------------------------
;;; g_format_size ()
;;; 
;;; gchar * g_format_size (guint64 size);
;;; 
;;; Formats a size (for example the size of a file) into a human readable
;;; string. Sizes are rounded to the nearest size prefix (kB, MB, GB) and are
;;; displayed rounded to the nearest tenth. E.g. the file size 3292528 bytes
;;; will be converted into the string "3.2 MB".
;;; 
;;; The prefix units base is 1000 (i.e. 1 kB is 1000 bytes).
;;; 
;;; This string should be freed with g_free() when not needed any longer.
;;; 
;;; See g_format_size_full() for more options about how the size might be
;;; formatted.
;;; 
;;; size :
;;; 	a size in bytes
;;; 
;;; Returns :
;;; 	a newly-allocated formatted string containing a human readable file
;;;     size.
;;; 
;;; Since 2.30
;;; ----------------------------------------------------------------------------

;;; *** NOT IMPLEMENTED ***

;;; ----------------------------------------------------------------------------
;;; enum GFormatSizeFlags
;;; 
;;; typedef enum {
;;;   G_FORMAT_SIZE_DEFAULT     = 0,
;;;   G_FORMAT_SIZE_LONG_FORMAT = 1 << 0,
;;;   G_FORMAT_SIZE_IEC_UNITS   = 1 << 1
;;; } GFormatSizeFlags;
;;; 
;;; Flags to modify the format of the string returned by g_format_size_full().
;;; 
;;; G_FORMAT_SIZE_DEFAULT
;;; 	behave the same as g_format_size()
;;; 
;;; G_FORMAT_SIZE_LONG_FORMAT
;;; 	include the exact number of bytes as part of the returned string. For
;;;     example, "45.6 kB (45,612 bytes)".
;;; 
;;; G_FORMAT_SIZE_IEC_UNITS
;;; 	use IEC (base 1024) units with "KiB"-style suffixes. IEC units should
;;;     only be used for reporting things with a strong "power of 2" basis,
;;;     like RAM sizes or RAID stripe sizes. Network and storage sizes should
;;;     be reported in the normal SI units.
;;; ----------------------------------------------------------------------------

;;; *** NOT IMPLEMENTED ***

;;; ----------------------------------------------------------------------------
;;; g_format_size_full ()
;;; 
;;; gchar * g_format_size_full (guint64 size, GFormatSizeFlags flags)
;;; 
;;; Formats a size.
;;; 
;;; This function is similar to g_format_size() but allows for flags that
;;; modify the output. See GFormatSizeFlags.
;;; 
;;; size :
;;; 	a size in bytes
;;; 
;;; flags :
;;; 	GFormatSizeFlags to modify the output
;;; 
;;; Returns :
;;; 	a newly-allocated formatted string containing a human readable file
;;;     size.
;;; 
;;; Since 2.30
;;; ----------------------------------------------------------------------------

;;; *** NOT IMPLEMENTED ***

;;; ----------------------------------------------------------------------------
;;; g_format_size_for_display ()
;;; 
;;; char * g_format_size_for_display (goffset size)
;;; 
;;; Warning
;;; 
;;; g_format_size_for_display has been deprecated since version 2.30 and should
;;; not be used in newly-written code. This function is broken due to its use of
;;; SI suffixes to denote IEC units. Use g_format_size() instead.
;;; 
;;; Formats a size (for example the size of a file) into a human readable
;;; string. Sizes are rounded to the nearest size prefix (KB, MB, GB) and are
;;; displayed rounded to the nearest tenth. E.g. the file size 3292528 bytes
;;; will be converted into the string "3.1 MB".
;;; 
;;; The prefix units base is 1024 (i.e. 1 KB is 1024 bytes).
;;; 
;;; This string should be freed with g_free() when not needed any longer.
;;; 
;;; size :
;;; 	a size in bytes.
;;; 
;;; Returns :
;;; 	a newly-allocated formatted string containing a human readable file
;;;     size.
;;; 
;;; Since 2.16
;;; ----------------------------------------------------------------------------

;;; *** NOT IMPLEMENTED ***

;;; ----------------------------------------------------------------------------
;;; g_find_program_in_path ()
;;; 
;;; gchar * g_find_program_in_path (const gchar *program)
;;; 
;;; Locates the first executable named program in the user's path, in the same
;;; way that execvp() would locate it. Returns an allocated string with the
;;; absolute path name, or NULL if the program is not found in the path. If
;;; program is already an absolute path, returns a copy of program if program
;;; exists and is executable, and NULL otherwise. On Windows, if program does
;;; not have a file type suffix, tries with the suffixes .exe, .cmd, .bat and
;;; .com, and the suffixes in the PATHEXT environment variable.
;;; 
;;; On Windows, it looks for the file in the same way as CreateProcess() would.
;;; This means first in the directory where the executing program was loaded
;;; from, then in the current directory, then in the Windows 32-bit system
;;; directory, then in the Windows directory, and finally in the directories in
;;; the PATH environment variable. If the program is found, the return value
;;; contains the full name including the type suffix.
;;; 
;;; program :
;;; 	a program name in the GLib file name encoding
;;; 
;;; Returns :
;;; 	absolute path, or NULL
;;; ----------------------------------------------------------------------------

;;; *** NOT IMPLEMENTED ***

;;; ----------------------------------------------------------------------------
;;; g_bit_nth_lsf ()
;;; 
;;; gint g_bit_nth_lsf (gulong mask, gint nth_bit)
;;; 
;;; Find the position of the first bit set in mask, searching from (but not
;;; including) nth_bit upwards. Bits are numbered from 0 (least significant) to
;;; sizeof(gulong) * 8 - 1 (31 or 63, usually). To start searching from the 0th
;;; bit, set nth_bit to -1.
;;; 
;;; mask :
;;; 	a gulong containing flags
;;; 
;;; nth_bit :
;;; 	the index of the bit to start the search from
;;; 
;;; Returns :
;;; 	the index of the first bit set which is higher than nth_bit
;;; ----------------------------------------------------------------------------

;;; *** NOT IMPLEMENTED ***

;;; ----------------------------------------------------------------------------
;;; g_bit_nth_msf ()
;;; 
;;; gint g_bit_nth_msf (gulong mask, gint nth_bit)
;;; 
;;; Find the position of the first bit set in mask, searching from (but not
;;; including) nth_bit downwards. Bits are numbered from 0 (least significant)
;;; to sizeof(gulong) * 8 - 1 (31 or 63, usually). To start searching from the
;;; last bit, set nth_bit to -1 or GLIB_SIZEOF_LONG * 8.
;;; 
;;; mask :
;;; 	a gulong containing flags
;;; 
;;; nth_bit :
;;; 	the index of the bit to start the search from
;;; 
;;; Returns :
;;; 	the index of the first bit set which is lower than nth_bit
;;; ----------------------------------------------------------------------------

;;; *** NOT IMPLEMENTED ***

;;; ----------------------------------------------------------------------------
;;; g_bit_storage ()
;;; 
;;; guint g_bit_storage (gulong number)
;;; 
;;; Gets the number of bits used to hold number, e.g. if number is 4, 3 bits
;;; are needed.
;;; 
;;; number :
;;; 	a guint
;;; 
;;; Returns :
;;; 	the number of bits used to hold number
;;; ----------------------------------------------------------------------------

;;; *** NOT IMPLEMENTED ***

;;; ----------------------------------------------------------------------------
;;; g_spaced_primes_closest ()
;;; 
;;; guint g_spaced_primes_closest (guint num)
;;; 
;;; Gets the smallest prime number from a built-in array of primes which is
;;; larger than num. This is used within GLib to calculate the optimum size of a
;;; GHashTable.
;;; 
;;; The built-in array of primes ranges from 11 to 13845163 such that each prime
;;; is approximately 1.5-2 times the previous prime.
;;; 
;;; num :
;;; 	a guint
;;; 
;;; Returns :
;;; 	the smallest prime number from a built-in array of primes which is
;;;     larger than num
;;; ----------------------------------------------------------------------------

;;; *** NOT IMPLEMENTED ***

;;; ----------------------------------------------------------------------------
;;; g_atexit ()
;;; 
;;; void g_atexit (GVoidFunc func)
;;; 
;;; Specifies a function to be called at normal program termination.
;;; 
;;; Since GLib 2.8.2, on Windows g_atexit() actually is a preprocessor macro
;;; that maps to a call to the atexit() function in the C library. This means
;;; that in case the code that calls g_atexit(), i.e. atexit(), is in a DLL, the
;;; function will be called when the DLL is detached from the program. This
;;; typically makes more sense than that the function is called when the GLib
;;; DLL is detached, which happened earlier when g_atexit() was a function in
;;; the GLib DLL.
;;; 
;;; The behaviour of atexit() in the context of dynamically loaded modules is
;;; not formally specified and varies wildly.
;;; 
;;; On POSIX systems, calling g_atexit() (or atexit()) in a dynamically loaded
;;; module which is unloaded before the program terminates might well cause a
;;; crash at program exit.
;;; 
;;; Some POSIX systems implement atexit() like Windows, and have each
;;; dynamically loaded module maintain an own atexit chain that is called when
;;; the module is unloaded.
;;; 
;;; On other POSIX systems, before a dynamically loaded module is unloaded, the
;;; registered atexit functions (if any) residing in that module are called,
;;; regardless where the code that registered them resided. This is presumably
;;; the most robust approach.
;;; 
;;; As can be seen from the above, for portability it's best to avoid calling
;;; g_atexit() (or atexit()) except in the main executable of a program.
;;; 
;;; func :
;;; 	the function to call on normal program termination.
;;; ----------------------------------------------------------------------------

;;; *** NOT IMPLEMENTED ***

;;; ----------------------------------------------------------------------------
;;; g_parse_debug_string ()
;;; 
;;; guint g_parse_debug_string (const gchar *string,
;;;                             const GDebugKey *keys,
;;;                             guint nkeys)
;;; 
;;; Parses a string containing debugging options into a guint containing bit
;;; flags. This is used within GDK and GTK+ to parse the debug options passed
;;; on the command line or through environment variables.
;;; 
;;; If string is equal to "all", all flags are set. If string is equal to
;;; "help", all the available keys in keys are printed out to standard error.
;;; 
;;; string :
;;; 	a list of debug options separated by colons, spaces, or commas, or NULL.
;;; 
;;; keys :
;;; 	pointer to an array of GDebugKey which associate strings with bit flags.
;;; 
;;; nkeys :
;;; 	the number of GDebugKeys in the array.
;;; 
;;; Returns :
;;; 	the combined set of bit flags.
;;; ----------------------------------------------------------------------------

;;; *** NOT IMPLEMENTED ***

;;; ----------------------------------------------------------------------------
;;; struct GDebugKey
;;; 
;;; struct GDebugKey {
;;;   const gchar *key;
;;;   guint	       value;
;;; };
;;; 
;;; Associates a string with a bit flag. Used in g_parse_debug_string().
;;; 
;;; const gchar *key;
;;; 	the string
;;; 
;;; guint value;
;;; 	the flag
;;; ----------------------------------------------------------------------------

;;; *** NOT IMPLEMENTED ***

;;; ----------------------------------------------------------------------------
;;; GVoidFunc ()
;;; 
;;; void (*GVoidFunc) (void)
;;; 
;;; Declares a type of function which takes no arguments and has no return
;;; value. It is used to specify the type function passed to g_atexit().
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GFreeFunc ()
;;; 
;;; void (*GFreeFunc) (gpointer data)
;;; 
;;; Declares a type of function which takes an arbitrary data pointer argument
;;; and has no return value. It is not currently used in GLib or GTK+.
;;; 
;;; data :
;;; 	a data pointer
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_qsort_with_data ()
;;; 
;;; void g_qsort_with_data (gconstpointer pbase,
;;;                         gint total_elems,
;;;                         gsize size,
;;;                         GCompareDataFunc compare_func,
;;;                         gpointer user_data)
;;; 
;;; This is just like the standard C qsort() function, but the comparison
;;; routine accepts a user data argument.
;;; 
;;; pbase :
;;; 	start of array to sort
;;; 
;;; total_elems :
;;; 	elements in the array
;;; 
;;; size :
;;; 	size of each element
;;; 
;;; compare_func :
;;; 	function to compare elements
;;; 
;;; user_data :
;;; 	data to pass to compare_func
;;; ----------------------------------------------------------------------------

;;; *** NOT IMPLEMENTED ***

;;; ----------------------------------------------------------------------------
;;; g_nullify_pointer ()
;;; 
;;; void g_nullify_pointer (gpointer *nullify_location)
;;; 
;;; Set the pointer at the specified location to NULL.
;;; 
;;; nullify_location :
;;; 	the memory address of the pointer.
;;; ----------------------------------------------------------------------------

;;; *** NOT IMPLEMENTED ***

;;; --- End of file glib.utils.lisp --------------------------------------------
