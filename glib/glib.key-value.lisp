;;; ----------------------------------------------------------------------------
;;; glib.key-value.lisp
;;;
;;; The documentation of this file is taken from the GLib 2.38.2 Reference
;;; Manual and modified to document the Lisp binding to the GLib library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) Dieter Kaiser
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
;;; Key-value file parser
;;;
;;; parses .ini-like config files
;;;
;;; Synopsis
;;;
;;;     GKeyFile
;;;
;;;     G_KEY_FILE_ERROR
;;;
;;;     GKeyFileError
;;;     GKeyFileFlags
;;;
;;;     g_key_file_new
;;;     g_key_file_free
;;;     g_key_file_ref
;;;     g_key_file_unref
;;;     g_key_file_set_list_separator
;;;     g_key_file_load_from_file
;;;     g_key_file_load_from_data
;;;     g_key_file_load_from_data_dirs
;;;     g_key_file_load_from_dirs
;;;     g_key_file_to_data
;;;     g_key_file_get_start_group
;;;     g_key_file_get_groups
;;;     g_key_file_get_keys
;;;     g_key_file_has_group
;;;     g_key_file_has_key
;;;
;;;     g_key_file_get_value
;;;     g_key_file_get_string
;;;     g_key_file_get_locale_string
;;;     g_key_file_get_boolean
;;;     g_key_file_get_integer
;;;     g_key_file_get_int64
;;;     g_key_file_get_uint64
;;;     g_key_file_get_double
;;;     g_key_file_get_string_list
;;;     g_key_file_get_locale_string_list
;;;     g_key_file_get_boolean_list
;;;     g_key_file_get_integer_list
;;;     g_key_file_get_double_list
;;;     g_key_file_get_comment
;;;
;;;     g_key_file_set_value
;;;     g_key_file_set_string
;;;     g_key_file_set_locale_string
;;;     g_key_file_set_boolean
;;;     g_key_file_set_integer
;;;     g_key_file_set_int64
;;;     g_key_file_set_uint64
;;;     g_key_file_set_double
;;;     g_key_file_set_string_list
;;;     g_key_file_set_locale_string_list
;;;     g_key_file_set_boolean_list
;;;     g_key_file_set_integer_list
;;;     g_key_file_set_double_list
;;;     g_key_file_set_comment
;;;     g_key_file_remove_group
;;;     g_key_file_remove_key
;;;     g_key_file_remove_comment
;;;
;;;     G_KEY_FILE_DESKTOP_GROUP
;;;     G_KEY_FILE_DESKTOP_KEY_TYPE
;;;     G_KEY_FILE_DESKTOP_KEY_VERSION
;;;     G_KEY_FILE_DESKTOP_KEY_NAME
;;;     G_KEY_FILE_DESKTOP_KEY_GENERIC_NAME
;;;     G_KEY_FILE_DESKTOP_KEY_NO_DISPLAY
;;;     G_KEY_FILE_DESKTOP_KEY_COMMENT
;;;     G_KEY_FILE_DESKTOP_KEY_ICON
;;;     G_KEY_FILE_DESKTOP_KEY_HIDDEN
;;;     G_KEY_FILE_DESKTOP_KEY_ONLY_SHOW_IN
;;;     G_KEY_FILE_DESKTOP_KEY_NOT_SHOW_IN
;;;     G_KEY_FILE_DESKTOP_KEY_TRY_EXEC
;;;     G_KEY_FILE_DESKTOP_KEY_EXEC
;;;     G_KEY_FILE_DESKTOP_KEY_PATH
;;;     G_KEY_FILE_DESKTOP_KEY_TERMINAL
;;;     G_KEY_FILE_DESKTOP_KEY_MIME_TYPE
;;;     G_KEY_FILE_DESKTOP_KEY_CATEGORIES
;;;     G_KEY_FILE_DESKTOP_KEY_STARTUP_NOTIFY
;;;     G_KEY_FILE_DESKTOP_KEY_STARTUP_WM_CLASS
;;;     G_KEY_FILE_DESKTOP_KEY_URL
;;;     G_KEY_FILE_DESKTOP_TYPE_APPLICATION
;;;     G_KEY_FILE_DESKTOP_TYPE_LINK
;;;     G_KEY_FILE_DESKTOP_TYPE_DIRECTORY
;;; ----------------------------------------------------------------------------

(in-package :glib)

;;; ----------------------------------------------------------------------------
;;; GKeyFile
;;;
;;; The GKeyFile struct contains only private data and should not be accessed
;;; directly.
;;; ----------------------------------------------------------------------------

(defcstruct g-key-file)

#+cl-cffi-gtk-documentation
(setf (gethash 'g-key-file atdoc:*type-name-alias*) "CStruct"
      (documentation 'g-key-file 'type)
 "@version{2013-11-13}
  @begin{short}
    @sym{g-key-file} lets you parse, edit or create files containing groups of
    key-value pairs, which we call key files for lack of a better name. Several
    freedesktop.org specifications use key files now, e. g the Desktop Entry
    Specification and the Icon Theme Specification.
  @end{short}

  The syntax of key files is described in detail in the Desktop Entry
  Specification, here is a quick summary: Key files consists of groups of
  key-value pairs, interspersed with comments.
  @begin{pre}
 # this is just an example
 # there can be comments before the first group

 [First Group]

 Name=Key File Example\tthis value shows\nescaping

 # localized strings are stored in multiple key-value pairs
 Welcome=Hello
 Welcome[de]=Hallo
 Welcome[fr_FR]=Bonjour
 Welcome[it]=Ciao
 Welcome[be@@latin]=Hello

 [Another Group]

 Numbers=2;20;-200;0

 Booleans=true;false;true;true
  @end{pre}
  Lines beginning with a @code{'#'} and blank lines are considered comments.

  Groups are started by a header line containing the group name enclosed in
  @code{'['} and @code{']'}, and ended implicitly by the start of the next group
  or the end of the file. Each key-value pair must be contained in a group.

  Key-value pairs generally have the form @code{key=value}, with the exception
  of localized strings, which have the form @code{key[locale]=value}, with a
  locale identifier of the form @code{lang_COUNTRYMODIFIER} where @code{COUNTRY}
  and @code{MODIFIER} are optional. Space before and after the @code{'='}
  character are ignored. Newline, tab, carriage return and backslash characters
  in value are escaped as @code{\n}, @code{\t}, @code{\r}, and @code{\\},
  respectively. To preserve leading spaces in values, these can also be escaped
  as @code{\s}.

  Key files can store strings, possibly with localized variants, integers,
  booleans and lists of these. Lists are separated by a separator character,
  typically @code{';'} or @code{','}. To use the list separator character in a
  value in a list, it has to be escaped by prefixing it with a backslash.

  This syntax is obviously inspired by the .ini files commonly met on Windows,
  but there are some important differences:
  @begin{itemize}
    @item{.ini files use the @code{';'} character to begin comments, key files
      use the @code{'#'} character.}
    @item{Key files do not allow for ungrouped keys meaning only comments can
      precede the first group.}
    @item{Key files are always encoded in UTF-8.}
    @item{Key and Group names are case-sensitive. For example, a group called
      @code{[GROUP]} is a different from @code{[group]}.}
    @item{.ini files do not have a strongly typed boolean entry type, they only
      have @code{GetProfileInt()}. In key files, only true and false (in lower
      case) are allowed.}
  @end{itemize}
  Note that in contrast to the Desktop Entry Specification, groups in key
  files may contain the same key multiple times; the last entry wins. Key
  files may also contain multiple groups with the same name; they are merged
  together. Another difference is that keys and group names in key files are
  not restricted to ASCII characters.")

(export 'g-key-file)

;;; ----------------------------------------------------------------------------
;;; G_KEY_FILE_ERROR
;;;
;;; #define G_KEY_FILE_ERROR g_key_file_error_quark()
;;;
;;; Error domain for key file parsing. Errors in this domain will be from the
;;; GKeyFileError enumeration.
;;;
;;; See GError for information on error domains.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; enum GKeyFileError
;;;
;;; typedef enum {
;;;   G_KEY_FILE_ERROR_UNKNOWN_ENCODING,
;;;   G_KEY_FILE_ERROR_PARSE,
;;;   G_KEY_FILE_ERROR_NOT_FOUND,
;;;   G_KEY_FILE_ERROR_KEY_NOT_FOUND,
;;;   G_KEY_FILE_ERROR_GROUP_NOT_FOUND,
;;;   G_KEY_FILE_ERROR_INVALID_VALUE
;;; } GKeyFileError;
;;;
;;; Error codes returned by key file parsing.
;;;
;;; G_KEY_FILE_ERROR_UNKNOWN_ENCODING
;;;     the text being parsed was in an unknown encoding
;;;
;;; G_KEY_FILE_ERROR_PARSE
;;;     document was ill-formed
;;;
;;; G_KEY_FILE_ERROR_NOT_FOUND
;;;     the file was not found
;;;
;;; G_KEY_FILE_ERROR_KEY_NOT_FOUND
;;;     a requested key was not found
;;;
;;; G_KEY_FILE_ERROR_GROUP_NOT_FOUND
;;;     a requested group was not found
;;;
;;; G_KEY_FILE_ERROR_INVALID_VALUE
;;;     a value could not be parsed
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; enum GKeyFileFlags
;;;
;;; typedef enum {
;;;   G_KEY_FILE_NONE              = 0,
;;;   G_KEY_FILE_KEEP_COMMENTS     = 1 << 0,
;;;   G_KEY_FILE_KEEP_TRANSLATIONS = 1 << 1
;;; } GKeyFileFlags;
;;;
;;; Flags which influence the parsing.
;;;
;;; G_KEY_FILE_NONE
;;;     No flags, default behaviour
;;;
;;; G_KEY_FILE_KEEP_COMMENTS
;;;     Use this flag if you plan to write the (possibly modified) contents of
;;;     the key file back to a file; otherwise all comments will be lost when
;;;     the key file is written back.
;;;
;;; G_KEY_FILE_KEEP_TRANSLATIONS
;;;     Use this flag if you plan to write the (possibly modified) contents of
;;;     the key file back to a file; otherwise only the translations for the
;;;     current language will be written back.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_key_file_new ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_key_file_new" g-key-file-new) (:pointer (:struct g-key-file))
 #+cl-cffi-gtk-documentation
 "@version{2013-11-13}
  @return{An empty @symbol{g-key-file} object.}
  @begin{short}
    Creates a new empty @symbol{g-key-file} object.
  @end{short}
  Use the functions @fun{g-key-file-load-from-file},
  @fun{g-key-file-load-from-data}, @fun{g-key-file-load-from-dirs} or
  @fun{g-key-file-load-from-data-dirs} to read an existing key file.

  Since 2.6
  @see-symbol{g-key-file}
  @see-function{g-key-file-load-from-file}
  @see-function{g-key-file-load-from-data}
  @see-function{g-key-file-load-from-dirs}
  @see-function{g-key-file-load-from-data-dirs}")

(export 'g-key-file-new)

;;; ----------------------------------------------------------------------------
;;; g_key_file_free ()
;;;
;;; void g_key_file_free (GKeyFile *key_file);
;;;
;;; Clears all keys and groups from key_file, and decreases the reference count
;;; by 1. If the reference count reaches zero, frees the key file and all its
;;; allocated memory.
;;;
;;; key_file :
;;;     a GKeyFile
;;;
;;; Since 2.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_key_file_ref ()
;;;
;;; GKeyFile * g_key_file_ref (GKeyFile *key_file);
;;;
;;; Increases the reference count of key_file.
;;;
;;; key_file :
;;;     a GKeyFile
;;;
;;; Returns :
;;;     the same key_file.
;;;
;;; Since 2.32
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_key_file_unref ()
;;;
;;; void g_key_file_unref (GKeyFile *key_file);
;;;
;;; Decreases the reference count of key_file by 1. If the reference count
;;; reaches zero, frees the key file and all its allocated memory.
;;;
;;; key_file :
;;;     a GKeyFile
;;;
;;; Since 2.32
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_key_file_set_list_separator ()
;;;
;;; void g_key_file_set_list_separator (GKeyFile *key_file, gchar separator);
;;;
;;; Sets the character which is used to separate values in lists. Typically ';'
;;; or ',' are used as separators. The default list separator is ';'.
;;;
;;; key_file :
;;;     a GKeyFile
;;;
;;; separator :
;;;     the separator
;;;
;;; Since 2.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_key_file_load_from_file ()
;;;
;;; gboolean g_key_file_load_from_file (GKeyFile *key_file,
;;;                                     const gchar *file,
;;;                                     GKeyFileFlags flags,
;;;                                     GError **error);
;;;
;;; Loads a key file into an empty GKeyFile structure. If the file could not be
;;; loaded then error is set to either a GFileError or GKeyFileError.
;;;
;;; key_file :
;;;     an empty GKeyFile struct
;;;
;;; file :
;;;     the path of a filename to load, in the GLib filename encoding
;;;
;;; flags :
;;;     flags from GKeyFileFlags
;;;
;;; error :
;;;     return location for a GError, or NULL
;;;
;;; Returns :
;;;     TRUE if a key file could be loaded, FALSE otherwise
;;;
;;; Since 2.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_key_file_load_from_data ()
;;;
;;; gboolean g_key_file_load_from_data (GKeyFile *key_file,
;;;                                     const gchar *data,
;;;                                     gsize length,
;;;                                     GKeyFileFlags flags,
;;;                                     GError **error);
;;;
;;; Loads a key file from memory into an empty GKeyFile structure. If the object
;;; cannot be created then error is set to a GKeyFileError.
;;;
;;; key_file :
;;;     an empty GKeyFile struct
;;;
;;; data :
;;;     key file loaded in memory
;;;
;;; length :
;;;     the length of data in bytes (or -1 if data is nul-terminated)
;;;
;;; flags :
;;;     flags from GKeyFileFlags
;;;
;;; error :
;;;     return location for a GError, or NULL
;;;
;;; Returns :
;;;     TRUE if a key file could be loaded, FALSE otherwise
;;;
;;; Since 2.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_key_file_load_from_data_dirs ()
;;;
;;; gboolean g_key_file_load_from_data_dirs (GKeyFile *key_file,
;;;                                          const gchar *file,
;;;                                          gchar **full_path,
;;;                                          GKeyFileFlags flags,
;;;                                          GError **error);
;;;
;;; This function looks for a key file named file in the paths returned from
;;; g_get_user_data_dir() and g_get_system_data_dirs(), loads the file into
;;; key_file and returns the file's full path in full_path. If the file could
;;; not be loaded then an error is set to either a GFileError or GKeyFileError.
;;;
;;; key_file :
;;;     an empty GKeyFile struct
;;;
;;; file :
;;;     a relative path to a filename to open and parse. [type filename]
;;;
;;; full_path :
;;;     return location for a string containing the full path of the file, or
;;;     NULL.
;;;
;;; flags :
;;;     flags from GKeyFileFlags
;;;
;;; error :
;;;     return location for a GError, or NULL
;;;
;;; Returns :
;;;     TRUE if a key file could be loaded, FALSE othewise
;;;
;;; Since 2.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_key_file_load_from_dirs ()
;;;
;;; gboolean g_key_file_load_from_dirs (GKeyFile *key_file,
;;;                                     const gchar *file,
;;;                                     const gchar **search_dirs,
;;;                                     gchar **full_path,
;;;                                     GKeyFileFlags flags,
;;;                                     GError **error);
;;;
;;; This function looks for a key file named file in the paths specified in
;;; search_dirs, loads the file into key_file and returns the file's full path
;;; in full_path. If the file could not be loaded then an error is set to either
;;; a GFileError or GKeyFileError.
;;;
;;; key_file :
;;;     an empty GKeyFile struct
;;;
;;; file :
;;;     a relative path to a filename to open and parse
;;;
;;; search_dirs :
;;;     NULL-terminated array of directories to search
;;;
;;; full_path :
;;;     return location for a string containing the full path of the file,
;;;     or NULL
;;;
;;; flags :
;;;     flags from GKeyFileFlags
;;;
;;; error :
;;;     return location for a GError, or NULL
;;;
;;; Returns :
;;;     TRUE if a key file could be loaded, FALSE otherwise
;;;
;;; Since 2.14
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_key_file_to_data ()
;;;
;;; gchar * g_key_file_to_data (GKeyFile *key_file,
;;;                             gsize *length,
;;;                             GError **error);
;;;
;;; This function outputs key_file as a string.
;;;
;;; Note that this function never reports an error, so it is safe to pass NULL
;;; as error.
;;;
;;; key_file :
;;;     a GKeyFile
;;;
;;; length :
;;;     return location for the length of the returned string, or NULL
;;;
;;; error :
;;;     return location for a GError, or NULL
;;;
;;; Returns :
;;;     a newly allocated string holding the contents of the GKeyFile
;;;
;;; Since 2.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_key_file_get_start_group ()
;;;
;;; gchar * g_key_file_get_start_group (GKeyFile *key_file);
;;;
;;; Returns the name of the start group of the file.
;;;
;;; key_file :
;;;     a GKeyFile
;;;
;;; Returns :
;;;     The start group of the key file.
;;;
;;; Since 2.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_key_file_get_groups ()
;;;
;;; gchar ** g_key_file_get_groups (GKeyFile *key_file, gsize *length);
;;;
;;; Returns all groups in the key file loaded with key_file. The array of
;;; returned groups will be NULL-terminated, so length may optionally be NULL.
;;;
;;; key_file :
;;;     a GKeyFile
;;;
;;; length :
;;;     return location for the number of returned groups, or NULL
;;;
;;; Returns :
;;;     a newly-allocated NULL-terminated array of strings. Use g_strfreev() to
;;;     free it.
;;;
;;; Since 2.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_key_file_get_keys ()
;;;
;;; gchar ** g_key_file_get_keys (GKeyFile *key_file,
;;;                               const gchar *group_name,
;;;                               gsize *length,
;;;                               GError **error);
;;;
;;; Returns all keys for the group name group_name. The array of returned keys
;;; will be NULL-terminated, so length may optionally be NULL. In the event that
;;; the group_name cannot be found, NULL is returned and error is set to
;;; G_KEY_FILE_ERROR_GROUP_NOT_FOUND.
;;;
;;; key_file :
;;;     a GKeyFile
;;;
;;; group_name :
;;;     a group name
;;;
;;; length :
;;;     return location for the number of keys returned, or NULL
;;;
;;; error :
;;;     return location for a GError, or NULL
;;;
;;; Returns :
;;;     a newly-allocated NULL-terminated array of strings. Use g_strfreev() to
;;;     free it.
;;;
;;; Since 2.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_key_file_has_group ()
;;;
;;; gboolean g_key_file_has_group (GKeyFile *key_file, const gchar *group_name);
;;;
;;; Looks whether the key file has the group group_name.
;;;
;;; key_file :
;;;     a GKeyFile
;;;
;;; group_name :
;;;     a group name
;;;
;;; Returns :
;;;     TRUE if group_name is a part of key_file, FALSE otherwise.
;;;
;;; Since 2.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_key_file_has_key ()
;;;
;;; gboolean g_key_file_has_key (GKeyFile *key_file,
;;;                              const gchar *group_name,
;;;                              const gchar *key,
;;;                              GError **error);
;;;
;;; Looks whether the key file has the key key in the group group_name.
;;;
;;; Note
;;; This function does not follow the rules for GError strictly; the return
;;; value both carries meaning and signals an error. To use this function, you
;;; must pass a GError pointer in error, and check whether it is not NULL to see
;;; if an error occurred.
;;;
;;; Language bindings should use g_key_file_get_value() to test whether or not
;;; a key exists.
;;;
;;; key_file :
;;;     a GKeyFile
;;;
;;; group_name :
;;;     a group name
;;;
;;; key :
;;;     a key name
;;;
;;; error :
;;;     return location for a GError
;;;
;;; Returns :
;;;     TRUE if key is a part of group_name, FALSE otherwise.
;;;
;;; Since 2.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_key_file_get_value ()
;;;
;;; gchar * g_key_file_get_value (GKeyFile *key_file,
;;;                               const gchar *group_name,
;;;                               const gchar *key,
;;;                               GError **error);
;;;
;;; Returns the raw value associated with key under group_name. Use
;;; g_key_file_get_string() to retrieve an unescaped UTF-8 string.
;;;
;;; In the event the key cannot be found, NULL is returned and error is set to
;;; G_KEY_FILE_ERROR_KEY_NOT_FOUND. In the event that the group_name cannot be
;;; found, NULL is returned and error is set to
;;; G_KEY_FILE_ERROR_GROUP_NOT_FOUND.
;;;
;;; key_file :
;;;     a GKeyFile
;;;
;;; group_name :
;;;     a group name
;;;
;;; key :
;;;     a key
;;;
;;; error :
;;;     return location for a GError, or NULL
;;;
;;; Returns :
;;;     a newly allocated string or NULL if the specified key cannot be found.
;;;
;;; Since 2.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_key_file_get_string ()
;;;
;;; gchar * g_key_file_get_string (GKeyFile *key_file,
;;;                                const gchar *group_name,
;;;                                const gchar *key,
;;;                                GError **error);
;;;
;;; Returns the string value associated with key under group_name. Unlike
;;; g_key_file_get_value(), this function handles escape sequences like \s.
;;;
;;; In the event the key cannot be found, NULL is returned and error is set to
;;; G_KEY_FILE_ERROR_KEY_NOT_FOUND. In the event that the group_name cannot be
;;; found, NULL is returned and error is set to
;;; G_KEY_FILE_ERROR_GROUP_NOT_FOUND.
;;;
;;; key_file :
;;;     a GKeyFile
;;;
;;; group_name :
;;;     a group name
;;;
;;; key :
;;;     a key
;;;
;;; error :
;;;     return location for a GError, or NULL
;;;
;;; Returns :
;;;     a newly allocated string or NULL if the specified key cannot be found.
;;;
;;; Since 2.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_key_file_get_locale_string ()
;;;
;;; gchar * g_key_file_get_locale_string (GKeyFile *key_file,
;;;                                       const gchar *group_name,
;;;                                       const gchar *key,
;;;                                       const gchar *locale,
;;;                                       GError **error);
;;;
;;; Returns the value associated with key under group_name translated in the
;;; given locale if available. If locale is NULL then the current locale is
;;; assumed.
;;;
;;; If key cannot be found then NULL is returned and error is set to
;;; G_KEY_FILE_ERROR_KEY_NOT_FOUND. If the value associated with key cannot be
;;; interpreted or no suitable translation can be found then the untranslated
;;; value is returned.
;;;
;;; key_file :
;;;     a GKeyFile
;;;
;;; group_name :
;;;     a group name
;;;
;;; key :
;;;     a key
;;;
;;; locale :
;;;     a locale identifier or NULL. [allow-none]
;;;
;;; error :
;;;     return location for a GError, or NULL
;;;
;;; Returns :
;;;     a newly allocated string or NULL if the specified key cannot be found.
;;;
;;; Since 2.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_key_file_get_boolean ()
;;;
;;; gboolean g_key_file_get_boolean (GKeyFile *key_file,
;;;                                  const gchar *group_name,
;;;                                  const gchar *key,
;;;                                  GError **error);
;;;
;;; Returns the value associated with key under group_name as a boolean.
;;;
;;; If key cannot be found then FALSE is returned and error is set to
;;; G_KEY_FILE_ERROR_KEY_NOT_FOUND. Likewise, if the value associated with key
;;; cannot be interpreted as a boolean then FALSE is returned and error is set
;;; to G_KEY_FILE_ERROR_INVALID_VALUE.
;;;
;;; key_file :
;;;     a GKeyFile
;;;
;;; group_name :
;;;     a group name
;;;
;;; key :
;;;     a key
;;;
;;; error :
;;;     return location for a GError
;;;
;;; Returns :
;;;     the value associated with the key as a boolean, or FALSE if the key was
;;;     not found or could not be parsed.
;;;
;;; Since 2.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_key_file_get_integer ()
;;;
;;; gint g_key_file_get_integer (GKeyFile *key_file,
;;;                              const gchar *group_name,
;;;                              const gchar *key,
;;;                              GError **error);
;;;
;;; Returns the value associated with key under group_name as an integer.
;;;
;;; If key cannot be found then 0 is returned and error is set to
;;; G_KEY_FILE_ERROR_KEY_NOT_FOUND. Likewise, if the value associated with key
;;; cannot be interpreted as an integer then 0 is returned and error is set to
;;; G_KEY_FILE_ERROR_INVALID_VALUE.
;;;
;;; key_file :
;;;     a GKeyFile
;;;
;;; group_name :
;;;     a group name
;;;
;;; key :
;;;     a key
;;;
;;; error :
;;;     return location for a GError
;;;
;;; Returns :
;;;     the value associated with the key as an integer, or 0 if the key was not
;;;     found or could not be parsed.
;;;
;;; Since 2.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_key_file_get_int64 ()
;;;
;;; gint64 g_key_file_get_int64 (GKeyFile *key_file,
;;;                              const gchar *group_name,
;;;                              const gchar *key,
;;;                              GError **error);
;;;
;;; Returns the value associated with key under group_name as a signed 64-bit
;;; integer. This is similar to g_key_file_get_integer() but can return 64-bit
;;; results without truncation.
;;;
;;; key_file :
;;;     a non-NULL GKeyFile
;;;
;;; group_name :
;;;     a non-NULL group name
;;;
;;; key :
;;;     a non-NULL key
;;;
;;; error :
;;;     return location for a GError
;;;
;;; Returns :
;;;     the value associated with the key as a signed 64-bit integer, or 0 if
;;;     the key was not found or could not be parsed.
;;;
;;; Since 2.26
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_key_file_get_uint64 ()
;;;
;;; guint64 g_key_file_get_uint64 (GKeyFile *key_file,
;;;                                const gchar *group_name,
;;;                                const gchar *key,
;;;                                GError **error);
;;;
;;; Returns the value associated with key under group_name as an unsigned
;;; 64-bit integer. This is similar to g_key_file_get_integer() but can return
;;; large positive results without truncation.
;;;
;;; key_file :
;;;     a non-NULL GKeyFile
;;;
;;; group_name :
;;;     a non-NULL group name
;;;
;;; key :
;;;     a non-NULL key
;;;
;;; error :
;;;     return location for a GError
;;;
;;; Returns :
;;;     the value associated with the key as an unsigned 64-bit integer, or 0
;;;     if the key was not found or could not be parsed.
;;;
;;; Since 2.26
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_key_file_get_double ()
;;;
;;; gdouble g_key_file_get_double (GKeyFile *key_file,
;;;                                const gchar *group_name,
;;;                                const gchar *key,
;;;                                GError **error);
;;;
;;; Returns the value associated with key under group_name as a double. If
;;; group_name is NULL, the start_group is used.
;;;
;;; If key cannot be found then 0.0 is returned and error is set to
;;; G_KEY_FILE_ERROR_KEY_NOT_FOUND. Likewise, if the value associated with key
;;; cannot be interpreted as a double then 0.0 is returned and error is set to
;;; G_KEY_FILE_ERROR_INVALID_VALUE.
;;;
;;; key_file :
;;;     a GKeyFile
;;;
;;; group_name :
;;;     a group name
;;;
;;; key :
;;;     a key
;;;
;;; error :
;;;     return location for a GError
;;;
;;; Returns :
;;;     the value associated with the key as a double, or 0.0 if the key was not
;;;     found or could not be parsed.
;;;
;;; Since 2.12
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_key_file_get_string_list ()
;;;
;;; gchar ** g_key_file_get_string_list (GKeyFile *key_file,
;;;                                      const gchar *group_name,
;;;                                      const gchar *key,
;;;                                      gsize *length,
;;;                                      GError **error);
;;;
;;; Returns the values associated with key under group_name.
;;;
;;; In the event the key cannot be found, NULL is returned and error is set to
;;; G_KEY_FILE_ERROR_KEY_NOT_FOUND. In the event that the group_name cannot be
;;; found, NULL is returned and error is set to
;;; G_KEY_FILE_ERROR_GROUP_NOT_FOUND.
;;;
;;; key_file :
;;;     a GKeyFile
;;;
;;; group_name :
;;;     a group name
;;;
;;; key :
;;;     a key
;;;
;;; length :
;;;     return location for the number of returned strings, or NULL
;;;
;;; error :
;;;     return location for a GError, or NULL
;;;
;;; Returns :
;;;     a NULL-terminated string array or NULL if the specified key cannot be
;;;     found. The array should be freed with g_strfreev().
;;;
;;; Since 2.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_key_file_get_locale_string_list ()
;;;
;;; gchar ** g_key_file_get_locale_string_list (GKeyFile *key_file,
;;;                                             const gchar *group_name,
;;;                                             const gchar *key,
;;;                                             const gchar *locale,
;;;                                             gsize *length,
;;;                                             GError **error);
;;;
;;; Returns the values associated with key under group_name translated in the
;;; given locale if available. If locale is NULL then the current locale is
;;; assumed.
;;;
;;; If key cannot be found then NULL is returned and error is set to
;;; G_KEY_FILE_ERROR_KEY_NOT_FOUND. If the values associated with key cannot be
;;; interpreted or no suitable translations can be found then the untranslated
;;; values are returned. The returned array is NULL-terminated, so length may
;;; optionally be NULL.
;;;
;;; key_file :
;;;     a GKeyFile
;;;
;;; group_name :
;;;     a group name
;;;
;;; key :
;;;     a key
;;;
;;; locale :
;;;     a locale identifier or NULL
;;;
;;; length :
;;;     return location for the number of returned strings or NULL
;;;
;;; error :
;;;     return location for a GError or NULL
;;;
;;; Returns :
;;;     a newly allocated NULL-terminated string array or NULL if the key isn't
;;;     found. The string array should be freed with g_strfreev().
;;;
;;; Since 2.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_key_file_get_boolean_list ()
;;;
;;; gboolean * g_key_file_get_boolean_list (GKeyFile *key_file,
;;;                                         const gchar *group_name,
;;;                                         const gchar *key,
;;;                                         gsize *length,
;;;                                         GError **error);
;;;
;;; Returns the values associated with key under group_name as booleans.
;;;
;;; If key cannot be found then NULL is returned and error is set to
;;; G_KEY_FILE_ERROR_KEY_NOT_FOUND. Likewise, if the values associated with key
;;; cannot be interpreted as booleans then NULL is returned and error is set to
;;; G_KEY_FILE_ERROR_INVALID_VALUE.
;;;
;;; key_file :
;;;     a GKeyFile
;;;
;;; group_name :
;;;     a group name
;;;
;;; key :
;;;     a key
;;;
;;; length :
;;;     the number of booleans returned. [out]
;;;
;;; error :
;;;     return location for a GError
;;;
;;; Returns :
;;;     the values associated with the key as a list of booleans, or NULL if the
;;;     key was not found or could not be parsed. The returned list of booleans
;;;     should be freed with g_free() when no longer needed.
;;;
;;; Since 2.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_key_file_get_integer_list ()
;;;
;;; gint * g_key_file_get_integer_list (GKeyFile *key_file,
;;;                                     const gchar *group_name,
;;;                                     const gchar *key,
;;;                                     gsize *length,
;;;                                     GError **error);
;;;
;;; Returns the values associated with key under group_name as integers.
;;;
;;; If key cannot be found then NULL is returned and error is set to
;;; G_KEY_FILE_ERROR_KEY_NOT_FOUND. Likewise, if the values associated with key
;;; cannot be interpreted as integers then NULL is returned and error is set to
;;; G_KEY_FILE_ERROR_INVALID_VALUE.
;;;
;;; key_file :
;;;     a GKeyFile
;;;
;;; group_name :
;;;     a group name
;;;
;;; key :
;;;     a key
;;;
;;; length :
;;;     the number of integers returned. [out]
;;;
;;; error :
;;;     return location for a GError
;;;
;;; Returns :
;;;     the values associated with the key as a list of integers, or NULL if the
;;;     key was not found or could not be parsed. The returned list of integers
;;;     should be freed with g_free() when no longer needed.
;;;
;;; Since 2.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_key_file_get_double_list ()
;;;
;;; gdouble * g_key_file_get_double_list (GKeyFile *key_file,
;;;                                       const gchar *group_name,
;;;                                       const gchar *key,
;;;                                       gsize *length,
;;;                                       GError **error);
;;;
;;; Returns the values associated with key under group_name as doubles.
;;;
;;; If key cannot be found then NULL is returned and error is set to
;;; G_KEY_FILE_ERROR_KEY_NOT_FOUND. Likewise, if the values associated with key
;;; cannot be interpreted as doubles then NULL is returned and error is set to
;;; G_KEY_FILE_ERROR_INVALID_VALUE.
;;;
;;; key_file :
;;;     a GKeyFile
;;;
;;; group_name :
;;;     a group name
;;;
;;; key :
;;;     a key
;;;
;;; length :
;;;     the number of doubles returned. [out]
;;;
;;; error :
;;;     return location for a GError
;;;
;;; Returns :
;;;     the values associated with the key as a list of doubles, or NULL if the
;;;     key was not found or could not be parsed. The returned list of doubles
;;;     should be freed with g_free() when no longer needed.
;;;
;;; Since 2.12
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_key_file_get_comment ()
;;;
;;; gchar * g_key_file_get_comment (GKeyFile *key_file,
;;;                                 const gchar *group_name,
;;;                                 const gchar *key,
;;;                                 GError **error);
;;;
;;; Retrieves a comment above key from group_name. If key is NULL then comment
;;; will be read from above group_name. If both key and group_name are NULL,
;;; then comment will be read from above the first group in the file.
;;;
;;; key_file :
;;;     a GKeyFile
;;;
;;; group_name :
;;;     a group name, or NULL. [allow-none]
;;;
;;; key :
;;;     a key
;;;
;;; error :
;;;     return location for a GError
;;;
;;; Returns :
;;;     a comment that should be freed with g_free()
;;;
;;; Since 2.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_key_file_set_value ()
;;;
;;; void g_key_file_set_value (GKeyFile *key_file,
;;;                            const gchar *group_name,
;;;                            const gchar *key,
;;;                            const gchar *value);
;;;
;;; Associates a new value with key under group_name.
;;;
;;; If key cannot be found then it is created. If group_name cannot be found
;;; then it is created. To set an UTF-8 string which may contain characters that
;;; need escaping (such as newlines or spaces), use g_key_file_set_string().
;;;
;;; key_file :
;;;     a GKeyFile
;;;
;;; group_name :
;;;     a group name
;;;
;;; key :
;;;     a key
;;;
;;; value :
;;;     a string
;;;
;;; Since 2.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_key_file_set_string ()
;;;
;;; void g_key_file_set_string (GKeyFile *key_file,
;;;                             const gchar *group_name,
;;;                             const gchar *key,
;;;                             const gchar *string);
;;;
;;; Associates a new string value with key under group_name. If key cannot be
;;; found then it is created. If group_name cannot be found then it is created.
;;; Unlike g_key_file_set_value(), this function handles characters that need
;;; escaping, such as newlines.
;;;
;;; key_file :
;;;     a GKeyFile
;;;
;;; group_name :
;;;     a group name
;;;
;;; key :
;;;     a key
;;;
;;; string :
;;;     a string
;;;
;;; Since 2.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_key_file_set_locale_string ()
;;;
;;; void g_key_file_set_locale_string (GKeyFile *key_file,
;;;                                    const gchar *group_name,
;;;                                    const gchar *key,
;;;                                    const gchar *locale,
;;;                                    const gchar *string);
;;;
;;; Associates a string value for key and locale under group_name. If the
;;; translation for key cannot be found then it is created.
;;;
;;; key_file :
;;;     a GKeyFile
;;;
;;; group_name :
;;;     a group name
;;;
;;; key :
;;;     a key
;;;
;;; locale :
;;;     a locale identifier
;;;
;;; string :
;;;     a string
;;;
;;; Since 2.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_key_file_set_boolean ()
;;;
;;; void g_key_file_set_boolean (GKeyFile *key_file,
;;;                              const gchar *group_name,
;;;                              const gchar *key,
;;;                              gboolean value);
;;;
;;; Associates a new boolean value with key under group_name. If key cannot be
;;; found then it is created.
;;;
;;; key_file :
;;;     a GKeyFile
;;;
;;; group_name :
;;;     a group name
;;;
;;; key :
;;;     a key
;;;
;;; value :
;;;     TRUE or FALSE
;;;
;;; Since 2.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_key_file_set_integer ()
;;;
;;; void g_key_file_set_integer (GKeyFile *key_file,
;;;                              const gchar *group_name,
;;;                              const gchar *key,
;;;                              gint value);
;;;
;;; Associates a new integer value with key under group_name. If key cannot be
;;; found then it is created.
;;;
;;; key_file :
;;;     a GKeyFile
;;;
;;; group_name :
;;;     a group name
;;;
;;; key :
;;;     a key
;;;
;;; value :
;;;     an integer value
;;;
;;; Since 2.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_key_file_set_int64 ()
;;;
;;; void g_key_file_set_int64 (GKeyFile *key_file,
;;;                            const gchar *group_name,
;;;                            const gchar *key,
;;;                            gint64 value);
;;;
;;; Associates a new integer value with key under group_name. If key cannot be
;;; found then it is created.
;;;
;;; key_file :
;;;     a GKeyFile
;;;
;;; group_name :
;;;     a group name
;;;
;;; key :
;;;     a key
;;;
;;; value :
;;;     an integer value
;;;
;;; Since 2.26
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_key_file_set_uint64 ()
;;;
;;; void g_key_file_set_uint64 (GKeyFile *key_file,
;;;                             const gchar *group_name,
;;;                             const gchar *key,
;;;                             guint64 value);
;;;
;;; Associates a new integer value with key under group_name. If key cannot be
;;; found then it is created.
;;;
;;; key_file :
;;;     a GKeyFile
;;;
;;; group_name :
;;;     a group name
;;;
;;; key :
;;;     a key
;;;
;;; value :
;;;     an integer value
;;;
;;; Since 2.26
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_key_file_set_double ()
;;;
;;; void g_key_file_set_double (GKeyFile *key_file,
;;;                             const gchar *group_name,
;;;                             const gchar *key,
;;;                             gdouble value);
;;;
;;; Associates a new double value with key under group_name. If key cannot be
;;; found then it is created.
;;;
;;; key_file :
;;;     a GKeyFile
;;;
;;; group_name :
;;;     a group name
;;;
;;; key :
;;;     a key
;;;
;;; value :
;;;     an double value
;;;
;;; Since 2.12
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_key_file_set_string_list ()
;;;
;;; void g_key_file_set_string_list (GKeyFile *key_file,
;;;                                  const gchar *group_name,
;;;                                  const gchar *key,
;;;                                  const gchar * const list[],
;;;                                  gsize length);
;;;
;;; Associates a list of string values for key under group_name. If key cannot
;;; be found then it is created. If group_name cannot be found then it is
;;; created.
;;;
;;; key_file :
;;;     a GKeyFile
;;;
;;; group_name :
;;;     a group name
;;;
;;; key :
;;;     a key
;;;
;;; list :
;;;     an array of string values
;;;
;;; length :
;;;     number of string values in list
;;;
;;; Since 2.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_key_file_set_locale_string_list ()
;;;
;;; void g_key_file_set_locale_string_list (GKeyFile *key_file,
;;;                                         const gchar *group_name,
;;;                                         const gchar *key,
;;;                                         const gchar *locale,
;;;                                         const gchar * const list[],
;;;                                         gsize length);
;;;
;;; Associates a list of string values for key and locale under group_name. If
;;; the translation for key cannot be found then it is created.
;;;
;;; key_file :
;;;     a GKeyFile
;;;
;;; group_name :
;;;     a group name
;;;
;;; key :
;;;     a key
;;;
;;; locale :
;;;     a locale identifier
;;;
;;; list :
;;;     a NULL-terminated array of locale string values
;;;
;;; length :
;;;     the length of list
;;;
;;; Since 2.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_key_file_set_boolean_list ()
;;;
;;; void g_key_file_set_boolean_list (GKeyFile *key_file,
;;;                                   const gchar *group_name,
;;;                                   const gchar *key,
;;;                                   gboolean list[],
;;;                                   gsize length);
;;;
;;; Associates a list of boolean values with key under group_name. If key cannot
;;; be found then it is created. If group_name is NULL, the start_group is used.
;;;
;;; key_file :
;;;     a GKeyFile
;;;
;;; group_name :
;;;     a group name
;;;
;;; key :
;;;     a key
;;;
;;; list :
;;;     an array of boolean values. [array length=length]
;;;
;;; length :
;;;     length of list
;;;
;;; Since 2.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_key_file_set_integer_list ()
;;;
;;; void g_key_file_set_integer_list (GKeyFile *key_file,
;;;                                   const gchar *group_name,
;;;                                   const gchar *key,
;;;                                   gint list[],
;;;                                   gsize length);
;;;
;;; Associates a list of integer values with key under group_name. If key cannot
;;; be found then it is created.
;;;
;;; key_file :
;;;     a GKeyFile
;;;
;;; group_name :
;;;     a group name
;;;
;;; key :
;;;     a key
;;;
;;; list :
;;;     an array of integer values. [array length=length]
;;;
;;; length :
;;;     number of integer values in list
;;;
;;; Since 2.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_key_file_set_double_list ()
;;;
;;; void g_key_file_set_double_list (GKeyFile *key_file,
;;;                                  const gchar *group_name,
;;;                                  const gchar *key,
;;;                                  gdouble list[],
;;;                                  gsize length);
;;;
;;; Associates a list of double values with key under group_name. If key cannot
;;; be found then it is created.
;;;
;;; key_file :
;;;     a GKeyFile
;;;
;;; group_name :
;;;     a group name
;;;
;;; key :
;;;     a key
;;;
;;; list :
;;;     an array of double values. [array length=length]
;;;
;;; length :
;;;     number of double values in list
;;;
;;; Since 2.12
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_key_file_set_comment ()
;;;
;;; gboolean g_key_file_set_comment (GKeyFile *key_file,
;;;                                  const gchar *group_name,
;;;                                  const gchar *key,
;;;                                  const gchar *comment,
;;;                                  GError **error);
;;;
;;; Places a comment above key from group_name. If key is NULL then comment will
;;; be written above group_name. If both key and group_name are NULL, then
;;; comment will be written above the first group in the file.
;;;
;;; key_file :
;;;     a GKeyFile
;;;
;;; group_name :
;;;     a group name, or NULL. [allow-none]
;;;
;;; key :
;;;     a key. [allow-none]
;;;
;;; comment :
;;;     a comment
;;;
;;; error :
;;;     return location for a GError
;;;
;;; Returns :
;;;     TRUE if the comment was written, FALSE otherwise
;;;
;;; Since 2.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_key_file_remove_group ()
;;;
;;; gboolean g_key_file_remove_group (GKeyFile *key_file,
;;;                                   const gchar *group_name,
;;;                                   GError **error);
;;;
;;; Removes the specified group, group_name, from the key file.
;;;
;;; key_file :
;;;     a GKeyFile
;;;
;;; group_name :
;;;     a group name
;;;
;;; error :
;;;     return location for a GError or NULL
;;;
;;; Returns :
;;;     TRUE if the group was removed, FALSE otherwise
;;;
;;; Since 2.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_key_file_remove_key ()
;;;
;;; gboolean g_key_file_remove_key (GKeyFile *key_file,
;;;                                 const gchar *group_name,
;;;                                 const gchar *key,
;;;                                 GError **error);
;;;
;;; Removes key in group_name from the key file.
;;;
;;; key_file :
;;;     a GKeyFile
;;;
;;; group_name :
;;;     a group name
;;;
;;; key :
;;;     a key name to remove
;;;
;;; error :
;;;     return location for a GError or NULL
;;;
;;; Returns :
;;;     TRUE if the key was removed, FALSE otherwise
;;;
;;; Since 2.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_key_file_remove_comment ()
;;;
;;; gboolean g_key_file_remove_comment (GKeyFile *key_file,
;;;                                     const gchar *group_name,
;;;                                     const gchar *key,
;;;                                     GError **error);
;;;
;;; Removes a comment above key from group_name. If key is NULL then comment
;;; will be removed above group_name. If both key and group_name are NULL, then
;;; comment will be removed above the first group in the file.
;;;
;;; key_file :
;;;     a GKeyFile
;;;
;;; group_name :
;;;     a group name, or NULL. [allow-none]
;;;
;;; key :
;;;     a key. [allow-none]
;;;
;;; error :
;;;     return location for a GError
;;;
;;; Returns :
;;;     TRUE if the comment was removed, FALSE otherwise
;;;
;;; Since 2.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_KEY_FILE_DESKTOP_GROUP
;;;
;;; #define G_KEY_FILE_DESKTOP_GROUP                "Desktop Entry"
;;;
;;; The name of the main group of a desktop entry file, as defined in the
;;; Desktop Entry Specification. Consult the specification for more details
;;; about the meanings of the keys below.
;;;
;;; Since 2.14
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_KEY_FILE_DESKTOP_KEY_TYPE
;;;
;;; #define G_KEY_FILE_DESKTOP_KEY_TYPE             "Type"
;;;
;;; A key under G_KEY_FILE_DESKTOP_GROUP, whose value is a string giving the
;;; type of the desktop entry. Usually G_KEY_FILE_DESKTOP_TYPE_APPLICATION,
;;; G_KEY_FILE_DESKTOP_TYPE_LINK, or G_KEY_FILE_DESKTOP_TYPE_DIRECTORY.
;;;
;;; Since 2.14
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_KEY_FILE_DESKTOP_KEY_VERSION
;;;
;;; #define G_KEY_FILE_DESKTOP_KEY_VERSION          "Version"
;;;
;;; A key under G_KEY_FILE_DESKTOP_GROUP, whose value is a string giving the
;;; version of the Desktop Entry Specification used for the desktop entry file.
;;;
;;; Since 2.14
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_KEY_FILE_DESKTOP_KEY_NAME
;;;
;;; #define G_KEY_FILE_DESKTOP_KEY_NAME             "Name"
;;;
;;; A key under G_KEY_FILE_DESKTOP_GROUP, whose value is a localized string
;;; giving the specific name of the desktop entry.
;;;
;;; Since 2.14
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_KEY_FILE_DESKTOP_KEY_GENERIC_NAME
;;;
;;; #define G_KEY_FILE_DESKTOP_KEY_GENERIC_NAME     "GenericName"
;;;
;;; A key under G_KEY_FILE_DESKTOP_GROUP, whose value is a localized string
;;; giving the generic name of the desktop entry.
;;;
;;; Since 2.14
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_KEY_FILE_DESKTOP_KEY_NO_DISPLAY
;;;
;;; #define G_KEY_FILE_DESKTOP_KEY_NO_DISPLAY       "NoDisplay"
;;;
;;; A key under G_KEY_FILE_DESKTOP_GROUP, whose value is a boolean stating
;;; whether the desktop entry should be shown in menus.
;;;
;;; Since 2.14
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_KEY_FILE_DESKTOP_KEY_COMMENT
;;;
;;; #define G_KEY_FILE_DESKTOP_KEY_COMMENT          "Comment"
;;;
;;; A key under G_KEY_FILE_DESKTOP_GROUP, whose value is a localized string
;;; giving the tooltip for the desktop entry.
;;;
;;; Since 2.14
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_KEY_FILE_DESKTOP_KEY_ICON
;;;
;;; #define G_KEY_FILE_DESKTOP_KEY_ICON             "Icon"
;;;
;;; A key under G_KEY_FILE_DESKTOP_GROUP, whose value is a localized string
;;; giving the name of the icon to be displayed for the desktop entry.
;;;
;;; Since 2.14
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_KEY_FILE_DESKTOP_KEY_HIDDEN
;;;
;;; #define G_KEY_FILE_DESKTOP_KEY_HIDDEN           "Hidden"
;;;
;;; A key under G_KEY_FILE_DESKTOP_GROUP, whose value is a boolean stating
;;; whether the desktop entry has been deleted by the user.
;;;
;;; Since 2.14
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_KEY_FILE_DESKTOP_KEY_ONLY_SHOW_IN
;;;
;;; #define G_KEY_FILE_DESKTOP_KEY_ONLY_SHOW_IN     "OnlyShowIn"
;;;
;;; A key under G_KEY_FILE_DESKTOP_GROUP, whose value is a list of strings
;;; identifying the environments that should display the desktop entry.
;;;
;;; Since 2.14
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_KEY_FILE_DESKTOP_KEY_NOT_SHOW_IN
;;;
;;; #define G_KEY_FILE_DESKTOP_KEY_NOT_SHOW_IN      "NotShowIn"
;;;
;;; A key under G_KEY_FILE_DESKTOP_GROUP, whose value is a list of strings
;;; identifying the environments that should not display the desktop entry.
;;;
;;; Since 2.14
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_KEY_FILE_DESKTOP_KEY_TRY_EXEC
;;;
;;; #define G_KEY_FILE_DESKTOP_KEY_TRY_EXEC         "TryExec"
;;;
;;; A key under G_KEY_FILE_DESKTOP_GROUP, whose value is a string giving the
;;; file name of a binary on disk used to determine if the program is actually
;;; installed. It is only valid for desktop entries with the Application type.
;;;
;;; Since 2.14
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_KEY_FILE_DESKTOP_KEY_EXEC
;;;
;;; #define G_KEY_FILE_DESKTOP_KEY_EXEC             "Exec"
;;;
;;; A key under G_KEY_FILE_DESKTOP_GROUP, whose value is a string giving the
;;; command line to execute. It is only valid for desktop entries with the
;;; Application type.
;;;
;;; Since 2.14
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_KEY_FILE_DESKTOP_KEY_PATH
;;;
;;; #define G_KEY_FILE_DESKTOP_KEY_PATH             "Path"
;;;
;;; A key under G_KEY_FILE_DESKTOP_GROUP, whose value is a string containing the
;;; working directory to run the program in. It is only valid for desktop
;;; entries with the Application type.
;;;
;;; Since 2.14
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_KEY_FILE_DESKTOP_KEY_TERMINAL
;;;
;;; #define G_KEY_FILE_DESKTOP_KEY_TERMINAL         "Terminal"
;;;
;;; A key under G_KEY_FILE_DESKTOP_GROUP, whose value is a boolean stating
;;; whether the program should be run in a terminal window. It is only valid for
;;; desktop entries with the Application type.
;;;
;;; Since 2.14
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_KEY_FILE_DESKTOP_KEY_MIME_TYPE
;;;
;;; #define G_KEY_FILE_DESKTOP_KEY_MIME_TYPE        "MimeType"
;;;
;;; A key under G_KEY_FILE_DESKTOP_GROUP, whose value is a list of strings
;;; giving the MIME types supported by this desktop entry.
;;;
;;; Since 2.14
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_KEY_FILE_DESKTOP_KEY_CATEGORIES
;;;
;;; #define G_KEY_FILE_DESKTOP_KEY_CATEGORIES       "Categories"
;;;
;;; A key under G_KEY_FILE_DESKTOP_GROUP, whose value is a list of strings
;;; giving the categories in which the desktop entry should be shown in a menu.
;;;
;;; Since 2.14
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_KEY_FILE_DESKTOP_KEY_STARTUP_NOTIFY
;;;
;;; #define G_KEY_FILE_DESKTOP_KEY_STARTUP_NOTIFY   "StartupNotify"
;;;
;;; A key under G_KEY_FILE_DESKTOP_GROUP, whose value is a boolean stating
;;; whether the application supports the Startup Notification Protocol
;;; Specification.
;;;
;;; Since 2.14
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_KEY_FILE_DESKTOP_KEY_STARTUP_WM_CLASS
;;;
;;; #define G_KEY_FILE_DESKTOP_KEY_STARTUP_WM_CLASS "StartupWMClass"
;;;
;;; A key under G_KEY_FILE_DESKTOP_GROUP, whose value is string identifying the
;;; WM class or name hint of a window that the application will create, which
;;; can be used to emulate Startup Notification with older applications.
;;;
;;; Since 2.14
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_KEY_FILE_DESKTOP_KEY_URL
;;;
;;; #define G_KEY_FILE_DESKTOP_KEY_URL              "URL"
;;;
;;; A key under G_KEY_FILE_DESKTOP_GROUP, whose value is a string giving the URL
;;; to access. It is only valid for desktop entries with the Link type.
;;;
;;; Since 2.14
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_KEY_FILE_DESKTOP_TYPE_APPLICATION
;;;
;;; #define G_KEY_FILE_DESKTOP_TYPE_APPLICATION     "Application"
;;;
;;; The value of the G_KEY_FILE_DESKTOP_KEY_TYPE, key for desktop entries
;;; representing applications.
;;;
;;; Since 2.14
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_KEY_FILE_DESKTOP_TYPE_LINK
;;;
;;; #define G_KEY_FILE_DESKTOP_TYPE_LINK            "Link"
;;;
;;; The value of the G_KEY_FILE_DESKTOP_KEY_TYPE, key for desktop entries
;;; representing links to documents.
;;;
;;; Since 2.14
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_KEY_FILE_DESKTOP_TYPE_DIRECTORY
;;;
;;; #define G_KEY_FILE_DESKTOP_TYPE_DIRECTORY "Directory"
;;;
;;; The value of the G_KEY_FILE_DESKTOP_KEY_TYPE, key for desktop entries
;;; representing directories.
;;;
;;; Since 2.14
;;; ----------------------------------------------------------------------------

;;; --- End of file glib.key-value.lisp ----------------------------------------
