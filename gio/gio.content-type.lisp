;;; ----------------------------------------------------------------------------
;;; gio.content-type.lisp
;;;
;;; The documentation of this file is taken from the GIO Reference Manual
;;; Version 2.38.1 and modified to document the Lisp binding to the GIO library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2013, 2014 Dieter Kaiser
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
;;; GContentType
;;;
;;; Platform-specific content typing
;;;
;;; Synopsis
;;;
;;;     g_content_type_equals
;;;     g_content_type_is_a
;;;     g_content_type_is_unknown
;;;     g_content_type_get_description
;;;     g_content_type_get_mime_type
;;;     g_content_type_get_icon
;;;     g_content_type_get_symbolic_icon
;;;     g_content_type_get_generic_icon_name
;;;     g_content_type_can_be_executable
;;;     g_content_type_from_mime_type
;;;     g_content_type_guess
;;;     g_content_type_guess_for_tree
;;;     g_content_types_get_registered
;;;
;;; Description
;;;
;;; A content type is a platform specific string that defines the type of a
;;; file. On UNIX it is a mime type like "text/plain" or "image/png". On Win32
;;; it is an extension string like ".doc", ".txt" or a perceived string like
;;; "audio". Such strings can be looked up in the registry at HKEY_CLASSES_ROOT.
;;; ----------------------------------------------------------------------------

(in-package :gio)

;;; ----------------------------------------------------------------------------
;;; g_content_type_equals ()
;;;
;;; gboolean g_content_type_equals (const gchar *type1, const gchar *type2);
;;;
;;; Compares two content types for equality.
;;;
;;; type1 :
;;;     a content type string
;;;
;;; type2 :
;;;     a content type string
;;;
;;; Returns :
;;;     TRUE if the two strings are identical or equivalent, FALSE otherwise.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_content_type_is_a ()
;;;
;;; gboolean g_content_type_is_a (const gchar *type, const gchar *supertype);
;;;
;;; Determines if type is a subset of supertype.
;;;
;;; type :
;;;     a content type string
;;;
;;; supertype :
;;;     a content type string
;;;
;;; Returns :
;;;     TRUE if type is a kind of supertype, FALSE otherwise.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_content_type_is_unknown ()
;;;
;;; gboolean g_content_type_is_unknown (const gchar *type);
;;;
;;; Checks if the content type is the generic "unknown" type. On UNIX this is
;;; the "application/octet-stream" mimetype, while on win32 it is "*".
;;;
;;; type :
;;;     a content type string
;;;
;;; Returns :
;;;     TRUE if the type is the unknown type.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_content_type_get_description ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_content_type_get_description" g-content-type-get-description)
    (:string :free-from-foreign t)
 #+cl-cffi-gtk-documentation
 "@version{2013-12-31}
  @argument[type]{a content type string}
  @begin{return}
    A short description of the content type type.
  @end{return}
  Gets the human readable description of the content type."
  (type :string))

(export 'g-content-type-get-description)

;;; ----------------------------------------------------------------------------
;;; g_content_type_get_mime_type ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_content_type_get_mime_type" g-content-type-get-mime-type) :string
 #+cl-cffi-gtk-documentation
 "@version{2013-12-31}
  @argument[type]{a content type string}
  @begin{return}
    The registered mime type for the given type, or @code{nil} if unknown.
  @end{return}
  Gets the mime type for the content type, if one is registered."
  (type :string))

(export 'g-content-type-get-mime-type)

;;; ----------------------------------------------------------------------------
;;; g_content_type_get_icon ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_content_type_get_icon" g-content-type-get-icon) (g-object g-icon)
 #+cl-cffi-gtk-documentation
 "@version{2013-12-31}
  @argument[type]{a content type string}
  @begin{return}
    @class{g-icon} corresponding to the content type.
  @end{return}
  Gets the icon for a content type."
  (type :string))

(export 'g-content-type-get-icon)

;;; ----------------------------------------------------------------------------
;;; g_content_type_get_symbolic_icon ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_content_type_get_symbolic_icon" g-content-type-get-symbolic-icon)
    (g-object g-icon)
 #+cl-cffi-gtk-documentation
 "@version{2013-12-31}
  @argument[type]{a content type string}
  @begin{return}
    Symbolic @class{g-icon} corresponding to the content type.
  @end{return}
  @short{Gets the symbolic icon for a content type.}

  Since 2.34"
  (type :string))

(export 'g-content-type-get-symbolic-icon)

;;; ----------------------------------------------------------------------------
;;; g_content_type_get_generic_icon_name ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_content_type_get_generic_icon_name"
           g-content-type-get-generic-icon-name) :string
 #+cl-cffi-gtk-documentation
 "@version{2014-1-2}
  @argument[type]{a content type string}
  @begin{return}
    The registered generic icon name for the given type, or @code{nil} if
    unknown.
  @end{return}
  @short{Gets the generic icon name for a content type.}

  See the shared-mime-info specification for more on the generic icon name.

  Since 2.34"
  (type :string))

(export 'g-content-type-get-generic-icon-name)

;;; ----------------------------------------------------------------------------
;;; g_content_type_can_be_executable ()
;;;
;;; gboolean g_content_type_can_be_executable (const gchar *type);
;;;
;;; Checks if a content type can be executable. Note that for instance things
;;; like text files can be executables (i.e. scripts and batch files).
;;;
;;; type :
;;;     a content type string
;;;
;;; Returns :
;;;     TRUE if the file type corresponds to a type that can be executable,
;;;     FALSE otherwise.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_content_type_from_mime_type ()
;;;
;;; gchar * g_content_type_from_mime_type (const gchar *mime_type);
;;;
;;; Tries to find a content type based on the mime type name.
;;;
;;; mime_type :
;;;     a mime type string
;;;
;;; Returns :
;;;     Newly allocated string with content type or NULL. Free with g_free().
;;;
;;; Since 2.18
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_content_type_guess ()
;;;
;;; gchar * g_content_type_guess (const gchar *filename,
;;;                               const guchar *data,
;;;                               gsize data_size,
;;;                               gboolean *result_uncertain);
;;;
;;; Guesses the content type based on example data. If the function is
;;; uncertain, result_uncertain will be set to TRUE. Either filename or data may
;;; be NULL, in which case the guess will be based solely on the other argument.
;;;
;;; filename :
;;;     a string, or NULL
;;;
;;; data :
;;;     a stream of data, or NULL
;;;
;;; data_size :
;;;     the size of data
;;;
;;; result_uncertain :
;;;     return location for the certainty of the result, or NULL
;;;
;;; Returns :
;;;     a string indicating a guessed content type for the given data.
;;;     Free with g_free()
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_content_type_guess_for_tree ()
;;;
;;; gchar ** g_content_type_guess_for_tree (GFile *root);
;;;
;;; Tries to guess the type of the tree with root root, by looking at the files
;;; it contains. The result is an array of content types, with the best guess
;;; coming first.
;;;
;;; The types returned all have the form x-content/foo, e.g.
;;; x-content/audio-cdda (for audio CDs) or x-content/image-dcf (for a camera
;;; memory card). See the shared-mime-info specification for more on x-content
;;; types.
;;;
;;; This function is useful in the implementation of
;;; g_mount_guess_content_type().
;;;
;;; root :
;;;     the root of the tree to guess a type for
;;;
;;; Returns :
;;;     an NULL-terminated array of zero or more content types. Free with
;;;     g_strfreev().
;;;
;;; Since 2.18
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_content_types_get_registered ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_content_types_get_registered" g-content-types-get-registered)
    (g-list :string :free-from-foreign t)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-2}
  @return{A list of strings of the registered content types.}
  Gets a list of strings containing all the registered content types known to
  the system.")

(export 'g-content-types-get-registered)

;;; --- End of file gio.content-type.lisp --------------------------------------
