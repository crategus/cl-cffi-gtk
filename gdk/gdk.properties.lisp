;;; ----------------------------------------------------------------------------
;;; gdk.properties.lisp
;;;
;;; Copyright (C) 2009, 2011 Kalyanov Dmitry
;;; Copyright (C) 2011, 2012 Dr. Dieter Kaiser
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See http://common-lisp.net/project/cl-gtk2/
;;;
;;; The documentation has been copied from the GDK 2 Reference Manual
;;; See http://www.gtk.org
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
;;; Properties and Atoms
;;; 
;;; Functions to manipulate properties on windows
;;; 	
;;; Synopsis
;;; 
;;;     GdkAtom
;;;     GDK_ATOM_TO_POINTER
;;;     GDK_POINTER_TO_ATOM
;;;     GDK_NONE
;;;     gdk_text_property_to_text_list
;;;     gdk_text_property_to_text_list_for_display
;;;     gdk_free_text_list
;;;     gdk_text_property_to_utf8_list
;;;     gdk_text_property_to_utf8_list_for_display
;;;     gdk_string_to_compound_text
;;;     gdk_string_to_compound_text_for_display
;;;     gdk_free_compound_text
;;;     gdk_utf8_to_string_target
;;;     gdk_utf8_to_compound_text
;;;     gdk_utf8_to_compound_text_for_display
;;;     gdk_atom_intern
;;;     gdk_atom_intern_static_string
;;;     gdk_atom_name
;;;     gdk_property_get
;;;     gdk_property_change
;;;     GdkPropMode
;;;     gdk_property_delete
;;; 
;;; Description
;;; 
;;; Each window under X can have any number of associated properties attached
;;; to it. Properties are arbitrary chunks of data identified by atoms. (An
;;; atom is a numeric index into a string table on the X server. They are used
;;; to transfer strings efficiently between clients without having to transfer
;;; the entire string.) A property has an associated type, which is also
;;; identified using an atom.
;;; 
;;; A property has an associated format, an integer describing how many bits
;;; are in each unit of data inside the property. It must be 8, 16, or 32. When
;;; data is transferred between the server and client, if they are of different
;;; endianesses it will be byteswapped as necessary according to the format of
;;; the property. Note that on the client side, properties of format 32 will be
;;; stored with one unit per long, even if a long integer has more than 32 bits
;;; on the platform. (This decision was apparently made for Xlib to maintain
;;; compatibility with programs that assumed longs were 32 bits, at the expense
;;; of programs that knew better.)
;;; 
;;; The functions in this section are used to add, remove and change properties
;;; on windows, to convert atoms to and from strings and to manipulate some
;;; types of data commonly stored in X window properties.
;;; ----------------------------------------------------------------------------

(in-package :gdk)

;;; ----------------------------------------------------------------------------
;;; GdkAtom
;;; 
;;; typedef struct _GdkAtom GdkAtom;
;;; 
;;; An opaque type representing a string as an index into a table of strings on
;;; the X server.
;;; ----------------------------------------------------------------------------

(defctype gdk-atom :pointer)

(export 'gdk-atom)

;; Extension to return a string for an GdkAtom

(define-foreign-type gdk-atom-as-string-type ()
  ()
  (:actual-type :pointer)
  (:simple-parser gdk-atom-as-string))

(defmethod translate-from-foreign (value (type gdk-atom-as-string-type))
  (gdk-atom-name value))

(defmethod translate-to-foreign (value (type gdk-atom-as-string-type))
  (gdk-atom-intern value nil))

(export 'gdk-atom-as-string)

;;; ----------------------------------------------------------------------------
;;; GDK_ATOM_TO_POINTER()
;;; 
;;; #define GDK_ATOM_TO_POINTER(atom) (atom)
;;; 
;;; Converts a GdkAtom into a pointer type.
;;; 
;;; atom :
;;; 	a GdkAtom.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GDK_POINTER_TO_ATOM()
;;; 
;;; #define GDK_POINTER_TO_ATOM(ptr)  ((GdkAtom)(ptr))
;;; 
;;; Extracts a GdkAtom from a pointer. The GdkAtom must have been stored in the
;;; pointer with GDK_ATOM_TO_POINTER().
;;; 
;;; ptr :
;;; 	a pointer containing a GdkAtom.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GDK_NONE
;;; 
;;; #define GDK_NONE _GDK_MAKE_ATOM (0)
;;; 
;;; A null value for GdkAtom, used in a similar way as None in the Xlib API.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_text_property_to_text_list ()
;;; 
;;; gint gdk_text_property_to_text_list (GdkAtom encoding,
;;;                                      gint format,
;;;                                      const guchar *text,
;;;                                      gint length,
;;;                                      gchar ***list);
;;; 
;;; Warning
;;; 
;;; gdk_text_property_to_text_list is deprecated and should not be used in
;;; newly-written code.
;;; 
;;; Converts a text string from the encoding as it is stored in a property into
;;; an array of strings in the encoding of the current local. (The elements of
;;; the array represent the nul-separated elements of the original text string.)
;;; 
;;; encoding :
;;; 	an atom representing the encoding. The most common values for this are
;;;     STRING, or COMPOUND_TEXT. This is value used as the type for the
;;;     property.
;;; 
;;; format :
;;; 	the format of the property.
;;; 
;;; text :
;;; 	the text data.
;;; 
;;; length :
;;; 	the length of the property, in items.
;;; 
;;; list :
;;; 	location to store a terminated array of strings in the encoding of the
;;;     current locale. This array should be freed using gdk_free_text_list().
;;; 
;;; Returns :
;;; 	the number of strings stored in list, or 0, if the conversion failed.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_text_property_to_text_list_for_display ()
;;; 
;;; gint gdk_text_property_to_text_list_for_display (GdkDisplay *display,
;;;                                                  GdkAtom encoding,
;;;                                                  gint format,
;;;                                                  const guchar *text,
;;;                                                  gint length,
;;;                                                  gchar ***list);
;;; 
;;; Warning
;;; 
;;; gdk_text_property_to_text_list_for_display has been deprecated since
;;; version 2.24 and should not be used in newly-written code. Use
;;; gdk_x11_display_text_property_to_text_list()
;;; 
;;; Convert a text string from the encoding as it is stored in a property into
;;; an array of strings in the encoding of the current locale. (The elements of
;;; the array represent the nul-separated elements of the original text string.)
;;; 
;;; display :
;;; 	The GdkDisplay where the encoding is defined.
;;; 
;;; encoding :
;;; 	an atom representing the encoding. The most common values for this are
;;;     STRING, or COMPOUND_TEXT. This is value used as the type for the
;;;     property.
;;; 
;;; format :
;;; 	the format of the property.
;;; 
;;; text :
;;; 	The text data.
;;; 
;;; length :
;;; 	The number of items to transform.
;;; 
;;; list :
;;; 	location to store a terminated array of strings in the encoding of the
;;;     current locale. This array should be freed using gdk_free_text_list().
;;; 
;;; Returns :
;;; 	the number of strings stored in list, or 0, if the conversion failed.
;;; 
;;; Since 2.2
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_free_text_list ()
;;; 
;;; void gdk_free_text_list (gchar **list);
;;; 
;;; Warning
;;; 
;;; gdk_free_text_list is deprecated and should not be used in newly-written
;;; code.
;;; 
;;; Frees the array of strings created by gdk_text_property_to_text_list().
;;; 
;;; list :
;;; 	the value stored in the list parameter by a call to
;;;     gdk_text_property_to_text_list().
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_text_property_to_utf8_list ()
;;; 
;;; gint gdk_text_property_to_utf8_list (GdkAtom encoding,
;;;                                      gint format,
;;;                                      const guchar *text,
;;;                                      gint length,
;;;                                      gchar ***list);
;;; 
;;; Warning
;;; 
;;; gdk_text_property_to_utf8_list is deprecated and should not be used in
;;; newly-written code.
;;; 
;;; Convert a text property in the giving encoding to a list of UTF-8 strings.
;;; 
;;; encoding :
;;; 	an atom representing the encoding of the text
;;; 
;;; format :
;;; 	the format of the property
;;; 
;;; text :
;;; 	the text to convert
;;; 
;;; length :
;;; 	the length of text, in bytes
;;; 
;;; list :
;;; 	location to store the list of strings or NULL. The list should be
;;;     freed with g_strfreev().
;;; 
;;; Returns :
;;; 	the number of strings in the resulting list.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_text_property_to_utf8_list_for_display ()
;;; 
;;; gint gdk_text_property_to_utf8_list_for_display (GdkDisplay *display,
;;;                                                  GdkAtom encoding,
;;;                                                  gint format,
;;;                                                  const guchar *text,
;;;                                                  gint length,
;;;                                                  gchar ***list);
;;; 
;;; Converts a text property in the given encoding to a list of UTF-8 strings.
;;; 
;;; display :
;;; 	a GdkDisplay
;;; 
;;; encoding :
;;; 	an atom representing the encoding of the text
;;; 
;;; format :
;;; 	the format of the property
;;; 
;;; text :
;;; 	the text to convert
;;; 
;;; length :
;;; 	the length of text, in bytes
;;; 
;;; list :
;;; 	location to store the list of strings or NULL. The list should be freed
;;;     with g_strfreev().
;;; 
;;; Returns :
;;; 	the number of strings in the resulting list.
;;; 
;;; Since 2.2
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_string_to_compound_text ()
;;; 
;;; gint gdk_string_to_compound_text (const gchar *str,
;;;                                   GdkAtom *encoding,
;;;                                   gint *format,
;;;                                   guchar **ctext,
;;;                                   gint *length);
;;; 
;;; Warning
;;; 
;;; gdk_string_to_compound_text is deprecated and should not be used in
;;; newly-written code.
;;; 
;;; Converts a string from the encoding of the current locale into a form
;;; suitable for storing in a window property.
;;; 
;;; str :
;;; 	a nul-terminated string.
;;; 
;;; encoding :
;;; 	location to store the encoding atom (to be used as the type for the
;;;     property).
;;; 
;;; format :
;;; 	location to store the format for the property.
;;; 
;;; ctext :
;;; 	location to store newly allocated data for the property.
;;; 
;;; length :
;;; 	location to store the length of ctext in items.
;;; 
;;; Returns :
;;; 	0 upon sucess, non-zero upon failure.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_string_to_compound_text_for_display ()
;;; 
;;; gint gdk_string_to_compound_text_for_display (GdkDisplay *display,
;;;                                               const gchar *str,
;;;                                               GdkAtom *encoding,
;;;                                               gint *format,
;;;                                               guchar **ctext,
;;;                                               gint *length);
;;; 
;;; Warning
;;; 
;;; gdk_string_to_compound_text_for_display has been deprecated since version
;;; 2.24 and should not be used in newly-written code.
;;; Use gdk_x11_display_string_to_compound_text()
;;; 
;;; Convert a string from the encoding of the current locale into a form
;;; suitable for storing in a window property.
;;; 
;;; display :
;;; 	the GdkDisplay where the encoding is defined.
;;; 
;;; str :
;;; 	a nul-terminated string.
;;; 
;;; encoding :
;;; 	location to store the encoding atom (to be used as the type for the
;;;     property).
;;; 
;;; format :
;;; 	location to store the format of the property
;;; 
;;; ctext :
;;; 	location to store newly allocated data for the property.
;;; 
;;; length :
;;; 	the length of text, in bytes
;;; 
;;; Returns :
;;; 	0 upon success, non-zero upon failure.
;;; 
;;; Since 2.2
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_free_compound_text ()
;;; 
;;; void gdk_free_compound_text (guchar *ctext);
;;; 
;;; Warning
;;; 
;;; gdk_free_compound_text is deprecated and should not be used in
;;; newly-written code.
;;; 
;;; Frees the data returned from gdk_string_to_compound_text().
;;; 
;;; ctext :
;;; 	The pointer stored in ctext from a call to
;;;     gdk_string_to_compound_text().
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_utf8_to_string_target ()
;;; 
;;; gchar * gdk_utf8_to_string_target (const gchar *str);
;;; 
;;; Convert an UTF-8 string into the best possible representation as a STRING.
;;; The representation of characters not in STRING is not specified; it may be
;;; as pseudo-escape sequences \x{ABCD}, or it may be in some other form of
;;; approximation.
;;; 
;;; str :
;;; 	a UTF-8 string
;;; 
;;; Returns :
;;; 	the newly allocated string, or NULL if the conversion failed. (It
;;;     should not fail for any properly formed UTF-8 string.)
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_utf8_to_compound_text ()
;;; 
;;; gboolean gdk_utf8_to_compound_text (const gchar *str,
;;;                                     GdkAtom *encoding,
;;;                                     gint *format,
;;;                                     guchar **ctext,
;;;                                     gint *length);
;;; 
;;; Warning
;;; 
;;; gdk_utf8_to_compound_text is deprecated and should not be used in
;;; newly-written code.
;;; 
;;; Convert from UTF-8 to compound text.
;;; 
;;; str :
;;; 	a UTF-8 string
;;; 
;;; encoding :
;;; 	location to store resulting encoding
;;; 
;;; format :
;;; 	location to store format of the result
;;; 
;;; ctext :
;;; 	location to store the data of the result
;;; 
;;; length :
;;; 	location to store the length of the data stored in ctext
;;; 
;;; Returns :
;;; 	TRUE if the conversion succeeded, otherwise false.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_utf8_to_compound_text_for_display ()
;;; 
;;; gboolean gdk_utf8_to_compound_text_for_display (GdkDisplay *display,
;;;                                                 const gchar *str,
;;;                                                 GdkAtom *encoding,
;;;                                                 gint *format,
;;;                                                 guchar **ctext,
;;;                                                 gint *length);
;;; 
;;; Warning
;;; 
;;; gdk_utf8_to_compound_text_for_display has been deprecated since
;;; version 2.24 and should not be used in newly-written code.
;;; Use gdk_x11_display_utf8_to_compound_text()
;;; 
;;; Converts from UTF-8 to compound text.
;;; 
;;; display :
;;; 	a GdkDisplay
;;; 
;;; str :
;;; 	a UTF-8 string
;;; 
;;; encoding :
;;; 	location to store resulting encoding
;;; 
;;; format :
;;; 	location to store format of the result
;;; 
;;; ctext :
;;; 	location to store the data of the result
;;; 
;;; length :
;;; 	location to store the length of the data stored in ctext
;;; 
;;; Returns :
;;; 	TRUE if the conversion succeeded, otherwise FALSE.
;;; 
;;; Since 2.2
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_atom_intern ()
;;; 
;;; GdkAtom gdk_atom_intern (const gchar *atom_name, gboolean only_if_exists);
;;; 
;;; Finds or creates an atom corresponding to a given string.
;;; 
;;; atom_name :
;;; 	a string.
;;; 
;;; only_if_exists :
;;; 	if TRUE, GDK is allowed to not create a new atom, but just return
;;;     GDK_NONE if the requested atom doesn't already exists. Currently, the
;;;     flag is ignored, since checking the existance of an atom is as
;;;     expensive as creating it.
;;; 
;;; Returns :
;;; 	the atom corresponding to atom_name.
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_atom_intern" gdk-atom-intern) gdk-atom
  (name :string)
  (only-if-exists :boolean))

(export 'gdk-atom-intern)

;;; ----------------------------------------------------------------------------
;;; gdk_atom_intern_static_string ()
;;; 
;;; GdkAtom gdk_atom_intern_static_string (const gchar *atom_name);
;;; 
;;; Finds or creates an atom corresponding to a given string.
;;; 
;;; Note that this function is identical to gdk_atom_intern() except that if a
;;; new GdkAtom is created the string itself is used rather than a copy. This
;;; saves memory, but can only be used if the string will always exist. It can
;;; be used with statically allocated strings in the main program, but not with
;;; statically allocated memory in dynamically loaded modules, if you expect to
;;; ever unload the module again (e.g. do not use this function in GTK+ theme
;;; engines).
;;; 
;;; atom_name :
;;; 	a static string
;;; 
;;; Returns :
;;; 	the atom corresponding to atom_name
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_atom_name ()
;;; 
;;; gchar * gdk_atom_name (GdkAtom atom);
;;; 
;;; Determines the string corresponding to an atom.
;;; 
;;; atom :
;;; 	a GdkAtom.
;;; 
;;; Returns :
;;; 	a newly-allocated string containing the string corresponding to atom.
;;;     When you are done with the return value, you should free it
;;;     using g_free().
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_atom_name" gdk-atom-name) (g-string :free-from-foreign t)
  (atom gdk-atom))

(export 'gdk-atom-name)

;;; ----------------------------------------------------------------------------
;;; gdk_property_get ()
;;; 
;;; gboolean gdk_property_get (GdkWindow *window,
;;;                            GdkAtom property,
;;;                            GdkAtom type,
;;;                            gulong offset,
;;;                            gulong length,
;;;                            gint pdelete,
;;;                            GdkAtom *actual_property_type,
;;;                            gint *actual_format,
;;;                            gint *actual_length,
;;;                            guchar **data);
;;; 
;;; Retrieves a portion of the contents of a property. If the property does not
;;; exist, then the function returns FALSE, and GDK_NONE will be stored in
;;; actual_property_type.
;;;
;;; Note
;;; 
;;; The XGetWindowProperty() function that gdk_property_get() uses has a very
;;; confusing and complicated set of semantics. Unfortunately,
;;; gdk_property_get() makes the situation worse instead of better (the
;;; semantics should be considered undefined), and also prints warnings to
;;; stderr in cases where it should return a useful error to the program. You
;;; are advised to use XGetWindowProperty() directly until a replacement
;;; function for gdk_property_get() is provided.
;;; 
;;; window :
;;; 	a GdkWindow.
;;; 
;;; property :
;;; 	the property to retrieve.
;;; 
;;; type :
;;; 	the desired property type, or GDK_NONE, if any type of data is
;;;     acceptable. If this does not match the actual type, then actual_format
;;;     and actual_length will be filled in, a warning will be printed to
;;;     stderr and no data will be returned.
;;; 
;;; offset :
;;; 	the offset into the property at which to begin retrieving data, in
;;;     4 byte units.
;;; 
;;; length :
;;; 	the length of the data to retrieve in bytes. Data is considered to be
;;;     retrieved in 4 byte chunks, so length will be rounded up to the next
;;;     highest 4 byte boundary (so be careful not to pass a value that might
;;;     overflow when rounded up).
;;; 
;;; pdelete :
;;; 	if TRUE, delete the property after retrieving the data.
;;; 
;;; actual_property_type :
;;; 	location to store the actual type of the property.
;;; 
;;; actual_format :
;;; 	location to store the actual return format of the data; either 8, 16
;;;     or 32 bits.
;;; 
;;; actual_length :
;;; 	location to store the length of the retrieved data, in bytes. Data
;;;     returned in the 32 bit format is stored in a long variable, so the
;;;     actual number of 32 bit elements should be be calculated via
;;;     actual_length/sizeof(glong) to ensure portability to 64 bit systems.
;;; 
;;; data :
;;; 	location to store a pointer to the data. The retrieved data should be
;;;     freed with g_free() when you are finished using it.
;;; 
;;; Returns :
;;; 	TRUE if data was successfully received and stored in data,
;;;     otherwise FALSE.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_property_change ()
;;; 
;;; void gdk_property_change (GdkWindow *window,
;;;                           GdkAtom property,
;;;                           GdkAtom type,
;;;                           gint format,
;;;                           GdkPropMode mode,
;;;                           const guchar *data,
;;;                           gint nelements);
;;; 
;;; Changes the contents of a property on a window.
;;; 
;;; window :
;;; 	a GdkWindow.
;;; 
;;; property :
;;; 	the property to change.
;;; 
;;; type :
;;; 	the new type for the property. If mode is GDK_PROP_MODE_PREPEND or
;;;     GDK_PROP_MODE_APPEND, then this must match the existing type or an
;;;     error will occur.
;;; 
;;; format :
;;; 	the new format for the property. If mode is GDK_PROP_MODE_PREPEND or
;;;     GDK_PROP_MODE_APPEND, then this must match the existing format or an
;;;     error will occur.
;;; 
;;; mode :
;;; 	a value describing how the new data is to be combined with the current
;;;     data.
;;; 
;;; data :
;;; 	the data (a guchar * gushort *, or gulong *, depending on format),
;;;     cast to a guchar *.
;;; 
;;; nelements :
;;; 	the number of elements of size determined by the format, contained
;;;     in data.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; enum GdkPropMode
;;; 
;;; typedef enum
;;; {
;;;   GDK_PROP_MODE_REPLACE,
;;;   GDK_PROP_MODE_PREPEND,
;;;   GDK_PROP_MODE_APPEND
;;; } GdkPropMode;
;;; 
;;; Describes how existing data is combined with new data when using
;;; gdk_property_change().
;;; 
;;; GDK_PROP_MODE_REPLACE
;;; 	the new data replaces the existing data.
;;; 
;;; GDK_PROP_MODE_PREPEND
;;; 	the new data is prepended to the existing data.
;;; 
;;; GDK_PROP_MODE_APPEND
;;; 	the new data is appended to the existing data.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_property_delete ()
;;; 
;;; void gdk_property_delete (GdkWindow *window, GdkAtom property)
;;; 
;;; Deletes a property from a window.
;;; 
;;; window :
;;; 	a GdkWindow.
;;; 
;;; property :
;;; 	the property to delete.
;;; ----------------------------------------------------------------------------

;;; --- End of file gdk.properties.lisp ----------------------------------------
