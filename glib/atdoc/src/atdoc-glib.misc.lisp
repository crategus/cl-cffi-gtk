;;; ----------------------------------------------------------------------------
;;; atdoc-glib.misc.lisp
;;;
;;; Documentation strings for the library GLib.
;;;
;;; The documentation of this file has been copied from the
;;; GLib 2.32.3 Reference Manual. See http://www.gtk.org.
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

(in-package :glib)

;;; ----------------------------------------------------------------------------

(setf (documentation 'g-size 'type)
 "@version{2012-12-24}
  @begin{short}
    An unsigned integer type of the result of the sizeof operator,
    corresponding to the size_t type defined in C99.
  @end{short}
  This type is wide enough to hold the numeric value of a pointer, so it is
  usually 32bit wide on a 32bit platform and 64bit wide on a 64bit platform.
  Values of this type can range from 0 to G_MAXSIZE.
  
  To print or scan values of this type, use G_GSIZE_MODIFIER and/or
  G_GSIZE_FORMAT.")

;;; ----------------------------------------------------------------------------

(setf (documentation 'g-ssize 'type)
 "@version{2012-12-24}
  @begin{short}
    A signed variant of @type{g-size}, corresponding to the ssize_t defined
    on most platforms.
  @end{short}
  Values of this type can range from G_MINSSIZE to G_MAXSSIZE.
  
  To print or scan values of this type, use G_GSIZE_MODIFIER and/or
  G_GSSIZE_FORMAT.")

;;; ----------------------------------------------------------------------------

(setf (documentation 'g-offset 'type)
 "@version{2012-12-24}
  @begin{short}
    A signed integer type that is used for file offsets, corresponding to the
    C99 type off64_t.
  @end{short}
  Values of this type can range from G_MINOFFSET to G_MAXOFFSET.

  To print or scan values of this type, use G_GOFFSET_MODIFIER and/or
  G_GOFFSET_FORMAT.

  Since: 2.14")

;;; ----------------------------------------------------------------------------

(setf (documentation 'g-malloc 'function)
 "@version{2012-12-24}
  @argument[n-bytes]{the number of bytes to allocate}
  @return{A pointer to the allocated memory.}
  @begin{short}
    Allocates @code{n_bytes} bytes of memory. If @code{n_bytes} is 0 it returns
    NULL.
  @end{short}
  @see{g-free}")

;;; ----------------------------------------------------------------------------

(setf (documentation 'g-free 'function)
 "@version{2012-12-24}
  @argument[mem]{the memory to free}
  @begin{short}
    Frees the memory pointed to by @code{mem}. If @code{mem} is NULL it simply
    returns.
  @end{short}
  @see{g-malloc}")

;;; ----------------------------------------------------------------------------

(setf (gethash 'g-time-val atdoc:*type-name-alias*) "CStruct")
(setf (documentation 'g-time-val 'type)
 "@version{2012-12-24}
  @begin{short}
    Represents a precise time, with seconds and microseconds.
  @end{short}
  Similar to the struct @code{timeval} returned by the @code{gettimeofday()}
  UNIX call.

  GLib is attempting to unify around the use of 64bit integers to represent
  microsecond-precision time. As such, this type will be removed from a
  future version of GLib.
  @begin{pre}
(defcstruct g-time-val
  (tv-sec :long)
  (tv-usec :long))
  @end{pre}
  @begin{table}
    @entry[tv-sec]{seconds}
    @entry[tv-usec]{microseconds}
  @end{table}")

;;; ----------------------------------------------------------------------------

(setf (documentation 'g-string 'type)
 "@version{2012-12-24}
  @begin{short}
    The @sym{g-string} struct contains the public fields of a @code{GString}.
  @end{short}
  @begin{pre}
  struct GString {
    gchar  *str;
    gsize   len;
    gsize   allocated_len;
  @};
  @end{pre}
  @begin{table}
    @entry[gchar *str]{points to the character data. It may move as text is
      added. The @arg{str} field is null-terminated and so can be used as an
      ordinary C string.} 
    @entry[gsize len]{contains the length of the string, not including the
      terminating nul byte.}
    @entry[gsize allocated_len]{the number of bytes that can be stored in the
      string before it needs to be reallocated. May be larger than len.}
  @end{table}
  @begin[Lisp Implementation]{dictionary}
    A type that it almost like the foreign type @code{:string} but uses
    @fun{g-malloc} and @fun{g-free}.
  @end{dictionary}")

;;; ----------------------------------------------------------------------------

(setf (documentation 'g-strv 'type)
 "@version{2012-12-24}
  @short{A C representable type name for @code{G_TYPE_STRV}.}")

;;; ----------------------------------------------------------------------------

;;; This function is for internal use and not exported.

(setf (documentation 'g-strdup 'function)
 "@argument[str]{the string to duplicate}
  @return{a newly-allocated copy of @arg{str}}
  @short{Duplicates a string.}
  If @arg{str} is @code{NULL} it returns @code{NULL}. The returned string should
  be freed with @fun{g-free} when no longer needed.")

;;; ----------------------------------------------------------------------------

(setf (documentation 'g-list 'type)
 "@version{2012-12-24}
  @begin{short}
    The GList struct is used for each element in a doubly-linked list.
  @end{short}
  @begin{pre}
  struct GList {
    gpointer data;
    GList   *next;
    GList   *prev;
  @};
  @end{pre}
  @begin{table}
    @entry[gpointer data]{holds the element's data, which can be a pointer to
      any kind of data, or any integer value using the Type Conversion Macros.}
    @entry[GList *next]{contains the link to the next element in the list.} 
    @entry[GList *prev]{contains the link to the previous element in the list.}
  @end{table}")

;;; ----------------------------------------------------------------------------

;;; This function is for internal use and not exported

(setf (documentation 'g-list-free 'function)
 "@version{2012-12-24}
  @argument[lst]{a @code{GList}}
  @begin{short}
    Frees all of the memory used by a GList. The freed elements are returned to
    the slice allocator.
  @end{short}
  @begin[Note]{dictionary}
    If list elements contain dynamically-allocated memory, you should either
    use @code{g_list_free_full()} or free them manually first.
  @end{dictionary}")

;;; ----------------------------------------------------------------------------

;;; This function is for internal use and not exported.

(setf (documentation 'g-list-next 'function)
 "@version{2012-12-24}
  @argument[list]{an element in a GList.}
  @return{the next element, or NULL if there are no more elements.}
  @short{A convenience macro to get the next element in a GList.}")

;;; ----------------------------------------------------------------------------

(setf (documentation 'g-slist 'type)
 "@version{2012-12-24}
  @begin{short}
    The GSList struct is used for each element in the singly-linked list.
  @end{short}
  @begin{pre}
  GSList

  struct GSList {
    gpointer data;
    GSList  *next;
  @};
  @end{pre}
  @begin{table}
    @entry[gpointer data]{holds the element's data, which can be a pointer to
      any kind of data, or any integer value using the Type Conversion Macros.} 
    @entry[GSList *next]{contains the link to the next element in the list.}
  @end{table}")


;;; ----------------------------------------------------------------------------

;;; This function is not exported.

(setf (documentation 'g-slist-alloc 'function)
 "@return{a pointer to the newly-allocated GSList element.}
  @short{Allocates space for one GSList element.}
  It is called by the g_slist_append(), g_slist_prepend(), g_slist_insert() and
  g_slist_insert_sorted() functions and so is rarely used on its own.")

;;; ----------------------------------------------------------------------------

;;; This function is not exported.

(setf (documentation 'g-slist-free 'function)
 "@argument[lst]{a GSList}
  @short{Frees all of the memory used by a GSList.}
  The freed elements are returned to the slice allocator.
  @begin[Note]{dictionary}
    If list elements contain dynamically-allocated memory, you should either
    use g_slist_free_full() or free them manually first.
  @end{dictionary}")

;;; ----------------------------------------------------------------------------

;;; This function is not exported.

(setf (documentation 'g-slist-next 'function)
 "@argument[slist]{an element in a GSList.}
  @return{the next element, or NULL if there are no more elements.}
  @short{A convenience macro to get the next element in a GSList.}")

;;; --- End of file atdoc-glib.misc.lisp ---------------------------------------
