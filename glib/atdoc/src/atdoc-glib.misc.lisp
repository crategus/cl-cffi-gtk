;;; ----------------------------------------------------------------------------
;;; atdoc-glib.misc.lisp
;;;
;;; Documentation strings for the library GLib.
;;;
;;; The documentation of this file has been copied from the
;;; GLib 2.32.3 Reference Manual. See http://www.gtk.org.
;;;
;;; Copyright (C) 2012, 2013 Dieter Kaiser
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

;;; --- g-size -----------------------------------------------------------------

(setf (documentation 'g-size 'type)
 "@version{2013-1-15}
  @begin{short}
    An unsigned integer type of the result of the sizeof operator,
    corresponding to the @code{size_t} type defined in C99.
  @end{short}
  This type is wide enough to hold the numeric value of a pointer, so it is
  usually 32bit wide on a 32bit platform and 64bit wide on a 64bit platform.
  Values of this type can range from @code{0} to @code{G_MAXSIZE}.")

;;; --- g-ssize ----------------------------------------------------------------

(setf (documentation 'g-ssize 'type)
 "@version{2013-1-15}
  @begin{short}
    A signed variant of @type{g-size}, corresponding to the @code{ssize_t}
    defined on most platforms.
  @end{short}
  Values of this type can range from @code{G_MINSSIZE} to @code{G_MAXSSIZE}.")

;;; --- g-offset ---------------------------------------------------------------

(setf (documentation 'g-offset 'type)
 "@version{2013-1-15}
  @begin{short}
    A signed integer type that is used for file offsets, corresponding to the
    C99 type @code{off64_t}.
  @end{short}
  Values of this type can range from @code{G_MINOFFSET} to @code{G_MAXOFFSET}.

  Since: 2.14")

;;; --- g-malloc ---------------------------------------------------------------

(setf (documentation 'g-malloc 'function)
 "@version{2013-1-15}
  @argument[n-bytes]{the number of bytes to allocate}
  @return{A foreign pointer to the allocated memory.}
  @begin{short}
    Allocates @arg{n-bytes} bytes of memory. If @arg{n-bytes} is @code{0}
    @sym{g-malloc} returns a foreign @code{null}-pointer.
  @end{short}
  @see{g-free}")

;;; --- g-free -----------------------------------------------------------------

(setf (documentation 'g-free 'function)
 "@version{2013-1-15}
  @argument[mem]{a foreign pointer to the memory to free}
  @begin{short}
    Frees the memory pointed to by the foreign pointer @arg{mem}. If @arg{mem}
    is a @code{null}-pointer @sym{g-free} simply returns.
  @end{short}
  @see{g-malloc}")

;;; --- g-time-val -------------------------------------------------------------

(setf (gethash 'g-time-val atdoc:*type-name-alias*) "CStruct")
(setf (documentation 'g-time-val 'type)
 "@version{2013-1-15}
  @begin{short}
    Represents a precise time, with seconds and microseconds.
  @end{short}
  Similar to the struct @code{timeval} returned by the @code{gettimeofday()}
  UNIX call.

  GLib is attempting to unify around the use of 64bit integers to represent
  microsecond-precision time. As such, this type will be removed from a
  future version of GLib.
  @begin[Lisp Implementation]{dictionary}
    @begin{pre}
(defcstruct g-time-val
  (tv-sec :long)
  (tv-usec :long))
    @end{pre}
    @begin[code]{table}
      @entry[tv-sec]{seconds}
      @entry[tv-usec]{microseconds}
    @end{table}
  @end{dictionary}")

;;; --- g-string ---------------------------------------------------------------

(setf (documentation 'g-string 'type)
 "@version{2013-1-15}
  @begin{short}
    A type that is almost like the foreign CFFI type @code{:string} but uses the
    GLib functions @fun{g-malloc} and @fun{g-free} to allocate and free memory.
  @end{short}

  The @sym{g-string} type performs automatic conversion between Lisp and C
  strings. Note that, in the case of functions the converted C string will
  have dynamic extent (i.e. it will be automatically freed after the foreign
  function returns).

  In addition to Lisp strings, this type will accept foreign pointers and pass
  them unmodified.

  A method for free-translated-object is specialized for this type. So, for
  example, foreign strings allocated by this type and passed to a foreign
  function will be freed after the function returns.
  @see-type{g-strv}")

;;; --- g-strv -----------------------------------------------------------------

(setf (documentation 'g-strv 'type)
 "@version{2013-1-15}
  @begin{short}
    This type represents and performs automatic conversion between a list of
    Lisp strings and an array of C strings of type @type{g-string}.
  @end{short}
  
  @begin[Example]{dictionary}
    @begin{pre}
 (setq str (convert-to-foreign (list \"Hello\" \"World\") 'g-strv))
=> #.(SB-SYS:INT-SAP #X01541998)
 (convert-from-foreign str 'g-strv)
=> (\"Hello\" \"World\")
    @end{pre}
  @end{dictionary}
  @see-type{g-string}")

;;; ----------------------------------------------------------------------------

;;; This function is for internal use and not exported.

(setf (documentation 'g-strdup 'function)
 "@argument[str]{the string to duplicate}
  @return{a newly-allocated copy of @arg{str}}
  @short{Duplicates a string.}
  If @arg{str} is @code{NULL} it returns @code{NULL}. The returned string should
  be freed with @fun{g-free} when no longer needed.")

;;; --- g-list -----------------------------------------------------------------

(setf (documentation 'g-list 'type)
 "@version{2013-1-15}
  @begin{short}
    The @sym{g-list} type represents a C doubly-linked list with elements of
    type @code{GList} struct.
  @end{short}
  The type @sym{g-list} performs automatic conversion from a C list to a Lisp
  list. The conversion from a Lisp list to a C reprensentation is not
  implemented.")

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

;;; --- g-slist ----------------------------------------------------------------

(setf (documentation 'g-slist 'type)
 "@version{2013-1-15}
  @begin{short}
    The @sym{g-slist} type represents a C singly-linked list with elements of
    type @code{GSList} struct.
  @end{short}
  The type @sym{g-slist} performs automatic conversion from a C list to a Lisp
  list. The conversion from a Lisp list to a C reprensentation is not
  implemented.")

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
