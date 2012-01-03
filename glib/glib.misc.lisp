;;; ----------------------------------------------------------------------------
;;; glib.misc.lisp
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
;;; This file contains several type definitions and functions, which are
;;; needed for the implemenation of the GTK library, but are not fully
;;; implemented.
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Basic Types
;;; 
;;; Standard GLib types, defined for ease-of-use and portability
;;;
;;; Only the following types are implemented:
;;;
;;;    g-size
;;;    g-ssize
;;;    g-offset
;;; ----------------------------------------------------------------------------
;;;
;;; Memory Allocation
;;; 
;;; The following functions for the general memory-handling are implemented:
;;;
;;;    g-malloc (n-bytes)
;;;    g-free (mem)
;;; ----------------------------------------------------------------------------
;;;
;;; Date and Time Functions
;;; 
;;; Calendrical calculations and miscellaneous time stuff
;;;
;;; Only the following struct is implemented:
;;;
;;;    g-time-val
;;; ----------------------------------------------------------------------------
;;;
;;; Spawning Processes - Process launching
;;;
;;; The following bitfield is needed:
;;;
;;;    g-spawn-flags
;;; ----------------------------------------------------------------------------
;;;
;;; String Utility Functions - Various string-related functions
;;;
;;; Implemented is:
;;; 
;;;    g-string
;;;    g-string-type
;;;    g-strv
;;;    g-strv-type
;;;
;;;    g-strdup (str)
;;; ----------------------------------------------------------------------------
;;; 
;;; Doubly-Linked Lists
;;;
;;; Linked lists containing integer values or pointers to data, with the ability
;;; to iterate over the list in both directions
;;;
;;; Implemented is:
;;;
;;;    g-list
;;;    g-list-type
;;;
;;;    g-list-free (lst)
;;;    g-list-next (lst)
;;; ----------------------------------------------------------------------------
;;;
;;; Singly-Linked Lists
;;; 
;;; Linked lists containing integer values or pointers to data, limited to
;;; iterating over the list in one direction
;;;
;;; Implemented is:
;;;
;;;    g-slist
;;;    g-slist-type
;;;
;;;    g-slist-alloc ()
;;;    g-slist-free (lst)
;;;    g-slist-next (lst)
;;; ----------------------------------------------------------------------------

(in-package :glib)

;;; ----------------------------------------------------------------------------
;;; g-size
;;; 
;;; typedef unsigned long gsize;
;;; 
;;; An unsigned integer type of the result of the sizeof operator,
;;; corresponding to the size_t type defined in C99. This type is wide enough
;;; to hold the numeric value of a pointer, so it is usually 32bit wide on a
;;; 32bit platform and 64bit wide on a 64bit platform. Values of this type can
;;; range from 0 to G_MAXSIZE.
;;; 
;;; To print or scan values of this type, use G_GSIZE_MODIFIER and/or
;;; G_GSIZE_FORMAT.
;;; ----------------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (cond
    ((cffi-features:cffi-feature-p :x86-64) (defctype g-size :uint64))
    ((cffi-features:cffi-feature-p :x86)    (defctype g-size :ulong))
    ((cffi-features:cffi-feature-p :ppc32)  (defctype g-size :uint32))
    ((cffi-features:cffi-feature-p :ppc64)  (defctype g-size :uint64))
    (t
     (error "Can not define 'g-size', unknown CPU architecture ~
            (known are x86 and x86-64)"))))

(export 'g-size)

;;; ----------------------------------------------------------------------------
;;; g-ssize
;;; 
;;; typedef signed long gssize;
;;; 
;;; A signed variant of gsize, corresponding to the ssize_t defined on most
;;; platforms. Values of this type can range from G_MINSSIZE to G_MAXSSIZE.
;;; 
;;; To print or scan values of this type, use G_GSIZE_MODIFIER and/or
;;; G_GSSIZE_FORMAT.
;;; ----------------------------------------------------------------------------

(defctype g-ssize :long)

(export 'g-ssize)

;;; ----------------------------------------------------------------------------
;;; g-offset
;;; 
;;; typedef gint64 goffset;
;;; 
;;; A signed integer type that is used for file offsets, corresponding to the
;;; C99 type off64_t. Values of this type can range from G_MINOFFSET to
;;; G_MAXOFFSET.
;;; 
;;; To print or scan values of this type, use G_GOFFSET_MODIFIER and/or
;;; G_GOFFSET_FORMAT.
;;;
;;; Since: 2.14
;;; ----------------------------------------------------------------------------

(defctype g-offset :uint64)

(export 'g-offset)

;;; ----------------------------------------------------------------------------
;;; g-malloc (n-bytes)
;;; 
;;; gpointer g_malloc (gsize n_bytes)
;;; 
;;; Allocates n_bytes bytes of memory. If n_bytes is 0 it returns NULL.
;;; 
;;; n-bytes :
;;;     the number of bytes to allocate
;;; 
;;; Returns :
;;;     a pointer to the allocated memory
;;; ----------------------------------------------------------------------------

(defcfun ("g_malloc" g-malloc) :pointer
  (n-bytes g-size))

(export 'g-malloc)

;;; ----------------------------------------------------------------------------
;;; g-free (mem)
;;; 
;;; void g_free (gpointer mem)
;;; 
;;; Frees the memory pointed to by mem. If mem is NULL it simply returns.
;;; 
;;; mem :
;;;     the memory to free
;;; ----------------------------------------------------------------------------

(defcfun ("g_free" g-free) :void
  (mem :pointer))

(export 'g-free)

;;; ----------------------------------------------------------------------------
;;; g-time-val
;;; 
;;; struct GTimeVal {
;;;   glong tv_sec;
;;;   glong tv_usec;
;;; };
;;; 
;;; Represents a precise time, with seconds and microseconds. Similar to the
;;; struct timeval returned by the gettimeofday() UNIX call.
;;; 
;;; GLib is attempting to unify around the use of 64bit integers to represent
;;; microsecond-precision time. As such, this type will be removed from a
;;; future version of GLib.
;;; 
;;; glong tv_sec;
;;; 	seconds
;;; 
;;; glong tv_usec;
;;; 	microseconds
;;; ----------------------------------------------------------------------------

(defcstruct g-time-val
  (tv-sec :long)
  (tv-usec :long))

(export 'g-time-val)

;;; ----------------------------------------------------------------------------
;;; g-spawn-flags
;;; 
;;; typedef enum {
;;;   G_SPAWN_LEAVE_DESCRIPTORS_OPEN = 1 << 0,
;;;   G_SPAWN_DO_NOT_REAP_CHILD      = 1 << 1,
;;;   /* look for argv[0] in the path i.e. use execvp() */
;;;   G_SPAWN_SEARCH_PATH            = 1 << 2,
;;;   /* Dump output to /dev/null */
;;;   G_SPAWN_STDOUT_TO_DEV_NULL     = 1 << 3,
;;;   G_SPAWN_STDERR_TO_DEV_NULL     = 1 << 4,
;;;   G_SPAWN_CHILD_INHERITS_STDIN   = 1 << 5,
;;;   G_SPAWN_FILE_AND_ARGV_ZERO     = 1 << 6
;;; } GSpawnFlags;
;;; 
;;; Flags passed to g_spawn_sync(), g_spawn_async() and
;;; g_spawn_async_with_pipes().
;;; 
;;; G_SPAWN_LEAVE_DESCRIPTORS_OPEN
;;; 	the parent's open file descriptors will be inherited by the child;
;;;     otherwise all descriptors except stdin/stdout/stderr will be closed
;;;     before calling exec() in the child.
;;; 
;;; G_SPAWN_DO_NOT_REAP_CHILD
;;; 	the child will not be automatically reaped; you must use
;;;     g_child_watch_add() yourself (or call waitpid() or handle SIGCHLD
;;;     yourself), or the child will become a zombie.
;;; 
;;; G_SPAWN_SEARCH_PATH
;;; 	argv[0] need not be an absolute path, it will be looked for in the
;;;     user's PATH.
;;; 
;;; G_SPAWN_STDOUT_TO_DEV_NULL
;;; 	the child's standard output will be discarded, instead of going to the
;;;     same location as the parent's standard output.
;;; 
;;; G_SPAWN_STDERR_TO_DEV_NULL
;;; 	the child's standard error will be discarded.
;;; 
;;; G_SPAWN_CHILD_INHERITS_STDIN
;;; 	the child will inherit the parent's standard input (by default, the
;;;     child's standard input is attached to /dev/null).
;;; 
;;; G_SPAWN_FILE_AND_ARGV_ZERO
;;; 	the first element of argv is the file to execute, while the remaining
;;;     elements are the actual argument vector to pass to the file. Normally
;;;     g_spawn_async_with_pipes() uses argv[0] as the file to execute, and
;;;     passes all of argv to the child.
;;; ----------------------------------------------------------------------------

(defbitfield g-spawn-flags
  :leave-descriptors-open
  :do-not-reap-child
  :search-path
  :stdout-to-dev-null
  :stderr-to-dev-null
  :child-inherits-stdin
  :file-and-argv-zero)

(export 'g-spawn-flags)

;;; ----------------------------------------------------------------------------
;;; g-string
;;; g-string-type
;;; 
;;; struct GString {
;;;   gchar  *str;
;;;   gsize len;    
;;;   gsize allocated_len;
;;; };
;;; 
;;; The GString struct contains the public fields of a GString.
;;; 
;;; gchar *str;
;;; 	points to the character data. It may move as text is added. The str
;;;     field is null-terminated and so can be used as an ordinary C string.
;;; 
;;; gsize len;
;;; 	contains the length of the string, not including the terminating nul
;;;     byte.
;;; 
;;; gsize allocated_len;
;;; 	the number of bytes that can be stored in the string before it needs to
;;;     be reallocated. May be larger than len.
;;; ----------------------------------------------------------------------------

;; A type that it almost like :string but uses g_malloc and g_free

(define-foreign-type g-string-type ()
  ((free-from-foreign :initarg :fff
                      :reader g-string-type-fff
                      :initform nil)
   (free-to-foreign :initarg :ftf
                    :reader g-string-type-ftf
                    :initform t))
  (:actual-type :pointer))

(define-parse-method g-string (&key (free-from-foreign nil) (free-to-foreign t))
  (make-instance 'g-string-type :fff free-from-foreign :ftf free-to-foreign))

(defmethod translate-to-foreign (value (type g-string-type))
  (g-strdup value))

(defmethod translate-from-foreign (value (type g-string-type))
  (prog1
      (convert-from-foreign value '(:string :free-from-foreign nil))
    (when (g-string-type-fff type)
      (g-free value))))

(export 'g-string)

;;; ----------------------------------------------------------------------------
;;; g-strv
;;; g-strv-type
;;; 
;;; typedef gchar** GStrv;
;;; 
;;; A C representable type name for G_TYPE_STRV.
;;; ----------------------------------------------------------------------------

;;; Another string type g-strv

(define-foreign-type g-strv-type ()
  ((free-from-foreign :initarg :free-from-foreign
                      :initform t
                      :reader g-strv-type-fff)
   (free-to-foreign :initarg :free-to-foreign
                    :initform t
                    :reader g-strv-type-ftf))
  (:actual-type :pointer))

(define-parse-method g-strv (&key (free-from-foreign t) (free-to-foreign t))
  (make-instance 'g-strv-type
                 :free-from-foreign free-from-foreign
                 :free-to-foreign free-to-foreign))

(defmethod translate-from-foreign (value (type g-strv-type))
  (unless (null-pointer-p value)
    (prog1
        (iter (for i from 0)
              (for str-ptr = (mem-aref value :pointer i))
              (until (null-pointer-p str-ptr))
              (collect (convert-from-foreign str-ptr
                                             '(:string :free-from-foreign nil)))
              (when (g-strv-type-fff type)
                (g-free str-ptr)))
      (when (g-strv-type-fff type)
        (g-free value)))))

(defmethod translate-to-foreign (str-list (type g-strv-type))
  (let* ((n (length str-list))
         (result (g-malloc (* (1+ n) (foreign-type-size :pointer)))))
    (iter (for i from 0)
          (for str in str-list)
          (setf (mem-aref result :pointer i) (g-strdup str)))
    (setf (mem-aref result :pointer n) (null-pointer))
    result))

(export 'g-strv)

;;; ----------------------------------------------------------------------------
;;; g-strdup (str)
;;; 
;;; gchar * g_strdup (const gchar *str)
;;; 
;;; Duplicates a string. If str is NULL it returns NULL. The returned string
;;; should be freed with g_free() when no longer needed.
;;; 
;;; str :
;;; 	the string to duplicate
;;; 
;;; Returns :
;;; 	a newly-allocated copy of str
;;; ----------------------------------------------------------------------------

(defcfun ("g_strdup" g-strdup) :pointer
  (str (:string :free-to-foreign t)))

;;; ----------------------------------------------------------------------------
;;; g-list
;;; g-list-type
;;; 
;;; struct GList {
;;;   gpointer data;
;;;   GList *next;
;;;   GList *prev;
;;; };
;;; 
;;; The GList struct is used for each element in a doubly-linked list.
;;; 
;;; gpointer data;
;;; 	holds the element's data, which can be a pointer to any kind of data,
;;;     or any integer value using the Type Conversion Macros.
;;; 
;;; GList *next;
;;; 	contains the link to the next element in the list.
;;; 
;;; GList *prev;
;;; 	contains the link to the previous element in the list.
;;; ----------------------------------------------------------------------------

(defcstruct %g-list
  (data :pointer)
  (next :pointer)
  (prev :pointer))

(define-foreign-type g-list-type ()
  ((type :reader g-list-type-type
         :initarg :type
         :initform :pointer)
   (free-from-foreign :reader g-list-type-free-from-foreign 
                      :initarg :free-from-foreign
                      :initform t)
   (free-to-foreign :reader g-list-type-free-to-foreign
                    :initarg :free-to-foreign
                    :initform t))
  (:actual-type :pointer))

(define-parse-method g-list (type &key (free-from-foreign t)
                                       (free-to-foreign t))
  (make-instance 'g-list-type
                 :type type
                 :free-from-foreign free-from-foreign
                 :free-to-foreign free-to-foreign))

(defmethod translate-from-foreign (pointer (type g-list-type))
  (prog1
      (iter (for c initially pointer then (g-list-next c))
            (until (null-pointer-p c))
            (collect (convert-from-foreign (foreign-slot-value c '%g-list 'data)
                                           (g-list-type-type type))))
    (when (g-list-type-free-from-foreign type)
      (g-list-free pointer))))

(export 'g-list)

;;; ----------------------------------------------------------------------------
;;; g-list-free (lst)
;;; 
;;; void g_list_free (GList *list)
;;; 
;;; Frees all of the memory used by a GList. The freed elements are returned to
;;; the slice allocator.
;;; 
;;; Note
;;; 
;;; If list elements contain dynamically-allocated memory, you should either
;;; use g_list_free_full() or free them manually first.
;;; 
;;; lst :
;;; 	a GList
;;; ----------------------------------------------------------------------------

(defcfun ("g_list_free" g-list-free) :void
  (lst (:pointer %g-list)))

(export 'g-list-free)

;;; ----------------------------------------------------------------------------
;;; g-list-next (list)
;;; 
;;; #define g_list_next(list)
;;; 
;;; A convenience macro to get the next element in a GList.
;;; 
;;; list :
;;; 	an element in a GList.
;;; 
;;; Returns :
;;; 	the next element, or NULL if there are no more elements.
;;; ----------------------------------------------------------------------------

(defun g-list-next (lst)
  (if (null-pointer-p lst)
      (null-pointer)
      (foreign-slot-value lst '%g-list 'next)))

(export 'g-list-next)

;;; ----------------------------------------------------------------------------
;;; g-slist
;;; g-slist-type
;;; 
;;; struct GSList {
;;;   gpointer data;
;;;   GSList *next;
;;; };
;;; 
;;; The GSList struct is used for each element in the singly-linked list.
;;; 
;;; gpointer data;
;;; 	holds the element's data, which can be a pointer to any kind of data,
;;;     or any integer value using the Type Conversion Macros.
;;; 
;;; GSList *next;
;;; 	contains the link to the next element in the list.
;;; ----------------------------------------------------------------------------

(defcstruct %g-slist
  (data :pointer)
  (next :pointer))

(define-foreign-type g-slist-type ()
  ((type :reader g-slist-type-type :initarg :type :initform :pointer)
   (free-from-foreign :reader g-slist-type-free-from-foreign
                      :initarg :free-from-foreign
                      :initform t)
   (free-to-foreign :reader g-slist-type-free-to-foreign
                    :initarg :free-to-foreign
                    :initform t))
  (:actual-type :pointer))

(define-parse-method g-slist (type &key (free-from-foreign t)
                                        (free-to-foreign t))
  (make-instance 'g-slist-type
                 :type type
                 :free-from-foreign free-from-foreign
                 :free-to-foreign free-to-foreign))

(defmethod translate-from-foreign (pointer (type g-slist-type))
  (prog1
    (iter (for c initially pointer then (g-slist-next c))
          (until (null-pointer-p c))
          (collect (convert-from-foreign (foreign-slot-value c '%g-slist 'data)
                                         (g-slist-type-type type))))
    (when (g-slist-type-free-from-foreign type)
      (g-slist-free pointer))))

(defmethod translate-to-foreign (lst (type g-slist-type))
  (let ((result (null-pointer))
        last)
    (iter (for item in lst)
          (for n = (g-slist-alloc))
          (for ptr = (convert-to-foreign item (g-slist-type-type type)))
          (setf (foreign-slot-value n '%g-slist 'data) ptr)
          (setf (foreign-slot-value n '%g-slist 'next) (null-pointer))
          (when last
            (setf (foreign-slot-value last '%g-slist 'next) n))
          (setf last n)
          (when (first-iteration-p)
            (setf result n)))
    result))

(export 'g-slist)

;;; ----------------------------------------------------------------------------
;;; g-slist-alloc ()
;;; 
;;; GSList * g_slist_alloc (void)
;;; 
;;; Allocates space for one GSList element. It is called by the
;;; g_slist_append(), g_slist_prepend(), g_slist_insert() and
;;; g_slist_insert_sorted() functions and so is rarely used on its own.
;;; 
;;; Returns :
;;; 	a pointer to the newly-allocated GSList element.
;;; ----------------------------------------------------------------------------

(defcfun ("g_slist_alloc" g-slist-alloc)
  (:pointer %g-slist))

(export 'g-slist-alloc)

;;; ----------------------------------------------------------------------------
;;; g-slist-free (lst)
;;; 
;;; void g_slist_free (GSList *list)
;;; 
;;; Frees all of the memory used by a GSList. The freed elements are returned
;;; to the slice allocator.
;;; 
;;; Note
;;; 
;;; If list elements contain dynamically-allocated memory, you should either
;;; use g_slist_free_full() or free them manually first.
;;; 
;;; lst :
;;; 	a GSList
;;; ----------------------------------------------------------------------------

(defcfun ("g_slist_free" g-slist-free) :void
  (lst (:pointer %g-slist)))

(export 'g-slist-free)

;;; ----------------------------------------------------------------------------
;;; g_slist_next()
;;; 
;;; #define g_slist_next(slist)
;;; 
;;; A convenience macro to get the next element in a GSList.
;;; 
;;; slist :
;;; 	an element in a GSList.
;;; 
;;; Returns :
;;; 	the next element, or NULL if there are no more elements.
;;; ----------------------------------------------------------------------------

(defun g-slist-next (lst)
  (if (null-pointer-p lst)
      (null-pointer)
      (foreign-slot-value lst '%g-slist 'next)))

(export 'g-slist-next)

;;; --- End of file glib.misc.lisp ---------------------------------------------
