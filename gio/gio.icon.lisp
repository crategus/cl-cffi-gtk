;;; ----------------------------------------------------------------------------
;;; gio.icon.lisp
;;;
;;; The documentation of this file is taken from the GIO Reference Manual
;;; Version 2.40 and modified to document the Lisp binding to the GIO library.
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
;;; GIcon
;;;
;;; Interface for icons
;;;
;;; Synopsis
;;;
;;;     GIcon
;;;     GIconIface
;;;
;;;     g_icon_hash
;;;     g_icon_equal
;;;     g_icon_to_string
;;;     g_icon_new_for_string
;;;
;;; Object Hierarchy
;;;
;;;   GInterface
;;;    +----GIcon
;;; ----------------------------------------------------------------------------

(in-package :gio)

;;; ----------------------------------------------------------------------------
;;; GIcon
;;;
;;; typedef struct _GIcon GIcon;
;;;
;;; An abstract type that specifies an icon.
;;; ----------------------------------------------------------------------------

(define-g-interface "GIcon" g-icon
  (:export t
   :type-initializer "g_icon_get_type"))

#+cl-cffi-gtk-documentation
(setf (gethash 'g-icon atdoc:*class-name-alias*) "Interface"
      (documentation 'g-icon 'type)
 "@version{2014-9-20}
  @begin{short}
    @sym{g-icon} is a very minimal interface for icons. It provides functions
    for checking the equality of two icons, hashing of icons and serializing an
    icon to and from strings.
  @end{short}

  @sym{g-icon} does not provide the actual pixmap for the icon as this is out
  of GIO's scope, however implementations of @sym{g-icon} may contain the name
  of an icon, see @class{g-themed-icon}, or the path to an icon, see
  @class{g-loadable-icon}.

  To obtain a hash of a @sym{g-icon}, see the function @fun{g-icon-hash}. To
  check if two @sym{g-icon}s are equal, see the function @fun{g-icon-equal}.
  For serializing a @sym{g-icon}, use the functions @fun{g-icon-to-string} and
  @fun{g-icon-new-for-string}.

  If your application or library provides one or more @sym{g-icon}
  implementations you need to ensure that each @class{g-type} is registered with
  the type system prior to calling the function @fun{g-icon-new-for-string}.
  @see-class{g-themed-icon}
  @see-class{g-loadable-icon}
  @see-function{g-icon-hash}
  @see-function{g-icon-equal}
  @see-function{g-icon-to-string}
  @see-function{g-icon-new-for-string}")

;;; ----------------------------------------------------------------------------
;;; struct GIconIface
;;;
;;; struct GIconIface {
;;;   GTypeInterface g_iface;
;;;
;;;   /* Virtual Table */
;;;
;;;   guint       (* hash)        (GIcon   *icon);
;;;   gboolean    (* equal)       (GIcon   *icon1,
;;;                                GIcon   *icon2);
;;;   gboolean    (* to_tokens)   (GIcon   *icon,
;;;                    GPtrArray *tokens,
;;;                                gint    *out_version);
;;;   GIcon *     (* from_tokens) (gchar  **tokens,
;;;                                gint     num_tokens,
;;;                                gint     version,
;;;                                GError **error);
;;; };
;;;
;;; GIconIface is used to implement GIcon types for various different systems.
;;; See GThemedIcon and GLoadableIcon for examples of how to implement this
;;; interface.
;;;
;;; GTypeInterface g_iface;
;;;     The parent interface.
;;;
;;; hash ()
;;;     A hash for a given GIcon.
;;;
;;; equal ()
;;;     Checks if two GIcons are equal.
;;;
;;; to_tokens ()
;;;     Serializes a GIcon into tokens. The tokens must not contain any
;;;     whitespace. Don't implement if the GIcon can't be serialized
;;;     (Since 2.20).
;;;
;;; from_tokens ()
;;;     Constructs a GIcon from tokens. Set the GError if the tokens are
;;;     malformed. Don't implement if the GIcon can't be serialized
;;;     (Since 2.20).
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_icon_hash ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_icon_hash" g-icon-hash) :uint
 #+cl-cffi-gtk-documentation
 "@version{2013-7-19}
  @argument[icon]{a @class{g-icon} object}
  @begin{return}
    An unsigned integer containing a hash for the icon, suitable for use in a
    Hash table or similar data structure.
  @end{return}
  Gets a hash for an icon."
  (icon (g-object g-icon)))

(export 'g-icon-hash)

;;; ----------------------------------------------------------------------------
;;; g_icon_equal ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_icon_equal" g-icon-equal) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-7-19}
  @argument[icon1]{the first @class{g-icon} object}
  @argument[icon2]{the second @class{g-icon} object}
  @begin{return}
    @em{True} if @arg{icon1} is equal to @arg{icon2}, @code{nil} otherwise.
  @end{return}
  Checks if two icons are equal."
  (icon1 (g-object g-icon))
  (icon2 (g-object g-icon)))

(export 'g-icon-equal)

;;; ----------------------------------------------------------------------------
;;; g_icon_to_string ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_icon_to_string" g-icon-to-string) (:string :free-from-foreign t)
 #+cl-cffi-gtk-documentation
 "@version{2013-7-19}
  @argument[icon]{a @class{g-icon} object}
  @begin{return}
    An UTF8 string or @code{nil} if @arg{icon} cannot be serialized.
  @end{return}
  @begin{short}
    Generates a textual representation of @arg{icon} that can be used for
    serialization such as when passing @arg{icon} to a different process or
    saving it to persistent storage.
  @end{short}
  Use the function @fun{g-icon-new-for-string} to get @arg{icon} back from
  the returned string.

  The encoding of the returned string is proprietary to @class{g-icon} except in
  the following two cases
  @begin{itemize}
    @begin{item}
      If @arg{icon} is a @class{g-file-icon}, the returned string is a native
      path (such as /path/to/my icon.png) without escaping if the @class{g-file}
      for icon is a native file. If the file is not native, the returned string
      is the result of the function @fun{g-file-get-uri} (such as
       sftp://path/to/my%20icon.png).
    @end{item}
    @begin{item}
      If @arg{icon} is a @class{g-themed-icon} with exactly one name, the
      encoding is simply the name (such as network-server).
    @end{item}
  @end{itemize}
  Since 2.20
  @see-function{g-icon-new-for-string}"
  (icon (g-object g-icon)))

(export 'g-icon-to-string)

;;; ----------------------------------------------------------------------------
;;; g_icon_new_for_string ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_icon_new_for_string" %g-icon-new-for-string) (g-object g-icon)
  (str :string)
  (error :pointer))

(defun g-icon-new-for-string (str)
 #+cl-cffi-gtk-documentation
 "@version{2013-7-19}
  @argument[str]{a string obtained via the function @fun{g-icon-to-string}}
  @begin{return}
    An object implementing the @class{g-icon} interface or
    nil if an error is set.
  @end{return}
  @begin{short}
    Generate a @class{g-icon} instance from @arg{str}.
  @end{short}
  This function can fail if @arg{str} is not valid - see the function
  @fun{g-icon-to-string} for discussion.

  If your application or library provides one or more @class{g-icon}
  implementations you need to ensure that each @class{g-type} is registered
  with the type system prior to calling the function
  @fun{g-icon-new-for-string}.

  Since 2.20
  @see-function{g-icon-to-string}"
  (with-g-error (err)
    (%g-icon-new-for-string str err)))

(export 'g-icon-new-for-string)

;;; --- End of file gio.icon.lisp ----------------------------------------------
