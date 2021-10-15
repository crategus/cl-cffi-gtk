;;; ----------------------------------------------------------------------------
;;; gio.file-icon.lisp
;;;
;;; The documentation of this file is taken from the GIO Reference Manual
;;; Version 2.68 and modified to document the Lisp binding to the GIO library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2021 Dieter Kaiser
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
;;; GFileIcon
;;;
;;;     Icons pointing to an image file
;;;
;;; Types and Values
;;;
;;;     GFileIcon
;;;
;;; Functions
;;;
;;;     g_file_icon_new
;;;     g_file_icon_get_file
;;;
;;; Properties
;;;
;;;     GFile*   file
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GFileIcon
;;;
;;; Implemented Interfaces
;;;
;;;     GFileIcon implements GIcon and GLoadableIcon.
;;; ----------------------------------------------------------------------------

(in-package :gio)

;;; ----------------------------------------------------------------------------
;;; GFileIcon
;;; ----------------------------------------------------------------------------

(define-g-object-class "GFileIcon" g-file-icon
  (:superclass g-object
   :export t
   :interfaces ("GIcon"
                "GLoadableIcon")
   :type-initializer "g_file_icon_get_type")
  ((file
    g-file-icon-file
    "file" "GFile" t t)))

#+cl-cffi-gtk-documentation
(setf (documentation 'g-file-icon 'type)
 "@version{2021-10-8}
  @begin{short}
    The @class{g-file-icon} class specifies an icon by pointing to an image file
    to be used as icon.
  @end{short}
  @see-slot{g-file-icon-file}
  @see-class{g-icon}
  @see-class{g-loadable-icon}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- g-file-icon-file -------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "file" 'g-file-icon) 't)
 "The @code{file} property of type @class{g-file}
  (Read / Write / Construct Only) @br{}
  The file containing the icon.")

#+cl-cffi-gtk-documentation
(setf (gethash 'g-file-icon-file atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'g-file-icon-file 'function)
 "@version{2021-10-8}
  @syntax[]{g-file-icon-file object) => file}
  @argument[object]{a @class{g-file-icon} object}
  @argument[file]{a @class{g-file} object}
  @begin{short}
    Accessor of the @slot[g-file-icon]{file} slot of the @class{g-file-icon}
    class.
  @end{short}

  The @sym{g-file-icon-file} slot access function gets the @class{g-file}
  object associated with the given icon.
  @see-class{g-file-icon}
  @see-class{g-file}")

;;; ----------------------------------------------------------------------------
;;; g_file_icon_new ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_file_icon_new" g-file-icon-new) (g-object g-icon)
 #+cl-cffi-gtk-documentation
 "@version{2021-10-8}
  @argument[file]{a @class{g-file} object}
  @return{A @class{g-icon} object for the given file, or @code{nil} on error.}
  @begin{short}
    Creates a new icon for a file.
  @end{short}
  @see-class{g-file-icon}
  @see-class{g-file}"
  (file (g-object g-file)))

(export 'g-file-icon-new)

;;; ----------------------------------------------------------------------------
;;; g_file_icon_get_file ()
;;;
;;; GFile *
;;; g_file_icon_get_file (GFileIcon *icon);
;;;
;;;
;;; icon :
;;;     a GIcon
;;;
;;; Returns :
;;;     a GFile
;;; ----------------------------------------------------------------------------

;;; --- End of file gio.file-icon.lisp -----------------------------------------
