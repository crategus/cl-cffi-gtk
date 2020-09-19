;;; ----------------------------------------------------------------------------
;;; gtk.tool-shell.lisp
;;;
;;; The documentation of this file is taken from the GTK+ 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GTK+ library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2020 Dieter Kaiser
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
;;; GtkToolShell
;;;
;;;     Interface for containers containing GtkToolItem widgets
;;;
;;; Types and Values
;;;
;;;     GtkToolShell
;;;
;;; Functions
;;;
;;;     gtk_tool_shell_get_ellipsize_mode
;;;     gtk_tool_shell_get_icon_size
;;;     gtk_tool_shell_get_orientation
;;;     gtk_tool_shell_get_relief_style
;;;     gtk_tool_shell_get_style
;;;     gtk_tool_shell_get_text_alignment
;;;     gtk_tool_shell_get_text_orientation
;;;     gtk_tool_shell_rebuild_menu
;;;     gtk_tool_shell_get_text_size_group
;;;
;;; Object Hierarchy
;;;
;;;     GInterface
;;;     ╰── GtkToolShell
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkToolShell
;;; ----------------------------------------------------------------------------

(define-g-interface "GtkToolShell" gtk-tool-shell
  (:export t
   :type-initializer "gtk_tool_shell_get_type"))

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-tool-shell atdoc:*class-name-alias*) "Interface"
      (documentation 'gtk-tool-shell 'type)
 "@version{2020-9-4}
  @begin{short}
    The @sym{gtk-tool-shell} interface allows container widgets to provide
    additional information when embedding @class{gtk-tool-item} widgets.
  @end{short}
  @see-class{gtk-tool-item}")

;;; ----------------------------------------------------------------------------
;;; gtk_tool_shell_get_ellipsize_mode () -> gtk-tool-shell-ellipsize-mode
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tool_shell_get_ellipsize_mode" gtk-tool-shell-ellipsize-mode)
    pango-ellipsize-mode
 #+cl-cffi-gtk-documentation
 "@version{2020-9-4}
  @argument[shell]{a @class{gtk-tool-shell} widget}
  @return{The current ellipsize mode of type @symbol{pango-ellipsize-mode}
    of @arg{shell}.}
  @begin{short}
    Retrieves the current ellipsize mode for the tool shell.
  @end{short}
  Tool items must not call this function directly, but rely on the function
  @fun{gtk-tool-item-ellipsize-mode} instead.
  @see-class{gtk-tool-shell}
  @see-symbol{pango-ellipsize-mode}
  @see-function{gtk-tool-item-ellipsize-mode}"
  (shell (g-object gtk-tool-shell)))

(export 'gtk-tool-shell-ellipsize-mode)

;;; ----------------------------------------------------------------------------
;;; gtk_tool_shell_get_icon_size () -> gtk-tool-shell-icon-size
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tool_shell_get_icon_size" gtk-tool-shell-icon-size)
    gtk-icon-size
 #+cl-cffi-gtk-documentation
 "@version{2020-9-4}
  @argument[shell]{a @class{gtk-tool-shell} widget}
  @return{The current size of type @symbol{gtk-icon-size} for icons of
    @arg{shell}.}
  @begin{short}
    Retrieves the icon size for the tool shell.
  @end{short}
  Tool items must not call this function directly, but rely on the function
  @fun{gtk-tool-item-icon-size} instead.
  @see-class{gtk-tool-shell}
  @see-symbol{gtk-icon-size}
  @see-function{gtk-tool-item-icon-size}"
  (shell (g-object gtk-tool-shell)))

(export 'gtk-tool-shell-icon-size)

;;; ----------------------------------------------------------------------------
;;; gtk_tool_shell_get_orientation () -> gtk-tool-shell-orientation
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tool_shell_get_orientation" gtk-tool-shell-orientation)
    gtk-orientation
 #+cl-cffi-gtk-documentation
 "@version{2020-9-4}
  @argument[shell]{a @class{gtk-tool-shell} widget}
  @return{The current orientation of type @symbol{gtk-orientation} of
    @arg{shell}.}
  @begin{short}
    Retrieves the current orientation for the tool shell.
  @end{short}
  Tool items must not call this function directly, but rely on the function
  @fun{gtk-tool-item-orientation} instead.
  @see-class{gtk-tool-shell}
  @see-symbol{gtk-orientation}
  @see-function{gtk-tool-item-orientation}"
  (shell (g-object gtk-tool-shell)))

(export 'gtk-tool-shell-orientation)

;;; ----------------------------------------------------------------------------
;;; gtk_tool_shell_get_relief_style () -> gtk-tool-shell-relief-style
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tool_shell_get_relief_style" gtk-tool-shell-relief-style)
    gtk-relief-style
 #+cl-cffi-gtk-documentation
 "@version{2020-9-4}
  @argument[shell]{a @class{gtk-tool-shell} widget}
  @return{The relief style of type @symbol{gtk-relief-style} of buttons on
    @arg{shell}.}
  @begin{short}
    Returns the relief style of buttons on the tool shell.
  @end{short}
  Tool items must not call this function directly, but rely on the function
  @fun{gtk-tool-item-relief-style} instead.
  @see-class{gtk-tool-shell}
  @see-symbol{gtk-relief-style}
  @see-function{gtk-tool-item-relief-style}"
  (shell (g-object gtk-tool-shell)))

(export 'gtk-tool-shell-relief-style)

;;; ----------------------------------------------------------------------------
;;; gtk_tool_shell_get_style () -> gtk-tool-shell-style
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tool_shell_get_style" gtk-tool-shell-style)
    gtk-toolbar-style
 #+cl-cffi-gtk-documentation
 "@version{2020-9-4}
  @argument[shell]{a @class{gtk-tool-shell} widget}
  @return{The current style of type @symbol{gtk-toolbar-style} of @arg{shell}.}
  @begin{short}
    Retrieves whether the tool shell has text, icons, or both.
  @end{short}
  Tool items must not call this function directly, but rely on the function
  @fun{gtk-tool-item-toolbar-style} instead.
  @see-class{gtk-tool-shell}
  @see-symbol{gtk-toolbar-style}
  @see-function{gtk-tool-item-toolbar-style}"
  (shell (g-object gtk-tool-shell)))

(export 'gtk-tool-shell-style)

;;; ----------------------------------------------------------------------------
;;; gtk_tool_shell_get_text_alignment () -> gtk-tool-shell-text-alignment
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tool_shell_get_text_alignment" gtk-tool-shell-text-alignment)
    :float
 #+cl-cffi-gtk-documentation
 "@version{2020-9-4}
  @argument[shell]{a @class{gtk-tool-shell} widget}
  @return{A @code{:float} with the current text alignment of @arg{shell}.}
  @begin{short}
    Retrieves the current text alignment for the tool shell.
  @end{short}
  Tool items must not call this function directly, but rely on the function
  @fun{gtk-tool-item-text-alignment} instead.
  @see-class{gtk-tool-shell}
  @see-function{gtk-tool-item-text-alignment}"
  (shell (g-object gtk-tool-shell)))

(export 'gtk-tool-shell-text-alignment)

;;; ----------------------------------------------------------------------------
;;; gtk_tool_shell_get_text_orientation () -> gtk-tool-shell-text-orientation
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tool_shell_get_text_orientation" gtk-tool-shell-text-orientation)
    gtk-orientation
 #+cl-cffi-gtk-documentation
 "@version{2020-9-4}
  @argument[shell]{a @class{gtk-tool-shell} widget}
  @return{The current text orientation of type @symbol{gtk-orientation} of
    @arg{shell}.}
  @begin{short}
    Retrieves the current text orientation for the tool shell.
  @end{short}
  Tool items must not call this function directly, but rely on the function
  @fun{gtk-tool-item-text-orientation} instead.
  @see-class{gtk-tool-shell}
  @see-symbol{gtk-orientation}
  @see-function{gtk-tool-item-text-orientation}"
  (shell (g-object gtk-tool-shell)))

(export 'gtk-tool-shell-text-orientation)

;;; ----------------------------------------------------------------------------
;;; gtk_tool_shell_rebuild_menu ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tool_shell_rebuild_menu" gtk-tool-shell-rebuild-menu) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-9-4}
  @argument[shell]{a @class{gtk-tool-shell} widget}
  @begin{short}
    Calling this function signals the tool shell that the overflow menu item
    for tool items have changed.
  @end{short}
  If there is an overflow menu and if it is visible when this function it
  called, the menu will be rebuilt.

  Tool items must not call this function directly, but rely on the function
  @fun{gtk-tool-item-rebuild-menu} instead.
  @see-class{gtk-tool-shell}
  @see-function{gtk-tool-item-rebuild-menu}"
  (shell (g-object gtk-tool-shell)))

(export 'gtk-tool-shell-rebuild-menu)

;;; ----------------------------------------------------------------------------
;;; gtk_tool_shell_get_text_size_group () -> gtk-tool-shell-text-size-group
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tool_shell_get_text_size_group" gtk-tool-shell-text-size-group)
    (g-object gtk-size-group)
 #+cl-cffi-gtk-documentation
 "@version{2020-9-4}
  @argument[shell]{a @class{gtk-tool-shell} widget}
  @return{The current text size group of type @class{gtk-size-group} of
    @arg{shell}.}
  @begin{short}
    Retrieves the current text size group for the tool shell.
  @end{short}
  Tool items must not call this function directly, but rely on the function
  @fun{gtk-tool-item-text-size-group} instead.
  @see-class{gtk-tool-shell}
  @see-class{gtk-size-group}
  @see-function{gtk-tool-item-get-text-size-group}"
  (shell (g-object gtk-tool-shell)))

(export 'gtk-tool-shell-text-size-group)

;;; --- End of file gtk.tool-shell.lisp ----------------------------------------
