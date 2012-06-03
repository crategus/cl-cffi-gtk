;;; ----------------------------------------------------------------------------
;;; gtk.tool-shell.lisp
;;; 
;;; This file contains code from a fork of cl-gtk2.
;;; See http://common-lisp.net/project/cl-gtk2/
;;;
;;; The documentation has been copied from the GTK+ 3 Reference Manual
;;; Version 3.4.3. See http://www.gtk.org.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2012 Dieter Kaiser
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
;;; Interface for containers containing GtkToolItem widgets
;;;     
;;; Synopsis
;;; 
;;;     GtkToolShell
;;;     GtkToolShellIface
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
;;;   GInterface
;;;    +----GtkToolShell
;;; 
;;; Prerequisites
;;; 
;;; GtkToolShell requires GtkWidget.
;;;
;;; Known Implementations
;;; 
;;; GtkToolShell is implemented by GtkToolItemGroup and GtkToolbar.
;;;
;;; Description
;;; 
;;; The GtkToolShell interface allows container widgets to provide additional
;;; information when embedding GtkToolItem widgets.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkToolShell
;;; 
;;; typedef struct _GtkToolShell GtkToolShell;
;;; 
;;; Dummy structure for accessing instances of GtkToolShellIface.
;;; ----------------------------------------------------------------------------

(define-g-interface "GtkToolShell" gtk-tool-shell
  (:export t
   :type-initializer "gtk_tool_shell_get_type")
  (:cffi icon-size
         gtk-tool-shell-icon-size gtk-icon-size
         "gtk_tool_shell_get_icon_size" nil)
  (:cffi orientation
         gtk-tool-shell-orientation gtk-orientation
         "gtk_tool_shell_get_orientation" nil)
  (:cffi relief-style
         gtk-tool-shell-relief-style gtk-relief-style
         "gtk_tool_shell_get_relief_style" nil)
  (:cffi style
         gtk-tool-shell-style gtk-toolbar-style
         "gtk_tool_shell_get_style" nil))

;;; ----------------------------------------------------------------------------
;;; struct GtkToolShellIface
;;; 
;;; struct GtkToolShellIface {
;;;   GtkIconSize        (*get_icon_size)        (GtkToolShell *shell);
;;;   GtkOrientation     (*get_orientation)      (GtkToolShell *shell);
;;;   GtkToolbarStyle    (*get_style)            (GtkToolShell *shell);
;;;   GtkReliefStyle     (*get_relief_style)     (GtkToolShell *shell);
;;;   void               (*rebuild_menu)         (GtkToolShell *shell);
;;;   GtkOrientation     (*get_text_orientation) (GtkToolShell *shell);
;;;   gfloat             (*get_text_alignment)   (GtkToolShell *shell);
;;;   PangoEllipsizeMode (*get_ellipsize_mode)   (GtkToolShell *shell);
;;;   GtkSizeGroup *     (*get_text_size_group)  (GtkToolShell *shell);
;;; };
;;; 
;;; Virtual function table for the GtkToolShell interface.
;;; 
;;; get_icon_size ()
;;;     mandatory implementation of gtk_tool_shell_get_icon_size().
;;; 
;;; get_orientation ()
;;;     mandatory implementation of gtk_tool_shell_get_orientation().
;;; 
;;; get_style ()
;;;     mandatory implementation of gtk_tool_shell_get_style().
;;; 
;;; get_relief_style ()
;;;     optional implementation of gtk_tool_shell_get_relief_style().
;;; 
;;; rebuild_menu ()
;;;     optional implementation of gtk_tool_shell_rebuild_menu().
;;; 
;;; get_text_orientation ()
;;;     optional implementation of gtk_tool_shell_get_text_orientation().
;;; 
;;; get_text_alignment ()
;;;     optional implementation of gtk_tool_shell_get_text_alignment().
;;; 
;;; get_ellipsize_mode ()
;;;     optional implementation of gtk_tool_shell_get_ellipsize_mode().
;;; 
;;; get_text_size_group ()
;;;     optional implementation of gtk_tool_shell_get_text_size_group().
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tool_shell_get_ellipsize_mode ()
;;; 
;;; PangoEllipsizeMode gtk_tool_shell_get_ellipsize_mode (GtkToolShell *shell);
;;; 
;;; Retrieves the current ellipsize mode for the tool shell. Tool items must not
;;; call this function directly, but rely on gtk_tool_item_get_ellipsize_mode()
;;; instead.
;;; 
;;; shell :
;;;     a GtkToolShell
;;; 
;;; Returns :
;;;     the current ellipsize mode of shell
;;; 
;;; Since 2.20
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tool_shell_get_icon_size ()
;;; 
;;; GtkIconSize gtk_tool_shell_get_icon_size (GtkToolShell *shell);
;;; 
;;; Retrieves the icon size for the tool shell. Tool items must not call this
;;; function directly, but rely on gtk_tool_item_get_icon_size() instead.
;;; 
;;; shell :
;;;     a GtkToolShell
;;; 
;;; Returns :
;;;     the current size for icons of shell. [type int]
;;; 
;;; Since 2.14
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tool_shell_get_orientation ()
;;; 
;;; GtkOrientation gtk_tool_shell_get_orientation (GtkToolShell *shell);
;;; 
;;; Retrieves the current orientation for the tool shell. Tool items must not
;;; call this function directly, but rely on gtk_tool_item_get_orientation()
;;; instead.
;;; 
;;; shell :
;;;     a GtkToolShell
;;; 
;;; Returns :
;;;     the current orientation of shell
;;; 
;;; Since 2.14
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tool_shell_get_relief_style ()
;;; 
;;; GtkReliefStyle gtk_tool_shell_get_relief_style (GtkToolShell *shell);
;;; 
;;; Returns the relief style of buttons on shell. Tool items must not call this
;;; function directly, but rely on gtk_tool_item_get_relief_style() instead.
;;; 
;;; shell :
;;;     a GtkToolShell
;;; 
;;; Returns :
;;;     The relief style of buttons on shell.
;;; 
;;; Since 2.14
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tool_shell_get_style ()
;;; 
;;; GtkToolbarStyle gtk_tool_shell_get_style (GtkToolShell *shell);
;;; 
;;; Retrieves whether the tool shell has text, icons, or both. Tool items must
;;; not call this function directly, but rely on
;;; gtk_tool_item_get_toolbar_style() instead.
;;; 
;;; shell :
;;;     a GtkToolShell
;;; 
;;; Returns :
;;;     the current style of shell
;;; 
;;; Since 2.14
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tool_shell_get_text_alignment ()
;;; 
;;; gfloat gtk_tool_shell_get_text_alignment (GtkToolShell *shell);
;;; 
;;; Retrieves the current text alignment for the tool shell. Tool items must not
;;; call this function directly, but rely on gtk_tool_item_get_text_alignment()
;;; instead.
;;; 
;;; shell :
;;;     a GtkToolShell
;;; 
;;; Returns :
;;;     the current text alignment of shell
;;; 
;;; Since 2.20
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tool_shell_get_text_orientation ()
;;; 
;;; GtkOrientation gtk_tool_shell_get_text_orientation (GtkToolShell *shell);
;;; 
;;; Retrieves the current text orientation for the tool shell. Tool items must
;;; not call this function directly, but rely on
;;; gtk_tool_item_get_text_orientation() instead.
;;; 
;;; shell :
;;;     a GtkToolShell
;;; 
;;; Returns :
;;;     the current text orientation of shell
;;; 
;;; Since 2.20
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tool_shell_rebuild_menu ()
;;; 
;;; void gtk_tool_shell_rebuild_menu (GtkToolShell *shell);
;;; 
;;; Calling this function signals the tool shell that the overflow menu item for
;;; tool items have changed. If there is an overflow menu and if it is visible
;;; when this function it called, the menu will be rebuilt.
;;; 
;;; Tool items must not call this function directly, but rely on
;;; gtk_tool_item_rebuild_menu() instead.
;;; 
;;; shell :
;;;     a GtkToolShell
;;; 
;;; Since 2.14
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tool_shell_rebuild_menu" gtk-tool-shell-rebuild-menu) :void
  (shell (g-object gtk-tool-shell)))

(export 'gtk-tool-shell-rebuild-menu)

;;; ----------------------------------------------------------------------------
;;; gtk_tool_shell_get_text_size_group ()
;;; 
;;; GtkSizeGroup * gtk_tool_shell_get_text_size_group (GtkToolShell *shell);
;;; 
;;; Retrieves the current text size group for the tool shell. Tool items must
;;; not call this function directly, but rely on
;;; gtk_tool_item_get_text_size_group() instead.
;;; 
;;; shell :
;;;     a GtkToolShell
;;; 
;;; Returns :
;;;     the current text size group of shell
;;; 
;;; Since 2.20
;;; ----------------------------------------------------------------------------

;;; --- End of file gtk.tool-shell.lisp ----------------------------------------
