;;; ----------------------------------------------------------------------------
;;; gtk.shortcuts-window.lisp
;;;
;;; The documentation of this file is taken from the GTK+ 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GTK+ library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2019 - 2020 Dieter Kaiser
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
;;; GtkShortcutsWindow
;;;
;;;     Toplevel which shows help for shortcuts
;;;
;;; Types and Values
;;;
;;;     GtkShortcutsWindow
;;;
;;; Properties
;;;
;;;     gchar*   section-name    Read / Write
;;;     gchar*   view-name       Read / Write
;;;
;;; Signals
;;;
;;;      void    close           Action
;;;      void    search          Action
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkContainer
;;;                 ╰── GtkBin
;;;                     ╰── GtkWindow
;;;                         ╰── GtkShortcutsWindow
;;;
;;; Implemented Interfaces
;;;
;;;     GtkShortcutsWindow implements AtkImplementorIface and GtkBuildable.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkShortcutsWindow
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkShortcutsWindow" gtk-shortcuts-window
  (:superclass gtk-window
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable")
   :type-initializer "gtk_shortcuts_window_get_type")
   ((section-name
    gtk-shortcuts-window-section-name
    "section-name" "gchararray" t t)
   (view-name
    gtk-shortcuts-window-view-name
    "view-name" "gchararray" t t)))

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-shortcuts-window 'type)
 "@version{2020-9-8}
  @begin{short}
    A @sym{gtk-shortcuts-window} widget shows brief information about the
    keyboard shortcuts and gestures of an application.
  @end{short}
  The shortcuts can be grouped, and you can have multiple sections in this
  window, corresponding to the major modes of your application.

  Additionally, the shortcuts can be filtered by the current view, to avoid
  showing information that is not relevant in the current application context.

  The recommended way to construct a @sym{gtkshortcuts-window} widget is with
  @class{gtk-builder}, by populating a @sym{gtk-shortcuts-window} widget with
  one or more @class{gtk-shortcuts-section} objects, which contain
  @class{gtk-shortcuts-groups} that in turn contain objects of class
  @class{gtk-shortcuts-shortcut}.
  @begin[Example]{dictionary}
    A simple example:
    This example has as single section. As you can see, the shortcut groups are
    arranged in columns, and spread across several pages if there are too many
    to find on a single page. The @code{.ui} file for this example can be found
    @url[https://gitlab.gnome.org/GNOME/gtk/-/blob/master/demos/gtk-demo/shortcuts-gedit.ui]{here}.

    @image[gedit-shortcuts]{}

    An example with multiple views:
    This example shows a @sym{gtk-shortcuts-window} widget that has been
    configured to show only the shortcuts relevant to the \"stopwatch\" view.
    The @code{.ui} file for this example can be found
    @url[https://gitlab.gnome.org/GNOME/gtk/-/blob/master/demos/gtk-demo/shortcuts-clocks.ui]{here}.

    @image[clocks-shortcuts]{}

    An example with multiple sections:
    This example shows a @sym{gtk-shortcuts-window} widget with two sections,
    \"Editor Shortcuts\" and \"Terminal Shortcuts\". The @code{.ui} file for
    this example can be found
    @url[https://gitlab.gnome.org/GNOME/gtk/-/blob/master/demos/gtk-demo/shortcuts-builder.ui]{here}.

    @image[builder-shortcuts]{}
  @end{dictionary}
  @begin[Signal Details]{dictionary}
    @subheading{The \"close\" signal}
      @begin{pre}
 lambda (shortcutswindow)    : Action
      @end{pre}
      The \"close\" signal is a keybinding signal which gets emitted when the
      user uses a keybinding to close the window. The default binding for this
      signal is the Escape key.
      @begin[code]{table}
        @entry[shortcutswindow]{The @sym{gtk-shortcuts-window} object.}
      @end{table}
    @subheading{The \"search\" signal}
      @begin{pre}
 lambda (shortcutswindow)    : Action
      @end{pre}
      The \"search\" signal is a keybinding signal which gets emitted when the
      user uses a keybinding to start a search. The default binding for this
      signal is Control-F.
      @begin[code]{table}
        @entry[shortcutswindow]{The @sym{gtk-shortcuts-window} object.}
      @end{table}
  @end{dictionary}
  @see-slot{gtk-shortcuts-window-section-name}
  @see-slot{gtk-shortcuts-window-view-name}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk-shortcuts-window-section-name --------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "section-name"
                      'gtk-shortcuts-window) 't)
 "The @code{section-name} property of type @code{:string} (Read / Write) @br{}
  The name of the section to show. This should be the section name of one of the
  @class{gtk-shortcuts-section} objects that are in this shortcuts window. @br{}
  Default value: \"internal-search\"")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-shortcuts-window-section-name atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-shortcuts-window-section-name 'function)
 "@version{2020-9-8}
  @syntax[]{(gtk-shortcuts-window-section-name object) => section-name}
  @syntax[]{(setf (gtk-shortcuts-window-section-name object) section-name)}
  @argument[object]{a @class{gtk-shortcuts-window} widget}
  @argument[section-name]{a @code{:string} with a name of the section to show}
  @begin{short}
    Accessor of the @slot[gtk-shortcuts-window]{section-name} slot of the
    @class{gtk-shortcuts-window} class.
  @end{short}

  The name of the section to show. This should be the section name of one of the
  @class{gtk-shortcuts-section} objects that are in this shortcuts window.
  @see-class{gtk-shortcuts-window}
  @see-class{gtk-shortcuts-section}")

;;; --- gtk-shortcuts-window-view-name -----------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "view-name"
                      'gtk-shortcuts-window) 't)
 "The @code{view-name} property of type @code{:string} (Read / Write) @br{}
  The view name by which to filter the contents. This should correspond to the
  @code{view} property of some of the @class{gtk-shortcuts-group} objects that
  are inside this shortcuts window. Set this to @code{nil} to show all groups.
  @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-shortcuts-window-view-name atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-shortcuts-window-view-name 'function)
 "@version{2020-9-8}
  @syntax[]{(gtk-shortcuts-window-view-name object) => view-name}
  @syntax[]{(setf (gtk-shortcuts-window-view-name object) view-name)}
  @argument[object]{a @class{gtk-shortcuts-window} widget}
  @argument[view-name]{a @code{:string} with the view name by which to filter
    the contents}
  @begin{short}
    Accessor of the @slot[gtk-shortcuts-window]{view-name} slot of the
    @class{gtk-shortcuts-window} class.
  @end{short}

  The view name by which to filter the contents. This should correspond to the
  @code{view} property of some of the @class{gtk-shortcuts-group} objects that
  are inside this shortcuts window. Set this to @code{nil} to show all groups.
  @see-class{gtk-shortcuts-window}")

;;; --- End of file gtk.shortcuts-window.lisp ----------------------------------
