;;; ----------------------------------------------------------------------------
;;; gtk.popover-menu.lisp
;;;
;;; The documentation of this file is taken from the GTK+ 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2019 Dieter Kaiser
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
;;; GtkPopoverMenu
;;;
;;;     Popovers to use as menus
;;;
;;; Types and Values
;;;
;;;     GtkPopoverMenu
;;;
;;; Functions
;;;
;;;     gtk_popover_menu_new ()
;;;     gtk_popover_menu_open_submenu ()
;;;
;;; Properties
;;;
;;;     gchar*  visible-submenu    Read / Write
;;;
;;; Child Properties
;;;
;;;      gint   position           Read / Write
;;;     gchar*  submenu            Read / Write
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkContainer
;;;                 ╰── GtkBin
;;;                     ╰── GtkPopover
;;;                         ╰── GtkPopoverMenu
;;;
;;; Implemented Interfaces
;;;
;;;     GtkPopoverMenu implements AtkImplementorIface and GtkBuildable.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkPopoverMenu
;;; ----------------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (register-object-type "GtkPopoverMenu" 'gtk-popover-menu))

(define-g-object-class "GtkPopoverMenu" gtk-popover-menu
  (:superclass gtk-popover
    :export t
    :interfaces ("AtkImplementorIface"
                 "GtkBuildable")
    :type-initializer "gtk_popover_menu_get_type")
  ((visible-submenu
    gtk-popover-menu-visible-submenu
    "visible-submenu" "gchararray" t t)))

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-popover-menu 'type)
 "@version{2019-4-6}
  @begin{short}
    The @sym{gtk-popover-menu} class is a subclass of the @class{gtk-popover}
    class that treats its children like menus and allows switching between them.
  @end{short}
  It is meant to be used primarily together with @class{gtk-model-button}
  widgets, but any widget can be used, such as @class{gtk-spin-button} or
  @class{gtk-scale} widgets. In this respect, the @sym{gtk-popover-menu} class
  is more flexible than popovers that are created from a @class{g-menu-model}
  class with the @fun{gtk-popover-new-from-model} function.

  To add a child as a submenu, set the @code{submenu} child property to the name
  of the submenu. To let the user open this submenu, add a
  @class{gtk-model-button} widget whose @code{menu-name} property is set to the
  name you've given to the submenu.

  By convention, the first child of a submenu should be a
  @class{gtk-model-button} widget to switch back to the parent menu. Such a
  button should use the @code{inverted} and @code{centered} properties to
  achieve a title-like appearance and place the submenu indicator at the
  opposite side. To switch back to the main menu, use @code{main} as the menu
  name.
  @begin[Example]{dictionary}
    @begin{pre}
<object class=\"GtkPopoverMenu\">
  <child>
    <object class=\"GtkBox\">
      <property name=\"visible\">True</property>
      <property name=\"margin\">10</property>
      <child>
        <object class=\"GtkModelButton\">
          <property name=\"visible\">True</property>
          <property name=\"action-name\">win.frob</property>
          <property name=\"text\" translatable=\"yes\">Frob</property>
        </object>
      </child>
      <child>
        <object class=\"GtkModelButton\">
          <property name=\"visible\">True</property>
          <property name=\"menu-name\">more</property>
          <property name=\"text\" translatable=\"yes\">More</property>
        </object>
      </child>
    </object>
  </child>
  <child>
    <object class=\"GtkBox\">
      <property name=\"visible\">True</property>
      <property name=\"margin\">10</property>
      <child>
        <object class=\"GtkModelButton\">
          <property name=\"visible\">True</property>
          <property name=\"action-name\">win.foo</property>
          <property name=\"text\" translatable=\"yes\">Foo</property>
        </object>
      </child>
      <child>
        <object class=\"GtkModelButton\">
          <property name=\"visible\">True</property>
          <property name=\"action-name\">win.bar</property>
          <property name=\"text\" translatable=\"yes\">Bar</property>
        </object>
      </child>
    </object>
    <packing>
      <property name=\"submenu\">more</property>
    </packing>
  </child>
</object>
    @end{pre}
  @end{dictionary}
  @begin[CSS nodes]{dictionary}
    Just like normal popovers created using the
    @fun{gtk-popover-new-from-model} function, @sym{gtk-popover-menu} instances
    have a single css node called @code{popover} and get the @code{.menu}
    style class.
  @end{dictionary}
  @begin[Child Property Details]{dictionary}
    @begin[code]{table}
      @begin[position]{entry}
        The @code{position} child property of type @code{:int} (Read / Write)
        @br{}
        The index of the child in the parent. @br{}
        Allowed values: >= -1 @br{}
        Default value: 0
      @end{entry}
      @begin[submenu]{entry}
        The @code{submenu} child property of type @code{:string} (Read / Write)
        @br{}
        The @code{submenu} child property specifies the name of the submenu If
        it is @code{nil} or @code{main}, the child is used as the main menu,
        which is shown initially when the popover is mapped. @br{}
        Default value: @code{nil}
      @end{entry}
    @end{table}
  @end{dictionary}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk-popover-menu-visible-submenu ---------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "visible-submenu"
                                               'gtk-popover-menu) 't)
 "The @code{visible-submenu} property of type @code{:string}
  (Read / Write) @br{}
  The name of the visible submenu. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-popover-menu-visible-submenu atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-popover-menu-visible-submenu 'function)
 "@version{2019-4-6}
  @begin{short}
    Accessor of the @slot[gtk-popover-menu]{visible-submenu} slot of the
    @class{gtk-popover-menu} class.
  @end{short}
  @see-class{gtk-popover}")

;;; ----------------------------------------------------------------------------
;;; Child Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk-popover-menu-child-position ----------------------------------------

(define-child-property "GtkPopoverMenu"
                       gtk-popover-menu-child-position
                       "position" "gint" t t t)

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-popover-menu-child-position atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-popover-menu-child-position 'function)
 "@version{2019-4-6}
  @begin{short}
    Accessor of the @code{position} child property of the
    @class{gtk-popover-menu} class.
  @end{short}
  @see-class{gtk-popover-menu}")

;;; --- gtk-popover-menu-child-submenu -----------------------------------------

(define-child-property "GtkPopoverMenu"
                       gtk-popover-menu-child-submenu
                       "submenu" "gchararray" t t t)

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-popover-menu-child-position atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-popover-menu-child-position 'function)
 "@version{2019-4-6}
  @begin{short}
    Accessor of the @code{submenu} child property of the
    @class{gtk-popover-menu} class.
  @end{short}
  @see-class{gtk-popover-menu}")

;;; ----------------------------------------------------------------------------
;;; gtk_popover_menu_new ()
;;;
;;; GtkWidget * gtk_popover_menu_new (void);
;;;
;;; Creates a new popover menu.
;;;
;;; Returns :
;;;     a new GtkPopoverMenu
;;;
;;; Since: 3.16
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_popover_menu_open_submenu ()
;;;
;;; void
;;; gtk_popover_menu_open_submenu (GtkPopoverMenu *popover,
;;;                                const gchar *name);
;;;
;;; Opens a submenu of the popover . The name must be one of the names given to
;;; the submenus of popover with “submenu”, or "main" to switch back to the main
;;; menu.
;;;
;;; GtkModelButton will open submenus automatically when the “menu-name”
;;; property is set, so this function is only needed when you are using other
;;; kinds of widgets to initiate menu changes.
;;;
;;; popover :
;;;     a GtkPopoverMenu
;;;
;;; name :
;;;     the name of the menu to switch to
;;;
;;; Since: 3.16
;;; ----------------------------------------------------------------------------

;;; --- End of file gtk.popover-menu.lisp --------------------------------------
