;;; ----------------------------------------------------------------------------
;;; gtk.image-menu-item.lisp
;;;
;;; The documentation of this file is taken from the GTK+ 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2019 Dieter Kaiser
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
;;; GtkImageMenuItem
;;;
;;;     A deprecated widget for a menu item with an icon.
;;;
;;; Types and Values
;;;
;;;     GtkImageMenuItem
;;;
;;; Functions
;;;
;;;     gtk_image_menu_item_set_image                      Accessor
;;;     gtk_image_menu_item_get_image                      Accessor
;;;     gtk_image_menu_item_new
;;;     gtk_image_menu_item_new_from_stock
;;;     gtk_image_menu_item_new_with_label
;;;     gtk_image_menu_item_new_with_mnemonic
;;;     gtk_image_menu_item_get_use_stock                  Accessor
;;;     gtk_image_menu_item_set_use_stock                  Accessor
;;;     gtk_image_menu_item_get_always_show_image          Accessor
;;;     gtk_image_menu_item_set_always_show_image          Accessor
;;;     gtk_image_menu_item_set_accel_group                Accessor
;;;
;;; Properties
;;;
;;;     GtkAccelGroup*  accel-group          Write
;;;          gboolean   always-show-image    Read / Write / Construct
;;;         GtkWidget*  image                Read / Write
;;;          gboolean   use-stock            Read / Write / Construct
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkContainer
;;;                 ╰── GtkBin
;;;                     ╰── GtkMenuItem
;;;                         ╰── GtkImageMenuItem
;;;
;;; Implemented Interfaces
;;;
;;;     GtkImageMenuItem implements AtkImplementorIface, GtkBuildable,
;;;     GtkActivatable and GtkActionable.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkImageMenuItem
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkImageMenuItem" gtk-image-menu-item
  (:superclass gtk-menu-item
    :export t
    :interfaces ("AtkImplementorIface"
                 "GtkBuildable"
                 "GtkActivatable")
    :type-initializer "gtk_image_menu_item_get_type")
  ((accel-group
    gtk-image-menu-item-accel-group
    "accel-group" "GtkAccelGroup" nil t)
   (always-show-image
    gtk-image-menu-item-always-show-image
    "always-show-image" "gboolean" t t)
   (image
    gtk-image-menu-item-image
    "image" "GtkWidget" t t)
   (use-stock
    gtk-image-menu-item-use-stock
    "use-stock" "gboolean" t t)))

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-image-menu-item 'type)
 "@version{2019-5-31}
  @begin{short}
    A @sym{gtk-image-menu-item} is a menu item which has an icon next to the
    text label.
  @end{short}

  This is functionally equivalent to:
  @begin{pre}
GtkWidget *box = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 6);
GtkWidget *icon = gtk_image_new_from_icon_name (\"folder-music-symbolic\",
                                                GTK_ICON_SIZE_MENU);
GtkWidget *label = gtk_label_new (\"Music\");
GtkWidget *menu_item = gtk_menu_item_new ();

gtk_container_add (GTK_CONTAINER (box), icon);
gtk_container_add (GTK_CONTAINER (box), label);

gtk_container_add (GTK_CONTAINER (menu_item), box);

gtk_widget_show_all (menu_item);
  @end{pre}
  Note that the user may disable display of menu icons using the
  @slot[gtk-settings]{gtk-menu-images} setting, so make sure to still fill in
  the text label. If you want to ensure that your menu items show an icon you
  are strongly encouraged to use a @class{gtk-menu-item} with a
  @class{gtk-image} instead.

  @sym{gtk-image-menu-item} has been deprecated since GTK+ 3.10. If you want to
  display an icon in a menu item, you should use @class{gtk-menu-item} and pack
  a @class{gtk-box} with a @class{gtk-image} and a @class{gtk-label} instead.
  You should also consider using @class{gtk-builder} and the XML @class{g-menu}
  description for creating menus, by following the @class{g-menu} guide. You
  should consider using icons in menu items only sparingly, and for \"objects\"
  or \"nouns\" elements only, like bookmarks, files, and links; \"actions\" or
  \"verbs\" should not have icons.

  Furthermore, if you would like to display keyboard accelerator, you must pack
  the accel label into the box using the @fun{gtk-box-pack-end} function and
  align the label, otherwise the accelerator will not display correctly. The
  following code snippet adds a keyboard accelerator to the menu item, with a
  key binding of Ctrl+M:
  @begin{pre}
GtkWidget *box = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 6);
GtkWidget *icon = gtk_image_new_from_icon_name (\"folder-music-symbolic\",
                                                GTK_ICON_SIZE_MENU);
GtkWidget *label = gtk_accel_label_new (\"Music\");
GtkWidget *menu_item = gtk_menu_item_new ();
GtkAccelGroup *accel_group = gtk_accel_group_new ();

gtk_container_add (GTK_CONTAINER (box), icon);

gtk_label_set_use_underline (GTK_LABEL (label), TRUE);
gtk_label_set_xalign (GTK_LABEL (label), 0.0);

gtk_widget_add_accelerator (menu_item, \"activate\", accel_group,
                            GDK_KEY_m, GDK_CONTROL_MASK, GTK_ACCEL_VISIBLE);
gtk_accel_label_set_accel_widget (GTK_ACCEL_LABEL (label), menu_item);

gtk_box_pack_end (GTK_BOX (box), label, TRUE, TRUE, 0);

gtk_container_add (GTK_CONTAINER (menu_item), box);

gtk_widget_show_all (menu_item);
  @end{pre}
  @see-slot{gtk-image-menu-item-accel-group}
  @see-slot{gtk-image-menu-item-always-show-image}
  @see-slot{gtk-image-menu-item-image}
  @see-slot{gtk-image-menu-item-use-stock}
  @see-class{gtk-menu-item}
  @see-class{g-menu}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk-image-menu-item-accel-group ----------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "accel-group"
                                               'gtk-image-menu-item) 't)
 "The @code{accel-group} property of type @class{gtk-accel-group}
  (Write) @br{}
  The Accel Group to use for stock accelerator keys.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-image-menu-item-accel-group atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-image-menu-item-accel-group 'function)
 "@version{2019-5-21}
  @syntax[]{(setf (gtk-image-menu-item-accel-group object) accel-group)}
  @argument[object]{a @class{gtk-image-menu-item} widget}
  @argument[accel-group]{a @class{gtk-accel-group} object}
  @begin{short}
    Accessor of @slot[gtk-image-menu-item]{accel-group} slot of the
    @class{gtk-image-menu-item} class.
  @end{short}

  Specifies an accel group to add the menu items accelerator to, this only
  applies to stock items so a stock item must already be set. Make sure to call
  the @fun{gtk-image-menu-item-use-stock} and @fun{gtk-menu-item-label}
  functions with a valid stock item first.

  If you want this menu item to have changeable accelerators then you shouldn't
  need this. See the @fun{gtk-image-menu-item-new-from-stock} function.
  @begin[Warning]{dictionary}
    The @sym{gtk-image-menu-item-accel-group} function has been deprecated
    since version 3.10 and should not be used in newly-written code.
  @end{dictionary}
  @see-class{gtk-image-menu-item}")

;;; --- gtk-image-menu-item-always-show-image ----------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "always-show-image"
                                               'gtk-image-menu-item) 't)
 "The @code{always-show-image} property of type @code{:boolean}
  (Read / Write / Construct) @br{}
  If @em{true}, the menu item will ignore the
  @slot[gtk-settings]{gtk-menu-images} setting and always show the image, if
  available. Use this property if the menuitem would be useless or hard to use
  without the image. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-image-menu-item-always-show-image atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-image-menu-item-always-show-image 'function)
 "@version{2019-5-31}
  @syntax[]{(gtk-image-menu-item-always-show-image object) => always-show}
  @syntax[]{(setf (gtk-image-menu-item-always-show-image object) always-show)}
  @argument[object]{a @class{gtk-image-menu-item} widget}
  @argument[always-show]{@em{true} if the menu item should always show the
    image}
  @begin{short}
    Accessor of the @slot[gtk-image-menu-item]{always-show-image} slot of the
    @class{gtk-image-menu-item} class.
  @end{short}

  If @em{true}, the menu item will ignore the
  @slot[gtk-settings]{gtk-menu-images} setting and always show the image, if
  available.

  Use this property if the menu item would be useless or hard to use without
  the image.
  @begin[Warning]{dictionary}
    The @sym{gtk-image-menu-item-always-show-image} function has been deprecated
    since version 3.10 and should not be used in newly-written code.
  @end{dictionary}
  @see-class{gtk-image-menu-item}")

;;; --- gtk-image-menu-item-image ----------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "image"
                                               'gtk-image-menu-item) 't)
 "The @code{image} property of type @class{gtk-widget} (Read / Write) @br{}
  Child widget to appear next to the menu text.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-image-menu-item-image atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-image-menu-item-image 'function)
 "@version{2019-5-31}
  @syntax[]{(gtk-image-menu-item-image object) => image}
  @syntax[]{(setf (gtk-image-menu-item-image object) image)}
  @argument[object]{a @class{gtk-image-menu-item} widget}
  @argument[image]{a @class{gtk-widget} object to set as the image for the menu
    item}
  @begin{short}
    Accessor of the @slot[gtk-image-menu-item]{image} slot of the
    @class{gtk-image-menu-item} class.
  @end{short}

  The @sym{gtk-image-menu-item-image} slot access function
  gets the widget that is currently set as the image of the menu item.

  The @sym{(setf gtk-image-menu-item-image)} slot access function
  sets the image of the menu item to the given widget. Note that it depends
  on the @slot[gtk-settings]{show-menu-images} setting whether the image will
  be displayed or not.
  @begin[Warning]{dictionary}
    The @sym{gtk-image-menu-item-image} function has been deprecated
    since version 3.10 and should not be used in newly-written code.
  @end{dictionary}
  @see-class{gtk-image-menu-item}")

;;; --- gtk-image-menu-item-use-stock ------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "use-stock"
                                               'gtk-image-menu-item) 't)
 "The @code{use-stock} property of type @code{:boolean}
  (Read / Write / Construct) @br{}
  If @em{true}, the label set in the menuitem is used as a stock id to select
  the stock item for the item. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-image-menu-item-use-stock atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-image-menu-item-use-stock 'function)
 "@version{2019-5-31}
  @syntax[]{(gtk-image-menu-item-use-stock object) => use-stock}
  @syntax[]{(setf (gtk-image-menu-item-use-stock object) use-stock)}
  @argument[object]{a @class{gtk-image-menu-item} widget}
  @argument[use-stock]{@em{true} if the menu item should use a stock item}
  @begin{short}
    Accessor of the @slot[gtk-image-menu-item]{use-stock} slot of the
    @class{gtk-image-menu-item} class.
  @end{short}

  If @em{true}, the label set in the menu item is used as a stock id to select
  the stock item for the item.
  @begin[Warning]{dictionary}
    The @sym{gtk-image-menu-item-use-stock} function has been deprecated
    since version 3.10 and should not be used in newly-written code.
  @end{dictionary}
  @see-class{gtk-image-menu-item}")

;;; ----------------------------------------------------------------------------
;;; gtk_image_menu_item_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-image-menu-item-new))

(defun gtk-image-menu-item-new ()
 "@version{2019-5-31}
  @argument[object]{a @class{gtk-image-menu-item} widget}
  @return{A new @class{gtk-image-menu-item} widget.}
  @begin{short}
    Creates a new @class{gtk-image-menu-item} widget with an empty label.
  @end{short}
  @begin[Warning]{dictionary}
    The @sym{gtk-image-menu-item-new} function has been deprecated since
    version 3.10 and should not be used in newly-written code. Use the
    @fun{gtk-menu-item-new} function instead.
  @end{dictionary}
  @see-class{gtk-image-menu-item}"
  (make-instance 'gtk-image-menu-item))

(export 'gtk-image-menu-item-new)

;;; ----------------------------------------------------------------------------
;;; gtk_image_menu_item_new_from_stock ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_image_menu_item_new_from_stock"
           gtk-image-menu-item-new-from-stock) (g-object gtk-image-menu-item)
 "@version{2019-5-31}
  @argument[stock-id]{the name of the stock item}
  @argument[accel-group]{the @class{gtk-accel-group} object to add the menu
    items accelerator to, or @code{nil}}
  @return{A new @class{gtk-image-menu-item} widget.}
  @begin{short}
    Creates a new @class{gtk-image-menu-item} widget containing the image and
    text from a stock item.
  @end{short}
  Some stock ids have preprocessor macros like @code{GTK_STOCK_OK} and
  @code{GTK_STOCK_APPLY}.

  If you want this menu item to have changeable accelerators, then pass in
  NULL for @arg{accel-group}. Next call the @fun{gtk-menu-item-set-accel-path}
  with an appropriate path for the menu item, use the @fun{gtk-stock-lookup}
  function to look up the standard accelerator for the stock item, and if one
  is found, call the @fun{gtk-accel-map-add-entry} function to register it.
  @begin[Warning]{dictionary}
    The @sym{gtk-image-menu-item-new-from-stock} function has been deprecated
    since version 3.10 and should not be used in newly-written code. Use the
    @fun{gtk-menu-item-new-with-mnemonic} function instead.
  @end{dictionary}
  @see-class{gtk-image-menu-item}"
  (stock-id :string)
  (accel-group (g-object gtk-accel-group)))

(export 'gtk-image-menu-item-new-from-stock)

;;; ----------------------------------------------------------------------------
;;; gtk_image_menu_item_new_with_label ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_image_menu_item_new_with_label"
           gtk-image-menu-item-new-with-label) (g-object gtk-image-menu-item)
 "@version{2019-5-31}
  @argument[label]{the text of the menu item}
  @return{A new @class{gtk-image-menu-item} widget.}
  @begin{short}
    Creates a new @class{gtk-image-menu-item} widget containing a label.
  @end{short}
  @begin[Warning]{dictionary}
    The @sym{gtk-image-menu-item-new-with-label} function has been deprecated
    since version 3.10 and should not be used in newly-written code. Use the
    @fun{gtk-menu-item-new-with-label} function instead.
  @end{dictionary}
  @see-class{gtk-image-menu-item}"
  (label :string))

(export 'gtk-image-menu-item-new-with-label)

;;; ----------------------------------------------------------------------------
;;; gtk_image_menu_item_new_with_mnemonic ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_image_menu_item_new_with_mnemonic"
           gtk-image-menu-item-new-with-mnemonic) (g-object gtk-image-menu-item)
 "@version{2019-5-31}
  @argument[label]{the text of the menu item, with an underscore in front of
    the mnemonic character}
  @return{A new @class{gtk-image-menu-item} widget.}
  @begin{short}
    Creates a new @class{gtk-image-menu-item} widget containing a label.
  @end{short}
  The label will be created using the @fun{gtk-label-new-with-mnemonic}
  function, so underscores in label indicate the mnemonic for the menu item.
  @begin[Warning]{dictionary}
    The @sym{gtk-image-menu-item-new-with-mnemonic} function has been deprecated
    since version 3.10 and should not be used in newly-written code. Use the
    @fun{gtk-menu-item-new-with-mnemonic} function instead.
  @end{dictionary}
  @see-class{gtk-image-menu-item}"
  (label :string))

(export 'gtk-image-menu-item-new-with-mnemonic)

;;; --- End of file gtk.image-menu-item.lisp -----------------------------------
