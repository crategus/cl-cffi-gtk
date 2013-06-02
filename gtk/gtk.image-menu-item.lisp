;;; ----------------------------------------------------------------------------
;;; gtk.image-menu-item.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See <http://common-lisp.net/project/cl-gtk2/>.
;;;
;;; The documentation has been copied from the GTK+ 3 Reference Manual
;;; Version 3.6.4. See <http://www.gtk.org>. The API documentation of the
;;; Lisp binding is available at <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2013 Dieter Kaiser
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
;;; A menu item with an icon
;;;
;;; Synopsis
;;;
;;;     GtkImageMenuItem
;;;
;;;     gtk_image_menu_item_set_image
;;;     gtk_image_menu_item_get_image
;;;     gtk_image_menu_item_new
;;;     gtk_image_menu_item_new_from_stock
;;;     gtk_image_menu_item_new_with_label
;;;     gtk_image_menu_item_new_with_mnemonic
;;;     gtk_image_menu_item_get_use_stock
;;;     gtk_image_menu_item_set_use_stock
;;;     gtk_image_menu_item_get_always_show_image
;;;     gtk_image_menu_item_set_always_show_image
;;;     gtk_image_menu_item_set_accel_group
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
 "@version{2013-6-1}
  @begin{short}
    A @sym{gtk-image-menu-item} is a menu item which has an icon next to the
    text label.
  @end{short}

  Note that the user can disable display of menu icons, so make sure to still
  fill in the text label.
  @see-slot{gtk-image-menu-item-accel-group}
  @see-slot{gtk-image-menu-item-always-show-image}
  @see-slot{gtk-image-menu-item-image}
  @see-slot{gtk-image-menu-item-use-stock}")

;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "accel-group"
                                               'gtk-image-menu-item) 't)
 "The @code{\"accel-group\"} property of type @class{gtk-accel-group}
  (Write) @br{}
  The Accel Group to use for stock accelerator keys. @br{}
  Since 2.16")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "always-show-image"
                                               'gtk-image-menu-item) 't)
 "The @code{\"always-show-image\"} property of type @code{:boolean}
  (Read / Write / Construct) @br{}
  If @em{true}, the menu item will ignore the @code{\"gtk-menu-images\"} setting
  and always show the image, if available.
  Use this property if the menuitem would be useless or hard to use without
  the image. @br{}
  Default value: @code{nil} @br{}
  Since 2.16")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "image"
                                               'gtk-image-menu-item) 't)
 "The @code{\"image\"} property of type @class{gtk-widget} (Read / Write) @br{}
  Child widget to appear next to the menu text.")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "use-stock"
                                               'gtk-image-menu-item) 't)
 "The @code{\"use-stock\"} property of type @code{:boolean}
  (Read / Write / Construct) @br{}
  If @em{true}, the label set in the menuitem is used as a stock id to select
  the stock item for the item. @br{}
  Default value: @code{nil} @br{}
  Since 2.16")

;;; ----------------------------------------------------------------------------
;;;
;;; Accessors of Properties
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-image-menu-item-accel-group atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-image-menu-item-accel-group 'function)
 "@version{2013-3-27}
  Accessor of the slot @code{\"accel-group\"} of the
  @class{gtk-image-menu-item-accel-group} class.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-image-menu-item-always-show-image atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-image-menu-item-always-show-image 'function)
 "@version{2013-3-27}
  Accessor of the slot @code{\"always-show-image\"} of the
  @class{gtk-image-menu-item-accel-group} class.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-image-menu-item-image atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-image-menu-item-image 'function)
 "@version{2013-3-27}
  Accessor of the slot @code{\"image\"} of the
  @class{gtk-image-menu-item-accel-group} class.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-image-menu-item-use-stock atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-image-menu-item-use-stock 'function)
 "@version{2013-3-27}
  Accessor of the slot @code{\"use-stock\"} of the
  @class{gtk-image-menu-item-accel-group} class.")

;;; ----------------------------------------------------------------------------
;;; gtk_image_menu_item_set_image ()
;;;
;;; void gtk_image_menu_item_set_image (GtkImageMenuItem *image_menu_item,
;;;                                     GtkWidget *image);
;;;
;;; Sets the image of image_menu_item to the given widget. Note that it depends
;;; on the show-menu-images setting whether the image will be displayed or not.
;;;
;;; image_menu_item :
;;;     a GtkImageMenuItem.
;;;
;;; image :
;;;     a widget to set as the image for the menu item
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_image_menu_item_get_image ()
;;;
;;; GtkWidget * gtk_image_menu_item_get_image
;;;                                         (GtkImageMenuItem *image_menu_item);
;;;
;;; Gets the widget that is currently set as the image of image_menu_item. See
;;; gtk_image_menu_item_set_image().
;;;
;;; image_menu_item :
;;;     a GtkImageMenuItem
;;;
;;; Returns :
;;;     the widget set as image of image_menu_item
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_image_menu_item_new ()
;;;
;;; GtkWidget * gtk_image_menu_item_new (void);
;;;
;;; Creates a new GtkImageMenuItem with an empty label.
;;;
;;; Returns :
;;;     a new GtkImageMenuItem
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_image_menu_item_new_from_stock ()
;;;
;;; GtkWidget * gtk_image_menu_item_new_from_stock (const gchar *stock_id,
;;;                                                 GtkAccelGroup *accel_group);
;;;
;;; Creates a new GtkImageMenuItem containing the image and text from a stock
;;; item. Some stock ids have preprocessor macros like GTK_STOCK_OK and
;;; GTK_STOCK_APPLY.
;;;
;;; If you want this menu item to have changeable accelerators, then pass in
;;; NULL for accel_group. Next call gtk_menu_item_set_accel_path() with an
;;; appropriate path for the menu item, use gtk_stock_lookup() to look up the
;;; standard accelerator for the stock item, and if one is found, call
;;; gtk_accel_map_add_entry() to register it.
;;;
;;; stock_id :
;;;     the name of the stock item.
;;;
;;; accel_group :
;;;     the GtkAccelGroup to add the menu items accelerator to, or NULL
;;;
;;; Returns :
;;;     a new GtkImageMenuItem.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_image_menu_item_new_with_label ()
;;;
;;; GtkWidget * gtk_image_menu_item_new_with_label (const gchar *label);
;;;
;;; Creates a new GtkImageMenuItem containing a label.
;;;
;;; label :
;;;     the text of the menu item.
;;;
;;; Returns :
;;;     a new GtkImageMenuItem.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_image_menu_item_new_with_mnemonic ()
;;;
;;; GtkWidget * gtk_image_menu_item_new_with_mnemonic (const gchar *label);
;;;
;;; Creates a new GtkImageMenuItem containing a label. The label will be created
;;; using gtk_label_new_with_mnemonic(), so underscores in label indicate the
;;; mnemonic for the menu item.
;;;
;;; label :
;;;     the text of the menu item, with an underscore in front of the mnemonic
;;;     character
;;;
;;; Returns :
;;;     a new GtkImageMenuItem
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_image_menu_item_get_use_stock ()
;;;
;;; gboolean gtk_image_menu_item_get_use_stock
;;;                                         (GtkImageMenuItem *image_menu_item);
;;;
;;; Checks whether the label set in the menuitem is used as a stock id to select
;;; the stock item for the item.
;;;
;;; image_menu_item :
;;;     a GtkImageMenuItem
;;;
;;; Returns :
;;;     TRUE if the label set in the menuitem is used as a stock id to select
;;;     the stock item for the item
;;;
;;; Since 2.16
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_image_menu_item_set_use_stock ()
;;;
;;; void gtk_image_menu_item_set_use_stock (GtkImageMenuItem *image_menu_item,
;;;                                         gboolean use_stock);
;;;
;;; If TRUE, the label set in the menuitem is used as a stock id to select the
;;; stock item for the item.
;;;
;;; image_menu_item :
;;;     a GtkImageMenuItem
;;;
;;; use_stock :
;;;     TRUE if the menuitem should use a stock item
;;;
;;; Since 2.16
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_image_menu_item_get_always_show_image ()
;;;
;;; gboolean gtk_image_menu_item_get_always_show_image
;;;                                         (GtkImageMenuItem *image_menu_item);
;;;
;;; Returns whether the menu item will ignore the "gtk-menu-images" setting and
;;; always show the image, if available.
;;;
;;; image_menu_item :
;;;     a GtkImageMenuItem
;;;
;;; Returns :
;;;     TRUE if the menu item will always show the image
;;;
;;; Since 2.16
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_image_menu_item_set_always_show_image ()
;;;
;;; void gtk_image_menu_item_set_always_show_image
;;;                                          (GtkImageMenuItem *image_menu_item,
;;;                                           gboolean always_show);
;;;
;;; If TRUE, the menu item will ignore the "gtk-menu-images" setting and always
;;; show the image, if available.
;;;
;;; Use this property if the menuitem would be useless or hard to use without
;;; the image.
;;;
;;; image_menu_item :
;;;     a GtkImageMenuItem
;;;
;;; always_show :
;;;     TRUE if the menuitem should always show the image
;;;
;;; Since 2.16
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_image_menu_item_set_accel_group ()
;;;
;;; void gtk_image_menu_item_set_accel_group (GtkImageMenuItem *image_menu_item,
;;;                                           GtkAccelGroup *accel_group);
;;;
;;; Specifies an accel_group to add the menu items accelerator to (this only
;;; applies to stock items so a stock item must already be set, make sure to
;;; call gtk_image_menu_item_set_use_stock() and gtk_menu_item_set_label() with
;;; a valid stock item first).
;;;
;;; If you want this menu item to have changeable accelerators then you shouldnt
;;; need this (see gtk_image_menu_item_new_from_stock()).
;;;
;;; image_menu_item :
;;;     a GtkImageMenuItem
;;;
;;; accel_group :
;;;     the GtkAccelGroup
;;;
;;; Since 2.16
;;; ----------------------------------------------------------------------------

;;; --- End of file gtk.image-menu-item.lisp -----------------------------------
