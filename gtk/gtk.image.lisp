;;; ----------------------------------------------------------------------------
;;; gtk.image.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See http://common-lisp.net/project/cl-gtk2/
;;;
;;; The documentation has been copied from the GTK 2.2.2 Reference Manual
;;; See http://www.gtk.org.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2012 Dr. Dieter Kaiser
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
;;; GtkImage
;;; 
;;; A widget displaying an image
;;; 	
;;; Synopsis
;;; 
;;;     GtkImage
;;;     GtkImageType
;;;     gtk_image_get_icon_set
;;;     gtk_image_get_pixbuf
;;;     gtk_image_get_stock
;;;     gtk_image_get_animation
;;;     gtk_image_get_icon_name
;;;     gtk_image_get_gicon
;;;     gtk_image_get_storage_type
;;;     gtk_image_new_from_file
;;;     gtk_image_new_from_icon_set
;;;     gtk_image_new_from_pixbuf
;;;     gtk_image_new_from_stock
;;;     gtk_image_new_from_animation
;;;     gtk_image_new_from_icon_name
;;;     gtk_image_new_from_gicon
;;;     gtk_image_set_from_file
;;;     gtk_image_set_from_icon_set
;;;     gtk_image_set_from_pixbuf
;;;     gtk_image_set_from_stock
;;;     gtk_image_set_from_animation
;;;     gtk_image_set_from_icon_name
;;;     gtk_image_set_from_gicon
;;;     gtk_image_clear
;;;     gtk_image_new
;;;     gtk_image_set_pixel_size
;;;     gtk_image_get_pixel_size
;;; 
;;; Object Hierarchy
;;; 
;;;   GObject
;;;    +----GInitiallyUnowned
;;;          +----GtkWidget
;;;                +----GtkMisc
;;;                      +----GtkImage
;;; 
;;; Implemented Interfaces
;;; 
;;; GtkImage implements AtkImplementorIface and GtkBuildable.
;;;
;;; Properties
;;; 
;;;   "file"                     gchar*                : Read / Write
;;;   "gicon"                    GIcon*                : Read / Write
;;;   "icon-name"                gchar*                : Read / Write
;;;   "icon-set"                 GtkIconSet*           : Read / Write
;;;   "icon-size"                gint                  : Read / Write
;;;   "pixbuf"                   GdkPixbuf*            : Read / Write
;;;   "pixbuf-animation"         GdkPixbufAnimation*   : Read / Write
;;;   "pixel-size"               gint                  : Read / Write
;;;   "stock"                    gchar*                : Read / Write
;;;   "storage-type"             GtkImageType          : Read
;;;   "use-fallback"             gboolean              : Read / Write
;;; 
;;; Description
;;; 
;;; The GtkImage widget displays an image. Various kinds of object can be
;;; displayed as an image; most typically, you would load a GdkPixbuf 
;;; ("pixel buffer") from a file, and then display that. There's a convenience
;;; function to do this, gtk_image_new_from_file(), used as follows:
;;; 
;;; GtkWidget *image;
;;; image = gtk_image_new_from_file ("myfile.png");
;;; 
;;; If the file isn't loaded successfully, the image will contain a
;;; "broken image" icon similar to that used in many web browsers. If you want
;;; to handle errors in loading the file yourself, for example by displaying an
;;; error message, then load the image with gdk_pixbuf_new_from_file(), then
;;; create the GtkImage with gtk_image_new_from_pixbuf().
;;; 
;;; The image file may contain an animation, if so the GtkImage will display an
;;; animation (GdkPixbufAnimation) instead of a static image.
;;; 
;;; GtkImage is a subclass of GtkMisc, which implies that you can align it
;;; (center, left, right) and add padding to it, using GtkMisc methods.
;;; 
;;; GtkImage is a "no window" widget (has no GdkWindow of its own), so by
;;; default does not receive events. If you want to receive events on the image,
;;; such as button clicks, place the image inside a GtkEventBox, then connect
;;; to the event signals on the event box.
;;; 
;;; Example 50. Handling button press events on a GtkImage.
;;; 
;;; static gboolean
;;; button_press_callback (GtkWidget      *event_box,
;;;                        GdkEventButton *event,
;;;                        gpointer        data)
;;; {
;;;   g_print ("Event box clicked at coordinates %f,%f\n",
;;;            event->x, event->y);
;;; 
;;;   /* Returning TRUE means we handled the event, so the signal
;;;    * emission should be stopped (don't call any further
;;;    * callbacks that may be connected). Return FALSE
;;;    * to continue invoking callbacks.
;;;    */
;;;   return TRUE;
;;; }
;;; 
;;; static GtkWidget*
;;; create_image (void)
;;; {
;;;   GtkWidget *image;
;;;   GtkWidget *event_box;
;;; 
;;;   image = gtk_image_new_from_file ("myfile.png");
;;; 
;;;   event_box = gtk_event_box_new ();
;;; 
;;;   gtk_container_add (GTK_CONTAINER (event_box), image);
;;; 
;;;   g_signal_connect (G_OBJECT (event_box),
;;;                     "button_press_event",
;;;                     G_CALLBACK (button_press_callback),
;;;                     image);
;;; 
;;;   return image;
;;; }
;;; 
;;; When handling events on the event box, keep in mind that coordinates in the
;;; image may be different from event box coordinates due to the alignment and
;;; padding settings on the image (see GtkMisc). The simplest way to solve this
;;; is to set the alignment to 0.0 (left/top), and set the padding to zero. Then
;;; the origin of the image will be the same as the origin of the event box.
;;; 
;;; Sometimes an application will want to avoid depending on external data
;;; files, such as image files. GTK+ comes with a program to avoid this, called
;;; gdk-pixbuf-csource. This program allows you to convert an image into a C
;;; variable declaration, which can then be loaded into a GdkPixbuf using
;;; gdk_pixbuf_new_from_inline().
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "file" property
;;; 
;;;   "file"                     gchar*                : Read / Write
;;; 
;;; Filename to load and display.
;;; 
;;; Default value: NULL
;;;
;;; ----------------------------------------------------------------------------
;;; The "gicon" property
;;; 
;;;   "gicon"                    GIcon*                : Read / Write
;;; 
;;; The GIcon displayed in the GtkImage. For themed icons, If the icon theme
;;; is changed, the image will be updated automatically.
;;; 
;;; Since 2.14
;;;
;;; ----------------------------------------------------------------------------
;;; The "icon-name" property
;;; 
;;;   "icon-name"                gchar*                : Read / Write
;;; 
;;; The name of the icon in the icon theme. If the icon theme is changed, the
;;; image will be updated automatically.
;;; 
;;; Default value: NULL
;;; 
;;; Since 2.6
;;;
;;; ----------------------------------------------------------------------------
;;; The "icon-set" property
;;; 
;;;   "icon-set"                 GtkIconSet*           : Read / Write
;;; 
;;; Icon set to display.
;;;
;;; ----------------------------------------------------------------------------
;;; The "icon-size" property
;;; 
;;;   "icon-size"                gint                  : Read / Write
;;; 
;;; Symbolic size to use for stock icon, icon set or named icon.
;;; 
;;; Allowed values: >= 0
;;; 
;;; Default value: 4
;;;
;;; ----------------------------------------------------------------------------
;;; The "pixbuf" property
;;; 
;;;   "pixbuf"                   GdkPixbuf*            : Read / Write
;;; 
;;; A GdkPixbuf to display.
;;;
;;; ----------------------------------------------------------------------------
;;; The "pixbuf-animation" property
;;; 
;;;   "pixbuf-animation"         GdkPixbufAnimation*   : Read / Write
;;; 
;;; GdkPixbufAnimation to display.
;;;
;;; ----------------------------------------------------------------------------
;;; The "pixel-size" property
;;; 
;;;   "pixel-size"               gint                  : Read / Write
;;; 
;;; The "pixel-size" property can be used to specify a fixed size overriding
;;; the "icon-size" property for images of type GTK_IMAGE_ICON_NAME.
;;; 
;;; Allowed values: >= G_MAXULONG
;;; 
;;; Default value: -1
;;; 
;;; Since 2.6
;;;
;;; ----------------------------------------------------------------------------
;;; The "stock" property
;;; 
;;;   "stock"                    gchar*                : Read / Write
;;; 
;;; Stock ID for a stock image to display.
;;; 
;;; Default value: NULL
;;;
;;; ----------------------------------------------------------------------------
;;; The "storage-type" property
;;; 
;;;   "storage-type"             GtkImageType          : Read
;;; 
;;; The representation being used for image data.
;;; 
;;; Default value: GTK_IMAGE_EMPTY
;;;
;;; ----------------------------------------------------------------------------
;;; The "use-fallback" property
;;; 
;;;   "use-fallback"             gboolean              : Read / Write
;;; 
;;; Whether the icon displayed in the GtkImage will use standard icon names
;;; fallback. The value of this property is only relevant for images of type
;;; GTK_IMAGE_ICON_NAME and GTK_IMAGE_GICON.
;;; 
;;; Default value: FALSE
;;; 
;;; Since 3.0
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkImage
;;; 
;;; struct GtkImage;
;;;
;;; This struct contain private data only and should be accessed by the
;;; functions below.
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkImage" gtk-image
                       (:superclass gtk-misc
                        :export t
                        :interfaces ("AtkImplementorIface" "GtkBuildable")
                        :type-initializer "gtk_image_get_type")
                       ((file gtk-image-file "file" "gchararray" t t)
                        (gicon gtk-image-gicon "gicon" "GIcon" t t)
                        (icon-name gtk-image-icon-name
                                   "icon-name" "gchararray" t t)
                        (icon-set gtk-image-icon-set
                                  "icon-set" "GtkIconSet" t t)
                        (icon-size gtk-image-icon-size "icon-size" "gint" t t)
                        (image gtk-image-image "image" "GdkImage" t t)
                        (mask gtk-image-mask "mask" "GdkPixmap" t t)
                        (pixbuf gtk-image-pixbuf "pixbuf" "GdkPixbuf" t t)
                        (pixbuf-animation gtk-image-pixbuf-animation
                         "pixbuf-animation" "GdkPixbufAnimation" t t)
                        (pixel-size gtk-image-pixel-size
                                    "pixel-size" "gint" t t)
                        (pixmap gtk-image-pixmap "pixmap" "GdkPixmap" t t)
                        (stock gtk-image-stock "stock" "gchararray" t t)
                        (storage-type gtk-image-storage-type "storage-type"
                                      "GtkImageType" t nil)))

;;; ----------------------------------------------------------------------------
;;; enum GtkImageType
;;; 
;;; typedef enum {
;;;   GTK_IMAGE_EMPTY,
;;;   GTK_IMAGE_PIXBUF,
;;;   GTK_IMAGE_STOCK,
;;;   GTK_IMAGE_ICON_SET,
;;;   GTK_IMAGE_ANIMATION,
;;;   GTK_IMAGE_ICON_NAME,
;;;   GTK_IMAGE_GICON
;;; } GtkImageType;
;;; 
;;; Describes the image data representation used by a GtkImage. If you want to
;;; get the image from the widget, you can only get the currently-stored
;;; representation. e.g. if the gtk_image_get_storage_type() returns
;;; GTK_IMAGE_PIXBUF, then you can call gtk_image_get_pixbuf() but not
;;; gtk_image_get_stock(). For empty images, you can request any storage type
;;; (call any of the "get" functions), but they will all return NULL values.
;;; 
;;; GTK_IMAGE_EMPTY
;;; 	there is no image displayed by the widget
;;; 
;;; GTK_IMAGE_PIXBUF
;;; 	the widget contains a GdkPixbuf
;;; 
;;; GTK_IMAGE_STOCK
;;; 	the widget contains a stock icon name (see Stock Items(3))
;;; 
;;; GTK_IMAGE_ICON_SET
;;; 	the widget contains a GtkIconSet
;;; 
;;; GTK_IMAGE_ANIMATION
;;; 	the widget contains a GdkPixbufAnimation
;;; 
;;; GTK_IMAGE_ICON_NAME
;;; 	the widget contains a named icon. This image type was added in GTK+ 2.6
;;; 
;;; GTK_IMAGE_GICON
;;; 	the widget contains a GIcon. This image type was added in GTK+ 2.14
;;; ----------------------------------------------------------------------------

(define-g-enum "GtkImageType" gtk-image-type
  (:export t
   :type-initializer "gtk_image_type_get_type")
  (:empty 0)
  (:pixmap 1)
  (:image 2)
  (:pixbuf 3)
  (:stock 4)
  (:icon-set 5)
  (:animation 6)
  (:icon-name 7)
  (:gicon 8))

;;; ----------------------------------------------------------------------------
;;; gtk_image_get_icon_set ()
;;; 
;;; void gtk_image_get_icon_set (GtkImage *image,
;;;                              GtkIconSet **icon_set,
;;;                              GtkIconSize *size);
;;; 
;;; Gets the icon set and size being displayed by the GtkImage. The storage
;;; type of the image must be GTK_IMAGE_EMPTY or GTK_IMAGE_ICON_SET (see
;;; gtk_image_get_storage_type()).
;;; 
;;; image :
;;; 	a GtkImage
;;; 
;;; icon_set :
;;; 	location to store a GtkIconSet, or NULL.
;;; 
;;; size :
;;; 	location to store a stock icon size, or NULL.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_image_get_pixbuf ()
;;; 
;;; GdkPixbuf * gtk_image_get_pixbuf (GtkImage *image);
;;; 
;;; Gets the GdkPixbuf being displayed by the GtkImage. The storage type of the
;;; image must be GTK_IMAGE_EMPTY or GTK_IMAGE_PIXBUF (see
;;; gtk_image_get_storage_type()). The caller of this function does not own a
;;; reference to the returned pixbuf.
;;; 
;;; image :
;;; 	a GtkImage
;;; 
;;; Returns :
;;; 	the displayed pixbuf, or NULL if the image is empty. [transfer none]
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_image_get_stock ()
;;; 
;;; void gtk_image_get_stock (GtkImage *image,
;;;                           gchar **stock_id,
;;;                           GtkIconSize *size);
;;; 
;;; Gets the stock icon name and size being displayed by the GtkImage. The
;;; storage type of the image must be GTK_IMAGE_EMPTY or GTK_IMAGE_STOCK (see
;;; gtk_image_get_storage_type()). The returned string is owned by the GtkImage
;;; and should not be freed.
;;; 
;;; image :
;;; 	a GtkImage
;;; 
;;; stock_id :
;;; 	place to store a stock icon name, or NULL.
;;; 
;;; size :
;;; 	place to store a stock icon size, or NULL.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_image_get_animation ()
;;; 
;;; GdkPixbufAnimation * gtk_image_get_animation (GtkImage *image);
;;; 
;;; Gets the GdkPixbufAnimation being displayed by the GtkImage. The storage
;;; type of the image must be GTK_IMAGE_EMPTY or GTK_IMAGE_ANIMATION (see
;;; gtk_image_get_storage_type()). The caller of this function does not own a
;;; reference to the returned animation.
;;; 
;;; image :
;;; 	a GtkImage
;;; 
;;; Returns :
;;; 	the displayed animation, or NULL if the image is empty. [transfer none]
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_image_get_animation" gtk-image-get-animation)
    (g-object gdk-pixbuf-animation)
  (image (g-object gtk-image)))

(export 'gtk-image-get-animation)

;;; ----------------------------------------------------------------------------
;;; gtk_image_get_icon_name ()
;;; 
;;; void gtk_image_get_icon_name (GtkImage *image,
;;;                               const gchar **icon_name,
;;;                               GtkIconSize *size);
;;; 
;;; Gets the icon name and size being displayed by the GtkImage. The storage
;;; type of the image must be GTK_IMAGE_EMPTY or GTK_IMAGE_ICON_NAME (see
;;; gtk_image_get_storage_type()). The returned string is owned by the GtkImage
;;; and should not be freed.
;;; 
;;; image :
;;; 	a GtkImage
;;; 
;;; icon_name :
;;; 	place to store an icon name, or NULL.
;;; 
;;; size :
;;; 	place to store an icon size, or NULL.
;;; 
;;; Since 2.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_image_get_gicon ()
;;; 
;;; void gtk_image_get_gicon (GtkImage *image, GIcon **gicon, GtkIconSize *size)
;;; 
;;; Gets the GIcon and size being displayed by the GtkImage. The storage type
;;; of the image must be GTK_IMAGE_EMPTY or GTK_IMAGE_GICON (see
;;; gtk_image_get_storage_type()). The caller of this function does not own a
;;; reference to the returned GIcon.
;;; 
;;; image :
;;; 	a GtkImage
;;; 
;;; gicon :
;;; 	place to store a GIcon, or NULL. 
;;; 
;;; size :
;;; 	place to store an icon size, or NULL. 
;;; 
;;; Since 2.14
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_image_get_storage_type ()
;;; 
;;; GtkImageType gtk_image_get_storage_type (GtkImage *image);
;;; 
;;; Gets the type of representation being used by the GtkImage to store image
;;; data. If the GtkImage has no image data, the return value will be
;;; GTK_IMAGE_EMPTY.
;;; 
;;; image :
;;; 	a GtkImage
;;; 
;;; Returns :
;;; 	image representation being used
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_image_new_from_file ()
;;; 
;;; GtkWidget * gtk_image_new_from_file (const gchar *filename)
;;; 
;;; Creates a new GtkImage displaying the file filename. If the file isn't
;;; found or can't be loaded, the resulting GtkImage will display a "broken
;;; image" icon. This function never returns NULL, it always returns a valid
;;; GtkImage widget.
;;; 
;;; If the file contains an animation, the image will contain an animation.
;;; 
;;; If you need to detect failures to load the file, use
;;; gdk_pixbuf_new_from_file() to load the file yourself, then create the
;;; GtkImage from the pixbuf. (Or for animations,
;;; use gdk_pixbuf_animation_new_from_file()).
;;; 
;;; The storage type (gtk_image_get_storage_type()) of the returned image is
;;; not defined, it will be whatever is appropriate for displaying the file.
;;; 
;;; filename :
;;; 	a filename.
;;; 
;;; Returns :
;;; 	a new GtkImage
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_image_new_from_file" gtk-image-new-from-file) (g-object widget)
  (filename :string))

(export 'gtk-image-new-from-file)

;;; ----------------------------------------------------------------------------
;;; gtk_image_new_from_icon_set ()
;;; 
;;; GtkWidget * gtk_image_new_from_icon_set (GtkIconSet *icon_set,
;;;                                          GtkIconSize size);
;;; 
;;; Creates a GtkImage displaying an icon set. Sample stock sizes are
;;; GTK_ICON_SIZE_MENU, GTK_ICON_SIZE_SMALL_TOOLBAR. Instead of using this
;;; function, usually it's better to create a GtkIconFactory, put your icon
;;; sets in the icon factory, add the icon factory to the list of default
;;; factories with gtk_icon_factory_add_default(), and then use
;;; gtk_image_new_from_stock(). This will allow themes to override the icon you
;;; ship with your application.
;;; 
;;; The GtkImage does not assume a reference to the icon set; you still need to
;;; unref it if you own references. GtkImage will add its own reference rather
;;; than adopting yours.
;;; 
;;; icon_set :
;;; 	a GtkIconSet
;;; 
;;; size :
;;; 	a stock icon size. [type int]
;;; 
;;; Returns :
;;; 	a new GtkImage
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_image_new_from_pixbuf ()
;;; 
;;; GtkWidget * gtk_image_new_from_pixbuf (GdkPixbuf *pixbuf);
;;; 
;;; Creates a new GtkImage displaying pixbuf. The GtkImage does not assume a
;;; reference to the pixbuf; you still need to unref it if you own references.
;;; GtkImage will add its own reference rather than adopting yours.
;;; 
;;; Note that this function just creates an GtkImage from the pixbuf. The
;;; GtkImage created will not react to state changes. Should you want that,
;;; you should use gtk_image_new_from_icon_set().
;;; 
;;; pixbuf :
;;; 	a GdkPixbuf, or NULL.
;;; 
;;; Returns :
;;; 	a new GtkImage
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_image_new_from_stock ()
;;; 
;;; GtkWidget * gtk_image_new_from_stock (const gchar *stock_id,
;;;                                       GtkIconSize size);
;;; 
;;; Creates a GtkImage displaying a stock icon. Sample stock icon names are
;;; GTK_STOCK_OPEN, GTK_STOCK_QUIT. Sample stock sizes are GTK_ICON_SIZE_MENU,
;;; GTK_ICON_SIZE_SMALL_TOOLBAR. If the stock icon name isn't known, the image
;;; will be empty. You can register your own stock icon names, see
;;; gtk_icon_factory_add_default() and gtk_icon_factory_add().
;;; 
;;; stock_id :
;;; 	a stock icon name
;;; 
;;; size :
;;; 	a stock icon size.
;;; 
;;; Returns :
;;; 	a new GtkImage displaying the stock icon
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_image_new_from_animation ()
;;; 
;;; GtkWidget * gtk_image_new_from_animation (GdkPixbufAnimation *animation);
;;; 
;;; Creates a GtkImage displaying the given animation. The GtkImage does not
;;; assume a reference to the animation; you still need to unref it if you own
;;; references. GtkImage will add its own reference rather than adopting yours.
;;; 
;;; Note that the animation frames are shown using a timeout with
;;; G_PRIORITY_DEFAULT. When using animations to indicate busyness, keep in
;;; mind that the animation will only be shown if the main loop is not busy
;;; with something that has a higher priority.
;;; 
;;; animation :
;;; 	an animation
;;; 
;;; Returns :
;;; 	a new GtkImage widget
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_image_new_from_icon_name ()
;;; 
;;; GtkWidget * gtk_image_new_from_icon_name (const gchar *icon_name,
;;;                                           GtkIconSize size);
;;; 
;;; Creates a GtkImage displaying an icon from the current icon theme. If the
;;; icon name isn't known, a "broken image" icon will be displayed instead. If
;;; the current icon theme is changed, the icon will be updated appropriately.
;;; 
;;; icon_name :
;;; 	an icon name
;;; 
;;; size :
;;; 	a stock icon size. [type int]
;;; 
;;; Returns :
;;; 	a new GtkImage displaying the themed icon
;;; 
;;; Since 2.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_image_new_from_gicon ()
;;; 
;;; GtkWidget * gtk_image_new_from_gicon (GIcon *icon, GtkIconSize size);
;;; 
;;; Creates a GtkImage displaying an icon from the current icon theme. If the
;;; icon name isn't known, a "broken image" icon will be displayed instead. If
;;; the current icon theme is changed, the icon will be updated appropriately.
;;; 
;;; icon :
;;; 	an icon
;;; 
;;; size :
;;; 	a stock icon size. [type int]
;;; 
;;; Returns :
;;; 	a new GtkImage displaying the themed icon
;;; 
;;; Since 2.14
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_image_set_from_file ()
;;; 
;;; void gtk_image_set_from_file (GtkImage *image, const gchar *filename);
;;; 
;;; See gtk_image_new_from_file() for details.
;;; 
;;; image :
;;; 	a GtkImage
;;; 
;;; filename :
;;; 	a filename or NULL.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_image_set_from_icon_set ()
;;; 
;;; void gtk_image_set_from_icon_set (GtkImage *image,
;;;                                   GtkIconSet *icon_set,
;;;                                   GtkIconSize size);
;;; 
;;; See gtk_image_new_from_icon_set() for details.
;;; 
;;; image :
;;; 	a GtkImage
;;; 
;;; icon_set :
;;; 	a GtkIconSet
;;; 
;;; size :
;;; 	a stock icon size.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_image_set_from_pixbuf ()
;;; 
;;; void gtk_image_set_from_pixbuf (GtkImage *image, GdkPixbuf *pixbuf);
;;; 
;;; See gtk_image_new_from_pixbuf() for details.
;;; 
;;; image :
;;; 	a GtkImage
;;; 
;;; pixbuf :
;;; 	a GdkPixbuf or NULL. [allow-none]
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_image_set_from_stock ()
;;; 
;;; void gtk_image_set_from_stock (GtkImage *image,
;;;                                const gchar *stock_id,
;;;                                GtkIconSize size);
;;; 
;;; See gtk_image_new_from_stock() for details.
;;; 
;;; image :
;;; 	a GtkImage
;;; 
;;; stock_id :
;;; 	a stock icon name
;;; 
;;; size :
;;; 	a stock icon size. [type int]
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_image_set_from_animation ()
;;; 
;;; void gtk_image_set_from_animation (GtkImage *image,
;;;                                    GdkPixbufAnimation *animation);
;;; 
;;; Causes the GtkImage to display the given animation (or display nothing, if
;;; you set the animation to NULL).
;;; 
;;; image :
;;; 	a GtkImage
;;; 
;;; animation :
;;; 	the GdkPixbufAnimation
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_image_set_from_animation" gtk-image-set-from-animation) :void
  (image (g-object gtk-image))
  (animation (g-object gdk-pixbuf-animation)))

(export 'gtk-image-set-from-animation)

;;; ----------------------------------------------------------------------------
;;; gtk_image_set_from_icon_name ()
;;; 
;;; void gtk_image_set_from_icon_name (GtkImage *image,
;;;                                    const gchar *icon_name,
;;;                                    GtkIconSize size);
;;; 
;;; See gtk_image_new_from_icon_name() for details.
;;; 
;;; image :
;;; 	a GtkImage
;;; 
;;; icon_name :
;;; 	an icon name
;;; 
;;; size :
;;; 	an icon size. [type int]
;;; 
;;; Since 2.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_image_set_from_gicon ()
;;; 
;;; void gtk_image_set_from_gicon (GtkImage *image,
;;;                                GIcon *icon,
;;;                                GtkIconSize size);
;;; 
;;; See gtk_image_new_from_gicon() for details.
;;; 
;;; image :
;;; 	a GtkImage
;;; 
;;; icon :
;;; 	an icon
;;; 
;;; size :
;;; 	an icon size. [type int]
;;; 
;;; Since 2.14
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_image_clear ()
;;; 
;;; void gtk_image_clear (GtkImage *image);
;;; 
;;; Resets the image to be empty.
;;; 
;;; image :
;;; 	a GtkImage
;;; 
;;; Since 2.8
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_image_new ()
;;; 
;;; GtkWidget * gtk_image_new (void);
;;; 
;;; Creates a new empty GtkImage widget.
;;; 
;;; Returns :
;;; 	a newly created GtkImage widget.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_image_set_pixel_size ()
;;; 
;;; void gtk_image_set_pixel_size (GtkImage *image, gint pixel_size);
;;; 
;;; Sets the pixel size to use for named icons. If the pixel size is set to a
;;; value != -1, it is used instead of the icon size set by
;;; gtk_image_set_from_icon_name().
;;; 
;;; image :
;;; 	a GtkImage
;;; 
;;; pixel_size :
;;; 	the new pixel size
;;; 
;;; Since 2.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_image_get_pixel_size ()
;;; 
;;; gint gtk_image_get_pixel_size (GtkImage *image);
;;; 
;;; Gets the pixel size used for named icons.
;;; 
;;; image :
;;; 	a GtkImage
;;; 
;;; Returns :
;;; 	the pixel size used for named icons.
;;; 
;;; Since 2.6
;;; ----------------------------------------------------------------------------


;;; --- End of file gtk.image.lisp ---------------------------------------------
