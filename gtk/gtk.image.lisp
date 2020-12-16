;;; ----------------------------------------------------------------------------
;;; gtk.image.lisp
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
;;; GtkImage
;;;
;;;     A widget displaying an image
;;;
;;; Types and Values
;;;
;;;     GtkImage
;;;     GtkImageType
;;;
;;; Functions
;;;
;;;     gtk_image_get_icon_set                             deprecated
;;;     gtk_image_get_pixbuf                               Accessor
;;;     gtk_image_get_stock                                deprecated
;;;     gtk_image_get_animation
;;;     gtk_image_get_icon_name
;;;     gtk_image_get_gicon
;;;     gtk_image_get_storage_type                         Accessor
;;;
;;;     gtk_image_new_from_file
;;;     gtk_image_new_from_icon_set                        deprecated
;;;     gtk_image_new_from_pixbuf
;;;     gtk_image_new_from_stock                           deprecated
;;;     gtk_image_new_from_animation
;;;     gtk_image_new_from_icon_name
;;;     gtk_image_new_from_gicon
;;;     gtk_image_new_from_resource
;;;     gtk_image_new_from_surface ()
;;;
;;;     gtk_image_set_from_file
;;;     gtk_image_set_from_icon_set                        deprecated
;;;     gtk_image_set_from_pixbuf
;;;     gtk_image_set_from_stock                           deprecated
;;;     gtk_image_set_from_animation
;;;     gtk_image_set_from_icon_name
;;;     gtk_image_set_from_gicon
;;;     gtk_image_set_from_resource
;;;     gtk_image_set_from_surface ()
;;;
;;;     gtk_image_clear
;;;     gtk_image_new
;;;     gtk_image_set_pixel_size                           Accessor
;;;     gtk_image_get_pixel_size                           Accessor
;;;
;;; Properties
;;;
;;;                  gchar*  file                Read / Write
;;;                  GIcon*  gicon               Read / Write
;;;                  gchar*  icon-name           Read / Write
;;;             GtkIconSet*  icon-set            Read / Write
;;;                   gint   icon-size           Read / Write
;;;              GdkPixbuf*  pixbuf              Read / Write
;;;     GdkPixbufAnimation*  pixbuf-animation    Read / Write
;;;                   gint   pixel-size          Read / Write
;;;                  gchar*  resource            Read / Write
;;;                  gchar*  stock               Read / Write
;;;           GtkImageType   storage-type        Read
;;;           CairoSurface*  surface             Read / Write
;;;               gboolean   use-fallback        Read / Write

;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkMisc
;;;                 ╰── GtkImage
;;;
;;; Implemented Interfaces
;;;
;;;     GtkImage implements AtkImplementorIface and GtkBuildable.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------

;;; CairoSurface represents a cairo-surface-t, but we need a boxed type in GTK+.

(define-g-boxed-opaque cairo-surface "CairoSurface"
  :alloc (error "CairoSurface cannot be created from the Lisp side."))

#+cl-cffi-gtk-documentation
(setf (gethash 'cairo-surface atdoc:*class-name-alias*) "CStruct"
      (documentation 'cairo-surface 'type)
 "@version{2020-4-16}
  @begin{short}
    @sym{cairo-surface} represents the type @symbol{cairo-surface-t} in GTK+.
  @end{short}
  See the documentation of @symbol{cairo-surface-t} for more information.
  @begin{pre}
(define-g-boxed-opaque cairo-surface \"CairoSurface\"
  :alloc (error \"CairoSurface cannot be created from the Lisp side.\"))
  @end{pre}
  @see-symbol{cairo-surface-t}")

(export (boxed-related-symbols 'cairo-surface))

;;; ----------------------------------------------------------------------------
;;; enum GtkImageType
;;; ----------------------------------------------------------------------------

(define-g-enum "GtkImageType" gtk-image-type
  (:export t
   :type-initializer "gtk_image_type_get_type")
  (:empty 0)
  (:pixbuf 1)
  (:stock 2)
  (:icon-set 3)
  (:animation 4)
  (:icon-name 5)
  (:gicon 6)
  #+gtk-3-10
  (:surface 7)
  )

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-image-type atdoc:*symbol-name-alias*) "Enum"
      (gethash 'gtk-image-type atdoc:*external-symbols*)
 "@version{2020-4-16}
  @begin{short}
    Describes the image data representation used by a @class{gtk-image}.
  @end{short}
  If you want to get the image from the widget, you can only get the
  currently stored representation. e. g. if the slot access function
  @fun{gtk-image-storage-type} returns @code{:pixbuf}, then you can call the
  function @fun{gtk-image-pixbuf} but not @fun{gtk-image-stock}. For empty
  images, you can request any storage type, but they will all return
  @code{nil} values.
  @begin{pre}
(define-g-enum \"GtkImageType\" gtk-image-type
  (:export t
   :type-initializer \"gtk_image_type_get_type\")
  (:empty 0)
  (:pixbuf 1)
  (:stock 2)
  (:icon-set 3)
  (:animation 4)
  (:icon-name 5)
  (:gicon 6)
  (:surface 7))
  @end{pre}
  @begin[code]{table}
    @entry[:empty]{There is no image displayed by the widget.}
    @entry[:pixbuf]{The widget contains a @class{gdk-pixbuf} object.}
    @entry[:stock]{The widget contains a stock icon name.}
    @entry[:icon-set]{The widget contains a @class{gtk-icon-set} structure.}
    @entry[:animation]{The widget contains a @class{gdk-pixbuf-animation}
      object.}
    @entry[:icon-name]{The widget contains a named icon.}
    @entry[:gicon]{The widget contains a @class{g-icon} object.}
    @entry[:surface]{The widget contains a @symbol{cairo-surface-t} instance.
      Since 3.10}
  @end{table}
  @see-class{gtk-image}
  @see-class{gdk-pixbuf}
  @see-class{gdk-pixbuf-animation}
  @see-class{gtk-icon-set}
  @see-class{g-icon}
  @see-symbol{cairo-surface-t}
  @see-function{gtk-image-storage-type}")

;;; ----------------------------------------------------------------------------
;;; struct GtkImage
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkImage" gtk-image
  (:superclass gtk-misc
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable")
   :type-initializer "gtk_image_get_type")
  ((file
    gtk-image-file
    "file" "gchararray" t t)
   (gicon
    gtk-image-gicon
    "gicon" "GIcon" t t)
   (icon-name
    gtk-image-icon-name
    "icon-name" "gchararray" t t)
   (icon-set
    gtk-image-icon-set
    "icon-set" "GtkIconSet" t t)
   ;; TODO: In the C implementation icon-size has the type :int, the accessor
   ;; gtk-image-icon-size returns therefore an integer and not a keyword of the
   ;; gtk-icon-size enumeration. This is not consistent.
   (icon-size
    gtk-image-icon-size
    "icon-size" "GtkIconSize" t t)
   (pixbuf
    gtk-image-pixbuf
    "pixbuf" "GdkPixbuf" t t)
   (pixbuf-animation
    gtk-image-pixbuf-animation
    "pixbuf-animation" "GdkPixbufAnimation" t t)
   (pixel-size
    gtk-image-pixel-size
    "pixel-size" "gint" t t)
   #+gtk-3-8
   (resource
    gtk-image-resource
    "resource" "gchararray" t t)
   (stock
    gtk-image-stock
    "stock" "gchararray" t t)
   (storage-type
    gtk-image-storage-type
    "storage-type" "GtkImageType" t nil)
   #+gtk-3-10
   (surface
    gtk-image-surface
    "surface" "CairoSurface" t t)
   (use-fallback
    gtk-image-use-fallback
    "use-fallback" "gboolean" t t)))

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-image 'type)
 "@version{2020-4-17}
  @begin{short}
    The @sym{gtk-image} widget displays an image. Various kinds of object can be
    displayed as an image; most typically, you would load a @class{gdk-pixbuf}
    object from a file, and then display that.
  @end{short}

  @image[image]{}

  There is a convenience function @fun{gtk-image-new-from-file} to do this,
  used as follows:
  @begin{pre}
 (let ((image (gtk-image-new-from-file \"myfile.png\")))
   ... )
  @end{pre}
  If the file is not loaded successfully, the image will contain a
  \"broken image\" icon similar to that used in many web browsers. If you want
  to handle errors in loading the file yourself, for example by displaying an
  error message, then load the image with the function
  @fun{gdk-pixbuf-new-from-file}, then create the @sym{gtk-image} with
  the function @fun{gtk-image-new-from-pixbuf}.

  The image file may contain an animation, if so the @sym{gtk-image} will
  display an animation of type @class{gdk-pixbuf-animation} instead of a static
  image.

  @sym{gtk-image} is a subclass of @class{gtk-misc}, which implies that you can
  align it (center, left, right) and add padding to it, using @class{gtk-misc}
  methods.

  @sym{gtk-image} is a \"no window\" widget (has no @class{gdk-window} of its
  own), so by default does not receive events. If you want to receive events on
  the image, such as button clicks, place the image inside a
  @class{gtk-event-box}, then connect to the event signals on the event box.
  @begin[Example]{dictionary}
    Handling button press events on a @sym{gtk-image}.
    @begin{pre}
(defun create-image ()
  (let ((image (gtk-image-new-from-file (rel-path \"ducky.png\")))
        (event-box (make-instance 'gtk-event-box)))
    ;; Set the event mask for the event box
    (setf (gtk-widget-events event-box) :button-press-mask)
    ;; Connect a signal to the event box
    (g-signal-connect event-box \"button-press-event\"
                      (lambda (box event)
                        (declare (ignore box))
                        (format t \"Event box clicked at : ~6,2f, ~6,2f~%\"
                                  (gdk-event-button-x event)
                                  (gdk-event-button-y event))
                        +gdk-event-stop+))
    ;; Add the image to the event box
    (gtk-container-add event-box image)
    ;; Return the event box with the image
    event-box))
    @end{pre}
    When handling events on the event box, keep in mind that coordinates in the
    image may be different from event box coordinates due to the alignment and
    padding settings on the image, see @class{gtk-misc}. The simplest way to
    solve this is to set the alignment to 0.0 (left/top), and set the padding
    to zero. Then the origin of the image will be the same as the origin of the
    event box.
  @end{dictionary}
  @see-slot{gtk-image-file}
  @see-slot{gtk-image-gicon}
  @see-slot{gtk-image-icon-name}
  @see-slot{gtk-image-icon-set}
  @see-slot{gtk-image-icon-size}
  @see-slot{gtk-image-pixbuf}
  @see-slot{gtk-image-pixbuf-animation}
  @see-slot{gtk-image-pixel-size}
  @see-slot{gtk-image-stock}
  @see-slot{gtk-image-storage-type}
  @see-slot{gtk-image-surface}
  @see-slot{gtk-image-use-fallback}
  @see-class{gdk-pixbuf}
  @see-class{gdk-pixbuf-animation}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk-image-file ---------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "file" 'gtk-image) 't)
 "The @code{file} property of type @code{g-string} (Read / Write) @br{}
  The name of the file to load and display. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-image-file atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-image-file 'function)
 "@version{2020-4-17}
  @syntax[]{(gtk-image-file object) => filename}
  @syntax[]{(setf (gtk-image-file object) filename)}
  @argument[object]{a @class{gtk-image} object}
  @argument[filename]{a string with the name of the file to load and display}
  @begin{short}
    Accessor of the @slot[gtk-image]{file} slot of the @class{gtk-image} class.
  @end{short}

  The name of the file to load and display.
  @see-class{gtk-image}")

;;; --- gtk-image-gicon --------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "gicon" 'gtk-image) 't)
 "The @code{gicon} property of type @class{g-icon} (Read / Write) @br{}
  The icon displayed in the image. For themed icons, if the
  icon theme is changed, the image will be updated automatically.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-image-gicon atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-image-gicon 'function)
 "@version{2020-4-17}
  @syntax[]{(gtk-image-gicon object) => gicon}
  @syntax[]{(setf (gtk-image-gicon object) gicon)}
  @argument[object]{a @class{gtk-image} object}
  @argument[gicon]{a icon of type @class{g-icon}}
  @begin{short}
    Accessor of the @slot[gtk-image]{gicon} slot of the @class{gtk-image} class.
  @end{short}

  The icon displayed in the image. For themed icons, if the icon theme is
  changed, the image will be updated automatically.
  @see-class{gtk-image}
  @see-class{g-icon}
  @see-class{g-themed-icon}")

;;; --- gtk-image-icon-name ----------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "icon-name" 'gtk-image) 't)
 "The @code{icon-name} property of type @code{g-string} (Read / Write) @br{}
  The name of the icon in the icon theme. If the icon theme is changed, the
  image will be updated automatically. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-image-icon-name atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-image-icon-name 'function)
 "@version{2020-4-17}
  @syntax[]{(gtk-image-icon-name object) => icon-name}
  @syntax[]{(setf (gtk-image-icon-name object) icon-name)}
  @argument[object]{a @class{gtk-image} object}
  @argument[icon-name]{a string with the name of the icon}
  @begin{short}
    Accessor of the @slot[gtk-image]{icon-name} slot of the @class{gtk-image}
    class.
  @end{short}

  The name of the icon in the icon theme. If the icon theme is changed, the
  image will be updated automatically.
  @see-class{gtk-image}
  @see-function{gtk-image-get-icon-name}")

;;; --- gtk-image-icon-set -----------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "icon-set" 'gtk-image) 't)
 "The @code{icon-set} property of type @class{gtk-icon-set} (Read / Write) @br{}
  The icon set to display. @br{}
  @em{Warning:} The @code{icon-set} poperty has been deprecated since version
  3.10 and should not be used in newly-written code. Use the
  @code{icon-name} property instead.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-image-icon-set atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-image-icon-set 'function)
 "@version{2020-4-17}
  @syntax[]{(gtk-image-icon-set object) => icon-set}
  @syntax[]{(setf (gtk-image-icon-set object) icon-set)}
  @argument[object]{a @class{gtk-image} object}
  @argument[icon-set]{a icon set of type @class{gtk-icon-set}}
  @begin{short}
    Accessor of the @slot[gtk-image]{icon-set} slot of the @class{gtk-image}
    class.
  @end{short}

  The icon set to display.
  @begin[Warning]{dictionary}
    The function @sym{gtk-image-icon-set} has been deprecated since version
    3.10 and should not be used in newly-written code. Use the function
    @fun{gtk-image-icon-name} instead.
  @end{dictionary}
  @see-class{gtk-image}
  @see-function{gtk-image-icon-name}
  @see-function{gtk-image-get-icon-set}")

;;; --- gtk-image-icon-size ----------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "icon-size" 'gtk-image) 't)
 "The @code{icon-size} property of type @code{:int} (Read / Write) @br{}
  Symbolic size to use for a stock icon, icon set or named icon. @br{}
  Allowed values: >= 0 @br{}
  Default value: 4")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-image-icon-size atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-image-icon-size 'function)
 "@version{2020-4-17}
  @syntax[]{(gtk-image-icon-size object) => icon-size}
  @syntax[]{(setf (gtk-image-icon-size object) icon-size)}
  @argument[object]{a @class{gtk-image} object}
  @argument[icon-set]{an integer with the icon size}
  @begin{short}
    Accessor of the @slot[gtk-image]{icon-size} slot of the @class{gtk-image}
    class.
  @end{short}

  Symbolic size to use for a stock icon, icon set or named icon.
  @begin[Note]{dictionary}
    In C the @code{icon-size} property is implemented as an integer type.
    Therefore the accessor @sym{gtk-image-icon-size} returns an integer value
    and not an keyword value of the @symbol{gtk-icon-size} enumeration.
  @end{dictionary}
  @see-class{gtk-image}")

;;; --- gtk-image-pixbuf -------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "pixbuf" 'gtk-image) 't)
 "The @code{pixbuf} property of type @class{gdk-pixbuf} (Read / Write) @br{}
  A @class{gdk-pixbuf} object to display.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-image-pixbuf atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-image-pixbuf 'function)
 "@version{2020-4-17}
  @syntax[]{(gtk-image-pixbuf object) => pixbuf}
  @syntax[]{(setf (gtk-image-pixbuf object) pixbuf)}
  @argument[object]{a @class{gtk-image} widget}
  @argument[pixbuf]{a @class{gdk-pixbuf} object}
  @begin{short}
    Accessor of the @slot[gtk-image]{pixbuf} slot of the @class{gtk-image}
    class.
  @end{short}

  The slot access function @sym{gtk-image-pixbuf} gets the @class{gdk-pixbuf}
  object being displayed by the image. The slot access function
  @sym{(setf gtk-image-pixbuf)} sets the @class{gdk-pixbuf} object.

  The @symbol{gtk-image-type} storage type of the image must be @code{:empty}
  or @code{:pixbuf}, see the function @fun{gtk-image-storage-type}.
  @see-class{gtk-image}
  @see-class{gdk-pixbuf}
  @see-symbol{gtk-image-type}
  @see-function{gtk-image-storage-type}")

;;; --- gtk-image-pixbuf-animation ---------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "pixbuf-animation"
                                               'gtk-image) 't)
 "The @code{pixbuf-animation} property of type @class{gdk-pixbuf-animation}
  (Read / Write) @br{}
  The @class{gdk-pixbuf-animation} object to display.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-image-pixbuf-animation atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-image-pixbuf-animation 'function)
 "@version{2020-4-17}
  @syntax[]{(gtk-image-pixbuf-animation object) => animation}
  @syntax[]{(setf (gtk-image-pixbuf-animation object) animation)}
  @argument[object]{a @class{gtk-image} widget}
  @argument[animation]{a @class{gdk-pixbuf-animation} object}
  @begin{short}
    Accessor of the @slot[gtk-image]{pixbuf-animation} slot of the
    @class{gtk-image} class.
  @end{short}
  @see-class{gtk-image}
  @see-function{gtk-image-get-animation}")

;;; --- gtk-image-pixel-size ---------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "pixel-size" 'gtk-image) 't)
 "The @code{pixel-size} property of type @code{:int} (Read / Write) @br{}
  The @code{pixel-size} property can be used to specify a fixed size
  overriding the @code{icon-size} property for images of type
  @code{:icon-name}. @br{}
  Allowed values: >= -1 @br{}
  Default value: -1")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-image-pixel-size atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-image-pixel-size 'function)
 "@version{2020-4-17}
  @syntax[]{(gtk-image-pixel-size object) => pixel-size}
  @syntax[]{(setf (gtk-image-pixel-size object) pixel-size)}
  @argument[object]{a @class{gtk-image} widget}
  @argument[pixel-size]{an integer with the new pixel size}
  @begin{short}
    Accessor of the @slot[gtk-image]{pixel-size} slot of the @class{gtk-image}
    class.
  @end{short}

  The slot access function @sym{gtk-image-pixel-size} sets the pixel size used
  for named icons. The slot access function @sym{(setf gtk-image-pixel-size)}
  sets the pixel size to use for named icons. If the pixel size is set to a
  value not equal to -1, it is used instead of the @code{icon-size} property.
  @see-class{gtk-image}")

;;; --- gtk-image-resource -----------------------------------------------------

#+(and gtk-3-8 cl-cffi-gtk-documentation)
(setf (documentation (atdoc:get-slot-from-name "resource" 'gtk-image) 't)
 "The @code{resource} property of type @code{g-string} (Read / Write) @br{}
  A path to a resource file to display. Since 3.8 @br{}
  Default value: @code{nil}")

#+(and gtk-3-8 cl-cffi-gtk-documentation)
(setf (gethash 'gtk-image-resource atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-image-resource 'function)
 "@version{2020-4-17}
  @syntax[]{(gtk-image-resource object) => path}
  @syntax[]{(setf (gtk-image-resource object) path)}
  @argument[object]{a @class{gtk-image} widget}
  @argument[path]{a string with a resource path}
  @begin{short}
    Accessor of the @slot[gtk-image]{stock} slot of the @class{gtk-image} class.
  @end{short}

  A path to a resource file to display.

  Since 3.8
  @see-class{gtk-image}")

;;; --- gtk-image-stock --------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "stock" 'gtk-image) 't)
 "The @code{stock} property of type @code{g-string} (Read / Write) @br{}
  The stock ID for a stock image to display. @br{}
  @em{Warning:} The @code{stock} property has been deprecated since version
  3.10 and should not be used in newly-written code. Use the @code{icon-name}
  property instead. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-image-stock atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-image-stock 'function)
 "@version{2020-4-17}
  @syntax[]{(gtk-image-stock object) => stock-id}
  @syntax[]{(setf (gtk-image-stock object) stock-id)}
  @argument[object]{a @class{gtk-image} widget}
  @argument[path]{a string with a stock ID}
  @begin{short}
    Accessor of the @slot[gtk-image]{stock} slot of the @class{gtk-image} class.
  @end{short}

  The stock ID for a stock image to display.
  @begin[Warning]{dictionary}
    The function @sym{gtk-image-stock} property has been deprecated since
    version 3.10 and should not be used in newly-written code. Use the
    function @fun{gtk-image-icon-name} instead.
  @end{dictionary}
  @see-class{gtk-image}
  @see-function{gtk-image-get-stock}")

;;; --- gtk-image-storage-type -------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "storage-type" 'gtk-image) 't)
 "The @code{storage-type} property of type @symbol{gtk-image-type} (Read) @br{}
  The representation being used for image data. @br{}
  Default value: @code{:empty}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-image-storage-type atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-image-storage-type 'function)
 "@version{2020-4-17}
  @syntax[]{(gtk-image-storage-type object) => type}
  @syntax[]{(setf (gtk-image-storage-type object) type)}
  @argument[object]{a @class{gtk-image} widget}
  @argument[type]{a value of the @symbol{gtk-image-type} enumeration}
  @begin{short}
    Accessor of the @slot[gtk-image]{storage-type} slot of the @class{gtk-image}
    class.
  @end{short}

  The slot access function @sym{gtk-image-storage-type} gets the type of
  representation being used by the @class{gtk-image} widget to store image data.
  The slot access function @sym{(setf gtk-image-storage-type)} sets the image
  type.

  If the @class{gtk-image} widget has no image data, the return value will be
  @code{:empty}.
  @see-class{gtk-image}
  @see-symbol{gtk-image-type}")

;;; --- gtk-image-surface ------------------------------------------------------

#+(and gtk-3-10 cl-cffi-gtk-documentation)
(setf (documentation (atdoc:get-slot-from-name "surface" 'gtk-image) 't)
 "The @code{surface} property of type @code{g-string} (Read / Write) @br{}
  A @symbol{cairo-surface-t} instance to display. Since 3.10")

#+(and gtk-3-10 cl-cffi-gtk-documentation)
(setf (gethash 'gtk-image-surface atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-image-surface 'function)
 "@version{2020-4-17}
  @syntax[]{(gtk-image-surface object) => surface}
  @syntax[]{(setf (gtk-image-surface object) surface)}
  @argument[object]{a @class{gtk-image} widget}
  @argument[surface]{a @class{cairo-surface} structure}
  @begin{short}
    Accessor of the @slot[gtk-image]{surface} slot of the @class{gtk-image}
    class.
  @end{short}

  Since 3.10
  @see-class{gtk-image}
  @see-class{cairo-surface}")

;;; --- gtk-image-use-fallback -------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "use-fallback" 'gtk-image) 't)
 "The @code{use-fallback} property of type @code{:boolean} (Read / Write) @br{}
  Whether the icon displayed in the @sym{gtk-image} will use standard icon
  names fallback. The value of this property is only relevant for images of
  type @code{:icon-name} and @code{:gicon}. @br{}
  Default value: @em{false}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-image-use-fallback atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-image-use-fallback 'function)
 "@version{2020-4-17}
  @syntax[]{(gtk-image-use-fallback object) => use-fallback}
  @syntax[]{(setf (gtk-image-use-fallback object) use-fallback)}
  @argument[object]{a @class{gtk-image} widget}
  @argument[use-fallback]{a @code{:boolean} wether to use standard icon names
    fallback}
  @begin{short}
    Accessor of the @slot[gtk-image]{use-fallback} slot of the @class{gtk-image}
    class.
  @end{short}

  Whether the icon displayed in the @sym{gtk-image} will use standard icon
  names fallback. The value of this property is only relevant for images of
  type @code{:icon-name} and @code{:gicon}.
  @see-class{gtk-image}")

;;; ----------------------------------------------------------------------------
;;; gtk_image_get_icon_set ()
;;; ----------------------------------------------------------------------------

(defun gtk-image-get-icon-set (image)
 #+cl-cffi-gtk-documentation
 "@version{2020-4-17}
  @argument[image]{a @class{gtk-image} widget}
  @begin{return}
    @code{icon-set} -- a @class{gtk-icon-set} structure @br{}
    @code{icon-size} -- a icon size of type @symbol{gtk-icon-size}
  @end{return}
  @begin{short}
    Gets the icon set and icon size being displayed by the @class{gtk-image}.
  @end{short}

  The storage type of the image must be @code{:empty} or @code{:icon-set}. See
  the function @fun{gtk-image-storage-type}.
  @begin[Warning]{dictionary}
    The function @sym{gtk-image-get-icon-set} has been deprecated since version
    3.10 and should not be used in newly-written code. Use the function
    @fun{gtk-image-get-icon-name} instead.
  @end{dictionary}
  @see-class{gtk-image}
  @see-class{gtk-icon-set}
  @see-function{gtk-image-get-icon-name}
  @see-function{gtk-image-storage-type}"
  (values (gtk-image-icon-set image)
          (foreign-enum-keyword 'gtk-icon-size (gtk-image-icon-size image))))

(export 'gtk-image-get-icon-set)

;;; ----------------------------------------------------------------------------
;;; gtk_image_get_stock ()
;;; ----------------------------------------------------------------------------

(defun gtk-image-get-stock (image)
 #+cl-cffi-gtk-documentation
 "@version{2020-4-17}
  @argument[image]{a @class{gtk-image} widget}
  @begin{return}
    @code{stock-id} -- a string with a stock icon name @br{}
    @code{size} -- a stock icon size of type @symbol{gtk-icon-size}
  @end{return}
  @begin{short}
    Gets the stock icon name and icon size being displayed by the image.
  @end{short}
  The @symbol{gtk-image-type} storage type of the image must be @code{:empty}
  or @code{:stock}, see the function @fun{gtk-image-storage-type}.
  @begin[Warning]{dictionary}
    The function @sym{gtk-image-get-stock} has been deprecated since version
    3.10 and should not be used in newly-written code. Use the function
    @fun{gtk-image-get-icon-name} instead.
  @end{dictionary}
  @see-class{gtk-image}
  @see-symbol{gtk-icon-size}
  @see-symbol{gtk-image-type}
  @see-function{gtk-image-storage-type}
  @see-function{gtk-image-get-icon-name}"
  (values (gtk-image-stock image)
          (foreign-enum-keyword 'gtk-icon-size (gtk-image-icon-size image))))

(export 'gtk-image-get-stock)

;;; ----------------------------------------------------------------------------
;;; gtk_image_get_animation ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_image_get_animation" gtk-image-get-animation)
    (g-object gdk-pixbuf-animation)
 #+cl-cffi-gtk-documentation
 "@version{2020-4-17}
  @argument[image]{a @class{gtk-image} widget}
  @return{The displayed animation, or @code{nil} if the image is empty.}
  @begin{short}
    Gets the @class{gdk-pixbuf-animation} object being displayed by the
    @class{gtk-image} widget.
  @end{short}
  The @symbol{gtk-image-type} storage type of the image must be @code{:empty}
  or @code{:animation}, see the function @fun{gtk-image-storage-type}.
  @see-class{gtk-image}
  @see-class{gdk-pixbuf-animation}
  @see-symbol{gtk-image-type}
  @see-function{gtk-image-storage-type}"
  (image (g-object gtk-image)))

(export 'gtk-image-get-animation)

;;; ----------------------------------------------------------------------------
;;; gtk_image_get_icon_name ()
;;; ----------------------------------------------------------------------------

(defun gtk-image-get-icon-name (image)
 #+cl-cffi-gtk-documentation
 "@version{2020-4-17}
  @argument[image]{a @class{gtk-image} object}
  @begin{return}
    @code{icon-name} -- an icon name @br{}
    @code{size} -- an icon size of type @symbol{gtk-icon-size}
  @end{return}
  @begin{short}
    Gets the icon name and icon size being displayed by the image.
  @end{short}
  The storage type of the image must be @code{:empty} or @code{:icon-name},
  see the function @fun{gtk-image-storage-type}.
  @see-class{gtk-image}
  @see-function{gtk-image-storage-type}"
  (values (gtk-image-icon-name image)
          (foreign-enum-keyword 'gtk-icon-size (gtk-image-icon-size image))))

(export 'gtk-image-get-icon-name)

;;; ----------------------------------------------------------------------------
;;; gtk_image_get_gicon ()
;;; ----------------------------------------------------------------------------

(defun gtk-image-get-gicon (image)
 #+cl-cffi-gtk-documentation
 "@version{2020-4-17}
  @argument[image]{a @class{gtk-image} widget}
  @begin{return}
    @code{gicon} -- a @class{g-icon} object @br{}
    @code{size} -- an icon size of type @symbol{gtk-icon-size}
  @end{return}
  @begin{short}
    Gets the @class{g-icon} object and icon size being displayed by the
    image.
  @end{short}
  The storage type of the image must be @code{:empty} or @code{:gicon}, see the
  function @fun{gtk-image-storage-type}.
  @see-class{gtk-image}
  @see-class{g-icon}
  @see-function{gtk-image-storage-type}"
  (values (gtk-image-gicon image)
          (foreign-enum-keyword 'gtk-icon-size (gtk-image-icon-size image))))

(export 'gtk-image-get-gicon)

;;; ----------------------------------------------------------------------------
;;; gtk_image_new_from_file ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_image_new_from_file" gtk-image-new-from-file)
    (g-object gtk-widget)
 #+cl-cffi-gtk-documentation
 "@version{2020-4-17}
  @argument[filename]{a string with the name of the file}
  @return{A new @class{gtk-image} object.}
  @begin{short}
    Creates an image displaying the file.
  @end{short}
  If the file is not found or cannot be loaded, the resulting @class{gtk-image}
  object will display a \"broken image\" icon. This function never returns
  @code{nil}, it always returns a valid @class{gtk-image} widget.

  If the file contains an animation, the image will contain an animation.

  If you need to detect failures to load the file, use the function
  @fun{gdk-pixbuf-new-from-file} to load the file yourself, then create the
  @class{gtk-image} object from the pixbuf. Or for animations, use the function
  @fun{gdk-pixbuf-animation-new-from-file}.

  The storage type, see the function @fun{gtk-image-storage-type}, of the
  returned image is not defined, it will be whatever is appropriate for
  displaying the file.
  @see-class{gtk-image}
  @see-function{gdk-pixbuf-new-from-file}
  @see-function{gdk-pixbuf-animation-new-from-file}
  @see-function{gtk-image-storage-type}"
  (filename g-string))

(export 'gtk-image-new-from-file)

;;; ----------------------------------------------------------------------------
;;; gtk_image_new_from_icon_set ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_image_new_from_icon_set" gtk-image-new-from-icon-set)
    (g-object gtk-image)
 #+cl-cffi-gtk-documentation
 "@version{2020-4-17}
  @argument[icon-set]{a @class{gtk-icon-set} structure}
  @argument[icon-size]{a icon size of type @symbol{gtk-icon-size}}
  @return{A new @class{gtk-image} object.}
  @begin{short}
    Creates an image displaying an icon set.
  @end{short}

  Sample icon sizes are @code{:menu}, @code{:small-toolbar}. Instead of using
  this function, usually it is better to create a @class{gtk-icon-factory}, put
  your icon sets in the icon factory, add the icon factory to the list of
  default factories with the function @fun{gtk-icon-factory-add-default}, and
  then use the function @fun{gtk-image-new-from-stock}. This will allow themes
  to override the icon you ship with your application.
  @begin[Warning]{dictionary}
    The function @sym{gtk-image-new-from-icon-set} has been deprecated since
    version 3.10 and should not be used in newly-written code. Use the function
    @fun{gtk-image-new-from-icon-name} instead.
  @end{dictionary}
  @see-class{gtk-image}
  @see-class{gtk-icon-factory}
  @see-function{gtk-icon-factory-add-default}
  @see-function{gtk-image-new-from-icon-name}
  @see-function{gtk-image-new-from-stock}"
  (icon-set (g-boxed-foreign gtk-icon-set))
  (icon-size gtk-icon-size))

(export 'gtk-image-new-from-icon-set)

;;; ----------------------------------------------------------------------------
;;; gtk_image_new_from_pixbuf ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_image_new_from_pixbuf" gtk-image-new-from-pixbuf)
    (g-object gtk-image)
 #+cl-cffi-gtk-documentation
 "@version{2020-4-17}
  @argument[pixbuf]{a @class{gdk-pixbuf} object}
  @return{A new @class{gtk-image} object.}
  @begin{short}
    Creates an image displaying @arg{pixbuf}.
  @end{short}

  Note that this function just creates an @class{gtk-image} object from the
  pixbuf. The @class{gtk-image} object created will not react to state changes.
  Should you want that, you should use the function
  @fun{gtk-image-new-from-icon-name}.
  @see-class{gtk-image}
  @see-class{gdk-pixbuf}
  @see-function{gtk-image-new-from-icon-name}"
  (pixbuf (g-object gdk-pixbuf)))

(export 'gtk-image-new-from-pixbuf)

;;; ----------------------------------------------------------------------------
;;; gtk_image_new_from_stock ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_image_new_from_stock" gtk-image-new-from-stock)
    (g-object gtk-widget)
 #+cl-cffi-gtk-documentation
 "@version{2020-4-17}
  @argument[stock-id]{a string with the stock icon name}
  @argument[icon-size]{a stock icon size from the @symbol{gtk-icon-size}
    enumeration}
  @return{A new @class{gtk-image} widget displaying the stock icon.}
  @begin{short}
    Creates a an image displaying a stock icon.
  @end{short}

  Sample stock icon names are @code{\"gtk-open\"}, @code{\"gtk-quit\"}. Sample
  stock sizes are @code{:menu}, @code{:small-toolbar}. If the stock icon name
  is not known, the image will be empty. You can register your own stock icon
  names, see the functions @fun{gtk-icon-factory-add-default} and
  @fun{gtk-icon-factory-add}.
  @begin[Warning]{dictionary}
    The function @sym{gtk-image-new-from-stock} has been deprecated since
    version 3.10 and should not be used in newly-written code. Use the function
    @fun{gtk-image-new-from-icon-name} instead.
  @end{dictionary}
  @see-class{gtk-image}
  @see-symbol{gtk-icon-size}
  @see-function{gtk-icon-factory-add}
  @see-function{gtk-icon-factory-add-default}"
  (stock-id g-string)
  (icon-size gtk-icon-size))

(export 'gtk-image-new-from-stock)

;;; ----------------------------------------------------------------------------
;;; gtk_image_new_from_animation ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_image_new_from_animation" gtk-image-new-from-animation)
    (g-object gtk-image)
 #+cl-cffi-gtk-documentation
 "@version{2020-4-17}
  @argument[animation]{an animation of type @class{gdk-pixbuf-animation}}
  @return{A new @class{gtk-image} widget.}
  @begin{short}
    Creates a image displaying the given animation.
  @end{short}

  Note that the animation frames are shown using a timeout with
  @code{:default}. When using animations to indicate busyness, keep in mind
  that the animation will only be shown if the main loop is not busy with
  something that has a higher priority.
  @see-class{gtk-image}
  @see-class{gdk-pixbuf-animation}"
  (animation (g-object gdk-pixbuf-animation)))

(export 'gtk-image-new-from-animation)

;;; ----------------------------------------------------------------------------
;;; gtk_image_new_from_icon_name ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_image_new_from_icon_name" gtk-image-new-from-icon-name)
    (g-object gtk-image)
 #+cl-cffi-gtk-documentation
 "@version{2020-4-17}
  @argument[icon-name]{a string with an icon name}
  @argument[size]{a icon size of type @symbol{gtk-icon-size}}
  @return{A new @class{gtk-image} widget displaying the themed icon.}
  @begin{short}
    Creates an image displaying an icon from the current icon theme.
  @end{short}

  If the icon name is not known, a \"broken image\" icon will be displayed
  instead. If the current icon theme is changed, the icon will be updated
  appropriately.
  @see-class{gtk-image}
  @see-function{gtk-image-set-from-icon-name}"
  (icon-name g-string)
  (size gtk-icon-size))

(export 'gtk-image-new-from-icon-name)

;;; ----------------------------------------------------------------------------
;;; gtk_image_new_from_gicon ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_image_new_from_gicon" gtk-image-new-from-gicon)
    (g-object gtk-image)
 #+cl-cffi-gtk-documentation
 "@version{2020-4-17}
  @argument[icon]{an icon of type @class{g-icon}}
  @argument[size]{an icon size of type @symbol{gtk-icon-size}}
  @return{A new @class{gtk-image} displaying the themed icon.}
  @begin{short}
    Creates an image displaying an icon from the current icon theme.
  @end{short}

  If the icon name is not known, a \"broken image\" icon will be displayed
  instead. If the current icon theme is changed, the icon will be updated
  appropriately.
  @see-class{gtk-image}
  @see-class{g-icon}
  @see-symbol{gtk-icon-size}"
  (icon (g-object g-icon))
  (size gtk-icon-size))

(export 'gtk-image-new-from-gicon)

;;; ----------------------------------------------------------------------------
;;; gtk_image_new_from_resource ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_image_new_from_resource" gtk-image-new-from-resource)
    (g-object gtk-image)
 #+cl-cffi-gtk-documentation
 "@version{2020-4-17}
  @argument[resource-path]{a string with a resource path}
  @return{A new @class{gtk-image} widget.}
  @begin{short}
    Creates an image displaying the resource file in @arg{resource-path}.
  @end{short}

  If the file is not found or can not be loaded, the resulting @class{gtk-image}
  will display a \"broken image\" icon. This function always returns a valid
  @class{gtk-image} widget.

  If the file contains an animation, the image will contain an animation.

  If you need to detect failures to load the file, use the function
  @fun{gdk-pixbuf-new-from-file} to load the file yourself, then create the
  @class{gtk-image} from the pixbuf. Or for animations, use the function
  @fun{gdk-pixbuf-animation-new-from-file}.

  The storage type, see the function @fun{gtk-image-storage-type}, of the
  returned image is not defined, it will be whatever is appropriate for
  displaying the file.
  @see-class{gtk-image}
  @see-function{gdk-pixbuf-new-from-file}
  @see-function{gdk-pixbuf-animation-new-from-file}"
  (resource-path g-string))

(export 'gtk-image-new-from-resource)

;;; ----------------------------------------------------------------------------
;;; gtk_image_new_from_surface ()
;;; ----------------------------------------------------------------------------

#+gtk-3-10
(defcfun ("gtk_image_new_from_surface" gtk-image-new-from-surface)
    (g-object gtk-image)
 #+cl-cffi-gtk-documentation
 "@version{2020-4-17}
  @argument[surface]{a @symbol{cairo-surface-t} instance}
  @return{A new @class{gtk-image} object.}
  @begin{short}
    Creates a new image displaying @arg{surface}.
  @end{short}

  Since 3.10
  @see-class{gtk-image}
  @see-symbol{cairo-surface-t}"
  (surface (:pointer (:struct cairo-surface-t))))

#+gtk-3-10
(export 'gtk-image-new-from-surface)

;;; ----------------------------------------------------------------------------
;;; gtk_image_set_from_file ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_image_set_from_file" gtk-image-set-from-file) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-4-17}
  @argument[image]{a @class{gtk-image} widget}
  @argument[filename]{a string with a filename}
  @begin{short}
    See the function @fun{gtk-image-new-from-file} for details.
  @end{short}
  @see-class{gtk-image}
  @see-function{gtk-image-new-from-file}"
  (image (g-object gtk-image))
  (filename g-string))

(export 'gtk-image-set-from-file)

;;; ----------------------------------------------------------------------------
;;; gtk_image_set_from_icon_set ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_image_set_from_icon_set" gtk-image-set-from-icon-set) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-4-17}
  @argument[image]{a @class{gtk-image} widget}
  @argument[icon-set]{a @class{gtk-icon-set} structure}
  @argument[icon-size]{a stock icon size of type @symbol{gtk-icon-size}}
  @begin{short}
    See the function @fun{gtk-image-new-from-icon-set} for details.
  @end{short}
  @begin[Warning]{dictionary}
    The function @sym{gtk-image-set-from-icon-set} has been deprecated since
    version 3.10 and should not be used in newly-written code. Use the function
    @fun{gtk-image-set-from-icon-name} instead.
  @end{dictionary}
  @see-class{gtk-image}
  @see-function{gtk-image-new-from-icon-set}
  @see-function{gtk-image-set-from-icon-name}"
  (image (g-object gtk-image))
  (icon-set (g-boxed-foreign gtk-icon-set))
  (icon-size gtk-icon-size))

(export 'gtk-image-set-from-icon-set)

;;; ----------------------------------------------------------------------------
;;; gtk_image_set_from_pixbuf ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_image_set_from_pixbuf" gtk-image-set-from-pixbuf) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-4-17}
  @argument[image]{a @class{gtk-image} widget}
  @argument[pixbuf]{a @class{gdk-pixbuf} object}
  @begin{short}
    See the function @fun{gtk-image-new-from-pixbuf} for details.
  @end{short}
  @see-class{gtk-image}
  @see-class{gdk-pixbuf}
  @see-function{gtk-image-new-from-pixbuf}"
  (image (g-object gtk-image))
  (pixbuf (g-object gdk-pixbuf)))

(export 'gtk-image-set-from-pixbuf)

;;; ----------------------------------------------------------------------------
;;; gtk_image_set_from_stock ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_image_set_from_stock" gtk-image-set-from-stock) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-4-17}
  @argument[image]{a @class{gtk-image} widget}
  @argument[stock-id]{a string with a stock icon name}
  @argument[icon-size]{a stock icon size of type @symbol{gtk-icon-size}}
  @begin{short}
    See the function @fun{gtk-image-new-from-stock} for details.
  @end{short}
  @begin[Warning]{dictionary}
    The function @sym{gtk-image-set-from-stock} has been deprecated since
    version 3.10 and should not be used in newly-written code. Use the function
    @fun{gtk-image-set-from-icon-name} instead.
  @end{dictionary}
  @see-class{gtk-image}
  @see-function{gtk-image-new-from-stock}
  @see-function{gtk-image-set-from-icon-name}"
  (image (g-object gtk-image))
  (stock-id g-string)
  (icon-size gtk-icon-size))

(export 'gtk-image-set-from-stock)

;;; ----------------------------------------------------------------------------
;;; gtk_image_set_from_animation ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_image_set_from_animation" gtk-image-set-from-animation) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-4-17}
  @argument[image]{a @class{gtk-image} object}
  @argument[animation]{the @class{gdk-pixbuf-animation} object}
  @begin{short}
    Causes the image to display the given animation, or display nothing, if you
    set the animation to @code{nil}.
  @end{short}
  @see-class{gtk-image}
  @see-class{gdk-pixbuf-animation}"
  (image (g-object gtk-image))
  (animation (g-object gdk-pixbuf-animation)))

(export 'gtk-image-set-from-animation)

;;; ----------------------------------------------------------------------------
;;; gtk_image_set_from_icon_name ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_image_set_from_icon_name" gtk-image-set-from-icon-name) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-4-17}
  @argument[image]{a @class{gtk-image} widget}
  @argument[icon-name]{a string with an icon name}
  @argument[icon-size]{an icon size of type @symbol{gtk-icon-size}}
  @begin{short}
    See the function @fun{gtk-image-new-from-icon-name} for details.
  @end{short}
  @see-class{gtk-image}
  @see-function{gtk-image-new-from-icon-name}"
  (image (g-object gtk-image))
  (icon-name g-string)
  (icon-size gtk-icon-size))

(export 'gtk-image-set-from-icon-name)

;;; ----------------------------------------------------------------------------
;;; gtk_image_set_from_gicon ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_image_set_from_gicon" gtk-image-set-from-gicon) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-4-17}
  @argument[image]{a @class{gtk-image} widget}
  @argument[icon]{an icon of type @class{g-icon}}
  @argument[icon-size]{an icon size of type @symbol{gtk-icon-size}}
  @begin{short}
    See the funcion @fun{gtk-image-new-from-gicon} for details.
  @end{short}
  @see-class{gtk-image}
  @see-class{g-icon}
  @see-symbol{gtk-icon-size}
  @see-function{gtk-image-new-from-gicon}"
  (image (g-object gtk-image))
  (icon (g-object g-icon))
  (icon-size gtk-icon-size))

(export 'gtk-image-set-from-gicon)

;;; ----------------------------------------------------------------------------
;;; gtk_image_set_from_resource ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_image_set_from_resource" gtk-image-set-from-resource) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-4-17}
  @argument[image]{a @class{gtk-image} widget}
  @argument[resource-path]{a string with a resource path}
  @begin{short}
    See the function @fun{gtk-image-new-from-resource} for details.
  @end{short}
  @see-class{gtk-image}
  @see-function{gtk-image-new-from-resource}"
  (image (g-object gtk-image))
  (resource-path g-string))

(export 'gtk-image-set-from-resource)

;;; ----------------------------------------------------------------------------
;;; gtk_image_set_from_surface ()
;;; ----------------------------------------------------------------------------

#+gtk-3-10
(defcfun ("gtk_image_set_from_surface" gtk-image-set-from-surface) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-4-17}
  @argument[image]{a @class{gtk-image} widget}
  @argument[surface]{a @symbol{cairo-surface-t} instance}
  @begin{short}
    See the function @fun{gtk-image-new-from-surface} for details.
  @end{short}
  @see-class{gtk-image}
  @see-symbol{cairo-surface-t}
  @see-function{gtk-image-new-from-surface}"
  (image (g-object gtk-image))
  (surface (:pointer (:struct cairo-surface-t))))

#+gtk-3-10
(export 'gtk-image-set-from-surface)

;;; ----------------------------------------------------------------------------
;;; gtk_image_clear ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_image_clear" gtk-image-clear) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-4-17}
  @argument[image]{a @class{gtk-image} widget}
  @short{Resets the image to be empty.}
  @see-class{gtk-image}"
  (image (g-object gtk-image)))

(export 'gtk-image-clear)

;;; ----------------------------------------------------------------------------
;;; gtk_image_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-image-new))

(defun gtk-image-new ()
 #+cl-cffi-gtk-documentation
 "@version{2020-4-17}
  @return{A newly created @class{gtk-image} widget.}
  Creates a new image.
  @see-class{gtk-image}"
  (make-instance 'gtk-image))

(export 'gtk-image-new)

;;; --- End of file gtk.image.lisp ---------------------------------------------
