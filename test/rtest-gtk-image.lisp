(def-suite gtk-image :in gtk-suite)
(in-suite gtk-image)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkImageType

(test gtk-image-type
  ;; Check the type
  (is (g-type-is-enum "GtkImageType"))
  ;; Check the type initializer
  (is (eq (gtype "GtkImageType")
          (gtype (foreign-funcall "gtk_image_type_get_type" g-size))))
  ;; Check the registered name
  (is (eq 'gtk-image-type (registered-enum-type "GtkImageType")))
  ;; Check the names
  (is (equal '("GTK_IMAGE_EMPTY" "GTK_IMAGE_PIXBUF" "GTK_IMAGE_STOCK"
               "GTK_IMAGE_ICON_SET" "GTK_IMAGE_ANIMATION" "GTK_IMAGE_ICON_NAME"
               "GTK_IMAGE_GICON" "GTK_IMAGE_SURFACE")
             (mapcar #'enum-item-name
                     (get-enum-items "GtkImageType"))))
  ;; Check the values
  (is (equal '(0 1 2 3 4 5 6 7)
             (mapcar #'enum-item-value
                     (get-enum-items "GtkImageType"))))
  ;; Check the nick names
  (is (equal '("empty" "pixbuf" "stock" "icon-set" "animation" "icon-name"
               "gicon" "surface")
             (mapcar #'enum-item-nick
                     (get-enum-items "GtkImageType"))))
  ;; Check the enum definition
  (is (equal '(DEFINE-G-ENUM "GtkImageType"
                             GTK-IMAGE-TYPE
                             (:EXPORT T
                              :TYPE-INITIALIZER "gtk_image_type_get_type")
                             (:EMPTY 0)
                             (:PIXBUF 1)
                             (:STOCK 2)
                             (:ICON-SET 3)
                             (:ANIMATION 4)
                             (:ICON-NAME 5)
                             (:GICON 6)
                             (:SURFACE 7))
             (get-g-type-definition "GtkImageType"))))

;;;     GtkImage

(test gtk-image-class
  ;; Type check
  (is (g-type-is-object "GtkImage"))
  ;; Check the registered name
  (is (eq 'gtk-image
          (registered-object-type-by-name "GtkImage")))
  ;; Check the type initializer
  (is (eq (gtype "GtkImage")
          (gtype (foreign-funcall "gtk_image_get_type" g-size))))
  ;; Check the parent
  (is (eq (gtype "GtkMisc") (g-type-parent "GtkImage")))
  ;; Check the children
  (is (equal '()
             (mapcar #'g-type-name (g-type-children "GtkImage"))))
  ;; Check the interfaces
  (is (equal '("AtkImplementorIface" "GtkBuildable")
             (mapcar #'g-type-name (g-type-interfaces "GtkImage"))))
  ;; Check the class properties
  (is (equal '("app-paintable" "can-default" "can-focus" "composite-child"
               "double-buffered" "events" "expand" "file" "focus-on-click"
               "gicon" "halign" "has-default" "has-focus" "has-tooltip"
               "height-request" "hexpand" "hexpand-set" "icon-name" "icon-set"
               "icon-size" "is-focus" "margin" "margin-bottom" "margin-end"
               "margin-left" "margin-right" "margin-start" "margin-top" "name"
               "no-show-all" "opacity" "parent" "pixbuf" "pixbuf-animation"
               "pixel-size" "receives-default" "resource" "scale-factor"
               "sensitive" "stock" "storage-type" "style" "surface"
               "tooltip-markup" "tooltip-text" "use-fallback" "valign" "vexpand"
               "vexpand-set" "visible" "width-request" "window" "xalign" "xpad"
               "yalign" "ypad")
             (sort (mapcar #'g-param-spec-name
                           (g-object-class-list-properties "GtkImage"))
                   #'string-lessp)))
  ;; Check the style properties.
  (is (equal '("cursor-aspect-ratio" "cursor-color" "focus-line-pattern"
               "focus-line-width" "focus-padding" "interior-focus" "link-color"
               "scroll-arrow-hlength" "scroll-arrow-vlength"
               "secondary-cursor-color" "separator-height" "separator-width"
               "text-handle-height" "text-handle-width" "visited-link-color"
               "wide-separators" "window-dragging")
             (mapcar #'g-param-spec-name
                     (gtk-widget-class-list-style-properties "GtkImage"))))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GtkImage" GTK-IMAGE
                       (:SUPERCLASS GTK-MISC :EXPORT T :INTERFACES
                        ("AtkImplementorIface" "GtkBuildable")
                        :TYPE-INITIALIZER "gtk_image_get_type")
                       ((FILE GTK-IMAGE-FILE "file" "gchararray" T T)
                        (GICON GTK-IMAGE-GICON "gicon" "GIcon" T T)
                        (ICON-NAME GTK-IMAGE-ICON-NAME "icon-name" "gchararray"
                         T T)
                        (ICON-SET GTK-IMAGE-ICON-SET "icon-set" "GtkIconSet" T
                         T)
                        (ICON-SIZE GTK-IMAGE-ICON-SIZE "icon-size" "gint" T T)
                        (PIXBUF GTK-IMAGE-PIXBUF "pixbuf" "GdkPixbuf" T T)
                        (PIXBUF-ANIMATION GTK-IMAGE-PIXBUF-ANIMATION
                         "pixbuf-animation" "GdkPixbufAnimation" T T)
                        (PIXEL-SIZE GTK-IMAGE-PIXEL-SIZE "pixel-size" "gint" T
                         T)
                        (RESOURCE GTK-IMAGE-RESOURCE "resource" "gchararray" T
                         T)
                        (STOCK GTK-IMAGE-STOCK "stock" "gchararray" T T)
                        (STORAGE-TYPE GTK-IMAGE-STORAGE-TYPE "storage-type"
                         "GtkImageType" T NIL)
                        (SURFACE GTK-IMAGE-SURFACE "surface" "CairoSurface" T
                         T)
                        (USE-FALLBACK GTK-IMAGE-USE-FALLBACK "use-fallback"
                         "gboolean" T T)))
             (get-g-type-definition "GtkImage"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-image-properties
  (let ((image (make-instance 'gtk-image)))
    (is-false (gtk-image-file image))
    (is-false (gtk-image-gicon image))
    (is-false (gtk-image-icon-name image))
    (is-false (gtk-image-icon-set image))
    (is (= 4 (gtk-image-icon-size image)))
    (is-false (gtk-image-pixbuf image))
    (is-false (gtk-image-pixbuf-animation image))
    (is (= -1 (gtk-image-pixel-size image)))
    (is-false (gtk-image-resource image))
    (is-false (gtk-image-stock image))
    (is (eq :empty (gtk-image-storage-type image)))
    ;; at this point surface is a null-pointer, this causes an error
    (is-false (gtk-image-surface image))
    (is-false (gtk-image-use-fallback image))))

(test gtk-image-icon-size
  ;; The accessor gtk-image-icon-size is implemented with an integer.
  ;; This integer can be converted to a gtk-icon-size keyword
  (let ((image (gtk-image-new)))
    (is (eq :button
            (foreign-enum-keyword 'gtk-icon-size (gtk-image-icon-size image))))
    (is (= (foreign-enum-value 'gtk-icon-size :button)
           (gtk-image-icon-size image)))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_image_get_icon_set

(test gtk-image-get-icon-set
  (let ((image (gtk-image-new-from-icon-set
                   (gtk-icon-factory-lookup-default "gtk-ok") :dialog)))
    (is (typep image 'gtk-image))
    (is (typep (gtk-image-get-icon-set image) 'gtk-icon-set))
    (multiple-value-bind (icon-set icon-size)
        (gtk-image-get-icon-set image)
      (is (typep icon-set 'gtk-icon-set))
      (is (eq :dialog icon-size)))))

;;;     gtk_image_get_stock

(test gtk-image-get-stock
  (let ((image (gtk-image-new-from-stock "gtk-ok" :dialog)))
    (is (typep image 'gtk-image))
    (is (string= "gtk-ok" (gtk-image-get-stock image)))
    (multiple-value-bind (icon-set icon-size)
        (gtk-image-get-stock image)
      (is (string= "gtk-ok" icon-set))
      (is (eq :dialog icon-size)))))

;;;     gtk_image_get_animation

(test gtk-image-get-animation
  (let* ((animation (gdk-pixbuf-animation-new-from-file "floppybuddy.gif"))
         (image (gtk-image-new-from-animation animation)))
    (is (typep image 'gtk-image))
    (is (typep (gtk-image-get-animation image) 'gdk-pixbuf-animation))))

;;;     gtk_image_get_icon_name

(test gtk-image-get-icon-name
  (let ((image (gtk-image-new-from-icon-name "gtk-ok" :dialog)))
    (is (typep image 'gtk-image))
    (is (string= "gtk-ok" (gtk-image-get-icon-name image)))
    (multiple-value-bind (icon-set icon-size)
        (gtk-image-get-icon-name image)
      (is (string= "gtk-ok" icon-set))
      (is (eq :dialog icon-size)))))

;;;     gtk_image_get_gicon

(test gtk-image-get-gicon
  (let* ((icon (g-themed-icon-new-from-names "gtk-ok"))
         (image (gtk-image-new-from-gicon icon :dialog)))
    (is (typep image 'gtk-image))
    (is (typep (gtk-image-gicon image) 'g-themed-icon))
    (multiple-value-bind (icon-set icon-size)
        (gtk-image-get-gicon image)
      (is (typep icon-set 'g-themed-icon))
      (is (eq :dialog icon-size)))))

;;;     gtk_image_new_from_file

(test gtk-image-new-from-file
  (let ((image (gtk-image-new-from-file "gtk-logo-24.png")))
    (is (typep image 'gtk-image))
    (is (string= "gtk-logo-24.png" (gtk-image-file image)))
    (is-false (gtk-image-gicon image))
    (is-false (gtk-image-icon-name image))
    (is-false (gtk-image-icon-set image))
    (is (= 0 (gtk-image-icon-size image)))
    (is (typep (gtk-image-pixbuf image) 'gdk-pixbuf))
    (is-false (gtk-image-pixbuf-animation image))
    (is (= -1 (gtk-image-pixel-size image)))
    (is-false (gtk-image-resource image))
    (is-false (gtk-image-stock image))
    (is (eq :pixbuf (gtk-image-storage-type image)))
    ;; at this point surface is a null-pointer, this causes an error
    (is-false (gtk-image-surface image))
    (is-false (gtk-image-use-fallback image))))

;;;     gtk_image_new_from_icon_set

(test gtk-image-new-from-icon-set
  (let ((image (gtk-image-new-from-icon-set
                   (gtk-icon-factory-lookup-default "gtk-ok") :dialog)))
    (is (typep image 'gtk-image))
    (is-false (gtk-image-file image))
    (is-false (gtk-image-gicon image))
    (is-false (gtk-image-icon-name image))
    (is (typep (gtk-image-icon-set image) 'gtk-icon-set))
    (is (= 6 (gtk-image-icon-size image)))
    (is-false (gtk-image-pixbuf image))
    (is-false (gtk-image-pixbuf-animation image))
    (is (= -1 (gtk-image-pixel-size image)))
    (is-false (gtk-image-resource image))
    (is-false (gtk-image-stock image))
    (is (eq :icon-set (gtk-image-storage-type image)))
    ;; at this point surface is a null-pointer, this causes an error
    (is-false (gtk-image-surface image))
    (is-false (gtk-image-use-fallback image))))

;;;     gtk_image_new_from_pixbuf

(test gtk-image-new-from-pixbuf
  (let* ((pixbuf (gdk-pixbuf-new-from-file "gtk-logo-24.png"))
         (image (gtk-image-new-from-pixbuf pixbuf)))
    (is (typep image 'gtk-image))
    (is-false (gtk-image-file image))
    (is-false (gtk-image-gicon image))
    (is-false (gtk-image-icon-name image))
    (is-false (gtk-image-icon-set image))
    (is (= 0 (gtk-image-icon-size image)))
    (is (typep (gtk-image-pixbuf image) 'gdk-pixbuf))
    (is-false (gtk-image-pixbuf-animation image))
    (is (= -1 (gtk-image-pixel-size image)))
    (is-false (gtk-image-resource image))
    (is-false (gtk-image-stock image))
    (is (eq :pixbuf (gtk-image-storage-type image)))
    ;; at this point surface is a null-pointer, this causes an error
    (is-false (gtk-image-surface image))
    (is-false (gtk-image-use-fallback image))))

;;;     gtk_image_new_from_stock

(test gtk-image-new-from-stock
  (let ((image (gtk-image-new-from-stock "gtk-ok" :dialog)))
    (is (typep image 'gtk-image))
    (is-false (gtk-image-file image))
    (is-false (gtk-image-gicon image))
    (is-false (gtk-image-icon-name image))
    (is-false (gtk-image-icon-set image))
    (is (= 6 (gtk-image-icon-size image)))
    (is-false (gtk-image-pixbuf image))
    (is-false (gtk-image-pixbuf-animation image))
    (is (= -1 (gtk-image-pixel-size image)))
    (is-false (gtk-image-resource image))
    (is (string= "gtk-ok" (gtk-image-stock image)))
    (is (eq :stock (gtk-image-storage-type image)))
    ;; at this point surface is a null-pointer, this causes an error
    (is-false (gtk-image-surface image))
    (is-false (gtk-image-use-fallback image))))

;;;     gtk_image_new_from_animation

(test gtk-image-new-from-animation
  (let* ((animation (gdk-pixbuf-animation-new-from-file "floppybuddy.gif"))
         (image (gtk-image-new-from-animation animation)))
    (is (typep image 'gtk-image))
    (is-false (gtk-image-file image))
    (is-false (gtk-image-gicon image))
    (is-false (gtk-image-icon-name image))
    (is-false (gtk-image-icon-set image))
    (is (= 0 (gtk-image-icon-size image)))
    (is-false (gtk-image-pixbuf image))
    (is (typep (gtk-image-pixbuf-animation image) 'gdk-pixbuf-animation))
    (is (= -1 (gtk-image-pixel-size image)))
    (is-false (gtk-image-resource image))
    (is-false (gtk-image-stock image))
    (is (eq :animation (gtk-image-storage-type image)))
    ;; at this point surface is a null-pointer, this causes an error
    (is-false (gtk-image-surface image))
    (is-false (gtk-image-use-fallback image))))

;;;     gtk_image_new_from_icon_name

(test gtk-image-new-from-icon-name
  (let ((image (gtk-image-new-from-icon-name "gtk-ok" :dialog)))
    (is (typep image 'gtk-image))
    (is-false (gtk-image-file image))
    (is-false (gtk-image-gicon image))
    (is (string= "gtk-ok" (gtk-image-icon-name image)))
    (is-false (gtk-image-icon-set image))
    (is (= 6 (gtk-image-icon-size image)))
    (is-false (gtk-image-pixbuf image))
    (is-false (gtk-image-pixbuf-animation image))
    (is (= -1 (gtk-image-pixel-size image)))
    (is-false (gtk-image-resource image))
    (is-false (gtk-image-stock image))
    (is (eq :icon-name (gtk-image-storage-type image)))
    ;; at this point surface is a null-pointer, this causes an error
    (is-false (gtk-image-surface image))
    (is-false (gtk-image-use-fallback image))))

;;;     gtk_image_new_from_gicon

(test gtk-image-new-from-gicon
  (let* ((icon (g-themed-icon-new-from-names "gtk-ok"))
         (image (gtk-image-new-from-gicon icon :dialog)))
    (is (typep image 'gtk-image))
    (is-false (gtk-image-file image))
    (is (typep (gtk-image-gicon image) 'g-themed-icon))
    (is-false (gtk-image-icon-name image))
    (is-false (gtk-image-icon-set image))
    (is (= 6 (gtk-image-icon-size image)))
    (is-false (gtk-image-pixbuf image))
    (is-false (gtk-image-pixbuf-animation image))
    (is (= -1 (gtk-image-pixel-size image)))
    (is-false (gtk-image-resource image))
    (is-false (gtk-image-stock image))
    (is (eq :gicon (gtk-image-storage-type image)))
    ;; at this point surface is a null-pointer, this causes an error
    (is-false (gtk-image-surface image))
    (is-false (gtk-image-use-fallback image))))

;;;     gtk_image_new_from_resource

(test gtk-image-new-from-resource
  (let ((resource (g-resource-load "rtest-gio-resource.gresource")))
    ;; Register the resources
    (is-false (g-resources-register resource))
    (let ((image (gtk-image-new-from-resource "/com/crategus/test/ducky.png")))
      (is (typep image 'gtk-image))
      (is-false (gtk-image-file image))
      (is-false (gtk-image-gicon image))
      (is-false (gtk-image-icon-name image))
      (is-false (gtk-image-icon-set image))
      (is (= 0 (gtk-image-icon-size image)))
      (is (typep (gtk-image-pixbuf image) 'gdk-pixbuf))
      (is-false (gtk-image-pixbuf-animation image))
      (is (= -1 (gtk-image-pixel-size image)))
      (is (string= "/com/crategus/test/ducky.png" (gtk-image-resource image)))
      (is-false (gtk-image-stock image))
      (is (eq :pixbuf (gtk-image-storage-type image)))
      ;; at this point surface is a null-pointer, this causes an error
      (is-false (gtk-image-surface image))
      (is-false (gtk-image-use-fallback image)))
      ;; Unregister the resources
      (is-false (g-resources-unregister resource))))

;;;     gtk_image_new_from_surface ()

(test gtk-image-new-from-surface
  (let* ((theme (gtk-icon-theme-default))
         (surface (gtk-icon-theme-load-surface theme "gtk-ok"
                                                     48 1 nil :use-builtin))
         (image (gtk-image-new-from-surface surface)))
    (is (typep image 'gtk-image))
    (is-false (gtk-image-file image))
    (is-false (gtk-image-gicon image))
    (is-false (gtk-image-icon-name image))
    (is-false (gtk-image-icon-set image))
    (is (= 0 (gtk-image-icon-size image)))
    (is-false (gtk-image-pixbuf image))
    (is-false (gtk-image-pixbuf-animation image))
    (is (= -1 (gtk-image-pixel-size image)))
    (is-false (gtk-image-resource image))
    (is-false (gtk-image-stock image))
    (is (eq :surface (gtk-image-storage-type image)))
    ;; we have a valid Cairo surface
    (is (typep (gtk-image-surface image) 'cairo-surface))
    (is-false (gtk-image-use-fallback image))))

;;;     gtk_image_set_from_file

(test gtk-image-set-from-file
  (let ((image (make-instance 'gtk-image)))
    ;; Set image from file
    (is-false (gtk-image-set-from-file image "gtk-logo-24.png"))
    ;; Check the properties
    (is (typep image 'gtk-image))
    (is (string= "gtk-logo-24.png" (gtk-image-file image)))
    (is-false (gtk-image-gicon image))
    (is-false (gtk-image-icon-name image))
    (is-false (gtk-image-icon-set image))
    (is (= 0 (gtk-image-icon-size image)))
    (is (typep (gtk-image-pixbuf image) 'gdk-pixbuf))
    (is-false (gtk-image-pixbuf-animation image))
    (is (= -1 (gtk-image-pixel-size image)))
    (is-false (gtk-image-resource image))
    (is-false (gtk-image-stock image))
    (is (eq :pixbuf (gtk-image-storage-type image)))
    ;; at this point surface is a null-pointer, this causes an error
    (is-false (gtk-image-surface image))
    (is-false (gtk-image-use-fallback image))))

;;;     gtk_image_set_from_icon_set

(test gtk-image-set-from-icon-set
  (let ((image (make-instance 'gtk-image)))
    ;; Set image from icon set
    (is-false (gtk-image-set-from-icon-set image
                  (gtk-icon-factory-lookup-default "gtk-ok") :dialog))
    ;; Check the properties
    (is (typep image 'gtk-image))
    (is-false (gtk-image-file image))
    (is-false (gtk-image-gicon image))
    (is-false (gtk-image-icon-name image))
    (is (typep (gtk-image-icon-set image) 'gtk-icon-set))
    (is (= 6 (gtk-image-icon-size image)))
    (is-false (gtk-image-pixbuf image))
    (is-false (gtk-image-pixbuf-animation image))
    (is (= -1 (gtk-image-pixel-size image)))
    (is-false (gtk-image-resource image))
    (is-false (gtk-image-stock image))
    (is (eq :icon-set (gtk-image-storage-type image)))
    ;; at this point surface is a null-pointer, this causes an error
    (is-false (gtk-image-surface image))
    (is-false (gtk-image-use-fallback image))))

;;;     gtk_image_set_from_pixbuf

(test gtk-image-set-from-pixbuf
  (let ((pixbuf (gdk-pixbuf-new-from-file "gtk-logo-24.png"))
        (image (make-instance 'gtk-image)))
    ;; Set image from pixbuf
    (is-false (gtk-image-set-from-pixbuf image pixbuf))
    ;; Check the properties
    (is (typep image 'gtk-image))
    (is-false (gtk-image-file image))
    (is-false (gtk-image-gicon image))
    (is-false (gtk-image-icon-name image))
    (is-false (gtk-image-icon-set image))
    (is (= 0 (gtk-image-icon-size image)))
    (is (typep (gtk-image-pixbuf image) 'gdk-pixbuf))
    (is-false (gtk-image-pixbuf-animation image))
    (is (= -1 (gtk-image-pixel-size image)))
    (is-false (gtk-image-resource image))
    (is-false (gtk-image-stock image))
    (is (eq :pixbuf (gtk-image-storage-type image)))
    ;; at this point surface is a null-pointer, this causes an error
    (is-false (gtk-image-surface image))
    (is-false (gtk-image-use-fallback image))))

;;;     gtk_image_set_from_stock

(test gtk-image-set-from-stock
  (let ((image (make-instance 'gtk-image)))
    ;; Set image from stock
    (is-false (gtk-image-set-from-stock image "gtk-ok" :dialog))
    ;; Check the properties
    (is (typep image 'gtk-image))
    (is-false (gtk-image-file image))
    (is-false (gtk-image-gicon image))
    (is-false (gtk-image-icon-name image))
    (is-false (gtk-image-icon-set image))
    (is (= 6 (gtk-image-icon-size image)))
    (is-false (gtk-image-pixbuf image))
    (is-false (gtk-image-pixbuf-animation image))
    (is (= -1 (gtk-image-pixel-size image)))
    (is-false (gtk-image-resource image))
    (is (string= "gtk-ok" (gtk-image-stock image)))
    (is (eq :stock (gtk-image-storage-type image)))
    ;; at this point surface is a null-pointer, this causes an error
    (is-false (gtk-image-surface image))
    (is-false (gtk-image-use-fallback image))))

;;;     gtk_image_set_from_animation

(test gtk-image-set-from-animation
  (let ((animation (gdk-pixbuf-animation-new-from-file "floppybuddy.gif"))
        (image (make-instance 'gtk-image)))
    ;; Set image from animation
    (is-false (gtk-image-set-from-animation image animation))
    ;; Check the properties
    (is (typep image 'gtk-image))
    (is-false (gtk-image-file image))
    (is-false (gtk-image-gicon image))
    (is-false (gtk-image-icon-name image))
    (is-false (gtk-image-icon-set image))
    (is (= 0 (gtk-image-icon-size image)))
    (is-false (gtk-image-pixbuf image))
    (is (typep (gtk-image-pixbuf-animation image) 'gdk-pixbuf-animation))
    (is (= -1 (gtk-image-pixel-size image)))
    (is-false (gtk-image-resource image))
    (is-false (gtk-image-stock image))
    (is (eq :animation (gtk-image-storage-type image)))
    ;; at this point surface is a null-pointer, this causes an error
    (is-false (gtk-image-surface image))
    (is-false (gtk-image-use-fallback image))))

;;;     gtk_image_set_from_icon_name

(test gtk-image-set-from-icon-name
  (let ((image (make-instance 'gtk-image)))
    ;; Set image from icon name
    (is-false (gtk-image-set-from-icon-name image "gtk-ok" :dialog))
    ;; Check the properties
    (is (typep image 'gtk-image))
    (is-false (gtk-image-file image))
    (is-false (gtk-image-gicon image))
    (is (string= "gtk-ok" (gtk-image-icon-name image)))
    (is-false (gtk-image-icon-set image))
    (is (= 6 (gtk-image-icon-size image)))
    (is-false (gtk-image-pixbuf image))
    (is-false (gtk-image-pixbuf-animation image))
    (is (= -1 (gtk-image-pixel-size image)))
    (is-false (gtk-image-resource image))
    (is-false (gtk-image-stock image))
    (is (eq :icon-name (gtk-image-storage-type image)))
    ;; at this point surface is a null-pointer, this causes an error
    (is-false (gtk-image-surface image))
    (is-false (gtk-image-use-fallback image))))

;;;     gtk_image_set_from_gicon

(test gtk-image-set-from-gicon
  (let ((icon (g-themed-icon-new-from-names "gtk-ok"))
        (image (make-instance 'gtk-image)))
    ;; Set image from gicon
    (is-false (gtk-image-set-from-gicon image icon :dialog))
    ;; Check the properties
    (is (typep image 'gtk-image))
    (is-false (gtk-image-file image))
    (is (typep (gtk-image-gicon image) 'g-themed-icon))
    (is-false (gtk-image-icon-name image))
    (is-false (gtk-image-icon-set image))
    (is (= 6 (gtk-image-icon-size image)))
    (is-false (gtk-image-pixbuf image))
    (is-false (gtk-image-pixbuf-animation image))
    (is (= -1 (gtk-image-pixel-size image)))
    (is-false (gtk-image-resource image))
    (is-false (gtk-image-stock image))
    (is (eq :gicon (gtk-image-storage-type image)))
    ;; at this point surface is a null-pointer, this causes an error
    (is-false (gtk-image-surface image))
    (is-false (gtk-image-use-fallback image))))

;;;     gtk_image_set_from_resource

(test gtk-image-set-from-resource
  (let ((resource (g-resource-load "rtest-gio-resource.gresource"))
        (image (gtk-image-new)))
    ;; Register the resources
    (is-false (g-resources-register resource))
    ;; Set image from resource
    (is-false (gtk-image-set-from-resource image "/com/crategus/test/ducky.png"))
    ;; Check the properties
    (is (typep image 'gtk-image))
    (is-false (gtk-image-file image))
    (is-false (gtk-image-gicon image))
    (is-false (gtk-image-icon-name image))
    (is-false (gtk-image-icon-set image))
    (is (= 0 (gtk-image-icon-size image)))
    (is (typep (gtk-image-pixbuf image) 'gdk-pixbuf))
    (is-false (gtk-image-pixbuf-animation image))
    (is (= -1 (gtk-image-pixel-size image)))
    (is (string= "/com/crategus/test/ducky.png" (gtk-image-resource image)))
    (is-false (gtk-image-stock image))
    (is (eq :pixbuf (gtk-image-storage-type image)))
    ;; at this point surface is a null-pointer, this causes an error
    (is-false (gtk-image-surface image))
    (is-false (gtk-image-use-fallback image))
    ;; Unregister the resources
    (is-false (g-resources-unregister resource))))

;;;     gtk_image_set_from_surface ()

(test gtk-image-set-from-surface
  (let* ((theme (gtk-icon-theme-default))
         (surface (gtk-icon-theme-load-surface theme "gtk-ok"
                                                     48 1 nil :use-builtin))
         (image (make-instance 'gtk-image)))
    ;; Set image from surface
    (is-false (gtk-image-set-from-surface image surface))
    ;; Check the properties
    (is (typep image 'gtk-image))
    (is-false (gtk-image-file image))
    (is-false (gtk-image-gicon image))
    (is-false (gtk-image-icon-name image))
    (is-false (gtk-image-icon-set image))
    (is (= 0 (gtk-image-icon-size image)))
    (is-false (gtk-image-pixbuf image))
    (is-false (gtk-image-pixbuf-animation image))
    (is (= -1 (gtk-image-pixel-size image)))
    (is-false (gtk-image-resource image))
    (is-false (gtk-image-stock image))
    (is (eq :surface (gtk-image-storage-type image)))
    ;; we have a valid Cairo surface
    (is (typep (gtk-image-surface image) 'cairo-surface))
    (is-false (gtk-image-use-fallback image))))

;;;     gtk_image_clear

(test gtk-image-clear
  (let ((image (gtk-image-new-from-icon-name "gtk-ok" 4)))
    ;; Create image from icon name
    (is (typep image 'gtk-image))
    (is-false (gtk-image-file image))
    (is-false (gtk-image-gicon image))
    (is (string= "gtk-ok" (gtk-image-icon-name image)))
    (is-false (gtk-image-icon-set image))
    (is (= 4 (gtk-image-icon-size image)))
    (is-false (gtk-image-pixbuf image))
    (is-false (gtk-image-pixbuf-animation image))
    (is (= -1 (gtk-image-pixel-size image)))
    (is-false (gtk-image-resource image))
    (is-false (gtk-image-stock image))
    (is (eq :icon-name (gtk-image-storage-type image)))
    ;; at this point surface is a null-pointer, this causes an error
    (is-false (gtk-image-surface image))
    (is-false (gtk-image-use-fallback image))
    ;; Clear the image
    (is-false (gtk-image-clear image))
    (is (typep image 'gtk-image))
    (is-false (gtk-image-file image))
    (is-false (gtk-image-gicon image))
    (is-false (gtk-image-icon-name image))
    (is-false (gtk-image-icon-set image))
    (is (= 0 (gtk-image-icon-size image)))
    (is-false (gtk-image-pixbuf image))
    (is-false (gtk-image-pixbuf-animation image))
    (is (= -1 (gtk-image-pixel-size image)))
    (is-false (gtk-image-resource image))
    (is-false (gtk-image-stock image))
    (is (eq :empty (gtk-image-storage-type image)))
    ;; at this point surface is a null-pointer, this causes an error
    (is-false (gtk-image-surface image))
    (is-false (gtk-image-use-fallback image))))

;;;     gtk_image_new

(test gtk-image-new
  (is (typep (gtk-image-new) 'gtk-image)))

;;; 2021-8-16
