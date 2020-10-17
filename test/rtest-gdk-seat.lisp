(def-suite gdk-seat :in gdk-suite)
(in-suite gdk-seat)

;;;     GdkSeatCapabilities

(test gdk-seat-capabilities
  ;; Type checks
  (is-false (g-type-is-object "GdkSeatCapabilities"))
  (is-false (g-type-is-abstract "GdkSeatCapabilities"))
  (is-true  (g-type-is-derived "GdkSeatCapabilities"))
  (is-false (g-type-is-fundamental "GdkSeatCapabilities"))
  (is-true  (g-type-is-value-type "GdkSeatCapabilities"))
  (is-true  (g-type-has-value-table "GdkSeatCapabilities"))
  (is-true  (g-type-is-classed "GdkSeatCapabilities"))
  (is-false (g-type-is-instantiatable "GdkSeatCapabilities"))
  (is-true  (g-type-is-derivable "GdkSeatCapabilities"))
  (is-false (g-type-is-deep-derivable "GdkSeatCapabilities"))
  (is-false (g-type-is-interface "GdkSeatCapabilities"))

  ;; Check the registered name
  ;; no registered name

  ;; Check infos about the C class implementation
  (let ((class (g-type-class-ref (gtype "GdkSeatCapabilities"))))
    (is (equal (gtype "GdkSeatCapabilities") (g-type-from-class class)))
    (is (equal (gtype "GdkSeatCapabilities") (g-object-class-type class)))
    (is (equal "GdkSeatCapabilities" (g-object-class-name class)))
    (is (equal (gtype "GdkSeatCapabilities")
               (g-type-from-class  (g-type-class-peek "GdkSeatCapabilities"))))
    (is (equal (gtype "GdkSeatCapabilities")
               (g-type-from-class  (g-type-class-peek-static "GdkSeatCapabilities"))))
    (g-type-class-unref class))

  ;; Check infos about the Lisp class implementation
  ;; no Lisp class implementation

  ;; Check some more GType information
  (is (equal (gtype "GFlags") (g-type-parent "GdkSeatCapabilities")))
  (is (= 2 (g-type-depth "GdkSeatCapabilities")))
  (is (equal (gtype "GdkSeatCapabilities")
             (g-type-next-base "GdkSeatCapabilities" "GFlags")))
  (is-false (g-type-is-a "GdkSeatCapabilities" "GtkWidget"))
  (is-true  (g-type-is-a "GdkSeatCapabilities" "GFlags"))
  (is-false (g-type-is-a "GdkSeatCapabilities" "gboolean"))
  (is-false (g-type-is-a "GdkSeatCapabilities" "GtkWindow"))

  ;; Check the children
  ;; no children
  ;; Check the interfaces
  ;; no interfaces

  ;; Query infos about the class
  (with-foreign-object (query '(:struct g-type-query))
    (g-type-query "GdkSeatCapabilities" query)
    (is (equal (gtype "GdkSeatCapabilities")
               (foreign-slot-value query '(:struct g-type-query) :type)))
    (is (equal "GdkSeatCapabilities"
               (foreign-slot-value query '(:struct g-type-query) :type-name)))
    (is (= 24  (foreign-slot-value query '(:struct g-type-query) :class-size)))
    (is (=  0  (foreign-slot-value query '(:struct g-type-query) :instance-size))))

  ;; Get the names of the class properties.
  ;; no class properties for an Enum

  ;; Get the names of the style properties.
  ;; no style properties for an Enum

  ;; Get the names to the child properties
  ;; no child properties for an Enum

  ;; Get the class definition
  (is (equal '(DEFINE-G-FLAGS "GdkSeatCapabilities"
    GDK-SEAT-CAPABILITIES
    (:EXPORT T :TYPE-INITIALIZER "gdk_seat_capabilities_get_type")
  (:NONE 0)
  (:POINTER 1)
  (:TOUCH 2)
  (:TABLET-STYLUS 4)
  (:KEYBOARD 8)
  (:ALL-POINTING 7)
  (:ALL 15))
             (get-g-type-definition "GdkSeatCapabilities"))))

(test gdk-seat-capabilities-value
  (is (=  0 (foreign-enum-value 'gdk-seat-capabilities :none)))
  (is (=  1 (foreign-enum-value 'gdk-seat-capabilities :pointer)))
  (is (=  2 (foreign-enum-value 'gdk-seat-capabilities :touch)))
  (is (=  4 (foreign-enum-value 'gdk-seat-capabilities :tablet-stylus)))
  (is (=  8 (foreign-enum-value 'gdk-seat-capabilities :keyboard)))
  (is (=  7 (foreign-enum-value 'gdk-seat-capabilities :all-pointing)))
  (is (= 15 (foreign-enum-value 'gdk-seat-capabilities :all))))

(test gdk-seat-capabilities-keyword
  (is (eq :none (foreign-enum-keyword 'gdk-seat-capabilities 0)))
  (is (eq :pointer (foreign-enum-keyword 'gdk-seat-capabilities 1)))
  (is (eq :touch (foreign-enum-keyword 'gdk-seat-capabilities 2)))
  (is (eq :tablet-stylus (foreign-enum-keyword 'gdk-seat-capabilities 4)))
  (is (eq :keyboard (foreign-enum-keyword 'gdk-seat-capabilities 8)))
  (is (eq :all-pointing (foreign-enum-keyword 'gdk-seat-capabilities 7)))
  (is (eq :all (foreign-enum-keyword 'gdk-seat-capabilities 15))))

;;;     GdkSeat

(test gdk-seat-class
  ;; Type checks
  (is-true  (g-type-is-object "GdkSeat"))
  ;; GdkSeat is abstract, we have the child class GdkSeatDefault
  (is-true  (g-type-is-abstract "GdkSeat"))
  (is-true  (g-type-is-derived "GdkSeat"))
  (is-false (g-type-is-fundamental "GdkSeat"))
  (is-true  (g-type-is-value-type "GdkSeat"))
  (is-true  (g-type-has-value-table "GdkSeat"))
  (is-true  (g-type-is-classed "GdkSeat"))
  (is-true  (g-type-is-instantiatable "GdkSeat"))
  (is-true  (g-type-is-derivable "GdkSeat"))
  (is-true  (g-type-is-deep-derivable "GdkSeat"))
  (is-false (g-type-is-interface "GdkSeat"))

  ;; Check the registered name
  (is (eq 'gdk-seat
          (registered-object-type-by-name "GdkSeat")))

  ;; Check infos about the C class implementation
  (let ((class (g-type-class-ref (gtype "GdkSeat"))))
    (is (equal (gtype "GdkSeat") (g-type-from-class class)))
    (is (equal (gtype "GdkSeat") (g-object-class-type class)))
    (is (equal "GdkSeat" (g-object-class-name class)))
    (is (equal (gtype "GdkSeat")
               (g-type-from-class  (g-type-class-peek "GdkSeat"))))
    (is (equal (gtype "GdkSeat")
               (g-type-from-class (g-type-class-peek-static "GdkSeat"))))
    (g-type-class-unref class))

  ;; Check infos about the Lisp class implementation
  (let ((class (find-class 'gdk-seat)))
    ;; Check the class name and type of the class
    (is (eq 'gdk-seat (class-name class)))
    (is (eq 'gobject-class (type-of class)))
    (is (eq (find-class 'gobject-class) (class-of class)))
    ;; Properties of the metaclass gobject-class
    (is (equal "GdkSeat" (gobject-class-g-type-name class)))
    (is (equal "GdkSeat" (gobject-class-direct-g-type-name class)))
    (is (equal "gdk_seat_get_type"
               (gobject-class-g-type-initializer class)))
    (is-false (gobject-class-interface-p class)))

  ;; Check some more GType information
  (is (equal (gtype "GObject") (g-type-parent "GdkSeat")))
  (is (= 2 (g-type-depth "GdkSeat")))
  (is (equal (gtype "GdkSeat")
             (g-type-next-base "GdkSeat" "GObject")))
  (is-true  (g-type-is-a "GdkSeat" "GObject"))
  (is-false (g-type-is-a "GdkSeat" "GtkWidget"))
  (is-false (g-type-is-a "GdkSeat" "gboolean"))

  ;; Check the children
  (is (equal '("GdkSeatDefault")
             (mapcar #'gtype-name (g-type-children "GdkSeat"))))

  ;; Check the interfaces
  (is (equal '()
             (mapcar #'gtype-name (g-type-interfaces "GdkSeat"))))

  ;; Query infos about the class
  (with-foreign-object (query '(:struct g-type-query))
    (g-type-query "GdkSeat" query)
    (is (equal (gtype "GdkSeat")
               (foreign-slot-value query '(:struct g-type-query) :type)))
    (is (equal "GdkSeat"
               (foreign-slot-value query '(:struct g-type-query) :type-name)))
    (is (= 208 (foreign-slot-value query '(:struct g-type-query) :class-size)))
    (is (=  24 (foreign-slot-value query '(:struct g-type-query) :instance-size))))

  ;; Get the names of the class properties.
  (is (equal '("display")
             (mapcar #'g-param-spec-name
                     (g-object-class-list-properties "GdkSeat"))))

  ;; No style properties
  ;; No child properties

  ;; Get the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GdkSeat" GDK-SEAT
                       (:SUPERCLASS G-OBJECT :EXPORT T :INTERFACES NIL
                        :TYPE-INITIALIZER "gdk_seat_get_type")
                       ((DISPLAY GDK-SEAT-DISPLAY "display" "GdkDisplay" T
                         NIL)))
             (get-g-type-definition "GdkSeat"))))

;;;     gdk-seat-display                               Accessor

(test gdk-seat-display
  (let ((seat (gdk-display-default-seat (gdk-display-default))))
    (is (eq 'gdk-display
            (type-of (gdk-seat-display seat))))))

;;;     gdk_seat_grab
;;;     gdk_seat_ungrab

;;;     gdk_seat_get_capabilities

(test gdk-seat-get-capabilities
  (let ((seat (gdk-display-default-seat (gdk-display-default))))
    (is (equal '(:POINTER :KEYBOARD)
               (gdk-seat-get-capabilities seat)))))

;;;     gdk_seat_get_pointer

(test gdk-seat-pointer
  (let ((seat (gdk-display-default-seat (gdk-display-default))))
    (is (eq 'GDK-X11-DEVICE-XI2
            (type-of (gdk-seat-pointer seat))))))

;;;     gdk_seat_get_keyboard

(test gdk-seat-get-keyboard
  (let ((seat (gdk-display-default-seat (gdk-display-default))))
    (is (eq 'GDK-X11-DEVICE-XI2
            (type-of (gdk-seat-get-keyboard seat))))))

;;;     gdk_seat_get_slaves

(test gdk-seat-get-slaves
  (let ((seat (gdk-display-default-seat (gdk-display-default))))
    (is-true (stringp (first (mapcar #'gdk-device-name (gdk-seat-get-slaves seat :pointer)))))
    (is (equal '() (mapcar #'gdk-device-name (gdk-seat-get-slaves seat :touch))))
    (is (equal '() (mapcar #'gdk-device-name (gdk-seat-get-slaves seat :tablet-stylus))))
    (is-true (stringp (first (mapcar #'gdk-device-name (gdk-seat-get-slaves seat :keyboard)))))
    (is-true (stringp (first (mapcar #'gdk-device-name (gdk-seat-get-slaves seat :all-pointing)))))
    (is-true (stringp (first (mapcar #'gdk-device-name (gdk-seat-get-slaves seat :all)))))
    (is-true (stringp (first (mapcar #'gdk-device-name (gdk-seat-get-slaves seat '(:pointer :keyboard))))))))

