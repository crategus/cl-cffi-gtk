(in-package :gtk)

#+gtk-3-12
(define-g-object-class "GtkActionBar" gtk-action-bar
  (:superclass gtk-bin
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable")
   :type-initializer "gtk_action_bar_get_type")
  ())

#+gtk-3-12
(define-child-property "GtkActionBar"
                       gtk-action-bar-child-pack-type
                       "pack-type" "GtkPackType" t t t)

#+gtk-3-12
(define-child-property "GtkActionBar"
                       gtk-action-bar-child-position
                       "position" "gint" t t t)

#+gtk-3-12
(defcfun ("gtk_action_bar_pack_start" gtk-action-bar-pack-start) :void
  (action-bar (g-object gtk-action-bar))
  (child (g-object gtk-widget)))

#+gtk-3-12
(export 'gtk-action-bar-pack-start)

#+gtk-3-12
(defcfun ("gtk_action_bar_pack_end" gtk-action-bar-pack-end) :void
  (action-bar (g-object gtk-action-bar))
  (child (g-object gtk-widget)))

#+gtk-3-12
(export 'gtk-action-bar-pack-end)

#+gtk-3-12
(defcfun ("gtk_action_bar_get_center_widget" gtk-action-bar-center-widget)
    (g-object gtk-widget)
  (action-bar (g-object gtk-action-bar)))

#+gtk-3-12
(defcfun ("gtk_action_bar_set_center_widget" %gtk-action-bar-set-center-widget) :void
  (action-bar (g-object gtk-action-bar))
  (center-widget (g-object gtk-widget)))

#+gtk-3-12
(defun (setf gtk-action-bar-center-widget) (new-value action-bar)
  (%gtk-action-bar-set-center-widget action-bar new-value))

#+gtk-3-12
(export 'gtk-action-bar-center-widget)
