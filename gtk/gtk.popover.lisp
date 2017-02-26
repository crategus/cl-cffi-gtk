(in-package :gtk)

#+gtk-3-12
(define-g-enum "GtkPopoverConstraint" gtk-popover-constraint
  (:export t
   :type-initializer "gtk_popover_constraint_get_type")
  (:none 0)
  (:window 1))

#+gtk-3-12
(eval-when (:compile-toplevel :load-toplevel :execute)
  (register-object-type "GtkPopover" 'gtk-popover))

#+gtk-3-12
(define-g-object-class "GtkPopover" gtk-popover
  (:superclass gtk-bin
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable")
   :type-initializer "gtk_popover_get_type")
    ((constrain-to
      gtk-popover-constrain-to
      "constrain-to" "GtkPopoverConstraint" t t)
     (modal
      gtk-popover-modal
      "modal" "gboolean" t t)
     (pointing-to
      gtk-popover-pointing-to
      "pointing-to" "GdkRectangle" t t)
     (position
      gtk-popover-position
      "position" "GtkPositionType" t t)
     (relative-to
      gtk-popover-relative-to
      "relative-to" "GtkWidget" t t)
     (transitions-enabled
      gtk-popover-transitions-enabled
      "transitions-enabled" "gboolean" t t)))

#+gtk-3-12
(defun gtk-popover-new (relative-to)
  (make-instance 'gtk-popover
                 :relative-to relative-to))

#+gtk-3-12
(export 'gtk-popover-new)

#+gtk-3-12
(defcfun ("gtk_popover_bind_model" gtk-popover-bind-model) :void
  (popover (g-object gtk-popover))
  (model (g-object g-menu-model))
  (action-namespace :string))

#+gtk-3-12
(export 'gtk-popover-bind-model)

#+gtk-3-12
(defcfun ("gtk_popover_new_from_model" gtk-popover-new-from-model)
    (g-object gtk-popover)
  (relative-to (g-object gtk-widget))
  (model (g-object g-menu-model)))

#+gtk-3-12
(export 'gtk-popover-new-from-model)

#+gtk-3-12
(defcfun ("gtk_popover_popup" gtk-popover-popup) :void
  (popover (g-object gtk-popover)))

#+gtk-3-12
(export 'gtk-popover-popup)

#+gtk-3-12
(defcfun ("gtk_popover_popdown" gtk-popover-popdown) :void
  (popover (g-object gtk-popover)))

#+gtk-3-12
(export 'gtk-popover-popdown)

#+gtk-3-12
(defcfun ("gtk_popover_get_default_widget" gtk-popover-default-widget)
    (g-object gtk-widget)
  (popover (g-object gtk-popover)))

#+gtk-3-12
(defcfun ("gtk_popover_set_default_widget" %gtk-popover-set-default-widget) :void
  (popover (g-object gtk-popover))
  (widget (g-object gtk-widget)))

#+gtk-3-12
(defun (setf gtk-popover-default-widget) (new-value popover)
  (%gtk-popover-set-default-widget popover new-value))

#+gtk-3-12
(export 'gtk-popover-default-widget)
