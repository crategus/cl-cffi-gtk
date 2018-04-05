;;;; Glade and GtkBuilder

(in-package #:gtk-demo)

(defvar *xml*
  "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<interface>
  <!-- interface-requires gtk+ 3.0 -->
  <object class=\"GtkWindow\" id=\"window\">
    <property name=\"can_focus\">False</property>
    <signal name=\"destroy\" handler=\"destroy\" swapped=\"no\"/>
    <child>
      <object class=\"GtkButton\" id=\"button\">
        <property name=\"label\" translatable=\"yes\">button</property>
        <property name=\"use_action_appearance\">False</property>
        <property name=\"visible\">True</property>
        <property name=\"can_focus\">True</property>
        <property name=\"receives_default\">True</property>
        <property name=\"use_action_appearance\">False</property>
        <signal name=\"pressed\" handler=\"pressed\" swapped=\"no\"/>
      </object>
    </child>
  </object>
</interface>")

(defun destroy (widget)
  (declare (ignore widget))
  (leave-gtk-main))

(defun pressed (widget)
  (declare (ignore widget))
  (let ((dialog (make-instance 'gtk-message-dialog
                               :message-type :info
                               :buttons :ok
                               :text "Hello, World!")))
    (gtk-dialog-run dialog)
    (gtk-widget-destroy dialog)))

(defun example-builder ()
  (within-main-loop
    (let ((builder (make-instance 'gtk-builder :from-string *xml*)))
      (let ((*package* #.(find-package '#:gtk-demo)))
        (gtk-builder-connect-signals-auto builder))
      (gtk-widget-show-all (gtk-builder-get-object builder "window")))))
