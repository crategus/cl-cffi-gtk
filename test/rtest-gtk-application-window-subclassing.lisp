(def-suite gtk-application-window-subclassing :in gtk-suite)
(in-suite gtk-application-window-subclassing)

(defparameter *window-ui*
"<?xml version='1.0' encoding='UTF-8'?>
<interface>
  <template class='ExampleAppWindow' parent='GtkApplicationWindow'>
    <property name='title' translatable='yes'>Example Application</property>
    <property name='default-width'>600</property>
    <property name='default-height'>400</property>
    <child>
      <object class='GtkBox' id='content_box'>
        <property name='visible'>True</property>
        <property name='orientation'>vertical</property>
        <child>
          <object class='GtkHeaderBar' id='header'>
            <property name='visible'>True</property>
            <child type='title'>
              <object class='GtkStackSwitcher' id='tabs'>
                <property name='visible'>True</property>
                <property name='stack'>stack</property>
              </object>
            </child>
          </object>
        </child>
        <child>
          <object class='GtkStack' id='stack'>
            <property name='visible'>True</property>
          </object>
        </child>
      </object>
    </child>
  </template>
</interface>")

(defclass example-app-window (gtk-application-window)
  ((template-p :initform nil
               :accessor app-window-template-p
               :allocation :class)
   (stack :initform nil
          :accessor example-app-window-stack))
  (:g-type-name . "ExampleAppWindow")
  (:metaclass gobject-class))

(register-object-type-implementation "ExampleAppWindow"      ; name
                                     example-app-window      ; class
                                     "GtkApplicationWindow"  ; parent
                                     nil                     ; interfaces
                                     (("stack"               ; prop-name
                                       "GtkWidget"           ; prop-type
                                       example-app-window-stack ; prop-accessor
                                       t                     ; prop-readable
                                       t)))                  ; prop-writeable

(defmethod initialize-instance :after
    ((window example-app-window) &key &allow-other-keys)
  (format t "~&in INITIALIZE-INSTANCE for ExampleAppWindow~%")

;  (unless (app-window-template-p window)
;    (let ((resource (g-resource-load "application.gresource")))
;      (format t "~&Register template from resource~%")
;      (g-resources-register resource)
;      (gtk-widget-class-set-template-from-resource
;                                          "ExampleAppWindow"
;                                          "/com/crategus/application/window.ui")
;      (g-resources-unregister resource)
;      (setf (app-window-template-p window) t)))

  (unless (app-window-template-p window)
    (gtk-widget-class-set-template "ExampleAppWindow" *window-ui*)
;    (gtk-widget-class-bind-template-child "ExampleAppWindow"
;                                          "ExampleAppWindow"
;                                          "stack")
    (setf (app-window-template-p window) t))

  (gtk-widget-init-template window))

(defun example-app-window-new (app)
  (make-instance 'example-app-window
                 :application app))

(defun example-app-window-open (win filename)
  (declare (ignore win filename))
  ;; Nothing to do.
)

;;; ----------------------------------------------------------------------------

(test example-app-window-class
  ;; Type check
  (is (g-type-is-object "ExampleAppWindow"))
  ;; Check the registered name
  (is (eq 'example-app-window
          (registered-object-type-by-name "ExampleAppWindow")))
  ;; TODO: Do we have a type initializer?
  ;; Check the type initializer
;  (is (eq (gtype "GtkPrintOperation")
;          (gtype (foreign-funcall "gtk_print_operation_get_type" g-size))))
  ;; Check the parent
  (is (eq (gtype "GtkApplicationWindow") (g-type-parent "ExampleAppWindow")))
  ;; Check the children
  (is (equal '()
             (mapcar #'g-type-name (g-type-children "ExampleAppWindow"))))
  ;; Check the interfaces
  (is (equal '("AtkImplementorIface" "GtkBuildable" "GActionGroup" "GActionMap")
             (mapcar #'g-type-name (g-type-interfaces "ExampleAppWindow"))))
  ;; Check the class properties
  (is (equal '("stack")
             (list-class-property-names "ExampleAppWindow")))
  ;; Get the names of the style properties.
  (is (equal '()
             (list-class-style-property-names "ExampleAppWindow")))
  ;; Get the names of the child properties
  (is (equal '()
             (mapcar #'g-param-spec-name
                     (gtk-container-class-list-child-properties "ExampleAppWindow"))))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "ExampleAppWindow" EXAMPLE-APP-WINDOW
                       (:SUPERCLASS GTK-APPLICATION-WINDOW :EXPORT T
                        :INTERFACES
                        ("AtkImplementorIface" "GActionGroup" "GActionMap"
                         "GtkBuildable"))
                       ((STACK EXAMPLE-APP-WINDOW-STACK "stack" "GtkWidget" T
                         T)))
             (get-g-type-definition "ExampleAppWindow"))))

;;; --- Properties -------------------------------------------------------------

(test example-app-window-properties
  (let ((window (make-instance 'example-app-window)))
    (is-false (example-app-window-stack window))
    (is (typep (setf (example-app-window-stack window)
                     (make-instance 'gtk-stack)) 'gtk-stack))
    (is (typep (example-app-window-stack window) 'gtk-stack))))

;;; 2021-10-17
