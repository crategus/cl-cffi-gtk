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
          :accessor example-app-window-stack)
   (readable :initform nil
             :accessor example-app-window-readable)
   (writable :initform nil
             :accessor example-app-window-writable))
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
                                       t)                    ; prop-writable
                                      ("readable"
                                       "gint"
                                       example-app-window-readable
                                       t
                                       nil)
                                      ("writable"
                                       "gint"
                                       example-app-window-writable
                                       nil
                                       t)))

(test register-object-type-implementation-macro
  (is (equal '(PROGN
 (SETF (GETHASH "ExampleAppWindow" GOBJECT::*REGISTERED-TYPES*)
         (GOBJECT::MAKE-OBJECT-TYPE :NAME "ExampleAppWindow" :CLASS
                                    'EXAMPLE-APP-WINDOW :PARENT
                                    "GtkApplicationWindow" :INTERFACES 'NIL
                                    :PROPERTIES
                                    '(("stack" "GtkWidget"
                                       EXAMPLE-APP-WINDOW-STACK NIL NIL))))
 (GLIB-INIT::AT-INIT ('EXAMPLE-APP-WINDOW)
   (GOBJECT::LOG-FOR :SUBCLASS
                     "Registering GObject type implementation ~A for type ~A~%"
                     'EXAMPLE-APP-WINDOW "ExampleAppWindow")
   (WITH-FOREIGN-OBJECT (GOBJECT::QUERY '(:STRUCT GOBJECT::G-TYPE-QUERY))
     (GOBJECT::G-TYPE-QUERY (GTYPE "GtkApplicationWindow") GOBJECT::QUERY)
     (GOBJECT::G-TYPE-REGISTER-STATIC-SIMPLE (GTYPE "GtkApplicationWindow")
                                             "ExampleAppWindow"
                                             (FOREIGN-SLOT-VALUE GOBJECT::QUERY
                                                                 '(:STRUCT
                                                                   GOBJECT::G-TYPE-QUERY)
                                                                 :CLASS-SIZE)
                                             (CALLBACK GOBJECT::CLASS-INIT-CB)
                                             (FOREIGN-SLOT-VALUE GOBJECT::QUERY
                                                                 '(:STRUCT
                                                                   GOBJECT::G-TYPE-QUERY)
                                                                 :INSTANCE-SIZE)
                                             (CALLBACK
                                              GOBJECT::INSTANCE-INIT-CB)
                                             NIL))
   (GOBJECT::ADD-INTERFACES "ExampleAppWindow"))
 (CLOSER-MOP:DEFMETHOD INITIALIZE-INSTANCE
   :BEFORE
   ((GOBJECT::OBJECT EXAMPLE-APP-WINDOW) &KEY POINTER)
   (GOBJECT::LOG-FOR :SUBCLASS
                     ":subclass INITIAlIZE-INSTANCE :before ~A :pointer ~A~%"
                     GOBJECT::OBJECT POINTER)
   (UNLESS
       (OR POINTER
           (AND (SLOT-BOUNDP GOBJECT::OBJECT 'POINTER)
                (POINTER GOBJECT::OBJECT)))
     (GOBJECT::LOG-FOR :SUBCLASS "calling g-object-constructor~%")
     (SETF (POINTER GOBJECT::OBJECT)
             (GOBJECT::CALL-GOBJECT-CONSTRUCTOR "ExampleAppWindow" NIL NIL)
           (G-OBJECT-HAS-REFERENCE GOBJECT::OBJECT) T)))
 "ExampleAppWindow")
             (macroexpand-1 '(register-object-type-implementation
                               "ExampleAppWindow"
                               example-app-window
                               "GtkApplicationWindow"
                               nil
                               (("stack"
                                 "GtkWidget"
                                 example-app-window-stack
                                 nil
                                 nil)))))))


(defmethod initialize-instance :after
    ((window example-app-window) &key &allow-other-keys)

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
  (is (equal '("readable" "stack" "writable")
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
                       ((READABLE EXAMPLE-APP-WINDOW-READABLE "readable" "gint"
                         T NIL)
                        (STACK EXAMPLE-APP-WINDOW-STACK "stack" "GtkWidget" T
                         T)
                        (WRITABLE EXAMPLE-APP-WINDOW-WRITABLE "writable" "gint"
                         NIL T)))
             (get-g-type-definition "ExampleAppWindow"))))

;;; --- Properties -------------------------------------------------------------

;; Call the accessor from the Lisp side
(test example-app-window-properties.1
  (let ((window (make-instance 'example-app-window)))
    (is-false (example-app-window-stack window))
    (is (typep (setf (example-app-window-stack window)
                     (make-instance 'gtk-stack)) 'gtk-stack))
    (is (typep (example-app-window-stack window) 'gtk-stack))))

;; Call the accesor from the C side
(test example-app-window-properties.2
  (let ((window (make-instance 'example-app-window)))
    (is-false (g-object-property window "stack"))
    (is (typep (setf (g-object-property window "stack")
                     (make-instance 'gtk-stack)) 'gtk-stack))
    (is (typep (g-object-property window "stack") 'gtk-stack))))

(test example-app-window-readable
  (let ((window (make-instance 'example-app-window :readable 100)))
    ;; Access from the Lisp side
    (is-false (example-app-window-readable window))
    (is (= 10 (setf (example-app-window-readable window) 10)))
    ;; Access from the C side
    (is (= 10 (g-object-property window "readable")))
    (signals (error) (setf (g-object-property window "readable") 20))))

(test example-app-window-writable
  (let ((window (make-instance 'example-app-window :writable 100)))
    ;; Access from the Lisp side
    (is-false (example-app-window-writable window))
    (is (= 10 (setf (example-app-window-writable window) 10)))
    ;; Access from the C side
    (signals (error) (g-object-property window "writable"))
    (is (= 20 (setf (g-object-property window "writable") 20)))))

;;; 2021-10-29
