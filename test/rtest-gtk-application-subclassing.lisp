(def-suite gtk-application-subclassing :in gtk-suite)
(in-suite gtk-application-subclassing)

;(trace gobject::initialize-gobject-class-g-type)
;(trace gobject::register-object-type)
;(trace gobject::class-name)

;(trace gobject::gobject-class)
;(trace gobject::gobject-class-g-type-name)
;(trace gobject::gobject-class-direct-g-type-name)
;(trace gobject::gobject-class-g-type-initializer)
;(trace gobject::gobject-class-interface-p)

(defclass bloatpad (gtk-application)
  ((inhibit :initarg :inhibit
            :initform 0
            :accessor bloatpad-inhibit)
   (time :initarg :time
         :initform nil
         :accessor bloatpad-time)
   (timeout :initarg :timeout
            :initform 0
            :accessor bloatpad-timeout))
  (:g-type-name . "BloatPad")
  (:metaclass gobject-class))

#+nil
(register-object-type-implementation "BloatPad"              ; name
                                     bloatpad                ; class
                                     "GtkApplication"        ; parent
                                     nil                     ; interfaces
                                     nil)                    ; properties

(test register-object-type-implementation
  (is (equal
'(PROGN
 (SETF (GETHASH "BloatPad" GOBJECT::*REGISTERED-TYPES*)
         (GOBJECT::MAKE-OBJECT-TYPE :NAME "BloatPad" :CLASS 'BLOATPAD :PARENT
                                    "GtkApplication" :INTERFACES 'NIL
                                    :PROPERTIES 'NIL))
 (GLIB-INIT::AT-INIT ('BLOATPAD)
   (GOBJECT::LOG-FOR :SUBCLASS
                     "Registering GObject type implementation ~A for type ~A~%"
                     'BLOATPAD "BloatPad")
   (WITH-FOREIGN-OBJECT (GOBJECT::QUERY '(:STRUCT GOBJECT::G-TYPE-QUERY))
     (GOBJECT::G-TYPE-QUERY (GTYPE "GtkApplication") GOBJECT::QUERY)
     (GOBJECT::G-TYPE-REGISTER-STATIC-SIMPLE (GTYPE "GtkApplication")
                                             "BloatPad"
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
   (GOBJECT::ADD-INTERFACES "BloatPad"))
 (CLOSER-MOP:DEFMETHOD INITIALIZE-INSTANCE
   :BEFORE
   ((GOBJECT::OBJECT BLOATPAD) &KEY POINTER)
   (GOBJECT::LOG-FOR :SUBCLASS
                     ":subclass INITIAlIZE-INSTANCE :before ~A :pointer ~A~%"
                     GOBJECT::OBJECT POINTER)
   (UNLESS
       (OR POINTER
           (AND (SLOT-BOUNDP GOBJECT::OBJECT 'POINTER)
                (POINTER GOBJECT::OBJECT)))
     (GOBJECT::LOG-FOR :SUBCLASS "calling g-object-constructor~%")
     (SETF (POINTER GOBJECT::OBJECT)
             (GOBJECT::CALL-GOBJECT-CONSTRUCTOR "BloatPad" NIL NIL)
           (G-OBJECT-HAS-REFERENCE GOBJECT::OBJECT) T)))
 (PROGN)
 "BloatPad")
             (macroexpand '(register-object-type-implementation "BloatPad"
                                                                bloatpad
                                                                "GtkApplication"
                                                                nil
                                                                nil)))))

;;; 2021-10-12
