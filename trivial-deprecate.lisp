(in-package #:org.shirakumo.trivial-deprecate)

#-sbcl
(define-condition deprecation-warning (style-warning)
  ((kind :initarg :kind :initform (error "KIND required.") :reader kind)
   (name :initarg :name :initform (error "NAME required.") :reader name)
   (software :initarg :software :initform NIL :reader software)
   (since-version :initarg :since-version :initform NIL :reader since-version)
   (alternatives :initarg :alternatives :initform NIL :reader alternatives)
   (description :initarg :description :initform NIL :reader description))
  (:report (lambda (c s) (format s "The ~a ~s is deprecated in~@[ ~a~]~@[ since ~a~]
~@[Please consider using one of the following alternatives:~{~%  ~a~}~]~@[~%~%~a~]"
                                 (kind c) (name c) (software c) (since-version c)
                                 (alternatives c) (description c)))))

#+sbcl
(progn
  (deftype deprecation-warning ()
    'sb-ext:deprecation-condition)
  (defun kind (condition)
    (sb-ext:deprecation-condition-namespace condition))
  (defun name (condition)
    (sb-ext:deprecation-condition-name condition))
  (defun software (condition)
    (sb-ext:deprecation-condition-software condition))
  (defun since-version (condition)
    (sb-ext:deprecation-condition-version condition))
  (defun alternatives (condition)
    (sb-ext:deprecation-condition-replacements condition))
  (defun description (condition)
    (declare (ignore condition))
    NIL))

(defmacro declaim-deprecated ((kind name) &key version (software (package-name (symbol-package name))) alternatives description)
  (declare (ignore description))
  (unless (listp alternatives)
    (setf alternatives (list alternatives)))
  #+sbcl
  `(declaim (sb-ext:deprecated :early (,software ,version) (,kind ,name :replacement ,alternatives)))
  #-sbcl
  (let ((warn-form `(warn 'deprecation-warning
                          :kind ',kind
                          :name ',name
                          :software ',software
                          :since-version ',version
                          :alternatives ',alternatives
                          :description ',description)))
    (ecase kind
      (function
       `(define-compiler-macro ,name (&whole whole &rest args)
          (declare (ignore args))
          ,warn-form
          whole))
      (variable
       NIL)
      (type
       (cond ((subtypep name 'standard-object)
              `(defmethod initialize-instance :before ((,name ,name) &key)
                 ,warn-form))
             ((subtypep name 'structure-object)
              `(define-compiler-macro ,(intern (format NIL "~a-~a" 'make name)
                                               (symbol-package name))
                   (&whole whole &rest args)
                 (declare (ignore args))
                 ,warn-form
                 whole))
             (T
              NIL))))))
