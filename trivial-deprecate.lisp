(defpackage #:org.shirakumo.trivial-deprecate
  (:nicknames #:trivial-deprecate)
  (:export
   #:deprecation-warning
   #:kind
   #:name
   #:software
   #:since-version
   #:alternatives
   #:description
   #:declaim-deprecated))

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
    (sb-ext:deprecation-condition-replacements))
  (defun description (condition)
    NIL))

(defmacro declaim-deprecated ((kind name) &key version (software (package-name (symbol-package name))) alternatives description)
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

;;; Documentation
(setf (documentation 'deprecation-warning 'type)
      "Condition signalled when a deprecated object is used.

See KIND
See NAME
See SOFTWARE
See SINCE-VERSION
See ALTERNATIVES
See DESCRIPTION
See DECLAIM-DEPRECATED")

(setf (documentation 'kind 'function)
      "Returns the kind of definition this is a warning for.

Can be one of CL:TYPE CL:VARIABLE CL:FUNCTION

See DEPRECATION-WARNING (type)")

(setf (documentation 'name 'function)
      "Returns the name of the definition this is a warning for.

See DEPRECATION-WARNING (type)")

(setf (documentation 'software 'function)
      "Returns the name of the software this definition is a part of, if any.

See DEPRECATION-WARNING (type)")

(setf (documentation 'since-version 'function)
      "Returns the version of the software since which the deprecation exists, if any.

See DEPRECATION-WARNING (type)")

(setf (documentation 'alternatives 'function)
      "Returns a list of alternate definition names that should be investigated.

See DEPRECATION-WARNING (type)")

(setf (documentation 'description 'function)
      "Returns a human-readable description of the deprecation reason, if any.

See DEPRECATION-WARNING (type)")

(setf (documentation 'declaim-deprecated 'function)
      "Declare the definition identified by KIND and NAME as deprecated.

KIND can be one of CL:TYPE CL:VARIABLE CL:FUNCTION
NAME should be the name of the definition to deprecate

VERSION may be a version string since which the definition is
deprecated. SOFTWARE may be a string identifying the name of the
software responsible for the definition. The SOFTWARE defaults to the
package-name of the NAME.

ALTERNATIVES may be a name or a list of names of alternative
definitions the user should investigate to resolve the deprecated
functionality.

DESCRIPTION may be a string describing the deprecation reason and
actions the user should take in detail.

You should always put a DECLAIM-DEPRECATED form after all definitions
that affect the name have been created (compiler macros, etc).

NOTE: On implementations other than SBCL, the following kludges apply:
  FUNCTION  --- A compiler-macro will be defined to issue the
                deprectation warning.
  VARIABLE  --- This has no effect.
  TYPE      --- If the type is a subtype of STANDARD-OBJECT, an
                INITIALIZE-INSTANCE :BEFORE method is defined that
                will issue the deprecation warning.
                If the type is a subtype of STRUCTURE-OBJECT, a
                compiler-macro will be defined for MAKE-NAME to issue
                the deprecation warning.
                Otherwise this has no effect.

See DEPRECATION-WARNING (type)")
