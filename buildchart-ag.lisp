;;; buildchart-ag.lisp

;;; SPDX-FileCopyrightText: (C) 2023 Anthony Green <green@redhat.com>
;;; SPDX-License-Identifier: MIT

(in-package :buildchart-ag)

(defvar *verbose* nil)
(defvar *timestamps* nil)
(defvar *scale* "10")
(defvar *filter* "3")

(defvar *parallelism-score* 0)

(opts:define-opts
  (:name :verbose
   :description "produce verbose output"
   :short #\v
   :long "verbose")
  (:name :scale
   :description "scale multiplier (default: 10)"
   :short #\s
   :arg-parser (lambda (arg) (setf *scale* arg))
   :meta-var "SCALE"
   :default "10"
   :long "scale")
  (:name :timestamps
   :description "show timestamps"
   :short #\t
   :long "timestamps")
  (:name :filter
   :description "filter seconds (default: 3)"
   :short #\f
   :arg-parser (lambda (arg) (setf *filter* arg))
   :meta-var "FILTER"
   :default "3"
   :long "filter"))

(defun usage ()
  (opts:describe
   :prefix "buildchart-ag - copyright (C) 2023 Anthony Green <green@redhat.com>"
   :suffix "Distributed under the terms of the MIT license."
   :usage-of "buildchart-ag"
   :args "logs-file [log-file]*"))

(defun unknown-option (condition)
  (format t "warning: ~s option is unknown!~%" (opts:option condition))
  (invoke-restart 'opts:skip-option))

(defmacro when-option ((options opt) &body body)
  `(let ((it (getf ,options ,opt)))
     (when it
       ,@body)))

(defun main ()
  (multiple-value-bind (options free-args)
      (handler-case
          (handler-bind ((opts:unknown-option #'unknown-option))
            (opts:get-opts))
        (opts:missing-arg (condition)
          (format t "fatal: option ~s needs an argument!~%"
                  (opts:option condition)))
        (opts:arg-parser-failed (condition)
          (format t "fatal: cannot parse ~s as argument of ~s~%"
                  (opts:raw-arg condition)
                  (opts:option condition))))

    (when-option (options :verbose)
      (setf *verbose* t))

    (when-option (options :timestamps)
      (setf *timestamps* t))

    (setf *scale* (parse-integer *scale*))
    (setf *filter* (parse-integer *filter*))

    (if (eq 0 (length free-args))
        (usage)
        (process-logs free-args))))
