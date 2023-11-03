;;; process.lisp

;;; SPDX-FileCopyrightText: (C) 2023 Anthony Green <green@redhat.com>
;;; SPDX-License-Identifier: MIT

(in-package :buildchart-ag)

(defvar *start-time* nil)

(defun guess-file-type (filename)
  (with-open-file (stream filename
                          :direction :input
                          :if-does-not-exist :error)
    (let ((first-char (read-char stream nil nil)))
      (cond
       ((null first-char)
        (error "The input file ~A is empty." filename))
       ((char= first-char #\{)
        :JSON)
       ((char= first-char #\<)
        :HTML)
       (t
        (loop for line = (read-line stream nil)
              while line
              when (str:contains? " [Pipeline]" line)
                do (return-from guess-file-type :JENKINS))
        (if (digit-char-p first-char)
            :EVENTS
            (error "Unrecognized file ~A." filename)))))))

(defun dump-jt (indent jt)
  (loop for i from 0 to indent do (format t "  "))
  (format t "~A[~A] ~A~%"
          (slot-value jt 'node-id)
          (slot-value jt 'parent-id)
          (slot-value jt 'description))
  (let ((c (slot-value jt 'children)))
    (when c
      (progn
        (loop for i from 0 to indent do (format t "  "))
        (format t "{~%")
        (dolist (ct c)
          (dump-jt (1+ indent) ct))
        (loop for i from 0 to indent do (format t "  "))
        (format t "}~%")))))

(defparameter +scan-log-entry+ "\\[([^\\]]+)\\] (.*)$")

(defun fix-duration (tlist)
  (if (cdr tlist)
      (setf (slot-value (car tlist) 'duration)
            (abs (local-time:timestamp-difference (slot-value (car tlist) 'start-time)
                                                  (fix-duration (cdr tlist))))))
  (slot-value (car tlist) 'start-time))

(defun clean-up-tasks (start-time tasks)
  (let ((sorted-tasks (sort tasks (lambda (f g)
                                    (local-time:timestamp< (slot-value f 'start-time)
                                                           (slot-value g 'start-time))))))
    (if (null start-time)
        (setf start-time (slot-value (car sorted-tasks) 'start-time)))
    (fix-duration sorted-tasks)
    (dolist (task sorted-tasks)
      (let ((children (slot-value task 'children)))
        (if children
            (progn
              (setf (slot-value task 'children) (clean-up-tasks start-time children))
              (if (eq -1 (slot-value task 'duration))
                  (setf (slot-value task 'duration) (abs (local-time:timestamp-difference (slot-value task 'start-time)
                                                                                          (slot-value (car (last children)) 'start-time)))))))))
    (remove-if (lambda (n) (> *filter* (slot-value n 'duration))) sorted-tasks)))

(defun make-description (timestamp string)
  (let ((cleaned-description (if (and (>= (length string) 6)
                                      (string= (subseq string (- (length string) 6)) "(hide)"))
                                 (subseq string 0 (- (length string) 6))
                                 string)))
    (if *timestamps*
        (format nil "~A: ~A" timestamp cleaned-description)
        (format nil "~A" cleaned-description))))

(defun process-jenkins-html (file-name)
  (let* ((doc (lquery:$ (initialize (pathname file-name))))
         (jenkins-tasks (make-hash-table :test #'equal))
         (top-jenkins-tasks (make-hash-table :test #'equal))
         (nn (lquery:$ doc "span.pipeline-new-node")))
    (loop for i from 0 to (1- (length nn))
          for node = (aref nn i)
          do (multiple-value-bind (string matches)
                 (ppcre:scan-to-strings +scan-log-entry+ (plump:text node))
               (declare (ignore string))
               (unless *start-time*
                 (setf *start-time* (local-time:parse-timestring (string-trim '(#\[ #\]) (aref matches 0)))))
               (let ((node-id (plump:attribute node "nodeid"))
                     (start-id (plump:attribute node "startid"))
                     (enclosing-id (plump:attribute node "enclosingid"))
                     (timestamp (local-time:parse-timestring (string-trim '(#\[ #\]) (aref matches 0)))))
                 (if (and start-id
                          (not (string= node-id start-id)))
                     ;; This is the end of a group.  Set the duration.
                     (let ((start-node (gethash start-id jenkins-tasks)))
                       (setf (slot-value start-node 'duration)
                             (abs (local-time:timestamp-difference (slot-value start-node 'start-time) timestamp))))

                     ;; This is something else. Possibly a child node.
                     (let ((jt (make-instance '<jenkins-task>
                                              :description (make-description (aref matches 0) (aref matches 1))
                                              :node-id node-id
                                              :parent-id enclosing-id
                                              :start-time timestamp
                                              :start-seconds (abs (local-time:timestamp-difference *start-time* timestamp))))
                           (parent (gethash enclosing-id jenkins-tasks))
                           (span (format nil "span.pipeline-node-~A" node-id)))
                       (if parent
                           (push jt (slot-value parent 'children))
                           (setf (gethash node-id top-jenkins-tasks) jt))
                       (setf (gethash node-id jenkins-tasks) jt)
                       (let ((kids (lquery:$ doc span)))
                         (loop for j from 0 to (1- (length kids))
                               do (with-input-from-string (stream (plump:text (aref kids j)))
                                    (loop for line = (read-line stream nil)
                                          while line
                                          do (multiple-value-bind (string matches)
                                                 (ppcre:scan-to-strings +scan-log-entry+ line)
                                               (declare (ignore string))
                                               ;; FIXME: why would matches ever be null?
                                               (when matches
                                                 (push (make-instance '<jenkins-task>
                                                                      :description (make-description (aref matches 0) (aref matches 1))
                                                                      :node-id (plump:attribute node "node-id")
                                                                      :start-time (local-time:parse-timestring (string-trim '(#\[ #\]) (aref matches 0)))
                                                                      :start-seconds (abs (local-time:timestamp-difference *start-time* timestamp)))
                                                       (slot-value jt 'children)))))))))))))
    (let ((tlist nil))
      (maphash (lambda (key value)
                 (declare (ignore key))
                 (push value tlist))
               top-jenkins-tasks)
      (setf *parallelism-score* (calculate-parallel-percentage tlist))
      (let ((cut (clean-up-tasks nil tlist)))
        cut))))

(defun process-json-file (file-name)
  (with-open-file (stream file-name
                          :direction :input
                          :if-does-not-exist :error)
    (let ((json (json:decode-json stream)))
      (if (string= "v1" (cdr (assoc :API-VERSION json)))
          (loop for item in (cdr (assoc :ITEMS json))
                collect (make-instance '<event>
                                       :start-time (local-time:parse-timestring (or (cdr (assoc :EVENT-TIME item))
                                                                                    (cdr (assoc :FIRST-TIMESTAMP item))))
                                       :message (cdr (assoc :MESSAGE item))))
          (error "Unrecognized JSON file ~A." file-name)))))

(defun parse-time-period (string)
  (let ((value (parse-float:parse-float string :junk-allowed t))
        (unit (subseq string (1+ (position-if #'digit-char-p string :from-end t)))))
    (cond ((str:starts-with? "s " unit) value)
          ((str:starts-with? "ms " unit) (/ value 1000))
          (t (error "Unknown time unit")))))

(defun process-jenkins-logs (file-name)
  (with-open-file (stream file-name
                          :direction :input
                          :if-does-not-exist :error)
    (loop for line = (read-line stream nil)
          while line
          for event = (let ((split-line (split-at-first-space line)))
                        (handler-case
                            (make-instance '<jenkins-event>
                                           :start-time (local-time:parse-timestring
                                                        (string-trim '(#\[ #\]) (car split-line)))
                                           :message (cadr split-line))
                          (error (e)
                            (declare (ignore e))
                            nil)))
          when event
            collect event)))

(defun split-at-first-space (str)
  (let ((space-pos (position #\Space str)))
    (if space-pos
        (list (subseq str 0 space-pos)
              (subseq str (1+ space-pos)))
        (list str ""))))

(defun process-build-logs (file-name)
  (with-open-file (stream file-name
                          :direction :input
                          :if-does-not-exist :error)
    (loop for line = (read-line stream nil)
          while line
          collect (let ((split-line (split-at-first-space line)))
                    (make-instance '<event>
                                   :start-time (local-time:parse-timestring (car split-line))
                                   :message (cadr split-line))))))

(defparameter +scan-pulled-image+ (ppcre:create-scanner "Successfully pulled image \"(.+)\" in (.+)"))
(defparameter +scan-push-successful+ (ppcre:create-scanner "^Push successful"))
(defparameter +scan-trying-to-pull+ (ppcre:create-scanner "^Storing signatures"))
(defparameter +scan-docker-build-commit+ (ppcre:create-scanner "^COMMIT"))

(defun find-open-brace (events index)
  (loop for i from index to (1- (length events))
        for msg = (slot-value (aref events i) 'message)
        when (str:starts-with? " [Pipeline]" msg)
          do (progn
               (return-from find-open-brace (if (str:starts-with? " [Pipeline] {" msg)
                                                i nil))))
  nil)

(defun match-anything (events index strings)
  (declare (ignore strings))
  (values (1+ index) (make-instance '<task>
                                    :start-time (slot-value (aref events index) 'start-time)
                                    :duration 1
                                    :description (slot-value (aref events index) 'message))))

(defun match-image-pull (events index strings)
  (declare (ignore strings))
  (loop for i from (1+ index) to (1- (length events))
        do (multiple-value-bind (string matches)
               (ppcre:scan-to-strings +scan-pulled-image+ (slot-value (aref events i) 'message))
             (when string
               (return (values (1+ i) (make-instance '<task> :start-time (slot-value (aref events index) 'start-time)
                                                             :duration (parse-time-period (aref matches 1))
                                                             :description (format nil "Pulling image ~A" (aref matches 0)))))))))

(defun create-dockerfile-build-stage-tasks (events index)
  (loop for i from index to (1- (length events))
        with prev-step = nil
        with done = nil
        for step = (if (or (str:starts-with? "STEP" (slot-value (aref events i) 'message))
                           (str:starts-with? "COMMIT" (slot-value (aref events i) 'message))
                             (and (str:starts-with? "Successfully tagged" (slot-value (aref events i) 'message))
                                  (setf done t)))
                         (progn
                           (when prev-step
                             (setf (slot-value prev-step 'duration)
                                   (abs (local-time:timestamp-difference (slot-value (aref events i) 'start-time)
                                                                         (slot-value prev-step 'start-time)))))
                           (setf prev-step (make-instance '<task>
                                                          :start-time (slot-value (aref events i) 'start-time)
                                                          :description (slot-value (aref events i) 'message))))
                         nil)
        when step
          collect step
        until done))

(defun match-dockerfile-build (events index strings)
 (declare (ignore strings))
  (loop for i from (1+ index) to (1- (length events))
        do (multiple-value-bind (string matches)
               (ppcre:scan-to-strings +scan-docker-build-commit+ (slot-value (aref events i) 'message))
             (declare (ignore matches))
             (when string
               (let ((stages (create-dockerfile-build-stage-tasks events index)))
                 (return (values (1+ i) (make-instance '<task> :start-time (slot-value (aref events index) 'start-time)
                                                               :duration (abs (local-time:timestamp-difference
                                                                               (slot-value (aref events index) 'start-time)
                                                                               (slot-value (aref events i) 'start-time)))
                                                               :children stages
                                                               :description "Dockerfile build"))))))))

(defun match-trying-pull (events index strings)
  (let ((image (aref strings 0)))
    (loop for i from (1+ index) to (1- (length events))
          do (multiple-value-bind (string matches)
                 (ppcre:scan-to-strings +scan-trying-to-pull+ (slot-value (aref events i) 'message))
               (declare (ignore matches))
               (when string
                 (return (values (1+ i) (make-instance '<task> :start-time (slot-value (aref events index) 'start-time)
                                                               :duration (abs (local-time:timestamp-difference
                                                                               (slot-value (aref events index) 'start-time)
                                                                               (slot-value (aref events i) 'start-time)))
                                                               :description (format nil "Pulling image ~A" image)))))))))

(defun match-image-push (events index strings)
  (let ((image (aref strings 0)))
    (loop for i from (1+ index) to (1- (length events))
          do (multiple-value-bind (string matches)
                 (ppcre:scan-to-strings +scan-pulled-image+ (slot-value (aref events i) 'message))
               (declare (ignore matches))

               (when string
                (return (values (1+ i) (make-instance '<task> :start-time (slot-value (aref events index) 'start-time)
                                                               :duration (abs (local-time:timestamp-difference
                                                                               (slot-value (aref events index) 'start-time)
                                                                               (slot-value (aref events i) 'start-time)))
                                                               :description (format nil "Pushing image ~A" image)))))))))

(defparameter +event-patterns+
  (list (list (ppcre:create-scanner "Pulling image \"(.+)\"") #'match-image-pull)
        (list (ppcre:create-scanner "Pushing image (.+)") #'match-image-push)
        (list (ppcre:create-scanner "Trying to pull (.+)") #'match-trying-pull)
        (list (ppcre:create-scanner "STEP 1/.+: (.+)") #'match-dockerfile-build)
        (list (ppcre:create-scanner "(.*)") #'match-anything)))

(defun create-tasks (evector start-index end-index)
  (let ((tasks nil))
    (loop with i = start-index
          while (and i (< i end-index))
          do (loop for p in +event-patterns+
                   while i
                   do (if (str:starts-with? " [Pipeline] }" (slot-value (aref evector i) 'message))
                          (return-from create-tasks (values (1+ i) (reverse tasks)))
                          (multiple-value-bind (string matches)
                              (ppcre:scan-to-strings (car p) (slot-value (aref evector i) 'message))
                            (when string
                              (multiple-value-bind (next-i task)
                                  (funcall (cadr p) evector i matches)
                                (setf i next-i)
                                (when task
                                  (push task tasks)))))))
          finally (return (values i (reverse tasks))))))

(defun process-logs (file-list)
  (let ((platform-tasks nil)
        (jenkins-tasks nil))
    (dolist (file-name file-list)
      (let ((file-type (guess-file-type file-name)))
        (cond
          ((eq file-type :JSON)
           (let ((evector (coerce
                           (sort (process-json-file file-name)
                                 (lambda (a b)
                                   (local-time:timestamp< (slot-value a 'start-time)
                                                          (slot-value b 'start-time))))
                           'vector)))
             (multiple-value-bind (next-i tasks)
                 (create-tasks evector 0 (1- (length evector)))
               (declare (ignore next-i))
               (setf platform-tasks tasks))))

          ((eq file-type :HTML)
           (setf jenkins-tasks (process-jenkins-html file-name))))))

    (dolist (ptask platform-tasks)
      (when (local-time:timestamp> (slot-value ptask 'start-time) *start-time*)
        (setf jenkins-tasks (insert-task jenkins-tasks ptask))))

    (let ((start-time (slot-value (car jenkins-tasks) 'start-time)))
      (dolist (task jenkins-tasks)
        (set-offset-seconds start-time task)))

    (draw jenkins-tasks)))
