;;; task.lisp

;;; SPDX-FileCopyrightText: (C) 2023 Anthony Green <green@redhat.com>
;;; SPDX-License-Identifier: MIT

(in-package :buildchart-ag)

(defclass <task> ()
  ((description :initarg :description :initform "[empty]")
   (start-time :initarg :start-time)
   (duration :initarg :duration :initform -1)
   (start-seconds :initform nil :initarg :start-seconds)
   (end-seconds :initform nil)
   (children :initarg :children :initform nil)))

(defclass <jenkins-task> (<task>)
  ((node-id :initarg :node-id)
   (parent-id :initarg :parent-id :initform -1)))

(defmethod color ((task <jenkins-task>))
  +blue+)

(defmethod color ((task <task>))
  +yellow+)

(defmethod print-object ((task <task>) out)
  (print-unreadable-object (task out :type t)
    (format out "~A (~As)[~A:~A]: ~A"
            (slot-value task 'start-time)
            (slot-value task 'duration)
            (slot-value task 'start-seconds)
            (slot-value task 'end-seconds)
            (slot-value task 'description))))

(defun set-offset-seconds (start-time task)
  (let ((start-seconds (abs (local-time:timestamp-difference start-time (slot-value task 'start-time)))))
    (setf (slot-value task 'start-seconds) start-seconds)
    (setf (slot-value task 'end-seconds) (+ start-seconds (slot-value task 'duration))))
  (dolist (c (slot-value task 'children))
    (set-offset-seconds start-time c)))

(defun calculate-parallel-percentage (tasks)
  (labels ((gather-start-end-times (tasks)
             (loop for task in tasks
                   appending (if (slot-value task 'children)
                                 (gather-start-end-times (slot-value task 'children))
                                 (if (eq (slot-value task 'duration) -1)
                                     (list (list 0 'start) (list 0 'end))
                                     (list (list (slot-value task 'start-seconds) 'start) (list (+ (slot-value task 'start-seconds)
                                                                                                   (slot-value task 'duration)) 'end)))))))
    (let ((events (sort (gather-start-end-times tasks) #'(lambda (a b) (< (car a) (car b))))))
      (loop with run-time = 0.0
            with parallel-time = 0.0
            with concurrent-tasks = 0
            with last-time = 0
            for (time event) in events
            do (let ((interval (- time last-time)))
                 (incf run-time interval)
                 (incf parallel-time (* concurrent-tasks interval)))
               (cond
                 ((eq event 'start) (incf concurrent-tasks))
                 ((eq event 'end) (decf concurrent-tasks)))
               (setf last-time time)
            finally (return (if (> run-time 0) (* 100 (/ parallel-time run-time)) 0))))))

(defmethod insert-task ((tasks list) (new-task <task>))
  (cond
    ((endp tasks) (values (list new-task) t))
    ((or (local-time:timestamp= (slot-value new-task 'start-time) (slot-value (first tasks) 'start-time))
         (local-time:timestamp< (slot-value new-task 'start-time) (slot-value (first tasks) 'start-time)))
     (values (cons new-task tasks) t))
    ((slot-value (first tasks) 'children)
     (multiple-value-bind (new-task-list changed?)
         (insert-task-into-children (first tasks) new-task)
       (if changed?
           (values tasks t)
           (if (rest tasks)
               (multiple-value-bind (new-task-list changed?)
                   (insert-task (rest tasks) new-task)
                 (values (cons (first tasks) new-task-list) changed?))
               (values tasks nil)))))
    (t
     (if (rest tasks)
         (multiple-value-bind (new-task-list changed?)
             (insert-task (rest tasks) new-task)
           (values (cons (first tasks) new-task-list) changed?))
         (values tasks nil)))))

(defmethod insert-task-into-children ((task <task>) (new-task <task>))
  (multiple-value-bind (new-task-list changed?)
      (insert-task (slot-value task 'children) new-task)
    (setf (slot-value task 'children) new-task-list)
    (values task changed?)))
