;;; event.lisp

;;; SPDX-FileCopyrightText: (C) 2023 Anthony Green <green@redhat.com>
;;; SPDX-License-Identifier: MIT

(in-package :buildchart-ag)

(defclass <event> ()
  ((start-time :initarg :start-time)
   (duration :initarg :duration :initform 0)
   (message :initarg :message)))

(defmethod print-object ((event <event>) out)
  (print-unreadable-object (event out :type t)
    (format out "~A (~As): ~A"
            (slot-value event 'start-time)
            (slot-value event 'duration)
            (slot-value event 'message))))
