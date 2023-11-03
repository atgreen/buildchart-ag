;;; draw.lisp

;;; SPDX-FileCopyrightText: (C) 2023 Anthony Green <green@redhat.com>
;;; SPDX-License-Identifier: MIT

(in-package :buildchart-ag)

(defparameter +black+ '(0 0 0 1))
(defparameter +grey+ '(0.3 0.3 0.3 .5))
(defparameter +blue+ '(0 0 1 1))
(defparameter +yellow+ '(1 1 0 1))

(defparameter +border-color+ '(.5 .5 .5 1))
(defparameter +parent-color+ '(.2 .2 .2 .05))
(defparameter +event-text-color+ +black+)
(defparameter +sec-w+ 50)
(defparameter +header-font-size+ 18)
(defparameter +summary-font-size+ 12)
(defparameter +axis-font-size+ 11)
(defparameter +event-height+ 16)
(defparameter +bar-color+ +blue+)
(defparameter +header-text-color+ +black+)

(defun depth (task)
  (+ 1 (loop for child in (slot-value task 'children)
             sum (depth child))))

(defun extents (tasks)
  (let ((end-time (loop for task in tasks
                        maximize (slot-value task 'end-seconds))))
    (values (+ (floor (* +sec-w+ end-time) *scale*) 20)
            (+ 150 ; header
               (* (loop for task in tasks
                        sum (depth task))
                  +event-height+)))))

(defun draw-text (color text x y)
  (apply #'cairo:set-source-rgba color)
  (cairo:move-to x y)
  (cairo:show-text text))

(defun draw-rectangle (color rect)
	(apply #'cairo:set-source-rgba color)
	(apply #'cairo:rectangle rect)
	(cairo:stroke))

(defun draw-fill-rectangle (color rect)
	(apply #'cairo:set-source-rgba color)
	(apply #'cairo:rectangle rect)
	(cairo:fill-path))

(defun draw-box-ticks (rectangle sec-w)
  (draw-rectangle +black+ rectangle)
  (cairo:set-line-cap :square)
  (loop for i from sec-w to (1+ (third rectangle)) by sec-w
        do (progn
             (if (eq 0 (mod i (* sec-w 5)))
                 (apply #'cairo:set-source-rgba +black+)
                 (apply #'cairo:set-source-rgba +grey+))
             (cairo:move-to (+ (first rectangle) i) (1+ (second rectangle)))
             (cairo:line-to (+ (first rectangle) i) (1- (+ (second rectangle) (fourth rectangle))))
             (cairo:stroke)))
  (cairo:set-line-cap :butt))

(defun draw-tick-labels (rectangle)
  (cairo:set-font-size +axis-font-size+)
  (loop for i from 0 to (1+ (third rectangle)) by +sec-w+
        do (let* ((label (format nil "~As" (* *scale* (/ i +sec-w+))))
                  (label-width (cairo:text-width (cairo:get-text-extents label))))
             (draw-text +black+ label (- (+ (first rectangle) i) (/ label-width 2)) (- (second rectangle) 2)))))

(defun draw-label-in-box (color label x y w max)
  (let* ((label-w (cairo:text-width (cairo:get-text-extents label)))
         (label-x (if (> (+ x w label-w 5) max)
                      (max 15 (- x label-w 5))
                      (+ x w 5))))
    (draw-text color label label-x y)))

(defun draw-task (chart-duration y task rectangle)
  (let ((x (+ (first rectangle)
              (* (slot-value task 'start-seconds)
                 (/ (third rectangle) chart-duration))))
        (w (* (slot-value task 'duration)
                     (/ (third rectangle) chart-duration))))

    (if (null (slot-value task 'children))
        (progn
          (draw-fill-rectangle (color task) (list x y w +event-height+))
          (draw-label-in-box +event-text-color+
                             (slot-value task 'description)
                             x (- (+ y +event-height+) 4)
                             w (+ (first rectangle) (third rectangle)))
          (+ y +event-height+))
        (let ((depth (depth task)))
          (draw-fill-rectangle +parent-color+ (list x y w (* depth +event-height+)))
          (draw-text +event-text-color+ (slot-value task 'description) (+ x 5) (+ y +event-height+))
          (let ((next-y (+ y +event-height+)))
            (loop for child in (slot-value task 'children)
                  do (setf next-y (draw-task chart-duration next-y child rectangle)))
            next-y)))))

(defun draw (tasks)
  (multiple-value-bind (w h)
      (extents tasks)
    (let ((surface (cairo:create-svg-surface "out.svg" w h)))
      (let* ((ctx (cairo:create-context surface)))
        (unwind-protect
             (cairo:with-context (ctx)
               (cairo:set-line-width 1.0)
               (cairo:select-font-face "Bitstream Vera Sans" :normal :normal)

               (cairo:set-source-rgb 1 1 1)
               (cairo:paint)

               (setf current-y 20)

               (cairo:set-font-size +header-font-size+)
               (draw-text +header-text-color+ "buildchart-ag" 10 current-y)
               (incf current-y (cairo:font-height (cairo:get-font-extents)))

               (cairo:set-font-size +summary-font-size+)
               (draw-text +header-text-color+ (format nil "Parallelism Score: ~,2F%" *parallelism-score*) 10 current-y)
               (incf current-y (+ 0 (cairo:font-height (cairo:get-font-extents))))
               (draw-text +header-text-color+ (format nil "Filtering out Jenkins tasks shorter than: ~As" *filter*) 10 current-y)
               (incf current-y (+ 0 (cairo:font-height (cairo:get-font-extents))))

               (draw-fill-rectangle +blue+ (list 10 current-y 10 +event-height+))
               (draw-text +header-text-color+ "Jenkins Tasks" 25 (+ 10 current-y))

               (draw-fill-rectangle +yellow+ (list 120 current-y 10 +event-height+))
               (draw-text +header-text-color+ "Kubernetes Events" 135 (+ 10 current-y))

               (incf current-y (+ 20 (cairo:font-height (cairo:get-font-extents))))

               (let ((rectangle
                       (list 10
                             current-y
                             (- w 15)
                             (+ (- (- h (* 2 10)) (+ current-y 60)) 32))))
                 (let ((end-time (loop for task in tasks
                                       maximize (slot-value task 'end-seconds))))
                   (draw-box-ticks rectangle +sec-w+)
                   (draw-tick-labels rectangle)
                   (dolist (task tasks)
                     (setf current-y (draw-task end-time current-y task rectangle))))))
          (progn
            (cairo:surface-finish surface)
            (cairo:destroy ctx)))))))
