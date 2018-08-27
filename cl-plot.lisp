;;; 
;;; cl-plot.lisp
;;;
;;;
;;; simple interface to gnuplot. Depends on cm and uiop. Load with
;;; (ql:quickload "cm") (ql:quickload "uiop")
;;;
;;; **********************************************************************
;;; Copyright (c) 2018 Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>
;;;
;;; Revision history: See git repository.
;;;
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the Gnu Public License, version 2 or
;;; later. See https://www.gnu.org/licenses/gpl-2.0.html for the text
;;; of this agreement.
;;; 
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;;; GNU General Public License for more details.
;;;
;;; **********************************************************************

(in-package :cl-plot)

(defparameter *gnuplot-program* "/usr/bin/gnuplot")
(defparameter *gnuplot-options* "notitle with lines;")
(defparameter *gnuplot-header* nil)

(defun construct-plot-command (&key region (grid t) (header *gnuplot-header*) (options *gnuplot-options*) &allow-other-keys)
  (concatenate 'string
               (if grid (format nil "set grid xtics lt 1 lc rgb \"#bbbbbb\";set grid ytics lt 1 lc rgb \"#bbbbbb\";~%") "")
               (if header (format nil "~a~%" header) "")
               "plot "
               (if region (format nil "[~{~,2f~^:~}] " region) "")
               "'<cat' "
               options))

(defgeneric plot (data &rest args &key region header options grid &allow-other-keys))

(defmethod plot ((data list) &rest args &key region (header *gnuplot-header*) (options *gnuplot-options*) (grid t))
  "Plot input data given as a list. In case of a list of numbers, the
data is interpreted as y values and their index is taken as x
value. In case the list is comprised of sublists, the first two
elements of the sublists are interpreted as x y pairs. Return the original data list."
  (declare (ignore header options grid))
  (let* ((region (or region (if (numberp (first data)) `(0 ,(1- (length data)))
                                (let ((x-vals (mapcar #'first data)))
                                  (list
                                   (float (apply #'min x-vals) 1.0)
                                   (float (apply #'max x-vals) 1.0))))))
         (gnuplot-instance
          (uiop:launch-program
           (list *gnuplot-program*  "-p" "-e"
                 (apply #'construct-plot-command :region region args))
           :input :stream)))
    (with-open-stream (out (uiop:process-info-input gnuplot-instance))
      (cond
        ((numberp (car data))
                 (loop for idx from 0 for x in data
                      do (format out "~a ~a~%" idx x)))
        (t (format out "~{~{~a ~}~%~}" data)))))
  (values data))

#|
     Examples:

     (plot '(3 1 8 6 5 2 4))

     (plot '((0 3) (1 1) (2 8) (3 6) (4 5) (5 2) (6 4)))

|#

(defmethod plot ((obj simple-array) &rest args &key region (header *gnuplot-header*) (options *gnuplot-options*) (grid t))
  "Plot input data given as a one-dimensional array. :region specifies
array-bounds (rounded to nearest integer). Return the original array"
  (declare (ignore header options grid))
  (let* ((gnuplot-instance
          (uiop:launch-program
           (list *gnuplot-program*  "-p" "-e"
                 (apply #'construct-plot-command :region region args))
           :input :stream)))
    (with-open-stream (out (uiop:process-info-input gnuplot-instance))
      (destructuring-bind (start end) (or region `(0 ,(1- (length obj))))
        (loop for idx from (round start) to (round end)
           do
             (let ((item (aref obj idx)))
               (cond
                 ((numberp item) (format out "~a ~a~%" idx (float (aref obj idx) 1.0)))
                 (t (cond
                      ((consp item)
                       (format out "~a ~a~%"
                               (float (first item) 1.0)
                               (float (second item) 1.0)))
                      (t
                       (format out "~a ~a~%"
                               (float (aref item 0) 1.0)
                               (float (aref item 1) 1.0)))))))))))
  (values obj))

#|
     Examples:

     (plot #(3 1 8 6 5 2 4))

     (plot #((0 3) (1 1) (2 8) (3 6) (4 5) (5 2) (6 4)))

     (plot #(#(0 3) #(1 1) #(2 8) #(3 6) #(4 5) #(5 2) #(6 4)))
|#

(defmethod plot ((fn function) &rest args
                 &key (region '(0 1)) (header *gnuplot-header*)
                   (options *gnuplot-options*) (num-values 100) (grid t))
  "Plot function (has to be a function accepting 1 argument). :region specifies xmin and xmax (default (0 1)),
:num-values the number of values to plot (default 100). Return the original function"
  (declare (ignore header options grid))
  (let* ((gnuplot-instance
          (uiop:launch-program
           (list *gnuplot-program*  "-p" "-e"
                 (apply #'construct-plot-command :region region args))
           :input :stream))
         (data (destructuring-bind (xmin xmax) region
                 (loop
                    for count below (1+ num-values)
                    collect
                      (let ((x (+ xmin (/ (* count (- xmax xmin)) num-values))))
                        (list (float x 1.0) (float (funcall fn x) 1.0)))))))
    (with-open-stream (out (uiop:process-info-input gnuplot-instance))
      (format out "~{~{~a ~}~%~}" data)))
  (values fn))

#|

Examples:

(plot #'sin :region `(0 ,(* 2 pi)))

(plot #'exp :region `(0 4))

;; default region is '(0 1)

(plot #'sin)

|#
