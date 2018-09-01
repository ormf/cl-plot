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

(defun launch-gnuplot (&rest args &key region &allow-other-keys)
  (uiop:launch-program
   (list *gnuplot-program*  "-p" "-e"
         (apply #'construct-plot-command :region region args))
   :input :stream))

(defgeneric plot (data &rest args &key region header options grid &allow-other-keys))

 (format out "~a ~a~%" idx x)

;;;  generic function returning gnuplot data. The Return value should
;;;  be of the format (values x y &rest z ...)

(defgeneric gnuplot-data-fn (obj idx))

(defmethod gnuplot-data-fn ((obj number) idx)
  (values idx obj))

(defmethod gnuplot-data-fn ((obj list) idx)
  (cond ((> (length obj) 1)
         (apply #'values obj))
        ((numberp (first obj)) (values idx (first obj)))
        (:else (error "value not a number: ~a" (first obj)))))

(defmethod gnuplot-data-fn ((obj simple-array) idx)
  (cond ((> (length obj) 1)
         (apply #'values (coerce obj 'list)))
        ((numberp (aref obj 0)) (values (aref obj 0)))
        (:else (error "value not a number: ~a" (aref obj 0)))))

(defgeneric get-first (obj))

(defmethod get-first ((obj list))
  (first obj))

(defmethod get-first ((obj simple-array))
  (aref obj 0))


(defmethod plot ((data list) &rest args &key region
                                          (header *gnuplot-header*)
                                          (data-fn #'gnuplot-data-fn)
                                          (options *gnuplot-options*) (grid t))
  "Plot input data given as a list. The :data-fn key specifies a
  function which is applied to each element of the sequence (with its
  idx as second argument) and should return the data of one gnuplot
  dataset as values. The default data-fn handles numbers in the list
  as y values and their index is taken as x value. In case the list is
  comprised of sublists, the first elements of the sublists are
  interpreted as x y ... values. plot returns the original data list."
  (declare (ignore header options grid))
  (let* ((region (or region (if (numberp (first data)) `(0 ,(1- (length data)))
                                (let ((x-vals (mapcar #'get-first data)))
                                  (list
                                   (float (apply #'min x-vals) 1.0)
                                   (float (apply #'max x-vals) 1.0))))))
         (gnuplot-instance (apply #'launch-gnuplot :region region args)))
    (with-open-stream (out (uiop:process-info-input gnuplot-instance))
      (loop for idx from 0 for x in data
         do (format out "~{~,4f~^ ~}~%"
                    (multiple-value-list (funcall data-fn x idx))))))
  (values data))

#|
     Examples:

     (plot '(3 1 8 6 5 2 4))

     (plot '((0 3) (1 1) (2 8) (3 6) (4 5) (5 2) (6 4)))

     (plot '(#(0 3) #(1 1) #(2 8) #(3 6) #(4 5) #(5 2) #(6 4)))

|#

(defmethod plot ((data simple-array) &rest args &key region
                                          (header *gnuplot-header*)
                                          (data-fn #'gnuplot-data-fn)
                                          (options *gnuplot-options*) (grid t))
  "Plot input data given as a list. The :data-fn key specifies a
  function which is applied to each element of the sequence (with its
  idx as second argument) and should return the data of one gnuplot
  dataset as values. The default data-fn handles numbers in the list
  as y values and their index is taken as x value. In case the list is
  comprised of sublists, the first elements of the sublists are
  interpreted as x y ... values. plot returns the original data list."
  (declare (ignore header options grid))
  (let* ((region (or region (if (numberp (aref data 0)) `(0 ,(1- (length data)))
                                (let ((x-vals (loop for x across data collect
                                                   (get-first x))))
                                  (list
                                   (float (apply #'min x-vals) 1.0)
                                   (float (apply #'max x-vals) 1.0))))))
         (gnuplot-instance (apply #'launch-gnuplot :region region args)))
    (with-open-stream (out (uiop:process-info-input gnuplot-instance))
      (loop for idx from 0 for x across data
         do (format out "~{~,4f~^ ~}~%"
                    (multiple-value-list (funcall data-fn x idx))))))
  (values data))

#|
     Examples:

     (plot #(3 1 8 6 5 2 4))

     (plot #((0 3) (1 1) (2 8) (3 6) (4 5) (5 2) (6 4)))

     (plot #(#(0 3) #(1 1) #(2 8) #(3 6) #(4 5) #(5 2) #(6 4)))

|#

(defmethod plot ((fn function) &rest args
                 &key (region '(0 1)) (header *gnuplot-header*)
                   (options *gnuplot-options*) (num-values 100) (grid t)
                   &allow-other-keys)
  "Plot function fn (fn has to be a function accepting 1 argument). :region specifies xmin and xmax (default (0 1)),
:num-values the number of values to plot (default 100). Return the
original function."
  (declare (ignore header options grid))
  (let* ((gnuplot-instance (apply #'launch-gnuplot :region region args)))
    (with-open-stream (out (uiop:process-info-input gnuplot-instance))
      (destructuring-bind (xmin xmax) region
        (loop
           for count below (1+ num-values)
           collect
             (let ((x (+ xmin (/ (* count (- xmax xmin)) num-values))))
               (format out "~,4f ~,4f~%" x (funcall fn x)))))))
  (values fn))

#|

Examples:

(plot #'sin :region `(0 ,(* 2 pi)))

(plot #'exp :region `(0 4))

;; default region is '(0 1)

(plot #'sin)

|#
