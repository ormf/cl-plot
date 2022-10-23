;;; 
;;; cl-plot.lisp
;;;
;;; simple interface to gnuplot. Depends on uiop version > 3.2.
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

;;; map-indexed utility to make plot more generic (using map rather than loop)

(defmacro map-indexed (result-type fn data)
  "map across a sequence by (funcall fn idx elem) on all elems of
sequence with incrementing idx (similar to the clojure function with
the same name, but not lazy and with additional return type like
Common Lisp's #'map."
  (let ((idx (gensym)))
    `(let ((,idx -1))
       (map ,result-type (lambda (elem) (funcall ,fn (incf ,idx) elem)) ,data))))

(defun construct-plot-command (&key region (grid t) (header *gnuplot-header*) (options *gnuplot-options*) &allow-other-keys)
  "construct the gnuplot command with a given header, options and a grid flag."
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

(defgeneric plot (data &rest args &key region header options grid &allow-other-keys)
  (:documentation "Plot input data. In case of sequence input data,
  the :data-fn key specifies a function which is applied to each
  element of the sequence (with its idx as first argument and the
  element as second argument) and should return the data of one
  gnuplot dataset as values. The default data-fn handles numbers in
  the sequence as y values and their index is taken as x value. In
  case the sequence is comprised of subsequences, the first elements
  of the sublists are interpreted as x y &rest z ... values.

  plot returns the original data list."))

;;;  default function for the :data-fn arg of plot. The Return value
;;;  should be of the format (values x y &rest z ...)

(defun gnuplot-data-fn (idx obj)
  (if (numberp obj)
      (values idx obj)
      (let ((lst (coerce obj 'list)))
        (cond ((consp (cdr lst)) (apply #'values lst))
              ((numberp (first lst)) (values idx (first lst)))
              (:else (error "value not a number: ~a" (first lst)))))))

(declaim (inline get-first))
(defun get-first (seq)
  (elt seq 0))

(defun get-first-min-max (seq)
  "return the min and max values of the first elements of the subseqs
in seq."
  (let ((min (get-first (get-first seq)))
        (max (get-first (get-first seq))))
    (map
     nil
     (lambda (x) (let ((num (get-first x)))
              (setf min (min num min))
              (setf max (max num max))))
     seq)
    (list min max)))

#|

alternative (consing) definition:

(defun get-first-min-max (seq)
  "return the min and max values of the first elements of the subseqs
in seq."
  (let ((x-vals (map 'list #'get-first seq)))
    (list
     (float (apply #'min x-vals) 1.0)
     (float (apply #'max x-vals) 1.0))))

|#

(defmacro with-gnuplot-instance ((stream &rest args)
                                 &body body)
  "start an external gnuplot process with a data input stream open for the extent of body.

stream is bound to gnuplot's input stream. Printing to it is
equivalent to printing into a file read by gnuplot as a dataset with
its plot command.

args are arguments sent to #'launch-gnuplot. 

Leaving the macro is equivalent to gnuplot reaching EOF when reading
an external dataset."
  (let ((gnuplot-instance (gensym)))
    `(let ((,gnuplot-instance (apply #'launch-gnuplot ,args)))
       (with-open-stream (,stream (uiop:process-info-input ,gnuplot-instance))
         ,@body))))

(defmethod plot ((data sequence) &rest args &key region
                                                (header *gnuplot-header*)
                                                (data-fn #'gnuplot-data-fn)
                                                (options *gnuplot-options*) (grid t))
  "Plot input data given as a sequence. The :data-fn key specifies a
  function which is applied to each element of the sequence (with its
  idx as second argument) and should return the data of one gnuplot
  dataset as values. The default data-fn handles numbers in the
  sequence as y values and their index is taken as x value. In case
   the sequence is comprised of subsequences, the elements of the
  subseqs are interpreted as (x y &rest z...) values. 

  plot returns the original data sequence."
    (declare (ignore header options grid))
    (setf (getf args :region)
          (or region (if (numberp (get-first data))
                         `(0 ,(1- (length data)))
                         (get-first-min-max data))))
    (with-gnuplot-instance (out . args)
      (map-indexed nil (lambda (idx x)
                         (format out "~{~,4f~^ ~}~%"
                                 (multiple-value-list (funcall data-fn idx x))))
                   data))
    (values data))

#|
     Examples:

     (plot '(3 1 8 6 5 2 4 3 1 2 5 6))

     (plot '((0 3) (1 1) (2 8) (3 6) (4 5) (5 2) (6 4)))

     (plot '(#(0 3) #(1 1) #(2 8) #(3 6) #(4 5) #(5 2) #(6 4)))

     (plot #(3 1 8 6 5 2 4))

     (plot #((0 3) (1 1) (2 8) (3 6) (4 5) (5 2) (6 4)))

     (plot #(#(0 3) #(1 1) #(2 8) #(3 6) #(4 5) #(5 2) #(6 4)))

|#

(defmethod plot ((fn function) &rest args
                 &key (region '(0 1)) (header *gnuplot-header*)
                   (options *gnuplot-options*) (num-values 100) (grid t)
                   &allow-other-keys)
  "Plot function fn (fn has to be a function accepting 1 number argument). 

:region specifies xmin and xmax (default (0 1)),

:num-values the number of values to plot (default 100). 

Return the original function."
  (declare (ignore header options grid))
  (with-gnuplot-instance (out . args)
    (destructuring-bind (xmin xmax) region
      (loop
         for count below (1+ num-values)
         collect
           (let ((x (+ xmin (/ (* count (- xmax xmin)) num-values))))
             (format out "~,4f ~,4f~%" x (funcall fn x))))))
  (values fn))

#|

Examples:

(plot #'sin :region `(0 ,(* 2 pi)))

(plot #'exp :region `(0 4))

;; default region is '(0 1)

(plot #'sin)

|#
