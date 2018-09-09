;;; package.lisp
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

(defpackage #:cl-plot
  (:use #:cl)
  (:export
   #:PLOT
   #:CONSTRUCT-PLOT-COMMAND
   #:WITH-GNUPLOT-INSTANCE
   #:*GNUPLOT-HEADER*
   #:*GNUPLOT-PROGRAM*
   #:*GNUPLOT-OPTIONS*))
