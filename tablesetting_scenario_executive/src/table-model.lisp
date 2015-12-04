;;; Copyright (c) 2014, Jan Winkler <winkler@cs.uni-bremen.de>
;;; All rights reserved.
;;; 
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions are met:
;;; 
;;;     * Redistributions of source code must retain the above copyright
;;;       notice, this list of conditions and the following disclaimer.
;;;     * Redistributions in binary form must reproduce the above copyright
;;;       notice, this list of conditions and the following disclaimer in the
;;;       documentation and/or other materials provided with the distribution.
;;;     * Neither the name of Willow Garage, Inc. nor the names of its
;;;       contributors may be used to endorse or promote products derived from
;;;       this software without specific prior written permission.
;;; 
;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
;;; AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
;;; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
;;; SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
;;; CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
;;; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
;;; POSSIBILITY OF SUCH DAMAGE.

(in-package :tablesetting-scenario-executive)

(deftype table-side () '(member :east :west :north :south))

(defclass seat ()
  ((table-side :initarg :table-side :reader table-side)
   (index :initarg :index :reader index)))

(defclass table ()
  ((seats :initarg :seats :reader seats)
   (width :initarg :width :reader width)
   (depth :initarg :depth :reader depth)))

(defmethod make-seat (table-side index)
  (make-instance
   'seat
   :table-side table-side
   :index index))

(defun make-table (width depth seats)
  (make-instance
   'table
   :width width
   :depth depth
   :seats seats))

(defun table-template-4seat ()
  (make-table
   1.0 2.0
   `(,(make-seat :west 0)
     ,(make-seat :west 1)
     ,(make-seat :east 0)
     ,(make-seat :east 1))))

(defun draw-table (table)
  (ql:quickload "vecto")
  (ql:quickload "flexi-streams")
  (vecto:with-canvas (:width 100 :height 100)
    (vecto:set-rgb-fill 1.0 0.65 0.3)
    (vecto:rounded-rectangle 0 0 100 100 10 10)
    (vecto:fill-path)
    (flexi-streams:with-output-to-sequence (output)
      (vecto:save-png-stream output))))

(defun create-png-image-message (data)
  (roslisp:make-message
   "sensor_msgs/CompressedImage"
   :format "png"
   :data data))

(defun test-state-informer-png ()
  (let ((adv (roslisp:advertise "/state_informer/compressed_image_in/compressed" "sensor_msgs/CompressedImage")))
    (roslisp:publish
     adv
     (create-png-image-message
      (flexi-streams:with-output-to-sequence (output)
        (flexi-streams:with-input-from-sequence
            (input (draw-table nil))
          (png:encode (png:decode input) output)))))))
