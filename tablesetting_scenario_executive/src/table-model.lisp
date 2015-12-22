;;; Copyright (c) 2015, Jan Winkler <winkler@cs.uni-bremen.de>
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

;(ql:quickload "vecto")
;(ql:quickload "flexi-streams")
;(ql:quickload "png")

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

;; (defun draw-table (table)
;;   (vecto:with-canvas (:width 100 :height 100)
;;     (vecto:set-rgb-fill 1.0 0.65 0.3)
;;     (vecto:rounded-rectangle 0 0 100 100 10 10)
;;     (vecto:fill-path)
;;     (flexi-streams:with-output-to-sequence (output)
;;       (vecto:save-png-stream output))))

(defun create-png-image-message (data)
  (roslisp:make-message
   "sensor_msgs/CompressedImage"
   :format "png"
   :data data))

;; (defun test-state-informer-png ()
;;   (let ((adv (roslisp:advertise "/state_informer/compressed_image_in/compressed" "sensor_msgs/CompressedImage")))
;;     (roslisp:publish
;;      adv
;;      (create-png-image-message
;;       (flexi-streams:with-output-to-sequence (output)
;;         (flexi-streams:with-input-from-sequence
;;             (input (draw-table nil))
;;           (png:encode (png:decode input) output)))))))

(defun remove-marker (id)
  (desig-int:call-designator-service
   "/state_informer/control"
   (make-designator
    'action
    `((command "remove")
      (id ,id)))))

(defun display-box (id pose dimensions colors)
  (desig-int:call-designator-service
   "/state_informer/control"
   (make-designator
    'action
    `((command "add")
      (what "box")
      (id ,id)
      (where ,pose)
      (dimensions
       ((width ,(first dimensions))
        (height ,(second dimensions))
        (depth ,(third dimensions))))
      (colors
       ((r ,(first colors))
        (g ,(second colors))
        (b ,(third colors))
        (a ,(fourth colors))))))))

(defun display-mesh (id pose path &key (rotation (tf:euler->quaternion)) (scale 1.0))
  (desig-int:call-designator-service
   "/state_informer/control"
   (make-designator
    'action
    `((command "add")
      (what "mesh")
      (id ,id)
      (where ,(cl-transforms:transform-pose
               (tf:pose->transform pose)
               (tf:make-pose
                (tf:make-3d-vector 0 0 0)
                rotation)))
      (details
       ((path ,path)
        (scale ,scale)))))))

(defun display-candlestick (id x y z)
  (display-mesh
   id
   (tf:make-pose
    (tf:make-3d-vector x y z)
    (tf:euler->quaternion))
   "accessories/candlestick/candlestick.stl"
   :rotation (tf:euler->quaternion :ax (/ pi 2))
   :scale 0.03))

(defun table-pose ()
  (let* ((sem-obj (first
                   (sem-map-utils:designator->semantic-map-objects
                    (make-designator 'location `((desig-props:name "kitchen_island"))))))
         (table-pose (sem-map-utils:pose sem-obj)))
    table-pose))

(defun table-dimensions ()
  (let* ((sem-obj (first
                   (sem-map-utils:designator->semantic-map-objects
                    (make-designator 'location `((desig-props:name "kitchen_island"))))))
         (dimensions (sem-map-utils:dimensions sem-obj)))
    dimensions))

(defun seat-dimensions ()
  (tf:make-3d-vector 0.6 0.4 0.0))

(defun table-relative-pose (relative-pose table-pose)
  (cl-transforms:transform-pose
   (tf:pose->transform table-pose)
   relative-pose))

(defun seat-pose (side index max seat-dimensions table-dimensions table-pose)
  (let ((rotation (tf:euler->quaternion
                   :az (ecase side
                         (:south 0.0)
                         (:north pi)
                         (:west (/ pi -2))
                         (:east (/ pi 2)))))
        (x (ecase side
             ;; Kind of hackish, but it works.
             (:south 0);;(- (* (/ index max)
             ;;    table-width)
             ;; (/ table-width (* 2 max))))
             (:north 0);;(- (* (/ index max)
             ;;    table-width)
             ;; (/ table-width (* max 2))))
             (:west (- (/ (tf:y seat-dimensions) 2)
                       (/ (tf:x table-dimensions) 2)
                       -0.02))
             (:east (- (/ (tf:x table-dimensions) 2)
                       (/ (tf:y seat-dimensions) 2)
                       0.02))))
        (y (ecase side
             (:south (- (/ (tf:y seat-dimensions) 2)
                        (/ (tf:y table-dimensions) 2)
                        -0.02))
             (:north (- (/ (tf:y table-dimensions) 2)
                        (/ (tf:y seat-dimensions) 2)
                        0.02))
             (:west (- (* (/ index max)
                          (tf:y table-dimensions))
                       (/ (tf:y table-dimensions) (* max 2))))
             (:east (- (* (/ index max)
                          (tf:y table-dimensions))
                       (/ (tf:y table-dimensions) (* max 2))))))
        (z (+ 0.005
              (/ (tf:x table-dimensions) 2))))
    (table-relative-pose (tf:make-pose
                          (tf:make-3d-vector x y z)
                          rotation)
                         table-pose)))
                 
(defun display-table-scene (&key highlight
                              (size (tf:make-3d-vector 1.0 2.0 0.8))
                              (table-pose (tf:make-pose
                                           (tf:make-3d-vector 0 0 (/ (tf:z size) 2))
                                           (tf:euler->quaternion)))
                              (seat-dimensions (seat-dimensions)))
  (let* ((table-width (tf:x size))
         (table-depth (tf:y size))
         (table-height (tf:z size))
         (seat-width (tf:x seat-dimensions))
         (seat-depth (tf:y seat-dimensions)))
    (display-candlestick "candle-1" 0.1 0.0 table-height)
    (display-candlestick "candle-2" 0.0 0.1 table-height)
    (display-candlestick "candle-3" -0.1 -0.1 table-height)
    (display-table table-pose table-width table-depth)
    (let ((table-definition (table-definition)))
      (dolist (def table-definition)
        (destructuring-bind (side max-seats) def
          (loop for seat from 0 below max-seats
                as id = (table-seat-id def seat)
                as highlight-current = (loop for h in highlight
                                             when (string=
                                                   (first h) id)
                                               collect (second h))
                do (display-seat
                    id (seat-pose side seat max-seats
                                  (tf:make-3d-vector seat-width seat-depth 0.0)
                                  size table-pose)
                    seat-width seat-depth
                    :highlight highlight-current)))))))

(defun seat-location (side index max)
  (make-designator
   'location `((desig-props::seat-side ,side)
               (desig-props::seat-index ,index)
               (desig-props::seats-max ,max))))

(defun table-seat-id (def index)
  (destructuring-bind (side max-seats) def
    (declare (ignore max-seats))
    (concatenate
     'string
     "seat-"
     (subseq (string-downcase (write-to-string side)) 1)
     "-"
     (write-to-string index))))

(defun table-definition ()
  `((:west 2)))

(defmethod get-sem-map-obj (name)
  (let* ((parts (slot-value (sem-map-utils:get-semantic-map) 'sem-map-utils::parts))
         (part (gethash name parts)))
    part))

(defun indexed-table-seat (index)
  (let* ((definitions (table-definition))
         (expansion
           (loop for definition in definitions
                 append
                 (loop for i from 0 below (second definition)
                       collect (table-seat-id definition i)))))
    (nth index expansion)))

(defun display-table (pose width depth)
  (display-box
   "table"
   pose `(,width 0.8 ,depth) '(1.0 1.0 1.0 0.5)))

(defun dataset (identifier width depth)
  (let ((back-fraction 0.25)
        (side-fraction 0.25)
        (data (make-hash-table :test 'equal)))
    (labels ((add-dataset (id coordinates dimensions color)
               (setf (gethash id data)
                     `(,coordinates ,dimensions ,color))))
      (add-dataset
       "back"
       `(0 ,(* depth (* back-fraction 1.5)))
       `(,(* (- 1 (* 2 side-fraction)) width)
         ,(* depth back-fraction))
       `(1.0 0.0 1.0))
      (add-dataset
       "center"
       `(0 ,(- (* depth back-fraction 0.5)))
       `(,(* (- 1 (* 2 side-fraction)) width)
         ,(* depth (- 1 back-fraction)))
       `(1.0 1.0 0.0))
      (add-dataset
       "right"
       `(,(* width side-fraction 1.5)
         ,(- (* depth back-fraction 0.5)))
       `(,(* side-fraction width)
         ,(* depth (- 1 back-fraction)))
       `(1.0 0.0 0.0))
      (add-dataset
       "left"
       `(,(- (* width side-fraction 1.5))
         ,(- (* depth back-fraction 0.5)))
       `(,(* side-fraction width)
         ,(* depth (- 1 back-fraction)))
       `(0.0 0.0 1.0))
      (add-dataset
       "left-back"
       `(,(- (* width side-fraction 1.5))
         ,(* depth (* back-fraction 1.5)))
       `(,(* side-fraction width)
         ,(* depth back-fraction))
       `(0.0 1.0 1.0))
      (add-dataset
       "right-back"
       `(,(* width side-fraction 1.5)
         ,(* depth (* back-fraction 1.5)))
       `(,(* side-fraction width)
         ,(* depth back-fraction))
       `(0.0 1.0 0.0)))
    (gethash identifier data)))

(defun relative-seat-pose (identifier width depth pose)
  )

(defun display-seat (id pose width depth &key highlight)
  (labels ((seat-relative-pose (relative-pose)
             (cl-transforms:transform-pose
              (tf:pose->transform pose)
              relative-pose))
           (seat-relative-id (relative-id)
             (concatenate 'string id "-" relative-id))
           (display-dataset (id)
             (let* ((dataset (dataset id width depth))
                    (coordinates (first dataset))
                    (dimensions (second dataset))
                    (color (third dataset)))
               (display-box
                (seat-relative-id id)
                (seat-relative-pose
                 (tf:make-pose
                  (tf:make-3d-vector
                   (first coordinates)
                   (second coordinates)
                   0.0)
                  (tf:euler->quaternion)))
                `(,(first dimensions) 0.005 ,(second dimensions))
                (cond ((find id highlight :test #'string=)
                       (append color `(1.0)))
                      (t (let ((dim-factor 0.4))
                           `(,(* dim-factor (first color))
                             ,(* dim-factor (second color))
                             ,(* dim-factor (third color))
                             1.0))))))))
    (loop for id in `("back" "center" "right" "left" "left-back" "right-back")
          do (display-dataset id))))
