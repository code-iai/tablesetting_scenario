;;; Copyright (c) 2015, Jan Winkler <winkler@cs.uni-bremen.de>
;;; All rights reserved.
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions are met:
;;;
;;; * Redistributions of source code must retain the above copyright
;;; notice, this list of conditions and the following disclaimer.
;;; * Redistributions in binary form must reproduce the above copyright
;;; notice, this list of conditions and the following disclaimer in the
;;; documentation and/or other materials provided with the distribution.
;;; * Neither the name of the Institute for Artificial Intelligence/
;;; Universitaet Bremen nor the names of its contributors may be used to 
;;; endorse or promote products derived from this software without specific 
;;; prior written permission.
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

;;;
;;; Top-Level Plans
;;;

(defun task-detail (task detail)
  (cadr (find detail task
              :test (lambda (subject list-item)
                      (eql subject (car list-item))))))

(def-top-level-cram-function tablesetting-highlight ()
  (set-random-scene)
  (let* ((tablesetting-task (required-scene-objects))
         (reordered (order-scene-objects tablesetting-task))
         (seats
           (remove-if-not
            #'cadr
            (mapcar (lambda (object)
                      (let* ((at (desig-prop-value object 'desig-props:at))
                             (seat (1- (desig-prop-value at 'seat)))
                             (center-of (desig-prop-value at 'center-of))
                             (left-of (desig-prop-value at 'left-of))
                             (right-of (desig-prop-value at 'right-of))
                             (behind-of (desig-prop-value at 'behind-of))
                             (seat-specializer
                               (cond (center-of "center")
                                     ((and left-of behind-of) "left-back")
                                     ((and right-of behind-of) "right-back")
                                     (left-of "left")
                                     (right-of "right")
                                     (behind-of "back"))))
                        `(,(indexed-table-seat seat) ,seat-specializer)))
                    reordered))))
    (display-table-scene :highlight seats)))

(def-top-level-cram-function tablesetting ()
  (set-random-scene)
  (let* ((tablesetting-task (required-scene-objects))
         (reordered (order-scene-objects tablesetting-task)))
    (dolist (object reordered)
      (let* ((new-location (object-location object))
             (refactored-object
              (copy-designator
               object
               :new-description
               (append `((at ,new-location))
                       (object->perception-task object)))))
        (fetch-object refactored-object)
        (let ((target-location (desig-prop-value object 'at)))
          (place-object refactored-object target-location))))))

(defun object-location (object)
  (let ((type (desig-prop-value object 'type)))
    (make-designator
     'location
     (location-for-object-type type))))

(defun location-for-object-type (type)
  (case type
    (cup `((in cupboard)
           (name "cupboard-1")))
    (milkbox `((in fridge)))
    (plate `((in cupboard)
             (name "cupboard-1")))
    (knife `((in drawer)
             (name "drawer-1")))
    (spoon `((in drawer)
             (name "drawer-1")))
    (glass `((in cupboard)
             (name "cupboard-1")))))

(defun object->perception-task (object)
  (let ((type (desig-prop-value object 'desig-props:type)))
    (append `((type ,type))
            (case type
              (cup `((shape round) (size small) (color white)))
              (milkbox `((shape box) (color blue) (size medium)))
              (plate `((shape flat) (color white)))
              (knife `((shape flat) (color red)))
              (spoon `((shape flat) (color red)))
              (glass `((shape round) (size small) (color white)))))))

(def-cram-function fetch-object (object)
  (let* ((potential-residence (desig-prop-value object 'at))
         (perception-task (object->perception-task object))
         (desc (description potential-residence)))
    (articulate-environment :before desc)
    (format t "Fetching ~a from ~a~%" (desig-prop-value object 'type) desc)
    (format t "Perception task: ~a~%" perception-task)
    (articulate-environment :after desc)))

(def-cram-function articulate-environment (modifier desc)
  (case modifier
    (:before (cond ((find `(in drawer) desc :test #'equal)
                    (open-drawer desc))
                   ((find `(in fridge) desc :test #'equal)
                    (open-fridge desc))
                   ((find `(in cupboard) desc :test #'equal)
                    (open-cupboard desc))
                   ((find `(on table) desc :test #'equal)
                    (approach-table))))
    (:after (cond ((find `(in drawer) desc :test #'equal)
                   (close-drawer desc))
                  ((find `(in fridge) desc :test #'equal)
                   (close-fridge desc))
                  ((find `(in cupboard) desc :test #'equal)
                   (close-cupboard))
                  ((find `(on table) desc :test #'equal))))))

(def-cram-function approach (desc)
  (cpl:with-retry-counters ((nav-retry-counter 3))
    (cpl:with-failure-handling
        ((cram-plan-failures:location-not-reached-failure (f)
           (declare (ignore f))
           (cpl:do-retry nav-retry-counter
             (cpl:retry))))
      ;; TODO(winkler): Navigate here.
      )))

(def-cram-function approach-table (desc)
  (approach desc)
  (format t "Approaching table~%"))

(def-cram-function open-drawer (desc)
  (cpl:with-retry-counters ((retry-counter 3))
    (cpl:with-failure-handling
        ((cram-plan-failures:manipulation-pose-unreachable (f)
           (declare (ignore f))
           (cpl:do-retry retry-counter
             (cpl:retry))))
      (approach desc)
      (format t "Opening drawer~%")
      ;; TODO(winkler): Open drawer
      )))

(def-cram-function open-fridge (desc)
  (cpl:with-retry-counters ((retry-counter 3))
    (cpl:with-failure-handling
        ((cram-plan-failures:manipulation-pose-unreachable (f)
           (declare (ignore f))
           (cpl:do-retry retry-counter
             (cpl:retry))))
      (approach desc)
      (format t "Opening fridge~%")
      ;; TODO(winkler): Open fridge
      )))

(def-cram-function open-cupboard (desc)
  (cpl:with-retry-counters ((retry-counter 3))
    (cpl:with-failure-handling
        ((cram-plan-failures:manipulation-pose-unreachable (f)
           (declare (ignore f))
           (cpl:do-retry retry-counter
             (cpl:retry))))
      (approach desc)
      (format t "Opening cupboard~%")
      ;; TODO(winkler): Open cupboard
      )))

(def-cram-function close-drawer (desc)
  (cpl:with-retry-counters ((retry-counter 3))
    (cpl:with-failure-handling
        ((cram-plan-failures:manipulation-pose-unreachable (f)
           (declare (ignore f))
           (cpl:do-retry retry-counter
             (cpl:retry))))
      (approach desc)
      (format t "Closing drawer~%")
      ;; TODO(winkler): Close drawer
      )))

(def-cram-function close-fridge (desc)
  (cpl:with-retry-counters ((retry-counter 3))
    (cpl:with-failure-handling
        ((cram-plan-failures:manipulation-pose-unreachable (f)
           (declare (ignore f))
           (cpl:do-retry retry-counter
             (cpl:retry))))
      (approach desc)
      (format t "Closing fridge~%")
      ;; TODO(winkler): Close fridge
      )))

(def-cram-function close-cupboard (desc)
  (cpl:with-retry-counters ((retry-counter 3))
    (cpl:with-failure-handling
        ((cram-plan-failures:manipulation-pose-unreachable (f)
           (declare (ignore f))
           (cpl:do-retry retry-counter
             (cpl:retry))))
      (approach desc)
      (format t "Closing cupboard~%")
      ;; TODO(winkler): Close cupboard
      )))

(def-cram-function ts-place-object (object location)
  (approach (description location))
  (format t "Placing ~a at seat ~a~%"
          (desig-prop-value object 'type)
          (desig-prop-value location 'seat)))

(def-top-level-cram-function prepare-scene (&key (object-type "Cup"))
  (prepare-settings)
  (move-arms-away)
  (move-torso)
  ;(spawn-new-instance-at-common-place object-type)
  )

(def-top-level-cram-function tablesetting-test (&key (runs 1))
  (with-simulation-process-modules
    (prepare-settings)
    (move-torso)
    (move-arms-away)
    (loop for i from 0 below runs
          do (with-designators ((cup (object `((desig-props:type "Cup")))))
               (cpl:with-failure-handling
                   ((cram-plan-failures:location-not-reached-failure (f)
                      (declare (ignore f))
                      (cpl:retry)))
                 (perceive-a cup))
               (cpl:with-failure-handling
                   ((cram-plan-failures:location-not-reached-failure (f)
                      (declare (ignore f))
                      (cpl:retry)))
                 (pick-object cup))
               (with-designators ((place (location
                                          `((desig-props:on Cupboard)
                                            (desig-props:name "kitchen_island")))))
                 (let ((place-seat
                         (combine-locations
                          place
                          (seat-location :west 1 2 "left"))))
                   (cpl:with-failure-handling
                       ((cram-plan-failures:location-not-reached-failure (f)
                          (declare (ignore f))
                          (cpl:retry)))
                     (place-object cup place-seat))))))))

(defun remove-index (sequence index)
  (cond ((= index 0)
         (subseq sequence 1))
        ((= index (1- (length sequence)))
         (subseq sequence 0 (1- (length sequence))))
        (t (concatenate (type-of sequence)
                        (subseq sequence 0 index)
                        (subseq sequence (1+ index))))))

(defun cleanup-everything! ()
  (detach-all)
  (moveit::detach-all-attachments)
  (moveit:clear-collision-environment)
  (loop for object in (tablesetting-objects)
        do (retract-tablesetting-object object)))

(def-top-level-cram-function tablesetting-test-seq ()
  (cleanup-everything!)
  (let ((table (make-designator 'location
                                `((desig-props:on Cupboard)
                                  (desig-props:name "kitchen_island"))))
        (source-hint "kitchen_sink_block")
        (objects `(("Knife" ,(seat-location :west 1 2 "right"))
                   ("Plate" ,(seat-location :west 1 2 "center"))
                   ("Cup" ,(seat-location :west 1 2 "left-back"))
                   )))
    (with-simulation-process-modules
      (prepare-settings)
      (move-torso)
      (move-arms-away)
      (loop while objects
            do (let* ((random-index (random (length objects)))
                      (random-object (elt objects random-index)))
                 (destructuring-bind (object-type destination-place) random-object
                   (spawn-new-instance-at-common-place object-type :hint source-hint)
                   (with-designators ((loc (location `((desig-props::on Cupboard)
                                                       (desig-props::name ,source-hint))))
                                      (object (object `((desig-props:type ,object-type)
                                                        (desig-props:at ,loc)))))
                     (cpl:with-failure-handling
                         ((cram-plan-failures:location-not-reached-failure (f)
                            (declare (ignore f))
                            (cpl:retry)))
                       (perceive-a object))
                     (cpl:with-failure-handling
                         (((or cram-plan-failures:location-not-reached-failure
                               cram-plan-failures:manipulation-pose-unreachable) (f)
                            (declare (ignore f))
                            (cpl:retry)))
                       (pick-object object))
                     (let ((place (combine-locations table destination-place)))
                       (cpl:with-failure-handling
                           (((or cram-plan-failures:location-not-reached-failure
                                 cram-plan-failures:manipulation-pose-unreachable) (f)
                              (declare (ignore f))
                              (cpl:retry)))
                         (place-object object place)
                         (setf objects (remove-index objects random-index)))))))))))

(defun spawn-object (&key (object "Cup"))
  (spawn-new-instance-at-common-place object))

(defun remove-object (object side)
  (moveit:detach-collision-object-from-link object
   (case side
     (:left "l_wrist_roll_link")
     (:right "r_wrist_roll_link")))
  (moveit:remove-collision-object object)
  (detach object side)
  (cram-gazebo-utilities::delete-gazebo-model object))
