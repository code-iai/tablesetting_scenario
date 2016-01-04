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

(defun make-scenario-area-restriction-cost-function ()
  (let ((min-x -1.0)
        (max-x 1.55)
        (min-y 0.3)
        (max-y 1.8))
    (lambda (x y)
      (if (and (>= x min-x)
               (<= x max-x)
               (>= y min-y)
               (<= y max-y))
          (if (and (>= x 0.0)
                   (>= y 0.7))
              0.0d0
              1.0d0)
          0.0d0))))

(defun make-scenario-in-front-of-seat-distribution-cost-function (seat)
  )

(defmethod costmap-generator-name->score
    ((name (common-lisp:eql 'scenario-area-restriction-distribution)))
  100)

(defmethod costmap-generator-name->score
    ((name (common-lisp:eql 'scenario-in-front-of-seat-distribution)))
  101)

(defmethod costmap-generator-name->score
    ((name (common-lisp:eql 'seat-area-distribution)))
  102)

(defun object-color (colors color)
  (let ((color-pair (find color colors :test (lambda (x y) (eql x (car y))))))
    (if color-pair
        (cadr color-pair)
        0.0d0)))

(defun make-handles (distance-from-center
                     &key
                       (segments 1)
                       (ax 0.0) (ay 0.0) (az 0.0)
                       (offset-angle 0.0)
                       grasp-type
                       (center-offset
                        (tf:make-identity-vector)))
  (loop for i from 0 below segments
        as current-angle = (+ (* 2 pi (float (/ i segments)))
                              offset-angle)
        as handle-pose = (tf:make-pose
                          (tf:make-3d-vector
                           (+ (* distance-from-center (cos current-angle))
                              (tf:x center-offset))
                           (+ (* distance-from-center (sin current-angle))
                              (tf:y center-offset))
                           (+ 0.0
                              (tf:z center-offset)))
                          (tf:euler->quaternion
                           :ax ax :ay ay :az (+ az current-angle)))
        as handle-object = (make-designator
                            'cram-designators:object
                            (append
                             `((desig-props:type desig-props:handle)
                               (desig-props:at
                                ,(a location `((desig-props:pose
                                                ,handle-pose)))))
                             (when grasp-type
                               `((desig-props:grasp-type ,grasp-type)))))
        collect handle-object))

(defun make-seat-area-distribution-cost-function (side index max section)
  (let ((table-pose (table-pose))
        (table-dimensions (table-dimensions))
        (seat-dimensions (seat-dimensions)))
    (lambda (x y)
      (multiple-value-bind (pose dimensions)
          (seat-pose side index max section
                     seat-dimensions table-dimensions table-pose)
        (if (and (>= x (- (tf:x (tf:origin pose)) (/ (tf:y dimensions) 2)))
                 (<= x (+ (tf:x (tf:origin pose)) (/ (tf:y dimensions) 2)))
                 (>= y (- (tf:y (tf:origin pose)) (/ (tf:x dimensions) 2)))
                 (<= y (+ (tf:y (tf:origin pose)) (/ (tf:x dimensions) 2))))
            1.0d0
            0.0d0)))))

(def-fact-group scenario-costmap-area-restriction (desig-costmap)

  (<- (desig-costmap ?desig ?cm)
    (desig-prop ?desig (desig-props::seat-side ?side))
    (desig-prop ?desig (desig-props::seat-index ?index))
    (desig-prop ?desig (desig-props::seats-max ?max))
    (crs:once
     (or (desig-prop ?desig (desig-props::seat-section ?section))
         (equal ?section nil)))
    (costmap ?cm)
    (costmap-add-function
     seat-area-distribution
     (make-seat-area-distribution-cost-function
      ?side ?index ?max ?section)
     ?cm))
  
  (<- (desig-costmap ?desig ?cm)
    (desig-prop ?desig (desig-props:in-front-of desig-props:seat))
    (desig-prop ?desig (desig-props:seat ?seat))
    (costmap ?cm)
    (costmap-add-function
     scenario-in-front-of-seat-distribution
     (make-scenario-in-front-of-seat-distribution-cost-function ?seat)))
  
  (<- (desig-costmap ?desig ?cm)
    (or (desig-prop ?desig (desig-props:to desig-props:see))
        (desig-prop ?desig (desig-props:to desig-props:reach))
        (desig-prop ?desig (desig-props:on Cupboard)))
    (costmap ?cm)
    (costmap-add-function scenario-area-restriction-distribution
                          (make-scenario-area-restriction-cost-function)
                          ?cm)))

(def-fact-group inference-facts (infer-object-property object-handle)
  
  (<- (make-handle ?segments ?offset-angle ?handle)
    (symbol-value pi ?pi)
    (crs:lisp-fun / ?pi 2 ?pi-half)
    (make-handle 0.04 ?segments ?offset-angle 'desig-props::push
                  ?pi-half 0 0 0 0 0 ?handle))
  
  (<- (make-handle ?distance-from-center ?segments ?offset-angle ?grasp-type
                   ?hand-ax ?hand-ay ?hand-az ?co-x ?co-y ?co-z ?handle)
    (crs:lisp-fun tf:make-3d-vector ?co-x ?co-y ?co-z ?co)
    (crs:lisp-fun make-handles ?distance-from-center
                  :segments ?segments
                  :offset-angle ?offset-angle
                  :grasp-type ?grasp-type
                  :ax ?hand-ax
                  :ay ?hand-ay
                  :az ?hand-az
                  :center-offset ?co
                  ?handles)
    (member ?handle ?handles))
  
  (<- (object-color ?object ?color ?value)
    (desig-prop ?object (desig-props:color ?colors))
    (crs:lisp-fun object-color ?colors ?color ?value))
  
  (<- (infer-object-property ?object desig-props:type desig-props::pancakemix)
    (object-color ?object desig-props:yellow ?yellow)
    (> ?yellow 0.3))
  
  (<- (infer-object-property ?object desig-props:type desig-props::bowl)
    (object-color ?object desig-props:white ?white)
    (desig-prop ?object (desig-props::shape desig-props::round))
    (desig-prop ?object (desig-props::shape desig-props::flat))
    (> ?white 0.8))
  
  (<- (infer-object-property ?object desig-props:type desig-props::milk)
    (object-color ?object desig-props:white ?white)
    (> ?white 0.8))
  
  (<- (infer-object-property ?object desig-props:shape desig-props::box)
    (desig-prop ?object (desig-props::type desig-props::milk)))
  
  (<- (infer-object-property ?object desig-props:dimensions ?dim)
    (desig-prop ?object (desig-props::type desig-props::milk))
    (crs:lisp-fun vector 0.05 0.08 0.15 ?dim))
  
  (<- (infer-object-property ?object desig-props:handle ?handle)
    (crs:once
     (or (desig-prop ?object (desig-props:type ?type))
         (infer-object-property ?object desig-props:type ?type)))
    (object-handle ?type ?handle))

  (<- (infer-object-property ?object desig-props::carry-handles ?handles)
    (crs:once
     (or (desig-prop ?object (desig-props:type ?type))
         (infer-object-property ?object desig-props:type ?type)))
    (object-carry-handles ?type ?handles))
  
  (<- (show-handle ?symbol ?handle)
    (desig-prop ?handle (at ?at))
    (desig-prop ?at (pose ?pose))
    (format "~a: ~a~%" ?symbol ?pose))
  
  ;; Bowl handles begin
  (<- (object-handle desig-props::bowl ?handle)
    (symbol-value pi ?pi)
    (crs:lisp-fun / ?pi 2 ?pi-half)
    (crs:lisp-fun / ?pi 4 ?pi-quarter)
    (make-handle -0.06 1 ?pi-half push 0.0 ?pi-quarter 0 0.0 0.0 -0.01 ?handle))
  
  (<- (object-handle desig-props::bowl ?handle)
    (symbol-value pi ?pi)
    (crs:lisp-fun / ?pi 2 ?pi-half)
    (crs:lisp-fun * 3 ?pi-half ?three-pi-half)
    (crs:lisp-fun / ?pi 4 ?pi-quarter)
    (make-handle -0.06 1 ?three-pi-half push 0.0 ?pi-quarter 0 0.0 0.0 -0.01 ?handle))
  ;; Bowl handles end

  (<- (object-handle "Cup" ?handle)
    (symbol-value pi ?pi)
    (crs:lisp-fun / ?pi 2 ?pi-half)
    (make-handle -0.02 1 ?pi-half push ?pi-half 0.0 0 0.0 0.0 0.05 ?handle))

  (<- (object-handle desig-props:pancakemix ?handle)
    (symbol-value pi ?pi)
    (crs:lisp-fun / ?pi 2 ?pi-half)
    (make-handle 1 ?pi-half ?handle))
  
  (<- (object-handle desig-props:pancakemix ?handle)
    (symbol-value pi ?pi)
    (crs:lisp-fun / ?pi 2 ?pi-half)
    (crs:lisp-fun * ?pi-half 3 ?three-pi-half)
    (make-handle 1 ?three-pi-half ?handle))
  
  (<- (object-handle desig-props::milk ?handle)
    (symbol-value pi ?pi)
    (crs:lisp-fun / ?pi 2 ?pi-half)
    (make-handle 0.04 1 ?pi-half desig-props::push ?pi-half 0 0 0 0 0 ?handle))
    ;(make-handle 1 ?pi-half ?handle))
  
  (<- (object-handle desig-props::milk ?handle)
    (symbol-value pi ?pi)
    (crs:lisp-fun / ?pi 2 ?pi-half)
    (crs:lisp-fun * ?pi-half 3 ?three-pi-half)
    (make-handle 0.08 1 ?three-pi-half desig-props::push ?pi-half 0 0 0 0 0 ?handle))
    ;(make-handle 1 ?three-pi-half ?handle))
  
  (<- (object-carry-handles desig-props::pancakemix 1))
  (<- (object-carry-handles desig-props::milk 1))
  (<- (object-carry-handles desig-props::bowl 1)))

(def-fact-group occassions (holds)

  (<- (object-picked-from-rack ?rack ?object)
    (crs:fail))

  (<- (objects-detected-in-rack ?rack ?object-template)
    (crs:fail))

  (<- (rack-scene-perceived)
    (crs:fail))
  
  (<- (object-handover ?object ?target-hand)
    (not (pr2-manip-pm::object-in-hand ?object ?target-hand)))
  
  (<- (object-placed-on-rack ?object ?level ?x ?y)
    (crs:fail)))
