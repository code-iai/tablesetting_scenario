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

(def-top-level-cram-function tablesetting ()
  (let* ((tablesetting-task (task))
         (state-start nil)
         (state-goal nil)
         (action-sequence (task->action-sequence
                           task state-start state-goal))
         (loc-1 (make-designator
                 'location
                 `((:on :table)
                   (:name "table_1")
                   (:side :west)
                   (:seat-index 0)
                   (:location-hint :back)
                   (:location-hint :left)))))
    
    tablesetting-task))

(def-cram-function fetch-object (object)
  )

(def-cram-function place-object (object place)
  )

(defun task ()
  `((:task-type :tablesetting)
    (:task-details
     (:guests (tim mary))
     (:weekday :monday)
     (:meal-time :breakfast))))

(defun task->action-seqeuence (task state-start state-goal)
  (ecase (task-detail task :task-type)
    (:tablesetting
     )))
