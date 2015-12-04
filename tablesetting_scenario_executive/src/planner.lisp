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

(defvar *tic* 0.0)

(defun states-equal? (state-1 state-2 &key relaxed)
  ;; TODO(winkler): Implement this for tablesetting scenarios
  )

(defun heuristic-cost-estimate (state-1 state-2 &optional transition)
  ;; TODO(winkler): Implement this for tablesetting scenarios
  )

(defun apply-transition (state transition)
  ;; TODO(winkler): Implement this for tablesetting scenarios
  )

(defun make-transitions (state goal-state)
  ;; TODO(winkler): Implement this for tablesetting scenarios
  )

(defmacro setstate (key-state state-map value)
  `(let ((new-state-map
           (append
            (remove-if (lambda (list-item)
                         (states-equal? ,key-state (first list-item)))
                       ,state-map)
            `((,,key-state ,,value)))))
     (setf ,state-map new-state-map)))

(defun getstate (key-state state-map &optional default relaxed)
  (let ((value (second
                (find key-state state-map
                      :test (lambda (subject list-item)
                              (states-equal? subject (first list-item)
                                             :relaxed relaxed))))))
    (or value default)))

(defun tic ()
  (setf *tic* (/ (get-internal-real-time) 1000.0)))

(defun toc()
  (- (/ (get-internal-real-time) 1000.0) *tic*))

(defun make-state-map ()
  `())

(defun reconstruct-path (came-from state &optional inject-into-came-from)
  (let ((first t)
        (total-path `(,state))
        (came-from (cond (inject-into-came-from
                          (let ((came-from-copy came-from))
                            (setstate state came-from-copy
                                      inject-into-came-from)
                            came-from-copy))
                         (t came-from))))
    (loop while (getstate state came-from nil first)
          do (setf state (getstate state came-from nil first))
             (push state total-path)
             (setf first nil))
    total-path))

(defun statekeys (state-map)
  (loop for state in state-map
        collect (first state)))

(defun lowest-score-key (state-map)
  (let ((lowest-score 10000)
        (lowest-key nil))
    (loop for key in (statekeys state-map)
          when (< (getstate key state-map) lowest-score)
            do (setf lowest-score (getstate key state-map))
               (setf lowest-key key))
    lowest-key))

(defun lowest-state-score (states scores)
  (let ((lowest-score most-positive-fixnum)
        (lowest-state nil))
    (cond ((= (length states) 1)
           (setf lowest-score (first scores))
           (setf lowest-state (first states)))
          (t (loop for state in states
                   when (< (getstate state scores) lowest-score)
                     do (setf lowest-score (getstate state scores))
                        (setf lowest-state state))))
    (values lowest-state lowest-score)))

(defun modified-a-star (start-state goal-state &key (number-of-solutions 1))
  (let ((closed-set `())
        (open-set `(,start-state))
        (came-from (make-state-map))
        (g-score (make-state-map))
        (f-score (make-state-map))
        (transitions-map `())
        (solutions `())
        (solver-durations `()))
    (setstate start-state g-score 0)
    (setstate start-state f-score
              (+ (getstate start-state g-score)
                 (heuristic-cost-estimate start-state goal-state)))
    (block a-star-main
      (tic)
      (loop while open-set
            as current-state = (multiple-value-bind (state score)
                                   (lowest-state-score open-set f-score)
                                 (declare (ignore score))
                                 state)
            if (progn
                 (states-equal? current-state goal-state :relaxed t))
              do (push `(,(reconstruct-path
                           came-from
                           current-state)
                         ,transitions-map)
                       solutions)
                 (push (toc) solver-durations)
                 (return-from a-star-main)
            else
              do (setf open-set (remove-if
                                 (lambda (subject-state)
                                   (states-equal? subject-state current-state))
                                 open-set))
                 (push current-state closed-set)
                 (loop for transition in (make-transitions
                                          current-state goal-state)
                       as projected-state = (apply-transition current-state
                                                              transition)
                       do (unless (find projected-state closed-set
                                        :test #'states-equal?)
                            (push `(,current-state ,projected-state ,transition)
                                  transitions-map)
                            (let ((tentative-g-score
                                    (+ (getstate current-state g-score
                                                 most-positive-fixnum)
                                       (distance-between current-state
                                                         projected-state)
                                       (cond ((states-equal? projected-state goal-state
                                                             :relaxed t)
                                              (if (= number-of-solutions 1)
                                                  0
                                                  (progn
                                                    (format t "~a more solution~a to generate~%"
                                                            number-of-solutions
                                                            (cond ((= number-of-solutions 1) "")
                                                                  (t "s")))
                                                    (decf number-of-solutions)
                                                    (push `(,(reconstruct-path
                                                              came-from
                                                              projected-state
                                                              current-state)
                                                            ,transitions-map)
                                                          solutions)
                                                    (push (toc) solver-durations)
                                                    (tic)
                                                    most-positive-fixnum)))
                                             (t 0)))))
                              (block intermediate-check
                                (if (not (find projected-state open-set
                                               :test #'states-equal?))
                                    (push projected-state open-set)
                                    (when (>= tentative-g-score
                                              (getstate projected-state g-score
                                                        most-positive-fixnum))
                                      (return-from intermediate-check)))
                                (setstate projected-state came-from
                                          current-state)
                                (setstate projected-state g-score
                                          tentative-g-score)
                                (setstate projected-state f-score
                                          (+ (getstate projected-state g-score
                                                       most-positive-fixnum)
                                             (heuristic-cost-estimate
                                              projected-state goal-state
                                              transition))))))))
      (format t "FAILURE~%"))
    (mapcar (lambda (solution duration)
              (let ((steps (first solution))
                    (transitions (second solution)))
                (append
                 `(,duration)
                 (loop for i from 0 below (1- (length steps))
                       as step-current = (nth i steps)
                       as step-next = (nth (+ i 1) steps)
                       collect
                       `(,step-current
                         ,(third
                           (find `(,step-current ,step-next)
                                 transitions
                                 :test (lambda (subject list-item)
                                         (and (states-equal?
                                               (first subject)
                                               (first list-item))
                                              (states-equal?
                                               (second subject)
                                     (second list-item))))))))
                 `(,(last steps) nil))))
            (reverse solutions) (reverse solver-durations))))
