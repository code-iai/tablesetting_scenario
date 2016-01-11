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


(defmacro with-first-prolog-vars-bound (vars prolog-query &body body)
  "Evaluates the prolog query `prolog-query' and transforms variables `vars' via `body', returning the result."
  (cond ((> (length vars) 0)
         `(with-vars-bound ,vars
              (lazy-car
               (json-prolog:prolog ,prolog-query))
            ,@body))
        (t `(progn
              (json-prolog:prolog ,prolog-query)
              ,@body))))

(defmacro with-prolog-vars-bound (vars prolog-query &body body)
  "Lists all results from the prolog query `prolog-query', each being transformed by `body'. `vars' denotes all variables to make available in `body'."
  `(force-ll
    (lazy-mapcar
     (lambda (bdgs)
       (with-vars-bound ,vars bdgs
         ,@body))
     (json-prolog:prolog ,prolog-query))))

(defun json-symbol->string (symbol)
  "Converts `symbol' as returned from json-prolog to a lisp-usable string by trimming `|' characters at the beginning and the end."
  (let* ((string-symbol (write-to-string symbol)))
    (subseq string-symbol 2 (- (length string-symbol) 2))))

(defun split-prolog-symbol (prolog-symbol &key (delimiter '\#))
  "Splits the namespace from the symbol of a prolog identifier symbol `prolog-symbol'. The two parts must be delimited by the delimiter `delimiter'. Returns a values list, consisting of the symbol, and the namespace."
  (let ((delimiter-position
          (position delimiter prolog-symbol :test #'string=)))
    (when delimiter-position
      (values
       (subseq prolog-symbol (1+ delimiter-position))
       (subseq prolog-symbol 0 delimiter-position)))))

(defun strip-prolog-string (symbol)
  "Combines the functionality of `json-symbol->string' and `split-prolog-symbol', resulting in a namespace-less string representing the value of `symbol'."
  (split-prolog-symbol (json-symbol->string symbol)))

(defun add-prolog-namespace (symbol &key (namespace "http://knowrob.org/kb/knowrob.owl") (delimiter '\#))
  "Concatenates a string that consists of the given `namespace', the `delimiter', and finally the `symbol'. The default namespace represents the base KnowRob OWL namespace, and the default delimiter is `#'."
  (concatenate
   'string
   namespace
   (json-symbol->string (write-to-string delimiter))
   symbol))

(defun storage-locations (&optional type)
  (remove-if-not
   #'identity
   (with-prolog-vars-bound (?location ?type)
       `("storage_location" ?location ?type)
     (let ((loc-string (json-symbol->string ?location)))
       (with-first-prolog-vars-bound (?semanticreference)
           `("semantic_reference" ,loc-string ?semanticreference)
         (let* ((ref (split-prolog-symbol (json-symbol->string ?semanticreference)))
                (type-str (json-symbol->string ?type)))
           (when (or (not type) (string= type type-str))
             (let ((desig-modif (cond
                                  ((string= type-str "counter")
                                   `(desig-props:on Cupboard))
                                  ((string= type-str "drawer")
                                   `(desig-props:on Cupboard)))))
               (make-designator 'location `(,desig-modif (desig-props:name ,ref)))))))))))

(defun assert-tablesetting-object (type)
  (with-first-prolog-vars-bound (?instance)
      `("assert_tablesetting_object" ?instance ,(add-prolog-namespace type))
    (split-prolog-symbol (json-symbol->string ?instance))))

(defun retract-tablesetting-object (instance)
  (with-first-prolog-vars-bound ()
      `("retract_tablesetting_object" ,(add-prolog-namespace instance))))

(defun tablesetting-objects ()
  (with-prolog-vars-bound (?object)
      `("tablesetting_object" ?object)
    (split-prolog-symbol (json-symbol->string ?object))))

(defun object-urdf-path (object)
  (with-first-prolog-vars-bound (?urdfpath)
      `("object_urdf_path" ,(add-prolog-namespace object) ?urdfpath)
    (json-symbol->string ?urdfpath)))

(defun common-storage-location (object)
  (with-prolog-vars-bound (?location)
      `("common_storage_location" ,(add-prolog-namespace object) ?location)
    (json-symbol->string ?location)))

(defun get-free-pose-at-location (location)
  ;; TODO: Correctly find the free pose here.
  (reference location))

(defun spawn-new-instance-at-common-place (type &key hint)
  (let* ((instance (assert-tablesetting-object type))
         (common-location-types (common-storage-location instance))
         (random-location-type (elt common-location-types
                                    (random (length common-location-types))))
         (locations (storage-locations random-location-type))
         (locations-filtered
           (remove-if (lambda (location)
                        (when hint
                          (not (string= (desig-prop-value location 'desig-props::name)
                                        hint))))
                      locations))
         (random-location (elt locations-filtered (random (length locations-filtered))))
         (pose (get-free-pose-at-location random-location)))
    (cond (pose
           (let ((pose-elevated (tf:make-pose-stamped
                                 (tf:frame-id pose)
                                 (tf:stamp pose)
                                 (tf:make-3d-vector (tf:x (tf:origin pose))
                                                    (tf:y (tf:origin pose))
                                                    (+ (tf:z (tf:origin pose)) 0.1))
                                 (tf:orientation pose))))
             (set-object-pose instance pose-elevated)
             (spawn-instance instance)))
          (t (format t "Failed to spawn object, as no free space was found.")))
    (let ((dimensions (get-item-dimensions instance))
          (shape (get-item-primitive-shape-symbol instance)))
      (make-designator 'object `((desig-props:type ,type)
                                 (desig-props:dimensions ,dimensions)
                                 (desig-props:shape ,shape)
                                 (desig-props:at
                                  ,(make-designator 'location
                                                    `((desig-props:pose ,pose)))))))))

(defun spawn-instance (instance)
  (let* ((urdf (object-urdf-path instance))
         (pose (get-object-pose instance)))
    (cram-gazebo-utilities:spawn-gazebo-model instance pose urdf)))

(defun spawn-new-instance (type pose)
  (let* ((instance (assert-tablesetting-object type)))
    (set-object-pose instance pose)
    (spawn-instance instance)))

(defun set-object-pose (object pose)
  (let* ((matrix (cl-transforms:pose->matrix pose))
         (m-sequence (loop for i from 0 below 4
                           append
                           (loop for j from 0 below 4
                                 collect (aref matrix i j)))))
    (json-prolog:prolog `("assert_object_pose"
                          ,(add-prolog-namespace object)
                          ,@m-sequence))))

(defun get-object-pose (object)
  (with-vars-bound (?pose)
      (lazy-car (json-prolog:prolog `("get_object_pose"
                                      ,(add-prolog-namespace object) ?pose)))
    (tf:pose->pose-stamped
     "map" 0.0
     (tf:transform->pose
      (tf:matrix->transform
       (make-array
        `(4 4)
        :initial-contents
        (loop for i from 0 below 4
              collect
              (subseq ?pose (* i 4) (+ (* i 4) 4)))))))))

(defun get-shopping-items ()
  "Returns all shopping items known in the current semantic environment."
  (with-prolog-vars-bound (?item)
      `("shopping_item" ?item)
    (strip-prolog-string ?item)))

(defun is-stackable (item)
  "Returns whether the shopping item `item' is stackable or not."
  (not (not (json-prolog:prolog
             `("is_stackable" ,(add-prolog-namespace item))))))

(defun get-tables ()
  (with-prolog-vars-bound (?table)
      `("table" ?table)
    (strip-prolog-string ?table)))

(defun get-table-seats (table)
  (with-prolog-vars-bound (?seat)
      `("seat" ,(add-prolog-namespace table) ?seat)
    (strip-prolog-string ?seat)))

(defun get-seat-areas (seat)
  (with-prolog-vars-bound (?area)
      `("seat_area" ,(add-prolog-namespace seat) ?area)
    (strip-prolog-string ?area)))

(defun get-area-locations (area)
  (with-prolog-vars-bound (?location)
      `("area_location" ,(add-prolog-namespace area) ?location)
    (strip-prolog-string ?location)))

(defun get-location-hints (location)
  (with-prolog-vars-bound (?hint)
      `("location_hint" ,(add-prolog-namespace location) ?hint)
    (let ((string-hint (string-upcase (write-to-string ?hint))))
      (intern (subseq string-hint 2 (- (length string-hint) 2))
              :keyword))))

(defun get-seat-position-index (seat)
  (with-first-prolog-vars-bound (?index)
      `("seat_position_index" ,(add-prolog-namespace seat) ?index)
    (let ((string (string-upcase (write-to-string ?index))))
      (parse-integer (subseq string 2 (- (length string) 2))))))

(defun get-seat-position-side (seat)
  (with-first-prolog-vars-bound (?side)
      `("seat_position_side" ,(add-prolog-namespace seat) ?side)
    (let ((string (string-upcase (write-to-string ?side))))
      (intern (subseq string 2 (- (length string) 2))
              :keyword))))

(defun get-rack-pose (rack)
  "Return the pose of the given rack, in `map' coordinates."
  (with-first-prolog-vars-bound (?mat)
      `("rack_pose" ,(add-prolog-namespace rack) ?mat)
    (tf:pose->pose-stamped "map" 0.0 (rotmat->pose ?mat))))

(defun get-rack-levels (rack)
  "Returns all rack levels for the given rack `rack'."
  (with-prolog-vars-bound (?racklevel)
      `("rack_level" ,(add-prolog-namespace rack) ?racklevel)
    (strip-prolog-string ?racklevel)))

(defun get-rack-on-level (rack level)
  "Returns the rack level `level' on rack `rack'. `level' is an integer."
  (with-first-prolog-vars-bound (?racklevel)
      `("rack_on_level" ,(add-prolog-namespace rack) ,level ?racklevel)
    (strip-prolog-string ?racklevel)))

(defun location-on-rack-level (rack level)
  "Generates a location designator that describes a three dimensional pose on the two dimensional plane of the given rack level `level' on rack `rack'. `level' is an integer."
  (let ((rack-level (get-rack-on-level rack level)))
    (make-designator 'location
                     `((desig-props::on "RackLevel")
                       (desig-props::name ,(add-prolog-namespace rack-level))))))

(defun get-object-rack-level (rack object)
  "Returns which level of a rack `rack' an object `object' resides on. Returns the (namespace-less) OWL identifier of the rack level."
  (let* ((at (desig-prop-value object 'desig-props::at))
         (pose (reference at))
         (origin (tf:origin pose)))
    (with-first-prolog-vars-bound (?racklevel)
        `("position_on_rack"
          ,(tf:x origin) ,(tf:y origin) ,(tf:z origin)
          0.3 ,(add-prolog-namespace rack) ?racklevel)
      (strip-prolog-string ?racklevel))))

(defun get-rack-level-elevation (racklevel)
  "Returns the z-coordinate of the surface of the rack level `racklevel'."
  (with-first-prolog-vars-bound (?elevation)
      `("rack_level_elevation" ,(add-prolog-namespace racklevel) ?elevation)
    ?elevation))

(defun get-rack-level-relative-pose (rack-level x y z &optional (rotation (tf:euler->quaternion)))
  "Returns (in absolute map coordinates) a pose stamped that describes the relative pose ((x y z) rotation) on the rack level `racklevel'."
  (with-first-prolog-vars-bound (?result)
      `("rack_level_relative_position"
        ,(add-prolog-namespace rack-level) ,x ,y ?result)
    (destructuring-bind (x-abs y-abs z-abs) ?result
      (tf:make-pose-stamped
       "map" 0.0 (tf:make-3d-vector x-abs y-abs (+ z z-abs)) rotation))))

(defun get-item-urdf-path (item)
  "Returns the absolute URDF file path for an item `item' (if set in the semantic information supplied to KnowRob)."
  (with-first-prolog-vars-bound (?urdfpath)
      `("item_urdf_path" ,(add-prolog-namespace item) ?urdfpath)
    (json-symbol->string ?urdfpath)))

(defun get-item-dimensions (item)
  "Returns the dimensions `(width depth height)' of an item `item' as vector."
  (with-first-prolog-vars-bound (?width ?depth ?height)
      `("object_dimensions_restricted"
        ,(add-prolog-namespace item) ?width ?depth ?height)
    (vector ?width ?depth ?height)))

(defun get-items-by-class-type (class-type)
  "Returns all item instances that are of class type `class-type'."
  (with-prolog-vars-bound (?item)
      `("item_class_type" ,(add-prolog-namespace class-type) ?item)
    (strip-prolog-string ?item)))

(defun get-item-primitive-shape (item)
  "Returns the primitive shape of the shopping item `item' as denoted by the knowledge base."
  (with-first-prolog-vars-bound (?primitiveshape)
      `("object_primitive_shape" ,(add-prolog-namespace item)
                                 ?primitiveshape)
    (json-symbol->string ?primitiveshape)))

(defun get-item-primitive-shape-symbol (item)
  "Returns the same primitive shape as `get-item-primitive-shape', but transforms the returned string value into a Lisp symbol for all known shapes (`:box', `:cylinder'). The function defaults to `:box' if the shape is not available or not known."
  (or (case (get-item-primitive-shape item)
        ("box" :box)
        ("cylinder" :cylinder))
      :box))

(defun get-item-semantic-handles (item)
  "Lists all semantic handles of the item identified by `item' that are known in the knowledge base. All handles are returned as object designators of type `handle' and have a grasp-type (denoting how to approach the handle while grasping it) and a object-center relative pose."
  (with-prolog-vars-bound (?semantichandle)
      `("object_semantic_handle" ,(add-prolog-namespace item)
                                 ?semantichandle)
    (let ((grasp-type
            (with-first-prolog-vars-bound (?grasptype)
                `("grasp_type" ,(json-symbol->string ?semantichandle)
                               ?grasptype)
              (json-symbol->string ?grasptype)))
          (handle-pose
            (with-first-prolog-vars-bound (?pose)
                `("handle_pose" ,(json-symbol->string ?semantichandle)
                                ?pose)
              (rotmat->pose ?pose))))
      (make-designator
       'object
       `((desig-props:type desig-props:handle)
         (desig-props:at ,(make-designator
                           'location
                           `((desig-props:pose ,handle-pose))))
         (desig-props:grasp-type ,(intern (string-upcase grasp-type)
                                          'desig-props)))))))

(defun shopping-item-classes (&key (base-class "ShoppingItem"))
  "Lists all subclasses of the KnowRob base-class `ShoppingItem'. The base-class itself does not count towards the shopping item classes and is not considered a valid instantiation of a usable shopping item."
  (cpl:mapcar-clean
   (lambda (class)
     (let ((stripped-class (strip-prolog-string class)))
       (unless (string= stripped-class base-class)
         stripped-class)))
   (with-prolog-vars-bound (?class)
      `("owl_subclass_of" ?class ,(add-prolog-namespace base-class))
    ?class)))

(defun shopping-item-class-p (class)
  "Returns a boolean value denoting whether `class' is a valid shopping item class (i.e., a subclass of `ShoppingItem')."
  (not (not (find class (shopping-item-classes) :test #'string=))))

(defun add-shopping-item (class)
  "Adds a new shopping item instance of type `class' to the knowledge base."
  (assert (shopping-item-class-p class)
          ()
          "Class `~a' is not a subclass of `ShoppingItem'." class)
  (with-first-prolog-vars-bound (?instance)
      `("add_shopping_item" ,(add-prolog-namespace class) ?instance)
    (strip-prolog-string ?instance)))

(defun remove-shopping-item (item)
  "Removes the shopping item with the (namespace-less) OWL identifier `item'."
  (json-prolog:prolog `("remove_shopping_item"
                        ,(add-prolog-namespace item))))

(defun get-item-class (item)
  (with-first-prolog-vars-bound (?class)
      `("object_type" ,(add-prolog-namespace item) ?class)
    (strip-prolog-string ?class)))

(defun remove-all-shopping-items ()
  "Removes all known shopping items from the knowledge base."
  (let ((items (get-shopping-items)))
    (dolist (item items)
      (remove-shopping-item item))))
