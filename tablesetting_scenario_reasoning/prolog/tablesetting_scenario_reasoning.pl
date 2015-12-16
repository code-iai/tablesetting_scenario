/**  <module> tablesetting_scenario_reasoning

  Copyright (C) 2015 Jan Winkler
  All rights reserved.

  Redistribution and use in source and binary forms, with or without
  modification, are permitted provided that the following conditions are met:
      * Redistributions of source code must retain the above copyright
        notice, this list of conditions and the following disclaimer.
      * Redistributions in binary form must reproduce the above copyright
        notice, this list of conditions and the following disclaimer in the
        documentation and/or other materials provided with the distribution.
      * Neither the name of the Institute for Artificial Intelligence,
        University of Bremen nor the names of its contributors may be used to
        endorse or promote products derived from this software without specific
        prior written permission.

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
  ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
  WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
  DISCLAIMED. IN NO EVENT SHALL THE INSTITUTE FOR ARTIFICIAL INTELLIGENCE BE LIABLE FOR ANY
  DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
  (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
  LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
  ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

  @author Jan Winkler
  @license BSD
*/

:- module(tablesetting_scenario_reasoning,
    [
      table/1,
      storage_location/2,
      semantic_reference/2,
      tablesetting_object/1,
      assert_tablesetting_object/2,
      get_object_pose/2,
      assert_object_pose/17,
      rotation_matrix/2,
      object_urdf_path/2,
      assert_rotation_matrix_entries/2,
      seat/2,
      seat_area/2,
      area_location/2,
      location_hint/2,
      seat_position_index/2,
      seat_position_side/2,
      ts_call/3,
      common_storage_location/2
    ]).


:- use_module(library('semweb/rdfs')).
:- use_module(library('owl_parser')).
:- use_module(library('owl')).
:- use_module(library('rdfs_computable')).
:- use_module(library('knowrob_owl')).


:-  rdf_meta
    table(r),
    storage_location(r, r),
    semantic_reference(r, r),
    assert_tablesetting_object(r, r),
    assert_object_pose(r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r),
    get_object_pose(r, r),
    rotation_matrix(r, r),
    tablesetting_object(r),
    assert_rotation_matrix_entries(r, r),
    seat(r, r),
    seat_area(r, r),
    area_location(r, r),
    location_hint(r, r),
    seat_position_index(r, r),
    seat_position_side(r, r),
    object_urdf_path(r, r),
    ts_call(r, r, r),
    common_storage_location(r, r).


:- rdf_db:rdf_register_ns(rdf, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#', [keep(true)]).
:- rdf_db:rdf_register_ns(owl, 'http://www.w3.org/2002/07/owl#', [keep(true)]).
:- rdf_db:rdf_register_ns(knowrob, 'http://knowrob.org/kb/knowrob.owl#', [keep(true)]).
:- rdf_db:rdf_register_ns(xsd, 'http://www.w3.org/2001/XMLSchema#', [keep(true)]).


%% table(?Table) is nondet.
table(Table) :-
    rdf_has(Table, rdf:type, A),
    rdf_reachable(A, rdfs:subClassOf, knowrob:'MealTable').


%% ts_call(?Function, ?Parameters, ?Result) is nondet.
%
% Calls a function in the Java class 'TablesettingReasoner'. This is a shorthand for jpl_new... and jpl_call... specific to RackReasoner.
%
% @param Function     The function to call
% @param Parameters   The parameters to pass to the function
% @param Result       The returned result
%
ts_call(Function, Parameters, Result) :-
    jpl_new('org.knowrob.tablesetting_scenario_reasoning.TablesettingReasoner', [], TS),
    jpl_call(TS, Function, Parameters, Result).


%% storage_location(?StorageLocation, ?LocationType) is nondet.
storage_location(StorageLocation, LocationType) :-
    rdf_has(StorageLocation, rdf:type, A),
    rdf_reachable(A, rdfs:subClassOf, knowrob:'StorageLocation'),
    rdf_has(StorageLocation, knowrob:'locationType', literal(type(_, LocationType))).


%% tablesetting_object(?Object) is nondet.
tablesetting_object(Object) :-
    rdf_has(Object, rdf:type, A),
    rdf_reachable(A, rdfs:subClassOf, knowrob:'TablesettingObject').


%% assert_tablesetting_object(?Instance, ?Type) is nondet.
assert_tablesetting_object(Instance, Type) :-
    rdf_instance_from_class(Type, Instance).


%% assert_object_pose(?Instance, ?Pose) is nondet.
assert_object_pose(Instance, M00, M01, M02, M03, M10, M11, M12, M13, M20, M21, M22, M23, M30, M31, M32, M33) :-
    tablesetting_object(Instance),
    rotation_matrix(Instance, RotMat),
    assert_rotation_matrix_entries(RotMat, [M00, M01, M02, M03, M10, M11, M12, M13, M20, M21, M22, M23, M30, M31, M32, M33]).


get_object_pose(Instance, [M00, M01, M02, M03, M10, M11, M12, M13, M20, M21, M22, M23, M30, M31, M32, M33]) :-
    tablesetting_object(Instance),
    rotation_matrix(Instance, RotMat),
    rdf_has(RotMat, knowrob:'m00', literal(type(xsd:float, M00))),
    rdf_has(RotMat, knowrob:'m01', literal(type(xsd:float, M01))),
    rdf_has(RotMat, knowrob:'m02', literal(type(xsd:float, M02))),
    rdf_has(RotMat, knowrob:'m03', literal(type(xsd:float, M03))),
    rdf_has(RotMat, knowrob:'m10', literal(type(xsd:float, M10))),
    rdf_has(RotMat, knowrob:'m11', literal(type(xsd:float, M11))),
    rdf_has(RotMat, knowrob:'m12', literal(type(xsd:float, M12))),
    rdf_has(RotMat, knowrob:'m13', literal(type(xsd:float, M13))),
    rdf_has(RotMat, knowrob:'m20', literal(type(xsd:float, M20))),
    rdf_has(RotMat, knowrob:'m21', literal(type(xsd:float, M21))),
    rdf_has(RotMat, knowrob:'m22', literal(type(xsd:float, M22))),
    rdf_has(RotMat, knowrob:'m23', literal(type(xsd:float, M23))),
    rdf_has(RotMat, knowrob:'m30', literal(type(xsd:float, M30))),
    rdf_has(RotMat, knowrob:'m31', literal(type(xsd:float, M31))),
    rdf_has(RotMat, knowrob:'m32', literal(type(xsd:float, M32))),
    rdf_has(RotMat, knowrob:'m33', literal(type(xsd:float, M33))).


rotation_matrix(Instance, RotMat) :-
    tablesetting_object(Instance),
    rdf_has(Instance, knowrob:'eventOccursAt', RotMat),!.


rotation_matrix(Instance, RotMat) :- 
    tablesetting_object(Instance),
    rdf_instance_from_class(knowrob:'RotationMatrix3D', RotMat),
    rdf_assert(Instance, knowrob:'eventOccursAt', RotMat).


assert_rotation_matrix_entries(RotMat, [M00, M01, M02, M03, M10, M11, M12, M13, M20, M21, M22, M23, M30, M31, M32, M33]) :-
    rdf_assert(RotMat, knowrob:'m00', literal(type(xsd:float, M00))),
    rdf_assert(RotMat, knowrob:'m01', literal(type(xsd:float, M01))),
    rdf_assert(RotMat, knowrob:'m02', literal(type(xsd:float, M02))),
    rdf_assert(RotMat, knowrob:'m03', literal(type(xsd:float, M03))),
    rdf_assert(RotMat, knowrob:'m10', literal(type(xsd:float, M10))),
    rdf_assert(RotMat, knowrob:'m11', literal(type(xsd:float, M11))),
    rdf_assert(RotMat, knowrob:'m12', literal(type(xsd:float, M12))),
    rdf_assert(RotMat, knowrob:'m13', literal(type(xsd:float, M13))),
    rdf_assert(RotMat, knowrob:'m20', literal(type(xsd:float, M20))),
    rdf_assert(RotMat, knowrob:'m21', literal(type(xsd:float, M21))),
    rdf_assert(RotMat, knowrob:'m22', literal(type(xsd:float, M22))),
    rdf_assert(RotMat, knowrob:'m23', literal(type(xsd:float, M23))),
    rdf_assert(RotMat, knowrob:'m30', literal(type(xsd:float, M30))),
    rdf_assert(RotMat, knowrob:'m31', literal(type(xsd:float, M31))),
    rdf_assert(RotMat, knowrob:'m32', literal(type(xsd:float, M32))),
    rdf_assert(RotMat, knowrob:'m33', literal(type(xsd:float, M33))).


%% semantic_reference(?Location, ?Reference)
semantic_reference(Location, Reference) :-
    rdf_has(Location, knowrob:'semanticReference', Reference).


%% object_urdf_path(?Object, ?URDFPath) is nondet.
object_urdf_path(Object, URDFPath) :-
    owl_has(Object, knowrob:'urdf', literal(type(_, URDFRelative))),
    ts_call('resolveRelativePath', [URDFRelative], URDFPath).


%% common_storage_location(?Object, ?Location) is nondet.
common_storage_location(Object, Location) :-
    owl_has(Object, knowrob:'commonStorageLocation', literal(type(_, Location))).


%% seat(?Table, ?Seat) is nondet.
seat(Table, Seat) :-
    table(Table),
    rdf_has(Table, knowrob:'seat', Seat),
    rdf_has(Seat, rdf:type, knowrob:'TableSeat').


%% seat_area(?Seat, ?Area) is nondet.
seat_area(Seat, Area) :-
    seat(_, Seat),
    rdf_has(Seat, knowrob:'area', Area),
    rdf_has(Area, rdf:type, knowrob:'SeatArea').


%% area_location(?Area, ?Location) is nondet.
area_location(Area, Location) :-
    seat_area(_, Area),
    rdf_has(Area, knowrob:'location', Location),
    rdf_has(Location, rdf:type, knowrob:'AreaLocation').


%% location_hint(?Location, ?Hint) is nondet.
location_hint(Location, Hint) :-
    area_location(_, Location),
    owl_has(Location, knowrob:'locationHint', literal(type(_, Hint))).


%% seat_position_index(?Seat, ?Index)
seat_position_index(Seat, Index) :-
    seat_area(Seat, _),
    rdf_has(Seat, knowrob:'index', literal(type(_, Index))).


%% seat_position_side(?Seat, ?Side)
seat_position_side(Seat, Side) :-
    seat_area(Seat, _),
    rdf_has(Seat, knowrob:'side', literal(type(_, Side))).
