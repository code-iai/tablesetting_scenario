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
      seat/2,
      seat_area/2,
      area_location/2,
      location_hint/2,
      seat_position_index/2,
      seat_position_side/2
    ]).


:- use_module(library('semweb/rdfs')).
:- use_module(library('owl_parser')).
:- use_module(library('owl')).
:- use_module(library('rdfs_computable')).
:- use_module(library('knowrob_owl')).


:-  rdf_meta
    table(r),
    seat(r, r),
    seat_area(r, r),
    area_location(r, r),
    location_hint(r, r),
    seat_position_index(r, r),
    seat_position_side(r, r).


:- rdf_db:rdf_register_ns(rdf, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#', [keep(true)]).
:- rdf_db:rdf_register_ns(owl, 'http://www.w3.org/2002/07/owl#', [keep(true)]).
:- rdf_db:rdf_register_ns(knowrob, 'http://knowrob.org/kb/knowrob.owl#', [keep(true)]).
:- rdf_db:rdf_register_ns(xsd, 'http://www.w3.org/2001/XMLSchema#', [keep(true)]).


%% table(?Table) is nondet.
table(Table) :-
    rdf_has(Table, rdf:type, A),
    rdf_reachable(A, rdfs:subClassOf, knowrob:'MealTable').


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
