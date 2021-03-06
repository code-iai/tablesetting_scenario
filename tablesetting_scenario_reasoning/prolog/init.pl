/** 
  Copyright (C) 2015 Jan Winkler <winkler@cs.uni-bremen.de>
  All rights reserved.
  
  Redistribution and use in source and binary forms, with or without
  modification, are permitted provided that the following conditions are met:
  
      * Redistributions of source code must retain the above copyright
        notice, this list of conditions and the following disclaimer.
      * Redistributions in binary form must reproduce the above copyright
        notice, this list of conditions and the following disclaimer in the
        documentation and/or other materials provided with the distribution.
      * Neither the name of the <organization> nor the
        names of its contributors may be used to endorse or promote products
        derived from this software without specific prior written permission.
  
  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
  ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
  WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
  DISCLAIMED. IN NO EVENT SHALL <COPYRIGHT HOLDER> BE LIABLE FOR ANY
  DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
  (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
  LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
  ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
  
  @author Jan Winkler
  @license BSD
*/

:- register_ros_package(knowrob_common).
:- register_ros_package(comp_temporal).

:- register_ros_package(tablesetting_scenario_reasoning).
:- use_module(library('tablesetting_scenario_reasoning')).

:- register_ros_package(iai_semantic_maps).
/**:- owl_parser:owl_parse('package://tablesetting_scenario_models/owl/tablesetting_area.owl').
:- owl_parser:owl_parse('package://tablesetting_scenario_models/owl/tablesetting_rack.owl').*/
:- owl_parser:owl_parse('package://tablesetting_scenario_models/owl/table.owl').
:- owl_parser:owl_parse('package://tablesetting_scenario_models/owl/eating_area.owl').
:- owl_parser:owl_parse('package://tablesetting_scenario_models/owl/objects.owl').
:- owl_parser:owl_parse('package://iai_semantic_maps/owl/iai_maps_addons.owl').
:- owl_parser:owl_parse('package://iai_semantic_maps/owl/room.owl').

:- rdf_db:rdf_register_ns(knowrob, 'http://ias.cs.tum.edu/kb/knowrob.owl#', [keep(true)]).

