:- module(load, []).
/*
 *  Copyright 2013 by The Friendularity Project (www.friendularity.org).
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *       http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */
:- license(apache).

user:file_search_path(ethel, '../ethel/').

:- use_module(library(semweb/rdf_db)).

:- rdf_register_ns(flo, 'http://www.friendularity.org/ontology/flo#').

:- ensure_loaded(floj_compiler).
:- ensure_loaded(ethel(flo)).
:- ensure_loaded(floj).


