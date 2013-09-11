:- module(debug, [go/0]).
/** <module> Consult this file to start in dev mode

*/
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
:- multifile license:license/3.

license:license(apache, lgpl,
                [ comment('Friendularity Apache License'),
                  url('http://www.apache.org/licenses/LICENSE-2.0')
                ]).

:- license(apache).
:- use_module(library(http/http_path)).

:- portray_text(true).

:- ensure_loaded(load).

:- use_module(ethel_compiler).

http:location(pldoc, root('help/source'), [priority(10)]).

go :- compile('examples/testcase.eth', 'testcaseout.flo').

:- doc_server(5000).

:- www_open_url('http://localhost:5000/help/source').




