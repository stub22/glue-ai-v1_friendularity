:- module(floj, [write_java_class/1]).
/** <module> Core of the Floj compiler, everything that happens in RDF world
 *
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
:- use_module(library(semweb/rdf_db)).

:- rdf_meta
	write_java_class(r),
	write_prefix(r),
	write_guts(r),
	write_suffix(r).

%%	write_java_class(+Node:r) is det
%
%	write out a java class for Node to current_output
%
%	@arg Node  the flo:JavaBinding resource to compile
%
write_java_class(Node) :-
	\+ rdf(Node, _, _),
	format(user_error, '~w is not known~n', [Node]).
write_java_class(Node) :-
	\+ rdf(Node, rdf:type, flo:'JavaBinding'),
	format(user_error, '~w is not a valid JavaBinding~n', [Node]).
write_java_class(Node) :-
	phrase(java_class(Node), Text),
	format('~s', [Text]).

write_java_class(_) :-
	format(user_error, 'Could not write valid java class', []).

%%	java_class(+Node)// is nondet
%
%	define a java class based on Node
%
java_class(Node) -->
	java_prefix(Node),
	java_guts(Node),
	java_suffix(Node).

% will figure out the exact java later
java_prefix(Node) -->
	" /*\n",
        " *  Copyright 2013 by The Friendularity Project (www.friendularity.org).\n",
        " * \n",
        " *  Licensed under the Apache License, Version 2.0 (the \"License\");\n",
        " *  you may not use this file except in compliance with the License.\n",
        " *  You may obtain a copy of the License at\n",
        " * \n",
        " *       http://www.apache.org/licenses/LICENSE-2.0\n",
        " * \n",
        " *  Unless required by applicable law or agreed to in writing, software\n",
        " *  distributed under the License is distributed on an \"AS IS\" BASIS,\n",
        " *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.\n",
        " *  See the License for the specific language governing permissions and\n",
        " *  limitations under the License.\n",
        " */\n",
        "package ",
	package_of(Node),
	";\n",
        "\n",
        "import com.hp.hpl.jena.rdf.model.Model;\n",
        "import com.hp.hpl.jena.rdf.model.Resource;\n",
        "import com.hp.hpl.jena.rdf.model.Statement;\n",
        "import java.awt.Color;\n",
        "import java.awt.Graphics2D;\n",
        "import java.awt.Image;\n",
        "import java.awt.Rectangle;\n",
        "import java.awt.image.ImageObserver;\n",
        "import java.io.IOException;\n",
        "import java.util.logging.Level;\n",
        "import java.util.logging.Logger;\n",
        "import org.friendularity.bundle.blockflow.util.OSGi_ResourceLoader;\n",
        "import org.friendularity.bundle.blockflow.util.QN;\n",
        "/**\n",
        " *  A block prototype\n",
        " * \n",
        " * Part of BlockBuilder's flies.\n",
        " * \n",
        " * @author Annie\n",
        " */\n",
        "class ",
	name_of(Node),
	" extends BlockType, implements BentoListener {\n",
	"     public ",
	name_of(Node),
	"() {\n",
	"\n",
	"     }\n".

java_suffix(_) -->
	"}\n".


package_of(Node)  -->
	{
	    rdf(Node, rdf:type, flo:'JavaBinding'),
	    rdf(Node, flo:javaPackage, literal(Package)),
            atom_codes(Package, CPackage)
        },
	CPackage.

name_of(Node) -->
	{
	    rdf(Node, rdf:type, flo:'JavaBinding'),
	    rdf(Node, flo:javaName, literal(Name)),
            atom_codes(Name, CName)
        },
	CName.

java_guts(_Node) -->
	"The guts will go here".
