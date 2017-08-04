/*
 *  Copyright 2012 by The Appdapter Project (www.appdapter.org).
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
package org.friendularity.spec.connection;

import com.hp.hpl.jena.assembler.Assembler;
import com.hp.hpl.jena.assembler.Mode;
import java.util.Set;
import org.appdapter.bind.rdf.jena.assembly.CachingComponentAssembler;
import org.appdapter.bind.rdf.jena.assembly.ItemAssemblyReader;
import org.appdapter.core.item.Item;
import org.appdapter.core.item.ItemFuncs;
import org.appdapter.core.name.Ident;
import com.hp.hpl.jena.rdf.model.Resource;

/**
 *
 * @author
 */
public class RepoClientSpecBuilder extends CachingComponentAssembler<RepoClientSpec> {

    public RepoClientSpecBuilder(Resource builderConfRes) {
        super(builderConfRes);
    }

    protected Class<RepoClientSpec> decideComponentClass(
            Ident ident, Item item) {
        return RepoClientSpec.class;
    }

    protected void initExtendedFieldsAndLinks(RepoClientSpec repoClientSpec, Item item, Assembler asmblr, Mode mode) {
        ItemAssemblyReader reader = getReader();



    }
}
