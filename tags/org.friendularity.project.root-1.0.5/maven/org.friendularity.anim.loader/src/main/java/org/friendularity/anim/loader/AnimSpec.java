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

package org.friendularity.anim.loader;

import org.appdapter.bind.rdf.jena.assembly.KnownComponentImpl;

/**
 * Data Object representing a ConnectionSpec
 * 
 * @author Jason R. Eads <eadsjr>
 */
public class AnimSpec extends KnownComponentImpl {
  private String Type;
  private String Name;
  private String Path;
  private String Folder;

    public String getType() {
        return Type;
    }

    public void setType(String Type) {
        this.Type = Type;
    }

    public String getName() {
        return Name;
    }

    public void setName(String Name) {
        this.Name = Name;
    }

    public String getPath() {
        return Path;
    }

    public void setPath(String Path) {
        this.Path = Path;
    }

    public String getFolder() {
        return Folder;
    }

    public void setFolder(String Folder) {
        this.Folder = Folder;
    }
}
