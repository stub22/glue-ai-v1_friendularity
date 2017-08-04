package org.friendularity.spec.connection;

import org.appdapter.bind.rdf.jena.assembly.KnownComponentImpl;
import org.appdapter.help.repo.RepoClient;

/**
 *
 * @author
 */
public class RepoClientSpec extends KnownComponentImpl {

    private RepoClient repoClient;

    public RepoClientSpec() {
    }

    public void setRepoClient(RepoClient rc) {
        repoClient = rc;
    }

    public RepoClient getRepoClient() {
        return repoClient;
    }
}
