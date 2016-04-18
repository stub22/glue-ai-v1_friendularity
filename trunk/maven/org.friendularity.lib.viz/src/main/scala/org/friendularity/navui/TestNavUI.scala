package org.friendularity.navui

import com.hp.hpl.jena.rdf.model.Model
import org.appdapter.core.store.Repo
import org.appdapter.fancy.log.VarargsLogging
import org.appdapter.fancy.rclient.EnhancedLocalRepoClient
import org.cogchar.blob.entry.EntryHost
import org.cogchar.impl.scene.read.BehavMasterConfigTest
import org.friendularity.appro.TestRaizLoad
import org.friendularity.chnkr.ChnkrWrapRepoSpec

/**
  * Created by Owner on 4/1/2016.
  */
class TestNavUI extends VarargsLogging {

	// Goal - load vworld *incrementally* using messages found in "modern" config chunk(s),
	// mediated by higher-level instructions from profile recipes.  Most of these messages
	// (other than gross system-startup and system-shutdown) should be conveyable over
	// network, so the load ordering logic is independent from the instruction execution.

	// From the outside, the VWorld entities are all identified *only* by URI (optionally
	// extended by offset params).  Anything not identified by URI(+offset) must be private
	// to the VWorld.

	// Currently URIs for all entities are assigned from *outside* the V-World (VWorldBossActor),
	// and then passed in to it via entity creation messages.   All such URIs come from one of:
	// cogchar+app ontologies, app profile data, app config chunks, or app java/scala code.



	// Legacy config load section, gradually becoming obsolete:
	// From:  private void attachVizTChunkLegConfRepo(final BundleContext bunCtx) {

	val tchunkEHost: EntryHost = TestRaizLoad.makeBundleEntryHost(TestRaizLoad.getClass)
	val mergedProfileGraph: Model = TestRaizLoad.getMergedProfileGraph_RegularDesktop(tchunkEHost)
	val vzBrkRcpUriTxt: String = TestRaizLoad.vizappBrokerRecipeUriTxt
	val legConfRepoSpec: ChnkrWrapRepoSpec = TestRaizLoad.makeVWConfRepoSpec(mergedProfileGraph, vzBrkRcpUriTxt, tchunkEHost)
	getLogger.info("legConfRepoSpec={}", legConfRepoSpec)

	// TestRaizLoad.makeAndRegisterAvatarConfigRC(bunCtx, legConfRepoSpec) =
	val legConfRepoHandle : Repo.WithDirectory = legConfRepoSpec.getOrMakeRepo();
	// TODO: Get these SPARQL keys from profile, instead
	val erc = new EnhancedLocalRepoClient(legConfRepoSpec, legConfRepoHandle,
	BehavMasterConfigTest.TGT_GRAPH_SPARQL_VAR, BehavMasterConfigTest.QUERY_SOURCE_GRAPH_QN);
	// registerAvatarConfigRepoClient(bunCtx, erc);

}
