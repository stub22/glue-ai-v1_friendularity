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
