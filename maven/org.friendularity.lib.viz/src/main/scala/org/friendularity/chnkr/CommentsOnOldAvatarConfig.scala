/*
 *  Copyright 2016 by The Friendularity Project (www.friendularity.org).
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

package org.friendularity.chnkr

trait CommentsOnOldAvatarConfig  {

}
/*

Comments below copied from org.appdapter.fancy.rspec.RepoSpecDefaultNames on 2016-04-17

	  // These 2 string constants establish repo-client wrapper defaults, giving a default
  // query context to easily fetch from.
  // Either value may be bypassed/overidden using either
  //      A) Overrides of the RepoSpec methods below
  //   or B) The more general forms of queryIndirect_.
  // 1) Default query *source* graph QName used in directory model (Sheet or RDF).
  // We read SPARQL text from this graph, which we use to query *other* graphs.
  // This graph is typically not used as a regular data graph by  other low-order
  // query operations, although there is no prohibition or protection from doing so
  // at this time.   This query source graph may be overridden using the more general
  // forms of queryIndirect_.

  val DFLT_QRY_SRC_GRAPH_TYPE = "ccrt:qry_sheet_22"

  // 2) default variable name for a single target graph in a SPARQL query.
  // This is used in the convenience forms of queryIndirect that handle many common
  // use cases, wherein the query needs a single graph to operate on that is switched
  // by application logic or user selection.

  val DFLT_TGT_GRAPH_SPARQL_VAR = "qGraph"


  ...BUT, then in Cogchar we have  MasterDemoNames which appears to be the QuerySource
   name actually used at present.

   Fortunately we note that our qry_sheet_77 and qry_sheet_22 are identical in contents.

   {

	  val QUERY_SOURCE_GRAPH_QN : String = "ccrt:qry_sheet_77"
	  val CHAN_BIND_GRAPH_QN : String = "csi:chan_sheet_77"

	  val GROUP_KEY_CHAN_BIND : String = "ChannelBindingGroupId"
	  val CHAN_GROUP_QN : String = "csi:dm_chan_group_22"

	  val DIRECT_BEHAV_GRAPH_QN : String = "csi:behavScene_sheet_77"

	  val BEHAV_STEP_GRAPH_QN : String = "csi:behavStep_sheet_77"
	  val BEHAV_SCENE_GRAPH_QN : String = "csi:behavScene_sheet_77"
	  val DERIVED_BEHAV_GRAPH_QN : String = "csi:merged_model_5001"

	  val PIPELINE_GRAPH_QN : String = "csi:pipeline_sheet_77"
	  val PIPE_QUERY_QN : String = "ccrt:find_pipes_77"
	  val PIPE_SOURCE_QUERY_QN : String = "ccrt:find_pipe_sources_78"


	  val GROUP_KEY_SCENE_SPEC : String = "SceneSpecGroupId"
	  val SCENE_GROUP_QN : String = "csi:scene_group_33"


	  val GROUP_KEY_THEATER : String = "TheaterGroupId"
	  val THEATER_GROUP_QN : String = "csi:theater_group_44"


}

     [java] java.lang.RuntimeException: Unable to find Query Called ccrt:template_globalmode_99 in Model wrongQrySrcQName
     [java] 	at org.appdapter.fancy.repo.FancyRepo$class.checkQueryText(FancyRepo.scala:153)
     [java] 	at org.friendularity.chnkr.ChnkrWrapRepo.checkQueryText(ChnkrWrapRepo.scala:30)
     [java] 	at org.appdapter.fancy.repo.FancyRepo$class.queryIndirectForAllSolutions(FancyRepo.scala:147)
     [java] 	at org.friendularity.chnkr.ChnkrWrapRepo.queryIndirectForAllSolutions(ChnkrWrapRepo.scala:30)
     [java] 	at org.appdapter.fancy.rclient.LocalRepoClientImpl.queryIndirectForAllSolutions(LocalRepoClientImpl.scala:98)
     [java] 	at org.appdapter.fancy.rclient.RepoClientImpl.queryIndirectForAllSolutions(RepoClientImpl.scala:56)
     [java] 	at org.appdapter.fancy.rclient.RepoClientImpl.queryIndirectForAllSolutions(RepoClientImpl.scala:62)
     [java] 	at org.cogchar.blob.emit.GlobalConfigEmitter.<init>(GlobalConfigEmitter.scala:83)
     [java] 	at org.cogchar.app.puma.config.PumaGlobalModeManager.applyGlobalConfig(PumaGlobalModeManager.java:57)
     [java] 	at org.cogchar.app.puma.boot.PumaAppContext.startRepositoryConfigServices(PumaAppContext.java:150)
     [java] 	at org.cogchar.app.puma.boot.PumaBooter.pumaBootUnsafeUnderOSGi(PumaBooter.java:133)
     [java] 	at org.cogchar.app.puma.boot.PumaBooter.bootUnderOSGi(PumaBooter.java:83)
     [java] 	at org.friendularity.bundle.demo.ccmio.CCMIO_DemoActivator.launchPumaRobotsAndChars(CCMIO_DemoActivator.java:151)
     [java] 	at org.friendularity.bundle.demo.ccmio.CCMIO_DemoActivator.launchCcmioDemo(CCMIO_DemoActivator.java:114)
     [java] 	at org.friendularity.bundle.demo.ccmio.CCMIO_DemoActivator.handleFrameworkStartedEvent(CCMIO_DemoActivator.java:104)
 */