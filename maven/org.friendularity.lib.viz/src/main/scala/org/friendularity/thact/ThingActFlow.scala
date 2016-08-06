package org.friendularity.thact

import org.appdapter.core.name.{Ident, FreeIdent}
import org.appdapter.fancy.log.VarargsLogging
import org.cogchar.api.vworld.GoodyActionParamWriter
import org.cogchar.impl.thing.basic.{BasicTypedValueMapWithConversion, BasicTypedValueMap, BasicThingActionSpec}
import org.cogchar.name.goody.GoodyNames

/**
  * Created by Stub22 on 8/6/2016.
  */
class ThingActionFlow extends VarargsLogging {
	val TEST_INIT_X = 30.0f;
	val TEST_INIT_Y = 15.0f;
	val TEST_INIT_Z = 10.0f;
	val goodyGraphQN = "ccrt:thing_sheet_22";
	val boxBaseURI = "http://dummy.org/bitbox#num_";

	def makeThingActionSpec () : BasicThingActionSpec = {
		val btvm : BasicTypedValueMap = new BasicTypedValueMapWithConversion(); // ConcreteTVM();
		val gapw = new GoodyActionParamWriter(btvm);

		gapw.putLocation(TEST_INIT_X, TEST_INIT_Y, TEST_INIT_Z);
		gapw.putRotation(1.0f, 1.0f, 1.0f, 10.0f);
		//gapw.putSize(4f, 0f, 0f);
		gapw.putScaleUniform(4f);

		val dummyBoxID = new FreeIdent(boxBaseURI + System.currentTimeMillis());
		// sendBitBoxTAS(dummyBoxID, GoodyNames.ACTION_CREATE, btvm, ran, debugFlag);

		// 	public void sendBitBoxTAS(Ident tgtThingID, Ident verbID, SerTypedValueMap paramTVMap, Random ran, boolean debugFlag) {
		// return dummyBoxID;
		//
		import java.util.Random;

		val ran = new Random();
		val actRecID = new FreeIdent("action_#" + ran.nextInt());
		val verbID = GoodyNames.ACTION_CREATE
		val tgtThingID = dummyBoxID
		val tgtThingTypeID = GoodyNames.TYPE_BIT_BOX;
		val paramTVMap = btvm
		val srcAgentID : Ident = null;
		val postedTStampMsec = System.currentTimeMillis();
		val btas = new BasicThingActionSpec(actRecID, tgtThingID, tgtThingTypeID, verbID, srcAgentID, paramTVMap, postedTStampMsec);
		// sendThingActionSpec(btas, ran, debugFlag);
		btas

	}
}

