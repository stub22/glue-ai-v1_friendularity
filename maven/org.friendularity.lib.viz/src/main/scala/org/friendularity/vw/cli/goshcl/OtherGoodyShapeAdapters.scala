package org.friendularity.vw.cli.goshcl

import org.appdapter.core.name.Ident
import org.cogchar.name.goody.GoodyNames
import org.friendularity.vw.msg.cor.VWContentRq

/**
  * Created by Owner on 1/22/2017.
  */
trait OtherGoodyShapeXlator extends GoodyRqPartialXlator {
	override def makeCreateRqs(verbID : Ident, tgtTypeID : Ident, tgtID : Ident) : List[VWContentRq] = {
		val msgList : List[VWContentRq] = tgtTypeID match {
			case GoodyNames.TYPE_BOX => {
				Nil
			}
			case GoodyNames.TYPE_FLOOR => {
				Nil
			}
		}
		msgList
	}

}
