package org.friendularity.util

import org.appdapter.core.name.{FreeIdent, Ident}

import scala.util.Random

/**
  * Code moved here on 1/18/2017.
  */
trait IdentHlp {
	val uniqueR = new Random()

	val noSuffix = ""
	val idSuffix = "#id"

	def makeStampyRandyString (prefix : String, suffix : String) : String = {
		val rnum = uniqueR.nextInt(1000 * 1000)
		val tstamp = System.currentTimeMillis()
		String.format("%s%d_%06d%s", prefix, tstamp : java.lang.Long, rnum : Integer, suffix)
	}
	def makeStampyRandyIdentAnon() : Ident = {
		makeStampyRandyIdent("")
	}
	def makeStampyRandyIdent(shortLab : String) : Ident = {
		val prePre = "urn:sri_"
		val prefix : String = if (shortLab.length > 0) {prePre + shortLab + "_"} else prePre
		val uriTxt = makeStampyRandyString(prefix, idSuffix)
		val id = new FreeIdent(uriTxt)
		id
	}
}
