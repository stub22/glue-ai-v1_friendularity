package org.friendularity.vwimpl

import java.util.{ArrayList => JArrayList, Map => JMap}

import com.hp.hpl.jena.rdf.model.{Model => JenaModel}

/** README
  * As of June 2016, none of this VWChef.scala code is used in CCMIO demo.
  * However, it provides important reference points and documented design info.
  * ---------------------------------------------------------------------------
  *
  * Chef uses modern recipes and fresh chunks, to prepare tasty delights!
  *
  * Chef is responsible for preparing the state of the runtime, using the
  * same ingredients (config-records translated to config-messages) that any other client
  * can also send programmatically (as RDF, Java-serialized message, HTTP/JSON,
  * QPid/Avro, etc).  Nothing the Chef does is magic; it's just hard work that
  * Chef is good at doing quickly.
  *
  * Chef likes 3 kinds of chunks:
  *
  * grump - and other character sets, which set up motion, speech, and intrinsic char behavior.
  * vwon - and other vworld-conf sets, which set up the vworld canvas, lighting, cameras, facilities
  * funpak - and other goody-conf/game/content+rule sets
  *
  * It is not Chef's job to select/load the appro-profile from disk, that happens upstream of her.
  * The profile she receives contains all the recipes she needs.  Using those she is able to scan
  * the 3 kinds of chunks listed above, looking for relevant config/content data graphs and URIs.
  */

class VWChef {
	def prepareDishes(mergedProfJM : JenaModel) : Unit = {
		// Starting with:
		// 0A) existing profile recipes already loaded, and
		// 0B) An appropriate way to get eHosts for the given platform, which may use profile #0A as much
		// as appropriate, as well as injected services.  This part is suitable
		// for lifecycle/registry integration, especially the eHost connection.  Also works
		// OK from regular classpath, when OSGi is not present.

		// Do:
		// 1) Find recipes for nexuses and circuses (nexi y circi = crosses and circles).
		// Each of these corresponds to a channel-actor that we must find or create
		// (steps 2 + 3), then pump up with initial population in step 4.

		// 2) Get all the connected feature-broker config recipes.

		// 3) Load/find the corresponding chunks, from appropriate eHosts.  (See #0B above).

		// 4) Send appropriate population messages to appropriate channel-actors, creating those
		// actors when needed.

		// 5) Mark actors with appropriate recipe URIs (or related value-data) so they can be found by clients,
		// either/both implicitly using  lower level Actor paths, or more explicitly as tellers known to CPump sys.
		// The latter is preferred.
	}
}




