package bootstrap.liftweb

import _root_.net.liftweb.util._
import _root_.net.liftweb.common._
import _root_.net.liftweb.http._
import _root_.net.liftweb.http.provider._
import _root_.net.liftweb.sitemap._
import _root_.net.liftweb.sitemap.Loc._
import Helpers._
import _root_.net.liftweb.mapper.{DB, ConnectionManager, Schemifier, DefaultConnectionIdentifier, StandardDBVendor}
import _root_.java.sql.{Connection, DriverManager}
import _root_.org.friendularity.bundle.lifter.model._
import _root_.org.friendularity.bundle.lifter.commander._
import _root_.org.cogchar.bind.lift.LiftAmbassador


/**
 * A class that's instantiated early and run.  It allows the application
 * to modify lift's environment
 */
class Boot {
  def boot {
	
	/* Causing horrible exceptions and none of the rest of Boot.scala runs (I discovered after being utterly confused for 30 min!)
	 * A lot of this DB stuff is just crashing, so for now I'll kill it and figure out what we really need going forward
	 if (!DB.jndiJdbcConnAvailable_?) {
	 val vendor = 
	 new StandardDBVendor(Props.get("db.driver") openOr "org.h2.Driver",
	 Props.get("db.url") openOr 
	 "jdbc:h2:lift_proto.db;AUTO_SERVER=TRUE",
	 Props.get("db.user"), Props.get("db.password"))

	 LiftRules.unloadHooks.append(vendor.closeAllConnections_! _)

	 DB.defineConnectionManager(DefaultConnectionIdentifier, vendor)
	 }
	 */

    // where to search snippet
    LiftRules.addToPackages("org.friendularity.bundle.lifter")
	
	/*
	 Schemifier.schemify(true, Schemifier.infoF _, User)

	 // Build SiteMap
	 def sitemap() = SiteMap(
	 Menu("Home") / "index" >> User.AddUserMenusAfter, // Simple menu form
	 // Menu with special Link
	 Menu(Loc("Static", Link(List("static"), true, "/static/index"), 
	 "Static Content")))

	 LiftRules.setSiteMapFunc(() => User.sitemapMutator(sitemap()))
	
	 */

    /*
     * Show the spinny image when an Ajax call starts
     */
    LiftRules.ajaxStart =
      Full(() => LiftRules.jsArtifacts.show("ajax-loader").cmd)

    /*
     * Make the spinny image go away when it ends
     */
    LiftRules.ajaxEnd =
      Full(() => LiftRules.jsArtifacts.hide("ajax-loader").cmd)

    LiftRules.early.append(makeUtf8)

    //LiftRules.loggedInTest = Full(() => User.loggedIn_?)

    //S.addAround(DB.buildLoanWrapper) Disabling for now due to other DB problems to identify...
	
	// Establish connection from LiftAmbassador into PageCommander
	LiftAmbassador.setLiftMessenger(PageCommander.getMessenger)
	
	// Is config already ready? If so, we missed it. Let's update now.
	if (LiftAmbassador.checkConfigReady) {
	  PageCommander.initFromCogcharRDF
	}

  }

  /**
   * Force the request to be UTF-8
   */
  private def makeUtf8(req: HTTPRequest) {
    req.setCharacterEncoding("UTF-8")
  }
}
