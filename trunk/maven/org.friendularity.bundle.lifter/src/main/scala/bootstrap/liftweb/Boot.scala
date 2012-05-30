package bootstrap.liftweb

import _root_.net.liftweb.util._
import _root_.net.liftweb.common._
import _root_.net.liftweb.http._
import _root_.net.liftweb.http.provider._
import _root_.net.liftweb.mapper._
import _root_.net.liftweb.sitemap._
import _root_.net.liftweb.sitemap.Loc._
import Helpers._
//import _root_.net.liftweb.mapper.{DB, ConnectionManager, Schemifier, DefaultConnectionIdentifier, StandardDBVendor}
import _root_.java.sql.{Connection, DriverManager}
import _root_.org.friendularity.bundle.lifter.model._
import _root_.org.cogchar.bind.lift.LiftAmbassador



/**
 * A class that's instantiated early and run.  It allows the application
 * to modify lift's environment
 * 
 * Currently a mess. It'll come together soon.
 */
class Boot {
  def boot {
	
	//println("Boot.scala here we go")
	/* Original DB loading
	 if (!DB.jndiJdbcConnAvailable_?) {
	  
	 val vendor = 
	 new StandardDBVendor(Props.get("db.driver") openOr "org.h2.Driver",
	 Props.get("db.url") openOr 
	 "jdbc:h2:lift_proto.db;AUTO_SERVER=TRUE",
	 Props.get("db.user"), Props.get("db.password"))
	  
	 */
	 
	 /* No DB for now because it is causing performance hit - maybe just from all DB log messages
	  * Also, we seem to need to add an h2 to POM, but isn't it supposed to come from appdapter
	  // A little different format from Lift in Action p.50
	  object vendor extends StandardDBVendor(
	  Props.get("db.class").openOr("org.h2.Driver"),
	  Props.get("db.url").openOr("jdbc:h2:lift_proto.db;AUTO_SERVER=TRUE"),
	  Props.get("db.user"),
	  Props.get("db.pass"))

	  LiftRules.unloadHooks.append(vendor.closeAllConnections_! _)

	  DB.defineConnectionManager(DefaultConnectionIdentifier, vendor)
	  
	  S.addAround(DB.buildLoanWrapper) // From Lift in Action p.49/50  
	  }
	  Schemifier.schemify(true, Schemifier.infoF _, User)
	  */

	 // where to search snippet
	 LiftRules.addToPackages("org.friendularity.bundle.lifter")


	 // SiteMap II: The re-constitution!
	 // We'll take it nice and easy.
	 val pushyMenu = Menu("pushy") / "index" // This very different format from Lift in Action p.45
	 val cogcharMenu = Menu(Loc("cogchar", ("cogchar" :: Nil) -> true, "Cogchar Interaction Internals")) // This is for the /cogchar directory and directories inside - format from Exploring Lift I think
	 val repoMenu = Menu(Loc("repo", ("repo" :: Nil) -> true, "Repo Ops")) // This is for the /repo directory and directories inside. The part we might actually want menus for!
	 //val sitemap = List(pushyMenu, cogcharMenu, repoMenu) ::: User.menus // Make sitemap list and append User menus - this version includes user admin stuff we may want in the future
	 val sitemap = List(pushyMenu, cogcharMenu) // Just what we need for Pushy right now
	 LiftRules.setSiteMap(SiteMap(sitemap:_*))
	 
	 // Build SiteMap
	 /* This is the old style one from the original Pushy 
	  def sitemap() = SiteMap(
	  Menu("Home") / "index" >> User.AddUserMenusAfter, // Simple menu form
	  // Menu with special Link
	  Menu(Loc("Static", Link(List("static"), true, "/static/index"), 
	  "Static Content"))) // huh, does this make everythingin static visible?
	 
	  LiftRules.setSiteMapFunc(() => User.sitemapMutator(sitemap()))
	
	  println("Boot.scala got past SiteMap config")
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
	 //println("Boot.scala got to end!")
	 }

	 /**
	  * Force the request to be UTF-8
	  */
	 private def makeUtf8(req: HTTPRequest) {
		req.setCharacterEncoding("UTF-8")
	  }
	 }
