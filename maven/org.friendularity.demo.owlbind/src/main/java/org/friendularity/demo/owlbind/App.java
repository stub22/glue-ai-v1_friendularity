package org.friendularity.demo.owlbind;

import java.io.File;
import org.semanticweb.owlapi.apibinding.OWLManager;
import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLOntologyCreationException;
import org.semanticweb.owlapi.model.OWLOntologyManager;
import org.semanticweb.owlapi.util.SimpleIRIMapper;

/**
 * Hello world!
 *
 */
public class App {

	public static void main(String[] args) {
		System.out.println("Hello World!");
		App a = new App();
		try {
			a.shouldLoad();

		} catch (Throwable t) {
			t.printStackTrace();
		}
	}
	// private static final String PIZZA_IRI =  "http://owl.cs.manchester.ac.uk/co-ode-files/ontologies/pizza.owl";
	// 										 "http://owl.cs.manchester.ac.uk/co-ode-files/ontologies/pizza.owl"
	public String pizzaURL = "http://130.88.198.11/co-ode-files/ontologies/pizza.owl";
	/**
	 * The examples here show how to load ontologies.
	 *
	 * @throws OWLOntologyCreationException
	 */
	public void shouldLoad() throws OWLOntologyCreationException {
		// Get hold of an ontology manager
		OWLOntologyManager manager = OWLManager.createOWLOntologyManager();
		// Let's load an ontology from the web
		IRI iri = IRI.create(pizzaURL);
		OWLOntology pizzaOntology = manager.loadOntologyFromOntologyDocument(iri);
		System.out.println("Loaded ontology: " + pizzaOntology);
		// Remove the ontology so that we can load a local copy.
		manager.removeOntology(pizzaOntology);
		// We can also load ontologies from files. Download the pizza ontology
		// from http://owl.cs.manchester.ac.uk/co-ode-files/ontologies/pizza.owl
		// and put it
		// somewhere on your hard drive Create a file object that points to the
		// local copy
		File file = new File("/tmp/pizza.owl");
		// Now load the local copy
		OWLOntology localPizza = manager.loadOntologyFromOntologyDocument(file);
		System.out.println("Loaded ontology: " + localPizza);
		// We can always obtain the location where an ontology was loaded from
		IRI documentIRI = manager.getOntologyDocumentIRI(localPizza);
		System.out.println("    from: " + documentIRI);
		// Remove the ontology again so we can reload it later
		manager.removeOntology(pizzaOntology);
		// In cases where a local copy of one of more ontologies is used, an
		// ontology IRI mapper can be used to provide a redirection mechanism.
		// This means that ontologies can be loaded as if they were located on
		// the web. In this example, we simply redirect the loading from
		// http://owl.cs.manchester.ac.uk/co-ode-files/ontologies/pizza.owl to
		// our local copy
		// above.
		manager.addIRIMapper(new SimpleIRIMapper(iri, IRI.create(file)));
		// Load the ontology as if we were loading it from the web (from its
		// ontology IRI)
		IRI pizzaOntologyIRI = IRI
			.create("http://owl.cs.manchester.ac.uk/co-ode-files/ontologies/pizza.owl");
		OWLOntology redirectedPizza = manager.loadOntology(pizzaOntologyIRI);
		System.out.println("Loaded ontology: " + redirectedPizza);
		System.out.println("    from: "
			+ manager.getOntologyDocumentIRI(redirectedPizza));
		// Note that when imports are loaded an ontology manager will be
		// searched for mappings
	}
}
