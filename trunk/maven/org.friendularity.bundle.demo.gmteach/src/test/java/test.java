import org.friendularity.bundle.demo.gmteach.GMTEACH_DemoActivator;

public class test {

	public static void main(String[] args) throws Exception {
		System.err.println("This is a fake maven target");
		//ScriptEngineExperiment.class;
		String jvmArgs[] = new String[] { "-Xmx20G", "-XX:MaxPermSize=2G", "-Dlog4j.debug", "-Dfelix.config.properties=file:feclipse.properties", "-Dcom.hrkind.robot.connections=robot01; 127.0.0.1, robot02; 127.0.0.1, avatar01; 127.0.0.1",//
				"-Dcom.hrkind.demo.behavior.master.source=sheetKey;0ArBjkBoH40tndDdsVEVHZXhVRHFETTB5MGhGcWFmeGc, namespaceSheetNumber;9, directorySheetNumber;8, workBookPath;GluePuma_BehavMasterDemo.xlsx, workBookNamespaceSheet;Nspc.csv, workBookDirectorySheet;Dir.csv" };
		for (String ja : jvmArgs) {
			String jas[] = ja.split("=");
			String v = "";
			String p = jas[0];
			if (p.startsWith("-D"))
				p = p.substring(2);
			if (jas.length == 2) {
				v = jas[1];
			}
			System.setProperty(p, v);
		}
		System.getProperties().save(System.out, GMTEACH_DemoActivator.class.getSuperclass().getName());
		org.apache.felix.main.Main.main(args);
	}

}
