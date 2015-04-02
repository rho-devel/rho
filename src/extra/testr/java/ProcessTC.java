import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.util.Arrays;
import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.LinkedBlockingQueue;

public class ProcessTC {
	private static String R = "R";
	private static volatile ConcurrentLinkedQueue<String> tcFileAddr = new ConcurrentLinkedQueue<>();
	private static volatile ConcurrentLinkedQueue<String> tcFileNames = new ConcurrentLinkedQueue<>();
	private static volatile ConcurrentLinkedQueue<String> virtualMachines = new ConcurrentLinkedQueue<>();
	private static volatile String tcResultLocation;
	static ProcessTC pr = new ProcessTC();
	private static String tcDB;

	public static void main(String[] args) throws IOException,
			InterruptedException {
		if (args.length < 3) {
			throw new RuntimeException(
					"Usage : <folder to process>\n"
							+ "<filtering result location> <R vms file> OPTIONAL <TC DB Folder use NONE if no TC db> \n");
		}
		String tcFolder = args[0];
		tcResultLocation = args[1];
		String virtualMachinesFile = args[2];
		if (args.length == 4) {
			tcDB = args[3];
			if (!folderExists(tcDB))
				throw new RuntimeException(
						"Specified folder with TC Databases does not exist!");
			else
				tcDB = new File(tcDB).getAbsolutePath();
		}
		if (!folderExists(tcFolder))
			throw new RuntimeException(
					"Specified folder with TCs does not exist!");
		if (!folderExists("info"))
			(new File("info")).mkdir();
		File tcFolderFile = new File(tcFolder);
		listFilesForFolder(tcFolderFile);
		readVMs(virtualMachinesFile);

		System.out.println("Number of files - " + tcFileAddr.size());
		int cores = Runtime.getRuntime().availableProcessors();
		System.out.println("Number of available cores - " + cores);
		System.out.println("Number of available VMs - "
				+ virtualMachines.size());
		int threads = cores < virtualMachines.size() ? cores : virtualMachines
				.size();
		ExecutorService executor = Executors.newFixedThreadPool(threads);
		for (int i = 0; i < tcFileAddr.size() + threads; i++) {
			Runnable worker = pr.new TCThread(i);
			executor.execute(worker);
		}
		executor.shutdown();
		// Think how to make it sleep or maybe delete this line
		while (!executor.isTerminated()) {
		}
		System.out.println("Finished all threads");
	}

	private static boolean folderExists(String folderName) {
		File folder = new File(folderName);
		if (!folder.exists())
			return false;
		return true;
	}

	private static void readVMs(String virtualMachinesFile) throws IOException {
		BufferedReader br = new BufferedReader(new FileReader(
				virtualMachinesFile));
		String line;
		while ((line = br.readLine()) != null) {
			virtualMachines.add(line);
		}
		br.close();
	}

	private static void listFilesForFolder(final File folder) {
		for (final File fileEntry : folder.listFiles()) {
			if (fileEntry.isDirectory()) {
				listFilesForFolder(fileEntry);
			} else {
				tcFileAddr.add(fileEntry.getAbsolutePath());
				tcFileNames.add(fileEntry.getName());
			}
		}
	}

	private class TCThread implements Runnable {
		private int index;

		public TCThread(int i) {
			super();
			index = i;
		}

		@Override
		public void run() {
			String vm = "", name, file, sline = null, eline = null;
			int exit;
			try {
				synchronized (this) {
					vm = virtualMachines.poll();
					file = tcFileAddr.poll();
					name = tcFileNames.poll();
				}
				BufferedWriter writer = new BufferedWriter(
						// System dependent change it
						new OutputStreamWriter(new FileOutputStream(
								"./info/" + name + "_info", true)));
				System.out.println("Starting file - " + file);
				String processCall = String.format("-e processTC(\'%s\',\'%s\',%s,\'%s\',\'src/main\')", 
					file, tcResultLocation, (tcDB == null) ? "NULL" : "'" + tcDB + "'", vm);
				String command = "R -q -e library(rcov) -e library(testr) -e library(tools) " + processCall;
				writer.write(command + "\n");
				Runtime rt = Runtime.getRuntime();
				Process proc = rt.exec(command);
				// proc.waitFor();

				BufferedReader stdInput = new BufferedReader(
						new InputStreamReader(proc.getInputStream()));

				BufferedReader stdError = new BufferedReader(
						new InputStreamReader(proc.getErrorStream()));

				while (((sline = stdInput.readLine()) != null)
						|| ((eline = stdError.readLine()) != null)) {
					if (sline != null)
						writer.write(sline + "\n");
					if (eline != null)
						writer.write(eline + "\n");
					writer.flush();
					try {
						exit = proc.exitValue();
						if (exit == 0) {
						}
					} catch (IllegalThreadStateException t) {

					}
				}
				writer.close();
				System.out.println("Finished File - " + file
						+ ". Files left - " + tcFileAddr.size());

				virtualMachines.add(vm);
				stdError.close();
				stdInput.close();
			} catch (Exception e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}
	}
}
