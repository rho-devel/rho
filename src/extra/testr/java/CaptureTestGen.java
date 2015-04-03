import java.io.BufferedWriter;
import java.io.IOException;
import java.nio.charset.Charset;
import java.nio.file.*;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.*;
import java.util.concurrent.LinkedBlockingQueue;

public class CaptureTestGen {

	static Path inputDir;
	static Path outputDir;
	static boolean verbose = false;
	static String TEST_GEN;
	static volatile boolean done = false;

	final static LinkedBlockingQueue<Path> ifileQueue = new LinkedBlockingQueue<Path>();

	public static void main(String[] args) {
		processArgument(args);
		setupWorkEnv();
		processFiles();
	}

	private static void processFiles() {
		if (verbose) {
			System.out.println("Read from: " + inputDir.toAbsolutePath());
			System.out.println("Output to: " + outputDir.toAbsolutePath());
		}
		try {
			if (!Files.isDirectory(inputDir)) {
				HashMap<String, HashSet<FuncEntry>> entryMap = processFile(inputDir);
				genTests(entryMap);
			} else {
				DirectoryStream<Path> seeds = Files
						.newDirectoryStream(inputDir);
				for (Path seed : seeds) {
					if (!Files.isDirectory(seed)) {
						HashMap<String, HashSet<FuncEntry>> entryMap = processFile(seed);
						genTests(entryMap);
					} else {
						DirectoryStream<Path> cseeds = Files
								.newDirectoryStream(seed);
						for (Path cseed : cseeds) {
							HashMap<String, HashSet<FuncEntry>> entryMap = processFile(cseed);
							genTests(entryMap);
						}
					}
				}
			}
		} catch (IOException e) {
			e.printStackTrace();
			System.exit(0);
		}
		done = true;
	}

	private static void genTests(HashMap<String, HashSet<FuncEntry>> entryMap)
			throws IOException {
		Random random = new Random();
		String ifileName = Long.toString(System.currentTimeMillis()
				+ random.nextInt(200000000));
		Path ifile = Files.createFile(outputDir.resolve(ifileName));
		BufferedWriter writer = Files.newBufferedWriter(ifile,
				Charset.defaultCharset());
		for (HashSet<FuncEntry> entries : entryMap.values()) {
			for (FuncEntry entry : entries) {
                writer.write(entry.symb);
				writer.write(entry.func);
				writer.write(entry.body);
				writer.write(entry.args);
				writer.write(entry.warn);
                writer.write(entry.retv);
                writer.write(entry.errs);
				writer.write('\n');
			}
		}
		writer.flush();
		writer.close();
		try {
			ifileQueue.put(ifile);
		} catch (InterruptedException e) {
			e.printStackTrace();
		}
	}

	private static HashMap<String, HashSet<FuncEntry>> processFile(Path seed)
			throws IOException {
		if (verbose) {
			System.out.println(">>> processing file: " + seed.toAbsolutePath()
					+ "...");
		}
		List<String> lines = Files.readAllLines(seed, Charset.defaultCharset());
		HashMap<String, HashSet<FuncEntry>> entryMap = new HashMap<>();
		String[] linesArr = new String[lines.size()];
		lines.toArray(linesArr);
		assert (linesArr.length == 0);
		int[] s = { 0 };
		while (s[0] != linesArr.length) {
			FuncEntry entry = processEntry(linesArr, s);
			HashSet<FuncEntry> set = entryMap.get(entry.func);
			if (set == null) {
				set = new HashSet<>();
			}
			set.add(entry);
			entryMap.put(entry.func, set);
		}
		return entryMap;
	}
    private static int s;

	private static FuncEntry processEntry(String[] lines, int[] ss) {
		s = ss[0];
		FuncEntry entry = new FuncEntry();
		assert (s < lines.length);
        entry.symb = ReadSymbolValue(lines);
        entry.func = ReadValue(lines, Prefixes.FUNC_PREFIX);
        entry.body = ReadValue(lines, Prefixes.BODY_PREFIX);
        entry.args = ReadValue(lines, Prefixes.ARGS_PREFIX);
        entry.warn = ReadValue(lines, Prefixes.WARN_PREFIX);
        entry.retv = ReadValue(lines, Prefixes.RETV_PREFIX);
        entry.errs = ReadValue(lines, Prefixes.ERRS_PREFIX);
		ss[0] = ++s;
		return entry;
	}

    private static String ReadSymbolValue(String []lines){
        String result = "";
        while (lines[s].startsWith(Prefixes.SYM_PREFIX) || lines[s].startsWith(Prefixes.VSYM_PREFIX)) {
            result += lines[s] + "\n";
            s++;
        }
        return result;
    }

    private static String ReadValue(String []lines, String prefix){
        StringBuilder result = new StringBuilder() ;
        while (s < lines.length && lines[s].startsWith(prefix)) {
            result.append(lines[s]);
	    result.append("\n");
	    s++;
        }
        return result.toString();
    }
	private static void setupWorkEnv() {
		if (!Files.exists(inputDir, LinkOption.NOFOLLOW_LINKS)) {
			System.err.println("Cannot find input directory: " + inputDir);
			System.exit(0);
		}
		if (!Files.exists(outputDir, LinkOption.NOFOLLOW_LINKS)) {
			System.err.println("Cannot find output directory: " + outputDir);
			System.exit(0);
		}
		DateFormat dateFormat = new SimpleDateFormat("yyyy_MM_dd_HH_mm_ss");
		outputDir = outputDir.resolve(dateFormat.format(new Date()));
		try {
			Files.createDirectories(outputDir);
		} catch (IOException e) {
			e.printStackTrace();
			System.exit(0);
		}
		new Thread(new RscriptExecutor()).start();
	}

	static void processArgument(String[] args) {
		if (args.length < 2) {
			System.out
					.println("Usage: CaptureTestGen <input_dir> <output_dir> <optional: true | false>");
			System.exit(0);
		}
		inputDir = FileSystems.getDefault().getPath(args[0]);
		outputDir = FileSystems.getDefault().getPath(args[1]);
		if (args.length > 2) {
			verbose = Boolean.valueOf(args[2]);
		}
	}

	static class FuncEntry {
		String func;
        String symb;
		String body;
		String args;
        String warn;
		String retv;
        String errs;

		@Override
		public int hashCode() {
			return symb.hashCode() ^ func.hashCode() ^ body.hashCode() ^ args.hashCode()
					^ retv.hashCode();
		}

		@Override
		public boolean equals(Object o) {
			if (!(o instanceof FuncEntry))
				return false;
			FuncEntry o1 = (FuncEntry) o;
			return o1.symb.equals(symb) && o1.func.equals(func) && o1.body.equals(body)
					&& o1.args.equals(args) && o1.retv.equals(retv);
		}

	}

	static class RscriptExecutor implements Runnable {
		public void run() {
			while (!ifileQueue.isEmpty() || !done) {
				try {
					process(ifileQueue.take());
				} catch (IOException | InterruptedException e) {
					e.printStackTrace();
				}
			}
		}

		void process(Path ifile) throws IOException {
			Process proc = null;
//		    throw new RuntimeException("Forced exit");
			System.out.println("Processing file - " + ifile.toString());
            String testGen = "R -q -e library(testr) -e library(rcov) -e TestGen(\"" + ifile.toAbsolutePath().toString() + "\"," + "\"" + outputDir.toAbsolutePath().toString() + "\");";
//            String testGen = "R -q -e cat(\"Roman\")";
            System.out.println(testGen);
           proc = Runtime.getRuntime().exec(testGen);
			boolean doneWait = false;
			while (!doneWait) {
				try {
					doneWait = true;
					proc.waitFor();
				} catch (InterruptedException e) {
					doneWait = false;
				}
			}
//            String line;
//            BufferedReader in = new BufferedReader(
//                    new InputStreamReader(proc.getInputStream()) );
//            while ((line = in.readLine()) != null) {
//                System.out.println(line);
//            }
//            in = new BufferedReader(
//                    new InputStreamReader(proc.getErrorStream()) );
//            while ((line = in.readLine()) != null) {
//                System.out.println(line);
//            }
//            in.close();
			Files.delete(ifile);
		}
	}
}
