import java.io.*;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

public class CaptureFileSplitter {

	static String dirName = "functions";
	static final String FILE_PREFIX = "func_";
	static final String SYS_SEP = System.getProperty("file.separator");
	static final String SEP_REP = "sep";

	static int totalFunctionCalls = 0;
	static int savedFunctionCalls = 0;
    static BufferedWriter writer = null;
    static StringBuilder bufferGeneral = new StringBuilder();
    static StringBuilder bufferArguments = new StringBuilder();
    static Set<String> functions = new HashSet<>();
    static Map<String, Integer> funcFileCounter = new HashMap<>();
    static Map<String, HashSet<String>> funcArguments = new HashMap<>();
    static int fileIndex = 1;
    static boolean verbose;
	public static void main(String[] args) throws IOException {
        String fileName = args[0];
        File captureFile = new File(fileName);
        if (captureFile.exists() && !captureFile.isDirectory()) {
            if (args.length > 1) {
                dirName = args[1];
                verbose = Boolean.valueOf(args[2]);
            }
            createDir(dirName);
            doSplitting(fileName);
        } else {
            System.err.println("Specified file does not exists");
        }
    }

    private static void doSplitting(String fineName) {
        String line;
        String funcName = "";
        System.out.println("Starting split");
        try (BufferedReader reader = new BufferedReader(new InputStreamReader(
                new FileInputStream(fineName)))){
            while ((line = reader.readLine())!= null) {
                totalFunctionCalls++;
                // symbols and values
                line = ReadSymbolValues(reader, line);
                   while (!line.startsWith("func:"))
                       line = reader.readLine();
                // might be needed quote removal
                try{
                    funcName = line.substring(line.indexOf(':') + 2,
                            line.length());
                } catch (StringIndexOutOfBoundsException e){
                    System.out.println(line);
                    System.out.println(reader.readLine());
                    System.out.println(reader.readLine());
                    System.out.println(reader.readLine());
                    System.out.println("\n\n\n");
                    System.exit(1);;

                }
                if (verbose)
                    System.out.println("function name - " + funcName);
                // check if already encountered this function
                getFileIndex(funcName);

                line = ReadValue(Prefixes.FUNC_PREFIX, reader, line, false);
                line = ReadValue(Prefixes.ARGS_PREFIX, reader, line, true);
                line = ReadValue(Prefixes.WARN_PREFIX, reader, line, false);
                line = ReadValue(Prefixes.RETV_PREFIX, reader, line, false);
                ReadValue(Prefixes.ERRS_PREFIX, reader, line, false);
                bufferGeneral.append("\n"); // line break between captures
                if (!checkSameArguments(funcName)) {
                    writeDown(funcName);
                    savedFunctionCalls++;
                }
                // Renew for next step
                bufferArguments = new StringBuilder();
                bufferGeneral = new StringBuilder();
            }
        } catch (NullPointerException e){
            e.printStackTrace();
        } catch (Exception e) {
            e.printStackTrace();
        } finally {
        }
        System.out.println("Function calls processed - " + totalFunctionCalls);
        System.out.println("Function calls saved - " + savedFunctionCalls);
        System.out.println("Split finished");
    }

    private static String ReadSymbolValues(BufferedReader reader, String line) throws IOException {
        while (line.startsWith(Prefixes.SYM_PREFIX) || line.startsWith(Prefixes.VSYM_PREFIX)) {
            bufferGeneral.append(line + '\n');
            line = reader.readLine();
        }
        return line;
    }

    private static String ReadValue(String prefix, BufferedReader reader, String line, boolean args) throws IOException {
        while (line != null && line.startsWith(prefix)) {
            if (args)
                bufferArguments.append(line + "\n");
            bufferGeneral.append(line + "\n");
            line = reader.readLine();
        }
        return line;
    }

    // Make check take symbols and value into consideration also
    private static boolean checkSameArguments(String funcName){
        // check if already encountered this function
        if (!functions.contains(funcName)) {
            functions.add(funcName);
            createDir(dirName + SYS_SEP + FILE_PREFIX + funcName);
            funcFileCounter.put(funcName, fileIndex);
            HashSet<String> arguments = new HashSet<>();
            arguments.add(bufferArguments.toString());
            funcArguments.put(funcName, arguments);
        } else {
            HashSet<String> arguments = funcArguments.get(funcName);
            if (arguments.contains(bufferArguments.toString())) {
                return true;
            } else {
                arguments.add(bufferArguments.toString());
                funcArguments.put(funcName, arguments);
            }
        }
        return false;
    }

    private static void writeDown(String funcName) throws IOException {
            writer = new BufferedWriter(new OutputStreamWriter(
                    new FileOutputStream(dirName + SYS_SEP
                            + FILE_PREFIX + funcName + SYS_SEP
                            + FILE_PREFIX + funcName + "_" + fileIndex,
                            true)));
            writer.write(bufferGeneral.toString());
            writer.flush();
            writer.close();
    }

    private static void getFileIndex(String funcName){
        if (functions.contains(funcName)) {
            fileIndex = funcFileCounter.get(funcName);
            File forSizeCheck = new File(dirName + SYS_SEP
                    + FILE_PREFIX + funcName + SYS_SEP + FILE_PREFIX
                    + funcName + "_" + fileIndex);
            if (forSizeCheck.length() > 10 * 1000 * 1000) { // 50 MB
                fileIndex++;
                funcFileCounter.put(funcName, fileIndex);
            }
        }
    }
	private static void createDir(String dirName) {
		File theDir = new File(dirName);
		if (!theDir.exists()) {
			boolean result = theDir.mkdir();
			if (!result) {
				throw new RuntimeException("Could not create directory - "
						+ dirName + ". Please check previlages");
			}
		}
	}
}
