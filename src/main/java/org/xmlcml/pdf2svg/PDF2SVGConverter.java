/**
 * Copyright (C) 2012 pm286 <peter.murray.rust@googlemail.com>
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.xmlcml.pdf2svg;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.FilenameFilter;
import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import javax.print.attribute.standard.PageRanges;

import nu.xom.Document;
import nu.xom.Serializer;

import org.apache.log4j.Logger;
import org.apache.pdfbox.exceptions.CryptographyException;
import org.apache.pdfbox.pdmodel.PDDocument;
import org.apache.pdfbox.pdmodel.PDPage;
import org.apache.pdfbox.util.PDFStreamEngine;
import org.xmlcml.font.CodePointSet;
import org.xmlcml.font.FontFamilySet;
import org.xmlcml.font.NonStandardFontManager;
import org.xmlcml.graphics.svg.SVGSVG;
import org.xmlcml.pdf2svg.log.XMLLogger;
import org.xmlcml.pdf2svg.util.MenuSystem;

/**
 * Simple app to read PDF documents based on PDFReader.java
 */
public class PDF2SVGConverter extends PDFStreamEngine {

	private static final String DEFAULT_OUTPUT_DIRECTORY = "target";

	private final static Logger LOG = Logger.getLogger(PDF2SVGConverter.class);

	private static final String PDF = ".pdf";
	private static final double _DEFAULT_PAGE_WIDTH = 600.0;
	private static final double _DEFAULT_PAGE_HEIGHT = 800.0;
	
	@SuppressWarnings("unused")
	private static final long serialVersionUID = 1L;

	public static final String DEBUG_CHAR_CODE = "-debugCharCode";
	public static final String DEBUG_CHAR_NAME = "-debugCharName";
	public static final String DEBUG_FONT_NAME = "-debugFontName";
	public static final String EXITONERR = "-exitonerr";
	public static final String INFO_FILES = "-infofiles";
	public static final String LOGGER = "-logger";
	public static final String LOGFILE = "-logfile";
	public static final String LOGMORE = "-logmore";
	public static final String LOGGLYPHS = "-logglyphs";
	public static final String MKDIR = "-mkdir";
	public static final String NO_SVG = "-nosvg";
	public static final String NONSEQ = "-nonseq";
	public static final String OUTDIR = "-outdir";
	public static final String PAGES = "-pages";
	public static final String PASSWORD = "-password";
	public static final String PUB = "-pub";
	public static final String STORE_SVG = "-storesvg";

	public static final String HTTP = "http";
	private static final int DEFAULT_MAX_PAGE = 200;

	private String PDFpassword = "";
//	private boolean useNonSeqParser = false;
	private boolean useNonSeqParser = true;
	private String outputDirectory = ".";
	private boolean basenameOutdir = false;
	private PageRanges pageRanges = null;

	private PDDocument document;
	private List<SVGSVG> svgPageList;
	private boolean fixFont = true;
	
	private NonStandardFontManager amiFontManager;
	private Map<String, AMIFont> amiFontMap;
	CodePointSet knownCodePointSet;
	CodePointSet newCodePointSet;
	File outdir;
	private int iarg;
	
	Double pageHeight = _DEFAULT_PAGE_HEIGHT;
	Double pageWidth = _DEFAULT_PAGE_WIDTH;
	private PDFPage2SVGConverter page2svgConverter;
	private SVGSVG currentSVGPage;
	public boolean writeFile = true;
	public boolean storeSVG = true;
	private boolean writeInfoFiles = false;
	private boolean exitOnError = false;
	
	public boolean drawBoxesForClipPaths = false;
	public boolean addTooltipDebugTitles = false;
	public boolean useXMLLogger = false;
	public XMLLogger xmlLogger = null;
	public String XMLLoggerFile = "pdfLog.xml";
	public boolean xmlLoggerLogGlyphs = false;
	public boolean xmlLoggerLogMore = false;

	private int maxPage = DEFAULT_MAX_PAGE;

	public String debugCharname = null;
	public Integer debugCharCode = null;
	public String debugFontName = null;

	private List<File> outfileList;
	private List<SVGSVG> svgList;
	private List<PDPage> pdPages;

	String inputBasename;
	int pageNumber;
	int imageNumber;

	public int maxInlineImageSize = 100; // size in pixels - arbitrary 

	public int getMaxPage() {
		return maxPage;
	}

	public void setMaxPage(int maxPage) {
		this.maxPage = maxPage;
	}

	private static void usage() {
		System.err
				.printf("Usage: pdf2svg [%s <pw>] [%s] [%s <page-ranges>] [%s <pub>] [%s <dir>] [%s] [%s]%n"
						+ "               [%s] [%s] [%s] [%s <filename>] [%s] [%s] [%s] <input-file(s)> ...%n%n"
						+ "  %s <password>  Password to decrypt the document (default none)%n"
						+ "  %s               Enables the new non-sequential parser%n"
						+ "  %s <page-ranges>  Restrict pages to be output (default all)%n"
						+ "  %s <publisher>      Use publisher-specific info%n"
						+ "  %s <dirname>     Location to write output SVG pages (default '.')%n"
						+ "  %s                Create dir in outdir using PDF basename and set as outdir for this PDF%n"
						+ "  %s                Don't write SVG files%n"
						+ "  %s             Store SVG%n"
						+ "  %s            Write info files%n"
						+ "  %s               Use XML logger to record unknown characters/fonts/etc%n"
						+ "  %s <filename>   Write the XML Logger output into 'filename' (default 'pdfLog.xml')%n"
						+ "  %s              log lots more characters (could produce a VERY big log)%n"
						+ "  %s            Attempt to include char glyphs as svg paths in the XML logger%n"
						+ "  %s            exit on PDF parse error (otherwise continue to next pdf)%n"
						+ "  %s            enter debug loop (with charCode)%n"
						+ "  %s            enter debug loop (with charName)%n"
						+ "  %s            enter debug loop (with fontName)%n"
						+ "  <input-file(s)>       The PDF document(s) to be loaded%n%n",
                                                PASSWORD, NONSEQ, PAGES, PUB, OUTDIR, MKDIR, NO_SVG, STORE_SVG,
                                                INFO_FILES, LOGGER, LOGFILE, LOGMORE, LOGGLYPHS, EXITONERR,
                                                PASSWORD, NONSEQ, PAGES, PUB, OUTDIR, MKDIR, NO_SVG, STORE_SVG,
                                                INFO_FILES, LOGGER, LOGFILE, LOGMORE, LOGGLYPHS, EXITONERR, DEBUG_CHAR_CODE, DEBUG_CHAR_NAME, DEBUG_FONT_NAME);
	}

	public void openPDFURL(String urlString) throws Exception {
		URL url = new URL(urlString);
		openPDFURL(url);
	}

	public void openPDFURL(URL url) throws IOException {
		InputStream is = url.openStream();
		openPDFInputStream(is);
		is.close();
	}

	public void openPDFInputStream(InputStream is) throws IOException {
		page2svgConverter = new PDFPage2SVGConverter();
		readDocument(is);
		openAndProcess((File) null, (URL) null);
	}

	public void openPDFFile(File file) throws Exception {
		svgPageList = null;
		page2svgConverter = new PDFPage2SVGConverter();
		LOG.trace("PDF " + file.getCanonicalPath());
		readDocument(file, useNonSeqParser, PDFpassword);
		openAndProcess(file, (URL) null);
		document.close();
	}

	private void openAndProcess(File inputFile, URL url) {
		pdPages = (List<PDPage>) document.getDocumentCatalog().getAllPages();

		pageRanges = new PageRanges(String.format("1-%d", pdPages.size()));

		if (useXMLLogger) {
			xmlLogger.newPDFFile(inputFile.getAbsolutePath(), pdPages.size());
		}

		LOG.debug(" .. pages "+pageRanges.toString()+" ("+pdPages.size()+") "); 

		createBasename(inputFile);
		createOutputDirectory(inputBasename);

		pageNumber = pageRanges.next(0);
		if (writeFile) {
			outfileList = new ArrayList<File>();
		}
		
		iterateOverPagesAndWriteFiles();

		if (writeInfoFiles) {
			reportHighCodePoints();
			reportNewFontFamilyNames();
			writeHTMLSystem(outfileList);
		}
	}

	private void iterateOverPagesAndWriteFiles() {
		while (pageNumber > 0) {
			PDPage page = pdPages.get(pageNumber - 1);
			imageNumber = 0;

			if (useXMLLogger) {
				xmlLogger.newPDFPage(pageNumber);
			}

			System.out.print(pageNumber + " = ");

			currentSVGPage = page2svgConverter.convertPageToSVG(page, this);

			if (storeSVG) {
				addPageToPageList();
			}
			if (writeFile) {
				File outfile = writeFile(pageNumber);
				outfileList.add(outfile);
			}

			pageNumber = pageRanges.next(pageNumber);
			if (pageNumber > maxPage) {
				LOG.error("terminated after "+pageNumber+" pages");
				break;
			}
		}
		System.out.println();
	}

	private void createBasename(File inputFile) {
		inputBasename = null;
		if (inputFile != null) {
			inputBasename = inputFile.getName().toLowerCase();
			if (inputBasename.endsWith(PDF)) {
				inputBasename = inputBasename.substring(0, inputBasename.length() - 4);
			}
		} else {
			inputBasename = DEFAULT_OUTPUT_DIRECTORY; // change later
		}
	}

	private void addPageToPageList() {
		ensureSVGPageList();
		SVGSVG svgPage = page2svgConverter.getSVG();
		svgPageList.add(svgPage);
	}

	private void createOutputDirectory(String basename) {
		if (basenameOutdir) {
			outdir = new File(outputDirectory, basename);
		} else {
			outdir = new File(outputDirectory);
		}
		if (!outdir.exists()) {
			outdir.mkdirs();
		}
		if (!outdir.isDirectory()) {
			throw new RuntimeException(String.format(
					"'%s' is not a directory!", outdir.getAbsoluteFile()));
		}
	}

	private File writeFile(int pageNumber) {

		File outfile = null;
		try {
			outfile = new File(outdir, inputBasename + "-page" + pageNumber + ".svg");
			LOG.trace("Writing output to file '"+outfile.getCanonicalPath());
			Serializer serializer = new SVGSerializer(new FileOutputStream(outfile), "UTF-8");
			Document document = currentSVGPage.getDocument();
			document = (document == null) ? new Document(currentSVGPage) : document;
			serializer.setIndent(1);
			serializer.write(document);
		} catch (Exception e) {
			throw new RuntimeException("Cannot convert PDF to SVG", e);
		}
		return outfile;
	}

	private void reportNewFontFamilyNames() {
		FontFamilySet newFontFamilySet = amiFontManager.getNewFontFamilySet();
		LOG.trace("new fontFamilyNames: "+newFontFamilySet.createElement().toXML());
	}

	private void writeHTMLSystem(List<File> outfileList) {
		MenuSystem menuSystem = new MenuSystem(outdir);
		menuSystem.writeDisplayFiles(outfileList, "");
	}

	private void ensureSVGPageList() {
		if (svgPageList == null) {
			svgPageList = new ArrayList<SVGSVG>();
		}
	}

	private void readDocument(File file, boolean useNonSeqParser, String password) throws IOException {
		if (useNonSeqParser) {
			document = PDDocument.loadNonSeq(file, null, password);
		} else {
			document = PDDocument.load(file);
			if (document.isEncrypted()) {
				try {
					document.decrypt(password);
				} catch (CryptographyException e) {
					System.err
							.printf("Error: Failed to decrypt document in file '%s'.%n",
									file.getAbsolutePath());
					return;
				}
			}
		}

	}

	private void readDocument(InputStream inputStream) throws IOException {
		document = PDDocument.load(inputStream);
		/*if (document.isEncrypted()) {
			try {
				document.decrypt(password);
			} catch (InvalidPasswordException e) {
				System.err
						.printf("Error: The document in inputSstream is encrypted (use '-password' option).%n");
				return;
			} catch (CryptographyException e) {
				System.err
						.printf("Error: Failed to decrypt document in inputStream");
				return;
			}
		}*/
	}

	public static void main(String[] args) throws Exception {

		PDF2SVGConverter converter = new PDF2SVGConverter();

		if (!converter.run(args))
			System.exit(1);

		System.exit(0);
	}

	public boolean run(String argString) {
		return run(argString.split("[\\s+]"));
	}

	public boolean run(String... args) {

		if (args.length == 0) {
			usage();
			return false;
		}

		List<String> fileList = new ArrayList<String>();

		for (iarg = 0; iarg < args.length; iarg++) {

			LOG.trace(args[iarg]);
			if (args[iarg].equals(PASSWORD)) {
				if (!incrementArg(args))
					return false;
				PDFpassword = args[iarg];
				continue;
			}

			if (args[iarg].equals(NONSEQ)) {
				useNonSeqParser = true;
				continue;
			}

			if (args[iarg].equals(OUTDIR)) {
				if (!incrementArg(args))
					return false;
				outputDirectory = args[iarg];
				continue;
			}

			if (args[iarg].equals(MKDIR)) {
				basenameOutdir = true;
				continue;
			}

			if (args[iarg].equals(NO_SVG)) {
				setWriteFile(false);
				continue;
			}

			if (args[iarg].equals(STORE_SVG)) {
				setStoreSVG(true);
				continue;
			}

			if (args[iarg].equals(DEBUG_CHAR_CODE)) {
				if (!incrementArg(args))
					return false;
				debugCharCode = new Integer(args[iarg]);
				continue;
			}

			if (args[iarg].equals(DEBUG_CHAR_NAME)) {
				if (!incrementArg(args))
					return false;
				debugCharname = args[iarg];
				continue;
			}

			if (args[iarg].equals(DEBUG_FONT_NAME)) {
				if (!incrementArg(args))
					return false;
				debugFontName = args[iarg];
				continue;
			}

			if (args[iarg].equals(INFO_FILES)) {
				writeInfoFiles = true;
				continue;
			}

			if (args[iarg].equals(LOGGER)) {
				useXMLLogger = true;
				continue;
			}

			if (args[iarg].equals(LOGFILE)) {
				if (!incrementArg(args))
					return false;
				XMLLoggerFile = args[iarg];
				continue;
			}

			if (args[iarg].equals(LOGMORE)) {
				xmlLoggerLogMore = true;
				continue;
			}

			if (args[iarg].equals(LOGGLYPHS)) {
				xmlLoggerLogGlyphs = true;
				continue;
			}

			if (args[iarg].equals(EXITONERR)) {
				exitOnError = true;
				continue;
			}

			if (args[iarg].equals(PAGES)) {
				if (!incrementArg(args))
					return false;
				pageRanges = new PageRanges(args[iarg]);
				continue;
			}

			if (args[iarg].startsWith("-")) {
				throw new RuntimeException("Unknown arg: "+args[iarg]);
			}

			fileList.add(args[iarg]);
		}

		if (fileList.size() == 0) {
			usage();
			return false;
		}

		ensureXMLLogger();

		boolean succeeded = true;

		for (String filename : fileList) {
			try {
				readFileOrDirectoryOrURL(filename);
			} catch (Exception e) {
				e.printStackTrace();
				System.err.printf("Cannot parse PDF '" + filename + "':" + e+"\n");
				if (exitOnError) {
					return false;
				}
				succeeded = false;
			}
		}

		writeXMLLoggerOutput();

		return succeeded;
	}

	public void setWriteFile(boolean b) {
		writeFile = b;
	}

	public void setStoreSVG(boolean b) {
		storeSVG = b;
	}

	private void ensureXMLLogger() {
		if (useXMLLogger && xmlLogger == null)
			xmlLogger = new XMLLogger(xmlLoggerLogGlyphs);
	}

	private void writeXMLLoggerOutput() {
		if (useXMLLogger) {
			File outfile = new File(outdir, XMLLoggerFile);
			try {
				LOG.debug("Writing XML logger output to file '"+outfile.getCanonicalPath()+"'.");
			} catch (IOException e) {
				throw new RuntimeException("caught I/O Exception while writing XML Logger output");
			}
			try {
				xmlLogger.writeXMLFile(new FileOutputStream(outfile));
			} catch (FileNotFoundException e) {
				throw new RuntimeException("caught File Not Found Exception while writing XML Logger output");
			}
		}
	}

	private void readFileOrDirectoryOrURL(String filename) {
		if (filename == null) {
			throw new RuntimeException("No input filename");
		} else if (filename.startsWith(HTTP)) {
			try {
				openPDFURL(filename);
			} catch (Exception e) {
				LOG.error("Failed to convert URL: "+filename, e);
			}
		} else {
			File file = new File(filename);
			if (!file.exists()) {
				throw new RuntimeException("File does not exist: " + filename);
			}
			if (file.isDirectory()) {
				File[] pdfFiles = file.listFiles(new FilenameFilter() {
					public boolean accept(File dir, String filename) {
						return filename.endsWith(PDF);
					}
				});
				if (pdfFiles != null) {
					for (File pdf : pdfFiles) {
						try {
							openPDFFile(pdf);
						} catch (Exception e) {
							LOG.error("Failed to convert file: "+pdf+", skipping", e);
						}
					}
				}
			} else {
				try {
					openPDFFile(file);
				} catch (Exception e) {
					throw new RuntimeException("Cannot convert file: "+file, e);
				}
			}
		}
	}

	private boolean incrementArg(String... args) {
		iarg++;
		if (iarg >= args.length) {
			usage();
			return false;
		}
		return true;
	}

	public List<SVGSVG> getPageList() {
		ensureSVGPageList();
		return svgPageList;
	}

	private void reportHighCodePoints() {
		ensureCodePointSets();
		int newCodePointCount = newCodePointSet.size();
		if (newCodePointCount > 0) {
			LOG.trace("New High CodePoints: " + newCodePointSet.size());
			LOG.trace(newCodePointSet.createElementWithSortedIntegers().toXML());
		}
	}

	void ensureCodePointSets() {
		if (newCodePointSet == null) {
			newCodePointSet = new CodePointSet();
		}
		if (knownCodePointSet == null) {
			knownCodePointSet = CodePointSet.readCodePointSet(CodePointSet.UNICODE_POINT_SET_XML); 
		}
	}

	public CodePointSet getKnownCodePointSet() {
		ensureCodePointSets();
		return knownCodePointSet;
	}

	public CodePointSet getNewCodePointSet() {
		ensureCodePointSets();
		return newCodePointSet;
	}

	public void setFixFont(boolean fixFont) {
		this.fixFont = fixFont;
	}

	public boolean isFixFont() {
		return fixFont;
	}
	
	public NonStandardFontManager getAmiFontManager() {
		ensureAmiFontManager();
		return amiFontManager;
	}

	private void ensureAmiFontManager() {
		if (amiFontManager == null) {
			amiFontManager = new NonStandardFontManager();
			amiFontMap = NonStandardFontManager.readAmiFonts();
			for (String fontName : amiFontMap.keySet()) {
				AMIFont font = amiFontMap.get(fontName);
			}
		}
	}

	Map<String, AMIFont> getAMIFontMap() {
		ensureAmiFontManager();
		return amiFontMap;
	}

}
