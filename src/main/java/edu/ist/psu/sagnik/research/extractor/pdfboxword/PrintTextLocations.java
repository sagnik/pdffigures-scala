package edu.ist.psu.sagnik.research.extractor.pdfboxword;

/**
 * Created by sagnik on 10/30/15.
 */
import java.io.File;
import java.io.IOException;
import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.apache.pdfbox.exceptions.CryptographyException;
import org.apache.pdfbox.pdmodel.PDDocument;
import org.apache.pdfbox.pdmodel.PDPage;
import org.apache.pdfbox.pdmodel.common.PDStream;
import org.apache.pdfbox.util.PDFTextStripper;
import org.apache.pdfbox.util.TextPosition;

public class PrintTextLocations extends PDFTextStripper {

    public static StringBuilder tWord = new StringBuilder();
    //public static String seek;
    //public static String[] seekA;
    public static List wordList = new ArrayList();
    public static boolean is1stChar = true;
    public static boolean lineMatch;
    public static int pageNo = 1;
    public static double lastYVal;

    public PrintTextLocations()
            throws IOException {
        super.setSortByPosition(true);
    }

    public static void main(String[] args)
            throws Exception {
        PDDocument document = null;
        //seekA = args[1].split(",");
        //seek = args[1];
        try {
            File input = new File(args[0]);
            document = PDDocument.load(input);
            if (document.isEncrypted()) {
                try {
                    document.decrypt("");
                } catch (CryptographyException e) {
                    System.err.println("Error: Document is encrypted with a password.");
                    System.exit(1);
                }
            }
            PrintTextLocations printer = new PrintTextLocations();
            List allPages = document.getDocumentCatalog().getAllPages();

            for (int i = 0; i < allPages.size(); i++) {
                PDPage page = (PDPage) allPages.get(i);
                PDStream contents = page.getContents();

                if (contents != null) {
                    printer.processStream(page, page.findResources(), page.getContents().getStream());
                }
                pageNo += 1;
            }
        } finally {
            if (document != null) {
                System.out.println(wordList);
                document.close();
            }
        }
    }

    @Override
    protected void processTextPosition(TextPosition text) {
        String tChar = text.getCharacter();
        /*
        System.out.println("String[" + text.getXDirAdj() + ","
                + text.getYDirAdj() + " fs=" + text.getFontSize() + " xscale="
                + text.getXScale() + " height=" + text.getHeightDir() + " space="
                + text.getWidthOfSpace() + " width="
                + text.getWidthDirAdj() + "]" + text.getCharacter());
        */
        String REGEX = "[,.\\[\\](:;!?)/]";
        char c = tChar.charAt(0);
        lineMatch = matchCharLine(text);
        if ((!tChar.matches(REGEX)) && (!Character.isWhitespace(c))) {
            if ((!is1stChar) && (lineMatch == true)) {
                appendChar(tChar);
            } else if (is1stChar == true) {
                setWordCoord(text, tChar);
            }
        } else {
            endWord();
        }
    }

    protected void appendChar(String tChar) {
        tWord.append(tChar);
        is1stChar = false;
    }

    protected void setWordCoord(TextPosition text, String tChar) {
        tWord.append("(")
                .append(pageNo)
                .append(")[")
                .append(roundVal(Float.valueOf(text.getXDirAdj())))
                .append(" : ")
                .append(roundVal(Float.valueOf(text.getYDirAdj())))
                .append(" : ")
                .append(roundVal(Float.valueOf(text.getXDirAdj()+text.getWidth())))
                .append(" : ")
                .append(roundVal(Float.valueOf(text.getY()-text.getHeight())))
                .append("] ")
                .append(tChar);
        is1stChar = false;
    }

    protected void endWord() {
        String newWord = tWord.toString().replaceAll("[^\\x00-\\x7F]", "");
        String sWord = newWord.substring(newWord.lastIndexOf(' ') + 1);
        if (!"".equals(sWord))
            wordList.add(newWord);
        /*{
            if (Arrays.asList(seekA).contains(sWord)) {
                wordList.add(newWord);
            } else if ("SHOWMETHEMONEY".equals(seek)) {
                wordList.add(newWord);
            }
        }*/
        tWord.delete(0, tWord.length());
        is1stChar = true;
    }

    protected boolean matchCharLine(TextPosition text) {
        Double yVal = roundVal(Float.valueOf(text.getYDirAdj()));
        if (yVal.doubleValue() == lastYVal) {
            return true;
        }
        lastYVal = yVal.doubleValue();
        endWord();
        return false;
    }

    protected boolean matchCharWord(TextPosition text) {
        Double yVal = roundVal(Float.valueOf(text.getYDirAdj()));
        if (yVal.doubleValue() == lastYVal) {
            return true;
        }
        lastYVal = yVal.doubleValue();
        endWord();
        return false;
    }

    protected Double roundVal(Float yVal) {
        DecimalFormat rounded = new DecimalFormat("0.0'0'");
        Double yValDub = new Double(rounded.format(yVal));
        return yValDub;
    }
}
