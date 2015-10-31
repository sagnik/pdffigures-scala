package edu.ist.psu.sagnik.research.extractor.pdfboxword;

/**
 * Created by sagnik on 10/30/15.
 */

import org.apache.pdfbox.exceptions.CryptographyException;
import org.apache.pdfbox.pdmodel.PDDocument;
import org.apache.pdfbox.pdmodel.PDPage;
import org.apache.pdfbox.pdmodel.common.PDStream;
import org.apache.pdfbox.util.PDFTextStripper;
import org.apache.pdfbox.util.TextPosition;

import java.io.File;
import java.io.IOException;
import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.List;


class Word{
    String text="";
    float x1,x2,y1,y2;

    public Word(String text, float x1,float x2, float y1, float y2) {
        this.text=text;
        this.x1=x1;
        this.y1=y1;
        this.x2=x2;
        this.y2=y2;
    }

    public void setText(String text) {
        this.text = text;
    }
    public String getText(){return this.text;}

    public void setX1(float x1) {
        this.x1 = x1;
    }
    public float getX1(){return this.x1;}

    public void setX2(float x2) {
        this.x2 = x2;
    }
    public float getX2(){return this.x2;}

    public void setY1(float y1) {
        this.y1 = y1;
    }
    public float getY1(){return this.y1;}

    public void setY2(float y2) {
        this.y2 = y2;
    }
    public float getY2(){return this.y2;}

}

public class PrintWordLocations extends PDFTextStripper {

    public static StringBuilder tWord = new StringBuilder();
    public static ArrayList<Word> words= new ArrayList<Word>();
    public static Word currentWord=new Word("",0,0,0,0);
    //public static String seek;
    //public static String[] seekA;
    public static List wordList = new ArrayList();
    public static boolean is1stChar = true;
    public static boolean wordMatch;
    public static int pageNo = 1;
    public static double lastYVal;
    public static double lastXend;

    public PrintWordLocations()
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
        wordMatch = matchCharWord(text);
        if ((!tChar.matches(REGEX)) && (!Character.isWhitespace(c))) {
            if ((!is1stChar) && (wordMatch == true)) {
                appendChar(tChar,text);
            } else if (is1stChar == true) {
                setWordCoord(text, tChar);
            }
        } else {
            endWord();
        }
    }

    protected void appendChar(String tChar, TextPosition t) {
        currentWord.setText(currentWord.getText()+tChar);
        currentWord.setX1(java.lang.Math.max(currentWord.getX1(),t.getX()));
        currentWord.setY1(java.lang.Math.max(currentWord.getX1(),t.getY()));

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
       tWord.delete(0, tWord.length());
        is1stChar = true;
    }


    protected boolean matchCharWord(TextPosition text) {
        Double yVal = roundVal(Float.valueOf(text.getYDirAdj()));
        Double thisXStart=roundVal(Float.valueOf(text.getXDirAdj()));
        if ((thisXStart-lastXend)<3f && yVal.doubleValue() == lastYVal) { //not a a new word
            lastXend=java.lang.Math.max(lastXend, thisXStart + text.getWidth());
            return true;
        }
        lastXend=java.lang.Math.max(lastXend,thisXStart+text.getWidth());
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
