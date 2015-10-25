/**
 * pdfXtk - PDF Extraction Toolkit
 * Copyright (c) by the authors/contributors.  All rights reserved.
 * This project includes code from PDFBox and TouchGraph.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice,
 *    this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions and the following disclaimer in the documentation
 *    and/or other materials provided with the distribution.
 * 3. Neither the names pdfXtk or PDF Extraction Toolkit; nor the names of its
 *    contributors may be used to endorse or promote products derived from this
 *    software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE FOR ANY
 * DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
 * ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 * http://pdfxtk.sourceforge.net
 *
 */
package at.ac.tuwien.dbai.pdfwrap.operator;

import at.ac.tuwien.dbai.pdfwrap.pdfread.PDFObjectExtractor;
import org.apache.pdfbox.cos.COSNumber;
import org.apache.pdfbox.util.PDFOperator;
import org.apache.pdfbox.util.operator.OperatorProcessor;

import java.awt.geom.GeneralPath;
import java.awt.geom.Point2D;
import java.util.List;

/**
 * Implementation of content stream operator for PDFObjectExtractor.
 * 
 * Adapted from PDFBox code
 * @author Ben Litchfield, ben@benlitchfield.com
 * @author Tamir Hassan, pdfanalyser@tamirhassan.com
 * @version PDF Analyser 0.9
 */
public class CurveToReplicateInitialPoint extends OperatorProcessor
{


    /**
     * process : v : Append curved segment to path (initial point replicated).
     * @param operator The operator that is being executed.
     * @param arguments List
     */
    public void process(PDFOperator operator, List arguments) 
    {
        PDFObjectExtractor drawer = (PDFObjectExtractor)context;
        
        COSNumber x2 = (COSNumber)arguments.get( 0 );
        COSNumber y2 = (COSNumber)arguments.get( 1 );
        COSNumber x3 = (COSNumber)arguments.get( 2 );
        COSNumber y3 = (COSNumber)arguments.get( 3 );
        GeneralPath path = drawer.getLinePath();
        Point2D currentPoint = path.getCurrentPoint();
        /*
        float x2f = x2.floatValue();
        float y2f = (float)drawer.fixY( x2f, y2.floatValue() );
        float x3f = x3.floatValue();
        float y3f = (float)drawer.fixY( x3f, y3.floatValue() );
        
        float currentX = (float)currentPoint.getX();
        float currentY = (float)currentPoint.getY();
        drawer.getLinePath().curveTo(currentX,currentY,x2f,y2f,x3f,y3f);
        */
        
        Point2D P2 = drawer.TransformedPoint(x2.doubleValue(), y2.doubleValue());
        Point2D P3 = drawer.TransformedPoint(x3.doubleValue(), y3.doubleValue());
        
        drawer.getLinePath().curveTo((float)currentPoint.getX(), (float)currentPoint.getY(), (float)P2.getX(), (float)P2.getY(), (float)P3.getX(), (float)P3.getY());
        drawer.simpleCurveTo((float)currentPoint.getX(), (float)currentPoint.getY(), (float)P2.getX(), (float)P2.getY(), (float)P3.getX(), (float)P3.getY());
    }
}
