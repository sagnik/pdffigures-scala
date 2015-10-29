# PDF to Figures & Tables

##Goal
Given a scholarly PDF (optimized for computer science papers), extract the figures, tables and associated metadata (caption/mention). In future, we can also include algorithms.

##What's it? What's new?

We often include multiple tables/figures/algorithms in scholarly documents. In Latex, we typically use commands to draw lines for tables and include figures as png/eps/svg/PDF files. In MS Word, we draw shapes similarly. When we convert these Latex/ MS Word source files to PDFs, the graphics or text painting operations are converted into PDF operations. Given a PDF, it is hard to **automatically** identify regions that correspond to figures or tables. This can be done manually using InkScape but that isn't scalable.   

A recent project by AllenAI proposed a method to automatically extract these regions from PDFs (https://github.com/allenai/pdffigures). This command line utility (the code is in C++) works very well on scholarly documents especially Computer Science papers. Unfortunately it has some _system dependencies_ such as `poppler`. Also, after identifying the bounding boxes for the figures and tables, they convert the PDF pages into images and crop the regions. Therefore, the output is raster graphics, which is not as good as vector graphics.
  
This repo removes all such system dependencies to create a standalone **Scala** project where all dependencies are managed through sbt and Maven. Also, we output vector graphics (SVG files).

##Dependencies

Dependencies are included in the distribution itself because they were changed to support the needs.

1. **pdfXtk** : https://github.com/tamirhassan/pdfxtk

2. **pdf2svg** : https://bitbucket.org/petermr/pdf2svg/
     
##Test
1. **Compilation**: This code is provided with the required library in the lib folder. However, we need to compile the dependency pdfxtk because the pom in pdfXtk has an alternate source for javax.jai.\*. Go to pdfxtk folder, run `mvn clean install` and copy the jar file `pdfXtk-0.9-SNAPSHOT.jar` to the lib directory.  
2. **Test**: There are multiple test classes for different modules that I plan to explain later. The final test class is `edu.ist.psu.sagnik.research.extractor.FigureTableExtractionSVGTest`. Delete the folder `src/test/resources/pdfs/pdffigures` and run `testOnly edu.ist.psu.sagnik.research.extractor.FigureTableExtractionSVGTest` on your _sbt console_. This will extract the figures (SVG files) and figure metadata from the file `src/test/resorces/pdfs/pdffigures.pdf` into the folder `src/test/resorces/pdfs/pdffigures`. An html file (`src/test/resorces/pdfs/pdffigures/pdffigures-FigureTable.html`) should be created to display the extracted figures and tables. An SVG file can include a raster graphics as an external resource. These graphics will not be displayed on browsers (see https://bugzilla.mozilla.org/show_bug.cgi?id=628747 and http://stackoverflow.com/questions/28269347/ie-does-but-chrome-firefox-do-not-render-svg-embedded-image-in-img-element). But, if you right click on the empty blue bounding boxes, and `open in new tab` (Chrome) or `view image` (Firefox) you should be able to see the images.        

##Approach

**More details later sometime**

We closely follow the approach in the paper pdffigures (project: http://pdffigures.allenai.org/ , paper: https://sites.google.com/a/allenai.org/figure-extractor/paper.pdf?attredirects=0&d=1).

##TODO

1. ~~The immediate todo is to output the metadata (currently as case classes) as JSON files. This should be done soon.~~
2. Lot's of TODOs are mentioned in the code.
3. Any suggestions/ feature/ pull requests will be deeply appreciated. 
   
 



