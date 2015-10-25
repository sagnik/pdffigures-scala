# PDF to Figure & Tables

##Goal
Given a scholarly PDF (optimized for computer science papers), extract the figures, tables and associated metadata (caption/mention). In future, we can also include algorithms.

##Test
1. To compile the
To test, run testOnly

##Approach
We closely follow the approach in the paper  pdffigures (project: http://pdffigures.allenai.org/ , paper: https://sites.google.com/a/allenai.org/figure-extractor/paper.pdf?attredirects=0&d=1).
Basic steps are:
1. Get a higher abstraction of the PDF. This step converts the PDF into following objects: graphics paths, images, text (character, word, line, paragraphs).
A modified version of the library pdfXtk (https://github.com/tamirhassan/pdfxtk) is used.
2.




