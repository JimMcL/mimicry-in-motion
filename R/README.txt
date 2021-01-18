Steps in the analysis
---------------------

The main script is analyse.R. It can be run directly to produce graphs and report analysis results.
However some intermediate steps are very slow, so intermediate results are cached and reused rather than recalculated. See the comments in analyse.R for more info.

Note that a number of the analyses use resampling techniques, which means there is an element of randomness so results might vary by a couple of percent between runs.

______________________________________
Trajectory analysis

1. Videos were originally downloaded from my sampleIt database. Videos
  were downloaded to ../data/videos/lab, ../data/videos/wild, or
  ../data/videos/scale as appropriate by running the R script
  "download videos.R"

2. Trajectories were extracted from videos using
  https://github.com/JimMcL/YetAnotherTracker. track-extract.bat in each
  video directory records the parameters used to extract the
  trajectories. Output is in <file>.csv. When a mask was used (mainly
  for wild videos), it is stored in <file>.json.

3. ../data/videos/video-info.csv is a list of trajectory files, together with
  what tracks to use and how they should be smoothed. It was created by
  running parts of check-tracks.R on the CSV files and looking at the
  results for each file.

4.analyse.R uses ../data/videos/video-info.csv and the CSV files to
  create trajr trajectories and analyse them. Since trajectory
  characterisation is very time consuming, results are stored in
  ../data/TrajectoryData.rds rather than recalculated every time.


______________________________________
Morphometrics

1. Body shapes outlines were obtained from my sampleIt database as
  well as from outlines prepared for another paper (Measuring mimicry:
  Methods for quantifying visual similarity).  Outline images are in
  ../data/outlines/sampleIt (my sampleIt data) and
  ../data/outlines/Morphometrics B-W (Measuring mimicry).  The CSV
  files ../data/outlines/sampleIt/outline-info.csv and
  ../data/outlines/Morphometrics B-W/List - current.csv describe
  outlines in their respective directories.
  
  The Morphometrics B-W directory and CSV file were created manually.
  The sampleIt contents were created by running download outlines.R.

2. The standalone R script cache_morphometrics.R reads the outline
  CSVs and outline images to perform morphometric analysis and
  estimate accuracy of mimicry for individuals, species and types,
  divided into lateral and dorsal aspects. The output is written to
  CSV files in ../data/outlines, named accuracy-<type>-<aspect>.csv.



____________________________________________
R version info

- Session info -------------------------------------------------------------------
 setting  value                       
 version  R version 4.0.2 (2020-06-22)
 os       Windows 10 x64              
 system   x86_64, mingw32             
 ui       RStudio                     
 language (EN)                        
 collate  English_Australia.1252      
 ctype    English_Australia.1252      
 tz       Australia/Sydney            
 date     2021-01-08                  

- Packages -----------------------------------------------------------------------
 package     * version date       lib source        
 askpass       1.1     2019-01-13 [1] CRAN (R 4.0.2)
 assertthat    0.2.1   2019-03-21 [1] CRAN (R 4.0.2)
 backports     1.1.7   2020-05-13 [1] CRAN (R 4.0.0)
 boot          1.3-25  2020-04-26 [2] CRAN (R 4.0.2)
 callr         3.4.3   2020-03-28 [1] CRAN (R 4.0.2)
 cli           2.0.2   2020-02-28 [1] CRAN (R 4.0.2)
 codetools     0.2-16  2018-12-24 [2] CRAN (R 4.0.2)
 colorspace    1.4-1   2019-03-18 [1] CRAN (R 4.0.2)
 crayon        1.3.4   2017-09-16 [1] CRAN (R 4.0.2)
 dabestr       0.3.0   2020-07-13 [1] CRAN (R 4.0.2)
 desc          1.2.0   2018-05-01 [1] CRAN (R 4.0.2)
 devtools      2.3.0   2020-04-10 [1] CRAN (R 4.0.2)
 digest      * 0.6.25  2020-02-23 [1] CRAN (R 4.0.2)
 dplyr         1.0.0   2020-05-29 [1] CRAN (R 4.0.2)
 ellipsis      0.3.1   2020-05-15 [1] CRAN (R 4.0.2)
 fansi         0.4.1   2020-01-08 [1] CRAN (R 4.0.2)
 fs            1.4.2   2020-06-30 [1] CRAN (R 4.0.2)
 generics      0.0.2   2018-11-29 [1] CRAN (R 4.0.2)
 ggplot2       3.3.2   2020-06-19 [1] CRAN (R 4.0.2)
 glue          1.4.1   2020-05-13 [1] CRAN (R 4.0.2)
 gtable        0.3.0   2019-03-25 [1] CRAN (R 4.0.2)
 JUtils      * 0.1.0   2021-01-02 [1] local         
 lifecycle     0.2.0   2020-03-06 [1] CRAN (R 4.0.2)
 magrittr      1.5     2014-11-22 [1] CRAN (R 4.0.2)
 memoise       1.1.0   2017-04-21 [1] CRAN (R 4.0.2)
 munsell       0.5.0   2018-06-12 [1] CRAN (R 4.0.2)
 openssl     * 1.4.2   2020-06-27 [1] CRAN (R 4.0.2)
 packrat       0.5.0   2018-11-14 [1] CRAN (R 4.0.2)
 pillar        1.4.4   2020-05-05 [1] CRAN (R 4.0.2)
 pkgbuild      1.0.8   2020-05-07 [1] CRAN (R 4.0.2)
 pkgconfig     2.0.3   2019-09-22 [1] CRAN (R 4.0.2)
 pkgload       1.1.0   2020-05-29 [1] CRAN (R 4.0.2)
 prettyunits   1.1.1   2020-01-24 [1] CRAN (R 4.0.2)
 processx      3.4.3   2020-07-05 [1] CRAN (R 4.0.2)
 ps            1.3.3   2020-05-08 [1] CRAN (R 4.0.2)
 purrr         0.3.4   2020-04-17 [1] CRAN (R 4.0.2)
 R6            2.4.1   2019-11-12 [1] CRAN (R 4.0.2)
 remotes       2.2.0   2020-07-21 [1] CRAN (R 4.0.3)
 rlang         0.4.7   2020-07-09 [1] CRAN (R 4.0.2)
 rprojroot     1.3-2   2018-01-03 [1] CRAN (R 4.0.2)
 rstudioapi    0.11    2020-02-07 [1] CRAN (R 4.0.2)
 scales        1.1.1   2020-05-11 [1] CRAN (R 4.0.2)
 sessioninfo   1.1.1   2018-11-05 [1] CRAN (R 4.0.2)
 stringi       1.4.6   2020-02-17 [1] CRAN (R 4.0.0)
 stringr       1.4.0   2019-02-10 [1] CRAN (R 4.0.2)
 testthat      2.3.2   2020-03-02 [1] CRAN (R 4.0.2)
 tibble        3.0.1   2020-04-20 [1] CRAN (R 4.0.2)
 tidyselect    1.1.0   2020-05-11 [1] CRAN (R 4.0.2)
 tinytex       0.24    2020-06-20 [1] CRAN (R 4.0.2)
 trajr       * 1.4.0   2020-12-16 [1] local         
 usethis       1.6.1   2020-04-29 [1] CRAN (R 4.0.2)
 vctrs         0.3.1   2020-06-05 [1] CRAN (R 4.0.2)
 withr         2.2.0   2020-04-20 [1] CRAN (R 4.0.2)
 xfun          0.15    2020-06-21 [1] CRAN (R 4.0.2)
