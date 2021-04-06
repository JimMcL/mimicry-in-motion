Steps in the analysis
---------------------

The main script is analyse.R. It can be run directly to produce graphs
and report analysis results.  However some intermediate steps are very
slow, so intermediate results are cached and reused rather than
recalculated. See the comments in analyse.R for more info.

Note that a number of the analyses use resampling techniques, which
means there is an element of randomness so results vary by a few
percent between runs.

______________________________________
Trajectory analysis

1. Videos were originally downloaded from my sampleIt database. Videos
  were downloaded to ../data/videos/lab or ../data/videos/scale as
  appropriate by running the R script "download videos.R"

2. Trajectories were extracted from videos using
  https://github.com/JimMcL/YetAnotherTracker. track-extract.bat in
  the video directory records the parameters used to extract the
  trajectories. Output is in <file>.csv. When a mask was used, it is
  stored in <file>.json.

3. ../data/videos/video-info.csv is a list of trajectory files,
  together with what tracks to use and how they should be smoothed. It
  was created by running parts of check-tracks.R on the CSV files and
  looking at the results for each file. The script find-duplicates.R
  was to to find investigate multiple trajectories for a speciemn, and
  decide which one to use.

4. analyse.R uses ../data/videos/video-info.csv and the CSV files to
  create trajr trajectories and analyse them. Since trajectory
  characterisation is time consuming, results are stored in
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
Analyses in appendix

Appendix 1 describes two bootstrap analyses: 1) to test whether ant trajectories are different from those of non mimics, and 2) whether mimic trajectories are part way between ants and non-mimics. Code for these analyses is in test-conditions-of-mimicry.R.

____________________________________________
R version info

> devtools::session_info()
- Session info -----------------------------------------------------------------------------------------------------
 setting  value                       
 version  R version 4.0.2 (2020-06-22)
 os       Windows 10 x64              
 system   x86_64, mingw32             
 ui       RStudio                     
 language (EN)                        
 collate  English_Australia.1252      
 ctype    English_Australia.1252      
 tz       Australia/Sydney            
 date     2021-04-06                  

- Packages ---------------------------------------------------------------------------------------------------------
 package     * version    date       lib source                        
 abind         1.4-5      2016-07-21 [1] CRAN (R 4.0.0)                
 askpass       1.1        2019-01-13 [1] CRAN (R 4.0.2)                
 assertthat    0.2.1      2019-03-21 [1] CRAN (R 4.0.2)                
 backports     1.1.7      2020-05-13 [1] CRAN (R 4.0.0)                
 bitops        1.0-6      2013-08-17 [1] CRAN (R 4.0.0)                
 boot          1.3-25     2020-04-26 [2] CRAN (R 4.0.2)                
 callr         3.4.3      2020-03-28 [1] CRAN (R 4.0.2)                
 car           3.0-8      2020-05-21 [1] CRAN (R 4.0.2)                
 carData       3.0-4      2020-05-22 [1] CRAN (R 4.0.0)                
 cellranger    1.1.0      2016-07-27 [1] CRAN (R 4.0.2)                
 cli           2.0.2      2020-02-28 [1] CRAN (R 4.0.2)                
 codetools     0.2-16     2018-12-24 [2] CRAN (R 4.0.2)                
 colorspace    1.4-1      2019-03-18 [1] CRAN (R 4.0.2)                
 crayon        1.3.4      2017-09-16 [1] CRAN (R 4.0.2)                
 curl          4.3        2019-12-02 [1] CRAN (R 4.0.2)                
 dabestr       0.3.0      2020-07-13 [1] CRAN (R 4.0.2)                
 data.table    1.12.8     2019-12-09 [1] CRAN (R 4.0.0)                
 desc          1.2.0      2018-05-01 [1] CRAN (R 4.0.2)                
 devtools      2.3.0      2020-04-10 [1] CRAN (R 4.0.2)                
 digest        0.6.27     2020-10-24 [1] CRAN (R 4.0.4)                
 doParallel  * 1.0.15     2019-08-02 [1] CRAN (R 4.0.2)                
 dplyr         1.0.0      2020-05-29 [1] CRAN (R 4.0.2)                
 ellipsis      0.3.1      2020-05-15 [1] CRAN (R 4.0.2)                
 evaluate      0.14       2019-05-28 [1] CRAN (R 4.0.2)                
 fansi         0.4.1      2020-01-08 [1] CRAN (R 4.0.2)                
 farver        2.0.3      2020-01-16 [1] CRAN (R 4.0.2)                
 forcats       0.5.0      2020-03-01 [1] CRAN (R 4.0.2)                
 foreach     * 1.5.0      2020-03-30 [1] CRAN (R 4.0.2)                
 foreign       0.8-80     2020-05-24 [2] CRAN (R 4.0.2)                
 fs            1.4.2      2020-06-30 [1] CRAN (R 4.0.2)                
 generics      0.0.2      2018-11-29 [1] CRAN (R 4.0.2)                
 ggmap       * 3.0.0      2019-02-04 [1] CRAN (R 4.0.2)                
 ggplot2     * 3.3.2      2020-06-19 [1] CRAN (R 4.0.2)                
 glue          1.4.1      2020-05-13 [1] CRAN (R 4.0.2)                
 gtable        0.3.0      2019-03-25 [1] CRAN (R 4.0.2)                
 haven         2.3.1      2020-06-01 [1] CRAN (R 4.0.2)                
 hms           0.5.3      2020-01-08 [1] CRAN (R 4.0.2)                
 htmltools     0.5.1.1    2021-01-22 [1] CRAN (R 4.0.4)                
 httr          1.4.2      2020-07-20 [1] CRAN (R 4.0.2)                
 iterators   * 1.0.12     2019-07-26 [1] CRAN (R 4.0.2)                
 jpeg        * 0.1-8.1    2019-10-24 [1] CRAN (R 4.0.0)                
 JUtils      * 0.1.0      2021-04-02 [1] Github (JimMcL/JUtils@9bfa02d)
 knitr         1.29       2020-06-23 [1] CRAN (R 4.0.2)                
 labeling      0.3        2014-08-23 [1] CRAN (R 4.0.0)                
 lattice       0.20-41    2020-04-02 [2] CRAN (R 4.0.2)                
 lifecycle     0.2.0      2020-03-06 [1] CRAN (R 4.0.2)                
 magrittr      2.0.1      2020-11-17 [1] CRAN (R 4.0.4)                
 mapdata     * 2.3.0      2018-03-30 [1] CRAN (R 4.0.2)                
 maps        * 3.3.0      2018-04-03 [1] CRAN (R 4.0.2)                
 MASS        * 7.3-51.6   2020-04-26 [2] CRAN (R 4.0.2)                
 memoise     * 1.1.0      2017-04-21 [1] CRAN (R 4.0.2)                
 Momocs      * 1.3.3      2021-03-26 [1] Github (MomX/Momocs@e7ba9b8)  
 munsell       0.5.0      2018-06-12 [1] CRAN (R 4.0.2)                
 openssl     * 1.4.3      2020-09-18 [1] CRAN (R 4.0.4)                
 openxlsx      4.1.5      2020-05-06 [1] CRAN (R 4.0.2)                
 packrat       0.5.0      2018-11-14 [1] CRAN (R 4.0.2)                
 pillar        1.4.4      2020-05-05 [1] CRAN (R 4.0.2)                
 pkgbuild      1.0.8      2020-05-07 [1] CRAN (R 4.0.2)                
 pkgconfig     2.0.3      2019-09-22 [1] CRAN (R 4.0.2)                
 pkgload       1.1.0      2020-05-29 [1] CRAN (R 4.0.2)                
 plyr          1.8.6      2020-03-03 [1] CRAN (R 4.0.2)                
 png           0.1-7      2013-12-03 [1] CRAN (R 4.0.0)                
 prettyunits   1.1.1      2020-01-24 [1] CRAN (R 4.0.2)                
 processx      3.4.3      2020-07-05 [1] CRAN (R 4.0.2)                
 ps            1.3.3      2020-05-08 [1] CRAN (R 4.0.2)                
 purrr         0.3.4      2020-04-17 [1] CRAN (R 4.0.2)                
 R6            2.5.0      2020-10-28 [1] CRAN (R 4.0.4)                
 Rcpp          1.0.6      2021-01-15 [1] CRAN (R 4.0.4)                
 readxl        1.3.1      2019-03-13 [1] CRAN (R 4.0.2)                
 remotes       2.2.0      2020-07-21 [1] CRAN (R 4.0.3)                
 RgoogleMaps   1.4.5.3    2020-02-12 [1] CRAN (R 4.0.2)                
 rio           0.5.16     2018-11-26 [1] CRAN (R 4.0.2)                
 rjson         0.2.20     2018-06-08 [1] CRAN (R 4.0.0)                
 rlang         0.4.7      2020-07-09 [1] CRAN (R 4.0.2)                
 rmarkdown     2.3        2020-06-18 [1] CRAN (R 4.0.2)                
 rprojroot     1.3-2      2018-01-03 [1] CRAN (R 4.0.2)                
 rsconnect     0.8.16     2019-12-13 [1] CRAN (R 4.0.2)                
 rstudioapi    0.11       2020-02-07 [1] CRAN (R 4.0.2)                
 scales        1.1.1      2020-05-11 [1] CRAN (R 4.0.2)                
 sessioninfo   1.1.1      2018-11-05 [1] CRAN (R 4.0.2)                
 signal        0.7-6      2015-07-30 [1] CRAN (R 4.0.2)                
 sp          * 1.4-5      2021-01-10 [1] CRAN (R 4.0.4)                
 stringi       1.4.6      2020-02-17 [1] CRAN (R 4.0.0)                
 stringr       1.4.0      2019-02-10 [1] CRAN (R 4.0.2)                
 testthat      2.3.2      2020-03-02 [1] CRAN (R 4.0.2)                
 tibble        3.0.1      2020-04-20 [1] CRAN (R 4.0.2)                
 tidyr         1.1.0      2020-05-20 [1] CRAN (R 4.0.2)                
 tidyselect    1.1.0      2020-05-11 [1] CRAN (R 4.0.2)                
 tinytex       0.24       2020-06-20 [1] CRAN (R 4.0.2)                
 trajr       * 1.4.0.9000 2021-01-16 [1] local                         
 usethis       1.6.1      2020-04-29 [1] CRAN (R 4.0.2)                
 vctrs         0.3.1      2020-06-05 [1] CRAN (R 4.0.2)                
 WaveletComp * 1.1        2018-03-18 [1] CRAN (R 4.0.0)                
 withr         2.2.0      2020-04-20 [1] CRAN (R 4.0.2)                
 xfun          0.15       2020-06-21 [1] CRAN (R 4.0.2)                
 zip           2.0.4      2019-09-01 [1] CRAN (R 4.0.2)                
