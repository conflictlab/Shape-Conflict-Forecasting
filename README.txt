# README for Thomas Chadefaux: A shape-based approach to conflict forecasting.


## Overview

The code in this replication package constructs the analysis file from the data sources in Data/ using R. The R version used here and associated dependencies can be retrieved using Packrat, which should automatically load upon running the masterfile (masterFile_RUN_ME.R).


## Data availability and provenance

The data used comes from the ViEWS project (https://github.com/UppsalaConflictDataProgram/OpenViEWS2) and is included for convenience in this replication package (see folder Data/). The data will need to be unzipped prior to running the code. The original data is at https://views.pcr.uu.se/download/datasets/


## Instructions for Replicators

1. Download the parquet data files at the following address and save in folder Data/
https://www.dropbox.com/sh/vs3919nqba4kgxp/AACK5A5v6sNvzfJgAzOcof3Pa?dl=0
2. Using R, run masterFile_RUN_ME.R

The replicator should expect each "step-unit" (e.g. step 2, country-months) to take about 4 hours.


## License for Data

The code is licensed under a Creative Commons/CC-BY-NC/CC0 license. See LICENSE.txt for details.


