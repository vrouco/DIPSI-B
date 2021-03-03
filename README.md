# DIPSI-B
Development of the brief version of the Dimensional Personality Symptom Item Pool




---CONTENTS----
#PDF files#

1- Manuscript pre-print.pdf
Submitted to Psychological Assessment.

2- DIPSI-B_TheInstrument.pdf
Items that compose the DIPSI-B

#R scripts#

1- data input.R
data pre-processing script

2- missing data.R
exploratory analysis of missing data

3- cross-validation.R
split-sample algorithm to separate train and test datasets

4- development phase.R
application of the GRM and DIF analysis to each of the facets.
All lines except the load data line are commented out. This loop is computationally intense.

5- cfa development.R
Script used to derive good fitting models from tau-equivalent models

6- tif.R
Creates Test Information Function plots of both facets and domains. They are stored in the folder "facet tif"

7- validation table.R
Computes internal consistency estimates and goodness-of-fit of the models with the validation sample. Creates Table 2 in the manuscript.

8- part to whole cors.R
Computes congruency between the original and the brief DIPSI

9- theta facet scores.R
Extracts latent factor scores and sumscores. Then compute the correlation between them.

10 & 11- gender and age measurement invariance.R
Performs measurement invariance with respect to age and gender. The results are included in the measurement invariance table.

#FOLDERS#

1- supplemental materials
Contains additional figures and tables not included in the manuscript due to space limits.

2- tools
Contains some wrapper functions useful for conducting the main analysis.

3- data
The most important file is raw data_anonymized.csv. Also published in this repository 
Also stored some processed data files to improve efficiency of analysis.
And two files with keys relating item content, facet names and domains.

#MPLUS files#

1- small dipsi esem.out
Output file extracted from mplus with the esem model of the DIPSI-B



