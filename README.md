# Overview

This repository contains all data and code to reproduce analyses in the paper:

>Mathur MB, et al. Multi-site replication of tempting-fate effects in Risen & Gilovich (2008). In preparation.

# How to reproduce analyses

The simplest way to reproduce analyses reported in the paper (<i>analysis.pdf</i>) is simply to run the Markdown file <i>analysis.Rmd</i>,
which embeds all analyses in the manuscript.

# Data files

All data are in the "data" folder:

- "raw" contains raw, unmodified site-level datasets exactly as provided by site investigators to MBM.
- "prepped" contains site-level datasets that have undergone cleaning and variable name standardization. These were used
for site-level audits but are not directly used in analysis. 
- <i>full_prepped_dataset.csv</i>, created by <i>data_prep.Rmd</i>, contains all subject-level data from all sites and is the only ML5 data file
used directly for analyses. Also see its data dictionary, <i>analysis_data_dictionary.csv</i>. 
- <i>data_prepped_rpp.csv</i> and <i>data_prepped_rpp.txt</i> are from Mathur & Frank (2012)'s previous replication and are used for reporting
their effect sizes in the manuscript. 

# Data preparation code files

- <i>data_prep.Rmd</i>: Takes the site-level raw datasets (see above) and uses them to produce the site-level prepped datasets as well as <i>full_prepped_dataset.csv</i>. 
- <i>data_prep.pdf</i>: A report produced by <i>data_prep.Rmd</i> that was used for site-level audits. 


# Analysis code files
The key files are:

- <i>analysis.Rmd</i>: The manuscript written in Markdown, which integrates statistics into the manuscript by loading saved R objects from <i>analysis_objects.rds</i>. 

- <i>analysis.R</i>: The workhorse analysis script that conducts all analyses and produces <i>analysis_objects.rds</i> as well as figures
(<i>forest_main.pdf</i> and <i>forest_int.pdf</i>) for use in the manuscript.

- <i>analysis_objects.rds</i>: Saved R objects (e.g., effect sizes) produced by <i>analysis.R</i>. 

- <i>helper.R</i>: Helper functions and is called by <i>analysis.R</i>.

# Other files

- <i>refs_ml5.bib</i>: BibTeX references cited in manuscript.  
- <i>summary_table</i>: Table 1 in various formats. Originally created in Excel.  
- "protocols": Contains power simulation used to determine sample sizes and protocols before and after editorial review. The file <i>Risen & Gilovich Across-Site Analyses</i> is the final, preregistered protocol. 
- "materials": Contains questionnaire and MTurk information and materials. The file <i>2016-11-7 Qualtrics questionnaire.qsf</i> can be directly loaded into Qualtrics to rerun the experiment. 


