# Overview

This repository contains all data and code to reproduce analyses in the paper:

>Mathur MB, et al. Multi-site replication of tempting-fate effects in Risen & Gilovich (2008). In preparation.

# How to reproduce analyses

The simplest way to reproduce analyses reported in the paper (<i>main_text.pdf</i>) is simply to run the Markdown file <i>main_text.Rmd</i>,
which embeds all analyses in the manuscript.

# Data files

All data are in the "data" folder:

- "raw" contains raw, unmodified site-level datasets exactly as provided by site investigators to MBM.
- "prepped" contains site-level datasets that have undergone cleaning and variable name standardization. These were used
- "other_replications" contains data from other replications (RPP and ML2) to which we compared our results in the manuscript
for site-level audits but are not directly used in analysis. 
- <i>full_prepped_dataset.csv</i>, created by <i>data_prep.Rmd</i>, contains all subject-level data from all sites and is the only ML5 data file
used directly for analyses. Also see its data dictionary, <i>analysis_data_dictionary.csv</i>. 
- <i>data_prepped_rpp.csv</i> and <i>data_prepped_rpp.txt</i> are from Mathur & Frank (2012)'s previous replication and are used for reporting
their effect sizes in the manuscript. 
- <i>analysis_data_dictionary.csv</i> is a data dictionary for <i>full_prepped_dataset.csv</i> (along with additional variables, like site-level stats, added by <i>analysis.R</i>)

# Data preparation code files

- <i>main_code/data_prep.Rmd</i>: Takes the site-level raw datasets (see above) and uses them to produce the site-level prepped datasets as well as <i>full_prepped_dataset.csv</i>. 
- <i>main_code/data_prep.pdf</i>: A report produced by <i>data_prep.Rmd</i> that was used for site-level audits. 


# Analysis code files
The key files are:

- <i>manuscript/main_text.Rmd</i>: The manuscript written in Markdown, which integrates statistics into the manuscript by loading saved R objects from <i>analysis_objects.rds</i>. 

- <i>main_code/analysis.R</i>: The workhorse analysis script that conducts all analyses and produces <i>analysis_objects.rds</i> as well as figures
(<i>forest_main.pdf</i> and <i>forest_int.pdf</i>) for use in the manuscript.

- <i>analysis_objects.rds</i>: Saved R objects (e.g., effect sizes) produced by <i>analysis.R</i>. 

- <i>helper.R</i>: Helper functions and is called by <i>analysis.R</i>.

- "supplement" contains a Markdown file containing code and prose for the online supplement

# Other files

- <i>refs_ml5.bib</i>: BibTeX references cited in manuscript.  
- <i>summary_table</i>: Table 1 in various formats. Originally created in Excel.  
- "protocols": Contains power simulation used to determine sample sizes and protocols before and after editorial review. The file <i>Risen & Gilovich Across-Site Analyses</i> is the final, preregistered protocol. 
- "materials": Contains questionnaire and MTurk information and materials. The file <i>2016-11-7 Qualtrics questionnaire.qsf</i> can be directly loaded into Qualtrics to rerun the experiment. 
- "to_submit": Various versions of the manuscript submitted to AMPPS


