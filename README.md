# Pathways Segmentation repo

The pathways-segmentation repo contains the workflow for conducting a Pathways Segmentation analysis in R. The Pathways Segmentation methodology is a population grouping analysis framework used for understanding social, cultural, economic, and environmental vulnerability to improve women's health and well-being.

This analysis is intended for researche scientists familiar with R and requires custom variable coding for each analysis.

The workflow is designed to use STATA (.DTA) survey datasets from the Demographic and Health Surveys (DHS) program, however any survey dataset will work with adjustments to the 2_data_cleaning.R script and associated functions.

# Table of contents

- [Abstract](#abstract)
    - [Pathways Segmentation methodology](#pathways-segmentation-methodology)
- [Prerequisites](#rerequisites)
    - [Technical requirements](#technical-requirements)
    - [Get code](#get-code)
    - [Environment setup](#environment-setup)
- [The Pathways Workbook](#the-pathways-workbook)
- [Repo structure](#repo-structure)
    - [New project template](#new-project-template)

- [Quickstart](#quickstart)
- [Pathways Segmentation workflow phases](#pathways-segmentation-workflow-phases)
    - [Analysis setup](#analysis-setup)
    - [Data cleaning and variable generation](#data-cleaning-and-variable-generation)
    - [Univariate analysis](#univariate-analysis)
    - [Exploratory data analysis](#exploratory-data-analysis)
    - [Principal component analysis (PCA)](#principal-component-analysis-pca)
    - [Latent class analysis (LCA)](#latent-class-analysis-lca)
    - [Country segment ranking](#country-segment-ranking)
    - [Quantitative segment profiling](#quantitative-segment-profiling)
    - [Segment typing tool](#segment-typing-tool)
- [Pathways Segmentation analysis quickstart tutorial](#pathways-segmentation-analysis-quickstart-tutorial)

## Abstract

### Pathways Segmentation methodology

#### Terminology

* **Vulnerability:** The quality or state of potentially being harmed, either physically, socially, cognitively, or emotionally, related to reproductive, newborn, maternal and child health and nutrition

* **Segment:** A mutually exclusive grouping of individuals based on shared characteristics.

* **Segmentation:** The process of dividing a population into smaller sub-groups (known as segments) based on shared characteristics.

* **Segmentation strata:** Driven by the survey design; how the survey data should be initially divided for the segmentation analysis i.e., how many independent segmentation solutions will be produced?  Often times this is represented by separate urban/rural solutions.

* **Health Outcome:** A health-related event (e.g., number of ANC visits, U5 mortality).

* **Vulnerability factor:** A specific fact, situation, or construct that helps define how women experience vulnerability.

* **Vulnerability variable (measure):** A standardized way of quantifying factors so they can be used in statistical analysis.

* **Pathways Domain:** A characteristic grouping of Vulnerability factors.  A Vulnerability Factor can be associated with multiple Domains. There are 6 Domains total:
    * Woman and her past experiences
    * Health mental models
    * Natural and human systems
    * Household relationships
    * Household economics and living conditions
    * Social Support

* **Segmentation solution:** The final set of segments resulting from completing the Pathways Segmentation methodology.

* **Cluster analysis:** A statistical method used to organize observations into meantinful groups - or clusters - that share common characteristics.  The standardized Pathways methodology uses latent class analysis (LCA).

* **Quantitative segmentation profile:** Health Outcomes and Vulnerability variables viewed from the perspective of the segmentation solution highlighting differences across the segments.

## Prerequisites

### Technical requirements

#### Statistical experience

An advanced understanding of applied statistics is expected in order to conduct a Pathways Segmentation analysis.  This includes the following statistical topics:

* variable distribution parameters (mean, median, standard deviation)
* bivariate regression interpretation (linear, logistic, predicted probabilities/values, odds ratios, statistical significance)
* principal component analysis (PCA) and interpretation of PCA visuals
* latent class analysis (LCA) algorithm and model fit statistics
* classification and regression tree fitting and evaluation

#### R experience

An intermediate level of coding in R and using RStudio Desktop is expected in order to successfully work with this codebase.  While most phases of the analysis require minimal coding and are more centered on variable selection via the Pathways Workbook, the initial variable recoding requires researchers to define these variables directly in the fun_gen_vulnerabilities.R and fun_gen_outcomes_dhs.R scripts.  A good source for R relevant training is [this]() course in Codecademy.

#### Software requirements

* [R (recommended >= 4.4.2, renv implementation assumes 4.4.2) & RStudio Desktop](https://posit.co/download/rstudio-desktop/)
* Microsoft Excel (recommended)
* [GitHub Desktop](https://desktop.github.com/download/) (suggested for managing code from GitHub repository)

Software will run on Windows or MAC operating system (although on a MAC OS the RENV framework may require some technical hurdles.)

#### Hardware requirements

There are no specific hardware requirements (although compute needs for running the LCA algorithm increase in the number of repititions specified).

### Environment setup

There are two options for setting up the initial R environment:
1. Use the default configuration and the [renv library](https://rstudio.github.io/renv/articles/renv.html).  This library is installed and initialized at the beginning in the 1_libraries.R script which is called as part of the 1_setup.R script.  Once initialized, the renv framework will install all libraries and their appropriate versions used in the analysis as specified in the lock.file.
2. Edit the 1_libraries.R to only call the renv::deactivate() line of code which will ignore the renv files and intall all libraries and their latest versions in the global R library location.

This project also uses the R [config](https://rstudio.github.io/config/articles/introduction.html) library and methods for defining file paths and variables used throughout the project. Using a config file creates a layer of abstraction for managing the codebase across multiple projects.  This can be edited directly in RStudio and is included in the .gitignore.

## The Pathways Workbook

The Pathways Segmentation workflow is anchored by the Pathways Workbook (either in Excel or CSV format) that drives and captures decisions made throughout each phase of the analysis.  The workflow is designed so that, outside of generating the initial datasets for Health Outcomes and Vulnerability variables in the data cleaning and variable generation phase, very little code needs to be edited directly.

### Generating the Pathways Workbook

The Pathways Workbook (in either format) is created in the 2_data_cleaning.R script. The config.yml parameter "create_new_pathways_workbook" (TRUE/FALSE) will generate the workbook from the Outcome and Vulnerability variables defined.  The parameter "pathways_workbook_is_excel" (TRUE/FALSE) determines whether the user intends to use the XLSX (TRUE) or CSV (FALSE) version of the Pathways Workbook.

Once generated, the Pathways Workbook should be reviewed to ensure all metadata for variables is defined and the vulnerability - domain mapping is accurate for the analysis.

### Workbook tabs (and columns)

* **params:** This tab serves as a catch-all for some project-specific variables used in the analysis.
    * **domains:** 6 Pathways domains
    * **strata:** Segmentation strata of the survey
    * **final_model:** the final Segmentation solution
* **outcomes:** Health Outcomes used in the analysis; inclusion/exclusion decisions for each phase are driven from this tab.
    * **category:** the grouping of the Health Outcome variable
    * **outcome_variable:** the name of the outcome variable as defined in the coding scripts and datasets
    * **short_name:** the friendly label of the outcome variable that is used in plotting
    * **description:** variable definition
    * **univariate_include:** include in the univariate analysis PDF plots
    * **eda_include:** include in the exploratory analysis PDF plots
    * **ranking_include:** include in the in-country ranking analysis
    * **profile_include:** include in the quantitative profile PDF plots
    * **notes:** space to capture information about decisions made, observations, etc
* **vulnerabilities:** Vulnerability variables used in the analysis; inclusion/exclusion decisions for each phase are drien from this tab.
    * **vulnerability_variable:** the name of the vulnerability_factor as defined in the coding scripts and datasets
    * **short_name:** the friendly label of the vulnerability variable that is used in plotting
    * **description:** variable definition
    * **univariate_include:** include in the univariate analysis phase
    * **eda_include:** include in the exploratory analysis phase
    * **pca_strata:** strata to include this variable in for PCA  (both/all/urban/rural)
    * **pca_include:** include in the next run of PCA phase
    * **lca_strata:** strata to include this variable in for LCA (both/all/urban/rural)
    * **lca_include:** include in the next run of LCA phase
    * **profile_strata:** which strata to include this variable in for the quantitative segment profiling (both/all/urban/rural)
    * **profile_include:** include in the quantitative segment profiling
    * **typing_tool_strata:** which strata to include this variable in for developing the segmentation typing tool (both/all/urban/rural)
    * **typing_tool_include:** include in the typing tool phase
    * **notes:** space to capture information about decisions made, observations, etc
    * **domain specific columns O-T:** include this variable in this domain (relevant for PCA phase and quantitative profiling phase)

## Repo structure

At the top level of the repository exists the project README and the global .gitignore file.

Within the "analyses" folder exists the following:

* a_new_analysis_template: templated set of files needed for a new analysis
* data_cleaning_scripts: includes coding from previous analyses for reference
* projects: contains a tutorial project and is a recommended location for future projects

### New project template

The workflow is modularized according to the different phases of a segmentation analysis.  Each module has a parent script which calls a function to generate the corresponding outputs (e.g., data.frame, PDF visualization file).

* config - template.yml: define file paths and some commonly used variables throughout the analysis
* 1_setup.R: create the folder structure for the project and import basic project files
* 1_libraries.R: install/load all libraries required for the project
* 1_import_data.R: load the survey data and save as .rds files for faster read
* 2_data_cleaning.R: generate the health Outcomes and Vulnerability variables used through the analysis
* 3_univariate_analysis.R: generate a PDF output of univariate visualizations for each Outcome and Vulnerability Factor
* 4_exploratory_analysis.R: generate a PDF output of exploratory plots to capture the relationship between each Vulnerability Factor and all health Outcomes
* 5_principal_component_output.R: generate a PDF output of Principal Component Analysis for all Vulnerability variables within a Pathways Domain
* 6_latent_class_anlaysis.R: generate a population segmentation output using a set of Vulnerability variables
* 7_country_vulnerability_ranking.R: generate the in-country segment vulnerability rankings
* 8_quantitative_segment_profile.R: generate the quantitative profile segmentation plots for a segmentation solution
* 9_typing_tool.R: generate the outputs to help define which variables should be used in developing a Pathways typing tool.
* 11_analysis_helper_script.R: a script to run some ad-hoc analyses throughout the workflow
* functions/*: functions called throughout each module of the workflow.
* 'pathways data dictionary.xlxs': data dictionary for variables used in the pathways segmentation analysis

#### Config - template.yml file variables

* **project_name:** project title to be placed on PDF outputs
* **survey_name** name of the survey to be added as a column in the output datasets
* **root_path:** directory path from which analysis output paths will build from; this is the location of the project
* **user_path:** subfolder for analysis outputs (allows for multiple runs within the same project)

* **dhs_ir_file:** STATA file name of the DHS individual recode file
8 **dhs_hh_file:** STATA file name of the DHS household recode file
* **dhs_br_file:** STATA file name of the DHS birth recode file
* **dhs_kr_file:** STATA file name of the DHS child recode file
* **survey_file:** file name of a non-DHS survey

* **create_new_pathways_workbook:** TRUE/FALSE, determines whether to import an existing Pathways Workbook or create a new one as part of the data cleaning and variable generation phase
* **pathways_workbook_is_excel:** TRUE/FALSE, use the Pathways Workbook in Excel or CSV format
* **pathways_workbook_path:** name of Pathways Workbook to import from
* **new_pathways_workbook_path:** name of Pathways Workbook to generate

* **shp_file:** name of shp file to use when generating maps in quantitative profiling phase
* **data_state_var:** variable to define geographic state, also used for generating maps

* **use_svy_design:** TRUE/FALSE (determines whether to use survey weights and design throughout the analysis)
* **svy_id_var:** survey id var (e.g., survey cluster no)
* **svy_strata_var:** survey stratification variable

## Pathways Segmentation workflow phases

There are 8 iterative phases in a Pathways Segmentation workflow:

- Analysis setup
- Data cleaning and variable generation
- Univariate analysis
- Exploratory analysis
- Principal component analysis (PCA)
- Latent class analysis (LCA)
- In-country vulnerability ranking
- Quantitative segment profiling
- Segmentation typing tool 

Each phase is iterative in the sense that information obtained from a given phase may motivate a return to prior phases to make changes (how a variable is coded, which variables to include, etc.)  The workflow begins with a population survey dataset and concludes with the same survey dataset with an additional column dividing the individuals into segments.

### Analysis setup

In this phase we set up the R environment for the analysis, including:

* Install/load all required R libraries using the renv framework (optional)
* Read in variables defined in the config.yml file
* Create folder structure for the analysis and define file paths
* Import data dictionary and Pathways Workbook (if exists)

### Data cleaning and variable generation

In this phase we import the survey data, clean the data, and code the Health Outcomes and Vulnerability variables.  We assume that these are defined survey-specific based on the location, time, and sample population characteristics of the survey.

The following guidance applies to coding variables:

* **Health Outcomes:** Health Outcomes should be defined as binary 0/1 variables with 1 representing the less disirable health/behavioral state.  For example, the variable anc.less4.last should = 1 when the individual had 3 or less ANC visits during the most recent pregnancy and = 0 when there were 4 or more ANC visits.  Continuous variables may also be created for Health Outcomes but they will not fit into the quantitative segment profiling phase well.  Multi-level categorical variables should not be created as Health Outcomes as their complex interpretation as a dependent variable does not fit into the exploratory data analysis regression analysis.

* **Vulnerability variables:** Vulnerability variables have more freedom in how they are defined relative to Health Outcomes as multi-level categorical variables are expected and it's often less clear what which is the more "positive" or "negative" state.  For example, type of work (Agricultural, Professional, etc.).  Variables should be coded as binary/categorical data type, or have an additional categorical version of the variable to use, as in all phases beyond the Exploratory Data Analysis variables should be categorical. Continuous Vulnerability variables can be created to be used in the Univariate and Exploratory Analysis phases in order to help identify thresholds, relationships, and gradients across variable states.

**Scripts used:** The parent script 2_data_cleaning.R calls the function defined in:
* functions/fun_gen_vulnerabilities_dhs.R
* functions/fun_gen_outcomes_dhs.R

 * Inputs:
    * A survey dataset(s)
 * Outputs:
    * Dataframes for Health Outcomes (outcomes), Vulnerability variables (vulnerability), and combination (outcomes_vulnerability) saved as rds and csv files.
    * Pathways Workbook with all Outcomes, Vulnerabilities, and available metadata populated.

### Univariate analysis

In this phase we generate univariate distribution plots (in PDF) for each of the Health Outcomes and Vulnerability variables. These plots help us to understand the following:

* Is the variable coded as expected?
* What is the sample size of the variable?
* Can categories be collapsed/combined to simplify the variable?
* If we coded a variable as numeric, is there a clear shift in the distribution where we can make it binary/categorical?
* Are data types correct?

**Scripts used:** The parent script 3_univariate_analysis.R calls the function defined in functions/fun_univariate_output.R

* Inputs:
    * Pathways Workbook
        * outcomes.univariate_include
        * vulnerabilities.univariate_include
    * outcomes dataframe
    * vulnerability dataframe
* Outputs:
    * univariate_plots_outcomes.pdf
    * univariate_plots_vulnerability.pdf

### Exploratory data analysis

In this phase we generate exploratory plots (in PDF) that identify the relationship between Vulnerability variables and Health Outcomes. We want to create a segmentation solution using Vulnerability variables that have a statistically significant relationship with relevant Health Outcomes. The PDF output contains bivariate regression results by Vulnerability Factor and the full set of Health Outcome (regress Outcome on Vulnerability Factor); these outputs help us determine the following:

* Is the Vulnerability Factor consistently associated with important Health Outcomes (e.g., ANC visits, U5 mortality)
* Can some levels in a categorical variable be collapsed based on their relationship with important Health Outcomes?  I.e., is the predicted probability of the Health Outcome effectively the same for multiple categories?
* Do some relationships exist that are contrary to what we believe?  This could be motivation to check the variable coding.
* Are data types correct?

**Scripts used:** The parent script 4_exploratory_data_analysis.R calls the function defined in functions/fun_gen_exploratory_data_analysis.R

* Inputs:
    * Pathways Workbook
        * outcomes.eda_include
        * vulnerabilities.eda_include
    * outcomes dataframe
    * vulnerability dataframe
    * outcomes_vulnerability dataframe
* Outputs:
    * eda_{x}.pdf (where x is the Vulnerability Factor)

### Principal component analysis (PCA)

In this phase we are looking to reduce dimensionality of Vulnerability variables within a Pathways Domain. From this phase forward, the analyses are conducted separately for each Segmentation strata. The PDF outputs help us determine the following:

* Wich variables are highly correlated with each other and can be dropped from the next (LCA) phase?
* Do we have Vulnerability variables from each Domain included before we move to the LCA phase?
* Are the data types correct?

**Scripts used:** The parent script 5_principal_component_analysis.R calls the function defined in functions/fun_gen_pca_output.R

* Inputs:
    * Pathways Workbook
        * vulnerabilities.pca_strata
        * vulnerabilities.pca_include
    * vulnerability dataframe
* Outputs:
    * pca_plots_{x}.pdf (where x is the Segmentation strata)

### Latent classification analysis (LCA)

In this critical phase we input a set of Vulnerability variables into the LCA algorithm to generate a series of n-class solutions (n = [2,10]). This phase is conducted separately for each segmentation strata. There are two types of PDF outputs generated at this phase: diagnostic plots, which focus on the statistical properties of the classification solution, and exploratory plots which focus on the differences across the input variables based on the classes of the output. This approach balances statistical and socio-epidemiological criteria in deciding upon the correct n-class solution.

**Scripts used:** The parent script 6_latent_class_analysis.R calls the functions defined in:

* functions/fun_gen_lca.R
* functions/fun_gen_lca_diagnostic_output.R
* functions/fun_gen_lca_exploratory_output.R

* Inputs:
    * Pathways Workbook
        * vulnerabilities.lca_strata
        * vulnerabilities.lca_include
    * outcomes dataframe
    * vulnerability dataframe
    * outcomes_vulnerability dataframe
* Outputs (where x is Segmentation strata):
    * {x}_outcomes_vulnerability_class dataframe as rds and csv
    * {x}_lca_output_plots.pdf
    * {x}_lca_exploratory_plots.pdf

### In-country vulnerability ranking

In this phase we use a set of outcome variables to rank the segments by vulnerability.

**Scripts used:** The parent script 7_country_vulnerability_ranking.R calls the function defined in functions/fun_gen_country_vulnerability_rankings.R

* Inputs:
    * Pathways Workbook
        * params.final_model column
        * outcomes.ranking_include column
    * outcomes_vulnerability_class dataframe
* Outputs:
    * outcomes_vulnerability_class_ranked dataframe

### Quantitative segment profiling

In this phase we have chosen our n-class solution and want to view how the larger scope of Health Outcomes and Vulnerability variables differ across the segments. 

These visual outputs help us to create a quantitative narrative for each segment. 

**Scripts used:** The parent script 8_quantitative_segment_profile.R calls the function defined in functions/fun_gen_quantitative_segment_profile.R

* Inputs:
    * Pathways Workbook
        * outcomes.profile_include
        * vulnerabilities.profile_strata
        * vulnerabilities.profile_include
    * Country shp file
    * outcomes_vulnerability_class dataframe
* Outputs:
    * {x}_segment_profile_plots.pdf (where x is Segmentation strata)

### Segment typing tool

In this phase we use statistical methods to identify a parsimonious set of Vulnerability variables that determine segment membership.  The overarching goal for this phase is to develop the inputs for a questionnaire tool that can be used to classify out-of-sample women.

**Scripts used:** The parent script 9_segmentation_typing_tool.R calls the function defined in functions/fun_gen_segmentation_typing_tool.R

* Inputs:
    * Pathways Workbook
        * vulnerabilities.typing_tool_strata
        * vulnerabilities.typing_tool_include
    * outcomes_vulnerability_class dataframe
* Outputs:
    * typing_tool_outputs_{x}.pdf (where x is Segmentation strata)

## Pathways Segmentation analysis quickstart tutorial

The following is a step-by-step walkthrough using a sample of the Ethiopia 2016 DHS data (500 women).  

This tutorial assumes the Pathways Workbook is generated as xlsx file but if generating as a csv then each Excel tab will be treated as a standalone CSV.  The workflow itself is the same.

### Analysis setup

1. verify the 'analyses\projects\eth-2016-tutorial' folder exists.  This folder contains the data and config file needed for this tutorial.
3. copy all the files from the analyses\a-new-analysis-template folder into the 'analyses\projects\eth-2016-tutorial\' folder
5. inside this folder open the pathways-segmentation.Rpoj file to automatically set the working directory
6. If using renv run renv::restore() in the console to install libraries into the project specific library folder (you may need to install the renv library).  Once complete, run renv::status() to confirm project is up to date.
7. from within RStudio, open and confirm the config.yml parameters are set for this tutorial:
    - project_name: "Ethiopia DHS 2016 - Tutorial"
    - survey_name: "ET71FL"
    - root_path: "path-to-repository/analyses/projects/eth-2016-tutorial/"
    - user_path: "output/"
    - dhs_ir_file: ""
    - dhs_hh_file: ""
    - dhs_br_file: ""
    - dhs_kr_file: ""
    - dhs_mr_file: ""
    - survey_file: ""
    - create_new_pathways_workbook: TRUE
    - pathways_workbook_is_excel: TRUE for xlsx, FALSE for csv
    - pathways_workbook_path: "pathways workbook - ethiopia dhs 2016 tutorial"
    - new_pathways_workbook_path: "pathways workbook - ethiopia dhs 2016 tutorial (new)"
    - shp_file: "gadm41_ETH_shp/gadm41_ETH_1.shp"
    - data_state_var: "v024"
    - use_svy_design: TRUE
    - svy_id_var: "v021"
    - svy_strata_var: "v023"
8. open the 1_setup.R script and run the entire script to install/load libraries, define input variables, and set file paths.

### Data cleaning and variable generation 

1. edit functions/fun_gen_vulnerabilities.R to define the initial coding for Vulnerability variables (for this tutorial it is not necessary to change anything but recommended to review since any new project will require coding variables.)
2. edit functions/fun_get_outcomes.R to define the initial coding for Health Outcomes (for this tutorial it is not necessary to change anything but recommended to review)
3. open the 2_data_cleaning.R script and run the entire script to create the vulnerability.rds and outcomes.rds outputs along with the project Pathways Workbook (pathways workbook - ethiopia dhs 2016 tutorial (new)). Note that if using the CSV formatted Pathways Workbook, there will be three separate output files (params, outcomes, vulnerabilities)
4. rename the Pathways Workbook from (pathways workbook - ethiopia dhs 2016 tutorial (new)) to (pathways workbook - ethiopia dhs 2016 tutorial.xlsx) to match the values in the config.yml file.  This renaming step is to avoid unintentionally overwriting an existing Pathways Workbook.
5. review the contents of this workbook and fill in any missing values for short_name and desciption.
6. open the config.yml file and set the create_new_pathways_workbook = FALSE (we will be using the newly created workbook going forward)

### Univariate analysis

1. open the Pathways Workbook and edit the univariate_include columns in the 'outcomes' and 'vulnerabilities' tabs, set to 1 to include variable in the Univariate PDFs. By default all variables created in the data cleaning phase are set to be included in the univariate analysis step.
2. save and close the workbook
3. open the 3_univariate_analysis.R script and run the entire script to generate the PDF outputs
4. review the PDF outputs.  In this step we are looking at the variable distributions, specifically we want to identify where some categorical variable levels can be collapsed due to small sample size.

### Exploratory analysis

1. Open the Pathways Workbook and edit the eda_include columns in the 'outcomes' and 'vulnerabilities' tabs in the Pathways Workbook, set to 1 to include variable in the Exploratory Analysis PDFs.  
2. save and close the Workbook
3. open the 4_exploratory_data_analysis.R script and run the entire script to generate the PDF outputs.  At the beginninf of the script is the option to parallelize the generation of these outputs.  When running this initially it's recommended to set this to TRUE 
4. review the PDF outputs and make any changes to to the function scripts in the Variable Generation step
5. run the Univariate Analysis step to generate new PDF outputs

### Principal component analysis (PCA)

1. open the Pathways Workbook and edit the 'vulnerabilities' tab
    * define the strata for which the variable should be included in the pca_strata column
    * set the pca_include column to 1 to include in the PCA output PDF
    * ensure the appropriate domain columns are set to 1 for the variable
2. save and close the Workbook
3. open the 5_principal_component_analysis.R script and run the entire script to generate the PDF outputs
4. review the PDF outputs to reduce the number of vulnerabilities included in each domain
5. repeat any previous steps as needed and iterate with the PCA phase until ready to proceed to the next phase

### Latent class analysis (LCA)

1. open the Pathways Workbook and edit the 'vulnerabilities' tab
    * define the strata for which the variable should be included in the lca_strata column
    * set the lca_include column to 1 to include in the LCA output PDF
2. save and close the Workbook
3. open the 6_latent_class_analysis.R script, set the nreps parameter (number of algorithm repititions) and run the entire script to generate the two sets of LCA PDF outputs
4. review both the lca_output PDF and the LCA_exploratory PDF to assess the statistical and socio-epidemiological outputs of the classification algorithm
5. repeat any previos steps as needed and iterate with the LCA phase until an acceptable segmentation solution exists

### Country segment ranking

1. open the Pathways Workbook and edit the 'outcomes' tab
    * set the ranking_include column to 1 to include in the in-country analysis
2. save and close the Workbook
3. open the 7_country_vulnerability_ranking.R script, set the nreps parameter to an existing LCA output, and run the entire script to generate the {strata}_outcomes_vulnerability_class_ranked.rds outputs which contain an additional vulnerability ranked labelling for the segments

### Quantitative segment profiling

1. open the Pathways Workbook and edit the 'vulnerabilities' tab
    * define the strata for which the variable should be included in the profile_strata column
    * set the profile_include column to 1 to include in the segment profile PDF
2. save and close the Workbook
3. open the 8_quantitative_segment_profile.R script, set the nreps parameter to an existing LCA output, and run the entire script to generate the quantitative segment profile PDF outputs

### Segment typing tool

1. open the Pathways Workbook and edit the 'vulnerabilities' tab
    * define the strata for which the variable should be included in the typing_tool_strata column
    * set the typing_tool_include column to 1 to include in the typing tool PDF
2. edit the final_model column in the 'params' tab to identify the final models for each strata (e.g., LCA5_class for a 5 class solution)
2. save and close the Workbook
3. open the 8_typing_tool.R script, set the nreps parameter to an existing LCA output, and run the entire script to generate the typing tool PDF outputs

## Common actions throughout the analysis

### Adding a new variable to the Pathways Workbook

After the initial creation of the Pathways Workbook in the Data cleaning step (2_data_cleaning.R), it is common to add additional variables to the fun_gen_vulnerabilities.R and fun_gen_outcomes.R scripts.  These new variables need to be added individually to the Pathways Workbook to be included in subsequent phases of the analysis.  The recommended workflow for this is as follows:

1. Use the 9_analysis_helper_script.R to test out the new variable and ensure that the output is as expected.
2. Add the variable(s) directly to the fun_gen_vulnerabilities.R and/or fun_gen_outcomes.R script.
3. Add the variable to the appropriate Pathways Workbook tab (outcomes or vulnerabilities).
4. Flag the appropriate columns to include in the analysis


