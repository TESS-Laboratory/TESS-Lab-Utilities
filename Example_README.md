# Template Readme for TESS Lab Repositories

*This document provides the recommended README template for GitHub repositories accompanying research projects, publications, datasets and software developed within TESS Lab. All public repositories should follow this structure where appropriate to ensure they contain the essential information needed to understand, reproduce and reuse the work.

---

# REPLACE WITH PROJECT TITLE


This repository contains the data and code used to reproduce the analyses, figures and tables presented in:

> Author A., Author B. & Author C. (Year). *Paper title*. Journal. DOI: https://doi.org/...

A permanent version of this repository is archived at [![DOI](https://zenodo.org/badge/DOI/...svg)](...)

Use of this code is licensed under [![Licence: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](...)

Contact Author A. and/or Author B at EMAIL.


---

## Project overview

Provide a brief overview of the project (approximately 2–4 sentences), including the research question or objective, the study system or data, and the purpose of the repository.

---

## Repository contents
Provide a brief description of all key datasets and scripts included in the repository.

### Scripts
*(It is good practice to number scripts when the workflow is sequential (`01_`, `02_`, `03_`...), unless using [Targets](https://books.ropensci.org/targets/))*

| Script | Description |
|---------|-------------|
| *01_download_data.R* | Downloads or imports raw datasets. |
| *02_preprocess_data.R* | Cleans and prepares datasets for analysis. |
| *03_train_models.R* | Fits statistical or machine learning models. |
| *04_validation.R* | Performs model validation and accuracy assessment. |
| *05_generate_figures.R* | Produces all manuscript figures. |
| *06_tables.R* | Produces manuscript tables and supplementary outputs. |

### Data

| File | Description |
|------|-------------|
| *field_data.csv* | Field observations used for model calibration. |
| *training_points.gpkg* | Training dataset. |
| *validation_points.gpkg* | Independent validation dataset. |

If datasets cannot be shared openly, explain why.

---

## Getting started

Clone this repository and review the project overview and repository structure before running the analysis. 

TESS Lab projects typically use [renv](https://rstudio.github.io/renv/) to record package dependencies and software versions. 
Where an renv.lock file is included, restore the project environment before running any analyses:
```r
renv::restore()
```

## Running the analysis

Unless otherwise stated, scripts should be run in numerical order:

1. *01_download_data.R*
2. *02_preprocess_data.R*
3. *03_train_models.R*
4. *04_validation.R*
5. *05_generate_figures.R*
6. *06_tables.R*

---


## Recommended practice

- Use consistent directory names (`data`, `scripts`, `outputs`, `docs`) across repositories.
- Never modify files in (`data/raw/`). Store original input data in (`data/raw/`) and write all intermediate and processed datasets to (`data/processed/`) (or another appropriate output directory).
- Include a table of every important script and dataset — this is often the most useful part for new users.
- Include ORCID IDs for repository contributors to improve attribution and researcher traceability.
- If data cannot be shared (e.g. commercial satellite imagery or confidential field data), explain exactly why and state what substitute or sample data are included instead.
- Use `renv.lock` to record package dependencies and software versions.
- Archive a permanent release of the repository (e.g. using Zenodo) and cite the DOI in the associated publication.
- Include a LICENSE file (we normally use the GNU GPL v3 licence in TESS Lab)

### Optional enhancements
- It can be very helpful to include workflow visualisations (we often create these in [draw.io](https://app.diagrams.net/))
- Funding acknowledgement if applicable
- Italicise filenames in tables and text (a nice touch that improves readability).
- Project logo

---

## Checklist 
 Every repository should include

✔ Clear project title

✔ Short project description (2–4 sentences)

✔ Citation to associated paper/preprint

✔ Repository structure

✔ Complete list of scripts (numbered where execution order matters)

✔ Description of all key input and output datasets

✔ Instructions for reproducing the analysis

✔ Licence

✔ Contact name and email