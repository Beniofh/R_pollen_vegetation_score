<div align="justify">
  
# Inference of vegetation by calculating pollen scores

## Usage
This GitHub repository provides the R scripts and datasets needed to reproduce the results of the article **More objective inference of local and regional vegetation types using pollen assemblages from the East African Rift (Ethiopia)** currently being reviewed for the **Review of Palaeobotany and Palynology**. The R scripts were created using **version 4.1.2 of R** and **version 3.5.0 of the R package ggplot2**. An R_vegetation_score_article.Rproj file is also available for RStudio users. 
  
## Installation and use
To use the scripts, download this GitHub repository and unzip the `R_pollen_vegetation_score-main.zip` archive. You can then move the `R_pollen_vegetation_score-main` directory but you must not change the organisation of the elements within the directory. The scripts to run are `main_1_modern_V_score.R` and `main_2_additional_visualisation`. To run the scripts, make sure that the current working directory is `./R_pollen_vegetation_score-main`: getwd() function. If this is not the case, you need to redefine `./R_pollen_vegetation_score-main` as the current directory using the setwd() function. Finally, you need to install the ggplot2 package, preferably version 3.5.0 with the command: install_version("ggplot2", version = "2.3.0"). 

:warning: The main_1_modern_V_score.R file can take between 30 and 60 seconds to run.
## Organisation of this repository
The GitHub repository consists of 3 main directories.  

- `data`: this directory contains the datasets used in the article in csv format (**separator ";"**). More specifically, there are two csv files containing the data from **Table S2** and **Table S5** in the article.  
- `scr`: this directory contains the R scripts. The scripts to be run are those beginning with "main". The comments at the beginning of each script include: an explanation of what the script does, a presentation of the input data, a presentation of the adjustable parameters and a list of the data and images produced by the script. In the scr folder, there is also the `Functions` directory, which contains custom functions used by the "main" scripters.  
- `output`: this is the folder in which all script output is stored by default.  

## Credits

This work was produced at [INRIA](https://www.inria.fr/en) research institute. If you use this framework in your research, please cite the article "More objective inference of local and regional vegetation types using pollen assemblages from the East African Rift (Ethiopia)".

Author: Benjamin BOUREL [:envelope:](mailto:benjamin.bourel@inria.fr)