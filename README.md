# Final Project

## Original Paper

We chose to re-create the data from the paper found here: <https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0087628>

The original paper has three figures, but we chose to create a few of our own figures from the data since we had six group members.

## Data

The original data files can be found in the `Data` folder. These files were used to re-create the plots.

## Code

All the code we used can be found in `Code` folder.

### Figure 1

The final code used to generate figure 1 can be found in the `Figure1.Rmd` file. The `Figure1_FinalProject.R` file contains the crude code troubleshooting that was used to generate the final .Rmd file. The `Figure1.Rmd` file contains all the descriptions of the code in markdown text and how the data was formatted to create figure 1. 

### Table 4

The PLOS 2014 paper provides a tabular format to show significant QTL contributing to elemental phenotypes. On the same premise, files were generated to perform the QTL analysis, but instead of presenting them in tabular format, they are presented in QTL visualizations and QTL effect plots for chromosome 1. The final code used to generate the visualizations using inspiration from Table 4 can be found in the `Table4QTLanalysis.Rmd` file. The `Table4QTLanalysis.R` file is a raw script used to generate the visualizations for this portion of the project.

## Visualizations

Final figures can be found in the `Visualizations` folder. 

### Figure 1

The final file for figure 1 is the `Figure1.png` file. An additional figure was generated from the data used to make figure 1 that visualizes all 20 elements at once. This file can be found as `Figure1_all_elements.png`. 


### Figures from Table 4
There are three figures generated from table 4. `Table4_QTL_ScanChr1` is a QTL scan of chromosome 1. `Table4_QTL_effect_MarkerFromAnalysis` is a visualization depicting the effect of the QTL on the mean Selenium phenotype when using the marker from the upstream analysis. `Table4_QTL_effect_MarkerFromPaper` is a visualization depicting the effect of the QTL on the mean Selenium phenotype when using the marker that was reported in the paper.
