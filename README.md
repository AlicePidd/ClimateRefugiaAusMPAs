# ClimateRefugiaAusMPAs

This repo contains R code underpinning the analyses for our paper, submitted to Global Change Biology:

> Climate refugia could disappear from Australia’s marine protected areas by 2040

**Authors:** Alice M. Pidd<sup>1</sup>, David S. Schoeman<sup>1-2</sup>, Anthony J. Richardson<sup>3-5</sup>, Kylie L. Scales<sup>1</sup>

<sup>1</sup> Ocean Futures Research Cluster, Global-Change Ecology Research Group, School of Science, Technology and Engineering, University of the Sunshine Coast.

<sup>2</sup> Centre for African Conservation Ecology, Department of Zoology, Nelson Mandela University, Gqeberha, South Africa

<sup>3</sup> Centre for Biodiversity and Conservation Science (CBCS), The University of Queensland, Brisbane, Queensland, Australia

<sup>4</sup> School of the Environment, The University of Queensland, Brisbane, Queensland, Australia

<sup>5</sup> Commonwealth Scientific and Industrial Research Organization (CSIRO) Environment, Queensland Biosciences Precinct (QBP), Queensland, Australia

## Contents

```         
ClimateRefugiaAusMPAs
├── figures_tables      <--- .pdf files of figures and tables in the main text   
├── figure_scripts      <--- code to generate figures in main text (outputs in \figures)  
├── functions           <--- functions used in \scripts 
├── scripts             <--- code used in the analysis 
└── supplementary       <--- supplementary materials for the manuscript 
```

## Workflow

Earth System Model (ESM) outputs used in this study and the associated code were obtained from publicly available data nodes via the Earth System Grid Federation MetaGrid (<https://esgf.nci.org.au/search>). Workflow for downloading, wrangling, and processing ESMs can be followed in the <i>hotrstuff</i> package and GitHub repo (Buenafe, Schoeman, & Everett 2024) at <https://github.com/SnBuenafe/hotrstuff> .

In addition to data preparation scripts relevant to the case study region (here, continental Australia), this repo includes general code for computing and plotting the following metrics of marine climate exposure:

-   Rates of decadal change in ocean climate variables
-   Gradient-based thermal climate velocity
-   Marine heatwave cumulative intensity
-   Identifying climate refugia based on prescribed thresholds of climate exposure

## Machine specifications

All analyses were run on a machine with the following specifications:

```         
Model Name:     MacBook Pro
Chip:           Apple M3 Max
Cores:          16 (12 performance and 4 efficiency)
Memory:         64 GB
OS:             Sequoia Version 15.3.1 (24D70)
R version:      4.4.3 (2025-02-28) -- "Trophy Case"
GitHub:         Version 3.4.16 (arm64)
```

## Questions or feedback?

Please submit an issue, or email your questions to A.Pidd: alicempidd(at)gmail(dot)com
