# ClimateRefugiaAusMPAs

This repo contains R code underpinning the analyses for our paper, submitted to Global Change Biology:

> Climate refugia could disappear from Australia’s marine protected areas by 2040

**Authors:** Alice M. Pidd<sup>1</sup>, David S. Schoeman<sup>1-2</sup>, Anthony J. Richardson<sup>3-5</sup>, Kylie L. Scales<sup>1</sup>

<sup>1</sup> Ocean Futures Research Cluster, Global-Change Ecology Research Group, School of Science, Technology and Engineering, University of the Sunshine Coast.

<sup>2</sup> Centre for African Conservation Ecology, Department of Zoology, Nelson Mandela University, Gqeberha, South Africa

<sup>3</sup> Centre for Biodiversity and Conservation Science (CBCS), The University of Queensland, Brisbane, Queensland, Australia

<sup>4</sup> School of the Environment, The University of Queensland, Brisbane, Queensland, Australia

<sup>5</sup> Commonwealth Scientific and Industrial Research Organization (CSIRO) Environment, Queensland Biosciences Precinct (QBP), Queensland, Australia

## Questions or feedback?

Please submit an issue, or email your questions to A.Pidd: alicempidd(at)gmail(dot)com

## Workflow

Earth System Model (ESM) outputs used in this study and the associated code were obtained from publicly available data nodes via the Earth System Grid Federation MetaGrid (<https://esgf.nci.org.au/search>). Workflow for downloading, wrangling, and processing ESMs can be followed in the <i>hotrstuff</i> package and GitHub repo (Buenafe, Schoeman, & Everett 2024) at <https://github.com/SnBuenafe/hotrstuff> .

In addition to data preparation scripts relevant to the case study region (continental Australia), this repo includes relatively generalisable code that can be used to compute and plot the following metrics of climate exposure:

> Rates of decadal change in ocean climate variables

> Gradient-based thermal climate velocity

> Marine heatwave cumulative intensity

> Identifying climate refugia based on prescribed thresholds of climate exposure

## Machine specifications

All analyses were run on a machine with the following specifications:

```         

R version 4.4.2 (2024-10-31) -- "Pile of Leaves"
```
