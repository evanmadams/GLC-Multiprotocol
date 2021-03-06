# GLC-Multiprotocol Description
A project where we are exploring methods for integrating multiple aerial survey data sets observing waterbirds in the Great Lakes. We modeled the abundance and distribution of six waterbird taxonomic groups using a multi-protocol distance sampling approach. This repository includes a JAGS model file (in .txt format) and the code to run the file in R with the appropriate data and initial values for each of the six taxa. Simply run the JAGS model and place it in the working directory of your R instance then Additionally, we've included an R script that processes waterbird observation data that was given to the modeling team. Data sets are not included in this repository but can be found in the Midwest Avian Data Center (http://data.pointblue.org/partners/mwadc/index.php?page=home). More details on model development and background can be found in the report Adams et al. (2017): 'The effect of ice coverage on non-breeding abundance of Great Lakes waterbirds'. All scripts are designed to work on a personal workstation, to implement these scripts the original data set and some modification to the code to work on a new workstation is needed.

## Files Included

### Data management file for R:

1. glc_spp8_5km_data_mgmt.R

### Taxon-specific models:

Long-tailed Duck

1. JAGS model - glc_p6_spp1_hn.txt

2. R script - glc_prot6_spp1_hn_run.R

Gulls

1. JAGS model - glc_spp2_haz.txt

2. R script - glc_spp2_haz_run.R

Goldeneyes

1. JAGS model - glc_p6_spp3.txt

2. R script - glc_prot6_spp3_hn_run.R

Loons

1. JAGS model - glc_p7_spp5.txt

2. R script - glc_prot7_spp5_hn_run.R

Mergansers

1. JAGS model - glc_p6_spp6.txt

2. R script - glc_prot6_spp6_hn_run.R

Scaup

1. JAGS model - glc_p6_spp7.txt

2. R script - glc_prot6_spp7_hn_run.R

