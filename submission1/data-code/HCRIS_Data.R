# Meta --------------------------------------------------------------------
## Author:        Genevieve DeBell
## Date Created:  2/5/2025
## Date Edited:   2/5/2025


# Preliminaries -----------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate)
# Read and combine data ---------------------------------------------------
source('Users/genevievedebell/Documents/GitHub/hwk2/submission1/data-code/H1_HCRISv1996.R')
source('Users/genevievedebell/Documents/GitHub/hwk2/submission1/data-code/H2_HCRISv2010.R')

final.hcris.v1996=read_rds('data/output/HCRIS_Data_v1996.rds')
final.hcris.v2010=read_rds('data/output/HCRIS_Data_v2010.rds')
