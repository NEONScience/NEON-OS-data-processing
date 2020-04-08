# clean out workspace
rm(list = ls())
gc()

# options
options(stringsAsFactors = FALSE)

# load packages
library(tidyverse)
library(restR)

# install and load package

library(devtools)

# devtools::install_local('C:/Users/esokol/Documents/Git/FORKS/NEON-OS-data-processing/neonOSbase')
library(neonOSbase)

# example L1 data to use
my_table <- 'mam_perplotnight_pub'
my_dpid <- 'DP1.10072.001'
my_site_id <- 'SJER'
my_start_date <- '2020-01-01'
my_end_date <- '2020-01-31'

L1_data <- restR::get.os.l1.by.tab.all.opts(
  tab = paste0(my_dpid,':',my_table),
  inclSamples = 'true',
  minStartDate = my_start_date,
  maxStartDate = my_end_date
  )

my_pub_wb <- restR::get.pub.workbook(DPID = my_dpid, table = my_table)


required_pub_field_names <- my_pub_wb %>% 
  filter(table == my_table, downloadPkg != 'none') %>%
  select(fieldName) %>%
  unlist(use.names = FALSE)

L1_names <- names(L1_data)

# fields in data that need to be removed
L1_names %>% setdiff(required_pub_field_names)

# missing any required fieldNames?
required_pub_field_names %>% setdiff(L1_names)

# filter out fields that bork the function
L1_data_formatted <- L1_data[,L1_names %>% intersect(required_pub_field_names)]

# do dupe check
neonOSbase::removeDups(data = L1_data_formatted,variables = my_pub_wb, table = 'mam_perplotnight_pub')
