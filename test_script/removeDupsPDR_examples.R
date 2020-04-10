

# using removeDups.L1, example 1 -- 
# pulling L1 data from restR within the function and then checking for dups

mam_perplotnight_pub__dups_flagged_ex1 <- removeDups.L1(
  tab = 'DP1.10072.001:mam_perplotnight_pub', #Name of table for which to get data, in the form DP#.#####.001:table_name_in. [character]
  tableName = NA, #maps to table #optional
  minStartDate = '2020-01-01',
  maxStartDate = '2020-01-31')


# using removeDups.L1, example 2 -- 
# sending L1 data to fxn, but pulling pubWB from restR

# get L1 data
my_L1_data <- restR::get.os.l1.by.tab.all.opts(
  tab = 'DP1.10072.001:mam_perplotnight_pub',
  inclSamples = 'true',
  minStartDate = '2020-01-01',
  maxStartDate = '2020-01-31')

# send L1 data to dup check function, get putwb using restR
mam_perplotnight_pub__dups_flagged_ex2 <- removeDups.L1(
  tab = 'DP1.10072.001:mam_perplotnight_pub', #Name of table for which to get data, in the form DP#.#####.001:table_name_in. [character]
  L1Data = my_L1_data) 


# using removeDups.L1, example 3 -- 
# sending L1 data and pub wb to fxn, using tab argument

# get L1 data
my_L1_data <- restR::get.os.l1.by.tab.all.opts(
  tab = 'DP1.10072.001:mam_perplotnight_pub',
  inclSamples = 'true',
  minStartDate = '2020-01-01',
  maxStartDate = '2020-01-31')

# get pubWB
my_pubWB <- restR::get.pub.workbook(DPID = 'DP1.10072.001', table = 'mam_perplotnight_pub', stack = 'prod')

# send L1 data to dup check function, get putwb using restR
mam_perplotnight_pub__dups_flagged_ex3 <- removeDups.L1(
  tab = 'DP1.10072.001:mam_perplotnight_pub', #Name of table for which to get data, in the form DP#.#####.001:table_name_in. [character]
  pubWB = my_pubWB,
  L1Data = my_L1_data) 


# using removeDups.L1, example 4 -- 
# sending L1 data and pub wb to fxn, using tableName argument

# get L1 data
my_L1_data <- restR::get.os.l1.by.tab.all.opts(
  tab = 'DP1.10072.001:mam_perplotnight_pub',
  inclSamples = 'true',
  minStartDate = '2020-01-01',
  maxStartDate = '2020-01-31')

# get pubWB
my_pubWB <- restR::get.pub.workbook(DPID = 'DP1.10072.001', table = 'mam_perplotnight_pub', stack = 'prod')

# send L1 data to dup check function, get putwb using restR
mam_perplotnight_pub__dups_flagged_ex4 <- removeDups.L1(
  tableName = 'mam_perplotnight_pub',
  pubWB = my_pubWB,
  L1Data = my_L1_data) 


# using removeDups.L1, example 5 -- 
# should error out, you need to send either 'tab' or 'tableName'

# get L1 data
my_L1_data <- restR::get.os.l1.by.tab.all.opts(
  tab = 'DP1.10072.001:mam_perplotnight_pub',
  inclSamples = 'true',
  minStartDate = '2020-01-01',
  maxStartDate = '2020-01-31')

# get pubWB
my_pubWB <- restR::get.pub.workbook(DPID = 'DP1.10072.001', table = 'mam_perplotnight_pub', stack = 'prod')

# send L1 data to dup check function, get putwb using restR
mam_perplotnight_pub__dups_flagged_ex5 <- removeDups.L1(
  pubWB = my_pubWB,
  L1Data = my_L1_data) 


# using removeDups.L1, example 5 -- 
# should error out, you need to send arguments to get L1 data or L1Data

# get pubWB
my_pubWB <- restR::get.pub.workbook(DPID = 'DP1.10072.001', table = 'mam_perplotnight_pub', stack = 'prod')

# send L1 data to dup check function, get putwb using restR
mam_perplotnight_pub__dups_flagged_ex6 <- removeDups.L1(
  tableName = 'mam_perplotnight_pub',
  pubWB = my_pubWB) 


