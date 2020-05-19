### Description

The `neonOS` package is designed to facilitate working with NEON observational (OS) data. **It is under development, and may change without notice.** The functionality planned for immediate development plans include (1) duplicate checking and (2) table joining.

#### Duplicate checking:

NEON OS data are collected and ingested via semi-automated systems that include data validation at several steps. However, in a variety of circumstances it is possible for duplicate records to be entered into the system - for example, an analytical lab may accidentally upload multiple copies of the results for a given sample, or field staff may accidentally make the same measurement twice.

The `removeDups()` function uses metadata to identify and resolve duplicates in OS data. In the `variables` file provided with data downloads, the primaryKey field identifies a set of fields that should define a unique record; i.e., for every unique combination of primaryKey fields, there should be a single record of data. `removeDups()` uses these fields to identify duplicates, then attempts to resolve the duplicates it finds, using these steps:

1. Duplicates are defined as >1 data record with identical values in the primary key fields.
2. If duplicate records contain data fields that are populated in one record and contain NA or empty string values in another record, the non-empty values are kept.
3. If differences between duplicate records are in uid, remarks, or personnel (xxxxxBy) fields, unique values are concatenated, separating by pipes (|).
4. If records are identical after steps 2 and 3, a single merged record is retained and flagged with duplicateRecordQF=1.
5. Records that can't be resolved by steps 2-4 are flagged with duplicateRecordQF=2. Note that in a set of three or more duplicates, some records may be resolveable and some may not; if two or more records are left after steps 2-4, all remaining records are flagged with duplicateRecordQF=2.

Please note that NEON staff are also working to identify and resolve duplicates - if you find duplicates in a data set, and then download the same data later, some or all of the duplicates may have been removed.

#### Table joining

NEON OS data are typically available as multiple data tables, representing data collected at different times or different temporal or spatial resolution. These are relational tables, and can be joined into a smaller number of flattened tables. Guidance in performing these joins can be found in the Data Relationships section of the Data Product User Guide for each product. A generic joining function for OS data is under development and will be added to this package when possible.



### Instructions

The neonOS package currently contains only one function, `removeDups()`. To access the package, install from [GitHub](https://github.com/NEONScience/NEON-OS-data-processing/tree/master/neonOS).

Installation and loading:

```
library(devtools)
install_github('NEONScience/NEON-OS-data-processing/neonOS')
library(neonOS)
library(neonUtilities)
```

`removeDups()` requires 3 inputs: a NEON OS data table, the corresponding variables file, and the name of the data table. An example using the fish data product:

```
fish <- loadByProduct(dpID='DP1.20107.001',
	check.size=F, 
	startdate='2017-05', 
	enddate='2019-08',
	package='expanded')
fish.pass.dups <- removeDups(data=fish$fsh_perPass,
	variables=fish$variables_20107,
	table='fsh_perPass')
```
