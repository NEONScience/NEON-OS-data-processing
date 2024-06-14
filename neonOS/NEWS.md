# version 1.1.0

Released 2024-06-14

## Enhancements

* removeDups() now includes a parallel processing option. Use input ncores= to set the maximum number of cores to use. Note that parallelizing only improves performance when there are a large number of duplicates; the function only enables parallelization when the number of duplicates is greater than 100.
* Improved messaging and exception handling around edge cases and exceptions in removeDups().
* joinTableNEON() now includes the option of forcing either a full or left join. Join defaults to full unless otherwise specified in the Quick Start Guide; input left.join= allows the user to override the default or QSG. Use with caution.


-----------------------
