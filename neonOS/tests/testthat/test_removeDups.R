context('De-duplicating')
library(neonOS)

testd <- removeDups(data=read.csv(system.file("extdata", "cfc_lignin_test_dups.csv", package="neonOS")), 
                    variables=read.delim(system.file("extdata", "cfc_lignin_variables.txt", package="neonOS")),
                    table='cfc_lignin')
test_that("Test that duplicates are identified correctly in foliar lignin", {
  expect_equal(testd$duplicateRecordQF,
              c(0,1,0,0,2,2,0,2,2,0,0,0,0,0,2,2,0,0,0,1,0,0))
})