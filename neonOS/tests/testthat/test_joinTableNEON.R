context('Joining tables')
library(neonOS)

testj <- joinTableNEON(table1=read.csv(system.file("extdata", "brd_countdata.csv", package="neonOS")), 
                    table2=read.csv(system.file("extdata", "brd_perpoint.csv", package="neonOS")),
                    name1="brd_countdata", name2="brd_perpoint")
test_that("Test that bird tables join correctly", {
  expect_equal(nrow(read.csv(system.file("extdata", "brd_countdata.csv", package="neonOS"))),
               nrow(testj))
})