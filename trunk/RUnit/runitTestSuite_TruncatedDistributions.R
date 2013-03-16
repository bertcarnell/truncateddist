################################################################################
#
# Program Name:  runitTestSuite_TruncatedDistributions.R
# Purpose:       To provide test functions for the TruncatedDistributions package
# Author:        Rob Carnell
# Date:          June 2007
#
# Required Packages:  RUnit
# R version:          2.4.0 (>=2.3.1)
#
################################################################################

rm(list=ls())
require(RUnit)

## used as part of package
#require(riskTreeEngineInput)
#defaultPath <- chartr("/", "//", paste(.path.package("riskTreeEngineInput"), "/RUnit", sep=""))

################# used in development ##########################################
defaultPath <- file.path("c:", "Documents and Settings", "carnellr",
                         "My Documents", "Repositories", "BTRA",
                         "Trunk", "Source", "Rscripts", "TruncatedDistributions")
                         
temp <- list.files(file.path(defaultPath, "R"), full.names=TRUE)
junk <- lapply(temp, source)

testSuite.rtei <-
  defineTestSuite("TruncatedDistributions", dirs=file.path(defaultPath, "RUnit"),
                  testFileRegexp="^runit_[[:alnum:]]+.[rR]$")

testResult <- runTestSuite(testSuite.rtei)

################# used in development ##########################################

htmlFile <- file.path(defaultPath, "RUnit", "Test Results.html")

## warning expected about gcc compiler
suppressWarnings(printHTMLProtocol(testResult, fileName=htmlFile))

browseURL(htmlFile, browser=getOption("browser"))

