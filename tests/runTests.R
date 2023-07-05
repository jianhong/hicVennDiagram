require("hicVennDiagram") || stop("unable to load hicVennDiagram")
require("InteractionSet") ||
    stop("unable to load InteractionSet")
require("GenomicRanges") ||
    stop("unable to load GenomicRanges")
testthat::test_check("hicVennDiagram")
