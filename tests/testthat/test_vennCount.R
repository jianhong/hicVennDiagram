test_that("vennCount works not correct", {
    set.seed(123)
    regions1=GRanges("chr1", IRanges(start=seq(1, 20000000, by=5000),
                                     width=5000))
    regions2=GRanges("chr1", IRanges(start=seq(20000001, 40000000, by=5000),
                                     width=5000))
    regions3=GRanges("chr1", IRanges(start=seq(40000001, 60000000, by=5000),
                                     width=5000))
    a1 <- sample(seq_along(regions1),
                 size = 1000,
                 replace = FALSE)
    a2 <- sample(seq_along(regions2),
                 size = 1000,
                 replace = FALSE)
    gi1 <- GInteractions(anchor1 = a1[seq.int(500)],
                         anchor2 = a2[seq.int(500)],
                         regions = regions1)
    gi2 <- GInteractions(anchor1 = a1[seq.int(500)],
                         anchor2 = a2[seq.int(500)],
                         regions = regions2)
    gi3 <- GInteractions(anchor1 = a1[seq.int(500)],
                         anchor2 = a2[seq.int(500)],
                         regions = regions3)
    gi_exact <- GInteractions(anchor1 = a1[-seq.int(500)],
                              anchor2 = a2[-seq.int(500)],
                              regions = regions1)
    gi_shift <- GInteractions(anchor1 = a1[-seq.int(500)],
                              anchor2 = a2[-seq.int(500)],
                              regions = shift(regions1, 5000))
    venn <- vennCount(list(A=c(gi1, gi_exact), B=c(gi2, gi_exact)))
    expect_equal(venn$vennCounts["10", "A"], 500)
    expect_equal(venn$vennCounts["01", "B"], 500)
    expect_equal(venn$vennCounts["11", "A"], 500)
    expect_equal(venn$vennCounts["11", "B"], 500)
    venn <- vennCount(list(A=c(gi1, gi_exact, gi_shift),
                           B=c(gi2, gi_exact)), maxgap = 1L)
    expect_equal(venn$vennCounts["10", "A"], 500)
    expect_equal(venn$vennCounts["01", "B"], 500)
    expect_equal(venn$vennCounts["11", "A"], 1000)
    expect_equal(venn$vennCounts["11", "B"], 500)
    venn <- vennCount(list(A=c(gi1, gi_exact, gi_shift),
                           B=c(gi2, gi_exact)),
                      FUN = max,
                      maxgap = 1L)
    expect_true(venn$counts["11"]==1000)
    venn <- vennCount(list(A=c(gi1, gi_exact),
                           B=c(gi2, gi_exact),
                           C=c(gi3, gi_exact)))
    expect_equal(venn$vennCounts["100", "A"], 500)
    expect_equal(venn$vennCounts["010", "B"], 500)
    expect_equal(venn$vennCounts["001", "C"], 500)
    expect_equal(venn$vennCounts["111", "A"], 500)
    expect_equal(venn$vennCounts["111", "B"], 500)
    expect_equal(venn$vennCounts["111", "C"], 500)
    venn <- vennCount(list(A=c(gi1, gi_exact, gi_shift),
                           B=c(gi2, gi_exact),
                           C=c(gi3, gi_exact)))
    expect_equal(venn$vennCounts["100", "A"], 1000)
    expect_equal(venn$vennCounts["010", "B"], 500)
    expect_equal(venn$vennCounts["001", "C"], 500)
    expect_equal(venn$vennCounts["111", "A"], 500)
    expect_equal(venn$vennCounts["111", "B"], 500)
    expect_equal(venn$vennCounts["111", "C"], 500)
    venn <- vennCount(list(A=c(gi1, gi_exact, gi_shift),
                           B=c(gi2, gi_exact),
                           C=c(gi3, gi_exact)),
                      maxgap = 1L)
    expect_equal(venn$vennCounts["100", "A"], 500)
    expect_equal(venn$vennCounts["010", "B"], 500)
    expect_equal(venn$vennCounts["001", "C"], 500)
    expect_equal(venn$vennCounts["111", "A"], 1000)
    expect_equal(venn$vennCounts["111", "B"], 500)
    expect_equal(venn$vennCounts["111", "C"], 500)
})
