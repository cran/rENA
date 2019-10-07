suppressMessages(library(rENA, quietly = T, verbose = F))
context("Test making R6 sets");

code_names <- c("Data", "Technical.Constraints","Performance.Parameters",
  "Client.and.Consultant.Requests","Design.Reasoning","Collaboration")

test_that("Accumulate returns an R6", {
  data(RS.data)

  df.file <- RS.data

  df.accum <- suppressWarnings(
    ena.accumulate.data.file(
      df.file, units.by = c("UserName", "Condition"),
      conversations.by = c("ActivityNumber", "GroupName"),
      codes = code_names, as.list = FALSE)
  )

  testthat::expect_is(df.accum, "ENAdata",
    "Accumulation with as.list = FALSE did not return ENAdata"
  )
})

test_that("Make.set returns an R6", {
  data(RS.data)

  df.file <- RS.data

  df.accum <- suppressWarnings(
    rENA:::ena.accumulate.data.file(
      df.file, units.by = c("UserName", "Condition"), 
      conversations.by = c("ActivityNumber", "GroupName"),
      codes = code_names, as.list = FALSE
    )
  )
  df.set <- suppressWarnings(
    ena.make.set(df.accum, as.list = FALSE)
  )

  testthat::expect_is(df.set, "ENAset",
    "Set with as.list = FALSE did not return ENAset")

  df.accum2 <- rENA:::ena.accumulate.data.file(
    df.file, units.by = c("UserName", "Condition"), 
    conversations.by = c("ActivityNumber", "GroupName"),
    codes = code_names, as.list = T
  )
  error_set <- testthat::expect_error(
    suppressWarnings(ena.make.set(df.accum2, as.list = F)),
    regexp = "Re-run the accumulation with as.list=FALSE"
  )

  error_set2 <- testthat::expect_warning(
    ena.make.set(df.accum, as.list = T),
    regexp = "ENAdata objects will be deprecated"
  )
})

test_that("Old sets are the same as the new ones", {
  data(RS.data)

  units.by <- c("UserName", "Condition")
  conv.by <- c("Condition", "GroupName")

  df.accum <- suppressWarnings(
    ena.accumulate.data.file(
      RS.data, units.by = units.by,
      conversations.by = conv.by,
      codes = code_names, as.list = FALSE, window.size.back = 4
    )
  )

  df.set <- suppressWarnings(
    ena.make.set(df.accum, as.list = FALSE)
  )

  new.set <- ena.accumulate.data(
          units = RS.data[, units.by],
          conversation = RS.data[, conv.by],
          metadata = RS.data[, code_names],
          codes = RS.data[,code_names],
          model = "EndPoint",
          window.size.back = 4
        ) %>%
          ena.make.set()

  testthat::expect_equivalent(df.set$points.rotated[1, ],
    as.matrix(new.set$points)[1, ])
  testthat::expect_equivalent(df.set$line.weights[1, ],
    as.matrix(new.set$line.weights)[1, ])
})