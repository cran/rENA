# context("Test piping")

data(RS.data)
RS.data <- data.table::as.data.table(RS.data)

codes = c('Data','Technical.Constraints','Performance.Parameters','Client.and.Consultant.Requests','Design.Reasoning','Collaboration');
units = c("Condition", "UserName");
conversation = c("Condition","GroupName");

# Original
old_accum <- rENA::ena.accumulate.data(
  units = RS.data[, ..units],
  conversation = RS.data[, ..conversation],
  codes = RS.data[, ..codes],
  model = "EndPoint",
  window.size.back = 4
)
old_model <- rENA::ena.make.set(old_accum);

testthat::test_that("Test basic model accumulation", {
  # Simple
  enaset1 <- RS.data |> rENA::accumulate(units, codes, conversation, default_window = 4);
  
  testthat::expect_identical(
    as.matrix(old_model$connection.counts)[1,],
    as.matrix(enaset1$connection.counts)[1,]
  )
})

# data <- RS.data |> units("Condition", "UserName") |> 
#   horizon("Condition", "GroupName") |> 
#   codes("Data", "Technical.Constraints", "Performance.Parameters",
#     "Client.and.Consultant.Requests", "Design.Reasoning", "Collaboration"
#   )

# set4 <- data |> accumulate(default_window = 4) |>
#   model()

# set5 <- data |> accumulate(default_window = 5) |>
#   model()


# data(RS.data)

# RS.data <- data.table::as.data.table(RS.data)

# profvis::profvis({
#   accumulate(
#     x = RS.data, 
#     units = c("Condition", "UserName"), 
#     horizon = c("Condition", "GroupName"), 
#     codes = c("Data", "Technical.Constraints", "Performance.Parameters", "Client.and.Consultant.Requests", "Design.Reasoning", "Collaboration"), 
#     default_window = 5
#   )
# })
