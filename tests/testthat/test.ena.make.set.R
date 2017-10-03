# suppressMessages(library(rENA, quietly = T, verbose = F))
# context("Test making sets");
#
# #codeNames = c("E.data","S.data","E.design","S.design","S.professional","E.client","V.client","E.consultant","V.consultant","S.collaboration","I.engineer","I.intern","K.actuator","K.rom","K.materials","K.power");
# codeNames = c('Data','Technical.Constraints','Performance.Parameters','Client.and.Consultant.Requests','Design.Reasoning','Collaboration');
#
# test_that("Simple data.frame to accumulate and make set", {
#   df.file <- system.file("extdata", "rs.data.csv", package="rENA")
#   df.accum = ena.accumulate.data.file(df.file, units.by = c("UserName","Condition"), conversations.by = c("ActivityNumber","GroupName"), codes = codeNames);
#   df.set = ena.make.set(df.accum)
#
#   testthat::expect_equal(
#     label = "Used 10 codes",
#     object = length(df.set$codes),
#     expected = 6
#   );
#   testthat::expect_equal(
#     label = "48 units with 2 dimensions",
#     object = dim(df.set$points.rotated),
#     expected = c(48,2)
#   );
#   testthat::expect_equal(
#     label = "Has all 48 units",
#     object = length(df.set$enadata$unit.names),
#     expected = 48
#   );
# })
# test_that("Simple data.frame to accumulate and make set with Linderoth method(s)", {
#   df.file <- system.file("extdata", "rs.data.csv", package="rENA")
#   df.accum = ena.accumulate.data.file(df.file, units.by = c("UserName","Condition"), conversations.by = c("ActivityNumber","GroupName"), codes = codeNames);
#   df.set.lws.es = ena.make.set(df.accum, position.method = lws.positions.es)
#
#   testthat::expect_equal(
#     label = "48 units by 2 dimensions",
#     object = dim(df.set.lws.es$points.rotated),
#     expected = c(48,2)
#   );
#   testthat::expect_equal(
#     label = "48 units",
#     object = length(df.set.lws.es$enadata$unit.names),
#     expected = 48
#   );
# })
# test_that("Make a simple trajectory set", {
#   df.file <- system.file("extdata", "rs.data.csv", package="rENA")
#   df.accum = ena.accumulate.data.file(df.file, units.by = c("UserName","Condition"), conversations.by = c("ActivityNumber","GroupName"), codes = codeNames);
#   df.accum.traj = ena.accumulate.data.file(
#     df.file, units.by = c("UserName","Condition"),
#     conversations.by = c("ActivityNumber","GroupName"),
#     codes = codeNames,
#     model = "AccumulatedTrajectory",
#     trajectory.by = c("ActivityNumber")
#   );
#
#   df.set.lws = ena.make.set(df.accum.traj, node.position.method = lws.positions)
#
#   # testthat::expect_equal(
#   #   length(attr(df.set.lws$points.rotated, opts$UNIT_NAMES)[,UserName]),
#   #   517
#   # );
# })
