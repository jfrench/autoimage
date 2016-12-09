# uncomment second p to generate pdf in home directory
# pdf shows examples of autolayout to ensure accuracy
p <- FALSE
# p <- TRUE
if(p){
setwd("~")
pdf("test-autolayout.pdf")
plot(0:1, 0:1, type = "n")
text(0.5, 0.5, label = "regular order")
# basic 2x2 layout
autolayout(c(2, 2))
# 3x2 layout with space for legends
autolayout(c(3, 2), legend = "h")
autolayout(c(3, 2), legend = "v")
# 3x2 layout with individuals legends
autolayout(c(3, 2), legend = "h", common.legend = FALSE)
autolayout(c(3, 2), legend = "v", common.legend = FALSE)
# if outer title is desired
suppressWarnings(autolayout(c(2, 2), outer = TRUE))
# reset oma parameters
par(oma = c(0, 0, 0, 0))
# impact of mratio when legend used
autolayout(c(2, 2), legend = "h", mratio = 2)
autolayout(c(2, 2), legend = "h", mratio = 5)

# reverse
par(mfrow = c(1, 1))
plot(0:1, 0:1, type = "n")
text(0.5, 0.5, label = "reverse order")
# 3x2 layout with space for legends
autolayout(c(3, 2), legend = "h", reverse = TRUE)
autolayout(c(3, 2), legend = "v", reverse = TRUE)
# 3x2 layout with individuals legends
autolayout(c(3, 2), legend = "h", common.legend = FALSE, reverse = TRUE)
autolayout(c(3, 2), legend = "v", common.legend = FALSE, reverse = TRUE)
# if outer title is desired
suppressWarnings(autolayout(c(2, 2), outer = TRUE, reverse = TRUE))
# reset oma parameters
par(oma = c(0, 0, 0, 0))
# impact of mratio when legend used
autolayout(c(2, 2), legend = "h", mratio = 2, reverse = TRUE)
autolayout(c(2, 2), legend = "h", mratio = 5, reverse = TRUE)
dev.off()
}

test_that("autolayout check",{
  expect_warning(autolayout(c(2, 2), outer = TRUE))
  par(oma = c(0, 0, 0, 0))
  expect_warning(autolayout(c(2, 2), outer = TRUE, reverse = TRUE))
  par(oma = c(0, 0, 0, 0))
})
