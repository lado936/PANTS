context("color_bar")

test_that("alternative", {
  #2-sided
  score.v <- (-5):5 #no NAs in plot_pwy before calls match_mats
  lim <- c(-max(abs(score.v)), max(abs(score.v)))
  color.pal <- RColorBrewer::brewer.pal(n=9, name="RdYlBu")[9:1]
  
  pp.na <- function(){ plot.new(); color_bar(col=c(NA, color.pal), lev=lim) }
  expect_doppelganger(title="cb.na", pp.na)
  
  pp.nona <- function(){ plot.new(); color_bar(col=color.pal, lev=lim) }
  expect_doppelganger(title="cb.nona", pp.nona)
  
  cb.na <- color_bar(cols=c(NA, color.pal), lev=lim, plot=FALSE)
  expect_equal(cb.na[1, "barcolors"], "#ffffff")
})
