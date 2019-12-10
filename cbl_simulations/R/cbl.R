# cbl.R

rm(list = objects())
options(stringsAsFactors = FALSE)
set.seed(1234)
while(dev.next()>1) dev.off()

# kNumSims is the number of simulations
kNumSims <- 1000

##################################################################
# Functions

source("~/cbl/cbl_functions.R")

##################################################################
# Main

cat("65% weighting\n")
res.65 <- PrGradeASim(num.sims = kNumSims,
                   num.outcomes.grid = seq(4, 20, by = 1),
                   a.grid = seq(3.2, 3.9, by = 0.1),
                   b.grid = seq(0.0, 0.3, by = 0.1),
                   num.assignments.grid = c(NA, 4, 8),
                   per.assignment.sd.grid = 0.1,
                   assignment.scoring.fun = LastAssessed65Mean)

cat("35% weighting\n")
res.35 <- PrGradeASim(num.sims = kNumSims,
                      num.outcomes.grid = seq(4, 20, by = 1),
                      a.grid = seq(3.2, 3.9, by = 0.1),
                      b.grid = seq(0.0, 0.3, by = 0.1),
                      num.assignments.grid = c(NA, 4, 8),
                      per.assignment.sd.grid = 0.1,
                      assignment.scoring.fun = LastAssessed35Mean)

cat("Even weighting\n")
res.even <- PrGradeASim(num.sims = kNumSims,
                        num.outcomes.grid = seq(4, 20, by = 1),
                        a.grid = seq(3.2, 3.9, by = 0.1),
                        b.grid = seq(0.0, 0.3, by = 0.1),
                        num.assignments.grid = c(NA, 4, 8),
                        per.assignment.sd.grid = 0.1,
                        assignment.scoring.fun = mean)

save.image("res.RData")

cat("Plotting\n")
library(ggplot2)
X11()
PlotCBLSimResults <- function(res) {
  t <- ggplot() +
    geom_line(data = res, aes(x = num.outcomes, y = prob.A.CBL, group = factor(num.assignments), 
                              color = factor(num.assignments)))
  t <- t + geom_point(data = subset(res, num.outcomes == 20 & is.na(num.assignments)), 
                      aes(x = num.outcomes, y = prob.A.CBL))
  t <- t + coord_cartesian()               
  t <- t + facet_grid(a ~ b) + labs(color="Assignments") + theme(text = element_text(size=8))
  return(t)
}

t <- PlotCBLSimResults(res = res.65)
print(t)
ggsave("plot_probA_vs_num.outcomes_all_assignments_65_0p1sd_CBL.png", width=5, height=4)

t <- PlotCBLSimResults(res = res.35)
print(t)
ggsave("plot_probA_vs_num.outcomes_all_assignments_35_0p1sd_CBL.png", width=5, height=4)

t <- PlotCBLSimResults(res = res.even)
print(t)
ggsave("plot_probA_vs_num.outcomes_all_assignments_even_0p1sd_CBL.png", width=5, height=4)

X11()
PlotAvgSimResults <- function(res) {
  t <- ggplot() +
    geom_line(data = res, aes(x = num.outcomes, y = prob.A.Avg, group = factor(num.assignments), 
                              color = factor(num.assignments)))
  t <- t + geom_point(data = subset(res, num.outcomes == 20 & is.na(num.assignments)), 
                      aes(x = num.outcomes, y = prob.A.Avg))
  t <- t + coord_cartesian()               
  t <- t + facet_grid(a ~ b) + labs(color="Assignments") + theme(text = element_text(size=8))
  return(t)
}

t <- PlotAvgSimResults(res = res.65)
print(t)
ggsave("plot_probA_vs_num.outcomes_all_assignments_65_0p1sd_Avg.png", width=5, height=4)

t <- PlotAvgSimResults(res = res.35)
print(t)
ggsave("plot_probA_vs_num.outcomes_all_assignments_35_0p1sd_Avg.png", width=5, height=4)

t <- PlotAvgSimResults(res = res.even)
print(t)
ggsave("plot_probA_vs_num.outcomes_all_assignments_even_0p1sd_Avg.png", width=5, height=4)

cat("Summary Statistics\n")

SummarizeLastGradeError <- function(res) {
  res.perfect <- subset(res, num.outcomes == max(res$num.outcomes) & is.na(num.assignments))
  names(res.perfect)[names(res.perfect) == "prob.A.CBL"] <- "prob.A.CBL.perfect"
  names(res.perfect)[names(res.perfect) == "prob.A.Avg"] <- "prob.A.Avg.perfect"
  
  res.4 <- subset(res, num.assignments == 4)
  names(res.4)[names(res.4) == "prob.A.CBL"] <- "prob.A.CBL.4"
  names(res.4)[names(res.4) == "prob.A.Avg"] <- "prob.A.Avg.4"

  res.8 <- subset(res, num.assignments == 8)
  names(res.8)[names(res.8) == "prob.A.CBL"] <- "prob.A.CBL.8"
  names(res.8)[names(res.8) == "prob.A.Avg"] <- "prob.A.Avg.8"

  comp <- res.perfect[, c("a","b","prob.A.CBL.perfect","prob.A.Avg.perfect")]
  comp <- merge(comp, res.4[, c("num.outcomes","a","b","prob.A.CBL.4", "prob.A.Avg.4")], by = c("a","b"))
  comp <- merge(comp, res.8[, c("a","b","prob.A.CBL.8", "prob.A.Avg.8")], by = c("a","b"))
  
  SummarizeFunction <- function(x) {
    retval <- c(min(x), quantile(x, 0.1), quantile(x, 0.5), quantile(x, 0.9), max(x), mean(x), sd(x))
    names(retval) <- c("min", "q10", "q50", "q90", "max", "mean", "sd")
    return(retval)
  }
  x <- SummarizeFunction(abs(comp$prob.A.CBL.4 - comp$prob.A.CBL.perfect))
  retval <- data.frame(CBL.4 = x)
  row.names(retval) <- names(x)
  retval$CBL.8 <- SummarizeFunction(abs(comp$prob.A.CBL.8 - comp$prob.A.CBL.perfect))
  retval$Avg.4 <- SummarizeFunction(abs(comp$prob.A.Avg.4 - comp$prob.A.Avg.perfect))
  retval$Avg.8 <- SummarizeFunction(abs(comp$prob.A.Avg.8 - comp$prob.A.Avg.perfect))
  
  return(retval)
}

x <- SummarizeLastGradeError(res.even)
print(round(x, digits = 3))

