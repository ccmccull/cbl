CBLGrade <- function(outcomes, t1 = 3.3, t2 = 3.0) {
  # Apply the CBL grading system for a single grade level to a vector of outcomes.
  #
  # Args:
  #   outcomes: vector of outcomes
  #   t1: the threshold for criteria 1
  #   t2: the threshold for criteria 2
  # 
  # Returns:
  #   A vector of grades coded as "A" or "<A".
  
  orderstats <- sort(outcomes)
  grade <- ifelse(
    (orderstats[floor(0.25 * length(outcomes))] >= t1) & (orderstats[1] >= t2),
    "A", "<A")
  return(grade)
}

TestCBLGrade <- function() {
  outcomes <- c(2.9, 3.3, 3.86, 3.5)
  stopifnot(CBLGrade(outcomes) == "<A")
  outcomes <- c(3.0, 3.3, 3.86, 3.5)
  stopifnot(CBLGrade(outcomes) == "A")
}

AverageGrade <- function(outcomes, t1 = 3.5) {
  # Apply a simple outcome averaging grading system.
  #
  # Args:
  #   outcomes: vector of outcomes
  #   t1: the threshold for the single criterion
  #
  # Returns:
  #   a vector of grades coded as "A" or "<A"
  
  avg.grade <- mean(outcomes)
  grade <- ifelse(avg.grade >= t1, "A", "<A")
    
  return(grade)
}

TestAverageGrade <- function() {
  stopifnot(AverageGrade(c(2.9,3.0,3.2), 3.0) == "A")
  stopifnot(AverageGrade(c(2.9,3.0,3.0), 3.0) == "<A")
}

TestAverageGrade()

RoundToHalfs <- function(x, min.grade = 1.0, max.grade = 4.0) {
  # Round an assignment grade to half-integers 1.0, 1.5, ..., 4.0
  #  
  # Args:
  #   x: vector (or array) of float assignment grades
  #   min.grade, max.grade: Thesholds on returned grades.
  #
  # Returns:
  #   vector (or array) of half-integer grades.

  if(is.null(dim(x))) {
    x <- pmin(max.grade, pmax(min.grade, x))
  } else{
    x <- apply(x, 1:length(dim(x)), min, max.grade)
    x <- apply(x, 1:length(dim(x)), max, min.grade)
  }
  return(0.5 * round(2*x))
}  

TestRoundToHalfs <- function() {
  stopifnot(all(RoundToHalfs(c(2.4, 2.9, 4.1, 0.4)) == c(2.5, 3.0, 4.0, 1.0)))
  
  x <- array(c(2.4, 2.9, 4.1, 0.4), dim = c(2,1,2))
  correct.val <- array(c(2.5, 3.0, 4.0, 1.0), c(2,1,2))
  stopifnot(all(RoundToHalfs(x) == correct.val))
}
TestRoundToHalfs()

LastAssessed65Mean <- function(x) {
  # A weighted mean with 65% weight applied to the last item.
  #
  # Args:
  #   x: vector of numeric
  #
  # Returns:
  #   Scalar weighted mean.
  
  early.average <- mean(x[1:(length(x) - 1)])
  return(weighted.mean(c(early.average, x[length(x)]), w = c(0.35, 0.65)))
}

TestLastAssessed65Mean <- function() {
  stopifnot(LastAssessed65Mean(c(0, 1)) == 0.65)
  stopifnot(LastAssessed65Mean(c(0, 0, 1)) == 0.65)
  stopifnot(LastAssessed65Mean(c(1, 2, 3)) == 2.475)
}
TestLastAssessed65Mean()

LastAssessed35Mean <- function(x) {
  # A weighted mean with 35% weight applied to the last item.
  #
  # Args:
  #   x: vector of numeric
  #
  # Returns:
  #   Scalar weighted mean.

  early.average <- mean(x[1:(length(x) - 1)])
  return(weighted.mean(c(early.average, x[length(x)]), w = c(0.65, 0.35)))
}

TestLastAssessed35Mean <- function() {
  stopifnot(LastAssessed35Mean(c(0, 1)) == 0.35)
  stopifnot(LastAssessed35Mean(c(0, 0, 1)) == 0.35)
  stopifnot(LastAssessed35Mean(c(1, 2, 3)) == 2.025)
}
TestLastAssessed35Mean()


AssignmentArray <- function(average.outcomes, num.assignments, per.assignment.sd) {
  # Generate a 3 dimensional array of continuous assignment scores given a set of average outcomes.
  #
  # Args:
  #   average.outcomes: a 2-dimensional array of dimensions num.sims by num.outcomes
  #   num.assignments: Number of assignments used to score each outcome.
  #   per.assignment.sd: The between-assignment Normal variability for a single outcome
  #
  # Returns:
  #   A 3-dimensional array of dimension {num.sims, num.outcomes, num.assignments}

  stopifnot(length(num.assignments) == 1)
  stopifnot(length(per.assignment.sd) == 1)

  mean.array <- replicate(num.assignments, average.outcomes)
  assignments <- mean.array + rnorm(length(mean.array), mean = 0, sd = per.assignment.sd)
  dimnames(assignments) <- list(sim = NULL, num.outcomes = NULL, assignment = NULL)
  return(assignments)
}

TestAssignmentArray <- function() {
  outcomes <- array(1, dim = c(3, 1), dimnames = list(sim = NULL, num.outcomes = NULL))
  x <- AssignmentArray(average.outcomes = outcomes, num.assignments = 2, per.assignment.sd = 0)
  stopifnot(all(dim(x) == c(3,1,2)))
  stopifnot(all(x == 1))
  stopifnot(class(x) == "array")

  outcomes <- array(1:(3*4), dim = c(3, 4), dimnames = list(sim = NULL, num.outcomes = NULL))
  x <- AssignmentArray(average.outcomes = outcomes, num.assignments = 2, per.assignment.sd = 0)
  stopifnot(length(unique(x)) == length(x)) 

  outcomes <- array(c(rep(1, 3), rep(10, 3)), dim = c(3, 2), dimnames = list(sim = NULL, num.outcomes = NULL))
  x <- AssignmentArray(average.outcomes = outcomes, num.assignments = 4, per.assignment.sd = 0.1)
  stopifnot(all(dim(x) == c(3,2,4)))
  stopifnot(length(unique(x)) == length(x)) 
  stopifnot(mean(x[1, 1,]) < 2)
  stopifnot(mean(x[1, 2,]) > 9)
}
TestAssignmentArray()

PrGradeASim <- function(num.sims, num.outcomes.grid, a.grid, b.grid, num.assignments.grid = NA, 
                        per.assignment.sd.grid = NA, assignment.scoring.fun = mean, 
                        return.first.raw.data = FALSE, ...) {
  # Perform a set of grading simulations.
  #
  # Args:  
  #   num.sims: Number of simulations.
  #   num.outcomes.grid: the number of outcomes in a class.
  #   a.grid: the mean of the Normal distribution on the average outcomes.
  #   b.grid: the standard deviation on the Normal distribution.
  #   num.assignments.grid: number of assignments used to assess a single outcome.
  #   per.assignment.sd.grid: within-outcome variability in the assignment scores.
  #   assignment.scoring.fun: The function used to score assignments in an outcome.
  #   return.first.raw.data: Return the first version of the raw data before estimating the actual grades.
  #   ...: passed to the grading function CBLGrade() & AverageGrade().
  #
  # Returns:
  #   If return.raw.data is TRUE, then a list of raw data.
  #   Otherwise, a data frame with all the simulation summaries.
  
  
  # This will hold the results of the simulation.
  res <- expand.grid(num.outcomes = num.outcomes.grid, a = a.grid, b = b.grid, num.assignments = num.assignments.grid,
                     per.assignment.sd = per.assignment.sd.grid, prob.A = as.numeric(NA))
  for(i in 1:nrow(res)) {
    num.outcomes <- res$num.outcomes[i]
    a <- res$a[i]
    b <- res$b[i]
    num.assignments <- res$num.assignments[i]
    per.assignment.sd <- res$per.assignment.sd[i]
    

    outcomes <- average.outcomes <- array(rnorm(num.outcomes * num.sims, a, b), dim = c(num.sims, num.outcomes), 
                                          dimnames = list(sim = NULL, num.outcomes = NULL))
    if (!is.na(num.assignments)) {
      # In this style of simulation we construct outcome averages as the average of a fixed number
      # of assessments, scored on a scale {1.0, 1.5, 2.0, 2.5, 3.0, 3.5, 4.0}
      assignments <- AssignmentArray(average.outcomes = average.outcomes, num.assignments = num.assignments,
                                     per.assignment.sd = per.assignment.sd)
      assignments <- RoundToHalfs(assignments)    
      outcomes <- apply(assignments, c(1,2), assignment.scoring.fun)
    }

    if (return.raw.data) {
      return(list(assignments = assignments, average.outcomes = average.outcomes, outcomes = outcomes))
    }
    
    grades <- apply(outcomes, 1, CBLGrade, ...)
    res$prob.A.CBL[i] <- mean(grades == "A")
    grades <- apply(outcomes, 1, AverageGrade, ...)
    res$prob.A.Avg[i] <- mean(grades == "A")
  }
  
  return(res)   
}

if(FALSE) {
  # This is testing code.0
  x <- PrGradeASim(num.sims=10, num.outcomes.grid = 2000, a.grid = 3.0, b.grid = 1.0, num.assignments.grid = 4, 
                 per.assignment.sd.grid = 5, return.first.raw.data = TRUE, assignment.scoring.fun = mean)
  cat("average outcomes\n")
  print(x$average.outcomes)
  cat("Assignments\n")
  print(x$assignments)
  cat("outcomes\n")
  print(x$outcomes)
  cat("The outcome-to-outcome stdev in each simulation\n")
  print(apply(x$outcomes, 1, sd))
}
