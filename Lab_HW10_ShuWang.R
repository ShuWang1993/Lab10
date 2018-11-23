# Question 1:
CumBinomial <- function (x, n, pi) {
  sum <- 0
  for (i in 1 : x) {
    sum <- sum + dbinom(i, n, pi)
  }
  print(sum)
}

result <- c("function" = CumBinomial(4, 10, 0.4), "pbinom()" = pbinom(4, 10, 0.4))
# It can be found that the two results are pretty similar. They don't differ so much.


# Question 2:
TestPower <- function (n, delta, sd, sig.level) {
  reps <- 10000
  counter <- 0
  
  for (i in seq_len(reps)) {
    sample <- rnorm(n, delta, sd)
    result <- t.test(sample, mu=0, conf.level = (1 - sig.level))
    p_value <- result$p.value
    
    if (p_value < sig.level) {
      counter <- counter + 1
    }
  }
  
  return(counter / reps)
}

TestPower(30, 0.5, 1, 0.05)

power.t.test(n = 30, delta = 0.5, sd = 1, sig.level = 0.05, type = 'one.sample')

# It can be found that the results are approximately similar.

