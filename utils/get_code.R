get.code <- function(p) {
  if (p$distribution[[1]] == "Normal") {
    return(sprintf("rnorm(n=100, mean=%s, sd=%s)", 
                   p$value[p$parameter == "Mean"], 
                   p$value[p$parameter == "Standard Dev."]))
  } else if (p$distribution[[1]] == "Uniform") {
    return(sprintf("runif(n=100, min=%s, max=%s)", 
                   p$value[p$parameter == "Minimum"], 
                   p$value[p$parameter == "Maximum"]))
  } else if (p$distribution[[1]] == "Beta") {
    return(sprintf("rbeta(n=100, shape1=%s, shape2=%s)", 
                   p$value[p$parameter == "Alpha"], 
                   p$value[p$parameter == "Beta"]))
  } else if (p$distribution[[1]] == "Binomial") {
    return(sprintf("rbinom(n=100, size=%s, prob=%s)", 
                   p$value[p$parameter == "Size"], 
                   p$value[p$parameter == "Probability of Success"]))
  } else if (p$distribution[[1]] == "Cauchy") {
    return(sprintf("rcauchy(n=100, location=%s, scale=%s)", 
                   p$value[p$parameter == "Location"], 
                   p$value[p$parameter == "Scale"]))
  } else if (p$distribution[[1]] == "Chi-Square") {
    return(sprintf("rchisq(n=100, df=%s)", 
                   p$value[p$parameter == "DF"]))
  } else if (p$distribution[[1]] == "Exponential") {
    return(sprintf("rexp(n=100, rate=%s)", 
                   p$value[p$parameter == "Rate"]))
  } else if (p$distribution[[1]] == "Gamma") {
    return(sprintf("rgamma(n=100, shape=%s, scale=%s)", 
                   p$value[p$parameter == "Shape"], 
                   p$value[p$parameter == "Scale"]))
  } else if (p$distribution[[1]] == "Geometric") {
    return(sprintf("rgeom(n=100, prob=%s)", 
                   p$value[p$parameter == "Probability of Success"]))
  } else if (p$distribution[[1]] == "Poisson") {
    return(sprintf("rpois(n=100, lambda=%s)", 
                   p$value[p$parameter == "Rate"]))
  } else {
    return("")
  }
}