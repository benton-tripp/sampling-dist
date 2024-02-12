get.formula <- function(p) {
  if (p$distribution[[1]] == "Normal") {
    return(sprintf("$$f(y) \\sim N(\\mu, \\sigma),\\; where\\; \\mu=%s\\; and\\; \\sigma=%s$$",
                   p$value[p$parameter == "Mean"], 
                   p$value[p$parameter == "Standard Dev."]))
  } else if (p$distribution[[1]] == "Uniform") {
    return(sprintf("$$f(y) \\sim Uniform(a, b),\\; where\\; a=%s\\; and\\; b=%s$$",
                   p$value[p$parameter == "Minimum"], 
                   p$value[p$parameter == "Maximum"]))
  } else if (p$distribution[[1]] == "Beta") {
      return(sprintf("$$f(y) \\sim Beta(\\alpha, \\beta),\\; where\\; \\alpha=%s\\; and\\; \\beta=%s$$",
                     p$value[p$parameter == "Alpha"], 
                     p$value[p$parameter == "Beta"]))
    } else if (p$distribution[[1]] == "Binomial") {
      return(sprintf("$$f(y) \\sim Binomial(n, p),\\; where\\; n=%s\\; and\\; p=%s$$",
                     p$value[p$parameter == "Size"], 
                     p$value[p$parameter == "Probability of Success"]))
    } else if (p$distribution[[1]] == "Cauchy") {
      return(sprintf("$$f(y) \\sim Cauchy(location, scale),\\; where\\; location=%s\\; and\\; scale=%s$$",
                     p$value[p$parameter == "Location"], 
                     p$value[p$parameter == "Scale"]))
    } else if (p$distribution[[1]] == "Chi-Square") {
      return(sprintf("$$f(y) \\sim \\chi^2(df),\\; where\\; df=%s$$",
                     p$value[p$parameter == "DF"]))
    } else if (p$distribution[[1]] == "Exponential") {
      return(sprintf("$$f(y) \\sim Exp(rate),\\; where\\; rate=%s$$",
                     p$value[p$parameter == "Rate"]))
    } else if (p$distribution[[1]] == "Gamma") {
      return(sprintf("$$f(y) \\sim Gamma(shape, scale),\\; where\\; shape=%s\\; and\\; scale=%s$$",
                     p$value[p$parameter == "Shape"], 
                     p$value[p$parameter == "Scale"]))
    } else if (p$distribution[[1]] == "Geometric") {
      return(sprintf("$$f(y) \\sim Geometric(p),\\; where\\; p=%s$$",
                     p$value[p$parameter == "Probability of Success"]))
    } else if (p$distribution[[1]] == "Poisson") {
      return(sprintf("$$f(y) \\sim Poisson(\\lambda),\\; where\\; \\lambda=%s$$",
                     p$value[p$parameter == "Rate"]))
    } else {
      return("")
    }
  }