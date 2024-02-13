get.dist.details <- function(.dist) {
  details <- data.table(
    Aspect = c("Notation", "Parameters", "Support", "PDF", "CDF"),
    Details = rep("", 4)
  )
  if (.dist == "Normal") {
    details$Details <- c(
      "$$\\mathcal{N} \\left(\\mu ,\\sigma^{2}\\right)$$",
      paste0("$$\\mu \\in \\mathbb{R} = \\text{mean (location), }$$ ", 
             "$$\\sigma \\in \\mathbb{R}_{\\gt 0} = \\text{variance (squared scale)}$$"),
      "$$x \\in \\mathbb{R}$$",
      paste0("$$\\frac{1}{\\sigma\\sqrt{2\\pi}} e^{-\\frac{1}{2}", 
      "\\left(\\frac{x-\\mu}{\\sigma}\\right)^2}$$"),
      paste0("$$\\phi \\left(\\frac{x-\\mu}{\\sigma}\\right) = ", 
             "\\frac{1}{2} \\left[1 + \\text{erf}\\", 
             "left(\\frac{x-\\mu}{\\sigma\\sqrt{2}}\\right)\\right]$$")
    )
  } else if (.dist == "Uniform") {
    details$Details <- c(
      "$$\\mathcal{U}_{[a,b]}$$",
      "$$-\\infty < a < b < \\infty$$",
      "$$x \\in [a, b]$$",
      paste0("$$\\begin{cases}{\\frac {1}{b-a}}&{\\text{for }}x\\in [a,b]\\\\",
             "0&{\\text{otherwise}}\\end{cases}$$"),
      paste0("$$\\begin{cases}0&{\\text{for }}x < a\\\\", 
             "{\\frac {x-a}{b-a}}&{\\text{for }}x\\in [a,b]\\\\",
             "1&{\\text{for }}x > b \\end{cases}$$")
    )
  } else if (.dist == "Beta") {
    details$Details <- c(
      "$$\\text{Beta}\\left(\\alpha,\\beta\\right)$$",
      "$$\\alpha > 0 \\text{ (shape),}\\\\ \\beta > 0 \\text{ (shape)}$$",
      "$$x \\in (0, 1)$$",
      paste0("$$\\frac{x^{\\alpha - 1}(1 - x)^{\\beta - 1}}{B(\\alpha, \\beta)}\\\\", 
             "\\text{where }B(\\alpha, \\beta) = ", 
             "\\frac{\\Gamma(\\alpha)\\Gamma(\\beta)}{\\Gamma(\\alpha+\\beta)},\\\\", 
             "\\text{and } \\Gamma \\text{ is the Gamma function }", 
             "\\Gamma(n)=(n-1)!$$"),
      paste0("$$I_x(\\alpha, \\beta) = ",
             "\\frac{\\mathrm{B}(x;\\;\\alpha,\\beta)}{\\mathrm{B}(\\alpha,\\beta)}",
             "\\\\ \\text{(regularized incomplete beta function)}$$")
    )
  } else if (.dist == "Binomial") {
    details$Details <- c(
      "$$B(n,p)$$",
      paste0("$$n \\in \\{0,1,2,\\dots\\} \\text{ (number of trials)},\\\\",
             "p \\in \\[0,1\\] \\text{ (probability of success for each trial)}$$"),
      "$$k \\in \\{0,1,2,\\dots,n\\} \\text{ (number of successes)}$$",
      "$$\\binom{n}{k} p^k (1-p)^{n-k}$$",
      "$$I_{1-p}(n-k, k+1) \\text{ (regularized incomplete beta function)}$$"
    )
  } else if (.dist == "Cauchy") {
    details$Details <- c(
      "$$Cauchy(x_0, \\gamma)$$",
      "$$x_0\\in\\mathbb{R}\\text{ (location)},\\\\ \\gamma>0\\text{ (scale)}$$",
      "$$x \\in (-\\infty, +\\infty)$$",
      "$$\\frac{1}{\\pi \\gamma \\left[1 + \\left(\\frac{x - x_0}{\\gamma}\\right)^2\\right]}$$",
      "$$\\frac{1}{\\pi} \\arctan\\left(\\frac{x - x_0}{\\gamma}\\right) + \\frac{1}{2}$$"
    )
  } else if (.dist == "Chi-Square") {
    details$Details <- c(
      "$$\\chi^{2}(k)\\text{ or }\\chi_{k}^{2}$$",
      "$$k > 0, k \\in \\mathbb{N} \\text{ (known as 'degrees of freedom')}$$",
      "$$x\\in(0,\\infty)\\text{ if }k=1\\text{, otherwise }x\\in[0,\\infty)$$",
      "$$\\frac{1}{2^{k/2}\\Gamma(k/2)} x^{k/2 - 1}e^{-x/2}$$",
      paste0("$$\\frac{1}{\\Gamma(k/2)}\\gamma",
             "\\left(\\frac{k}{2},\\frac{x}{2}\\right),\\\\",
             "\\text{where }\\gamma(s,t)",
             "\\text{ is the lower incomplete gamma function}$$")
    )
  } else if (.dist == "Exponential") {
    details$Details <- c(
      "$$Exp(\\lambda)$$",
      "$$\\lambda > 0\\text{, where }\\lambda\\text{ is the rate, or inverse scale}$$",
      "$$x \\in [0, \\infty)$$",
      "$$\\lambda e^{-\\lambda x}$$",
      "$$1 - e^{-\\lambda x}$$"
    )
  } else if (.dist == "Gamma") {
    details$Details <- c(
      "$$\\Gamma\\left(k, \\theta \\right)$$",
      "$$k > 0 \\text{ (shape),}$$ $$\\theta > 0 \\text{ (scale)}$$",
      "$$x \\in \\left(0, +\\infty\\right)$$",
      "$$f(x)={\\frac {1}{\\Gamma (k)\\theta ^{k}}}x^{k-1}e^{-x/\\theta }$$",
      "$$F(x)={\\frac {1}{\\Gamma (k)}}\\gamma \\left(k,{\\frac {x}{\\theta }}\\right)$$"
    )
  } else if (.dist == "Geometric") {
    details$Details <- c(
      "$$G(p)$$",
      "$$0 < p \\le 1 \\text{ (probability of success)}$$",
      "$$k \\in \\{0, 1, 2, \\dots\\}$$",
      "$$p(1-p)^k$$",
      "$$1 - (1-p)^{k+1}\\text{ for }k\\ge0,\\;0\\text{ for }k<0$$"
    )
  } else if (.dist == "Poisson") {
    details$Details <- c(
      "$$Pois(\\lambda)$$",
      "$$\\lambda \\in (0,\\infty)\\text{ (rate)}$$",
      "$$k \\in \\{0, 1, 2, \\dots\\}$$",
      "$$\\frac{e^{-\\lambda}\\lambda^k}{k!}$$",
      paste0("$$\\frac{\\Gamma(k+1, \\lambda)}{k!}\\\\",
             "\\text{where }\\Gamma(x,y)\\text{ is the upper incomplete gamma function}$$")
    )
  } 
  return(details)
}