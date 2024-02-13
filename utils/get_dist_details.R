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
      "",
      "$$a < b, a \\in \\mathbb{R}, b \\in \\mathbb{R}$$",
      "$$x \\in [a, b]$$",
      "$$\\frac{1}{b-a}$$",
      "$$\\frac{x-a}{b-a} \\text{ for } x \\in [a,b]$$"
    )
  } else if (.dist == "Beta") {
    details$Details <- c(
      "",
      "$$\\alpha > 0, \\beta > 0$$",
      "$$x \\in (0, 1)$$",
      "$$\\frac{x^{\\alpha - 1}(1 - x)^{\\beta - 1}}{B(\\alpha, \\beta)}$$",
      "$$I_x(\\alpha, \\beta)$$"
    )
  } else if (.dist == "Binomial") {
    details$Details <- c(
      "",
      "$$n \\in \\{0,1,2,\\dots\\}, 0 \\leq p \\leq 1$$",
      "$$k \\in \\{0,1,2,\\dots,n\\}$$",
      "$$\\binom{n}{k} p^k (1-p)^{n-k}$$",
      "$$I_{1-p}(n-k, k+1) \\text{ or equivalent expressions}$$"
    )
  } else if (.dist == "Cauchy") {
    details$Details <- c(
      "",
      "$$x_0 \\in \\mathbb{R}, \\gamma > 0$$",
      "$$x \\in (-\\infty, +\\infty)$$",
      "$$\\frac{1}{\\pi \\gamma \\left[1 + \\left(\\frac{x - x_0}{\\gamma}\\right)^2\\right]}$$",
      "$$\\frac{1}{\\pi} \\arctan\\left(\\frac{x - x_0}{\\gamma}\\right) + \\frac{1}{2}$$"
    )
  } else if (.dist == "Chi-Square") {
    details$Details <- c(
      "",
      "$$k > 0, k \\in \\mathbb{N}$$",
      "$$x \\in (0, +\\infty)$$",
      "$$\\frac{1}{2^{k/2}\\Gamma(k/2)} x^{k/2 - 1}e^{-x/2}$$",
      "$$1 - \\frac{\\Gamma(k/2, x/2)}{\\Gamma(k/2)}$$"
    )
  } else if (.dist == "Exponential") {
    details$Details <- c(
      "",
      "$$\\lambda > 0$$",
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
      "",
      "$$0 < p < 1$$",
      "$$k \\in \\{0, 1, 2, \\dots\\}$$",
      "$$p(1-p)^k$$",
      "$$1 - (1-p)^{k+1}$$"
    )
  } else if (.dist == "Poisson") {
    details$Details <- c(
      "",
      "$$\\lambda > 0$$",
      "$$k \\in \\{0, 1, 2, \\dots\\}$$",
      "$$\\frac{e^{-\\lambda}\\lambda^k}{k!}$$",
      "$$\\frac{\\Gamma(k+1, \\lambda)}{k!}$$"
    )
  } 
  return(details)
}