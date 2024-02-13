# https://stackoverflow.com/questions/71207531/latex-in-dtdatatable
render.KaTeX <- paste(
  "function(data, type, row, meta){",
  "  if(type === 'display'){",
  "    data = katex.renderToString(data);",
  "  }",
  "  return data;",
  "}", sep="\n"
)

