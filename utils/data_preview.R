data.preview <- function(dt, .round, col.name="Dataset Preview") {
  datatable(
    dt[, .(y=round(y, .round))], 
    escape=F, 
    rownames=F, 
    selection="none", 
    filter='none',
    colnames=col.name,
    options=list(
      searching=F, 
      paging=T, 
      scrollX=F, 
      scrollY=F, 
      orderMulti=F, 
      info=T,
      dom='tip', 
      ordering=F, 
      pageLength=8, 
      lengthChange=F
    )
  ) %>%
    DT::formatStyle(1, "white-space"="nowrap") %>%
    DT::formatStyle(1, fontSize="15px") %>%
    DT::formatStyle(1, "cursor"="default")
}