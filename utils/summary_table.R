summary.table <- function(ss) {
  # `ss` is a `data.table` with the fields:
  # .mean, .variance, .stdev, .median, .min, .max
  ss.dt <- data.table::transpose(ss) %>%
    .[, .(stat=c("Sample Mean", "Sample Variance",
                 "Sample Standard Deviation", "Sample Median",
                 "Sample Min.", "Sample Max."),
          val=round(V1, 4))]

  datatable(
    ss.dt,
    escape=F, 
    rownames=F, 
    colnames=c("", ""),
    selection="none", 
    filter='none',
    options=list(
      searching=F, 
      paging=F, 
      scrollX=F, 
      scrollY=F, 
      orderMulti=F, 
      info=F,
      ordering=F, 
      lengthChange=F
    )
  ) %>%
    DT::formatStyle(1:2, "white-space"="nowrap") %>%
    DT::formatStyle(1:2, fontSize="15px") %>%
    DT::formatStyle(1:2, "cursor"="default")
}
