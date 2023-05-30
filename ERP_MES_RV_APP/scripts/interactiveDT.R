### - Code tiré https://stackoverflow.com/questions/75948475/value-not-updated-in-shiny-using-dt-and-drop-down-selection/75950123#75950123
outputDT <- function(dt) {
  DT::renderDataTable({
    DT::datatable(
      dt, escape = FALSE, selection = 'none', rownames = FALSE, filter = 'top',
      options = list(paging = FALSE, ordering = FALSE, scrollx = TRUE, dom = 't',
                     preDrawCallback = JS('function() { Shiny.unbindAll(this.api().table().node()); }'),
                     drawCallback = JS('function() { Shiny.bindAll(this.api().table().node()); } ')
      )
    )
  })
}
