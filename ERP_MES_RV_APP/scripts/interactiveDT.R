### - Code tiré https://stackoverflow.com/questions/75948475/value-not-updated-in-shiny-using-dt-and-drop-down-selection/75950123#75950123
InteractiveDT <- function(link_gs, sheet, mapping_sheet = "mapping", id = "CustomerOrders", dropdownCol, input, output, session) {
    require(shiny)
    require(DT)
    require(googlesheets4)

    resultDF <- displayHTMLDF <- initHTMLDF <- initData <-
        read_sheet(link_gs, sheet = sheet)

    Statuts_Orders <- read_sheet(link_gs, sheet = mapping_sheet) |>
        pull(dropdownCol)

    # Set up IDS if several dropDownColumns
    dropdownIDs <- setNames(lapply(dropdownCol, function(x) {paste0(x, seq_len(nrow(initData)))}), dropdownCol)
    colDropdownIDs <- dropdownIDs[[dropdownCol]]
    initHTMLDF[[dropdownCol]] <- sapply(seq_along(colDropdownIDs), function(i) {
        as.character(selectInput(inputId = colDropdownIDs[i], label = "", choices = Statuts_Orders, selected = initData$Statut[i]))
    })
    print(isolate(colDropdownIDs))

    reactiveHTMLDF <- reactive({
        colDropdownIDs <- dropdownIDs[[dropdownCol]]
        displayHTMLDF[[dropdownCol]] <- sapply(seq_along(colDropdownIDs), function(i) {
            as.character(selectInput(inputId = colDropdownIDs[i], label = "", choices = Statuts_Orders, selected = input[[colDropdownIDs[i]]]))
        })
        return(displayHTMLDF)
    })

    reactiveResultDF <- reactive({
        colDropdownIDs <- dropdownIDs[[dropdownCol]]
        resultDF[[dropdownCol]] <- sapply(seq_along(colDropdownIDs), function(i) {
            input[[colDropdownIDs[i]]]
        })
        return(resultDF)
    })

    output[[paste0(id, "_DT")]] <- DT::renderDataTable({
        DT::datatable(
            initHTMLDF, escape = FALSE, selection = 'none', rownames = FALSE, filter = 'top',
            options = list(paging = FALSE, ordering = FALSE, scrollx = TRUE, dom = 't',
                           preDrawCallback = JS('function() { Shiny.unbindAll(this.api().table().node()); }'),
                           drawCallback = JS('function() { Shiny.bindAll(this.api().table().node()); } ')
            )
        )
    })

    return(list(reactiveHTMLDF = reactiveHTMLDF, reactiveResultDF = reactiveResultDF))
}
