InteractiveDT <- function(link_gs, sheet, dropdownCol, input, output, session) {
    library(shiny)
    library(DT)

    resultDF <- displayHTMLDF <- initHTMLDF <- initData <-
        read_sheet(link_gs, sheet = sheet)

    Statuts_Orders <- read_sheet(link_gs, sheet = 'mapping') |>
        pull(dropdownCol)

    # Set up IDS if several dropDownColumns
    dropdownIDs <- setNames(lapply(dropdownCol, function(x) {paste0(x, seq_len(nrow(initData)))}), dropdownCol)
    colDropdownIDs <- dropdownIDs[[dropdownCol]]
    initHTMLDF[[dropdownCol]] <- sapply(seq_along(colDropdownIDs), function(i) {
        as.character(selectInput(inputId = colDropdownIDs[i], label = "", choices = Statuts_Orders, selected = initData$Statut[i]))
    })

    reactiveHTMLDF <- reactive({
        colDropdownIDs <- dropdownIDs[[dropdownCol]]
        displayHTMLDF[[dropdownCol]] <- sapply(seq_along(colDropdownIDs), function(i) {
            as.character(selectInput(colDropdownIDs[i], label = "", choices = Statuts_Orders, selected = input[[colDropdownIDs[i]]]))
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

    output$CustomerOrders_DT <- DT::renderDataTable({
        DT::datatable(
            initHTMLDF, escape = FALSE, selection = 'none', rownames = FALSE,
            options = list(paging = FALSE, ordering = FALSE, scrollx = TRUE, dom = "t",
                           preDrawCallback = JS('function() { Shiny.unbindAll(this.api().table().node()); }'),
                           drawCallback = JS('function() { Shiny.bindAll(this.api().table().node()); } ')
            )
        )
    })

    CustomerOrders_proxy <- dataTableProxy("CustomerOrders_DT", session)

    observeEvent({sapply(unlist(dropdownIDs), function(x) {input[[x]]})}, {
        replaceData(proxy = CustomerOrders_proxy, data = reactiveHTMLDF(), rownames = FALSE)
    }, ignoreInit = TRUE)
    shiny::observeEvent(input$saveBtn, {
        googlesheets4::sheet_write(data = reactiveResultDF(), ss = link_gs, sheet = "commandes_clients")
    })

}
