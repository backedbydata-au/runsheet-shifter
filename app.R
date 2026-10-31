library(shiny)
library(xml2)

# ---- Helper: read XML and shift all <start> and <end> values ----------------
shift_xml <- function(path, offset_seconds) {
  doc   <- read_xml(path)
  nodes <- xml_find_all(doc, "//start | //end")

  old <- suppressWarnings(as.numeric(xml_text(nodes)))
  new <- old + offset_seconds

  for (i in seq_along(nodes)) {
    if (!is.na(old[i])) {
      xml_set_text(nodes[[i]], sprintf("%.2f", new[i]))
    }
  }

  list(doc = doc, tag = xml_name(nodes), old = old, new = new)
}

# ---- UI ---------------------------------------------------------------------
ui <- fluidPage(
  titlePanel("XML Time Shifter"),

  sidebarLayout(
    sidebarPanel(
      fileInput(
        "xml_file", "Upload XML file",
        accept = c(".xml", "text/xml", "application/xml")
      ),

      h5("Time to add"),
      fluidRow(
        column(6, numericInput("minutes", "Minutes", value = 0, min = 0, step = 1)),
        column(6, numericInput("seconds", "Seconds", value = 0, min = 0, step = 0.01))
      ),

      textInput("out_name", "Output file name", value = "shifted.xml"),
      downloadButton("download", "Download updated XML")
    ),

    mainPanel(
      uiOutput("summary"),
      h4("Preview (first 10 values)"),
      tableOutput("preview")
    )
  )
)

# ---- Server -----------------------------------------------------------------
server <- function(input, output, session) {

  # Total offset in seconds
  offset <- reactive({
    req(is.numeric(input$minutes), is.numeric(input$seconds))
    (input$minutes * 60) + input$seconds
  })

  # Suggest an output name when a file is uploaded
  observeEvent(input$xml_file, {
    base <- tools::file_path_sans_ext(input$xml_file$name)
    updateTextInput(session, "out_name", value = paste0(base, "_shifted.xml"))
  })

  # Shifted XML (re-runs whenever the file or time changes)
  shifted <- reactive({
    req(input$xml_file)
    tryCatch(
      shift_xml(input$xml_file$datapath, offset()),
      error = function(e) {
        validate(need(FALSE, paste("Could not process XML:", conditionMessage(e))))
      }
    )
  })

  output$summary <- renderUI({
    if (is.null(input$xml_file)) {
      return(p("Upload an XML file to get started."))
    }
    res <- shifted()
    n_ok <- sum(!is.na(res$old))
    n_skipped <- sum(is.na(res$old))

    mins <- floor(offset() / 60)
    secs <- offset() - mins * 60

    tagList(
      p(strong(n_ok), "start/end values will be shifted by",
        strong(sprintf("%d min %.2f sec", mins, secs)),
        sprintf("(%.2f seconds total).", offset())),
      if (n_skipped > 0) {
        p(style = "color:#b36b00;",
          n_skipped, "start/end node(s) had non-numeric values and were left unchanged.")
      }
    )
  })

  output$preview <- renderTable({
    res <- shifted()
    n <- min(10, length(res$old))
    validate(need(n > 0, "No <start> or <end> nodes found in this file."))
    data.frame(
      Node     = res$tag[seq_len(n)],
      Original = sprintf("%.2f", res$old[seq_len(n)]),
      Updated  = sprintf("%.2f", res$new[seq_len(n)])
    )
  })

  output$download <- downloadHandler(
    filename = function() {
      nm <- trimws(input$out_name)
      if (!nzchar(nm)) nm <- "shifted.xml"
      if (!grepl("\\.xml$", nm, ignore.case = TRUE)) nm <- paste0(nm, ".xml")
      nm
    },
    content = function(file) {
      write_xml(shifted()$doc, file)
    }
  )
}

shinyApp(ui, server)
