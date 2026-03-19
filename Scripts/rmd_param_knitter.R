smart_knitter <- function(inputFile,
                            out_path = NULL,
                            launch_in_browser = FALSE,
                            auto_close = TRUE) {
  pm <- knitr::knit_params(readLines(inputFile))
  
  pre_knit_time <- Sys.time()
  
  shinyArgs <- function(param, ns) {
    param$inputId <- ns(param$name)
    if (is.null(param$label))
      param$label <- param$name
    if (!is.null(param$input) &&
        param$input %in% c("select", "radio")) {
      param$selected <- param$value
      param$value <- NULL
    }
    param$name <- NULL
    param$input <- NULL
    param
  }
  
  getInputFun <- function(x) {
    if (is.null(x))
      return(shiny::textInput)
    if (x == "radio")
      return(shiny::radioButtons)
    get(paste0(x, "Input"), asNamespace("shiny"))
  }
  
  paramsUI <- function(id) {
    ns <- shiny::NS(id)
    shiny::tagList(lapply(names(pm), function(n) {
      p <- pm[[n]]
      do.call(getInputFun(p$input), shinyArgs(p, ns))
    }))
  }
  
  getParams <- function(values) {
    setNames(lapply(names(values), function(n) {
      if (!is.null(pm[[n]]$input) && pm[[n]]$input == "file") {
        v <- values[[n]]
        if (is.null(v))
          pm[[n]]$value
        else
          v$datapath
      } else
        values[[n]]
    }), names(values))
  }
  
  app <- shiny::shinyApp(
    ui = shiny::fluidPage(
      shiny::titlePanel("Analysis Configuration"),
      shiny::fluidRow(
        shiny::column(8, paramsUI("p")),
        shiny::column(
          4,
          shiny::actionButton("go", "Render"),
          shiny::actionButton("exit", "Exit"),
          shiny::verbatimTextOutput("status")
        )
      )
    ),
    
    server = function(input, output, session) {
      params_out <- shiny::callModule(function(input, output, session) {
        shiny::reactive(getParams(shiny::reactiveValuesToList(input)))
      }, "p")
      
      output$status <- shiny::renderText("No report yet.")
      
      shiny::observeEvent(input$go, {
        params <- params_out()
        
        out <- if (is.null(out_path) || identical(out_path, "")) {
          here::here(paste0(tools::file_path_sans_ext(basename(inputFile)), ".html"))
        } else if (is.function(out_path)) {
          out_path(params, inputFile)
        } else if (grepl("\\.html$", out_path, ignore.case = TRUE)) {
          dir.create(dirname(out_path),
                     recursive = TRUE,
                     showWarnings = FALSE)
          out_path
        } else {
          dir.create(out_path,
                     recursive = TRUE,
                     showWarnings = FALSE)
          file.path(out_path,
                    paste0(tools::file_path_sans_ext(basename(inputFile)), ".html"))
        }
        
        output$status <- shiny::renderText("Rendering...")
        
        rmarkdown::render(
          input = inputFile,
          params = params,
          output_file = out,
          envir = new.env(parent = globalenv())
        )
        
        end_knit_time <- Sys.time()
        runtime <- round(end_knit_time - pre_knit_time, 2)

        if (isTRUE(auto_close)) {
          start_time <- Sys.time()
          output$status <- shiny::renderText({
            invalidateLater(1000, session)  # re-run every 1 second
            
            elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
            
            if (elapsed >= 5) {
              shiny::stopApp()
            }
            
            paste0(
              "Rendered in ", runtime, " ", attr(runtime, "units"), 
              " and saved to:\n", out, "\nApp will close in ", 
              as.character(round(5 - elapsed)), " seconds..."
            )
          })
        } else {
          output$status <- renderText(paste0("Rendered in ", 
                                             runtime, " ", 
                                             attr(runtime, "units"), 
                                             " and saved to:\n", out))
        }
      })
      
      shiny::observeEvent(input$exit, {
        shiny::stopApp()
      })
    }
  )
  
  shiny::runApp(app, launch.browser = if (isTRUE(launch_in_browser))
    TRUE
    else
      rstudioapi::viewer)
}