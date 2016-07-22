status <- function(status, style = "info") {
  as.character(tags$div(class = paste0("alert alert-", style), style = "margin: 20px 20px 0;",
                        tags$strong("Status: "), status
  ))
}

textAreaInput <- function (inputId, label, value = "", rows = 3, placeholder = NULL)
{
  tags$div(class = "form-group shiny-input-container", style = "width: 100%;",
           tags$label(`for` = inputId, label),
           tags$textarea(id = inputId, rows = rows, width = "100%", class = "form-control")
  )
}

column <- function(width, ..., offset = 0) {

  shiny::column(width, ..., offset = offset, style = "padding: 0px;")

}

panel <- function(..., title, style = "default") {

  tags$div(class = paste0("panel panel-", style),
           style = "margin: 10px;",
           tags$div(class = "panel-heading",
                    tags$h3(class = "panel-title", title)
           ),
           tags$div(class = "panel-body", style = "padding-bottom: 0;",
                    ...
           )
  )
}

miniColumn <- function(width, ..., offset = 0) {
  if (!is.numeric(width) || (width < 1) || (width > 12))
    stop("column width must be between 1 and 12")
  colClass <- paste0("col-sm-", width)
  if (offset > 0)
    colClass <- paste0(colClass, " col-sm-offset-", offset)
  div(class = colClass, ..., style = "padding: 0;")
}

horizontalForm <- function(..., input_width = 10) {
  label_width <- 12 - input_width
  form_elements <- list(...)
  form_elements <- lapply(form_elements, function(tag) {
    if("class" %in% names(tag$attribs)) {
      if(grepl("shiny-input-container", tag$attribs$class, fixed = TRUE)) {
        x <- try({

          label <- tag$children[[1]]
          if(label$name == "label") {
            label$attribs$class <- paste0(label$attribs$class, " control-label col-sm-", label_width)
          }

          ip <- tag$children[[2]]
          ip$attribs$style <- paste0("width: 100%; ", ip$attribs$style)
          ip <- tags$div(class = paste0("col-sm-", input_width), ip)
        })
        if(!"try-error" %in% class(x)) {
          tag <- tags$div(class = "form-group",
            label,
            ip
          )
        } else {
          tag <- tags$div(class = "form-group",
            tags$div(class = paste0("col-sm-offset-", label_width, " col-sm-", input_width),
              tag
            )
          )
        }
      }
    }
    return(tag)
  })
  tags$form(class = "form-horizontal", tagList(form_elements))
}

horizontalSelectInput <- function(id, label, choices, selected = NULL,
                                  multiple = FALSE, selectize = TRUE,
                                  width = 10, size = NULL) {

  x <- selectInput(id, label, choices, selected, multiple, selectize, width = "100%", size)
  label_width <- 12 - width

  label <- x$children[[1]]
  label$attribs$class <- paste0(label$attribs$class, " col-sm-", label_width)

  select <- x$children[[2]]
  select$attribs$class <- paste0(select$attribs$class, " col-sm-", width)

  tags$form(class = "form-horizontal",
            tags$div(class = "form-group",
                     label,
                     select
            )
  )

}

passwordInput <- function (inputId, label, value = "", width = NULL, placeholder = NULL) {

  x <- textInput(inputId, label, value, width, placeholder)

  x$children[[2]]$attribs$type = "password"

  return(x)

}
