library(shiny)
library(DT)
library(jpeg)
library(imager)

fluidPage(
  fluidRow(
    column(width = 4,
           fileInput("files", label = h4("Upload your jpeg image:"), multiple = FALSE, accept = "image/jpeg"),
           br(),
           radioButtons("obj", label = h4("Please select a object name:"),
                        c("person" = "person",
                          "face" = "face")),
           br(),
           downloadButton("download", label = "Download file", class = NULL)
    ),
    column(width = 7,
           plotOutput("plot", height = 416, width = 416,
                      dblclick = "plot_dblclick",
                      brush = brushOpts(id = "plot_brush", resetOnNew = TRUE)),
           br(),
           actionButton("delete", strong("Delete selected box!"), icon("list-alt")),
           br(),
           br(),
           DT::dataTableOutput('table')
    )
  )
)