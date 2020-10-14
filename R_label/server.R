library(shiny)
library(DT)
library(jpeg)
library(imager)


Show_img = function (img, box_info = NULL, col_bbox = '#FFFFFF00') {
  
  obj_name <- c("person" = "person",
                "face" = "face")
  
  col_label <- c("#FF0000",
                 "#00FF00")
  
  par(mar = rep(0, 4))
  plot(NA, xlim = c(0, 1), ylim = c(1, 0), xaxt = "n", yaxt = "n", bty = "n")
  img = (img - min(img))/(max(img) - min(img))
  img = as.raster(img)
  rasterImage(img, 0, 1, 1, 0, interpolate=FALSE)
  
  select_col <- cbind(obj_name , col_label)
  rownames(select_col) <- 1:nrow(select_col)
  
  
  if (!is.null(box_info)) {
    bbox_info <- c("")
    bbox_info <- merge(box_info, select_col, by="obj_name", all.x = F)
    if (nrow(bbox_info) > 0) {
      for (i in 1:nrow(bbox_info)) {
        size = max(bbox_info[i,3] - bbox_info[i,2], 0.2)
        rect(xleft = bbox_info[i,2], xright = bbox_info[i,2] + 0.06*sqrt(size)*nchar(bbox_info[i,1]),
             ybottom = bbox_info[i,5] + 0.08*sqrt(size), ytop = bbox_info[i,5],
             col = bbox_info[i, "col_label"], border = bbox_info[i, "col_label"], lwd = 0)
        text(x = bbox_info[i,2] + 0.03*sqrt(size) * nchar(bbox_info[i,1]),
             y = bbox_info[i,5] + 0.04*sqrt(size),
             labels = bbox_info[i,1],
             col = 'white', cex = 1.5*sqrt(size), font = 2)
        rect(xleft = bbox_info[i,2], xright = bbox_info[i,3],
             ybottom = bbox_info[i,4], ytop = bbox_info[i,5],
             col = col_bbox, border = bbox_info[i, "col_label"], lwd = 5*sqrt(size))
      }
    }
  }
  
}

shinyServer(function(input, output) {
  
  IMAGE = reactive({
    if (is.null(input$files)) {return()} else {
      img = readJPEG(input$files$datapath)
      return(img) 
    }
  })
  
  MY_TABLE = reactiveValues(table = NULL)
  output$plot = renderPlot({
    img = IMAGE()
    if (!is.null(input$files$name)) {
      box_info = MY_TABLE$table
      box_info = box_info[box_info[,"img_id"] == input$files$name,]
    } else {
      box_info = NULL
    }
    if (is.null(img)) {return()} else {
      if(is.null(box_info)) {
        Show_img(img = img, box_info = box_info)
      } else{
        Show_img(img = img, box_info = box_info)
      }
    }
  })
  
  
  observeEvent(input$plot_dblclick, {
    brush = input$plot_brush
    if (!is.null(brush) & !is.null(input$files$name)) {
      new_table = data.frame(obj_name = input$obj,
                             col_left = brush$xmin,
                             col_right = brush$xmax,
                             row_bot = brush$ymax,
                             row_top = brush$ymin,
                             prob = 1,
                             img_id = input$files$name,
                             stringsAsFactors = FALSE)
      MY_TABLE$table = rbind(MY_TABLE$table, new_table)
    }
  })
  
  observeEvent(input$delete, {
    selection = as.numeric(input$table_rows_selected)
    if (length(selection)!=0) {
      MY_TABLE$table = MY_TABLE$table[-selection,]
    }
  })
  
  output$table = DT::renderDataTable({
    dat = MY_TABLE$table
    if (is.null(dat)) {return()} else {
      dat[,2] = round(dat[,2], 3)
      dat[,3] = round(dat[,3], 3)
      dat[,4] = round(dat[,4], 3)
      dat[,5] = round(dat[,5], 3)
      rownames(dat) <- 1:nrow(dat)
      Result = DT::datatable(dat)
      return(Result)
    }
  })
  
  output$download = downloadHandler(
    filename = function() {'label.csv'},
    content = function(con) {
      dat = MY_TABLE$table
      if (is.null(dat)) {return()} else {
        write.csv(dat, con, row.names = FALSE)
      }
    }
  )
  
  
})