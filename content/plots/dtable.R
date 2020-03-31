
plotDataTable <- function(){
  datatable( finalUptodate,
             class = 'cell-border stripe',
             editable = TRUE,
             options = list(scrollX = T)
  ) 
  
}




output$data.table<-renderDataTable({
  plotDataTable()
  
})
