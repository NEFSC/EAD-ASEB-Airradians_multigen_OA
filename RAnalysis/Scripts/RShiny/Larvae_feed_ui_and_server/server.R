library(shiny)

server <- shinyServer(function(input, output,session){

            
            tableValues <- reactiveValues(df = data.frame(Date = as.Date(as.character()),
                                                         `Number of tanks` = as.numeric(), 
                                                         `Tank volume` = as.numeric(),
                                                         `Target cells density (cells ml-1)` = as.numeric(),
                                                         `Volume algae #1 (total L)` = as.numeric(),
                                                         `Volume algae #2 (total L)` = as.numeric(),
                                                         `Volume algae #3 (total L)` = as.numeric(),
                                                         `Master mix (L)` = as.numeric(),
                                                         `FEED PER TANK (L)` = as.numeric(),
                                                         check.names = FALSE))
                                          
             
            
            
            
            
            observeEvent(input$submit, {
              
                          temp <- tableValues$m
                          
                          newRow <- data.frame(Date = input$date,
                                               `Number of tanks` = input$how_many_tanks, 
                                               `Tank volume (L)` =  input$tank_volume, 
                                               `Target cells density (cells ml-1)` = input$target_cell_density,
                                               `Volume algae #1 (total L)` = ( ( ( ((input$tank_volume*1000)*(input$target_cell_density)) *(input$perc_algae1/100))  /input$algae_1) /1000 ) * input$how_many_tanks,
                                               `Volume algae #2 (total L)` = ( ( ( ((input$tank_volume*1000)*(input$target_cell_density)) *(input$perc_algae2/100))  /input$algae_2) /1000 ) * input$how_many_tanks,
                                               `Volume algae #3 (total L)` = ( ( ( ((input$tank_volume*1000)*(input$target_cell_density)) *(input$perc_algae3/100))  /input$algae_3) /1000 ) * input$how_many_tanks,
                                               `Master mix (L)` = sum( (  ( ( ( ((input$tank_volume*1000)*(input$target_cell_density)) *(input$perc_algae1/100))  /input$algae_1) /1000 ) * input$how_many_tanks   ), 
                                                                       (  ( ( ( ((input$tank_volume*1000)*(input$target_cell_density)) *(input$perc_algae2/100))  /input$algae_2) /1000 ) * input$how_many_tanks   ), 
                                                                       ( ( ( ( ((input$tank_volume*1000)*(input$target_cell_density)) *(input$perc_algae3/100))  /input$algae_3) /1000 ) * input$how_many_tanks    )),
                                               `FEED PER TANK (L)` = sum( (  ( ( ( ((input$tank_volume*1000)*(input$target_cell_density)) *(input$perc_algae1/100))  /input$algae_1) /1000 ) ), 
                                                                          (  ( ( ( ((input$tank_volume*1000)*(input$target_cell_density)) *(input$perc_algae2/100))  /input$algae_2) /1000 ) ), 
                                                                          ( ( ( ( ((input$tank_volume*1000)*(input$target_cell_density)) *(input$perc_algae3/100))  /input$algae_3) /1000 ) )),
                                               check.names = FALSE)
                          
                          
                          if(!is.null(temp)) {
                            
                            if(isolate(input$date) < temp[nrow(temp), 1]) {
                              temp <- rbind(temp, newRow)
                              temp <- dplyr::arrange(temp, Date)
                            } else {
                              temp <- rbind(temp, newRow)
                            }
                          } else {
                            temp <- rbind(temp, newRow)
                          }
                          
                          tableValues$m <- temp
                          
             })
  
  
                        
             output$feed_table <- DT::renderDataTable({
                  
                  if(!is.null(tableValues$m)){
                    
                    table_data <- tableValues$m
                    
                    if(nrow(table_data > 2)){
                      table_data <- dplyr::arrange(table_data, Date)
                    }
                  }
                  rownames(table_data)[nrow(table_data)] <- "Boom, algae!"
                  table_data
                  
             })
    
observe({print(colnames(tableValues$m))})
observe({print(!is.null(tableValues$m))}) 

})



