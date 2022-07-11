
library(shiny)
# push lines 4 - 7 to send edits of ui and server scripts to URL
# rsconnect::setAccountInfo(name='sam-geoduck',
#                            token='340A1FE56E654C48A346EDD10D0CC45F',
#                            secret='hmitiSg16pfLx0ckLCu/1+/GByjPw/sdhcjzw4N/')
# rsconnect::deployApp('C:/Users/samjg/Documents/Github_repositories/Airradians_multigen_OA/RAnalysis/Scripts/RShiny/Larvae_feed_ui_and_server')


ui <- shinyUI(fluidPage(

  
  
  # Application title 
    div(h2('Bivalve larvae feeder', style="margin: 0;color:teal"), h3('(designed for static larval-rearing systems)', style="color:teal"), h5('quick. efficient. effective.', style="color:teal")),
  #h5("Subtitle 1", style="color:orange"),
  # Input data 
  mainPanel(

    h5({ paste("RShiny application built by Samuel J. Gurr (last update 07/08/2019)")}), 
    h5({ paste("Contact: samjgurr@gmail.com; samuel.gurr@noaa.gov")}),
    
           dateInput('date', "Choose today's date:", value = NULL, min = NULL, max = NULL,
                    format = "yyyy-mm-dd", startview = "month", weekstart = 0,
                    language = "en", width = NULL),
    
            numericInput("how_many_tanks", "How many tanks? (all same volume)", 18, min = 1, max = 50),
    
            numericInput("tank_volume", "Tank volume? (L)", 10 ,min = 0, max = 5000),
    
            numericInput("target_cell_density", "What is the target cell density? (cells mL-1)", 25000,min = 0, max = 1000000), # ,
          
    h4("once you've completed above..."),
          
          column(3,
            # numericInput("feed_conical", "target mL per hour continuous feed", 1000 ,min = 0, max = 1000),
            selectInput("algae_species_1", "Choose species for Algae #1:",
                        list("T-iso", "Pavlova", "Chaetoceros","Nano","Tetraselmis, NA")),
            numericInput("algae_1", "Algae #1 cells mL-1 (Pav, T-iso, Tet, Nano, Chaet, etc.)", 2000000,min = 0, max = 200000000),
            numericInput("perc_algae1", "Target % composition of algae #1", 75,min = 0, max = 100) #,
                ),
    
          column(3,offset =1,
            selectInput("algae_species_2", "Choose species for Algae #2:",
                       list("T-iso", "Pavlova", "Chaetoceros","Nano","Tetraselmis, NA")),   
            numericInput("algae_2", "Algae #2 cells mL-1  (Pav, T-iso, Tet, Nano, Chaet, etc.)", 2000000,min = 0, max = 200000000),
            numericInput("perc_algae2", "Target % composition of algae #2", 25,min = 0, max = 100)
                ),
    
          column(3,offset =1,
            selectInput("algae_species_3", "Choose species for Algae #3:",
                       list("T-iso", "Pavlova", "Chaetoceros","Nano","Tetraselmis, NA")),   
            numericInput("algae_3", "Algae #3 cells mL-1  (Pav, T-iso, Tet, Nano, Chaet, etc.)", 2000000,min = 0, max = 200000000),
            numericInput("perc_algae3", "Target % composition of algae #3", 0,min = 0, max = 100) #,
                 ),
    
    # submit button - cumulative outputs 
    actionButton("submit", "Feed 'em!", icon("Submit") , width = NULL, style="color: #fff; background-color: #337ab7; border-color: #2e6da4"), 
   
    DT::DTOutput("feed_table"),

           ) # main panel
     ) # fluid page
) # shinyUI



# assign table values 
table_data <- data.frame(Date = as.Date(as.character()),
                         `Number of tanks` = as.numeric(), 
                         `Tank volume` = as.numeric(),
                         `Target cells density (cells ml-1)` = as.numeric(),
                         `Volume algae #1 (total L)` = as.numeric(),
                         `Volume algae #2 (total L)` = as.numeric(),
                         `Volume algae #3 (total L)` = as.numeric(),
                         `Master mix (L)` = as.numeric(),
                         `FEED PER TANK (L)` = as.numeric(),
                         check.names = FALSE)



server <- shinyServer(function(input, output,session){
  
  
  
  
  
            # vals <- reactiveValues()
            # observe({
            #   vals$x                   <- input$tank_volume*1000 # convert to cells per milliliter
            #   vals$t                   <- input$target_cell_density # in cells per milliliter
            #   vals$q                   <- input$how_many_tanks
            #   vals$f                   <- input$flow_rate*1000 # convert to milliliters
            #   vals$total_cells         <- vals$x*vals$t
            #   vals$vol                 <- input$feed_conical
            #   vals$algae_mix_conc      <- (input$algae_1*(input$perc_algae1/100)) +
            #     (input$algae_2*(input$perc_algae2/100)) +
            #     (input$algae_3*(input$perc_algae3/100)) # total cell concentration of mixed diet
            #   vals$assumed_hourly_loss <- (vals$f*60)*vals$t  # in cells per milliliter
            #   vals$percent_loss_hourly <- (vals$assumed_hourly_loss/vals$total_cells)*100
            #   vals$Liters_algae_initial <- ((((vals$total_cells*(input$perc_algae1/100))/input$algae_1)/1000)+
            #                                   (((vals$total_cells*(input$perc_algae2/100))/input$algae_2)/1000)+
            #                                   (((vals$total_cells*(input$perc_algae3/100))/input$algae_3)/1000))
            #   vals$LPH_feed <- ((vals$percent_loss_hourly/100)*vals$Liters_algae_initial)
            #   
            # })
  
            
            
            
            
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
                    
                    # FeedLarvae <- data.frame(Date = table_data[nrow(table_data),1],
                    #                          `Number of tanks` = as.numeric(), 
                    #                          `Tank volume` = as.numeric(),
                    #                          `Target cells density (cells ml-1)` = as.numeric(),
                    #                          `Volume algae #1 (total L)` = as.numeric(),
                    #                          `Volume algae #2 (total L)` = as.numeric(),
                    #                          `Volume algae #3 (total L)` = as.numeric(),
                    #                          `Master mix (L)` = as.numeric(),
                    #                          `FEED PER TANK (L)` = as.numeric(),
                    #                          check.names = FALSE)
                    # 
                    # table_data <- rbind(table_data, FeedLarvae)
                    
                    if(nrow(table_data > 2)){
                      table_data <- dplyr::arrange(table_data, Date)
                    }
                  }
                  rownames(table_data)[nrow(table_data)] <- "Boom, algae!"
                  table_data
                  
             })
    
observe({print(colnames(tableValues$m))})
observe({print(!is.null(tableValues$m))}) 
    
    
            

  # 
  # 
  # output$batch_algae1 <- renderText({
  #   paste("Algae #1 (L per tank) =",
  #         ((vals$total_cells*(input$perc_algae1/100))/input$algae_1)/1000)
  # })
  # 
  # output$batch_algae2 <- renderText({
  #   paste("Algae #2 (L per tank) =",
  #         ((vals$total_cells*(input$perc_algae2/100))/input$algae_2)/1000)
  # })
  # 
  # output$batch_algae3 <- renderText({
  #   paste( "Algae #3 (L per tank) =",
  #          ((vals$total_cells*(input$perc_algae3/100))/input$algae_3)/1000)
  # })
  # 
  # output$batchtotal <- renderText({
  #   paste("Master mix total (L) =", 
  #         vals$Liters_algae_initial * vals$q)
  # })
  # 
  
})



  
  
  
  shinyApp(ui = ui, server = server)
