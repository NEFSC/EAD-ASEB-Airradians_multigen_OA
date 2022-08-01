library(shiny)

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
                        list("T-iso", "Pavlova", "Chaetoceros","Nano","Tetraselmis", "NA")),
            numericInput("algae_1", "Algae #1 cells mL-1", 2000000,min = 0, max = 200000000),
            numericInput("perc_algae1", "Target % composition of algae #1", 75,min = 0, max = 100) #,
                ),
    
          column(3,offset =1,
            selectInput("algae_species_2", "Choose species for Algae #2:",
                       list("Chaetoceros", "T-iso", "Pavlova","Nano","Tetraselmis", "NA")),   
            numericInput("algae_2", "Algae #2 cells mL-1)", 2000000,min = 0, max = 200000000),
            numericInput("perc_algae2", "Target % composition of algae #2", 25,min = 0, max = 100)
                ),
    
          column(3,offset =1,
            selectInput("algae_species_3", "Choose species for Algae #3:",
                       list("Pavlova", "T-iso", "Chaetoceros","Nano","Tetraselmis", "NA")),   
            numericInput("algae_3", "Algae #3 cells mL-1", 2000000,min = 0, max = 200000000),
            numericInput("perc_algae3", "Target % composition of algae #3", 0,min = 0, max = 100) #,
                 ),
    
    # submit button - cumulative outputs 
    actionButton("submit", "Feed 'em!", icon("Submit") , width = NULL, style="color: #fff; background-color: #337ab7; border-color: #2e6da4"), 
   
    DT::DTOutput("feed_table"),

           ) # main panel
     ) # fluid page
) # shinyUI