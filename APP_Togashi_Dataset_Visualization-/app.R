library(shiny)
library(shinydashboard)
library(ggpubr)
library(plotly)
library(shinyjs)
library(DT)

#Download The Togashi Dataset from GitHub
data <- read.csv("https://raw.githubusercontent.com/jrobledob/Data_Colloquium/main/Supplementary_Data_Togashi_FINAL.csv", na.strings = "")
#Filter criterium options
options_criterium<- c("--SELECT--", "Host","Pathogen")
#List of host options in The Togashi Dataset
hosts<- c("--SELECT--", unique(na.omit(data$HOST_PLANT)))
#List of pathogens The Togashi Dataset
pathogens<- c("--SELECT--",unique(data$SPECIES_FINAL))






if (interactive()) {
    # Define UI for application that draws a histogram
    ui <- fluidPage(
        useShinyjs(),
        #Dashboard
        dashboardPage(
            dashboardHeader(title = "Togashi Dataset Finder"),
            dashboardSidebar(
                #Filter sidebar----
                div(
                    id = "form",
                    selectInput('filter_criterium', 'Select Main Filter Criterium:', options_criterium, selected= "--Select--"),
                    #when host is selected
                    conditionalPanel('input.filter_criterium == "Host"',
                                     selectizeInput('host','Choose one of Hosts Available in The Togashi Dataset', choices = hosts),
                                     #Selecting pathogen by host
                                     conditionalPanel('input.host!="--SELECT--"',
                                                      selectizeInput('pathogen_by_host', 'Choose one of Pathogens Available for this Host',choices = pathogens))),
                    
                    #when Pathogen is selected
                    conditionalPanel('input.filter_criterium == "Pathogen"',
                                     selectizeInput('pathogen', 'Choose one of Pathogens Available in The Togashi Dataset', choices = pathogens))),
                #Action button----
                actionButton("button", "Reset Values!"),
                tags$p("For updating the plot and downloading the CORRECT data set please ALWAYS reset the values between "),
                downloadButton('downloadData', 'Download Filtered Data')
            ),
            dashboardBody(
                #Values Boxes ----
                fluidRow(
                    valueBoxOutput("XXXX1"),
                    valueBoxOutput("XXXX2"),
                    valueBoxOutput("XXXX3")
                ),
                #Main plot temperatures ----
                fluidRow(
                    #Table
                    box(title = "Summary table",
                        solidHeader = T,
                        width = 4,
                        collapsible = T,
                        div(DT::DTOutput("table"), style = "font-size: 70%;")),
                    #Main plot
                    box(title = "Plot", solidHeader = T,
                        width = 8, collapsible = T,
                        plotlyOutput("macro_plot"))
                ),
                #Next ----
                
                
            )
        )
    )
    
    
    
    
    
    
    
    
    
    
    # Define server logic required to draw a histogram
    server <- function(input, output, session) {
        #Update list of pathogens by hosts----
        observe({
            hosts <- input$host
            # Can use character(0) to remove all choices
            if (is.null(hosts))
                hosts <- character(0)
            # Can also set the label and select items
            updateSelectInput(session, "pathogen_by_host",
                              label = paste('Choose one of Pathogens Available for ', hosts,":"),
                              choices = c("---SELECT---", unique(data$SPECIES_FINAL[which(data$HOST_PLANT==hosts)]))
            )
        })
        #Value boxes ----
        output$XXXX1 <- renderValueBox({
            host<- input$host
            pathogen_by_host<- input$pathogen_by_host
            pathogen<- input$pathogen
            if(host!="--SELECT--"){
                values<-data %>%
                    filter(SPECIES_FINAL==pathogen_by_host,HOST_PLANT==host)
            }else{
                values<- data %>%
                    filter(SPECIES_FINAL==pathogen)
            }
            
            valueBox("Kingdom:",unique(values[,1]), color = "olive")
        })
        output$XXXX2 <- renderValueBox({
            host<- input$host
            pathogen_by_host<- input$pathogen_by_host
            pathogen<- input$pathogen
            if(host!="--SELECT--"){
                values<-data %>%
                    filter(SPECIES_FINAL==pathogen_by_host,HOST_PLANT==host)
            }else{
                values<- data %>%
                    filter(SPECIES_FINAL==pathogen)
            }
            
            valueBox("Specie:",unique(values[,2]), color = "yellow")
        })
        
        output$XXXX3 <- renderValueBox({
            host<- input$host
            pathogen_by_host<- input$pathogen_by_host
            pathogen<- input$pathogen
            if(host!="--SELECT--"){
                values<-data %>%
                    filter(SPECIES_FINAL==pathogen_by_host,HOST_PLANT==host)
            }else{
                values<- data %>%
                    filter(SPECIES_FINAL==pathogen)
            }
            
            valueBox("Number of authors" ,paste("reporting information: ",length(unique(values[,5]))), color = "red")
        })
        #Table macroplot ----
        output$table <- DT::renderDataTable({
            host<- input$host
            pathogen_by_host<- input$pathogen_by_host
            pathogen<- input$pathogen
            if(host!="--SELECT--"){
                data %>%
                    filter(SPECIES_FINAL==pathogen_by_host,HOST_PLANT==host) %>%
                    select(PROCESS,CARDINAL,TEMPERATURE)
            }else{
                data %>%
                    filter(SPECIES_FINAL==pathogen)%>%
                    select(PROCESS,CARDINAL,TEMPERATURE)
            }
        })
        #Macroplot ----
        output$macro_plot <- renderPlotly({
            #Filter data
            host<- input$host
            pathogen_by_host<- input$pathogen_by_host
            pathogen<- input$pathogen
            filter_data<- 
                if(host %in% unique(na.omit(data$HOST_PLANT))){
                    filter(data,SPECIES_FINAL==pathogen_by_host,HOST_PLANT==host)
                }else{
                    filter(data,SPECIES_FINAL==pathogen)
                }
            plot_macro<-ggbarplot(data = filter_data, x= "PROCESS", y= "TEMPERATURE", 
                                  add=c("mean_sd","jitter"), add.params = list(shape="CARDINAL"), 
                                  palette = c("#0021A5", "#FA4616", "#FFFFFF") ,color = "#666666", 
                                  fill="CARDINAL", position = position_dodge(0.8))
            ggplotly(plot_macro)
            
            
            
            
        })
        
        
        #Reset values
        observeEvent(input$button, {
            reset("form")
        })
        #Download data
        output$downloadData <- downloadHandler(
            filename = function() {
                paste('data-', Sys.Date(), '.csv', sep='')
            },
            content = function(con) {
                host<- input$host
                pathogen_by_host<- input$pathogen_by_host
                pathogen<- input$pathogen
                if(host!="--SELECT--"){
                    values<-data %>%
                        filter(SPECIES_FINAL==pathogen_by_host,HOST_PLANT==host)
                }else{
                    values<- data %>%
                        filter(SPECIES_FINAL==pathogen)
                }
                
                write.csv(values, con)
            }
        )
    }
    
    # Run the application 
    shinyApp(ui = ui, server = server)
}