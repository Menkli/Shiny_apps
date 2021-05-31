#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(ggthemes)
library(ggiraph)
library(plotly)

# Read file
vacc<-read.csv("https://info.gesundheitsministerium.gv.at/data/timeline-eimpfpass.csv", sep=";", encoding="UTF-8", quote = "\"" )

# Some preparations to work with the dataset
vacc <- vacc[vacc$Name != "KeineZuordnung", ]
vacc <- vacc[vacc$Name != "Österreich", ]
vacc$X.U.FEFF.Datum <- as.Date(vacc$X.U.FEFF.Datum)
vacc$VollimmunisiertePro100 <- as.numeric(vacc$VollimmunisiertePro100)

# Here starts the Shiny part------
# Define UI for application that shows ggplots that change what they display according to what the user chooses. 
ui <- navbarPage(
    title = "Impfdashboard",
            tabPanel(title="Geimpfte in Österreich", 
                fluidRow(column(1),
                    column(10,plotlyOutput(outputId = "panels",
                                            width = "80%",
                                            height = "800px")))),
                 
            tabPanel(title="Impftstoff",
                fluidRow(column(4),column(4,
                    selectInput(inputId = "par",
                                label = "Impfstoff",
                                choices= c("Pfizer", "Moderna", "AstraZeneca"),
                                selected = "Teilgeimpfte")),
                        column(4),
                        column(12,(plotlyOutput(outputId = "impfstoff",
                                                width = "80%",
                                                height = "800px"))),
                    
                           
                )  
            )
        )

# 
server <- function(input, output) {
    
    # render functions are run every time the user changes a widget
    output$panels <- renderPlotly({
        
        d<-ggplot(data=vacc, aes(X.U.FEFF.Datum, TeilgeimpftePro100)) + 
            geom_area(stat="identity", fill="#74C2B8") + 
            ggtitle("Gemeldete Impfungen pro 100 Einwohner") + 
            theme(plot.title = element_text(hjust = 0.5, size = 20, margin = 20)) +
            ylab("Anzahl") + 
            xlab("") + 
            geom_area(aes(x=X.U.FEFF.Datum, y=VollimmunisiertePro100), 
                     fill="#409B92") + 
            # Create a panel for each federal state (variable -Name- in the dataset) with facet_wrap()
            facet_wrap(Name~., ncol = 3)
        
       # Hand over the ggplot to ggplotly to make it interactive 
        ggplotly(p = d, tooltip = c("TeilgeimpftePro100", "X.U.FEFF.Datum")) 
    })
    
    
    output$impfstoff <- renderPlotly({
        choice    <- switch(input$par,
                          "Pfizer" = vacc$EingetrageneImpfungenBioNTechPfizer_1, 
                          "Moderna" = vacc$EingetrageneImpfungenModerna_1,
                          "AstraZeneca" = vacc$EingetrageneImpfungenAstraZeneca_1
        ) 
        
        p<-ggplot(data = vacc, aes(x=X.U.FEFF.Datum, y=choice)) +
            geom_bar(stat="identity", fill="#74C2B8")+
            ggtitle("Verwendeter Impfstoff")+ylab("Anzahl")+xlab("") +
            theme(plot.title = element_text(hjust = 0.5, size = 20)) +
            facet_wrap(Name~., ncol = 3) 
            
        ggplotly(p) 
        
    })
    
}

# Run once
shinyApp(ui = ui, server = server)


# Homework: Center the Impfstoff panel and fix the tooltips :)
