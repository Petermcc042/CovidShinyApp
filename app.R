library(shiny)
library(ggplot2)
library(ggthemes)
library(plotly)
library(tidyverse)
library(tidyquant)
library(shinythemes)



Cases <- read.csv("https://raw.githubusercontent.com/Petermcc042/CovidShinyApp/main/cases.csv")
Deaths <- read.csv("https://raw.githubusercontent.com/Petermcc042/CovidShinyApp/main/dailydeaths.csv")
Vaccinations <- read.csv("https://raw.githubusercontent.com/Petermcc042/CovidShinyApp/main/dailyvaccinations.csv")
Admissions <- read.csv("https://raw.githubusercontent.com/Petermcc042/CovidShinyApp/main/hospitaladmissions.csv")

casest <- Cases %>% select(date, newCasesBySpecimenDate)
deathst <- Deaths %>% select(date, newDailyNsoDeathsByDeathDate)
vacct <- Vaccinations %>% select(date, newVaccinesGivenByPublishDate)
admisst <- Admissions %>% select(date, newAdmissions)

length(casest)
pageone <- merge(casest, deathst, by="date", all = T)
pageone <- merge(pageone, vacct, by="date", all = T)
pageone <- merge(pageone, admisst, by="date", all = T)
colnames(pageone) <- c('date', 'Cases', 'Deaths', 'Vaccinations', 'Admissions')


casesselect <- c('Cases', 'Deaths', 'Vaccinations', 'Admissions')

ui <- navbarPage("COVID-19 Dashboard", theme = shinytheme("flatly"),
                 tabPanel("Home Page",
                          titlePanel("COVID-19 Visualisation Dashboard"),
                          fluidRow(column(8, "The data above shows a basic plot of the UK government's data provided on their website. The number of vaccinations per day 
                          has been included on a second axis as it is 100x the amount of the next highest variable. As deaths and hospital admissions are 
                          much lower again than cases you can analyse these closer by deselecting variable names in the legend.")),
                          plotlyOutput("plot"),
                          titlePanel("Individual Time Series Breakdown"),
                          selectInput("dataselected", "What data would you like to look at?", casesselect),
                          fluidPage(
                              fluidRow(column(4, plotOutput("onebox")), column(3, tableOutput('summarytable')), column(5, plotOutput("onehist") )),
                              fluidRow(column(8, ""), column(4, sliderInput(inputId = "bins", label = "Bin Width:", min = 1, max = 20, value = 10))
                                       )
                              ),
                          )
)

server <- function(input, output, session) {
    
    binwid <- reactiveVal(10)
    
    ##################################### home page #####################################
    output$plot <- renderPlotly({
        rangebuttons <- list(buttons=list(list(count=1, label="1M", step="month", stepmode="backward"),
                                          list(count=6, label="6M", step="month", stepmode="backward"),
                                          list(count=1, label="YTD", step="year", stepmode="todate"),
                                          list(count=1, label="1Y", step="year", stepmode="backward"),
                                          list(label="ALL", step="all")))
        xformats <- list(rangeslider = list(visible = T),
                        rangeselector = rangebuttons,
                        zerolinecolor = '#ffff',
                        zerolinewidth = 2,
                        gridcolor = 'ffff')
        yformats <- list( zerolinecolor = '#ffff', zerolinewidth = 2, gridcolor = 'ffff')
        y2formats <- list( overlaying = "y", side = "right", title = "")
        
        fig <- plot_ly(pageone, type = 'scatter', mode = 'lines')%>%
            add_trace(x = ~date, y = ~Cases, name = 'Cases', line = list(color = '#FF6229'))%>%
            add_trace(x = ~date, y = ~Deaths, name = 'Deaths', line = list(color = '#F0845D'))%>%
            add_trace(x = ~date, y = ~Vaccinations, name = 'Vaccines', yaxis = "y2", line = list(color = '#1FA389'))%>%
            add_trace(x = ~date, y = ~Admissions, name = 'Admissions', line = list(color = '#9EF3E2'))%>%
            layout(title = 'COVID-19 Basic Visualisation', legend=list(title=list(text='Variable')),
                   xaxis = xformats, yaxis = yformats, yaxis2 = y2formats)
        options(warn = -1)
        
        fig
    })
    
    
    output$onebox <- renderPlot({
        ggplot(pageone, aes(x="", y=eval(parse(text = input$dataselected)) ))+
            geom_boxplot(color="#1FA389", fill="aquamarine", alpha=0.2) +
            labs(x = input$dataselected, y = "") +
            theme_tq()
    }, res = 96)
    

    
    output$onehist <- renderPlot({
        ggplot(pageone, aes(x=eval(parse(text = input$dataselected)))) + 
            geom_histogram(bins = input$bins, color="#1FA389", fill="aquamarine", alpha=0.2) +
            labs(x = paste(input$dataselected, " per day"), y = "Count") +
            theme_tq()
    }, res = 96)
    
    summarydf <- eventReactive(input$dataselected, {
        data <- na.omit(pageone[,input$dataselected])
        Summary <- c(Min = min(data),
                     quantile(data, 0.25),
                     quantile(data, 0.75),
                     Max = max(data),
                     Mean = mean(data),
                     Median = median(data),
                     StandardDeviation = sd(data))
        rownames <- c('Min', '1st Quantile', '3rd Quantile', 'Max', 'Mean', 'Median', 'Standard Deviation')
        
        df <- data.frame(rownames, Summary) %>% format(digits = 0, big.mark = ",")
        
        df
    }, ignoreNULL = FALSE)
    
    output$summarytable <- renderTable({summarydf()}, 
                                       striped = TRUE, spacing = 'l', hover = TRUE, align = 'c', bordered = TRUE, width = '100%')
    
    observeEvent(input$reset, {
        updateSliderInput(inputId = "bins", value = 10)
    })
    
}

shinyApp(ui, server)