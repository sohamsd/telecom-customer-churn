library(shiny)
library(dplyr)
library(shinydashboard)
library(gridExtra)
library(grid)
library(ggplot2)

source("data/clean_data.R")

gender_options <- c("No Selection", "Female", "Male")
senior_options <- c("No Selection", "Yes", "No")
relationship_options <- c("No Selection", "Married", "Single")
dep_options <- c("No Selection", "Yes", "No")

ui <- fluidPage(
  
  dashboardPage(skin = "purple", 
    dashboardHeader(title = "Customer Churn Dashboard", titleWidth = 300),
    dashboardSidebar( title = "Filters", width = 160,
                      sidebarMenu(style = "position: fixed; width:150px;",
                                  selectInput("gender", 
                                              label = "Gender:",
                                              choices = c(gender_options), 
                                              selected = "No Selection"),
                                  selectInput("senior", 
                                              label = "Senior Citizen:",
                                              choices = c(senior_options), 
                                              selected = "No Selection"),
                                  selectInput("relationship", 
                                              label = "Relationship Status:",
                                              choices = c(relationship_options), 
                                              selected = "No Selection"),
                                  selectInput("dependents", 
                                              label = "Dependents:",
                                              choices = c(dep_options), 
                                              selected = "No Selection")
                      )

    ),
    
    dashboardBody(
      fluidRow(
        box(title = "Customer Base Info", width = NULL, solidHeader = TRUE, status = "primary",
        column(width = 4,
               box(title = "All Customers", width = NULL, solidHeader = TRUE, status = "primary",
                   box(title = "Number of Customers", width = NULL, status = "primary", infoBoxOutput("customerCountNoFilter")),
                   
               )),
        column(width = 4,
               box(title = "Customers Retained", width = NULL, solidHeader = TRUE, status = "success",
                   box(title = "Number of Customers", width = NULL, status = "success", infoBoxOutput("customerCountRetained")),
                   
               )),
        column(width = 4,
               box(title = "Customers Churned", width = NULL, solidHeader = TRUE, status = "danger",
                   box(title = "Number of Customers", width = NULL, status = "danger", infoBoxOutput("customerCountChurn")),
                   
               ))
      )),
      fluidRow(
        box(title = "Customer Data Histogram", width = NULL, solidHeader = TRUE,status = "primary",
            fluidRow(
              box(align = "center",
                  title = NULL, 
                  width = 12,
                  status = NULL,
                  box(width = NULL, plotOutput("plot1", height = "150px")))
            ),
            
      )),
      
      fluidRow(
        column(width = 4,
               box(title = "All Customers", width = NULL, solidHeader = TRUE, status = "primary",
                  
                   box(title = "Average Monthly Charges", width = NULL, status = "primary", infoBoxOutput("avgMonthlyChargesNoFilter")))),
        column(width = 4,
               box(title = "Customers Retained", width = NULL, solidHeader = TRUE, status = "success",
                   
                   box(title = "Average Monthly Charges", width = NULL, status = "success", infoBoxOutput("avgMonthlyChargesRetained")))),
        column(width = 4,
               box(title = "Customers Churned", width = NULL, solidHeader = TRUE, status = "danger",
                   
                   box(title = "Average Monthly Charges", width = NULL, status = "danger", infoBoxOutput("avgMonthlyChargesChurn"))))
      ),
      fluidRow(
        box(width = NULL, title = "Monthly Charges Scatterplot", solidHeader = TRUE, status = "primary",
          fluidRow(
            box(align = "center",
                title = NULL, 
                width = 12,
                status = NULL,
                box(width = NULL, plotOutput("plot9"))
               
               
        )
      ))),
      fluidRow(
        box(title = "Tenure", width = NULL, solidHeader = TRUE, status = "primary",
            
            fluidRow(
              column(width = 4,
                     box(title = "All Customers", width = NULL, solidHeader = TRUE, status = "primary",
                         box(title = "Average Number of Months with Company", width = NULL, status = "primary", infoBoxOutput("avgMonthsNoFilter"))
                     )),
              column(width = 4,
                     box(title = "Customers Retained", width = NULL, solidHeader = TRUE, status = "success",
                         box(title = "Average Number of Months with Company", width = NULL, status = "success", infoBoxOutput("avgMonthsRetained"))
                     )),
              column(width = 4,
                     box(title = "Customers Churned", width = NULL, solidHeader = TRUE, status = "danger",
                         box(title = "Average Number of Months with Company", width = NULL, status = "danger", infoBoxOutput("avgMonthsChurn"))
                     ))
            ),
            fluidRow(
              box(title = "Tenure and Churn", width = NULL, solidHeader = TRUE,status = "primary",
                fluidRow(
                        box(align = "center", width = 12,status = NULL, title = "Months with Company"),
                        box(width = NULL, plotOutput("plot2") )
                              )
                     )
                     
              ),
            
        )),
      fluidRow(
        box(title = "Customer Billing Info", width = NULL, solidHeader = TRUE, status = "primary",
            fluidRow(
              tabBox(width = NULL, title = "Billing", side = "right",
                     tabPanel("Contract Type",
                              fluidRow(
                                box(width = NULL, title = NULL, status = "primary", plotOutput("plot3"))
                              )
                     ),
                     tabPanel("Payment Methods",
                              fluidRow(
                                box(width = NULL, title = NULL, status = "primary", plotOutput("plot4"))
                              )
                     ),
                     tabPanel("Paperless Billing",
                              fluidRow(
                                box(width = NULL, title = NULL, status = "primary", plotOutput("plot6"))
                              )
                     )
              )
            )
        )),
      
      fluidRow(
        box(title = "Added Services", width = NULL, solidHeader = TRUE, status = "primary",
            fluidRow(
              tabBox(width = NULL, title = "Type of Service", side = "right",
                     tabPanel("Internet ",
                              fluidRow(
                                box(width = 12, title = "Internet ", plotOutput("plot10"))
                              )
                     ),
                     tabPanel("Phone",
                              fluidRow(
                                box(width = 12, title = "Phone", plotOutput("plot11"))
                              )
                     ),
                     tabPanel("Streaming",
                              fluidRow(
                                box(width = 12, title = "Streaming", plotOutput("plot12"))
                              )
                     )
              )
            )
        ))
    )
   )
 )

server <- function(input, output) {
  
  rv = reactiveValues()
  rv$dataset=dataset
  
  observe({
    rv$dataset = dataset %>% filter(if(input$gender == 'No Selection'){gender %in% gender_options} else {gender == input$gender}) %>%
      filter(if(input$senior == 'No Selection'){SeniorCitizen %in% senior_options} else {SeniorCitizen == input$senior}) %>%
      filter(if(input$relationship == 'No Selection'){Partner %in% relationship_options} else {Partner == input$relationship}) %>%
      filter(if(input$dependents == 'No Selection'){Dependents %in% dep_options} else {Dependents == input$dependents}) 

    rv$churn = filter(rv$dataset, Churn == 'Churned')
    rv$retained = filter(rv$dataset, Churn == 'Retained')

    rv$dim = dim(rv$dataset)
    rv$dimRetained = dim(rv$retained)
    rv$dimChurn = dim(rv$churn)
  })
  
  output$customerCountNoFilter <- renderInfoBox({
    x = format(rv$dim[1], big.mark = ",")
    infoBox("Customer Count", x,
      color = "light-blue",
      icon = icon("users")
    )
  })
  output$customerCountRetained <- renderInfoBox({
    x = format(rv$dimRetained[1], big.mark = ",")
    infoBox("Customer Count", x,
            color = "green",
            icon = icon("fa-solid fa-check")
    )
  })
  output$customerCountChurn <- renderInfoBox({
    x = format(rv$dimChurn[1], big.mark = ",")
    infoBox("Customer Count",x,
            color = "red",
            icon = icon("fa-solid fa-skull-crossbones")
    )
  })



  output$avgMonthlyChargesNoFilter <- renderInfoBox({
    x = format(round(mean(rv$dataset$MonthlyCharges),2))
    infoBox("Average Monthly", paste(x),
             color = "light-blue",
             icon = icon("dollar-sign")
    )
  })
  output$avgMonthlyChargesRetained <- renderInfoBox({
    x = format(round(mean(rv$retained$MonthlyCharges),2))
    infoBox("Average Monthly", paste(x),
            color = "green",
            icon = icon("dollar-sign")
    )
  })
  output$avgMonthlyChargesChurn <- renderInfoBox({
    x = format(round(mean(rv$churn$MonthlyCharges),2))
    infoBox("Average Monthly", paste(x),
            color = "red",
            icon = icon("dollar-sign")
    )
  })
  output$avgMonthsNoFilter <- renderInfoBox({
    x = format(round(mean(rv$dataset$tenure),2))
    infoBox("Avg Months", paste(x),
             color = "light-blue",
            icon = icon("calendar-alt")
    )
  })
  output$avgMonthsRetained <- renderInfoBox({
    x = format(round(mean(rv$retained$tenure),2))
    infoBox("Avg Months", paste(x),
            color = "green",
            icon = icon("fa-solid fa-calendar-check")
    )
  })
  output$avgMonthsChurn <- renderInfoBox({
    x = format(round(mean(rv$churn$tenure),2))
    infoBox("Avg Months", paste(x),
            color = "red",
            icon = icon("fa-solid fa-calendar")
    )
  })
  
### Plots  
  output$plot1 <- renderPlot({

    plot1 <- ggplot(rv$dataset, aes(x = Churn, fill = Churn)) +
      geom_bar(position = "dodge", fill = c("#f0ad4e", "#5cb85c"), color = c("#ba7412", "#376e37")) +
      coord_flip() +
      geom_text(aes(label = paste(round(..count../nrow(rv$dataset)*100,2), "%")),
                stat = "count",
                position = 'dodge',
                hjust = -0.5,
                size = 3,
                inherit.aes = TRUE) +
      theme_minimal() +
      theme(legend.position = "none",
            axis.title=element_blank(),
            panel.border=element_blank())

    plot1
    })
  
  output$plot2 <- renderPlot({
    bar_fills <- c("orange", "#5cb85c")
    plot2 <- ggplot(rv$dataset, aes(x=tenure, fill= Churn))+
      geom_histogram(stat = 'bin',
                     bins = 50,
                     position = "dodge") +
      scale_fill_manual(values = bar_fills,
                        guide = "none") +
      xlab('Number of Months') +
      ylab('') +
      theme_minimal()
    plot2
  })
  # 
  output$plot3 <- renderPlot({

    p1 <- ggplot(rv$dataset, aes(x=Contract, fill = factor(Contract)))+
      geom_bar(position = "dodge",  fill = c("#5b94c5","#337ab7","#286192"), color = "#1e496d") +
      coord_flip() +
      geom_text(aes(label = paste(round(..count../nrow(rv$dataset)*100,2), "%")),
                stat = "count", position = "dodge", hjust = -0.1) +
      labs(title = "All Customers", fill = '')+
      theme_minimal() +
      theme(legend.position = "none",
            axis.ticks=element_blank(),
            axis.title=element_blank(),
            panel.grid=element_blank(),
            panel.border=element_blank())
    p2 <- ggplot(rv$retained, aes(x=Contract, fill = factor(Contract)))+
      geom_bar(position = "dodge", fill = c("#7cc67c", "#5cb85c", "#499349"), color = ("#376e37")) +
      coord_flip() +
      geom_text(aes(label = paste(round(..count../nrow(rv$retained)*100,2), "%")),
                stat = "count", position = "dodge",hjust = -0.1) +
      labs(title = "Customers Retained", fill = '')+
      theme_minimal() +
      theme(legend.position = "none",
            axis.text.x = element_text(size = 10),
            axis.ticks=element_blank(),
            axis.title=element_blank(),
            panel.grid=element_blank(),
            panel.border=element_blank())
    p3 <- ggplot(rv$churn, aes(x=Contract, fill = factor(Contract)))+
      geom_bar(position = "dodge", fill = c("#f3bd71","#f0ad4e", "#d89b46"), color = "#ba7412") +
      coord_flip() +
      geom_text(aes(label = paste(round(..count../nrow(rv$churn)*100,2), "%")),
                stat = "count", position = "dodge", hjust = -0.1) +
      labs(title = "Customers Churned", fill = '')+
      theme_minimal() +
      theme(legend.position = "none",
            axis.ticks=element_blank(),
            axis.title=element_blank(),
            panel.grid=element_blank(),
            panel.border=element_blank())
    plot3 <- grid.arrange(p1, p2, p3, ncol = 1)
    plot3
  })
  
  output$plot4 <- renderPlot({
    
    p1 <- ggplot(rv$dataset, aes(x=PaymentMethod, fill = factor(PaymentMethod)))+
      geom_bar(position = "dodge",  fill = c("#99bcdb","#5b94c5","#337ab7","#286192"), color = "#1e496d") +
      coord_flip() +
      geom_text(aes(label = paste(round(..count../nrow(rv$dataset)*100,2), "%")),
                stat = "count", position = "dodge", hjust = -0.1) +
      labs(title = "All Customers", fill = '')+
      theme_minimal() +
      theme(legend.position = "none",
            axis.ticks=element_blank(),
            axis.title=element_blank(),
            panel.grid=element_blank(),
            panel.border=element_blank())
    p2 <- ggplot(rv$retained, aes(x=PaymentMethod, fill = factor(PaymentMethod)))+
      geom_bar(position = "dodge", fill = c("#addbad","#7cc67c", "#5cb85c", "#499349"), color = ("#376e37")) +
      coord_flip() +
      geom_text(aes(label = paste(round(..count../nrow(rv$retained)*100,2), "%")),
                stat = "count", position = "dodge",hjust = -0.1) +
      labs(title = "Customers Retained", fill = '')+
      theme_minimal() +
      theme(legend.position = "none",
            axis.text.x = element_text(size = 10),
            axis.ticks=element_blank(),
            axis.title=element_blank(),
            panel.grid=element_blank(),
            panel.border=element_blank())
    p3 <- ggplot(rv$churn, aes(x=PaymentMethod, fill = factor(PaymentMethod)))+
      geom_bar(position = "dodge", fill = c("#f7d6a6","#f3bd71","#f0ad4e", "#d89b46"), color = "#ba7412") +
      coord_flip() +
      geom_text(aes(label = paste(round(..count../nrow(rv$churn)*100,2), "%")),
                stat = "count", position = "dodge", hjust = -0.1) +
      labs(title = "Customers Churned", fill = '')+
      theme_minimal() +
      theme(legend.position = "none",
            axis.ticks=element_blank(),
            axis.title=element_blank(),
            panel.grid=element_blank(),
            panel.border=element_blank())
    plot4 <- grid.arrange(p1, p2, p3, ncol = 1)
    plot4
  })


  
  output$plot6 <- renderPlot({
    
    p1 <- ggplot(rv$dataset, aes(x=PaperlessBilling, fill = factor(PaperlessBilling)))+
      geom_bar(position = "dodge",  fill = c("#337ab7","#286192"), color = "#1e496d") +
      coord_flip() +
      geom_text(aes(label = paste(round(..count../nrow(rv$dataset)*100,2), "%")),
                stat = "count", position = "dodge", hjust = -0.1) +
      labs(title = "All Customers", fill = '')+
      theme_minimal() +
      theme(legend.position = "none",
            axis.ticks=element_blank(),
            axis.title=element_blank(),
            panel.grid=element_blank(),
            panel.border=element_blank())
    p2 <- ggplot(rv$retained, aes(x=PaperlessBilling, fill = factor(PaperlessBilling)))+
      geom_bar(position = "dodge", fill = c("#5cb85c", "#499349"), color = ("#376e37")) +
      coord_flip() +
      geom_text(aes(label = paste(round(..count../nrow(rv$retained)*100,2), "%")),
                stat = "count", position = "dodge",hjust = -0.1) +
      labs(title = "Customers Retained", fill = '')+
      theme_minimal() +
      theme(legend.position = "none",
            axis.text.x = element_text(size = 10),
            axis.ticks=element_blank(),
            axis.title=element_blank(),
            panel.grid=element_blank(),
            panel.border=element_blank())
    p3 <- ggplot(rv$churn, aes(x=PaperlessBilling, fill = factor(PaperlessBilling)))+
      geom_bar(position = "dodge", fill = c("#f0ad4e", "#d89b46"), color = "#ba7412") +
      coord_flip() +
      geom_text(aes(label = paste(round(..count../nrow(rv$churn)*100,2), "%")),
                stat = "count", position = "dodge", hjust = -0.1) +
      labs(title = "Customers Churned", fill = '')+
      theme_minimal() +
      theme(legend.position = "none",
            axis.ticks=element_blank(),
            axis.title=element_blank(),
            panel.grid=element_blank(),
            panel.border=element_blank())
    plot6 <- grid.arrange(p1, p2, p3, ncol = 1)
    plot6
  })
  
  output$plot7 <- renderPlot({
    
    p1 <- ggplot(rv$dataset, aes(x=PaperlessBilling, fill = factor(PaperlessBilling)))+
      geom_bar(position = "dodge",  fill = c("#337ab7","#286192"), color = "#1e496d") +
      coord_flip() +
      geom_text(aes(label = paste(round(..count../nrow(rv$dataset)*100,2), "%")),
                stat = "count", position = "dodge", hjust = -0.1) +
      labs(title = "All Customers", fill = '')+
      theme_minimal() +
      theme(legend.position = "none",
            axis.ticks=element_blank(),
            axis.title=element_blank(),
            panel.grid=element_blank(),
            panel.border=element_blank())
    p2 <- ggplot(rv$retained, aes(x=PaperlessBilling, fill = factor(PaperlessBilling)))+
      geom_bar(position = "dodge", fill = c("#5cb85c", "#499349"), color = ("#376e37")) +
      coord_flip() +
      geom_text(aes(label = paste(round(..count../nrow(rv$retained)*100,2), "%")),
                stat = "count", position = "dodge",hjust = -0.1) +
      labs(title = "Customers Retained", fill = '')+
      theme_minimal() +
      theme(legend.position = "none",
            axis.text.x = element_text(size = 10),
            axis.ticks=element_blank(),
            axis.title=element_blank(),
            panel.grid=element_blank(),
            panel.border=element_blank())
    p3 <- ggplot(rv$churn, aes(x=PaperlessBilling, fill = factor(PaperlessBilling)))+
      geom_bar(position = "dodge", fill = c("#f0ad4e", "#d89b46"), color = "#ba7412") +
      coord_flip() +
      geom_text(aes(label = paste(round(..count../nrow(rv$churn)*100,2), "%")),
                stat = "count", position = "dodge", hjust = -0.1) +
      labs(title = "Customers Churned", fill = '')+
      theme_minimal() +
      theme(legend.position = "none",
            axis.ticks=element_blank(),
            axis.title=element_blank(),
            panel.grid=element_blank(),
            panel.border=element_blank())
    plot7 <- grid.arrange(p1, p2, p3, ncol = 1)
    plot7
  })

  output$plot9 <- renderPlot({
    plot9 <- ggplot(rv$dataset, aes(Churn %in% c("Retained", "Churn"))) +
      geom_point(aes(x = tenure, y = MonthlyCharges, color= Churn)) +
      scale_color_manual(values =  c("red", "#5cb85c")) +
      xlab("Number of Months")+
      ylab("Monthly Charges") +
      theme_minimal()
    plot9
  })
  output$plot10 <- renderPlot({
    
    p1 <- ggplot(rv$dataset, aes(x=InternetService, fill = factor(InternetService)))+
      geom_bar(position = "dodge",  fill = c("#99bcdb","#5b94c5","#286192"), color = "#1e496d") +
      coord_flip() +
      geom_text(aes(label = paste(round(..count../nrow(rv$dataset)*100,2), "%")),
                stat = "count", position = "dodge", hjust = -0.1) +
      labs(title = "All Customers", fill = '')+
      theme_minimal() +
      theme(legend.position = "none",
            axis.ticks=element_blank(),
            axis.title=element_blank(),
            panel.grid=element_blank(),
            panel.border=element_blank())
    p2 <- ggplot(rv$retained, aes(x=InternetService, fill = factor(InternetService)))+
      geom_bar(position = "dodge",fill = c("#addbad","#7cc67c", "#499349"), color = ("#376e37")) +
      coord_flip() +
      geom_text(aes(label = paste(round(..count../nrow(rv$retained)*100,2), "%")),
                stat = "count", position = "dodge",hjust = -0.1) +
      labs(title = "Customers Retained", fill = '')+
      theme_minimal() +
      theme(legend.position = "none",
            axis.text.x = element_text(size = 10),
            axis.ticks=element_blank(),
            axis.title=element_blank(),
            panel.grid=element_blank(),
            panel.border=element_blank())
    p3 <- ggplot(rv$churn, aes(x=InternetService, fill = factor(InternetService)))+
      geom_bar(position = "dodge", fill = c("#f7d6a6","#f3bd71","#d89b46"), color = "#ba7412") +
      coord_flip() +
      geom_text(aes(label = paste(round(..count../nrow(rv$churn)*100,2), "%")),
                stat = "count", position = "dodge", hjust = -0.1) +
      labs(title = "Customers Churned", fill = '')+
      theme_minimal() +
      theme(legend.position = "none",
            axis.ticks=element_blank(),
            axis.title=element_blank(),
            panel.grid=element_blank(),
            panel.border=element_blank())
    plot10 <- grid.arrange(p1, p2, p3, ncol = 1)

    plot10
  })
  output$plot11 <- renderPlot({
    p1 <- ggplot(rv$dataset, aes(x=PhoneServ, fill = factor(PhoneServ)))+
      geom_bar(position = "dodge",  fill = c("#99bcdb","#5b94c5","#286192"), color = "#1e496d") +
      coord_flip() +
      geom_text(aes(label = paste(round(..count../nrow(rv$dataset)*100,2), "%")),
                stat = "count", position = "dodge", hjust = -0.1) +
      labs(title = "All Customers", fill = '')+
      theme_minimal() +
      theme(legend.position = "none",
            axis.ticks=element_blank(),
            axis.title=element_blank(),
            panel.grid=element_blank(),
            panel.border=element_blank())
    p2 <- ggplot(rv$retained, aes(x=PhoneServ, fill = factor(PhoneServ)))+
      geom_bar(position = "dodge",fill = c("#addbad","#7cc67c", "#499349"), color = ("#376e37")) +
      coord_flip() +
      geom_text(aes(label = paste(round(..count../nrow(rv$retained)*100,2), "%")),
                stat = "count", position = "dodge",hjust = -0.1) +
      labs(title = "Customers Retained", fill = '')+
      theme_minimal() +
      theme(legend.position = "none",
            axis.text.x = element_text(size = 10),
            axis.ticks=element_blank(),
            axis.title=element_blank(),
            panel.grid=element_blank(),
            panel.border=element_blank())
    p3 <- ggplot(rv$churn, aes(x=PhoneServ, fill = factor(PhoneServ)))+
      geom_bar(position = "dodge", fill = c("#f7d6a6","#f3bd71","#d89b46"), color = "#ba7412") +
      coord_flip() +
      geom_text(aes(label = paste(round(..count../nrow(rv$churn)*100,2), "%")),
                stat = "count", position = "dodge", hjust = -0.1) +
      labs(title = "Customers Churned", fill = '')+
      theme_minimal() +
      theme(legend.position = "none",
            axis.ticks=element_blank(),
            axis.title=element_blank(),
            panel.grid=element_blank(),
            panel.border=element_blank())
    plot11 <- grid.arrange(p1, p2, p3, ncol = 1)
    plot11
  })
  output$plot12 <- renderPlot({
    p1 <- ggplot(rv$dataset, aes(x=Streaming, fill = factor(Streaming)))+
      geom_bar(position = "dodge",  fill = c("#99bcdb","#5b94c5","#337ab7","#286192"), color = "#1e496d") +
      coord_flip() +
      geom_text(aes(label = paste(round(..count../nrow(rv$dataset)*100,2), "%")),
                stat = "count", position = "dodge", hjust = -0.1) +
      labs(title = "All Customers", fill = '')+
      theme_minimal() +
      theme(legend.position = "none",
            axis.ticks=element_blank(),
            axis.title=element_blank(),
            panel.grid=element_blank(),
            panel.border=element_blank())
    p2 <- ggplot(rv$retained, aes(x=Streaming, fill = factor(Streaming)))+
      geom_bar(position = "dodge",fill = c("#addbad","#7cc67c", "#5cb85c", "#499349"), color = ("#376e37")) +
      coord_flip() +
      geom_text(aes(label = paste(round(..count../nrow(rv$retained)*100,2), "%")),
                stat = "count", position = "dodge",hjust = -0.1) +
      labs(title = "Customers Retained", fill = '')+
      theme_minimal() +
      theme(legend.position = "none",
            axis.text.x = element_text(size = 10),
            axis.ticks=element_blank(),
            axis.title=element_blank(),
            panel.grid=element_blank(),
            panel.border=element_blank())
    p3 <- ggplot(rv$churn, aes(x=Streaming, fill = factor(Streaming)))+
      geom_bar(position = "dodge",fill = c("#f7d6a6","#f3bd71","#f0ad4e", "#d89b46"), color = "#ba7412") +
      coord_flip() +
      geom_text(aes(label = paste(round(..count../nrow(rv$churn)*100,2), "%")),
                stat = "count", position = "dodge", hjust = -0.1) +
      labs(title = "Customers Churned", fill = '')+
      theme_minimal() +
      theme(legend.position = "none",
            axis.ticks=element_blank(),
            axis.title=element_blank(),
            panel.grid=element_blank(),
            panel.border=element_blank())
    plot12 <- grid.arrange(p1, p2, p3, ncol = 1)
    plot12
  })
}

# Run the application 
shinyApp(ui, server)

