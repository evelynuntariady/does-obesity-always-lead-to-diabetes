#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#
#install.packages("shinyjs")
library(shiny)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(plotly)
library(shiny)
library(reshape2)
library(plotrix)
library(webshot)
library(markdown)
library(shinyjs)
library(htmltools)
library(shinyWidgets)
#setwd('C:/BINUS/SMS2/DataMining/AOL/AOL')
heatMap_dat <- read.csv("data_hitmap.csv", header = TRUE)
heatMap_dat <- heatMap_dat %>% 
  select(-X)

preg_dat <- read.csv("preg_data.csv", header = TRUE)
preg_dat <- preg_dat%>% 
  select(-X)

age_dat <- read.csv("age_data.csv", header = TRUE)
age_dat<- age_dat%>% 
  select(-X)

pedigree_dat <- read.csv("pedigree_data.csv", header = TRUE)
pedigree_dat <- pedigree_dat %>% 
  select(-X)

bmicat_dat <- read.csv("bmicat_data.csv", header = TRUE)
bmicat_dat <- bmicat_dat %>% 
  select(-X)

bmi_dat <- read.csv("bmi_data.csv", header = TRUE)
bmi_dat <- bmi_dat %>% 
  select(-X)
# Define UI for application that draws a histogram
ui <- fluidPage(
  tags$head(
    # Note the wrapping of the string in HTML()
    tags$style(HTML("
      @import url('https://fonts.googleapis.com/css2?family=Yusei+Magic&display=swap');
      @import url('https://fonts.googleapis.com/css2?family=Acme&display=swap');
      @import url('https://fonts.googleapis.com/css2?family=Acme&family=Josefin+Sans:ital,wght@0,100..700;1,100..700&display=swap');
      body {
        background-color: #D6E7D3;
        color: black;
      }
      h2 {
        font-family: 'Acme';
      }
      
      p{
        font-family: 'Josefin Sans';
      }
      
      li{
        font-family: 'Josefin Sans';
      }
      
      
      .navbar{
        background-color: #526C3F;
        font-color: white;
        margin: 0px;
        padding: 0px;
        
      }
      .navbar-default .navbar-nav > li > a {
        color: white !important;
        font-family: 'Yusei Magic', sans-serif;
      }
   
      
       .navbar-default .navbar-nav > li.active > a{
        color: black !important;
        background-color: #E2E8D5 !important;
      }
      "))
  ),
  
  navbarPage(title =  span( "Does Obesity Always Lead to Diabetes?",
                            style = "background-color: #526C3F;
                            color: white;
                            font-family: 'Yusei Magic';
                            "),
             id = "navbar",
             
             tabPanel("Diabetes",
                      sidebarLayout(
                          includeMarkdown("Description_definition.md"),
                          
                        
                        mainPanel(
                          plotlyOutput("deathrate"),
                          includeMarkdown("Description_factors.md"),
                            plotlyOutput("heatMap"),
                            includeMarkdown("Description_top3.md"),
                           #plotlyOutput("glucose"),
                           selectInput(inputId = "age_input",
                                       label = "Choose Age Category",
                                       choices = age_dat$Age_Category,
                                       selected = "> 65",
                                       multiple = TRUE),
                           plotlyOutput("age"),
                           includeMarkdown("Desc_age.md"),
                           selectInput(inputId = "pedigree_input",
                                       label = "Choose BMI Category",
                                       choices = pedigree_dat$BMI_Category,
                                       selected = "Obese",
                                       multiple = TRUE),
                           plotlyOutput("pedigree"),
                           includeMarkdown("Desc_pedigree.md"),
                          plotlyOutput("preg")
                        )
                        
                      )
             ),
             
             tabPanel(
               "Pima Indians",
               sidebarLayout(
                 div(
                 includeMarkdown("Description_pima.md"),
                 selectInput(inputId = "bmi_input",
                             label = "Select BMI Category",
                             choices = bmi_dat$BMI_Category,
                             selected = "Obese",
                             multiple = TRUE)
                 ),
                 mainPanel(
                   plotlyOutput("Bmi"),
                   includeMarkdown("Desc_bmi.md"),
                   plotlyOutput("Bmi_category"),
                   includeMarkdown("Desc_obes.md")
                 )
               )
             ),
             tabPanel(
               "Conclusion",
               sidebarLayout(
                 div(
                   includeMarkdown("Description_conclusion.md")
                 ),
                 mainPanel()
               )
             )
           
            
             
             
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  output$deathrate <- renderPlotly({
    # Create the data frame
    DeathRate_Causes <- data.frame(
      Causes = c("Cardiovascular diseases", "Cancer", "Hypertension", "Air pollution", "Chronic obstructive respiratory diseases", "Alcohol", "Pneumonia", "Diabetes", "Tuberculosis", "Road traffic accidents"),
      Deaths = c(17.9, 9.6, 7.5, 7, 3.17, 3, 2.56, 1.6, 1.5, 1.35)
    )
    
    # Sort the data frame by Deaths in descending order
    DeathRate_Causes_Sort <- DeathRate_Causes[order(DeathRate_Causes$Deaths, decreasing = TRUE),]
    
    DeathRate_Causes_Sort
    
    unique_causes <- unique(DeathRate_Causes_Sort$Causes)
    
    deathrate_plot <- ggplot(DeathRate_Causes_Sort, aes(y = reorder(Causes, Deaths), x = Deaths, fill = as.factor(Causes))) +
      geom_col(position = "dodge", color = "black", width = 0.7) +
      labs(title = "Death Rates by Causes",
           x = "Number of Deaths (in mill)",
           y = "Causes") +
      scale_fill_manual(
        values = c(
          "Cardiovascular diseases" = "#C4C4C3",
          "Cancer" = "#C4C4C3",
          "Hypertension" = "#C4C4C3",
          "Air pollution" = "#C4C4C3",
          "Chronic obstructive respiratory diseases" = "#C4C4C3",
          "Alcohol" = "#C4C4C3",
          "Pneumonia" = "#C4C4C3",
          "Diabetes" = "#E16463",
          "Tuberculosis" = "#C4C4C3",
          "Road traffic accidents" = "#C4C4C3"
        )
      ) +
      theme(
        panel.background = element_rect(fill = "white"),
        legend.position = "none"
      )
    ggplotly(deathrate_plot)
  })
  
  
  
  
  output$heatMap <- renderPlotly({
    
    correlation_matrix2 <- cor(heatMap_dat[, c("SkinThickness", "BloodPressure", "Insulin", "BMI", "Pregnancies", "DiabetesPedigreeFunction", "Age", "Outcome")])
    
    # Menghitung korelasi dengan variabel "Outcome"
    outcome_correlations <- correlation_matrix2["Outcome",]
    
    # Urutkan nama variabel berdasarkan korelasi dengan "Outcome"
    ordered_vars <- names(sort(outcome_correlations, decreasing = TRUE))
    
    # Urutkan matriks korelasi berdasarkan urutan variabel
    ordered_correlation_matrix2 <- correlation_matrix2[ordered_vars, ordered_vars]
    
    # Membuat heatmap korelasi dengan urutan yang sudah diatur
    temp <- ggplot(data = melt(ordered_correlation_matrix2), aes(Var1, Var2, fill = value)) +
      geom_tile() +
      labs(fill = "Correlation") +
      scale_fill_gradient(low = "#FFF3DD", high = "#F0A829") +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.background = element_rect(fill = "white")
      )+
      labs(title = "Factors that causes diabetes",
           x = '',
           y = "")
    
    ggplotly(temp)
  })
  

  
  
  
  
  output$age <- renderPlotly({
    observeEvent(
      input$age_input,
      updatePickerInput(session = session,
                        inputId = "age",
                        label = "Choose Age Category",
                        choices =age_dat$Age_Category)
    )
    
    chosen_age <- reactive({
      input$age_input
    })
    
    filtered_age_data <- age_dat %>% 
      filter(Age_Category %in% chosen_age())
    
      top3_age <- ggplot(filtered_age_data, aes(x = Age_Category, y = Percentage, fill = Combined)) +
        geom_col() +
        labs(
          x = "Age Categories",
          y = "Percentage"
        ) +
        labs(title = "Percentage of Diabetic Status for Each Age Category")+
        scale_fill_manual(values = c(
          "20 - 35 Diabetic" = "#F0A829", "20 - 35 Non-Diabetic" = "#F8DFB2",
          "36 - 50 Diabetic" = "#F0A829", "36 - 50 Non-Diabetic" = "#F8DFB2",
          "51 - 65 Diabetic" = "#F0A829", "51 - 65 Non-Diabetic" = "#F8DFB2",
          "> 65 Diabetic" = "#F0A829", "> 65 Non-Diabetic" = "#F8DFB2"
        ),
        name = "Age Category") +
        guides(fill = FALSE)+
        theme(
          panel.background = element_rect(fill = "white", color = NA),
          plot.background = element_rect(fill = "white", color = NA),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_line(color = "black"),
          legend.background = element_rect(fill = "white"),
          legend.key = element_rect(fill = "white")
        )
      ggplotly(top3_age)
  })
      
  output$pedigree <- renderPlotly({
    observeEvent(
      input$pedigree_input,
      updatePickerInput(session = session,
                        inputId = "pedigree",
                        label = "Choose BMI Category",
                        choices =pedigree_dat$BMI_Category)
    )
    
    chosen_pedigree <- reactive({
      input$pedigree_input
    })
    
    filtered_pedigree_data <- pedigree_dat %>% 
      filter(BMI_Category %in% chosen_pedigree())
    
      top3_pedigree <-  ggplot(filtered_pedigree_data, aes(x = BMI_Category, y = Average, fill = Combined)) +
        geom_col(position = "dodge", width = 0.7) +
        geom_text(aes(label = round(Average, 2)),  # Adding text on top of the bars
                  position = position_dodge(width = 0.7), 
                  vjust = -0.5) +
        labs(
          title = "Diabetic Percentage based on BMI Category and Genetic Factor Index", 
          x = "BMI Category", 
          y = "Genetic Factor Index", 
          fill = "Diabetes Status"
        ) +
        scale_fill_manual(
          values = c(
            "Obese Diabetic" = "#F0A829", "Obese Non-Diabetic" = "#F8DFB2",
            "OverWeight Diabetic" = "#F0A829", "OverWeight Non-Diabetic" = "#F8DFB2",
            "Normal Weight Diabetic" = "#F0A829", "Normal Weight Non-Diabetic" = "#F8DFB2",
            "Underweight Diabetic" = "#F0A829", "Underweight Non-Diabetic" = "#F8DFB2"
          )
        ) +
        guides(fill = FALSE)+
        theme(
          panel.background = element_rect(fill = "white")
        )
      
      ggplotly(top3_pedigree)
  })
  
  output$glucose <- renderPlotly({
    top3_glucose <- ggplot(glucose_dat, aes(x = Outcome_Category,
                                                        y = glucose_average,
                                                        fill = Outcome_Category))+
      geom_col()+
      labs(title = "Average Glucose Levels for Each Diabetes Outcome Category",
           x = "Diabetes Outcome",
           y = "Glucose Levels")+
      scale_fill_manual(values = c("Diabetic" = "#F0A829",
                                   "Non-Diabetic" = "#F8DFB2"),
                        name = "Diabetes Outcome Category")+
      theme(
        panel.background = element_rect(fill = "white")
      )
    ggplotly(top3_glucose)
  })
  
  
  
  output$Bmi <- renderPlotly({
    observeEvent(
      input$bmi_input,
      updatePickerInput(session = session,
                        inputId = "Bmi",
                        label = "Select BMI Category",
                        choices =bmi_dat$BMI_Category)
    )
    
    chosen_bmi <- reactive({
      input$bmi_input
    })
    
    filtered_bmi_data <- bmi_dat %>% 
      filter(BMI_Category %in% chosen_bmi())
    
    Bmi_plot <- ggplot(filtered_bmi_data, aes(x = BMI_Category, y = Percentage, fill = BMI_Category))+
      geom_col()+
      labs(title = "BMI Distribution for Pima Indian  Women",
           x = "BMI Category",
           y = "Percentage")+
      theme_minimal()+
      scale_fill_manual(
        values = c(
          "Obese" = "#F0A829",
          "Normal Weight" =  "#F8DFB2",
          "OverWeight" =  "#F8DFB2",
          "Underweight" =  "#F8DFB2"
        )
      )+
      guides(
        fill = FALSE
      )
    ggplotly(Bmi_plot)
  })
  
  output$Bmi_category <- renderPlotly({
    plot_ly(data = bmicat_dat, 
            type = 'pie', 
            values = ~percent, 
            labels = ~Outcome_Category, 
            textinfo = 'label+percent', 
            hole = 0.4) %>%
      layout(title = "Pie Chart of Obesity Diabetic Status")
  })
  
  output$preg <- renderPlotly({
    preg_plot<- ggplot(preg_dat, aes(x = Outcome_Category, y =aver, fill = Outcome_Category))+
      geom_col()+
      labs( y = "Average Pregnancies Count",
            x = "Diabetes Status")+
      scale_fill_manual(values = c("Diabetic" = "#F0A829",
                                   "Non-Diabetic" = "#F8DFB2"))+
      theme(
        panel.background = element_rect(fill = "white")
      )
    ggplotly(preg_plot)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)



