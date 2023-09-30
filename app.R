#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(tidyverse)
library(ggplot2)
library(plotly)
library(DT)
library(dplyr)

data = read.csv('data2902.csv')
data_chr = c('Task Approach' = 'task_approach', 
             'Attitude Toward Life' = 'life', 
             'Gender' = 'gender_clean', 
             'UoS from Faculty of Arts and Social Sciences' = 'fass_unit', 
             'Major from Faculty of Arts and Social Sciences' = 'fass_major', 
             'Reading Novel' = 'novel', 
             'Favourite Library' = 'library', 
             'Private Health Insurance' = 'private_health', 
             'Paying Rent' = 'rent', 
             'Urinal Position' = 'urinal_position', 
             'Stall Position' = 'stall_position', 
             'Type of Social Media' = 'social_media_clean', 
             'Diet Style' = 'diet', 
             'Steak Preference' = 'steak_preference', 
             'Pineapple on Pizza' = 'pineapple', 
             'Dominant Hand' = 'dominant_hand', 
             'Normal or Advanced Class' = 'normal_advanced', 
             'Experience on R' = 'used_r_before', 
             'Year of university currently in' = 'uni_year')
data_num = c('Number of units this semester' = 'n_units', 
             'Years old' = 'age', 
             'Consumption of Sugar (Day/Week)' = 'sugar_days_clean', 
             'Food Budget' = 'food_budget', 
             'Height (cm)' = 'height_clean', 
             'Studying Hours (Hours/Day)' = 'study_hrs', 
             'Sleeping Time (Hours/Day)' = 'sleep_time_clean', 
             'Exercise Hours (Hours/Week)' = 'exercise_hrs', 
             'Employment Hours (Hours/Week)' = 'employment_hrs', 
             'Social Media Hours (Hours/Day)' = 'social_media_hrs', 
             'WAM' = 'wam')

# Define UI for application that draws a histogram
ui <- fluidPage(
    withMathJax(),
    theme = shinytheme("flatly"),

    # Application title
    navbarPage(
        title = 'DATA2902 SURVEY DATA ANALYSIS',
        tabPanel('Dataset', DT::dataTableOutput("data")),
        tabPanel('Chi-Squared Test for Independence',
           sidebarLayout(
             sidebarPanel(
               selectInput("c1", "Category Variable 1", data_chr, selected  = "task_approach"),
               selectInput("c2", "Category Variable 2", data_chr, selected = "life"),
               checkboxInput("monte", "Monte Carlo Simulation", FALSE),
               checkboxInput("correct", "Continuity Correction", FALSE)
             ),
             mainPanel(
               tabsetPanel(
                 tabPanel("Visualisation",
                    br(),
                    plotOutput("hist")
                 ), 
                 tabPanel("Test",
                    h4('Hypothesis'),
                    uiOutput('hypo'),
                    br(),
                    h4('Contingency Table'),
                    verbatimTextOutput('contingencyTable'),
                    br(),
                    h4('Assumption'),
                    verbatimTextOutput('exp'),
                    textOutput('ass'),
                    br(),
                    h4('Chi Square Test'),
                    verbatimTextOutput('chiTest'),
                    br(),
                    h4('Conclusion'),
                    textOutput("checkpchi"),
                    br(),
                    br()
                  )
               )
             )
           )
        ),
        tabPanel('Two Sample t-Test',
           sidebarLayout(
             sidebarPanel(
               selectInput("n1", "Quantitative Variable", data_num, selected = "height_clean"),
               selectInput("n2", "Catgorical Variable", data_chr, selected = "dominant_hand"),
               selectInput("g1", "Categorical Sample 1", choices = NULL), 
               selectInput("g2", "Categorical Sample 2", choices = NULL),
               selectInput("visual", "Plot to check Assumption", choices = c("Box Plot", "QQ Plot")),
               selectInput("alter", "Alternative Hypothesis", choices = c('Greater', 'Less', 'Two-sided')),
               checkboxInput("welch", "Welch", FALSE)
             ),
             mainPanel(
               tabsetPanel(
                 tabPanel("Visualisation",
                    br(),
                    h4('Assumption'),
                    textOutput("ass2"),
                    br(),
                    plotOutput('plot')
                 ), 
                 tabPanel("Test",
                    h4('Hypothesis'),
                    textOutput('hypottest'),
                    uiOutput('ttest'),
                    br(),
                    verbatimTextOutput('tTestResult'),
                    br(),
                    h4('Conclusion'),
                    textOutput('checkttest')
                  )
               )
             )
           )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    tab <- reactive({
      table(data[[input$c1]], data[[input$c2]])
    })

    output$data = DT::renderDataTable({
      data
    })
    
    output$contingencyTable = renderPrint({
      tab_result <- tab()
      tab_result
    })
    
    
    output$chiTest = renderPrint({
      if (input$monte && input$correct) {
        chisq.test(tab(), simulate.p.value = TRUE, correct = TRUE)
      } else if (input$monte) {
        chisq.test(tab(), simulate.p.value = TRUE, correct = FALSE)
      } else if (input$correct) {
        chisq.test(tab(), simulate.p.value = FALSE, correct = TRUE)
      } else {
        chisq.test(tab(), simulate.p.value = FALSE, correct = FALSE)
      }
    })
    
    chisqtest <- reactive({
      chisq.test(tab())
    })
    
    output$checkpchi = renderText ({
      if (chisqtest()$p.value < 0.05) {
        paste("Since p-value = ", round(chisqtest()$p.value, 3), " is less than 0.05. We reject the null hypothesis.") 
      } else {
        paste("Since p-value = ", round(chisqtest()$p.value, 3), " is greater than 0.05. We fail to reject the null hypothesis.") 
      }
    })
    
    expectbool <- reactive({
      chisqtest()$expected
    })
    
    output$exp = renderPrint({
      chisqtest()$expected
    })
    
    output$ass = renderText({
      if (all(expectbool() > 5)) {
        "Assumption is met since all expected counts are greater than 5"
      } else {
        "Assumption is not met since all/some expected counts are less than 5"
      }
    })
    
    output$ass2 = renderText({
      "To assess the assumption that two samples are both independent and identically distributed, we will investigate their distribution in the plot."
    })
    
    
    output$hypo <- renderUI({
      withMathJax(
        HTML(paste('\\(H_{0}\\):<b>', x1_var_name(), '</b>is independent from<b>', legend_var_name(), '</b>')),
        br(),
        HTML(paste('\\(H_{1}\\):<b>', x1_var_name(), '</b>is dependent on<b>', legend_var_name(), '</b>'))
      )
    })
    
    observe({
      selected_var = input$n2
      
      unique_level = unique(data[[selected_var]])
      unique_level = unique_level[!is.na(unique_level)]
      
      updateSelectInput(session, "g1", choices = unique_level)
      updateSelectInput(session, "g2", choices = unique_level)
    })
    
    data_plot = reactive({
      data |> 
        filter (.data[[input$n2]] %in% c(input$g1, input$g2)) |> 
        select(.data[[input$n1]], .data[[input$n2]])
    })
    
    
    output$plot = renderPlot({
      if (input$visual == "Box Plot") {
          ggplot(data_plot()) + aes(x = .data[[input$n2]], y = .data[[input$n1]], fill = .data[[input$n2]]) +
          geom_boxplot() +
          labs(x = x_var_name(), y = y_var_name(), fill = '') +
          ggtitle(paste("Box Plot of", x_var_name(), "and", y_var_name())) +
          theme(plot.title = element_text(hjust = 0.5, face = "bold"))
      } else {
        ggplot(data_plot()) + 
          aes(sample = .data[[input$n1]], color = .data[[input$n2]]) +
          geom_qq() +
          geom_qq_line() +
          facet_wrap(~ .data[[input$n2]]) +
          labs(x = "Theoretical", y = "Sample", color = "") +
          scale_color_manual(values = c("indianred1", "dodgerblue2")) +
          ggtitle("QQ Plot") +
          theme(plot.title = element_text(hjust = 0.5, face = "bold"))
      }
        
    })
    
    
    data2_plot = reactive ({
      data |> select(.data[[input$c1]], .data[[input$c2]]) |> drop_na()
    })
    
    output$hist = renderPlot({
      ggplot(data2_plot()) +
        aes(x = .data[[input$c1]], fill = .data[[input$c2]]) +
        geom_bar(linewidth = 2) +
        labs(x = x1_var_name(), y = 'Frequency', fill = legend_var_name()) +
        ggtitle(paste("Bar chart of", x1_var_name(), "filled by", legend_var_name())) +
        theme(plot.title = element_text(hjust = 0.5, face = "bold"))
    })
    
    output$hypottest = renderText({
      "Let A be the categorical sample 1, B be the categorical sample 2"
    })
    
    output$ttest <- renderUI({
      withMathJax(
        HTML(paste('\\(H_{0}\\): \\(&mu;_A\\) = \\(&mu;_B\\)')),
        br(),
        if (input$alter == "Greater") {
          HTML(paste('\\(H_{1}\\): \\(&mu;_A\\) > \\(&mu;_B\\)'))
        } else if (input$alter == "Less") {
          HTML(paste('\\(H_{1}\\): \\(&mu;_A\\) < \\(&mu;_B\\)'))
        } else {
          HTML(paste('\\(H_{1}\\): \\(&mu;_A\\) &ne; \\(&mu;_B\\)'))
        }
      )
    })
    
    t_test_result <- reactive({
      g1 <- data |> filter (.data[[input$n2]] %in% c(input$g1)) |> select(.data[[input$n1]])
      g2 <- data |> filter (.data[[input$n2]] %in% c(input$g2)) |> select(.data[[input$n1]])
      alternative <- input$alter
      
      if (input$welch) {
        if (alternative == "Greater") {
          t.test(g1, g2, alternative = "greater", var.equal = FALSE)
        } else if (alternative == "Less") {
          t.test(g1, g2, alternative = "less", var.equal = FALSE)
        } else {
          t.test(g1, g2, alternative = "two.sided", var.equal = FALSE)
        }
      } else {
        if (alternative == "Greater") {
          t.test(g1, g2, alternative = "greater", var.equal = TRUE)
        } else if (alternative == "Less") {
          t.test(g1, g2, alternative = "less", var.equal = TRUE)
        } else {
          t.test(g1, g2, alternative = "two.sided", var.equal = TRUE)
        }
      }
      
    })
    
    output$tTestResult = renderPrint({
      t_test_result()
    })
    
    output$checkttest = renderText ({
      if (t_test_result()$p.value < 0.05) {
        paste("Since p-value = ", round(t_test_result()$p.value, 3), ", which is less than 0.05. We reject the null hypothesis.") 
      } else {
        paste("Since p-value = ", round(t_test_result()$p.value, 3), ", which is greater than 0.05. We fail to reject the null hypothesis.") 
      }
    })
    
    x_var_name = reactive(
      names(data_chr[data_chr==input$n2])
    )
    y_var_name = reactive(
      names(data_num[data_num==input$n1])
    )
    
    x1_var_name = reactive(
      names(data_chr[data_chr==input$c1])
    )
    legend_var_name = reactive(
      names(data_chr[data_chr==input$c2])
    )
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
