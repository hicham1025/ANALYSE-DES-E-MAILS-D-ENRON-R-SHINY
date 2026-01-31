#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(tidyverse)
library(lubridate)
library(ggplot2)

# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("Enron Email Dashboard"),
  dateRangeInput("date_range", "Filter by Date Range:",
                 start = as.Date("1999-01-01"),
                 end = as.Date("2002-12-31"),
                 min = as.Date("1999-01-01"),
                 max = as.Date("2002-12-31")),

  tabsetPanel(
    tabPanel("Top Senders", plotOutput("topSendersPlot")),
    tabPanel("Top Senders Named", plotOutput("topSendersNamedPlot")),
    tabPanel("Job Roles", plotOutput("jobRolesPlot")),
    tabPanel("Activity Timeline", plotOutput("activityTimelinePlot")),
    tabPanel("Recipients", plotOutput("recipientsPlot")),
    tabPanel("References", plotOutput("referencesPlot")),
    tabPanel("Content", plotOutput("subjectWordsPlot"))
    
    
  )
  
)



      


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
  
  filtered_data <- reactive({
  req(input$date_range)
  message %>%
    filter(date >= input$date_range[1],
           date <= input$date_range[2])
  })

  
  output$topSendersPlot <- renderPlot({
    filtered_data() %>%
      group_by(sender) %>%
      summarise(total_sent = n()) %>%
      arrange(desc(total_sent)) %>%
      head(10) %>%
      ggplot(aes(x = reorder(sender, total_sent), y = total_sent)) +
      geom_col(fill = "steelblue") +
      coord_flip() +
      labs(title = "Top 10 Email Senders",
           x = "Sender",
           y = "Number of Emails")
  })
  
  output$topSendersNamedPlot <- renderPlot({
    ggplot(top_senders_named, aes(x = reorder(paste(firstName, lastName), total_sent), y = total_sent)) +
      geom_col(fill = "steelblue") +
      geom_text(aes(label = total_sent), hjust = -0.1, size = 4) +  # This adds the number
      coord_flip() +
      labs(title = "Top 10 Email Senders (Named)",
           x = "Employee",
           y = "Number of Emails") +
      theme_minimal()
  })
  
  emails_by_role_filtered <- reactive({
    filtered_data() %>%
      left_join(employeelist, by = c("sender" = "Email_id")) %>%
      group_by(status) %>%
      summarise(total_sent = n()) %>%
      mutate(
        status = ifelse(is.na(status), "Unknown", as.character(status))
      ) %>%
      arrange(desc(total_sent))
  })
  
  
  
  
  
  output$jobRolesPlot <- renderPlot({
    emails_by_role_filtered() %>%
      ggplot(aes(x = reorder(status, total_sent), y = total_sent)) +
      geom_col(fill = "steelblue") +
      coord_flip() +
      labs(title = "Emails Sent by Job Role",
           x = "Job Role",
           y = "Number of Emails")
  })
  
  
  output$activityTimelinePlot <- renderPlot({
    ggplot(emails_by_day, aes(x = date, y = total_sent)) +
      geom_line(color = "blue") +
      labs(
        title = "Email Activity Over Time",
        x = "Date",
        y = "Number of Emails"
      )
  })
  
  output$recipientsPlot <- renderPlot({
    ggplot(recipients_per_message_filtered, aes(x = recipient_count)) +
      geom_histogram(binwidth = 1, fill = "steelblue", color = "white") +
      labs(
        title = "Recipients per Message (Filtered ≤ 30)",
        x = "Number of Recipients",
        y = "Count of Messages"
      )
  })
  
  output$referencesPlot <- renderPlot({
    ggplot(references_per_message, aes(x = reference_count)) +
      geom_histogram(binwidth = 1, fill = "steelblue", color = "white") +
      labs(
        title = "Distribution of References per Message",
        x = "Number of References",
        y = "Count of Messages"
      )
  })
  
  output$subjectWordsPlot <- renderPlot({
    ggplot(top_words, aes(x = reorder(word, n), y = n)) +
      geom_col(fill = "steelblue") +
      coord_flip() +
      labs(title = "Top 20 Words in Email Subjects",
           x = "Word",
           y = "Frequency")
  })
  
  
  
}


# Run the application 
shinyApp(ui = ui, server = server)


