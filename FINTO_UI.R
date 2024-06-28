###############################################################################
#
# Settings                                      
#
##############################################################################

# Set CRAN mirror
chooseCRANmirror(graphics = FALSE, ind = 1)

# Install required packages
install.packages("htmltools")
install.packages("sass")
install.packages("bslib")
install.packages("shiny")
install.packages("shinydashboard")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("lubridate")
install.packages("lubridate")

# Load the installed packages
library(htmltools)
library(sass)
library(bslib)
library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(lubridate)
library(DT)

# Setting the working directory
setwd("/Users/liahruby/Desktop/BAM RSM/Period 5/FinTech_Business Models & Applications/Exams/AS 2") # please make sure to link your working directory here
getwd()


###############################################################################
#
# Start
#
##############################################################################

# Generate pseudodata for transactions
generate_transactions <- function(n = 100) {
  set.seed(123)  # Ensuring reproducibility
  dates <- as.Date('2024-01-01') + sample(0:365, n, replace = TRUE)
  descriptions <- sample(c("Grocery", "Rent", "Accenture", "Utilities", "Entertainment"), n, replace = TRUE, prob = c(0.2, 0.2, 0.1, 0.25, 0.25))
  categories <- ifelse(descriptions == "Accenture", "Income", sample(c("Food", "Housing", "Utilities", "Entertainment"), n, replace = TRUE))
  amounts <- ifelse(descriptions == "Accenture", 4000, round(runif(n, -500, -10), 2))
  
  # Income is on the same date every month and descriptions 
  income_dates <- seq(as.Date('2024-01-31'), length.out = length(which(descriptions == "Accenture")), by = "month")
  dates[descriptions == "Accenture"] <- income_dates
  
  data.frame(
    Date = dates,
    Description = descriptions,
    Category = categories,
    Amount = amounts
  )
}

# Generate pseudodata for budgeting over the last three years
generate_budget_data <- function() {
  set.seed(123)
  months <- seq.Date(from = as.Date("2021-01-01"), to = as.Date("2023-12-01"), by = "month")
  incomes <- rep(4000, length(months))  # Normalized income
  expenses <- round(runif(length(months), 1500, 4200), 2)  
  
  data.frame(
    Date = months,
    Income = incomes,
    Expenses = expenses
  )
}

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "FINTO - Personal Finance Manager"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Transactions", tabName = "transactions", icon = icon("list")),
      menuItem("Financial Advice", tabName = "advice", icon = icon("lightbulb")),
      menuItem("Budget Planning", tabName = "budget", icon = icon("chart-line"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "dashboard",
              fluidRow(
                box(title = "Welcome to FINTO!", status = "primary", solidHeader = TRUE, width = 12,
                    "Manage your personal finances efficiently with real-time monitoring, AI-driven advice, and comprehensive budgeting tools.")
              ),
              fluidRow(
                valueBoxOutput("total_income", width = 4),
                valueBoxOutput("total_expenses", width = 4),
                valueBoxOutput("net_savings", width = 4)
              ),
              fluidRow(
                box(title = "Notifications", status = "warning", solidHeader = TRUE, width = 12,
                    textOutput("notifications"))
              )
      ),
      tabItem(tabName = "transactions",
              fluidRow(
                box(title = "Enter Transaction", status = "primary", solidHeader = TRUE, width = 6,
                    textInput("description", "Description"),
                    selectInput("category", "Category", choices = c("Food", "Housing", "Income", "Utilities", "Entertainment")),
                    numericInput("amount", "Amount", value = 0, min = -Inf, max = Inf, step = 0.01),
                    actionButton("add_transaction", "Add Transaction")
                ),
                box(title = "Your Investments", status = "info", solidHeader = TRUE, width = 6,
                    DTOutput("investmentsTable"))
              ),
              fluidRow(
                box(title = "Transaction History", status = "primary", solidHeader = TRUE, width = 12,
                    dateRangeInput("date_range", "Filter by Date Range", start = min(generate_transactions()$Date), end = max(generate_transactions()$Date)),
                    textInput("search_description", "Search by Description"),
                    DTOutput("transaction_table"),
                    plotOutput("category_plot")
                )
              )
      ),
      tabItem(tabName = "advice",
              fluidRow(
                box(title = "AI-Driven Financial Advice", status = "primary", solidHeader = TRUE, width = 9,
                    textOutput("financial_advice"),
                    tags$style(type = "text/css", "#financial_advice { white-space: pre-wrap; }")
                ),
                box(title = "Featured ETFs", status = "info", solidHeader = TRUE, width = 3,
                    "1. S&P 500 ETF: Tracks top 500 companies.\n2. Tech Growth ETF: Focuses on high-growth tech sector.\n3. Green Energy ETF: Invests in renewable energy companies.")
              ),
              fluidRow(
                box(title = "Ask a Question", status = "primary", solidHeader = TRUE, width = 12,
                    textInput("user_question", "Your Question"),
                    actionButton("ask_question", "Ask"),
                    textOutput("chatbot_response"),
                    tags$style(type = "text/css", "#chatbot_response { white-space: pre-wrap; }")
                )
              )
      ),
      tabItem(tabName = "budget",
              fluidRow(
                box(title = "Budget Overview", status = "primary", solidHeader = TRUE, width = 12,
                    plotOutput("budget_plot"),
                    tableOutput("budget_table")
                ),
                box(title = "Set Budget Goal", status = "primary", solidHeader = TRUE, width = 12,
                    numericInput("budget_goal", "Budget Goal", value = 1000, min = 0, step = 1),
                    numericInput("amount_spent", "Amount Spent", value = 0, min = 0, step = 1),
                    actionButton("update_budget", "Update Budget")
                )
              )
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  transactions <- reactiveVal(generate_transactions(100))
  
  observeEvent(input$add_transaction, {
    new_transaction <- data.frame(
      Date = Sys.Date(),
      Description = input$description,
      Category = input$category,
      Amount = ifelse(input$category == "Income", input$amount, -abs(input$amount))
    )
    transactions(rbind(transactions(), new_transaction))
  })
  
  filtered_transactions <- reactive({
    trans <- transactions()
    if (!is.null(input$date_range)) {
      trans <- trans %>%
        filter(Date >= input$date_range[1] & Date <= input$date_range[2])
    }
    if (input$search_description != "") {
      trans <- trans %>%
        filter(grepl(input$search_description, Description, ignore.case = TRUE))
    }
    trans
  })
  
  output$transaction_table <- renderDT({
    datatable(filtered_transactions(), options = list(pageLength = 10))
  })
  
  output$category_plot <- renderPlot({
    trans <- filtered_transactions() %>%
      group_by(Category) %>%
      summarize(Total = sum(Amount))
    
    ggplot(trans, aes(x = Category, y = Total, fill = Category)) +
      geom_bar(stat = "identity") +
      labs(title = "Total Transactions by Category", x = "Category", y = "Total Amount") +
      theme_minimal()
  })
  
  output$investmentsTable <- renderDT({
    investments <- data.frame(
      Date = Sys.Date() + sample(1:365, 10, replace = TRUE),
      Description = sample(c("S&P 500 ETF", "Tech Growth ETF", "Green Energy ETF"), 10, replace = TRUE),
      Amount = round(runif(10, 1000, 5000), 2)
    )
    datatable(investments, options = list(pageLength = 5))
  })
  
  output$total_income <- renderValueBox({
    total_income <- transactions() %>% filter(Category == "Income") %>% summarize(Total = sum(Amount)) %>% .$Total
    valueBox(
      value = paste0("€", format(total_income, big.mark = ",")),
      subtitle = "Total Income",
      icon = icon("arrow-up"),
      color = "green"
    )
  })
  
  output$total_expenses <- renderValueBox({
    total_expenses <- transactions() %>% filter(Category != "Income") %>% summarize(Total = sum(Amount)) %>% .$Total
    valueBox(
      value = paste0("€", format(abs(total_expenses), big.mark = ",")),
      subtitle = "Total Expenses",
      icon = icon("arrow-down"),
      color = "red"
    )
  })
  
  output$net_savings <- renderValueBox({
    total_income <- transactions() %>% filter(Category == "Income") %>% summarize(Total = sum(Amount)) %>% .$Total
    total_expenses <- transactions() %>% filter(Category != "Income") %>% summarize(Total = sum(Amount)) %>% .$Total
    net_savings <- total_income + total_expenses
    valueBox(
      value = paste0("€", format(net_savings, big.mark = ",")),
      subtitle = "Net Savings",
      icon = icon("piggy-bank"),
      color = "blue"
    )
  })
  
  output$notifications <- renderText({
    current_month_expenses <- transactions() %>%
      filter(Date >= as.Date(format(Sys.Date(), "%Y-%m-01")), Category != "Income") %>%
      summarize(Total = sum(Amount)) %>%
      .$Total
    budget_goal <- 5000
    if (abs(current_month_expenses) > 0.9 * budget_goal) {
      paste("Alert: You have reached 90% of your monthly budget limit of €", budget_goal, "!", sep = "")
    } else {
      "You are within your budget limits."
    }
  })
  
  output$financial_advice <- renderText({
    advice <- "Based on your current spending patterns, here are some tips to improve your financial health:\n\n"
    advice <- paste0(advice, "- Consider reducing discretionary expenses, such as entertainment and dining out.\n")
    advice <- paste0(advice, "- Increase your savings rate to 10% of your salary to build an emergency fund.\n")
    advice <- paste0(advice, "- Review your recurring expenses and look for opportunities to save.\n")
    advice <- paste0(advice, "- Invest in low-cost index funds for long-term growth.\n")
    advice
  })
  
  observeEvent(input$ask_question, {
    user_question <- input$user_question
    response <- ""
    
    if (grepl("ETF", user_question, ignore.case = TRUE)) {
      response <- "ETFs (Exchange Traded Funds) are a type of investment fund that are traded on stock exchanges, similar to stocks. They can offer diversification and lower fees compared to mutual funds."
    } else {
      response <- "I'm here to help with your financial questions. Please ask about investments, budgeting, or any financial topic."
    }
    
    output$chatbot_response <- renderText({ response })
  })
  
  budget_data <- reactiveVal(generate_budget_data())
  
  observeEvent(input$update_budget, {
    budget_data(rbind(budget_data(), data.frame(
      Date = Sys.Date(),
      Income = 0,
      Expenses = input$amount_spent
    )))
  })
  
  output$budget_plot <- renderPlot({
    budget <- budget_data()
    ggplot(budget, aes(x = Date)) +
      geom_line(aes(y = Income, color = "Income")) +
      geom_line(aes(y = Expenses, color = "Expenses")) +
      geom_hline(aes(yintercept = input$budget_goal), color = "blue", linetype = "dashed") +
      labs(title = "Budget Overview", x = "Date", y = "Amount") +
      theme_minimal() +
      scale_color_manual(values = c("Income" = "green", "Expenses" = "red"))
  })
  
  output$budget_table <- renderTable({
    budget_data()
  })
}

# Run the application
shinyApp(ui = ui, server = server)














