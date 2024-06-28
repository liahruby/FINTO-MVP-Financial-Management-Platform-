# FINTO-MVP-Financial-Management-Platform

## Table of Contents
- [General Information](#general-information)
- [Installation using R (Studio)](#installation-using-r-studio)
- [Usage and structure of this MVP](#usage-and-structure-of-this-mvp)
- [Main functionality and architecture of this MVP](#main-functionality-and-architecture-of-this-mvp)

## General Information
This project is an example (MVP) of a personal finance management platform designed to empower users 
with comprehensive financial tools and personalized, real-time financial advice. FINTO integrates budgeting, 
investment tracking, and financial planning services into a single, user-friendly interface, leveraging advanced 
AI technology to provide tailored recommendations and insights - FINTO is the all-in-one solution.

## Installation using R (Studio)
To install and run FINTO locally, follow these steps:
1. Clone the repository:
   git clone [https://github.com/liahruby/FINTO.git](https://github.com/liahruby/FINTO-MVP-Financial-Management-Platform-.git)
   cd FINTO
2. Set the CRAN mirror and install the required packages:
   chooseCRANmirror(graphics = FALSE, ind = 1)
   install.packages(c("htmltools", "sass", "bslib", "shiny", "shinydashboard", "ggplot2", "dplyr", "lubridate", "DT"))
3. Load the installed packages:
   library(htmltools)
   library(sass)
   library(bslib)
   library(shiny)
   library(shinydashboard)
   library(ggplot2)
   library(dplyr)
   library(lubridate)
   library(DT)
4. Run the Shiny application:
   shinyApp(ui = ui, server = server)

## Usage and structure of this MVP
Dashboard:
- Provides a summary of total income, expenses, and net savings.
- Displays notifications related to your financial activities.

Transactions:
- Add new transactions with descriptions, categories, and amounts.
- View and filter transaction history with visualizations of spending by category.

Financial Advice:
- AI-driven financial advice based on spending patterns.
- Ask questions and get instant responses from the integrated chatbot.

Budget Planning:
- Set and track budget goals.
- Visualize budget overview and track expenses against goals.

## Main functionality and architecture of this MVP
FINTO is built using R and the Shiny framework, leveraging various packages for a rich user interface and robust server-side logic.

Programming Language: R

Main Frameworks: Shiny, ShinyDashboard, ggplot2, dplyr, lubridate, DT

Paradigm: Functional programming for UI and reactive programming for server logic.

UI Definition: Shiny's dashboardPage, dashboardHeader, dashboardSidebar, and dashboardBody components define the user interface.

Server Logic: The server logic uses reactive values and observers to manage the state and update UI components dynamically.

Data Management: Transactions and budget data are generated using custom functions and managed using reactive values.




