## app.R ##
library(bs4Dash)
library(shiny)
library(googlesheets4)
library(DT)
library(googledrive)
library(shinyalert)
library(ggplot2)
library(lubridate)
library(dplyr)
library(shinyjs)
library(waiter)
library(plotly)

#gs4_auth(cache = ".secrets",token = drive_token())
gs4_auth(cache = ".secrets",
         email = TRUE)

ui <- dashboardPage(dark = NULL,
  title = "Home Expense Dashboard",
  dashboardHeader(status = "gray-dark"),
  dashboardSidebar(status = "olive", minified = F,
    sidebarMenu(
      id = "tabs",
      menuItem(
        "Dashboard",
        tabName = "dashboard",
        icon = icon("tachometer-alt")
      ),
      menuItem("Data Table",
               tabName = "table",
               icon = icon("table")),
      menuItem("Data Entry", tabName = "data_entry", icon = icon("th")),
      menuItem(
        "Upload",
        tabName = "upload",
        icon = icon("cloud-upload-alt")
      )
    ),
    actionButton(
      inputId = "refresh",
      label = "Fetch/Refresh",
      icon = icon("sync"),
      class = "btn-info"
    )
  ),
  dashboardBody(
    tags$head(
      tags$link(rel = "shortcut icon", href = "https://w7.pngwing.com/pngs/692/588/png-transparent-house-home-business-house-angle-rectangle-triangle-thumbnail.png")
    ),
    useShinyjs(),
    autoWaiter(html = spin_2(), color = "white"),
    tabItems(
      # First tab content
      tabItem(
        tabName = "dashboard",
        fluidRow(
          valueBoxOutput(outputId = "current_expense_total", width = 6),
          valueBoxOutput(outputId = "max_expense_item", width = 6)
        ),
        fluidRow(
          box(
            height = "400px",
            width = 6,
            status = "info",
            solidHeader = T,
            title = "Expense Chart",
            selectInput(
              inputId = "month",
              label = "Select month",
              choices = month.name[1:month(Sys.Date())],
              selected = months(Sys.Date()),
              width = "50%"
            ),
            plotOutput(outputId = "item_expense", height = "250px")
          ),
          
          box(
            height = "400px",
            width = 6,
            status = "info",
            solidHeader = T,
            title = "Monthly Expense Trend",
            plotlyOutput(outputId = "trend", height = "330px")
          ),
        )
      ),
      
      # Second tab content
      tabItem(
        tabName = "table",
        box(solidHeader = T, title = "Monthly Expense Details",
          status = "info",
          width = 12,
          selectInput(
            inputId = "dt_month",
            label = "Select month",
            selected = months(Sys.Date()),
            choices = month.name[1:month(Sys.Date())],
            width = "50%"
          ),
          dataTableOutput(outputId = "data")
        )
      ),
      
      #Third tab
      tabItem(
        tabName = "data_entry",
        
        #Date selector
        box(
          width = 12,
          title = "Daily Expenses",
          solidHeader = T,
          status = "info",
          
          dateInput(
            inputId = "date",
            label = "Entry date",
            format = "yyyy/mm/dd",
            width = "50%"
          ),
          hr(),
          
          #Fixed expenses
          selectInput(
            inputId = "expenseItem",
            label = "Choose expense item",
            choices = c("Medicine",
                        "Grocery",
                        "Vegetable",
                        "Fruits",
                        "Others"),
            width = "50%"
          ),
          
          uiOutput(outputId = "customTextField"),
          
          numericInput(
            inputId = "expenseAmount",
            label = "Expense amount",
            value = 0
          ),
          
          actionButton(
            inputId = "addItem",
            label = "Add item",
            icon = icon("plus-circle"),
            class = "btn-secondary"
          ),
          actionButton(
            inputId = "preview",
            label = "Preview",
            icon = icon("eye"),
            class = "btn-warning"
          )
        )
      ),
      
      #Fourth tab
      tabItem(
        tabName = "upload",
        
        box(title = "Preview Data", solidHeader = T,
          width = 12,
          status = "info",
          dataTableOutput(outputId = "preview"),
          
          #Add record button
          actionButton(
            inputId = "add",
            label = "Add record",
            icon = icon("plus-circle"),
            class = "btn-success"
          )
        )
      )
    )
  )
)



server <- function(input, output, session) {
  shinyalert("Welcome!",
             "Click on Fetch/Refresh button to get the latest data.",
             type = "info",showConfirmButton = T,confirmButtonCol = "#5C8374")
  disable("add")
  onclick("preview", enable("add"))
  onclick("add", disable("add"))
  
  #  Retrieve excel sheet data ----------------------------------------------
  
  sheet_data0 <- function() {
    form = tryCatch({
      gs4_get(
        'https://docs.google.com/spreadsheets/d/16MP7ohlNQpgKG0fvEQifpGPEqWkMnTPQBb5sO9H2Qak/edit?usp=sharing'
      )
    }, error = function(e) {
      shinyalert("Oops!",
                 "Unable to fetch data! Please try again",
                 type = "error",confirmButtonCol = "#5C8374")
      NULL
    })
    
    if (is.null(form)) {
      return(data.frame(
        "Date" = Date(length = 0L),
        "Item" = character(0),
        "Price" = integer(0)
      ))
    }
    else{
      data <- read_sheet(form)
      data[[1]] <- as.Date(data[[1]])
      return(data)
    }
  }
  
  sheet_data <- reactive({
    input$refresh
    sheet_data0()
  })
  
  # Tab 1 codes -------------------------------------------------------------
  
  # Refresh button --- Refreshes the entire dashboard
  observeEvent(input$refresh, {
    #Expense bar plot
    output$item_expense <- renderPlot({
      month_data <-
        sheet_data()[months(sheet_data()[[1]]) %in% input$month,]
      if (nrow(month_data) == 0) {
        NULL
      }
      else{
        total_expense <-
          aggregate(month_data[[3]],
                    by = list(Items = month_data[[2]]),
                    FUN = sum)
        
        ggplot(total_expense, aes(x = Items, y = x)) + geom_bar(aes(fill = Items), stat = "identity", width = 0.5) + theme(axis.text.x = element_text(size = 10),
                                                                                                                           legend.position = "none") + geom_text(aes(label = x), vjust = -0.5, size = 4) + ylab("Expense Amount (in Rs.)") + theme(
                                                                                                                             panel.background = element_rect(fill = "transparent"),
                                                                                                                             plot.background = element_rect(fill = "transparent")
                                                                                                                           ) + labs(x = NULL) + ylim(0, max(total_expense$x + 100))
      }
    })
    
    #Expense trend line
    output$trend <- renderPlotly({
      month_expense <- sheet_data() %>%
        group_by(month = floor_date(Date, "month")) %>%
        summarize(Monthly_expense = sum(Price))
      if (nrow(month_expense)==0){
        NULL
      }
      else{
      tooltip_text <-
        paste(
          "Month:",
          format(month_expense$month, format = "%b, %Y")," ",
          "Total expense:",
          paste0("₹", format(month_expense$Monthly_expense, big.mark = ","))
        )
     
      plot_ly(
        month_expense,
        x = ~ month,
        y = ~ Monthly_expense,
        text = tooltip_text,
        hovertemplate = paste('%{text}'),
        type = "scatter",
        mode = "lines",
        name = " ",
        line = list(color = "#176B87")
      ) %>%
        layout(
          xaxis = list(
            title = "",
            showgrid = FALSE,
            zeroline = FALSE
          ),
          yaxis = list(
            title = "",
            showgrid = FALSE,
            zeroline = FALSE
          ),
          paper_bgcolor = "rgba(0,0,0,0)",
          # Transparent background
          plot_bgcolor = "rgba(0,0,0,0)"    # Transparent plot area
        ) 
      }
    })
    
    #Valuebox
    output$current_expense_total <- renderValueBox({
      month_data <-
        sheet_data()[months(sheet_data()[[1]]) %in% months(Sys.Date()),]
      total_expense <- sum(month_data[[3]])
      
      valueBox(
        value = h1(paste("₹", total_expense)),
        subtitle = paste("Total expense of", months(Sys.Date())),
        icon = icon("rupee-sign"),
        color = "teal"
      )
      
    })
    
    output$max_expense_item <- renderValueBox({
      month_data <-
        sheet_data()[months(sheet_data()[[1]]) %in% months(Sys.Date()),]
      if (nrow(month_data) == 0) {
        valueBox(
          value = "No Data Found",
          subtitle = "Highest expense item",
          icon = icon("shopping-bag"),
          color = "maroon"
        )
      }
      else{
        item_total_expense <-
          aggregate(month_data$Price,
                    by = list(items = month_data$Item),
                    FUN = sum)
        item_total_expense <-
          item_total_expense %>% filter(x == max(item_total_expense[[2]])) %>% select("items")
        item_total_expense <- unlist(item_total_expense)
        item_total_expense <-
          ifelse(length(item_total_expense) > 1,
                 "Multiple",
                 item_total_expense)
        
        
        valueBox(
          value = h1(item_total_expense),
          subtitle = "Highest expense item",
          icon = icon("shopping-bag"),
          color = "maroon"
        )
      }
      
    })
    
    # Data Table
    output$data <- renderDataTable({
      month_data <-
        sheet_data()[months(sheet_data()[[1]]) %in% input$dt_month,]
      if (nrow(month_data) == 0) {
        NULL
      }
      else{
        total_expense <-
          aggregate(month_data[[3]],
                    by = list(Items = month_data[[2]]),
                    FUN = sum)
        names(total_expense)[2] <- "Total expense"
        total_expense
      }
    })
  })
  
  # Tab 2 codes -------------------------------------------------------------
  
  #UIOutput for custom entries
  
  observe({
    if (input$expenseItem == "Others") {
      output$customTextField <- renderUI({
        textInput(inputId = "otherExpenseItem", label = "Expense item")
      })
    }
    else
      (output$customTextField <- renderUI({
        NULL
      }))
  })
  
  #Create blank dataframe
  createSheet <<- data.frame()
  
  #Create record for listed items
  record1 <<- reactive({
    NULL
  })
  
  observe({
    if (input$expenseItem != "Others") {
      record1 <<- reactive({
        rec <-
          list(
            Date = as.character(as.character(input$date)),
            Item = input$expenseItem,
            Price = input$expenseAmount
          )
        if (input$expenseAmount != 0 &&
            is.na(input$expenseAmount) == F)
          return(rec)
        else
          return(NULL)
      })
    }
    else
      record1 <<- reactive({
        NULL
      })
  })
  
  #Create record for custom items
  record2 <<- reactive({
    NULL
  })
  observe({
    if (input$expenseItem == "Others") {
      record2 <<- reactive({
        rec <-
          list(
            Date = as.character(as.character(input$date)),
            Item = input$otherExpenseItem,
            Price = input$expenseAmount
          )
        if (input$expenseAmount != 0 &&
            is.na(input$expenseAmount) == F)
          return(rec)
        else
          return(NULL)
      })
    }
    else
      record2 <<- reactive({
        NULL
      })
  })
  
  #Combine records with blank dataframe
  sheet <- function() {
    createSheet <<- rbind(createSheet, record1(), record2())
  }
  
  
  #Add item button adds entries one by one into the dataframe
  onclick(id = "addItem", expr = sheet())
  
  #Preview button previews entries made
  observeEvent(input$preview, {
    output$preview <- renderDataTable({
      createSheet
    })
    updateTabItems(session, "tabs", "upload")
  })
  
  # Tab 3 codes -------------------------------------------------------------
  
  #Add record to sheet
  observeEvent(input$add, {
    if (dim(createSheet)[1] == 0) {
      showModal(modalDialog(title = "Status", "Nothing to add. Please enter data first."))
    }
    else{
      form = tryCatch({
        gs4_get(
          'https://docs.google.com/spreadsheets/d/16MP7ohlNQpgKG0fvEQifpGPEqWkMnTPQBb5sO9H2Qak/edit?usp=sharing'
        )
      }, error = function(e) {
        NULL
      })
      if (is.null(form)) {
        shinyalert("Oops!",
                   "Something went wrong! Please try again.",
                   type = "error",confirmButtonCol = "#5C8374")
      }
      else{
        sheet_append(data = createSheet,
                     ss = form,
                     sheet = 'Sheet1')
        
        shinyalert("Success!",
                   "Your record has been successfully added",
                   type = "success",confirmButtonCol = "#5C8374")
        
        output$preview <- renderDataTable({
          NULL
        })
        createSheet <<- subset(createSheet, F)
      }
    }
  })
  
}

shinyApp(ui, server)