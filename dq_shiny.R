library(tidyverse)
library(shiny)
## get data
ice <- read.csv("https://raw.githubusercontent.com/py-cia/DQ_ice_cream_analysis/main/dq_all_shift_cap.csv")
## crew member names
crew <- sort(c("Miguel", "Jose", "Heather", "Danielle", "Martha", "Honesty", "All"))
## different ice cream cone sizes
weight <- c("Small", "Medium", "Large")

ui <- fluidPage(
  ## theme
  theme = bslib::bs_theme(bootswatch = "journal"),
  ## Application title
  titlePanel("Dairy Queen Ice-Cream Cone Weight Analysis: For Accuracy & Bragging Rights"),
  sidebarLayout(
    sidebarPanel(
      ## dq pic
      tags$img(src = "dq_logo.jpg", height = "50%", width = "50%", alt = "Something went wrong!", deleteFile = FALSE, align = "right"),
  titlePanel("Inputs"), width = 3,
  ## inputs
  selectInput("name", "Crew Member", crew),
  selectInput("size", "Weight", weight)
  ),
    mainPanel(width = 9,
  tabsetPanel(
    tabPanel(title = "Histogram",
             br(),
  plotOutput("poly"),
  br(),
  fluidRow(
    column(width = 4,
           selectInput("sample", strong("Customer POV: Buying a Cone. How lucky are you?"), choices = weight),
           verbatimTextOutput("results"),
           verbatimTextOutput("outcome")
           ),
    column(width = 3,
           strong(textOutput("mat_name")),
           br(),
           verbatimTextOutput("matrix")
           ),
    column(width = 3, offset = 2,
           tableOutput("summary")
      )
     ) 
    ),
  tabPanel(title = "Static Graphs",
           plotOutput("count"),
           hr(),
           plotOutput("box")   
     ),
  tabPanel(title = "About",
           titlePanel("About"),
           "Created with R Shiny,",
           br(),
           "March 2023",
           br(),
           br(),
           strong("Mission"),
           br(),
           "This started as a fun project, only identifying the employee with best accuracy, but later I took it a step further by looking at
           the sample proportion, shape of the distribution, and added a fun element of sampling from the real data to emulate ordering from this
           Dairy Queen location.",
           br(),
           "This project looks at individual and total distributions for ice cream cones. Ice cream cones are small, medium, and large; their sizes
           are 5oz, 7oz, and 10oz. Observations were self-reported on paper and measured using a scale that reports to the nearest ounce.",
           br(),
           strong("Key Issues"),
           br(),
           "There is some definite implicit bias present. This is something that can not be helped unless I shadowed whoever was working on
           ice cream and secretly reported their measurements. Although I advised the employees not to worry too much about the results I still believe it was not enough to offset.
           Also there is very little data. This was initially a school project and the window I had to collect and make this app was small. There is nearly
           not enough data to make a claim about individuals but enough to comment on the sizes overall, at least for small and medium.",
           br(),
           strong("Finds"),
           br(),
           "The most significant find was not a stark difference in employee accuracy, but that medium-sized cones across employees were being 
           made with more ice cream on average. This was huge when compared to small and large cones. From the consumer perspective, you don't want
           to know that you are getting less ice cream than you paid for, but from the company's point of view, this is a win. Sell more small cones!!!",
           br(),
           strong("What's next?"),
           br(),
           "The next step for this project would be to calculate the cost of mix per-ounce and see if buying a medium or large is worth it based on the
           data. This would be interesting since the medium cone distribution did not look approximately normal. I wonder that if the number of observations
           increased, would this turn normal or become more positively skewed?"
     )
    )
   )
  )
 )
## Define server logic
server <- function(input, output, session) {
  
  df <- reactive(filter(ice, Name == input$name, Size == input$size))
  weight <- reactive(filter(ice, Name == input$name, Size == input$size)[, 1])
  sm <- reactive(filter(ice, Size == input$sample)[ , 1])
  df_all <- reactive(filter(ice, Size == input$size))
  df_all_oz <- reactive(filter(ice, Size == input$size)[ , 1])
  
  ounce_print <- reactive(if (input$size == "Small") {
    print("(5oz)", quote = FALSE)
  } else if (input$size == "Medium") {
    print("(7oz)", quote = FALSE)
  } else if (input$size == "Large") {
    print("(10oz)", quote = FALSE)
  }
 )
  
  tab <- function(dframe) {
    Matrix <- rep("Below", nrow(dframe))
    nfram <- weight()
 if (input$size == "Small") {
      Matrix[nfram >= 5] = "Above"
      print(table(Matrix)); table(Matrix)[1]/sum(table(Matrix))
    } else if (input$size == "Medium") {
      Matrix[nfram >= 7] = "Above"
      print(table(Matrix)); table(Matrix)[1]/sum(table(Matrix))
    } else if (input$size == "Large") {
      Matrix[nfram >= 10] = "Above"
      print(table(Matrix)); table(Matrix)[1]/sum(table(Matrix))
    }
  }

  output$mat_name <- renderText("Probability of ordering a cone that is MORE than or EQUAL to its size.")
    
  output$matrix <- renderPrint({
    Matrix <- rep("Below", length(df_all_oz()))
    newdf <- df_all_oz()
    if (input$name == "All") {
      if(input$size == "Small") {
        Matrix[newdf >= 5] = "Above"
        print(table(Matrix));table(Matrix)[1]/sum(table(Matrix))
          }else if (input$size == "Medium"){
            Matrix[newdf >= 7] = "Above"
            print(table(Matrix));table(Matrix)[1]/sum(table(Matrix))
          } else if (input$size == "Large") {
            Matrix[newdf >= 10] = "Above"
            print(table(Matrix));table(Matrix)[1]/sum(table(Matrix))
          }
    } else{
          tab(df())
        }
        
  })
  
  output$summary <- renderTable({
    if (input$name == "All") {
      Statistic <-c("Min", "1st Qu.", "Median", "3rd Qu.", "Max", "Mean", "Std")
      Values <- c(min(df_all_oz()), quantile(df_all_oz())[2], median(df_all_oz()), quantile(df_all_oz())[4], max(df_all_oz()), mean(df_all_oz()), sd(df_all_oz()))
      data.table::data.table(Statistic, Values)
          } else {
    Statistic <-c("Min", "1st Qu.", "Median", "3rd Qu.", "Max", "Mean", "Std")
    Values <- c(min(weight()), quantile(weight())[2], median(weight()), quantile(weight())[4], max(weight()), mean(weight()), sd(weight()))
    data.table::data.table(Statistic, Values)
          }
    })
  
  output$poly <- renderPlot({
    if (input$name == "All") {
      ggplot(data = df_all(), mapping = aes(x = Weight)) +
        geom_histogram(binwidth = .1, fill = "blue") +
        labs(x = paste("Weight", "[oz]"), y = "Count", title = paste(input$size, "Cone Distribution")) +
        theme(plot.caption = element_text(size = 19),
              axis.title = element_text(face = "bold"),
              plot.title = element_text(face = "bold"),
              panel.background = element_rect(fill = "white"),
              plot.background = element_rect(fill = "gray95"))
    } else {
    ggplot(data = df(), aes(x = Weight)) +
      geom_histogram(binwidth = .1, fill = "blue") +
      labs(x = paste("Weight", "[oz]"), y = "Count", title = paste("Cone Distribution for", input$name)) +
      theme(plot.caption = element_text(size = 19),
      axis.title = element_text(face = "bold"),
      plot.title = element_text(face = "bold"),
      panel.background = element_rect(fill = "white"),
      plot.background = element_rect(fill = "gray95"))
    }
  })
  
  output$results <- renderPrint({
    x <- sample(sm(), size = 1)
    print(x)
    if (input$sample == "Small") {
      y = round(x - 5, 1); print(paste("How much ice cream you gained/lost:", y, "oz"))
    } else if(input$sample == "Medium") {
      y = round(x - 7, 1); print(paste("How much ice cream you gained/lost:", y, "oz"))
    } else if(input$sample == "Large") {
      y = round(x - 10, 1); print(paste("How much ice cream you gained/lost:", y, "oz"))
    }
  })
  
  output$count <- renderPlot({
    ggplot(data = ice, mapping = aes(x = Size)) +
             geom_bar(width = .9, fill = "blue") +
             labs(title = "Most Cones Sold through 3-day Period", x = "Size", y = "Count")
  })
  
  output$box <- renderPlot({
    ggplot(filter(ice, Size == "Small"), mapping = aes(x = Name, y = Weight, fill = Position)) +
      geom_boxplot() +
      labs(x = "Employee", y = "Weight", title = "Boxplot for Small Cones")
  })
}

## Run the application 
shinyApp(ui = ui, server = server)


