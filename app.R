if(!require(shiny)){install.packages('shiny', dependencies = TRUE)}
if(!require(DT)){install.packages('DT', dependencies = TRUE)}
if(!require(ggplot2)){install.packages('ggplot2', dependencies = TRUE)}
if(!require(dplyr)){install.packages('dplyr', dependencies = TRUE)}

library(shiny)
library(DT)
library(ggplot2)
library(dplyr)

data <- read.csv("Auschwitz_Death_Certificates.csv", stringsAsFactors = FALSE)

# Define UI
ui <- fluidPage(
  titlePanel("Victims of the Holocaust"),
  sidebarLayout(
    sidebarPanel(
      selectInput("religion", "Select Religion:",
                  choices = c("Jew", "Protestant", "Catholic", "andere", "Believes in God",
                              "Greek Catholic", "Atheist", "Greek Orthodox", "Unknown",
                              "Eastern Orthodox", "Russian Orthodox", "Jehovah's Witness",
                              "Czech-Moravian", "Buddhist", "Hussite", "Unaffiliated",
                              "Muslim", "Agnostic"),
                  selected = NULL, multiple = TRUE)
    ),
    mainPanel(
      plotOutput("victimPlot"),
      DTOutput("victimTable")
    )
  )
)

# Define server logic
server <- function(input, output) {
  # Filter data based on selected religion
  filtered_data <- reactive({
    if (is.null(input$religion)) {
      return(NULL)
    } else {
      data[data$Religion %in% input$religion, ]
    }
  })
  
  # Render interactive plot
  output$victimPlot <- renderPlot({
    if (!is.null(filtered_data())) {
      ggplot(filtered_data(), aes(x = Religion, label = ..count..)) +
        geom_bar(fill = "skyblue") +
        geom_text(stat = "count", vjust = -0.5) +  # Add text labels
        labs(title = "Number of Holocaust Victims by Religion",
             x = "Religion", y = "Number of Victims") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }
  })
  # Render interactive table
  output$victimTable <- renderDT({
    if (!is.null(filtered_data())) {
      datatable(filtered_data(), options = list(pageLength = 10))
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
