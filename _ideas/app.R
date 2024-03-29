
library(shiny)
library(tidyverse)
library(scales)
library(dplyr)
# tuesdata <- tidytuesdayR::tt_load('2022-03-29')
# tuesdata <- tidytuesdayR::tt_load(2022, week = 13)
# CollegiateSports <- tuesdata$sports
CollegiateSports <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-03-29/sports.csv')


# filter so its only Division 1 sports 
div1 <- CollegiateSports |> filter(grepl('NCAA Division I', classification_name)) |> filter(classification_code <= 3)
#str detect
# check that after I, its not another I, or exactly one I

# figured out that classifications for all D1 sports are either 1, 2, or 3
div1_smaller <- div1 |> select(1,3,7,8,20:28)

# selected only the columns that I'm interested in. 

div1_rev_expend <- div1_smaller |> select(1:4, 9, 12, 13)

university_names <- div1_rev_expend |> pull(institution_name) 



# Define UI for application that draws histrgram for different schools 
ui <- fluidPage(

  # Give the page a title
  titlePanel("Revenue and Expenditure for D1 Sports 2015 - 2019, By School"),
  
  # Generate a row with a sidebar
  sidebarLayout(      
    
    # Define the sidebar with one input
    sidebarPanel(
      selectInput("institution_name", "University:",
                  choices = unique(university_names)),
      hr(),
    ),
    
    # Create a spot for the barplot
    mainPanel(
      plotOutput("plot1")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  ## This select is where I'm still having problems and don't know how to use my data as an input instead 
  selectedData <- reactive({
    # div1_rev_expend[, c(input$institution_name)]
    select_for_plot <- div1_rev_expend |> filter(institution_name == input$institution_name) |>
      filter(!is.na(total_rev_menwomen), !is.na(total_exp_menwomen)) |>
      group_by(institution_name, year) |>
      summarise(total_rev = sum(total_rev_menwomen), total_exp = sum(total_exp_menwomen)) |>
      pivot_longer(cols = c(total_rev, total_exp),
                   names_to = "Type", values_to = "Dollars")
  })

  # Fill in the spot we created for a plot
  output$plot1 <- renderPlot({
    
    # Render a barplot
    ggplot(data = selectedData(), aes(x = year, y = Dollars, fill = Type)) +
      geom_col(position = "dodge") +
      scale_y_continuous("Dollars",
                         breaks = scales::breaks_extended(8),
                         labels = scales::label_dollar()) +
      labs(title = str_glue("Total Revenue and Expenditure for Sports\nat {input$institution_name}")) +
      theme_minimal() +
      scale_fill_viridis_d(labels = c('Total Expenditiure', 'Total Revenue'),
                           option = 'cividis')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

## Next I want to incorporate being able to select a division or just a sport 
