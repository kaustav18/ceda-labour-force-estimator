library(shiny)
library(plotly)
library(readxl)
library(htmltools)

# UI
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      @import url('https://fonts.googleapis.com/css2?family=Georgia&display=swap');
      body { font-family: 'Georgia', serif; }
      .plotly-tooltip { font-family: 'Georgia', serif; }
      .xtick, .ytick, .plotly .cartesianlayer .plotbackground { font-family: 'Georgia', serif; }
      .custom-initial-values {
        font-size: 10px;
        font-style: italic;
      }
      .custom-notes {
        font-style: normal;
        font-size: 14px;
      }
      .custom-title {
        font-size: 20px;
      }
      @media (max-width: 768px) {
        .custom-title {
          font-size: 16px;
        }
        .sidebar {
          padding-right: 5px;
        }
      }
      .custom-logo {
        display: flex;
        justify-content: flex-end;
        align-items: flex-start;
      }
      .attribution {
        font-size: 10px
      }
    "))
  ),
  tags$script(HTML('document.title = "CEDA Labour Force Estimator";')),
  titlePanel(
    div(
      HTML(paste(
        "Projected labour force population of", "<span style='color: #316395;'>India</span>", "and", "<span style='color: #af0038;'>China</span>"
      )),
      class = "custom-title",
      style = "text-align: center;"
    )
  ),
  sidebarLayout(
    sidebarPanel(
      numericInput("country1", "India LFPR*", value = 51.3),
      numericInput("country2", "China LFPR*", value = 75.8),
      helpText("*Initial values on loading show the current values for the countries", class = "custom-initial-values"),
      div (
        HTML("Enter a LFPR for India and China above to see when will India’s labour force exceed China’s in absolute numbers (assuming everything else remains constant). The calculations are based on UN Estimates of Population for the working age group (15-64 years)."),
        class = "custom-notes"
      ),
      p (
        HTML("<br>Data visualization by the Centre for Economic Data & Analysis, Ashoka University"),
        class = "attribution"
      ),
      div(
        class = "custom-logo",
        tags$img(src = "https://viz.ceda.ashoka.edu.in/assets/logo.png", height = "15px", width = "100px")
      ),
    ),
    mainPanel(
      div(
        class = "container-fluid",
        plotlyOutput("populationPlot"),
        div (textOutput("intersectionText"),
             style = "text-align: center;")
      )
    )
  )
)



# Server
server <- function(input, output) {
  
  # Generate random population data for two countries
  df <- read_excel('app.xls')
  country1_population <- df$i_pop
  country2_population <- df$c_pop 
  
  # Multiply the population data by user inputs
  multiplied_country1 <- reactive({
    country1_population * input$country1 * 0.01
  })
  
  multiplied_country2 <- reactive({
    country2_population * input$country2 * 0.01
  })
  
  
  # Find the year of intersection
  intersection_year <- reactive({
    year <- 1
    while (year <= 35 && multiplied_country1()[year] < multiplied_country2()[year]) {
      year <- year + 1
    }
    if (year <= 35) {
      year
    } else {
      NA
    }
  })
  
  # Plot the multiplied population data
  
  
  output$populationPlot <- renderPlotly({
    text_pop_country1 <- country1_population
    text_pop_country2 <- country2_population
    plot_ly() %>%
      add_trace(x = df$year, y = multiplied_country1(), type = "scatter", mode = "lines", name = "India", 
                line = list(color = "#316395",shape = "spline"), hovertemplate = "Projected Labour Force: %{y:.0f} million<br>Projected Population (15-64 years): %{customdata:,.0f} million",
                customdata = df$i_pop) %>%
      add_trace(x = df$year, y = multiplied_country2(), type = "scatter", mode = "lines", name = "China",
                line = list(color = "#af0038",shape = "spline"), hovertemplate = "Projected Labour Force: %{y:.0f} million<br>Projected Population (15-64 years): %{customdata:,.0f} million",
                customdata = df$c_pop) %>%
      layout(title = list(text = "", font = list(family = "Georgia",color = "#000")),
             xaxis = list(title = list(text = "", font = list(family = "Georgia", size = 14, color = "#000")), tickfont = list(family = "Georgia", size = 12, color = "#000"),fixedrange = TRUE, tickmode = "array", tickvals = c(2021, seq(min(df$year), max(df$year), by = 3))),
             yaxis = list(title = list(text = "Labour force (in millions)", font = list(family = "Georgia", size = 14, color = "#000")), tickfont = list(family = "Georgia", size = 12, color = "#000"), fixedrange = TRUE),
             hoverlabel = list(font = list(family = "Georgia", size = 12)),
             hovermode = "x unified",
             showlegend = FALSE)
  })
  
  
  # Generate the intersection text
  output$intersectionText <- renderText({
    if (!is.na(intersection_year())) {
      intersection_value <- multiplied_country1()[intersection_year()]
      paste("If India's LFPR was",input$country1,"percent and China's were",input$country2,"percent (everything else remaining constant), then India's labour force will exceed China's in "
            ,intersection_year() + 2020)
    } else {
      "India's labour force will not catch up with China's within this time period."
    }
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)
