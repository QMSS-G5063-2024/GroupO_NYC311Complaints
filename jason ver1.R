packages <-
  c(
    "shiny",
    "shinythemes",
    "leaflet",
    "dplyr",
    "readr",
    "leaflet.extras",
    "ggplot2",
    "lubridate",
    "plotly"
  )

packages <- lapply(packages, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x)
    library(x, character.only = TRUE)
  }
})

# Read and preprocess data
df_2017 <- read.csv("./Data/df_2017.csv")

df <- df_2017 %>%
  mutate(
    Complaint.Type = case_when(
      Complaint.Type %in% c("Noise - Residential") ~ "Residential",
      Complaint.Type %in% c("Noise - Commercial") ~ "Commercial",
      Complaint.Type %in% c("Noise - Park") ~ "Park",
      Complaint.Type %in% c("Noise - Street/Sidewalk") ~ "Street/Sidewalk",
      Complaint.Type %in% c("Public Assembly") ~ "Public Assembly",
      Complaint.Type %in% c("Noise") ~ "Misc",
      TRUE ~ "Misc"
    ),
    Borough = case_when(
      Borough %in% c("BROOKLYN") ~ "Brooklyn",
      Borough %in% c("MANHATTAN") ~ "Manhattan",
      Borough %in% c("QUEENS") ~ "Queens",
      Borough %in% c("BRONX") ~ "Bronx",
      Borough %in% c("STATEN ISLAND") ~ "Staten Island",
      TRUE ~ "Other"
    )
  ) %>%
  filter(
    Borough != "Other",
    !is.na(Longitude), !is.na(Latitude),
    Longitude >= -74.257159, Longitude <= -73.699215,
    Latitude >= 40.477399, Latitude <= 40.917577
  )

# Define UI
ui <- fluidPage(
  theme = shinytheme("yeti"),
  titlePanel("Visualizing NYC 311 Noise Complaints"),
  tags$h4("Choose your level of analysis using the various tabs!", class = "text-muted"),
  
  tabsetPanel(
    tabPanel("New York City Level", 
             sidebarLayout(
               sidebarPanel(
                 checkboxGroupInput("complaintType", "Select Complaint Type:",
                                    choices = unique(df$Complaint.Type),
                                    selected = "Residential"),
                 checkboxGroupInput("borough", "Select Borough:",
                                    choices = unique(df$Borough),
                                    selected = "Manhattan")
               ),
               mainPanel(
                 leafletOutput("map"),
                 hr(),
                 h3("An Overview of Noise Complaints"),
                 plotOutput("staticTypePlot"),
                 plotOutput("staticBoroughPlot")
               )
             )
    ),
    tabPanel("Time Series",
             sidebarLayout(
               sidebarPanel(
                 dateRangeInput("dateInput", "Select Date Range:",
                                start = "2017-01-01", end = "2017-12-31"),
                 checkboxGroupInput("complaintTypeTS", "Select Complaint Type (Time Series):",
                                    choices = unique(df$Complaint.Type),
                                    selected = unique(df$Complaint.Type))
               ),
               mainPanel(
                 plotlyOutput("timeSeriesPlot"),
                 leafletOutput("detailedMap")
               )
             )
    )
  )
)

# Define server logic
server <- function(input, output) {
  filteredDataNYC <- reactive({
    df %>%
      filter(Complaint.Type %in% input$complaintType, Borough %in% input$borough)
  })
  
  filteredDataTS <- reactive({
    df %>%
      filter(Complaint.Type %in% input$complaintTypeTS)
  })
  
  output$map <- renderLeaflet({
    leaflet(data = filteredDataNYC()) %>%
      addTiles() %>%
      addHeatmap(lng = ~Longitude, lat = ~Latitude, intensity = ~1,
                 blur = 20, max = 0.05, radius = 15)
  })
  
  output$timeSeriesPlot <- renderPlotly({
    req(input$dateInput)
    time_filtered_data <- filteredDataTS() %>%
      mutate(DateOnly = as.Date(Created.Date)) %>%
      filter(DateOnly >= as.Date(input$dateInput[1]), DateOnly <= as.Date(input$dateInput[2])) %>%
      group_by(DateOnly, Complaint.Type) %>%
      summarise(Count = n(), .groups = 'drop')
    
    p <- ggplot(time_filtered_data, aes(x = DateOnly, y = Count, color = Complaint.Type, group = Complaint.Type)) +
      geom_line() +
      labs(title = "Time Series of Complaints", x = "Date", y = "Number of Complaints") +
      theme_minimal() +
      scale_x_date(date_breaks = "1 month", date_labels = "%b %Y")
    
    ggplotly(p)  # Convert ggplot object to plotly interactive plot
  })
  
  # Static plot renderings for complaint types in descending order
  output$staticTypePlot <- renderPlot({
    ggplot(data = df %>% count(Complaint.Type) %>% arrange(desc(n)), aes(x = reorder(Complaint.Type, n), y = n)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      theme_minimal() +
      labs(title = "Overall Distribution of Complaints by Type", x = "Complaint Type", y = "Count")
  })
  
  # Static plot renderings for boroughs in descending order
  output$staticBoroughPlot <- renderPlot({
    ggplot(data = df %>% count(Borough) %>% arrange(desc(n)), aes(x = reorder(Borough, n), y = n)) +
      geom_bar(stat = "identity", fill = "tomato") +
      theme_minimal() +
      labs(title = "Overall Distribution of Complaints by Borough", x = "Borough", y = "Count")
  })
}

# Run the application
shinyApp(ui = ui, server = server)