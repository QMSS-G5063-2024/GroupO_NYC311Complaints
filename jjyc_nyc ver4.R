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
    "plotly",
    "tidyr",
    "data.table",
    "highr",
    "ggthemes",
    "highcharter",
    "DT"
  )

packages <- lapply(packages, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x)
    library(x, character.only = TRUE)
  }
})

# Read and preprocess data
df_2017 <- read.csv("C:/Users/limre/Desktop/QMSS/Spring Semester/5063 Data Visualization/Data Viz Assignments/Final Group Project/df_2017.csv")

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


########## CREATE DATAFRAMES FOR COMMUNITY DISTRICT TAB
data = read.csv("C:/Users/limre/Desktop/QMSS/Spring Semester/5063 Data Visualization/Data Viz Assignments/Final Group Project/CD_dataset.csv")
##### Subsetting the data further into 5 subsets based on Borough 
# vector of all CD names
temp = sort(unique(data$Community.Board))
# Queens Borough CDs
indices <- grep("QUEENS", temp)
temp2 = temp[indices]
Queens.CD = data[data$Community.Board %in% temp2, ]
Queens.CD$Community.Board = gsub("QUEENS", "QN", Queens.CD$Community.Board)
# Manhattan Borough CDs
indices <- grep("MANHATTAN", temp)
temp2 = temp[indices]
Manhattan.CD = data[data$Community.Board %in% temp2, ]
Manhattan.CD$Community.Board = gsub("MANHATTAN", "MN", Manhattan.CD$Community.Board)
# Bronx Borough CDs
indices <- grep("BRONX", temp)
temp2 = temp[indices]
Bronx.CD = data[data$Community.Board %in% temp2, ]
Bronx.CD$Community.Board = gsub("BRONX", "BX", Bronx.CD$Community.Board)
# Brooklyn Borough CDs
indices <- grep("BROOKLYN", temp)
temp2 = temp[indices]
Brooklyn.CD = data[data$Community.Board %in% temp2, ]
Brooklyn.CD$Community.Board = gsub("BRONX", "BX", Brooklyn.CD$Community.Board)
# Staten Island Borough CDs
indices <- grep("STATEN ISLAND", temp)
temp2 = temp[indices]
StatenIsland.CD = data[data$Community.Board %in% temp2, ]
StatenIsland.CD$Community.Board = gsub("STATEN ISLAND", "SI", StatenIsland.CD$Community.Board)

# clearing uneeded items
rm(indices, temp2)
##### CREATE DATAFRAME FOR CD DATA TABLE
CD.DT.data = data %>%
  group_by(Community.Board) %>%
  summarize(Borough = unique(Borough[1]),
            Neighborhoods = unique(Neighborhoods),
            Environmental_calls = sum(Complaint.Category=="Environmental concerns"),
            Housing_calls = sum(Complaint.Category=="Housing concerns"),
            Noise_calls = sum(Complaint.Category=="Noise-related complaints"),
            Transportation_calls = sum(Complaint.Category=="Transportation problems"),
            Sanitation_calls = sum(Complaint.Category=="Sanitation issues"),
            Other_calls = sum(Complaint.Category=="Others"),
            Safety_calls = sum(Complaint.Category=="Safety and security")
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
    ),
    tabPanel("NYC Community Districts", 
             fluidRow(
               column(12, plotlyOutput("CD_Manhattan")),
               column(12,dataTableOutput("CD_NYC311_table"))
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
  
  CD_table_data = CD.DT.data
  
  
  output$CD_NYC311_table = renderDataTable({
    
    datatable(CD_table_data,
      rownames = FALSE, 
      colnames = c("Community Board", 
                   "Borough", 
                   "Neighborhoods Included", 
                   "Calls About Environmental Concerns",
                   "Calls About Housing Issues",
                   "Calls About Noise Complaints", 
                   "Calls About Transportation Issues",
                   "Calls About Sanitation Concerns",
                   "Other Miscellaneous Concerns",
                   "Calls About Safety Issues"),
      filter = list(position="top"), 
      options = list(
        dom = "Bfrtip",
        buttons = I("colvis"),
        language = list(sSearch = "Filter:"),
        pageLength = 10,
        lengthMenu = c(10, 15, 25)
      ),
      extensions = c("Buttons", "Responsive"))
  })
  
  CD_Manhattan_data = Manhattan.CD
  
  output$CD_Manhattan = renderPlotly({
    ggplot(CD_Manhattan_data, aes(x = Community.Board, fill = Complaint.Category)) +
      geom_bar(position = "stack") +
      labs(title = "Breakdown Of NYC311 By Community District",
           x = "Area",
           y = "Number of Events") +
      scale_fill_manual(values = c("Noise-related complaints" = "orange1", 
                                   "Transportation problems" = "dodgerblue",
                                   "Environmental concerns" = "lawngreen",
                                   "Sanitation issues" = "black",
                                   "Housing concerns" = "yellow",
                                   "Safety and security" = "indianred1",
                                   "Others" = "grey")) +
      theme_minimal()
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