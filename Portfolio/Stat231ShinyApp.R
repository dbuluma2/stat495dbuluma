# Member contributions: Daizy - Bar chart, Clara - Histogram, Christian — scatterplot, Daizy - table, all group members worked on map

# Load packages
library(shiny)
library(tidyverse)
library(ggplot2)
library(shiny)
library(ggpubr)
library(sf)
library(viridis)
library(kableExtra)
library(plotly)

# Read in Data

df <- read.csv("data/HealthData.csv")
healthdata <- read_csv("data/Key_indicator_districtwise.csv") %>% filter(State_Name != "Uttar Pradesh")
all_data <- read_csv("data/Key_indicator_districtwise.csv")


#Data Wrangling
# Select variables to include and rename as needed
df_2 <- select(all_data,
             State_Name,
             State_District_Name,
             
             LL_Median_Age_At_First_Live_Birth_Of_Women_Aged_15_49_Years_Total,
             LL_Median_Age_At_First_Live_Birth_Of_Women_Aged_15_49_Years_Rural,
             LL_Median_Age_At_First_Live_Birth_Of_Women_Aged_15_49_Years_Urban,
             LL_Median_Age_At_First_Live_Birth_Of_Women_Aged_25_49_Years_Total,
             LL_Median_Age_At_First_Live_Birth_Of_Women_Aged_25_49_Years_Rural,
             LL_Median_Age_At_First_Live_Birth_Of_Women_Aged_25_49_Years_Urban,
             
             YY_Infant_Mortality_Rate_Imr_Total_Male, 
             YY_Infant_Mortality_Rate_Imr_Total_Female,
             YY_Infant_Mortality_Rate_Imr_Rural_Person,
             YY_Infant_Mortality_Rate_Imr_Rural_Male,
             YY_Infant_Mortality_Rate_Imr_Rural_Female,
             
             contains("PP_"),
             
             TT_Children_Whose_Birth_Weight_Was_Taken_Total,
             TT_Children_Whose_Birth_Weight_Was_Taken_Rural,
             TT_Children_Whose_Birth_Weight_Was_Taken_Urban,
             
             TT_Children_With_Birth_Weight_Less_Than_2_5_Kg_Rural,
             TT_Children_With_Birth_Weight_Less_Than_2_5_Kg_Urban,
             
             YY_Neo_Natal_Mortality_Rate_Total,
             YY_Neo_Natal_Mortality_Rate_Rural,
             YY_Neo_Natal_Mortality_Rate_Urban,
             
             ZZ_Infant_Mortality_Rate_Total_Lower_Limit,
             ZZ_Infant_Mortality_Rate_Total_Upper_Limit,
             ZZ_Infant_Mortality_Rate_Rural_Lower_Limit,
             ZZ_Infant_Mortality_Rate_Rural_Upper_Limit,
             ZZ_Infant_Mortality_Rate_Urban_Lower_Limit,
             ZZ_Infant_Mortality_Rate_Urban_Upper_Limit
) 


# DATA WRANGLING FOR SPECIFIC APP COMPONENTS

cols <- names(all_data)

# remove prefix characters
new_cols <- str_replace(cols, "[A-Z][A-Z].", "")

# remove underscores
new_cols <- str_replace_all(new_cols, "_", " ")

# update variables that miss some info
new_cols <- str_replace_all(new_cols, "2 5", "2.5")


#For TAB 1 Bar Chart
col_choice_values <- names(healthdata[10:100]);
#col_choice_names <- c("Infant Mortality Rate(Male)","Infant Mortality Rate(Female)","Neo-Natal Mortality Rate in Urban Areas")
#names(col_choice_values) <- col_choice_names
state_choices <-  unique(healthdata$State_Name) 

#For TAB 2: HISTOGRAM
hist_choices <- c()
var_choices_hist <- names(df[3:50])

# For TAB 3: SCATTERPLOT

# create duplicate vector with newly named cols
all_cols_values <- names(all_data)
names(all_cols_values) <- new_cols

# For TAB 4: MAP
states <- st_read("./data/Admin2.shp") %>%
  mutate(
    Name = str_to_lower(ST_NM)
  )
df_grp <- df_2 %>%
  mutate(Name = str_to_lower(State_Name)) %>% 
  group_by(Name) %>%
  summarize(
    mean_birthAge = mean(LL_Median_Age_At_First_Live_Birth_Of_Women_Aged_25_49_Years_Total), Neonatal_MortalityRate = mean( YY_Neo_Natal_Mortality_Rate_Total),
    Infant_MortalityRate = mean(YY_Infant_Mortality_Rate_Imr_Total_Male),  Infant_MortalityRateFemale = mean(YY_Infant_Mortality_Rate_Imr_Total_Female)
  )
map_var_choices <- c("mean_birthAge")
############
#    ui    #
############
ui <- navbarPage(
  
  title="Public Health",
  
  tabPanel(
    title = "Bar Chart",
    sidebarLayout(
      sidebarPanel(
        selectInput(inputId = "colvar"
                    , label = "Choose a variable of interest:"
                    , choices = col_choice_values
                    , selected = "YY_Infant_Mortality_Rate_Imr_Total_Male"),
        selectInput(inputId = "statevar"
                           , label = "Select the state of Interest:"
                           , choices = state_choices
                           , selected = "Assam"
                           )
      ),
      mainPanel(
        plotOutput(outputId = "bar")
      )
    )
  ),
  #TAB 2: HISTOGRAM
  # Application title
  tabPanel(
  title = "HISTOGRAM",
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      sliderInput("bins",
                  "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 30),
      selectInput(inputId = "id_x"
                  , label = "Select variable to plot on x-axis:"
                  , choices = var_choices_hist
                  , selected = c("YY_Infant_Mortality_Rate_Imr_Total_Female"))
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
    )
  )
 ),
 
 #Tab 3 :Scatterplot
 tabPanel(
   
   title = "SCATTERPLOT",
   
   sidebarLayout(
     
     sidebarPanel(
       selectInput(inputId = "id_name"
                   , label = "Select variable to plot on x-axis:"
                   , choices = all_cols_values
                   , selected = NULL )
     ),
     fluidPage(
       mainPanel(
         fluidRow(
           splitLayout(cellWidths = c("50%", "50%"),
                       plotlyOutput("scatter_rural", width = "100%", height = "400px"),
                       plotlyOutput("scatter_urban", width = "100%", height = "400px"))
         )
       )
     )
   )
 ),
  #TAB 4: TABLE
 tabPanel(
   title = "TABLE",
   
   sidebarLayout(
     sidebarPanel(
       selectizeInput(inputId = "state"
                      , label = "Choose one or more states:"
                      , choices = state_choices
                      , selected = "Assam"
                      , multiple = TRUE)
     ),
     mainPanel(
       DT::dataTableOutput(outputId = "table")
     )
   )
 ),
 #TAB 5: MAP
 tabPanel(
   title = "MAP",
   
   sidebarLayout(
     sidebarPanel(
        selectizeInput(inputId = "mapvar",
                      label = "Select a variable to plot:",
                      choices = map_var_choices,
                      selected = "mean_birthAge",
                     )
     ),
     mainPanel(
       plotOutput(outputId = "map")
     )
   )
 )
 )


############
# server   #
############
server <- function(input,output){
  
  # TAB 1: BARCHART
  data_for_bar <- reactive({
    data <- filter(healthdata, State_Name %in% input$statevar )
  })
  
  output$bar <- renderPlot({
    ggplot( data = data_for_bar(), aes_string(x = "State_District_Name", y = input$colvar, fill = "State_District_Name" )) +
      geom_bar(stat = "identity")+
      labs(x = "Names of Districts"
           , title = paste("Distribution of variables"
                           , "among districts in"
                           , input$statevar
                           , "(per thousand live births)")) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "bottom",
            plot.title = element_text(size = 15), 
            plot.background = element_rect(fill = "white") 
      ) 
  })
  
  #TAB 2: HISTOGRAM
  output$distPlot <- renderPlot({
    
    ggplot(data = df, aes_string(x = input$id_x)) +
      geom_histogram(color = "#2c7fb8", fill = "#7fcdbb", alpha = 0.7, bins = input$bins) +
      #labs(x = hist_choice_names[hist_choice_values == input$histvar]
      labs(x = input$id_x
           , y = "Frequency"
           , title = paste("Distribution of the"
                           ,  input$id_x))
    
  })
  
  #TAB 3: SCATTERPLOT
  #SUB-SECTION I: LEFT SCATTERPLOT
  output$scatter_rural <- renderPlotly({
    
    # holds the x-variable
    input_var <- new_cols[input$id_name == all_cols_values]
    
    # y-variable for left plot
    pt_type_rural <- str_subset(names(all_data), "Birth_Weight_Less_Than_2_5_Kg_Rural")
    
    
    all_data %>%
      ggplot(aes_string(y=pt_type_rural, x=input$id_name)) +
      geom_point(color = "#2c7fb8") +
      stat_smooth(method="lm", formula = y ~ x, geom = "smooth") +
      labs(x = input_var, y = "Children With Birth Weight Less Than 2.5 Kg Rural") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, size = rel(0.5)))
  })
  
  #SUB-SECTION II: RIGHT SCATTERPLOT
  output$scatter_urban <- renderPlotly({
    
    # holds the x-variable
    input_var <- new_cols[input$id_name == all_cols_values]
    
    # y-variable for right plot
    pt_type_urban <- str_subset(names(all_data), "Birth_Weight_Less_Than_2_5_Kg_Urban")
    
    all_data %>%
      ggplot(aes_string(y=pt_type_urban, x=input$id_name)) +
      geom_point(color = "#2c7fb8") +
      stat_smooth(method="lm", formula = y ~ x, geom = "smooth") +
      labs(x = input_var, y = "Children With Birth Weight Less Than 2.5 Kg Urban")+
      theme(axis.text.x = element_text(angle = 90, hjust = 1, size = rel(0.5)))
    
    # Sources used
    # Source 1: Used link for guide on making an interactive scatter plot https://psrc.github.io/intro-shiny-guide/packages_i.html#:~:text=Plotly%20on%20its%20own%20can,package%20in%20the%20global%20section.
    # Source 2: Used to make the main panel fluid -- display two scatter plots side-by-side https://stackoverflow.com/questions/34384907/how-can-put-multiple-plots-side-by-side-in-shiny-r
    
  })
  
  #TAB 4: TABLE
  
  data_for_table <- reactive({
    data <- filter(healthdata, State_Name %in% input$state) 
  })
  
  output$table <- DT::renderDataTable({ 
    data_for_table()
  })
  
  #TAB 5: MAP
  
  output$map <- renderPlot({
    
    # merge spatial data with population data, also convert state names to lower case in the latter
    #states_population <- states %>%
    # left_join(states_data %>% mutate(Name = str_to_lower(Name)), "Name")
    # grey states are the result of unmatched states outlined above
    
    dfgrp_expend_map <- df_grp |>
      inner_join(states, by = c("Name"="Name"))
    
    
    ggplot(dfgrp_expend_map) +
      geom_sf(aes(geometry=geometry, fill = mean_birthAge)) +
      theme_void() +
      labs(fill = "Average Weight"
           , caption = "Source: Phealth Mammoths"
           , title = "Map Showing Average Age of mothers at Birth in 9 states in India") 
    
  })
}
####################
# call to shinyApp #
####################
shinyApp(ui = ui, server = server)