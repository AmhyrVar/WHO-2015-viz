library(tidyverse)
library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(DT)
library(shinydashboard)
library(rnaturalearth)
library(sf)
library(reshape2)


le <- read.csv("Life-Expectancy-Data-Updated.csv")
le<- le %>% 
  mutate(Country= if_else(Country == "Bolivia (Plurinational State of)", "Bolivia", Country)) %>% 
  mutate(Country= if_else(Country == "Venezuela, RB", "Venezuela", Country)) %>% 
  mutate(Country= if_else(Country == "United States of America", "United States", Country)) %>% 
  mutate(Country= if_else(Country == "Central African Republic", "Central African Rep.", Country)) %>% 
  mutate(Country= if_else(Country == "Congo, Dem. Rep.", "Dem. Rep. Congo", Country)) %>% 
  mutate(Country= if_else(Country == "Congo, Rep.", "Congo", Country)) %>% 
  mutate(Country= if_else(Country == "United Republic of Tanzania", "Tanzania", Country)) %>%
  mutate(Country= if_else(Country == "United Kingdom of Great Britain and Northern Ireland", "United Kingdom", Country)) %>% 
  mutate(Country= if_else(Country == "Russian Federation", "Russia", Country)) %>% 
  mutate(Country= if_else(Country == "Iran, Islamic Rep.", "Iran", Country)) %>% 
  mutate(Country= if_else(Country == "Syrian Arab Republic","Syria", Country)) %>% 
  mutate(Country= if_else(Country == "Viet Nam","Vietnam", Country)) %>% 
  mutate(Country= if_else(Country == "Czechia","Czech Rep.", Country)) %>% 
  mutate(Country= if_else(Country == "Bosnia and Herzegovina","Bosnia and Herz.", Country)) %>% 
  mutate(Country= if_else(Country == "Republic of Moldova","Moldova", Country)) %>% 
  mutate(Country= if_else(Country == "The former Yugoslav republic of Macedonia","Macedonia", Country)) %>% 
  mutate(Country= if_else(Country == "Lao People's Democratic Republic","Lao PDR", Country)) %>% 
  mutate(Country= if_else(Country == "Equatorial Guinea","Eq. Guinea", Country)) %>%
  mutate(Country= if_else(Country == "Kyrgyz Republic","Kyrgyzstan", Country)) %>% 
  mutate(Country= if_else(Country == "Turkiye","Turkey", Country)) %>% 
  mutate(Country= if_else(Country == "Yemen, Rep.","Yemen", Country)) %>% 
  mutate(Country= if_else(Country == "Egypt, Arab Rep.","Egypt", Country)) %>% 
  mutate(Country= if_else(Country == "Cote d'Ivoire","Côte d'Ivoire", Country)) %>% 
  mutate(Country= if_else(Country == "Dominican Republic","Dominican Rep.", Country)) 






countries_status <- le %>% 
  select(Country,Economy_status_Developed) %>% 
  distinct(Country,Economy_status_Developed)



world <- ne_countries(scale = "medium", returnclass = "sf") 

world <- world %>% 
  select(name, geometry)

world_data <- countries_status %>%
  left_join(world, by = c("Country" = "name"))

wd <- world %>% 
  left_join(countries_status,by = c("name" = "Country") )









le_2015 <- le %>% 
  filter(Year==2015)



# Rename 'name' column to 'Country' in df2
wd1 <- wd %>% rename(Country = name) %>% 
  select(Country)

countries_to_add <- c("S. Sudan", "Sudan", "Dem. Rep. Korea","Korea")

rows_to_add <- wd1 %>% filter(Country %in% countries_to_add)


# Append this row to df1
le_2015 <- bind_rows(le_2015, rows_to_add)
le_2015 <- le_2015 %>% 
  select(-geometry)




wd_2015 <- inner_join(wd, le_2015, by = c("name" = "Country"))



map_monde_plotly <- function(var_to_plot) {
  
  variable_labels <- c(
    Infant_deaths = "Décès d'infants",
    Under_five_deaths = "Décès d'enfants sous cinq ans",
    Adult_mortality = "Mortalité adulte",
    GDP_per_capita = "PIB par habitant",
    Thinness_five_nine_years = "Maigreur chez les 5-9 ans",
    Thinness_ten_nineteen_years = "Maigreur chez les 10-19 ans",
    Life_expectancy = "Espérance de vie"
  )
  
  label <- variable_labels[[var_to_plot]]
  
  ggplotly(ggplot(data = wd_2015) +
             geom_sf(aes_string(fill = var_to_plot, tooltip = "name")) +
             labs(fill = label , title = "Map Monde")+
             theme_minimal()+
             theme(panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   axis.text.x = element_blank(), 
                   axis.text.y = element_blank(),)+
             scale_fill_viridis_c(option = "D")
           )
   
}








infection_plotly <- function(selected_infection) {
  
  if(selected_infection == "Polio") {
    plot <- ggplot(wd_2015, aes(x = GDP_per_capita, y = Polio, color = name)) +
      geom_point() +
      geom_hline(yintercept = 85, linetype = "dashed", color = "red") +
      labs(title = "PIB par hab. vs Immunisation Polio",
           x = "PIB par hab.",
           y = "Polio Vaccination Rate") +
      theme_minimal() +
      theme(legend.position = "none")
    
  } else if(selected_infection == "Hepatitis_B") {
    plot <- ggplot(wd_2015, aes(x = GDP_per_capita, y = Hepatitis_B, color = name)) +
      geom_point() +
      geom_hline(yintercept = 85, linetype = "dashed", color = "red") +
      labs(title = "PIB par hab. vs Immunisation Hep B",
           x = "PIB par hab.",
           y = "Immunisation Hep B") +
      theme_minimal() +
      theme(legend.position = "none")
    
  } else if(selected_infection == "Measles") {
    plot <- ggplot(wd_2015, aes(x = GDP_per_capita, y = Measles, color = name)) +
      geom_point() +
      geom_hline(yintercept = 85, linetype = "dashed", color = "red") +
      labs(title = "PIB par hab. vs Immunisation Rougeole",
           x = "PIB par hab.",
           y = "Immunisation Rougeole") +
      theme_minimal() +
      theme(legend.position = "none")
    
  } else if(selected_infection == "Diphtheria") {
    plot <- ggplot(wd_2015, aes(x = GDP_per_capita, y = Diphtheria, color = name)) +
      geom_point() +
      geom_hline(yintercept = 85, linetype = "dashed", color = "red") +
      labs(title = "PIB par hab. vs Immunisation Diphtérie",
           x = "PIB par hab.",
           y = "Immunisation Diphtérie") +
      theme_minimal() +
      theme(legend.position = "none")
    
  } else if(selected_infection == "Incidents_HIV") {
    plot <- ggplot(wd_2015, aes(x = GDP_per_capita, y = Incidents_HIV, color = name)) +
      geom_point() +
      labs(title = "PIB par hab. vs Incidents HIV",
           x = "PIB par hab.",
           y = "Incidents HIV") +
      theme_minimal() +
      theme(legend.position = "none")
    
  } else {
    plot <- NULL
  }
  
  # Retourner le ggplotly de la visualisation sélectionnée
  ggplotly(plot)
}

heat_df = wd_2015 %>% 
  select(-Year,-Region,-name,-Economy_status_Developed.x)

heat_df<- heat_df %>%
  st_set_geometry(NULL) 

cor_matrix <- cor(heat_df, use = "complete.obs") 


cor_melted <- melt(cor_matrix)


corr_plotly <- function()
{
  ggplotly(
    
    # Créer la heatmap de corrélation
    ggplot(data = cor_melted, aes(x = Var1, y = Var2, fill = value)) +
      geom_tile() +  
      scale_fill_gradient2(low = "red", high = "blue", mid = "white", 
                           midpoint = 0, limit = c(-1,1), space = "Lab", 
                           name="Correlation") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(x = "", y = "", title = "Heatmap de Corrélation")
  )
}




####NEw Shiny

library(shiny)
library(plotly)

ui <- fluidPage(
  titlePanel("Espérance de vie OMS 2015"),
  
 
  tabsetPanel(
    
    tabPanel("MAP",
             sidebarLayout(
               sidebarPanel(
                 selectInput("variable", "Indicateur:",
                             choices = c("Infant_deaths", "Under_five_deaths", "Adult_mortality", 
                                         "GDP_per_capita", "Thinness_five_nine_years", 
                                         "Thinness_ten_nineteen_years", "Life_expectancy")),
                 width = 2
               ),
               mainPanel(
                 plotlyOutput("mapPlot"),
                 width = 10
               )
             )
    ),
    # Second onglet
    tabPanel("Infections et PIB",
             sidebarLayout(
               sidebarPanel(
                 # Ajouter un autre selectInput ici pour le deuxième onglet
                 selectInput("infection", "Infection:",
                             choices = c("Polio", "Hepatitis_B", "Measles","Diphtheria","Incidents_HIV")),
                 width = 2# Mettez vos propres options ici
               ),
               mainPanel(
                 # Vous pouvez ajouter des éléments de sortie ici, par exemple un autre graphique plotly
                 plotlyOutput("anotherPlot"),
                 width = 10
               )
             )
    ),
    
    tabPanel("Matrice de corrélation",
             
             
             mainPanel(
               
               plotlyOutput("corrplot")
             )
    )
  )
  
)


server <- function(input, output) {
  selected_value <- reactive({
    input$variable
  })
  
  output$mapPlot <- renderPlotly({
    map_monde_plotly(selected_value() )
  })
  selected_infection <- reactive({
    input$infection
  })
  # Vous pouvez ajouter la logique du serveur pour le deuxième onglet ici
  # Par exemple:
  output$anotherPlot <- renderPlotly({
    infection_plotly(selected_infection())
  })
  
  output$corrplot <- renderPlotly({
    corr_plotly()
  })
}

shinyApp(ui, server)







