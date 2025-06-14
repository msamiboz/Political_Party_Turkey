library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(ggplot2)
library(dplyr)
library(kableExtra)
library(tidyverse)
library(gtsummary)
library(stargazer)

# UI
ui <- dashboardPage(
  dashboardHeader(title = tags$span("Turkish Political Party Analysis", style = "font-size: 16px;")),
  
  dashboardSidebar(
    sidebarMenu(
      # Global data selector at the top of sidebar
      div(style = "padding: 15px;",
        selectInput("data_choice", "Select Dataset:", 
                   choices = list("July 2023" = "july23", 
                                 "January 2025" = "january25"),
                   selected = "january25")
      ),
      
      menuItem("Overview", tabName = "overview", icon = icon("chart-line")),
      menuItem("Party Comparisons", tabName = "comparisons", icon = icon("balance-scale")),
      menuItem("Geographic Analysis", tabName = "geographic", icon = icon("map")),
      menuItem("Statistical Models", tabName = "models", icon = icon("calculator")),
      menuItem("Data Explorer", tabName = "data", icon = icon("table"))
    )
  ),
  
  dashboardBody(
    tabItems(
      # Overview Tab
      tabItem(tabName = "overview",
        fluidRow(
          box(title = "Introduction", status = "info", solidHeader = TRUE, width = 12, collapsible = TRUE,
            p("The vibrant and dynamic landscape of Turkish politics has undergone significant transformations over the years, reflecting the multifaceted nature of its democracy. Central to this democratic process are the political parties that form the cornerstone of the nation's political infrastructure. The composition and strategies of these parties play a pivotal role in shaping the political discourse and in mobilizing citizens to participate in the democratic process."),
            
            p("This research seeks to shed light on the underlying party characteristics that influence party membership, with a particular focus on the independent variables of party age, the presence of co-chairs, and the extent of comprehensive data disclosure."),
            
            p("Turkey's political party system has evolved over time, adapting to changing societal dynamics and political realities. As parties navigate the complex Turkish political terrain, one pertinent question arises: does the age of a political party have an impact on its ability to attract and retain members? Political parties with longer histories may be expected to have developed a broader base of support, while newer parties may struggle to gain a foothold."),
            
            p("Furthermore, the level of information disclosure by political parties can significantly affect their ability to connect with potential members. The degree to which parties share simple information about their chair name, telephone, and address can represent their willingness to be more transparent. Transparency may affect the public's perception of a party's accountability and trustworthiness.")
          )
        ),
        
        fluidRow(
          box(title = "Dataset Information", status = "info", solidHeader = TRUE, width = 12, collapsible = TRUE,
            p("According to the constitution of the Turkish Republic and relevant law (Political Parties Law), the Court of Cassation holds information and oversees political parties in Turkey. The court publishes general information on parties annually. The dataset used in this analysis was collected from these general information tables via web scraping."),
            
            p("The original dataset includes Party name, Chair Name, Telephone, Address and Member count of the parties. Further variables like having co-chair or not and party age were generated from existing variables. In this analysis, party ages are calculated in years from the founding date."),
            
            p(strong("Openness Variable:")),
            p("Since the information collected is simple and basic, the variable named 'openness' represents the party's willingness to be transparent and accountable. The methodology for creating this variable is as follows:"),
            tags$ul(
              tags$li("Maximum score is 5 points"),
              tags$li("For every missing data field (Chair, Telephone, Address, Member Count), the party loses 1 point"),
              tags$li("Higher scores indicate greater transparency and data disclosure")
            ),
            
            p(strong("Co-chair Variable:")),
            p("This binary variable indicates whether a party has co-chairs (TRUE) or a single chair (FALSE). The presence of co-chairs may contribute to distinctive party images and membership recruitment strategies."),
            
            p(em("Note: The analysis focuses on active political parties and excludes parties with zero membership ('zombie parties') for more reliable results."))
          )
        ),
        
        fluidRow(
          box(title = "Dataset Controls", status = "primary", solidHeader = TRUE, width = 12,
            fluidRow(
              column(3, checkboxInput("exclude_akp", "Exclude AKP", value = FALSE)),
              column(3, checkboxInput("exclude_zombie", "Exclude Zombie Parties", value = FALSE)),
              column(3, sliderInput("min_members", "Minimum Members:", 
                                   min = 0, max = 100000, value = 0, step = 1000))
            )
          )
        ),
        
        fluidRow(
          box(title = "Member Count vs Party Age", status = "primary", solidHeader = TRUE, width = 6,
            plotlyOutput("scatter_age_members")
          ),
          
          box(title = "Openness Score Distribution", status = "primary", solidHeader = TRUE, width = 6,
            plotlyOutput("openness_dist")
          )
        ),
        
        fluidRow(
          box(title = "Party Statistics Summary", status = "info", solidHeader = TRUE, width = 12,
            htmlOutput("summary_stats")
          )
        )
      ),
      
      # Party Comparisons Tab
      tabItem(tabName = "comparisons",
        fluidRow(
          box(title = "Dataset and Party Selection", status = "primary", solidHeader = TRUE, width = 12,
            fluidRow(
              column(6,
                selectInput("selected_parties", "Choose Up to 5 Parties:", 
                           choices = NULL, multiple = TRUE, selected = NULL)
              ),
              column(6,
                div(id = "party_selection_info", 
                    p("Please select 1-5 parties to compare.", 
                      style = "color: #777; margin-top: 25px;"))
              )
            )
          )
        ),
        
        fluidRow(
          box(title = "Member Count Comparison", status = "primary", solidHeader = TRUE, width = 6,
            plotlyOutput("comparison_member_count")
          ),
          
          box(title = "Party Age Comparison", status = "primary", solidHeader = TRUE, width = 6,
            plotlyOutput("comparison_age")
          )
        ),
        
        fluidRow(
          box(title = "Openness Score Comparison", status = "primary", solidHeader = TRUE, width = 6,
            plotlyOutput("comparison_openness")
          ),
          
          box(title = "Co-chair Status Comparison", status = "primary", solidHeader = TRUE, width = 6,
            plotlyOutput("comparison_cochair")
          )
        ),
        
        fluidRow(
          box(title = "Party Details", status = "info", solidHeader = TRUE, width = 12,
            DT::dataTableOutput("party_details")
          )
        )
      ),
      
      # Geographic Analysis Tab
      tabItem(tabName = "geographic",
        fluidRow(
          box(title = "Geographic Analysis", status = "primary", solidHeader = TRUE, width = 12,
            h3("In Progress", style = "text-align: center; color: #777;")
          )
        )
      ),
      
      # Statistical Models Tab
      tabItem(tabName = "models",
        
        fluidRow(
          box(title = "Model Explanation", status = "info", solidHeader = TRUE, width = 12, collapsible = TRUE,
            p("In our first model, we applied a standard OLS regression using simple explanatory variables. However, we soon observed that the dependent variable—member count—was not normally distributed. Since this variable represents count data, we transitioned to a Poisson generalized linear model (GLM)."),
            
            p("The Poisson model, however, yielded an excessively high deviance, indicating poor fit. As a result, we moved to a quasi-Poisson model to account for overdispersion. Upon closer inspection, we noticed that some influential observations—primarily the major political parties—were disproportionately affecting the regression results. Since our focus was to investigate the relationship between party age and member count, particularly among smaller parties, we used Cook's distance to identify and exclude these outlier observations."),
            
            p(strong("The following major parties were excluded for this reason:")),
            tags$ul(
              tags$li("Adalet ve Kalkınma Partisi"),
              tags$li("Milliyetçi Hareket Partisi"),
              tags$li("Cumhuriyet Halk Partisi"),
              tags$li("Demokrat Parti"),
              tags$li("İyi Parti")
            ),
            
            p("After removing these influential observations, we also reassessed the necessity of control variables. Variables that did not contribute to a significant reduction in deviance were excluded from the model. This led to the development of our fourth and final model."),
            
            p(strong("Despite these refinements, residual diagnostic plots indicate a lack of model adequacy, suggesting that important unobserved variables may be influencing the dependent variable (i.e., an omitted variable bias is likely present). Consequently, we conclude that there is no clear or statistically reliable relationship between party age and member count based on the available data."), 
              style = "color: #d9534f; font-style: italic;"),
            
            p(em("These findings are true for both datasets, but the main analysis was conducted on the newer dataset."), 
              style = "color: #5bc0de; margin-top: 10px;")
          )
        ),
        
        fluidRow(
          box(title = "Regression Model", status = "primary", solidHeader = TRUE, width = 6,
            htmlOutput("regression_output")
          ),
          
          box(title = "Model Residual Plots", status = "primary", solidHeader = TRUE, width = 6,
            selectInput("model_choice", "Select Model:", 
                       choices = list("Model 1" = "model1", 
                                     "Model 3" = "model3", 
                                     "Model 4" = "model4"),
                       selected = "model1"),
            plotlyOutput("residual_plots")
          )
        )
      ),
      
      # Data Explorer Tab
      tabItem(tabName = "data",
        
        fluidRow(
          box(title = "Raw Data", status = "primary", solidHeader = TRUE, width = 12,
            DT::dataTableOutput("raw_data_table")
          )
        )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Load both datasets
  data_july23 <- reactive({
    # Load July 2023 data
    data <- read_csv("Party_data.csv")
    dataset <- data %>% mutate(Openness= 5-rowSums(is.na(.)),
                               Age=as.numeric(Sys.Date()-Founded),
                               Age=Age/365) %>% 
      separate(Chair,c("Chair","Co_Chair"),sep = "-") %>% 
      mutate(Have_cochair=as.factor(ifelse(is.na(Co_Chair),FALSE,TRUE)))
  })
  
  data_january25 <- reactive({
    # Load January 2025 data
    data2 <- read_csv("Party_data1.csv") %>%
      mutate(Founded = as.Date(Founded,"%d.%m.%Y"),
            Member_Count=as.double(gsub("\\.", "",Member_Count)))
    
    dataset <- data2 %>% mutate(Openness= 5-rowSums(is.na(.)),
                     Age=as.numeric(Sys.Date()-Founded),
                     Age=Age/365) %>% 
      separate(Chair,c("Chair","Co_Chair"),sep = "-") %>% 
      mutate(Have_cochair=as.factor(ifelse(is.na(Co_Chair),FALSE,TRUE)))
  })
  
  # Reactive data selection - simplified approach
  selected_data <- reactive({
    if(input$data_choice == "july23") {
      return(data_july23())
    } else {
      return(data_january25())
    }
  })
  
  # Remove the get_selected_data function and update filtered_data_overview
  filtered_data_overview <- reactive({
    data <- selected_data()
    
    # Apply filters based on input controls
    if(input$exclude_akp){
      data <- data %>% filter(Party_Name != "Adalet ve Kalkınma Partisi")
    } 
    if(input$exclude_zombie){
      data <- data %>% filter(Member_Count > 0)
    } 
    data <- data %>% filter(Member_Count >= input$min_members)
    
    return(data)
  })

  
  # Overview plots
  output$scatter_age_members <- renderPlotly({
    # Create interactive scatter plot using filtered_data_overview()
    p <- ggplot(filtered_data_overview(), aes(x = Age, y = Member_Count)) +
       geom_point(aes(color = Have_cochair, size = Openness,name=Party_Name)) +
       theme_minimal()
     ggplotly(p)
  })
  
  output$openness_dist <- renderPlotly({
    # Create openness distribution plot using filtered_data_overview()
    data <- filtered_data_overview()
    ggplot(data,aes(Openness))+
      geom_histogram()
  })
  
  # Update party choices for comparison based on selected dataset
  observe({
    # Update choices based on selected dataset
    updateSelectInput(session, "selected_parties", 
                     choices = selected_data()$Party_Name)
  })
  
  # Comparison plots
  output$comparison_member_count <- renderPlotly({
    req(input$selected_parties)
    if(length(input$selected_parties) == 0 || length(input$selected_parties) > 5) return(NULL)
    
    comparison_data <- selected_data() %>% 
      filter(Party_Name %in% input$selected_parties)
    
    p <- ggplot(comparison_data, aes(x = reorder(Party_Name, -Member_Count), y = Member_Count, fill = Party_Name)) +
      geom_col() +
      labs(title = "Member Count Comparison", 
           x = "Party", y = "Member Count") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "none") +
      scale_fill_brewer(type = "qual", palette = "Set3")
    
    ggplotly(p)
  })
  
  output$comparison_age <- renderPlotly({
    req(input$selected_parties)
    if(length(input$selected_parties) == 0 || length(input$selected_parties) > 5) return(NULL)
    
    comparison_data <- selected_data() %>% 
      filter(Party_Name %in% input$selected_parties)
    
    p <- ggplot(comparison_data, aes(x = reorder(Party_Name, -Age), y = Age, fill = Party_Name)) +
      geom_col() +
      labs(title = "Party Age Comparison (Years)", 
           x = "Party", y = "Age (Years)") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "none") +
      scale_fill_brewer(type = "qual", palette = "Set3")
    
    ggplotly(p)
  })
  
  output$comparison_openness <- renderPlotly({
    req(input$selected_parties)
    if(length(input$selected_parties) == 0 || length(input$selected_parties) > 5) return(NULL)
    
    comparison_data <- selected_data() %>% 
      filter(Party_Name %in% input$selected_parties)
    
    p <- ggplot(comparison_data, aes(x = reorder(Party_Name, -Openness), y = Openness, fill = Party_Name)) +
      geom_col() +
      labs(title = "Openness Score Comparison", 
           x = "Party", y = "Openness Score (1-5)") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "none") +
      scale_fill_brewer(type = "qual", palette = "Set3") +
      ylim(0, 5)
    
    ggplotly(p)
  })
  
  output$comparison_cochair <- renderPlotly({
    req(input$selected_parties)
    if(length(input$selected_parties) == 0 || length(input$selected_parties) > 5) return(NULL)
    
    comparison_data <- selected_data() %>% 
      filter(Party_Name %in% input$selected_parties) %>%
      mutate(Cochair_Status = ifelse(Have_cochair == TRUE, "Has Co-chair", "Single Chair"))
    
    p <- ggplot(comparison_data, aes(x = Party_Name, fill = Cochair_Status)) +
      geom_bar(stat = "count") +
      labs(title = "Co-chair Status Comparison", 
           x = "Party", y = "Count", fill = "Chair Status") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_fill_manual(values = c("Has Co-chair" = "#66c2a5", "Single Chair" = "#fc8d62"))
    
    ggplotly(p)
  })
  
  output$party_details <- DT::renderDataTable({
    req(input$selected_parties)
    if(length(input$selected_parties) == 0 || length(input$selected_parties) > 5) return(NULL)
    
    comparison_data <- selected_data() %>% 
      filter(Party_Name %in% input$selected_parties) %>%
      select(Party_Name, Chair, Founded, Member_Count, Age, Openness, Have_cochair) %>%
      mutate(Age = round(Age, 1),
             Founded = as.character(Founded)) %>%
      arrange(desc(Member_Count))
    
    DT::datatable(comparison_data, 
                  options = list(scrollX = TRUE, pageLength = 10),
                  rownames = FALSE,
                  colnames = c("Party Name", "Chair", "Founded", "Member Count", 
                              "Age (Years)", "Openness", "Has Co-chair"))
  })
  
  # Statistical models
  output$regression_output <- renderUI({
    dataset <- filtered_data_overview()
    dataset_mod <- dataset %>%  filter(!is.na(Member_Count),!is.na(Age))
    model1 <- dataset %>% lm(Member_Count~Age+Openness+Have_cochair,data = .) 
    
    model3 <- dataset_mod %>% 
      glm(formula=Member_Count~Age+I(Age^2)+I(Age^3)+
            Age*Openness+Age*Have_cochair,data = .,
          family="quasipoisson")
    
    indices_remove <-which(rowSums(abs(dfbetas(model3))>(2/sqrt(108)))>0) %>% unname
    indices_remove2 <- which(cooks.distance(model3) >0.01) %>% unname
    
    dataset_mod2 <- dataset_mod[-(indices_remove2),]
    
    model4 <-  dataset_mod2 %>% 
      glm(formula=Member_Count~Age+I(Age^2)+
            Openness+Have_cochair,data = .,
          family="quasipoisson")
    
    # Store models in reactive values for use in residual plots
    values$model1 <- model1
    values$model3 <- model3
    values$model4 <- model4
    
    # Calculate additional statistics for GLM models
    deviance_stats <- c(
      paste("Deviance:", round(deviance(model1), 2)),
      paste("Deviance:", round(deviance(model3), 2)),
      paste("Deviance:", round(deviance(model4), 2))
    )
    
    # Generate stargazer table with custom styling
    regression_table <- stargazer(model1, model3, model4, 
                                 type = "html",
                                 title = "Regression Results",
                                 column.labels = c("Model 1 (OLS)", "Model 3 (Quasi-Poisson)", "Model 4 (Quasi-Poisson)"),
                                 dep.var.labels = "Member Count",
                                 covariate.labels = c("Age", "Age²", "Age³", "Openness", "Have Co-chair", 
                                                     "Age × Openness", "Age × Have Co-chair"),
                                 omit.stat = c("ser", "f"),
                                 add.lines = list(c("Deviance", 
                                                   round(deviance(model1), 2),
                                                   round(deviance(model3), 2), 
                                                   round(deviance(model4), 2))),
                                 table.placement = "H",
                                 single.row = FALSE)
    
    # Add custom CSS styling to make table wider and more readable
    styled_table <- paste0(
      "<style>
        .stargazer-table { 
          width: 100% !important; 
          font-size: 12px;
          margin: 0 auto;
        }
        .stargazer-table td, .stargazer-table th { 
          padding: 4px 8px !important; 
          text-align: center;
        }
        .stargazer-table th {
          background-color: #f8f9fa;
          font-weight: bold;
        }
      </style>",
      gsub("<table", "<table class='stargazer-table'", regression_table)
    )
    
    HTML(styled_table)
  })
  
  # Add reactive values to store models
  values <- reactiveValues()
  
  output$residual_plots <- renderPlotly({
    req(input$model_choice)
    
    if(input$model_choice == "model1" && !is.null(values$model1)) {
      residual_data <- data.frame(
        fitted = fitted(values$model1),
        residuals = residuals(values$model1)
      )
      p <- ggplot(residual_data, aes(x = fitted, y = residuals)) +
        geom_point() +
        geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
        labs(title = "Model 1 Residuals vs Fitted",
             x = "Fitted Values", y = "Residuals") +
        theme_minimal()
      ggplotly(p)
    } else if(input$model_choice == "model3" && !is.null(values$model3)) {
      residual_data <- data.frame(
        fitted = fitted(values$model3),
        residuals = residuals(values$model3)
      )
      p <- ggplot(residual_data, aes(x = fitted, y = residuals)) +
        geom_point() +
        geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
        labs(title = "Model 3 Residuals vs Fitted",
             x = "Fitted Values", y = "Residuals") +
        theme_minimal()
      ggplotly(p)
    } else if(input$model_choice == "model4" && !is.null(values$model4)) {
      residual_data <- data.frame(
        fitted = fitted(values$model4),
        residuals = residuals(values$model4)
      )
      p <- ggplot(residual_data, aes(x = fitted, y = residuals)) +
        geom_point() +
        geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
        labs(title = "Model 4 Residuals vs Fitted",
             x = "Fitted Values", y = "Residuals") +
        theme_minimal()
      ggplotly(p)
    }
  })
  
  # Data explorer
  output$raw_data_table <- DT::renderDataTable({
    # Use filtered_data_overview()
    data <- filtered_data_overview() %>% mutate(Age=round(Age,2))
    DT::datatable(data, options = list(scrollX = TRUE))
  })
  
  output$summary_stats <- renderUI({
    dataset <- filtered_data_overview()
    
    summary_table <- dataset %>% as_tibble() %>% 
      select(Age, Member_Count, Openness, Have_cochair) %>%
      tbl_summary(missing = "no",
                  statistic = list(all_continuous() ~ "{median} ({sd})")) %>% 
      add_n() %>% 
      bold_labels() %>% 
      as_kable_extra() %>% 
      kable_styling(bootstrap_options = c("striped", "hover"))
    
    HTML(summary_table)
  })
  
}

# Run the application
shinyApp(ui = ui, server = server) 