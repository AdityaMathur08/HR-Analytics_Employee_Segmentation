#  HR EMPLOYEE CLUSTERING WITH PYTHON
#  - EMPLOYEE ATTRITION EXPLORER

# LIBRARIES ----
library(shiny)
library(shinythemes)
library(plotly)

library(tidyverse)
library(tidyquant)

# Preprocessing
library(recipes)

# Python
library(reticulate)

# PYTHON SETUP ----

# Replace this with your conda environment containking sklearn, pandas, & numpy
use_condaenv("py3.8", required = TRUE)

# Source Python Sklearn Functions
source_python("py/clustering.py")
source_python("py/tsne.py")

# APP SETUP ----

selections_default <- c(
    "Sex", "MaritalDesc", "PayRate"
)

selections_available <- c(
    "Sex", "MaritalDesc", 
    "PayRate", "Department",
    "RaceDesc",
    "AgeRel", "TenureRel"
)

selection_names <- c(
    "Sex", "Marital Status",
    "Pay Rate", "Department",
    "Race", "Age", "Tenure"
)

# ---- 1.0 UI ----
ui <- navbarPage(
    title = "Human Resources",
    collapsible = TRUE,
    inverse     = TRUE, 
    theme       = shinytheme("superhero"),
    
    shiny::tabPanel(
        title = "Employee Attrition Explorer",
        sidebarLayout(
            # 1.1 Sidebar ----
            sidebarPanel(
                width = 3,
                h3("Attrition Factors"),
                p("These are potential factors that are used to detect Employee Communities and their relationship to Attrition."),
                br(),
                shiny::checkboxGroupInput(inputId = "selections", label = "Select one or more",
                                          choices     = selections_available, 
                                          selected    = selections_default, 
                                          choiceNames = selection_names),
                shiny::actionButton(inputId = "submit", "Submit", class = "btn-primary")
            ),
            
            # 1.2 Main Panel ----
            mainPanel(
                width = 9,
                div(
                    class = "row",
                    div(
                        class = "col-sm-12 panel",
                        div(class = "panel-heading", h5("Attrition By Cluster")),
                        div(class = "panel-body",
                            plotlyOutput("plotly_attrition", height = 250))
                    )
                ),
                tabsetPanel(
                    tabPanel(
                        "Employee Network Exploration",
                        div(
                            class = "row",
                            div(
                                class = "col-sm-12 panel",
                                div(class = "panel-heading", h5("Employee Clusters")),
                                div(class = "panel-body",
                                    plotlyOutput("plotly_cluster", height = 700))
                            )
                        )
                    ),
                    tabPanel(
                        "Data",
                        div(
                            class = "row",
                            div(
                                class = "col-sm-12 panel",
                                div(class = "panel-heading", h5("Cluster Assigments")),
                                div(
                                    class = "panel-body", 
                                    # Used for debugging
                                    verbatimTextOutput(outputId = "code"),
                                )
                            )
                        )
                    )
                )
            )
        )
    )
)

# ---- 2.0 SERVER ----
server <- function(session, input, output) {
    
    # 2.1 Setup Reactive Values ----
    rv <- reactiveValues()
    
    observeEvent(input$submit, {
        # Process data
        
        rv$data <-  read_csv("data/HRDataset_v13.csv")
        
        rv$data_subset <- rv$data %>%
            mutate(AgeRel    = ( mdy(DOB) - min(mdy(DOB), na.rm = TRUE) ) / dyears(1)) %>%
            mutate(TenureRel = ( mdy(DateofHire) - min(mdy(DateofHire), na.rm = TRUE) ) / dyears(1)) %>%
            select(
                one_of("Employee_Name", input$selections)
            ) %>%
            drop_na()
        
        # Handle user selections based on data type
        num_names <- rv$data_subset %>% select_if(is.numeric) %>% names()
        chr_names <- rv$data_subset %>% select(-Employee_Name) %>% select_if(is.character) %>% names()
        
        rec_obj <- recipe(~ ., rv$data_subset) %>%
            step_rm(Employee_Name) 
        
        if (length(num_names) > 0) {
            rec_obj <- rec_obj %>% step_normalize(all_numeric())
        }
        
        if (length(chr_names) > 0) {
            rec_obj <- rec_obj %>% step_dummy(all_nominal(), one_hot = TRUE)
        }
        
        rec_obj <- rec_obj %>% prep()
        
        
        
        rv$data_subset_processed <- juice(rec_obj)
        
        rv$X              <- as.matrix(rv$data_subset_processed)
        rv$employee_names <- rv$data_subset$Employee_Name
        
        rv$cluster_assignments <- cluster_dbscan(rv$X)
        rv$tsne_embedding      <- tsne_embedding(rv$X) 
            
        rv$employee_cluster_data <- tibble(
            Employee_Name = rv$employee_names,
            cluster_db    = rv$cluster_assignments,
        ) %>%
            bind_cols(as_tibble(rv$tsne_embedding)) %>%
            left_join(rv$data)
        
        rv$attrition_by_cluster_data <- rv$employee_cluster_data %>%
            select(cluster_db, Termd) %>%
            group_by(cluster_db) %>%
            summarise(
                term_rate  = sum(Termd) / length(Termd),
                term_count = n()
            ) %>%
            arrange(desc(term_rate))
        
    }, ignoreNULL = FALSE)
    
    output$code <- renderPrint({
        # Used for debugging
        req(rv$data, nrow(rv$data) > 1)

        list(
            rv$attrition_by_cluster_data
        )
    })
    
    # 2.2 Plotly Attrition by Cluster-----
    output$plotly_attrition <- renderPlotly({
        
        req(rv$attrition_by_cluster_data)
        
        g <- rv$attrition_by_cluster_data %>%
            mutate(cluster_db = as_factor(cluster_db) %>% fct_reorder(term_count)) %>%
            ggplot(aes(cluster_db, term_count)) +
            geom_col(aes(fill = term_rate)) +
            theme_tq() +
            labs(fill = "Attr. Rate", x = "Attrition Count", y = "Cluster Assignment") 
        
        ggplotly(g)
        
    })
    
    # 2.3 Plotly Clustering ----
    output$plotly_cluster <- renderPlotly({
        
        req(rv$employee_cluster_data) 
        
        data_formatted <-  rv$employee_cluster_data %>%
            left_join(rv$attrition_by_cluster_data) %>%
            mutate(description = str_glue("{Employee_Name}
                                  Position = {Position}
                                  MaritalDesc = {MaritalDesc}
                                  Sex = {Sex}
                                  Race = {RaceDesc}
                                  EmpStatusID = {EmpStatusID}
                                  PayRate = {PayRate}
                                  Terminated = {Termd}
                                  Term Reason = {TermReason}
                                  
                                  Cluster Term Rate: {scales::percent(term_rate)}
                                  Cluster Term Count: {term_count}
                                  
                                  ")
            ) %>%
            select(Employee_Name:V2, description, Termd, 
                   term_rate, term_count) 
        
        g <- data_formatted %>%
            ggplot(aes(V1, V2, color = factor(cluster_db))) +
            geom_point(aes(text = description, size = term_rate), alpha = 0.5) +
            scale_color_tq() +
            theme_tq() +
            # theme(legend.position = "none") + 
            labs(color = "Cluster")
        
        
        ggplotly(g)

    })
    
    
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
