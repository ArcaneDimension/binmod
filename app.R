library(shiny)
library(tidyverse)
library(broom)
library(DT)
library(bslib)
library(tidymodels)
library(pROC)
library(pscl)
library(car)
library(rpart)
library(vip)
library(rpart.plot)
library(doParallel)
library(yardstick)
library(rsconnect)
library(plotly)

model_files <- list(
  "Logistic Model" = "logit",
  "Decision Tree Model" = "tree"
)

var_choice <- list(
  "Quantitative" = "quanti",
  "Qualitative" = "quali"
)


# load data --------------------------------------------------------------------

logit_model_est <- readRDS(".//logit_model_est1.rds")
model_data_logit_metrics <- readRDS(".//logit_metrics1.rds")
model_data_tree_metrics <- readRDS(".//tree_metrics1.rds")
logit_model_data_roc <- readRDS(".//logit_roc1.rds")
tree_model_data_roc <- readRDS(".//tree_roc_curve1.rds")
model_data_treefis <- readRDS(".//tree_importance_df1.rds")
model_data_descriptives <- readRDS(".//desc_cont1.rds")
model_data_combineddata <- readRDS(".//combined_data1.rds")
logit_roc_data <- readRDS(".//logit_roc_data.rds")
tree_roc_data <- readRDS(".//tree_roc_data.rds")
logit_auc_value <- readRDS(".//logit_auc_value.rds")
tree_auc_value <- readRDS(".//tree_auc_value.rds")

# UI ----------------------------------------------------------------------
ui <- 
  
  page_navbar(
    theme = bs_theme(version = 5, bootswatch = "minty"),
    title = "BINARY MODEL",
    
    sidebar = sidebar(
      #tags$style(HTML(customcss)),
      fillable = FALSE,
      fill = FALSE,
      selectizeInput(
        inputId = "model_choice",
        label = "Select a Model:",
        choices = names(model_files),
        selected = "Logistic Model"
      )

    ),
    
    #uiOutput("model_title"),
    
    nav_panel(
      "Descriptives",
      
      selectizeInput(
        inputId = "vartype",
        label = "Variable Type:",
        choices = names(var_choice),
        selected = "Quantitative"
      ),
      
      conditionalPanel(
        condition = "input.vartype == 'Quantitative'",
        card(
          card_title("Farmers' willingness to continue planting rice"),
          tableOutput("descriptives"),
          full_screen = TRUE
        )
      ),
      
      conditionalPanel(
        condition = "input.vartype == 'Qualitative'",
        fluidRow(
          column(4,
                 card(
                   plotlyOutput("plant_sex_colchart", height = "400px", width = "550px"),
                   full_screen = TRUE
                 )
          ),
        
          column(4,
                 card(
                   plotlyOutput("plant_civil_colchart", height = "400px", width = "550px"),
                   full_screen = TRUE
                 )
          ),
          
          column(4,
                 card(
                   plotlyOutput("plant_majorsourceincome_colchart", height = "400px", width = "550px"),
                   full_screen = TRUE
                 )
          ),
          
          column(4,
                 card(
                   plotlyOutput("plant_orgmemb_colchart", height = "400px", width = "550px"),
                   full_screen = TRUE
                 )
          ),
          
          column(4,
                 card(
                   plotlyOutput("plant_attendseminar_colchart", height = "400px", width = "550px"),
                   full_screen = TRUE
                 )
          ),
          
          column(4,
                 card(
                   plotlyOutput("plant_eco_colchart", height = "400px", width = "550px"),
                   full_screen = TRUE
                 )
          ),
          
          column(4,
                 card(
                   plotlyOutput("plant_tenurial_colchart", height = "400px", width = "550px"),
                   full_screen = TRUE
                 )
          ),
          
          column(4,
                 card(
                   plotlyOutput("plant_seedtype_colchart", height = "400px", width = "550px"),
                   full_screen = TRUE
                 )
          ),
          
          column(4,
                 card(
                   plotlyOutput("plant_season_colchart", height = "400px", width = "550px"),
                   full_screen = TRUE
                 )
          ),
          
          column(4,
                 card(
                   plotlyOutput("plant_darate_colchart", height = "400px", width = "550px"),
                   full_screen = TRUE
                 )
          ),
          
          column(4,
                 card(
                   plotlyOutput("plant_qualitylife_colchart", height = "400px", width = "550px"),
                   full_screen = TRUE
                 )
          ),
          
          column(4,
                 card(
                   plotlyOutput("plant_availgovserv_colchart", height = "400px", width = "550px"),
                   full_screen = TRUE
                 )
          ),
          
          column(4,
                 card(
                   plotlyOutput("plant_wateravail_colchart", height = "400px", width = "550px"),
                   full_screen = TRUE
                 )
          ),
          
          column(4,
                 card(
                   plotlyOutput("plant_prodloss_colchart", height = "400px", width = "550px"),
                   full_screen = TRUE
                 )
          ),
          
          column(4,
                 card(
                   plotlyOutput("plant_capitalsource_colchart", height = "400px", width = "550px"),
                   full_screen = TRUE
                 )
          ),
          
        )
      )
      
    ),
    
    nav_panel(
      "Model Factors",
      
      conditionalPanel(
        condition = "input.model_choice == 'Logistic Model'",
        card(
          card_title("Model Coefficients"),
          dataTableOutput("model_coefficients"),
          full_screen = TRUE,
          height = "1000px"
        )
      ),
      
      conditionalPanel(
        condition = "input.model_choice == 'Decision Tree Model'",
        card(
          card_title("Feature Importance Scores"),
          plotlyOutput("scores_plot", height = "600px", width = "900px"),
          full_screen = TRUE,
          style = "width: 60%;"
        )
      )
      
    ),
    
    nav_panel(
      "Model Performance",
      
      conditionalPanel(
        condition = "input.model_choice == 'Logistic Model'",
        card(
          tableOutput("logit_model_summary"),
          style = "width: 25%;"
        )
      ),
      
      conditionalPanel(
        condition = "input.model_choice == 'Decision Tree Model'",
        card(
          tableOutput("tree_model_summary"),
          style = "width: 25%;"
        )
      )
      
    ),
    
    nav_panel(
      "ROC Curve",
      conditionalPanel(
        condition = "input.model_choice == 'Logistic Model'",
        card(
          plotlyOutput("logit_roc_plot", height = "600px", width = "600px"),
          full_screen = TRUE,
          style = "width: 40%;"
        )
      ),
      
      conditionalPanel(
        condition = "input.model_choice == 'Decision Tree Model'",
        card(
          plotlyOutput("tree_roc_plot", height = "600px", width = "600px"),
          full_screen = TRUE,
          style = "width: 40%;"
        )
      )
      
    )
    
  )
  
  

# SERVER ------------------------------------------------------------------

server <- function(input, output) {
  
  # model title ------------------------------------------------------------------
  output$model_title <- renderUI({
    req(input$model_choice)
    h3(paste(input$model_choice, "for farmer's willingness to continue planting rice"))
  })
  
  # render descriptives table ------------------------------------------------------------------
  output$descriptives <- renderTable({
    req(model_data_descriptives)
    model_data_descriptives
  })
  
  # render logit metrics table ------------------------------------------------------------------
  output$logit_model_summary <- renderTable({
    req(model_data_logit_metrics)
    model_data_logit_metrics
  })
  
  # render logit estimates table ------------------------------------------------------------------
  output$model_coefficients <- DT::renderDataTable({
    req(logit_model_est)
    datatable(logit_model_est, options = list(pageLength = 50))
  })
  
  # render tree metrics table ------------------------------------------------------------------
  output$tree_model_summary <- renderTable({
    req(model_data_tree_metrics)
    model_data_tree_metrics
  })
  
  # render logit roc plot ------------------------------------------------------------------
  output$logit_roc_plot <- renderPlotly({
    req(logit_roc_data, logit_auc_value)
    
    plot_ly(logit_roc_data, x = ~Specificity, y = ~Sensitivity, type = 'scatter', mode = 'lines',
            line = list(color = 'blue'), name = 'ROC Curve') |> 
      add_annotations(
        x = 0.6, y = 0.3,
        text = paste("AUC:", round(logit_auc_value, 3)),
        showarrow = FALSE,
        font = list(size = 15)
      ) |> 
      layout(
        title = "ROC Curve for Logit Model",
        xaxis = list(title = "1 - Specificity"),
        yaxis = list(title = "Sensitivity"),
        showlegend = FALSE
      ) |> 
      add_lines(x = c(0, 1), 
                y = c(0, 1), 
                name = "Diagonal Line", 
                line = list(dash = "dash", 
                            color = "red"))
  })
  
  # render tree roc plot ------------------------------------------------------------------
  output$tree_roc_plot <- renderPlotly({
    req(tree_roc_data, tree_auc_value)
    
    plot_ly(tree_roc_data, x = ~Specificity, y = ~Sensitivity, type = 'scatter', mode = 'lines',
            line = list(color = 'blue'), name = 'ROC Curve') |> 
      add_annotations(
        x = 0.6, y = 0.3,
        text = paste("AUC:", round(tree_auc_value, 3)),
        showarrow = FALSE,
        font = list(size = 15)
      ) |> 
      layout(
        title = "ROC Curve for Decision Tree Model",
        xaxis = list(title = "1 - Specificity"),
        yaxis = list(title = "Sensitivity"),
        showlegend = FALSE
      ) |> 
      add_lines(x = c(0, 1), 
                y = c(0, 1), 
                name = "Diagonal Line", 
                line = list(dash = "dash", 
                            color = "red"))
  })
  
  # render tree scores plot ------------------------------------------------------------------
  output$scores_plot <- renderPlotly({
    req(model_data_treefis)
    
    data <- model_data_treefis
    
    plot_ly(
      data, 
      x = ~Importance, 
      y = ~Feature, 
      type = 'bar') |> 
      layout(
        xaxis = list(title = "Importance",
                     automargin = TRUE
                     ),
        yaxis = list(title = "Feature",
                     categoryorder = "total ascending",
                     tickfont = list(size = 10),
                     automargin = TRUE
                     ),
        margin = list(l = 100)
      )
    
  })

  # render sex-plant chart ------------------------------------------------------------------
  output$plant_sex_colchart <- renderPlotly({
    req(model_data_combineddata)
    
    combined_data <- model_data_combineddata
    
    data <-  table(combined_data$pricepaddylowstillplant, combined_data$sex) |> 
      as.data.frame() |> 
      rename(
        plant = Var1,
        sex = Var2
      ) |> 
      group_by(plant) |> 
      mutate(
        perc = round((Freq/sum(Freq)*100), 2),
        sex = str_to_sentence(sex)
      ) |> 
      select(plant,sex,perc) |> 
      pivot_wider(
        names_from = sex,
        values_from = perc
      ) |> 
      mutate(plant = factor(plant, levels = c("yes", "no")))
    
    plot_ly(
      data,
      x = ~plant
      ) |> 
      add_bars(
        y = ~ `Female`,
        width = 0.4,
        name = 'Female',
        text = paste(data$Female, "%"),
        marker = list(color = '#FFD167',
                      line = list(color = "black",
                                  width = 1.5)),
        hoverinfo = 'y+name',
        hovertemplate = paste('%{y}%')
      ) |>
      add_bars(
        y = ~ `Male`,
        width = 0.4,
        name = 'Male',
        text = paste(data$Male, "%"),
        marker = list(color = '#108AB1',
                      line = list(color = "black",
                                  width = 2)),
        hoverinfo = 'y+name',
        hovertemplate = paste('%{y}%')
      ) |>
      
      layout(
        title = "Sex by Willingness to Plant",
        margin = list(t = 100, b = 100),
        showlegend = TRUE,
        barmode = 'stack',
        xaxis = list(title = "Willingnes to Plant"),
        yaxis = list(title = "Percentage (%)"),
        legend = list(x = 1)
      )
  })
  
  # render civil-plant chart ------------------------------------------------------------------
  output$plant_civil_colchart <- renderPlotly({
    req(model_data_combineddata)
    
    combined_data <- model_data_combineddata
    
    data <-  table(combined_data$pricepaddylowstillplant, combined_data$civil) |> 
      as.data.frame() |> 
      rename(
        plant = Var1,
        civil = Var2
      ) |> 
      group_by(plant) |> 
      mutate(
        perc = round((Freq/sum(Freq)*100), 2),
        civil = str_to_sentence(civil)
      ) |> 
      select(plant,civil,perc) |> 
      pivot_wider(
        names_from = civil,
        values_from = perc
      ) |> 
      mutate(plant = factor(plant, levels = c("yes", "no")))
    
    plot_ly(
      data,
      x = ~plant
      ) |> 
      add_bars(
        y = ~ `Separated`,
        width = 0.4,
        type = 'bar',
        name = 'Separated',
        text = paste(data$Separated, "%"),
        marker = list(color = '#F78C6A',
                      line = list(color = "black",
                                  width = 1.5)),
        hoverinfo = 'y+name',
        hovertemplate = paste('%{y}%')
      ) |>
      add_bars(
        y = ~ `Widow`,
        width = 0.4,
        name = 'Widow',
        text = paste(data$Widow, "%"),
        marker = list(color = '#FFD167',
                      line = list(color = "black",
                                  width = 1.5)),
        hoverinfo = 'y+name',
        hovertemplate = paste('%{y}%')
      ) |>
      add_bars(
        y = ~ `Married`,
        width = 0.4,
        name = 'Married',
        text = paste(data$Married, "%"),
        marker = list(color = '#06D7A0',
                      line = list(color = "black",
                                  width = 1.5)),
        hoverinfo = 'y+name',
        hovertemplate = paste('%{y}%')
      ) |>
      add_bars(
        y = ~ `Single`,
        width = 0.4,
        name = 'Single',
        text = paste(data$Single, "%"),
        marker = list(color = '#108AB1',
                      line = list(color = "black",
                                  width = 1.5)),
        hoverinfo = 'y+name',
        hovertemplate = paste('%{y}%')
      ) |>
      
      layout(
        title = "Civil Status by Willingness to Plant",
        margin = list(t = 100, b = 100),
        showlegend = TRUE,
        barmode = 'stack',
        xaxis = list(title = "Willingnes to Plant"),
        yaxis = list(title = "Percentage (%)"),
        legend = list(x = 1)
      )
  })
  
  # render majorsourceincome-plant chart ------------------------------------------------------------------
  output$plant_majorsourceincome_colchart <- renderPlotly({
    req(model_data_combineddata)
    
    combined_data <- model_data_combineddata
    
    data <-  table(combined_data$pricepaddylowstillplant, combined_data$majorsourceincome2) |> 
      as.data.frame() |> 
      rename(
        plant = Var1,
        majorsourceincome2 = Var2
      ) |> 
      group_by(plant) |> 
      mutate(
        perc = round(Freq/sum(Freq)*100, 2),
        majorsourceincome2 = str_to_sentence(majorsourceincome2)
      ) |> 
      select(plant,majorsourceincome2,perc) |> 
      pivot_wider(
        names_from = majorsourceincome2,
        values_from = perc
      ) |> 
      mutate(plant = factor(plant, levels = c("yes", "no")))
    
    plot_ly(
      data,
      x = ~plant
      ) |> 
      add_bars(
        y = ~ `Others`,
        width = 0.4,
        name = 'Others',
        text = paste(data$`Others`, "%"),
        marker = list(color = '#FFF883',
                      line = list(color = "black",
                                  width = 1.5)),
        hoverinfo = 'y+name',
        hovertemplate = paste('%{y}%')
      ) |>
      add_bars(
        y = ~ `Salary employment (private/government)`,
        width = 0.4,
        name = 'Salary employment',
        text = paste(data$`Salary employment (private/government)`, "%"),
        marker = list(color = '#F57F17',
                      line = list(color = "black",
                                  width = 1.5)),
        hoverinfo = 'y+name',
        hovertemplate = paste('%{y}%')
      ) |>
      add_bars(
        y = ~ `Livestock production`,
        width = 0.4,
        name = 'Livestock production',
        text = paste(data$`Livestock production`, "%"),
        marker = list(color = '#C62828',
                      line = list(color = "black",
                                  width = 1.5)),
        hoverinfo = 'y+name',
        hovertemplate = paste('%{y}%')
      ) |>
      add_bars(
        y = ~ `Self-employment/business`,
        width = 0.4,
        name = 'Self-employment/business',
        text = paste(data$`Self-employment/business`, "%"),
        marker = list(color = '#C2185B',
                      line = list(color = "black",
                                  width = 1.5)),
        hoverinfo = 'y+name',
        hovertemplate = paste('%{y}%')
      ) |>
      add_bars(
        y = ~ `Vegetable farming`,
        width = 0.4,
        name = 'Vegetable farming',
        text = paste(data$`Vegetable farming`, "%"),
        marker = list(color = '#4A148C',
                      line = list(color = "black",
                                  width = 1.5)),
        hoverinfo = 'y+name',
        hovertemplate = paste('%{y}%')
      ) |>
      add_bars(
        y = ~ `Rice farming`,
        width = 0.4,
        name = 'Rice Farming',
        text = paste(data$`Rice farming`, "%"),
        marker = list(color = '#3B93E4',
                      line = list(color = "black",
                                  width = 1.5)),
        hoverinfo = 'y+name',
        hovertemplate = paste('%{y}%')
      ) |> 
      
      layout(
        title = "Majour Source of Income by Willingness to Plant",
        margin = list(t = 100, b = 100, r = 20),
        showlegend = TRUE,
        barmode = 'stack',
        xaxis = list(title = "Willingnes to Plant"),
        yaxis = list(title = "Percentage (%)"),
        legend = list(x = 1)
      )
  })
  
  # render orgmemb-plant chart ------------------------------------------------------------------
  output$plant_orgmemb_colchart <- renderPlotly({
    req(model_data_combineddata)
    
    combined_data <- model_data_combineddata
    
    data <-  table(combined_data$pricepaddylowstillplant, combined_data$orgmember) |> 
      as.data.frame() |> 
      rename(
        plant = Var1,
        orgmemb = Var2
      ) |> 
      group_by(plant) |> 
      mutate(
        perc = round(Freq/sum(Freq)*100, 2),
        orgmemb = str_to_sentence(orgmemb)
      ) |> 
      select(plant,orgmemb,perc) |> 
      pivot_wider(
        names_from = orgmemb,
        values_from = perc
      ) |> 
      mutate(plant = factor(plant, levels = c("yes", "no")))
    
    plot_ly(
      data,
      x = ~plant
    ) |> 
      add_bars(
        y = ~ `No`,
        width = 0.4,
        name = 'Non-member',
        text = paste(data$No, "%"),
        marker = list(color = '#FFD167',
                      line = list(color = "black",
                                  width = 1.5)),
        hoverinfo = 'y+name',
        hovertemplate = paste('%{y}%')
      ) |>
      add_bars(
        y = ~ `Yes`,
        width = 0.4,
        name = 'Member',
        text = paste(data$Yes, "%"),
        marker = list(color = '#108AB1',
                      line = list(color = "black",
                                  width = 1.5)),
        hoverinfo = 'y+name',
        hovertemplate = paste('%{y}%')
      ) |>
      
      layout(
        title = "Organization Membership by Willingness to Plant",
        margin = list(t = 100, b = 100),
        showlegend = TRUE,
        barmode = 'stack',
        xaxis = list(title = "Willingnes to Plant"),
        yaxis = list(title = "Percentage (%)"),
        legend = list(x = 1)
      )
  })
  
  # render attendseminar-plant chart ------------------------------------------------------------------
  output$plant_attendseminar_colchart <- renderPlotly({
    req(model_data_combineddata)
    
    combined_data <- model_data_combineddata
    
    data <-  table(combined_data$pricepaddylowstillplant, combined_data$attendseminar) |> 
      as.data.frame() |> 
      rename(
        plant = Var1,
        attendseminar = Var2
      ) |> 
      group_by(plant) |> 
      mutate(
        perc = round(Freq/sum(Freq)*100, 2),
        attendseminar = str_to_sentence(attendseminar)
      ) |> 
      select(plant,attendseminar,perc) |> 
      pivot_wider(
        names_from = attendseminar,
        values_from = perc
      ) |> 
      mutate(plant = factor(plant, levels = c("yes", "no")))
    
    plot_ly(
      data,
      x = ~plant
    ) |> 
      add_bars(
        y = ~ `No`,
        width = 0.4,
        name = 'Did not attend seminar',
        text = paste(data$No, "%"),
        marker = list(color = '#FFD167',
                      line = list(color = "black",
                                  width = 1.5)),
        hoverinfo = 'y+name',
        hovertemplate = paste('%{y}%')
      ) |>
      add_bars(
        y = ~ `Yes`,
        width = 0.4,
        name = 'Attended seminar',
        text = paste(data$Yes, "%"),
        marker = list(color = '#108AB1',
                      line = list(color = "black",
                                  width = 1.5)),
        hoverinfo = 'y+name',
        hovertemplate = paste('%{y}%')
      ) |>
      
      layout(
        title = "Seminar/Training Attendance by Willingness to Plant",
        margin = list(t = 100, b = 100),
        showlegend = TRUE,
        barmode = 'stack',
        xaxis = list(title = "Willingnes to Plant"),
        yaxis = list(title = "Percentage (%)"),
        legend = list(x = 1)
      )
  })
  
  # render ecosystem-plant chart ------------------------------------------------------------------
  output$plant_eco_colchart <- renderPlotly({
    req(model_data_combineddata)
    
    combined_data <- model_data_combineddata
      
    data <-  table(combined_data$pricepaddylowstillplant, combined_data$ecosystem) |> 
      as.data.frame() |> 
      rename(
        plant = Var1,
        ecosystem = Var2
      ) |> 
      group_by(plant) |> 
      mutate(
        perc = round(Freq/sum(Freq)*100, 2),
        ecosystem = str_to_sentence(ecosystem)
      ) |> 
      select(plant,ecosystem,perc) |> 
      pivot_wider(
        names_from = ecosystem,
        values_from = perc
      ) |> 
      mutate(plant = factor(plant, levels = c("yes", "no")))
    
    plot_ly(
      data,
      x = ~plant
    ) |> 
      add_bars(
        y = ~ `Rainfed`,
        width = 0.4,
        name = 'Rainfed',
        text = paste(data$Rainfed, "%"),
        marker = list(color = '#FFD167',
                      line = list(color = "black",
                                  width = 1.5)),
        hoverinfo = 'y+name',
        hovertemplate = paste('%{y}%')
      ) |>
      add_bars(
        y = ~ `Irrigated`,
        width = 0.4,
        name = 'Irrigated',
        text = paste(data$Irrigated, "%"),
        marker = list(color = '#108AB1',
                      line = list(color = "black",
                                  width = 1.5)),
        hoverinfo = 'y+name',
        hovertemplate = paste('%{y}%')
      ) |>
      
      layout(
        title = "Ecosystem by Willingness to Plant",
        margin = list(t = 100, b = 100),
        showlegend = TRUE,
        barmode = 'stack',
        xaxis = list(title = "Willingnes to Plant"),
        yaxis = list(title = "Percentage (%)"),
        legend = list(x = 1)
      )
  })
  
  # render tenurial-plant chart ------------------------------------------------------------------
  output$plant_tenurial_colchart <- renderPlotly({
    req(model_data_combineddata)
    
    combined_data <- model_data_combineddata
    
    data <-  table(combined_data$pricepaddylowstillplant, combined_data$tenurialstatus) |> 
      as.data.frame() |> 
      rename(
        plant = Var1,
        tenurialstatus = Var2
      ) |> 
      group_by(plant) |> 
      mutate(
        perc = round(Freq/sum(Freq)*100, 2),
        tenurialstatus = str_to_sentence(tenurialstatus)
      ) |> 
      select(plant,tenurialstatus,perc) |> 
      pivot_wider(
        names_from = tenurialstatus,
        values_from = perc
      ) |> 
      mutate(plant = factor(plant, levels = c("yes", "no")))
    
    plot_ly(
      data,
      x = ~plant
    ) |> 
      add_bars(
        y = ~ `Others`,
        width = 0.4,
        name = 'Others',
        text = paste(data$Others, "%"),
        marker = list(color = '#F04770',
                      line = list(color = "black",
                                  width = 1.5)),
        hoverinfo = 'y+name',
        hovertemplate = paste('%{y}%')
      ) |>
      add_bars(
        y = ~ `Rent-free`,
        width = 0.4,
        name = 'Rent-free',
        text = paste(data$`Rent-free`, "%"),
        marker = list(color = '#F78C6A',
                      line = list(color = "black",
                                  width = 1.5)),
        hoverinfo = 'y+name',
        hovertemplate = paste('%{y}%')
      ) |>
      add_bars(
        y = ~ `Tenant`,
        width = 0.4,
        name = 'Tenant',
        text = paste(data$Tenant, "%"),
        marker = list(color = '#FFD167',
                      line = list(color = "black",
                                  width = 1.5)),
        hoverinfo = 'y+name',
        hovertemplate = paste('%{y}%')
      ) |> 
      add_bars(
        y = ~ `Lessee`,
        width = 0.4,
        name = 'Lessee',
        text = paste(data$Lessee, "%"),
        marker = list(color = '#06D7A0',
                      line = list(color = "black",
                                  width = 1.5)),
        hoverinfo = 'y+name',
        hovertemplate = paste('%{y}%')
      ) |> 
      add_bars(
        y = ~ `Amortizing/clt (partially paid)`,
        width = 0.4,
        name = 'Amortizing/clt (partially paid)',
        text = paste(data$`Amortizing/clt (partially paid)`, "%"),
        marker = list(color = '#108AB1',
                      line = list(color = "black",
                                  width = 1.5)),
        hoverinfo = 'y+name',
        hovertemplate = paste('%{y}%')
      ) |>
      add_bars(
        y = ~ `Owner/clt (fully paid)`,
        width = 0.4,
        name = 'Owner/clt (fully paid)',
        text = paste(data$`Owner/clt (fully paid)`, "%"),
        marker = list(color = '#073A4B',
                      line = list(color = "black",
                                  width = 1.5)),
        hoverinfo = 'y+name',
        hovertemplate = paste('%{y}%')
      ) |>
      
      layout(
        title = "Tenurial Status by Willingness to Plant",
        margin = list(t = 100, b = 100),
        showlegend = TRUE,
        barmode = 'stack',
        xaxis = list(title = "Willingnes to Plant"),
        yaxis = list(title = "Percentage (%)"),
        legend = list(x = 1)
      )
  })
  
  # render seed type-plant chart ------------------------------------------------------------------
  output$plant_seedtype_colchart <- renderPlotly({
    req(model_data_combineddata)
    
    combined_data <- model_data_combineddata
    
    data <-  table(combined_data$pricepaddylowstillplant, combined_data$seedtype) |> 
      as.data.frame() |> 
      rename(
        plant = Var1,
        seedtype = Var2
      ) |> 
      group_by(plant) |> 
      mutate(
        perc = round(Freq/sum(Freq)*100, 2),
        seedtype = str_to_sentence(seedtype)
      ) |> 
      select(plant,seedtype,perc) |> 
      pivot_wider(
        names_from = seedtype,
        values_from = perc
      ) |> 
      mutate(plant = factor(plant, levels = c("yes", "no")))
    
    plot_ly(
      data,
      x = ~plant
    ) |> 
      add_bars(
        y = ~ `Low quality inbred seeds`,
        width = 0.4,
        name = 'Low quality inbred seeds',
        text = paste(data$`Low quality inbred seeds`, "%"),
        marker = list(color = '#FFD167',
                      line = list(color = "black",
                                  width = 1.5)),
        hoverinfo = 'y+name',
        hovertemplate = paste('%{y}%')
      ) |>
      add_bars(
        y = ~ `Hybrid`,
        width = 0.4,
        name = 'Hybrid',
        text = paste(data$Hybrid, "%"),
        marker = list(color = '#06D7A0',
                      line = list(color = "black",
                                  width = 1.5)),
        hoverinfo = 'y+name',
        hovertemplate = paste('%{y}%')
      ) |>
      add_bars(
        y = ~ `High quality inbred seeds`,
        width = 0.4,
        name = 'High quality inbred seeds',
        text = paste(data$`High quality inbred seeds`, "%"),
        marker = list(color = '#108AB1',
                      line = list(color = "black",
                                  width = 1.5)),
        hoverinfo = 'y+name',
        hovertemplate = paste('%{y}%')
      ) |>
      
      layout(
        title = "Seed Type by Willingness to Plant",
        margin = list(t = 100, b = 100),
        showlegend = TRUE,
        barmode = 'stack',
        xaxis = list(title = "Willingnes to Plant"),
        yaxis = list(title = "Percentage (%)"),
        legend = list(x = 1)
      )
  })
  
  # render season-plant chart ------------------------------------------------------------------
  output$plant_season_colchart <- renderPlotly({
    req(model_data_combineddata)
    
    combined_data <- model_data_combineddata
    
    data <-  table(combined_data$pricepaddylowstillplant, combined_data$data_season) |> 
      as.data.frame() |> 
      rename(
        plant = Var1,
        data_season = Var2
      ) |> 
      group_by(plant) |> 
      mutate(
        perc = round(Freq/sum(Freq)*100, 2)
      ) |> 
      select(plant,data_season,perc) |> 
      pivot_wider(
        names_from = data_season,
        values_from = perc
      ) |> 
      mutate(plant = factor(plant, levels = c("yes", "no")))
    
    plot_ly(
      data,
      x = ~plant
    ) |> 
      add_bars(
        y = ~ `DS`,
        width = 0.4,
        name = 'DS',
        text = paste(data$DS, "%"),
        marker = list(color = '#FFD167',
                      line = list(color = "black",
                                  width = 1.5)),
        hoverinfo = 'y+name',
        hovertemplate = paste('%{y}%')
      ) |>
      add_bars(
        y = ~ `WS`,
        width = 0.4,
        name = 'WS',
        text = paste(data$WS, "%"),
        marker = list(color = '#108AB1',
                      line = list(color = "black",
                                  width = 1.5)),
        hoverinfo = 'y+name',
        hovertemplate = paste('%{y}%')
      ) |>
      
      layout(
        title = "Season by Willingness to Plant",
        margin = list(t = 100, b = 100),
        showlegend = TRUE,
        barmode = 'stack',
        xaxis = list(title = "Willingnes to Plant"),
        yaxis = list(title = "Percentage (%)"),
        legend = list(x = 1)
      )
  })
  
  # render da rate-plant chart ------------------------------------------------------------------
  output$plant_darate_colchart <- renderPlotly({
    req(model_data_combineddata)
    
    combined_data <- model_data_combineddata
    
    data <-  table(combined_data$pricepaddylowstillplant, combined_data$darate) |> 
      as.data.frame() |> 
      rename(
        plant = Var1,
        darate = Var2
      ) |> 
      group_by(plant) |> 
      mutate(
        perc = round(Freq/sum(Freq)*100, 2)
      ) |> 
      select(plant,darate,perc) |> 
      pivot_wider(
        names_from = darate,
        values_from = perc
      ) |> 
      mutate(plant = factor(plant, levels = c("yes", "no")))
    
    plot_ly(
      data,
      x = ~plant
    ) |> 
      add_bars(
        y = ~ `very dissatisfied`,
        width = 0.4,
        name = 'Very dissatisfied',
        text = paste(data$`very dissatisfied`, "%"),
        marker = list(color = '#F04770',
                      line = list(color = "black",
                                  width = 1.5)),
        hoverinfo = 'y+name',
        hovertemplate = paste('%{y}%')
      ) |>
      add_bars(
        y = ~ `dissatisfied`,
        width = 0.4,
        name = 'Dissatisfied',
        text = paste(data$dissatisfied, "%"),
        marker = list(color = '#F78C6A',
                      line = list(color = "black",
                                  width = 1.5)),
        hoverinfo = 'y+name',
        hovertemplate = paste('%{y}%')
      ) |>
      add_bars(
        y = ~ `don't know`,
        width = 0.4,
        name = 'Dont know',
        text = paste(data$`don't know`, "%"),
        marker = list(color = '#FFD167',
                      line = list(color = "black",
                                  width = 1.5)),
        hoverinfo = 'y+name',
        hovertemplate = paste('%{y}%')
      ) |>
      add_bars(
        y = ~ `satisfied`,
        width = 0.4,
        name = 'Satisfied',
        text = paste(data$satisfied, "%"),
        marker = list(color = '#06D7A0',
                      line = list(color = "black",
                                  width = 1.5)),
        hoverinfo = 'y+name',
        hovertemplate = paste('%{y}%')
      ) |>
      add_bars(
        y = ~ `very satisfied`,
        width = 0.4,
        name = 'Very satisfied',
        text = paste(data$`very satisfied`, "%"),
        marker = list(color = '#108AB1',
                      line = list(color = "black",
                                  width = 1.5)),
        hoverinfo = 'y+name',
        hovertemplate = paste('%{y}%')
      ) |>
      
      layout(
        title = "DA Rating by Willingness to Plant",
        margin = list(t = 100, b = 100),
        showlegend = TRUE,
        barmode = 'stack',
        xaxis = list(title = "Willingnes to Plant"),
        yaxis = list(title = "Percentage (%)"),
        legend = list(x = 1)
      )
  })
  
  # render quality of life-plant chart ------------------------------------------------------------------
  output$plant_qualitylife_colchart <- renderPlotly({
    req(model_data_combineddata)
    
    combined_data <- model_data_combineddata
    
    data <-  table(combined_data$pricepaddylowstillplant, combined_data$assesslife) |> 
      as.data.frame() |> 
      rename(
        plant = Var1,
        assesslife = Var2
      ) |> 
      group_by(plant) |> 
      mutate(
        perc = round(Freq/sum(Freq)*100, 2)
      ) |> 
      select(plant,assesslife,perc) |> 
      pivot_wider(
        names_from = assesslife,
        values_from = perc
      ) |> 
      mutate(plant = factor(plant, levels = c("yes", "no")))
    
    plot_ly(
      data,
      x = ~plant
    ) |> 
      add_bars(
        y = ~ `worsen`,
        width = 0.4,
        name = 'Worsen',
        text = paste(data$worsen, "%"),
        marker = list(color = '#FFD167',
                      line = list(color = "black",
                                  width = 1.5)),
        hoverinfo = 'y+name',
        hovertemplate = paste('%{y}%')
      ) |>
      add_bars(
        y = ~ `same`,
        width = 0.4,
        name = 'Same',
        text = paste(data$same, "%"),
        marker = list(color = '#06D7A0',
                      line = list(color = "black",
                                  width = 1.5)),
        hoverinfo = 'y+name',
        hovertemplate = paste('%{y}%')
      ) |>
      add_bars(
        y = ~ `improved`,
        width = 0.4,
        name = 'Improved',
        text = paste(data$improved, "%"),
        marker = list(color = '#108AB1',
                      line = list(color = "black",
                                  width = 1.5)),
        hoverinfo = 'y+name',
        hovertemplate = paste('%{y}%')
      ) |>
      
      layout(
        title = "Quality of Life by Willingness to Plant",
        margin = list(t = 100, b = 100),
        showlegend = TRUE,
        barmode = 'stack',
        xaxis = list(title = "Willingnes to Plant"),
        yaxis = list(title = "Percentage (%)"),
        legend = list(x = 1)
      )
  })
  
  # render gov service availed-plant chart ------------------------------------------------------------------
  output$plant_availgovserv_colchart <- renderPlotly({
    req(model_data_combineddata)
    
    combined_data <- model_data_combineddata
    
    data <-  table(combined_data$pricepaddylowstillplant, combined_data$gov_serv_ind) |> 
      as.data.frame() |> 
      rename(
        plant = Var1,
        gov_serv_ind = Var2
      ) |> 
      group_by(plant) |> 
      mutate(
        perc = round(Freq/sum(Freq)*100, 2)
      ) |> 
      select(plant,gov_serv_ind,perc) |> 
      pivot_wider(
        names_from = gov_serv_ind,
        values_from = perc
      ) |> 
      mutate(plant = factor(plant, levels = c("yes", "no")))
    
    plot_ly(
      data,
      x = ~plant
    ) |> 
      add_bars(
        y = ~ `no`,
        width = 0.4,
        name = 'Did not avail government service',
        text = paste(data$no, "%"),
        marker = list(color = '#FFD167',
                      line = list(color = "black",
                                  width = 1.5)),
        hoverinfo = 'y+name',
        hovertemplate = paste('%{y}%')
      ) |>
      add_bars(
        y = ~ `yes`,
        width = 0.4,
        name = 'Availed government service',
        text = paste(data$yes, "%"),
        marker = list(color = '#06D7A0',
                      line = list(color = "black",
                                  width = 1.5)),
        hoverinfo = 'y+name',
        hovertemplate = paste('%{y}%')
      ) |>
      
      layout(
        title = "Government Service Availment by Willingness to Plant",
        margin = list(t = 100, b = 100),
        showlegend = TRUE,
        barmode = 'stack',
        xaxis = list(title = "Willingnes to Plant"),
        yaxis = list(title = "Percentage (%)"),
        legend = list(x = 1)
      )
  })
  
  # render water availability-plant chart ------------------------------------------------------------------
  output$plant_wateravail_colchart <- renderPlotly({
    req(model_data_combineddata)
    
    combined_data <- model_data_combineddata
    
    data <-  table(combined_data$pricepaddylowstillplant, combined_data$wateravailability) |> 
      as.data.frame() |> 
      rename(
        plant = Var1,
        wateravailability = Var2
      ) |> 
      group_by(plant) |> 
      mutate(
        perc = round(Freq/sum(Freq)*100, 2)
      ) |> 
      select(plant,wateravailability,perc) |> 
      pivot_wider(
        names_from = wateravailability,
        values_from = perc
      ) |> 
      mutate(plant = factor(plant, levels = c("yes", "no")))
    
    plot_ly(
      data,
      x = ~plant
    ) |> 
      add_bars(
        y = ~ `Insufficient`,
        width = 0.4,
        name = 'Insufficient',
        text = paste(data$Insufficient, "%"),
        marker = list(color = '#FFD167',
                      line = list(color = "black",
                                  width = 1.5)),
        hoverinfo = 'y+name',
        hovertemplate = paste('%{y}%')
      ) |>
      add_bars(
        y = ~ `Excessive`,
        width = 0.4,
        name = 'Excessive',
        text = paste(data$Excessive, "%"),
        marker = list(color = '#06D7A0',
                      line = list(color = "black",
                                  width = 1.5)),
        hoverinfo = 'y+name',
        hovertemplate = paste('%{y}%')
      ) |>
      add_bars(
        y = ~ `Sufficient`,
        width = 0.4,
        name = 'Sufficient',
        text = paste(data$Sufficient, "%"),
        marker = list(color = '#108AB1',
                      line = list(color = "black",
                                  width = 1.5)),
        hoverinfo = 'y+name',
        hovertemplate = paste('%{y}%')
      ) |>
      
      layout(
        title = "Water Availability by Willingness to Plant",
        margin = list(t = 100, b = 100),
        showlegend = TRUE,
        barmode = 'stack',
        xaxis = list(title = "Willingnes to Plant"),
        yaxis = list(title = "Percentage (%)"),
        legend = list(x = 1)
      )
  })
  
  # render prod loss-plant chart ------------------------------------------------------------------
  output$plant_prodloss_colchart <- renderPlotly({
    req(model_data_combineddata)
    
    combined_data <- model_data_combineddata
    
    data <-  table(combined_data$pricepaddylowstillplant, combined_data$productionloss) |> 
      as.data.frame() |> 
      rename(
        plant = Var1,
        productionloss = Var2
      ) |> 
      group_by(plant) |> 
      mutate(
        perc = round(Freq/sum(Freq)*100, 2)
      ) |> 
      select(plant,productionloss,perc) |> 
      pivot_wider(
        names_from = productionloss,
        values_from = perc
      ) |> 
      mutate(plant = factor(plant, levels = c("yes", "no")))
    
    plot_ly(
      data,
      x = ~plant
    ) |> 
      add_bars(
        y = ~ `no`,
        width = 0.4,
        name = 'Without production loss',
        text = paste(data$no, "%"),
        marker = list(color = '#FFD167',
                      line = list(color = "black",
                                  width = 1.5)),
        hoverinfo = 'y+name',
        hovertemplate = paste('%{y}%')
      ) |>
      add_bars(
        y = ~ `yes`,
        width = 0.4,
        name = 'With production loss',
        text = paste(data$yes, "%"),
        marker = list(color = '#06D7A0',
                      line = list(color = "black",
                                  width = 1.5)),
        hoverinfo = 'y+name',
        hovertemplate = paste('%{y}%')
      ) |>
      
      layout(
        title = "Production Loss by Willingness to Plant",
        margin = list(t = 100, b = 100),
        showlegend = TRUE,
        barmode = 'stack',
        xaxis = list(title = "Willingnes to Plant"),
        yaxis = list(title = "Percentage (%)"),
        legend = list(x = 1)
      )
  })
  
  # render capital source-plant chart ------------------------------------------------------------------
  output$plant_capitalsource_colchart <- renderPlotly({
    req(model_data_combineddata)
    
    combined_data <- model_data_combineddata
    
    data <-  table(combined_data$pricepaddylowstillplant, combined_data$capitalsource) |> 
      as.data.frame() |> 
      rename(
        plant = Var1,
        capitalsource = Var2
      ) |> 
      group_by(plant) |> 
      mutate(
        perc = round(Freq/sum(Freq)*100, 2)
      ) |> 
      select(plant,capitalsource,perc) |> 
      pivot_wider(
        names_from = capitalsource,
        values_from = perc
      ) |> 
      mutate(plant = factor(plant, levels = c("yes", "no")))
    
    plot_ly(
      data,
      x = ~plant
    ) |> 
      add_bars(
        y = ~ `both own/borrowed`,
        width = 0.4,
        name = 'Both own/borrowed',
        text = paste(data$`both own/borrowed`, "%"),
        marker = list(color = '#FFD167',
                      line = list(color = "black",
                                  width = 1.5)),
        hoverinfo = 'y+name',
        hovertemplate = paste('%{y}%')
      ) |>
      add_bars(
        y = ~ `borrowed`,
        width = 0.4,
        name = 'Borrowed',
        text = paste(data$borrowed, "%"),
        marker = list(color = '#06D7A0',
                      line = list(color = "black",
                                  width = 1.5)),
        hoverinfo = 'y+name',
        hovertemplate = paste('%{y}%')
      ) |>
      add_bars(
        y = ~ `own`,
        width = 0.4,
        name = 'Own',
        text = paste(data$own, "%"),
        marker = list(color = '#108AB1',
                      line = list(color = "black",
                                  width = 1.5)),
        hoverinfo = 'y+name',
        hovertemplate = paste('%{y}%')
      ) |>
      
      layout(
        title = "Capital Source by Willingness to Plant",
        margin = list(t = 100, b = 100),
        showlegend = TRUE,
        barmode = 'stack',
        xaxis = list(title = "Willingnes to Plant"),
        yaxis = list(title = "Percentage (%)"),
        legend = list(x = 1)
      )
  })
  
}
  
shinyApp(ui, server)
