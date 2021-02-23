#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

function(input, output){
    set.seed(13)
    
    output$attr_rate <- renderValueBox({
        # hitung nilai attrition rate
        attrition_rate <- mean(employee$Attrition)
        valueBox(subtitle = "Attrition Rate", 
                 value = paste0(round(attrition_rate*100, 2), "%"),
                 # value = 6,
                 icon = icon("user-minus"),
                 color = "purple")
    })
    
    output$tot_employee <- renderValueBox({
        # hitung nilai attrition rate
        num_employee <- dim(employee)[1]
        valueBox(subtitle = "Total Employee", 
                 value = num_employee,
                 icon = icon("users"),
                 color = "purple")
    })
    
    output$department_ratio <- renderPlotly({
        num_attrition <- sum(employee$Attrition)
        
        department_ratio <- employee %>%
            group_by(Department) %>% 
            summarise(Attrition_Ratio = sum(Attrition) / num_attrition)
        
        plot_department_ratio <- plot_ly(department_ratio, 
                                         labels = ~Department,
                                         values = ~Attrition_Ratio,
                                         sort = FALSE,
                                         marker = list(
                                                 colors = c("#00b0f6", 
                                                            "#00bf7d", 
                                                            "#f8766d"),
                                                 line=list(
                                                     width=1)),
                                         hoverinfo = "text",
                                         hovertext = paste(
                                             department_ratio$Department, 
                                             "<br>Proportion: ",
                                             round(department_ratio$Attrition_Ratio*100, 2),
                                             "%")) %>% 
            add_pie(hole = 0.4) %>% 
            layout(title = "Attrition Proportion by Department",
                   legend = list(
                       x = 0.05,
                       y = -0.15,
                       font = list(
                           family = "sans-serif",
                           size = 10
                           # color = "#000"
                       ),
                       # bgcolor = "#E2E2E2",
                       # bordercolor = "#FFFFFF",
                       borderwidth = 2,
                       orientation = 'h'))
    })
    
    output$attr_composition <- renderPlotly({
        data_department <- employee %>% 
            group_by(Role) %>% 
            summarise(Attrition = sum(Attrition)) %>% 
            mutate(Total_Employees = table(employee$Role),
                   Total_Employees = as.numeric(Total_Employees),
                   "Non Attrition" = Total_Employees - Attrition) %>% 
            pivot_longer(cols = c(Attrition, "Non Attrition"),
                         names_to = "Attrition",
                         values_to = "Count") %>% 
            mutate(Proportion = Count / Total_Employees * 100)
        
        # rata-rata attrition rate tiap role
        non.attr_prop <- data_department %>% 
            filter(Attrition == "Non Attrition") %>% 
            select(Proportion)
        attr_prop_mean <- mean(non.attr_prop$Proportion)/100
        
        plot_attr_composition <- data_department %>% 
            ggplot(aes(x = Proportion, 
                       y = Role,
                       text = glue("{Role}
                         Proportion of {Attrition}: {round(Proportion, 2)}%"))) +
            geom_col(aes(fill = Attrition), position = "fill") +
            geom_vline(xintercept = attr_prop_mean, col = "black", lty = 2, lwd = 1.5) + 
            labs(title = "Composition of Attrition vs Non-Attrition Employees",
                 x = NULL,
                 y = NULL) +
            # scale_fill_manual(values = c("black", "firebrick")) +
            scale_y_discrete(labels = wrap_format(30)) +
            scale_x_continuous(labels = percent_format(accuracy = 1)) +
            theme(axis.text.y = element_text(size=rel(.8)),
                  plot.title = element_text(hjust = 0.5),
                  plot.margin=unit(c(0,0,0,-50), "mm"))
        
        ggplotly(plot_attr_composition, tooltip = "text")
    })
    
    output$department_agg <- renderPlotly({
        agg_col <- input$agg_col
        
        department_agg <- employee %>% 
            group_by(Department, employee[, agg_col]) %>% 
            summarise(Attrition = sum(Attrition))
        colnames(department_agg) <- c("Department", "Agg", "Attrition")
        
        plot_department_agg <- department_agg %>% 
            ggplot(aes(x = Attrition,
                       y = reorder(Department, Attrition),
                       fill = Agg,
                       text = glue("{agg_col}: {Agg}
                         Number of Attrition: {Attrition}"))) +
            geom_col() +
            labs(title = "Attrition Distribution Over Department",
                 x = NULL,
                 y = NULL,
                 fill = agg_col)
        
        ggplotly(plot_department_agg, tooltip = "text")
    })
    
    output$datasets <- renderDataTable({
        datasets <- datatable(employee, 
                              options = list(
                                  scrollX = T, 
                                  scrollY = F, 
                                  pageLength = 10))
    })
    
    output$top_feature <- renderPlotly({
        importances <- model.rf$importance
        total <- sum(importances)
        importance_features <- data.frame(feature = row.names(importances),
                                          importance = importances[, 1]/total, 
                                          row.names = NULL)
        n_top <- as.integer(input$n_features)
        print(n_top)
        top_n_features <- tail(importance_features[order(importance_features$importance),], n = n_top)
        
        plot_ranking <- top_n_features %>% 
            ggplot(aes(x = importance, 
                       y = reorder(feature, importance),
                       text = glue("{feature}
                         Affect: {round(importance*100, 2)}%"))) +
            geom_col(aes(fill = importance)) +
            
            labs( x = NULL,
                  y = NULL,
                  title = glue("Top {n_top} Most Important Features to Predict Employee Attrition")) +
            scale_y_discrete(labels = wrap_format(30)) + 
            scale_x_continuous(labels = percent_format(accuracy = 1)) +
            scale_fill_gradient(low = "#912121", high = "#FF194E") +
            theme(legend.position = "none")
        
        ggplotly(plot_ranking, tooltip = "text")
        
    })
    
    output$segmentation <- renderUI({
        input$updateSegment
        
        segment_panel <- function(x){
            condition <- strsplit(top10[x, "condition"], "&")
            condition <- sapply(condition[[1]], format_condition)
            ul <- lapply(condition, tags$li)
            
            tabPanel(title = paste("Segment-", x), 
                     br(),
                     tags$span("Here are the rules for this Segment:", style = "color: white; font-size:xx-large;"),
                     br(),
                     br(),
                     tags$ul(
                         ul
                     ),
                     br(),
                     tags$span(HTML(paste("There are <b>", top10[x, "freq"]*100,"%</b> of Total Employee that included into this segment.<br>")), style = "color: white; font-size:medium;"),
                     tags$span(HTML(paste("This Segment got <b>", top10[x, "attr_rate"]*100,"% Attrition Rate</b> ")), style = "color: white; font-size:medium;"),
                     br(),
                     br()
            )         
        }
        
        
        isolate(x <- X[, input$features])
        
        model.rfs <- randomForest(x, y, ntree = 50)
        treeList <- RF2List(model.rfs)
        
        ruleExec <- extractRules(treeList,x, maxdepth = 5)
        ruleMetric <- getRuleMetric(ruleExec,x,y) # measure rules
        ruleMetric <- pruneRule(ruleMetric, x, y)
        ruleMetric <- presentRules(ruleMetric, colnames(x))
        ruleMetric <- unique(ruleMetric)
        
        rule.df <- data.frame(condition = ruleMetric[,"condition"],
                              freq = as.numeric(ruleMetric[, "freq"]),
                              err = as.numeric(ruleMetric[, "err"]),
                              pred = as.numeric(ruleMetric[, "pred"]))
        rule.df <- rule.df %>% 
            mutate(attr_rate = abs(pred - err)) %>% 
            arrange(desc(attr_rate), desc(freq))
        
        top10 <- head(rule.df, n = 10)
        
        Tabs <- lapply(1:10, segment_panel)
        
        do.call(what = tabsetPanel, Tabs)
    })
}

