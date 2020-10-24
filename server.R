shinyServer(function(input, output, session) {
  OutlierData <- Data[, 3:11, 13:14]

  # 1.  Overview

  output$Summary <- renderPrint({
    dfSummary(Data)
  })

  output$Raw_data <- DT::renderDataTable({
    DT::datatable(data = as.data.frame(Data))
  })

  # 2. Exploratory Data Analysis

  # 2.1 Mosaic Plot

  output$Mosaic <- renderPlot({
    formula <- as.formula(paste("~", paste(input$VariablesA, collapse = " + ")))
    vcd::mosaic(formula,
      data = Data,
      main = "Mosaic Plot Comparing Type of Government and Healthcare System ",
      shade = TRUE, legend = TRUE)
  })
  
  output$RisingValue <- renderPlot({
    d <- Data[,c("POPULATION", input$variable)]
    d <- as.data.frame(d)
    for (col in 1:ncol(d)) {
      d[,col] <- d[order(d[,col]),col]
    }
    
    # Scaling for comparative plotting
    d <- scale(x = d, center = input$standardise_risingplot, scale = input$standardise_risingplot)
    mypalette <- rainbow(ncol(d))
    if (ncol(d) == 1) {
      matplot(x = seq(1, 100, length.out = nrow(d)), y = d, type = "l", xlab = "Relative Percentage", ylab = "Values",
              lty = 1, lwd = 1, col = mypalette, main = "Rising Value Chart for Population")
    }
    
    else {
      matplot(x = seq(1, 100, length.out = nrow(d)), y = d, type = "l", xlab = "Percentile", ylab = "Values",
              lty = 1, lwd = 1, col = mypalette)
      legend(legend = colnames(d), x = "topleft", y = "top", lty = 1, lwd = 1, col = mypalette, ncol = round(ncol(d)^0.3))
    }
  })

  # 2.3  Missingness  

  output$Values <- renderPlot({
    vis_dat(Data, sort_type = TRUE)
  })

  output$Pattern <- renderPlot({
    naniar::gg_miss_upset(Data)
  })

  output$Missingness <- renderPlot({
    Data$MISSINGNESS <- apply(X = is.na(Data), MARGIN = 1, FUN = sum)
    tree <- train(MISSINGNESS ~ . - COUNTRY, data = Data, method = "rpart", na.action = na.rpart)
    rpart.plot(tree$finalModel,
      box.palette = "RdBu", shadow.col = "gray",
      main = "Predicting the Number of Missing Variables in an Observation",
      nn = TRUE, roundint = TRUE, clip.facs = TRUE)
  })

  # 2.4 Correlation 
  
  output$Corrgram <- renderPlot({
    m <- is.na(Data) + 0 # Transform Logical to Binary
    cm <- colMeans(m)
    m <- m[, cm > 0 & cm < 1, drop = FALSE] # Remove none-missing or all-missing variables
    
    corrgram::corrgram(cor(m), order = "OLO", panel = panel.shade, abs = TRUE,
                       col.regions = colorRampPalette(c( "red3","orange","gold1", "yellow", "lightskyblue",
                                                         "steelblue3", "royalblue3","darkblue","darkblue")))
    title(main = "Missing Value Correlation")
  })
  
  output$Corrgram <- renderPlot({
    m <- is.na(Data) + 0 # Transform Logical to Binary
    cm <- colMeans(m)
    m <- m[, cm > 0 & cm < 1, drop = FALSE] # Remove none-missing or all-missing variables
    
    corrgram::corrgram(cor(m), order = "OLO", panel = panel.shade, abs = TRUE,
                       col.regions = colorRampPalette(c( "red3","orange","gold1", "yellow", "lightskyblue",
                                                         "steelblue3", "royalblue3","darkblue","darkblue")))
    title(main = "Missing Value Correlation")
  })
  
  # 2.5  Outliers

  output$Boxplot <- renderPlot({
    data <- as.matrix(OutlierData)
    data <- scale(data, center = input$standardise_boxplot, scale = input$standardise_boxplot)

    car::Boxplot(data,
      use.cols = TRUE, notch = FALSE, varwidth = FALSE,
      horizontal = FALSE, outline = input$outliers,
      col = brewer.pal(n = dim(OutlierData)[2], name = "Set3"),
      range = input$range, main = "Boxplots Showing Outliers - Indicated by Country Number"
    )
  })
  
  # 3 Modeling
  
  # 3.1 Predictors
  
  output$nzv_data <- DT::renderDataTable({
    data <- CovData
    nzv <- nearZeroVar(data, saveMetrics= TRUE)
    DT::datatable(data = as.data.frame(nzv), caption = "Table Displaying Fields with Non Zero Variance")
  })

  # 3.2 GLMNET Model
  
  glm_model <- reactive({
    showNotification(id = "glmnet", paste("Optimising", "glmnet", "Hyper Parameters using Cross Validation"),
                     session = session, duration = NULL)
    model <- caret::train(rec, data = train, method = "glmnet", metric = "RMSE",
                          trControl = trainControl(method = "cv", number = 10, verboseIter = TRUE) # Cross Validation
    )
    removeNotification(id = "glmnet")
    model
  })
  
  output$BestTune <- renderTable({
    model <- glm_model()
    as.data.frame(model$bestTune)
  })  
  
  output$GlmModelPlot <- renderPlot({
    model <- glm_model()
    plot(model$finalModel)
  })

  output$GlmModelSummary <- renderPrint({
    print(glm_model())
  })
  
  
  glm_predict <- reactive({
    model <- glm_model()
    predictions <- predict(model, newdata=test)
    d <- data.frame(test$DEATHRATE, predictions)
    colnames(d) <- c("Observed", "Predicted")
    d
  })
  
  output$GlmTestSummary <- renderPrint({
    print(glm_predict())
  })
  
  output$GlmTestPlot <- renderPlot({
    plot(glm_predict(), main="Predicted Vs Observed",col = "red", pch = 19 , cex=1.2)
    abline(a = 0, b=1, col = "blue")
  })
  
  output$CookDistance <- renderPlot({
    rec <- recipes::recipe(DEATHRATE ~., data = train) %>%
      step_naomit(everything() ) %>% 
      step_dummy(all_nominal()) %>% #encode nominals if any present 
      step_nzv(all_predictors()) %>%  # remove near zero variance predictor variables
      step_lincomb(all_predictors()) %>%   # remove predictors that are linear combinations of other predictors
      step_YeoJohnson(all_predictors()) %>% # power transform all remaining predictors to look more normal
      prep(data = train) # train these steps
    
    processed <- bake(rec, train)

    lmod <- glm(formula = DEATHRATE ~ ., data = processed, family = gaussian) #since HHV is numeric
    dc <- cooks.distance(lmod)
    thresh <- 4 * mean(dc)  # this is an empirical way to assign a threshold
    data <- data.frame(dc, id = (1:length(dc))/length(dc) )
    data$row <- ifelse(data$dc > thresh, rownames(processed), NA)
    cdoutlier <- data$dc > thresh
    names(cdoutlier) <- rownames(processed)

    ggplot(data = data, mapping = aes(y = dc, x = id, label = row)) +
      geom_point() +
      geom_text(nudge_x = 0.02) +
      scale_y_continuous(limits = c(0, max(data$dc)*1.1)) +
      labs(y = "Cook's distance", x = "Complete Observations", title = "Outlier pattern") +
      geom_abline(slope = 0, intercept = thresh, color = "red") +
      scale_x_continuous(breaks = c(0, 0.5, 1), labels = c("0%", "50%", "100%")) +
      theme(legend.position = "bottom")
  })
  
})
