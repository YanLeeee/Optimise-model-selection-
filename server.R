shinyServer(function(input, output, session) {
  
  models <- reactiveValues()
  
  getData <- reactive({
    read.csv(file = "Ass3Data.csv", row.names = "ID")
  })
  
  ############################################################################## 
  getTrControl <- reactive({
    # shared cross validation specification
    y <- getTrainData()[,"Y"]
    n <- 25
    trainControl(method = "boot", number = n, repeats = NA, allowParallel = TRUE, index = caret::createResample(y = y, times = n))
  })
  
  ############################################################################## 
  output$BoxPlots <- renderPlot({
    d <- getData()
    numeric <- sapply(d, FUN = is.numeric)
    req(d, input$Multiplier, length(numeric) > 0)
    d <- scale(d[,numeric], center = input$Normalise, scale = input$Normalise)
    boxplot(d, outline = TRUE, main = paste("Boxplot using IQR multiplier of", input$Multiplier), range = input$Multiplier)
  })
  
  ############################################################################## 
  output$Missing <- renderPlot({
    d <- getData()
    vis_dat(d)
  })
  
  ############################################################################## 
  output$Corr <- renderPlot({
    d <- getData()
    numeric <- sapply(d, FUN = is.numeric)
    req(d, length(numeric) > 0)
    corrgram::corrgram(d, order = "OLO", main = "Numeric Data Correlation")
  })
  
  ############################################################################## 
  output$DataSummary <- renderPrint({
    str(getData())
  })
  
  ############################################################################## 
  output$Table <- DT::renderDataTable({
    d <- getData()
    numeric <- c(FALSE, sapply(d, is.numeric)) # never round rownames which are the first column (when shown)
    DT::datatable(d) %>%
      formatRound(columns = numeric, digits = 3)
  })
  
  ############################################################################## 
  getSplit <- reactive({
    createDataPartition(y = getData()$Y, p = input$Split, list = FALSE)
  })
  
  ############################################################################## 
  getTrainData <- reactive({
    getData()[getSplit(),]
  })
  
  ############################################################################## 
  getTestData <- reactive({
    getData()[-getSplit(),]
  })
  
  ############################################################################## 
  output$SplitSummary <- renderPrint({
    cat(paste("Training observations:", nrow(getTrainData()), "\n", "Testing observations:", nrow(getTestData())))
  })
  
  ##############################################################################  
  getNullRecipe <- reactive({
    recipe <- steps(recipes::recipe(Y ~ ., data = getTrainData()), input$NullPreprocess)
  })
  
  ##############################################################################  
  getNullModel <- reactive({
    req(input$NullGo)
    isolate({
      clus <- startMode(input$Parallel)
      tryCatch({
        models$Null <- NULL
        method <- "null"
        showNotification(id = method, paste("Assessing", method, "model using resampling"), session = session, duration = NULL)
        models$Null <- caret::train(getNullRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 0)
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
      models$Null
    })
  })
  
  ############################################################################## 
  output$NullModelSummary2 <- renderPrint({
    print(getNullModel())
  })
  
  
  
  ##############################################################################  
  getGlmnetRecipe <- reactive({
    recipe <- steps(recipes::recipe(Y ~ ., data = getTrainData()), input$GlmnetPreprocess)
  })
  
  ##############################################################################  
  getGlmnetModel <- reactive({
    library(glmnet)
    req(input$GlmnetGo)
    isolate({
      clus <- startMode(input$Parallel)
      tryCatch({
        models$Glmnet <- NULL
        method <- "glmnet"
        showNotification(id = method, paste("Assessing", method, "model using resampling"), session = session, duration = NULL)
        models$Glmnet <- caret::train(getGlmnetRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 6)
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
      models$Glmnet
    })
  })
  
  ############################################################################## 
  output$GlmnetModelSummary0 <- renderText({
    description("glmnet")
  })

  ############################################################################## 
  output$GlmnetModelSummary1 <- renderTable({
    mod <- getGlmnetModel()
    req(mod)
    as.data.frame(mod$bestTune)
  })  
  
  ############################################################################## 
  output$GlmnetModelPlots <- renderPlot({
    plot(getGlmnetModel())
  })     
  
  ############################################################################## 
  output$GlmnetModelSummary2 <- renderPrint({
    mod <- getGlmnetModel()
    print(mod)
  })

  
  
  ##############################################################################  
  getPlsRecipe <- reactive({
    recipe <- steps(recipes::recipe(Y ~ ., data = getTrainData()), input$PlsPreprocess)
  })
  
  ##############################################################################
  getPlsModel <- reactive({
    library(pls)
    req(input$PlsGo)
    isolate({
      clus <- startMode(input$Parallel)
      tryCatch({
        models$Pls <- NULL
        method <- "pls"
        showNotification(id = method, paste("Assessing", method, "model using resampling"), session = session, duration = NULL)
        models$Pls <- caret::train(getPlsRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 6)
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
      models$Pls
    })
  })
  
  ############################################################################## 
  output$PlsModelSummary0 <- renderText({
    description("pls")
  })

  ############################################################################## 
  output$PlsModelSummary1 <- renderTable({
    mod <- getPlsModel()
    req(mod)
    as.data.frame(mod$bestTune)
  })  
  
  ############################################################################## 
  output$PlsModelPlots <- renderPlot({
    plot(getPlsModel())
  })     
  
  ############################################################################## 
  output$PlsModelSummary2 <- renderPrint({
    mod <- getPlsModel()
    summary(mod$finalModel)
  })
  
  
  
  ##############################################################################  
  getRpartRecipe <- reactive({
    recipe <- steps(recipes::recipe(Y ~ ., data = getTrainData()), input$RpartPreprocess)
  })
  
  ##############################################################################
  getRpartModel <- reactive({
    library(rpart)
    req(input$RpartGo)
    isolate({
      clus <- startMode(input$Parallel)
      tryCatch({
        models$Rpart <- NULL
        method <- "rpart"
        showNotification(id = method, paste("Assessing", method, "model using resampling"), session = session, duration = NULL)
        models$Rpart <- caret::train(getRpartRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 6)
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
      models$Rpart
    })
  })
  
  ############################################################################## 
  output$RpartModelSummary0 <- renderText({
    description("rpart")
  })
  
  ############################################################################## 
  output$RpartModelSummary1 <- renderTable({
    mod <- getRpartModel()
    req(mod)
    as.data.frame(mod$bestTune)
  })  
  
  ############################################################################## 
  output$RpartModelPlots <- renderPlot({
    plot(getRpartModel())
  })
  
  ############################################################################## 
  output$RpartModelTree <- renderPlot({
    library(rpart.plot)
    rpart.plot::rpart.plot(getRpartModel()$finalModel)
  })     
  
  ############################################################################## 
  output$RpartModelSummary2 <- renderPrint({
    mod <- getRpartModel()
    print(mod$finalModel)
  })
  
  ##############################################################################  
  getlassoRecipe <- reactive({
    recipe <- steps(recipes::recipe(Y ~ ., data = getTrainData()), input$lassoPreprocess)
  })
  
  ##############################################################################
  getlassoModel <- reactive({
    library(elasticnet)
    req(input$lassoGo)
    isolate({
      clus <- startMode(input$Parallel)
      tryCatch({
        models$lasso <- NULL
        method <- "lasso"
        showNotification(id = method, paste("Assessing", method, "model using resampling"), session = session, duration = NULL)
        models$lasso <- caret::train(getlassoRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 6)
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
      models$lasso
    })
  })
  
  ############################################################################## 
  output$lassoModelSummary0 <- renderText({
    description("lasso")
  })
  
  ############################################################################## 
  output$lassoModelSummary1 <- renderTable({
    mod <- getlassoModel()
    req(mod)
    as.data.frame(mod$bestTune)
  })  
  
  ############################################################################## 
  output$lassoModelPlots <- renderPlot({
    plot(getlassoModel())
  })     
  
  ############################################################################## 
  output$lassoModelSummary2 <- renderPrint({
    mod <- getlassoModel()
    print(mod)
  })
  

  ##############################################################################  
  getridgeRecipe <- reactive({
    recipe <- steps(recipes::recipe(Y ~ ., data = getTrainData()), input$ridgePreprocess)
  })
  
  ##############################################################################
  getridgeModel <- reactive({
    library(elasticnet)
    req(input$ridgeGo)
    isolate({
      clus <- startMode(input$Parallel)
      tryCatch({
        models$ridge <- NULL
        method <- "ridge"
        showNotification(id = method, paste("Assessing", method, "model using resampling"), session = session, duration = NULL)
        models$ridge <- caret::train(getridgeRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 6)
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
      models$ridge
    })
  })
  
  ############################################################################## 
  output$ridgeModelSummary0 <- renderText({
    description("ridge")
  })
  
  ############################################################################## 
  output$ridgeModelSummary1 <- renderTable({
    mod <- getridgeModel()
    req(mod)
    as.data.frame(mod$bestTune)
  })  
  
  ############################################################################## 
  output$ridgeModelPlots <- renderPlot({
    plot(getridgeModel())
  })     
  
  ############################################################################## 
  output$ridgeModelSummary2 <- renderPrint({
    mod <- getridgeModel()
    print(mod)
  })
  
  ##############################################################################  
  getsvmLinear3Recipe <- reactive({
    recipe <- steps(recipes::recipe(Y ~ ., data = getTrainData()), input$svmLinear3Preprocess)
  })
  
  ##############################################################################
  getsvmLinear3Model <- reactive({
    library(LiblineaR)
    req(input$svmLinear3Go)
    isolate({
      clus <- startMode(input$Parallel)
      tryCatch({
        models$svmLinear3 <- NULL
        method <- "svmLinear3"
        showNotification(id = method, paste("Assessing", method, "model using resampling"), session = session, duration = NULL)
        models$svmLinear3 <- caret::train(getsvmLinear3Recipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 6)
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
      models$svmLinear3
    })
  })
  
  ############################################################################## 
  output$svmLinear3ModelSummary0 <- renderText({
    description("svmLinear3")
  })
  
  ############################################################################## 
  output$svmLinear3ModelSummary1 <- renderTable({
    mod <- getsvmLinear3Model()
    req(mod)
    as.data.frame(mod$bestTune)
  })  
  
  ############################################################################## 
  output$svmLinear3ModelPlots <- renderPlot({
    plot(getsvmLinear3Model())
  })     
  
  ############################################################################## 
  output$svmLinear3ModelSummary2 <- renderPrint({
    mod <- getsvmLinear3Model()
    print(mod)
  })
  
  ##############################################################################  
  getqrfRecipe <- reactive({
    recipe <- steps(recipes::recipe(Y ~ ., data = getTrainData()), input$qrfPreprocess)
  })
  
  ##############################################################################
  getqrfModel <- reactive({
    library(quantregForest)
    req(input$qrfGo)
    isolate({
      clus <- startMode(input$Parallel)
      tryCatch({
        models$qrf <- NULL
        method <- "qrf"
        showNotification(id = method, paste("Assessing", method, "model using resampling"), session = session, duration = NULL)
        models$qrf <- caret::train(getqrfRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 6)
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
      models$qrf
    })
  })
  
  ############################################################################## 
  output$qrfModelSummary0 <- renderText({
    description("qrf")
  })
  
  ############################################################################## 
  output$qrfModelSummary1 <- renderTable({
    mod <- getqrfModel()
    req(mod)
    as.data.frame(mod$bestTune)
  })  
  
  ############################################################################## 
  output$qrfModelPlots <- renderPlot({
    plot(getqrfModel())
  })     
  
  ############################################################################## 
  output$qrfModelSummary2 <- renderPrint({
    mod <- getqrfModel()
    print(mod)
  })
  
  ##############################################################################  
  getglmboostRecipe <- reactive({
    recipe <- steps(recipes::recipe(Y ~ ., data = getTrainData()), input$glmboostPreprocess)
  })
  
  ##############################################################################
  getglmboostModel <- reactive({
    library(plyr)
    library(mboost)
    req(input$glmboostGo)
    isolate({
      clus <- startMode(input$Parallel)
      tryCatch({
        models$glmboost <- NULL
        method <- "glmboost"
        showNotification(id = method, paste("Assessing", method, "model using resampling"), session = session, duration = NULL)
        models$glmboost <- caret::train(getglmboostRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 6)
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
      models$glmboost
    })
  })
  
  ############################################################################## 
  output$glmboostModelSummary0 <- renderText({
    description("glmboost")
  })
  
  ############################################################################## 
  output$glmboostModelSummary1 <- renderTable({
    mod <- getglmboostModel()
    req(mod)
    as.data.frame(mod$bestTune)
  })  
  
  ############################################################################## 
  output$glmboostModelPlots <- renderPlot({
    plot(getglmboostModel())
  })     
  
  ############################################################################## 
  output$glmboostModelSummary2 <- renderPrint({
    mod <- getglmboostModel()
    print(mod)
  })
  
  ##############################################################################  
  getcubistRecipe <- reactive({
    recipe <- steps(recipes::recipe(Y ~ ., data = getTrainData()), input$cubistPreprocess)
  })
  
  ##############################################################################
  getcubistModel <- reactive({
    library(Cubist)
    req(input$cubistGo)
    isolate({
      clus <- startMode(input$Parallel)
      tryCatch({
        models$cubist <- NULL
        method <- "cubist"
        showNotification(id = method, paste("Assessing", method, "model using resampling"), session = session, duration = NULL)
        models$cubist <- caret::train(getcubistRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 6)
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
      models$cubist
    })
  })
  
  ############################################################################## 
  output$cubistModelSummary0 <- renderText({
    description("cubist")
  })
  
  ############################################################################## 
  output$cubistModelSummary1 <- renderTable({
    mod <- getcubistModel()
    req(mod)
    as.data.frame(mod$bestTune)
  })  
  
  ############################################################################## 
  output$cubistModelPlots <- renderPlot({
    plot(getcubistModel())
  })     
  
  ############################################################################## 
  output$cubistModelSummary2 <- renderPrint({
    mod <- getcubistModel()
    print(mod)
  })
  
  ##############################################################################  
  getBstLmRecipe <- reactive({
    recipe <- steps(recipes::recipe(Y ~ ., data = getTrainData()), input$BstLmPreprocess)
  })
  
  
  ############################################################################## 
  output$SelectionSummary <- renderPrint({
    results <- resamples(models)
    summary(results)
  })
  
  ############################################################################## 
  getResamples <- reactive({
    results <- caret::resamples(reactiveValuesToList(models))
    NullModel <- "Null"
    
    #scale metrics using null model. Tough code to follow -sorry
    if (input$NullNormalise & NullModel %in% results$models) {
      actualNames <- colnames(results$values)
      # Normalise the various hyper-metrics except R2 (as this is already normalised)
      for (metric in c("RMSE", "MAE")) {
        col <- paste(sep = "~", NullModel, metric)
        if (col %in% actualNames) {
          nullMetric <- mean(results$values[, col], na.rm = TRUE)
          if (!is.na(nullMetric) & nullMetric != 0) {
            for (model in results$models) {
              mcol <- paste(sep = "~", model, metric)
              if (mcol %in% actualNames) {
                results$values[, mcol] <- results$values[, mcol] / nullMetric
              }
            }
          }
        }
      }
    }
    updateRadioButtons(session = session, inputId = "Choice", choices = results$models)
    results
  })
  
  
  ############################################################################## 
  output$SelectionBoxPlot <- renderPlot({
    mod <- getResamples()
    bwplot(mod, notch = input$Notch)
  })
  
  
  ############################################################################## 
  output$Title <- renderUI({
    tags$h3(paste("Unseen data results for chosen model:", input$Choice))
  })
  
  ############################################################################## 
  getTestResults <- reactive({
    test <- getTestData()
    mod <- models[[input$Choice]]
    predictions <- predict(mod, newdata = test)
    d <- data.frame(test$Y, predictions)
    colnames(d) <- c("obs", "pred")
    d
  })
  
  ############################################################################## 
  output$TestSummary <- renderPrint({
    caret::defaultSummary(getTestResults())
  })
  
  ############################################################################## 
  output$TestPlot <- renderPlot({
    d <- getTestResults()
    req(nrow(d) > 0)
    par(pty = "s")
    range <- range(c(d$obs, d$pred), na.rm = TRUE)
    plot(d, xlim = range, ylim = range, main = "Predicted versus Observed for test data")
    abline(a = 0, b = 1, col = c("blue"), lty = c(2), lwd = c(3))
  }, height = 600)
  
})
