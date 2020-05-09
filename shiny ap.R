#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(GEOquery)
library(R.utils)
library(reshape2)
library(ggplot2)
library(limma)
library(dplyr)

library(shinycssloaders)

plot_f <- function(q1, cvk, nsim){
    largevar = apply(gse, 1, var)
    ind = which(largevar > quantile(largevar, q1))
    
    X = as.matrix(t(gse[ind,]))
    y = rejection_status
    
    # cvK = 5  # number of CV folds
    cv_50acc5_knn = cv_50acc5_svm = cv_50acc5_rf = c()
    cv_acc_knn = cv_acc_svm = cv_acc_rf = c()
    
    n_sim = nsim ## number of repeats
    for (i in 1:n_sim) {
        
        cvSets = cvTools::cvFolds(nrow(X), cvK)  # permute all the data, into 5 folds
        cv_acc_rf = c()
        
        for (j in 1:cvK) {
            test_id = cvSets$subsets[cvSets$which == j]
            X_test = X[test_id, ]
            X_train = X[-test_id, ]
            y_test = y[test_id]
            y_train = y[-test_id]
            
            ## RandomForest
            rf_res <- randomForest::randomForest(x = X_train, y = as.factor(y_train))
            fit <- predict(rf_res, X_test)
            cv_acc_rf[j] = table(fit, y_test) %>% diag %>% sum %>% `/`(length(y_test))
        }
        cv_50acc5_rf <- append(cv_50acc5_rf, mean(cv_acc_rf))
    } ## end for
    boxplot(list(RF=cv_50acc5_rf), xlab="Random forest")
}


ui <- fluidPage(
    sliderInput("s1", "quantile1:",
                min = 0, max = 1, value = 0.9
    ),
    sliderInput("s2", "cvk:",
                min = 1, max = 50, value = 2
    ),
    sliderInput("s3", "nsim:",
                min = 1, max = 100, value = 2
    ),
    actionButton("b1", "ok"),
    plotOutput("Plot") %>% withSpinner(type = 5)
)

# Server logic
server <- function(input, output) {
    
    p1 <- eventReactive(input$b1,{
        plot_f(input$s1, input$s2, input$s3)
    })
    
    output$Plot <- renderPlot({
        p1()
    })
}

shinyApp(ui, server)

