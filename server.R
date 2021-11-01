library(shiny)
library(shinyjs)
library(DT)
library(shinycssloaders)
library(corrgram)
library(ggplot2)
library(RColorBrewer)

data <- read.csv("Data.csv", header = TRUE, stringsAsFactors = TRUE)
data$Date <- as.Date(data$Date, format = "%d/%m/%Y")
data$ID <- as.numeric(substr(data$ID, 2, 6))

shinyServer(function(input, output, session) {
    
    output$DataSummary <- renderPrint({
        skimr::skim(data)
    })
    
    output$DataStructure <- renderPrint({
        str(data)
    })
    
    output$RawData <- DT::renderDataTable({
        DT::datatable(as.data.frame(data))
    })
    
    output$Missing <- renderPlot({
        visdat::vis_miss(data, cluster = input$cluster, sort_miss = input$sort)
    })

    output$RiseValue <- renderPlot({
        
        d <- scale(x = data[,input$NumVariable], center = input$center, scale = input$scale)
        mypalette = rainbow(ncol(d))
        matplot(x = seq(1, 100, length.out = nrow(d)), y = d, type = "l", 
                xlab = "Percentile", ylab = "Values", lty = 1, lwd = 1, 
                col = "blue")
        
    })
    
    output$Homogeneity <- renderPlot({
        
        d <- scale(x = data[,15:44], center = input$center2, scale = input$scale2)
        mypalette = rainbow(ncol(d))
        matplot(data, type = "l", col = mypalette, xlab = "Observations in sequence", ylab = "Value") 
        legend(legend = colnames(d), x = "topleft", y = "top", lty = 1, lwd = 1, col = mypalette, ncol = round(ncol(d)^0.3))
    }) 
    
    output$Histogram <- renderPlot({
        
        hist(data[,input$NumVariable2], xlab = "Value", main = "", breaks = input$bin)

    })
    
    output$table <- renderPrint({
        table(data[,input$CatVariable])
    })
    
    output$Pie <- renderPlot({
        x <- table(data[,input$CatVariable])
        piepercent<- round(100*x/sum(x), 1)
        pie(x, labels = piepercent, main = "Categorical Variables Pie Chart",col = rainbow(length(x)))
        legend("topright", levels(data[,input$CatVariable]), cex = 0.8,
               fill = rainbow(length(x)))

    })
    
    output$BoxPlot <- renderPlot({
        d <- as.matrix(data[, 15:44])
        d <- scale(d, center = input$standardise, scale = input$standardise)
        car::Boxplot(y = d, ylab = NA, use.cols = TRUE, notch = FALSE, varwidth = FALSE,  
                     horizontal = FALSE, outline = input$outliers, 
                     col = brewer.pal(n = dim(d)[2], name = "RdBu"),
                     range = input$range, main = "Boxplots of Numeric Variables", 
                     id = ifelse(input$outliers, list(n = Inf, location = "avoid"), FALSE)) 
    })
    
    output$Correlation <- renderPlot({
        corrgram::corrgram(data[, 15:44], 
                 order = input$Group, 
                 abs = input$abs, 
                 cor.method = input$CorrMeth,
                 text.panel = panel.txt,
                 main = "Correlation of Numeric Variables")
    })
    
    output$Mosaic <- renderPlot({
        formula <- as.formula(paste("~",paste(input$CatVariables, collapse = " + ")))
        vcd::mosaic(formula, data = data,
                    main = "Categorical Variables", shade = TRUE, legend = TRUE)
    })
    
    output$tableplot <- renderPlot({
        data2 <- data.frame(row = 1:nrow(data), data)  
        tabplot::tableplot(data2, sortCol = "row", decreasing = FALSE)
    })
    
    output$MixedPairs <- renderPlot({
        if (input$CatVariable2 == "NULL") {
            GGally::ggpairs(data = data[, c(input$Pair1, input$Pair2, input$Pair3, input$Pair4)], 
                            mapping = ggplot2::aes(colour = NULL), title = "")
        } else {
        GGally::ggpairs(data = data[, c(input$Pair1, input$Pair2, input$Pair3, input$Pair4)], 
                        mapping = ggplot2::aes(colour = data[,input$CatVariable2]), title = "")}

    })
    
    
    
    
    
    
})

#runApp(appDir = ".", display.mode = "showcase")
