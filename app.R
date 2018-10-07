library(ggplot2)
library(gridExtra)
library(dplyr)

Lister <- readRDS("my_list.rds")

MonthsData<- read.csv("HomeTable.csv")

Output <- read.csv("PredictionsL1.csv")

load("Final_Model1.rda")
shinyApp (
  fluidPage(
    
   titlePanel( h3("Price Estimation and Product Clickstream")),
   br(),
    
    fluidRow(
      column(12,div(style = "height:500px", 
                   tabsetPanel(
                                
                               tabPanel("Summary", uiOutput("summary")), 
                               tabPanel("Similar Products to the Input",dataTableOutput("table")),
                               tabPanel("Plot", plotOutput("plot")),
                               tabPanel("Plot1", plotOutput("plot1")),
                               tabPanel("Plot2", plotOutput("plot2")),
                               tabPanel("Plot3", plotOutput("plot3")),
                               tabPanel("Plot4", plotOutput("plot4"))
                               )
                   ))
             
             ),
      
    
    hr(),
    
    fluidRow(
      column(3,
            
             br(),
             sliderInput("DemandRate","Maximum Concession",min=0,max=100,value=0),
             checkboxInput("CheckInput",strong("Is Your Product Flipkart or Amazon Verified"))
             
      ),
      column(4,offset=1,
             selectInput("Category1","Category the Product Belongs",
                         choices =c("Clothing","Jewellery","Home","Footwear","Mobiles","Automotive","Kitchen",
                          "Watches","Toys","Sports","Computers")),
             selectInput("Category2","Category the Product Belongs","placeholder"),
                        
                  
             textInput("Category3","Category the Product Belongs"
                         )
             ),
      column(3,offset=1,
             textInput("Brand","Brand of the Product"),
             br(),
             actionButton("update" ,"Predict", icon("refresh"),
                          class = "btn btn-primary"))
    )
  ),
  server=function(input,output,session){
    
    observe({
     
      x <- input$Category1
      y <- Lister[[x]]
      updateSelectInput(session, "Category2", choices = y)
    })
    
    observe({
      if(input$update>0){
        Demandate <- input$DemandRate/100
        Demandate <- ((1)/(1-Demandate))
        rbind(Output,c(input$Category1,input$Category2,input$Category3,0,as.integer(input$CheckInput),input$Brand,Demandate)) -> Output
        as.numeric(Output$retail_price) -> Output$retail_price
        as.numeric(Output$Demand) -> Output$Demand
        as.integer(Output$is_FK_Advantage_product) -> Output$is_FK_Advantage_product
        Output[is.na(Output)] <- ""
        as.factor(Output$Col1) -> Output$Col1
        as.factor(Output$brand) -> Output$brand
       Prediction <-predict(Fit,Output[nrow(Output),])
        output$summary <- renderUI({
        x<-paste("The Price of Prediction of your required item is",round(abs(Prediction),2))
        x<-paste("<h1> ",x,"</h1>")
       # x<-paste(x,"<strong>Its about 66% Higher than element</strong>")
        HTML(x)
        
        })
        output$plot <- renderPlot({
          
          YY=subset(MonthsData,Col1==c(input$Category1))
          p1<-  ggplot(aes(x=YY$Months,y=YY$Price,group=1),data=YY)+geom_point(shape=8)+geom_line(linetype="dotdash",color="blue")+ylab(paste("Price",input$Category))+labs(x="Months",title=paste("Price Variation of Retail Price with Month on",input$Category1))
          p2 <- ggplot(aes(x=YY$Months,y=YY$Demand,group=1),data=YY)+geom_point(shape=8)+geom_line(linetype="dotdash",color="blue")+ylab(paste("Demand",input$Category))+labs(x="Months",title=paste("Price Variation of Demand with Month on",input$Category1))
          grid.arrange(p1,p2)
        })
        
        output$plot1 <- renderPlot({
          YY=subset(Output,Col1==input$Category1)
          p2<- ggplot(aes(x=Col1,y=retail_price,fill=Col1),data=Output)+geom_boxplot()+ylim(c(0,5000))
          p2 <- p2+labs(x="Categories",y="Retail Price",title="Comparision of Retail Price Variation on different Categorical Items using Boxplot")
          p2
          })
        
        output$plot2 <- renderPlot({
          YY=subset(Output,Col1==input$Category1)
          p1<- ggplot(aes(x=retail_price),data=YY)+geom_histogram(bins=30,fill="blue",col="red",alpha=.9)+xlim(0,5000)
          p1 <-p1+labs(x="Retail Price",title="Distribution of Retail Price of Categorical Item Choosen")
          p1
          })
        
        output$plot3 <- renderPlot({
          YY=subset(Output,Col1==input$Category1)
          p1 <- ggplot(aes(x=Col2,y=retail_price),data=YY)+geom_boxplot(aes(fill=factor(Col2)))+ylim(0,5000)
          p1 <- p1 +labs(x="Categories",y="Retail Price",title="Comparision of Retail Price Variation on different Categorical Items as according to first Categorical choosen itemset using Boxplot")
          p1
          })
        
        output$plot4 <- renderPlot({
          
          MonthsData<- read.csv("SummarisedL1.csv")
          YY=subset(MonthsData,Col1==input$Category1)
          
          p1<-ggplot(aes(x=Months,y=Price,col=Col2,group=Col2),data=YY)+geom_point(shape=4,size=12)+geom_line(linetype="dashed",size=1.25)+ylim(0,5000)
          p1 <-p1+labs(title="Variation of Price Monthly wise of the categories as according to first Categorical choosen Itemset ")
          p1
      })
        output$table <- renderDataTable({
          Train <- read.csv("PredictionsL1.csv")
          rbind(Output,c(input$Category1,input$Category2,input$Category3,0,as.integer(input$CheckInput),input$Brand,Demandate)) -> Output
          as.numeric(Output$retail_price) -> Output$retail_price
          as.numeric(Output$Demand) -> Output$Demand
          as.integer(Output$is_FK_Advantage_product) -> Output$is_FK_Advantage_product
          Output[is.na(Output)] <- ""
          as.factor(Output$Col1) -> Output$Col1
          as.factor(Output$brand) -> Output$brand
          With_Discount<-subset(Train,Col1==input$Category1)
         
         # With_Discount <- rbind(With_Discount,c("Clothing","Mens","Lingerie",1000,0,"Alisha",2))
          With_Discount1 <- select(With_Discount,c(retail_price,Demand,is_FK_Advantage_product))
          irisCluster <- kmeans(With_Discount1, 6, nstart = 5)
          XX <- cbind(With_Discount,Clusters=irisCluster$cluster)
          Points <- tail(XX$Clusters)[6]
          XX<- subset(XX,Clusters==Points)
          return (XX[sample(nrow(XX),4),][-8])
          
        })
      }
    })
   
    
  }
  )

