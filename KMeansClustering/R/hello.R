#' @title K-Means Clustering
#'
#' @description Performs K-Means Clustering on any numeric dataset given by the user. It shows the clusters, the centers, the profile plot and summary of data.
#'
#' @return NULL
#'
#' @export

KMeansClustering <- function() {
  requireNamespace("shiny")

  ui <- fluidPage(
    titlePanel(title = "K-Means Clustering"),
    h4("This application uses numeric datasets and performs K-Means Clustering"),
    sidebarLayout(
      sidebarPanel(
        fileInput(inputId = "file",label = "Upload your dataset here in .csv format"),
        sliderInput(inputId = "slider",label = "Number of clusters?",min = 1,max = 5,value = 3,step = 1)

      ),
      mainPanel(
        tabsetPanel(
          tabPanel("Data",tableOutput("Data")),
          tabPanel("summary",verbatimTextOutput("summary")),
          tabPanel(title = "Clusters",
                   tags$h4("Showing the clusters"),
                   verbatimTextOutput("clusters"),
                   verbatimTextOutput("size")),
          tabPanel(title="Centers and inter cluster square distances",
                   tags$h2("Showing the centers of all the clusters"),
                   tags$br(),
                   verbatimTextOutput("centers"),
                   tags$br(),
                   verbatimTextOutput("distance"),
                   tags$h3("Profile plot of centers"),
                   plotOutput("profileplot"))
        )

      )
    )
  )

  server <- function(input, output, session) {

    data<-reactive({
      file1<-input$file
      if(is.null(file1)){return()}
      read.table(file1$datapath,sep=",",header = T)
    })

    output$Data<-renderTable({
      data()
    })

    output$summary<-renderPrint({
      if(is.null(data())){return()}
      summary(data())
    })

    output$clusters<-renderPrint({
      data2<-data()
      data3<-sapply(data2, scale)
      rownames(data3)<-rownames(data2)
      km<-kmeans(data3,input$slider)
      km$cluster
    })

    output$centers<-renderPrint({
      data2<-data()
      data3<-sapply(data2, scale)
      rownames(data3)<-rownames(data2)
      km<-kmeans(data3,input$slider)
      km$centers
    })

    output$distance<-renderPrint({
      data2<-data()
      data3<-sapply(data2, scale)
      rownames(data3)<-rownames(data2)
      km<-kmeans(data3,input$slider)
      km$withinss
    })

    output$profileplot<-renderPlot({
      data2<-data()
      data3<-sapply(data2, scale)
      rownames(data3)<-rownames(data2)
      km<-kmeans(data3,input$slider)
      n<-length(colnames(data3))
      plot(c(0),xaxt="n",ylab = "",type = "l",ylim = c(min(km$centers),max(km$centers)),xlim = c(0,n))
      axis(1,at=c(1:n),labels=colnames(data3))
      for(i in c(1:input$slider)){
        lines(km$centers[i, ],lty=i,lwd=2,col="black")
      }
      text(x=0.5,y=km$centers[ ,1],labels = paste("Cluster", c(1:input$slider)))

    })
    output$size<-renderPrint({
      data2<-data()
      data3<-sapply(data2, scale)
      rownames(data3)<-rownames(data2)
      km<-kmeans(data3,input$slider)
      for(i in 1:input$slider){
        hi<-paste("The size of cluster",i,sep = " ")
        bye<-paste(hi,":",sep = " ")
        hello<-paste(bye,km$size[i])
        print(hello)
      }
    })

  }

  shinyApp(ui, server)
  }
