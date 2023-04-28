#Install packages if not installed
installReqs <- function(package_name, bioc) {
  if (requireNamespace(package_name, quietly = TRUE) == FALSE) {
    if (bioc == FALSE)
      install.packages(package_name)
    else if (bioc == TRUE) #install using Bioconductor package manager
      BiocManager::install(package_name)
  }
}


#Install pacakges:
installReqs("BiocManager", bioc = FALSE)
installReqs("shiny", bioc = TRUE)
installReqs("shinydashboard", bioc = TRUE)
installReqs("shinyjqui", bioc = TRUE)
installReqs("DT", bioc = TRUE)
installReqs("RColorBrewer", bioc = TRUE)
installReqs("pheatmap", bioc = TRUE)
installReqs("ggplot2", bioc = TRUE)
installReqs("shinycssloaders", bioc = TRUE)
installReqs("shinyWidgets", bioc = TRUE)
installReqs("shinyjs", bioc = TRUE)
installReqs("colourpicker", bioc = TRUE)
installReqs("bayestestR", bioc = TRUE)




app_version = getRversion()

#Loading packages:
library(shiny)
library(shinydashboard)
library(shinyjqui)
library(DT)
library(RColorBrewer)
library(pheatmap)
library(ggplot2)
library(shinycssloaders)
library(shinyWidgets)
library(shinyjs)
library(colourpicker)
library(bayestestR)


################# #################### #################### UI COMPONENT #################### #################### ####################
ui <- dashboardPage(
    #This is setting the title of the application
    dashboardHeader(title = "Dummy Project", titleWidth=250),
    
    dashboardSidebar(width = 250,
                     
         tags$style(".skin-blue .sidebar 
               a { 
                  color: #444; 
               }"),
               
         tags$style(".skin-blue .sidebar 
               a.sidebarlink:link, a.sidebarlink:visited { 
                                    color: #FFF;
               }"),
         
         tags$style(".skin-blue .sidebar
                a.sidebarlink:hover {
                                    color: #777;
               }"),
               
         tags$style(".skin-blue .sidebar
                .center {
                        text-align: center;
               }"),
               
         tags$style(".skin-blue .sidebar
                .borderbox {
                        border: 2px solid #666;
                        padding: 5px 5px 5px 5px;
                        margin: 2px;
               }"),
           
           
            #Panel for data uploading sampleshet
            conditionalPanel(condition = "input.program == 'Tab1'",
               div(id = 'samplesheet page',
                   tags$div('class' = "center",
                   tags$br(
                   h6(HTML('<br><b>This is a tool for the visualization and .... </b>.')),
                   tags$br(),
                   tags$br(),
                   tags$br())),
                   tags$div('class'="borderbox",
                         fileInput(inputId  = "SampleSheet",
                                   label    = "Choose .csv File",
                                   multiple = FALSE,
                                   accept   = c("text/csv","text/comma-separated-values,text/plain",".csv"))
                           ),
                   
                   tags$br(),
                   tags$div('class'="borderbox",
                            #sets the header to true automatically
                            checkboxInput("header", "Header", TRUE),
                            #sets the sep parameter when uploading files into R
                            radioButtons("sep", "Separator",
                                          choices  = c(comma = ",",Semicolon = ";",Tab = "\t"),selected = ","),
                                       )
                                     )
                                   ),
                            
           
           #Panel for Diff bind and gene annotation plot 
           conditionalPanel(condition = "input.program == 'Tab2'",
                      tags$br(),
                      tags$br(),
                      
                      tags$div('class'="borderbox",
                               h4(HTML('<b>Baseline Correction</b>')),
                               checkboxInput("BaselineCorrection", label = tags$h5("Baseline Correct"), value = FALSE),
                              ),
                      
               conditionalPanel(condition= "input.histogram == 'Tab1'",
                  div(id = 'Plots_Sidebar',
                   
                   tags$div('class'="center",
                            uiOutput("sampleID"),
                           ),
                   
                   tags$div('class'="borderbox",
                   textInput("Main_Title_Hist",label=h4("Main Title"),width="400",value="",placeholder="Plot..."),
                   textInput("Vertical_Title_Hist",label=h4("Y-Axis Title"),width="400",value="",placeholder="Plot..."),
                            ),
                   
                    tags$div('class'="center",
                     h4(HTML('<br><b>Download options</b>')),
                        tags$div('class' = "borderbox",
                            tags$br(),
                            h4(HTML('<b>Baseline Corrected</b>')),
                            downloadButton("downloadFinalReport", 
                                           "Baseline Corrected(.csv)",
                                           style = "color: #fff; background-color: #27ae60; border-color: #fff;padding: 5px 14px 5px 14px;margin: 5px 5px 5px 5px; "),
  
                            h4(HTML('<b>AUC Calculated</b>')),
                            downloadButton("downloadAUCSheet", 
                                           "Table_AUC (.csv)",
                                           style = "color: #fff; background-color: #27ae60; border-color: #fff;padding: 5px 14px 5px 14px;margin: 5px 5px 5px 5px; "),
                                          )
                                         )
                                        )
                                       ),
                                      
                  conditionalPanel(condition= "input.histogram == 'Tab2'",
                        div(id = 'Plots_Sidebar',
                           tags$div('class'="borderbox",
                                textInput("Main_Title_Bar",label=h4("Main Title"),width="400",value="",placeholder="Plot..."),
                                textInput("Vertical_Title_Bar",label=h4("X-axis Title"),width="400",value="",placeholder="Plot..."),
                                               )
                                              )
                                             )
                                            )
                                           
                                          
), #End dashboard sidebar

            
    dashboardBody(
                
               tabBox(id= "program", width=30,height=650, side = "left",
                      
                   tabPanel(title = "Sample Information", id = "Tab1", value = "Tab1",
                     HTML('<div style="padding:10px 10px 10px 10px;"><p>'),
                               tags$h3("Sample  Information"),
                               tags$hr(),
                               #Data table output
                               DT::dataTableOutput(outputId = "uploaded_samplesheet"),HTML('</p></div>')
                                          ), 
                                        
                       
                   
                   #Panel for PCA, Heatmap, MA and Volcano plots
                   tabPanel(title = "Plots", id = "Tab2", value = "Tab2",
                      HTML('<div style="padding:5px 5px 5px 20px;"<p>'),
                      tabsetPanel(id = "histogram",
                         tabPanel(title = "Histogram Plot", id = "Tab1", value = "Tab1",
                            tags$h3("Histogram Plot"),
                            tags$hr(),
                            fluidRow(width=40,
                               dropdown(label = "Save Plot",
                                  selectInput("Histogramplot_dpi", label = "Output dpi", width = "95",
                                              choices = list("24 dpi" = 24, "48 dpi" = 48, "96 dpi" = 96, "192 dpi" = 192),
                                              selected = 48),
                                  downloadButton("saveHistpng","PNG"),
                                  HTML('<br><br>'), 
                                  downloadButton("saveHistjpg","JPG"),
                                  HTML('<br><br>'),
                                  downloadButton("saveHisttiff","TIFF"),
                                  size = "default",
                                  icon = icon("download", class = ""), 
                                  up = FALSE),
                               HTML('</p>'),
                               jqui_resizable(
                                 plotOutput("Hist_plot", height = "350", width = "350"),
                                             )
                                            )
                                           ),
                         
                         tabPanel(title = "Area Under the Curve (AUC) Bar Plot", id = "Tab2", value = "Tab2",
                            fluidRow(width=40,
                               tags$h3("Area Under the Curve"),
                               tags$hr(),
                               dropdown(label = "Save Plot",
                                  selectInput("Plot2_dpi", label = "Output dpi", width = "95",
                                              choices = list("24 dpi" = 24, "48 dpi" = 48, "96 dpi" = 96, "192 dpi" = 192),
                                              selected = 48),
                                  downloadButton("savePlot2png", "PNG"),
                                  HTML('<br><br>'), 
                                  downloadButton("savePlot2jpg", "JPG"),
                                  HTML('<br><br>'),
                                  downloadButton("savePlot2tiff", "TIFF"),
                                  size = "default",
                                  icon = icon("download", class = ""), 
                                  up   = FALSE),
                               HTML('</p>'),
                               jqui_resizable(
                                  plotOutput("Plot2_plot", height = "350", width = "350")
                                             )
                                            )
                                          ),
                                        ),HTML('</p></div>')
                                      )
                   
               
)),#end dashboardBody
       
                     skin = "blue"

) #end dashboard page 


            
               
#################### #################### SERVER COMPONENT #################### ####################
shinyServer <- function(input, output, session) {
   # ---- DATA UPLOAD / input --------- #
   #reading in the user input and saving it as samples
   samplesheet <- reactive({
       req(input$SampleSheet)
       samples <- read.csv(input$SampleSheet$datapath,
                           header    = input$header,
                           sep       = input$sep,
                           row.names = 1)
       samples2 <- as.data.frame(t(samples))
       #modify rownames
       rownames <- row.names(samples2)
       Wavelength <- as.numeric(gsub("[a-zA-Z ]", "", rownames))
       #bind new rownames
       final_sample_datasheet <- cbind(Wavelength,samples2)
       return(final_sample_datasheet)
     }
   )
   
   
   #Baseline Correction
   baseline_correction <- reactive({
      samplesheet <- as.data.frame(samplesheet())
      
      for(i in 1:nrow(samplesheet)) {
         samplesheet[i,] <- samplesheet[i,] - samplesheet[600,]
      }
      samplesheet <- samplesheet[,2:ncol(samplesheet)]
      
      rownames <- row.names(samplesheet)
      Wavelength <- as.numeric(gsub("[a-zA-Z ]", "", rownames))
      normalized_sample_datasheet <- cbind(Wavelength,samplesheet)
      return(normalized_sample_datasheet)
     }
   )
   
   
   
   
   #Subsetting dataset
   output$sampleID <- renderUI({
      samplesheet <- samplesheet()
      subset <- colnames(samplesheet)
      selectInput("sampleID", h4(HTML('<b>Choose The Sample</b><br>')), subset, selected = subset[2]) 
      }
     )
   
   subsetsample <-  reactive({
      sheet <- as.data.frame(samplesheet())
      sheet_baseline_corrected <- as.data.frame(baseline_correction())
      if (input$BaselineCorrection == TRUE) {
         New_SampleSheet = as.data.frame(sheet_baseline_corrected[,input$sampleID])
      } else if(input$BaselineCorrection == FALSE) {
         New_SampleSheet = as.data.frame(sheet[,input$sampleID])
      }
      return(New_SampleSheet)
     }
    )


   #AUC calculation
   AUC_Sheet <- reactive({
      samples <- samplesheet()
      sheet_baseline_corrected <- baseline_correction()
      
      if (input$BaselineCorrection == TRUE) {
         samplesheet = as.data.frame(sheet_baseline_corrected)
      } else if(input$BaselineCorrection == FALSE) {
         samplesheet = as.data.frame(samples)
      }
      
      AUC = c()
      for(i in 1:ncol(samplesheet)) {
         auc_loop = area_under_curve(samplesheet[[1]],
                                     samplesheet[[i]],
                                     method="trapezoid")
         AUC = append(AUC,auc_loop)
                                    }
         new_sheet <- as.data.frame(rbind(
                                    colnames(samplesheet),
                                    t(AUC)
                                   )
                                  )
         
        row.names(new_sheet) <- c("Sample ID","Area Under the Curve")
        remove_wavelength_column <- new_sheet[,2:ncol(new_sheet)]
      
      return(remove_wavelength_column)
      }
    )
   
   
   
   ######DataTable output
   output$uploaded_samplesheet = DT::renderDataTable ({
       samplesheet <- samplesheet()
       samplesheet},
       options = list(scrollX=T, scrollY=T) 
   )
   
   
   
   ########### Plots
   #Drawing the Plot
   output$Hist_plot = renderPlot({
      withProgress(message = 'Rendering Histogram Plot...', value=1, min=1,max=100, {
         do_Hist_plot()
       }
      )
     }
    )
   do_Hist_plot <- reactive ({
      samples <- subsetsample()
      #Add ProgressBar
      total_steps  = 2
      current_step = 1
      current_step = current_step + 1
      incProgress(current_step/total_steps*100,
                  detail = paste("Generating histogram plot"))
      #plot histogram
      matplot(samples,
              ylab=input$Vertical_Title_Hist,
              xlab="Wavelength (nm)",
              main=input$Main_Title_Hist,
              cex=0.8)
    }
   )
   
   #Drawing the AUC plot
   output$Plot2_plot = renderPlot({
      withProgress(message = 'Rendering AUC Plot...', value=1, min=1,max=100, {
         do_AUC_plot()
       }
      )
     }
    )
   do_AUC_plot <- reactive ({
      samples <- as.data.frame(AUC_Sheet())
      samplesheet <- as.data.frame(samplesheet())
      #Add ProgressBar
      total_steps  = 2
      current_step = 1
      current_step = current_step + 1
      incProgress(current_step/total_steps*100,
                  detail = paste("Generating AUC Plot plot"))
      
      counts <- as.numeric(samples[2,1:ncol(samples)])
      names  <- c(colnames(samplesheet[,2:ncol(samplesheet)]))
      
      #plot barplot
      par(mar=c(11,4,4,4))
      barplot(counts,
              horiz=FALSE,
              ylab=input$Vertical_Title_Bar,
              main=input$Main_Title_Bar,
              cex.names=0.8,las=2,
              names.arg = names)
     }
    )
   
   
   
   ########### Table Downloads
   #Download the full report data table:
   output$downloadFinalReport <- downloadHandler(
      filename = function() {paste("Baseline_Corrected.csv")},
      content  = function(file){
         write.csv({Report<-baseline_correction() 
                    dataframe = as.data.frame(Report)
                    dataframe},
                    file, row.names=FALSE)
      }
    )
   
   #Download the AUC  data table:
   output$downloadAUCSheet <- downloadHandler(
      filename = function() {paste("AUC_calculated.csv")},
      content  = function(file){
         write.csv({Report<-AUC_Sheet() 
                   dataframe = as.data.frame(Report)
                   dataframe},
                   file, row.names=TRUE)
      }
    )

}




shinyApp(ui = ui, server = shinyServer)
