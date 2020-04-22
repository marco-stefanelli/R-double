# MODULO mod_analisi_serie.R# 
# Esegue varie anlisi neumeriche e/o grafiche su una o più serie numeriche

########################################################################################################
########################################################################################################
########################################################################################################
# MODULO


# solo per test ------------------------------------------
df<-read.table('BOLETTINO_PM10_08-12-2019.CSV',sep=';', header=TRUE)
RV   <- reactiveValues(data = df)
# solo per test ------------------------------------------



  ########################################################################################################
  # UI

  analisi_serieInput <- function(id) {
    # Creo il namespace
    ns <- NS(id)	
	
		# struttura pagina con sidebar e main 	 		  
		sidebarLayout(      

			sidebarPanel(
			# select di scelta della serire, viene generata dinamicamente 
			uiOutput(ns("ui_select_serie"))
			
			),
			
			# Create a spot for the barplot
			mainPanel(
			  h3("Analisi"),
			  plotOutput("plot_istogramma")
			)
		)
  }
  # END UI
  ########################################################################################################
  ########################################################################################################

  
  
  
  
  
  ########################################################################################################
  ########################################################################################################
  ########################################################################################################
  # SERVER
  
	analisi_serie <- function(input, output, session) {
    
    #################################################################################
	  # list dinamica scelta serie
	
    output$ui_select_serie <- renderUI({
      
      df_lavoro <- RV$data    
    
  		if(nrow(df_lavoro)>0){    
  			#trasformo in lista
  			lista_serie <- colnames(df_lavoro)[-1]      
  			selectInput("ui_select_serie", "Scelta serie", choices = lista_serie)
  		}  
      
    })
    ## ENDui_select_serie
    #################################################################################
    
    
    
    
    #################################################################################
    # rende i dati della serie correntenemente selezionata
    
    dati_cur_serie<-reactive({
       cur_serie <- input$ui_select_serie
       cat('serie scelta=',cur_serie)
       RV$DATA[,get(cur_serie)]
    })
    
    # END 
    #################################################################################
    
    
    
    
    
    
    #######################################################################################
    # genero istogramma distribuzione delle frequenze
    output$plot_istogramma <- renderPlot({
      
      valori=dati_cur_serie()
      nome_serie<-input$ui_select_serie
    
      titolo_grafico<-paste("Istogramma di distribuzione delle frequenze di valori")
      
      hist(valori,
           xlab=nome_serie,
           border="blue", 
           col="green",
           las=1,
           main=titolo_grafico
      )
      
    })
    #############################################################################
    
	} 
  # END SERVER - MODULO
  ########################################################################################################
