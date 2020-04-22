# APP MODULARE COMPLETA DI SELEZIONE ANALIZZATORE E RITORNO DEL DATASET
# 
# torna un dataset

########################################################################################################
########################################################################################################
########################################################################################################
# MODULO



  ########################################################################################################
  # UI

  seleziona_analizzatoreInput <- function(id) {
    # Creo il namespace
    ns <- NS(id)
    
    
    # recupero elenco reti.
    sql_txt="select netcd, netnm from netcd order by netnm"
    
    #esegue query
    ds_elencoreti <- query_postgres(sql_txt)
    
    if (nrow(ds_elencoreti)>0){
          
      # trasformo in lista
          lista_reti <- split(ds_elencoretnrowi$netcd, ds_elencoreti$netnm)
          lista_reti <- with(ds_elencoreti, split(netcd,netnm))
          
        
          tagList(
            p("Seleziona rete"),
            
            wellPanel(
              selectInput(ns("ui_select_rete"), "Scelta rete", choices =lista_reti),
              verbatimTextOutput(ns("netcd"))
            ),
            
            
            wellPanel(
              uiOutput(ns("ui_select_stazione"))
              
            ),
            
            
            wellPanel(
              uiOutput(ns("ui_select_analizzatore"))
             
            ),
            
               
              column(6, wellPanel(
                numericInput(ns("decimali"), label = p("Numero decimali"), value = 1)
              )),
            
               
              column(6, wellPanel(
                radioButtons(ns('aggregazione'), 'Aggregazione temporale',   c(orari='orari', giornalieri='giornalieri'))
              )
            ),
         
            wellPanel(
              actionButton(ns("button_estrai_dati"), "Estrazione Dati"),
              downloadButton(ns("downloadData"), "Download"),
              dataTableOutput(ns("tabella_dati")) 
            )
          
        )    
    }      
  
  
  }
  # END UI
  ########################################################################################################




  ########################################################################################################
  # SERVER
  
  seleziona_analizzatore <- function(input, output, session,master_data_start,master_data_end) {
    
    
    
    
   
    #################################################################################
    output$ui_select_stazione <- renderUI({
      # list dinamica scelta stazione
      
      # fondamentale per elementi ui creati dinamicamente
      ns <- session$ns
      
      netcd<-input$ui_select_rete
      
      sql_txt=paste("select statcd,statnm from testat where TESTAT.NETCD=",netcd,"  AND TESTAT.STATSTCD=1 order by statcd")
      
      ds_elencostaz <- query_postgres(sql_txt)
      
      #trasformo in lista
      lista_staz <- split(ds_elencostaz$statcd,ds_elencostaz$statnm)
      lista_staz <- with(ds_elencostaz, split(statcd,statnm))
      
      selectInput(ns("ui_select_stazione"), "Scelta stazione", choices = lista_staz)
      
    })## END ui_select_stazione
    #################################################################################
    
    
    
    
    #########################################################################################
    # list dinamica scelta analizzatore
    output$ui_select_analizzatore <- renderUI({
      
      req(input$ui_select_rete,input$ui_select_stazione)
      
      ns <- session$ns
      
      netcd<-input$ui_select_rete
      statcd<-input$ui_select_stazione
      
      sql_txt=paste("
                    select  TEANAPT.NETCD ||  '_' || TEANAPT.STATCD ||  '_' || TEANAPT.ANAPTID ||  '_' || TEANAPT.PARAMCD AS COORDINATE
                    ,ana1

                    FROM TEANAPT,TESTAT,NETCD
                    WHERE TEANAPT.NETCD=TESTAT.NETCD
                    AND NETCD.NETCD=TESTAT.NETCD
                    AND TEANAPT.STATCD=TESTAT.STATCD
                    AND TEANAPT.NETCD=TESTAT.NETCD
                    AND TESTAT.STATSTCD=1
                    AND TEANAPT.ANA999=1
                    
                    AND TEANAPT.PARAMCD<103
                    AND NETCD.NETCD=",netcd," 
                    AND TEANAPT.STATCD=", statcd, " 
                    ORDER BY TESTAT.STATNM, TEANAPT.PARAMCD

                        
                    ")
      
      #cat(sql_txt)
      
      #esegue query
      ds_elencoanaliz <- query_postgres(sql_txt)
      
      
      #trasformo in lista
      lista_analiz <- split(ds_elencoanaliz$ana1,ds_elencoanaliz$coordinate)
      lista_analiz <- with(ds_elencoanaliz, split(coordinate,ana1))
      
      selectInput(ns("ui_select_analizzatore"), "Scelta Analizzatore", choices = lista_analiz)
      
      
    })
    # end output$ui_select_analizzatore
    #######################################################################################
    
    
    
    
    
    
    
    #######################################################################################
    # click sul pulsante 'Estrazione Dati' nella pagina Provenienza
    #######################################################################################
    
    dyn_dati<-eventReactive(input$button_estrai_dati, {
      
      ## questa funzone deve ritornare un dataset in modo reattivo
      
      # ui_select_analizzatore contiene le coordnate della misura selezionata
      req(input$ui_select_analizzatore)
      
      # Create a Progress object
      progress <- shiny::Progress$new()
      # Make sure it closes when we exit this reactive, even if there's an error
      on.exit(progress$close())
      progress$set(message = "Elaborazione in corso...", value = 0)
      
      #data1=paste(input$date_oss[1])
      #data2=paste(input$date_oss[2])
      data1<-master_data_start
      data2<-master_data_end
      
      
      # estraggo dati analizzatore scelto dalla select prov_coord_analiz, contiene le info su inquinanante da monitorare
      
      str_coord_analiz=input$ui_select_analizzatore
      
      list_coord_analiz=unlist(strsplit(str_coord_analiz, split="_"))
      
      an_netcd=list_coord_analiz[1]
      an_statcd=list_coord_analiz[2]
      an_ptid=list_coord_analiz[3]
      an_paramcd=list_coord_analiz[4]
      
      tipo_aggregazione <-input$aggregazione
      decimali          <-input$decimali
      
      dataframe_serie_analiz = get_serie_dati_bypost(data1,data2,an_netcd,an_statcd,an_ptid,tipo_aggregazione,decimali)
      
      # rimuovo Na
      dataframe_serie_analiz<-na.omit(dataframe_serie_analiz)
      
      dataframe_serie_analiz
      
    })#
    #######################################################################################
    
    
   
    
    #######################################################################################
    # click sul pulsante 'donwload' nella pagina Provenienza
    #######################################################################################
    
    # Downloadable csv of selected dataset ----
    output$downloadData <- downloadHandler(
      filename = function() {
        
        #data1=paste(input$date_oss[1])
        #data2=paste(input$date_oss[2])
        data1<-master_data_start
        data2<-master_data_end
        
        
        coordinate<-paste(input$ui_select_analizzatore, "_", input$ui_select_stazione)
        analiz_staz<-analiz_staz_by_coordinate(coordinate)
        
        paste(analiz_staz,"_",data1,"_",data2,".csv")
        
      },
      content = function(file) {
        write.csv(dyn_dati(), file, sep=';', row.names = TRUE)
      }
    )
    
    
    
    # resituisco il dataset..
      dyn_dati()
      
    
    
    
  }
  
  # END SERVER - MODULO
  ########################################################################################################



# END MODULO
########################################################################################################
