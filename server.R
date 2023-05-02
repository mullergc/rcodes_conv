source("global.R",encoding = "UTF-8")


server <- function(input, output, session) {
  
  output$column_select1 <- renderUI({
    selectInput("column1", "Faça seu gráfico, selecione a coluna:", choices = exames_names)
  })
  
  output$column_select2 <- renderUI({
    selectInput("column2", "Faça seu gráfico,selecione a coluna:", choices = exames_names)
  })
  
  output$column_select3 <- renderUI({
    selectInput("column3", "Faça seu gráfico, selecione a coluna:", choices =exames_names)
  })
  
  output$column_select4 <- renderUI({
    selectInput("column4", "Faça seu gráfico, selecione a coluna:", choices = exames_names)
  })
  
  output$column_select5 <- renderUI({
    selectInput("column5", "Faça seu gráfico, selecione a coluna:", choices = exames_names)
  })
  
  output$column_select6 <- renderUI({
    selectInput("column6", "Faça seu gráfico, selecione a coluna:", choices = exames_names)
  })
  
  output$column_select7 <- renderUI({
    selectInput("column7", "Faça seu gráfico, selecione a coluna:", choices = exames_names)
  })
  
  output$column_select8 <- renderUI({
    selectInput("column8", "Faça seu gráfico, selecione a coluna:", choices = exames_names)
  })
  
  # Filter data based on input
  dados_tr_group <- reactive({
    if (input$TIPO_TRANSPLANTE == "TODOS") {
      return(data_transp)
    } else {
      return(subset(data_transp, TIPO_TRANSPLANTE == input$TIPO_TRANSPLANTE))
    }
  })
  
  data_table <- reactive({
    if (input$TIPO_TRANSPLANTE == "TODOS") {
      return(data)
    } else {
      return(subset(data, TIPO_TRANSPLANTE == input$TIPO_TRANSPLANTE))
    }
  })
  
  transp = reactive({data_table() %>% filter(PERIODO=="TRANSPLANTE")})
  pre_tx =  reactive({data_table() %>% filter(PERIODO=="PRÉ-TX")})
  pos_tx_prec =  reactive({data_table() %>% filter(PERIODO=="PÓS-TX TARDIO")})
  pos_tx_tardio =  reactive({data_table() %>% filter(PERIODO=="PÓS-TX PRECOCE")})

  # CURVA DE INCIDENCIA -----------------------------------------------------
  
df = cc %>%
    mutate(MES_ANO_TRANSP = factor(zoo::as.yearmon(date_index,"%Y-%m"))) %>%
    group_by(MES_ANO_TRANSP,TIPO_TRANSPLANTE) %>%
    summarise(CASOS = sum(count)) %>%
    ungroup()

curva_casos_gr <- reactive({
  

      if (input$TIPO_TRANSPLANTE == "TODOS") {
        return(df)
      } else {
        return(df %>% filter(TIPO_TRANSPLANTE == input$TIPO_TRANSPLANTE))
      }
})

# Dados Gerais ------------------------------------------------------------


## PLOT casos
output$plot_cc <- renderPlotly({
    
    ggplot(data=curva_casos_gr(), aes(x=MES_ANO_TRANSP, y=CASOS,fill=TIPO_TRANSPLANTE)) +
      geom_bar(stat="identity", color = 'white',width = 1) +
      scale_x_discrete(guide = guide_axis(check.overlap = TRUE,angle = 45)) +
      #theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1))+
      labs(title="Número de Transplantes por Mês", 
           x="Mês/ano", y = "Transplantes") + theme_minimal()
    
  })
  
  
### PROCESSAR PARA WORDCLOUD_procedimentos
  
  proced_data = reactive({
    
    proc_cols <- names(data_table())[grepl("PROCED_", names(data_table()))]
    
          proced_data_pretx = data_table() %>%
                dplyr::select(PERIODO,c(proc_cols)) %>% 
                 dplyr::group_by(PERIODO) %>%
                dplyr::summarize(across(all_of(proc_cols), sum, na.rm = TRUE)) %>% 
                ungroup() %>%
                pivot_longer(cols = starts_with("PROCED"), names_to = "PROCED", values_to = "COUNT") %>%
                filter(COUNT>=1) %>% 
                mutate(PROCED = str_remove_all(PROCED,"PROCED_")) %>%
                dplyr::select(PERIODO,PROCED,COUNT)})
  
  proced_data2 = reactive({
    
    proc_cols <- names(data_table())[grepl("PROCED_", names(data_table()))]
    
    proced_data2 = data_table() %>%
      dplyr::select(PRONTUARIO,
                    TIPO_TRANSPLANTE,
                    DT_TRANSPL,
                    EQUIPE_TRANSPLANTE,c(proc_cols),PERIODO) %>% group_by(PRONTUARIO,
                                                                          TIPO_TRANSPLANTE,
                                                                          DT_TRANSPL,
                                                                          EQUIPE_TRANSPLANTE,PERIODO) %>%    
      dplyr::summarize(across(all_of(proc_cols), sum, na.rm = TRUE)) %>% 
      ungroup() %>%
      pivot_longer(cols = starts_with("PROCED"), names_to = "PROCED", values_to = "COUNT") %>%
      mutate(PROCED = str_remove_all(PROCED,"PROCED_")) %>%
      group_by(PRONTUARIO,DT_TRANSPL,EQUIPE_TRANSPLANTE,TIPO_TRANSPLANTE,PERIODO) %>%
      summarise(COUNT=sum(COUNT)) %>% ungroup()})
  
words_pretx <- reactive({
    proced_pretx <- proced_data() %>%
      filter(PERIODO=="PRÉ-TX") %>%
      dplyr::select(-c(PERIODO))
  })

proc_data_pre =  reactive({
  proced_data2() %>%
    filter(PERIODO=="PRÉ-TX") %>%
    dplyr::select(-c(PERIODO))
})


words_tx <- reactive({
  proced_pretx <- proced_data() %>%
    filter(PERIODO=="TRANSPLANTE") %>%
    dplyr::select(-c(PERIODO))
})

proc_data_tx =  reactive({
  proced_data2() %>%
    filter(PERIODO=="TRANSPLANTE") %>%
    dplyr::select(-c(PERIODO))
})

words_pos_tx_prec <- reactive({
  proced_pretx <- proced_data() %>%
    filter(PERIODO=="PÓS-TX PRECOCE") %>%
    dplyr::select(-c(PERIODO))
})

proc_data_pos_tx_prec =  reactive({
  proced_data2() %>%
    filter(PERIODO=="TRANSPLANTE") %>%
    dplyr::select(-c(PERIODO))
})

words_pos_tx_tard <- reactive({
  proced_pretx <- proced_data() %>%
    filter(PERIODO=="PÓS-TX TARDIO") %>%
    dplyr::select(-c(PERIODO))
})


proc_data_pos_tx_tard =  reactive({
  proced_data2() %>%
    filter(PERIODO=="TRANSPLANTE") %>%
    dplyr::select(-c(PERIODO))
})
### PROCESSAR PARA WORDCLOUD_consultoria

consultoria_data = reactive({
  consultoria_cols <- names(data_table())[grepl("CONSULTORIA_", names(data_table()))]
  
    consultoria_data = data_table() %>%
    dplyr::select(PERIODO,c(consultoria_cols)) %>% 
    dplyr::group_by(PERIODO) %>%
    dplyr::summarize(across(all_of(consultoria_cols), sum, na.rm = TRUE)) %>% 
    ungroup() %>%
    pivot_longer(cols = starts_with("CONSULTORIA"), names_to = "CONSULTORIA", values_to = "COUNT") %>%
    filter(COUNT>=1) %>% 
    mutate(CONSULTORIA = str_remove_all(CONSULTORIA,"CONSULTORIA_")) %>%
    dplyr::select(PERIODO,CONSULTORIA,COUNT)})

cons_words_pretx <- reactive({
  proced_pretx <- consultoria_data() %>%
    filter(PERIODO=="PRÉ-TX") %>%
    dplyr::select(-c(PERIODO))
})


cons_words_tx <- reactive({
  proced_pretx <- consultoria_data() %>%
    filter(PERIODO=="TRANSPLANTE") %>%
    dplyr::select(-c(PERIODO))
})

cons_words_pos_tx_prec <- reactive({
  proced_pretx <- consultoria_data() %>%
    filter(PERIODO=="PÓS-TX PRECOCE") %>%
    dplyr::select(-c(PERIODO))
})

cons_words_pos_tx_tard <- reactive({
  proced_pretx <- consultoria_data() %>%
    filter(PERIODO=="PÓS-TX TARDIO") %>%
    dplyr::select(-c(PERIODO))
})


# EXAMES ------------------------------------------------------------------


exame_freq = reactive({
  
  exame_cols <- names(data_table())[grepl("EXAME_", names(data_table()))]
  
  proced_data_pretx = data_table() %>%
    dplyr::select(PERIODO,c(exame_cols)) %>% 
    dplyr::group_by(PERIODO) %>%
    dplyr::summarize(across(all_of(exame_cols), sum, na.rm = TRUE)) %>% 
    ungroup() %>%
    pivot_longer(cols = starts_with("EXAME"), names_to = "EXAME", values_to = "COUNT") %>%
    filter(COUNT>=1) %>% 
    mutate(EXAME = str_remove_all(EXAME,"EXAME_")) %>%
    dplyr::select(PERIODO,EXAME,COUNT)
  
  })


output$count_pretx_exame <- DT::renderDataTable({
  
  exame_freq() %>%
    filter(PERIODO=="PRÉ-TX") %>%
    dplyr::select(-c(PERIODO)) %>%
    arrange(desc(COUNT)) %>% head(10)
  
})

output$count_tx_exame <- DT::renderDataTable({
  
  exame_freq() %>%
    filter(PERIODO=="TRANSPLANTE") %>%
    dplyr::select(-c(PERIODO)) %>%
    arrange(desc(COUNT)) %>% head(10)
  
})

output$count_postxprec_exame <- DT::renderDataTable({
  
  exame_freq() %>%
    filter(PERIODO=="PÓS-TX PRECOCE") %>%
    dplyr::select(-c(PERIODO)) %>%
    arrange(desc(COUNT)) %>% head(10)
  
})


output$count_postxtard_exame <- DT::renderDataTable({
  
  exame_freq() %>%
    filter(PERIODO=="PÓS-TX TARDIO") %>%
    dplyr::select(-c(PERIODO)) %>%
    arrange(desc(COUNT)) %>% head(10)
  
})




# FIM FUNCAO --------------------------------------------------------------


exame_data = reactive({
  
  TOMO_STRINGS <- c("TOMOGRAFIA", "ANGIOTOMOGRAFIA","EXAME_TC")
  ECO_STRINGS <- c("EXAME_ECO","ECOGRAFIA","EXAME_ECODOPPLER")
  RX_STRINGS <- c("EXAME_CRANIO__ENCEFALO_", "EXAME_CRANIO_SEIOS_DA_FACE","EXAME_ABDOMINAL_SUPERIOR", "EXAME_ABDOMEN_TOTAL", "EXAME_PELVICA_TRANSVAGINAL",
                   "EXAME_ABDOMEN_PELVICO","EXAME_TORAX","EXAME_PELVICA_ABDOMINAL","EXAME_COLUNA_DORSAL","EXAME_CRANIO_OUVIDO", "EXAME_CRANIO_SELA_TURCICA",
                   "EXAME_ARTICULACAO_PARTES_MOLES__TORNOZELO_ESQUERDO", "EXAME_ARTICULACAO_PARTES_MOLES__JOELHO_DIREITO", 
                   "EXAME_ARTICULACAO_PARTES_MOLES__JOELHO_ESQUERDO", "EXAME_TORACICA","EXAME_MAMARIA","EXAME_ARTICULACAO_PARTES_MOLES__TORNOZELO_DIREITO", 
                   "EXAME_CRANIO__ORBITAS","EXAME_COLUNA__ARTICULACAO_SACRO_ILIACA_") 
IMUNO_STRINGS <- c("EXAME_EVEROLIMUS_DOSAGEM","EXAME_TACROLIMUS","EXAME_DOSAGEM_DE_CICLOSPORINA_SERICA")

data_table() %>% 
    mutate(TOMO_TOTAL = rowSums(dplyr::select(., contains(TOMO_STRINGS))),
           ECO_TOTAL = rowSums(dplyr::select(., contains(ECO_STRINGS))),
           RX_TOTAL = rowSums(dplyr::select(., contains(RX_STRINGS))),
           IMUNOSS_TOTAL = rowSums(dplyr::select(., contains(IMUNO_STRINGS))))
  
})

exame_data_pre =  reactive({
  exame_data() %>%
    filter(PERIODO=="PRÉ-TX") 
  })


exame_data_tx =  reactive({
  exame_data() %>%
    filter(PERIODO=="TRANSPLANTE") 
})

exame_data_pos_prec =  reactive({
  exame_data() %>%
    filter(PERIODO=="PÓS-TX PRECOCE") 
})

exame_data_pos_tard =  reactive({
  exame_data() %>%
    filter(PERIODO=="PÓS-TX TARDIO") 
})
  



  
## Tabela 1   ------------------------------------------------------------
  
  output$table1 <- render_gt({
    
    data_transp %>% 
      dplyr::select(PERM_INT_TRANSPL,PERM_PRE_TX,PERM_POS_TX,TIPO_TRANSPLANTE) %>% 
      rename('Permanencia durante a Int do Tx'= PERM_INT_TRANSPL,
             'Permanencia até o Tx'= PERM_PRE_TX,
             'Permanencia Após o Tx'= PERM_POS_TX) %>%
      tbl_summary(by=TIPO_TRANSPLANTE,
                  statistic = list(all_continuous() ~ "{median} [{p25}-{p75}]")) %>%
      #  add_p(all_categorical() ~ "fisher.test") %>%
      add_overall() %>% 
      bold_labels() %>% as_gt()
    
    })
  
#Plot LOS geral
output$plot1 <- renderPlotly({
    ggplot(dados_tr_group(), aes(x = PERM_INT_TRANSPL,fill=TIPO_TRANSPLANTE)) +
            geom_histogram(aes(y = ..density..),
                           colour = 1, bins=30) +
            labs(x = "Permanência em Dias") + 
      theme_classic() +
      guides(fill = "none") +
          scale_fill_nejm() +
      theme(legend.position="bottom") + theme(legend.position='none')
  })




output$plot2 <- renderPlotly({ 
  
  ggplot(data = dados_tr_group(),
    aes(x=TIPO_TRANSPLANTE,y=PERM_INT_TRANSPL,fill=TIPO_TRANSPLANTE)) + 
    geom_boxplot()  +
    stat_summary(fun.data=iqr, mult=1, color="black") +
    guides(color = FALSE) +
    # guides(fill = "none") +
    labs(x = "Tipo de Transplante", y = "Tempo de Permanência Total" , title = "", fill = "") +
    guides(fill = "none") +
    theme(legend.position="bottom") +
    theme_bw(base_size = 12) + 
    scale_fill_nejm() +
   # scale_x_discrete(labels=c("TMO Alogenico","TMO Autologo","CARDIACO","CONJUNTIVA","CORNEA","HEPATICO","PULMONAR","RENAL")) +
    theme_classic() + theme(legend.position='none')
})

#Plot LOS pre TX
output$plot3 <- renderPlotly({
  ggplot(data = dados_tr_group(),
         aes(x=TIPO_TRANSPLANTE,y=PERM_PRE_TX ,fill=TIPO_TRANSPLANTE)) + 
    geom_boxplot()  +
    stat_summary(fun.data=iqr, mult=1, color="black") +
    guides(color = FALSE) +
    # guides(fill = "none") +
    labs(x = "Tipo de Transplante", y = "Tempo de Permanência (Pré Tx)" , title = "", fill = "") +
    guides(fill = "none") +
    theme(legend.position="bottom") +
    theme_bw(base_size = 12) + 
    scale_fill_nejm() +
   # scale_x_discrete(labels=c("TMO Alogenico","TMO Autologo","CARDIACO","CONJUNTIVA","CORNEA","HEPATICO","PULMONAR","RENAL")) +
    theme_classic() + theme(legend.position='none')
})

output$plot4 <- renderPlotly({
  ggplot(data = dados_tr_group(),
         aes(x=TIPO_TRANSPLANTE,y=PERM_POS_TX ,fill=TIPO_TRANSPLANTE)) + 
    geom_boxplot()  +
    stat_summary(fun.data=iqr, mult=1, color="black") +
    guides(color = FALSE) +
    guides(fill = "none") +
    labs(x = "Tipo de Transplante", y = "Tempo de Permanência (Pós Tx)" , title = "", fill = "") +
    guides(fill = "none") +
    theme(legend.position="bottom") +
    theme_bw(base_size = 12) + 
    scale_fill_nejm() +
   # scale_x_discrete(labels=c("TMO Alogenico","TMO Autologo","CARDIACO","CONJUNTIVA","CORNEA","HEPATICO","PULMONAR","RENAL")) +
    theme_classic() + theme(legend.position='none')
})


output$plot5 <- renderPlotly({
  ggplot(data = dados_tr_group(),
         aes(x=TIPO_TRANSPLANTE,y=N_INTERNACOES ,fill=TIPO_TRANSPLANTE)) + 
    geom_boxplot()  +
    stat_summary(fun.data=iqr, mult=1, color="black") +
    guides(color = FALSE) +
    guides(fill = "none") +
    labs(x = "Tipo de Transplante", y = "Número de Internações (Total)" , title = "", fill = "") +
    guides(fill = "none") +
    theme(legend.position="bottom") +
    theme_bw(base_size = 12) + 
    scale_fill_nejm() +
  #  scale_x_discrete(labels=c("TMO Alogenico","TMO Autologo","CARDIACO","CONJUNTIVA","CORNEA","HEPATICO","PULMONAR","RENAL")) +
    theme_classic() + theme(legend.position='none')
})

output$plot6 <- renderPlotly({
  ggplot(data = transp(),
         aes(x=TIPO_TRANSPLANTE,y=N_INTERNACOES ,fill=TIPO_TRANSPLANTE)) + 
    geom_boxplot()  +
    stat_summary(fun.data=iqr, mult=1, color="black") +
    guides(color = FALSE) +
    guides(fill = "none") +
    labs(x = "Tipo de Transplante", y = "Número de Internações (Pré-Tx)" , title = "", fill = "") +
    guides(fill = "none") +
    theme(legend.position="bottom") +
    theme_bw(base_size = 12) + 
    scale_fill_nejm() +
   # scale_x_discrete(labels=c("TMO Alogenico","TMO Autologo","CARDIACO","CONJUNTIVA","CORNEA","HEPATICO","PULMONAR","RENAL")) +
    theme_classic() + theme(legend.position='none')
})

output$plot7 <- renderPlotly({
  ggplot(data = pos_tx_prec(),
         aes(x=TIPO_TRANSPLANTE,y=N_INTERNACOES ,fill=TIPO_TRANSPLANTE)) + 
    geom_boxplot()  +
    stat_summary(fun.data=iqr, mult=1, color="black") +
    guides(color = FALSE) +
    guides(fill = "none") +
    labs(x = "Tipo de Transplante", y = "Número de Internações (Pós Tx Precoce)" , title = "", fill = "") +
    guides(fill = "none") +
    theme_bw(base_size = 12) + 
    scale_fill_nejm() +
    scale_x_discrete() +
    theme_classic() + theme(legend.position='none')
})

output$plot8 <- renderPlotly({
  ggplot(data = pos_tx_tardio(),
         aes(x=TIPO_TRANSPLANTE,y=N_INTERNACOES ,fill=TIPO_TRANSPLANTE)) + 
    geom_boxplot()  +
    stat_summary(fun.data=iqr, mult=1, color="black") +
    guides(color = FALSE) +
    guides(fill = "none") +
    labs(x = "Tipo de Transplante", y = "Número de Internações (Pós Tx Tardio)" , title = "", fill = "") +
    guides(fill = "none") +
    theme_bw(base_size = 12) + 
    scale_fill_nejm() +
    scale_x_discrete() + # labels(c()) coloca labels aqui
    theme_classic() + theme(legend.position='none')
})


## PROCEDIMENTO --------------------------------------------------


output$plot_proc_pre <- renderPlotly({
  # proc_data_pre() %>% ungroup() %>%
  #   dplyr::select(TIPO_TRANSPLANTE,COUNT) %>%
  #   mutate(COUNT = as.numeric(COUNT)) %>%
  #   tbl_summary(by="TIPO_TRANSPLANTE") %>% 
  #   #,statistic = list(COUNT ~ "{median} [{p25}-{p75}]")) %>%
  #   bold_labels() %>% as_gt()
  
  ggplot(data = words_pretx(),
         aes(x=TIPO_TRANSPLANTE,y=COUNT ,fill=TIPO_TRANSPLANTE)) + 
    geom_boxplot()  +
    stat_summary(fun.data=iqr, mult=1, color="black") +
    guides(color = FALSE) +
    guides(fill = "none") +
    labs(x = "Tipo de Transplante", y = "Número de Internações (Pós Tx Tardio)" , title = "", fill = "") +
    guides(fill = "none") +
    theme_bw(base_size = 12) + 
    scale_fill_nejm() +
    scale_x_discrete() + # labels(c()) coloca labels aqui
    theme_classic() + theme(legend.position='none')
})


output$table3 <- render_gt({
  proced_data2() %>%
      filter(PERIODO=="PRÉ-TX") %>% 
      dplyr::select(TIPO_TRANSPLANTE,COUNT) %>%
    tbl_summary(by=TIPO_TRANSPLANTE) %>%
    add_overall() %>% 
    bold_labels() %>% as_gt()
})

output$table4 <- render_gt({
  proced_data2() %>%
    filter(PERIODO=="TRANSPLANTE") %>%
    dplyr::select(TIPO_TRANSPLANTE,COUNT) %>%
    tbl_summary(by=TIPO_TRANSPLANTE) %>%
    add_overall() %>% 
    bold_labels() %>% as_gt()
})

output$table5 <- render_gt({
  proced_data2() %>%
    filter(PERIODO=="PÓS-TX PRECOCE") %>%
    dplyr::select(TIPO_TRANSPLANTE,COUNT) %>%
    tbl_summary(by=TIPO_TRANSPLANTE) %>%
    add_overall() %>% 
    bold_labels() %>% as_gt()
  
  })

output$table6 <- render_gt({
  proced_data2() %>%
    filter(PERIODO=="PÓS-TX TARDIO") %>%
    dplyr::select(TIPO_TRANSPLANTE,COUNT) %>%
    tbl_summary(by=TIPO_TRANSPLANTE) %>%
    add_overall() %>% 
    bold_labels() %>% as_gt()
  
})

# EXAMES ------------------------------------------------------------------
output$plotexamespre <- render_gt({
  # exame_data2_pre() %>% ungroup() %>%
  #   dplyr::select(TIPO_TRANSPLANTE,COUNT) %>%
  #   mutate(COUNT = as.numeric(COUNT)) %>%
  #   tbl_summary(by="TIPO_TRANSPLANTE") %>% 
  #   #,statistic = list(COUNT ~ "{median} [{p25}-{p75}]")) %>%
  #   bold_labels() %>% as_gt()
})


# PLOTS_Profiles ----------------------------------------------------------


# WORDCLOUDS Procedimentos ------------------------------------------------


output$wordcloud_pretx <- renderPlot({
  
  words_pretx() %>% 
  ggplot(aes(label = PROCED, size = COUNT)) +
    geom_text_wordcloud_area(rm_outside = TRUE) +
    scale_size_area(max_size = 40) +
    theme_minimal()
  
})


output$wordcloud_tx <- renderPlot({
  words_tx() %>% 
    ggplot(aes(label = PROCED, size = COUNT)) +
    geom_text_wordcloud_area(rm_outside = TRUE) +
    scale_size_area(max_size = 40) +
    theme_minimal()
})  

output$wordcloud_postx_prec <- renderPlot({
  words_pos_tx_prec()%>% 
    ggplot(aes(label = PROCED, size = COUNT)) +
    geom_text_wordcloud_area(rm_outside = TRUE) +
    scale_size_area(max_size = 40) +
    theme_minimal()
})  

output$wordcloud_postx_tardio <- renderPlot({
  words_pos_tx_tard() %>% 
    ggplot(aes(label = PROCED, size = COUNT)) +
    geom_text_wordcloud_area(rm_outside = TRUE) +
    scale_size_area(max_size = 40) +
    theme_minimal()
}) 



# WORDCLOUD CONSULTORIAS --------------------------------------------------
output$wordcloud_pretx_cons <- renderPlot({
  
  cons_words_pretx()  %>% 
    ggplot(aes(label = CONSULTORIA, size = COUNT)) +
    geom_text_wordcloud_area(rm_outside = TRUE) +
    scale_size_area(max_size = 40) +
    theme_minimal()
  
})  

output$count_pretx_cons <- DT::renderDataTable({
  
  cons_words_pretx() %>% arrange(desc(COUNT)) %>% head(10)
  
})

output$wordcloud_tx_cons <- renderPlot({
  
  cons_words_tx()  %>% 
    ggplot(aes(label = CONSULTORIA, size = COUNT)) +
    geom_text_wordcloud_area(rm_outside = TRUE) +
    scale_size_area(max_size = 40) +
    theme_minimal()  
})  

output$count_tx_cons <- DT::renderDataTable({
  
  cons_words_tx() %>% arrange(desc(COUNT)) %>% head(10)
  
})

output$wordcloud_postx_prec_cons <- renderPlot({
  
  cons_words_pos_tx_prec()  %>% 
    ggplot(aes(label = CONSULTORIA, size = COUNT)) +
    geom_text_wordcloud_area(rm_outside = TRUE) +
    scale_size_area(max_size = 40) +
    theme_minimal()    
})  

output$count_postxprec_cons <- DT::renderDataTable({
  
  cons_words_pos_tx_prec() %>% arrange(desc(COUNT)) %>% head(10)
  
})

output$wordcloud_postx_tardio_cons <- renderPlot({

  cons_words_pos_tx_tard()  %>% 
    ggplot(aes(label = CONSULTORIA, size = COUNT)) +
    geom_text_wordcloud_area(rm_outside = TRUE) +
    scale_size_area(max_size = 40) +
    theme_minimal() 
  
})

output$count_postxtard_cons <- DT::renderDataTable({
  
  cons_words_pos_tx_prec() %>% arrange(desc(COUNT)) %>% head(10)
  
})


# graficos exames ---------------------------------------------------------

output$plot9 <- renderPlotly({
  ggplot(data = exame_data_pre(),
         aes(x=TIPO_TRANSPLANTE,y=RX_TOTAL ,fill=TIPO_TRANSPLANTE)) + 
    geom_boxplot()  +
    stat_summary(fun.data=iqr, mult=1, color="black") +
    guides(color = FALSE) +
    guides(fill = "none") +
    labs(x = "Tipo de Transplante", y = "Número Total de Exames" , title = "RX", fill = "") +
    guides(fill = "none") +
    theme_bw(base_size = 12) + 
    scale_fill_nejm() +
    scale_x_discrete() + # labels(c()) coloca labels aqui
    theme_classic() + theme(legend.position='none')
})


output$plot10 <- renderPlotly({
  ggplot(data = exame_data_pre(),
         aes(x=TIPO_TRANSPLANTE,y=ECO_TOTAL ,fill=TIPO_TRANSPLANTE)) + 
    geom_boxplot()  +
    stat_summary(fun.data=iqr, mult=1, color="black") +
    guides(color = FALSE) +
    guides(fill = "none") +
    labs(x = "Tipo de Transplante", y = "Número Total de Exames" , title = "Ecografias", fill = "") +
    guides(fill = "none") +
    theme_bw(base_size = 12) + 
    scale_fill_nejm() +
    scale_x_discrete() + # labels(c()) coloca labels aqui
    theme_classic() + theme(legend.position='none')
})


output$plot11 <- renderPlotly({
  ggplot(data = exame_data_pre(),
         aes(x=TIPO_TRANSPLANTE,y=TOMO_TOTAL ,fill=TIPO_TRANSPLANTE)) + 
    geom_boxplot()  +
    stat_summary(fun.data=iqr, mult=1, color="black") +
    guides(color = FALSE) +
    guides(fill = "none") +
    labs(x = "Tipo de Transplante", y = "Número Total de Exames" , title = "Tomografias", fill = "") +
    guides(fill = "none") +
    theme_bw(base_size = 12) + 
    scale_fill_nejm() +
    scale_x_discrete() + # labels(c()) coloca labels aqui
    theme_classic() + theme(legend.position='none')
})

output$plot12 <- renderPlotly({
  ggplot(data = exame_data_pre(),
         aes(x=TIPO_TRANSPLANTE,y=IMUNOSS_TOTAL ,fill=TIPO_TRANSPLANTE)) + 
    geom_boxplot()  +
    stat_summary(fun.data=iqr, mult=1, color="black") +
    guides(color = FALSE) +
    guides(fill = "none") +
    labs(x = "Tipo de Transplante", y = "Número Total de Exames" , title = "Tacrolimus/Siro/Evero/Cya", fill = "") +
    guides(fill = "none") +
    theme_bw(base_size = 12) + 
    scale_fill_nejm() +
    scale_x_discrete() + # labels(c()) coloca labels aqui
    theme_classic() + theme(legend.position='none')
})



# TX ----------------------------------------------------------------------

output$plot13 <- renderPlotly({
  ggplot(data = exame_data_tx(),
         aes(x=TIPO_TRANSPLANTE,y=RX_TOTAL ,fill=TIPO_TRANSPLANTE)) + 
    geom_boxplot()  +
    stat_summary(fun.data=iqr, mult=1, color="black") +
    guides(color = FALSE) +
    guides(fill = "none") +
    labs(x = "Tipo de Transplante", y = "Número Total de Exames" , title = "RX", fill = "") +
    guides(fill = "none") +
    theme_bw(base_size = 12) + 
    scale_fill_nejm() +
    scale_x_discrete() + # labels(c()) coloca labels aqui
    theme_classic() + theme(legend.position='none')
})


output$plot14 <- renderPlotly({
  ggplot(data = exame_data_tx(),
         aes(x=TIPO_TRANSPLANTE,y=ECO_TOTAL ,fill=TIPO_TRANSPLANTE)) + 
    geom_boxplot()  +
    stat_summary(fun.data=iqr, mult=1, color="black") +
    guides(color = FALSE) +
    guides(fill = "none") +
    labs(x = "Tipo de Transplante", y = "Número Total de Exames" , title = "Ecografias", fill = "") +
    guides(fill = "none") +
    theme_bw(base_size = 12) + 
    scale_fill_nejm() +
    scale_x_discrete() + # labels(c()) coloca labels aqui
    theme_classic() + theme(legend.position='none')
})


output$plot15 <- renderPlotly({
  ggplot(data = exame_data_tx(),
         aes(x=TIPO_TRANSPLANTE,y=TOMO_TOTAL ,fill=TIPO_TRANSPLANTE)) + 
    geom_boxplot()  +
    stat_summary(fun.data=iqr, mult=1, color="black") +
    guides(color = FALSE) +
    guides(fill = "none") +
    labs(x = "Tipo de Transplante", y = "Número Total de Exames" , title = "Tomografias", fill = "") +
    guides(fill = "none") +
    theme_bw(base_size = 12) + 
    scale_fill_nejm() +
    scale_x_discrete() + # labels(c()) coloca labels aqui
    theme_classic() + theme(legend.position='none')
})

output$plot16 <- renderPlotly({
  ggplot(data = exame_data_tx(),
         aes(x=TIPO_TRANSPLANTE,y=IMUNOSS_TOTAL ,fill=TIPO_TRANSPLANTE)) + 
    geom_boxplot()  +
    stat_summary(fun.data=iqr, mult=1, color="black") +
    guides(color = FALSE) +
    guides(fill = "none") +
    labs(x = "Tipo de Transplante", y = "Número Total de Exames" , title = "Tacrolimus/Siro/Evero/Cya", fill = "") +
    guides(fill = "none") +
    theme_bw(base_size = 12) + 
    scale_fill_nejm() +
    scale_x_discrete() + # labels(c()) coloca labels aqui
    theme_classic() + theme(legend.position='none')
})



# POS_TX PREC ----------------------------------------------------------------------

output$plot17 <- renderPlotly({
  ggplot(data = exame_data_pos_prec(),
         aes(x=TIPO_TRANSPLANTE,y=RX_TOTAL ,fill=TIPO_TRANSPLANTE)) + 
    geom_boxplot()  +
    stat_summary(fun.data=iqr, mult=1, color="black") +
    guides(color = FALSE) +
    guides(fill = "none") +
    labs(x = "Tipo de Transplante", y = "Número Total de Exames" , title = "RX", fill = "") +
    guides(fill = "none") +
    theme_bw(base_size = 12) + 
    scale_fill_nejm() +
    scale_x_discrete() + # labels(c()) coloca labels aqui
    theme_classic() + theme(legend.position='none')
})


output$plot18 <- renderPlotly({
  ggplot(data = exame_data_pos_prec(),
         aes(x=TIPO_TRANSPLANTE,y=ECO_TOTAL ,fill=TIPO_TRANSPLANTE)) + 
    geom_boxplot()  +
    stat_summary(fun.data=iqr, mult=1, color="black") +
    guides(color = FALSE) +
    guides(fill = "none") +
    labs(x = "Tipo de Transplante", y = "Número Total de Exames" , title = "Ecografias", fill = "") +
    guides(fill = "none") +
    theme_bw(base_size = 12) + 
    scale_fill_nejm() +
    scale_x_discrete() + # labels(c()) coloca labels aqui
    theme_classic() + theme(legend.position='none')
})


output$plot19 <- renderPlotly({
  ggplot(data = exame_data_pos_prec(),
         aes(x=TIPO_TRANSPLANTE,y=TOMO_TOTAL ,fill=TIPO_TRANSPLANTE)) + 
    geom_boxplot()  +
    stat_summary(fun.data=iqr, mult=1, color="black") +
    guides(color = FALSE) +
    guides(fill = "none") +
    labs(x = "Tipo de Transplante", y = "Número Total de Exames" , title = "Tomografias", fill = "") +
    guides(fill = "none") +
    theme_bw(base_size = 12) + 
    scale_fill_nejm() +
    scale_x_discrete() + # labels(c()) coloca labels aqui
    theme_classic() + theme(legend.position='none')
})

output$plot20 <- renderPlotly({
  ggplot(data = exame_data_pos_prec(),
         aes(x=TIPO_TRANSPLANTE,y=IMUNOSS_TOTAL ,fill=TIPO_TRANSPLANTE)) + 
    geom_boxplot()  +
    stat_summary(fun.data=iqr, mult=1, color="black") +
    guides(color = FALSE) +
    guides(fill = "none") +
    labs(x = "Tipo de Transplante", y = "Número Total de Exames" , title = "Tacrolimus/Siro/Evero/Cya", fill = "") +
    guides(fill = "none") +
    theme_bw(base_size = 12) + 
    scale_fill_nejm() +
    scale_x_discrete() + # labels(c()) coloca labels aqui
    theme_classic() + theme(legend.position='none')
})


# POS_TX TARD ----------------------------------------------------------------------

output$plot21 <- renderPlotly({
  ggplot(data = exame_data_pos_tard(),
         aes(x=TIPO_TRANSPLANTE,y=RX_TOTAL ,fill=TIPO_TRANSPLANTE)) + 
    geom_boxplot()  +
    stat_summary(fun.data=iqr, mult=1, color="black") +
    guides(color = FALSE) +
    guides(fill = "none") +
    labs(x = "Tipo de Transplante", y = "Número Total de Exames" , title = "RX", fill = "") +
    guides(fill = "none") +
    theme_bw(base_size = 12) + 
    scale_fill_nejm() +
    scale_x_discrete() + # labels(c()) coloca labels aqui
    theme_classic() + theme(legend.position='none')
})


output$plot22 <- renderPlotly({
  ggplot(data = exame_data_pos_tard(),
         aes(x=TIPO_TRANSPLANTE,y=ECO_TOTAL ,fill=TIPO_TRANSPLANTE)) + 
    geom_boxplot()  +
    stat_summary(fun.data=iqr, mult=1, color="black") +
    guides(color = FALSE) +
    guides(fill = "none") +
    labs(x = "Tipo de Transplante", y = "Número Total de Exames" , title = "Ecografias", fill = "") +
    guides(fill = "none") +
    theme_bw(base_size = 12) + 
    scale_fill_nejm() +
    scale_x_discrete() + # labels(c()) coloca labels aqui
    theme_classic() + theme(legend.position='none')
})


output$plot23 <- renderPlotly({
  ggplot(data = exame_data_pos_tard(),
         aes(x=TIPO_TRANSPLANTE,y=TOMO_TOTAL ,fill=TIPO_TRANSPLANTE)) + 
    geom_boxplot()  +
    stat_summary(fun.data=iqr, mult=1, color="black") +
    guides(color = FALSE) +
    guides(fill = "none") +
    labs(x = "Tipo de Transplante", y = "Número Total de Exames" , title = "Tomografias", fill = "") +
    guides(fill = "none") +
    theme_bw(base_size = 12) + 
    scale_fill_nejm() +
    scale_x_discrete() + # labels(c()) coloca labels aqui
    theme_classic() + theme(legend.position='none')
})

output$plot24 <- renderPlotly({
  ggplot(data = exame_data_pos_tard(),
         aes(x=TIPO_TRANSPLANTE,y=IMUNOSS_TOTAL,fill=TIPO_TRANSPLANTE)) + 
    geom_boxplot()  +
    stat_summary(fun.data=iqr, mult=1, color="black") +
    guides(color = FALSE) +
    guides(fill = "none") +
    labs(x = "Tipo de Transplante", y = "Número Total de Exames" , title = "Tacrolimus/Siro/Evero/Cya", fill = "") +
    guides(fill = "none") +
    theme_bw(base_size = 12) + 
    scale_fill_nejm() +
    scale_x_discrete() + # labels(c()) coloca labels aqui
    theme_classic() + theme(legend.position='none')
})


# DIY GRAPHS --------------------------------------------------------------



diy1 <- reactive({
  ggplot(data = exame_data_pre(),
         aes(x = TIPO_TRANSPLANTE, y = .data[[input$column1]], fill = TIPO_TRANSPLANTE)) + 
    geom_boxplot()  +
    stat_summary(fun.data = iqr, mult = 1, color = "black") +
    guides(color = FALSE) +
    labs(x = "Tipo de Transplante", y = "Número Total de Exames" , 
         title = paste0("N Exames"," ", input$column1), fill = "") +
    theme_bw(base_size = 12) + 
    scale_fill_nejm() +
    scale_x_discrete() + # labels(c()) coloca labels aqui
    theme_classic() + 
    theme(legend.position = 'none')
})


diy2 <- reactive({
  ggplot(data = exame_data_pre(),
         aes(x = TIPO_TRANSPLANTE, y = .data[[input$column2]], fill = TIPO_TRANSPLANTE)) + 
    geom_boxplot()  +
    stat_summary(fun.data = iqr, mult = 1, color = "black") +
    guides(color = FALSE) +
    labs(x = "Tipo de Transplante", y = "Número Total de Exames" , 
         title = paste0("N Exames"," ", input$column2), fill = "") +
    theme_bw(base_size = 12) + 
    scale_fill_nejm() +
    scale_x_discrete() + # labels(c()) coloca labels aqui
    theme_classic() + 
    theme(legend.position = 'none')
})


diy3 <- reactive({
  ggplot(data = exame_data_tx(),
         aes(x = TIPO_TRANSPLANTE, y = .data[[input$column3]], fill = TIPO_TRANSPLANTE)) + 
    geom_boxplot()  +
    stat_summary(fun.data = iqr, mult = 1, color = "black") +
    guides(color = FALSE) +
    labs(x = "Tipo de Transplante", y = "Número Total de Exames" , 
         title = paste0("N Exames"," ", input$column3), fill = "") +
    theme_bw(base_size = 12) + 
    scale_fill_nejm() +
    scale_x_discrete() + # labels(c()) coloca labels aqui
    theme_classic() + 
    theme(legend.position = 'none')
})



diy4 <- reactive({
  ggplot(data = exame_data_tx(),
         aes(x = TIPO_TRANSPLANTE, y = .data[[input$column4]], fill = TIPO_TRANSPLANTE)) + 
    geom_boxplot()  +
    stat_summary(fun.data = iqr, mult = 1, color = "black") +
    guides(color = FALSE) +
    labs(x = "Tipo de Transplante", y = "Número Total de Exames" , 
         title = paste0("N Exames"," ", input$colum4), fill = "") +
    theme_bw(base_size = 12) + 
    scale_fill_nejm() +
    scale_x_discrete() + # labels(c()) coloca labels aqui
    theme_classic() + 
    theme(legend.position = 'none')
})


diy5 <- reactive({
  ggplot(data = exame_data_pos_prec(),
         aes(x = TIPO_TRANSPLANTE, y = .data[[input$column5]], fill = TIPO_TRANSPLANTE)) + 
    geom_boxplot()  +
    stat_summary(fun.data = iqr, mult = 1, color = "black") +
    guides(color = FALSE) +
    labs(x = "Tipo de Transplante", y = "Número Total de Exames" , 
         title = paste0("N Exames"," ", input$column5), fill = "") +
    theme_bw(base_size = 12) + 
    scale_fill_nejm() +
    scale_x_discrete() + # labels(c()) coloca labels aqui
    theme_classic() + 
    theme(legend.position = 'none')
})

diy6 <- reactive({
  ggplot(data = exame_data_pos_prec(),
         aes(x = TIPO_TRANSPLANTE, y = .data[[input$column6]], fill = TIPO_TRANSPLANTE)) + 
    geom_boxplot()  +
    stat_summary(fun.data = iqr, mult = 1, color = "black") +
    guides(color = FALSE) +
    labs(x = "Tipo de Transplante", y = "Número Total de Exames" , 
         title = paste0("N Exames"," ", input$column6), fill = "") +
    theme_bw(base_size = 12) + 
    scale_fill_nejm() +
    scale_x_discrete() + # labels(c()) coloca labels aqui
    theme_classic() + 
    theme(legend.position = 'none')
})

diy7 <- reactive({
  ggplot(data = exame_data_pos_tard(),
         aes(x = TIPO_TRANSPLANTE, y = .data[[input$column7]], fill = TIPO_TRANSPLANTE)) + 
    geom_boxplot()  +
    stat_summary(fun.data = iqr, mult = 1, color = "black") +
    guides(color = FALSE) +
    labs(x = "Tipo de Transplante", y = "Número Total de Exames" , 
         title = paste0("N Exames"," ", input$column7), fill = "") +
    theme_bw(base_size = 12) + 
    scale_fill_nejm() +
    scale_x_discrete() + # labels(c()) coloca labels aqui
    theme_classic() + 
    theme(legend.position = 'none')
})

 
diy8 <- reactive({
  ggplot(data = exame_data_pos_tard(),
         aes(x = TIPO_TRANSPLANTE, y = .data[[input$column8]], fill = TIPO_TRANSPLANTE)) + 
    geom_boxplot()  +
    stat_summary(fun.data = iqr, mult = 1, color = "black") +
    guides(color = FALSE) +
    labs(x = "Tipo de Transplante", y = "Número Total de Exames" , 
         title = paste0("N Exames"," ", input$column8), fill = "") +
    theme_bw(base_size = 12) + 
    scale_fill_nejm() +
    scale_x_discrete() + # labels(c()) coloca labels aqui
    theme_classic() + 
    theme(legend.position = 'none')
})


# Render the plot using plotly
output$diy1_plot <- renderPlotly({
  ggplotly(diy1())
})

output$diy2_plot <- renderPlotly({
  ggplotly(diy2())
})

output$diy3_plot <- renderPlotly({
  ggplotly(diy3())
})

output$diy4_plot <- renderPlotly({
  ggplotly(diy4())
})

output$diy5_plot <- renderPlotly({
  ggplotly(diy5())
})

output$diy6_plot <- renderPlotly({
  ggplotly(diy6())
})

output$diy7_plot <- renderPlotly({
  ggplotly(diy7())
})

output$diy8_plot <- renderPlotly({
  ggplotly(diy8())
})

# TEXTOS ------------------------------------------------------------------

output$output1 <- renderPrint({
  "This is output 1"
})



# TABELA ADESAO -----------------------------------------------------------

output$adesao_tacro <- DT::renderDataTable({
  
  data_tacro %>% dplyr::select(-c(PRONTUARIO)) %>% head(10)
  
})

output$sum_adesao_tacro <- render_gt({
  
  data_tacro %>%
    dplyr::select(contains("ADESAO_")) %>%
    tbl_summary() %>%
    bold_labels() %>% as_gt()
})


output$plot_adesao <- renderPlotly({
  
         data_tacro_sum %>%
           ggplot(aes(x = MES_POS_TX, y = N_EXAMES)) +
           geom_path(aes(x = MES_POS_TX, y = N_EXAMES, group = as.character(PRONTUARIO), color = as.character(PRONTUARIO)),alpha=0.3) +
           geom_dotplot(binaxis='y', stackdir='center', dotsize=0.5,aes(fill=as.character(PRONTUARIO)),alpha=0.3) +
           stat_summary(fun.data=iqr, mult=1, 
                        geom="pointrange", color="black", size = 0.5, width = 0.5) +
           geom_line(aes(group = 1), color = "black", alpha = 0.5, size = 0.7) +
           guides(fill = FALSE) +
           labs(x = "Timepoint", y = "N Exames", title = "", fill = "") +
           scale_color_nejm() +
           scale_fill_nejm() +
           theme_classic(base_size = 12) + 
           theme(legend.position = 'none')
})


}
