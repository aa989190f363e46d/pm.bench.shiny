shinyServer(function(input, output) {
  
  getCode <- reactive({
    parse(paste0('pollutantmean variants pretty','/',input$script,'.R'))
  })

  output$postlink <- renderUI(a("Ссылка на форум",href=paste0(url.base,input$script)))
  output$calltext <- renderPrint({
    sprintf("pollutantmean('specdata', '%s', %s)"
      , switch(input$pollutant,'1' = 'nitrate','2' = 'sulfate')
      , ifelse(input$allmons,'1:332',sprintf("c(%s)", Reduce(function(x,y) sprintf('%s, %s',x,y),as.numeric(input$monitors)))))
  })
  
  output$code <- renderPrint({
    expr <- getCode()
    expr[1][[1]]
  })

  output$statistic <- renderTable({
    input$goButton

    isolate({
      expr <- getCode()
      eval(expr)
      # ss <- sapply(1:(input$loopnum[1]*10)
      #         ,function(x){system.time(do.call('pollutantmean',list( 'specdata'
      #                                                              , switch(input$pollutant,'1' = 'nitrate','2' = 'sulfate')
      #                                                              , ifelse(input$allmons,1:332,as.numeric(input$monitors)))))[c(1,3)]})
      
      ss <- data.frame()
      for(i in 1:(input$loopnum[1]*10)){
        ss <- rbind(ss
                   ,system.time(pollutantmean( 'specdata'
                                              , switch(input$pollutant,'1' = 'nitrate','2' = 'sulfate')
                                              , ifelse(input$allmons,1:332,as.numeric(input$monitors))))[c(1,3)])
      }

      rm(pollutantmean)
      rm(expr)
      
      names(ss) <- c('user self','elapsed')
      #ss <- as.data.frame(t(ss))
      rbind(ss,sapply(ss[,],summary))

    })
  })
  # output$statistic <- renderPrint({
  #   input$goButton

  #   isolate({
  #     expr <- getCode()
  #     eval(expr)
  #     ss <- sapply(1:input$loopnum[1]
  #             ,function(x){system.time(do.call('pollutantmean',list( 'specdata'
  #                                                                  , switch(input$pollutant,'1' = 'nitrate','2' = 'sulfate')
  #                                                                  , ifelse(input$allmons,1:322,as.numeric(input$monitors)))))})

  #     ss
  #   })
  # })
})

