shinyServer(function(input, output) {
  
  getCode <- reactive({
    parse(paste0('pollutantmean variants pretty','/',input$script,'.R'))
  })

  output$postlink <- renderUI(a("Ссылка на форум",href=paste0(url.base,input$script)))
  output$calltext <- renderPrint({
    sprintf("pollutantmean('specdata', '%s', %s)"
      , switch(input$pollutant,'1' = 'nitrate','2' = 'sulfate')
      , ifelse(input$allmons,'1:332',sprintf("c(%s)", Reduce(function(x,y) sprintf('%s, %s',x,y),input$monitors))))
  })
  
  output$code <- renderPrint({
    expr <- getCode()
    expr[1][[1]]
  })

  output$statistic <- renderTable({
    input$goButton

    isolate({
      expr = getCode()
      eval(expr)
      results <- numeric(input$loopnum[1])
      arguments <- list( directory = 'specdata'
                       , pollutant = switch(input$pollutant,'1' = 'nitrate','2' = 'sulfate')
                       , id = if(input$allmons) monitors.list else as.integer(input$monitors)
                       )
      ss = sapply(1:(input$loopnum[1])
              ,function(x){system.time({results[x] <<- do.call('pollutantmean',arguments)})[c(1,3)]})
      
      #ss = rbind(ss,result = results)
      ss = data.frame(t(ss)[,],result = as.numeric(results))
      
      rbind(ss,sapply(ss[,],summary))

    })
  })
})

