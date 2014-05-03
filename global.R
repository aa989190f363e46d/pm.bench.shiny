
code.root <- 'pollutantmean variants pretty'
variants.list <- sub('.R','',list.files(code.root, pattern = '*.R'))
url.base <- 'https://class.coursera.org/rprog-002/forum/thread?thread_id=1173#'
monitors.list <- 1:332
names(monitors.list) <- sprintf('%0.3d',monitors.list)
