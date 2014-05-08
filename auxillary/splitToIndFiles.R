fl.name             <- "complete.variants.R"
output.dir.name     <- "complete variants pretty"

comment.pattern     <- "# http.+(post|comment)-[[:digit:]]+"
bookmark.pattern    <- "(post|comment)-[[:digit:]]+"

fl                  <- file(fl.name)
fl.lines            <- readLines(fl)

begin.marks         <- append(grep(comment.pattern,fl.lines)
                             ,length(fl.lines))

save.code <- function(begin,end){
  
  new.file.name     <- regmatches(fl.lines[begin]
                        ,regexpr(bookmark.pattern
                          ,fl.lines[begin]))[1]
  
  print(sprintf("%i:%i %s",begin,end,new.file.name))

  code <- parse(text=fl.lines[begin:end])

  nfl <- file(paste0(output.dir.name,'/',new.file.name,'.R'),'w')
  writeLines(deparse(code[[1]]),nfl)
  flush(nfl)
  close(nfl)
}

mapply(save.code,begin.marks[-length(begin.marks)],begin.marks[-1])