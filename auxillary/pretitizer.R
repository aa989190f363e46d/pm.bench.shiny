variants.root <- 'pollutantmean variants'
variants.src <- dir(variants.root)

for(variant in variants.src){

  print(variant)
  fl <- file(variant,'awr')
  write(file=fl,deparse(parse(paste(variants.root,variant,sep='/'))[1][[1]]))
  flush(fl)
  close(fl)
  
}