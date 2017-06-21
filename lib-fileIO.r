
# Luke Zhou. June 2017.
# File I/O functions for COG/RMS data analysis.


def.constants = function(){
  
  # Graph output
  out.width <<- 840
  out.height <<- 560
  out.height.small <<- 480
  out.res <<- 144
  
  # For renaming labels
  OLD.LABELS <<- c("s","S","x")
  SH <<- "ʃ"
  C.FRIC <<- "ɕ"
  NEW.LABELS <<- c("s", SH, C.FRIC)
  
  FRICATIVES <<- c("s", SH)
  SPEAKERS <<- c("BM1","TP2","TP3","TP4","TP5","TP6","TP7","TP8","TP9")
  TP.SPEAKERS <<- SPEAKERS[grepl("TP",SPEAKERS)]
  
  cog.data.filename <<- "cog-data.txt"
  rms.data.filename <<- "rms-data.txt"
  soc.data.filename <<- "social-data.txt"
  
  rms.data <<- read.rms.data()
  soc.data <<- read.soc.data()
  cog.data <<- with.bm.reorder(read.cog.data())
  
  cog.bm <<- cog.data[cog.data$Spk=="BM1",]
  cog.tm <<- no.bm.reorder(merge(cog.data[cog.data$Spk!="BM1",],soc.data))
  write.table(cog.tm,file="soc-fric-data.txt",sep="\t",row.names=FALSE,quote=FALSE)
  
  POS.SPK <<- as.vector(soc.data[soc.data$Cond=="pos" & soc.data$Spk %in% TP.SPEAKERS, "Spk"])
  NEG.SPK <<- as.vector(soc.data[soc.data$Cond=="neg" & soc.data$Spk %in% TP.SPEAKERS, "Spk"])
  
  colour.palette <<- "Dark2"
}


read.cog.data = function(){ 
  
  orig.data = read.table(cog.data.filename, sep="\t", header=TRUE, strip.white=TRUE)
  
  # Drops start & end times from Praat
  orig.data = orig.data[,!(names(orig.data) %in% c("Start","End"))]
  
  # Revaluing & separation of long description strings
  orig.data$Label = mapvalues(orig.data$Label, from = OLD.LABELS, to = NEW.LABELS)
  orig.data$Spk = substr(orig.data$FileName, 1, regexpr('-', orig.data$FileName)-1)
  orig.data$SpkTask = substr(orig.data$FileName, 1, regexpr('-', orig.data$FileName)+4)
  orig.data$SpkTask = mapvalues(orig.data$SpkTask, from = c("BM1-fals"), to = c("BM1-good"))
  orig.data$Task = substr(orig.data$SpkTask, regexpr('-', orig.data$SpkTask)+1,nchar(orig.data$SpkTask))
  
  # Let's use fricative data only
  fric.data = orig.data[orig.data$Label %in% FRICATIVES,]
  fric.data$Task = mapvalues(fric.data$Task, from = c("good","base","imit"), to = c("model","baseline","shadowing"))
  fric.data$Task = factor(fric.data$Task, level=c("model","shadowing","baseline"))
  
  # Filtering by speakers
  filt.data = fric.data[fric.data$Spk %in% SPEAKERS,]
  filt.data$Spk = factor(filt.data$Spk)
  filt.data$Task = factor(filt.data$Task)
  filt.data$Label = factor(filt.data$Label)
  
  return(filt.data)
}

read.rms.data = function(){
  
  orig.data = read.table(rms.data.filename, sep="\t", header=TRUE)
  
  filt.by.spk = orig.data[orig.data$Spk %in% SPEAKERS,]
  filt.by.spk$s.sh.tasks = filt.by.spk$s.sh.imit - filt.by.spk$s.sh.base
  filt.by.spk$k.t.tasks = filt.by.spk$k.t.imit - filt.by.spk$k.t.base
  filt.by.spk$chg.quotient = abs(filt.by.spk$s.sh.tasks / filt.by.spk$k.t.tasks)
  
  filt.by.spk$Cond <- factor(filt.by.spk$Cond, levels = c("pos","neg"))
  
  NUM.SPK = length(SPEAKERS)
  
  concat.data = melt(filt.by.spk, 
                     id.vars=c("Spk","Cond","Sex","rot.tasks","IAT.id","IAT.score"),
                     variable.name="Type", value.name="y")
  
  return(concat.data)
}

read.soc.data = function(){
  
  soc.data = read.table(soc.data.filename, sep="\t", header=TRUE, strip.white=TRUE)
  
  # Inner join: fricative data table w/ social data table
  names(soc.data) <- c("Spk","IAT.score","Sex","Cond","IAT.order")
  
  soc.data$Cond = mapvalues(soc.data$Cond, from = c("+","-"), to = c("pos","neg"))
  
  return(soc.data)
}



no.bm.reorder = function(df){
  df$Spk = factor(df$Spk)
  df$Cond = factor(df$Cond)
  df$Task = factor(df$Task)
  df$Label = factor(df$Label)
  
  df$Cond = factor(df$Cond,levels=c("pos","neg"))
  df$Task = factor(df$Task,levels=c("baseline","shadowing"))
  return(df)
}

with.bm.reorder = function(df){
  df$Spk = factor(df$Spk)
  df$Task = factor(df$Task)
  df$Label = factor(df$Label)
  
  df$Task = factor(df$Task,levels=c("baseline","shadowing","model"))
  return(df)
}

tmp.rev = function(factor){
  return(rev(levels(factor)))
}
