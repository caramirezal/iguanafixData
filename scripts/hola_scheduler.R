
outBefore <- paste(date()," Before loading library\n")
write.csv(outBefore,'~/data/scheduler_output.csv')

library('xlsx')

outAfter <- paste(date()," After loading library\n")
write.csv(outAfter,'~/data/scheduler_output.csv')