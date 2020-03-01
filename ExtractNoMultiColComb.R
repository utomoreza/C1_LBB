# to extract each no-multicol of each combination

indices <- vector()
temp <- multicolRes[multicolRes$res == "No",]
lastEnergy <- str_match(string = temp[nrow(temp),]$vs, pattern = "^[a-z]{5}y(.{1,2}) \\&")
lastEnergy <- lastEnergy[1,2]
rownames(temp) <- NULL
for (i in 1:lastEnergy) {
     idx <- grep(pattern = paste0("energy", i, " "), x = temp$vs)[1]
     indices <- c(indices, idx)
}
indices <- indices[!is.na(indices)]
temp[indices,]