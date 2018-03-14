library(xlsx)
randomization <- data.frame()
set.seed(001)

for(i in 1:131){
    tests <- rep(1:5)
    tests <- sample(tests)
    randomization[i, "Teste 1"] <- tests[1]
    randomization[i, "Teste 2"] <- tests[2]
    randomization[i, "Teste 3"] <- tests[3]
    randomization[i, "Teste 4"] <- tests[4]
    randomization[i, "Teste 5"] <- tests[5]
}
randomization <- lapply(randomization, gsub, pattern = 1, replacement = "BPAQ", fixed = T)
randomization <- lapply(randomization, gsub, pattern = 2, replacement = "ASSIST", fixed = T)
randomization <- lapply(randomization, gsub, pattern = 3, replacement = "SDS", fixed = T)
randomization <- lapply(randomization, gsub, pattern = 4, replacement = "UPPS", fixed = T)
randomization <- lapply(randomization, gsub, pattern = 5, replacement = "FDT", fixed = T)
write.xlsx(randomization, "Alan 2018/Randomização.xlsx")
rm(tests, i, randomization)