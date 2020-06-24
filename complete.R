complete<- function(directory, id = 1:332) {
        files<- list.files(directory, full.names = TRUE)
        outcome<- data.frame()
        for (i in id) {
                read<- read.csv(files[i])
                nobs<- sum(complete.cases(read))
                outcome<- rbind(outcome, data.frame(i, nobs))
        }
        colnames(outcome)<- c("id", "nobs")
        return(outcome)
}

complete("specdata", 1:10)


