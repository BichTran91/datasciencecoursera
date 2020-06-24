corr<- function(directory, threshold = 0)
{
        files<- list.files(directory, full.names = TRUE)
        outcome<- vector(mode = "numeric", length = 0)
        for (i in 1:length(files)) {
                read<- read.csv(files[i])
                sum<- sum((!is.na(read$sulfate)) & (!is.na(read$nitrate)))
                if(sum > threshold) {
                        sul<- read[which(!is.na(read$sulfate)), ]
                        nit<- sul[which(!is.na(sul$nitrate)), ]
                        outcome<- c(outcome, cor(nit$sulfate, nit$nitrate))
                }
        }
        outcome
}

cr<- corr("specdata")
head(cr)
summary(cr)

cr<- corr("specdata", 150)
head(cr)
summary(cr)

cr<- corr("specdata", 400)
head(cr)
summary(cr)

cr<- corr("specdata", 5000)
head(cr)
summary(cr)