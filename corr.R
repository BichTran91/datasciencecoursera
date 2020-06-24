corr<- function(directory, threshold = 0) 
        {
        files<- list.files(directory, full.names = TRUE)
        outcome<- data.frame()
        for (i in 1:length(files)) 
                {
                read<- read.csv(files[i])
                sum<- sum((!is.na(read$sulfate)) & (!is.na(read$nitrate)))
                if(sum > threshold) 
                        {
                        sul<- read[which(!is.na(read$sulfate)), ]
                        nit<- read[which(!is.na(read$nitrate)), ]
                        outcome<- c(outcome, cor(nit$sulfate, nit$nitrate))
                        }
        }
        outcome
}