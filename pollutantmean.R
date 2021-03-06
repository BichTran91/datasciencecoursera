pollutantmean<- function(directory, pollutant, id = 1:332) {
        files<- list.files(directory, full.names = TRUE)
        data<- data.frame()
        for (i in id) {
                data<- rbind(data, read.csv(files[i]))
        }
        outcome<- mean(data[, pollutant], na.rm = TRUE)
        return(outcome)
}

pollutantmean("specdata", "sulfate", 1:332)

pollutantmean("specdata", "sulfate", 1:10)

pollutantmean("specdata", "nitrate", 70:72)