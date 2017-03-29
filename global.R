a <- 3
b <- 4
numbins <- 15
data <- read.csv('./www/data.csv')

feature2exclude <- 'date'
features <- names(data)
features2include <- features[!(features %in% feature2exclude)]

lookupData <- read.csv('./www/variable_lookup.csv')