## INTRODUCTION

packages <- c("ggplot2", "gridExtra", "RColorBrewer", "dplyr", "reshape2", "caret")
sapply(packages, require, character.only = TRUE, quietly = TRUE)

## Path fixé
path <- getwd()


### Lecture et visualisation de la donnée
data <- read.csv(file.path(path, "data.csv"), na.strings = c("NA", "#DIV/0!", ""))
head(data)

## PARTIE I : Statistiques descriptives

## Statistiques basiques
str(data)
summary(data)

### Conversion des classes
data$datetime <- as.POSIXct(strptime(data$datetime, format = "%Y-%m-%d %H:%M:%S"))
data$season <- factor(data$season)
data$holiday <- factor(data$holiday)
data$workingday <- factor(data$workingday)
data$weather <- factor(data$weather)

## Décomposition de la variable temporelle
data$hour <- as.factor(format(data$datetime, "%H:%M"))
data$day <- as.factor(format(data$datetime, "%d-%m-%Y"))
data$weekday <- as.factor(format(data$datetime, "%u"))
data$month <- as.factor(format(data$datetime, "%Y-%m"))
data$year <- as.factor(format(data$datetime, "%Y"))

## Dataframes par échelle de temps
perHour <- data %>% group_by(hour) %>% summarize(totalCount = sum(count), meanCount = mean(count))
perDay <- data %>% group_by(weekday) %>% summarize(totalCount = sum(count), meanCount = mean(count))
perMonth <- data %>% group_by(month) %>% summarize(totalCount = sum(count), meanCount = mean(count))

## Plot : Moyenne de locations par mois sur deux ans
ggplot(perMonth, aes(x = month)) + 
    geom_bar(aes(y = meanCount), stat = "Identity", fill = "#f6b058") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(title = "Locations de vélos par mois entre 2011 et 2012",
        x = "Mois",
        y = "Nombre moyen de locations")

## Plot : Moyenne de locations sur un jour et sur une semaine
hourPlot <- ggplot(perHour, aes(x = hour)) + 
    geom_bar(aes(y = meanCount), stat = "Identity", fill = "#f6b058") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(title = "Locations de vélos par heure",
         x = "Heure de la journée",
         y = "Nombre moyen de locations")
dayPlot <- ggplot(perDay, aes(x = weekday)) + 
    geom_bar(aes(y = meanCount), stat = "Identity", fill = "#c34242") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(title = "Locations de vélos par semaine",
         x = "Jour de la semaine",
         y = "Nombre moyen de locations")

grid.arrange(hourPlot, dayPlot, ncol = 1)

## Dataframe pour étude hebdomadaire
weekPattern <- data %>% group_by(hour, weekday, workingday) %>% summarize(meanCount = mean(count))
levels(weekPattern$workingday) = c("Lundi", "Mardi", "Mercredi", "Jeudi", "Vendredi", "Samedi", "Dimanche")

## Plot : Moyenne de locations sur une semaine par type de journée de travail
ggplot(weekPattern, aes(x = hour, fill = workingday)) + 
    geom_bar(aes(y = meanCount), stat = "Identity") +
    facet_grid(weekday ~ .) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_fill_discrete(name = "Jour travaillé",
        labels = c("Non", "Oui")) +
    labs(title = "Locations de vélos sur une semaine",
        x = "Heure de la journée",
        y = "Nombre moyen de locations")

## Dataframe pour étude des vacances scolaires
holiday <- data %>% filter(holiday == 1) %>% group_by(hour) %>% summarize(meanCount = mean(count))
nonWorkingday <- data %>% filter(workingday == 0) %>% group_by(hour) %>% summarize(meanCount = mean(count))

## Plot : Moyenne de locations sur un jour férié ou non travaillé
holidayPlot <- ggplot(holiday, aes(x = hour)) + 
    geom_bar(aes(y = meanCount), stat = "Identity", fill = "#f6b058") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(title = "Locations de vélos les jours fériés",
        x = "Heure de la journée",
        y = "Nombre moyen de locations")
nonWDPlot <- ggplot(nonWorkingday, aes(x = hour)) + 
    geom_bar(aes(y = meanCount), stat = "Identity", fill = "#c34242") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(title = "Locations de vélos les jours non travaillés",
        x = "Heure de la journée",
        y = "Nombre moyen de locations")

grid.arrange(holidayPlot, nonWDPlot, ncol = 1)

## Dataframe pour étude de la météorologie
weather <- data %>% group_by(temp, weather, season) %>% summarize(meanCount = mean(count))
levels(weather$season) = c("Printemps", "Été", "Automne", "Hiver")

## Plot : Moyenne de températures sur un jour férié ou non travaillé
ggplot(weather, aes(x = temp, fill = weather)) + 
    geom_bar(aes(y = meanCount), stat = "Identity") +
    scale_fill_brewer(palette = "RdYlBu") +
    facet_grid(season ~ .) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(title = "Locations de vélos selon la température, la météorologie et la saison",
        x = "Température",
        y = "Nombre moyen de locations")

## Résumé et corrélation entre température et température ressentie
summary(data[, 6:7])
cor(data$temp, data$atemp)

## Plot : Comparaison de densité de température et température ressentie
tempColPlot <- qplot(data$temp, data$atemp)
tempPlot <- ggplot(data = data) +
    geom_density(aes(temp), fill = "steelblue", alpha = .5) +
    geom_density(aes(atemp), fill = "coral", alpha = .5)

grid.arrange(tempColPlot, tempPlot, ncol = 2)

## Dataframe pour étude de la différence température/température ressentie
data %>% 
    select(day, temp, atemp, humidity, windspeed, count) %>% 
    filter(temp > 25, atemp < 15) %>%
    group_by(day) %>%
    summarize(temp = mean(temp), 
              atemp = mean(atemp), 
              humidity = mean(humidity), 
              windspeed = mean(windspeed), 
              count = mean(count))

## Résumé des variables humidité et vitesse du vent
summary(data[, 8:9])

## Corrélation entre les locations et l'humidité ou la vitesse du vent
cor(data$count, data$humidity)
cor(data$count, data$windspeed)

## Dataframe pour étude de l'humidité en fonction du temps
humidityWeather <- data %>% group_by(weather) %>% summarize(meanHumidity = mean(humidity), totalCount = sum(count))
humidityWeather


## PARTIE II : Machine learning

## Sélection de variables
newData <- data %>% select(season, workingday, temp, humidity, hour, weekday, year, count)

## Factorisation de la variable horaire
newData$hour <- factor(as.integer(newData$hour))

## Paramètres de cross-validation
    ## Choix de la "référence aléatoire" 
    set.seed(2016)

    ## Décomposition du dataframe de variables sélectionnées en test
    inTrain <- createDataPartition(y = newData$count, p = .8, list = FALSE)

    forTrain <- newData[inTrain, ]

    test <- newData[-inTrain, -8]
        ## Dummisation de test
        testMatrix = model.matrix( ~ . -1, test)
    testLabel <- newData[-inTrain, 8]

    ## Sous-décomposition pour train et validation
    outValid <- createDataPartition(y = forTrain$count, p = .8, list = FALSE)

    train <- forTrain[outValid, -8]
        ## Dummisation de train
        trainMatrix = model.matrix( ~ . -1, train)
    trainLabel <- forTrain[outValid, 8]

    valid <- forTrain[-outValid, -8]
        ## Dummisation de validation
        validMatrix = model.matrix( ~ . -1, valid)
    validLabel <- forTrain[-outValid, 8]

    
## Lecture des dimensions de chaque set
dim(train); dim(valid); dim(test)

## Paramétrage de la cross-validation automatique 
tControl <- trainControl(method = "cv", number = 2)

## Paramétrage du modèle Random Forest
rfGrid <- expand.grid(mtry = 12)
set.seed(2016)

## Entraînement du modèle
rfModel <- train(x = trainMatrix,
                 y = trainLabel,
                 method = "rf",
                 ntree = 500,
                 trControl = tControl,
                 tuneGrid = rfGrid,
                 importance = TRUE)
    ## RMSE : 66.8319
    ## NRMSE : 0.0707

rfModel$finalModel

## Prédiction sur validation
validPred <- as.integer(predict.train(classTree, newdata = validMatrix, na.action = na.pass))
validRMSE <- sqrt(sum((validPred - validLabel)^2)/length(validLabel))
validRMSE
    ## NRMSE <- validRMSE/(max(validLabel)-min(validLabel))
    ## NRMSE : 0.05900

## Plot : Importance des variables
rfModelVarImp <- varImp(rfModel)
dotPlot(rfModelVarImp)

## Prédiction sur test
testPred <- predict.train(rfModel, newdata = testMatrix)
testRMSE <- sqrt(sum((testPred-testLabel)^2 )/length(testLabel))
testRMSE

