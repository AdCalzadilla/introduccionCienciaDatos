# Clasificación
require(caret)
# Abrimos el fichero .dat
wbcd <- read.csv2("./wisconsin/wisconsin.dat", header = F, sep = ",",
                  comment.char = "@", stringsAsFactors = FALSE)
names(wbcd) <- c("ClumpThickness", "CellSize", "CellShape", "MarginalAdhesion", "EpithelialSize",
                 "BareNuclei", "BlandChromatin", "NormalNucleoli", "Mitoses", "Class")
## Características generales de wizmir
attach(wbcd)
is.data.frame(wbcd)
dim(wbcd)
str(wbcd)

### Cambio de tipo de la variable 'BareNuclei' a integer
wbcd$BareNuclei <- as.integer(BareNuclei)

### Existen valores "NA"
table(is.na(wbcd))
colSums(is.na(wbcd))

wbcd <- na.omit(wbcd)
nrow(wbcd)

summary(wbcd)

## Desviación estandar para cada una de las variables
allSd <- apply(wbcd, 2, sd)
## Mediana de cada una de las variables
allMedian <- apply(wbcd, 2, median)
## Rango intercuartílico
allIQR <- apply(wbcd, 2, IQR)

# Gráfica de todos con todos
plot(wbcd,  col=wbcd[,10])

# Convertimos la columna de la clase en un factor y le damos los valores de Benigno y Maligno
wbcd$Class <- factor(wbcd$Class, levels = c(2, 4), labels = c("Benign", "Malignant"))

# Vemos la tabla de diagnosis.
table(wbcd$Class)

# Porcentaje de valores Benigno y Maligno
tbOrig <- round(prop.table(table(wbcd$Class)) * 100, digits = 1)
tbOrig

# Modelando
# 80% train un 20% test
wbcd_train <- wbcd[1:546, ]
wbcd_test <- wbcd[547:683, ]

# knnFit1
knnFit1 <- train(Class ~ ., data=wbcd_train, method="knn",metric="Accuracy", tuneLength=20,
                 preProc = c("center","scale"))
knnFit1
# plot de knnFit1
plot(knnFit1)
# El predict
knnPredict1 <- predict(knnFit1, newdata=wbcd_test)
cmat1 <- confusionMatrix(knnPredict1, wbcd_test$Class, positive="Malignant")
cmat1

require(MASS)
trainData <- wbcd[,1:9]
trainClasses <- wbcd[,10]
ldaFit <- train(trainData, trainClasses,
                method = "lda",
                preProcess = c("center", "scale"),
                tuneLength = 10,
                trControl = trainControl(method = "cv"))
cmat2 <- confusionMatrix(ldaFit)
cmat2

qdaFit <- train(trainData, trainClasses,
                method = "qda",
                preProcess = c("center", "scale"),
                tuneLength = 10,
                trControl = trainControl(method = "cv"))
cmat3 <- confusionMatrix(qdaFit)
cmat3

#leemos la tabla con los datos de "Accuracy" (código sacado de los apuntes)
resultados <- read.csv("./wisconsin/clasif_test_alumos.csv")
tablatstClas <- cbind(resultados[,2:dim(resultados)[2]])
colnames(tablatstClas) <- names(resultados)[2:dim(resultados)[2]]
rownames(tablatstClas) <- resultados[,1]

tablatstClas["wisconsin", "out_test_knn"] <- 0.9781022
tablatstClas["wisconsin", "out_test_lda"] <- 0.9605
tablatstClas["wisconsin", "out_test_qda"] <- 0.9502

test_friedman <- friedman.test(as.matrix(tablatstClas))
test_friedman











