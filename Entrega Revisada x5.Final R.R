# Nombre: PabloCuestaMenéndez_Trabajo2.R

# Cargar los datos desde el archivo de texto
datos <- read.table("/Users/pablocuesta/Desktop/R/datos-trabajoR.txt", header = TRUE, sep = "\t")

# Pregunta 1. cargar los datos y asignarles una funcion usamos las funciones que tenemoms abajo 

cat("Visualización inicial de los datos:\n")
head(datos)
summary(datos)
dim(datos)
str(datos)
cat("Dimensiones del conjunto de datos:", dim(datos), "\n")
cat("Número de variables:", ncol(datos), "\n")
cat("Número de tratamientos únicos:", length(unique(datos$Tratamiento)), "\n")

# Pregunta 2. Crear boxplots para cada condicion (3) y guardarlos para las proximas graficas, elegimos un color para cada variable que guardamos
boxplot(Wildtype ~ Tratamiento, data = datos, col = "purple", main = "Wildtype")
boxplot(Sequia ~ Tratamiento, data = datos, col = "brown", main = "Sequía")
boxplot(ExcesoRiego ~ Tratamiento, data = datos, col = "gold", main = "Exceso de Riego")

# Pregunta 3. Gráficos de dispersión para comparar datos, 2.
# 3.1 Compara Sequia vs Wildtype
plot(datos$Wildtype, datos$Sequia, col = datos$Tratamiento, main = "Sequía vs Wildtype", xlab = "Wildtype", ylab = "Sequía")
# 3.2 Compara Exceso Riego vs Wildtype 
plot(datos$Wildtype, datos$ExcesoRiego, col = datos$Tratamiento, main = "Exceso de Riego vs Wildtype", xlab = "Wildtype", ylab = "Exceso de Riego")

# Pregunta 4. Agregar leyenda a los gráficos
legend("bottomright", legend = unique(datos$Tratamiento), col = 1:length(unique(datos$Tratamiento)), pch = 1)

# Pregunta 5. Crear histogramas para cada variable
# 5.1 Wildytype 
hist(datos$Wildtype, col = "purple", main = "Wildtype", xlab = "Wildtype")
#5.2 Exceso Riego
hist(datos$ExcesoRiego, col = "gold", main = "Exceso de Riego", xlab = "Exceso de Riego")
#5.3.Sequia
hist(datos$Sequia, col = "brown", main = "Sequía", xlab = "Sequía")

# Pregunta 6. Realizar un factor para la columna Tratamiento.
# Como objetivo de facilitar la identificación de valores, poder etiquetarlos y asignarles colores en el caso de necesitarlos, y definir un orden logico.
tratamiento_factor <- factor(datos$Tratamiento)

# Pregunta 7. Calcular media y desviación estándar por tratamiento
# Nos permite conocer la tendencia central (con la media), y la variabilidad (DE) en cada tto.
media_desviacion <- aggregate(. ~ Tratamiento, data = datos, FUN = function(x) c(Media = mean(x), SD = sd(x)))
print(media_desviacion)

# Pregunta 8. Contar el número de elementos por tratamiento
# Nos permite conocer como estan distribuidos los ttos. comparar en el caso de necesitarlo.
conteo_elementos <- table(tratamiento_factor)
print(conteo_elementos)

# Pregunta 9. Extraer datos para el Tratamiento 1 y Tratamiento 4, cada uno tiene una variable distinta
# Esto nos facilita el preparar los datos para que las siguientes preguntas sean mas sencillas de responder, como es elaborar estadisticas tipo t-test, ANOVA
datos_tratamiento1 <- subset(datos, Tratamiento == 1)
datos_tratamiento4 <- subset(datos, Tratamiento == 4)

# 10. Realizar pruebas de normalidad y comparaciones entre grupos tto 1 y 5, entre Wildtype y Sequia, y entre Wildtype y Exceso Riego
#Podemos evaluar si existen diferencias significativas entre los ttos.
shapiro.test(datos_tratamiento1$Wildtype)
shapiro.test(datos_tratamiento1$Sequia)
shapiro.test(datos_tratamiento1$ExcesoRiego)

# 10.1 Comprobar varianzas iguales
var.test1 <- var.test(datos_tratamiento1$Wildtype, datos_tratamiento1$Sequia)
var.test2 <- var.test(datos_tratamiento1$Wildtype, datos_tratamiento1$ExcesoRiego)

# 10.2 Realizar comparaciones según los resultados de las pruebas anteriores
t.test(datos_tratamiento1$Wildtype, datos_tratamiento1$Sequia)
t.test(datos_tratamiento1$Wildtype, datos_tratamiento1$ExcesoRiego)

# 10.3 Comparar Sequía con Exceso de Riego
t.test(datos_tratamiento1$Sequia, datos_tratamiento1$ExcesoRiego)

# 11. ANOVA para el Tratamiento 1 en dif.condiciones 
# Nos permite evaluar las dif. entre las condiciones que pusimos al inicio del tto 1,
# Validamos hipótesis. 
datos_anova <- datos[datos$Tratamiento == 1, ]
wildtype_data <- datos_anova$Wildtype
sequia_data <- datos_anova$Sequia
exceso_riego_data <- datos_anova$ExcesoRiego
anova_dataframe <- data.frame(Wildtype = wildtype_data, Sequía = sequia_data, ExcesoRiego = exceso_riego_data)


