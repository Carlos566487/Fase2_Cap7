# Carlos Eduardo de Souza, RM 566487, Fase 2, Cap 7

dados <- read.csv("dados.csv")
#dados <- read.csv("base_cana_2025_26_completa.csv")
# Selecionar a variável quantitativa "Peso (kg)"
peso <- dados$Peso..kg.
# Medidas de Tendência Central
media_peso <- mean(peso)
mediana_peso <- median(peso)
moda_peso <- names(sort(table(peso), decreasing = TRUE))[1] # Calcula a moda
cat("Medidas de Tendência Central:\n")
cat("Média:", media_peso, "\n")
cat("Mediana:", mediana_peso, "\n")
cat("Moda:", moda_peso, "\n\n")
# Medidas de Dispersão
desvio_padrao_peso <- sd(peso)
variancia_peso <- var(peso)
amplitude_peso <- max(peso) - min(peso)
iqr_peso <- IQR(peso) # Intervalo Interquartil
cat("Medidas de Dispersão:\n")
cat("Desvio Padrão:", desvio_padrao_peso, "\n")
cat("Variância:", variancia_peso, "\n")
cat("Amplitude:", amplitude_peso, "\n")
cat("Intervalo Interquartil (IQR):", iqr_peso, "\n\n")
# Medidas Separatrizes
quartis_peso <- quantile(peso, probs = c(0.25, 0.5, 0.75))
decis_peso <- quantile(peso, probs = seq(0.1, 0.9, 0.1))
percentis_peso <- quantile(peso, probs = seq(0.01, 0.99, 0.01))
cat("Medidas Separatrizes:\n")
cat("Quartis:\n", quartis_peso, "\n")
cat("Decis:\n", decis_peso, "\n")
cat("Percentis (alguns exemplos):\n", percentis_peso[c(1, 10, 50, 90, 99)], "\n\n")
# Análise Gráfica
boxplot(peso, main = "Boxplot do Peso (kg)", ylab = "Peso (kg)")
hist(peso, main = "Histograma do Peso (kg)", xlab = "Peso (kg)", ylab = "Frequência", col = "lightblue", border = "black")