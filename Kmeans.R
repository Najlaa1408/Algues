library(dplyr)
library(ggplot2)
library(tidyr)
library(cluster)

algues_k <- list(
  "Phaeocystis" = 11,
  "Asterionellopsis glacialis" = 8,
  "Pseudo-nitzschia, complexe delicatissima, groupe des fines (calliantha + delicatissima + pseudodelicatissima + subcurvata)" = 7
)

analyser_algue <- function(nom_algue, k) {
  data_algue <- resultat %>%
    filter(`Résultat : Nom du taxon référent` == nom_algue) %>%
    select(`Passage : Année`, `Passage : Mois`, Occurrences) %>%
    pivot_wider(names_from = `Passage : Mois`, values_from = Occurrences, values_fill = list(Occurrences = 0))
    if (nrow(data_algue) == 0) {
    message(paste("⚠️ Aucune donnée trouvée pour", nom_algue))
    return(NULL)
  }
    data_norm <- as.data.frame(scale(data_algue[,-1])) 
    wss <- sapply(1:15, function(k) kmeans(data_norm, centers = k, nstart = 20)$tot.withinss)
  
  elbow_plot <- ggplot(data.frame(k = 1:15, wss = wss), aes(x = k, y = wss)) +
    geom_point() +
    geom_line() +
    geom_vline(xintercept = k, linetype = "dashed", color = "red") + 
    labs(title = paste("Méthode du coude pour", nom_algue), 
         x = "Nombre de clusters (k)", y = "Somme des carrés intra-cluster") +
    theme_minimal()
  
  print(elbow_plot)
  
  set.seed(123)
  km_result <- kmeans(data_norm, centers = k, nstart = 25)
  
  print(paste("✅ Clustering avec k =", k, "pour", nom_algue))
  
  data_algue$Cluster <- factor(km_result$cluster)
  
  data_long <- data_algue %>%
    gather(key = "Mois", value = "Occurrences", -`Passage : Année`, -Cluster)
    data_long$Mois <- factor(data_long$Mois, 
                           levels = c("Janvier", "Février", "Mars", "Avril", "Mai", "Juin", 
                                      "Juillet", "Août", "Septembre", "Octobre", "Novembre", "Décembre"))
  
  cluster_plot <- ggplot(data_long, aes(x = Mois, y = Occurrences, color = Cluster, group = `Passage : Année`)) +
    geom_line(size = 1) +
    labs(title = paste("Profils temporels de", nom_algue, "par cluster (k =", k, ")"),
         x = "Mois", y = "Occurrences", color = "Cluster") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  print(cluster_plot)  
}
resultats_clusters <- lapply(names(algues_k), function(a) analyser_algue(a, algues_k[[a]]))
