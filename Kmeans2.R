# Charger les bibliothèques nécessaires
library(dplyr)
library(ggplot2)

# Calculer les occurrences mensuelles pour les algues dominantes
resultat <- data %>%
  select(`Passage : Année`, `Passage : Mois`, `Résultat : Nom du taxon référent`, `Résultat : Valeur de la mesure`) %>%
  filter(!is.na(`Passage : Année`) & !is.na(`Passage : Mois`) & 
           !is.na(`Résultat : Nom du taxon référent`) & !is.na(`Résultat : Valeur de la mesure`)) %>%
  group_by(`Passage : Année`, `Passage : Mois`, `Résultat : Nom du taxon référent`) %>%
  summarise(Occurrences = sum(`Résultat : Valeur de la mesure`, na.rm = TRUE), .groups = "drop") %>%
  mutate(`Passage : Mois` = factor(`Passage : Mois`, levels = 1:12,
                                   labels = c("Janvier", "Février", "Mars", "Avril", "Mai", "Juin", 
                                              "Juillet", "Août", "Septembre", "Octobre", "Novembre", "Décembre")))

# Identifier les 3 algues les plus dominantes globalement
top_algues_global <- resultat %>%
  group_by(`Résultat : Nom du taxon référent`) %>%
  summarise(Total_Occurrences = sum(Occurrences, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(Total_Occurrences)) %>%
  slice_max(Total_Occurrences, n = 3)

# Liste des 3 algues les plus dominantes
algues_top3 <- top_algues_global$`Résultat : Nom du taxon référent`

# Générer les graphiques pour chaque algue dominante
for (algue in algues_top3) {
  # Filtrer les données pour l'algue actuelle
  data_algue <- resultat %>%
    filter(`Résultat : Nom du taxon référent` == algue)
  
  # Calculer le pourcentage de chaque année par mois
  data_algue <- data_algue %>%
    group_by(`Passage : Mois`) %>%
    mutate(Percentage = Occurrences / sum(Occurrences) * 100) %>%
    ungroup()
  
  # Créer le graphique en barres empilées
  plot <- ggplot(data_algue, aes(x = `Passage : Mois`, y = Percentage, fill = factor(`Passage : Année`))) +
    geom_bar(stat = "identity", position = "fill") +  # Barres empilées en % (fill)
    scale_y_continuous(labels = scales::percent_format(scale = 1)) +  # Affichage en %
    labs(
      title = paste("Répartition en % des occurrences de l'algue :", algue),
      x = "Mois",
      y = "Pourcentage (%)",
      fill = "Année"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
    )
  
  # Afficher le graphique
  print(plot)
}






