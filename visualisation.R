# Charger les bibliothèques nécessaires
library(dplyr)
library(ggplot2)

# Nettoyer et préparer les données
data_clean <- data %>%
  select(`Passage : Année`, `Passage : Mois`, `Résultat : Nom du taxon référent`) %>%
  filter(!is.na(`Passage : Année`) & !is.na(`Passage : Mois`) & !is.na(`Résultat : Nom du taxon référent`)) %>%
  mutate(`Passage : Mois` = factor(`Passage : Mois`, levels = 1:12, 
                                   labels = c("Janvier", "Février", "Mars", "Avril", "Mai", "Juin", 
                                              "Juillet", "Août", "Septembre", "Octobre", "Novembre", "Décembre")))

# Identifier les 30 types d'algues les plus dominants
top_30_taxa <- data_clean %>%
  group_by(`Résultat : Nom du taxon référent`) %>%
  summarise(Total = n(), .groups = "drop") %>% 
  arrange(desc(Total)) %>%
  slice_head(n = 30) %>%
  pull(`Résultat : Nom du taxon référent`)

# Exclure les algues non dominantes
data_clean <- data_clean %>%
  filter(`Résultat : Nom du taxon référent` %in% top_30_taxa)

# Compter les occurrences par mois et type d'algue
data_grouped <- data_clean %>%
  group_by(`Passage : Année`, `Passage : Mois`, `Résultat : Nom du taxon référent`) %>%
  summarise(Count = n(), .groups = "drop")

# Générer un graphique pour chaque année
unique_years <- unique(data_grouped$`Passage : Année`)

for (year in unique_years) {
  # Filtrer les données pour l'année en cours
  data_year <- data_grouped %>% filter(`Passage : Année` == year)
  
  # Graphique en heatmap
  p2 <- ggplot(data_year, aes(x = `Passage : Mois`, y = `Résultat : Nom du taxon référent`, fill = Count)) +
    geom_tile(color = "white") +
    scale_fill_gradient(low = "white", high = "blue") +
    labs(title = paste("Carte thermique des algues en", year),
         x = "Mois",
         y = "Type d'algue",
         fill = "Occurrences") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Afficher le heatmap
  print(p2)
  ggsave(filename = paste0("heatmap_algues_", year, ".png"), plot = p2, width = 12, height = 8)
}
