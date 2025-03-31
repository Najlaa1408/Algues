library(dplyr)
library(tidyr)

# Sélectionner les colonnes nécessaires
selected_data <- data2 %>% 
  select(`Passage : Année`, `Résultat : Valeur de la mesure`, `Résultat : Libellé paramètre`)

# Liste pour stocker les datasets filtrés par paramètre
filtered_datasets <- list()
stats_datasets <- list()
final_results <- list()
cor_matrices <- list()

# Extraire tous les paramètres uniques
parametres <- unique(selected_data$`Résultat : Libellé paramètre`)

# Boucle sur chaque paramètre
for (param in parametres) {
  
  # Création du nom de la variable
  param_clean <- gsub(" ", "_", param)  # Remplace les espaces par des underscores
  
  # Filtrer les données pour le paramètre actuel
  filtered_data <- selected_data %>%
    filter(`Résultat : Libellé paramètre` == param) %>%
    mutate(`Résultat : Valeur de la mesure` = as.numeric(`Résultat : Valeur de la mesure`))
  
  # Stocker les données filtrées
  filtered_datasets[[paste0("filtered_data_", param_clean)]] <- filtered_data
  
  # Résumé statistique par année
  stats_temp <- filtered_data %>%
    group_by(`Passage : Année`) %>%
    summarise(
      min_value = min(`Résultat : Valeur de la mesure`, na.rm = TRUE),
      med_value = median(`Résultat : Valeur de la mesure`, na.rm = TRUE),
      max_value = max(`Résultat : Valeur de la mesure`, na.rm = TRUE)
    ) %>%
    rename(Année = `Passage : Année`)
  
  # Stocker les statistiques
  stats_datasets[[param_clean]] <- stats_temp
  
  # Associer les résultats des clusters
  final_data_clusters <- list()
  
  for (i in 1:3) {
    cluster_data <- resultaaats_clusters[[i]]$data %>%
      rename(Année = `Passage : Année`) %>%
      select(Année, Cluster) %>%
      pivot_wider(names_from = Cluster, values_from = Cluster, 
                  values_fn = length, values_fill = list(Cluster = 0))
    
    final_data <- left_join(stats_temp, cluster_data, by = "Année") %>% na.omit()
    final_data_clusters[[paste0("Cluster", i)]] <- final_data
  }
  
  # Stocker les résultats par paramètre
  final_results[[param]] <- final_data_clusters
  
  # Calcul des matrices de corrélation pour chaque cluster
  for (i in 1:3) {
    matrix_name <- paste0("cor_matrix_", param_clean, "_Cluster", i)
    
    if (!is.null(final_data_clusters[[paste0("Cluster", i)]])) {
      final_data_filtered <- final_data_clusters[[paste0("Cluster", i)]]
      
      # Supprimer les colonnes constantes ou entièrement NA
      final_data_filtered <- final_data_filtered[, sapply(final_data_filtered, function(x) length(unique(x)) > 1)]
      
      # Calcul de la matrice de corrélation
      cor_matrix <- cor(final_data_filtered[, -1], use = "complete.obs", method = "pearson")
      
      # Stocker la matrice de corrélation
      cor_matrices[[matrix_name]] <- cor_matrix
    }
  }
}

# Vérification des matrices générées
print(names(cor_matrices))

library(corrplot)



for (name in names(cor_matrices)) {
  print(paste("Affichage de", name))
  corrplot(cor_matrices[[name]], method = "color", title = name)
}





##########################################################################################################
library(dplyr)
library(tidyr)
library(corrplot)

# Liste pour stocker les clusters de chaque algue
all_clusters <- list()

# Boucle sur toutes les algues
for (i in seq_along(resultaaats_clusters)) {
  algue_name <- paste0("Algue_", i)  # Nom de l'algue (ex: Algue_1, Algue_2, ...)
  
  # Extraire les résultats des clusters pour cette algue
  clusters_data <- resultaaats_clusters[[i]]$data %>%
    rename(Année = `Passage : Année`) %>%
  
    select(Année, Cluster) %>%
    pivot_wider(names_from = Cluster, values_from = Cluster, 
                values_fn = length, values_fill = list(Cluster = 0))
  
  # Renommer les colonnes pour inclure le nom de l'algue
  colnames(clusters_data)[-1] <- paste0(colnames(clusters_data)[-1], "_", algue_name)
  
  # Stocker les données
  all_clusters[[algue_name]] <- clusters_data
}

# Fusionner les clusters de toutes les algues sur la colonne "Année"
final_clusters_data <- Reduce(function(x, y) full_join(x, y, by = "Année"), all_clusters)

# Supprimer les valeurs NA si nécessaire
final_clusters_data[is.na(final_clusters_data)] <- 0

# Calculer la matrice de corrélation entre les clusters des algues
cor_matrix_clusters <- cor(final_clusters_data[, -1], use = "complete.obs", method = "pearson")

# Afficher la matrice de corrélation
print(cor_matrix_clusters)

# Visualisation avec corrplot
corrplot(cor_matrix_clusters, method = "color", title = "Corrélation entre les clusters des algues")
