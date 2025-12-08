
# ---- 1. Charger les packages ----
library(tidyverse)  
library(janitor)    
library(lubridate) 
library(scales)    
library(forcats)   

# ---- 2. Importer et nettoyer les données ----
df <- read_delim(
  "/Users/flavielachance/Documents/uni/session_automne_2025/outils_numériques/site_web/projet dons/contributions.csv",
  delim = ";",
  locale = locale(encoding = "latin1"),
  trim_ws = TRUE,
  show_col_types = FALSE
) %>%
  clean_names() %>%
  mutate(
    nom_prenom = str_squish(str_to_upper(nom_prenom)),
    code_postal = str_replace_all(code_postal, " ", "") %>% str_to_upper(),
    # Créer un identifiant unique pour chaque donateur
    id_donateur_unique = paste(nom_prenom, code_postal, sep = "_")
  )

# ---- 3. Identifier les donateurs ----
df_fidelite <- df %>%
  group_by(id_donateur_unique) %>%
  summarise(
    nb_partis_distincts = n_distinct(entite_politique),
    nb_annees_distinctes = n_distinct(annee_financiere),
    liste_partis = paste(sort(unique(entite_politique)), collapse = ", "),
    liste_annees = paste(sort(unique(annee_financiere)), collapse = ", "),
    nb_contributions_totales = n()
  ) %>%
  ungroup() 

# ---- 4. Classement des donateurs selon le nombre d'années de contribution ----
df_fidelite <- df_fidelite %>%
  mutate(
    classe_annee = case_when(
      nb_annees_distinctes == 1 ~ "1 année",
      nb_annees_distinctes == 2 ~ "2 années",
      nb_annees_distinctes >= 3 & nb_annees_distinctes <= 4 ~ "3-4 années",
      nb_annees_distinctes >= 5 ~ "5 années et plus",
      TRUE ~ "Catégorie inconnue"
    ),
    classe_annee = factor(classe_annee, levels = c(
      "1 année", "2 années", "3-4 années", "5 années et plus"
    ))
  )

# ---- 5. Analyse des proportions par classe d'années ----
analyse_proportions_temporelles <- df_fidelite %>%
  group_by(classe_annee) %>%
  summarise(Nombre_Donateurs = n()) %>%
  ungroup() %>%
  mutate(
    Proportion = Nombre_Donateurs / sum(Nombre_Donateurs),
    Pourcentage = scales::percent(Proportion, accuracy = 0.1)
  )

print(analyse_proportions_temporelles)

# ---- 6. Graphique : fidélité temporelle ----
library(ggplot2)
library(RColorBrewer)
library(showtext)

font_add_google("Lato", "lato")
showtext_auto()
palette_ylgnbu <- brewer.pal(
  n = length(unique(analyse_proportions_temporelles$classe_annee)),
  name = "YlGnBu"
)
palette_ylgnbu[1] <- "#549955"  

graphique_fidelite_temporelle <- analyse_proportions_temporelles %>%
  ggplot(aes(x = classe_annee, y = Proportion, fill = classe_annee)) +
  geom_col(width = 0.7, color = "white", linewidth = 0.4) +
  geom_text(aes(label = Pourcentage),
            vjust = -0.6, size = 4, family = "lato") +
  scale_fill_manual(values = palette_ylgnbu) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title = "Distribution des donateurs québécois selon la fréquence de contribution",
    subtitle = "Nombre d'années distinctes de dons à des partis politiques",
    x = "",
    y = "Proportion des donateurs (%)",
    fill = "Nombre d'années",
    caption = "Source : Données de contributions politiques, Élections Québec (2018–2025)"
  ) +
  theme_minimal(base_family = "lato") +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
    plot.subtitle = element_text(size = 13, hjust = 0.5),
    axis.title.y = element_text(size = 12),
    axis.text = element_text(size = 11),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none",
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.margin = margin(20, 20, 20, 20)
  )

print(graphique_fidelite_temporelle)

# ---- 7. Analyse de la fidélité par parti politique ----
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(showtext)
library(tidyr)

font_add_google("Lato", "lato")
showtext_auto()

palette_ylgnbu <- brewer.pal(n = 4, name = "YlGnBu")
palette_ylgnbu[1] <- "#549955"  # remplacer la couleur trop pâle

# ---- Partis principaux ----
partis_principaux <- c(
  "Parti libéral du Québec/Quebec Liberal Party",
  "Parti québécois",
  "Coalition avenir Québec - L'équipe François Legault",
  "Québec solidaire",
  "Parti conservateur du Québec"
)

analyse_fidelite_structurelle <- df %>%
  left_join(df_fidelite, by = "id_donateur_unique") %>%
  filter(entite_politique %in% partis_principaux) %>%
  distinct(id_donateur_unique, entite_politique, classe_annee) %>%
  tidyr::complete(
    entite_politique = partis_principaux,
    classe_annee,
    fill = list(Nombre_Donateurs = 0)
  ) %>%
  group_by(entite_politique, classe_annee) %>%
  summarise(Nombre_Donateurs = n(), .groups = "drop") %>%
  group_by(entite_politique) %>%
  mutate(
    Proportion = Nombre_Donateurs / sum(Nombre_Donateurs),
    Pourcentage = scales::percent(Proportion, accuracy = 0.1)
  ) %>%
  ungroup()

# ---- Définir l'ordre des partis" ----
analyse_fidelite_structurelle <- analyse_fidelite_structurelle %>%
  mutate(
    entite_politique = case_when(
      entite_politique == "Coalition avenir Québec - L'équipe François Legault" ~ "Coalition avenir Québec",
      entite_politique == "Parti libéral du Québec/Quebec Liberal Party" ~ "Parti libéral du Québec",
      TRUE ~ as.character(entite_politique)
    )
  )
ordre_partis <- analyse_fidelite_structurelle %>%
  filter(classe_annee == "5 années et plus") %>%
  arrange(Proportion) %>%
  pull(entite_politique)

analyse_fidelite_structurelle <- analyse_fidelite_structurelle %>%
  mutate(entite_politique = factor(entite_politique, levels = ordre_partis))

# ---- visualisation du graphique ----
graphique_fidelite_structurelle <- analyse_fidelite_structurelle %>%
  ggplot(aes(x = entite_politique, y = Proportion, fill = classe_annee)) +
  geom_col(width = 0.7, color = "white", linewidth = 0.4) +
  scale_fill_manual(values = palette_ylgnbu) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  coord_flip() +
  labs(
    title = "Fidélité des donateurs par entité politique",
    subtitle = "Répartition de la proportion de donateurs par nombre d'années de contribution",
    x = "",
    y = "Proportion des donateurs (%)",
    fill = "Nombre d'années",
    caption = "Source : Données de contributions politiques, Élections Québec (2018–2025)"
  ) +
  theme_minimal(base_family = "lato") +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
    plot.subtitle = element_text(size = 13, hjust = 0.5),
    axis.title.y = element_text(size = 12),
    axis.text = element_text(size = 11),
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    plot.margin = margin(20, 20, 20, 20)
  )

print(graphique_fidelite_structurelle)


# ---- 8. Création d'un tableau pour afficher les données
analyse_fidelite_tableau <- df_donateurs %>%
  left_join(df_fidelite_classifiee, by = "id_donateur_unique") %>%
  
  # Regrouper les partis minoritaires
  mutate(
    entite_politique_regroupee = if_else(
      entite_politique %in% partis_principaux,
      entite_politique,
      "Autres entités politiques"
    )
  ) %>%
  
  # Garder un donateur unique par parti et par classe
  distinct(id_donateur_unique, entite_politique_regroupee, classe_annee) %>%
  
  # Comptage des donateurs
  group_by(entite_politique_regroupee, classe_annee) %>%
  summarise(Nombre_Donateurs = n(), .groups = "drop_last") %>%
  
  # Calcul des proportions
  mutate(
    Proportion = Nombre_Donateurs / sum(Nombre_Donateurs),
    Pourcentage = scales::percent(Proportion, accuracy = 0.1)
  ) %>%
  ungroup()

# Afficher le tableau
View(analyse_fidelite_tableau)
print(analyse_fidelite_tableau)
