########### PACKAGES ##############

# Installer (si nécessaire) les packages nécessaires
# À commenter après la première installation
# install.packages("haven")
# install.packages("tidyr")
# install.packages("labelled")
# install.packages("psych")
# install.packages("e1071")
# install.packages("forcats")
# install.packages("stringr")
# install.packages("ggrepel")
# install.packages("patchwork")
# install.packages("ggplot2")
# install.packages("magrittr")

#' Traitement des données alimentaires EHCVM
#'
#' Fonctions génériques pour traiter les fichiers de consommation
#' alimentaire (poissons, viandes, céréales, etc.) de l'enquête EHCVM.
#' @docType package
#' @name premierpackage

# Charger les packages requis
library(dplyr)
library(labelled)
library(tidyr)
library(haven)
library(psych)
library(e1071)
library(readxl)
library(magrittr)
library(ggplot2)
library(patchwork)
library(ggrepel)
library(stringr)
library(forcats)

#' Nettoyage et préparation de la base alimentaire
#'
#' @param df Dataframe brut lu avec haven::read_dta
#' @param nom_base Nom de la base (ex: "poissons", "viandes")
#' @param id_var Nom de l'identifiant produit (ex: "poissons__id")
#'
#' @return Dataframe nettoyé avec variables renommées, labellisées et cohérentes
#' @export
prepare_base <- function(df, nom_base, id_var) {
  # 1. Renommer l'identifiant et supprimer la colonne inutile
  df <- df %>%
    rename(ID = interview__key) %>%
    select(-interview__id)

  # 2. Renommer les variables de manière cohérente
  suffixe <- nom_base
  renoms <- c(
    "autre" = paste0("s07Bq02_autre_", suffixe),
    "qte_7j" = paste0("s07Bq03a_", suffixe),
    "unite" = paste0("s07Bq03b_", suffixe),
    "taille" = paste0("s07Bq03c_", suffixe),
    "qte_autoprod_7j" = paste0("s07Bq04_", suffixe),
    "qte_autres_src_7j" = paste0("s07Bq05_", suffixe),
    "dernier_achat" = paste0("s07Bq06_", suffixe),
    "qte_dern_achat" = paste0("s07Bq07a_", suffixe),
    "unite_dern_achat" = paste0("s07Bq07b_", suffixe),
    "taille_dern_achat" = paste0("s07Bq07c_", suffixe),
    "val_dern_achat" = paste0("s07Bq08_", suffixe)
  )

  df <- df %>% rename(!!!renoms)

  # 3. Labelliser les variables et leurs modalités
  var_label(df[[id_var]]) <- paste("Identifiant du produit -", nom_base)

  var_label(df) <- list(
    ID = "Identifiant de l'enquêté",
    autre = paste("Identification d'un autre produit non mentionné -", nom_base),
    qte_7j = paste("Quantité totale consommée (7j) -", nom_base),
    unite = paste("Unité de mesure de la quantité consommée -", nom_base),
    taille = paste("Taille de mesure de la quantité consommée -", nom_base),
    qte_autoprod_7j = paste("Quantité de production propre consommée -", nom_base),
    qte_autres_src_7j = paste("Quantité provenant d'autres sources -", nom_base),
    dernier_achat = paste("Date du dernier achat -", nom_base),
    qte_dern_achat = paste("Quantité achetée dernièrement -", nom_base),
    unite_dern_achat = paste("Unité du dernier achat -", nom_base),
    taille_dern_achat = paste("Taille du dernier achat -", nom_base),
    val_dern_achat = paste("Montant payé lors du dernier achat -", nom_base)
  )

  return(df)
}

#' Exporter les autres produits non listés
#'
#' @param df Dataframe brut
#' @param nom_base Nom de la base (ex: "poissons")
#'
#' @return Dataframe des autres produits avec leurs descriptions
#' @export
exporter_autres_produits <- function(df, nom_base) {
  var_autre <- paste0("s07Bq02_autre_", nom_base)

  df_autre <- df %>%
    filter(!is.na(.data[[var_autre]]) & .data[[var_autre]] != "") %>%
    select(ID, .data[[var_autre]])

  return(df_autre)
}

#' Calcul de la valeur unitaire (VU)
#'
#' @param df Base contenant les variables de prix et quantités
#'
#' @return La base avec VU calculée et observations sans quantité filtrées
#' @export
ajouter_valeur_unitaire <- function(df) {
  # 1. Identifier automatiquement la variable quantité 7j
  qte_var <- names(df)[grepl("qte_7j|qte_.*_7j", names(df))][1]

  # 2. Calcul de la VU
  df <- df %>%
    mutate(VU = val_dern_achat / qte_dern_achat) %>%
    mutate(VU = ifelse(is.infinite(VU), NA, VU))

  # 3. Filtre automatique des NA
  if(!is.na(qte_var)) {
    df <- df %>%
      filter(!is.na(.data[[qte_var]]))
    message(paste("Filtrage des NA appliqué sur la variable:", qte_var))
  } else {
    warning("Aucune variable de quantité (format qte_*_7j) n'a été trouvée")
  }

  return(df)
}

#' Construction de la base de référence des valeurs unitaires (VU)
#'
#' @param df Base avec VU et identifiants
#' @param id_var Nom de l'identifiant produit (ex: "poissons__id")
#'
#' @return Base VU (produit, unité, taille, VU représentatif)
#' @export
construire_base_VU <- function(df, id_var) {
  # Fonction pour calculer la mode
  get_mode <- function(x) {
    x <- na.omit(x)
    if (length(x) == 0) return(NA)
    ux <- unique(x)
    freq <- tabulate(match(x, ux))
    mode_vals <- ux[freq == max(freq)]
    return(mode_vals[1])
  }

  # Fonction pour choisir la valeur représentative
  select_representative_VU <- function(v) {
    v <- na.omit(v)
    if (length(v) == 0) return(NA)

    mode_val <- get_mode(v)
    median_val <- median(v)

    if (is.na(mode_val) || is.na(median_val)) return(NA)

    if (abs(mode_val - median_val) / median_val < 0.1) {
      return(mode_val)
    } else {
      return(median_val)
    }
  }

  # Création de la baseVU
  baseVU <- df %>%
    group_by(across(all_of(c(id_var, "unite_dern_achat", "taille_dern_achat")))) %>%
    summarise(VU = select_representative_VU(VU), .groups = "drop") %>%
    filter(!is.na(taille_dern_achat))

  # Copie des labels depuis la base d'origine
  baseVU <- baseVU %>%
    mutate(
      !!sym(id_var) := labelled::copy_labels(df[[id_var]], baseVU[[id_var]]),
      unite_dern_achat = labelled::copy_labels(df$unite_dern_achat, unite_dern_achat),
      taille_dern_achat = labelled::copy_labels(df$taille_dern_achat, taille_dern_achat)
    )

  return(baseVU)
}

#' Application des valeurs unitaires pour obtenir la consommation monétaire
#'
#' @param df Base d'origine (après ajout des VU individuelles)
#' @param baseVU Base des valeurs unitaires (produit, unité, taille, VU)
#' @param id_var Identifiant du produit (ex: "poissons__id")
#'
#' @return Base contenant les dépenses estimées (avec VU_ref)
#' @export
calculer_valeurs_consommees <- function(df, baseVU, id_var) {
  baseVU <- baseVU %>% rename(VU_ref = VU)

  df <- df %>%
    left_join(baseVU,
              by = c(id_var, "unite" = "unite_dern_achat", "taille" = "taille_dern_achat")) %>%
    mutate(
      QuantiteConsommee = qte_7j,
      ValeurConsommee = QuantiteConsommee * VU_ref
    )

  return(df)
}

#' Décomposer les consommations selon la source
#'
#' @param df Base avec VU et quantités par source
#' @param id_var Identifiant produit
#'
#' @return Base au format long : (source, quantité, valeur)
#' @export
calculer_conso_par_source <- function(df, id_var) {
  df <- df %>%
    mutate(qte_achat = qte_7j - qte_autoprod_7j - qte_autres_src_7j) %>%
    select(ID, all_of(id_var), unite, taille, VU, VU_ref,
           qte_autoprod_7j, qte_autres_src_7j, qte_achat) %>%
    pivot_longer(cols = starts_with("qte_"),
                 names_to = "source",
                 values_to = "quantite") %>%
    mutate(
      source = recode(source,
                      qte_autoprod_7j = "Autoconsommation",
                      qte_autres_src_7j = "Don",
                      qte_achat = "Achat"),
      ValeurConsommee = quantite * VU_ref
    )

  return(df)
}

#' Appliquer une table de conversion pour obtenir les quantités en kg
#'
#' @param df Base de consommation avec Produit, unite, taille, QuantiteConsommee
#' @param table_conversion Table avec poids en grammes (colonnes : produitID, uniteID, tailleID, poids)
#'
#' @return Base avec variable QuantiteConsommeeKG et colonne Source
#' @export
ajouter_quantite_kg <- function(df, table_conversion) {
  # Renommage des colonnes pour la jointure
  table_conversion <- table_conversion %>%
    rename(
      Unite = uniteID,
      Taille = tailleID,
      Produit = produitID,
      ConversionUniteGramme = poids
    )


  df <- df %>%
    rename_with(~ ifelse(.x == "quantite", "QuantiteConsommee", .x))

  # Gestion flexible des noms de colonnes
  if (!"Produit" %in% names(df)) {
    produit_col <- names(df)[grepl("id", names(df))][1]
    df <- df %>% rename(Produit = !!sym(produit_col))
  }

  if (!"Source" %in% names(df) && "source" %in% names(df)) {
    df <- df %>% mutate(Source = source)
  }

  # Jointure avec la table de conversion
  df <- df %>%
    rename(Unite = unite, Taille = taille) %>%
    left_join(table_conversion %>%
                select(Produit, Unite, Taille, ConversionUniteGramme),
              by = c("Produit", "Unite", "Taille"))

  # Conversion robuste des poids
  df <- df %>%
    mutate(
      ConversionUniteGramme = gsub(",", ".", ConversionUniteGramme),
      ConversionUniteGramme = gsub(";", ".", ConversionUniteGramme),
      ConversionUniteGramme = gsub("[^0-9.]", "", ConversionUniteGramme),
      ConversionUniteGramme = as.numeric(ConversionUniteGramme),
      QuantiteConsommeeKG = (ConversionUniteGramme * QuantiteConsommee) / 1000
    ) %>%
    select(-ConversionUniteGramme)

  return(df)
}

#' Winsoriser une variable quantitative groupée par modalité
#'
#' @param df Base avec QuantiteConsommee et variable Source
#' @return Base avec colonne QuantiteConsommee1 winsorisée et IDs disponibles
#' @export
winsoriser_quantite <- function(df) {
  # Gestion flexible des noms de colonnes
  if (!"IDs" %in% names(df)) {
    if ("ID" %in% names(df)) {
      df <- df %>% rename(IDs = ID)
    } else if ("interview__key" %in% names(df)) {
      df <- df %>% rename(IDs = interview__key)
    }
  }

  if (!"Source" %in% names(df) && "source" %in% names(df)) {
    df <- df %>% mutate(Source = source)
  }

  # Fonction de détection des outliers
  detect_outliers_vec <- function(x) {
    Q1 <- quantile(x, 0.25, na.rm = TRUE)
    Q3 <- quantile(x, 0.75, na.rm = TRUE)
    IQR <- Q3 - Q1
    x < (Q1 - 1.5 * IQR) | x > (Q3 + 1.5 * IQR)
  }

  # Fonction de winsorisation
  winsorize_vec <- function(x) {
    Q1 <- quantile(x, 0.25, na.rm = TRUE)
    Q3 <- quantile(x, 0.75, na.rm = TRUE)
    IQR <- Q3 - Q1
    lower <- Q1 - 1.5 * IQR
    upper <- Q3 + 1.5 * IQR
    x[x < lower] <- lower
    x[x > upper] <- upper
    return(x)
  }

  # Application groupée par Source
  df <- df %>%
    group_by(Source) %>%
    mutate(
      is_outlier_QC1 = detect_outliers_vec(QuantiteConsommeeKG),
      QuantiteConsommeeKG1 = winsorize_vec(QuantiteConsommeeKG),
      is_outlier_QC2 = detect_outliers_vec(QuantiteConsommeeKG1)
    ) %>%
    ungroup()

  return(df)
}

#' Ajouter les calories et identifier les observations hors plage
#'
#' @param df Base avec QuantiteConsommeeKG et Produit
#' @param table_calories Table avec conversion en calories
#' @param table_menage Table avec IDs et membres du ménage
#'
#' @return Base avec variable CaloriesParTete et HorsPlageCalories
#' @export
ajouter_calories <- function(df, table_calories, table_menage) {
  # Gestion flexible des noms de colonnes
  id_col <- if ("interview__key" %in% names(table_menage)) "interview__key" else "IDs"

  # Jointure avec la table des calories
  df <- df %>%
    left_join(table_calories %>%
                select(Produit = codpr, cal) %>%
                rename(ConversionCalories = cal),
              by = "Produit") %>%
    mutate(Calories = QuantiteConsommeeKG1 * ConversionCalories * 10) %>%
    select(-ConversionCalories)

  # Calcul de la taille du ménage
  table_menage <- table_menage %>%
    rename(IDs = !!sym(id_col)) %>%
    group_by(IDs) %>%
    summarise(Taille_Menage = n(), .groups = "drop")

  # Calcul des calories par tête
  df <- df %>%
    left_join(table_menage, by = "IDs") %>%
    group_by(IDs) %>%
    mutate(CalorieTotal = sum(Calories, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(
      CaloriesParTete = CalorieTotal / Taille_Menage,
      CaloriesParJour = CaloriesParTete / 7,
      HorsPlageCalories = ifelse(CaloriesParJour < 10 | CaloriesParJour > 100, 1, 0),
      SousSeuil2400 = ifelse(CaloriesParTete / 7 < 2400, 1, 0)
    ) %>%
    select(-CalorieTotal, -CaloriesParJour)

  return(df)
}

#' Enrichir les données avec les variables de contexte (région, département, milieu)
#'
#' @param df Base principale
#' @param base_all Base contextuelle avec s00q01 à s00q04
#'
#' @return Base enrichie
#' @export
ajouter_contexte_menage <- function(df, base_all) {
  base_all <- base_all %>%
    rename(
      IDs = interview__key,
      region = s00q01,
      departement = s00q02,
      milieu = s00q04
    )

  df <- df %>%
    left_join(base_all %>% select(IDs, region, departement, milieu), by = "IDs")

  return(df)
}


#' Générer les graphiques descriptifs pour une base alimentaire
#'
#' @param base Dataframe appuré avec les variables nécessaires
#' @param nom_aliment Nom de l'aliment (pour les titres des graphiques)
#' @param output_dir Dossier où sauvegarder les graphiques (NULL pour ne pas sauvegarder)
#' @param prefix_fichier Préfixe pour les noms de fichiers (ex: "poisson_")
#' @param faire_ACP Booléen indiquant si l'ACP doit être réalisée (TRUE par défaut)
#'
#' @return Liste contenant tous les graphiques et résultats analytiques générés
#' @export
generer_graphiques_descriptifs <- function(base, nom_aliment = "produits",
                                           output_dir = NULL, prefix_fichier = "",
                                           faire_ACP = TRUE) {

  # Vérification et installation des packages requis
  required_pkgs <- c("ggplot2", "dplyr", "tidyr", "scales", "forcats")
  for (pkg in required_pkgs) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      install.packages(pkg)
    }
  }

  # Conversion robuste des variables
  base <- base %>%
    mutate(across(where(is.labelled), as_factor)) %>%
    mutate(across(where(is.character), as.factor)) %>%
    mutate(across(c(Source, milieu, region, departement, Produit, Taille), as.factor)) %>%
    mutate(across(where(is.numeric), ~ifelse(!is.finite(.x), NA, .x)))

  # Vérification des colonnes nécessaires
  required_cols <- c("IDs", "QuantiteConsommeeKG", "ValeurConsommee", "Calories",
                     "Source", "milieu", "region", "departement", "Produit",
                     "Taille_Menage", "CaloriesParTete")

  missing_cols <- setdiff(required_cols, names(base))
  if (length(missing_cols) > 0) {
    stop(paste("Colonnes manquantes:", paste(missing_cols, collapse = ", ")))
  }

  # Initialisation des résultats
  results <- list(
    tables = list(),
    plots = list(),
    acp = NULL
  )

  # Fonctions internes pour les graphiques
  create_histogram <- function(data, x_var, title, x_lab, y_lab = "Nombre de ménages",
                               binwidth = NULL, bins = 30, fill = "steelblue") {
    ggplot(data, aes(x = !!sym(x_var))) +
      geom_histogram(fill = fill, color = "white", binwidth = binwidth, bins = bins) +
      labs(title = paste(title, "-", nom_aliment), x = x_lab, y = y_lab) +
      theme_minimal()
  }

  create_boxplot <- function(data, y_var, title, y_lab, x_var = NULL, fill = "orange") {
    p <- ggplot(data, aes(y = !!sym(y_var))) +
      geom_boxplot(fill = fill) +
      labs(title = paste(title, "-", nom_aliment), y = y_lab) +
      theme_minimal()

    if (!is.null(x_var)) {
      p <- p + aes(x = !!sym(x_var))
    }
    return(p)
  }

  # 1. Graphiques univariés ----

  # 1.1 Quantité totale consommée par ménage (KG)
  conso_par_menage <- base %>%
    group_by(IDs) %>%
    summarise(QuantiteTotaleKG = sum(QuantiteConsommeeKG, na.rm = TRUE))

  results$plots$hist_qte_menage <- create_histogram(
    conso_par_menage, "QuantiteTotaleKG",
    "Distribution des quantités totales consommées par ménage",
    "Quantité totale consommée (KG)"
  )

  skew <- e1071::skewness(conso_par_menage$QuantiteTotaleKG, na.rm = TRUE)
  y_max <- max(conso_par_menage$QuantiteTotaleKG, na.rm = TRUE)

  results$plots$boxplot_qte_menage <- create_boxplot(
    conso_par_menage, "QuantiteTotaleKG",
    "Boxplot de la quantité consommée par ménage",
    "Quantité consommée par ménage (KG)"
  ) +
    annotate("text", x = 0.1, y = y_max * 0.95,
             label = paste0("Coefficient d'asymétrie = ", round(skew, 2)),
             hjust = 0, size = 4, color = "darkred")

  # 1.2 Quantité consommée par tête
  conso_par_tete <- base %>%
    group_by(IDs, Taille_Menage) %>%
    summarise(
      QuantiteTotaleKG = sum(QuantiteConsommeeKG, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(QuantiteParTeteKG = QuantiteTotaleKG / Taille_Menage)

  results$plots$hist_qte_tete <- create_histogram(
    conso_par_tete, "QuantiteParTeteKG",
    "Distribution des quantités consommées par tête",
    "Quantité consommée par tête (KG)", binwidth = 0.2
  )

  skew <- e1071::skewness(conso_par_tete$QuantiteParTeteKG, na.rm = TRUE)
  y_max <- max(conso_par_tete$QuantiteParTeteKG, na.rm = TRUE)

  results$plots$boxplot_qte_tete <- create_boxplot(
    conso_par_tete, "QuantiteParTeteKG",
    "Boxplot de la quantité consommée par tête",
    "Quantité consommée par tête (KG)"
  ) +
    annotate("text", x = 0.1, y = y_max * 0.95,
             label = paste0("Coefficient d'asymétrie = ", round(skew, 2)),
             hjust = 0, size = 4, color = "darkred")

  # 1.3 Polygone des fréquences cumulées
  conso_sorted <- conso_par_tete %>%
    arrange(QuantiteParTeteKG) %>%
    mutate(
      Effectif = 1,
      Effectif_cum = cumsum(Effectif),
      Frequence_cum = Effectif_cum / n()
    )

  point_x <- 1
  point_df <- conso_sorted %>%
    filter(abs(QuantiteParTeteKG - point_x) == min(abs(QuantiteParTeteKG - point_x))) %>%
    slice(1)

  results$plots$freq_cum <- ggplot(conso_sorted, aes(x = QuantiteParTeteKG, y = Frequence_cum)) +
    geom_step(color = "darkgreen", size = 1) +
    geom_point(data = point_df, aes(x = QuantiteParTeteKG, y = Frequence_cum),
               color = "red", size = 3) +
    geom_text(
      data = point_df,
      aes(label = paste0("(", round(QuantiteParTeteKG, 2), ", ",
                         scales::percent(Frequence_cum, accuracy = 1), ")")),
      vjust = -1,
      color = "red"
    ) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    labs(
      title = paste("Polygone des fréquences cumulées -", nom_aliment),
      x = "Quantité consommée par tête (KG)",
      y = "Fréquence cumulée (%)"
    ) +
    theme_minimal()

  # 1.4 Calories
  calories_par_menage <- base %>%
    group_by(IDs) %>%
    summarise(CaloriesTotales = sum(Calories, na.rm = TRUE), .groups = "drop") %>%
    left_join(
      base %>% select(IDs, Taille_Menage) %>% distinct(),
      by = "IDs"
    ) %>%
    mutate(CaloriesParTete = CaloriesTotales / Taille_Menage)

  results$plots$hist_cal_menage <- create_histogram(
    calories_par_menage, "CaloriesTotales",
    "Répartition des calories consommées par ménage",
    "Calories totales consommées (sur 7 jours)", binwidth = 1000
  )

  results$plots$hist_cal_tete <- create_histogram(
    calories_par_menage, "CaloriesParTete",
    "Répartition des calories consommées par tête",
    "Calories totales consommées (sur 7 jours)", binwidth = 150
  )

  # 1.5 Proportion de ménages avec consommation > 0 par source
  prop_menages <- base %>%
    group_by(IDs, Source) %>%
    summarise(TotalKG = sum(QuantiteConsommeeKG, na.rm = TRUE), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = Source, values_from = TotalKG, values_fill = 0) %>%
    summarise(across(-IDs, ~ sum(.x > 0) / n())) %>%
    tidyr::pivot_longer(everything(), names_to = "Source", values_to = "Proportion")

  results$plots$prop_source <- ggplot(prop_menages, aes(x = Source, y = Proportion, fill = Source)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = scales::percent(Proportion, accuracy = 1)), vjust = -0.5) +
    scale_y_continuous(labels = scales::percent_format()) +
    labs(
      title = paste("Proportion de ménages consommant par source -", nom_aliment),
      y = "Proportion de ménages (%)"
    ) +
    theme_minimal() +
    theme(legend.position = "none")

  # 2. Graphiques bivariés ----

  # 2.1 Top produits consommés
  top_produits <- base %>%
    group_by(Produit) %>%
    summarise(TotalKG = sum(QuantiteConsommeeKG, na.rm = TRUE), .groups = "drop") %>%
    arrange(desc(TotalKG)) %>%
    mutate(
      Rang = row_number(),
      Pourcentage = 100 * TotalKG / sum(TotalKG, na.rm = TRUE)
    )

  top5 <- top_produits %>% slice_head(n = 5)

  results$plots$top_produits <- ggplot(top5, aes(x = reorder(as.character(Produit), TotalKG), y = TotalKG)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    geom_text(aes(label = paste0(round(TotalKG, 1), " kg (", round(Pourcentage, 1), "%)")),
              vjust = -0.5, size = 3.5) +
    labs(
      title = paste("Top 5 des", nom_aliment, "les plus consommés (en kg)"),
      x = "Produit",
      y = "Quantité totale consommée (kg)"
    ) +
    theme_minimal()

  # 2.2 Quantité consommée selon la source
  results$plots$qte_source <- ggplot(base, aes(x = Source, y = QuantiteConsommeeKG, fill = Source)) +
    geom_boxplot() +
    labs(title = paste("Quantité consommée selon la source -", nom_aliment),
         y = "Quantité (kg)") +
    theme_minimal()

  # 2.3 Quantité consommée selon le milieu
  results$plots$qte_milieu <- ggplot(base, aes(x = milieu, y = QuantiteConsommeeKG, fill = milieu)) +
    geom_violin(trim = FALSE, alpha = 0.5) +
    geom_boxplot(width = 0.1) +
    labs(title = paste("Distribution de la consommation par milieu -", nom_aliment),
         x = "Milieu", y = "Quantité consommée (kg)") +
    theme_minimal()

  # 2.4 Consommation moyenne par région et source
  conso_region_source <- base %>%
    group_by(region, Source) %>%
    summarise(QuantiteMoy = mean(QuantiteConsommeeKG, na.rm = TRUE), .groups = "drop")

  results$plots$conso_region_source <- ggplot(conso_region_source,
                                              aes(x = fct_reorder(region, QuantiteMoy),
                                                  y = QuantiteMoy, fill = Source)) +
    geom_col(position = "dodge") +
    coord_flip() +
    labs(title = paste("Consommation moyenne par région et source -", nom_aliment),
         x = "Région", y = "Quantité moyenne consommée (kg)") +
    theme_minimal()

  # 2.5 Relation quantité-valeur
  results$plots$qte_valeur <- ggplot(base, aes(x = QuantiteConsommeeKG, y = ValeurConsommee)) +
    geom_point(alpha = 0.3) +
    geom_smooth(method = "lm", se = TRUE, color = "blue") +
    labs(title = paste("Relation quantité-valeur -", nom_aliment),
         x = "Quantité consommée (kg)", y = "Valeur consommée") +
    theme_minimal()

  # 2.6 Apport calorique par région
  results$plots$cal_region <- ggplot(base,
                                     aes(x = fct_reorder(region, CaloriesParTete, median, na.rm = TRUE),
                                         y = CaloriesParTete)) +
    geom_boxplot(fill = "lightblue") +
    coord_flip() +
    labs(title = paste("Apports caloriques par région -", nom_aliment),
         x = "Région", y = "Calories par tête") +
    theme_minimal()

  # 3. Analyses multivariées ----

  # 3.1 Tableaux croisés
  results$tableau_croise_source_taille <- base %>%
    count(Produit, Taille) %>%
    arrange(desc(n))

  # 3.2 Graphiques multivariés
  results$plots$calories_source_produit <- ggplot(base, aes(x = Source, y = Calories, fill = Produit)) +
    geom_boxplot(outlier.shape = NA) +
    facet_wrap(~ Produit, scales = "free_y") +
    theme_minimal() +
    labs(title = paste("Distribution des calories par Source et Produit -", nom_aliment))

  # Heatmap Produit × région
  heatmap_data <- base %>%
    count(region, Produit) %>%
    mutate(region = str_wrap(region, width = 10))

  results$plots$heatmap_produit_region <- ggplot(heatmap_data,
                                                 aes(x = region, y = Produit, fill = n)) +
    geom_tile(color = "white") +
    scale_fill_gradient(low = "white", high = "steelblue") +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1)
    ) +
    labs(title = paste("Fréquence de consommation par région et produit -", nom_aliment),
         x = "Région", y = "Produit")

  # 3.3 Analyse en Composantes Principales (ACP)
  if(faire_ACP && requireNamespace("FactoMineR", quietly = TRUE) &&
     requireNamespace("factoextra", quietly = TRUE)) {

    variables_quanti <- c(
      "QuantiteConsommeeKG", "ValeurConsommee",
      "Calories", "Taille_Menage", "CaloriesParTete"
    )

    base_quanti <- base %>%
      select(all_of(variables_quanti)) %>%
      mutate(across(everything(), as.numeric)) %>%
      na.omit()

    if(nrow(base_quanti) > 10) {
      res_pca <- FactoMineR::PCA(base_quanti, scale.unit = TRUE, graph = FALSE)

      # Stockage des résultats ACP
      results$acp <- list(
        eigenvalues = factoextra::get_eig(res_pca),
        var = factoextra::get_pca_var(res_pca),
        ind = factoextra::get_pca_ind(res_pca),
        summary = summary(res_pca)
      )

      # Création des graphiques ACP
      results$plots$acp_inertie <- factoextra::fviz_eig(res_pca) +
        labs(title = paste("Variance expliquée -", nom_aliment))

      results$plots$acp_variables <- factoextra::fviz_pca_var(res_pca, col.var = "contrib") +
        scale_color_gradient2(low = "white", mid = "blue", high = "red") +
        labs(title = paste("Contribution des variables -", nom_aliment))

      # Graphique des individus (échantillonné si trop nombreux)
      n_ind <- min(100, nrow(base_quanti))
      results$plots$acp_individus <- factoextra::fviz_pca_ind(
        res_pca,
        select.ind = list(contrib = n_ind),
        col.ind = "cos2",
        repel = TRUE
      ) + labs(title = paste("Individus -", nom_aliment))
    } else {
      warning("Données insuffisantes pour l'ACP (n = ", nrow(base_quanti), ")")
    }
  }

  # Sauvegarde des résultats si output_dir spécifié
  if (!is.null(output_dir)) {
    if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

    # Sauvegarde des graphiques
    for (plot_name in names(results$plots)) {
      tryCatch({
        ggplot2::ggsave(
          filename = file.path(output_dir, paste0(prefix_fichier, plot_name, ".png")),
          plot = results$plots[[plot_name]],
          width = 10, height = 6, dpi = 300
        )
      }, error = function(e) {
        warning("Échec de sauvegarde du graphique ", plot_name, ": ", e$message)
      })
    }

    # Sauvegarde des résultats ACP
    if (!is.null(results$acp)) {
      tryCatch({
        write.csv(
          results$acp$eigenvalues,
          file.path(output_dir, paste0(prefix_fichier, "acp_eigenvalues.csv")),
          row.names = TRUE
        )
      }, error = function(e) {
        warning("Échec de sauvegarde des résultats ACP: ", e$message)
      })
    }
  }

  return(results)
}



############################### INDICATEURS ###################################

#' Calculer les indicateurs synthétiques à partir d'une base appurée
#'
#' @param base Dataframe appuré (avec QuantiteConsommeeKG, ValeurConsommee, Calories, etc.)
#' @param base_x1 (optionnel) Base brute avec une variable "dernier_achat_poisson" pour l'indicateur 9
#' @param var_source Nom de la variable source ("Source" par défaut)
#' @param var_produit Nom de la variable produit ("Produit" par défaut)
#' @param var_achat Nom de la variable d'achat récent dans la base brute ("dernier_achat_poisson")
#'
#' @return Liste avec les 10 indicateurs calculés
#' @export
calculer_indicateurs_synthetiques <- function(base, base_x1 = NULL,
                                              var_source = "Source",
                                              var_produit = "Produit",
                                              var_achat = "dernier_achat_poisson") {
  # Renommer les variables si nécessaire
  base <- base %>%
    rename(Source = all_of(var_source),
           Produit = all_of(var_produit))

  # 1. Dépense alimentaire moyenne par ménage
  depense_moyenne <- base %>%
    group_by(IDs) %>%
    summarise(DepenseTotale = sum(ValeurConsommee, na.rm = TRUE)) %>%
    summarise(DepenseMoyenne = mean(DepenseTotale, na.rm = TRUE))

  # 2. Quantité moyenne consommée (en kg)
  qte_moy_kg <- base %>%
    summarise(QuantiteMoyenneKG = mean(QuantiteConsommeeKG, na.rm = TRUE))

  # 3. Taux d'autoconsommation (quantité)
  taux_auto <- base %>%
    filter(Source == "Autoconsommation") %>%
    group_by(IDs) %>%
    summarise(VA = sum(ValeurConsommee, na.rm = TRUE)) %>%
    left_join(
      base %>%
        group_by(IDs) %>%
        summarise(VT = sum(ValeurConsommee, na.rm = TRUE)),
      by = "IDs"
    ) %>%
    mutate(TauxAuto = VA / VT)

  # 4. Part des dons dans la consommation
  part_don <- base %>%
    filter(Source == "Don") %>%
    group_by(IDs) %>%
    summarise(VD = sum(ValeurConsommee, na.rm = TRUE)) %>%
    left_join(
      base %>%
        group_by(IDs) %>%
        summarise(VT = sum(ValeurConsommee, na.rm = TRUE)),
      by = "IDs"
    ) %>%
    mutate(PartDon = VD / VT)

  # 5. Ratio dépense / apport énergétique
  ratio_val_cal <- base %>%
    group_by(IDs) %>%
    summarise(Cal = sum(Calories, na.rm = TRUE)/3,
              VT = sum(ValeurConsommee, na.rm = TRUE)) %>%
    filter(VT > 0) %>%
    mutate(KcalParUniteDepensee = Cal / VT)

  # 6. Proportion de ménages sous 2400 kcal/jour/pers
  prop_sous_2400 <- base %>%
    group_by(IDs) %>%
    summarise(Sous2400 = max(SousSeuil2400, na.rm = TRUE)) %>%
    summarise(Proportion = mean(Sous2400, na.rm = TRUE))

  # 7. Consommation énergétique moyenne (kcal/personne/jour)
  kcal_jour <- base %>%
    group_by(IDs) %>%
    summarise(Cal = mean(CaloriesParTete, na.rm = TRUE)) %>%
    summarise(KcalParJour = mean(Cal / 7, na.rm = TRUE))

  # 8. Taux de dépendance à l'achat
  taux_achat <- base %>%
    filter(Source == "Achat") %>%
    group_by(IDs) %>%
    summarise(VA = sum(ValeurConsommee, na.rm = TRUE)) %>%
    left_join(
      base %>%
        group_by(IDs) %>%
        summarise(VT = sum(ValeurConsommee, na.rm = TRUE)),
      by = "IDs"
    ) %>%
    mutate(TauxAchat = VA / VT)

  # 9. Fréquence d'achat récente
  if (!is.null(base_x1) && var_achat %in% names(base_x1)) {
    freq_achat <- base_x1 %>%
      select(ID, !!sym(var_achat)) %>%
      mutate(AchatRecent = ifelse(!!sym(var_achat) %in% c(1, 2, 3), 1, 0)) %>%
      group_by(ID) %>%
      summarise(AchatRecent = max(AchatRecent, na.rm = TRUE))
  } else {
    freq_achat <- NULL
  }

  # 10. Diversité de consommation
  diversite_produits <- base %>%
    group_by(IDs) %>%
    summarise(Diversite = n_distinct(Produit))

  # Retourner tous les indicateurs dans une liste nommée
  list(
    depense_moyenne = depense_moyenne,
    qte_moy_kg = qte_moy_kg,
    taux_auto = taux_auto,
    part_don = part_don,
    ratio_val_cal = ratio_val_cal,
    prop_sous_2400 = prop_sous_2400,
    kcal_jour = kcal_jour,
    taux_achat = taux_achat,
    freq_achat = freq_achat,
    diversite_produits = diversite_produits
  )
}



