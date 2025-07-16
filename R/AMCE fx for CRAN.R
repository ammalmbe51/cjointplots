#' plotAMCE: Beautiful AMCE plots
#'
#' Plot average marginal component effects (AMCEs) for displaying conjoint results
#' @param Model An AMCE object (created by amce() from cjoint package)
#' @param version Choose wide plots or tall plots (when version = "tall"). Defaults to wide.
#' @param trait_renames Optional field. Used to rename list of within-attribute traits (levels) from original dataframe for presentation.
#' @param attribute_renames Optional field. Used to rename list of attributes from original dataframe for presentation.
#' @param trait_levels Optional field listing within-attribute traits (levels).
#' @param plot_title Title of plot
#' @param plot_subtitle Subtitle of plot
#' @examples
#' #'
#' library(cjoint)
#' data(immigration)
#' model <- amce(Chosen_Immigrant ~ Gender + Country + Education, data = immigration,
#'               respondent.id = "ResID")
#' plotAMCE(model)



#devtools::install("cjointplots")

#devtools::load_all("~/Dropbox/CRAN/cjointplots")


plotAMCE <- function(Model, Type = "Nothing", version = NULL,  trait_renames = NULL,
                     attribute_renames = NULL,
                     trait_levels = NULL,
                     plot_title = NULL,
                     plot_subtitle = "Average Marginal Component Effects",
                     y_label = expression(Delta* " Predicted Probability of Selection (Percentage)")){
  #Extract Data Frame
  df <- data.frame(t(as.data.frame(Model$estimate)))
  df$lower <- df$AMCE - 1.96*df$Std..Error
  df$upper <- df$AMCE + 1.96*df$Std..Error
  df$Attribute <- sapply(strsplit(rownames(df), "[.]"), `[`, 1)
  df$Trait <- sapply(strsplit(rownames(df), "[.]"), `[`, 2)

  #Data Managment
  for(i in seq(1:length(unique(df$Attribute)))){
    df$Trait<- gsub(unique(df$Attribute)[i], "", df$Trait)


  }


  df$Trait <- gsub("([A-Z])([A-Z])", "\\1 \\2", df$Trait)
  df$Trait <- gsub("([a-z])([A-Z])", "\\1 \\2", df$Trait)
  df$Trait <- gsub("([0-9])([A-Z])", "\\1 \\2", df$Trait)
  df$Trait <- gsub("([a-z])([0-9])", "\\1 \\2", df$Trait)
  df$Trait <- gsub("([0-9])([a-z])", "\\1 \\2", df$Trait)

  df$Attribute <-gsub("([a-z])([A-Z])", "\\1 \\2", df$Attribute)

  #Manually Adjust Names
  if (!is.null(trait_renames)) {
    df$Trait <- plyr::mapvalues(df$Trait,
                                from = names(trait_renames),
                                to = trait_renames,
                                warn_missing = FALSE)
  }


  if (!is.null(attribute_renames)) {
    df$Attribute <- plyr::mapvalues(df$Attribute,
                                    from = names(attribute_renames),
                                    to = attribute_renames,
                                    warn_missing = FALSE)
  }


  #*************************** Extract Baseline ***************************

  Baseline_DF<- setNames(data.frame(t(data.frame(Model$baselines))), "Trait")
  Baseline_DF$Attribute <- rownames(Baseline_DF)

  Baseline_DF$Trait <- gsub("([A-Z])([A-Z])", "\\1 \\2", Baseline_DF$Trait)
  Baseline_DF$Trait <- gsub("([a-z])([A-Z])", "\\1 \\2", Baseline_DF$Trait)
  Baseline_DF$Trait <- gsub("([0-9])([A-Z])", "\\1 \\2", Baseline_DF$Trait)
  Baseline_DF$Trait <- gsub("([a-z])([0-9])", "\\1 \\2", Baseline_DF$Trait)
  Baseline_DF$Trait <- gsub("([0-9])([a-z])", "\\1 \\2", Baseline_DF$Trait)

  Baseline_DF$Attribute <-gsub("([a-z])([A-Z])", "\\1 \\2", Baseline_DF$Attribute)


  #*************************** Manual Change ***************************

  if (!is.null(trait_renames)) {
    Baseline_DF$Trait <- plyr::mapvalues(Baseline_DF$Trait,
                                         from = names(trait_renames),
                                         to = trait_renames,
                                         warn_missing = FALSE)
  }

  if (!is.null(attribute_renames)) {
    Baseline_DF$Attribute <- plyr::mapvalues(Baseline_DF$Attribute,
                                             from = names(attribute_renames),
                                             to = attribute_renames,
                                             warn_missing = FALSE)
  }



  #*************************** Combine Baseline & Data Set ***************************

  df <- plyr::rbind.fill(Baseline_DF, df)


  #*************************** Set Levels ***************************

  if (!is.null(trait_levels)) {
    df$Trait <- factor(df$Trait, levels = trait_levels)
  } else {
    df$Trait <- factor(df$Trait)
  }


if (is.null(version)){
  Plot_Final <- ggplot(df, aes(x = Trait, y = AMCE * 100, ymin = lower * 100, ymax = upper * 100)) +
    facet_wrap(Attribute ~., scales = "free") +
    geom_pointrange() +
    coord_flip() +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey") +
    theme_bw() +
    labs(
      y = y_label,
      x = "",
      title = plot_title,
      subtitle = plot_subtitle
    ) +
    theme(plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5))

} else {
  Plot_Final <- ggplot(df, aes(y = Trait, x = AMCE, xmin = lower, xmax = upper)) +
    facet_grid(Attribute ~ ., scales = "free", space = "free") +
    geom_pointrange() +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey") +
    theme_bw() +
    scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
    labs ( y = y_label,
           x = "",
           title = plot_title,
           subtitle = plot_subtitle) +
    theme(strip.background = element_blank(), strip.placement = "outside", plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5),
          axis.title.y=element_blank(),
          plot.caption= element_text(hjust = 0))
}
return(Plot_Final)
}

NULL
