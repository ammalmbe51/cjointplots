#' plotMM: Beautiful Marginal Mean plots
#'
#' Plot marginal means for displaying conjoint results
#' @param Model A marginal mean object (created by mm() from cregg package (https://github.com/leeper/cregg))
#' @param version Choose wide plots or tall plots (when version = "tall"). Defaults to wide.
#' @param trait_renames Optional field. Used to rename list of within-attribute traits (levels) from original dataframe for presentation.
#' @param attribute_renames Optional field. Used to rename list of attributes from original dataframe for presentation.
#' @param trait_levels Optional field listing within-attribute traits (levels).
#' @param plot_title Title of plot
#' @param plot_subtitle Subtitle of plot
#' @examples
#'
#' library(cjoint)
#' library(cregg)
#' data(immigration)
#' model <- amce(Chosen_Immigrant ~ Gender + Country + Education, data = immigration,
#'               respondent.id = "ResID")
#' model1 <- mm(model, Chosen_Immigrant ~ Gender + Country + Education, id=~"ResID"
#' plotMM(model1)


plotMM <- function(Model, Type = "Nothing", version = NULL,  trait_renames = NULL,
                     attribute_renames = NULL,
                     trait_levels = NULL,
                     plot_title = NULL,
                     plot_subtitle = "Marginal Means",
                     y_label = expression("Average Probability of Choosing Profile")){

   #Extract Data Frame
  Model$Trait <- gsub("([A-Z])([A-Z])", "\\1 \\2", Model$level)
  Model$Trait <- gsub("([a-z])([A-Z])", "\\1 \\2", Model$level)
  Model$Trait <- gsub("([0-9])([A-Z])", "\\1 \\2", Model$level)
  Model$Trait <- gsub("([a-z])([0-9])", "\\1 \\2", Model$level)
  Model$Trait <- gsub("([0-9])([a-z])", "\\1 \\2", Model$level)

  Model$Attribute <-gsub("([a-z])([A-Z])", "\\1 \\2", Model$feature)

  #Data Managment
  #for(i in seq(1:length(unique(df$Attribute)))){
   # df$Trait<- gsub(unique(df$Attribute)[i], "", df$Trait)


#  }

  #*************************** Manual Change ***************************
  #Manually Adjust Names
  if (!is.null(trait_renames)) {
    Model$Trait <- plyr::mapvalues(Model$Trait,
                                from = names(trait_renames),
                                to = trait_renames,
                                warn_missing = FALSE)
  }


  if (!is.null(attribute_renames)) {
    Model$Attribute <- plyr::mapvalues(Model$Attribute,
                                    from = names(attribute_renames),
                                    to = attribute_renames,
                                    warn_missing = FALSE)
  }

  #Model$Trait[Model$Trait %in% unique(subset(Model, Attribute == "Political Donations To Candidates")$Trait)] <- paste( unique(subset(Model, Attribute == "Political Donations To Candidates")$Trait), "Republican", sep = " ")

  #Model$Baseline <- "No"




  #*************************** Set Levels ***************************

  if (!is.null(trait_levels)) {
    Model$Trait <- factor(Model$Trait, levels = trait_levels)
  } else {
    Model$Trait <- factor(Model$Trait)
  }

  #*************************** Plot Formatting (Wide v Long) ***************************

  if (is.null(version)){
    Plot_Final <- ggplot(Model, aes(x = Trait, y = estimate, ymin = lower * 100, ymax = upper * 100)) +
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
    Plot_Final <- ggplot(Model, aes(y = Trait, x = estimate, xmin = lower, xmax = upper)) +
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
