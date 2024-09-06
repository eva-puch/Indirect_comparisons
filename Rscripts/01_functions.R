# This scrip gathered all the  R functions created and used across the various scripts.

#------------   Matching and calculation of SMD   ------------------------------

#' This function performs propensity score matching and calculates the standardized
#' mean differences (SMD) for variables before and after matching.
#' It also generates a plot to visualize these differences.
#'
#' @param treatment Character string representing the name of the treatment variable (binary variable 0/1).
#' @param ps Character string representing the name of the variable containing the propensity scores.
#' @param data Dataframe containing the dataset to be used for matching.
#' @param method Character string specifying the matching method to use (e.g., "nearest", "optimal", "full"). Default is "optimal".
#' @param ratio Numeric value specifying the matching ratio (number of control units to match with each treatment unit). Default is 1.
#' @param allvars Character vector of the names of all variables to include in the balance tables.
#' @param catvars Character vector of the names of categorical variables to include in the balance tables.
#' @param labels Character vector of names for the variables to display on the SMD plot.
#' @param strata Character string representing the name of the stratification variable for balance table calculations.
#'
#' Accessing the results
#' res$matched : data frame of matched data only
#' res$smdbe : SMD before matching
#' res$smdaf : SMD after matching

matched_smd <- function(treatment, ps, data, method = "optimal", ratio = 1,
                        allvars, catvars, labels, strata) {
  
  # Matching
  formula <- as.formula(paste(treatment, "~", ps))
  match <- matchit(formula, data = data, method = method, ratio = ratio)
  # Matched Data
  matched <- match.data(match)
  
  # Before Matching
  be <- CreateTableOne(vars = allvars, factorVars = catvars, strata = strata, data = data)
  smdbe <- as.vector(ExtractSmd(be, varLabels = TRUE))
  
  # After Matching
  af <- CreateTableOne(vars = allvars, factorVars = catvars, strata = strata, data = matched)
  smdaf <- as.vector(ExtractSmd(af, varLabels = TRUE))
  
  # R Outputs
  print(summary(match)) # Summary of matching
  print(kableone(print(be, showAllLevels = TRUE, smd = TRUE, formatOptions = list(big.mark = ",")))) # Before matching
  print(kableone(print(af, showAllLevels = TRUE, smd = TRUE, formatOptions = list(big.mark = ",")))) # After matching
  
  # SMD Graphs Before/After Matching
  SMD <- data.frame(variable = allvars, smdbe = smdbe, smdaf = smdaf)
  SMD$y <- seq(1, length(allvars), by = 1)
  minsmd <- min(smdbe, smdaf)
  maxsmd <- max(smdbe, smdaf)
  
  # Create plotly plot
  fig <- plot_ly(data = SMD) %>%
    # Add points for "Before Matching"
    add_markers(x = ~smdbe, y = ~y,
                marker = list(color = "#4D4D4D", size = 10, symbol = "circle-dot"),
                name = "No adjustment") %>%
    # Add points for "After Matching"
    add_markers(x = ~smdaf, y = ~y,
                marker = list(color = "#43CD80", size = 10, symbol = "x-dot"),
                name = "Matching") %>%
    # Add vertical lines for thresholds
    add_segments(x = ~c(0.1, 0.1), xend = ~c(0.1, 0.1),
                 y = ~c(min(SMD$y)-0.5, max(SMD$y)+0.5),
                 yend = ~c(max(SMD$y), min(SMD$y)),
                 line = list(dash = 'dash', color = "darkgrey"),
                 showlegend = FALSE) %>%
    add_segments(x = ~c(-0.1, -0.1), xend = ~c(-0.1, -0.1),
                 y = ~c(min(SMD$y-0.5), max(SMD$y)+0.5),
                 yend = ~c(max(SMD$y), min(SMD$y)),
                 line = list(dash = 'dash', color = "darkgrey"),
                 showlegend = FALSE) %>%
    # Customize layout
    layout(
      xaxis = list(title = "Standardized Mean Difference",
                   range = c(min(-0.2, minsmd-0.1), max(0.2, maxsmd+0.1)),
                   tickvals = seq(-1, 1, 0.1),
                   ticktext = format(seq(-1, 1, 0.1), digits = 1, nsmall = 1)),
      yaxis = list(title = "",
                   range = c(0.5, length(allvars) + 0.5),
                   tickvals = seq(1, length(allvars), 1),
                   ticktext = labels),
      legend = list(x = 1, y = 1, title = list(text = "Adjustment")),
      margin = list(l = 90, b = 90, t = 20, r = 20)  # Adjust margins
    )
  
  # Show the plot
  print(fig)
  
  # Return results
  list(matched = matched,
       smdbe = smdbe,
       smdaf = smdaf)
}

#------------   Distributions before and after matching   ----------------------

#' Plot distribution of a variable before and after matching.
#'
#' @param data Dataframe containing the dataset.
#' @param variable Character string representing the name of the variable to plot.
#' @param match_var Character string representing the name of the variable that contains the subclass matching IDs (used for filtering the post-matching data).
#' @param group Character string representing the name of the treatment group variable.
#' @param absname Character string specifying the label for the x-axis.
#' @param type Character string specifying the type of the variable to plot: "quanti" for continuous variables (default) or "quali" for categorical variables.
#' @param opac Numeric value (default = 0.75) specifying the opacity level for the plot elements.
#'
#' @return A side-by-side plot showing the distributions of the variable before (left) and after (right) matching.


match_distrib <- function(data, variable, match_var, group, absname, type = "quanti", opac = 0.5){
  data_be <- data
  data_af <- data[!is.na(data[[match_var]]),]
  
  if (type=="quanti") { # continuous variable (density)
    
    be <- ggplot(data_be, aes_string(x = variable, fill = group)) +
      geom_density(alpha = opac) +
      labs(x = absname, y = "") +
      theme_minimal() +
      scale_fill_manual(name = "Treatment",
                        values = c(colT1, colT2),
                        labels = c(T1name, T2name),
                        breaks = c("T1", "T2")) +
      theme(legend.position = "left") +
      guides(fill = guide_legend(title = "Treatment"))
    
    af <- ggplot(data_af, aes_string(x = variable, fill = group)) +
      geom_density(alpha = opac) +
      labs(x = absname, y = "") +
      theme_minimal() +
      scale_fill_manual(name = "Traitement",
                        values = c(colT1,colT2),
                        labels = c(T1name, T2name),
                        breaks = c("T1", "T2")) +
      theme(legend.position = "left") +
      guides(fill = guide_legend(title = "Treatment"))
  }
  
  if (type=="quali") { # qualitative variable (histogram)
    
    be <- ggplot(data_be, aes_string(x = variable, fill = group)) +
      geom_bar(position="dodge", alpha = opac) +
      labs(x = absname, y = "") +
      theme_minimal() +
      scale_fill_manual(name = "Treatment",
                        values = c(colT1, colT2),
                        labels = c(T1name, T2name),
                        breaks = c("T1", "T2")) +
      theme(legend.position = "left") +
      guides(fill = guide_legend(title = "Treatment"))
    
    af <- ggplot(data_af, aes_string(x = variable, fill = group)) +
      geom_bar(position="dodge", alpha = opac) +
      labs(x = absname, y = "") +
      theme_minimal() +
      scale_fill_manual(name = "Treatment",
                        values = c(colT1,colT2),
                        labels = c(T1name, T2name),
                        breaks = c("T1", "T2")) +
      theme(legend.position = "left") +
      guides(fill = guide_legend(title = "Treatment"))
  }
  # Same scale for both y-axis
  be_data <- ggplot_build(be)$data[[1]]
  max_density_be <- max(be_data$y)
  af_data <- ggplot_build(af)$data[[1]]
  max_density_af <- max(af_data$y)
  max_density <- max(max_density_be, max_density_af)
  
  be <- be + ylim(0, max_density)
  af <- af + ylim(0, max_density)
  
  be + af + plot_layout(ncol=2)
}

#------------   IPT Weighting (IPTW) and calculation of SMD   ------------------

#' This function performs Inverse Probability of Treatment Weighting (IPTW) based
#' on propensity scores and calculates the Standardized Mean Differences (SMD)
#' before and after weighting. It also generates a plot to visualize these differences.
#'
#' @param data Dataframe containing the dataset.
#' @param treatment Character string representing the name of the treatment variable, a binary variable where 0 = control and 1 = treatment.
#' @param ps Character string representing the name of the variable containing the propensity scores.
#' @param allvars Character vector of the names of all variables to include in the balance tables.
#' @param catvars Character vector of the names of categorical variables to include in the balance tables.
#' @param labels Character vector of names to be displayed on the graph for the respective variables.
#' @param strata Character string representing the name of the stratification variable for balance table calculations.
#'
#' Accessing the results
#'
#' res$weighted : data frame with the 'ps_weights' variable
#' res$weights : the weights obtained
#' res$smdbe : SMD before matching
#' res$smdaf : SMD after matching

iptw_smd <- function(data, treatment, ps, allvars, catvars, labels, strata) {
  
  # Before weighting
  be <- CreateTableOne(vars = allvars, factorVars = catvars, strata = strata, data = data)
  smdbe <- as.vector(ExtractSmd(be, varLabels = TRUE))
  
  # IPTW weights calculation
  data$ps_weights <- ifelse(data[[treatment]] == 1, 1 / data[[ps]], 1 / (1 - data[[ps]]))
  # Creation of a design survey
  design <- svydesign(ids = ~1, data = data, weights = ~ps_weights)
  # After weighting
  af <- svyCreateTableOne(vars = allvars, factorVars = catvars, strata = strata, data = design)
  smdaf <- as.vector(ExtractSmd(af, varLabels = TRUE))
  
  # R Outputs
  print(kableone(print(be, showAllLevels = TRUE, smd = TRUE, formatOptions = list(big.mark = ",")))) # before weighting
  print(kableone(print(af, showAllLevels = TRUE, smd = TRUE, formatOptions = list(big.mark = ",")))) # after weighting
  
  # SMD Graphs Before/After Matching
  SMD <- data.frame(allvars, smdbe = smdbe, smdaf = smdaf)
  SMD$y <- seq(1, length(allvars), by = 1)
  minsmd <- min(smdbe, smdaf)
  maxsmd <- max(smdbe, smdaf)
  
  # Create plotly plot
  fig <- plot_ly(data = SMD) %>%
    # Add points for "Before Matching"
    add_markers(x = ~smdbe, y = ~y,
                marker = list(color = "#4D4D4D", size = 10, symbol = "circle-dot"),
                name = "No adjustment") %>%
    # Add points for "After Matching"
    add_markers(x = ~smdaf, y = ~y,
                marker = list(color = "#FF6347", size = 10, symbol = "star-triangle-down"),
                name = "IPTW") %>%
    # Add vertical lines for thresholds
    add_segments(x = ~c(0.1, 0.1), xend = ~c(0.1, 0.1),
                 y = ~c(min(SMD$y)-0.5, max(SMD$y)+0.5),
                 yend = ~c(max(SMD$y), min(SMD$y)),
                 line = list(dash = 'dash', color = "darkgrey"),
                 showlegend = FALSE) %>%
    add_segments(x = ~c(-0.1, -0.1), xend = ~c(-0.1, -0.1),
                 y = ~c(min(SMD$y-0.5), max(SMD$y)+0.5),
                 yend = ~c(max(SMD$y), min(SMD$y)),
                 line = list(dash = 'dash', color = "darkgrey"),
                 showlegend = FALSE) %>%
    # Customize layout
    layout(
      xaxis = list(title = "Standardized Mean Difference",
                   range = c(min(-0.2, minsmd-0.1), max(0.2, maxsmd+0.1)),
                   tickvals = seq(-1, 1, 0.1),
                   ticktext = format(seq(-1, 1, 0.1), digits = 1, nsmall = 1)),
      yaxis = list(title = "",
                   range = c(0.5, length(allvars) + 0.5),
                   tickvals = seq(1, length(allvars), 1),
                   ticktext = labels),
      legend = list(x = 1, y = 1, title = list(text = "Adjustment")),
      margin = list(l = 90, b = 90, t = 20, r = 20)  # Adjust margins
    )
  
  # Show the plot
  print(fig)
  
  # Return results
  list(weighted = data,
       weights = data$ps_weights,
       smdbe = smdbe,
       smdaf = smdaf)
}

#------------   Distributions before and after IPTW   --------------------------

#' Plot distribution of a variable before and after IPTW
#'
#' @param data Dataframe containing the dataset.
#' @param variable Character string representing the name of the variable to plot.
#' @param weights Character string representing the name of the variable that contains the weights to use (IPT weights).
#' @param group Character string representing the name of the treatment group variable.
#' @param absname Character string specifying the label for the x-axis.
#' @param type Character string specifying the type of the variable to plot: "quanti" for continuous variables (default) or "quali" for categorical variables.
#' @param opac Numeric value (default = 0.75) specifying the opacity level for the plot elements.
#'
#' @return A side-by-side plot showing the distributions of the variable before (left) and after (right) matching.


iptw_distrib <- function(data, variable, weights, group, absname, type = "quanti", opac = 0.5){
  
  if (type=="quanti") { # continuous variable (density)
    
    be <- ggplot(data, aes_string(x = variable, fill = group)) +
      geom_density(alpha = opac) +
      labs(x = absname, y = "") +
      theme_minimal() +
      scale_fill_manual(name = "Treatment",
                        values = c(colT1, colT2),
                        labels = c(T1name, T2name),
                        breaks = c("T1", "T2")) +
      theme(legend.position = "left") +
      guides(fill = guide_legend(title = "Treatment"))
    
    af <- ggplot(data, aes_string(x = variable, fill = group, weight = weights)) +
      geom_density(alpha = opac) +
      labs(x = absname, y = "") +
      theme_minimal() +
      scale_fill_manual(name = "Traitement",
                        values = c(colT1,colT2),
                        labels = c(T1name, T2name),
                        breaks = c("T1", "T2")) +
      theme(legend.position = "left") +
      guides(fill = guide_legend(title = "Treatment"))
  }
  
  if (type=="quali") { # qualitative variable (histogram)
    
    be <- ggplot(data, aes_string(x = variable, fill = group)) +
      geom_bar(position="dodge", alpha = opac) +
      labs(x = absname, y = "") +
      theme_minimal() +
      scale_fill_manual(name = "Treatment",
                        values = c(colT1, colT2),
                        labels = c(T1name, T2name),
                        breaks = c("T1", "T2")) +
      theme(legend.position = "left") +
      guides(fill = guide_legend(title = "Treatment"))
    
    af <- ggplot(data, aes_string(x = variable, fill = group, weight = weights)) +
      geom_bar(position="dodge", alpha = opac) +
      labs(x = absname, y = "") +
      theme_minimal() +
      scale_fill_manual(name = "Treatment",
                        values = c(colT1,colT2),
                        labels = c(T1name, T2name),
                        breaks = c("T1", "T2")) +
      theme(legend.position = "left") +
      guides(fill = guide_legend(title = "Treatment"))
  }
  
  # Same scale for both y-axis
  be_data <- ggplot_build(be)$data[[1]]
  max_density_be <- max(be_data$y)
  af_data <- ggplot_build(af)$data[[1]]
  max_density_af <- max(af_data$y)
  max_density <- max(max_density_be, max_density_af)
  
  be <- be + ylim(0, max_density)
  af <- af + ylim(0, max_density)
  
  be + af + plot_layout(ncol=2)
}
