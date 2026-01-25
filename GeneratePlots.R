library(ggplot2)
library(tidyverse)
library(cowplot)
library(Hmisc)
library(ggh4x)
library(ggthemes)
library(data.table)
library(sf)
library(vroom)
library(glue)
library(fs)
library(purrr)
library(tibble)
library(dplyr)

if (!dir.exists("Manuscript_Vis")) {
  dir.create("Manuscript_Vis")
}

# Set up for toy example visualization
DATA_ROOT <- Sys.getenv("DATA_ROOT", "Ts_Data")          # default: Ts_Data
VIS_ROOT  <- Sys.getenv("VIS_ROOT",  "Manuscript_Vis")   # default: Manuscript_Vis
UP_TO     <- trimws(tolower(Sys.getenv("UP_TO", "all"))) # default: all

if (!dir.exists(VIS_ROOT)) dir.create(VIS_ROOT, recursive = TRUE)


tryCatch({

# Load Data ---------------------------------------------------------------
# Read in all files resulting from the ABM
files <- list.files(path = DATA_ROOT, pattern = "\\.csv$", full.names = TRUE)

# Data frame containing summary statistics for all experimental settings
df_full <- data.frame()

for (fname in files){
  
  # Read in single dataset
  AllSimResultsDF <- read.csv(fname)
  AllSimResultsDF
  
  # Preprocessing
  # Set colouring for the different scenarios
  cols <- c(
    "SSP1-2.6" = "#009E73",
    "SSP2-4.5" = "#F0E442",
    "SSP5-8.5" = "#D55E00"
  )
  
  # Rename scenarios from good, ok, bad, to actual model names
  AllSimResultsDF <- AllSimResultsDF %>%
    dplyr::mutate(
      Scenario = recode(
        Scenario,
        "good"     = "SSP1-2.6",
        "ok"       = "SSP2-4.5",
        "bad"      = "SSP5-8.5",
        .default   = Scenario
      ),
      Scenario = factor(Scenario, levels = names(cols))
    )
  
  # Combine
  scale_cols  <- scale_color_manual(
    values = cols, breaks = names(cols), drop = FALSE)
  scale_fills <- scale_fill_manual(
    values = cols,  breaks = names(cols), drop = FALSE)
  
  # Extract the experimental setting of the current dataset from its file name
  stem <- tools::file_path_sans_ext(basename(fname))
  
  rx <-
    "^FOREC_(.*?)_ADOP_(TRUE|FALSE)_FODD_(TRUE|FALSE)_PROPCONS_([0-9.]+)_FORECERR_([0-9.]+)(?:_beta_([0-9.]+))?$"
  
  m <- regexec(rx, stem)
  mm <- regmatches(stem, m)[[1]]
  if (length(mm) == 0) stop("Filename doesn't match expected pattern: ", stem)
  
  Forecasts                  <- mm[2]
  Set_Adoption               <- (mm[3] == "TRUE")
  Supplemental_fodder        <- (mm[4] == "TRUE")
  Starting_Prop_Conservation <- as.numeric(mm[5])
  ForecastError              <- as.numeric(mm[6])
  Beta                       <- ifelse(
    length(mm) >= 7 && nzchar(mm[7]), as.numeric(mm[7]), 0.1)
  
  # Set this as title string to use in later plots
  title_string <- paste0(
    "Forecast: ", Forecasts,
    ", Adoption: ", ifelse(Set_Adoption, "Yes", "No"),
    ", Supplemental Fodder: ", ifelse(Supplemental_fodder, "Yes", "No"),
    ", Starting Prop. Conservation: ", Starting_Prop_Conservation,
    ", Forecast Error: ", ForecastError,
    ", Beta: ", Beta
  )
  title_string
  
  # Add the experimental settings as columns
  AllSimResultsDF$Forecasts                  <- Forecasts
  AllSimResultsDF$Set_Adoption               <- Set_Adoption
  AllSimResultsDF$Supplemental_fodder        <- Supplemental_fodder
  AllSimResultsDF$Starting_Prop_Conservation <- Starting_Prop_Conservation
  AllSimResultsDF$ForecastError              <- ForecastError
  AllSimResultsDF$Beta              <- Beta
  
  # Append to the combined df
  df_full <- rbind(df_full, AllSimResultsDF)
  
  
  # Visualization of ABM Timeseries ---------------------------------------------
  
  SummaryStats <- AllSimResultsDF %>%
    group_by(
      Scenario, Time,
      Forecasts, Set_Adoption, Supplemental_fodder,
      Starting_Prop_Conservation, ForecastError
    ) %>%
    dplyr::summarize(
      across(
        c(AvgMoney, AvgCows, AvgGrass, Gini),
        list(mean = ~mean(.x, na.rm = TRUE), sd = ~sd(.x, na.rm = TRUE)),
        .names = "{.col}_{.fn}"
      ),
      .groups = "drop"
    )
  
  ts_long <- SummaryStats %>%
    dplyr::select(Scenario, Time,
                  AvgMoney_mean, AvgMoney_sd,
                  AvgCows_mean,  AvgCows_sd,
                  AvgGrass_mean, AvgGrass_sd,
                  Gini_mean,     Gini_sd) %>%
    pivot_longer(
      cols = -c(Scenario, Time),
      names_to = c("Variable", "stat"),
      names_pattern = "^(.*)_(mean|sd)$",
      values_to = "value"
    ) %>%
    pivot_wider(names_from = stat, values_from = value) %>%
    dplyr::mutate(Variable = sub("^Avg", "", Variable))
  
  ts_long <- ts_long %>%
    dplyr::mutate(Variable = factor(
      Variable,
      levels = c("Cows", "Grass", "Money", "Gini")
    ))
  
  burn_in_line <- geom_vline(xintercept = 70, linetype = "dashed", color = "black", size = 0.8)
  burn_in_text <- annotate("text", x = 35, y = Inf, label = "Model \n Burn-In",
                           vjust = 1.5, hjust = 0.5, size = 7, fontface = "bold")
  
  # Panel titles and per-panel y-axis labels
  panel_map <- tibble::tibble(
    Variable    = c("Cows", "Grass", "Money", "Gini"),
    panel_title = c("Livestock numbers", "Vegetation cover", "Economic wellbeing", "Wealth inequality"),
    ylab        = c(
      "Mean livestock number per individual",
      "Mean vegetation cover per plot",
      "Mean economic wellbeing per individual",
      "Gini index across individuals' wealth"
    )
  )
  
  # Helper to build a single panel
  make_panel <- function(var, panel_title, ylab) {
    df <- ts_long %>% dplyr::filter(Variable == var)
    
    bottom_row <- var %in% c("Money", "Gini")
    
    ggplot(df, aes(x = Time, y = mean, color = Scenario, fill = Scenario)) +
      geom_ribbon(aes(ymin = mean - sd, ymax = mean + sd), alpha = 0.20, color = NA) +
      geom_line(linewidth = 0.9) +
      burn_in_line + burn_in_text +
      labs(
        x = NULL,
        y = ylab,
        title = panel_title,
        color = "Precipitation scenario", fill = "Precipitation scenario"
      ) +
      scale_cols + scale_fills +
      ggthemes::theme_clean() +
      theme(
        legend.position   = "none",
        plot.title        = element_text(face = "bold", size = 14, hjust = 0.5),
        panel.grid.minor  = element_blank(),
        axis.text.x       = if (bottom_row) element_text(angle = 45, hjust = 1) else element_blank(),
        axis.ticks.x      = if (bottom_row) element_line() else element_blank(),
        axis.title.x      = element_blank(),
        axis.text         = element_text(size = 12),
        axis.title.y      = element_text(size = 12),
        panel.background  = element_blank(),
        plot.background   = element_blank(),
        panel.border      = element_blank(),
        plot.margin       = margin(0, 0, 0, 0)
      )
  }
  
  # Build all four panels
  panels <- purrr::pmap(panel_map, ~ make_panel(..1, ..2, ..3))
  
  # Shared legend in theme_clean() style
  legend_plot <- ggplot(ts_long, aes(Time, mean, color = Scenario, fill = Scenario)) +
    geom_line() + scale_cols + scale_fills +
    labs(color = "Precipitation scenario", fill = "Precipitation scenario") +
    ggthemes::theme_clean() +
    theme(
      legend.position = "bottom",
      strip.background = element_rect(),
      strip.text = element_text(face = "bold", size = 14),
      legend.text = element_text(size = 12),
      legend.title = element_text(size = 14, face = "bold")
    )
  shared_legend <- cowplot::get_legend(legend_plot)
  
  # Arrange the four panels into a 2x2 grid
  grid_panels <- cowplot::plot_grid(
    panels[[1]], panels[[2]],
    panels[[3]], panels[[4]],
    ncol = 2, align = "hv"
  )
  
  # x-axis label
  xlab_shared <- cowplot::ggdraw() +
    cowplot::draw_label(
      "Iterations",
      x = 0.5, y = 0.5, hjust = 0.5, vjust = 0.5,
      fontface = "plain", size = 14
    ) +
    theme(plot.margin = margin(4, 0, 4, 0))
  
  # Stack grid, shared x-axis label, and legend
  ABMTs_res <- cowplot::plot_grid(
    grid_panels,
    xlab_shared,
    shared_legend,
    ncol = 1,
    rel_heights = c(1, 0.06, 0.10)
  )
  
  # Add a thin outer border around the entire figure (grid + x label + legend)
  ABMTs_res <- cowplot::ggdraw(ABMTs_res) +
    theme(
      plot.background = element_rect(color = "black", size = 0.5, fill = NA),
      plot.margin = margin(10, 10, 10, 10)
    )
  
  print(ABMTs_res)
  
  ggsave(
    # filename = file.path(VIS_ROOT, paste0(fname, ".png")),
    file.path(
      VIS_ROOT,
      paste0(tools::file_path_sans_ext(basename(fname)), ".png")
    ),
    plot = ABMTs_res,
    width = 10, height = 8, dpi = 300
  )
  
  # Save combined dataframe of all outcome datasets post burn in phase
  # df_full_last30 <- df_full %>%
  #   dplyr::ungroup() %>%
  #   dplyr::filter(Time > max(Time) - 30) %>%
  #   dplyr::select(-c(
  #     TotalCows,
  #     TotalGrass,
  #     Time, 
  #     Rep)) 
  
  cutoff_time <- max(df_full$Time, na.rm = TRUE) - 30
  
  df_full_last30 <- df_full[df_full$Time > cutoff_time, , drop = FALSE]
  
  df_full_last30 <- df_full_last30[
    , !names(df_full_last30) %in% c("TotalCows", "TotalGrass", "Time", "Rep"),
    drop = FALSE
  ]
  
  
  write.csv(
    df_full_last30,
    file = "CombinedData_PostBurnIN.csv",
    row.names = FALSE
  )
  
}

# Toy example stop
if (identical(UP_TO, "exp1")) {
  message("Toy example stopping (UP_TO=", UP_TO, ").")
  stop("EARLY_EXIT")                   # caught by the tryCatch; no error shown
}

# Exp 1 -------------------------------------------------------------------
df_Exp1 <- read_csv("CombinedData_PostBurnIN.csv")
df_Exp1 <- df_Exp1 %>%
  dplyr::filter(
    Forecasts == "No_one",
    Set_Adoption == TRUE,
    Supplemental_fodder == FALSE,
    Starting_Prop_Conservation == 0.1,
    ForecastError==0
  )

outcome_vars <- c("AvgMoney", "AvgCows", "AvgGrass", "Gini")

# Normalize both mean and sd columns by median outcome values in SSP2-4.5 scenario
ok_median <- df_Exp1 %>%
  dplyr::filter(Scenario == "SSP2-4.5") %>%
  summarise(across(all_of(outcome_vars), median, na.rm = TRUE))

normalized_df <- df_Exp1 %>%
  mutate(across(all_of(outcome_vars),
                ~ . / ok_median[[cur_column()]],
                .names = "{.col}_normalized"))

# Reshape for plotting
plot_df <- normalized_df %>%
  pivot_longer(cols = ends_with("_normalized"),
               names_to = "Variable",
               values_to = "Value") %>%
  mutate(Variable = str_remove(Variable, "_normalized"),
         Scenario = factor(
           Scenario, levels = c("SSP1-2.6", "SSP2-4.5", "SSP5-8.5")))

# Coloured boxplot
Exp1_boxplot <- ggplot(plot_df, aes(x = Scenario, y = Value, fill = Scenario)) +
  geom_boxplot() +
  geom_hline(yintercept = 1, linetype = "dashed", color = "gray30", linewidth = 0.9) +
  facet_wrap(~ Variable, scales = "free_y",
             # labeller = labeller(Variable = function(x) sub("^Avg", "", x)),
             labeller = labeller(
               Variable = c(
                 AvgMoney = "Economic wellbeing",
                 AvgCows = "Livestock numbers",
                 AvgGrass = "Vegetation cover",
                 Gini = "Wealth inequality"
               )
             )) +
  scale_fill_manual(values = c(
    "SSP1-2.6"     = "#009E73",
    "SSP2-4.5"       = "#F0E442",
    "SSP5-8.5"      = "#D55E00"
  )) +
  labs(
    # title = "Outcomes under Baseline Setting (no forecasts, no supplemental food, 10% conservation) per climate scenario",
    x = "Precipitation scenario",
    y = "Normalized outcome") +
  ggthemes::theme_clean()+
  theme(
    legend.position = "none",
    strip.text = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 16)
  )


Exp1_boxplot

ggsave(
  filename = file.path(VIS_ROOT, "Exp1_boxplot.png"),
  plot = Exp1_boxplot,
  width = 10, height = 8, dpi = 300
)

# Exp 2 -------------------------------------------------------------------

## Normalized by SSP2-4.5 --------------------------------------------------
# Effects of conservation interventions on outcomes (conservation interventions 
# in different extents)

df_Exp2 <- read_csv("CombinedData_PostBurnIN.csv") %>%
  dplyr::filter(
    Forecasts %in% c("No_one", "Conservation"),
    Set_Adoption == TRUE,
    Supplemental_fodder %in% c(TRUE, FALSE),
    Starting_Prop_Conservation %in% c(0.1, 0.25, 0.5, 0.75, 0.9),
    ForecastError == 0
  )

outcome_vars <- c("AvgMoney", "AvgCows", "AvgGrass", "Gini")

df_long <- df_Exp2 %>%
  mutate(
    Forecasts_group = factor(Forecasts, levels = c("No_one", "Conservation")),
    Scenario = factor(Scenario)
  ) %>%
  pivot_longer(
    cols = all_of(outcome_vars),
    names_to = "Variable",
    values_to = "RawValue"
  )

# Normalize by median of no intervention (no fodder, no forecast) within Scenario SSP2-4.5, 
# by variable
baseline_df <- df_long %>%
  dplyr::filter(Scenario == "SSP2-4.5") %>%
  dplyr::group_by(Variable) %>%
  dplyr::summarise(
    Baseline = median(RawValue, na.rm = TRUE),
    .groups = "drop"
  )

df_long <- df_long %>%
  left_join(baseline_df, by = c("Variable")) %>%
  mutate(Value = RawValue / Baseline) %>%
  filter(is.finite(Value))

df_long

# Linear Interpolation
Exp2_LinInt <- ggplot(df_long, aes(
  x = Starting_Prop_Conservation, y = Value,
  color = interaction(Forecasts_group, Supplemental_fodder, sep = ".", drop = TRUE)
)) +
  geom_hline(yintercept = 1, linetype = "dashed", linewidth = 1, color = "#525252") +
  
  # error bars at observed x: mean ± 1 SD
  stat_summary(
    fun.data = mean_sdl, fun.args = list(mult = 1),
    geom = "errorbar", width = 0.02, linewidth = 0.6, lineend = "round",
    na.rm = TRUE, alpha=0.7
  ) +
  
  # mean points
  stat_summary(fun = mean, geom = "point", size = 2.2, na.rm = TRUE,  alpha=0.7) +
  # piecewise-linear interpolation between means
  stat_summary(fun = mean, geom = "line", linewidth = 0.9, na.rm = TRUE,  alpha=0.7) +
  
  ggh4x::facet_grid2(
    rows = vars(Variable),
    cols = vars(Scenario),
    scales = "free_y",
    independent = "y",
    labeller = labeller(
      # Scenario = label_value,
      # Variable = function(x) sub("^Avg", "", x)
      Scenario = c(
        "SSP1-2.6" = "SSP1-2.6\n(sustainability)",
        "SSP2-4.5" = "SSP2-4.5\n(middle-of-the-road)",
        "SSP5-8.5" = "SSP5-8.5\n(fossil fuel development)"
      ),
      Variable = c(
        AvgMoney = "Economic\nwellbeing",
        AvgCows = "Livestock\nnumbers",
        AvgGrass = "Vegetation\ncover",
        Gini = "Wealth\ninequality"
      )
    )
  ) +
  theme(
    strip.background = element_rect(fill = "grey90"),
    strip.text = element_text(face = "bold"),
    strip.placement = "outside",
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    panel.spacing.x = unit(1.4, "lines"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  labs(
    x = "Proportion of communities in conservation",
    y = "Normalized outcome",
    # title = "Effects of conservation interventions under different climate scenarios"
  ) +
  scale_color_manual(
    name   = "Programmatic configuration",
    breaks = c("Conservation.TRUE", "Conservation.FALSE",
               "No_one.TRUE",      "No_one.FALSE"),
    labels = c("Forecast + fodder",
               "Forecast only",
               "Fodder only",
               "Neither"),
    values = c(
      "Conservation.TRUE"  = "#1b9e77",
      "Conservation.FALSE" = "#d95f02",
      "No_one.TRUE"        = "#7570b3",
      "No_one.FALSE"       = "#e7298a"
    )
  ) +
  guides(color = guide_legend(order = 1))  +
  ggthemes::theme_clean()+
  theme(
    legend.position = "bottom",
    strip.text = element_text(size = 14, face = "bold"),
    panel.spacing = unit(2, "lines"),
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 16)
  ) +
  ggh4x::facetted_pos_scales(
    y = list(
      Variable == "AvgMoney" ~ scale_y_continuous(limits = c(-0.7, 2.1), n.breaks = 4),
      Variable == "AvgCows"  ~ scale_y_continuous(limits = c(0, 2), n.breaks = 4),
      Variable == "AvgGrass" ~ scale_y_continuous(limits = c(0.2, 1.1), n.breaks = 4),
      Variable == "Gini"     ~ scale_y_continuous(limits = c(0.15, 1.5), n.breaks = 4)
    )
  )

Exp2_LinInt

ggsave(
  filename = file.path(VIS_ROOT, "Exp2_lineplot.png"),
  plot = Exp2_LinInt,
  width = 10, height = 8, dpi = 300
)


## Trade-Offs 50% SSP2-4.5---------------------------------------------
# Visualization of outcome Trade-Offs

df_Exp2b <- read_csv("CombinedData_PostBurnIN.csv") %>%
  dplyr::filter(
    Forecasts %in% c("No_one", "Conservation"),
    Set_Adoption == TRUE,
    Supplemental_fodder %in% c(TRUE, FALSE),
    Starting_Prop_Conservation %in% c(0.5),
    ForecastError == 0,
    Scenario == "SSP2-4.5"
  )

outcome_vars <- c("AvgMoney", "AvgCows", "AvgGrass", "Gini")
axis_labels <- c(
  Gini     = "Wealth inequality",
  AvgMoney = "Economic wellbeing",
  AvgGrass = "Vegetation cover",
  AvgCows  = "Livestock numbers"
)

df_heat_in <- df_Exp2b %>%
  mutate(
    Config = case_when(
      Forecasts == "Conservation" & Supplemental_fodder ~ "Forecast + fodder",
      Forecasts == "Conservation" & !Supplemental_fodder ~ "Forecast only",
      Forecasts == "No_one"       & Supplemental_fodder  ~ "Fodder only",
      Forecasts == "No_one"       & !Supplemental_fodder ~ "Neither"
    ),
    Config = factor(Config, levels = c(
      "Forecast + fodder", "Forecast only", "Fodder only", "Neither"
    ))
  )

# fix order
desired_order <- c("AvgCows", "AvgGrass", "AvgMoney", "Gini")
lvl <- unname(axis_labels[desired_order])

cor_long <- df_heat_in %>% group_by(Config) %>%
  group_modify(~{
    m <- cor(.x[, outcome_vars],
             use = "pairwise.complete.obs",
             method = "pearson")
    as_tibble(m, rownames = "VarY") %>%
      pivot_longer(-VarY, names_to = "VarX", values_to = "r")
  }) %>%
  ungroup() %>%
  mutate(
    VarX = factor(VarX, levels = outcome_vars, labels = lvl),
    VarY = factor(VarY, levels = outcome_vars, labels = lvl),
    iX = match(as.character(VarX), lvl),
    iY = match(as.character(VarY), lvl)
  ) %>%
  filter(iY <= iX) # to only show lower triangle

Tradeoff_Heatmaps <- ggplot(cor_long, aes(VarX, VarY, fill = r)) +
  geom_tile(color = "grey90", linewidth = 0.4) +
  geom_text(aes(label = sprintf("%.2f", r)), size = 3) +
  coord_equal() +
  facet_wrap(~ Config, nrow = 1) +
  scale_fill_gradient2(
    name   = "Pearson correlation",
    limits = c(-1, 1),
    breaks = seq(-1, 1, by = 0.5),
    low = "#FFC20A", mid = "white", high = "#0C7BDC"
  ) +
  labs(
    x = NULL, y = NULL
    # ,
    # title = "Outcome trade-offs under 50% community conservation in the SSP2-4.5 climate scenario"
  ) +
  ggthemes::theme_clean() +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "bold", size = 12),
    panel.spacing = unit(1, "lines"),
    legend.margin = margin(10, 10, 10, 10) 
  ) 

Tradeoff_Heatmaps

ggsave(
  filename = file.path(VIS_ROOT, "Exp2_tradeoff_heatmaps.png"),
  plot = Tradeoff_Heatmaps,
  width = 10, height = 5, dpi = 300
)

# Exp 3 -------------------------------------------------------------------

# Impact of programmatic decisions on outcomes
df_Exp3 <- read_csv("CombinedData_PostBurnIN.csv")

df_Exp3 <- df_Exp3 %>%
  dplyr::filter(
    Forecasts %in% c("No_one", "Conservation"),
    Set_Adoption == FALSE,
    Supplemental_fodder %in% c(TRUE, FALSE),
    Starting_Prop_Conservation == 0.25,
    ForecastError==0,
    Beta == 0.1
  )

df_Exp3 %>% distinct(Forecasts, Supplemental_fodder, Beta)

# Outcome var of interest
outcome_vars <- c("PropCons")

df_long <- df_Exp3 %>%
  mutate(
    Forecasts_group = factor(Forecasts, levels = c("No_one", "Conservation")),
    Scenario = factor(
      Scenario,
      levels = c("SSP1-2.6", "SSP2-4.5", "SSP5-8.5"),
      labels = c(
        "SSP1-2.6\n(sustainability)",
        "SSP2-4.5\n(middle-of-the-road)",
        "SSP5-8.5\n(fossil fuel development)"
      )
    )
  ) %>%
  pivot_longer(
    cols = all_of(outcome_vars),
    names_to = "Variable",
    values_to = "RawValue"
  )

# normalize prop cons by no intervention (no fodder, no forecast) in SSP2-4.5
baseline_df <- df_long %>%
  filter(Supplemental_fodder == FALSE, Forecasts_group == "No_one", 
         Scenario == "SSP2-4.5\n(middle-of-the-road)") %>%
  group_by(Scenario, Variable) %>%
  summarise(
    Baseline = median(RawValue, na.rm = TRUE), # normalizing by median
    .groups = "drop"
  ) %>%
  dplyr::select(-Scenario)

plot_df <- df_long %>%
  filter(Variable == "PropCons") %>%
  left_join(baseline_df, by = c("Variable")) %>%
  mutate(
    Value = RawValue / Baseline,
    Variable = factor(Variable, levels = "PropCons", labels = "Conservation\nengagement"),
    Forecasts2 = dplyr::recode(Forecasts, Conservation = "Conservation", .default = Forecasts),
    Forecasts2 = factor(Forecasts2, levels = c("No_one", "Conservation")),
    FodderFacet = ifelse(Supplemental_fodder, "Fodder = TRUE", "Fodder = FALSE")
  )

plot_df2 <- plot_df %>%
  mutate(
    Combo = interaction(Forecasts2, Supplemental_fodder, sep = ".", drop = TRUE),
    Combo = factor(Combo, levels = c(
      "Conservation.TRUE", "Conservation.FALSE",
      "No_one.TRUE",       "No_one.FALSE"
    ))
  )

Exp3_plot <- ggplot(plot_df2, aes(x = Combo, y = Value, fill = Combo)) +
  geom_boxplot() +
  geom_hline(yintercept = 1, linetype = "dashed", color = "gray30", linewidth = 0.9) +
  facet_wrap(~ Scenario, nrow = 1) + 
  scale_fill_manual(
    name   = "Programmatic configuration",
    breaks = c("Conservation.TRUE", "Conservation.FALSE",
               "No_one.TRUE",       "No_one.FALSE"),
    labels = c("Forecast + fodder",
               "Forecast only",
               "Fodder only",
               "Neither"),
    values = c(
      "Conservation.TRUE"  = "#1b9e77",
      "Conservation.FALSE" = "#d95f02",
      "No_one.TRUE"        = "#7570b3",
      "No_one.FALSE"       = "#e7298a"
    )
  ) +
  labs(
    #title = "Conservation uptake under varying conservation interventions and climate scenarios",
    x = "Forecast",
    y = "Normalized conservation engagement"
  ) +
  ggthemes::theme_clean() +
  theme(
    legend.position = "bottom",
    strip.text = element_text(size = 14, face = "bold"),
    panel.spacing = unit(2, "lines"),
    axis.title.x = element_blank(),
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 16),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )

Exp3_plot

ggsave(
  filename = file.path(VIS_ROOT, "Exp3_boxplot.png"),
  plot = Exp3_plot,
  width = 10, height = 8, dpi = 300
)


# Supplemental ------------------------------------------------------------


## Exp 3 - other outcome variables ------------------------------------------
# adapted visualisation from experiment 3

df_Exp3 <- read_csv("CombinedData_PostBurnIN.csv") %>%
  dplyr::filter(
    Forecasts %in% c("No_one", "Conservation"),
    Set_Adoption == FALSE,
    Supplemental_fodder %in% c(TRUE, FALSE),
    Starting_Prop_Conservation == 0.25,
    ForecastError==0,
    Beta == 0.1
  )

df_Exp3 %>% distinct(Forecasts, Supplemental_fodder, Beta)

outcome_vars <- c("AvgMoney", "AvgCows", "AvgGrass", "Gini"
                  , "PropCons"
)

df_long <- df_Exp3 %>%
  mutate(
    Forecasts_group = factor(Forecasts, levels = c("No_one", "Conservation")),
    Scenario = factor(
      Scenario,
      levels = c("SSP1-2.6", "SSP2-4.5", "SSP5-8.5"),
      labels = c(
        "SSP1-2.6\n(sustainability)",
        "SSP2-4.5\n(middle-of-the-road)",
        "SSP5-8.5\n(fossil fuel development)"
      )
    )
  ) %>%
  pivot_longer(
    cols = all_of(outcome_vars),
    names_to = "Variable",
    values_to = "RawValue"
  )

baseline_df <- df_long %>%
  filter(Supplemental_fodder == FALSE, Forecasts_group == "No_one",
         Scenario == "SSP2-4.5\n(middle-of-the-road)") %>%
  group_by(Scenario, Variable) %>%
  summarise(
    Baseline = median(RawValue, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::select(-Scenario)

plot_df <- df_long %>%
  filter(Variable %in% outcome_vars) %>%
  left_join(baseline_df, by = c("Variable")) %>%
  mutate(
    Value = RawValue / Baseline,
    Forecasts2 = dplyr::recode(Forecasts, Conservation = "Conservation", .default = Forecasts),
    Forecasts2 = factor(Forecasts2, levels = c("No_one", "Conservation")),
    FodderFacet = ifelse(Supplemental_fodder, "Fodder = TRUE", "Fodder = FALSE")
  )

plot_df2 <- plot_df %>%
  mutate(
    Combo = interaction(Forecasts2, Supplemental_fodder, sep = ".", drop = TRUE),
    Combo = factor(Combo, levels = c(
      "Conservation.TRUE", "Conservation.FALSE",
      "No_one.TRUE",       "No_one.FALSE"
    ))
  )

Exp3_othervars_plot <- ggplot(plot_df2, aes(x = Combo, y = Value, fill = Combo)) +
  geom_boxplot() +
  geom_hline(yintercept = 1, linetype = "dashed", color = "gray30", linewidth = 0.9) +
  facet_grid(rows = vars(Variable), cols = vars(Scenario)) +
  scale_fill_manual(
    name   = "Programmatic configuration",
    breaks = c("Conservation.TRUE", "Conservation.FALSE",
               "No_one.TRUE",       "No_one.FALSE"),
    labels = c("Forecast + fodder",
               "Forecast only",
               "Fodder only",
               "Neither"),
    values = c(
      "Conservation.TRUE"  = "#1b9e77",
      "Conservation.FALSE" = "#d95f02",
      "No_one.TRUE"        = "#7570b3",
      "No_one.FALSE"       = "#e7298a"
    )
  ) +
  labs(
    x = "Forecast",
    y = "Normalized outcome"
  ) +
  ggthemes::theme_clean() +
  theme(
    legend.position = "bottom",
    strip.text = element_text(size = 12, face = "bold"),
    panel.spacing = unit(2, "lines"),
    axis.title.x = element_blank(),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )+
  ggh4x::facet_grid2(
    rows = vars(Variable),
    cols = vars(Scenario),
    scales = "free_y"
  ) + facet_grid(
    rows = vars(Variable),
    cols = vars(Scenario),
    scales = "free_y",
    labeller = labeller(Variable = c(
      Gini     = "Wealth\ninequality",
      AvgMoney = "Economic\nwellbeing",
      AvgGrass = "Vegetation\ncover",
      AvgCows  = "Livestock\nnumbers",
      PropCons = "Conservation\nengagement"
    ))
  )

Exp3_othervars_plot


ggsave(
  filename = file.path(VIS_ROOT, "Exp3_othervars_boxplot.png"),
  plot = Exp3_othervars_plot,
  width = 12, height = 14, dpi = 300
)

## Payoff bias sensitivity ----------------------------------------

# Impact of programmatic decisions on outcomes
df_Exp3 <- readr::read_csv("CombinedData_PostBurnIN.csv")

df_Exp3 <- df_Exp3 %>%
  dplyr::filter(
    Forecasts %in% c("No_one", "Conservation"),
    Set_Adoption == FALSE,
    Supplemental_fodder %in% c(TRUE, FALSE),
    Starting_Prop_Conservation == 0.25,
    ForecastError == 0,
    Beta %in% seq(0.1, 0.5, by = 0.1)
  )

# Outcome var of interest
outcome_vars <- c("PropCons")

df_long <- df_Exp3 %>%
  dplyr::mutate(
    Forecasts_group = factor(Forecasts, levels = c("No_one", "Conservation")),
    Scenario = factor(
      Scenario,
      levels = c("SSP1-2.6", "SSP2-4.5", "SSP5-8.5"),
      labels = c(
        "SSP1-2.6\n(sustainability)",
        "SSP2-4.5\n(middle-of-the-road)",
        "SSP5-8.5\n(fossil fuel development)"
      )
    ),
    Beta_lab = factor(
      Beta,
      levels = seq(0.1, 0.5, by = 0.1),
      labels = paste0("\u03b2 = ", seq(0.1, 0.5, by = 0.1))
    )
  ) %>%
  tidyr::pivot_longer(
    cols = tidyselect::all_of(outcome_vars),
    names_to = "Variable",
    values_to = "RawValue"
  )

# normalize prop cons by no intervention (no fodder, no forecast) in SSP2-4.5 ACROSS BETAS
baseline_df <- df_long %>%
  dplyr::filter(
    Supplemental_fodder == FALSE,
    Forecasts_group == "No_one",
    Scenario == "SSP2-4.5\n(middle-of-the-road)"
  ) %>%
  dplyr::group_by(Scenario, Variable) %>%
  dplyr::summarise(Baseline = median(RawValue, na.rm = TRUE), .groups = "drop") %>%
  dplyr::select(-Scenario)

plot_df <- df_long %>%
  dplyr::filter(Variable == "PropCons") %>%
  dplyr::left_join(baseline_df, by = c("Variable")) %>%
  dplyr::mutate(
    Value = RawValue / Baseline,
    Variable = factor(Variable, levels = "PropCons", labels = "Conservation\nengagement"),
    Forecasts2 = dplyr::recode(Forecasts, Conservation = "Conservation", .default = Forecasts),
    Forecasts2 = factor(Forecasts2, levels = c("No_one", "Conservation")),
    FodderFacet = ifelse(Supplemental_fodder, "Fodder = TRUE", "Fodder = FALSE")
  )

plot_df2 <- plot_df %>%
  dplyr::mutate(
    Combo = interaction(Forecasts2, Supplemental_fodder, sep = ".", drop = TRUE),
    Combo = factor(Combo, levels = c(
      "Conservation.TRUE", "Conservation.FALSE",
      "No_one.TRUE",       "No_one.FALSE"
    ))
  )

Exp3_plot <- ggplot2::ggplot(plot_df2, ggplot2::aes(x = Combo, y = Value, fill = Combo)) +
  ggplot2::geom_boxplot() +
  ggplot2::geom_hline(yintercept = 1, linetype = "dashed", color = "gray30", linewidth = 0.9) +
  ggplot2::facet_grid(rows = ggplot2::vars(Scenario),
                      cols  = ggplot2::vars(Beta_lab)) +
  ggplot2::scale_fill_manual(
    name   = "Programmatic configuration",
    breaks = c("Conservation.TRUE", "Conservation.FALSE", "No_one.TRUE", "No_one.FALSE"),
    labels = c("Forecast + fodder", "Forecast only", "Fodder only", "Neither"),
    values = c(
      "Conservation.TRUE"  = "#1b9e77",
      "Conservation.FALSE" = "#d95f02",
      "No_one.TRUE"        = "#7570b3",
      "No_one.FALSE"       = "#e7298a"
    )
  ) +
  ggplot2::labs(x = "Forecast", y = "Normalized conservation engagement") +
  ggthemes::theme_clean() +
  ggplot2::theme(
    legend.position = "bottom",
    legend.title = ggplot2::element_text(face = "bold"),
    strip.text = ggplot2::element_text(size = 12, face = "bold"),
    panel.spacing = grid::unit(2, "lines"),
    axis.title.x = ggplot2::element_blank(),
    axis.text.x  = ggplot2::element_blank(),
    axis.ticks.x = ggplot2::element_blank(),
    axis.text    = ggplot2::element_text(size = 12),
    axis.title   = ggplot2::element_text(size = 14)
  )

Exp3_plot

ggplot2::ggsave(
  filename = file.path(VIS_ROOT, "Exp3_supp_betas.png"),
  plot = Exp3_plot,
  width = 12, height = 14, dpi = 300
)


## Forecast accuracy -------------------------------------------
df_forecAcc <- read_csv("CombinedData_PostBurnIN.csv") %>%
  filter(
    Forecasts %in% c("No_one", "Conservation", "Everyone", "Rich", "Poor"),
    ForecastError %in% c(0, 10, 20, 30),
    Set_Adoption == FALSE,
    Supplemental_fodder == TRUE,
    Starting_Prop_Conservation == 0.25,
    Beta == 0.1
  )

outcome_vars <- c("AvgMoney", "AvgCows", "AvgGrass", "Gini", "PropCons")

df_long <- df_forecAcc %>%
  mutate(
    Forecasts_group = factor(
      Forecasts,
      levels = c("No_one", "Conservation", "Poor", "Rich", "Everyone"),
      labels = c("No one", "Conservation", "Poor", "Rich", "Everyone")
    ),
    Scenario = factor(
      Scenario,
      levels = c("SSP1-2.6", "SSP2-4.5", "SSP5-8.5"),
      labels = c(
        "SSP1-2.6\n(sustainability)",
        "SSP2-4.5\n(middle-of-the-road)",
        "SSP5-8.5\n(fossil fuel development)"
      ))
  ) %>%
  pivot_longer(
    cols = all_of(outcome_vars),
    names_to = "Variable",
    values_to = "RawValue"
  )

baseline_df <- df_long %>%
  filter(Forecasts_group == "No one") %>%
  group_by(Scenario, Variable, ForecastError) %>%
  summarise(Baseline = median(RawValue, na.rm = TRUE), .groups = "drop")

plot_df <- df_long %>%
  left_join(baseline_df, by = c("Scenario", "Variable", "ForecastError")) %>%
  mutate(Value = RawValue / Baseline) %>%
  filter(is.finite(Value))


forecAcc_plot <- ggplot(
  plot_df,
  aes(x = ForecastError, y = Value, color = Forecasts_group, group = Forecasts_group)
) +
  geom_hline(yintercept = 1, linetype = "dashed", linewidth = 0.9, color = "#525252") +
  
  # Error bars: mean ± 1 SD
  stat_summary(
    fun.data = mean_sdl, fun.args = list(mult = 1),
    geom = "errorbar", width = 0.8, linewidth = 0.6,
    na.rm = TRUE, alpha = 0.7
  ) +
  # Mean points
  stat_summary(fun = mean, geom = "point", size = 2.2, na.rm = TRUE, alpha = 0.7) +
  # Linear interpolation between means
  stat_summary(fun = mean, geom = "line", linewidth = 1, na.rm = TRUE, alpha = 0.7) +
  
  ggh4x::facet_grid2(
    rows = vars(Variable),
    cols = vars(Scenario),
    scales = "free_y",
    labeller = labeller(
      Scenario = label_value,
      Variable = c(
        AvgCows = "Livestock\nnumbers",
        AvgGrass = "Vegetation\ncover",
        AvgMoney = "Economic\nwellbeing",
        PropCons = "Conservation\nengagement",
        Gini = "Wealth\ninequality"
      )
    )) +
  scale_x_continuous(breaks = c(0, 10, 20, 30)) +
  labs(
    x = "Forecast error in percent",
    y = "Normalized outcome",
    color = "Forecast targeting strategy"
  ) +
  scale_color_manual(
    values = c(
      "No one"        = "#1f78b4",
      "Everyone"      = "#33a02c",
      "Poor"          = "#e31a1c",
      "Rich"          = "#ff7f00",
      "Conservation"  = "#6a3d9a"
    )
  ) +
  ggthemes::theme_clean()+
  theme(
    legend.position = "bottom",
    strip.text = element_text(size = 12, face = "bold"),
    panel.spacing = unit(2, "lines"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14)
  ) +
  ggh4x::facetted_pos_scales(
    y = list(
      Variable == "AvgMoney"  ~ scale_y_continuous(limits = c(0, 2), n.breaks = 4),
      Variable == "AvgCows"   ~ scale_y_continuous(limits = c(0.4, 1.6), n.breaks = 4),
      Variable == "AvgGrass"  ~ scale_y_continuous(limits = c(0.8, 1.3), n.breaks = 4),
      Variable == "Gini"      ~ scale_y_continuous(limits = c(0.9, 1.15), n.breaks = 4),
      Variable == "PropCons"  ~ scale_y_continuous(limits = c(0.6, 1.3),n.breaks = 4)
    )
  )

forecAcc_plot

ggplot2::ggsave(
  filename = file.path(VIS_ROOT, "Supp_Exp3_forecacc.png"),
  plot = forecAcc_plot,
  width = 12, height = 14, dpi = 500
)


## Forecast targeting strategy -------------------------------------

df_forecAcc <- read_csv("CombinedData_PostBurnIN.csv") %>%
  filter(
    Forecasts %in% c("No_one", "Conservation", "Everyone", "Rich", "Poor"),
    ForecastError %in% c(0, 10, 20, 30),
    Set_Adoption == FALSE,
    Supplemental_fodder == TRUE,
    Starting_Prop_Conservation == 0.25,
    Beta == 0.1
  )

outcome_vars <- c("AvgMoney", "AvgCows", "AvgGrass", "Gini", "PropCons")

df_long <- df_forecAcc %>%
  mutate(
    Forecasts_group = factor(
      Forecasts,
      levels = c("No_one", "Conservation", "Poor", "Rich", "Everyone"),
      labels = c("No one", "Conservation", "Poor", "Rich", "Everyone")
    ),
    Scenario = factor(
      Scenario,
      levels = c("SSP1-2.6", "SSP2-4.5", "SSP5-8.5"),
      labels = c(
        "SSP1-2.6\n(sustainability)",
        "SSP2-4.5\n(middle-of-the-road)",
        "SSP5-8.5\n(fossil fuel development)"
      ))
  ) %>%
  pivot_longer(
    cols = all_of(outcome_vars),
    names_to = "Variable",
    values_to = "RawValue"
  )

# Baseline is the No one group at the same Scenario/Variable/ForecastError
baseline_df <- df_long %>%
  filter(Forecasts_group == "No one") %>%
  group_by(Scenario, Variable, ForecastError) %>%
  summarise(Baseline = median(RawValue, na.rm = TRUE), .groups = "drop")

plot_df <- df_long %>%
  left_join(baseline_df, by = c("Scenario", "Variable", "ForecastError")) %>%
  mutate(Value = RawValue / Baseline) %>%
  filter(is.finite(Value))


plot_df0 <- plot_df %>% filter(ForecastError == 0)

lims_df <- plot_df0 %>%
  group_by(Variable) %>%
  summarise(
    lo = min(Value, na.rm = TRUE),
    hi = max(Value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    rng = hi - lo,
    pad = ifelse(rng == 0, pmax(abs(hi), 1) * 0.05, rng * 0.05),
    lo  = lo - pad,
    hi  = hi + pad
  )

forecAcc_plot_box <- ggplot(
  plot_df0,
  aes(x = Forecasts_group, y = Value, fill = Forecasts_group)
) +
  geom_hline(yintercept = 1, linetype = "dashed", linewidth = 0.9, color = "#525252") +
  geom_boxplot(width = 0.7, outlier.alpha = 0.4) +
  ggh4x::facet_grid2(
    rows = vars(Variable),
    cols = vars(Scenario),
    scales = "free_y",
    # independent = "y",
    labeller = labeller(
      Scenario = label_value,
      Variable = c(
        AvgCows  = "Livestock\nnumbers",
        AvgGrass = "Vegetation\ncover",
        AvgMoney = "Economic\nwellbeing",
        PropCons = "Conservation\nengagement",
        Gini     = "Wealth\ninequality"
      )
    )
  ) +
  labs(
    x = "Forecast targeting strategy",
    y = "Normalized outcome",
    fill = "Forecast"
  ) +
  scale_fill_manual(values = c(
    "No one"       = "#1f78b4",
    "Everyone"     = "#33a02c",
    "Poor"         = "#e31a1c",
    "Rich"         = "#ff7f00",
    "Conservation" = "#6a3d9a"
  )) +
  ggthemes::theme_clean() +
  theme(
    legend.position = "none",
    strip.text = element_text(size = 12, face = "bold"),
    panel.spacing = unit(2, "lines"),
    axis.text.x = element_text(size = 12, angle = 30, hjust = 1),
    axis.text.y = element_text(size = 12),
    axis.title  = element_text(size = 14),
    axis.ticks = element_blank()
  )
forecAcc_plot_box

ggplot2::ggsave(
  filename = file.path(VIS_ROOT, "Supp_Exp3_targeting.png"),
  plot = forecAcc_plot_box,
  width = 12, height = 14, dpi = 300
)


## Buying/selling (longer run-time)----------------------------------------------------------

# Files to process

files <- c("Ts_Data/Ranchers/FOREC_Conservation_ADOP_TRUE_FODD_TRUE_PROPCONS_0.5_FORECERR_0_Ranchers.csv.gz"
           # ,"Ts_Data/Ranchers/FOREC_Conservation_ADOP_TRUE_FODD_FALSE_PROPCONS_0.5_FORECERR_0_beta_0.1_Ranchers.csv.gz"
)


# sampling toggle for code check
DEV <- TRUE
set.seed(42)

# how much to keep in DEV
DEV_REPS_PER_SCEN <- 50     # keep at most N reps per Scenario
DEV_TS_STEP       <- 1     # keep every k-th ts
DEV_PLOT_FRAC <- 1
DEV_PLOT_MIN  <- 25

# Read precip with optional SQL; returns data.table
read_precip_min_plot <- function(f, where_sql = NULL) {
  lyr <- sf::st_layers(f)$name[1]
  base_cols <- '"Scenario","Rep","Time","PlotID","Precip"'
  from <- sprintf('FROM "%s"', lyr)
  where <- if (!is.null(where_sql)) sprintf("WHERE %s", where_sql) else ""
  sql <- sprintf("SELECT %s %s %s", base_cols, from, where)
  out <- sf::st_read(dsn = f, query = sql, quiet = TRUE)
  data.table::setDT(out); out
}

# Get distinct PlotIDs without loading geometry
get_plot_ids <- function(f) {
  lyr <- sf::st_layers(f)$name[1]
  sql <- sprintf('SELECT DISTINCT "PlotID" FROM "%s"', lyr)
  out <- sf::st_read(dsn = f, query = sql, quiet = TRUE)
  out$PlotID
}

# Build WHERE clause for precip read
build_precip_where <- function(rep_keep = NULL, plot_keep = NULL, ts_step = NULL) {
  clauses <- list()
  if (length(rep_keep))  clauses <- c(clauses, sprintf('"Rep" IN (%s)', paste(sprintf("'%s'", rep_keep), collapse=",")))
  if (length(plot_keep)) clauses <- c(clauses, sprintf('"PlotID" IN (%s)', paste(sprintf("'%s'", plot_keep), collapse=",")))
  if (!is.null(ts_step) && ts_step > 1) clauses <- c(clauses, sprintf('("Time" %% %d) = 0', ts_step))
  if (!length(clauses)) NULL else paste(clauses, collapse = " AND ")
}


# Find the precip .gpkg that matches a rancher file's tag
find_precip_for_tag <- function(tag) {
  # rancher tag looks like "..._Ranchers"; precip files typically share the same core
  core <- sub("_Ranchers.csv$", "", tag)
  cand <- Sys.glob(glue::glue("Ts_Data/Precip/final/{core}*.gpkg"))
  if (length(cand) == 0) return(NA_character_)
  cand[1]  # pick the first if multiple
}

# Output dir
out_dir <- file.path(VIS_ROOT, "ranchers")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# Build a WHERE clause that filters Time (every k), Rep (subset), PlotID (subset)
build_precip_where <- function(rep_keep = NULL, plot_keep = NULL, ts_step = NULL) {
  clauses <- list()
  if (!is.null(rep_keep) && length(rep_keep)) {
    rep_list <- paste(sprintf("'%s'", rep_keep), collapse = ",")
    clauses <- c(clauses, sprintf('"Rep" IN (%s)', rep_list))
  }
  if (!is.null(plot_keep) && length(plot_keep)) {
    plot_list <- paste(sprintf("'%s'", plot_keep), collapse = ",")
    clauses <- c(clauses, sprintf('"PlotID" IN (%s)', plot_list))
  }
  if (!is.null(ts_step) && ts_step > 1) {
    # SQLite modulo operator is %; Time assumed integer
    clauses <- c(clauses, sprintf('("Time" %% %d) = 0', ts_step))
  }
  if (!length(clauses)) return(NULL)
  paste(clauses, collapse = " AND ")
}


# Function to process one file and write plots
process_rancher_file <- function(path) {
  tag <- path_ext_remove(path_file(path))
  
  df <- vroom::vroom(
    path,
    col_select = c(
      Scenario, Rep, ts, person_id,
      actual_animals_change, proposed_change,
      Conservation, stock_count
    ),
    altrep = TRUE, show_col_types = FALSE, progress = FALSE
  )
  
  df <- df %>%
    dplyr::mutate(
      Scenario = dplyr::recode(
        Scenario,
        "good" = "SSP1-2.6",
        "ok"   = "SSP2-4.5",
        "bad"  = "SSP5-8.5"
      )
    )
  
  # DEV: sample reps and ts up-front so everything downstream is lighter
  if (DEV) {
    # ensure ts is integer for modulo
    if (!is.integer(df$ts)) df$ts <- as.integer(df$ts)
    
    # pick up to DEV_REPS_PER_SCEN reps *per Scenario*
    rep_keep <- df %>%
      dplyr::distinct(Scenario, Rep) %>%
      dplyr::group_by(Scenario) %>%
      dplyr::summarise(
        Rep = {
          reps <- unique(Rep)
          # sample(reps, size = min(DEV_REPS_PER_SCEN, length(reps)), replace = FALSE)
          head(reps, min(DEV_REPS_PER_SCEN, length(reps))) # only choose first reps instead of random sample per scenario
        },
        .groups = "drop"
      ) %>%
      dplyr::pull(Rep)
    
    df <- df %>% dplyr::filter(Rep %in% rep_keep)
    # downsample timesteps
    df <- df %>% dplyr::filter(ts %% DEV_TS_STEP == 0)
  } else {
    rep_keep <- NULL
  }
  
  # mean across Ranchers per Scenario/Rep/ts/Conservation
  by_rep_ts <- df %>%
    group_by(Scenario, Rep, ts, Conservation) %>%
    summarise(
      mean_change = mean(actual_animals_change, na.rm = TRUE),
      mean_stock  = mean(stock_count,           na.rm = TRUE),
      mean_proposed = mean(proposed_change,       na.rm = TRUE),
      .groups = "drop"
    )
  
  # mean across Reps
  summary_ts <- by_rep_ts %>%
    group_by(Scenario, ts, Conservation) %>%
    summarise(
      mean_stock  = mean(mean_stock,  na.rm = TRUE),
      mean_change = mean(mean_change, na.rm = TRUE),
      mean_proposed = mean(mean_proposed, na.rm = TRUE),
      n_rep       = n(),
      .groups = "drop"
    )
  
  # bring into order
  scenario_levels <- c("SSP1-2.6","SSP2-4.5","SSP5-8.5")
  
  # long format
  lines_full <- summary_ts %>%
    dplyr::select(Scenario, ts, Conservation, mean_stock, mean_change, mean_proposed) %>%
    tidyr::pivot_longer(
      cols = c(mean_stock, mean_change, mean_proposed),
      names_to = "metric", values_to = "value"
    ) %>%
    dplyr::mutate(
      metric = dplyr::recode(metric,
                             mean_stock = "Stock",
                             mean_change   = "Actual",
                             mean_proposed = "Proposed"),
      Scenario = factor(Scenario, levels = scenario_levels)
    )
  
  # Load matching precip file
  precip_path <- find_precip_for_tag(tag)
  if (!is.na(precip_path)) {
    
    # DEV: choose a small subset of PlotIDs for faster ribbons
    plot_keep <- NULL
    if (DEV) {
      plot_ids <- get_plot_ids(precip_path)
      k <- min(length(plot_ids), max(DEV_PLOT_MIN, ceiling(DEV_PLOT_FRAC * length(plot_ids))))
      plot_keep <- sample(plot_ids, k)
    }
    
    # Build WHERE to filter in the database: reps, plots, every k-th Time
    where_sql <- build_precip_where(
      rep_keep = rep_keep,
      plot_keep = plot_keep,
      ts_step   = if (DEV) DEV_TS_STEP else NULL
    )
    
    # Read only what we need
    precip_dt <- read_precip_min_plot(precip_path, where_sql = where_sql)
    
    # Aggregate to Scenario × Time (approximate in DEV due to plot sampling)
    precip_ts <- precip_dt[, .(
      precip_mean = mean(Precip, na.rm = TRUE),
      precip_q05  = stats::quantile(Precip, 0.05, na.rm = TRUE, type = 7), # linear interpolation between nearest ranks
      precip_q95  = stats::quantile(Precip, 0.95, na.rm = TRUE, type = 7) # linear interpolation between nearest ranks
    ), by = .(Scenario, Time)]
    
    precip_ts2 <- precip_ts %>%
      dplyr::transmute(
        Scenario = Scenario,
        ts       = Time,
        precip_mean, precip_q05, precip_q95
      ) %>%
      dplyr::mutate(
        Scenario = dplyr::recode(
          Scenario,
          "good" = "SSP1-2.6",
          "ok"   = "SSP2-4.5",
          "bad"  = "SSP5-8.5"
        )
      )
    
  } else {
    warning("No precip .gpkg found for tag: ", tag)
    precip_ts2 <- NULL
  }
  
  # Full period -----
  if (!is.null(precip_ts2)) {
    precip_full <- precip_ts2 %>%
      dplyr::mutate(Scenario = factor(Scenario, levels = scenario_levels))
    
    # Build long data for animal-related panels (unscaled)
    lines_animals_long <- summary_ts %>%
      dplyr::select(Scenario, ts, Conservation, mean_stock, mean_change, mean_proposed) %>%
      tidyr::pivot_longer(
        cols = c(mean_stock, mean_change, mean_proposed),
        names_to = "panel", values_to = "value"
      ) %>%
      dplyr::mutate(
        panel = dplyr::recode(panel,
                              mean_stock    = "Actual stock",
                              mean_change   = "Herd size management",
                              mean_proposed = "Proposed sell"),
        Scenario = factor(Scenario, levels = scenario_levels)
      )
    
    # Build precip series for its own panel (unscaled)
    precip_long <- precip_full %>%
      dplyr::transmute(
        Scenario = factor(Scenario, levels = scenario_levels),
        ts,
        panel = "Precipitation",
        value = precip_mean
      )
    
    # Compose the 4-facet plot with free y-scale per panel (no rescaling)
    p_facets_full <-
      ggplot2::ggplot() +
      # Animal panels: color by Scenario, linetype by Conservation
      ggplot2::geom_line(
        data = lines_animals_long,
        ggplot2::aes(x = ts, y = value,
                     color = Scenario,
                     linetype = Conservation,
                     group = interaction(Scenario, Conservation)),
        linewidth = 0.7
      ) +
      # Precipitation panel: color by Scenario
      ggplot2::geom_line(
        data = precip_long,
        ggplot2::aes(x = ts, y = value, color = Scenario),
        inherit.aes = FALSE, linewidth = 0.7
      ) +
      ggplot2::facet_wrap(~ panel, ncol = 1, scales = "free_y") +
      ggplot2::labs(
        x = "Iteration",
        y = NULL,
        title = glue::glue("{tag}"),
      ) +
      ggthemes::theme_clean() +
      ggplot2::theme(legend.position = "top")+
      scale_color_discrete(name = "Scenario") +
      scale_linetype_discrete(name = "Conservation")
    
    p_change_full_precip <- p_facets_full # for naming consistency for now
    
    # Save last-30 plot
    ggplot2::ggsave(
      glue::glue("{out_dir}/{tag}__change_full100__with_precip.png"),
      p_change_full_precip, width = 9, height = 6, dpi = 300
    )
  }
  
  # Post burn-in ----
  if (!is.null(precip_ts2)) {
    # Prepare last 30 timesteps for animal series
    summary_last30 <- summary_ts %>%
      dplyr::group_by(Scenario) %>%
      dplyr::filter(ts >= max(ts, na.rm = TRUE) - 30) %>%
      dplyr::ungroup()
    
    lines_animals_last30 <- summary_last30 %>%
      dplyr::select(Scenario, ts, Conservation, mean_change) %>%
      tidyr::pivot_longer(
        cols = c(mean_change), names_to = "panel", values_to = "value"
      ) %>%
      dplyr::mutate(
        panel = dplyr::recode(panel, mean_change = "Herd management"),
        Scenario = factor(Scenario, levels = scenario_levels)
      )
    
    # Rename conservation groups to desired legend entries
    lines_animals_last30 <- lines_animals_last30 %>%
      dplyr::mutate(
        Conservation = dplyr::case_when(
          Conservation == TRUE  ~ "Conservation (forecast + fodder)",
          Conservation == FALSE ~ "No conservation",
          TRUE ~ as.character(Conservation)
        ),
        Conservation = forcats::fct_relevel(
          Conservation,
          "Conservation (forecast + fodder)", "No conservation"
        )
      )
    
    # Prepare last 30 timesteps for precipitation
    precip_last30 <- precip_full %>%
      dplyr::group_by(Scenario) %>%
      dplyr::filter(ts >= max(ts, na.rm = TRUE) - 30) %>%
      dplyr::ungroup()
    
    precip_last30_long <- precip_last30 %>%
      dplyr::transmute(
        Scenario = factor(Scenario, levels = scenario_levels),
        ts, value = precip_mean
      )
    
    # Shared scales for consistent styling across panels
    scale_cols <- ggplot2::scale_color_manual(
      name   = NULL,
      breaks = c("SSP1-2.6","SSP2-4.5","SSP5-8.5"),
      values = c(
        "SSP1-2.6" = "#009E73",
        "SSP2-4.5" = "#F0E442",
        "SSP5-8.5" = "#D55E00"
      ),
      guide  = ggplot2::guide_legend(order = 1)
    )
    
    scale_ltypes <- ggplot2::scale_linetype_manual(
      name   = NULL,
      values = c("Conservation (forecast + fodder)" = "solid",
                 "No conservation"                  = "dotted"),
      limits = c("Conservation (forecast + fodder)", "No conservation"),
      drop   = FALSE,
      guide  = ggplot2::guide_legend(order = 2)
    )
    
    # Libraries to combine plots and place a shared legend on top
    
    # -------- Herd management panel --------
    p1 <- ggplot2::ggplot(
      lines_animals_last30,
      ggplot2::aes(x = ts, y = value,
                   color = Scenario,
                   linetype = Conservation,
                   group = interaction(Scenario, Conservation))
    ) +
      ggplot2::geom_line(linewidth = 0.7) +
      ggplot2::labs(
        x = NULL,
        y = "Mean proportion change in\nherd size per individual",
        title = "Change in herd size"
      ) +
      scale_cols + scale_ltypes +
      cowplot::theme_minimal_grid() + 
      cowplot::panel_border(remove = TRUE) + 
      ggplot2::theme(
        legend.position = "none",
        plot.title = ggplot2::element_text(hjust = 0.5, size = 14, face = "plain"),
        axis.title.y = ggplot2::element_text(size = 12),
        axis.text.x  = ggplot2::element_blank(),
        axis.ticks.x = ggplot2::element_blank(),
        axis.title.x = ggplot2::element_blank(),
        axis.line    = ggplot2::element_blank(),
        plot.background  = ggplot2::element_rect(fill = NA, colour = NA),
        panel.background = ggplot2::element_rect(fill = NA, colour = NA)
      )
    
    # -------- Precipitation panel --------
    p2 <- ggplot2::ggplot(
      precip_last30_long,
      ggplot2::aes(x = ts, y = value, color = Scenario, group = Scenario)
    ) +
      ggplot2::geom_line(linewidth = 0.7) +
      ggplot2::labs(
        x = "Iteration",
        y = "Mean precipitation per plot"
        # title = "Precipitation"
      ) +
      scale_cols +
      cowplot::theme_minimal_grid() +
      cowplot::panel_border(remove = TRUE) +
      ggplot2::theme(
        legend.position = "none",
        plot.title   = ggplot2::element_text(hjust = 0.5, size = 14, face = "plain"),
        axis.title.y = ggplot2::element_text(size = 12),
        axis.text         = element_text(size = 12),
        axis.line        = ggplot2::element_blank(),
        axis.line.x.top  = ggplot2::element_blank(),
        plot.background  = ggplot2::element_rect(fill = NA, colour = NA),
        panel.background = ggplot2::element_rect(fill = NA, colour = NA)
      )
    
    
    # Build a shared legend
    legend_plot <- ggplot2::ggplot(
      lines_animals_last30,
      ggplot2::aes(x = ts, y = value, color = Scenario, linetype = Conservation)
    ) +
      ggplot2::geom_line() +
      scale_cols + scale_ltypes +
      ggthemes::theme_clean() +
      ggplot2::theme(
        legend.position = "top",
        legend.box = "horizontal",
        legend.title = ggplot2::element_blank(),
        legend.text  = ggplot2::element_text(size = 12),
        # no extra margins so the legend sits right above the first title
        plot.margin = ggplot2::margin(0, 0, 0, 0)
      )
    shared_legend <- cowplot::get_legend(legend_plot)
    
    # Stack legend + panels
    grid_panels <- cowplot::plot_grid(p1, p2, ncol = 1, align = "v", rel_heights = c(1, 1))
    p_change_last30_precip <- cowplot::plot_grid(
      shared_legend, grid_panels, ncol = 1, rel_heights = c(0.12, 1)
    )
    
    # Thin outer border around the whole figure (legend + panels)
    p_change_last30_precip <- cowplot::ggdraw(p_change_last30_precip) +
      ggplot2::theme(
        plot.background = ggplot2::element_rect(color = "black", size = 0.5, fill = NA),
        plot.margin = ggplot2::margin(10, 10, 10, 10)
      )
    
    # Save
    ggplot2::ggsave(
      glue::glue("{out_dir}/{tag}__change_last30__with_precip.png"),
      p_change_last30_precip, width = 9, height = 6, dpi = 300
    )
  }
}

# Run for all files
results <- purrr::map(files, process_rancher_file)


# Toy example stop and error catch ------------------------------------------
}, error = function(e) {
  if (identical(conditionMessage(e), "EARLY_EXIT")) {
    message("Stopped cleanly after Exp 1.")
    # nothing else; session stays open, exit code = 0 when run via Rscript
  } else {
    stop(e)  # real errors still propagate
  }
})


