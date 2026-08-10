# Setting working directory -----------------------------------------------
# NOTE: rm(list=ls()) removed on purpose. This file gets source()'d inside a
# running Shiny app; wiping the whole global environment here would delete
# everything the app already set up (ui/server objects, other sourced
# modules, etc.) before this script even reaches the function definitions
# below.
# rm(list=ls())

# Libraries to be used -----------------------------------------------
# NOTE: only the packages actually used by the functions in this file are
# loaded here (dplyr/tidyr/forcats/stringr/ggplot2, all part of tidyverse).
# The previous version also loaded data.table, gridExtra, grid, plotly,
# knitr, kableExtra, htmltools, ggh4x, sf, and maps -- none of which are
# referenced anywhere in this file's function bodies. library() throws a
# hard error and stops sourcing the rest of the script if a package isn't
# installed, so any one of those being missing was enough to prevent
# finaltable() (and everything else below) from ever being defined, which is
# what produced "could not find function 'finaltable'". If you later add
# code to this file that genuinely needs one of the removed packages, just
# add its library() call back in.
library(dplyr)
library(tidyr)
library(ggplot2)
library(forcats)
library(stringr)
options(scipen=999)



## --------------------------------------------------------------------------------------------------------------------------
finaltable<-function(aal,mod_name){
  aal$Classification = factor(aal$Classification,levels = unique(aal$Classification))
  aal$Description = factor(aal$Description,levels = unique(aal$Description))
  
  # splitting classification
  aal = aal %>% mutate(type = substr(as.character(Classification),1,3),
                       modifier = substr(as.character(Classification),
                                         5,nchar(as.character(Classification))))
  
  aal$modifier<-mod_name$name[match(aal$modifier,mod_name$modifer)] # matching modifier names
  
  dv1=unique(aal$modifier) #selecting unique modifiers                                                    
  aal$modifier = factor(aal$modifier,levels = dv1) # making modifiers as levels for plotting
  aal$type = factor(aal$type,levels = c("SFD","COM")) # assign levels for type
  
  aal_final = aal %>% dplyr::group_by(STATECODE,type) %>% 
    dplyr::mutate(AAL_Unk = cur_data()$AAL[cur_data()$modifier=="Unknown"] )
  
  aal_final = aal_final %>% dplyr::mutate(AAL_rel = AAL/AAL_Unk, 
                                          AAL_ch = (AAL - AAL_Unk)/AAL_Unk) %>%
    dplyr::arrange(Classification,STATECODE,Description,type)
  
  return(aal_final)
}
## --------------------------------------------------------------------------------------------------------------------------

finaltable_allUSA<-function(aal,mod_name){
  aal$Classification = factor(aal$Classification,levels = unique(aal$Classification))
  aal$Description = factor(aal$Description,levels = unique(aal$Description))
  
  # splitting classification
  aal = aal %>% mutate(type = substr(as.character(Classification),1,3),
                       modifier = substr(as.character(Classification),
                                         5,nchar(as.character(Classification))))
  
  aal$modifier<-mod_name$name[match(aal$modifier,mod_name$modifer)] # matching modifier names
  
  dv1=unique(aal$modifier) #selecting unique modifiers                                                    
  aal$modifier = factor(aal$modifier,levels = dv1) # making modifiers as levels for plotting
  aal$type = factor(aal$type,levels = c("SFD","COM")) # assign levels for type
  
  aal_final = aal %>% dplyr::group_by(type) %>% 
    dplyr::mutate(AAL_Unk = cur_data()$AAL[cur_data()$modifier=="Unknown"] )
  
  aal_final = aal_final %>% dplyr::mutate(AAL_rel = AAL/AAL_Unk, 
                                          AAL_ch = (AAL - AAL_Unk)/AAL_Unk) %>%
    dplyr::arrange(Classification,Description,type)
  
  return(aal_final)
}
## --------------------------------------------------------------------------------------------------------------------------



## individual secondary modifiers
indmod <- function(aal_final, palette = c("#6FACDE", "#F0B323"), axis_wrap_width = 30) {
  # FIX: levels(...)[-match("Unknown", levels(...))] silently returns all-NA
  # when "Unknown" isn't a level (R's -NA indexing gotcha), which made every
  # modifier subset empty and every plot blank. setdiff() is safe either way.
  classif <- setdiff(levels(aal_final$modifier), "Unknown")
  pl <- list()
  for (i in seq_along(classif)) {
    choose_classif <- classif[i]
    aalp <- subset(aal_final, modifier == choose_classif)
    
    if (nrow(aalp) == 0) next  # skip instead of building a blank ggplot
    
    dv2 <- unique(aalp$STATECODE)
    tmp <- aalp[1:(length(dv2) * 2), ]
    tmp[, ] <- NA
    tmp$STATECODE <- rep(dv2, 2)
    tmp$type <- factor(rep(c("SFD", "COM"), each = length(dv2)), levels = c("SFD", "COM"))
    tmp$modifier <- factor(choose_classif)
    tmp$AAL_rel <- 1
    tmp$Description <- factor("Unknown")
    aalp <- rbind(aalp, tmp)
    
    # named by the modifier itself, so the caller's keys always line up
    pl[[choose_classif]] <- ggplot(data = aalp, mapping = aes(x = reorder(Description, -AAL_rel), y = AAL_rel, fill = type)) +
      geom_col(position = position_dodge2(preserve = "single"), width = 0.7) +
      facet_wrap(~STATECODE) +
      xlab(choose_classif) +
      ylab("Relative Loss Cost") +
      # X-tick (Description) word wrap so long labels don't collide/overlap.
      # This is a labels-only formatter -- it never touches the underlying
      # data/ordering, so it's safe to layer on top of reorder(Description, ...).
      scale_x_discrete(labels = function(x) str_wrap(x, width = axis_wrap_width)) +
      theme(axis.text.x = element_text(angle = 90, vjust = 0, hjust = 0)) +
      scale_fill_manual(values = palette)
  }
  return(pl)
}

## --------------------------------------------------------------------------------------------------------------------------

indmod_forpng <- function(aal_final,
                          out_dir = "indmod_plots",
                          width = 7, height = 5, dpi = 300,
                          drop_unknown_modifier = TRUE,
                          palette = c("#6FACDE", "#F0B323"),
                          axis_wrap_width = 30) {
  
  # Ensure plain data.frame (grouped_df can behave oddly with subset/rbind)
  aal_final <- as.data.frame(aal_final)
  
  # Modifier list (optionally drop "Unknown")
  classif <- levels(aal_final$modifier)
  if (drop_unknown_modifier && "Unknown" %in% classif) {
    classif <- classif[ classif != "Unknown" ]
  }
  
  # If modifier is not a factor for some reason, fallback
  if (is.null(classif) || length(classif) == 0) {
    classif <- sort(unique(as.character(aal_final$modifier)))
    if (drop_unknown_modifier) classif <- setdiff(classif, "Unknown")
  }
  
  # Create root output folder
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  
  pl <- vector("list", length(classif))
  names(pl) <- classif
  
  for (i in seq_along(classif)) {
    
    choose_classif <- classif[i]
    
    # safe folder / filename token (remove spaces + punctuation)
    safe_mod <- gsub("[^A-Za-z0-9]+", "", choose_classif)
    
    # Subset to one modifier
    aalp <- subset(aal_final, modifier == choose_classif)
    if (nrow(aalp) == 0) next
    
    # States in this modifier
    dv2 <- sort(unique(aalp$STATECODE))
    
    # --- Add Unknown baseline rows (AAL_rel=1) for every STATECODE x type (SFD, COM)
    # Use "rep(1, n)" indexing to create enough rows safely, then blank them out
    tmp <- aalp[rep(1, length(dv2) * 2), , drop = FALSE]
    tmp[,] <- NA
    
    tmp$STATECODE <- rep(dv2, 2)
    tmp$type      <- factor(rep(c("SFD", "COM"), each = length(dv2)),
                            levels = c("SFD", "COM"))
    
    # keep modifier as factor if it is in original data
    if (is.factor(aal_final$modifier)) {
      tmp$modifier <- factor(choose_classif, levels = levels(aal_final$modifier))
    } else {
      tmp$modifier <- choose_classif
    }
    
    # Force Description = "Unknown" with consistent factor levels if available
    if (is.factor(aal_final$Description)) {
      tmp$Description <- factor("Unknown", levels = levels(aal_final$Description))
    } else {
      tmp$Description <- "Unknown"
    }
    
    tmp$AAL_rel <- 1
    
    # Bind back
    aalp <- rbind(aalp, tmp)
    
    # Create modifier folder
    mod_dir <- file.path(out_dir, safe_mod)
    dir.create(mod_dir, showWarnings = FALSE, recursive = TRUE)
    
    # Plot list per modifier
    pl_state <- vector("list", length(dv2))
    names(pl_state) <- dv2
    
    # --- Save one plot per STATECODE
    for (st in dv2) {
      
      aalp_st <- subset(aalp, STATECODE == st)
      
      # Build plot (NO facet_wrap since this is per state)
      p <- ggplot(aalp_st,
                  aes(x = reorder(Description, -AAL_rel),
                      y = AAL_rel,
                      fill = type)) +
        geom_col(position = position_dodge2(preserve = "single"), width = 0.7) +
        labs(x = choose_classif, y = "Relative Loss Cost") +
        scale_x_discrete(labels = function(x) str_wrap(x, width = axis_wrap_width)) +
        theme_bw() +
        theme(
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
          panel.grid.major.x = element_blank()
        ) +
        scale_fill_manual(values = palette)
      
      # Save as PNG
      out_file <- file.path(mod_dir, paste0(st, "_", safe_mod, ".png"))
      ggsave(filename = out_file, plot = p, width = width, height = height, dpi = dpi)
      
      pl_state[[st]] <- p
    }
    
    pl[[i]] <- pl_state
  }
  
  return(pl)
}


## --------------------------------------------------------------------------------------------------------------------------

STATEminmax<-function(aal_final,mod_name){
  # create max and min based on each STATE
  aalc = aal_final %>% 
    # filter( !is.na(AAL_ch) & !is.na(region)) %>% 
    dplyr::group_by(STATECODE,Classification) %>% 
    dplyr::summarise(max = max(AAL_ch),min = min(AAL_ch), .groups = "drop") 
  
  aalc$max=ifelse(aalc$max<0,0,aalc$max)
  aalc$min=ifelse(aalc$min>0,0,aalc$min)   # Added NOW
  
  # splitting classification
  aalc = aalc %>% mutate(type = substr(as.character(Classification),1,3),
                         modifier = substr(as.character(Classification),5,nchar(as.character(Classification))))
  
  aalc$modifier<-mod_name$name[match(aalc$modifier,mod_name$modifer)]
  
  # reordering factor levels for plotting
  # dv1 = unique(substr(levels(aalc$Classification),5,nchar(levels(aalc$Classification))))
  dv1=unique(aalc$modifier)
  aalc$modifier = factor(aalc$modifier,levels = dv1)
  # aalc$type = factor(aalc$type,levels = c("SFD"))
  aalc$type = factor(aalc$type,levels = c("SFD","COM"))
  # write.table(aalc,"secmod_credit_penalty.csv",row.names=FALSE,sep=",",na="",col.names=TRUE,quote=FALSE)
  
  # melting data for plotting
  aalp = reshape2::melt(aalc,measure.vars = c("max","min"))
  penalty_credit<-data.frame("pc"=c("Penalty","Credit"),"variable"=c('max','min') )
  aalp$variable<-penalty_credit$pc[match(aalp$variable,penalty_credit$variable)]
  
  return(aalp)
  
}


## --------------------------------------------------------------------------------------------------------------------------

Countryminmax<-function(aal_final,mod_name,minThres = -0.1, maxThres = 0.1){ 
  # create max and min based on each STATE
  aalc = aal_final %>% 
    # filter( !is.na(AAL_ch) & !is.na(region)) %>% 
    dplyr::group_by(Classification) %>% 
    dplyr::summarise(max = max(AAL_ch),min = min(AAL_ch), .groups = "drop") 
  
  aalc$max=ifelse(aalc$max<0,0,aalc$max)
  aalc$min=ifelse(aalc$min>0,0,aalc$min)   # Added NOW
  
  # splitting classification
  aalc = aalc %>% mutate(type = substr(as.character(Classification),1,3),
                         modifier = substr(as.character(Classification),5,nchar(as.character(Classification))))
  
  aalc$modifier<-mod_name$name[match(aalc$modifier,mod_name$modifer)]
  
  # reordering factor levels for plotting
  # dv1 = unique(substr(levels(aalc$Classification),5,nchar(levels(aalc$Classification))))
  dv1=unique(aalc$modifier)
  aalc$modifier = factor(aalc$modifier,levels = dv1)
  # aalc$type = factor(aalc$type,levels = c("SFD"))
  aalc$type = factor(aalc$type,levels = c("SFD","COM"))
  # write.table(aalc,"secmod_credit_penalty.csv",row.names=FALSE,sep=",",na="",col.names=TRUE,quote=FALSE)
  
  # melting data for plotting
  aalp = reshape2::melt(aalc,measure.vars = c("max","min"))
  penalty_credit<-data.frame("pc"=c("Penalty","Credit"),"variable"=c('max','min') )
  aalp$variable<-penalty_credit$pc[match(aalp$variable,penalty_credit$variable)]
  
  aalp_final <- aalp[aalp$value> maxThres | aalp$value< minThres,]
  
  return(aalp_final)
  
}

## --------------------------------------------------------------------------------------------------------------------------

# Country-wide impact  
## Table for COM and SFD by modifiers

CountryminmaxTable<-function(aal_final,mod_name){ 
  # create max and min based on each STATE
  aalt = aal_final %>% 
    # filter( !is.na(AAL_ch) & !is.na(region)) %>% 
    dplyr::group_by(Classification) %>% 
    dplyr::summarise(Max = max(AAL_ch),Min = min(AAL_ch),Max_Description = Description[which.max(AAL_ch)],
                     Min_Description = Description[which.min(AAL_ch)],.groups = "drop") 
  
  aalt = aalt %>% mutate(type = substr(as.character(Classification),1,3),
                         modifier = substr(as.character(Classification),5,nchar(as.character(Classification))))
  aalt$modifier<-mod_name$name[match(aalt$modifier,mod_name$modifer)]
  
  dv1=unique(aalt$modifier)
  aalt$Modifier = factor(aalt$modifier,levels = dv1)
  aalt$LOB = factor(aalt$type,levels = c("SFD","COM"))
  # melting data for plotting
  aalt1 = aalt [,c("Modifier","LOB","Max","Min","Max_Description","Min_Description")]
  
  
  aalt1$Max=ifelse(aalt1$Max<0,0,aalt1$Max)
  aalt1$Min=ifelse(aalt1$Min>0,0,aalt1$Min)   
  aalt1$Max= aalt1$Max*100    # Converting to Percentage
  aalt1$Min= aalt1$Min*100
  
  aalt1$Max_Description= as.character(aalt1$Max_Description)
  aalt1$Min_Description= as.character(aalt1$Min_Description)
  
  return(aalt1)
}


## --------------------------------------------------------------------------------------------------------------------------

Credit_Penalty <- function(table_minmax_USA, type_colors, label_wrap_width = 12,
                           axis_wrap_width = 30, label_size = 5.2,
                           label_nudge_y = 8, dpi = 300) {
  
  table_minmax_USA <- table_minmax_USA %>%
    mutate(Class_Size = abs(Max) + abs(Min))
  
  df_long <- table_minmax_USA %>%
    pivot_longer(cols = c(Max, Min), names_to = "Type", values_to = "Value") %>%
    mutate(Description = ifelse(Type == "Max", Max_Description, Min_Description))
  
  df_long <- df_long %>%
    mutate(Modifier = fct_reorder(Modifier, Class_Size, .desc = TRUE))
  
  df_filtered <- df_long %>% filter(abs(Value) > 10)
  
  # ---- If no data, return a message plot ----
  if (nrow(df_filtered) == 0) {
    p <- ggplot() +
      annotate("text", x = 0.5, y = 0.5, 
               label = "No modifier exceeds the Â±10% threshold", 
               size = 6, hjust = 0.5) +
      theme_void() +
      labs(title = "No significant sensitivity changes")
    return(p)
  }
  
  # ---- Proceed with plotting ----
  df_filtered$Description <- str_wrap(df_filtered$Description, width = label_wrap_width)
  
  # ---- Pixel-based, line-count-independent data-label gap ----
  # The old `vjust = ifelse(Value >= 0, -0.2, 1.3)` is a FRACTION of each
  # label's own (wrapped) text height, so a 1-line label ("None") and a
  # 3-line label ("Roof mounted impact resistant array") end up with very
  # different visual gaps for the same fraction -- that's what made the
  # padding look random. Converting the desired pixel gap into a vjust
  # fraction and dividing by each label's own line count cancels the
  # height-dependence, so every bar gets the same *visual* gap regardless
  # of how many lines its own label wrapped to. (Mirrors the live override
  # in secmod_server.R's credit_penalty_plot_final() so this function's
  # raw output already looks correct before any UI control runs.)
  pt_per_mm <- 72.27 / 25.4  # ggplot2's internal mm -> pt constant (.pt)
  line_height_px <- label_size * pt_per_mm * (dpi / 72.27) * 1.2
  vjust_unit <- if (is.finite(line_height_px) && line_height_px > 0) {
    label_nudge_y / line_height_px
  } else {
    0.2
  }
  nlines <- pmax(1, lengths(strsplit(as.character(df_filtered$Description), "\n")))
  vjust_vec <- ifelse(df_filtered$Value >= 0,
                      -vjust_unit / nlines,
                      1 + vjust_unit / nlines)
  
  p <- ggplot(df_filtered, aes(x = Modifier, y = Value, fill = Type)) +
    geom_col(width = 0.6, position = "identity") +
    geom_text(aes(label = Description),
              vjust = vjust_vec,
              size = label_size, angle = 0) +
    # X-tick (Modifier) word wrap -- independent control from the
    # Description data-label wrap above; labels-only, doesn't touch data.
    scale_x_discrete(labels = function(x) str_wrap(x, width = axis_wrap_width)) +
    scale_y_continuous(
      labels = scales::percent_format(scale = 1),
      breaks = c(0, seq(-100, 100, by = 20)),
      expand = expansion(mult = c(0.1, 0.1)),
      limits = c(-80, 80)
    ) +
    scale_fill_manual(values = type_colors,
                      labels = c("Penalty", "Credit")) +
    facet_grid(rows = vars(LOB), scales = "free_y", space = "free") +
    geom_hline(yintercept = 0, colour = "black", linetype = "solid", size = 0.09) +
    theme_minimal() +
    labs(
      x = NULL,
      y = "Sensitivity in Loss Cost in %",
      title = "Modifiers with >10% Sensitivity Change"
    ) +
    theme(
      axis.text.x = element_text(angle = 55, hjust = 1, size = 20),
      axis.text.y = element_text(size = 20),
      axis.title.y = element_text(size = 20, face = "bold"),
      plot.title = element_text(hjust = 0.5, size = 22),
      strip.text = element_text(size = 20, face = "bold"),
      legend.position = "top",
      legend.title = element_blank(),
      legend.text = element_text(size = 20),
      panel.spacing = unit(3, "lines"),
      plot.margin = margin(t = 30, r = 10, b = 30, l = 10),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5)
    )
  
  return(p)
}






## --------------------------------------------------------------------------------------------------------------------------

# State-wide impact
STATE_plot <- function(one_HDv1, LOB, state_code, palette = c("#6FACDE", "#F0B323"),
                       axis_wrap_width = 30) {
  
  interval_sfd <- c(0, seq(-0.9, 5, by = 0.3))  # existing code
  
  p_sfd <- ggplot(
    data = one_HDv1 %>%
      subset(modifier != "Unknown" & type == LOB & STATECODE == state_code),
    mapping = aes(x = modifier, y = value, fill = variable)
  ) +
    geom_col(width = 0.7) +
    # X-tick (modifier) word wrap merged into the existing scale_x_discrete()
    # call -- a plot can only have one x scale, so the `labels=` formatter
    # goes here rather than as a separate `+ scale_x_discrete(...)` layer.
    scale_x_discrete(expand = c(0, 0), labels = function(x) str_wrap(x, width = axis_wrap_width)) +
    scale_y_continuous(
      expand = c(0, 0),
      labels = scales::percent,
      breaks = interval_sfd,
      limits = c(-0.50, 0.50)        # <-- Added limit
    ) +
    scale_fill_manual(values = palette) +
    geom_hline(yintercept = 0, colour = "black", linetype = "solid", size = 0.09) +
    theme_minimal() +
    labs(x = NULL) +   # Suppress x-axis title
    ylab("Sensitivity in Loss Cost in %") +
    theme(
      axis.text.x  = element_text(angle = 90, vjust = 1, hjust = 1, size = 12),
      axis.text.y  = element_text(size = 12),
      axis.title.y = element_text(size = 14, face = "bold"),
      axis.title.x = element_blank(),   # also suppress via theme
      plot.title   = element_text(hjust = 0.5, size = 14),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
      panel.grid.minor = element_blank(),
      legend.position  = "top",
      legend.title     = element_blank()
    )
  
  return(p_sfd)
}


## --------------------------------------------------------------------------------------------------------------------------
save_STATE_plots <- function(one_HDv1,
                             out_dir = "byStatebyLOB",
                             LOBs = c("COM", "SFD"),
                             width = 10, height = 5, dpi = 300,
                             y_limits = c(-0.50, 0.50),
                             y_breaks = seq(-0.5, 0.5, by = 0.1),
                             palette = c("#6FACDE", "#F0B323"),
                             axis_wrap_width = 30) {
  
  one_HDv1 <- as.data.frame(one_HDv1)
  
  # all states
  states <- sort(unique(one_HDv1$STATECODE))
  
  # root folder
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  
  # store plots (optional)
  out_plots <- list()
  
  for (lob in LOBs) {
    
    lob_dir <- file.path(out_dir, lob)
    dir.create(lob_dir, showWarnings = FALSE, recursive = TRUE)
    
    out_plots[[lob]] <- list()
    
    for (st in states) {
      
      p <- STATE_plot(
        one_HDv1 = one_HDv1,
        LOB = lob,
        state_code = st,
        palette = palette,
        axis_wrap_width = axis_wrap_width
      )
      
      # Save: e.g., TX_COM.png
      f <- file.path(lob_dir, paste0(st, "_", lob, ".png"))
      ggsave(filename = f, plot = p, width = width, height = height, dpi = dpi)
      
      out_plots[[lob]][[st]] <- p
    }
  }
  
  invisible(out_plots)
}

## --------------------------------------------------------------------------------------------------------------------------


