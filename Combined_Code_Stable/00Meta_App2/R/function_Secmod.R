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
indmod <- function(aal_final) {
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
      theme(axis.text.x = element_text(angle = 90, vjust = 0, hjust = 0)) +
      scale_fill_manual(values = mycolors)
  }
  return(pl)
}

## --------------------------------------------------------------------------------------------------------------------------

indmod_forpng <- function(aal_final,
                          out_dir = "indmod_plots",
                          width = 7, height = 5, dpi = 300,
                          drop_unknown_modifier = TRUE) {
  
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
        theme_bw() +
        theme(
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
          panel.grid.major.x = element_blank()
        ) +
        scale_fill_manual(values = mycolors)
      
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

Credit_Penalty <- function(table_minmax_USA, type_colors) {
  
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
               label = "No modifier exceeds the ±10% threshold", 
               size = 6, hjust = 0.5) +
      theme_void() +
      labs(title = "No significant sensitivity changes")
    return(p)
  }
  
  # ---- Proceed with plotting ----
  df_filtered$Description <- str_wrap(df_filtered$Description, width = 12)
  
  p <- ggplot(df_filtered, aes(x = Modifier, y = Value, fill = Type)) +
    geom_col(width = 0.6, position = "identity") +
    geom_text(aes(label = Description),
              vjust = ifelse(df_filtered$Value >= 0, -0.2, 1.3),
              size = 5.2, angle = 0) +
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
STATE_plot <- function(one_HDv1, LOB, state_code) {
  
  interval_sfd <- c(0, seq(-0.9, 5, by = 0.3))  # existing code
  
  p_sfd <- ggplot(
    data = one_HDv1 %>%
      subset(modifier != "Unknown" & type == LOB & STATECODE == state_code),
    mapping = aes(x = modifier, y = value, fill = variable)
  ) +
    geom_col(width = 0.7) +
    scale_x_discrete(expand = c(0, 0)) +
    scale_y_continuous(
      expand = c(0, 0),
      labels = scales::percent,
      breaks = interval_sfd,
      limits = c(-0.50, 0.50)        # <-- Added limit
    ) +
    scale_fill_manual(values = mycolors) +
    geom_hline(yintercept = 0, colour = "black", linetype = "solid", size = 0.09) +
    theme_minimal() +
    ylab("Sensitivity in Loss Cost in %") +
    theme(
      axis.text.x  = element_text(angle = 90, vjust = 1, hjust = 1, size = 12),
      axis.text.y  = element_text(size = 12),
      axis.title.y = element_text(size = 14, face = "bold"),
      axis.title.x = element_blank(),
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
                             y_breaks = seq(-0.5, 0.5, by = 0.1)) {
  
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
        y_limits = y_limits,
        y_breaks = y_breaks
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









# 
# 
# source_dir<-"C:/Users/hp/Downloads/Vulsen/module/"
# setwd(source_dir)                      # if you have this directory
# test<-source_dir
# 
# aal_State<- readRDS(paste0(source_dir,"/AllPerils_secmod_average_state_HDv1.rds")) #Changed the name of variable to aal_State whi ch was aal_HDv1 earlier, to make it Generic
# aal_USA<- readRDS(paste0(source_dir,"/AllPerils_secmod_average_USA_HDv1.rds"))
# 
# # Rename a column in the database
# # aal_State <- aal_State %>%  dplyr::rename("AAL" = 'HDv1')
# # aal_USA <- aal_USA %>%  dplyr::rename("AAL" = 'HDv1')
# 
# 
# type_colors <- c("Max" = "#F0B323",  # yellow
#                  "Min" = "#6FACDE")  # blue
# 
# mycolors <- c(rgb(111,172,222,maxColorValue = 255),rgb(240,179,35,maxColorValue = 255))
# 
# theme_set(  theme_bw()  %+replace% 
#               # no box
#               theme(strip.background = element_blank()) + 
#               # for box
#               # theme(strip.background = element_rect(fill = NA, color = "black")) + 
#               theme(strip.placement = "outside") + 
#               # no title for legend
#               theme(legend.title = element_blank() , 
#                     legend.position = "top",
#                     legend.spacing.x = unit(1,"mm"), 
#                     legend.box.background = element_rect(),
#                     legend.background = element_blank()
#               )
# )
# 
# 
# 
# # For All Peril
# 
# SecMod_name<-data.frame("modifer"=c("Unknown","ARCHITECT","CLADRATE", "CLADSYS","CONSTQUALI", "EXTORN", "FOUNDSYS", "GARAGING", "MECHGROUND", "RESISTOPEN","ROOFAGE", "ROOFANCH", "ROOFEQUIP", "ROOFGEOM", "ROOFMAINT","ROOFSYS","TODMGPROVISION","TOFLASHING","TREEDENSITY"),
#                         "name"=c("Unknown","Residential Exterior","Roof Sheathing Attachment", "Cladding Type","Construction Quality", "Commercial   Exterior", "Frame-Foundation Connection", "Garaging", "Ground-Level Equipment", "Opening Protection","Roof Age", "Roof Anchor", "Rooftop Equipment", "Roof Geometry", "Roof Condition","Roof covering","Damage Provision","Flashing and Coping Quality","Tree Density"))
# 
# 
# 
# 
# ## --------------------------------------------------------------------------------------------------------------------------
# aal_final<-finaltable(aal_State,SecMod_name)  
# # # write.table(aal_final,paste0(source_dir,"AAL_secmod_AllPeril_HDv1_byState.csv"),sep=",",row.names = F,quote=T)
# # 
# 
# 
# aal_final_USA<-finaltable_allUSA(aal_USA,SecMod_name)  
# # write.table(aal_final_USA,paste0(source_dir,"AAL_secmod_AllPeril_HDv1_USA.csv"),sep=",",row.names = F,quote=T)
# 
# 
# 
# 
# 
# 
# 
# 
# aalp<-STATEminmax(aal_final,SecMod_name)
# aalp_USA<-Countryminmax(aal_final_USA,SecMod_name)
# 
# 
# table_minmax_USA<-CountryminmaxTable(aal_final_USA,SecMod_name)
# p <- Credit_Penalty (table_minmax_USA,type_colors) 
# print(p)
# 
# 
# 
# unique_states <- sort(unique(aalp$STATECODE))
# 
# for (state in unique_states) {
#   cat("### Plot for SFD for state of ", state, "\n\n", sep = "")
#   print(STATE_plot(aalp, LOB = "SFD", state_code = state))
#   cat("\n\n")
# }
# 
# 
# 
# for (state in unique_states) {
#   cat("### Plot for COM for state of ", state, "\n\n", sep = "")
#   print(STATE_plot(aalp, LOB = "COM", state_code = state))
#   cat("\n\n")
# }
# 
# 
# 
# 
# classif = levels(aal_final$modifier)[-match("Unknown",levels(aal_final$modifier))]
# 
# for (i in 1:length(classif)) {
#   cat("## Plot for ", classif[i], "\n\n", sep = "")
#   print(indmod(aal_final)[[i]])
#   cat("\n\n")
# }
# 





































# 
# 
# 
# # ===============================================================
# # Script Name: generate_synthetic_secmod_data.R
# # Author: Dr.SKM
# # Purpose:
# #   Generate synthetic aal_State and aal_USA datasets that
# #   match the structure expected by finaltable()
# # ===============================================================
# 
# set.seed(2026)
# 
# # ===============================
# # States & LOBs
# # ===============================
# 
# state_codes <- c(
#   "AL","AR","AZ","CA","CO","CT","DC","DE","FL","GA",
#   "IA","ID","IL","IN","KS","KY","LA","MA","MD","ME",
#   "MI","MN","MO","MS","MT","NC","ND","NE","NH","NJ",
#   "NM","NV","NY","OH","OK","OR","PA","RI","SC","SD",
#   "TN","TX","UT","VA","VT","WA","WI","WV","WY"
# )
# 
# LOB <- c("COM","SFD")
# 
# # ===============================
# # Modifier dictionary
# # ===============================
# 
# modifier_dict <- list(
#   
#   ARCHITECT = c(
#     "Attached screen","Detached screen","Fences",
#     "None","Skylights","Skylights protection"),
#   
#   CLADRATE = c(
#     "10d high wind nail","6d any nail","8d high wind nail",
#     "8d minimum nail","Batten decking","Lumber and groove"),
#   
#   CLADSYS = c(
#     "Vinyl siding","EIFS","Wood","Metal sheathing",
#     "Impact rated glazing","Stucco","Brick veneer","Concrete block"),
#   
#   CONSTQUALI = c(
#     "Signs of distress",
#     "WH Roof existing","Wind Roof existing",
#     "Wind Gold existing","Wind Silver existing",
#     "WH Gold existing","WH Silver existing"),
#   
#   EXTORN = c(
#     "Extensive ornamentation",
#     "Extensive ornamentation and LPS",
#     "Large signs",
#     "Large signs and LPS",
#     "Lightning protection"),
#   
#   FOUNDSYS = c(
#     "Bolted","Engineered","Unbolted"),
#   
#   GARAGING = c(
#     "Attached garage","Detached garage","No garage"),
#   
#   MECHGROUND = c(
#     "Generally unprotected",
#     "Generally protected"),
#   
#   RESISTOPEN = c(
#     "Pressure small missile rated",
#     "Pressure medium missile rated",
#     "All openings pressure small missile rated",
#     "All openings pressure medium missile rated",
#     "All openings pressure large missile rated"),
#   
#   ROOFAGE = c(
#     "0 to 5 years",
#     "6 to 10 years",
#     "11 years or more"),
#   
#   ROOFANCH = c(
#     "Clips","Double wraps",
#     "Single wraps","Structural",
#     "Toenailing no anchorage"),
#   
#   ROOFEQUIP = c(
#     "No equipment present",
#     "Roof mounted photovoltaic array",
#     "Roof mounted impact resistant array",
#     "Roof mounted mechanically attached array"),
#   
#   ROOFGEOM = c(
#     "Complex roof",
#     "Flat roof no parapets",
#     "Flat roof parapets",
#     "Gable roof LE6:12",
#     "Braced gable LE6:12",
#     "Gable roof GT6:12",
#     "Braced gable GT6:12",
#     "Hip roof LE6:12",
#     "Hip roof GT6:12"),
#   
#   ROOFMAINT = c(
#     "Excellent condition",
#     "No deterioration or defects",
#     "Visible defects",
#     "Significant signs of distress"),
#   
#   ROOFSYS = c(
#     "Built up or single ply",
#     "Built up with gutters",
#     "Built up without gutters",
#     "Built up unknown gutters",
#     "High wind shingle",
#     "High wind shingle plus SWR",
#     "Normal shingle",
#     "Normal shingle plus SWR",
#     "Metal concealed fasteners",
#     "Metal exposed fasteners",
#     "Metal unknown fasteners",
#     "Wood shakes",
#     "Concrete roof",
#     "Hail class 1",
#     "Hail class 2",
#     "Hail class 3",
#     "Hail class 4",
#     "Rubber roof",
#     "Green roof",
#     "Bermuda style roof",
#     "Concrete tiles",
#     "Slate tile",
#     "Fabric membrane",
#     "Solar tile"),
#   
#   TODMGPROVISION = c(
#     "Cosmetic damage Fully covered",
#     "Cosmetic damage Mostly covered",
#     "Cosmetic damage Default",
#     "Cosmetic damage Rarely covered",
#     "Cosmetic damage Excluded"),
#   
#   TOFLASHING = c(
#     "Compliant with ES1",
#     "Not compliant with ES1"),
#   
#   TREEDENSITY = c(
#     "High tree risk high shielding",
#     "Some trees some shielding",
#     "No trees no shielding"),
#   
#   Unknown = "Unknown"
#   
# )
# 
# # ===============================
# # Generate State data
# # ===============================
# 
# rows <- list()
# k <- 1
# 
# for(st in state_codes){
#   
#   for(tp in LOB){
#     
#     unknown_aal <- round(runif(1,250,700),6)
#     
#     ## Unknown
#     rows[[k]] <- data.frame(
#       STATECODE=st,
#       Classification=paste0(tp,"_Unknown"),
#       Description="Unknown",
#       AAL=unknown_aal,
#       stringsAsFactors=FALSE
#     )
#     k <- k+1
#     
#     ## Remaining modifiers
#     for(mod in setdiff(names(modifier_dict),"Unknown")){
#       
#       modifier_aal <- round(
#         unknown_aal*runif(1,0.60,1.40),
#         6
#       )
#       
#       rows[[k]] <- data.frame(
#         STATECODE=st,
#         Classification=paste0(tp,"_",mod),
#         Description=modifier_dict[[mod]],
#         AAL=rep(modifier_aal,length(modifier_dict[[mod]])),
#         stringsAsFactors=FALSE
#       )
#       
#       k <- k+1
#     }
#     
#   }
#   
# }
# 
# aal_State <- dplyr::bind_rows(rows)
# 
# # ===============================
# # USA data
# # ===============================
# 
# aal_USA <-
#   aal_State |>
#   dplyr::group_by(Classification,Description) |>
#   dplyr::summarise(
#     AAL=mean(AAL),
#     .groups="drop"
#   )
# 
# # ===============================
# # Save
# # ===============================
# 
# saveRDS(aal_State,"AllPerils_secmod_average_state_HDv1.rds")
# saveRDS(aal_USA,"AllPerils_secmod_average_USA_HDv1.rds")
# 
# 
# 















