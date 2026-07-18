
set_script_wd <- function() {
  # Try RStudio (if running in RStudio)
  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    script_path <- rstudioapi::getActiveDocumentContext()$path
    if (nzchar(script_path)) {
      setwd(dirname(script_path))
      message("Working directory set to: ", getwd())
      return(invisible(getwd()))
    }
  }
  
  # Try command-line arguments (Rscript)
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg) > 0) {
    script_path <- sub("^--file=", "", file_arg[1])
    # If path is relative, convert to absolute
    if (!file.exists(script_path)) {
      # Try to find absolute path by joining with current dir
      script_path <- file.path(getwd(), script_path)
    }
    if (file.exists(script_path)) {
      setwd(dirname(script_path))
      message("Working directory set to: ", getwd())
      return(invisible(getwd()))
    }
  }
  
  # Try using sys.calls() to find source file
  calls <- sys.calls()
  for (i in rev(seq_along(calls))) {
    call <- as.character(calls[[i]])[1]
    if (grepl("^source\\(", call)) {
      # Extract filename
      script_path <- gsub("^source\\(", "", call)
      script_path <- gsub("\\)$", "", script_path)
      script_path <- tryCatch(eval(parse(text = script_path)), error = function(e) NULL)
      if (is.character(script_path) && file.exists(script_path)) {
        setwd(dirname(script_path))
        message("Working directory set to: ", getwd())
        return(invisible(getwd()))
      }
    }
  }
  
  # Fallback: use current working directory
  message("Could not detect script directory. Keeping current working directory: ", getwd())
  invisible(getwd())
}

# Call the function
set_script_wd()




# Enhanced modular function to combine code files
combine_code_with_prompt <- function(parent_folder = getwd(),
                                     output_file = NULL,
                                     file_types = NULL,
                                     recursive = TRUE,
                                     exclude_patterns = c("\\.rds$", "\\.RData$", "\\.Rhistory$", 
                                                          "~$", "\\.bak$", "\\.log$"),
                                     max_file_size_mb = 10,
                                     include_header = TRUE,
                                     include_structure = TRUE,
                                     quiet = FALSE) {
  
  # Check if folder exists
  if (!dir.exists(parent_folder)) {
    stop("❌ Folder not found: ", parent_folder)
  }
  
  # Normalize path
  parent_folder <- normalizePath(parent_folder, winslash = "/")
  
  # Set default output file name if not provided
  if (is.null(output_file)) {
    output_file <- file.path(parent_folder, paste0("Masanta_Dental_combined_code_", 
                                                   format(Sys.time(), "%Y%m%d_%H%M%S"), 
                                                   ".txt"))
  }
  
  # Define available file types
  available_types <- list(
    "R Scripts" = "r",
    "R Scripts (uppercase)" = "R", 
    "R Markdown" = "Rmd",
    "CSS Styles" = "css",
    "JavaScript" = "js",
    "HTML" = "html",
    "Python" = "py",
    "Text files" = "txt",
    "JSON" = "json",
    "YAML" = "yml",
    "Shell Scripts" = "sh",
    "Markdown" = "md",
    "Configuration" = c("conf", "config", "ini"),
    "XML" = "xml"
  )
  
  # If file_types not provided interactively, show menu
  if (is.null(file_types)) {
    cat("\n", paste(rep("=", 80), collapse = ""), "\n")
    cat("📁 SELECT FILE TYPES TO COMBINE\n")
    cat(paste(rep("=", 80), collapse = ""), "\n\n")
    
    # Display menu with categories
    type_names <- names(available_types)
    for (i in seq_along(type_names)) {
      ext_display <- paste0(".", paste(available_types[[i]], collapse = ", ."))
      cat(sprintf("%2d. %-25s (%s)\n", i, type_names[i], ext_display))
    }
    cat("\n 0. Select all types\n")
    
    # Get user selection
    cat("\nEnter numbers separated by commas (e.g., 1,2,3): ")
    selection <- readline(prompt = "")
    
    # Parse selection
    if (trimws(selection) == "0" || tolower(selection) == "all") {
      selected_extensions <- unique(unlist(available_types))
      cat("\n✅ Selected all file types\n")
    } else {
      selected_nums <- as.numeric(unlist(strsplit(selection, ",")))
      selected_nums <- selected_nums[!is.na(selected_nums) & selected_nums >= 1 & selected_nums <= length(available_types)]
      if (length(selected_nums) == 0) {
        cat("❌ Invalid selection. Using all file types.\n")
        selected_extensions <- unique(unlist(available_types))
      } else {
        selected_extensions <- unique(unlist(available_types[selected_nums]))
        cat("\n✅ Selected extensions:", paste(selected_extensions, collapse = ", "), "\n")
      }
    }
  } else {
    selected_extensions <- file_types
    cat("\n✅ Using predefined extensions:", paste(selected_extensions, collapse = ", "), "\n")
  }
  
  # Build pattern
  pattern <- paste0("\\.(", paste(selected_extensions, collapse = "|"), ")$")
  
  # Find all matching files
  if (!quiet) cat("\n🔍 Searching for files...\n")
  all_files <- list.files(
    path = parent_folder,
    pattern = pattern,
    full.names = TRUE,
    recursive = recursive,
    ignore.case = TRUE
  )
  
  # Remove duplicates and apply exclude patterns
  all_files <- unique(all_files)
  
  # Filter by exclude patterns
  exclude_pattern <- paste(exclude_patterns, collapse = "|")
  if (exclude_pattern != "") {
    all_files <- all_files[!grepl(exclude_pattern, basename(all_files), ignore.case = TRUE)]
  }
  
  # Filter by file size
  file_sizes <- file.info(all_files)$size / (1024^2)  # Size in MB
  all_files <- all_files[file_sizes <= max_file_size_mb | is.na(file_sizes)]
  
  if (length(all_files) == 0) {
    cat("❌ No files found with selected extensions.\n")
    return(invisible(NULL))
  }
  
  if (!quiet) cat("✅ Found", length(all_files), "files\n")
  
  # Sort files by directory for better organization
  all_files <- sort(all_files)
  
  # Open connection for output
  con <- file(output_file, "w", encoding = "UTF-8")
  
  # ============================================================
  # WRITE HEADER
  # ============================================================
  if (include_header) {
    writeLines(paste0(rep("#", 80), collapse = ""), con)
    writeLines(paste0("#", paste(rep(" ", 78), collapse = ""), "#"), con)
    writeLines("#                     APPLICATION - COMPLETE CODE                      #", con)
    writeLines(paste0("#", paste(rep(" ", 78), collapse = ""), "#"), con)
    writeLines(paste0(rep("#", 80), collapse = ""), con)
    writeLines(paste("# Generated:", format(Sys.time(), "%Y-%m-%d %H:%M:%S")), con)
    writeLines(paste("# Source Folder:", parent_folder), con)
    writeLines(paste("# Total Files:", length(all_files)), con)
    writeLines(paste("# File Types:", paste(selected_extensions, collapse = ", ")), con)
    writeLines(paste("# Recursive:", recursive), con)
    writeLines(paste("# Max File Size:", max_file_size_mb, "MB"), con)
    writeLines(paste0(rep("#", 80), collapse = ""), con)
    writeLines("", con)
    writeLines("", con)
  }
  
  # ============================================================
  # SECTION 1: TABLE OF CONTENTS
  # ============================================================
  if (include_structure) {
    writeLines(paste0(rep("#", 80), collapse = ""), con)
    writeLines("#                          📑 TABLE OF CONTENTS                                 #", con)
    writeLines(paste0(rep("#", 80), collapse = ""), con)
    writeLines("", con)
    
    # Group files by directory
    files_by_dir <- split(all_files, dirname(all_files))
    
    toc_counter <- 1
    for (dir in names(files_by_dir)) {
      rel_path <- gsub(paste0("^", parent_folder, "/?"), "", dir)
      if (rel_path == "") rel_path <- "Root"
      
      writeLines(sprintf("%3d. 📁 %s/ (%d files)", toc_counter, rel_path, length(files_by_dir[[dir]])), con)
      toc_counter <- toc_counter + 1
      
      for (file in files_by_dir[[dir]]) {
        file_name <- basename(file)
        writeLines(sprintf("       📄 %s", file_name), con)
      }
      writeLines("", con)
    }
    writeLines("", con)
  }
  
  # ============================================================
  # SECTION 2: FOLDER STRUCTURE
  # ============================================================
  if (include_structure) {
    writeLines(paste0(rep("#", 80), collapse = ""), con)
    writeLines("#                          📁 FOLDER STRUCTURE                                 #", con)
    writeLines(paste0(rep("#", 80), collapse = ""), con)
    writeLines("", con)
    
    # Get all unique directories
    all_dirs <- sort(unique(dirname(all_files)))
    
    for (dir in all_dirs) {
      # Calculate relative path
      rel_path <- gsub(paste0("^", parent_folder, "/?"), "", dir)
      if (rel_path == "") rel_path <- "📁 Root"
      
      # Count files in this directory
      files_in_dir <- all_files[dirname(all_files) == dir]
      
      writeLines(paste("\n📂", rel_path, paste0("(", length(files_in_dir), " files)")), con)
      writeLines(paste(rep("─", 80), collapse = ""), con)
      
      # List files in this directory with metadata
      for (file in sort(files_in_dir)) {
        file_name <- basename(file)
        file_ext <- tools::file_ext(file_name)
        file_size_mb <- file.info(file)$size / (1024^2)
        size_str <- ifelse(file_size_mb < 0.001, 
                           paste0(round(file_size_mb * 1024, 1), " KB"),
                           paste0(round(file_size_mb, 2), " MB"))
        
        # Add icon based on file type
        icon <- switch(tolower(file_ext),
                       "r" = "📜", "rmd" = "📝", "css" = "🎨", "js" = "⚡",
                       "py" = "🐍", "json" = "📋", "txt" = "📄", "html" = "🌐",
                       "md" = "📖", "sh" = "⚙️", "yml" = "🔧", "xml" = "📰",
                       "📄")
        
        # Add line count for text files
        line_count <- ""
        if (tolower(file_ext) %in% c("r", "R", "rmd", "py", "js", "css", "txt", "md")) {
          tryCatch({
            lines <- length(readLines(file, warn = FALSE))
            line_count <- paste0(", ", lines, " lines")
          }, error = function(e) {})
        }
        
        writeLines(paste("  ", icon, file_name, paste0("(", size_str, line_count, ")")), con)
      }
    }
    
    writeLines("", con)
    writeLines("", con)
  }
  
  # ============================================================
  # SECTION 3: CODE FILES
  # ============================================================
  writeLines(paste0(rep("#", 80), collapse = ""), con)
  writeLines("#                          📄 CODE FILES                                        #", con)
  writeLines(paste0(rep("#", 80), collapse = ""), con)
  writeLines("", con)
  
  # Progress tracking
  total_files <- length(all_files)
  processed <- 0
  
  for (i in seq_along(all_files)) {
    file_path <- all_files[i]
    file_name <- basename(file_path)
    rel_path <- gsub(paste0("^", parent_folder, "/?"), "", file_path)
    file_ext <- tools::file_ext(file_name)
    file_size_mb <- file.info(file_path)$size / (1024^2)
    
    # Progress indicator
    processed <- processed + 1
    if (!quiet && processed %% 10 == 0) {
      cat(sprintf("\r📦 Processing: %d/%d files (%.1f%%)", processed, total_files, 
                  processed/total_files*100))
    }
    
    # File header
    writeLines("", con)
    writeLines(paste0(rep("#", 80), collapse = ""), con)
    writeLines(paste0("# FILE ", sprintf("%3d", i), " OF ", total_files), con)
    writeLines(paste0(rep("#", 80), collapse = ""), con)
    writeLines(paste("# Name:", file_name), con)
    writeLines(paste("# Path:", rel_path), con)
    writeLines(paste("# Type:", ifelse(file_ext == "", "unknown", toupper(file_ext))), con)
    writeLines(paste("# Size:", ifelse(file_size_mb < 0.001, 
                                       paste0(round(file_size_mb * 1024, 1), " KB"),
                                       paste0(round(file_size_mb, 2), " MB"))), con)
    
    # Add line count for text files
    if (tolower(file_ext) %in% c("r", "R", "rmd", "py", "js", "css", "txt", "md", "html", "json")) {
      tryCatch({
        lines <- readLines(file_path, warn = FALSE)
        writeLines(paste("# Lines:", length(lines)), con)
      }, error = function(e) {})
    }
    
    writeLines(paste0(rep("#", 80), collapse = ""), con)
    writeLines("", con)
    
    # Read and write file content
    file_content <- tryCatch({
      readLines(file_path, warn = FALSE, encoding = "UTF-8")
    }, error = function(e) {
      c(paste("# ERROR: Could not read file -", e$message))
    })
    
    writeLines(file_content, con)
    writeLines("", con)
  }
  
  # Close connection
  close(con)
  
  if (!quiet) cat("\n")
  
  # Print summary
  cat("\n", paste(rep("=", 80), collapse = ""), "\n")
  cat("✅ COMBINATION COMPLETE!\n")
  cat(paste(rep("=", 80), collapse = ""), "\n")
  cat("📄 Output file:", output_file, "\n")
  cat("📊 Total files combined:", length(all_files), "\n")
  cat("📁 File types:", paste(selected_extensions, collapse = ", "), "\n")
  cat("💾 Output size:", round(file.info(output_file)$size / (1024^2), 2), "MB\n")
  
  # Show breakdown by file type
  cat("\n📊 BREAKDOWN BY FILE TYPE:\n")
  ext_counts <- table(tools::file_ext(all_files))
  ext_counts <- sort(ext_counts, decreasing = TRUE)
  for (ext in names(ext_counts)) {
    if (ext == "") next
    cat(sprintf("   .%-10s: %3d files\n", ext, ext_counts[ext]))
  }
  
  # Show largest files
  cat("\n📊 LARGEST FILES (by lines):\n")
  file_stats <- data.frame(
    file = basename(all_files),
    lines = sapply(all_files, function(f) {
      tryCatch(length(readLines(f, warn = FALSE)), error = function(e) 0)
    }),
    stringsAsFactors = FALSE
  )
  file_stats <- file_stats[order(-file_stats$lines), ]
  head_files <- head(file_stats[file_stats$lines > 0, ], 10)
  for (i in 1:nrow(head_files)) {
    cat(sprintf("   %2d. %-40s (%5d lines)\n", i, head_files$file[i], head_files$lines[i]))
  }
  
  return(invisible(list(
    output_file = output_file,
    total_files = length(all_files),
    file_types = selected_extensions,
    files = all_files,
    file_stats = file_stats
  )))
}

# ============================================================
# QUICK PRESET FUNCTIONS
# ============================================================

# Combine only R files
combine_r_files <- function(parent_folder = getwd(), output_file = NULL) {
  combine_code_with_prompt(
    parent_folder = parent_folder,
    output_file = output_file,
    file_types = c("r", "R", "Rmd"),
    quiet = FALSE
  )
}

# Combine all code files (no interaction)
combine_all_code <- function(parent_folder = getwd(), output_file = NULL) {
  combine_code_with_prompt(
    parent_folder = parent_folder,
    output_file = output_file,
    file_types = c("r", "R", "Rmd", "py", "js", "css", "html", "sh", "md", "txt"),
    quiet = FALSE
  )
}


# ============================================================
# USAGE EXAMPLES
# ============================================================

# Example 1: Interactive mode (default)
# result <- combine_code_with_prompt()

# Example 2: Preset - only R files
# result <- combine_r_files()

# Example 3: Preset - all code files
# result <- combine_all_code()

# Example 4: Quick combine with custom path
# result <- quick_combine()

# Example 5: Non-interactive with specific types
# result <- combine_code_with_prompt(
#   parent_folder = "C:/Users/hp/Downloads/USapp/2026_Dreamlit_lite",
#   file_types = c("r", "R", "Rmd", "css"),
#   quiet = FALSE
# )

















write_folder_architecture <- function(folder_path, output_file = "folder_architecture.txt", 
                                      exclude_patterns = c("\\.rds$", "\\.RData$", "\\.Rhistory$", 
                                                           "^\\.", "~$", "\\.bak$"),
                                      max_depth = NULL, show_file_size = TRUE) {
  
  # Check if folder exists
  if (!dir.exists(folder_path)) {
    stop("Folder does not exist: ", folder_path)
  }
  
  # Normalize path
  folder_path <- normalizePath(folder_path, winslash = "/")
  
  # Open connection
  con <- file(output_file, "w", encoding = "UTF-8")
  
  # Write header
  writeLines(paste0(rep("=", 80), collapse = ""), con)
  writeLines("📁 FOLDER ARCHITECTURE REPORT", con)
  writeLines(paste0(rep("=", 80), collapse = ""), con)
  writeLines(paste0("Generated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")), con)
  writeLines(paste0("Root Path: ", folder_path), con)
  writeLines("", con)
  
  # Count files
  all_files <- list.files(folder_path, recursive = TRUE, full.names = TRUE)
  writeLines(paste0("📊 Total files found: ", length(all_files)), con)
  writeLines("", con)
  
  # Function to check if file should be excluded
  should_exclude <- function(file_path) {
    basename_file <- basename(file_path)
    for (pattern in exclude_patterns) {
      if (grepl(pattern, basename_file, ignore.case = TRUE)) {
        return(TRUE)
      }
    }
    return(FALSE)
  }
  
  # Recursive function to print tree
  print_tree <- function(path, prefix = "", is_last = TRUE, depth = 0, con) {
    
    # Check depth limit
    if (!is.null(max_depth) && depth > max_depth) {
      return()
    }
    
    # Get base name
    base_name <- basename(path)
    
    # Determine connector
    connector <- if (is_last) "└── " else "├── "
    
    # Get file/dir info
    if (dir.exists(path)) {
      # Directory
      writeLines(paste0(prefix, connector, "📁 ", base_name, "/"), con)
      
      # Get children
      items <- list.files(path, full.names = TRUE, all.files = FALSE, no.. = TRUE)
      
      # Separate dirs and files
      dirs <- items[dir.exists(items)]
      files <- items[!dir.exists(items)]
      
      # Sort
      dirs <- sort(dirs)
      files <- sort(files)
      
      # Filter files
      files <- files[!sapply(files, should_exclude)]
      
      # Combine
      children <- c(dirs, files)
      
      # Update prefix
      new_prefix <- paste0(prefix, if (is_last) "    " else "│   ")
      
      # Process children
      for (i in seq_along(children)) {
        is_last_child <- (i == length(children))
        print_tree(children[i], new_prefix, is_last_child, depth + 1, con)
      }
      
    } else {
      # File - show with icon based on extension
      ext <- tools::file_ext(base_name)
      icon <- switch(tolower(ext),
                     "r" = "📜",
                     "rmd" = "📝",
                     "rds" = "💾",
                     "csv" = "📊",
                     "json" = "🔧",
                     "txt" = "📄",
                     "css" = "🎨",
                     "html" = "🌐",
                     "png" = "🖼️",
                     "jpg" = "🖼️",
                     "jpeg" = "🖼️",
                     "pdf" = "📕",
                     "md" = "📋",
                     "py" = "🐍",
                     "sh" = "⚙️",
                     "📄"
      )
      
      # Get file size if requested
      size_info <- ""
      if (show_file_size) {
        file_size <- file.info(path)$size
        if (!is.na(file_size)) {
          if (file_size < 1024) {
            size_info <- paste0(" (", file_size, " B)")
          } else if (file_size < 1024^2) {
            size_info <- paste0(" (", round(file_size/1024, 1), " KB)")
          } else {
            size_info <- paste0(" (", round(file_size/1024^2, 1), " MB)")
          }
        }
      }
      
      writeLines(paste0(prefix, connector, icon, " ", base_name, size_info), con)
    }
  }
  
  # Print tree starting from root
  writeLines("", con)
  writeLines("📂 DIRECTORY STRUCTURE:", con)
  writeLines(paste0("📁 ", basename(folder_path), "/"), con)
  
  # Get root children
  root_items <- list.files(folder_path, full.names = TRUE, all.files = FALSE, no.. = TRUE)
  root_dirs <- root_items[dir.exists(root_items)]
  root_files <- root_items[!dir.exists(root_items)]
  
  # Sort
  root_dirs <- sort(root_dirs)
  root_files <- sort(root_files)
  
  # Filter files
  root_files <- root_files[!sapply(root_files, should_exclude)]
  
  # Combine
  root_children <- c(root_dirs, root_files)
  
  # Process root children
  for (i in seq_along(root_children)) {
    is_last_child <- (i == length(root_children))
    print_tree(root_children[i], "", is_last_child, 1, con)
  }
  
  # Write file summary by extension
  writeLines("", con)
  writeLines(paste0(rep("=", 80), collapse = ""), con)
  writeLines("📊 FILE SUMMARY BY TYPE:", con)
  writeLines(paste0(rep("=", 80), collapse = ""), con)
  
  # Get all files (excluding excluded patterns)
  all_files_filtered <- all_files[!sapply(all_files, should_exclude)]
  
  # Count by extension
  ext_counts <- table(tools::file_ext(all_files_filtered))
  ext_counts <- sort(ext_counts, decreasing = TRUE)
  
  for (ext in names(ext_counts)) {
    ext_display <- ifelse(ext == "", "no extension", toupper(ext))
    writeLines(sprintf("  %-15s: %d files", ext_display, ext_counts[ext]), con)
  }
  
  # Write file size summary
  writeLines("", con)
  writeLines(paste0(rep("=", 80), collapse = ""), con)
  writeLines("📊 FILE SIZE SUMMARY:", con)
  writeLines(paste0(rep("=", 80), collapse = ""), con)
  
  total_size <- sum(file.info(all_files_filtered)$size, na.rm = TRUE)
  if (total_size < 1024) {
    total_size_display <- paste0(total_size, " B")
  } else if (total_size < 1024^2) {
    total_size_display <- paste0(round(total_size/1024, 2), " KB")
  } else if (total_size < 1024^3) {
    total_size_display <- paste0(round(total_size/1024^2, 2), " MB")
  } else {
    total_size_display <- paste0(round(total_size/1024^3, 2), " GB")
  }
  
  writeLines(paste0("  Total size: ", total_size_display), con)
  
  # Largest files
  if (length(all_files_filtered) > 0) {
    file_sizes <- file.info(all_files_filtered)$size
    largest_idx <- order(file_sizes, decreasing = TRUE)[1:min(10, length(all_files_filtered))]
    
    writeLines("", con)
    writeLines("📊 LARGEST FILES:", con)
    for (idx in largest_idx) {
      size <- file_sizes[idx]
      if (size < 1024) {
        size_display <- paste0(size, " B")
      } else if (size < 1024^2) {
        size_display <- paste0(round(size/1024, 1), " KB")
      } else {
        size_display <- paste0(round(size/1024^2, 1), " MB")
      }
      writeLines(sprintf("  %-50s %s", basename(all_files_filtered[idx]), size_display), con)
    }
  }
  
  # Close connection
  close(con)
  
  cat("\n✅ Architecture written to:", output_file, "\n")
  cat("📊 Total files:", length(all_files_filtered), "\n")
  cat("📁 Total directories:", length(list.dirs(folder_path, recursive = FALSE)), "\n")
  
  return(invisible(output_file))
}

# ============================================================
# RUN THE FUNCTION
# ============================================================
folder_path = getwd()
setwd(folder_path)
cat("Working directory:", getwd(), "\n")



# Quick combine (all R files, default output)
quick_combine <- function() {
  combine_r_files(
    parent_folder = folder_path,
    output_file = "Vulsen_combined_code.txt"
  )
}



# Run the function
result <- combine_code_with_prompt(
  parent_folder = folder_path,
  output_file = "Vulsen_combined_code.txt"
)






# Write architecture of Dreamlit_Lite folder
write_folder_architecture(
  folder_path = folder_path,
  output_file = "Vulsen_architecture.txt",
  exclude_patterns = c("\\.rds$", "\\.RData$", "\\.Rhistory$", "~$", "\\.bak$"),
  max_depth = NULL,
  show_file_size = TRUE
)














