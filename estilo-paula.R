import_roboto_condensed <- function() {
  # Define a local directory for the font
  roboto_font_dir <- file.path(tempdir(), "roboto_condensed_fonts")
  dir.create(roboto_font_dir, showWarnings = FALSE)
  
  # URL of the Roboto Condensed font (variable weight) from Google Fonts GitHub
  font_url <- "https://github.com/google/fonts/raw/main/apache/robotocondensed/RobotoCondensed%5Bwght%5D.ttf"
  
  # Destination path
  font_path <- file.path(roboto_font_dir, "RobotoCondensed[wght].ttf")
  
  # Download the font if not already present
  if (!file.exists(font_path)) {
    download.file(font_url, destfile = font_path, mode = "wb")
  }
  
  # Import the font using extrafont
  suppressWarnings(suppressMessages(
    extrafont::font_import(paths = roboto_font_dir, prompt = FALSE)
  ))
  
  message(
    sprintf(
      "Roboto Condensed font imported from Google Fonts.\nYou may need to install it manually if system-wide use is needed.\n\nPath: [%s]",
      roboto_font_dir
    )
  )
}


# theme_paula for ggplot

theme_paula <- function(
    base_family = "RobotoCondensed-Regular",
    base_size = 16,
    plot_title_family = if (.Platform$OS.type == "windows") "RobotoCondensed-Regular" else "RobotoCondensed-Bold",
    plot_title_size = 19,
    plot_title_face = "bold",
    plot_title_margin = 14,
    subtitle_family = if (.Platform$OS.type == "windows") "RobotoCondensed-Regular" else "RobotoCondensed-Light",
    subtitle_size = 17,
    subtitle_face = "plain",
    subtitle_margin = 19,
    strip_text_family = base_family,
    strip_text_size = 16,
    strip_text_face = "plain",
    caption_family = if (.Platform$OS.type == "windows") "RobotoCondensed-Regular" else "RobotoCondensed-Regular",
    caption_size = 14,
    caption_face = "plain",
    caption_margin = 12,
    axis_text_size = base_size + 4,
    axis_title_family = base_family,
    axis_title_size = 14,
    axis_title_face = "plain",
    axis_title_just = "rt",
    plot_margin = margin(30, 30, 30, 30)) {
  
  ggplot2::theme_minimal(base_family = base_family, base_size = base_size)
  
}
