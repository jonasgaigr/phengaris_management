#----------------------------------------------------------#
#
#
#       Phengaris spp. management in Czechia
#
#     Tool - Render the compiled report as PDF
#
#
#----------------------------------------------------------#
#
# NOT part of the cascade: it needs a Chromium browser (Chrome, Edge or
# Chromium), which the cascade does not. Run it after step 15:
#
#   Rscript R/tools/render_report_pdf.R
#
# 1. Outputs/REPORT.md is converted to HTML with commonmark (GitHub-flavoured
#    tables). Each heading gets the anchor its entry in the contents links
#    to (heading_anchor()), and each figure its print size: the PNGs are
#    written at FIGURE_DPI at a journal column width, 8.5 or 17.5 cm.
# 2. The HTML, with the style below, is written to Outputs/REPORT.html.
# 3. The browser prints it in headless mode to Outputs/REPORT.pdf: A4, page
#    numbers, every scope and step on a new page, a bookmark per heading,
#    and the contents linked to the sections.
#
# The browser is the first of CHROME_PATH and the usual install locations
# of Chrome, Edge and Chromium that exists. It runs with a fresh profile, so
# a browser already open is not touched.
#
#----------------------------------------------------------#

if (!exists("CONFIG")) source("R/00_setup.R", encoding = "UTF-8")

if (!requireNamespace("commonmark", quietly = TRUE)) {
  stop("Package commonmark is missing. Run renv::restore() in the project root.", call. = FALSE)
}

#--------------------------------------------------#
## Helpers -----
#--------------------------------------------------#

#' The width of a PNG in pixels, from its header; NA if not a PNG.
png_width_px <- function(path) {
  if (!file.exists(path)) {
    return(NA_real_)
  }
  con <- file(path, "rb")
  on.exit(close(con))
  header <- readBin(con, "raw", 24)
  signature <- as.raw(c(0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a))
  if (length(header) < 24 || !identical(header[1:8], signature)) {
    return(NA_real_)
  }
  sum(as.numeric(header[17:20]) * 256^(3:0))
}

#' Give every heading the anchor its entry in the contents links to;
#' repeated headings get -1, -2, ... as on GitHub.
add_heading_ids <- function(html) {
  pattern <- "<h([1-6])>(.*?)</h\\1>"
  m <- gregexpr(pattern, html, perl = TRUE)
  headings <- regmatches(html, m)[[1]]
  if (!length(headings)) {
    return(html)
  }
  level <- sub(pattern, "\\1", headings, perl = TRUE)
  text <- sub(pattern, "\\2", headings, perl = TRUE)
  ids <- heading_anchor(gsub("&[#a-z0-9]+;", "", gsub("<[^>]+>", "", text)))
  repeat_n <- stats::ave(seq_along(ids), ids, FUN = seq_along) - 1
  ids <- ifelse(repeat_n > 0, paste0(ids, "-", repeat_n), ids)
  regmatches(html, m) <- list(sprintf("<h%s id=\"%s\">%s</h%s>", level, ids, text, level))
  html
}

#' Give every figure its print size, from its width in pixels and the
#' resolution it was written at.
#'
#' @param root  the folder the image paths are relative to
size_figures <- function(html, root, dpi = FIGURE_DPI) {
  pattern <- "<img src=\"([^\"]+)\""
  m <- gregexpr(pattern, html, perl = TRUE)
  tags <- regmatches(html, m)[[1]]
  if (!length(tags)) {
    return(html)
  }
  paths <- file.path(root, utils::URLdecode(sub(pattern, "\\1", tags, perl = TRUE)))
  missing <- !file.exists(paths)
  if (any(missing)) warning("Figures not found: ", paste(paths[missing], collapse = ", "), call. = FALSE)
  width_cm <- vapply(paths, png_width_px, numeric(1)) / dpi * 2.54
  regmatches(html, m) <- list(ifelse(
    is.na(width_cm), tags, sprintf("%s style=\"width: %.2fcm\"", tags, width_cm)
  ))
  html
}

#' The first Chromium browser found.
find_browser <- function() {
  candidates <- c(
    Sys.getenv("CHROME_PATH"),
    "C:/Program Files/Google/Chrome/Application/chrome.exe",
    "C:/Program Files (x86)/Google/Chrome/Application/chrome.exe",
    "C:/Program Files/Microsoft/Edge/Application/msedge.exe",
    "C:/Program Files (x86)/Microsoft/Edge/Application/msedge.exe",
    "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome",
    unname(Sys.which(c("google-chrome", "chromium", "chromium-browser", "microsoft-edge")))
  )
  found <- candidates[nzchar(candidates) & file.exists(candidates)]
  if (!length(found)) {
    stop("No Chrome, Edge or Chromium found. Set CHROME_PATH to the browser.", call. = FALSE)
  }
  found[[1]]
}

# The style of the PDF: the type and colours of the figures
# (R/functions/plot_style.R), small tables that wrap long values, a page for
# every scope and step, and figures and tables kept with their captions.
REPORT_CSS <- sprintf(
  "
@page {
  size: A4;
  margin: 15mm 14mm 16mm 14mm;
  @bottom-center {
    content: counter(page) ' / ' counter(pages);
    font-family: 'Segoe UI', 'Helvetica Neue', Arial, sans-serif; font-size: 7.5pt; color: %s;
  }
}
body {
  font-family: 'Segoe UI', 'Helvetica Neue', Arial, sans-serif;
  font-size: 9.5pt; line-height: 1.4; color: %s;
}
h1, h2, h3, h4 { color: %s; break-after: avoid; line-height: 1.2; }
h1 { font-size: 17pt; break-before: page; }
body > h1:first-child, h1 + h2, h1 + p + h2, #contents { break-before: avoid; }
h2 { font-size: 13.5pt; break-before: page; border-bottom: 0.6pt solid %s; padding-bottom: 2pt; }
h3 { font-size: 11.5pt; margin-top: 14pt; }
h4 { font-size: 10pt; }
hr { display: none; }
a { color: %s; text-decoration: none; }
code { font-size: 85%%; }
blockquote { margin: 6pt 0; padding: 3pt 8pt; border-left: 2.5pt solid %s; background: #F6F6FA; }
table {
  border-collapse: collapse; font-size: 7pt; margin: 3pt 0 9pt; max-width: 100%%;
  font-variant-numeric: tabular-nums;
}
thead { display: table-header-group; }
th, td { border: 0.4pt solid #C9C9D6; padding: 1.2pt 3pt; vertical-align: top; overflow-wrap: break-word; }
th { background: #ECECF4; text-align: left; }
tr { break-inside: avoid; }
img { display: block; max-width: 100%%; height: auto; margin: 3pt auto 9pt; }
p:has(> img) { break-inside: avoid; }
p:has(+ table), p:has(+ p > img) { break-inside: avoid; break-after: avoid; }
li { margin: 1pt 0; }
",
  "#7F7F7F", phengaris_col("spot_black"), phengaris_col("deep_blue"), phengaris_col("teleius_blue"),
  phengaris_col("deep_blue"), phengaris_col("teleius_blue")
)

#--------------------------------------------------#
## Render -----
#--------------------------------------------------#

#' Render Outputs/REPORT.md as Outputs/REPORT.html and Outputs/REPORT.pdf.
#'
#' @param outputs  the folder holding REPORT.md and the figures it links to
#' @return the path of the PDF, invisibly
render_report_pdf <- function(outputs = CONFIG$paths$outputs, browser = find_browser()) {
  md_path <- file.path(outputs, "REPORT.md")
  if (!file.exists(md_path)) stop(md_path, " not found: run step 15 first.", call. = FALSE)
  md <- readLines(md_path, encoding = "UTF-8", warn = FALSE)
  title <- sub("^# ", "", md[grep("^# ", md)[1]])

  body <- commonmark::markdown_html(md, extensions = TRUE)
  body <- size_figures(add_heading_ids(body), root = outputs)
  # Column names wrap at their underscores rather than mid-word.
  header_cells <- gregexpr("<th[^>]*>[^<]*</th>", body, perl = TRUE)
  regmatches(body, header_cells) <- lapply(
    regmatches(body, header_cells), function(cell) gsub("_", "_<wbr>", cell, fixed = TRUE)
  )
  html <- c(
    "<!DOCTYPE html>",
    "<html lang=\"en\">",
    "<head>",
    "<meta charset=\"utf-8\">",
    sprintf("<title>%s</title>", title),
    "<style>", REPORT_CSS, "</style>",
    "</head>",
    "<body>", body, "</body>",
    "</html>"
  )
  html_path <- file.path(outputs, "REPORT.html")
  writeLines(enc2utf8(html), html_path, useBytes = TRUE)

  pdf_path <- normalizePath(file.path(outputs, "REPORT.pdf"), winslash = "/", mustWork = FALSE)
  if (file.exists(pdf_path)) file.remove(pdf_path)
  profile <- tempfile("report-browser-")
  on.exit(unlink(profile, recursive = TRUE), add = TRUE)
  # sRGB throughout, so that the figures' white stays white rather than
  # passing through the colour profile of the screen.
  args <- c(
    "--headless", "--disable-gpu", "--no-first-run", "--no-pdf-header-footer",
    "--force-color-profile=srgb", "--generate-pdf-document-outline", paste0("--user-data-dir=", profile),
    paste0("--print-to-pdf=", pdf_path),
    paste0("file:///", normalizePath(html_path, winslash = "/"))
  )
  status <- system2(browser, shQuote(args, type = if (.Platform$OS.type == "windows") "cmd" else "sh"),
    stdout = FALSE, stderr = FALSE
  )
  if (!file.exists(pdf_path)) {
    stop("The browser (", browser, ") wrote no PDF; exit status ", status, ".", call. = FALSE)
  }
  message(
    "  ", html_path, "\n  ", pdf_path, " (", round(file.size(pdf_path) / 2^20, 1), " MB)"
  )
  invisible(pdf_path)
}

if (sys.nframe() == 0L) render_report_pdf()

#----------------------------------------------------------#
# End script -----
#----------------------------------------------------------#
