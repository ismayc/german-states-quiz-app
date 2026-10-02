# Builds the browser icons and the link-preview image for the shinylive site
# from the app's own state shapes. The SVG files are the sources; the PNGs are
# rendered from them with magick.
#
# Re-render from the repo root with:
#   Rscript site/make_assets.R
# and commit the updated files in site/.

library(sf)

states <- readRDS("states2023.rds")
out <- "site"

# Germany's states, simplified, projected so the outline isn't stretched
shapes <- states |>
  st_transform(3035) |>
  st_simplify(dTolerance = 2500, preserveTopology = TRUE)
country <- shapes |> st_union() |> st_simplify(dTolerance = 4000)

# Turn sf geometry into an SVG path, fitted into a box at (x0, y0) of size w x h
to_path <- function(geom, bbox, x0, y0, w, h) {
  sx <- w / (bbox["xmax"] - bbox["xmin"])
  sy <- h / (bbox["ymax"] - bbox["ymin"])
  s <- min(sx, sy)
  dx <- x0 + (w - s * (bbox["xmax"] - bbox["xmin"])) / 2
  dy <- y0 + (h - s * (bbox["ymax"] - bbox["ymin"])) / 2
  rings <- st_coordinates(st_cast(st_geometry(geom), "MULTIPOLYGON"))
  key <- interaction(rings[, "L1"], rings[, "L2"], rings[, "L3"], drop = TRUE)
  parts <- lapply(split(as.data.frame(rings), key), function(r) {
    x <- dx + s * (r$X - bbox["xmin"])
    y <- dy + s * (bbox["ymax"] - r$Y)
    paste0("M", paste(sprintf("%.1f,%.1f", x, y), collapse = "L"), "Z")
  })
  paste(parts, collapse = "")
}

bbox <- st_bbox(country)

# Flag stripe colors
black <- "#000000"
red <- "#DD0000"
gold <- "#FFCE00"

# ---- favicon.svg (64 x 64) -------------------------------------------------
favicon <- sprintf(
  '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 64 64">
  <rect width="64" height="64" rx="12" fill="#ffffff"/>
  <path d="%s" fill="%s"/>
  <rect x="10" y="55" width="15" height="4" fill="%s"/>
  <rect x="25" y="55" width="14" height="4" fill="%s"/>
  <rect x="39" y="55" width="15" height="4" fill="%s"/>
</svg>
',
  to_path(country, bbox, 12, 4, 40, 48), black, black, red, gold
)
writeLines(favicon, file.path(out, "favicon.svg"))

# ---- og-image.svg (1200 x 630) ---------------------------------------------
state_paths <- vapply(
  seq_len(nrow(shapes)),
  function(i) to_path(shapes[i, ], bbox, 70, 45, 430, 540),
  character(1)
)
og <- sprintf(
  '<svg xmlns="http://www.w3.org/2000/svg" width="1200" height="630" viewBox="0 0 1200 630">
  <rect width="1200" height="630" fill="#f4f4f2"/>
  %s
  <g font-family="Helvetica, Arial, sans-serif">
    <text x="560" y="230" font-size="64" font-weight="700" fill="#111111">German Federal</text>
    <text x="560" y="305" font-size="64" font-weight="700" fill="#111111">State Shape Quiz</text>
    <text x="560" y="375" font-size="32" fill="#333333">Name all 16 states from their outlines,</text>
    <text x="560" y="420" font-size="32" fill="#333333">with optional capitals and largest cities.</text>
    <text x="560" y="500" font-size="28" fill="#555555">Created by Chester Ismay</text>
  </g>
  <rect x="560" y="530" width="180" height="14" fill="%s"/>
  <rect x="740" y="530" width="180" height="14" fill="%s"/>
  <rect x="920" y="530" width="180" height="14" fill="%s"/>
</svg>
',
  paste(sprintf('<path d="%s" fill="#111111" stroke="#ffffff" stroke-width="2.5"/>',
                state_paths), collapse = "\n  "),
  black, red, gold
)
writeLines(og, file.path(out, "og-image.svg"))

# ---- PNG renders ------------------------------------------------------------
# magick's built-in SVG reader sizes by density (96 dpi = 1 SVG unit per pixel),
# so the density is scaled to hit the target width exactly
render <- function(svg, png, width, svg_width, flatten = FALSE) {
  img <- magick::image_read(file.path(out, svg), density = 96 * width / svg_width)
  if (flatten) img <- magick::image_background(img, "#ffffff", flatten = TRUE)
  magick::image_write(img, file.path(out, png), format = "png")
}
render("favicon.svg", "favicon-32.png", 32, 64)
# iOS fills transparent corners black, so the touch icon is flattened onto white
render("favicon.svg", "apple-touch-icon.png", 180, 64, flatten = TRUE)
render("og-image.svg", "og-image.png", 1200, 1200)

message("Wrote ", paste(list.files(out, "\\.(svg|png)$"), collapse = ", "))
