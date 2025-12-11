library(ggplot2)
library(hexSticker)
library(dplyr)
library(tsbox)
library(seasonal)

# library(seasight)

# file_name <- "logo.png"
# file_path <- "man/figures/"
# file_path <- paste0(file_path, file_name)

main_series <- AirPassengers %>% ts_tbl() %>% ts_span(start = as.Date("1955-01-01"), end = as.Date("1960-01-01"))
main_series_sa <- predict(seas(AirPassengers))  %>% ts_tbl() %>% ts_span(start = as.Date("1955-01-01"), end = as.Date("1960-01-01"))

# lens (ellipse) in date/value coordinates – pick a spot you like
lens_df_time <- function(x0, y0, rx_days = 180, ry = 40, n = 360) {
  t <- seq(0, 2*pi, length.out = n)
  data.frame(
    x = as.Date(x0) + rx_days * cos(t),
    y = y0 + ry * sin(t)
  )
}
x0 <- as.Date("1957-07-01"); y0 <- 350
lens <- lens_df_time(x0, y0, rx_days = 480, ry = 360)  # slightly smaller lens

# handle: start at a point on the lens rim (angle theta), extend outward
theta <- -pi/4
x_start <- as.Date(x0) + 480 * cos(theta)
y_start <- y0 + 360 * sin(theta)
x_end   <- x_start + 160  # extend ~120 days to the right
y_end   <- y_start - 30   # and a bit down


pt_sa <- ggplot() +
  geom_line(data = main_series,
            aes(x = time, y = value),
            color = "#9B2226", linewidth = 1.1) +        # coral
  geom_line(data = main_series_sa,
            aes(x = time, y = value),
            color = "#778DA9", linewidth = 1.1) +       # sea blue
  geom_path(data = lens, aes(x, y),
            color = "#DDA15E", linewidth = 1.3, lineend = "round") +  # rim
  geom_polygon(data = lens, aes(x, y),
               fill = alpha("#DDA15E", 0.08), color = NA) +           # glass fill
  geom_segment(aes(x = x_start, y = y_start, xend = x_end, yend = y_end),
               color = "#DDA15E", linewidth = 1.3, lineend = "round") +  
  # inner padding so lines don’t touch edges
  scale_x_date(expand = expansion(mult = 0.04)) +
  scale_y_continuous(expand = expansion(mult = 0.10)) +
  # (optional) keep the focus window but with padding:
  coord_cartesian(xlim = c(as.Date("1955-01-01"), as.Date("1960-01-01"))) +
  theme_void() +
  theme(legend.position = "none",
        plot.margin = margin(14, 16, 20, 16))
pt_sa

# build sticker but save only to a temp file for now
s <- hexSticker::sticker(
  pt_sa,
  package  = "seasight",
  p_size   = 36,
  p_family = "Roboto Condensed",
  p_fontface = "bold",
  p_color = "#415A77",
  p_y      = 1.40,
  s_x      = 1.00,
  s_y      = 0.82,
  s_width  = 2.00,
  s_height = 1.40,
  h_fill   = "#D2CBE5",
  h_color  = "#2B2D42",
  url      = "github.com/p-wegmueller/seasight",
  u_size   = 3,
  u_color  = "#F2E5D5",
  white_around_sticker = TRUE,         # keep if you like the white corners
  filename = tempfile(fileext = ".png") # throw-away file
)

# fix: add a bit of *positive* bottom and left margin
s <- s + theme(
  plot.margin = margin(t = 0, r = 0, b = 0.2, l = 0.2, unit = "lines")
)

# now save the final sticker
hexSticker::save_sticker(
  filename = "seasight_hex.png",
  sticker  = s,
  dpi      = 600
)
