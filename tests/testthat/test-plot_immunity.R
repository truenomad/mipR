# plots and maps ---------------------------------------------------------------

plot_fixture <- function() {
  pop <- data.frame(
    adm1 = "Kano", year = c(2024, 2024, 2025, 2025), age = c(0, 1, 0, 1),
    pop = c(1200, 1150, 1240, 1180)
  )
  cohorts <- reconstruct_birth_cohorts(
    pop, geography = "adm1", method = "uniform",
    start = "2023-01", end = "2025-06"
  )
  coverage <- data.frame(
    adm1 = "Kano", year = 2023:2025, dose = "mcv1",
    coverage = c(0.6, 0.65, 0.7)
  )
  campaigns <- data.frame(
    adm1 = "Kano", campaign_id = "NGA-MSL-2024",
    start_date = as.Date("2024-03-01"), end_date = as.Date("2024-03-10"),
    age_min_months = 9, age_max_months = 60, coverage = 0.8
  )
  history <- build_vaccination_history(
    cohorts, coverage, campaigns,
    schedule = data.frame(dose = "mcv1", age_months = 9),
    geography = "adm1"
  )
  estimate_immunity(history, cohorts)
}

testthat::test_that("plot_cohort_protection orders rows and marks targets", {
  testthat::skip_if_not_installed("ggplot2")
  immunity <- plot_fixture()
  p <- plot_cohort_protection(immunity, at = "2025-06")

  testthat::expect_s3_class(p, "ggplot")
  built <- ggplot2::ggplot_build(p)
  rows <- built$layout$panel_params[[1]]$y$get_labels()
  # rows run from the youngest cohort at the bottom to the oldest at the top
  testthat::expect_identical(rows, sort(rows, decreasing = TRUE))
  testthat::expect_true(all(grepl("^Born 20", rows)))
  labels <- p$layers[[4]]$data$label
  testthat::expect_true(any(labels != "Not targeted"))

  path <- tempfile(fileext = ".png")
  on.exit(unlink(path), add = TRUE)
  plot_cohort_protection(
    immunity, at = "2025-06", output_file = path, width = 6, height = 4,
    dpi = 72, compress_image = FALSE
  )
  testthat::expect_true(file.exists(path))
})

testthat::test_that(".percent_bands names colours by bin", {
  colours <- get_palette("susceptibility")
  bands <- .percent_bands(c(0, 20, 40), colours)

  testthat::expect_identical(
    bands$label, c("0-20%", "20-40%", "40% or more")
  )
  testthat::expect_identical(bands$colour[1], colours[1])
  testthat::expect_identical(bands$colour[3], colours[length(colours)])
})

testthat::test_that("map_immunity draws with data-driven scales", {
  testthat::skip_if_not_installed("ggplot2")
  testthat::skip_if_not_installed("sntutils")
  immunity <- plot_fixture()
  summary <- summarise_immunity(immunity, at = "2025-06")
  boundaries <- sf::st_sf(
    adm1 = "Kano",
    geometry = sf::st_sfc(
      sf::st_polygon(list(rbind(c(8, 11), c(9, 11), c(9, 12), c(8, 12),
                                c(8, 11)))),
      crs = 4326
    )
  )

  bins <- map_immunity(summary, boundaries, type = "bins")
  gradient <- map_immunity(summary, boundaries, type = "gradient")
  testthat::expect_s3_class(bins, "ggplot")
  testthat::expect_s3_class(gradient, "ggplot")

  path <- tempfile(fileext = ".png")
  on.exit(unlink(path), add = TRUE)
  map_immunity(
    summary, boundaries, output_file = path, width = 4, height = 4,
    dpi = 72, compress_image = FALSE
  )
  testthat::expect_true(file.exists(path))
  testthat::expect_error(
    map_immunity(summary, sf::st_drop_geometry(boundaries)),
    "sf"
  )
})

testthat::test_that("get_palette returns presets and interpolates", {
  testthat::expect_true("susceptibility" %in% list_palettes())
  testthat::expect_length(get_palette("ylord", n = 5), 5)
  testthat::expect_identical(
    get_palette(c("#000000", "#ffffff")), c("#000000", "#ffffff")
  )
  testthat::expect_error(get_palette("rainbowish"), "Unknown palette")
})

testthat::test_that("save_plot writes and compresses a PNG", {
  testthat::skip_if_not_installed("ggplot2")
  testthat::skip_if(is.null(.find_pngquant()), "pngquant not installed")
  path <- tempfile(fileext = ".png")
  on.exit(unlink(path), add = TRUE)
  p <- ggplot2::ggplot(data.frame(x = 1:10, y = 1:10)) +
    ggplot2::geom_point(ggplot2::aes(.data$x, .data$y))

  save_plot(p, path, width = 4, height = 3, dpi = 100, compress = FALSE)
  uncompressed <- file.size(path)
  save_plot(p, path, width = 4, height = 3, dpi = 100)
  testthat::expect_lt(file.size(path), uncompressed)
})
