#******************************************************************************
#   Run the full R analysis for Paper I (S0 -> S4).
#
#   Usage (from the repository root):
#       Rscript src/run_all.R
#
#   Requires output/matched_climate_data.csv (made by the Python steps, see
#   README). Intermediate tables go to output/analysis/, figures to
#   output/figures/.
#******************************************************************************

required <- c("data.table", "ggplot2", "dplyr", "tidyr", "ggpubr", "car", "corrplot", "ragg")
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) stop("Install missing packages first: ", paste(missing, collapse = ", "))

if (!file.exists("src/run_all.R")) stop("Run this script from the repository root.")
dir.create("output/analysis", showWarnings = FALSE, recursive = TRUE)
dir.create("output/figures", showWarnings = FALSE, recursive = TRUE)

scripts <- c("Paper I_S0-Assemble Data.R",
             "Paper I_S1-Data prep.R",
             "Paper I_S2-Correlation.R",
             "Paper I_S3-Modeling.R",
             "Paper I_S4-Dataframe plots.R",
             "Paper I_S4-Plots.R")

pdf(NULL)  # discard on-screen plots made along the way
for (s in scripts) {
  message("==== ", s)
  local(source(file.path("src", s), encoding = "UTF-8", local = TRUE))
}
invisible(dev.off())

writeLines(capture.output(sessionInfo()), "output/analysis/sessionInfo.txt")
message("Done. Results: output/analysis/Table2_kendall.csv, output/analysis/final_models.csv, output/figures/")
