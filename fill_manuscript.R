###############################################################################
# Insert the numbers produced by ANC4_decomposition_R6.R into the manuscript.
# Usage: Rscript fill_manuscript.R Manuscript_R6.tex outputs Manuscript_R6_final.tex
# Replaces the blocks between %%MACROS-BEGIN/END, %%TABLE1-BEGIN/END and
# %%TABLE2-BEGIN/END with outputs/macros.tex, table1_body.tex and table2_body.tex.
###############################################################################
args <- commandArgs(trailingOnly = TRUE)
tex <- readLines(args[1], warn = FALSE); outdir <- args[2]
swap <- function(lines, tag, file) {
  b <- grep(paste0("^%%", tag, "-BEGIN"), lines); e <- grep(paste0("^%%", tag, "-END"), lines)
  stopifnot(length(b) == 1, length(e) == 1, e > b)
  c(lines[1:b], readLines(file.path(outdir, file), warn = FALSE), lines[e:length(lines)])
}
tex <- swap(tex, "MACROS", "macros.tex")
tex <- swap(tex, "TABLE1", "table1_body.tex")
tex <- swap(tex, "TABLE2", "table2_body.tex")
writeLines(tex, args[3])
left <- grep("\\\\ph\\{", tex[-grep("newcommand\\{\\\\ph\\}", tex)], value = TRUE)
if (length(left)) { cat("Placeholders still present:\n"); print(left) } else cat("All placeholders filled.\n")
