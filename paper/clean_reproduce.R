###############################################################
# Clean manuscript reproduction outputs
###############################################################

for (path in c(
  "paper/figures/output",
  "paper/data/source/output"
)) {
  if (dir.exists(path)) {
    unlink(path, recursive = TRUE, force = TRUE)
    message("Removed: ", path)
  }
}

message("Reproduction outputs cleaned.")