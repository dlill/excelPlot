# -------------------------------------------------------------------------#
# 0 Header ----
# -------------------------------------------------------------------------#
#
# generateExamplePlots.R
#
# [PURPOSE]
#
#
#
#
# [AUTHOR]
# Daniel Lill
#
# [Date]
# Sun Jul 13 17:06:22 2025
#
# [Git-hash]
# 8474e36e699386f2d6fed711d6a552fc2a16ccc5
#
rm(list = ls(all.names = TRUE))
try(setwd(dirname(rstudioapi::getSourceEditorContext()$path)))

library(data.table)
library(ggplot2)

# -------------------------------------------------------------------------#
# 1 Iris ----
# -------------------------------------------------------------------------#
d <- iris

pl <- ggplot(iris, aes(Sepal.Length, Sepal.Width, color = Species)) +
  geom_point() +
  theme_bw()

ggsave(plot = pl, filename = file.path("01-Iris.pdf"), width = 15.5, height = 10, scale = 1, units = "cm")
ggsave(plot = pl, filename = file.path("01-Iris.png"), width = 15.5, height = 10, scale = 1, units = "cm")

ggsave(plot = pl + scale_color_brewer(), filename = file.path("02-Iris-Brewer.pdf"), width = 15.5, height = 10, scale = 1, units = "cm")
ggsave(plot = pl + scale_color_brewer(), filename = file.path("02-Iris-Brewer.png"), width = 15.5, height = 10, scale = 1, units = "cm")

ggsave(plot = pl + scale_color_brewer(), filename = file.path("03-Iris-scale2.pdf"), width = 15.5, height = 10, scale = 2, units = "cm")
ggsave(plot = pl + scale_color_brewer(), filename = file.path("03-Iris-scale2.png"), width = 15.5, height = 10, scale = 2, units = "cm")


# -------------------------------------------------------------------------#
# Multipage ----
# -------------------------------------------------------------------------#

pdf("04-IrisMulti.pdf", width = 15.5 / 2.54, height = 15.5 / 2.54)
pl
pl + scale_color_brewer()
pl + scale_color_brewer() + theme(text = element_text(size = 7))
dev.off()




# Exit ----
