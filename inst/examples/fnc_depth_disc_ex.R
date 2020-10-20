df.soil <- data.frame(
  "mat" = 1:4,
  "upper" = c(0,13,60,100),
  "lower" = c(13,60,100,130),
  "texture" = c("Ls2", "Ls3", "Ls3", "Lts")
)

df.soil
fnc_depth_disc(df = df.soil)

