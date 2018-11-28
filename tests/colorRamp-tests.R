## taken from example(colorRamp) to ensure we do not introduce
## regressions

colorRamp(c("red", "green"))( (0:4)/4 ) ## (x) , x in [0,1]
colorRampPalette(c("blue", "red"))( 4 ) ## (n)
colorRampPalette(c(rgb(0,0,1,1), rgb(0,0,1,0)), alpha = TRUE)(8)

colorRampPalette(c("red", "white", "blue"))(6)
colorRampPalette(c("red", "white", "blue"), space = "Lab")(6)

## Interpolating a 'sequential' ColorBrewer palette

YlOrBr <- c("#FFFFD4", "#FED98E", "#FE9929", "#D95F0E", "#993404")
colorRampPalette(YlOrBr, space = "Lab")(6)
colorRampPalette(YlOrBr, space = "Lab", bias = 0.5)(6)

jet.colors <-c("#00007F", "blue", "#007FFF", "cyan",
                     "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000")
colorRampPalette(jet.colors)(6)

colorRampPalette(c("red", "orange", "blue"), space = "rgb")(6)
colorRampPalette(c("red", "orange", "blue"), space = "Lab")(6)
