library(plot3D)
library(viridis)  # For the colormap

# Generate grid data
x <- seq(0, 10, length.out = 30)
y <- seq(0, 10, length.out = 30)
z <- outer(x, y, function(x, y) sin(x) * cos(y))

# Matplotlib-style 3D surface plot
persp3D(x, y, z, colvar = z, col = viridis(50), 
        theta = 135, phi = 25, bty = "g",  
        # shade = 0.1,
        xlab = "Temperature", ylab = "Rainfall", zlab = "Response",
        ticktype = "detailed", border = "black", lwd = 0.5)
