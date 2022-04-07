#####################################################
#Ejercicio 1

x1 <- c(9, 2, 6, 5, 8)
x2 <- c(12, 8, 6, 4, 10)
x3 <- c(3, 4, 0, 2, 1)

x_barra = c(mean(x1), mean(x2), mean(x3))

X = cbind(x1, x2, x3)

Sn = cov(X)

R = cor(X)


#####################################################
#Ejercicio 2

install.packages("REdaS")
install.packages("plot3D")
install.packages("plot3D.package")

x <- c(5, 1, 3)
y <- c(-1, 3, 1)
X = cbind(x,y)

#2.a
x0 <- c(0, 0)
y0 <- c(0, 0)
z0 <- c(0, 0)
x1 <- X[1,]
y1 <- X[2,]
z1 <- X[3,]
cols <- c("#1B9E77", "#D95F02")

arrows3D(x0, y0, z0, x1, y1, z1, col = cols,
         lwd = 2, d = 3,
         main = "Grafico Vectores", bty ="g", ticktype = "detailed")
# Add starting point of arrow
points3D(x0, y0, z0, add = TRUE, col="darkred",
         colkey = FALSE, pch = 19, cex = 1)
# Add labels to the arrows
text3D(x1, y1, z1, c("Obs 1", "Obs 2"),
      col = cols, add=TRUE, colkey = FALSE)

#2.b
Lx = sqrt(sum(x^2))
Ly = sqrt(sum(y^2))

cos_tetha = (x%*%y)/(Lx*Ly)

tetha = rad2deg(acos(cos_tetha))

Px_y = ((y%*%x)/(x%*%x))*x

#2.c
x_d = c(2, -2, 0)
y_d = c(-2, 2, 0)

X_d = cbind(x_d, y_d)

x0 <- c(0, 0)
y0 <- c(0, 0)
z0 <- c(0, 0)
x1 <- X_d[1,]
y1 <- X_d[2,]
z1 <- X_d[3,]
cols <- c("#1B9E77", "#D95F02")

arrows3D(x0, y0, z0, x1, y1, z1, col = cols,
         lwd = 2, d = 3,
         main = "Grafico Vectores de desviación", bty ="g", ticktype = "detailed")
# Add starting point of arrow
points3D(x0, y0, z0, add = TRUE, col="darkred",
         colkey = FALSE, pch = 19, cex = 1)
# Add labels to the arrows
text3D(x1, y1, z1, c("Obs 1", "Obs 2"),
       col = cols, add=TRUE, colkey = FALSE)

#####################################################
#Ejercicio 3

#3.a
A = matrix(data = c(9, -2, -2, 6), nrow = 2, ncol = 2)
A_t = t(A)

#3.b y 3.c

#Valores propios
vals_A = eigen(A)$values
vals_A

#Vectores propios
vecs_A = eigen(A)$vectors
#Las columnas de la matriz son los vectores propios
vecs_A

#3.d
desc_esp = (vals_A[1]*(vecs_A[,1]%*%vecs_A[,1])) + (vals_A[2]*(vecs_A[,2]%*%vecs_A[,2]))
desc_esp

#3.e
inv_A = solve(A)
inv_A

#3.f
#Valores propios de la inversa
vals_inv_A = eigen(inv_A)$values
vals_inv_A

#Vectores propios
vecs_inv_A = eigen(inv_A)$vectors
#Las columnas de la matriz son los vectores propios
vecs_inv_A


#####################################################
#Ejercicio 6

#6.a
x1 = c(9,5,1)
x2 = c(1,3,2)
X = cbind(x1,x2)
U = c(mean(x1), mean(x2))

plot(x1,x2)
points(U[1],U[2])
text(U[1],U[2], labels = "media", pos = 1)

