r = 2
n = 6
alpha = 0.05

z_0 = c(1,1,1,1,1,1)
z1 = c(10,5,7,19,11,18)
z2 = c(2,3,3,6,7,9)
Z = cbind(z_0,z1,z2)

Y = c(15,9,3,25,7,13)

B_est = solve(t(Z)%*%Z)%*%t(Z)%*%Y

Y_est = Z%*%B_est

E_est = Y - Y_est

#### Ejercicio 3.b
#Intervalo de confianza para b1
S_square = (t(E_est) %*%(E_est))/(n-r-1)
S_square = S_square[1,1]

SZZ = S_square*solve(t(Z)%*%Z)
F_r_n = qf(1-alpha, df1 = r+1, df2=n-r-1)

lw_lm_b1 = B_est[2] - sqrt(SZZ[2,2]) * sqrt((r+1)*F_r_n)
upp_lm_b1 = B_est[2] + sqrt(SZZ[2,2]) * sqrt((r+1)*F_r_n)

#Intervalo de confianza para b2
S_square = (t(E_est) %*%(E_est))/(n-r-1)
S_square = S_square[1,1]

SZZ = S_square*solve(t(Z)%*%Z)
F_r_n = qf(1-alpha, df1 = r+1, df2=n-r-1)

lw_lm_b2 = B_est[3] - sqrt(SZZ[3,3]) * sqrt((r+1)*F_r_n)
upp_lm_b2 = B_est[3] + sqrt(SZZ[3,3]) * sqrt((r+1)*F_r_n)

#### Ejercicio 3.c
reg = lm(Y~z1+z2)
summary(reg)

########Ejercico 3.d
z0 = c(1, 6, 4)
EV_Y = t(z0)%*%B_est # Valor esperado

tstudent = qt((1-alpha/2),df = n-r-1 )
S_square_ph = (t(Y-Z%*%B_est)%*%(Y-Z%*%B_est))/(n-r-1)
S_square_ph = S_square_ph[1,1]

#Intervalo de confianza del valor esperado
EV_lw_lm = t(z0)%*%B_est - tstudent*sqrt((t(z0)%*%solve(t(Z)%*%Z)%*%z0)%*%S_square_ph)
EV_upp_lm = t(z0)%*%B_est + tstudent*sqrt((t(z0)%*%solve(t(Z)%*%Z)%*%z0)%*%S_square_ph)

#####Ejercicio 3.d
#Limites para Y
Y_lw_lm = t(z0)%*%B_est - tstudent*sqrt(S_square_ph%*%(1+t(z0)%*%solve(t(Z)%*%Z)%*%z0))
Y_upp_lm = t(z0)%*%B_est + tstudent*sqrt(S_square_ph%*%(1+t(z0)%*%solve(t(Z)%*%Z)%*%z0))
