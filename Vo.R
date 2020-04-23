A= 0.00022
B=0.0000027
c=1.124
i=0.05
v=1/(1+i)
x = 50
n = 15
m = 4



FOM = function(x) {
  A + Bc^x
}

tPx = function(x, t) {
  exp(-A)^t * exp(-B/log(c))^(c^(x) * (c^(t)-1))
}

term = 0
for (j in 0:(n*m-1)) {
  term = term + v^((j+1)/m)*(tPx(x,j/m)*(1-tPx(x+j/m,1/m)))
}

whole_life = 0
for (j in 0:100*m) {
  whole_life = whole_life + v^((j+1)/m)*(tPx(x+n,j/m)*(1-tPx(x+n+j/m,1/m)))
}
def_whole_life = v^(n)*tPx(x,n)*whole_life



EZ = 2000*term + 1000*def_whole_life
EZ
#b

term_2 = 0
for (j in 0:(n*m-1)) {
  term_2 = term_2 + v^(2*(j+1)/m)*(tPx(x,j/m)*(1-tPx(x+j/m,1/m)))
}

whole_life_2 = 0
for (j in 0:100*m) {
  whole_life_2 = whole_life_2 + v^(2*(j+1)/m)*(tPx(x+n,j/m)*(1-tPx(x+n+j/m,1/m)))
}
def_whole_life_2 = v^(2*n)*tPx(x,n)*whole_life



EZ_2 = 2000^2*term_2 + 1000^2*def_whole_life_2
sd = sqrt(EZ_2 -EZ^2)
sd

#c



#22
A=3.5*10^-4
B=5.5*10^-4
C=1.00085
D=1.0005
x=60
v = 1/1.05
mu_xs = function(s) {
  A + B*C^(x+s)*D^((x+s)^2)
}
tPx = c()
for (i in 1:80) {
  tPx[i] = exp(-integrate(mu_xs, 0, i/40)[[1]])
}
tPx
#b
integratable = function(t){
  v^(t)*exp(-integrate(mu_xs, 0, t)[[1]])*mu_xs(t)
}
integrate(integratable,0,2)






