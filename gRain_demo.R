
require(gRain)
require(gRbase)

## Asia (chest clinic) example:
## Version 1) Specify conditional probability tables.
yn <- c("yes","no")
a <- cptable(~asia, values=c(1,99),levels=yn)

t.a <- cptable(~tub+asia, values=c(5,95,1,99),levels=yn)
s <- cptable(~smoke, values=c(5,5), levels=yn)
l.s <- cptable(~lung+smoke, values=c(1,9,1,99), levels=yn)
b.s <- cptable(~bronc+smoke, values=c(6,4,3,7), levels=yn)
e.lt <- cptable(~either+lung+tub,values=c(1,0,1,0,1,0,0,1),levels=yn)
x.e <- cptable(~xray+either, values=c(98,2,5,95), levels=yn)
d.be <- cptable(~dysp+bronc+either, values=c(9,1,7,3,8,2,1,9), levels=yn)
plist <- compileCPT(list(a, t.a, s, l.s, b.s, e.lt, x.e, d.be))
pn1 <- grain(plist)
q1 <- querygrain(pn1)

## Version 2) Specify DAG and data
data(chestSim100000, package="gRbase")
dgf <- ~asia + tub * asia + smoke + lung * smoke +
    bronc * smoke + either * tub * lung +
    xray * either + dysp * bronc * either
dg <- dag(dgf)
pp <- extractCPT(chestSim100000, dg)
cpp2 <- compileCPT(pp)
pn2 <- grain(cpp2)
q2 <- querygrain(pn2)



## Version 2) Specify triangulated undirected graph and data
ugf <- list(c("either", "lung", "tub"), c("either", "lung", "bronc"),
            c("either", "xray"), c("either", "dysp", "bronc"), c("smoke",
                                                                 "lung", "bronc"), c("asia", "tub"))
gg <- ugList(ugf)
pp <- extractPOT(chestSim100000, gg)
cpp3 <- compilePOT(pp)
pn3 <- grain(cpp3)
q3 <- querygrain(pn3)
## Compare results:
str(q1)
str(q2[names(q1)])
str(q3[names(q1)])

