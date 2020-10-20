psi_vals <- 10^(seq(log10(1), log10(100000), length.out = 500))

# Ls3
thetas <- fnc_MvG.swc(psi = psi_vals,
                      alpha = 0.06835,
                      n = 1.20501,
                      ThS = 0.4091,
                      ThR = 0.07284)
#Ts3
thetas2 <- fnc_MvG.swc(psi = psi_vals,
                      alpha = 0.06194,
                      n = 1.14565,
                      ThS = 0.4374,
                      ThR = 0.07841)
# Ss
thetas3 <- fnc_MvG.swc(psi = psi_vals,
                       alpha = 0.26437,
                       n = 1.35154,
                       ThS = 0.3879,
                       ThR = 0)

plot(log10(psi_vals), thetas,  ylim =c(0,0.5),
     xlab = "pressure head (pF)", ylab = "soil water content")
points(log10(psi_vals), thetas2, col = "red")
points(log10(psi_vals), thetas3, col = "blue")
legend("topright", inset = 0.02, title = "soil texture", c("Ls3", "Ts3", "Ss"),
       col=c("black", "red", "blue"), pch = 19, cex=0.8)
