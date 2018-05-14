name1<-c("WRKY18","WRKY40","WRKY18-40","WRKY18-18","WRKY40-40","WRKY60","WRKY60-60")

A<- matrix(c(57.5780,18.9147,27.0171,48.0456,45.3106,29.5791,
             54.4373,17.7821,19.6102,54.7149,51.7689,24.1696,
             56.6631,19.8641),nrow =2)

x<-barplot(A, beside=T, col=c("red","black"), 
                 names.arg=name1, ylim=c(0,80),main="Maximum Expected Utility Plot using Bayesian Parameters")

text(x, A, label = A, pos = 3, cex = 1, col = c("red","black"))

legend("topleft", c("Activate","Inhibit"), pch=15, 
       col=c("red","black"), 
       bty="n")


B<- matrix(c(59.1681,17.9555,26.9477,48.6773,45.8855,29.0446,54.6098,17.8202,
             19.9975,55.0195,52.3418,23.7498,56.7895,19.9375),nrow =2)
y<-barplot(B, beside=T, col=c("red","black"), 
        names.arg=name1, ylim=c(0,80),main = "Maximum Expected Utility plot using MLE Parameters ")

text(y, B, label = B, pos = 3, cex = 1, col = c("red","black"))
legend("topleft", c("Activate","Inhibit"), pch=15, 
       col=c("red","black"), 
       bty="n")
