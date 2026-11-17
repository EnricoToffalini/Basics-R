# R popularity according to the TIOBE index
# data from https://www.tiobe.com/tiobe-index/ and https://www.tiobe.com/tiobe-index/r/ (downloaded November 2026)

# current ranking (top 20)
rk = read.csv("../data/tiobe_ranking.csv")
rk = rk[order(rk$rank, decreasing=T),]
cols = ifelse(rk$language=="R", "#c0392b", "gray70")

png("tiobeRanking.png", width=2000, height=1100, res=220)
par(mar=c(4,11,1,3))
bp = barplot(rk$rating, names.arg=paste0("#", rk$rank, "  ", rk$language), horiz=T, las=1, col=cols, border=NA,
             xlab="TIOBE rating (%), November 2026", xlim=c(0, max(rk$rating)*1.1), cex.names=0.8)
text(rk$rating, bp, paste0(rk$rating, "%"), pos=4, cex=0.7, col=ifelse(rk$language=="R", "#c0392b", "gray30"))
dev.off()

# trend of R over time
df = read.csv("../data/tiobe_R.csv")
df$date = as.Date(df$date)

png("tiobeR.png", width=2000, height=1100, res=220)
par(mar=c(4,4.5,1,1))
plot(df$date, df$rating, type="l", lwd=2.5, col="#1f6fb2", ylim=c(0, max(df$rating)*1.1),
     xlab="Year", ylab="TIOBE rating of R (%)", las=1, bty="n", xaxt="n")
axis.Date(1, at=seq(as.Date("2008-01-01"), as.Date("2026-01-01"), by="2 years"), format="%Y")
grid(nx=NA, ny=NULL, col="gray85", lty=1)
last = df[nrow(df),]
points(last$date, last$rating, pch=19, col="#c0392b", cex=1.3)
text(last$date, last$rating, paste0("Nov 2026: #7 (", last$rating, "%)"), pos=2, col="#c0392b", font=2)
dev.off()
