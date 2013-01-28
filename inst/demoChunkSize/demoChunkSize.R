

library(rhdf5)

Dim = 10000000
Data = 1:10000000L
Data2 = 1:10000000L

chunksize = 10L^(0:6)
level = c(0,3,6,9)

nchunks = length(chunksize)
WT = matrix(-1,nr=nchunks,nc=4)
RT = matrix(-1,nr=nchunks,nc=4)
S = matrix(-1,nr=nchunks,nc=4)

for (i in 1:nchunks) {
  for (j in 1:4) {
    h5createFile("test.h5")
    h5createDataset("test.h5","A",dims=Dim, storage.mode = "integer", chunk = chunksize[i], level = level[j])

    t1 = Sys.time()
    for (k in 1:100) {
      h5write(Data[1:10000+(k-1)*10000], "test.h5","A",index=list(1:10000+(k-1)*10000))
    }
    t2 = Sys.time()
    dt1 = as.double(t2-t1)
    WT[i,j] = dt1
    
    t1 = Sys.time()
    for (k in 1:100) {
      Data2[1:10000+(k-1)*10000] = h5read("test.h5","A",index=list(1:10000+(k-1)*10000))
    }
    t2 = Sys.time()
    dt2 = as.double(t2-t1)
    RT[i,j] = dt2

    s = file.info("test.h5")$size
    S[i,j] = s
    file.remove("test.h5")
    
    cat("i = ",i,"; j = ",j,"; t1 = ",dt1,"; t1 = ",dt2, " size=",s,"\n")
  }
}

pdf(file="chunksize.pdf")
par(mfrow=c(3,1))
ylim = range(c(WT))
col = rainbow(4)
plot(NA,xlim=c(1,nchunks),ylim=ylim,xlab="chunksize",ylab="time (sec)",main="writing time",xaxt="n")
axis(1,1:nchunks,chunksize)
chunk.x <- t(replicate(4, jitter(1:nchunks, factor=0.5)))
for (i in 1:4) {
  lines(chunk.x[i,],WT[,i],col=col[i], pch=20,lwd=3,type="b")
}
legend("topright",sprintf("level = %d",c(0,3,6,9)),fill=col,inset=0.01)

ylim = range(c(RT))
plot(NA,xlim=c(1,nchunks),ylim=ylim,xlab="chunksize",ylab="time (sec)",main="reading time",xaxt="n")
axis(1,1:nchunks,chunksize)
for (i in 1:4) {
  lines(chunk.x[i,],RT[,i],col=col[i], pch=20,lwd=3,type="b")
}
legend("topright",sprintf("level = %d",c(0,3,6,9)),fill=col,inset=0.01)

ylim = range(c(log10(S)))
plot(NA,xlim=c(1,nchunks),ylim=ylim,xlab="chunksize",ylab="byte [log10]",main="file size",xaxt="n")
axis(1,1:nchunks,chunksize)
for (i in 1:4) {
  lines(chunk.x[i,], log10(S[,i]),col=col[i], pch=20,lwd=3,type="b")
}
legend("topright",sprintf("level = %d",c(0,3,6,9)),fill=col,inset=0.01)
dev.off()
