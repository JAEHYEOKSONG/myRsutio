plot(NA, xlab = "동전을 던진 횟수", ylab = "앞면이 나오는 확률", 
     xlim = c(0,150), ylim = c(0,1),
     main = "동전 던지는 횟수에 따른 앞면이 나온 확률 변화")
abline(h=0.5, col="blue", lty=2)

count<-0

for (n in 1:1000) {
  coin <-sample(c("앞면", "뒷면"), 1)
  
  if(coin == "앞면") {
    count = count + 1
  }
  
  prob <- count / n 
  
  points(n, prob)
  
}
