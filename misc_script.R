## Script for storing random peices of code

# creates a vector of MN House seat names (01A, 01B, etc)
paste(
    # creates vector of house seat numbers (01, 01, 02, 02, etc)
    sprintf("%02d",rep(1:67,each=2)), 
    # creates vector of house seat letters (A, B, A, B)
    rep(c("A","B"),length=67), sep=""
)