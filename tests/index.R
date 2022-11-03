library(dclone)

x <- 1
x1 <- dclone(x, 1)
x2 <- dclone(x, 2)
stopifnot(length(x1) == length(x))
stopifnot(length(x2) == length(x)*2)

x <- 1:2
x1 <- dclone(x, 1)
x2 <- dclone(x, 2)
stopifnot(length(x1) == length(x))
stopifnot(length(x2) == length(x)*2)

x <- matrix(1:6, 2, 3)
x1 <- dclone(x, 1)
x2 <- dclone(x, 2)
stopifnot(all(dim(x1) == dim(x)))
stopifnot(all(dim(x2) == dim(x)*c(2,1)))

x <- dctr(matrix(1:6, 2, 3))
x1 <- dclone(x, 1)
x2 <- dclone(x, 2)
stopifnot(all(dim(x1) == dim(x)))
stopifnot(all(dim(x2) == dim(x)*c(1,2)))

x <- dciid(data.frame(a=1:3, b=4:6))
x1 <- dclone(x, 1)
x2 <- dclone(x, 2)
stopifnot(all(dim(x1) == dim(x)))
stopifnot(all(dim(x2) == dim(x)*c(2,1)))

## array
x <- dcdim(array(1:6, c(2, 3, 1)))
x1 <- dclone(x, 1)
x2 <- dclone(x, 2)
x
x1
x2
stopifnot(all(dim(x1) == dim(x)))
stopifnot(all(dim(x2) == dim(x)*c(1,1,2)))

x <- dcdim(array(1:6, c(1, 2, 3)), perm = 1)
x1 <- dclone(x, 1)
x2 <- dclone(x, 2)
x
x1
x2
stopifnot(all(dim(x1) == dim(x)))
stopifnot(all(dim(x2) == dim(x)*c(2,1,1)))


x <- dcdim(array(1:6, c(2, 1, 3)), perm = 2)
x1 <- dclone(x, 1)
x2 <- dclone(x, 2)
x
x1
x2
stopifnot(all(dim(x1) == dim(x)))
stopifnot(all(dim(x2) == dim(x)*c(1,2,1)))

## matrix
x <- dcdim(matrix(1:6, 2, 3))
x1 <- dclone(x, 1)
x2 <- dclone(x, 2)
x
x1
x2
# stopifnot(all(dim(x1) == dim(x)))
# stopifnot(all(dim(x2) == dim(x)*c(1,2)))

