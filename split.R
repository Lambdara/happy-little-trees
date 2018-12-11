# This is code to determine optimal splits

credit.dat <- read.csv("data/credit.csv")

# Gini index
impurity <- function (x) {
    zeroes <- length(x[x==0])
    ones <- length(x[x==1])
    total <- ones + zeroes

    (ones/total)*(zeroes/total)
}

impurity_reduction <- function (data,class,split) {
    left <- class[data <= split]
    right <- class[data > split]
    leftsize <- length(left)
    rightsize <- length(right)
    totalsize <- leftsize + rightsize
    reduction <- impurity(class) - (leftsize * impurity(left) + rightsize * impurity(right)) / totalsize
}

# Best split
bestsplit <- function (data,class,minleaf) {
    sorted <- sort(unique(data))
    l <- length(sorted)
    splits <- (sorted[1:l-1] + sorted[2:l])/2
    best.reduction <- -1
    best.split <- "none"
    for (split in splits) {
        left <- class[data <= split]
        right <- class[data > split]
        leftsize <- length(left)
        rightsize <- length(right)
        totalsize <- leftsize + rightsize
        reduction <- impurity(class) - (leftsize * impurity(left) + rightsize * impurity(right)) / totalsize
        if (reduction > best.reduction && leftsize >= minleaf && rightsize >= minleaf) {
            best.reduction <- reduction
            best.split <- split
        }
    }
    best.split
}
