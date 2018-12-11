source("split.R")

tree.grow <- function(x,y,nmin,minleaf,nfeat) {
    ## Start with a tree with just a root node and add it to the list the
    ## unexplored nodes
    tree <- data.frame("root" = c(TRUE),
                       "leaf" = c(FALSE),
                       "split_att" = c(-1),
                       "split_val" = c(-1),
                       "parent" = c(-1),
                       "left" = c(-1),
                       "right" = c(-1),
                       "class" = c(-1)
                       )
    unexplored = list(1)

    ## Build the tree by expanding unexplored nodes
    while(length(unexplored) > 0) {
        ## Get the first unexplored node from the list
        current.index <- unexplored[[1]]
        current.node <- tree[current.index,]
        unexplored <- unexplored[-1]

        ## Get only the data classified to the current node
        current.x <- x
        current.y <- y
        current.filter <- current.node
        current.filter.index <- current.index

        ## Filter data
        while(current.filter[,"root"] == FALSE) {
            next.filter.index <-current.filter[,"parent"]
            if(tree[next.filter.index,"left"] == current.filter.index) {
                direction <- "left"
            } else {
                direction <- "right"
            }

            current.filter.index <- next.filter.index
            current.filter <- tree[current.filter.index,]

            filter.att <- current.filter[,"split_att"]
            filter.val <- current.filter[,"split_val"]

            indices <- current.x[,filter.att] <= filter.val

            if(direction == "right") {
                indices <- !indices
            }

            current.x <- current.x[indices,]
            current.y <- current.y[indices]
        }

        ## Determine class
        if (length(current.y[current.y == 0]) < length(current.y[current.y == 1])) {
            tree[current.index, "class"] <- 1
        } else {
            tree[current.index, "class"] <- 0
        }

        ## print(cbind(current.x,current.y))

        ## Nodes with less than nmin values are leaves
        if (nrow(current.x) < nmin){
            tree[current.index,"leaf"] <- TRUE
            ## cat("Stopped ", current.index, " because nmin\n")
            next
        }

        ## Pure nodes are leaves
        if (length(unique(current.y)) <= 1) {
            tree[current.index,"leaf"] <- TRUE
            ## cat("Stopped ", current.index, " because pure\n")
            next
        }

        ## Select features
        features <- sample(colnames(x),nfeat)

        ## Compute best split
        features.best <- features[0]
        features.best_reduction <- -1
        features.best_at <- -1
        for(feature in features) {
            feature.current <- current.x[,feature]
            if (length(unique(feature.current)) <= 1) {
                next
            }
            at <- bestsplit(feature.current,current.y,minleaf)
            if (at != "none") {
                reduction <- impurity_reduction(feature.current,current.y,at)
                if (reduction > features.best_reduction) {
                    features.best_reduction <- reduction
                    features.best_at <- at
                    features.best <- feature
                }
            }
        }

        ## Nodes with no splits are leaves
        if (features.best_reduction == -1) {
            tree[current.index,"leaf"] <- TRUE
            ## cat("Stopped ", current.index, " because no split (possibly minleaf) \n")
            next
        }

        ## Nodes with no reduction are leaves
        if (features.best_reduction == 0) {
            tree[current.index,"leaf"] <- TRUE
            ## cat("Stopped ", current.index, " because no reduction \n")
            next
        }

        ## Perform split
        tree[current.index,"split_att"] <- features.best
        tree[current.index,"split_val"] <- features.best_at

        ## Add left and right to unexplored
        new.left = c("root"=FALSE,
                     "leaf"=FALSE,
                     "split_att"=-1,
                     "split_val"=-1,
                     "parent"=current.index,
                     "left"=-1,
                     "right"=-1)

        new.right = c("root"=FALSE,
                      "leaf"=FALSE,
                      "split_att"=-1,
                      "split_val"=-1,
                      "parent"=current.index,
                      "left"=-1,
                      "right"=-1)
        tree <- rbind(tree,new.left,new.right)
        tree[current.index, "left"] <- nrow(tree) - 1
        tree[current.index, "right"] <- nrow(tree)
        unexplored[[length(unexplored)+1]] <- nrow(tree) - 1
        unexplored[[length(unexplored)+1]] <- nrow(tree)
    }
    tree
}

tree.classify <- function(x,tr) {
    tree.classify.item <- function(item) {
        ## Assumes that the first node is root
        current.index <- 1

        ## Walk through the tr
        while(tr[current.index,"leaf"] == FALSE) {
            if(item[tr[current.index,"split_att"]] <= tr[current.index,"split_val"]) {
                current.index <- tr[current.index,"left"]
            } else {
                current.index <- tr[current.index,"right"]
            }
        }

        tr[current.index,"class"]
    }
    apply(x,1,tree.classify.item)
}

tree.grow.bag <- function(x,y,nmin,minleaf,nfeat,m) {
    trees <- list()
    for (i in 1:m) {
        trees[[i]] <- tree.grow(x,y,nmin,minleaf,nfeat)
    }
    trees
}

tree.classify.bag <- function(x,trs) {
    classify.one.tree <- function (tree) {
        tree.classify(x,tree)
    }
    results <- lapply(trs,classify.one.tree)
    sapply(1:length(results[[1]]), function(i) {
        elements <- sapply(1:length(results), function(j) {
            results[[j]][i]
        })
        ifelse(sum(elements) * 2 < length(elements),0,1)
    })
}
