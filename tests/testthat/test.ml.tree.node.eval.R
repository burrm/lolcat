# require(lolcat)
# 
# context("Machine Learning - Tree Node Evaluation")
# 
# test_that("Template Tree Node Evaluation", {
#   x <- data.frame(a=c(1,2,3), b=c(4,5,6))
#   node <- ml.tree.node.template()
#   
#   res <- predict(node, newdata = x)
#   
#   expect_equal(length(res), nrow(x))
#   
#   expect_equal(42, res[1])
#   expect_equal(42, res[2])
#   expect_equal(42, res[3])
#   
# })
# 
# 
# test_that("First and Second Level Tree Node Evaluation (no find-node results)", {
#   x <- expand.grid(b=c(4,5,6), a=c(1,2,3))
#   
#   root.node <- ml.tree.node.template()
#   node.1 <- ml.tree.node.template()
#   node.2 <- ml.tree.node.template()
#   node.3 <- ml.tree.node.template()
#   
#   node.1$node.id <- "node.1"
#   node.1$this.result <- function(this.node, transformed.data) {
#     if (transformed.data[["b"]] == 4) {
#       1
#     } else if (transformed.data[["b"]] == 5) {
#       2
#     } else if (transformed.data[["b"]] == 6) {
#       3
#     } else {
#       NA
#     }
#   }
#   node.1$result.type <- list("function", "value", "value")
#   node.1$result <- list(function(this.node,transformed.data) {101},102,103)
#   
#   
#   node.2$node.id <- "node.2"
#   node.2$this.result <- function(this.node, transformed.data) {
#     if (transformed.data[["b"]] == 4) {
#       1
#     } else if (transformed.data[["b"]] == 5) {
#       2
#     } else if (transformed.data[["b"]] == 6) {
#       3
#     } else {
#       NA
#     }
#   }
#   node.2$result.type <- list("value", "value", "value")
#   node.2$result <- list(104,105,106)
#   
#   node.3$node.id <- "node.3"
#   node.3$this.result <- parse(text="
#     if (transformed.data[[\"b\"]] == 4) {
#       1
#     } else if (transformed.data[[\"b\"]] == 5) {
#       2
#     } else if (transformed.data[[\"b\"]] == 6) {
#       3
#     } else {
#       NA
#     }")
#   
#   node.3$result.type <- list("value", "value", "value")
#   node.3$result <- list(107,108,109)
#   
#     
#   
#   root.node$node.id <- "root"
#   root.node$result.type <- list("node", "node", "node")
#   root.node$this.result <- function(this.node, transformed.data) {
#     if (transformed.data[["a"]] == 1) {
#       1
#     } else if (transformed.data[["a"]] == 2) {
#       2
#     } else if (transformed.data[["a"]] == 3) {
#       3
#     } else {
#       NA
#     }
#   }
#   
#   root.node$result <- list(node.1,node.2,node.3)
#   
#   res <- predict(root.node, newdata = x)
#   
#   expect_equal(length(res), nrow(x))
#   
#   expect_equal(101, res[1])
#   expect_equal(102, res[2])
#   expect_equal(103, res[3])
# 
#   expect_equal(104, res[4])
#   expect_equal(105, res[5])
#   expect_equal(106, res[6])
# 
#   expect_equal(107, res[7])
#   expect_equal(108, res[8])
#   expect_equal(109, res[9])
#   
# })

