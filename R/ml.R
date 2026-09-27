# Machine learning support: pattern clustering, PCA, outliers, analogs ---------------------
#
# Clusters patterns on standardized dimensionless variables so plots can be
# coloured by behaviour group, outliers can be spotted, and analog patterns can
# be proposed for prototypes. Base R (kmeans / hclust / prcomp) + cluster::silhouette.

ml_features <- c(dwi = "DWI", sec_rf = "Sec RF", opr = "OPR", wpr = "WPR", util = "Utilization",
                 tp12 = "Inj TP 12 m", iwr12 = "IWR 12 m", lwor = "log WOR", loss = "Loss", wc6 = "Water cut")

ml_matrix <- function(sn, features) {
  d <- data.table::copy(sn[dwi > 0])
  d[, lwor := log10(pmax(wor6, 1e-3))]
  x <- as.matrix(d[, ..features])
  # impute missing with the column median, then standardize
  for (j in seq_len(ncol(x))) { m <- stats::median(x[, j], na.rm = TRUE); x[!is.finite(x[, j]), j] <- ifelse(is.finite(m), m, 0) }
  keep <- apply(x, 2, stats::sd) > 0
  x <- x[, keep, drop = FALSE]
  z <- scale(x)
  rownames(z) <- d$entity
  list(z = z, entities = d$entity, features = colnames(x))
}

ml_cluster <- function(sn, features = c("dwi", "sec_rf", "opr", "wpr", "util", "tp12", "iwr12", "lwor"),
                       k = NULL, method = c("kmeans", "hclust"), seed = 1) {
  method <- match.arg(method)
  m <- ml_matrix(sn, features)
  z <- m$z
  n <- nrow(z)
  if (n < 4 || ncol(z) < 2) return(NULL)
  fit_k <- function(kk) {
    if (method == "kmeans") { set.seed(seed); cl <- stats::kmeans(z, kk, nstart = 25)$cluster }
    else cl <- stats::cutree(stats::hclust(stats::dist(z), "ward.D2"), kk)
    sil <- cluster::silhouette(cl, stats::dist(z))
    list(cl = cl, sil = mean(sil[, "sil_width"]), sil_i = sil[, "sil_width"])
  }
  ks <- 2:max(2, min(5, floor(n / 3)))
  scan <- data.table::rbindlist(lapply(ks, function(kk) data.table::data.table(k = kk, silhouette = fit_k(kk)$sil)))
  if (is.null(k)) k <- scan$k[which.max(scan$silhouette)]
  f <- fit_k(k)
  cl <- f$cl
  centers <- do.call(rbind, lapply(sort(unique(cl)), function(c) colMeans(z[cl == c, , drop = FALSE])))
  rownames(centers) <- sort(unique(cl))
  # name each cluster by its two most distinctive features
  labs <- ml_features[colnames(z)]; labs[is.na(labs)] <- colnames(z)[is.na(labs)]
  cname <- vapply(seq_len(nrow(centers)), function(i) {
    o <- order(-abs(centers[i, ]))[1:min(2, ncol(centers))]
    paste(sprintf("%s %s", ifelse(centers[i, o] > 0, "high", "low"), labs[o]), collapse = ", ")
  }, "")
  dist_c <- sqrt(rowSums((z - centers[as.character(cl), , drop = FALSE])^2))
  pca <- stats::prcomp(z)
  assign <- data.table::data.table(entity = m$entities, cluster = cl,
                                   cluster_name = paste0("C", cl, ": ", cname[match(cl, as.integer(rownames(centers)))]),
                                   silhouette = f$sil_i, outlier = dist_c / stats::median(dist_c),
                                   pc1 = pca$x[, 1], pc2 = pca$x[, 2])
  list(assign = assign, centers = centers, features = colnames(z), labels = labs, scan = scan, k = k, method = method,
       pca = pca, z = z, var_explained = summary(pca)$importance[2, 1:2])
}

# Nearest analog patterns in standardized feature space.
ml_analogs <- function(ml, entity, n = 3) {
  if (is.null(ml) || !entity %in% rownames(ml$z)) return(data.table::data.table())
  d <- sqrt(colSums((t(ml$z) - ml$z[entity, ])^2))
  d <- sort(d[names(d) != entity])[seq_len(min(n, length(d) - 0))]
  data.table::data.table(entity = names(d), distance = round(as.numeric(d), 2))
}

cluster_palette <- c("#22d3ee", "#f472b6", "#a3e635", "#fbbf24", "#a78bfa", "#fb923c")
