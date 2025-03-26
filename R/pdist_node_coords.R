
pdist_node_coords <- function(node_coords, other_xy, 
															snap_tolerance = NULL) {

	node_dist <- pdist(node_coords, other_xy)
	
	dist_matrix <- as.matrix(node_dist)
	# colnames(dist_matrix) <- rownames(other_xy)
	# rownames(dist_matrix) <- rownames(node_coords)
	
	n_flow <- apply(dist_matrix, 1, function(x) sum(x <= snap_tolerance))
	
	snap_check <- apply(dist_matrix, 1, function(x) {
		sum(x > 0 & x <= snap_tolerance)
	})

	return(out.list = list(n_flow = n_flow, snap_check = snap_check))
	
}
