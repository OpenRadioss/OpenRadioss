set(all_c_files  
              array_double.c
              array_Points.c
              csparse.c
              Polygon2D.c
              solver_for_graphblas.c
              vector_double.c
              vector_Points.c
              array_int.c
              bridge_c_fortran.c
              compute_lambdas3D.c
              general_clipping.c
              Point.c
              Polyhedron3D.c
              update_clipped.c
              vector_int.c
              edge_collision.c
              vector_indices_intersection.c
              AStar.c
              NodeWithCost_OrdSet.c
              polygon_fusion_and_split.c
              CACHE INTERNAL "")
add_library(mylib_c STATIC "${all_c_files}")
target_link_libraries(mylib_c GraphBLAS)
include_directories("${CMAKE_CURRENT_SOURCE_DIR}/include")
include_directories("${CMAKE_CURRENT_SOURCE_DIR}/../GraphBLAS/Include")