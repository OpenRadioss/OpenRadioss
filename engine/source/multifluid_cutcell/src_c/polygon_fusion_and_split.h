#ifndef POLYGON_FUSION_AND_SPLIT_H
#define POLYGON_FUSION_AND_SPLIT_H

#include "my_real_c.inc"
#include "Polygon2D.h"
#include "vector_int.h"

void break_edges_split_fusion(const Polygon2D* old_p, const Vector_uint* edge_intersect1, const Vector_uint* edge_intersect2,\
                             int8_t* pt_in_or_out, Polygon2D** p);
void fuse_faces(const Polygon2D* old_p, Vector_uint** faces_to_fuse, uint64_t size_faces_to_fuse, Polygon2D** result_p);
//It supposes that one edges can only be part of only one face => hard to ensure for more than two fluids..?
void polygons_fusion(const Polygon2D* old_p, const GrB_Matrix* original_edges, const Vector_points2D* pts_intersec, const Vector_uint* edge_intersect1, const Vector_uint* edge_intersect2, const int8_t* pt_in_or_out,\
                            Polygon2D** p, Vector_uint*** faces_to_fuse, uint64_t *size_faces_to_fuse);
void create_new_faces_split(const Polygon2D* old_p, Vector_uint** faces_to_split, uint64_t size_faces_to_split, Polygon2D **result_p);
void polygon_split(const Polygon2D* old_p, const GrB_Matrix* original_edges, const Vector_points2D* pts_intersec, const Vector_uint* edge_intersect1, const Vector_uint* edge_intersect2, const int8_t* pt_in_or_out,\
                    Polygon2D** p, Vector_uint*** faces_to_split, uint64_t *size_faces_to_split);



#endif