#ifndef EDGE_COLLISION_H
#define EDGE_COLLISION_H

#include "my_real_c.inc"
#include "Polygon2D.h"
#include "vector_int.h"
#include "vector_Points.h"

my_real_c compute_barycentric_coord(Point2D middle_pt, Point2D pt1, Point2D pt2);
void find_all_self_intersection(Polygon2D *p, Vector_points2D* IntersecList, Vector_uint *edge_intersect1, Vector_uint *edge_intersect2);

//All output arrays must be allocated before calling the function.
//Except for both arrays pt_in_or_out_*, they can be left empty. 
//The arrays pt_in_or_out_* are of size p->vertices->size, initialized with 0s.
void in_or_out_intersection(const Polygon2D* p, const Vector_points2D* normal_vectors, \
                            const Vector_points2D* IntersecList, const Vector_uint* edge_intersect1, const Vector_uint* edge_intersect2,\
                            int8_t* pt_in_or_out_split, Vector_points2D* IntersecList_split, \
                            Vector_uint* edge_intersect1_split, Vector_uint* edge_intersect2_split,\
                            int8_t* pt_in_or_out_fuse, Vector_points2D* IntersecList_fuse, \
                            Vector_uint* edge_intersect1_fuse, Vector_uint* edge_intersect2_fuse);

#endif