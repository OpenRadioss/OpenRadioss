#ifndef POLYGON2D_H
#define POLYGON2D_H

#include "GraphBLAS.h"
#include "vector_Points.h"
#include "vector_int.h"
#include "vector_double.h"
#include "my_real_c.inc"

typedef struct {
    Vector_points2D* vertices;
    GrB_Matrix* edges;
    GrB_Matrix* faces;
    Vector_int* status_edge;
    Vector_int* phase_face;
} Polygon2D;

Polygon2D* new_Polygon2D();
Polygon2D* new_Polygon2D_vesp(const Vector_points2D* vertices, const GrB_Matrix* edges, const Vector_int* status_edge, const Vector_int* phase_face);
Polygon2D* new_Polygon2D_vefsp(const Vector_points2D* vertices, const GrB_Matrix* edges, const GrB_Matrix* faces,\
                                 const Vector_int* status_edge, const Vector_int* phase_face);
Polygon2D* polygon2D_from_vertices(const my_real_c* x_v, unsigned long int n_x, const my_real_c* y_v, unsigned long int n_y);
Polygon2D* polygon_from_consecutive_points(const my_real_c *x_v, const my_real_c* y_v, unsigned long int nb_pts);
void copy_Polygon2D(const Polygon2D *src, Polygon2D *dest);
void dealloc_Polygon2D(Polygon2D* p);
void retrieve_ith_edge2D(Vector_points2D *vertices, GrB_Matrix* edges, int i, Point2D** e_ext1, Point2D** e_ext2);
void dropzeros(GrB_Matrix* M);
Polygon2D* fuse_polygons(Polygon2D* p1, Polygon2D* p2);
void compute_all_normals2D(const Polygon2D* p, Vector_points2D *normals_pts, Vector_points2D *normals_edges, my_real_c* min_lgth_nom_edg);
void clean_Polygon2D(const Polygon2D* p, Polygon2D** res_p);
Polygon2D* extract_ith_face2D(const Polygon2D *p, uint64_t extr_index);
void print_polygon2D(const Polygon2D* src, const char* file);

#endif
