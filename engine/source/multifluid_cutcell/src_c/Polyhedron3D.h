#ifndef POLYHEDRON3D_H
#define POLYHEDRON3D_H

#include "GraphBLAS.h"
#include "my_real_c.inc"
#include "vector_Points.h"
#include "vector_int.h"
#include "vector_double.h"

typedef struct {
    Vector_points3D* vertices;
    GrB_Matrix* edges;
    GrB_Matrix* faces;
    GrB_Matrix* volumes;
    Vector_int* status_face;
} Polyhedron3D;

Polyhedron3D* new_Polyhedron3D();
Polyhedron3D* new_Polyhedron3D_vefs(const Vector_points3D* vertices, const GrB_Matrix* edges, const GrB_Matrix* faces, const Vector_int* status_face);
Polyhedron3D* new_Polyhedron3D_vefvs(const Vector_points3D* vertices, const GrB_Matrix* edges, const GrB_Matrix* faces, const GrB_Matrix* volumes, const Vector_int* status_face);
Polyhedron3D* Polyhedron3D_from_vertices(const my_real_c* x_v, unsigned long int n_x, const my_real_c* y_v, unsigned long int n_y, const my_real_c* z_v, unsigned long int n_z);
//Deep copy
void copy_Polyhedron3D(const Polyhedron3D *src, Polyhedron3D *dest);
void dealloc_Polyhedron3D(Polyhedron3D* p);
Polyhedron3D* fuse_polyhedrons(const Polyhedron3D* p1, const Polyhedron3D* p2);
void clean_Polyhedron3D(const Polyhedron3D* p, Polyhedron3D** res_p);
void print_polyhedron3D(const Polyhedron3D* src, const char* file);

#endif
