#ifndef ASTAR_H
#define ASTAR_H

#include "my_real_c.inc"
#include "GraphBLAS.h"
#include "vector_Points.h"
#include "vector_int.h"

void astar(const Vector_points2D *vertices, const GrB_Matrix *edges, uint64_t start_pt, uint64_t end_pt, const Vector_uint *forbidden_edges, \
            Vector_uint* listEdges, Vector_uint* listNodes);

#endif
