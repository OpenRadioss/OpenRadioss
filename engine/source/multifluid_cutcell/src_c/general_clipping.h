#ifndef GENERAL_CLIPPING_H
#define GENERAL_CLIPPING_H

//######################################################################################################    
//                   GENERIC HELPER FUNCTIONS FOR CLIPPING IN ANY DIMENSION
//######################################################################################################    


#include "Polygon2D.h"
#include "Polyhedron3D.h"

//    """
//    Flags for fusion of cells. \n
//        NOT_FUSED: cell not fused with another \n
//        FUSED: cell fused \n
//        PROBLEMATIC: cell may be fused (or not) but the process failed (there were no cell in the neighborhood with enough "fluid" zone.)
//    """
    enum FusingCellType {
        NOT_FUSED,
        FUSED,
        TARGET_FUSED,
        PROBLEMATIC,
        PROBLEMATIC_UNSOLVED
    };

void cut_edges3D(const Polyhedron3D* p, const Point3D* normal, const Point3D* pt, int8_t sign_taken, Vector_points3D* pts_copy, GrB_Matrix* new_edges_in);

//[INOUT] cells_in 
//[IN] supercells
//[INOUT] status_cell
//[IN] mark_cells
//[OUT] supercells_in
void close_cells(GrB_Matrix *cells_in, const GrB_Matrix *supercells, Vector_int *status_cell, long int mark_cells, GrB_Matrix *supercells_in);

GrB_Vector surfaces_poly2D(const Polygon2D *p);
GrB_Matrix surfaces_poly3D(const Polyhedron3D *p);
//GrB_Matrix surfaces_poly4D(const Polytope4D *p);
GrB_Vector volumes_poly3D(const Polyhedron3D *p);
//GrB_Vector volumes_poly4D(const Polytope4D *p);
//GrB_Vector hypervolumes_poly4D(const Polytope4D *p);

Vector_points2D* points2D_from_matrix(const GrB_Matrix m_pts);
Vector_points3D* points3D_from_matrix(const GrB_Matrix m_pts);
Vector_points4D* points4D_from_matrix(const GrB_Matrix m_pts);

#endif