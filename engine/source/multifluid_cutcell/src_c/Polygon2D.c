#include "Polygon2D.h"
#include <stdio.h>
#include "macro_check_gb_call.h"

// Polygon2D constructor
Polygon2D* new_Polygon2D(){
    GrB_Info infogrb;
    Polygon2D* p = (Polygon2D*) malloc(sizeof(Polygon2D));

    p->vertices = alloc_empty_vec_pts2D();
    p->edges = (GrB_Matrix*) malloc(sizeof(GrB_Matrix));
    p->faces = (GrB_Matrix*) malloc(sizeof(GrB_Matrix));
    infogrb = GrB_Matrix_new(p->edges, GrB_INT8, 1,1);
    infogrb = GrB_Matrix_new(p->faces, GrB_INT8, 1,1);
    p->status_edge = alloc_empty_vec_int();
    p->phase_face = alloc_empty_vec_int();
    
    return p;
}

// Polygon2D constructor
Polygon2D* new_Polygon2D_vesp(const Vector_points2D* vertices, const GrB_Matrix* edges, const Vector_int* status_edge, const Vector_int* phase_face){
    GrB_Info infogrb;
    GrB_Index nb_rows;
    Polygon2D* p = (Polygon2D*) malloc(sizeof(Polygon2D));

    p->vertices = alloc_empty_vec_pts2D();
    copy_vec_pts2D(vertices, p->vertices);
    p->edges = (GrB_Matrix*) malloc(sizeof(GrB_Matrix));
    GrB_Matrix_dup(p->edges, *edges);
    p->faces = (GrB_Matrix*) malloc(sizeof(GrB_Matrix));

    infogrb = GrB_Matrix_ncols(&nb_rows, *(p->edges));
    infogrb = GrB_Matrix_new(p->faces, GrB_INT8, nb_rows, 1);

    p->status_edge = alloc_empty_vec_int();
    copy_vec_int(status_edge, p->status_edge);

    p->phase_face = alloc_empty_vec_int();
    copy_vec_int(phase_face, p->phase_face);

    return p;
}

// Polygon2D constructor
Polygon2D* new_Polygon2D_vefsp(const Vector_points2D* vertices, const GrB_Matrix* edges, const GrB_Matrix* faces,\
                                 const Vector_int* status_edge, const Vector_int* phase_face){
    //GrB_Index nb_rows;
    Polygon2D* p = (Polygon2D*) malloc(sizeof(Polygon2D));

    p->vertices = alloc_empty_vec_pts2D();
    copy_vec_pts2D(vertices, p->vertices);
    p->edges = (GrB_Matrix*) malloc(sizeof(GrB_Matrix));
    GrB_Matrix_dup(p->edges, *edges);
    p->faces = (GrB_Matrix*) malloc(sizeof(GrB_Matrix));
    GrB_Matrix_dup(p->faces, *faces);
    p->status_edge = alloc_empty_vec_int();
    copy_vec_int(status_edge, p->status_edge);
    p->phase_face = alloc_empty_vec_int();
    copy_vec_int(phase_face, p->phase_face);

    return p;
}

/// @brief Builds polygons based on a list of vertices (x_v, y_v) defining a grid
Polygon2D* polygon2D_from_vertices(const my_real_c* x_v, unsigned long int n_x, const my_real_c* y_v, unsigned long int n_y){
    GrB_Info infogrb;
    GrB_Index nb_pts, nb_edges, nb_faces;
    uint64_t curr_face, curr_pt, curr_edge, iy, ix;
    uint64_t ind_pt_SW, ind_pt_SE, ind_pt_NW;
    uint64_t ind_e_W, ind_e_S, ind_e_E, ind_e_N;
    my_real_c yS, xW;
    Point2D ptSW;
    Vector_points2D* vertices;
    Vector_int* status_edge, *phase_face;
    GrB_Matrix* edges = (GrB_Matrix*) malloc(sizeof(GrB_Matrix));
    GrB_Matrix* faces = (GrB_Matrix*) malloc(sizeof(GrB_Matrix));
    Polygon2D* res_p;

    if (n_x<2 || n_y<2)
        return new_Polygon2D();

    nb_pts = n_x*n_y;
    nb_edges = (2*(n_x-1) + 1)*(n_y-1) + (n_y-1);
    nb_faces = (n_x-1)*(n_y-1);

    vertices = alloc_with_capacity_vec_pts2D(nb_pts);
    infogrb = GrB_Matrix_new(edges, GrB_INT8, nb_pts, nb_edges);
    infogrb = GrB_Matrix_new(faces, GrB_INT8, nb_edges, nb_faces);

    ptSW = (Point2D){x_v[0], y_v[0]};
    push_back_vec_pts2D(&vertices, &ptSW);
    curr_face = 0;
    curr_pt = 0;
    curr_edge = 0;
    for (iy = 0; iy < n_y-2; iy++){
        yS = y_v[iy];
        for (ix = 0; ix < n_x-1; ix++){
            ind_pt_SW = curr_pt;
            ind_pt_SE = curr_pt + 1;
            ind_pt_NW = curr_pt + n_x;

            ind_e_W = curr_edge;
            ind_e_S = curr_edge + 1;
            ind_e_E = curr_edge + 2;
            ind_e_N = curr_edge + (2*(n_x-1) + 2);

            xW = x_v[ix];
            ptSW = (Point2D){xW,yS};

            set_ith_elem_vec_pts2D(vertices, ind_pt_SW, &ptSW);

            CHECK_GB_CALL(infogrb, GrB_Matrix_setElement(*edges, -1, ind_pt_SW, ind_e_W))
            CHECK_GB_CALL(infogrb, GrB_Matrix_setElement(*edges,  1, ind_pt_NW, ind_e_W))
            CHECK_GB_CALL(infogrb, GrB_Matrix_setElement(*edges, -1, ind_pt_SW, ind_e_S))
            CHECK_GB_CALL(infogrb, GrB_Matrix_setElement(*edges,  1, ind_pt_SE, ind_e_S))

            CHECK_GB_CALL(infogrb, GrB_Matrix_setElement(*faces,  1, ind_e_S, curr_face))
            CHECK_GB_CALL(infogrb, GrB_Matrix_setElement(*faces,  1, ind_e_E, curr_face))
            CHECK_GB_CALL(infogrb, GrB_Matrix_setElement(*faces, -1, ind_e_N, curr_face))
            CHECK_GB_CALL(infogrb, GrB_Matrix_setElement(*faces, -1, ind_e_W, curr_face))
        
            curr_pt += 1;
            curr_edge += 2;
            curr_face += 1;
        }
        //Treat east-most point and edge
        xW = x_v[n_x-1];
        ptSW = (Point2D){xW,yS};
        ind_pt_SW = curr_pt;
        ind_pt_NW = curr_pt + n_x;
        ind_e_W = curr_edge;

        set_ith_elem_vec_pts2D(vertices, ind_pt_SW, &ptSW);

        CHECK_GB_CALL(infogrb, GrB_Matrix_setElement(*edges, -1, ind_pt_SW, ind_e_W))
        CHECK_GB_CALL(infogrb, GrB_Matrix_setElement(*edges,  1, ind_pt_NW, ind_e_W))

        curr_pt += 1;
        curr_edge += 1;
    }

    //Treat north-most cells
    iy = n_y-2;
    yS = y_v[iy];
    ind_e_N = curr_edge + (2*(n_x-1));
    for (ix = 0; ix < n_x-1; ix++){
        ind_pt_SW = curr_pt;
        ind_pt_SE = curr_pt + 1;
        ind_pt_NW = curr_pt + n_x;

        ind_e_W = curr_edge;
        ind_e_S = curr_edge + 1 ;
        ind_e_E = curr_edge + 2;
        ind_e_N += 1;

        xW = x_v[ix];
        ptSW = (Point2D){xW,yS};
        set_ith_elem_vec_pts2D(vertices, ind_pt_SW, &ptSW);

        CHECK_GB_CALL(infogrb, GrB_Matrix_setElement(*edges, -1, ind_pt_SW, ind_e_W))
        CHECK_GB_CALL(infogrb, GrB_Matrix_setElement(*edges,  1, ind_pt_NW, ind_e_W))
        CHECK_GB_CALL(infogrb, GrB_Matrix_setElement(*edges, -1, ind_pt_SW, ind_e_S))
        CHECK_GB_CALL(infogrb, GrB_Matrix_setElement(*edges,  1, ind_pt_SE, ind_e_S))

        CHECK_GB_CALL(infogrb, GrB_Matrix_setElement(*faces,  1, ind_e_S, curr_face))
        CHECK_GB_CALL(infogrb, GrB_Matrix_setElement(*faces,  1, ind_e_E, curr_face))
        CHECK_GB_CALL(infogrb, GrB_Matrix_setElement(*faces, -1, ind_e_N, curr_face))
        CHECK_GB_CALL(infogrb, GrB_Matrix_setElement(*faces, -1, ind_e_W, curr_face))

        curr_pt += 1;
        curr_edge += 2;
        curr_face += 1;
    }
    //Treat north-east-most point and edge
    xW = x_v[n_x - 1];
    ptSW = (Point2D){xW,yS};
    ind_pt_SW = curr_pt;
    ind_pt_NW = curr_pt + n_x;
    ind_e_W = curr_edge;

    set_ith_elem_vec_pts2D(vertices, ind_pt_SW, &ptSW);

    CHECK_GB_CALL(infogrb, GrB_Matrix_setElement(*edges, -1, ind_pt_SW, ind_e_W))
    CHECK_GB_CALL(infogrb, GrB_Matrix_setElement(*edges,  1, ind_pt_NW, ind_e_W))

    curr_pt += 1;
    curr_edge += 1;
    

    //Treat north-most points and edges
    yS = y_v[n_y - 1];
    for (ix = 0; ix < n_x-1; ix++){
        ind_pt_SW = curr_pt;
        ind_pt_SE = curr_pt + 1;

        ind_e_S = curr_edge;

        xW = x_v[ix];
        ptSW = (Point2D){xW,yS};

        set_ith_elem_vec_pts2D(vertices, ind_pt_SW, &ptSW);

        CHECK_GB_CALL(infogrb, GrB_Matrix_setElement(*edges, -1, ind_pt_SW, ind_e_S))
        CHECK_GB_CALL(infogrb, GrB_Matrix_setElement(*edges,  1, ind_pt_SE, ind_e_S))

        curr_pt += 1;
        curr_edge += 1;
    }
    xW = x_v[n_x-1];
    ptSW = (Point2D){xW,yS};
    ind_pt_SW = curr_pt;
    set_ith_elem_vec_pts2D(vertices, ind_pt_SW, &ptSW);

    status_edge = alloc_empty_vec_int();
    status_edge->data = (long int*) calloc(nb_edges, sizeof(long int)); //all zeros
    status_edge->capacity = nb_edges;
    status_edge->size = nb_edges;

    phase_face = alloc_empty_vec_int();
    phase_face->data = (long int*) calloc(nb_faces, sizeof(long int)); //all zeros
    phase_face->capacity = nb_faces;
    phase_face->size = nb_faces;

    res_p = new_Polygon2D_vefsp(vertices, edges, faces, status_edge, phase_face);

    dealloc_vec_pts2D(vertices); free(vertices);
    dealloc_vec_int(status_edge); free(status_edge);
    dealloc_vec_int(phase_face); free(phase_face);
    GrB_free(edges);free(edges);
    GrB_free(faces);free(faces);

    return res_p;
}

/// @brief Builds 1 polygon based on a list of vertices (x_v, y_v) one after the other
Polygon2D* polygon_from_consecutive_points(const my_real_c *x_v, const my_real_c* y_v, unsigned long int nb_pts){
    GrB_Info infogrb;
    unsigned long int nb_edges, nb_faces;
    Vector_points2D* vertices;
    Vector_int* status_edge, *phase_face;
    GrB_Matrix* edges = (GrB_Matrix*) malloc(sizeof(GrB_Matrix));
    GrB_Matrix* faces = (GrB_Matrix*) malloc(sizeof(GrB_Matrix));
    Point2D pt;
    unsigned long int i;
    long int zero = 0;
    Polygon2D* res_p;

    if (nb_pts < 3)
        return new_Polygon2D();

    nb_edges = nb_pts;
    nb_faces = 1;

    vertices = alloc_with_capacity_vec_pts2D(nb_pts);
    status_edge = alloc_with_capacity_vec_int(nb_pts);
    phase_face = alloc_with_capacity_vec_int(nb_faces);
    infogrb = GrB_Matrix_new(edges, GrB_INT8, nb_pts, nb_edges);
    infogrb = GrB_Matrix_new(faces, GrB_INT8, nb_edges, nb_faces);

    for(i=0; i<nb_pts; i++){
        pt = (Point2D){x_v[i], y_v[i]};
        push_back_vec_pts2D(&vertices, &pt);
        push_back_vec_int(&status_edge, &zero);
    }
    zero = 2;
    for(i=0; i<nb_faces; i++){
        push_back_vec_int(&phase_face, &zero);
    }

    for(i=0; i<(nb_edges-1); i++){
        CHECK_GB_CALL(infogrb, GrB_Matrix_setElement(*edges, -1, i, i))
        CHECK_GB_CALL(infogrb, GrB_Matrix_setElement(*edges, 1, i+1, i))
    }
    infogrb = GrB_Matrix_setElement(*edges, -1, nb_pts-1, nb_pts-1);
    infogrb = GrB_Matrix_setElement(*edges, 1, 0, nb_pts-1);

    for(i=0; i<nb_edges; i++)
        CHECK_GB_CALL(infogrb, GrB_Matrix_setElement(*faces, 1, i, 0))

    
    res_p = new_Polygon2D_vefsp(vertices, edges, faces, status_edge, phase_face);

    dealloc_vec_pts2D(vertices); free(vertices);
    dealloc_vec_int(status_edge); free(status_edge);
    dealloc_vec_int(phase_face); free(phase_face);
    GrB_free(edges); free(edges);
    GrB_free(faces); free(faces);

    return res_p;
}

void copy_Polygon2D(const Polygon2D *src, Polygon2D *dest){
    GrB_Index nb_pts, nb_edges, nb_faces;

    if (src == NULL){
        if (dest != NULL) {
            dealloc_vec_pts2D(dest->vertices);
            if(dest->vertices) free(dest->vertices);
            GrB_free(dest->edges);
            GrB_free(dest->faces);
            dealloc_vec_int(dest->status_edge);
            if(dest->status_edge) free(dest->status_edge);
            dealloc_vec_int(dest->phase_face);
            if(dest->phase_face) free(dest->phase_face);
            dest->vertices = NULL;
            dest->edges = NULL;
            dest->faces = NULL;
            dest->status_edge = NULL;
            dest->phase_face = NULL;
        }
    } else {
        if (dest != NULL){
            //GrB_free(dest->edges); free(dest->edges);
            //GrB_free(dest->faces); free(dest->faces);
            GrB_Matrix_nrows(&nb_pts, *(src->edges));
            GrB_Matrix_ncols(&nb_edges, *(src->edges));
            GrB_Matrix_ncols(&nb_faces, *(src->faces));
            if (dest->edges) {
                GrB_Matrix_resize(*(dest->edges), nb_pts, nb_edges);
            } else {
                dest->edges = (GrB_Matrix*) malloc(sizeof(GrB_Matrix));
                GrB_Matrix_new(dest->edges, GrB_INT8, nb_pts, nb_edges);
            }
            if (dest->faces){
                GrB_Matrix_resize(*(dest->faces), nb_edges, nb_faces);
            } else {
                dest->faces = (GrB_Matrix*) malloc(sizeof(GrB_Matrix));
                GrB_Matrix_new(dest->faces, GrB_INT8, nb_edges, nb_faces);
            }
            if (!dest->vertices){
                dest->vertices = alloc_empty_vec_pts2D();
            }
            copy_vec_pts2D(src->vertices, dest->vertices);
            GrB_Matrix_dup(dest->edges, *(src->edges));
            GrB_Matrix_dup(dest->faces, *(src->faces));
            if (!dest->status_edge){
                dest->status_edge = alloc_empty_vec_int();
            }
            copy_vec_int(src->status_edge, dest->status_edge);
            if (!dest->phase_face){
                dest->phase_face = alloc_empty_vec_int();
            }
            copy_vec_int(src->phase_face, dest->phase_face);
        }
    }
}

void dealloc_Polygon2D(Polygon2D* p){
    if (p){
        dealloc_vec_pts2D(p->vertices);
        if(p->vertices) free(p->vertices);
        GrB_free(p->edges);
        if(p->edges) free(p->edges);
        GrB_free(p->faces);
        if(p->faces) free(p->faces);
        dealloc_vec_int(p->status_edge);
        if(p->status_edge) free(p->status_edge);
        dealloc_vec_int(p->phase_face);
        if(p->phase_face) free(p->phase_face);
    }
}

void retrieve_ith_edge2D(Vector_points2D *vertices, GrB_Matrix* edges, int i, Point2D** e_ext0, Point2D** e_ext1){
    GrB_Info infogrb;
    GrB_Index size_nz_ei;
    GrB_Vector ei;
    GrB_Vector nz_ei;
    GrB_Vector extr_vals_ei;
    GrB_Index i_pt0, i_pt1;
    GrB_Index nb_pts;

    *e_ext0 = NULL;
    *e_ext1 = NULL;
    GrB_Matrix_nrows(&nb_pts, *edges);
    CHECK_GB_CALL(infogrb, GrB_Vector_new(&ei, GrB_INT8, nb_pts))
    CHECK_GB_CALL(infogrb, GrB_Vector_new(&nz_ei, GrB_UINT64, nb_pts))
    CHECK_GB_CALL(infogrb, GrB_Vector_new(&extr_vals_ei, GrB_INT8, nb_pts))
    
    CHECK_GB_CALL(infogrb, GrB_extract(ei, GrB_NULL, GrB_NULL, *edges, GrB_ALL, 1, i, GrB_NULL)) //Get indices of points composing edge i
    CHECK_GB_CALL(infogrb, GxB_Vector_extractTuples_Vector(nz_ei, extr_vals_ei, ei, GrB_NULL))
    CHECK_GB_CALL(infogrb, GrB_Vector_size(&size_nz_ei, nz_ei))
    if (size_nz_ei>0){
        CHECK_GB_CALL(infogrb, GrB_Vector_extractElement(&i_pt0, nz_ei, 0))
        CHECK_GB_CALL(infogrb, GrB_Vector_extractElement(&i_pt1, nz_ei, 1))
        *e_ext0 = get_ith_elem_vec_pts2D(vertices, i_pt0);
        *e_ext1 = get_ith_elem_vec_pts2D(vertices, i_pt1);
    }

    GrB_free(&ei);
    GrB_free(&nz_ei);
    GrB_free(&extr_vals_ei);
}

/// @brief Removes all zeros from sparse matrix M.
void dropzeros(GrB_Matrix* M){
    //GrB_assign(*M, *M, GrB_NULL, *M, GrB_ALL, 0, GrB_ALL, 0, GrB_DESC_R); //In case GxB_select gets changed in the future, this line will always work
    GxB_select(*M, NULL, NULL, GxB_NONZERO, *M, NULL, NULL) ;
}

/// @brief Fuse all faces of each polygon into a single structure.
Polygon2D* fuse_polygons(Polygon2D* p1, Polygon2D* p2){
    GrB_Info infogrb;
    Vector_points2D* fused_vertices;
    GrB_Matrix *fused_edges = (GrB_Matrix*)malloc(sizeof(GrB_Matrix));
    GrB_Matrix *fused_faces = (GrB_Matrix*)malloc(sizeof(GrB_Matrix));
    Vector_int *fused_status, *fused_phase;
    GrB_Index nrows1, ncols1, nrows2, ncols2;
    GrB_Matrix zeros1, zeros2;
    Polygon2D* res_p;

    fused_vertices = cat_vec_pts2D(p1->vertices, p2->vertices);
    fused_status = cat_vec_int(p1->status_edge, p2->status_edge);
    fused_phase = cat_vec_int(p1->phase_face, p2->phase_face);

    GrB_Matrix_nrows(&nrows1, *(p1->edges));
    GrB_Matrix_ncols(&ncols1, *(p1->edges));
    GrB_Matrix_nrows(&nrows2, *(p2->edges));
    GrB_Matrix_ncols(&ncols2, *(p2->edges));
    GrB_Matrix_new(&zeros1, GrB_INT8, nrows1, ncols2);
    GrB_Matrix_new(&zeros2, GrB_INT8, nrows2, ncols1);
    GrB_Matrix_new(fused_edges, GrB_INT8, nrows1+nrows2, ncols1+ncols2);
    CHECK_GB_CALL(infogrb, GxB_Matrix_concat(*fused_edges, (GrB_Matrix[]){*(p1->edges), zeros1, zeros2, *(p2->edges)}, 2, 2, GrB_NULL))
    //fused_edges = [[p1.edges spzeros(Int8, size(p1.edges, 1), size(p2.edges, 2))];
    //                [spzeros(Int8, size(p2.edges, 1), size(p1.edges, 2)) p2.edges]];

    GrB_Matrix_nrows(&nrows1, *(p1->faces));
    GrB_Matrix_ncols(&ncols1, *(p1->faces));
    GrB_Matrix_nrows(&nrows2, *(p2->faces));
    GrB_Matrix_ncols(&ncols2, *(p2->faces));
    CHECK_GB_CALL(infogrb, GrB_Matrix_resize(zeros1, nrows1, ncols2))
    CHECK_GB_CALL(infogrb, GrB_Matrix_resize(zeros2, nrows2, ncols1))
    GrB_Matrix_new(fused_faces, GrB_INT8, nrows1+nrows2, ncols1+ncols2);
    CHECK_GB_CALL(infogrb, GxB_Matrix_concat(*fused_faces, (GrB_Matrix[]){*(p1->faces), zeros1, zeros2, *(p2->faces)}, 2, 2, GrB_NULL))
    //fused_faces = [[p1.faces  spzeros(Int8, size(p1.faces, 1), size(p2.faces, 2))];
    //                [spzeros(Int8, size(p2.faces, 1), size(p1.faces, 2)) p2.faces]];

    res_p = new_Polygon2D_vefsp(fused_vertices, fused_edges, fused_faces, fused_status, fused_phase);
    
    GrB_free(&zeros1);
    GrB_free(&zeros2);
    dealloc_vec_pts2D(fused_vertices); free(fused_vertices);
    dealloc_vec_int(fused_status); free(fused_status);
    dealloc_vec_int(fused_phase); free(fused_phase);
    GrB_free(fused_edges); free(fused_edges);
    GrB_free(fused_faces); free(fused_faces);

    return res_p;
}

/// @brief Compute all normals to the edges of p and a vector in the normal cone at each point
/// @param p Polygon
/// @param normals_pts Vector in normal cone at each point
/// @param normals_edges Normal vector to each edge
/// @param min_lgth_nom_edg Minimal length of the edges in p.
void compute_all_normals2D(const Polygon2D* p, Vector_points2D *normals_pts, Vector_points2D *normals_edges, my_real_c* min_lgth_nom_edg){
    GrB_Index j;
    GrB_Info infogrb;
    GrB_Vector ej, extr_vals_ej;
    GrB_Vector nz_ej;
    GrB_Index size_nz_ej, nb_pts, nb_edges;
    unsigned long pt_index1, pt_index2;
    Point2D *pt1, *pt2, *pt;
    Point2D normal;
    int8_t sign;
    my_real_c normVec;
                                        
    GrB_Matrix_nrows(&nb_pts, *(p->edges));
    GrB_Matrix_ncols(&nb_edges, *(p->edges));

    CHECK_GB_CALL(infogrb, GrB_Vector_new(&ej, GrB_INT8, nb_pts))
    CHECK_GB_CALL(infogrb, GrB_Vector_new(&nz_ej, GrB_UINT64, nb_pts))
    CHECK_GB_CALL(infogrb, GrB_Vector_new(&extr_vals_ej, GrB_INT8, nb_edges))

    if (min_lgth_nom_edg){
        #if (my_real_c == double)
        *min_lgth_nom_edg = __FLT64_MAX__;
        #else
        *min_lgth_nom_edg = __FLT32_MAX__;
        #endif
    }

    if(normals_pts){
        normal.x = 0.; normal.y = 0.;
        for(j=0; j<nb_pts; j++){
            set_ith_elem_vec_pts2D(normals_pts, j, &normal);
        }
    }
    if(normals_edges){
        normal.x = 0.; normal.y = 0.;
        for(j=0; j<nb_edges; j++){
            set_ith_elem_vec_pts2D(normals_edges, j, &normal);
        }
    }

    for (j=0; j<nb_edges; j++){
        CHECK_GB_CALL(infogrb, GrB_extract(ej, GrB_NULL, GrB_NULL, *(p->edges), GrB_ALL, 1, j, GrB_NULL)) 
        CHECK_GB_CALL(infogrb, GxB_Vector_extractTuples_Vector(nz_ej, extr_vals_ej, ej, GrB_NULL))
        CHECK_GB_CALL(infogrb, GrB_Vector_size(&size_nz_ej, nz_ej))

        if (size_nz_ej > 1){
            CHECK_GB_CALL(infogrb, GrB_Vector_extractElement(&pt_index1, nz_ej, 0)) 
            CHECK_GB_CALL(infogrb, GrB_Vector_extractElement(&pt_index2, nz_ej, 1)) 
            pt1 = get_ith_elem_vec_pts2D(p->vertices, pt_index1);
            pt2 = get_ith_elem_vec_pts2D(p->vertices, pt_index2);

            //Choice of normal: take the normal to the "edge" containing the point, sum of the adjacent edges
            CHECK_GB_CALL(infogrb, GrB_Matrix_extractElement(&sign, *(p->edges), pt_index1, j))
            if (normals_pts){
                pt = get_ith_elem_vec_pts2D(normals_pts, pt_index1);
                pt->x -= sign * pt1->y;
                pt->y += sign * pt1->x;
                pt = get_ith_elem_vec_pts2D(normals_pts, pt_index2);
                pt->x -= sign * pt1->y;
                pt->y += sign * pt1->x;
            }
            if (normals_edges){
                pt = get_ith_elem_vec_pts2D(normals_edges, j);
                pt->x -= sign * pt1->y;
                pt->y += sign * pt1->x;
            }

            CHECK_GB_CALL(infogrb, GrB_Matrix_extractElement(&sign, *(p->edges), pt_index2, j))
            if (normals_pts){
                pt = get_ith_elem_vec_pts2D(normals_pts, pt_index1);
                pt->x -= sign * pt2->y;
                pt->y += sign * pt2->x;
                pt = get_ith_elem_vec_pts2D(normals_pts, pt_index2);
                pt->x -= sign * pt2->y;
                pt->y += sign * pt2->x;
            }
            if (normals_edges){
                pt = get_ith_elem_vec_pts2D(normals_edges, j);
                pt->x -= sign * pt2->y;
                pt->y += sign * pt2->x;

                normVec = sqrt(pt->x*pt->x + pt->y*pt->y);
                if (normVec > 0.){
                    pt->x /= normVec;
                    pt->y /= normVec;

                    if(min_lgth_nom_edg){
                        if (normVec < *min_lgth_nom_edg)
                            *min_lgth_nom_edg = normVec;
                    }
                }
            }
        }
    }
    GrB_Vector_free(&ej);
    GrB_Vector_free(&extr_vals_ej);
    GrB_Vector_free(&nz_ej);

}

/// @brief Delete all empty faces and edges.
void clean_Polygon2D(const Polygon2D* p, Polygon2D** res_p){
    GrB_Index i, j, j_f;
    GrB_Vector fj, extr_vals_fj, edge_indices;
    GrB_Vector ej, extr_vals_ej, pt_indices;
    Vector_uint* ind_kept_pts;
    Vector_points2D* new_vertices;
    GrB_Matrix new_edges, new_faces;
    Vector_int *new_status_edge = NULL, *new_phase_face = NULL;
    GrB_Vector grb_ind_kept_pts, justone;
    GrB_Info infogrb;
    GrB_Index nb_edges, nb_faces, size_edge_indices, size_pt_indices, val, nb_pts;
    uint64_t ind_pt;
    GrB_Index ncols_new_edges, ell;
    uint64_t nrows_new_edges;
    Polygon2D* new_p = NULL;
    Polygon2D* copy_p = new_Polygon2D();

    GrB_Matrix_ncols(&nb_faces, *(p->faces));
    GrB_Matrix_ncols(&nb_edges, *(p->edges));
    GrB_Matrix_nrows(&nb_pts, *(p->edges));
    ind_kept_pts = alloc_empty_vec_uint();
    new_vertices = alloc_empty_vec_pts2D();
    GrB_Matrix_new(&new_edges, GrB_INT8, 1, 1);
    GrB_Vector_new(&grb_ind_kept_pts, GrB_UINT64, 1);
    GrB_Matrix_new(&new_faces, GrB_INT8, 1, 1);
    GrB_Vector_new(&justone, GrB_UINT64, 1);
    GrB_Vector_new(&fj, GrB_INT8, nb_edges);
    GrB_Vector_new(&edge_indices, GrB_UINT64, nb_edges);
    GrB_Vector_new(&extr_vals_fj, GrB_INT8, nb_edges);
    GrB_Vector_new(&ej, GrB_INT8, nb_pts);
    GrB_Vector_new(&pt_indices, GrB_UINT64, nb_pts);
    GrB_Vector_new(&extr_vals_ej, GrB_INT8, nb_pts);
    new_phase_face = alloc_with_capacity_vec_int(1);

    for (i=0; i<nb_faces; i++){
        CHECK_GB_CALL(infogrb, GrB_extract(fj, GrB_NULL, GrB_NULL, *(p->faces), GrB_ALL, 1, i, GrB_NULL)) //Get indices of edges composing face i
        CHECK_GB_CALL(infogrb, GxB_Vector_extractTuples_Vector(edge_indices, extr_vals_fj, fj, GrB_NULL))
        CHECK_GB_CALL(infogrb, GrB_Vector_nvals(&size_edge_indices, edge_indices))
        ind_kept_pts->size = 0;
        new_vertices->size = 0;

        for(j_f=0; j_f<size_edge_indices; j_f++){
            CHECK_GB_CALL(infogrb, GrB_Vector_extractElement(&j, edge_indices, j_f))
            CHECK_GB_CALL(infogrb, GrB_extract(ej, GrB_NULL, GrB_NULL, *(p->edges), GrB_ALL, 1, j, GrB_NULL)) //Get indices of points composing edge j
            CHECK_GB_CALL(infogrb, GxB_Vector_extractTuples_Vector(pt_indices, extr_vals_ej, ej, GrB_NULL))
            CHECK_GB_CALL(infogrb, GrB_Vector_size(&size_pt_indices, pt_indices))
            if(size_pt_indices>0){
                CHECK_GB_CALL(infogrb, GrB_Vector_extractElement(&val, pt_indices, 0))
                push_back_unique_vec_uint(&ind_kept_pts, &val);
                
                CHECK_GB_CALL(infogrb, GrB_Vector_extractElement(&val, pt_indices, 1))
                push_back_unique_vec_uint(&ind_kept_pts, &val);
            }
        }

        //Rebuild list of vertices in face i only
        sort_vec_uint(ind_kept_pts);
        for (j_f=0; j_f<ind_kept_pts->size; j_f++){
            ind_pt = *get_ith_elem_vec_uint(ind_kept_pts, j_f);
            set_ith_elem_vec_pts2D(new_vertices, j_f, get_ith_elem_vec_pts2D(p->vertices, ind_pt));
        }
        
        //new_edges = p.edges[ind_kept_pts, edge_indices]
        //GrB_reduce(&ncols_new_edges, GrB_NULL, GrB_MAX_MONOID_UINT64, edge_indices, GrB_NULL);
        if (ind_kept_pts->size>1){
            ncols_new_edges = size_edge_indices;
            //Rebuild matrix of edges in face i only
            nrows_new_edges = ind_kept_pts->size;
            CHECK_GB_CALL(infogrb, GrB_Vector_resize(grb_ind_kept_pts, ind_kept_pts->size))
            for(ell=0; ell<ind_kept_pts->size; ell++){
                CHECK_GB_CALL(infogrb, GrB_Vector_setElement(grb_ind_kept_pts, *get_ith_elem_vec_uint(ind_kept_pts, ell), ell))
            }
            CHECK_GB_CALL(infogrb, GrB_Matrix_resize(new_edges, nrows_new_edges, ncols_new_edges))
            CHECK_GB_CALL(infogrb, GrB_extract(new_edges, GrB_NULL, GrB_NULL, *(p->edges), grb_ind_kept_pts, edge_indices, GrB_NULL))

            //Rebuild matrix for face i only
            //new_faces = p.faces[edge_indices, i]
            CHECK_GB_CALL(infogrb, GrB_Matrix_resize(new_faces, nrows_new_edges, 1))
            CHECK_GB_CALL(infogrb, GrB_Vector_setElement(justone, i, 0))
            CHECK_GB_CALL(infogrb, GrB_extract(new_faces, GrB_NULL, GrB_NULL, *(p->faces), edge_indices, justone, GrB_NULL))

            //new_status_edge = zeros(nrows_new_edges)
            //if(new_status_edge->size <= nrows_new_edges){
            //    for (j_f=new_status_edge->size; j_f < nrows_new_edges; j_f++){
            //        set_ith_elem_vec_int(new_status_edge, j_f, &zero);
            //    }
            //}
            //new_status_edge->size = nrows_new_edges;
            if (new_status_edge){
                dealloc_vec_int(new_status_edge);
                free(new_status_edge);
            }
            new_status_edge = alloc_with_capacity_vec_int(ncols_new_edges);
            for(j_f = 0; j_f < ncols_new_edges; j_f++){
                CHECK_GB_CALL(infogrb, GrB_Vector_extractElement(&j, edge_indices, j_f))
                set_ith_elem_vec_int(new_status_edge, j_f, get_ith_elem_vec_int(p->status_edge, j));
            }
            set_ith_elem_vec_int(new_phase_face, 0, get_ith_elem_vec_int(p->phase_face, i));

            if (new_p){ //Faces already exist
                copy_Polygon2D(new_p, copy_p);
                dealloc_Polygon2D(new_p); free(new_p);
                new_p = fuse_polygons(copy_p, new_Polygon2D_vefsp(new_vertices, &new_edges, &new_faces, new_status_edge, new_phase_face));
            } else { //First face created
                new_p = new_Polygon2D_vefsp(new_vertices, &new_edges, &new_faces, new_status_edge, new_phase_face);
            }
        }
    }

    if(*res_p){
        dealloc_Polygon2D(*res_p); free(*res_p);
    }
    *res_p = new_p;

    dealloc_Polygon2D(copy_p);free(copy_p);
    if(ind_kept_pts) {dealloc_vec_uint(ind_kept_pts);free(ind_kept_pts);}
    if (new_status_edge) {dealloc_vec_int(new_status_edge);free(new_status_edge);}
    if(new_vertices) {dealloc_vec_pts2D(new_vertices);free(new_vertices);}
    if (new_phase_face){dealloc_vec_int(new_phase_face);free(new_phase_face);}
    GrB_free(&new_edges);
    GrB_free(&new_faces);
    GrB_free(&grb_ind_kept_pts);
    GrB_free(&justone);
    GrB_free(&fj);
    GrB_free(&extr_vals_fj);
    GrB_free(&edge_indices);
    GrB_free(&ej);
    GrB_free(&extr_vals_ej);
    GrB_free(&pt_indices);
}

/// @brief Returns a polygon consisting in only the extracted face.
Polygon2D* extract_ith_face2D(const Polygon2D *p, uint64_t extr_index){
    GrB_Matrix *new_faces, *intermediate_faces, *new_edges;
    Vector_points2D *new_vertices;
    Vector_int *new_status_edge, *new_phase_face;
    GrB_Vector val_extr, J_vect, pt_indices;
    GrB_Index ncols_faces, ncols_edges;
    GrB_Index nrows_faces, nrows_edges;
    GrB_Index length_edge_ind;
    GrB_Index i, e_i;
    unsigned long int indpt1, indpt2;
    GrB_Index *I_;
    GrB_Index *edge_indices;
    uint8_t *nm;
    Vector_uint *pts_kept_inds;
    Polygon2D* res_p;
    GrB_Info infogrb;

    GrB_Matrix_ncols(&ncols_faces, *(p->faces));
    if (extr_index>=ncols_faces){
        printf("Can't extract face %ld of a polygon with only %ld face(s).", extr_index, ncols_faces);
        return NULL;
    }

    GrB_Matrix_nrows(&nrows_faces, *(p->faces));
    GrB_Matrix_ncols(&ncols_edges, *(p->edges));
    GrB_Matrix_nrows(&nrows_edges, *(p->edges));

    new_faces = (GrB_Matrix*)malloc(sizeof(GrB_Matrix));
    intermediate_faces = (GrB_Matrix*)malloc(sizeof(GrB_Matrix));
    new_edges = (GrB_Matrix*)malloc(sizeof(GrB_Matrix));
    
    //First, extract new face.
    CHECK_GB_CALL(infogrb, GrB_Matrix_new(intermediate_faces, GrB_INT8, nrows_faces, 1))
    //CHECK_GB_CALL(infogrb, GrB_Matrix_extract(*intermediate_faces, GrB_NULL, GrB_NULL, *(p->faces), I_, nrows_faces, (GrB_Index[]){extr_index}, 1, GrB_NULL))
    CHECK_GB_CALL(infogrb, GrB_Matrix_extract(*intermediate_faces, GrB_NULL, GrB_NULL, *(p->faces), GrB_ALL, 1, (GrB_Index[]){extr_index}, 1, GrB_NULL))

    //Second, take all edge indices in the extracted face.
    GrB_Matrix_nvals(&length_edge_ind, *intermediate_faces);
    edge_indices = (GrB_Index*) calloc(length_edge_ind, sizeof(GrB_Index));
    nm = (uint8_t*) malloc(length_edge_ind*sizeof(uint8_t));
    I_ = (GrB_Index*)malloc(length_edge_ind*sizeof(GrB_Index));
    CHECK_GB_CALL(infogrb, GrB_Matrix_extractTuples(edge_indices, I_, nm, &length_edge_ind, *intermediate_faces))

    //Third, build a unique list of indices of points involved in kept edges
    GrB_Vector_new(&pt_indices, GrB_UINT64, 2);
    GrB_Vector_new(&J_vect, GrB_UINT64, nrows_edges);
    GrB_Vector_new(&val_extr, GrB_INT8, 2);
    pts_kept_inds = alloc_empty_vec_uint();
    for (i=0; i<length_edge_ind; i++){
        e_i = edge_indices[i];
        CHECK_GB_CALL(infogrb, GrB_extract(J_vect, GrB_NULL, GrB_NULL, *(p->edges), GrB_ALL, 1, e_i, GrB_NULL))
        CHECK_GB_CALL(infogrb, GrB_Vector_extractTuples(pt_indices, val_extr, J_vect, GrB_NULL))
        CHECK_GB_CALL(infogrb, GrB_Vector_extractElement(&indpt1, pt_indices, 0))
        CHECK_GB_CALL(infogrb, GrB_Vector_extractElement(&indpt2, pt_indices, 1))

        push_back_unique_vec_uint(&pts_kept_inds, &indpt1);
        push_back_unique_vec_uint(&pts_kept_inds, &indpt2);
    }

    //Build the new list of vertices
    new_vertices = alloc_with_capacity_vec_pts2D(pts_kept_inds->size);
    for (i=0; i<pts_kept_inds->size; i++){
        indpt1 = *get_ith_elem_vec_uint(pts_kept_inds, i);
        push_back_vec_pts2D(&new_vertices, get_ith_elem_vec_pts2D(p->vertices, indpt1));
    }
    //Build the new list of status of each edge
    new_status_edge = alloc_with_capacity_vec_int(length_edge_ind);
    for (i=0; i<length_edge_ind; i++){
        indpt1 = edge_indices[i];
        push_back_vec_int(&new_status_edge, get_ith_elem_vec_int(p->status_edge, indpt1));
    }

    new_phase_face = alloc_with_capacity_vec_int(1);
    push_back_vec_int(&new_phase_face, get_ith_elem_vec_int(p->phase_face, extr_index));

    //Build the new array describing the edges
    GrB_Matrix_new(new_edges, GrB_INT8, pts_kept_inds->size, length_edge_ind);
    CHECK_GB_CALL(infogrb, \
        GrB_Matrix_extract(*new_edges, GrB_NULL, GrB_NULL, *(p->edges), \
        pts_kept_inds->data, pts_kept_inds->size, edge_indices, length_edge_ind, GrB_NULL))

    //Re-extract new_faces in order to get rid of un-necessary zeros
    GrB_Matrix_new(new_faces, GrB_INT8, length_edge_ind, 1);
    CHECK_GB_CALL(infogrb, \
        GrB_Matrix_extract(*new_faces, GrB_NULL, GrB_NULL, \
            *intermediate_faces, edge_indices, length_edge_ind, (GrB_Index[]){0}, 1, GrB_NULL))
  
    res_p = new_Polygon2D_vefsp(new_vertices, new_edges, new_faces, new_status_edge, new_phase_face);

    free(I_);
    free(edge_indices);
    free(nm);
    dealloc_vec_uint(pts_kept_inds); free(pts_kept_inds);
    GrB_free(&val_extr);
    GrB_free(&J_vect);
    GrB_free(&pt_indices);
    GrB_free(intermediate_faces); free(intermediate_faces);
    dealloc_vec_pts2D(new_vertices); free(new_vertices);
    GrB_free(new_edges); free(new_edges);
    GrB_free(new_faces); free(new_faces);
    dealloc_vec_int(new_status_edge); free(new_status_edge);
    dealloc_vec_int(new_phase_face); free(new_phase_face);

    return res_p;
}

void print_polygon2D(const Polygon2D* src, const char* file){
    FILE* output = fopen(file, "w");
    GrB_Info infogrb;
    GrB_Vector fj, extr_vals_fj, edge_indices;
    GrB_Vector ej, extr_vals_ej, pt_indices;
    GrB_Index nb_edges, nb_faces, size_edge_indices, size_pt_indices, val, nb_pts;
    GrB_Index i, j_f, j;
    Point2D* pt;

    GrB_Matrix_ncols(&nb_faces, *(src->faces));
    GrB_Matrix_ncols(&nb_edges, *(src->edges));
    GrB_Matrix_nrows(&nb_pts, *(src->edges));
    GrB_Vector_new(&fj, GrB_INT8, nb_edges);
    GrB_Vector_new(&edge_indices, GrB_UINT64, nb_edges);
    GrB_Vector_new(&extr_vals_fj, GrB_INT8, nb_edges);
    GrB_Vector_new(&ej, GrB_INT8, nb_pts);
    GrB_Vector_new(&pt_indices, GrB_UINT64, nb_pts);
    GrB_Vector_new(&extr_vals_ej, GrB_INT8, nb_pts);

    //for (i=0; i<nb_faces; i++){
    //    infogrb = GrB_extract(fj, GrB_NULL, GrB_NULL, *(src->faces), GrB_ALL, 1, i, GrB_NULL); //Get indices of edges composing face i
    //    infogrb = GxB_Vector_extractTuples_Vector(edge_indices, extr_vals_fj, fj, GrB_NULL);
    //    infogrb = GrB_Vector_size(&size_edge_indices, edge_indices);

    //    for(j_f=0; j_f<size_edge_indices; j_f++){
    //        infogrb = GrB_Vector_extractElement(&j, edge_indices, j_f);
    //        infogrb = GrB_extract(ej, GrB_NULL, GrB_NULL, *(src->edges), GrB_ALL, 1, j, GrB_NULL); //Get indices of points composing edge j
    //        infogrb = GxB_Vector_extractTuples_Vector(pt_indices, extr_vals_ej, ej, GrB_NULL);
    //        infogrb = GrB_Vector_size(&size_pt_indices, pt_indices);
    //        if(size_pt_indices>0){
    //            infogrb = GrB_Vector_extractElement(&val, pt_indices, 0);
    //            pt = get_ith_elem_vec_pts2D(src->vertices, val);
    //            fprintf(output, "%lf %lf\n", pt->x, pt->y);
    //            
    //            infogrb = GrB_Vector_extractElement(&val, pt_indices, 1);
    //            pt = get_ith_elem_vec_pts2D(src->vertices, val);
    //            fprintf(output, "%lf %lf\n", pt->x, pt->y);
    //        }
    //        fprintf(output, "\n");
    //        fprintf(output, "\n");
    //    }
    //}
    //fclose(output);

    //#Solid vtk output
    fprintf(output,"# vtk DataFile Version 3.0\n");
    fprintf(output,"# Simulation Euler\n");
    fprintf(output,"ASCII\n");
    fprintf(output,"\n");
    fprintf(output,"DATASET UNSTRUCTURED_GRID\n") ;
    fprintf(output,"POINTS %ld DOUBLE\n", src->vertices->size);

    //#Output solid points
    for (i=0; i<src->vertices->size; i++){
        pt = get_ith_elem_vec_pts2D(src->vertices, i);
        fprintf(output, "0 % lf % lf\n", pt->x, pt->y);
    }

    //#Output solid edges
    fprintf(output, "CELLS %ld %ld\n", nb_edges, 3*nb_edges);
    for (i=0; i<nb_faces; i++){
        CHECK_GB_CALL(infogrb, GrB_extract(fj, GrB_NULL, GrB_NULL, *(src->faces), GrB_ALL, 1, i, GrB_NULL)) //Get indices of edges composing face i
        CHECK_GB_CALL(infogrb, GxB_Vector_extractTuples_Vector(edge_indices, extr_vals_fj, fj, GrB_NULL))
        CHECK_GB_CALL(infogrb, GrB_Vector_size(&size_edge_indices, edge_indices))

        for(j_f=0; j_f<size_edge_indices; j_f++){
            CHECK_GB_CALL(infogrb, GrB_Vector_extractElement(&j, edge_indices, j_f))
            CHECK_GB_CALL(infogrb, GrB_extract(ej, GrB_NULL, GrB_NULL, *(src->edges), GrB_ALL, 1, j, GrB_NULL)) //Get indices of points composing edge j
            CHECK_GB_CALL(infogrb, GxB_Vector_extractTuples_Vector(pt_indices, extr_vals_ej, ej, GrB_NULL))
            CHECK_GB_CALL(infogrb, GrB_Vector_size(&size_pt_indices, pt_indices))
            if(size_pt_indices>0){
                CHECK_GB_CALL(infogrb, GrB_Vector_extractElement(&val, pt_indices, 0))
                fprintf(output, "2 %ld ", val);
                
                CHECK_GB_CALL(infogrb, GrB_Vector_extractElement(&val, pt_indices, 1))
                fprintf(output, "%ld\n", val);
            }
        }
    }

    //#Output solid cell types
    fprintf(output, "CELL_TYPES %ld\n", nb_edges);
    for (i=0; i<nb_edges; i++){
        fprintf(output, "3\n"); //For segments
    }

    fclose(output);

    GrB_free(&fj);
    GrB_free(&extr_vals_fj);
    GrB_free(&edge_indices);
    GrB_free(&ej);
    GrB_free(&extr_vals_ej);
    GrB_free(&pt_indices);
}

