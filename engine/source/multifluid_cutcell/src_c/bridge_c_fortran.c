#define _USE_MATH_DEFINES

#include "my_real_c.inc"
#include "bridge_c_fortran.h"
#include "array_double.h"
#include "vector_double.h"
#include "compute_lambdas3D.h"
#include "solver_for_graphblas.h"
#include "update_clipped.h"
#include "macro_check_gb_call.h"
# define M_PI           3.14159265358979323846

Polygon2D* grid = NULL;
Polygon2D* clipped = NULL;
Polyhedron3D* clipped3D = NULL;

void launch_grb_(){
    //GrB_init(GrB_NONBLOCKING);
    GrB_init(GrB_BLOCKING);
}

void end_grb_(){
    if(grid) {
        dealloc_Polygon2D(grid); free(grid); grid = NULL;
    }
    if(clipped){
        dealloc_Polygon2D(clipped); free(clipped); clipped = NULL;
    }
    if(clipped3D){
        dealloc_Polyhedron3D(clipped3D); free(clipped3D); clipped3D = NULL;
    }

    GrB_finalize();
}

///  @brief Builds grid given consecutive points with coordinates (x_v, y_v).
///  @details This supposes that the grid is only 1 cell, defined through points given in the counter-clockwise order, and it builds a polygon out of it.
///            This polygon is built such that all edges always go from the smaller index to the bigger index, and the orientation is then
///            reversed in the faces matrix.
///  @param x_v x-coordinates of the points
///  @param y_v y-coordinates of the points
///  @param pt_indices Global indices of the points in the mesh.
///  @param signed_nb_pts Number of points in this cell.
///  @warning For now, it only works with 4 points.
void build_grid_from_points_fortran_(const my_real_c* x_v, const my_real_c* y_v, long long int* pt_indices, const long long int *signed_nb_pts){
    const unsigned long nb_pts = (unsigned long) *signed_nb_pts;
    Point2D p;
    unsigned long i;
    GrB_Info infogrb;
    unsigned long int nb_edges, nb_faces;
    Vector_points2D* vertices;
    Vector_int* status_edge, *phase_face;
    GrB_Matrix* edges;
    GrB_Matrix* faces;
    Point2D pt;
    long int zero = 0;
    
    if (nb_pts != 4) {
        if (grid){
            for(i=0; i<nb_pts; i++){
                p = (Point2D){x_v[i], y_v[i]};
                set_ith_elem_vec_pts2D(grid->vertices, i, &p);
            }
        } else {
            grid = polygon_from_consecutive_points(x_v, y_v, nb_pts);
        }
    } else {
        nb_edges = nb_pts;
        nb_faces = 1;

        if (!(grid)){
            grid = new_Polygon2D();
            grid->edges = (GrB_Matrix*) malloc(sizeof(GrB_Matrix));
            grid->faces = (GrB_Matrix*) malloc(sizeof(GrB_Matrix));
            grid->vertices = alloc_with_capacity_vec_pts2D(nb_pts);
            infogrb = GrB_Matrix_new(grid->edges, GrB_INT8, nb_pts, nb_edges);
            infogrb = GrB_Matrix_new(grid->faces, GrB_INT8, nb_edges, nb_faces);
            grid->status_edge = alloc_with_capacity_vec_int(nb_pts);
            grid->phase_face = alloc_with_capacity_vec_int(nb_faces);
        } 
        vertices = grid->vertices;
        if (grid->vertices->size > nb_pts)
            grid->vertices->size = nb_pts;
        edges = grid->edges;
        infogrb = GrB_Matrix_clear(*edges);
        faces = grid->faces;
        infogrb = GrB_Matrix_clear(*faces);
        status_edge = grid->status_edge;
        if (grid->status_edge->size > nb_pts)
            grid->status_edge->size = nb_pts;
        phase_face = grid->phase_face;
        if (grid->phase_face->size > nb_faces)
            grid->phase_face->size = nb_faces;

        for(i=0; i<nb_pts; i++){
            pt = (Point2D){x_v[i], y_v[i]};
            set_ith_elem_vec_pts2D(vertices, i, &pt);
            set_ith_elem_vec_int(status_edge, i, &zero);
        }
        zero = 2;
        for(i=0; i<nb_faces; i++){
            set_ith_elem_vec_int(phase_face, i, &zero);
        }

        for(i=0; i<(nb_pts-1); i++){
            if (pt_indices[i] < pt_indices[i+1]){
                infogrb = GrB_Matrix_setElement(*edges, -1, i, i);
                infogrb = GrB_Matrix_setElement(*edges, 1, i+1, i);
                infogrb = GrB_Matrix_setElement(*faces, 1, i, 0);
            } else {
                infogrb = GrB_Matrix_setElement(*edges, 1, i, i);
                infogrb = GrB_Matrix_setElement(*edges, -1, i+1, i);
                infogrb = GrB_Matrix_setElement(*faces, -1, i, 0);
            }
        }
        i = nb_pts-1;
        if (pt_indices[i] < pt_indices[0]){
            infogrb = GrB_Matrix_setElement(*edges, -1, i, i);
            infogrb = GrB_Matrix_setElement(*edges, 1, 0, i);
            infogrb = GrB_Matrix_setElement(*faces, 1, i, 0);
        } else {
            infogrb = GrB_Matrix_setElement(*edges, 1, i, i);
            infogrb = GrB_Matrix_setElement(*edges, -1, 0, i);
            infogrb = GrB_Matrix_setElement(*faces, -1, i, 0);
        }
    }
}

/// @brief Take consecutive points to build a polygon.
/// @param x_v array of x-coordinates of size signed_nb_pts
/// @param y_v array of y-coordinates of size signed_nb_pts 
/// @param signed_nb_pts size of the arrays (and number of points).
void build_clipped_from_pts_fortran_(const my_real_c* x_v, const my_real_c* y_v, const long long* limits, const long long *nb_polygons){
    unsigned long nb_pts;
    int64_t i;
    long long limit1, limit2;
    const my_real_c *begin_x, *begin_y;
    Polygon2D* copy_clipped = new_Polygon2D();
    
    if ((*nb_polygons)>0){
        nb_pts = (unsigned long) limits[1];
        clipped = polygon_from_consecutive_points(x_v, y_v, nb_pts);
        for(i=1; i<(*nb_polygons); i++){
            limit1 = limits[i];
            limit2 = limits[i+1]-1;
            nb_pts = (unsigned long)(limit2-limit1+1);
            begin_x = x_v + limit1;
            begin_y = y_v + limit1;
            copy_Polygon2D(clipped, copy_clipped);
            dealloc_Polygon2D(clipped); free(clipped);
            clipped = fuse_polygons(copy_clipped, polygon_from_consecutive_points(begin_x, begin_y, nb_pts));
        }
    } else {
        clipped = new_Polygon2D();
    }
    
    dealloc_Polygon2D(copy_clipped); free(copy_clipped);
}

/// @brief Compute cell occupancy of each phase.
/// @param 
void compute_lambdas2d_fortran_(const my_real_c *dt, \
                        my_real_c *ptr_lambdas_arr,   \
                        my_real_c *ptr_big_lambda_n,  \
                        my_real_c *ptr_big_lambda_np1,\
                        my_real_c *normals_x, my_real_c *normals_y, my_real_c *normals_t, \
                        long long* edge_indices, long long* max_length_array, long long* nb_normals, \
                        long long int *is_narrowband_ptr)
{
    Array_double *lambdas;
    Vector_double *Lambda_n;
    Vector_double *Lambda_np1;
    bool is_narrowband;
    Vector_int64 *edge_vector = NULL;
    Vector_points3D* normals = NULL;    
    uint64_t i, max_i;
 
    compute_lambdas2D(grid, clipped3D, *dt, &lambdas, &Lambda_n, &Lambda_np1, &normals, &edge_vector, &is_narrowband);
    max_i = normals->size < *max_length_array ? normals->size : *max_length_array;
    for(i=0; i<max_i; i++){
        normals_x[i] = normals->points[i].x;
        normals_y[i] = normals->points[i].y;
        normals_t[i] = normals->points[i].t;
    }
    *nb_normals = (long long) normals->size;
    max_i = edge_vector->size < *max_length_array ? edge_vector->size : *max_length_array;
    for(i=0; i<max_i; i++){
        edge_indices[i] = edge_vector->data[i];
    }
    if (is_narrowband) {
        *is_narrowband_ptr = 1;
    } else {
        *is_narrowband_ptr = -1;
    }

    memcpy(ptr_lambdas_arr, lambdas->data, lambdas->ncols*lambdas->nrows*sizeof(my_real_c));

    memcpy(ptr_big_lambda_n, Lambda_n->data, Lambda_n->size*sizeof(my_real_c));
    memcpy(ptr_big_lambda_np1, Lambda_np1->data, Lambda_np1->size*sizeof(my_real_c));

    dealloc_arr_double(lambdas); free(lambdas);
    dealloc_vec_double(Lambda_n); free(Lambda_n);
    dealloc_vec_double(Lambda_np1); free(Lambda_np1);
}



void nb_pts_clipped_fortran_(long long int* signed_nb_pts_solid){
    *signed_nb_pts_solid = ((long long) clipped->vertices->size);
}

void nb_edge_clipped_fortran_(long long int* signed_nb_edges_solid){
    unsigned long nb;
    GrB_Matrix_ncols(&nb, *(clipped->edges));
    *signed_nb_edges_solid = ((long long) nb);
}

void nb_face_clipped_fortran_(long long int* signed_nb_faces_solid){
    unsigned long nb;
    GrB_Matrix_ncols(&nb, *(clipped->faces));
    *signed_nb_faces_solid = ((long long) nb);
}

/// \brief Compute normals to the the polygonal interface.
void compute_normals_clipped_fortran_(my_real_c* normalVecx, my_real_c* normalVecy, \
                                        my_real_c* min_pos_Se){

    Vector_points2D* normals_pts;
    Vector_points2D* normals_edges;
    GrB_Index nb_pts, nb_edges;
    uint64_t i;
    my_real_c norm;

    GrB_Matrix_nrows(&nb_pts, *(clipped->edges));
    GrB_Matrix_ncols(&nb_edges, *(clipped->edges));
    normals_pts = alloc_with_capacity_vec_pts2D(nb_pts);
    normals_pts->size = nb_pts;
    normals_edges = alloc_with_capacity_vec_pts2D(nb_edges);
    normals_edges->size = nb_edges;
    compute_all_normals2D(clipped, normals_pts, normals_edges, min_pos_Se);


    for(i=0; i<normals_pts->size; i++){
        norm = sqrt(normals_pts->points[i].x*normals_pts->points[i].x + normals_pts->points[i].y*normals_pts->points[i].y);
        normalVecx[i] = -normals_pts->points[i].x/norm;
        normalVecy[i] = -normals_pts->points[i].y/norm;
    }

    dealloc_vec_pts2D(normals_pts); free(normals_pts);
    dealloc_vec_pts2D(normals_edges); free(normals_edges);
}

/// @brief Apply a laplacian filter to the velocity to stabilize polygonal displacement.
/// @param vec_move_clippedx [IN] Unsmoothed velocity / [OUT] Smoothed velocity
/// @param vec_move_clippedy [IN] Unsmoothed velocity / [OUT] Smoothed velocity
/// @param min_pos_Se minimal edge length
/// @param dt time step
void smooth_vel_clipped_fortran_(my_real_c* vec_move_clippedx, my_real_c* vec_move_clippedy, my_real_c* min_pos_Se, my_real_c *dt){
    my_real_c eps;
    GrB_Matrix smoothing_op, id;
    unsigned long nb_pts = clipped->vertices->size;
    //GrB_Index ind_vector[2] = {0, nb_pts-1};
    GrB_Info infogrb;
    GrB_Index i;
    
    eps = 0.001 / ((*min_pos_Se)*(*min_pos_Se));
    GrB_Matrix_new(&smoothing_op, GrB_FP64, nb_pts, nb_pts);
    GrB_Matrix_new(&id, GrB_FP64, nb_pts, nb_pts);

    //smoothing_op = Id + eps*dt*(edges*edges')
    CHECK_GB_CALL(infogrb, GrB_mxm(smoothing_op, GrB_NULL, GrB_NULL, GrB_PLUS_TIMES_SEMIRING_FP64, *(clipped->edges), *(clipped->edges), GrB_DESC_T1));
    for(i=0; i<nb_pts; i++){
        CHECK_GB_CALL(infogrb, GrB_Matrix_setElement(id, 1.0, i, i))
    }
    CHECK_GB_CALL(infogrb, GrB_apply(smoothing_op, GrB_NULL, GrB_NULL, GrB_TIMES_FP64, eps*(*dt), smoothing_op, GrB_NULL))
    CHECK_GB_CALL(infogrb, GrB_eWiseAdd(smoothing_op, GrB_NULL, GrB_NULL, GrB_PLUS_FP64, id, smoothing_op, GrB_NULL))

    solver_for_graphblas_FP_sym(smoothing_op, vec_move_clippedx);
    solver_for_graphblas_FP_sym(smoothing_op, vec_move_clippedy);

    GrB_free(&smoothing_op);
    GrB_free(&id);
}

void get_clipped_ith_vertex_fortran_(long long *signed_k, Point2D *pt){
    uint64_t k = (uint64_t)((*signed_k) - 1);
    *pt = *get_ith_elem_vec_pts2D(clipped->vertices, k); 
}

/// @brief Return the edges connected to point k.
/// @param k_signed Index of point
/// @param signed_eR First edge connected
/// @param signed_eL Second edge connected
void get_clipped_edges_ith_vertex_fortran_(long long *k_signed, long long *signed_eR, long long *signed_eL){
    uint64_t eR, eL;
    GrB_Matrix e_k;
    GrB_Vector nz_e_k, extr_vals_e_k, I_vec_e_k;
    GrB_Index size_nz_e_k;
    GrB_Index nb_pts;
    GrB_Info infogrb;
    GrB_Index k = (GrB_Index)(*k_signed - 1);

    nb_pts = clipped->vertices->size;

    infogrb = GrB_Matrix_new(&e_k, GrB_INT8, 1, nb_pts);
    infogrb = GrB_Vector_new(&nz_e_k, GrB_UINT64, 2);
    infogrb = GrB_Vector_new(&extr_vals_e_k, GrB_INT8, 2);
    infogrb = GrB_Vector_new(&I_vec_e_k, GrB_UINT64, 2);
    GrB_extract(e_k, GrB_NULL, GrB_NULL, *(clipped->edges), &k, 1, GrB_ALL, 1, GrB_NULL); //Get indices of edges connected with point k
    GxB_Matrix_extractTuples_Vector(I_vec_e_k, nz_e_k, extr_vals_e_k, e_k, GrB_NULL);
    GrB_Vector_size(&size_nz_e_k, nz_e_k);
    if (size_nz_e_k == 2){ //Exactly two edges connected to the point k
        GrB_Vector_extractElement(&eR, nz_e_k, 0);
        GrB_Vector_extractElement(&eL, nz_e_k, 1);
        *signed_eR = (long long)eR;
        *signed_eL = (long long)eL;
    } else {                   
        *signed_eR = -1;       
        *signed_eL = -1;       
    }                          
    GrB_free(&e_k);            
    GrB_free(&nz_e_k);         
    GrB_free(&extr_vals_e_k);  
    GrB_free(&I_vec_e_k);      
}                              
                     
/// @brief Advects the polygonal interface, solves possible intersection, modify edge length.
/// @param vec_move_clippedy 
/// @param vec_move_clippedz 
/// @param dt 
/// @param new_nb_edges New number of edges after the update
/// @param minimal_length minimal length of edge of polygonal interface
/// @param maximal_length maximal length of edge of polygonal interface
/// @param minimal_angle minimal angle formed by two edges.
void update_clipped_fortran_(const my_real_c* vec_move_clippedy, const my_real_c* vec_move_clippedz, const my_real_c* dt, \
                            long long* new_nb_edges, \
                            my_real_c *minimal_length, my_real_c *maximal_length, my_real_c *minimal_angle){

    uint64_t i;
    int64_t val; 

    update_solid(&clipped, &clipped3D, vec_move_clippedy, vec_move_clippedz, *dt, \
                    *minimal_length, *maximal_length, *minimal_angle);
    *new_nb_edges = 0;
    //Search for max in clipped3D->status_edge => maximal number of edges used in the polygonal interface
    if (clipped3D){
        if(clipped3D->status_face->size > 0){
            for(i=0; i<clipped3D->status_face->size; i++){
                val = *get_ith_elem_vec_int(clipped3D->status_face, i);
                val = -(val+2) + 1;
                if (val > *new_nb_edges){
                    *new_nb_edges = val;
                }
            }
        }
    }
}                              

void output_clipped_fortran_(my_real_c* x_v_clipped, my_real_c* y_v_clipped, long long* limits_polygons){ 
    uint64_t i, j_f, j, k, start, next, end_limits, pt_ind;
    int8_t val, orient;
    GrB_Index nb_edges, nb_faces, size_edge_indices, size_pt_indices, nb_pts, nvals;
    GrB_Index curr_edge;
    GrB_Info infogrb;
    Vector_uint* ind_kept_pts;
    GrB_Vector fj, extr_vals_fj, edge_indices;
    GrB_Vector ej, extr_vals_ej, pt_indices;
    GrB_Matrix e_k;
    GrB_Vector pot_ind_edges, I_vec_e_k, extr_vals_e_k;
    long long curr_limit;

    GrB_Matrix_ncols(&nb_faces, *(clipped->faces));
    GrB_Matrix_ncols(&nb_edges, *(clipped->edges));
    GrB_Matrix_nrows(&nb_pts, *(clipped->edges));
    
    ind_kept_pts = alloc_empty_vec_uint();
    GrB_Vector_new(&fj, GrB_INT8, nb_edges);
    GrB_Vector_new(&edge_indices, GrB_UINT64, nb_edges);
    GrB_Vector_new(&extr_vals_fj, GrB_INT8, nb_edges);
    GrB_Vector_new(&ej, GrB_INT8, nb_pts);
    GrB_Vector_new(&pt_indices, GrB_UINT64, nb_pts);
    GrB_Vector_new(&extr_vals_ej, GrB_INT8, nb_pts);
    GrB_Matrix_new(&e_k, GrB_INT8, 1, nb_edges);
    GrB_Vector_new(&pot_ind_edges, GrB_UINT64, 1);
    GrB_Vector_new(&I_vec_e_k, GrB_UINT64, 1);
    GrB_Vector_new(&extr_vals_e_k, GrB_INT8, 1);

    limits_polygons[0] = 0;
    end_limits = 0;

    for (i=0; i<nb_faces; i++){
        infogrb = GrB_extract(fj, GrB_NULL, GrB_NULL, *(clipped->faces), GrB_ALL, 1, i, GrB_NULL); //Get indices of edges composing face i
        infogrb = GxB_Vector_extractTuples_Vector(edge_indices, extr_vals_fj, fj, GrB_NULL);
        infogrb = GrB_Vector_size(&size_edge_indices, edge_indices);
        ind_kept_pts->size = 0;

        if (size_edge_indices > 0) {
            size_pt_indices = 0;
            curr_edge = 0;
            while ((size_pt_indices == 0) && (curr_edge < size_edge_indices)) {
                infogrb = GrB_Vector_extractElement(&j, edge_indices, curr_edge);
                infogrb = GrB_Vector_extractElement(&orient, extr_vals_fj, curr_edge);
                infogrb = GrB_extract(ej, GrB_NULL, GrB_NULL, *(clipped->edges), GrB_ALL, 1, j, GrB_NULL); //Get indices of points composing edge j
                infogrb = GxB_Vector_extractTuples_Vector(pt_indices, extr_vals_ej, ej, GrB_NULL);
                infogrb = GrB_Vector_size(&size_pt_indices, pt_indices);
                if(size_pt_indices>0){
                    infogrb = GrB_Vector_extractElement(&val, extr_vals_ej, 0);
                    if (orient*val<0){
                        infogrb = GrB_Vector_extractElement(&start, pt_indices, 0);
                        infogrb = GrB_Vector_extractElement(&next, pt_indices, 1);
                    } else {
                        infogrb = GrB_Vector_extractElement(&start, pt_indices, 1);
                        infogrb = GrB_Vector_extractElement(&next, pt_indices, 0);
                    }
                    push_back_vec_uint(&ind_kept_pts, &start);
                    push_back_vec_uint(&ind_kept_pts, &next);
                } else {
                    curr_edge++;
                }
            }

            pt_ind = next;
            while (pt_ind != start){
                GrB_extract(e_k, GrB_NULL, GrB_NULL, *(clipped->edges), &pt_ind, 1, GrB_ALL, 1, GrB_NULL); //Extract edges connected to point pt_ind
                GrB_Matrix_nvals(&nvals, e_k);
                GrB_Vector_resize(pot_ind_edges, nvals);
                GrB_Vector_resize(I_vec_e_k, nvals);
                GrB_Vector_resize(extr_vals_e_k, nvals);
                GxB_Matrix_extractTuples_Vector(I_vec_e_k, pot_ind_edges, extr_vals_e_k, e_k, GrB_NULL);

                infogrb = GrB_Vector_extractElement(&j, pot_ind_edges, 0); //Get first edge connected to pt_ind
                if (j == curr_edge){ //Make sure we do not go back
                    infogrb = GrB_Vector_extractElement(&j, pot_ind_edges, 1);
                }  

                curr_edge = j;
                infogrb = GrB_Matrix_extractElement(&orient, *(clipped->faces), j, i);
                infogrb = GrB_extract(ej, GrB_NULL, GrB_NULL, *(clipped->edges), GrB_ALL, 1, j, GrB_NULL); //Get indices of points composing edge j
                infogrb = GxB_Vector_extractTuples_Vector(pt_indices, extr_vals_ej, ej, GrB_NULL);
                infogrb = GrB_Vector_extractElement(&val, extr_vals_ej, 0);
                if (orient*val<0){//Edge oriented from pt_indices[0] to pt_indices[1], we need the end point
                    infogrb = GrB_Vector_extractElement(&pt_ind, pt_indices, 1);
                } else { //Edge oriented from pt_indices[1] to pt_indices[0], we need the end point
                    infogrb = GrB_Vector_extractElement(&pt_ind, pt_indices, 0);
                }
                if (pt_ind != start){ //Not yet back to the starting point
                    push_back_vec_uint(&ind_kept_pts, &pt_ind);
                }
            }

            //Rebuild list of vertices in face i only in correct order
            limits_polygons[end_limits + 1] = limits_polygons[end_limits] + ind_kept_pts->size;
            curr_limit = limits_polygons[end_limits];
            for (j_f=0; j_f<ind_kept_pts->size; j_f++){
                k = *get_ith_elem_vec_uint(ind_kept_pts, j_f);
                x_v_clipped[curr_limit + j_f] = get_ith_elem_vec_pts2D(clipped->vertices, k)->x;
                y_v_clipped[curr_limit+ j_f] = get_ith_elem_vec_pts2D(clipped->vertices, k)->y;
            }
            end_limits++;
        }
    }
    
    dealloc_vec_uint(ind_kept_pts); free(ind_kept_pts);
    GrB_free(&fj);
    GrB_free(&edge_indices);
    GrB_free(&extr_vals_fj);
    GrB_free(&ej);
    GrB_free(&pt_indices);
    GrB_free(&extr_vals_ej);
    GrB_free(&e_k);
    GrB_free(&pot_ind_edges);
    GrB_free(&I_vec_e_k);
    GrB_free(&extr_vals_e_k);
}

/// @brief Print in vtk format the polygonal interface
/// @param i_print 
void print_clipped_fortran_(long long* i_print){
    char filename[1024];
    int length_i = snprintf(NULL, 0,"%lld", *i_print);
    char* i_string = (char*)calloc(length_i, sizeof(char));

    sprintf(i_string, "%lld", *i_print);
    strcpy(filename, "Polygon");
    strcat(filename, i_string);
    strcat(filename, ".vtk");

    if(clipped){
        print_polygon2D(clipped, filename);
    }

    free(i_string);
}

