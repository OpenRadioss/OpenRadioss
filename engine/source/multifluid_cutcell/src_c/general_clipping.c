#define CHECK_GB_CALL(infogrb, call)                                                  \
infogrb = (call);                                                                     \
if (infogrb != GrB_SUCCESS){                                                          \
    printf("Warning: GrB failure at line %d, %s\n", __LINE__, __FILE_NAME__);         \
}                                                                                     \

#include "general_clipping.h"
#include "my_real_c.inc"
#include <math.h>

/// @brief Cut all edges in p in a clipping algorithm according to a line supported by point "pt" and normal vector "normal".
/// @param new_edges_in Result of clipping algorithm with cut edges
void cut_edges3D(const Polyhedron3D* p, const Point3D* normal, const Point3D* pt, int8_t sign_taken, Vector_points3D* pts_copy, GrB_Matrix* new_edges_in){
    GrB_Index i;
    Point3D *v, *x1, *x2, newPoint;
    GrB_Info infogrb;
    GrB_Vector pts_in;
    GrB_Matrix pts_in_m_ones, edges_in, addPoints;
    GrB_Vector broken_edges, temp_v;
    GrB_Vector nz_be, vals_be;
    //GrB_Index I_index[2];
    //int8_t extr_vals[2];
    GrB_Vector I_index, extr_vals;
    int8_t val_be_i;
    const uint64_t size = p->vertices->size;
    GrB_Index size_e, size_v, ind_2;
    unsigned int nb_cut_edges, countPoint;
    int64_t be_i;
    my_real_c* distances = (my_real_c*)malloc(size*sizeof(my_real_c));
    bool dist_i;
    GrB_Index i_x1, i_x2, size_Iindex;

    copy_vec_pts3D(p->vertices, pts_copy);

    //Compute distances of points to separation hyperplane
    for (i=0; i<size; i++){
        v = get_ith_elem_vec_pts3D(p->vertices, i);
        distances[i] = compute_distance3D(*v, *normal, *pt);
    }

    //Mark points inside the domain
    CHECK_GB_CALL(infogrb, GrB_Vector_new(&pts_in, GrB_INT8, size));
    if (sign_taken>0){
        for (i=0; i<size; i++){
            dist_i = distances[i]>=0.0;
            if (dist_i)
                CHECK_GB_CALL(infogrb, GrB_Vector_setElement(pts_in, 1, i));
        }
    }
    else{
        for (i=0; i<size; i++){
            dist_i = distances[i]<0.0;
            if (dist_i)
                CHECK_GB_CALL(infogrb, GrB_Vector_setElement(pts_in, 1, i));
        }
    }

    //Compute edges inside the domain.
    //edges_in = p.edges .* (pts_in * ones(1, size(p.edges, 2)))
    CHECK_GB_CALL(infogrb, GrB_Matrix_ncols(&size_e, *(p->edges)));
    CHECK_GB_CALL(infogrb, GrB_Matrix_new(&edges_in, GrB_INT8, size, size_e));
    CHECK_GB_CALL(infogrb, GrB_Vector_size(&size_v, pts_in));
    CHECK_GB_CALL(infogrb, GrB_Matrix_new(&pts_in_m_ones, GrB_INT8, size_v, size_e)); //Eventually, it will be equal to pts_in' * (row vector of ones)
    for (i = 0; i<size_e; i++){
        CHECK_GB_CALL(infogrb, GrB_Col_assign(pts_in_m_ones, GrB_NULL, GrB_NULL, pts_in, GrB_ALL, 0, i, GrB_NULL));
    }
    CHECK_GB_CALL(infogrb, GrB_eWiseMult(edges_in, GrB_NULL, GrB_NULL, GrB_TIMES_INT8, *(p->edges), pts_in_m_ones, GrB_NULL));

    //Detect broken edges
    CHECK_GB_CALL(infogrb, GrB_Vector_new(&broken_edges, GrB_INT64, size_e));
    CHECK_GB_CALL(infogrb, GrB_Vector_new(&nz_be, GrB_INT64, size_e));
    CHECK_GB_CALL(infogrb, GrB_Vector_new(&vals_be, GrB_INT64, size_e));
    CHECK_GB_CALL(infogrb, GrB_Vector_new(&temp_v, GrB_INT8, size_e));
    CHECK_GB_CALL(infogrb, GrB_Vector_new(&I_index, GrB_UINT64, size));
    CHECK_GB_CALL(infogrb, GrB_Vector_new(&extr_vals, GrB_INT8, size));
    CHECK_GB_CALL(infogrb, GrB_reduce(broken_edges, GrB_NULL, GrB_NULL, GrB_PLUS_MONOID_INT8, edges_in, GrB_DESC_T0)); //sum along the columns
    CHECK_GB_CALL(infogrb, GrB_assign(broken_edges, broken_edges, GrB_NULL, broken_edges, GrB_ALL, 0, GrB_DESC_R)); //suppress all zeros
    CHECK_GB_CALL(infogrb, GrB_apply(temp_v, GrB_NULL, GrB_NULL, GrB_ABS_INT8, broken_edges, GrB_NULL)); //abs on each component
    CHECK_GB_CALL(infogrb, GrB_reduce(&nb_cut_edges, GrB_NULL, GrB_PLUS_MONOID_INT8, temp_v, GrB_NULL));

    //Add points and complete the edges
    CHECK_GB_CALL(infogrb, GrB_Matrix_ncols(&size_e, edges_in));
    if (nb_cut_edges>0){
        CHECK_GB_CALL(infogrb, GrB_Matrix_new(&addPoints, GrB_INT8, nb_cut_edges, size_e));
        countPoint = 0;
        CHECK_GB_CALL(infogrb, GxB_Vector_extractTuples_Vector(nz_be, vals_be, broken_edges, GrB_NULL));
        CHECK_GB_CALL(infogrb, GrB_Vector_size(&size_v, nz_be));
    
        CHECK_GB_CALL(infogrb, GrB_Vector_resize(temp_v, size));
        for (i=0; i<size_v; i++){
            CHECK_GB_CALL(infogrb, GrB_Vector_extractElement(&be_i, nz_be, i));
            CHECK_GB_CALL(infogrb, GrB_Vector_extractElement(&val_be_i, vals_be, i));
            ind_2 = 2;
            CHECK_GB_CALL(infogrb, GrB_extract(temp_v, GrB_NULL, GrB_NULL, *(p->edges), GrB_ALL, 1, be_i, GrB_NULL));
            CHECK_GB_CALL(infogrb, GxB_Vector_extractTuples_Vector(I_index, extr_vals, temp_v, GrB_NULL));
            CHECK_GB_CALL(infogrb, GrB_Vector_size(&size_Iindex, I_index));

            if (size_Iindex>1){ //Check that the vector is not empty
                CHECK_GB_CALL(infogrb, GrB_Vector_extractElement(&i_x1, I_index, 0));
                CHECK_GB_CALL(infogrb, GrB_Vector_extractElement(&i_x2, I_index, 1));
                x1 = get_ith_elem_vec_pts3D(p->vertices, i_x1);
                x2 = get_ith_elem_vec_pts3D(p->vertices, i_x2);

                newPoint = find_intersection3D(*x1, distances[i_x1], *x2, distances[i_x2]);
                //printf("New point = (%lf, %lf, %lf), at distance %lf from point (%lf, %lf, %lf) and distance %lf from point (%lf, %lf, %lf)\n",
                //         be_i, newPoint.x, newPoint.y, newPoint.t, distances[i_x1], x1->x, x1->y, x1->t, distances[i_x2], x2->x, x2->y, x2->t);

                push_back_vec_pts3D(&pts_copy, &newPoint);
                CHECK_GB_CALL(infogrb, GrB_Matrix_setElement(addPoints, -val_be_i, countPoint, be_i));
                countPoint += 1;
            }
            
        }
        CHECK_GB_CALL(infogrb, GrB_Matrix_new(new_edges_in, GrB_INT8, size + nb_cut_edges, size_e));
        CHECK_GB_CALL(infogrb, GxB_Matrix_concat(*new_edges_in, (GrB_Matrix[]){edges_in, addPoints}, 2, 1, GrB_NULL));
    } else {
        CHECK_GB_CALL(infogrb, GrB_Matrix_new(new_edges_in, GrB_INT8, size, size_e));
        CHECK_GB_CALL(infogrb, GrB_Matrix_dup(new_edges_in, edges_in));
    }

    free(distances);
    GrB_free(&pts_in);
    GrB_free(&pts_in_m_ones);
    GrB_free(&edges_in);
    GrB_free(&broken_edges);
    GrB_free(&temp_v);
    GrB_free(&nz_be);
    GrB_free(&vals_be);
}

/// @brief Close cells after they have been cut
void close_cells(GrB_Matrix *cells_in, const GrB_Matrix *supercells, Vector_int *status_cell, long int mark_cells, GrB_Matrix *supercells_in){
        GrB_Info infogrb;
        GrB_Matrix temp_tab, in_m_ones;
        GrB_Matrix new_cells_in;
        //GrB_Matrix ones; 
        GrB_Matrix subcells_supercells_in, abs_subcells_supercells_in;
        GrB_Matrix addCells, newCells;
        GrB_Vector temp_v, clipped_in_cells, sum_abs, open_supercells_in;
        GrB_Vector nz_osi, vals_osi;
        GrB_Index i, size_e, size_v;
        my_real_c val = 0;
        //GrB_Index *J_range, *I_range;
        uint64_t nb_open_supercells, j;
        //GrB_Scalar one;
        uint64_t osi_j;

        CHECK_GB_CALL(infogrb, GrB_Matrix_nrows(&size_v, *cells_in));
        CHECK_GB_CALL(infogrb, GrB_Matrix_ncols(&size_e, *cells_in));
        CHECK_GB_CALL(infogrb, GrB_Matrix_new(&new_cells_in, GrB_INT8, size_v, size_e));
        CHECK_GB_CALL(infogrb, GrB_Matrix_dup(&new_cells_in, *cells_in));

        //Remaining cells
        CHECK_GB_CALL(infogrb, GrB_Matrix_new(&temp_tab, GrB_INT8, size_v, size_e));
        CHECK_GB_CALL(infogrb, GrB_Vector_new(&temp_v, GrB_INT64, size_e));
        CHECK_GB_CALL(infogrb, GrB_Vector_new(&clipped_in_cells, GrB_INT8, size_e));
        CHECK_GB_CALL(infogrb, GrB_apply(temp_tab, GrB_NULL, GrB_NULL, GrB_ABS_INT8, new_cells_in, GrB_NULL)); //abs on each component
        CHECK_GB_CALL(infogrb, GrB_reduce(temp_v, GrB_NULL, GrB_NULL, GrB_PLUS_INT64, temp_tab, GrB_DESC_T0)); //Sum along columns
        CHECK_GB_CALL(infogrb, GrB_apply(clipped_in_cells, GrB_NULL, GrB_NULL, GrB_GT_INT64, temp_v, 0, GrB_NULL)); //componentwise greater than 0.
        //GrB_wait(clipped_in_cells, GrB_MATERIALIZE);
        //GxB_print(clipped_in_cells, GxB_COMPLETE);
        
        //Get only clipped in cells
        CHECK_GB_CALL(infogrb, GrB_Matrix_ncols(&size_e, *supercells));
        CHECK_GB_CALL(infogrb, GrB_Matrix_nrows(&size_v, *supercells));
        
        CHECK_GB_CALL(infogrb, GrB_Matrix_new(&in_m_ones, GrB_INT8, size_v, size_e)); //Eventually, it will be equal to clipped_in_cells' * (row vector of ones)
        for (i = 0; i<size_e; i++){
            CHECK_GB_CALL(infogrb, GrB_Col_assign(in_m_ones, GrB_NULL, GrB_NULL, clipped_in_cells, GrB_ALL, 0, i, GrB_NULL));
        }

        //infogrb = GrB_Matrix_new(supercells_in, GrB_INT8, size_v, size_e);
        CHECK_GB_CALL(infogrb, GrB_Matrix_resize(temp_tab, size_v, size_e));
        CHECK_GB_CALL(infogrb, GrB_eWiseMult(temp_tab, GrB_NULL, GrB_NULL, GrB_TIMES_INT8, *supercells, in_m_ones, GrB_NULL)); //In temp_tab : keep only cells that are inside the clipping.
        //GrB_set (temp_tab, GrB_ROWMAJOR, GrB_STORAGE_ORIENTATION_HINT) ;
        //GrB_wait(temp_tab, GrB_MATERIALIZE);
        //GxB_print(temp_tab, GxB_COMPLETE);

        //Now, some cells are broken. This, we add cells to close supercells
        CHECK_GB_CALL(infogrb, GrB_Matrix_nrows(&size_v, new_cells_in));
        CHECK_GB_CALL(infogrb, GrB_Matrix_ncols(&size_e, temp_tab));
        CHECK_GB_CALL(infogrb, GrB_Matrix_new(&subcells_supercells_in, GrB_INT8, size_v, size_e));
        CHECK_GB_CALL(infogrb, GrB_Matrix_new(&abs_subcells_supercells_in, GrB_INT8, size_v, size_e));

        CHECK_GB_CALL(infogrb, GrB_mxm(subcells_supercells_in, GrB_NULL, GrB_NULL, GrB_PLUS_TIMES_SEMIRING_INT8, new_cells_in, temp_tab, GrB_NULL)); //new_cells_in x temp_tab
        CHECK_GB_CALL(infogrb, GrB_assign(subcells_supercells_in, subcells_supercells_in, GrB_NULL, subcells_supercells_in, GrB_ALL, 0, GrB_ALL, 0, GrB_DESC_R)); //suppress all zeros
        CHECK_GB_CALL(infogrb, GrB_apply(abs_subcells_supercells_in, GrB_NULL, GrB_NULL, GrB_ABS_INT8, subcells_supercells_in, GrB_NULL)); //abs(subcells_supercells_in)
        //GrB_set (subcells_supercells_in, GrB_COLMAJOR, GrB_STORAGE_ORIENTATION_HINT) ;
        //GrB_wait(subcells_supercells_in, GrB_MATERIALIZE);
        //GxB_print(subcells_supercells_in, GxB_COMPLETE);
        //GrB_set (abs_subcells_supercells_in, GrB_COLMAJOR, GrB_STORAGE_ORIENTATION_HINT) ;
        //GrB_wait(abs_subcells_supercells_in, GrB_MATERIALIZE);
        //GxB_print(abs_subcells_supercells_in, GxB_COMPLETE);

        CHECK_GB_CALL(infogrb, GrB_Vector_new(&sum_abs, GrB_INT64, size_e));
        CHECK_GB_CALL(infogrb, GrB_Vector_new(&open_supercells_in, GrB_UINT64, size_e));
        CHECK_GB_CALL(infogrb, GrB_reduce(sum_abs, GrB_NULL, GrB_NULL, GrB_PLUS_INT64, abs_subcells_supercells_in, GrB_DESC_T0)); //sum along the columns
        CHECK_GB_CALL(infogrb, GrB_apply(open_supercells_in, GrB_NULL, GrB_NULL, GrB_GT_INT64, sum_abs, 0, GrB_NULL)); //keep only strictly positive
        CHECK_GB_CALL(infogrb, GrB_reduce(&nb_open_supercells, GrB_NULL, GrB_PLUS_MONOID_UINT64, open_supercells_in, GrB_NULL)); //sum 
        //GrB_wait(sum_abs, GrB_MATERIALIZE);
        //GxB_print(sum_abs, GxB_COMPLETE);
        //GrB_wait(open_supercells_in, GrB_MATERIALIZE);
        //GxB_print(open_supercells_in, GxB_COMPLETE);

        if (nb_open_supercells>0){
            CHECK_GB_CALL(infogrb, GrB_Matrix_new(&addCells, GrB_INT8, nb_open_supercells, size_e));
            CHECK_GB_CALL(infogrb, GrB_Matrix_new(&newCells, GrB_INT8, size_v, nb_open_supercells));

            //CHECK_GB_CALL(infogrb, GrB_Vector_size(&size_e, open_supercells_in));
            CHECK_GB_CALL(infogrb, GrB_Vector_resize(temp_v, size_v));

            CHECK_GB_CALL(infogrb, GrB_Vector_new(&nz_osi, GrB_UINT64, size_e));
            CHECK_GB_CALL(infogrb, GrB_Vector_new(&vals_osi, GrB_UINT64, size_e));
            CHECK_GB_CALL(infogrb, GxB_Vector_extractTuples_Vector(nz_osi, vals_osi, open_supercells_in, GrB_NULL));
            CHECK_GB_CALL(infogrb, GrB_Vector_size(&size_e, nz_osi));
            for (j=0; j<size_e; j++){
                CHECK_GB_CALL(infogrb, GrB_Vector_extractElement(&osi_j, nz_osi, j));
                CHECK_GB_CALL(infogrb, GrB_Matrix_setElement(addCells, 1, j, osi_j));
                CHECK_GB_CALL(infogrb, GrB_extract(temp_v, GrB_NULL, GrB_NULL, subcells_supercells_in, GrB_ALL, 1, osi_j, GrB_NULL));
                CHECK_GB_CALL(infogrb, GrB_apply(temp_v, GrB_NULL, GrB_NULL, GrB_AINV_INT8, temp_v, GrB_DESC_R));
                CHECK_GB_CALL(infogrb, GrB_Col_assign(newCells, GrB_NULL, GrB_NULL, temp_v, GrB_ALL, 0, j, GrB_NULL)); //newCells[:,j] = -subcells_supercells_in[:,osi_j]
            }

            CHECK_GB_CALL(infogrb, GrB_Matrix_ncols(&size_e, new_cells_in));
            CHECK_GB_CALL(infogrb, GrB_Matrix_resize(*cells_in, size_v, size_e + nb_open_supercells));
            CHECK_GB_CALL(infogrb, GxB_Matrix_concat(*cells_in, (GrB_Matrix[]){new_cells_in, newCells}, 1, 2, GrB_NULL)); //cells_in = [cells_in  newCells]

            CHECK_GB_CALL(infogrb, GrB_Matrix_nrows(&size_v, temp_tab));
            CHECK_GB_CALL(infogrb, GrB_Matrix_ncols(&size_e, temp_tab));
            CHECK_GB_CALL(infogrb, GrB_Matrix_new(supercells_in, GrB_INT8,  size_v + nb_open_supercells, size_e));
            CHECK_GB_CALL(infogrb, GxB_Matrix_concat(*supercells_in, (GrB_Matrix[]){temp_tab, addCells}, 2, 1, GrB_NULL)); //supercells_in = [supercells_in ; addCells]

            GrB_free(&addCells);
            GrB_free(&newCells);
            GrB_free(&nz_osi);
            GrB_free(&vals_osi);
        } else {
            CHECK_GB_CALL(infogrb, GrB_Matrix_dup(supercells_in, *supercells));
        }

        if ((mark_cells > 0) && status_cell){
            for(i = 0; i<nb_open_supercells; i++){
                push_back_vec_int(&status_cell, &mark_cells); 
            }
        }

        //CHECK_GB_CALL(infogrb, GrB_Matrix_nrows(&size_v, *cells_in))
        //CHECK_GB_CALL(infogrb, GrB_Matrix_ncols(&size_e, *supercells_in))
        //CHECK_GB_CALL(infogrb, GrB_Matrix_resize(subcells_supercells_in, size_v, size_e))
        //CHECK_GB_CALL(infogrb, GrB_mxm(subcells_supercells_in, GrB_NULL, GrB_NULL, GrB_PLUS_TIMES_SEMIRING_INT8, *cells_in, *supercells_in, GrB_NULL));
        //CHECK_GB_CALL(infogrb, GrB_assign(subcells_supercells_in, subcells_supercells_in, GrB_NULL, subcells_supercells_in, GrB_ALL, 0, GrB_ALL, 0, GrB_DESC_R)); //suppress all zeros
        //GrB_wait(subcells_supercells_in, GrB_MATERIALIZE);
        //GxB_print(subcells_supercells_in, GxB_COMPLETE);

        //free(I_range);
        //free(J_range);
        GrB_free(&temp_tab);
        GrB_free(&in_m_ones);
        GrB_free(&new_cells_in);
        GrB_free(&subcells_supercells_in);
        GrB_free(&abs_subcells_supercells_in);
        GrB_free(&temp_v);
        GrB_free(&clipped_in_cells);
        GrB_free(&sum_abs);
        GrB_free(&open_supercells_in);
}

typedef struct {
    my_real_c* v;
    unsigned int n;
} form_struct;

//static form_struct new_form_struct(){
//    return (form_struct){NULL, 0};
//}
//    
//static form_struct new_form_struct_init(my_real_c* v, int n){
//    return (form_struct){v, n};
//}

//Compute 2-forms of two vectors p1 and p2, depending on ambient dimension
static form_struct twoform2D(Point2D p1, Point2D p2){
    return (form_struct){(my_real_c[]){p1.x*p2.y - p2.x*p1.y}, 1};
}

static form_struct twoform3D(Point3D p1, Point3D p2){
    return (form_struct){(my_real_c[]){p1.y*p2.t - p2.y*p1.t, p1.t*p2.x - p2.t*p1.x, p1.x*p2.y - p2.x*p1.y}, 3};
}

static form_struct twoform4D(Point4D p1, Point4D p2){
    my_real_c vals[] = {p1.x*p2.y - p2.y*p1.x, 
                     p1.x*p2.z - p2.z*p1.x, 
                     p1.x*p2.t - p2.x*p1.t,
                     p1.y*p2.z - p2.y*p1.z, 
                     p1.y*p2.t - p2.y*p1.t, 
                     p1.z*p2.t - p2.z*p1.t};
    return (form_struct){vals, 6};
}

//Compute 3-forms of a 2-form S and a vector p, depending on ambient dimension
static form_struct threeform3D(Point3D p, form_struct S){
    if (S.n != 3)
        return (form_struct){(my_real_c[]){nan("")}, 0};
    else{
        my_real_c val[] = {S.v[0]*p.x + S.v[1]*p.y + S.v[2]*p.t};
        return (form_struct){val, 1};
    }
}

static form_struct threeform3D_vec(Point3D p, GrB_Vector S){
    GrB_Index size_S;    
    my_real_c Sv0, Sv1, Sv2;

    GrB_Vector_size(&size_S, S);
    if (size_S != 3)
        return (form_struct){(my_real_c[]){nan("")}, 0};
    else{
        GrB_Vector_extractElement(&Sv0, S, 0);
        GrB_Vector_extractElement(&Sv1, S, 1);
        GrB_Vector_extractElement(&Sv2, S, 2);
        my_real_c val[] = {Sv0*p.x + Sv1*p.y + Sv2*p.t};
        return (form_struct){val, 1};
    }
}

static form_struct threeform4D(Point4D p, form_struct S){
    if (S.n!=6)
        return (form_struct){(my_real_c[]){nan("")}, 0};
    else{
        my_real_c vals[] = {p.y*S.v[6] - p.z*S.v[5] + p.t*S.v[4], 
                        -p.x*S.v[6] + p.z*S.v[3] - p.t*S.v[2], 
                         p.x*S.v[5] - p.y*S.v[3] + p.t*S.v[1], 
                         -p.x*S.v[4] + p.y*S.v[2] - p.z*S.v[1]};
        return (form_struct){vals, 4};
    }
}

//Compute 4-forms of a 3-form V and a vector p, depending on ambient dimension
static form_struct fourform4D(Point4D p, form_struct V){
    if (V.n != 4)
        return (form_struct){(my_real_c[]){nan("")}, 0};
    else{
        my_real_c val[] = {V.v[0]*p.x + V.v[1]*p.y + V.v[2]*p.z + V.v[3]*p.t};
        return (form_struct){val, 1};
    }
}

//Turn a vector of 2D points into a GrB_Matrix
static GrB_Matrix vector_pt2D_to_matrix(const Vector_points2D* vertices){
    GrB_Matrix mat_vert;
    unsigned int i;
    Point2D *pt;

    GrB_Matrix_new(&mat_vert, GrB_FP64, 2, vertices->size);
    for (i=0; i<vertices->size; i++){
        pt = get_ith_elem_vec_pts2D(vertices, i);
        GrB_Matrix_setElement(mat_vert, pt->x, 0, i);
        GrB_Matrix_setElement(mat_vert, pt->y, 1, i);
    }

    return mat_vert;
}

//Turn a vector of 3D points into a GrB_Matrix
static GrB_Matrix vector_pt3D_to_matrix(const Vector_points3D* vertices){
    GrB_Matrix mat_vert;
    unsigned int i;
    Point3D *pt;

    GrB_Matrix_new(&mat_vert, GrB_FP64, 3, vertices->size);
    for (i=0; i<vertices->size; i++){
        pt = get_ith_elem_vec_pts3D(vertices, i);
        //GrB_Col_assign(mat_vert, GrB_NULL, GrB_NULL, (my_real_c[]){pt->x, pt->y, pt->t}, GrB_ALL, 3, i, GrB_NULL);
        GrB_Matrix_setElement(mat_vert, pt->x, 0, i);
        GrB_Matrix_setElement(mat_vert, pt->y, 1, i);
        GrB_Matrix_setElement(mat_vert, pt->t, 2, i);
    }

    return mat_vert;
}

//Turn a GrB_Matrix into a vector of 2D points 
Vector_points2D* points2D_from_matrix(const GrB_Matrix m_pts){
    GrB_Index d, ncols, j;
    GrB_Vector m_pt;
    Vector_points2D *res;
    Point2D pt;

    GrB_Matrix_nrows(&d, m_pts);
    GrB_Matrix_ncols(&ncols, m_pts);
    if (d==2) {
        res = alloc_with_capacity_vec_pts2D(ncols);
        GrB_Vector_new(&m_pt, GrB_FP64, d);
        for(j=0; j<ncols; j++){
            GrB_Col_extract(m_pt, GrB_NULL, GrB_NULL, m_pts, GrB_ALL, 0, j, GrB_NULL);
            GrB_Vector_extractElement(&(pt.x), m_pt, 0);
            GrB_Vector_extractElement(&(pt.y), m_pt, 1);
            set_ith_elem_vec_pts2D(res, j, &pt);
        }
    } else {
        res = NULL;
    }

    GrB_free(&m_pt);

    return res;
}

//Turn a GrB_Matrix into a vector of 3D points 
Vector_points3D* points3D_from_matrix(const GrB_Matrix m_pts){
    GrB_Index d, ncols, j;
    GrB_Vector m_pt;
    Vector_points3D *res;
    Point3D pt;

    GrB_Matrix_nrows(&d, m_pts);
    GrB_Matrix_ncols(&ncols, m_pts);
    if (d==3) {
        GrB_Vector_new(&m_pt, GrB_FP64, d);
        res = alloc_with_capacity_vec_pts3D(ncols);
        for(j=0; j<ncols; j++){
            pt = (Point3D){0.0,0.0,0.0};
            //GrB_Col_extract(m_pt, GrB_NULL, GrB_NULL, m_pts, GrB_ALL, 0, j, GrB_NULL);
            //GrB_Vector_extractElement(&(pt.x), m_pt, 0);
            //GrB_Vector_extractElement(&(pt.y), m_pt, 1);
            //GrB_Vector_extractElement(&(pt.t), m_pt, 2);
            GrB_Matrix_extractElement(&(pt.x), m_pts, 0, j);
            GrB_Matrix_extractElement(&(pt.y), m_pts, 1, j);
            GrB_Matrix_extractElement(&(pt.t), m_pts, 2, j);
            set_ith_elem_vec_pts3D(res, j, &pt);
        }
    } else {
        res = NULL;
    }

    GrB_free(&m_pt);

    return res;
}

Vector_points4D* points4D_from_matrix(const GrB_Matrix m_pts){
    GrB_Index d, ncols, j;
    GrB_Vector m_pt;
    Vector_points4D *res;
    Point4D pt;

    GrB_Matrix_nrows(&d, m_pts);
    GrB_Matrix_ncols(&ncols, m_pts);
    if (d==3) {
        GrB_Vector_new(&m_pt, GrB_FP64, d);
        res = alloc_with_capacity_vec_pts4D(ncols);
        for(j=0; j<ncols; j++){
            GrB_Col_extract(m_pt, GrB_NULL, GrB_NULL, m_pts, GrB_ALL, 0, j, GrB_NULL);
            GrB_Vector_extractElement(&(pt.x), m_pt, 0);
            GrB_Vector_extractElement(&(pt.y), m_pt, 1);
            GrB_Vector_extractElement(&(pt.z), m_pt, 2);
            GrB_Vector_extractElement(&(pt.t), m_pt, 3);
            set_ith_elem_vec_pts4D(res, j, &pt);
        }
    } else {
        res = NULL;
    }

    GrB_free(&m_pt);

    return res;
}

//Compute length of all edges
static Vector_points2D* compute_length_edges2D(const GrB_Matrix* edges, const Vector_points2D* vertices){
    GrB_Matrix mat_vert = vector_pt2D_to_matrix(vertices);
    GrB_Matrix length_edges;
    Vector_points2D* le_as_pts;
    GrB_Index nbcols;

    GrB_Matrix_ncols(&nbcols, *edges);
    GrB_Matrix_new(&length_edges, GrB_FP64, 2, nbcols);
    GrB_mxm(length_edges, GrB_NULL, GrB_NULL, GrB_PLUS_TIMES_SEMIRING_FP64, mat_vert, *edges, GrB_NULL);
    le_as_pts = points2D_from_matrix(length_edges);

    GrB_free(&mat_vert);
    GrB_free(&length_edges);

    return le_as_pts;
}

//Compute the surface normals
static GrB_Vector surfaces_poly2D_pef(const Vector_points2D* points, const GrB_Matrix* edges, const GrB_Matrix* faces){
    //const unsigned int d=2;
    GrB_Index nf, j, e0, p0, p, size_nze0, ell;
    GrB_Info infogrb;
    GrB_Vector surfaces;
    Vector_points2D* length_edges = compute_length_edges2D(edges, points);//=((p.edges)' * p.vertices)'
    GrB_Vector fj, ee0;
    GrB_Vector nz_fj, nz_e0;
    GrB_Vector extr_vals_fj, extr_vals_ee0;
    my_real_c newval;
    Point2D *x0, *xe, *len_ei;
    form_struct tf;
    int8_t sign;
    GrB_Index nb_edges, size_nz_fj;
    const uint64_t nb_pts = points->size;

    CHECK_GB_CALL(infogrb, GrB_Matrix_ncols(&nf, *faces));
    CHECK_GB_CALL(infogrb, GrB_Matrix_ncols(&nb_edges, *edges));
    CHECK_GB_CALL(infogrb, GrB_Vector_new(&surfaces, GrB_FP64, nf));
    CHECK_GB_CALL(infogrb, GrB_Vector_new(&fj, GrB_INT8, nb_edges));
    CHECK_GB_CALL(infogrb, GrB_Vector_new(&nz_fj, GrB_UINT64, nb_edges));
    CHECK_GB_CALL(infogrb, GrB_Vector_new(&extr_vals_fj, GrB_INT8, nb_edges));
    CHECK_GB_CALL(infogrb, GrB_Vector_new(&ee0, GrB_INT8, nb_pts));
    CHECK_GB_CALL(infogrb, GrB_Vector_new(&nz_e0, GrB_INT8, nb_pts));
    CHECK_GB_CALL(infogrb, GrB_Vector_new(&extr_vals_ee0, GrB_INT8, nb_pts));

    for (j=0; j<nf; j++){
        CHECK_GB_CALL(infogrb, GrB_extract(fj, GrB_NULL, GrB_NULL, *faces, GrB_ALL, 1, j, GrB_NULL)); //Get indices of edges composing face j
        CHECK_GB_CALL(infogrb, GxB_Vector_extractTuples_Vector(nz_fj, extr_vals_fj, fj, GrB_NULL));
        CHECK_GB_CALL(infogrb, GrB_Vector_nvals(&size_nz_fj, nz_fj));

        if(size_nz_fj>0){
            CHECK_GB_CALL(infogrb, GrB_Vector_extractElement(&e0, nz_fj, 0));

            CHECK_GB_CALL(infogrb, GrB_extract(ee0, GrB_NULL, GrB_NULL, *edges, GrB_ALL, 1, e0, GrB_NULL)); //Point indices of first edge of face j
            CHECK_GB_CALL(infogrb, GxB_Vector_extractTuples_Vector(nz_e0, extr_vals_ee0, ee0, GrB_NULL));
            CHECK_GB_CALL(infogrb, GrB_Vector_size(&size_nze0, nz_e0));
            if(size_nze0>0){
                CHECK_GB_CALL(infogrb, GrB_Vector_extractElement(&p0, nz_e0, 0)); //Index of first point of first edge of face j
                x0 = get_ith_elem_vec_pts2D(points, p0); //First point of first edge of face j

                for(ell = 1; ell<size_nz_fj; ell++){
                    CHECK_GB_CALL(infogrb, GrB_Vector_extractElement(&e0, nz_fj, ell));
                    CHECK_GB_CALL(infogrb, GrB_extract(ee0, GrB_NULL, GrB_NULL, *edges, GrB_ALL, 1, e0, GrB_NULL)); //Point indices of next edge of face j
                    CHECK_GB_CALL(infogrb, GxB_Vector_extractTuples_Vector(nz_e0, extr_vals_ee0, ee0, GrB_NULL));
                    CHECK_GB_CALL(infogrb, GrB_Vector_size(&size_nze0, nz_e0));
                    infogrb = GxB_Vector_isStoredElement(ee0, p0);
                    if ((size_nze0>0) && (infogrb == GrB_NO_VALUE)){//Check if the edge has at least one point and the point x0 is not part of this edge.
                        CHECK_GB_CALL(infogrb, GrB_Vector_extractElement(&p, nz_e0, 0)); //Index of first point of next edge of face j
                        xe = get_ith_elem_vec_pts2D(points, p);//First point of next edge of face j
                        len_ei = get_ith_elem_vec_pts2D(length_edges, e0);
                        tf = twoform2D((Point2D){xe->x-x0->x, xe->y-x0->y}, *len_ei);
                        CHECK_GB_CALL(infogrb, GrB_Matrix_extractElement(&sign, *faces, e0, j));
                        //for (i=0; i<tf.n; i++)
                        //    GrB_Vector_setElement(newcol, 0.5*sign*tf.v[i], i);
                        //GrB_Col_assign(surfaces, GrB_NULL, GrB_PLUS_FP64, newcol, GrB_ALL, 3, j, GrB_NULL);//surfaces[:,j] += 0.5* nz_faces[k] * tf.v
                        newval = 0.0;
                        infogrb = GrB_Vector_extractElement(&newval, surfaces, j); //if surfaces[j] is empty, newval still is 0.0
                        newval += 0.5*sign*tf.v[0];
                        CHECK_GB_CALL(infogrb, GrB_Vector_setElement(surfaces, newval, j));
                        //free(tf.v);
                    }
                }
            }
        }
    }

    dealloc_vec_pts2D(length_edges); free(length_edges);
    GrB_free(&fj);
    GrB_free(&ee0);
    GrB_free(&nz_fj);
    GrB_free(&nz_e0);
    GrB_free(&extr_vals_fj);
    GrB_free(&extr_vals_ee0);

    return surfaces;
}


static Vector_points3D* compute_length_edges3D(const GrB_Matrix* edges, const Vector_points3D* vertices){
    GrB_Matrix mat_vert = vector_pt3D_to_matrix(vertices);
    GrB_Matrix length_edges;
    Vector_points3D* le_as_pts;
    GrB_Index nbcols;

    GrB_Matrix_ncols(&nbcols, *edges);
    GrB_Matrix_new(&length_edges, GrB_FP64, 3, nbcols);
    GrB_mxm(length_edges, GrB_NULL, GrB_NULL, GrB_PLUS_TIMES_SEMIRING_FP64, mat_vert, *edges, GrB_NULL);
    le_as_pts = points3D_from_matrix(length_edges);

    GrB_free(&mat_vert);
    GrB_free(&length_edges);

    return le_as_pts;
}
//Compute the surface normals
static GrB_Matrix surfaces_poly3D_pef(const Vector_points3D* points, const GrB_Matrix* edges, const GrB_Matrix* faces){
    const unsigned int d=3;
    GrB_Index nf, i, j, k, e0, p0, p, size_nze0;
    GrB_Info infogrb;
    GrB_Matrix surfaces;
    GrB_Vector fj, ee0;
    GrB_Vector nz_fj, nz_e0;
    GrB_Vector extr_vals_fj, extr_vals_ee0;
    GrB_Vector newcol;
    Point3D *x0, *xe, *len_ei;
    form_struct tf;
    int8_t sign;
    GrB_Index nb_edges, size_nz_fj, it;
    const uint64_t nb_pts = points->size;
    Vector_points3D* length_edges = compute_length_edges3D(edges, points);//=((p.edges)' * p.vertices)'

    infogrb = GrB_Matrix_ncols(&nf, *faces);
    infogrb = GrB_Matrix_ncols(&nb_edges, *edges);
    infogrb = GrB_Matrix_new(&surfaces, GrB_FP64, d*(d-1)/2, nf);
    infogrb = GrB_Vector_new(&newcol, GrB_FP64, d*(d-1)/2);
    infogrb = GrB_Vector_new(&fj, GrB_INT8, nb_edges);
    infogrb = GrB_Vector_new(&nz_fj, GrB_UINT64, nb_edges);
    infogrb = GrB_Vector_new(&extr_vals_fj, GrB_INT8, nb_edges);
    infogrb = GrB_Vector_new(&ee0, GrB_INT8, nb_pts);
    infogrb = GrB_Vector_new(&nz_e0, GrB_INT8, nb_pts);
    infogrb = GrB_Vector_new(&extr_vals_ee0, GrB_INT8, nb_pts);

    for (j=0; j<nf; j++){
        infogrb = GrB_Col_extract(fj, GrB_NULL, GrB_NULL, *faces, GrB_ALL, 1, j, GrB_NULL); //Get indices of edges composing face j
        infogrb = GxB_Vector_extractTuples_Vector(nz_fj, extr_vals_fj, fj, GrB_NULL);
        infogrb = GrB_Vector_size(&size_nz_fj, nz_fj);

        if (size_nz_fj>0){ //Face j not empty
            it = 0;
            infogrb = GrB_Vector_extractElement(&e0, nz_fj, it);//Index of first edge in face j

            infogrb = GrB_extract(ee0, GrB_NULL, GrB_NULL, *edges, GrB_ALL, 1, e0, GrB_NULL); //Point indices of first edge of face j
            infogrb = GxB_Vector_extractTuples_Vector(nz_e0, extr_vals_ee0, ee0, GrB_NULL);
            infogrb = GrB_Vector_size(&size_nze0, nz_e0);
            while ((size_nze0 == 0) && (it<size_nz_fj)){//In case the first edge is empty: check until you find one not empty
                it++;
                infogrb = GrB_Vector_extractElement(&e0, nz_fj, it);
                infogrb = GrB_extract(ee0, GrB_NULL, GrB_NULL, *edges, GrB_ALL, 1, e0, GrB_NULL); //Point indices of first edge of face j
                infogrb = GxB_Vector_extractTuples_Vector(nz_e0, extr_vals_ee0, ee0, GrB_NULL);
                infogrb = GrB_Vector_size(&size_nze0, nz_e0);
            }

            if(size_nze0 > 0){
                infogrb = GrB_Vector_extractElement(&p0, nz_e0, 0); //Index of first point of first edge of face j
                x0 = get_ith_elem_vec_pts3D(points, p0); //First point of first edge of face j
            }

            for(k=it+1; k<size_nz_fj; k++){
                infogrb = GrB_Vector_extractElement(&e0, nz_fj, k);
                infogrb = GrB_extract(ee0, GrB_NULL, GrB_NULL, *edges, GrB_ALL, 1, e0, GrB_NULL); //Point indices of next edge of face j
                infogrb = GxB_Vector_extractTuples_Vector(nz_e0, extr_vals_ee0, ee0, GrB_NULL);
                infogrb = GrB_Vector_size(&size_nze0, nz_e0);
                infogrb = GxB_Vector_isStoredElement(ee0, p0);
                if ((size_nze0>0) && (infogrb == GrB_NO_VALUE)){//Check if the edge has at least one point and the point x0 is not part of this edge.
                    infogrb = GrB_Vector_extractElement(&p, nz_e0, 0); //Index of first point of next edge of face j
                    xe = get_ith_elem_vec_pts3D(points, p);//First point of next edge of face j
                    len_ei = get_ith_elem_vec_pts3D(length_edges, e0);
                    tf = twoform3D((Point3D){xe->x-x0->x, xe->y-x0->y, xe->t-x0->t}, *len_ei);
                    GrB_Matrix_extractElement(&sign, *faces, e0, j);
                    for (i=0; i<tf.n; i++)
                        GrB_Vector_setElement(newcol, 0.5*sign*tf.v[i], i);
                    GrB_Col_assign(surfaces, GrB_NULL, GrB_PLUS_FP64, newcol, GrB_ALL, 3, j, GrB_NULL);//surfaces[:,j] += 0.5* nz_faces[k] * tf.v
                    //free(tf.v);
                }
            }
        }
    }

    dealloc_vec_pts3D(length_edges); free(length_edges);
    GrB_free(&fj);
    GrB_free(&ee0);
    GrB_free(&nz_fj);
    GrB_free(&nz_e0);
    GrB_free(&extr_vals_fj);
    GrB_free(&extr_vals_ee0);
    GrB_free(&newcol);

    return surfaces;
}

//Compute the volumes
static GrB_Vector volumes_poly3D_pefv(const Vector_points3D* points, const GrB_Matrix* edges, const GrB_Matrix* faces, const GrB_Matrix* volumes){
    //unsigned int d = 3;
    GrB_Matrix surfaces = surfaces_poly3D_pef(points, edges, faces);
    GrB_Vector volume_sizes;
    GrB_Vector vj, nz_vj, extr_vals_vj;
    GrB_Vector ff0, nz_f0, extr_vals_ff0;
    GrB_Vector ee0, nz_e0, extr_vals_ee0;
    GrB_Vector surface_j;
    GrB_Info infogrb;
    GrB_Index nv, e0, f0, size_nzf0, size_nze0;
    GrB_Index i, j, k, p0, p;
    Point3D *x0, *xe;
    int8_t sign;
    my_real_c newval = 0;
    const uint64_t nb_pts = points->size;
    GrB_Index nf, nb_edges, size_nz_vj, it;
    form_struct tf;
    
    infogrb = GrB_Matrix_ncols(&nv, *volumes);
    //infogrb = GrB_Matrix_new(volume_sizes, GrB_FP64, d*(d-1)*(d-2)/6, nv);
    infogrb = GrB_Vector_new(&volume_sizes, GrB_FP64, nv);
    infogrb = GrB_Matrix_ncols(&nf, *faces);
    infogrb = GrB_Matrix_ncols(&nb_edges, *edges);
    infogrb = GrB_Vector_new(&vj, GrB_INT8, nf);
    infogrb = GrB_Vector_new(&nz_vj, GrB_UINT64, nf);
    infogrb = GrB_Vector_new(&extr_vals_vj, GrB_INT8, nf);
    infogrb = GrB_Vector_new(&ff0, GrB_INT8, nb_edges);
    infogrb = GrB_Vector_new(&nz_f0, GrB_UINT64, nb_edges);
    infogrb = GrB_Vector_new(&extr_vals_ff0, GrB_INT8, nb_edges);
    infogrb = GrB_Vector_new(&ee0, GrB_INT8, nb_pts);
    infogrb = GrB_Vector_new(&nz_e0, GrB_INT8, nb_pts);
    infogrb = GrB_Vector_new(&extr_vals_ee0, GrB_INT8, nb_pts);
    infogrb = GrB_Vector_new(&surface_j, GrB_FP64, 3);

    for (j=0; j<nv; j++){
        //nz_vj = nzrange(volumes,j)
        infogrb = GrB_extract(vj, GrB_NULL, GrB_NULL, *volumes, GrB_ALL, 1, j, GrB_NULL); //Get indices of faces composing volume j
        infogrb = GxB_Vector_extractTuples_Vector(nz_vj, extr_vals_vj, vj, GrB_NULL);
        //infoit = GxB_Vector_Iterator_attach (iterator, nz_vj, GrB_NULL);
        infogrb = GrB_Vector_size(&size_nz_vj, nz_vj);
        if (size_nz_vj>0){//Volume j not empty
            it = 0;
            infogrb = GrB_Vector_extractElement(&f0, nz_vj, it);//Index of first face in volume j
            infogrb = GrB_extract(ff0, GrB_NULL, GrB_NULL, *faces, GrB_ALL, 1, f0, GrB_NULL); //Edge indices of first face of volume j
            infogrb = GxB_Vector_extractTuples_Vector(nz_f0, extr_vals_ff0, ff0, GrB_NULL);
            infogrb = GrB_Vector_size(&size_nzf0, nz_f0);

            //infoit = GxB_Vector_Iterator_next (iterator) ; //Move on to next face in volume
            while ((size_nzf0 == 0) && (it<size_nz_vj)){ //In case the first face is empty: check until you find one not empty
                it++;
                infogrb = GrB_Vector_extractElement(&f0, nz_vj, it);
                infogrb = GrB_extract(ff0, GrB_NULL, GrB_NULL, *faces, GrB_ALL, 1, f0, GrB_NULL); //Edge indices of first face of volume j
                infogrb = GxB_Vector_extractTuples_Vector(nz_f0, extr_vals_ff0, ff0, GrB_NULL);
                infogrb = GrB_Vector_size(&size_nzf0, nz_f0);
            }

            //kf0 = nz_vj[1]
            //f0 = vol_indices[kf0]
            //nz_f0 = nzrange(faces,f0)
            if (size_nzf0>0){
                i = 0;
                infogrb = GrB_Vector_extractElement(&e0, nz_f0, i); //Index of first edge of first face of volume j
                infogrb = GrB_extract(ee0, GrB_NULL, GrB_NULL, *edges, GrB_ALL, 1, e0, GrB_NULL); //Points indices of first edge of first face of volume j
                infogrb = GxB_Vector_extractTuples_Vector(nz_e0, extr_vals_ee0, ee0, GrB_NULL);
                infogrb = GrB_Vector_size(&size_nze0, nz_e0);
                i++;
                while ((size_nzf0 == 0) && (i < size_nzf0)) {//In case the first edge is empty: check until you find one not empty
                    infogrb = GrB_Vector_extractElement(&e0, nz_f0, i); //Index of first edge of first face of volume j
                    infogrb = GrB_extract(ee0, GrB_NULL, GrB_NULL, *edges, GrB_ALL, 1, e0, GrB_NULL); //Points indices of first edge of first face of volume j
                    infogrb = GxB_Vector_extractTuples_Vector(nz_e0, extr_vals_ee0, ee0, GrB_NULL);
                    infogrb = GrB_Vector_size(&size_nze0, nz_e0);
                    i++;
                }
                //ke0 = nz_f0[1]
                //e0 = face_indices[ke0]
                //nz_e0 = nzrange(edges,e0)
                if (size_nze0>0){
                    //kp0 = nz_e0[1]
                    //p0 = edge_indices[kp0]
                    infogrb = GrB_Vector_extractElement(&p0, nz_e0, 0); //Index of first point of first edge of first face of volume j
                    x0 = get_ith_elem_vec_pts3D(points, p0);//Take first point of first edge of first face of volume j
                    for (k=it+1; k<size_nz_vj; k++){
                        infogrb = GrB_Vector_extractElement(&f0, nz_vj, k);
                        infogrb = GrB_extract(ff0, GrB_NULL, GrB_NULL, *faces, GrB_ALL, 1, f0, GrB_NULL); //Edge indices of next face of volume j
                        infogrb = GxB_Vector_extractTuples_Vector(nz_f0, extr_vals_ff0, ff0, GrB_NULL);
                        infogrb = GrB_Vector_size(&size_nzf0, nz_f0);

                        if (size_nzf0>0){
                            i = 0;
                            infogrb = GrB_Vector_extractElement(&e0, nz_f0, i); //Index of first edge of next face of volume j
                            infogrb = GrB_extract(ee0, GrB_NULL, GrB_NULL, *edges, GrB_ALL, 1, e0, GrB_NULL); //Points indices of first edge of next face of volume j
                            infogrb = GxB_Vector_extractTuples_Vector(nz_e0, extr_vals_ee0, ee0, GrB_NULL);
                            infogrb = GrB_Vector_size(&size_nze0, nz_e0);
                            i++;
                            while ((size_nzf0 == 0) && (i < size_nzf0)) {//In case the first edge is empty: check until you find one not empty
                                infogrb = GrB_Vector_extractElement(&e0, nz_f0, i); //Index of first edge of first face of volume j
                                infogrb = GrB_extract(ee0, GrB_NULL, GrB_NULL, *edges, GrB_ALL, 1, e0, GrB_NULL); //Points indices of first edge of first face of volume j
                                infogrb = GxB_Vector_extractTuples_Vector(nz_e0, extr_vals_ee0, ee0, GrB_NULL);
                                infogrb = GrB_Vector_size(&size_nze0, nz_e0);
                                i++;
                            }
                            infogrb = GxB_Vector_isStoredElement(ee0, p0);
                            if ((size_nze0>0) && (infogrb == GrB_NO_VALUE)){//Check if the edge has at least one point and the point x0 is not part of this edge.
                                infogrb = GrB_Vector_extractElement(&p, nz_e0, 0); //Index of first point of first edge of next face of volume j
                                xe = get_ith_elem_vec_pts3D(points, p);//First point of first edge of ...
                                infogrb = GrB_extract(surface_j, GrB_NULL, GrB_NULL, surfaces, GrB_ALL, 1, j, GrB_NULL);
                                tf = threeform3D_vec((Point3D){xe->x-x0->x, xe->y-x0->y, xe->t-x0->t}, surface_j);

                                //for (i=0; i<tf.n; i++)
                                //    GrB_Vector_setElement(newcol, 0.5*sign*tf.v[i], i);
                                //GrB_Col_assign(volume_sizes, GrB_NULL, GrB_PLUS_FP64, newcol, GrB_ALL, 3, j, GrB_NULL);//surfaces[:,j] += 0.5* nz_faces[k] * tf.v
                                GrB_Matrix_extractElement(&sign, *volumes, f0, j);
                                GrB_Vector_extractElement(&newval, volume_sizes, j);
                                newval += sign*tf.v[0]/3.0;
                                GrB_Vector_setElement(volume_sizes, newval, j);
                                free(tf.v);
                            }
                        }
                    }
                }
            }
        }
    }
    GrB_free(&vj);
    GrB_free(&nz_vj);
    GrB_free(&extr_vals_vj);
    GrB_free(&ff0);
    GrB_free(&nz_f0);
    GrB_free(&extr_vals_ff0);
    GrB_free(&ee0);
    GrB_free(&nz_e0);
    GrB_free(&extr_vals_ee0);
    GrB_free(&surface_j);
    return volume_sizes;
}

//Compute the hypervolumes
/*
function hypervolumes_poly(d::Int, points::Vector{<:Point}, edges::SparseMatrixCSC{Int8, Int}, faces::SparseMatrixCSC{Int8, Int}, volumes::SparseMatrixCSC{Int8, Int}, hypervolumes::SparseMatrixCSC{Int8, Int}, 
                            volume_sizes = volumes_poly(d, points, edges, faces, volumes))
    nh = size(hypervolumes,2)
    hypervolume_sizes = zeros(Int(d*(d-1)*(d-2)*(d-3)/24),nv)

    hyper_indices = rowvals(hypervolumes)
    vol_indices = rowvals(volumes)
    face_indices = rowvals(faces)
    edge_indices = rowvals(edges)
    nz_hypervolumes = nonzeros(hypervolumes)
    for j=1:nh
        nz_hj = nzrange(hypervolumes,j)
        if length(nz_hj)>0
            kv0 = nz_hj[1]
            v0 = hyper_indices[kv0]
            kf0 = nzrange(volumes,v0)[1]
            f0 = vol_indices[kf0]
            ke0 = nzrange(faces,f0)[1]
            e0 = face_indices[ke0]
            kp0 = nzrange(edges,e0)[1]
            p0 = edge_indices[kp0]
            x0 = points[p0] //Take first point of first edge of first face of first volume of hypervolume j
            for k in nzrange(hypervolumes,j)
                v = hyper_indices[k]
                if length(nzrange(volumes,v))>0
                    kf = nzrange(volumes,v)[1]
                    f = vol_indices[kf]
                    if length(nzrange(faces,f))>0
                        ke = nzrange(faces,f)[1]
                        e = face_indices[ke]
                        if length(nzrange(edges,e))>0 && edges[p0,e]==0
                            kp = nzrange(edges,e)[1]
                            p = edge_indices[kp]
                            xv = points[p] #Take first point of first edge of first face of volume k of hypervolume j
                            ff = fourform(xv-x0,volume_sizes[:,v])
                            hypervolume_sizes[:,j] += nz_hypervolumes[k] * ff.v/ 4.0
                        end
                    end
                end
            end
        end
    end
    return hypervolume_sizes
end
*/

GrB_Vector surfaces_poly2D(const Polygon2D *p){
    return surfaces_poly2D_pef(p->vertices, p->edges, p->faces);
}

GrB_Matrix surfaces_poly3D(const Polyhedron3D *p){
    return surfaces_poly3D_pef(p->vertices, p->edges, p->faces);
}

//GrB_Matrix surfaces_poly4D(Polytope4D p){
//    return surfaces_poly4D_pef(p->vertices, p->edges, p->faces);
//}

GrB_Vector volumes_poly3D(const Polyhedron3D *p){
    return volumes_poly3D_pefv(p->vertices, p->edges, p->faces, p->volumes);
}

//GrB_Vector volumes_poly4D(const Polytope4D *p){
//    return volumes_poly4D_pefv(p->vertices, p->edges, p->faces, p->volumes);
//}

//GrB_Vector hypervolumes_poly4D(const Polytope4D *p){
//    return hypervolumes_poly4D_vefvh(p->vertices, p->edges, p->faces, p->volumes, p->hypervolumes);
//}
