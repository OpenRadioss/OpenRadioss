#include "polygon_fusion_and_split.h"
#include "AStar.h"
#include "edge_collision.h"
#include "general_clipping.h"
#include "macro_check_gb_call.h"

#define SIGN(x) ((x)>0 ? 1 : -1)

//Finds the smallest index in the array of sets faces_to_fuse.
static uint64_t find_smallest_index(Vector_uint **faces_to_fuse, uint64_t i){
    Vector_uint* curr_s;
    uint64_t curr_min, pot_min, next_curr_min, j;
    
    curr_s = faces_to_fuse[i];
    curr_min = *get_ith_elem_vec_uint(curr_s, 0);
    for(j=1; j<curr_s->size; j++){
        pot_min = *get_ith_elem_vec_uint(curr_s, j);
        if (pot_min < curr_min)
            curr_min = pot_min;
    }

    curr_s = faces_to_fuse[curr_min];
    next_curr_min = *get_ith_elem_vec_uint(curr_s, 0);
    for(j=1; j<curr_s->size; j++){
        pot_min = *get_ith_elem_vec_uint(curr_s, j);
        if (pot_min < next_curr_min)
            next_curr_min = pot_min;
    }

    while (curr_min>next_curr_min){
        curr_s = faces_to_fuse[curr_min];
        curr_min = next_curr_min;
        next_curr_min = *get_ith_elem_vec_uint(curr_s, 0);
        for(j=1; j<curr_s->size; j++){
            pot_min = *get_ith_elem_vec_uint(curr_s, j);
            if (pot_min < next_curr_min)
                next_curr_min = pot_min;
        }
    }

    return curr_min;
}

/// @brief Breaks all edges where one of the extreme point is marked as inside the intersection region (pt_in_or_out == -1).
/// @param old_p [IN] Polygon with intersecting face(s).
/// @param edge_intersect1 [IN] Edge intersecting (1st edge)
/// @param edge_intersect2 [IN] Edge intersecting (2nd edge)
/// @param pt_in_or_out [IN] Array noting if a point is not concerned by an intersection (0), concerned but outside the intersection region (1) or inside the intersection region (-1) and should be suppressed.
/// @param result_p [OUT] Resulting polygon once the edges are broken.
void break_edges_split_fusion(const Polygon2D* old_p, const Vector_uint* edge_intersect1, const Vector_uint* edge_intersect2, \
                            int8_t* pt_in_or_out, Polygon2D** result_p){
    Vector_uint* forbidden_edges;
    uint64_t i, j, k, l, ind;
    GrB_Index nb_edges, nb_pts, nb_faces;
    Vector_uint *listEdges = alloc_empty_vec_uint();
    Vector_uint *listNodes = alloc_empty_vec_uint();
    Polygon2D* p;
    GrB_Info infogrb;
    
    if (old_p != *result_p){
        *result_p = new_Polygon2D_vefsp(old_p->vertices, old_p->edges, old_p->faces, old_p->status_edge, old_p->phase_face);
    }
    p = *result_p;

    //First: delete all points which should be deleted, which breaks some edges.
    forbidden_edges = cat_vec_uint(edge_intersect1, edge_intersect2);
    GrB_Matrix_nrows(&nb_pts, *(p->edges));
    GrB_Matrix_ncols(&nb_edges, *(p->edges));
    GrB_Matrix_ncols(&nb_faces, *(p->faces));

    for(i=0; i<p->vertices->size; i++){
        if(pt_in_or_out[i]<0){
            for(j=0; j<i; j++){
                if(pt_in_or_out[j]<0){
                    astar(p->vertices, p->edges, i, j, forbidden_edges, listEdges, listNodes);
                    if (listEdges->size>0){
                        //GrB_assign(*(p->edges), GrB_NULL, GrB_NULL, 0, GrB_ALL, 1, listEdges->data, listEdges->size, GrB_NULL); //CHANGE THIS, it will put whole lines to 0, it will be a giant mess.
                        for(k=0; k<listEdges->size; k++){
                            ind = *get_ith_elem_vec_uint(listEdges, k);
                            for(l=0; l<nb_pts; l++){
                                CHECK_GB_CALL(infogrb, GrB_Matrix_removeElement(*(p->edges), l, ind))
                            }
                        }
                        //GrB_assign(*(p->faces), GrB_NULL, GrB_NULL, 0, listEdges->data, listEdges->size, GrB_ALL, 1, GrB_NULL);
                        for(l=0; l<nb_faces; l++){
                            for(k=0; k<listEdges->size; k++){
                                ind = *get_ith_elem_vec_uint(listEdges, k);
                                CHECK_GB_CALL(infogrb, GrB_Matrix_removeElement(*(p->faces), ind, l))
                            }
                        }
                        for(k=0; k<listNodes->size; k++){
                            pt_in_or_out[*get_ith_elem_vec_uint(listNodes, k)] = -1;
                        }
                    }
                }
            }
        }
    }

    for(i=0; i<p->vertices->size; i++){
        if(pt_in_or_out[i]<0){
            for(l=0; l<nb_edges; l++){
                CHECK_GB_CALL(infogrb, GrB_Matrix_removeElement(*(p->edges), i, l))
            }
        }
    }

    dealloc_vec_uint(listEdges); free(listEdges);
    dealloc_vec_uint(listNodes); free(listNodes);
}

/// @brief Based on the array faces_to_fuse, fuse the edges of some faces of old_p into a single face (in order to close a face for instance).
/// @param old_p [IN] Polygon with open faces
/// @param faces_to_fuse [IN] Array describing the faces to fuse. It is an array of size number of faces, each cell of this array being a set of indices.
/// @param size_faces_to_fuse [IN] Size of `faces_to_fuse`
/// @param result_p [OUT] Resulting array.
void fuse_faces(const Polygon2D* old_p, Vector_uint** faces_to_fuse, uint64_t size_faces_to_fuse, Polygon2D** result_p){
    uint64_t i, j, k, js, smallest_index;
    GrB_Index nb_edges;
    int8_t vali, valj;
    Vector_uint *s;
    Polygon2D* p;
    GrB_Info infogrb;
    
    if (old_p != *result_p)
        *result_p = new_Polygon2D_vefsp(old_p->vertices, old_p->edges, old_p->faces, old_p->status_edge, old_p->phase_face);
    p = *result_p; 

    GrB_Matrix_nrows(&nb_edges, *(p->faces));
    //Finally: fuse every face into only one. The choice is made to fuse it into the face with the smallest index.
    for (i=0; i<size_faces_to_fuse; i++){
        s = faces_to_fuse[i];
        if (s->size != 0){ //face i is fused
            smallest_index = find_smallest_index(faces_to_fuse, i);
            for (js=0; js<s->size; js++){
                j = *get_ith_elem_vec_uint(s, js);
                if (j != smallest_index){
                    //p.faces[:, i] += p.faces[:, j];
                    //p.faces[:, j] .= 0
                    for(k=0; k<nb_edges; k++){
                        vali = 0;
                        GrB_Matrix_extractElement(&vali, *(p->faces), k, i); //It could be normal that this function does not return GrB_SUCCESS
                                                                            //it would just mean that *(p->faces)[k,i]==0
                                                                            //If it is the case, vali's value is not changed and remains 0.
                        valj = 0;
                        GrB_Matrix_extractElement(&valj, *(p->faces), k, j);
                        vali += valj;
                        if (vali != 0){
                            CHECK_GB_CALL(infogrb, GrB_Matrix_setElement(*(p->faces), vali, k, i))
                        } else {
                            CHECK_GB_CALL(infogrb, GrB_Matrix_removeElement(*(p->faces), k, i))
                        }
                        CHECK_GB_CALL(infogrb, GrB_Matrix_removeElement(*(p->faces), k, j))
                    }
                    if (SIGN(p->phase_face->data[j]) != SIGN(p->phase_face->data[i]))
                        p->phase_face->data[i] = labs(p->phase_face->data[i]);
                }
            }
            if (i != smallest_index){
                //p.faces[:, smallest_index] += p.faces[:, i]
                //p.faces[:, i] .= 0
                for(k=0; k<nb_edges; k++){
                    vali = 0;
                    GrB_Matrix_extractElement(&vali, *(p->faces), k, i); //It could be normal that this function does not return GrB_SUCCESS
                                                                         //it would just mean that *(p->faces)[k,i]==0
                                                                         //If it is the case, vali's value is not changed and remains 0.
                    valj = 0;
                    GrB_Matrix_extractElement(&valj, *(p->faces), k, smallest_index);
                    valj += vali;
                    if (valj != 0){
                        CHECK_GB_CALL(infogrb, GrB_Matrix_setElement(*(p->faces), valj, k, smallest_index))
                    } else {
                        CHECK_GB_CALL(infogrb, GrB_Matrix_removeElement(*(p->faces), k, smallest_index))
                    }
                    CHECK_GB_CALL(infogrb, GrB_Matrix_removeElement(*(p->faces), k, i))
                }
                if (SIGN(p->phase_face->data[smallest_index]) != SIGN(p->phase_face->data[i]))
                    p->phase_face->data[smallest_index] = labs(p->phase_face->data[smallest_index]);
            }
        }
    }
}

/// @brief Solves an auto-intersection by fusing faces intersecting. Normally used after `break_edges_split_fusion`.
/// @details It supposes that one edge can only be part of only one face => hard to ensure for more than two fluids..?
/// @param old_p [IN] Polygon with intersections and broken edges.
/// @param original_edges [IN] Matrix of the edges before being broken
/// @param pts_intersec [IN] List of intersection points
/// @param edge_intersect1 [IN] Edge intersecting (1st edge)
/// @param edge_intersect2 [IN] Edge intersecting (2nd edge)
/// @param pt_in_or_out [IN] Array noting if a point is not concerned by an intersection (0), concerned but outside the intersection region (1) or inside the intersection region (-1) and should be suppressed.
/// @param p [OUT] Result polygon, with the edges modifed to correct the intersection.
/// @param faces_to_fuse [OUT] List of faces to fuse, then given to function `fuse_faces`. It is an array of size number of faces in `old_p`, each cell of this array being a set of indices.
/// @param size_faces_to_fuse [OUT] Size of `faces_to_fuse`
void polygons_fusion(const Polygon2D* old_p, const GrB_Matrix* original_edges, const Vector_points2D* pts_intersec, const Vector_uint* edge_intersect1, const Vector_uint* edge_intersect2, const int8_t* pt_in_or_out,\
                            Polygon2D** p, Vector_uint*** faces_to_fuse, uint64_t *size_faces_to_fuse){
    uint64_t former_nb_pts, former_nb_edges, former_nb_faces;
    uint64_t i, j, k, ie1, ie2, index_new_edge;
    uint64_t i_face1, i_face2, i_pt1, i_pt2;
    GrB_Vector ej, pot_ind_faces, I_vec_f_k, extr_vals_f_k, nz_ej, extr_vals_ej;
    GrB_Vector ej_orig, nz_ej_orig, extr_vals_ej_orig;
    GrB_Vector sub_ej, nz_sub_ej, extr_vals_sub_ej;
    GrB_Matrix f_k, new_pt, new_edges, new_faces, copy_mat;
    int8_t sum_col1, sum_col2, orient_edge, val;
    GrB_Index nvals, nb_edges, nb_pts, orig_pts1, orig_pts2, size_nz_ej, size_nz_sub_ej;
    GrB_Index I_ind[2];
    GrB_Index nb_cols, nb_rows;
    Point2D *pt1, *pt2, *pt;
    Vector_double* thetas;
    Vector_uint *indices_sorted;
    my_real_c res;
    GrB_Info infogrb;

    if (old_p != *p)
        *p = new_Polygon2D_vefsp(old_p->vertices, old_p->edges, old_p->faces, old_p->status_edge, old_p->phase_face);

    GrB_Matrix_ncols(&former_nb_pts, *original_edges);
    GrB_Vector_new(&ej_orig, GrB_INT8, former_nb_pts);
    GrB_Vector_new(&nz_ej_orig, GrB_UINT64, former_nb_pts);
    GrB_Vector_new(&extr_vals_ej_orig, GrB_INT8, former_nb_pts);

    former_nb_pts = (*p)->vertices->size;
    GrB_Matrix_ncols(&former_nb_edges, *((*p)->edges));
    GrB_Matrix_ncols(&former_nb_faces, *((*p)->faces));
    GrB_Matrix_new(&f_k, GrB_INT8, 1, former_nb_faces);
    GrB_Matrix_new(&new_pt, GrB_INT8, 1, former_nb_edges);
    GrB_Vector_new(&ej, GrB_INT8, former_nb_pts);
    GrB_Vector_new(&nz_ej, GrB_UINT64, former_nb_pts);
    GrB_Vector_new(&extr_vals_ej, GrB_INT8, former_nb_pts);
    GrB_Vector_new(&pot_ind_faces, GrB_UINT64, 1);
    GrB_Vector_new(&I_vec_f_k, GrB_UINT64, 1);
    GrB_Vector_new(&extr_vals_f_k, GrB_INT8, 1);
    GrB_Matrix_new(&copy_mat, GrB_INT8, 1, 1);
    thetas = alloc_empty_vec_double();
    indices_sorted = alloc_empty_vec_uint();
    nb_pts = former_nb_pts;

    for(i=0; i<pts_intersec->size; i++){
        push_back_vec_pts2D(&((*p)->vertices), get_ith_elem_vec_pts2D(pts_intersec, i));
    } 

    *size_faces_to_fuse = former_nb_faces;
    *faces_to_fuse = (Vector_uint**)malloc(former_nb_faces*sizeof(Vector_uint*));
    for(i=0; i<former_nb_faces; i++){
        (*faces_to_fuse)[i] = alloc_empty_vec_uint();
    }

    //Second: rebuild the edges by connecting the intersection points.
    for (i=0; i<edge_intersect1->size; i++){
        ie1 = *get_ith_elem_vec_uint(edge_intersect1, i);//edge_intersect1[i]
        ie2 = *get_ith_elem_vec_uint(edge_intersect2, i);//edge_intersect2[i]
            
        //i_face1 = findfirst(x->x!=0, p.faces[ie1, :]);
        CHECK_GB_CALL(infogrb, GrB_extract(f_k, GrB_NULL, GrB_NULL, *((*p)->faces), &ie1, 1, GrB_ALL, 1, GrB_NULL)) 
        CHECK_GB_CALL(infogrb, GrB_Matrix_nvals(&nvals, f_k))
        CHECK_GB_CALL(infogrb, GrB_Vector_resize(pot_ind_faces, nvals))
        CHECK_GB_CALL(infogrb, GrB_Vector_resize(I_vec_f_k, nvals))
        CHECK_GB_CALL(infogrb, GrB_Vector_resize(extr_vals_f_k, nvals))
        CHECK_GB_CALL(infogrb, GxB_Matrix_extractTuples_Vector(I_vec_f_k, pot_ind_faces, extr_vals_f_k, f_k, GrB_NULL))
        CHECK_GB_CALL(infogrb, GrB_Vector_extractElement(&i_face1, pot_ind_faces, 0))

        //i_face2 = findfirst(x->x!=0, p.faces[ie2, :]);
        CHECK_GB_CALL(infogrb, GrB_extract(f_k, GrB_NULL, GrB_NULL, *((*p)->faces), &ie2, 1, GrB_ALL, 1, GrB_NULL)) 
        CHECK_GB_CALL(infogrb, GrB_Matrix_nvals(&nvals, f_k))
        CHECK_GB_CALL(infogrb, GrB_Vector_resize(pot_ind_faces, nvals))
        CHECK_GB_CALL(infogrb, GrB_Vector_resize(I_vec_f_k, nvals))
        CHECK_GB_CALL(infogrb, GrB_Vector_resize(extr_vals_f_k, nvals))
        CHECK_GB_CALL(infogrb, GxB_Matrix_extractTuples_Vector(I_vec_f_k, pot_ind_faces, extr_vals_f_k, f_k, GrB_NULL))
        CHECK_GB_CALL(infogrb, GrB_Vector_extractElement(&i_face2, pot_ind_faces, 0))

        push_back_unique_vec_uint(&((*faces_to_fuse)[i_face1]), &i_face2);
        push_back_unique_vec_uint(&((*faces_to_fuse)[i_face2]), &i_face1);

        CHECK_GB_CALL(infogrb, GrB_extract(ej, GrB_NULL, GrB_NULL, *((*p)->edges), GrB_ALL, 1, ie1, GrB_NULL)) 
        CHECK_GB_CALL(infogrb, GrB_reduce(&sum_col1, GrB_NULL, GrB_PLUS_MONOID_INT8, ej, GrB_NULL))
        CHECK_GB_CALL(infogrb, GrB_extract(ej, GrB_NULL, GrB_NULL, *((*p)->edges), GrB_ALL, 1, ie2, GrB_NULL)) 
        CHECK_GB_CALL(infogrb, GrB_reduce(&sum_col2, GrB_NULL, GrB_PLUS_MONOID_INT8, ej, GrB_NULL))

        GrB_Matrix_clear(new_pt);
        if (sum_col1!=0){
            CHECK_GB_CALL(infogrb, GrB_Matrix_setElement(new_pt, (int8_t)(-sum_col1/abs(sum_col1)), 0, ie1))
            CHECK_GB_CALL(infogrb, GrB_Matrix_setElement(new_pt, (int8_t)(sum_col1/abs(sum_col1)), 0, ie2))
        } else if (sum_col2!=0) {
            CHECK_GB_CALL(infogrb, GrB_Matrix_setElement(new_pt, (int8_t)(-sum_col2/abs(sum_col2)), 0, ie2))
            CHECK_GB_CALL(infogrb, GrB_Matrix_setElement(new_pt, (int8_t)(sum_col2/abs(sum_col2)), 0, ie1))
        } else { //edges ie1 and ie2 intersect but none are broken: what should we do?
            CHECK_GB_CALL(infogrb, GrB_Matrix_setElement(new_pt, (int8_t)1, 0, ie1))
            CHECK_GB_CALL(infogrb, GrB_Matrix_setElement(new_pt, (int8_t)1, 0, ie2))
        }

        CHECK_GB_CALL(infogrb, GrB_Matrix_dup(&copy_mat, *((*p)->edges)))
        CHECK_GB_CALL(infogrb, GrB_Matrix_ncols(&nb_cols, copy_mat))
        CHECK_GB_CALL(infogrb, GrB_Matrix_nrows(&nb_rows, copy_mat))
        CHECK_GB_CALL(infogrb, GrB_Matrix_resize(*((*p)->edges), nb_rows + 1, nb_cols))
        CHECK_GB_CALL(infogrb, GxB_Matrix_concat(*((*p)->edges), (GrB_Matrix[]){copy_mat, new_pt}, 2, 1, GrB_NULL))
        CHECK_GB_CALL(infogrb, GrB_Vector_resize(ej, nb_rows + 1))
    }

    //Third: rebuild intermediate edges in case one edge was having multiple intersection points.
    CHECK_GB_CALL(infogrb, GrB_Matrix_ncols(&nb_edges, *((*p)->edges)))
    CHECK_GB_CALL(infogrb, GrB_Matrix_nrows(&nb_pts, *((*p)->edges)))
    CHECK_GB_CALL(infogrb, GrB_Vector_resize(ej, nb_pts))
    CHECK_GB_CALL(infogrb, GrB_Matrix_new(&new_edges, GrB_INT8, nb_pts, 2))
    CHECK_GB_CALL(infogrb, GrB_Matrix_new(&new_faces, GrB_INT8, 2, former_nb_faces))
    I_ind[GxB_BEGIN] = former_nb_pts;
    I_ind[GxB_END] = nb_pts-1;
    CHECK_GB_CALL(infogrb, GrB_Vector_new(&sub_ej, GrB_INT8, nb_pts-former_nb_pts))
    CHECK_GB_CALL(infogrb, GrB_Vector_new(&nz_sub_ej, GrB_UINT64, nb_pts-former_nb_pts))
    CHECK_GB_CALL(infogrb, GrB_Vector_new(&extr_vals_sub_ej, GrB_INT8, nb_pts-former_nb_pts))
    for(i=0; i<nb_edges; i++){
        CHECK_GB_CALL(infogrb, GrB_extract(ej, GrB_NULL, GrB_NULL, *((*p)->edges), GrB_ALL, 1, i, GrB_NULL)) 
        CHECK_GB_CALL(infogrb, GrB_apply(ej, GrB_NULL, GrB_NULL, GrB_ABS_INT8, ej, GrB_DESC_R)) //abs on each component
        CHECK_GB_CALL(infogrb, GrB_reduce(&sum_col1, GrB_NULL, GrB_PLUS_MONOID_INT8, ej, GrB_NULL))
        if (sum_col1>2){
            //i_face1 = findfirst(x->x!=0, p.faces[i, :])
            CHECK_GB_CALL(infogrb, GrB_extract(f_k, GrB_NULL, GrB_NULL, *((*p)->faces), &i, 1, GrB_ALL, 1, GrB_NULL)) 
            CHECK_GB_CALL(infogrb, GrB_Matrix_nvals(&nvals, f_k))
            CHECK_GB_CALL(infogrb, GrB_Vector_resize(pot_ind_faces, nvals))
            CHECK_GB_CALL(infogrb, GrB_Vector_resize(I_vec_f_k, nvals))
            CHECK_GB_CALL(infogrb, GrB_Vector_resize(extr_vals_f_k, nvals))
            CHECK_GB_CALL(infogrb, GxB_Matrix_extractTuples_Vector(I_vec_f_k, pot_ind_faces, extr_vals_f_k, f_k, GrB_NULL))
            CHECK_GB_CALL(infogrb, GrB_Vector_extractElement(&i_face1, pot_ind_faces, 0))

            CHECK_GB_CALL(infogrb, GrB_Matrix_extractElement(&orient_edge, *((*p)->faces), i, i_face1))

            //ej = p.edges[0:former_nb_pts-1, i]
            CHECK_GB_CALL(infogrb, GrB_extract(ej_orig, GrB_NULL, GrB_NULL, *(original_edges), GrB_ALL, 1, i, GrB_NULL)) 
            CHECK_GB_CALL(infogrb, GxB_Vector_extractTuples_Vector(nz_ej_orig, extr_vals_ej_orig, ej_orig, GrB_NULL))
            CHECK_GB_CALL(infogrb, GrB_Vector_extractElement(&orig_pts1, nz_ej_orig, 0))
            CHECK_GB_CALL(infogrb, GrB_Vector_extractElement(&orig_pts2, nz_ej_orig, 1))

            CHECK_GB_CALL(infogrb, GrB_Matrix_extractElement(&val, *(original_edges), orig_pts1, i))

            if (val<0){
                i_pt1 = orig_pts1;
                i_pt2 = orig_pts2;
            } else {
                i_pt1 = orig_pts2;
                i_pt2 = orig_pts1;
            }
            pt1 = get_ith_elem_vec_pts2D((*p)->vertices, i_pt1);
            pt2 = get_ith_elem_vec_pts2D((*p)->vertices, i_pt2);
            
            //ej = p.edges[former_nb_pts:end, i]
            CHECK_GB_CALL(infogrb, GrB_extract(sub_ej, GrB_NULL, GrB_NULL, *((*p)->edges), I_ind, GxB_RANGE, i, GrB_NULL)) 
            CHECK_GB_CALL(infogrb, GxB_Vector_extractTuples_Vector(nz_sub_ej, extr_vals_sub_ej, sub_ej, GrB_NULL))
            CHECK_GB_CALL(infogrb, GrB_Vector_size(&size_nz_sub_ej, nz_sub_ej))
            thetas->size = 0;
            for(j=0; j<size_nz_sub_ej; j++){
                CHECK_GB_CALL(infogrb, GrB_Vector_extractElement(&k, nz_sub_ej, j))
                k += former_nb_pts;
                pt = get_ith_elem_vec_pts2D((*p)->vertices, k);
                res = compute_barycentric_coord(*pt, *pt1, *pt2);
                push_back_vec_double(&thetas, &res);
            }

            sort_vec_double(thetas, thetas, indices_sorted);
            
            //new_edges = spzeros(Int8, size(p.edges, 1), div(length(indices_sorted),2))
            CHECK_GB_CALL(infogrb, GrB_Matrix_resize(new_edges, nb_pts, indices_sorted->size/2))
            CHECK_GB_CALL(infogrb, GrB_Matrix_clear(new_edges))
            for(j=0; j<nb_pts; j++){
                CHECK_GB_CALL(infogrb, GrB_Matrix_removeElement(*((*p)->edges), j, i))
            }
            if (pt_in_or_out[i_pt1]>0){
                //p.edges[i_pt1, i] = -1
                CHECK_GB_CALL(infogrb, GrB_Matrix_setElement(*((*p)->edges), -1, i_pt1, i))
                //p.edges[nz_ej[indices_sorted[1]] + former_nb_pts, i] = 1
                CHECK_GB_CALL(infogrb, GrB_Vector_extractElement(&k, nz_sub_ej, *get_ith_elem_vec_uint(indices_sorted, 0)))
                k += former_nb_pts;
                CHECK_GB_CALL(infogrb, GrB_Matrix_setElement(*((*p)->edges), 1, k, i))
            
                for (j = 1; j<indices_sorted->size-1 ; j += 2){
                    index_new_edge = j/2;
                    //new_edges[nz_ej[indices_sorted[j]],   index_new_edge] = -1
                    CHECK_GB_CALL(infogrb, GrB_Vector_extractElement(&k, nz_sub_ej, *get_ith_elem_vec_uint(indices_sorted, j)))
                    k += former_nb_pts;
                    CHECK_GB_CALL(infogrb, GrB_Matrix_setElement(new_edges, -1, k, index_new_edge))
                    //new_edges[nz_ej[indices_sorted[j+1]], index_new_edge] =  1
                    CHECK_GB_CALL(infogrb, GrB_Vector_extractElement(&k, nz_sub_ej, *get_ith_elem_vec_uint(indices_sorted, j+1)))
                    k += former_nb_pts;
                    CHECK_GB_CALL(infogrb, GrB_Matrix_setElement(new_edges, 1, k, index_new_edge))
                }
            } else {
                for (j = 0; j<indices_sorted->size-1 ; j += 2){
                    index_new_edge = j/2;
                    //new_edges[nz_ej[indices_sorted[j]],   index_new_edge] = -1
                    CHECK_GB_CALL(infogrb, GrB_Vector_extractElement(&k, nz_sub_ej, *get_ith_elem_vec_uint(indices_sorted, j)))
                    k += former_nb_pts;
                    CHECK_GB_CALL(infogrb, GrB_Matrix_setElement(new_edges, -1, k, index_new_edge))
                    //new_edges[nz_ej[indices_sorted[j+1]], index_new_edge] =  1
                    CHECK_GB_CALL(infogrb, GrB_Vector_extractElement(&k, nz_sub_ej, *get_ith_elem_vec_uint(indices_sorted, j+1)))
                    k += former_nb_pts;
                    CHECK_GB_CALL(infogrb, GrB_Matrix_setElement(new_edges, 1, k, index_new_edge))
                }
            }

            if (pt_in_or_out[i_pt2]>0){
                index_new_edge = indices_sorted->size/2-1;
                //new_edges[nz_ej[indices_sorted[end]], end] = -1
                CHECK_GB_CALL(infogrb, GrB_Vector_extractElement(&k, nz_sub_ej, *get_ith_elem_vec_uint(indices_sorted, indices_sorted->size-1)))
                k += former_nb_pts;
                CHECK_GB_CALL(infogrb, GrB_Matrix_setElement(new_edges, -1, k, index_new_edge))
                //new_edges[i_pt2, end] = 1
                CHECK_GB_CALL(infogrb, GrB_Matrix_setElement(new_edges, 1, i_pt2, index_new_edge))
            }

            //p.edges = [p.edges new_edges]
            CHECK_GB_CALL(infogrb, GrB_Matrix_dup(&copy_mat, *((*p)->edges)))
            CHECK_GB_CALL(infogrb, GrB_Matrix_ncols(&nb_cols, copy_mat))
            CHECK_GB_CALL(infogrb, GrB_Matrix_nrows(&nb_rows, copy_mat))
            CHECK_GB_CALL(infogrb, GrB_Matrix_resize(*((*p)->edges), nb_rows, nb_cols + indices_sorted->size/2))
            CHECK_GB_CALL(infogrb, GxB_Matrix_concat(*((*p)->edges), (GrB_Matrix[]){copy_mat, new_edges}, 1, 2, GrB_NULL))
            for(j=0; j<indices_sorted->size/2; j++){
                push_back_vec_int(&((*p)->status_edge), get_ith_elem_vec_int((*p)->status_edge, i));
            }

            //new_faces = spzeros(Int8, div(length(indices_sorted), 2), former_nb_faces)
            CHECK_GB_CALL(infogrb, GrB_Matrix_resize(new_faces, indices_sorted->size/2, former_nb_faces))
            CHECK_GB_CALL(infogrb, GrB_Matrix_clear(new_faces))
            CHECK_GB_CALL(infogrb, GrB_assign(new_faces, GrB_NULL, GrB_NULL, orient_edge, GrB_ALL, 1, &i_face1, 1, GrB_NULL))
            //p.faces = [p.faces; new_faces]
            CHECK_GB_CALL(infogrb, GrB_Matrix_dup(&copy_mat, *((*p)->faces)))
            CHECK_GB_CALL(infogrb, GrB_Matrix_ncols(&nb_cols, copy_mat))
            CHECK_GB_CALL(infogrb, GrB_Matrix_nrows(&nb_rows, copy_mat))
            CHECK_GB_CALL(infogrb, GrB_Matrix_resize(*((*p)->faces), nb_rows + indices_sorted->size/2, nb_cols))
            CHECK_GB_CALL(infogrb, GxB_Matrix_concat(*((*p)->faces), (GrB_Matrix[]){copy_mat, new_faces}, 2, 1, GrB_NULL))
        }
    }

    GrB_free(&f_k);
    GrB_free(&new_pt);
    GrB_free(&pot_ind_faces);
    GrB_free(&I_vec_f_k);
    GrB_free(&extr_vals_f_k);
    GrB_free(&ej);
    GrB_free(&nz_ej);
    GrB_free(&extr_vals_ej);
    GrB_free(&ej_orig);
    GrB_free(&nz_ej_orig);
    GrB_free(&extr_vals_ej_orig);
    GrB_free(&sub_ej);
    GrB_free(&nz_sub_ej);
    GrB_free(&extr_vals_sub_ej);
    GrB_free(&new_edges);
    GrB_free(&new_faces);
    GrB_free(&copy_mat);
    dealloc_vec_double(thetas); free(thetas);
    dealloc_vec_uint(indices_sorted); free(indices_sorted);
}

/// @brief Split some faces into two faces.
/// @param old_p [IN] Polygon with faces to split.
/// @param faces_to_split [IN] Array of size number of faces, each cell of this array being a set of edge indices intersecting and owned by this face.
/// @param size_faces_to_split [IN] Size of `faces_to_split`.
/// @param result_p [OUT] Result polygon with some faces split in two.
void create_new_faces_split(const Polygon2D* old_p, Vector_uint** faces_to_split, uint64_t size_faces_to_split, Polygon2D **result_p){
    uint64_t i, j, k, ie, i_pts1, i_pts2;
    const Vector_uint* s;
    Polygon2D* p; 
    GrB_Vector ej, nz_ej, extr_vals_ej;
    GrB_Vector old_face, new_face;
    GrB_Index nb_pts, nb_edges, nb_faces, nb_rows_copy, nb_cols_copy;
    int64_t sum_col = 0;
    Vector_uint* listEdges, *listNodes, *forbidden_edges;
    GrB_Matrix concat_face, copy_mat;
    long int zero = 0;
    Polygon2D* extr_p;
    GrB_Vector surf_vect;
    my_real_c surf;
    GrB_Info infogrb;
    int8_t val;
    uint64_t lek;

    if (old_p != *result_p)
        *result_p = new_Polygon2D_vefsp(old_p->vertices, old_p->edges, old_p->faces, old_p->status_edge, old_p->phase_face);

    p  = *result_p;

    GrB_Matrix_nrows(&nb_pts, *(p->edges));
    GrB_Matrix_ncols(&nb_edges, *(p->edges));
    GrB_Vector_new(&ej, GrB_INT8, nb_pts);
    GrB_Vector_new(&nz_ej, GrB_UINT64, nb_pts);
    GrB_Vector_new(&extr_vals_ej, GrB_INT8, nb_pts);
    GrB_Vector_new(&old_face, GrB_INT8, nb_edges);
    GrB_Vector_new(&new_face, GrB_INT64, nb_edges);
    GrB_Matrix_new(&concat_face, GrB_INT8, nb_edges, 1);
    GrB_Matrix_new(&copy_mat, GrB_INT8, 1, 1);
    listEdges = alloc_empty_vec_uint();
    listNodes = alloc_empty_vec_uint();
    forbidden_edges = alloc_with_capacity_vec_uint(1);
    forbidden_edges->size = 1;

    //Finally: Create new faces
    for (i=0; i<size_faces_to_split; i++){
        s = faces_to_split[i];
        if (s->size != 0){ //face i is split
            for(j=0; j<s->size; j++){
                ie = *get_ith_elem_vec_uint(s, j);
                CHECK_GB_CALL(infogrb, GrB_extract(ej, GrB_NULL, GrB_NULL, *(p->edges), GrB_ALL, 1, ie, GrB_NULL)) 
                CHECK_GB_CALL(infogrb, GxB_Vector_extractTuples_Vector(nz_ej, extr_vals_ej, ej, GrB_NULL))
                CHECK_GB_CALL(infogrb, GrB_Vector_extractElement(&i_pts1, nz_ej, 0))
                CHECK_GB_CALL(infogrb, GrB_Vector_extractElement(&i_pts2, nz_ej, 1))
                
                set_ith_elem_vec_uint(forbidden_edges, 0, &ie);

                astar(p->vertices, p->edges, i_pts1, i_pts2, forbidden_edges, listEdges, listNodes);
                push_back_vec_uint(&(listEdges), &ie);
                sort_vec_uint(listEdges);

                //old_face = p.faces[:, i]
                CHECK_GB_CALL(infogrb, GrB_extract(old_face, GrB_NULL, GrB_NULL, *(p->faces), GrB_ALL, 1, i, GrB_NULL)) 
                //old_face[listEdges] .= 0
                for(k=0; k<listEdges->size; k++){
                    CHECK_GB_CALL(infogrb, GrB_Vector_removeElement(old_face, *get_ith_elem_vec_uint(listEdges, k)))
                }
                CHECK_GB_CALL(infogrb, GrB_apply(new_face, GrB_NULL, GrB_NULL, GrB_ABS_INT8, old_face, GrB_NULL)) //abs on each component
                CHECK_GB_CALL(infogrb, GrB_reduce(&sum_col, GrB_NULL, GrB_PLUS_MONOID_INT64, new_face, GrB_NULL))
                if (sum_col != 0){
                    GrB_Vector_clear(new_face);
                    //new_face[listEdges] .= p.faces[listEdges, i]
                    for(k=0; k<listEdges->size; k++){
                        lek = *get_ith_elem_vec_uint(listEdges, k);
                        infogrb = GxB_Matrix_isStoredElement(*(p->faces), lek, i);
                        if(infogrb != GrB_NO_VALUE){
                            CHECK_GB_CALL(infogrb, GrB_Matrix_extractElement(&val, *(p->faces), lek, i))
                            CHECK_GB_CALL(infogrb, GrB_Vector_setElement(new_face, val, lek))
                            CHECK_GB_CALL(infogrb, GrB_Matrix_removeElement(*(p->faces), lek, i))
                        }
                    }

                    CHECK_GB_CALL(infogrb, GrB_apply(old_face, GrB_NULL, GrB_NULL, GrB_ABS_INT8, new_face, GrB_NULL)) //abs on each component
                    CHECK_GB_CALL(infogrb, GrB_reduce(&sum_col, GrB_NULL, GrB_PLUS_MONOID_INT64, old_face, GrB_NULL))
                    if (sum_col != 0){
                        //p.faces = [p.faces new_face]
                        CHECK_GB_CALL(infogrb, GrB_Matrix_clear(concat_face))
                        CHECK_GB_CALL(infogrb, GrB_Col_assign(concat_face, GrB_NULL, GrB_NULL, new_face, GrB_ALL, 1, 0, GrB_NULL))
                        CHECK_GB_CALL(infogrb, GrB_Matrix_dup(&copy_mat, *(p->faces)))
                        CHECK_GB_CALL(infogrb, GrB_Matrix_ncols(&nb_cols_copy, copy_mat))
                        CHECK_GB_CALL(infogrb, GrB_Matrix_nrows(&nb_rows_copy, copy_mat))
                        CHECK_GB_CALL(infogrb, GrB_Matrix_resize(*(p->faces), nb_rows_copy, nb_cols_copy + 1))
                        CHECK_GB_CALL(infogrb, GxB_Matrix_concat(*(p->faces), (GrB_Matrix[]){copy_mat, concat_face}, 1, 2, GrB_NULL))

                        CHECK_GB_CALL(infogrb, GrB_Matrix_ncols(&nb_faces, *(p->faces)))
                        push_back_vec_int(&(p->phase_face), &zero);
                        extr_p = extract_ith_face2D(p, nb_faces-1);
                        surf_vect = surfaces_poly2D(extr_p);
                        CHECK_GB_CALL(infogrb, GrB_Vector_extractElement(&surf, surf_vect, 0))
                        p->phase_face->data[nb_faces-1] = (surf>0 ? 1 : -1) * labs(p->phase_face->data[i]);
                        GrB_free(&surf_vect);
                        dealloc_Polygon2D(extr_p); free(extr_p);

                        extr_p = extract_ith_face2D(p, i);
                        surf_vect = surfaces_poly2D(extr_p);
                        CHECK_GB_CALL(infogrb, GrB_Vector_extractElement(&surf, surf_vect, 0))
                        p->phase_face->data[nb_faces-1] = (surf>0 ? 1 : -1) * labs(p->phase_face->data[i]);
                        GrB_free(&surf_vect);
                        dealloc_Polygon2D(extr_p); free(extr_p);
                    }
                }
            }
        }
    }

    GrB_free(&ej);
    GrB_free(&nz_ej);
    GrB_free(&extr_vals_ej);
    GrB_free(&old_face);
    GrB_free(&new_face);
    GrB_free(&concat_face);
    GrB_free(&copy_mat);
    dealloc_vec_uint(listEdges); free(listEdges);
    dealloc_vec_uint(listNodes); free(listNodes);
    dealloc_vec_uint(forbidden_edges); free(forbidden_edges);
}

/// @brief Solves an auto-intersection by splitting faces starting to form two faces. Normally used after `break_edges_split_fusion`.
/// @param old_p [IN] Polygon with intersections and broken edges.
/// @param original_edges [IN] Matrix of the edges before being broken
/// @param pts_intersec [IN] List of intersection points
/// @param edge_intersect1 [IN] Edge intersecting (1st edge)
/// @param edge_intersect2 [IN] Edge intersecting (2nd edge)
/// @param pt_in_or_out [IN] Array noting if a point is not concerned by an intersection (0), concerned but outside the intersection region (1) or inside the intersection region (-1) and should be suppressed.
/// @param p [OUT] Result polygon, with the edges modifed to correct the intersection.
/// @param faces_to_split [OUT] Array of size number of faces, each cell of this array being a set of edge indices intersecting and owned by this face.
/// @param size_faces_to_split [OUT] Size of `faces_to_split`.
void polygon_split(const Polygon2D* old_p, const GrB_Matrix* original_edges, const Vector_points2D* pts_intersec, const Vector_uint* edge_intersect1, const Vector_uint* edge_intersect2, const int8_t* pt_in_or_out,\
                    Polygon2D** res_p, Vector_uint*** faces_to_split, uint64_t *size_faces_to_split){
    uint64_t former_nb_pts, former_nb_edges, former_nb_faces;
    uint64_t i, j, k, ie1, ie2, index_new_edge;
    uint64_t i_face, i_pt1, i_pt2;
    GrB_Vector ej, pot_ind_faces, I_vec_f_k, extr_vals_f_k, nz_ej, extr_vals_ej;
    GrB_Vector ej_orig, nz_ej_orig, extr_vals_ej_orig;
    GrB_Vector sub_ej, nz_sub_ej, extr_vals_sub_ej;
    GrB_Matrix f_k, new_pt, new_edges, new_faces, copy_mat;
    int8_t sum_col1, sum_col2, orient_edge, val;
    GrB_Index nvals, nb_edges, nb_pts, orig_pts1, orig_pts2, size_nz_sub_ej;
    GrB_Index nb_rows_copy, nb_cols_copy;
    GrB_Index I_ind[2];
    Point2D *pt1, *pt2, *pt;
    Vector_double* thetas;
    Vector_uint *indices_sorted;
    my_real_c res;
    GrB_Info infogrb;
    Polygon2D *p;

    if (old_p != *res_p){
        *res_p = new_Polygon2D_vefsp(old_p->vertices, old_p->edges, old_p->faces, old_p->status_edge, old_p->phase_face);
    }
    p = *res_p;

    GrB_Matrix_nrows(&former_nb_pts, *original_edges);
    GrB_Vector_new(&ej_orig, GrB_INT8, former_nb_pts);
    GrB_Vector_new(&nz_ej_orig, GrB_UINT64, former_nb_pts);
    GrB_Vector_new(&extr_vals_ej_orig, GrB_INT8, former_nb_pts);

    former_nb_pts = p->vertices->size;
    GrB_Matrix_ncols(&former_nb_edges, *(p->edges));
    GrB_Matrix_ncols(&former_nb_faces, *(p->faces));

    for(i=0; i<pts_intersec->size; i++){
        push_back_vec_pts2D(&(p->vertices), get_ith_elem_vec_pts2D(pts_intersec, i));
    } 

    GrB_Matrix_new(&f_k, GrB_INT8, 1, former_nb_faces);
    GrB_Matrix_new(&new_pt, GrB_INT8, edge_intersect1->size, former_nb_edges);
    GrB_Vector_new(&ej, GrB_INT8, former_nb_pts);
    GrB_Vector_new(&nz_ej, GrB_UINT64, former_nb_pts);
    GrB_Vector_new(&extr_vals_ej, GrB_INT8, former_nb_pts);
    GrB_Vector_new(&pot_ind_faces, GrB_UINT64, 1);
    GrB_Vector_new(&I_vec_f_k, GrB_UINT64, 1);
    GrB_Vector_new(&extr_vals_f_k, GrB_INT8, 1);
    GrB_Matrix_new(&copy_mat, GrB_INT8, 1, 1);
    thetas = alloc_empty_vec_double();
    indices_sorted = alloc_empty_vec_uint();

    *size_faces_to_split = former_nb_faces;
    *faces_to_split = (Vector_uint**)malloc(former_nb_faces*sizeof(Vector_uint*));
    for(i=0; i<former_nb_faces; i++){
        (*faces_to_split)[i] = alloc_empty_vec_uint();
    }

    //Second: rebuild the edges by connecting the intersection points.
    for(i=0; i<edge_intersect1->size; i++){
        ie1 = *get_ith_elem_vec_uint(edge_intersect1, i);//edge_intersect1[i]
        ie2 = *get_ith_elem_vec_uint(edge_intersect2, i);//edge_intersect2[i]
            
        //i_face = findfirst(x->x!=0, p.faces[ie1, :])
        CHECK_GB_CALL(infogrb, GrB_extract(f_k, GrB_NULL, GrB_NULL, *(p->faces), &ie1, 1, GrB_ALL, 1, GrB_NULL)) 
        CHECK_GB_CALL(infogrb, GrB_Matrix_nvals(&nvals, f_k))
        CHECK_GB_CALL(infogrb, GrB_Vector_resize(pot_ind_faces, nvals))
        CHECK_GB_CALL(infogrb, GrB_Vector_resize(I_vec_f_k, nvals))
        CHECK_GB_CALL(infogrb, GrB_Vector_resize(extr_vals_f_k, nvals))
        CHECK_GB_CALL(infogrb, GxB_Matrix_extractTuples_Vector(I_vec_f_k, pot_ind_faces, extr_vals_f_k, f_k, GrB_NULL))
        CHECK_GB_CALL(infogrb, GrB_Vector_extractElement(&i_face, pot_ind_faces, 0))

        push_back_vec_uint(&((*faces_to_split)[i_face]), &ie1);
        push_back_vec_uint(&((*faces_to_split)[i_face]), &ie2);

        //sum_col1 = sum(p.edges[:, ie1])
        CHECK_GB_CALL(infogrb, GrB_extract(ej, GrB_NULL, GrB_NULL, *(p->edges), GrB_ALL, 1, ie1, GrB_NULL)) 
        CHECK_GB_CALL(infogrb, GrB_reduce(&sum_col1, GrB_NULL, GrB_PLUS_MONOID_INT8, ej, GrB_NULL))
        //sum_col2 = sum(p.edges[:, ie2])
        CHECK_GB_CALL(infogrb, GrB_extract(ej, GrB_NULL, GrB_NULL, *(p->edges), GrB_ALL, 1, ie2, GrB_NULL)) 
        CHECK_GB_CALL(infogrb, GrB_reduce(&sum_col2, GrB_NULL, GrB_PLUS_MONOID_INT8, ej, GrB_NULL))

        if (sum_col1!=0){
            CHECK_GB_CALL(infogrb, GrB_Matrix_setElement(new_pt, -sum_col1/abs(sum_col1), i, ie1))
            CHECK_GB_CALL(infogrb, GrB_Matrix_setElement(new_pt, sum_col1/abs(sum_col1), i, ie2))
        } else if (sum_col2!=0) {
            CHECK_GB_CALL(infogrb, GrB_Matrix_setElement(new_pt, -sum_col2/abs(sum_col2), i, ie2))
            CHECK_GB_CALL(infogrb, GrB_Matrix_setElement(new_pt, sum_col2/abs(sum_col2), i, ie1))
        } else { //edges ie1 and ie2 intersect but none are broken: what should we do?
            CHECK_GB_CALL(infogrb, GrB_Matrix_setElement(new_pt, 1, i, ie1))
            CHECK_GB_CALL(infogrb, GrB_Matrix_setElement(new_pt, 1, i, ie2))
        }
    }

    GrB_Matrix_dup(&copy_mat, *(p->edges));
    GrB_Matrix_ncols(&nb_cols_copy, copy_mat);
    GrB_Matrix_nrows(&nb_rows_copy, copy_mat);
    GrB_Matrix_resize(*(p->edges), nb_rows_copy + edge_intersect1->size, nb_cols_copy);
    CHECK_GB_CALL(infogrb, GxB_Matrix_concat(*(p->edges), (GrB_Matrix[]){copy_mat, new_pt}, 2, 1, GrB_NULL))

    //Third: rebuild intermediate edges in case one edge was having multiple intersection points.
    GrB_Matrix_ncols(&nb_edges, *(p->edges));
    GrB_Matrix_nrows(&nb_pts, *(p->edges));
    GrB_Vector_resize(ej, nb_pts);
    GrB_Matrix_new(&new_edges, GrB_INT8, nb_pts, 2);
    GrB_Matrix_new(&new_faces, GrB_INT8, 2, former_nb_faces);
    I_ind[GxB_BEGIN] = former_nb_pts;
    I_ind[GxB_END] = nb_pts-1;
    GrB_Vector_new(&sub_ej, GrB_INT8, nb_pts-former_nb_pts);
    GrB_Vector_new(&nz_sub_ej, GrB_UINT64, nb_pts-former_nb_pts);
    GrB_Vector_new(&extr_vals_sub_ej, GrB_INT8, nb_pts-former_nb_pts);
    for(i=0; i<nb_edges; i++){
        CHECK_GB_CALL(infogrb, GrB_extract(ej, GrB_NULL, GrB_NULL, *(p->edges), GrB_ALL, 1, i, GrB_NULL)) 
        CHECK_GB_CALL(infogrb, GrB_apply(ej, GrB_NULL, GrB_NULL, GrB_ABS_INT8, ej, GrB_DESC_R)) //abs on each component
        CHECK_GB_CALL(infogrb, GrB_reduce(&sum_col1, GrB_NULL, GrB_PLUS_MONOID_INT8, ej, GrB_NULL))
        if (sum_col1>2){
            //i_face1 = findfirst(x->x!=0, p.faces[i, :])
            CHECK_GB_CALL(infogrb, GrB_extract(f_k, GrB_NULL, GrB_NULL, *(p->faces), &i, 1, GrB_ALL, 1, GrB_NULL)) 
            CHECK_GB_CALL(infogrb, GrB_Matrix_nvals(&nvals, f_k))
            CHECK_GB_CALL(infogrb, GrB_Vector_resize(pot_ind_faces, nvals))
            CHECK_GB_CALL(infogrb, GrB_Vector_resize(I_vec_f_k, nvals))
            CHECK_GB_CALL(infogrb, GrB_Vector_resize(extr_vals_f_k, nvals))
            CHECK_GB_CALL(infogrb, GxB_Matrix_extractTuples_Vector(I_vec_f_k, pot_ind_faces, extr_vals_f_k, f_k, GrB_NULL))
            CHECK_GB_CALL(infogrb, GrB_Vector_extractElement(&i_face, pot_ind_faces, 0))

            CHECK_GB_CALL(infogrb, GrB_Matrix_extractElement(&orient_edge, *(p->faces), i, i_face))

            //ej = p.edges[0:former_nb_pts-1, i]
            CHECK_GB_CALL(infogrb, GrB_extract(ej_orig, GrB_NULL, GrB_NULL, *original_edges, GrB_ALL, 1, i, GrB_NULL)) 
            CHECK_GB_CALL(infogrb, GxB_Vector_extractTuples_Vector(nz_ej_orig, extr_vals_ej_orig, ej_orig, GrB_NULL))
            CHECK_GB_CALL(infogrb, GrB_Vector_extractElement(&orig_pts1, nz_ej_orig, 0))
            CHECK_GB_CALL(infogrb, GrB_Vector_extractElement(&orig_pts2, nz_ej_orig, 1))

            CHECK_GB_CALL(infogrb, GrB_Matrix_extractElement(&val, *original_edges, orig_pts1, i))

            if (val<0){
                i_pt1 = orig_pts1;
                i_pt2 = orig_pts2;
            } else {
                i_pt1 = orig_pts2;
                i_pt2 = orig_pts1;
            }
            pt1 = get_ith_elem_vec_pts2D(p->vertices, i_pt1);
            pt2 = get_ith_elem_vec_pts2D(p->vertices, i_pt2);
            
            //ej = p.edges[former_nb_pts:end, i]
            CHECK_GB_CALL(infogrb, GrB_extract(sub_ej, GrB_NULL, GrB_NULL, *(p->edges), I_ind, GxB_RANGE, i, GrB_NULL)) 
            CHECK_GB_CALL(infogrb, GxB_Vector_extractTuples_Vector(nz_sub_ej, extr_vals_sub_ej, sub_ej, GrB_NULL))
            CHECK_GB_CALL(infogrb, GrB_Vector_size(&size_nz_sub_ej, nz_sub_ej))
            thetas->size = 0;
            for(j=0; j<size_nz_sub_ej; j++){
                CHECK_GB_CALL(infogrb, GrB_Vector_extractElement(&k, nz_sub_ej, j))
                k += former_nb_pts;
                pt = get_ith_elem_vec_pts2D(p->vertices, k);
                res = compute_barycentric_coord(*pt, *pt1, *pt2);
                push_back_vec_double(&thetas, &res);
            }

            sort_vec_double(thetas, thetas, indices_sorted);
            
            //new_edges = spzeros(Int8, size(p.edges, 1), div(length(indices_sorted),2))
            CHECK_GB_CALL(infogrb, GrB_Matrix_resize(new_edges, nb_pts, indices_sorted->size/2))
            CHECK_GB_CALL(infogrb, GrB_Matrix_clear(new_edges))
            for(j=0; j<nb_pts; j++){
                CHECK_GB_CALL(infogrb, GrB_Matrix_removeElement(*(p->edges), j, i))
            }
            if (pt_in_or_out[i_pt1]>0){
                //p.edges[i_pt1, i] = -1
                CHECK_GB_CALL(infogrb, GrB_Matrix_setElement(*(p->edges), -1, i_pt1, i))
                //p.edges[nz_ej[indices_sorted[1]] + former_nb_pts, i] = 1
                CHECK_GB_CALL(infogrb, GrB_Vector_extractElement(&k, nz_sub_ej, *get_ith_elem_vec_uint(indices_sorted, 0)))
                k += former_nb_pts;
                CHECK_GB_CALL(infogrb, GrB_Matrix_setElement(*(p->edges), 1, k, i))
            
                for (j = 1; j<indices_sorted->size-1 ; j += 2){
                    index_new_edge = j/2;
                    //new_edges[nz_ej[indices_sorted[j]],   index_new_edge] = -1
                    CHECK_GB_CALL(infogrb, GrB_Vector_extractElement(&k, nz_sub_ej, *get_ith_elem_vec_uint(indices_sorted, j)))
                    k += former_nb_pts;
                    CHECK_GB_CALL(infogrb, GrB_Matrix_setElement(new_edges, -1, k, index_new_edge))
                    //new_edges[nz_sub_ej[indices_sorted[j+1]], index_new_edge] =  1
                    CHECK_GB_CALL(infogrb, GrB_Vector_extractElement(&k, nz_sub_ej, *get_ith_elem_vec_uint(indices_sorted, j+1)))
                    k += former_nb_pts;
                    CHECK_GB_CALL(infogrb, GrB_Matrix_setElement(new_edges, 1, k, index_new_edge))
                }
            } else {
                for (j = 0; j<indices_sorted->size-1 ; j += 2){
                    index_new_edge = j/2;
                    //new_edges[nz_ej[indices_sorted[j]],   index_new_edge] = -1
                    CHECK_GB_CALL(infogrb, GrB_Vector_extractElement(&k, nz_sub_ej, *get_ith_elem_vec_uint(indices_sorted, j)))
                    k += former_nb_pts;
                    CHECK_GB_CALL(infogrb, GrB_Matrix_setElement(new_edges, -1, k, index_new_edge))
                    //new_edges[nz_sub_ej[indices_sorted[j+1]], index_new_edge] =  1
                    CHECK_GB_CALL(infogrb, GrB_Vector_extractElement(&k, nz_sub_ej, *get_ith_elem_vec_uint(indices_sorted, j+1)))
                    k += former_nb_pts;
                    CHECK_GB_CALL(infogrb, GrB_Matrix_setElement(new_edges, 1, k, index_new_edge))
                }
            }

            if (pt_in_or_out[i_pt2]>0){
                index_new_edge = indices_sorted->size/2-1;
                //new_edges[nz_sub_ej[indices_sorted[end]], end] = -1
                CHECK_GB_CALL(infogrb, GrB_Vector_extractElement(&k, nz_sub_ej, *get_ith_elem_vec_uint(indices_sorted, indices_sorted->size-1)))
                k += former_nb_pts;
                CHECK_GB_CALL(infogrb, GrB_Matrix_setElement(new_edges, -1, k, index_new_edge))
                //new_edges[i_pt2, end] = 1
                CHECK_GB_CALL(infogrb, GrB_Matrix_setElement(new_edges, 1, i_pt2, index_new_edge))
            }

            //p.edges = [p.edges new_edges]
            CHECK_GB_CALL(infogrb, GrB_Matrix_dup(&copy_mat, *(p->edges)))
            CHECK_GB_CALL(infogrb, GrB_Matrix_ncols(&nb_cols_copy, copy_mat))
            CHECK_GB_CALL(infogrb, GrB_Matrix_nrows(&nb_rows_copy, copy_mat))
            CHECK_GB_CALL(infogrb, GrB_Matrix_resize(*(p->edges), nb_rows_copy, nb_cols_copy + indices_sorted->size/2))
            CHECK_GB_CALL(infogrb, GxB_Matrix_concat(*(p->edges), (GrB_Matrix[]){copy_mat, new_edges}, 1, 2, GrB_NULL))
            for(j=0; j<indices_sorted->size/2; j++){
                push_back_vec_int(&(p->status_edge), get_ith_elem_vec_int(p->status_edge, i));
            }

            //new_faces = spzeros(Int8, div(length(indices_sorted), 2), former_nb_faces)
            CHECK_GB_CALL(infogrb, GrB_Matrix_resize(new_faces, indices_sorted->size/2, former_nb_faces))
            CHECK_GB_CALL(infogrb, GrB_Matrix_clear(new_faces))
            //CHECK_GB_CALL(infogrb, GrB_assign(new_faces, GrB_NULL, GrB_NULL, orient_edge, GrB_ALL, 1, &i_face, 1, GrB_NULL))
            for(j=0; j<indices_sorted->size/2; j++){
                CHECK_GB_CALL(infogrb, GrB_Matrix_setElement(new_faces, orient_edge, j, i_face))
            }
            //p.faces = [p.faces; new_faces]
            CHECK_GB_CALL(infogrb, GrB_Matrix_dup(&copy_mat, *(p->faces)))
            CHECK_GB_CALL(infogrb, GrB_Matrix_ncols(&nb_cols_copy, copy_mat))
            CHECK_GB_CALL(infogrb, GrB_Matrix_nrows(&nb_rows_copy, copy_mat))
            CHECK_GB_CALL(infogrb, GrB_Matrix_resize(*(p->faces), nb_rows_copy + indices_sorted->size/2, nb_cols_copy))
            CHECK_GB_CALL(infogrb, GxB_Matrix_concat(*(p->faces), (GrB_Matrix[]){copy_mat, new_faces}, 2, 1, GrB_NULL))
        }
    }

    GrB_free(&f_k);
    GrB_free(&new_pt);
    GrB_free(&pot_ind_faces);
    GrB_free(&I_vec_f_k);
    GrB_free(&extr_vals_f_k);
    GrB_free(&ej);
    GrB_free(&nz_ej);
    GrB_free(&extr_vals_ej);
    GrB_free(&ej_orig);
    GrB_free(&nz_ej_orig);
    GrB_free(&extr_vals_ej_orig);
    GrB_free(&sub_ej);
    GrB_free(&nz_sub_ej);
    GrB_free(&extr_vals_sub_ej);
    GrB_free(&new_edges);
    GrB_free(&new_faces);
    GrB_free(&copy_mat);
    dealloc_vec_double(thetas); free(thetas);
    dealloc_vec_uint(indices_sorted); free(indices_sorted);
}

/*
function polygon_eliminate_auto_intersections(p::Polygon2D)
    _, normalVec, _ = compute_all_normals(p)
    IntersecList, edge_intersect1, edge_intersect2 = find_all_self_intersection(p)
    if !isempty(IntersecList)
        pt_in_or_out_split, IntersecList_split, edge_intersect1_split, edge_intersect2_split, 
                pt_in_or_out_fuse, IntersecList_fuse, edge_intersect1_fuse, edge_intersect2_fuse = in_or_out_intersection(p, normalVec, IntersecList, edge_intersect1, edge_intersect2)
        if !isempty(IntersecList_split)
            println("Splitting done")
            new_p, pair_intersection_split = break_edges_split_fusion(p, edge_intersect1_split, edge_intersect2_split, pt_in_or_out_split)
            new_p, faces_to_split = polygon_split(new_p, IntersecList_split, edge_intersect1_split, edge_intersect2_split)
            new_p = create_new_faces_split(new_p, faces_to_split)
        else
            new_p = p
            pair_intersection_split = repeat(Int8.([-1]), length(p.vertices))
        end
        if !isempty(IntersecList_fuse)
            println("Fusion done")
            new_p, pair_intersection_fuse = break_edges_split_fusion(p, edge_intersect1_fuse, edge_intersect2_fuse, pt_in_or_out_fuse)
            new_p, faces_to_fuse = polygons_fusion(new_p, IntersecList_fuse, edge_intersect1_fuse, edge_intersect2_fuse, pt_in_or_out_fuse)
            new_p = fuse_faces(new_p, faces_to_fuse)
        end
    else
        new_p = p
        pt_in_or_out_split = repeat(Int8.([0]), length(p.vertices))
        pt_in_or_out_fuse = repeat(Int8.([0]), length(p.vertices))
        pair_intersection_split = repeat(Int8.([-1]), length(p.vertices))
        pair_intersection_fuse = repeat(Int8.([-1]), length(p.vertices))
        #IntersecList_split = Vector{Point2D}()
        #IntersecList_fuse = Vector{Point2D}()
        #edge_intersect1_split = Vector{Int}()
        #edge_intersect2_split = Vector{Int}()
        #edge_intersect1_fuse = Vector{Int}()
        #edge_intersect2_fuse = Vector{Int}()
    end

    return new_p, pair_intersection_split, pair_intersection_fuse
                pt_in_or_out_split,# IntersecList_split, edge_intersect1_split, edge_intersect2_split, 
                pt_in_or_out_fuse#, IntersecList_fuse, edge_intersect1_fuse, edge_intersect2_fuse
end
*/