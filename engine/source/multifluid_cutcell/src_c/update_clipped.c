#include "update_clipped.h"
#include <string.h>
#include "edge_collision.h"
#include "polygon_fusion_and_split.h"
#include "AStar.h"
#include <stdlib.h>
#include "macro_check_gb_call.h"

//Project d in [min, max].
static my_real_c clamp(my_real_c d, my_real_c min, my_real_c max) {
  const my_real_c t = d < min ? min : d;
  return t > max ? max : t;
}

//Compute the angle formed by the two segments [e1_ext1, e1_ext2] and [e2_ext1, e2_ext2]
static my_real_c compute_angle_between_edges2D(Point2D *e1_ext1, Point2D *e1_ext2, Point2D *e2_ext1, Point2D *e2_ext2){
        my_real_c n1x, n1y, n2x, n2y;
        my_real_c dotp, norm_n1, norm_n2;
        if ((e1_ext1->x == e2_ext1->x) && (e1_ext1->y == e2_ext1->y)){
            n1x = e1_ext2->x - e1_ext1->x;
            n1y = e1_ext2->y - e1_ext1->y;
            n2x = e2_ext2->x - e2_ext1->x;
            n2y = e2_ext2->y - e2_ext1->y;
        } else if ((e1_ext1->x == e2_ext2->x) && (e1_ext1->y == e2_ext2->y)) {
            n1x = e1_ext2->x - e1_ext1->x;
            n1y = e1_ext2->y - e1_ext1->y;
            n2x = e2_ext1->x - e2_ext2->x;
            n2y = e2_ext1->y - e2_ext2->y;
        } else if (e1_ext2 == e2_ext1){
            n1x = e1_ext1->x - e1_ext2->x;
            n1y = e1_ext1->y - e1_ext2->y;
            n2x = e2_ext2->x - e2_ext1->x;
            n2y = e2_ext2->y - e2_ext1->y;
        } else { //e1_ext2 == e2_ext2
            n1x = e1_ext1->x - e1_ext2->x;
            n1y = e1_ext1->y - e1_ext2->y;
            n2x = e2_ext1->x - e2_ext2->x;
            n2y = e2_ext1->y - e2_ext2->y;
        }

        dotp = n1x*n2x + n1y*n2y;
        norm_n1 = sqrt(n1x*n1x + n1y*n1y);
        norm_n2 = sqrt(n2x*n2x + n2y*n2y);

    #if (my_real_c == double)
        return acos(clamp(dotp/(norm_n1*norm_n2), -1.0, 1.0));
    #else
       return acosf(clamp(dotp/(norm_n1*norm_n2), -1.0, 1.0));
    #endif
}

/*
list_del_pts : array of size (nb_pts x 2). For each line, indicates if a point must be deleted or not.
    - if -1 : should not be deleted.
    - otherwise : should be deleted, and the cells of this line register the two other points linked to the one deleted.
list_changed_edges : array of size (nb_pts x 3). Same thing as list_del_pts, but each line registers the two edges connected to this point,
and the face concerned.
*/
static void detect_pts_to_delete(Polygon2D* p, my_real_c dt, const my_real_c *vsx, const my_real_c *vsy, my_real_c minimal_length, my_real_c minimal_angle,\
                                 Array_int **list_del_pts, Array_int **list_changed_edges){
    uint64_t nb_pts = p->vertices->size;
    GrB_Index nb_faces, nb_edges;
    GrB_Index size_nz_fj, size_nz_ej, size_nz_e_ipt0;
    uint64_t i_f, i_e, i_e_fj, i_pt0;
    uint64_t i_e1, i_e0;
    uint64_t i_e0_pt0, i_e0_pt1;
    uint64_t i_e1_pt0, i_e1_pt1;
    int64_t val;
    GrB_Info infogrb;
    GrB_Vector fj, ej;
    GrB_Matrix e_ipt0;
    GxB_Iterator iterator;
    GrB_Vector nz_fj, nz_ej;
    GrB_Vector extr_vals_fj, extr_vals_ej;
    GrB_Vector nz_e_ipt0, extr_vals_e_ipt0, I_vec;
    Point2D *e1_ext1, *e1_ext2, *e2_ext1, *e2_ext2;
    my_real_c alpha, norm_e1, norm_e2;
    GrB_Vector inds_pts_edge0, inds_pts_edge1;
    int64_t ind_pt_del, ind_pt1, ind_pt2, curr_i;
    int64_t signed_ie0, signed_ie1, signed_i_f;
    Vector_int64 *list_inds_del1, *list_inds_del2, *list_inds_del, *set_del_edges;
    int8_t sign_f, sign_e;
    Point2D barycenter;

    GxB_Iterator_new(&iterator) ;

    *list_del_pts = alloc_with_capacity_arr_int(nb_pts, 2);
    *list_changed_edges = alloc_with_capacity_arr_int(nb_pts, 3);

    val = -1;
    for (i_f=0; i_f<nb_pts; i_f++){
        set_ijth_elem_arr_int(*list_del_pts, i_f, 0, &val);
        set_ijth_elem_arr_int(*list_del_pts, i_f, 1, &val);
        set_ijth_elem_arr_int(*list_changed_edges, i_f, 0, &val);
        set_ijth_elem_arr_int(*list_changed_edges, i_f, 1, &val);
        set_ijth_elem_arr_int(*list_changed_edges, i_f, 2, &val);
    }

    GrB_Matrix_ncols(&nb_faces, *(p->faces));
    GrB_Matrix_ncols(&nb_edges, *(p->edges));

    //rows_f = rowvals(p.faces)
    //rows_e = rowvals(p.edges)
    CHECK_GB_CALL(infogrb, GrB_Vector_new(&fj, GrB_INT8, nb_edges))
    CHECK_GB_CALL(infogrb, GrB_Vector_new(&nz_fj, GrB_UINT64, nb_edges))
    CHECK_GB_CALL(infogrb, GrB_Vector_new(&extr_vals_fj, GrB_INT8, nb_edges))
    CHECK_GB_CALL(infogrb, GrB_Vector_new(&ej, GrB_INT8, nb_pts))
    CHECK_GB_CALL(infogrb, GrB_Vector_new(&nz_ej, GrB_UINT64, 2))
    CHECK_GB_CALL(infogrb, GrB_Vector_new(&extr_vals_ej, GrB_INT8, 2))
    CHECK_GB_CALL(infogrb, GrB_Matrix_new(&e_ipt0, GrB_INT8, 1, nb_edges))
    CHECK_GB_CALL(infogrb, GrB_Vector_new(&nz_e_ipt0, GrB_UINT64, nb_edges))
    CHECK_GB_CALL(infogrb, GrB_Vector_new(&I_vec, GrB_UINT64, nb_edges))
    CHECK_GB_CALL(infogrb, GrB_Vector_new(&extr_vals_e_ipt0, GrB_INT8, nb_edges))
    CHECK_GB_CALL(infogrb, GrB_Vector_new(&inds_pts_edge0, GrB_UINT64, 2))
    CHECK_GB_CALL(infogrb, GrB_Vector_new(&inds_pts_edge1, GrB_UINT64, 2))
    list_inds_del1 = alloc_with_capacity_vec_int64(1);
    list_inds_del2 = alloc_with_capacity_vec_int64(1);
    list_inds_del = alloc_with_capacity_vec_int64(1);
    set_del_edges = alloc_with_capacity_vec_int64(1);

    for (i_f=0 ; i_f<nb_faces ; i_f++){ //for each face
        CHECK_GB_CALL(infogrb, GrB_extract(fj, GrB_NULL, GrB_NULL, *(p->faces), GrB_ALL, 1, i_f, GrB_NULL)) //Get indices of edges composing face i_f
        CHECK_GB_CALL(infogrb, GxB_Vector_extractTuples_Vector(nz_fj, extr_vals_fj, fj, GrB_NULL))
        CHECK_GB_CALL(infogrb, GrB_Vector_size(&size_nz_fj, nz_fj))

        for(i_e_fj = 0; i_e_fj < size_nz_fj; i_e_fj++){
            CHECK_GB_CALL(infogrb, GrB_Vector_extractElement(&i_e, nz_fj, i_e_fj))
            CHECK_GB_CALL(infogrb, GrB_extract(ej, GrB_NULL, GrB_NULL, *(p->edges), GrB_ALL, 1, i_e, GrB_NULL)) //Get indices of points composing edge i_e
            CHECK_GB_CALL(infogrb, GxB_Vector_extractTuples_Vector(nz_ej, extr_vals_ej, ej, GrB_NULL))
            CHECK_GB_CALL(infogrb, GrB_Vector_size(&size_nz_ej, nz_ej))
            if (size_nz_ej>0){
                CHECK_GB_CALL(infogrb, GrB_Vector_extractElement(&sign_f, extr_vals_fj, i_e_fj))
                CHECK_GB_CALL(infogrb, GrB_Vector_extractElement(&sign_e, extr_vals_ej, 0))
                if (sign_f*sign_e == -1)
                    CHECK_GB_CALL(infogrb, GrB_Vector_extractElement(&i_pt0, nz_ej, 0))
                else
                    CHECK_GB_CALL(infogrb, GrB_Vector_extractElement(&i_pt0, nz_ej, 1))
                CHECK_GB_CALL(infogrb, GrB_extract(e_ipt0, GrB_NULL, GrB_NULL, *(p->edges), (GrB_Index[]){i_pt0}, 1, GrB_ALL, 1, GrB_NULL)) //Get indices of edges connected with point i_pt0
                CHECK_GB_CALL(infogrb, GxB_Matrix_extractTuples_Vector(I_vec, nz_e_ipt0, extr_vals_e_ipt0, e_ipt0, GrB_NULL))
                CHECK_GB_CALL(infogrb, GrB_Vector_size(&size_nz_e_ipt0, nz_e_ipt0))
                if (size_nz_e_ipt0 == 2){ //Exactly two edges connected to the point i_pt0
                    CHECK_GB_CALL(infogrb, GrB_Vector_extractElement(&i_e0, nz_e_ipt0, 0))
                    CHECK_GB_CALL(infogrb, GrB_Vector_extractElement(&i_e1, nz_e_ipt0, 1))
                    retrieve_ith_edge2D(p->vertices, p->edges, i_e0, &e1_ext1, &e1_ext2); 
                    retrieve_ith_edge2D(p->vertices, p->edges, i_e1, &e2_ext1, &e2_ext2);
                    if (((e1_ext1->x != e1_ext2->x) || ((e1_ext1->y != e1_ext2->y))) && \
                        ((e2_ext1->x != e2_ext2->x) || ((e2_ext1->y != e2_ext2->y)))){ //The edges do not degenerate to only one point
                        alpha = compute_angle_between_edges2D(e1_ext1, e1_ext2, e2_ext1, e2_ext2);
                        norm_e1 = sqrt((e1_ext1->x - e1_ext2->x)*(e1_ext1->x - e1_ext2->x) + (e1_ext1->y - e1_ext2->y)*(e1_ext1->y - e1_ext2->y));
                        norm_e2 = sqrt((e2_ext1->x - e2_ext2->x)*(e2_ext1->x - e2_ext2->x) + (e2_ext1->y - e2_ext2->y)*(e2_ext1->y - e2_ext2->y));
                        if ((norm_e1 + norm_e2 < minimal_length) || (alpha <= minimal_angle)){ //This point must be deleted!
                            //Find the two edges connected to this point
                            if (i_e == i_e0){
                                inds_pts_edge0 = nz_ej;
                                CHECK_GB_CALL(infogrb, GrB_extract(ej, GrB_NULL, GrB_NULL, *(p->edges), GrB_ALL, 1, i_e1, GrB_NULL))
                                CHECK_GB_CALL(infogrb, GxB_Vector_extractTuples_Vector(inds_pts_edge1, extr_vals_ej, ej, GrB_NULL))
                            } else { //i_e == i_e1
                                inds_pts_edge1 = nz_ej;
                                CHECK_GB_CALL(infogrb, GrB_extract(ej, GrB_NULL, GrB_NULL, *(p->edges), GrB_ALL, 1, i_e0, GrB_NULL))
                                CHECK_GB_CALL(infogrb, GxB_Vector_extractTuples_Vector(inds_pts_edge0, extr_vals_ej, ej, GrB_NULL))
                            }
                            //Find the index of the points composing these two edges
                            CHECK_GB_CALL(infogrb, GrB_Vector_extractElement(&i_e0_pt0, inds_pts_edge0, 0))
                            CHECK_GB_CALL(infogrb, GrB_Vector_extractElement(&i_e0_pt1, inds_pts_edge0, 1))
                            CHECK_GB_CALL(infogrb, GrB_Vector_extractElement(&i_e1_pt0, inds_pts_edge1, 0))
                            CHECK_GB_CALL(infogrb, GrB_Vector_extractElement(&i_e1_pt1, inds_pts_edge1, 1))

                            //The point in common in these two edges is the point to delete.
                            if ((i_e0_pt0 == i_e1_pt0) || (i_e0_pt0 == i_e1_pt1)){
                                ind_pt_del = (int64_t)i_e0_pt0;
                                ind_pt1 = (int64_t)i_e0_pt1;
                            } else {
                                ind_pt_del = (int64_t)i_e0_pt1;
                                ind_pt1 = (int64_t)i_e0_pt0;
                            }
                            if ((int64_t)i_e1_pt0 == ind_pt_del)
                                ind_pt2 = (int64_t)i_e1_pt1;
                            else
                                ind_pt2 = (int64_t)i_e1_pt0;

                            //Store the indices of the points connected to the one to delete
                            set_ijth_elem_arr_int(*list_del_pts, ind_pt_del, 0, &ind_pt1);
                            set_ijth_elem_arr_int(*list_del_pts, ind_pt_del, 1, &ind_pt2);

                            //Store the indices of the edges and face connected to the point to delete
                            signed_ie0 = (int64_t)i_e0;
                            signed_ie1 = (int64_t)i_e1;
                            signed_i_f = (int64_t)i_f;
                            set_ijth_elem_arr_int(*list_changed_edges, ind_pt_del, 0, &signed_ie0);
                            set_ijth_elem_arr_int(*list_changed_edges, ind_pt_del, 1, &signed_ie1);
                            set_ijth_elem_arr_int(*list_changed_edges, ind_pt_del, 2, &signed_i_f);
                        }
                    }
                }
            }
        }
    }

    //Correction on the lists: when several points, forming a path connected by edges, are supposed to be suppressed,
    //then only register the end points of this path. Also, register the edges in the middle to be entirely suppressed.
    i_pt0 = p->vertices->size;
    for (ind_pt_del=0; ind_pt_del<(int64_t)i_pt0 ; ind_pt_del++){ //for each point
        ind_pt1 = *get_ijth_elem_arr_int(*list_del_pts, ind_pt_del, 0);
        if ((ind_pt1 > -1) && (ind_pt1<(int64_t)i_pt0)) { //point should be deleted
            ind_pt2 = *get_ijth_elem_arr_int(*list_del_pts, ind_pt_del, 1);
            set_ith_elem_vec_int64(list_inds_del1, 0, &ind_pt_del);
            list_inds_del1->size = 1;
            set_ith_elem_vec_int64(list_inds_del2, 0, &ind_pt_del);
            list_inds_del2->size = 1;

            curr_i = ind_pt_del;
            
            signed_ie0 = *get_ijth_elem_arr_int(*list_del_pts, ind_pt1, 0);
            while (signed_ie0 > -1) {
                if (signed_ie0 == curr_i){
                    curr_i = ind_pt1;
                    ind_pt1 = *get_ijth_elem_arr_int(*list_del_pts, ind_pt1, 1);
                } else {
                    curr_i = ind_pt1;
                    ind_pt1 = signed_ie0;
                }
                if (curr_i == ind_pt_del){
                    ind_pt1 = curr_i;
                    break;
                }
                push_back_vec_int64(&list_inds_del1, &curr_i);
                signed_ie0 = *get_ijth_elem_arr_int(*list_del_pts, ind_pt1, 0);
            }

            if ((list_inds_del1->size == 1) || (curr_i != ind_pt_del)){ //no loop detected: we can explore the other end.
                curr_i = ind_pt_del;
                signed_ie0 = *get_ijth_elem_arr_int(*list_del_pts, ind_pt2, 0);
                while (signed_ie0 > -1){
                    if (signed_ie0 == curr_i){
                        curr_i = ind_pt2;
                        ind_pt2 = *get_ijth_elem_arr_int(*list_del_pts, ind_pt2, 1);
                    } else {
                        curr_i = ind_pt2;
                        ind_pt2 = signed_ie0;
                    }
                    if (curr_i == ind_pt_del){
                        ind_pt2 = curr_i;
                        break;
                    }
                    push_back_vec_int64(&list_inds_del2, &curr_i);
                    signed_ie0 = *get_ijth_elem_arr_int(*list_del_pts, ind_pt2, 0);
                }
            }

            if ((list_inds_del1->size > 1) || (list_inds_del2->size > 1)){ //There are several points linked together to be deleted!
                if ((ind_pt1 == ind_pt_del) && (list_inds_del1->size < 5)){ //we have looped, and it is a triangle or a square!
                    //list_inds_del = unique([list_inds_del1; list_inds_del2]);
                    set_ith_elem_vec_int64(list_inds_del, 0, get_ith_elem_vec_int64(list_inds_del1, 0));
                    list_inds_del->size = 1;
                    for(i_f = 1; i_f<list_inds_del1->size; i_f++){
                        push_back_unique_vec_int64(&list_inds_del, get_ith_elem_vec_int64(list_inds_del1, i_f));
                    }
                    for(i_f = 0; i_f<list_inds_del2->size; i_f++){
                        push_back_unique_vec_int64(&list_inds_del, get_ith_elem_vec_int64(list_inds_del2, i_f));
                    }

                    barycenter = (Point2D){0., 0.};
                    signed_i_f = (int64_t)(list_inds_del->size + 1);
                    for (i_f = 0; i_f < list_inds_del->size; i_f++){
                        i_e = *get_ith_elem_vec_int64(list_inds_del, i_f);
                        e1_ext1 = get_ith_elem_vec_pts2D(p->vertices, i_e); //p.vertices[list_inds_del1[i_f]]
                        barycenter.x += e1_ext1->x + dt*vsx[i_e];
                        barycenter.y += e1_ext1->y + dt*vsy[i_e];
                        set_ijth_elem_arr_int(*list_del_pts, i_e, 0, &signed_i_f);
                        set_ijth_elem_arr_int(*list_del_pts, i_e, 1, &signed_i_f);
                    }
                    barycenter.x /= list_inds_del->size;
                    barycenter.y /= list_inds_del->size;
                    push_back_vec_pts2D(&(p->vertices), &barycenter);

                    //signed_i_f = *get_ijth_elem_arr_int(*list_changed_edges, *get_ith_elem_vec_int64(list_inds_del, 0), 0);
                    //for(i_f = 0; i_f < list_inds_del->size; i_f++){
                    //    i_e = *get_ith_elem_vec_int64(list_inds_del, i_f);
                    //    set_ijth_elem_arr_int(*list_changed_edges, i_e, 0, &signed_i_f);
                    //    set_ijth_elem_arr_int(*list_changed_edges, i_e, 1, &signed_i_f);
                    //}
                    set_del_edges->size = 0;
                    for (i_e=0; i_e<list_inds_del->size; i_e++){
                        signed_i_f = *get_ijth_elem_arr_int(*list_changed_edges, i_e, 0);
                        push_back_unique_vec_int64(&set_del_edges, &signed_i_f);
                        signed_i_f = *get_ijth_elem_arr_int(*list_changed_edges, i_e, 1);
                        push_back_unique_vec_int64(&set_del_edges, &signed_i_f);
                    }
                    for (i_e=0; i_e<list_inds_del->size; i_e++){
                        signed_i_f = *get_ith_elem_vec_int64(list_inds_del, i_e);
                        set_ijth_elem_arr_int(*list_changed_edges, signed_i_f, 0, get_ith_elem_vec_int64(set_del_edges, signed_i_f));
                        set_ijth_elem_arr_int(*list_changed_edges, signed_i_f, 1, get_ith_elem_vec_int64(set_del_edges, signed_i_f));
                    }
                } else {
                    if (list_inds_del1->size > 1){
                        i_f = 1;
                        while (i_f<list_inds_del1->size){
                            signed_i_f = *get_ith_elem_vec_int64(list_inds_del1, i_f);
                            i_e = (uint64_t)signed_i_f;
                            signed_i_f = -1;
                            set_ijth_elem_arr_int(*list_del_pts, i_e, 0, &signed_i_f);//Actually don't suppress that point...
                            set_ijth_elem_arr_int(*list_del_pts, i_e, 1, &signed_i_f);
                            set_ijth_elem_arr_int(*list_changed_edges, i_e, 0, &signed_i_f);//Actually don't suppress that point...
                            set_ijth_elem_arr_int(*list_changed_edges, i_e, 1, &signed_i_f);
                            set_ijth_elem_arr_int(*list_changed_edges, i_e, 2, &signed_i_f);
                            i_f += 2;
                        }
                    }
                    if (list_inds_del2->size > 1){
                        i_f = 1;
                        while (i_f<list_inds_del2->size){
                            signed_i_f = *get_ith_elem_vec_int64(list_inds_del2, i_f);
                            i_e = (uint64_t)signed_i_f;
                            signed_i_f = -1;
                            set_ijth_elem_arr_int(*list_del_pts, i_e, 0, &signed_i_f);//Actually don't suppress that point...
                            set_ijth_elem_arr_int(*list_del_pts, i_e, 1, &signed_i_f);
                            set_ijth_elem_arr_int(*list_changed_edges, i_e, 0, &signed_i_f);//Actually don't suppress that point...
                            set_ijth_elem_arr_int(*list_changed_edges, i_e, 1, &signed_i_f);
                            set_ijth_elem_arr_int(*list_changed_edges, i_e, 2, &signed_i_f);
                            i_f += 2;
                        }
                    }
                }
            }
        }
    }
    infogrb = GrB_free(&fj);
    infogrb = GrB_free(&nz_fj);
    infogrb = GrB_free(&extr_vals_fj);
    infogrb = GrB_free(&ej);
    infogrb = GrB_free(&nz_ej);
    infogrb = GrB_free(&extr_vals_ej);
    infogrb = GrB_free(&e_ipt0);
    infogrb = GrB_free(&nz_e_ipt0);
    infogrb = GrB_free(&extr_vals_e_ipt0);
    infogrb = GrB_free(&inds_pts_edge0);
    infogrb = GrB_free(&inds_pts_edge1);
    infogrb = GrB_free(&I_vec);
    dealloc_vec_int64(list_inds_del1); free(list_inds_del1);
    dealloc_vec_int64(list_inds_del2); free(list_inds_del2);
    dealloc_vec_int64(list_inds_del); free(list_inds_del);
    dealloc_vec_int64(set_del_edges); free(set_del_edges);
    GxB_Iterator_free(&iterator);
}

//Delete edge i_e and split it in two new edges passing through new_pt.
//Actually, it modifies edge i_e and only creates a new one...
static void split_edge(Polygon2D* p, uint64_t i_e, const Point2D *new_pt){
    GrB_Index nb_edge;
    GrB_Info infogrb;
    GrB_Vector ej, extr_vals_ej, inds_pts_edge;
    GrB_Index i_e0_pt0, i_e0_pt1;
    uint64_t ind_new_pt;
    int8_t val;
    GrB_Matrix new_line, new_edge, copy_mat;
    GrB_Index nb_faces, nb_cols, nb_rows;
    long int i_s;

    GrB_Matrix_ncols(&nb_edge, *(p->edges));
    GrB_Matrix_ncols(&nb_faces, *(p->faces));
    infogrb = GrB_Vector_new(&ej, GrB_INT8, p->vertices->size);
    infogrb = GrB_Vector_new(&extr_vals_ej, GrB_INT8, p->vertices->size);
    infogrb = GrB_Vector_new(&inds_pts_edge, GrB_UINT64, p->vertices->size);
    infogrb = GrB_Matrix_new(&copy_mat, GrB_INT8, 1, 1);

    //Extract the column i_e of p.edges, and take the two first non-zero values.
    CHECK_GB_CALL(infogrb, GrB_extract(ej, GrB_NULL, GrB_NULL, *(p->edges), GrB_ALL, 1, i_e, GrB_NULL))
    CHECK_GB_CALL(infogrb, GxB_Vector_extractTuples_Vector(inds_pts_edge, extr_vals_ej, ej, GrB_NULL))
    CHECK_GB_CALL(infogrb, GrB_Vector_extractElement(&i_e0_pt0, inds_pts_edge, 0))
    CHECK_GB_CALL(infogrb, GrB_Vector_extractElement(&i_e0_pt1, inds_pts_edge, 1))
    CHECK_GB_CALL(infogrb, GrB_Vector_extractElement(&val, extr_vals_ej, 0))

    //pts_indices = rowvals(p.edges)
    //inds_pts_edge = pts_indices[nzrange(p.edges, i_e)]

    push_back_vec_pts2D(&(p->vertices), new_pt);
    ind_new_pt = p->vertices->size;

    infogrb = GrB_Matrix_removeElement(*(p->edges), i_e0_pt0, i_e);

    CHECK_GB_CALL(infogrb, GrB_Matrix_new(&new_line, GrB_INT8, 1, nb_edge))
    CHECK_GB_CALL(infogrb, GrB_Matrix_new(&new_edge, GrB_INT8, ind_new_pt, 1))
    CHECK_GB_CALL(infogrb, GrB_Matrix_setElement(new_line, val, 0, i_e))
    CHECK_GB_CALL(infogrb, GrB_Matrix_setElement(new_edge, val, i_e0_pt0, 0))
    CHECK_GB_CALL(infogrb, GrB_Matrix_setElement(new_edge, -val, ind_new_pt-1, 0))
    
    GrB_Matrix_dup(&copy_mat, *(p->edges));
    GrB_Matrix_ncols(&nb_cols, copy_mat);
    GrB_Matrix_nrows(&nb_rows, copy_mat);
    CHECK_GB_CALL(infogrb, GrB_Matrix_resize(*(p->edges), nb_rows + 1, nb_cols))
    CHECK_GB_CALL(infogrb, GxB_Matrix_concat(*(p->edges), (GrB_Matrix[]){copy_mat, new_line}, 2, 1, GrB_NULL)) //edges = [edges ; new_line]
    GrB_Matrix_dup(&copy_mat, *(p->edges));
    GrB_Matrix_ncols(&nb_cols, copy_mat);
    GrB_Matrix_nrows(&nb_rows, copy_mat);
    CHECK_GB_CALL(infogrb, GrB_Matrix_resize(*(p->edges), nb_rows, nb_cols + 1))
    CHECK_GB_CALL(infogrb, GxB_Matrix_concat(*(p->edges), (GrB_Matrix[]){copy_mat, new_edge}, 1, 2, GrB_NULL)) //edges = [edges   new_edge]

    CHECK_GB_CALL(infogrb, GrB_Matrix_resize(new_line, 1, nb_faces))
    CHECK_GB_CALL(infogrb, GrB_extract(new_line, GrB_NULL, GrB_NULL, *(p->faces), (GrB_Index[]){i_e}, 1, GrB_ALL, 1,GrB_NULL)) 
    GrB_Matrix_dup(&copy_mat, *(p->faces));
    GrB_Matrix_ncols(&nb_cols, copy_mat);
    GrB_Matrix_nrows(&nb_rows, copy_mat);
    CHECK_GB_CALL(infogrb, GrB_Matrix_resize(*(p->faces), nb_rows + 1, nb_cols))
    CHECK_GB_CALL(infogrb, GxB_Matrix_concat(*(p->faces), (GrB_Matrix[]){copy_mat, new_line}, 2, 1, GrB_NULL)) //faces = [faces ; faces[i_e,:]]

    i_s = *get_ith_elem_vec_int(p->status_edge, i_e);
    push_back_vec_int(&(p->status_edge), &i_s);

    GrB_Vector_free(&ej);
    GrB_Vector_free(&extr_vals_ej);
    GrB_Vector_free(&inds_pts_edge);
    GrB_Matrix_free(&new_line);
    GrB_Matrix_free(&new_edge);
    GrB_free(&copy_mat);
}

//The common point of edges ind_e0_fused and ind_e1_fused (both in face ind_face) is suppressed, and the edges are transformed to become only one.
static void fuse_points(Polygon2D *p, int64_t ind_e0_fused, int64_t ind_e1_fused, int64_t ind_face){
    GrB_Info infogrb;
    GrB_Vector ej, extr_vals_ej, inds_pts_edge0, inds_pts_edge1;
    GrB_Index i_e0_pt0, i_e0_pt1, i_e1_pt0, i_e1_pt1, nb_pts, size0, size1;
    uint64_t ind_pt_del, ind_pt1, ind_pt2;
    int8_t val;

    //nb_pts = p->vertices->size;
    GrB_Matrix_ncols(&nb_pts, *(p->edges));
    infogrb = GrB_Vector_new(&ej, GrB_INT8, nb_pts);
    infogrb = GrB_Vector_new(&extr_vals_ej, GrB_INT8, nb_pts);
    infogrb = GrB_Vector_new(&inds_pts_edge0, GrB_UINT64, nb_pts);
    infogrb = GrB_Vector_new(&inds_pts_edge1, GrB_UINT64, nb_pts);

    if (ind_e1_fused != ind_e0_fused) { //one edge to suppress, the other one is modified.
        CHECK_GB_CALL(infogrb, GrB_extract(ej, GrB_NULL, GrB_NULL, *(p->edges), GrB_ALL, 1, ind_e0_fused, GrB_NULL))
        CHECK_GB_CALL(infogrb, GxB_Vector_extractTuples_Vector(inds_pts_edge0, extr_vals_ej, ej, GrB_NULL))
        GrB_Vector_size(&size0, inds_pts_edge0);
        CHECK_GB_CALL(infogrb, GrB_extract(ej, GrB_NULL, GrB_NULL, *(p->edges), GrB_ALL, 1, ind_e1_fused, GrB_NULL))
        CHECK_GB_CALL(infogrb, GxB_Vector_extractTuples_Vector(inds_pts_edge1, extr_vals_ej, ej, GrB_NULL))
        GrB_Vector_size(&size1, inds_pts_edge1);
        if ((size0>1) && (size1>1)){
            //Find the index of the points composing these two edges
            CHECK_GB_CALL(infogrb, GrB_Vector_extractElement(&i_e0_pt0, inds_pts_edge0, 0))
            CHECK_GB_CALL(infogrb, GrB_Vector_extractElement(&i_e0_pt1, inds_pts_edge0, 1))
            CHECK_GB_CALL(infogrb, GrB_Vector_extractElement(&i_e1_pt0, inds_pts_edge1, 0))
            CHECK_GB_CALL(infogrb, GrB_Vector_extractElement(&i_e1_pt1, inds_pts_edge1, 1))

            //The point in common in these two edges is the point to delete.
            if ((i_e0_pt0 == i_e1_pt0) || (i_e0_pt0 == i_e1_pt1)){
                ind_pt_del = i_e0_pt0;
                ind_pt1 = i_e0_pt1;
            } else {
                ind_pt_del = i_e0_pt1;
                ind_pt1 = i_e0_pt0;
            }
            if (i_e1_pt0 == ind_pt_del)
                ind_pt2 = i_e1_pt1;
            else
                ind_pt2 = i_e1_pt0;
        
            CHECK_GB_CALL(infogrb, GrB_Matrix_extractElement(&val, *(p->edges), ind_pt_del, ind_e0_fused))
            CHECK_GB_CALL(infogrb, GrB_Matrix_setElement(*(p->edges), val, ind_pt2, ind_e0_fused))
            CHECK_GB_CALL(infogrb, GrB_Matrix_removeElement(*(p->edges), ind_pt_del, ind_e0_fused))
            CHECK_GB_CALL(infogrb, GrB_Matrix_removeElement(*(p->edges), ind_pt2, ind_e1_fused))
            CHECK_GB_CALL(infogrb, GrB_Matrix_removeElement(*(p->edges), ind_pt_del, ind_e1_fused))

            CHECK_GB_CALL(infogrb, GrB_Matrix_removeElement(*(p->faces), ind_e1_fused, ind_face))
        }
    } else { //only one edge: means it must be suppressed.
        CHECK_GB_CALL(infogrb, GrB_extract(ej, GrB_NULL, GrB_NULL, *(p->edges), GrB_ALL, 1, ind_e0_fused, GrB_NULL))
        CHECK_GB_CALL(infogrb, GxB_Vector_extractTuples_Vector(inds_pts_edge0, extr_vals_ej, ej, GrB_NULL))
        CHECK_GB_CALL(infogrb, GrB_Vector_extractElement(&i_e0_pt0, inds_pts_edge0, 0))
        CHECK_GB_CALL(infogrb, GrB_Vector_extractElement(&i_e0_pt1, inds_pts_edge0, 1))

        //inds_pts_edge = pts_indices[nzrange(p.edges, ind_e1_fused)]
        CHECK_GB_CALL(infogrb, GrB_Matrix_removeElement(*(p->edges), i_e0_pt0, ind_e1_fused))
        CHECK_GB_CALL(infogrb, GrB_Matrix_removeElement(*(p->edges), i_e0_pt1, ind_e1_fused))

        CHECK_GB_CALL(infogrb, GrB_Matrix_removeElement(*(p->faces), ind_e1_fused, ind_face))
    }

    GrB_Vector_free(&ej);
    GrB_Vector_free(&extr_vals_ej);
    GrB_Vector_free(&inds_pts_edge0);
    GrB_Vector_free(&inds_pts_edge1);
}

//The vertices of polygon pn (at time tn) are supplemented by vertices at time tn+dt (in vertices_tnp1) backpropagated in time.
//These new vertices added in pn are the points of intersection detected at time tn+dt and propagated back at time tn.
static void backpropagate_pts(Polygon2D *pn, Vector_points2D* vertices_tnp1,\
    const Vector_points2D* pts_intersec, const Vector_uint* edge_intersect1, const Vector_uint* edge_intersect2, Vector_int8* pt_in_or_out){

    uint64_t i, ie, ipts1, ipts2, iptp, iptn;
    GrB_Index nb_pts;
    GrB_Vector ej, ipts, extr_vals_ej;
    my_real_c theta;
    Point2D new_pt;
    int8_t mark8 = -2;
    const Point2D *ptp, *ptn;
    GrB_Info infogrb;

    GrB_Vector_new(&ej, GrB_INT8, 0);
    GrB_Vector_new(&ipts, GrB_UINT64, 0);
    GrB_Vector_new(&extr_vals_ej, GrB_INT8, 0);

    for(i=0; i<edge_intersect1->size; i++){
        //ind_pts = rowvals(pn.edges)
        //ipts = ind_pts[nzrange(pn.edges, ie)]
        GrB_Matrix_nrows(&nb_pts, *(pn->edges));
        GrB_Vector_resize(ej, nb_pts);
        GrB_Vector_resize(ipts, nb_pts);
        GrB_Vector_resize(extr_vals_ej, nb_pts);
        ie = *get_ith_elem_vec_uint(edge_intersect1, i);
        CHECK_GB_CALL(infogrb, GrB_extract(ej, GrB_NULL, GrB_NULL, *(pn->edges), GrB_ALL, 1, ie, GrB_NULL)) 
        CHECK_GB_CALL(infogrb, GxB_Vector_extractTuples_Vector(ipts, extr_vals_ej, ej, GrB_NULL))
        CHECK_GB_CALL(infogrb, GrB_Vector_extractElement(&ipts1, ipts, 0))
        CHECK_GB_CALL(infogrb, GrB_Vector_extractElement(&ipts2, ipts, 1))
        if (*get_ith_elem_vec_int8(pt_in_or_out, ipts1) > 0){
            iptp = ipts1;
            iptn = ipts2;
        } else {
            iptp = ipts2;
            iptn = ipts1;
        }

        ptp = get_ith_elem_vec_pts2D(vertices_tnp1, iptp);
        ptn = get_ith_elem_vec_pts2D(vertices_tnp1, iptn);
        theta = compute_barycentric_coord(*get_ith_elem_vec_pts2D(pts_intersec, i), *ptp, *ptn);

        ptp = get_ith_elem_vec_pts2D(pn->vertices, iptp);
        ptn = get_ith_elem_vec_pts2D(pn->vertices, iptn);
        new_pt.x = (1-theta)*(ptp->x) + theta*ptn->x;
        new_pt.y = (1-theta)*(ptp->y) + theta*ptn->y;

        split_edge(pn, ie, &new_pt);
        push_back_vec_pts2D(&vertices_tnp1, get_ith_elem_vec_pts2D(pts_intersec, i));
        push_back_vec_int8(&pt_in_or_out, &mark8);

        GrB_Matrix_nrows(&nb_pts, *(pn->edges));
        GrB_Vector_resize(ej, nb_pts);
        GrB_Vector_resize(ipts, nb_pts);
        GrB_Vector_resize(extr_vals_ej, nb_pts);
        ie = *get_ith_elem_vec_uint(edge_intersect2, i);
        CHECK_GB_CALL(infogrb, GrB_extract(ej, GrB_NULL, GrB_NULL, *(pn->edges), GrB_ALL, 1, ie, GrB_NULL)) 
        CHECK_GB_CALL(infogrb, GxB_Vector_extractTuples_Vector(ipts, extr_vals_ej, ej, GrB_NULL))
        CHECK_GB_CALL(infogrb, GrB_Vector_extractElement(&ipts1, ipts, 0))
        CHECK_GB_CALL(infogrb, GrB_Vector_extractElement(&ipts2, ipts, 1))
        if (*get_ith_elem_vec_int8(pt_in_or_out, ipts1) > 0){
            iptp = ipts1;
            iptn = ipts2;
        } else {
            iptp = ipts2;
            iptn = ipts1;
        }

        ptp = get_ith_elem_vec_pts2D(vertices_tnp1, iptp);
        ptn = get_ith_elem_vec_pts2D(vertices_tnp1, iptn);
        theta = compute_barycentric_coord(*get_ith_elem_vec_pts2D(pts_intersec, i), *ptp, *ptn);

        ptp = get_ith_elem_vec_pts2D(pn->vertices, iptp);
        ptn = get_ith_elem_vec_pts2D(pn->vertices, iptn);
        new_pt.x = (1-theta)*(ptp->x) + theta*ptn->x;
        new_pt.y = (1-theta)*(ptp->y) + theta*ptn->y;

        split_edge(pn, ie, &new_pt);
        push_back_vec_pts2D(&vertices_tnp1, get_ith_elem_vec_pts2D(pts_intersec, i));
        push_back_vec_int8(&pt_in_or_out, &mark8);
    }

    GrB_free(&ej);
    GrB_free(&ipts);
    GrB_free(&extr_vals_ej);
}

//Project pt on edge [ext1, ext2]
static Point2D project_on_edge(const Point2D* pt, const Point2D* ext1, const Point2D* ext2){
    Point2D tangentVec, pt_prime, diffpt;
    my_real_c nt, bottom, up, left, right;

    tangentVec.x = ext2->x - ext1->x;
    tangentVec.y = ext2->y - ext1->y;
    nt = norm_pt2D(tangentVec);   

    if (nt>0){
        tangentVec.x /= nt;
        tangentVec.y /= nt;
    }

    diffpt.x = pt->x - ext1->x;
    diffpt.y = pt->y - ext1->y;
    nt = scalProd2D(diffpt, tangentVec);
    pt_prime.x = ext1->x + nt*tangentVec.x;
    pt_prime.y = ext1->y + nt*tangentVec.y;

    if (ext1->y < ext2->y){
        bottom = ext1->y; up = ext2->y;
    } else {
        up = ext1->y; bottom = ext2->y;
    }
    if (ext1->x < ext2->x){
        left = ext1->x; right = ext2->x;
    } else {
        right = ext1->x; left = ext2->x;
    }

    if (left < right){
        if (!((pt_prime.x >= left) && (pt_prime.x <= right))){
            if (pt_prime.x < left){
                pt_prime.x = left;
            } else { //pt_prime.x > right
                pt_prime.x = right;
            }
        }
    }
    if (bottom < up){
        if (!((pt_prime.y >= bottom) && (pt_prime.y <= up))){
            if (pt_prime.y < bottom){
                pt_prime.y = bottom;
            } else { //pt_prime.y > up
                pt_prime.y = up;
            }
        }
    }

    return pt_prime;
}

//Marks all points between two intersections.
//pair_intersection will be an array of size (nb_points x 2). For each line:
// - if -1 : the point is not part of a path linking two intersection points and that will be suppressed.
// - otherwise : the point is part of a path linking two intersection points and the two columns indicates the indices of the two intersection (as ordered in edge_intersect*).
static void find_pair_intersection(const Polygon2D* p, \
                            const Vector_uint* edge_intersect1, const Vector_uint* edge_intersect2, const Vector_int8* pt_in_or_out,\
                            Array_int** pair_intersection){
    uint64_t ie, je, j, nb_intersec, ie1, je1;
    GrB_Index i, k, ipts1, ipts2, nb_pts;
    int64_t val = -1;
    Vector_uint* forbidden_edges;
    GrB_Vector ej, ipts, extr_vals_ej;
    Vector_uint *listEdges = alloc_empty_vec_uint();
    Vector_uint *listNodes = alloc_empty_vec_uint();
    GrB_Info infogrb;

    GrB_Matrix_nrows(&nb_pts, *(p->edges));
    GrB_Vector_new(&ej, GrB_INT8, nb_pts);
    GrB_Vector_new(&ipts, GrB_UINT64, nb_pts);
    GrB_Vector_new(&extr_vals_ej, GrB_INT8, nb_pts);

    *pair_intersection = alloc_with_capacity_arr_int(pt_in_or_out->size, 2);
    for(ie=0; ie<pt_in_or_out->size; ie++){
        for(j=0; j<2; j++){
            set_ijth_elem_arr_int(*pair_intersection, ie, j, &val);
        }
    }

    forbidden_edges = cat_vec_uint(edge_intersect1, edge_intersect2);

    nb_intersec = edge_intersect1->size;
    //ind_pts = rowvals(p.edges)
    for (ie=0; ie<nb_intersec; ie++){
        //First edge of first intersection
        ie1 = *get_ith_elem_vec_uint(edge_intersect1, ie);
        //krange = ind_pts[nzrange(p.edges, ie1)]
        CHECK_GB_CALL(infogrb, GrB_extract(ej, GrB_NULL, GrB_NULL, *(p->edges), GrB_ALL, 1, ie1, GrB_NULL)) 
        CHECK_GB_CALL(infogrb, GxB_Vector_extractTuples_Vector(ipts, extr_vals_ej, ej, GrB_NULL))
        CHECK_GB_CALL(infogrb, GrB_Vector_extractElement(&ipts1, ipts, 0))
        CHECK_GB_CALL(infogrb, GrB_Vector_extractElement(&ipts2, ipts, 1))

        if (*get_ith_elem_vec_int8(pt_in_or_out, ipts1) < 0) {
            i = ipts1;
        } else if (*get_ith_elem_vec_int8(pt_in_or_out, ipts2) < 0) {
            i = ipts2;
        } else {
            i = UINT64_MAX;
        }
        if (i < UINT64_MAX){
            for (je = ie + 1; je<nb_intersec; je++){
                //First edge of second intersection
                je1 = *get_ith_elem_vec_uint(edge_intersect1, je);
                CHECK_GB_CALL(infogrb, GrB_extract(ej, GrB_NULL, GrB_NULL, *(p->edges), GrB_ALL, 1, je1, GrB_NULL)) 
                CHECK_GB_CALL(infogrb, GxB_Vector_extractTuples_Vector(ipts, extr_vals_ej, ej, GrB_NULL))
                CHECK_GB_CALL(infogrb, GrB_Vector_extractElement(&ipts1, ipts, 0))
                CHECK_GB_CALL(infogrb, GrB_Vector_extractElement(&ipts2, ipts, 1))

                if (*get_ith_elem_vec_int8(pt_in_or_out, ipts1) < 0) {
                    j = ipts1;
                } else if (*get_ith_elem_vec_int8(pt_in_or_out, ipts2) < 0) {
                    j = ipts2;
                } else {
                    j = UINT64_MAX;
                }

                if (j < UINT64_MAX){
                    astar(p->vertices, p->edges, i, j, forbidden_edges, listEdges, listNodes);
                    if (listEdges->size>0){ //If a path has been found between two points.
                        for(k=0; k<listNodes->size; k++){
                            set_ijth_elem_arr_int(*pair_intersection, *get_ith_elem_vec_uint(listNodes, k), 0, &ie);
                            set_ijth_elem_arr_int(*pair_intersection, *get_ith_elem_vec_uint(listNodes, k), 1, &je);
                        }
                    }
                }

                //Second edge of second intersection
                je1 = *get_ith_elem_vec_uint(edge_intersect2, je);
                CHECK_GB_CALL(infogrb, GrB_extract(ej, GrB_NULL, GrB_NULL, *(p->edges), GrB_ALL, 1, je1, GrB_NULL)) 
                CHECK_GB_CALL(infogrb, GxB_Vector_extractTuples_Vector(ipts, extr_vals_ej, ej, GrB_NULL))
                CHECK_GB_CALL(infogrb, GrB_Vector_extractElement(&ipts1, ipts, 0))
                CHECK_GB_CALL(infogrb, GrB_Vector_extractElement(&ipts2, ipts, 1))

                if (*get_ith_elem_vec_int8(pt_in_or_out, ipts1) < 0) {
                    j = ipts1;
                } else if (*get_ith_elem_vec_int8(pt_in_or_out, ipts2) < 0) {
                    j = ipts2;
                } else {
                    j = UINT64_MAX;
                }

                if (j < UINT64_MAX){
                    astar(p->vertices, p->edges, i, j, forbidden_edges, listEdges, listNodes);
                    if (listEdges->size>0){ //If a path has been found between two points.
                        for(k=0; k<listNodes->size; k++){
                            set_ijth_elem_arr_int(*pair_intersection, *get_ith_elem_vec_uint(listNodes, k), 0, &ie);
                            set_ijth_elem_arr_int(*pair_intersection, *get_ith_elem_vec_uint(listNodes, k), 1, &je);
                        }
                    }
                }
            }
        }

        //Second edge of first intersection
        ie1 = *get_ith_elem_vec_uint(edge_intersect2, ie);
        //krange = ind_pts[nzrange(p.edges, ie1)]
        CHECK_GB_CALL(infogrb, GrB_extract(ej, GrB_NULL, GrB_NULL, *(p->edges), GrB_ALL, 1, ie1, GrB_NULL)) 
        CHECK_GB_CALL(infogrb, GxB_Vector_extractTuples_Vector(ipts, extr_vals_ej, ej, GrB_NULL))
        CHECK_GB_CALL(infogrb, GrB_Vector_extractElement(&ipts1, ipts, 0))
        CHECK_GB_CALL(infogrb, GrB_Vector_extractElement(&ipts2, ipts, 1))

        if (*get_ith_elem_vec_int8(pt_in_or_out, ipts1) < 0) {
            i = ipts1;
        } else if (*get_ith_elem_vec_int8(pt_in_or_out, ipts2) < 0) {
            i = ipts2;
        } else {
            i = UINT64_MAX;
        }
        if (i < UINT64_MAX){
            for (je = ie+1; je<nb_intersec; je++){
                //First edge of second intersection
                je1 = *get_ith_elem_vec_uint(edge_intersect1, je);
                CHECK_GB_CALL(infogrb, GrB_extract(ej, GrB_NULL, GrB_NULL, *(p->edges), GrB_ALL, 1, je1, GrB_NULL)) 
                CHECK_GB_CALL(infogrb, GxB_Vector_extractTuples_Vector(ipts, extr_vals_ej, ej, GrB_NULL))
                CHECK_GB_CALL(infogrb, GrB_Vector_extractElement(&ipts1, ipts, 0))
                CHECK_GB_CALL(infogrb, GrB_Vector_extractElement(&ipts2, ipts, 1))

                if (*get_ith_elem_vec_int8(pt_in_or_out, ipts1) < 0) {
                    j = ipts1;
                } else if (*get_ith_elem_vec_int8(pt_in_or_out, ipts2) < 0) {
                    j = ipts2;
                } else {
                    j = UINT64_MAX;
                }

                if (j < UINT64_MAX){
                    astar(p->vertices, p->edges, i, j, forbidden_edges, listEdges, listNodes);
                    if (listEdges->size>0){ //If a path has been found between two points.
                        for(k=0; k<listNodes->size; k++){
                            set_ijth_elem_arr_int(*pair_intersection, *get_ith_elem_vec_uint(listNodes, k), 0, &ie);
                            set_ijth_elem_arr_int(*pair_intersection, *get_ith_elem_vec_uint(listNodes, k), 1, &je);
                        }
                    }
                }

                //Second edge of second intersection
                je1 = *get_ith_elem_vec_uint(edge_intersect2, je);
                CHECK_GB_CALL(infogrb, GrB_extract(ej, GrB_NULL, GrB_NULL, *(p->edges), GrB_ALL, 1, je1, GrB_NULL)) 
                CHECK_GB_CALL(infogrb, GxB_Vector_extractTuples_Vector(ipts, extr_vals_ej, ej, GrB_NULL))
                CHECK_GB_CALL(infogrb, GrB_Vector_extractElement(&ipts1, ipts, 0))
                CHECK_GB_CALL(infogrb, GrB_Vector_extractElement(&ipts2, ipts, 1))

                if (*get_ith_elem_vec_int8(pt_in_or_out, ipts1) < 0) {
                    j = ipts1;
                } else if (*get_ith_elem_vec_int8(pt_in_or_out, ipts2) < 0) {
                    j = ipts2;
                } else {
                    j = UINT64_MAX;
                }

                if (j < UINT64_MAX){
                    astar(p->vertices, p->edges, i, j, forbidden_edges, listEdges, listNodes);
                    if (listEdges->size>0){ //If a path has been found between two points.
                        for(k=0; k<listNodes->size; k++){
                            set_ijth_elem_arr_int(*pair_intersection, *get_ith_elem_vec_uint(listNodes, k), 0, &ie);
                            set_ijth_elem_arr_int(*pair_intersection, *get_ith_elem_vec_uint(listNodes, k), 1, &je);
                        }
                    }
                }
            }
        }
    }

    GrB_free(&ej);
    GrB_free(&ipts);
    GrB_free(&extr_vals_ej);
    dealloc_vec_uint(listEdges); free(listEdges);
    dealloc_vec_uint(listNodes); free(listNodes);
}

//Computes (pt->x+dt*vsx, pt->y+dt*vsy) for all pt in p->vertices.
//In other words : computes the points at tn+dt.
static Vector_points2D* build_vertices_tnp1(const Polygon2D* p, const my_real_c* vsx, const my_real_c* vsy, uint64_t size_vs, my_real_c dt, Array_int* list_del_pts){
    Vector_points2D* vertices_tnp1;
    uint64_t i;
    int64_t ind_pt1, ind_pt2;
    Point2D pt;
    Point2D pt3D, pt3D_2;

    vertices_tnp1 = alloc_with_capacity_vec_pts2D(p->vertices->size);
    if (!list_del_pts){
        for (i = 0; i<p->vertices->size; i++){
            pt = *get_ith_elem_vec_pts2D(p->vertices, i);
            pt3D = (Point2D){pt.x + dt*vsx[i], pt.y + dt*vsy[i]};
            set_ith_elem_vec_pts2D(vertices_tnp1, i, &pt3D);
        }
    } else {
        for (i = 0; i<size_vs; i++){
            ind_pt1 = *get_ijth_elem_arr_int(list_del_pts, i, 0);
            if (ind_pt1 == -1){
                pt = *get_ith_elem_vec_pts2D(p->vertices, i);
                pt3D = (Point2D){pt.x + dt*vsx[i], pt.y + dt*vsy[i]};
                set_ith_elem_vec_pts2D(vertices_tnp1, i, &pt3D);
            }
        }

        for (i = 0; i<size_vs; i++){ //correct when point must be suppressed.
            ind_pt1 = *get_ijth_elem_arr_int(list_del_pts, i, 0);
            if ((ind_pt1 > -1) && (ind_pt1<(int64_t)size_vs)){
                ind_pt2 = *get_ijth_elem_arr_int(list_del_pts, i, 1);

                pt3D = *get_ith_elem_vec_pts2D(vertices_tnp1, ind_pt1);
                pt3D_2 = *get_ith_elem_vec_pts2D(vertices_tnp1, ind_pt2);

                pt3D.x = 0.5*(pt3D.x + pt3D_2.x);
                pt3D.y = 0.5*(pt3D.y + pt3D_2.y);

                set_ith_elem_vec_pts2D(vertices_tnp1, i, &pt3D);
            } else if (ind_pt1>=(int64_t)size_vs){
                pt = *get_ith_elem_vec_pts2D(p->vertices, ind_pt1);
                set_ith_elem_vec_pts2D(vertices_tnp1, i, &pt);
            }
        }
    }
    
    return vertices_tnp1;
}

//Compute the polyhedron in space-time given vertices at time tn+dt.
//It supposes no intersection occurs at time tn+dt.
static Polyhedron3D* build_space_time_cell_given_tnp1_vertices(const Polygon2D* fn, const Vector_points2D* vertices_tnp1, my_real_c dt, bool split){
    uint64_t i, j, ie, size_edge_indices;
    int8_t fne_ptind;
    long int val;
    my_real_c val_r;
    Point2D *pt2D;
    Point3D *pt3D;
    GrB_Index nb_edges, nb_rows_faces, nb_cols_faces, nb_cols_newedges, nb_cols_newfaces;
    GrB_Index curr_edge, v_edges_index, curr_face;
    GrB_Index pt_index1, pt_index2, temp;
    GrB_Index *I_index, *J_index;
    GrB_Info infogrb;
    GrB_Vector ed_i, nz_ei, val_nz_ei;
    GrB_Vector fj, extr_vals_fj, edge_indices;
    GrB_Matrix emptyNE, emptySW, emptyE_N, emptyE_S;
    GrB_Matrix *new_edges = (GrB_Matrix*)malloc(sizeof(GrB_Matrix));
    GrB_Matrix *new_faces = (GrB_Matrix*)malloc(sizeof(GrB_Matrix));
    GrB_Matrix *new_volume = (GrB_Matrix*)malloc(sizeof(GrB_Matrix));
    Vector_int *status_faces;
    const uint64_t nb_pts = fn->vertices->size;
    Vector_points3D *new_vertices = alloc_with_capacity_vec_pts3D(2*nb_pts);
    Polyhedron3D* p;
    
    pt3D = (Point3D*)malloc(sizeof(Point3D));
    for (i = 0; i<nb_pts; i++){
        pt2D = get_ith_elem_vec_pts2D(fn->vertices, i);
        *pt3D = (Point3D){pt2D->x, pt2D->y, 0.0};
        set_ith_elem_vec_pts3D(new_vertices, i, pt3D);
    }
    //for (i = nb_pts; i<2*nb_pts; i++){
    //    pt2D = get_ith_elem_vec_pts2D(fn->vertices, i-nb_pts);
    //    v = get_ith_elem_vec_pts2D(vs, i-nb_pts);
    //    *pt3D = (Point3D){pt2D->x + dt*v->x, pt2D->y + dt*v->y, dt};
    //    set_ith_elem_vec_pts3D(new_vertices, i, pt3D);
    //}
    for(i = 0; i<nb_pts; i++){
        pt2D = get_ith_elem_vec_pts2D(vertices_tnp1, i);
        *pt3D = (Point3D){pt2D->x, pt2D->y, dt};
        set_ith_elem_vec_pts3D(new_vertices, nb_pts + i, pt3D);
    }

    GrB_Matrix_ncols(&nb_edges, *(fn->edges));

    GrB_Matrix_new(&emptyNE, GrB_INT8, nb_pts, nb_edges);
    GrB_Matrix_new(&emptySW, GrB_INT8, nb_pts, nb_edges);
    if (split){
        GrB_Matrix_new(&emptyE_N, GrB_INT8, nb_pts, 2*nb_pts);
        GrB_Matrix_new(&emptyE_S, GrB_INT8, nb_pts, 2*nb_pts);
        GrB_Matrix_new(new_edges, GrB_INT8, 2*nb_pts, 2*(nb_edges + nb_pts));
    } else {
        GrB_Matrix_new(&emptyE_N, GrB_INT8, nb_pts, nb_pts);
        GrB_Matrix_new(&emptyE_S, GrB_INT8, nb_pts, nb_pts);
        GrB_Matrix_new(new_edges, GrB_INT8, 2*nb_pts, 2*nb_edges + nb_pts);
    }
    CHECK_GB_CALL(infogrb, \
        GxB_Matrix_concat(*new_edges, \
            (GrB_Matrix[]){*(fn->edges), emptyNE, emptyE_N, emptySW, *(fn->edges), emptyE_S}, \
            2, 3, GrB_NULL)) //new_edges = [fn->edges, 0, 0) 0, fn->edges, 0]

    GrB_Matrix_ncols(&nb_cols_newedges, *new_edges);
    GrB_Matrix_ncols(&nb_cols_faces, *(fn->faces));
    

    if (split){
        GrB_Matrix_new(new_faces, GrB_INT8, nb_cols_newedges, 2*(nb_cols_faces + nb_edges));
    } else {
        GrB_Matrix_new(new_faces, GrB_INT8, nb_cols_newedges, 2*nb_cols_faces + nb_edges);
    }
    I_index = (GrB_Index*)malloc(nb_edges*sizeof(GrB_Index));
    J_index = (GrB_Index*)malloc(nb_cols_faces*sizeof(GrB_Index));
    for (i = 0; i<nb_edges; i++){
        I_index[i] = nb_edges + i;
    }
    for (i = 0; i<nb_cols_faces; i++){
        J_index[i] = nb_cols_faces + i;
    }
    GxB_Matrix_subassign(*new_faces, GrB_NULL, GrB_NULL, *(fn->faces), I_index, nb_edges, J_index, nb_cols_faces, GrB_NULL);
    GrB_apply(*new_faces, GrB_NULL, GrB_NULL, GrB_AINV_INT8, *new_faces, GrB_NULL);
    for (i = 0; i<nb_edges; i++){
        I_index[i] = i;
    }
    for (i = 0; i<nb_cols_faces; i++){
        J_index[i] = i;
    }
    GxB_Matrix_subassign(*new_faces, GrB_NULL, GrB_NULL, *(fn->faces), I_index, nb_edges, J_index, nb_cols_faces, GrB_NULL);

    GrB_Matrix_ncols(&nb_cols_newfaces, *new_faces);
    status_faces = alloc_with_capacity_vec_int(nb_cols_newfaces);
    val = 1;
    for(i = 0; i < nb_cols_faces; i++){
        set_ith_elem_vec_int(status_faces, i, &val);
    }
    val = 2;
    for(i = nb_cols_faces; i < 2*nb_cols_faces; i++){
        set_ith_elem_vec_int(status_faces, i, &val);
    }
    val = 0;
    for(i = 2*nb_cols_faces; i < nb_cols_newfaces; i++){
        set_ith_elem_vec_int(status_faces, i, &val);
    }

    GrB_Matrix_new(new_volume, GrB_INT8, nb_cols_newfaces, 1);
    for(i=0; i<nb_edges; i++){
        CHECK_GB_CALL(infogrb, GrB_Matrix_setElement(*new_volume, -1, i, 0))
    }
    for(i=nb_edges; i<nb_cols_newfaces; i++){
        CHECK_GB_CALL(infogrb, GrB_Matrix_setElement(*new_volume, 1, i, 0))
    }

    curr_edge = 2*nb_edges;
    v_edges_index = curr_edge;
    curr_face = 2*nb_cols_faces;

    for(i=0; i<nb_pts; i++){
        CHECK_GB_CALL(infogrb, GrB_Matrix_setElement(*new_edges, -1, i, curr_edge))
        CHECK_GB_CALL(infogrb, GrB_Matrix_setElement(*new_edges, 1, i + nb_pts, curr_edge))
        curr_edge++;
    }

    //At this point, new_edges has the following organisation :
    //First nb_edges: original edges of f at t=tn
    //Following nb_edges: edges of f at t=tn+dt
    //Following nb_pts: edges linking pt at t=tn and pt at t=tn+dt
    GrB_Matrix_ncols(&nb_rows_faces, *(fn->faces));
    infogrb = GrB_Vector_new(&ed_i, GrB_INT8, nb_pts);
    infogrb = GrB_Vector_new(&nz_ei, GrB_UINT64, nb_pts);
    infogrb = GrB_Vector_new(&val_nz_ei, GrB_INT8, nb_pts);
    GrB_Vector_new(&fj, GrB_INT8, nb_edges);
    GrB_Vector_new(&edge_indices, GrB_UINT64, nb_edges);
    GrB_Vector_new(&extr_vals_fj, GrB_INT8, nb_edges);
    if (split){ //Each edge in 2D is turned into 2 faces (triangulation)
        //Now, we create the diagonal edges, and create all faces between t=tn and t=tn+dt
        for(j=0; j<nb_cols_faces; j++){
            CHECK_GB_CALL(infogrb, GrB_extract(fj, GrB_NULL, GrB_NULL, *(fn->faces), GrB_ALL, 1, j, GrB_NULL)) //Get indices of edges composing face i
            CHECK_GB_CALL(infogrb, GxB_Vector_extractTuples_Vector(edge_indices, extr_vals_fj, fj, GrB_NULL))
            CHECK_GB_CALL(infogrb, GrB_Vector_size(&size_edge_indices, edge_indices))

            for (ie=0; ie<size_edge_indices; ie++){
                CHECK_GB_CALL(infogrb, GrB_Vector_extractElement(&i, edge_indices, ie))
                CHECK_GB_CALL(infogrb, GrB_extract(ed_i, GrB_NULL, GrB_NULL, *(fn->edges), GrB_ALL, 1, i, GrB_NULL)) //Get point indices of edge i
                CHECK_GB_CALL(infogrb, GxB_Vector_extractTuples_Vector(nz_ei, val_nz_ei, ed_i, GrB_NULL))

                CHECK_GB_CALL(infogrb, GrB_Vector_extractElement(&pt_index1, nz_ei, 0))
                CHECK_GB_CALL(infogrb, GrB_Vector_extractElement(&pt_index2, nz_ei, 1))

                //CHECK_GB_CALL(infogrb, GrB_Matrix_extractElement(&fne_ptind, *(fn->edges), pt_index1, i))
                CHECK_GB_CALL(infogrb, GrB_Vector_extractElement(&fne_ptind, val_nz_ei, 0))
                if (fne_ptind > 0){ //pt_index1 should always be the index where the edges starts
                    temp = pt_index1;
                    pt_index1 = pt_index2;
                    pt_index2 = temp;
                }

                CHECK_GB_CALL(infogrb, GrB_Matrix_setElement(*new_edges,  1, pt_index2 + nb_pts, curr_edge)) //diagonal edge
                CHECK_GB_CALL(infogrb, GrB_Matrix_setElement(*new_edges, -1, pt_index1         , curr_edge)) //diagonal edge

                CHECK_GB_CALL(infogrb, GrB_Matrix_setElement(*new_faces,  1, i                        , curr_face)) //edge at time tn
                CHECK_GB_CALL(infogrb, GrB_Matrix_setElement(*new_faces,  1, v_edges_index + pt_index2, curr_face)) //vertical edge
                CHECK_GB_CALL(infogrb, GrB_Matrix_setElement(*new_faces, -1, curr_edge                , curr_face)) //diagonal edge

                CHECK_GB_CALL(infogrb, GrB_Matrix_setElement(*new_faces, -1, i + nb_edges             , curr_face + 1)) //edge at time tn+dt
                CHECK_GB_CALL(infogrb, GrB_Matrix_setElement(*new_faces, -1, v_edges_index + pt_index1, curr_face + 1)) //vertical edge of pt_index1 between t=tn and t=tn+dt
                CHECK_GB_CALL(infogrb, GrB_Matrix_setElement(*new_faces,  1, curr_edge                , curr_face + 1)) //diagonal edge

                val = 3+i;
                set_ith_elem_vec_int(status_faces, curr_face    , &val);
                set_ith_elem_vec_int(status_faces, curr_face + 1, &val);

                CHECK_GB_CALL(infogrb, GrB_Matrix_extractElement(&fne_ptind, *(fn->faces), i, j))
                CHECK_GB_CALL(infogrb, GrB_Matrix_setElement(*new_volume, fne_ptind, curr_face    , 0))
                CHECK_GB_CALL(infogrb, GrB_Matrix_setElement(*new_volume, fne_ptind, curr_face + 1, 0))

                curr_edge += 1;
                curr_face += 2;
            }
        }
    } else {//Each edge in 2D is turned into 1 single face (no triangulation)
        //Now, we create all faces between t=tn and t=tn+dt
        for(j=0; j<nb_rows_faces; j++){
            CHECK_GB_CALL(infogrb, GrB_extract(fj, GrB_NULL, GrB_NULL, *(fn->faces), GrB_ALL, 1, j, GrB_NULL)) //Get indices of edges composing face i
            CHECK_GB_CALL(infogrb, GxB_Vector_extractTuples_Vector(edge_indices, extr_vals_fj, fj, GrB_NULL))
            CHECK_GB_CALL(infogrb, GrB_Vector_size(&size_edge_indices, edge_indices))

            for (ie=0; ie<size_edge_indices; ie++){
                CHECK_GB_CALL(infogrb, GrB_Vector_extractElement(&i, edge_indices, ie))
                CHECK_GB_CALL(infogrb, GrB_extract(ed_i, GrB_NULL, GrB_NULL, *(fn->edges), GrB_ALL, 1, i, GrB_NULL)) //Get point indices of edge i
                CHECK_GB_CALL(infogrb, GxB_Vector_extractTuples_Vector(nz_ei, val_nz_ei, ed_i, GrB_NULL))

                CHECK_GB_CALL(infogrb, GrB_Vector_extractElement(&pt_index1, nz_ei, 0))
                CHECK_GB_CALL(infogrb, GrB_Vector_extractElement(&pt_index2, nz_ei, 1))
                
                CHECK_GB_CALL(infogrb, GrB_Matrix_extractElement(&fne_ptind, *(fn->edges), pt_index1, i))
                if (fne_ptind > 0){ //pt_index1 should always be the index where the edges starts
                    temp = pt_index1;
                    pt_index1 = pt_index2;
                    pt_index2 = temp;
                }

                CHECK_GB_CALL(infogrb, GrB_Matrix_setElement(*new_faces,  1, i                        , curr_face)) //edge at time tn
                CHECK_GB_CALL(infogrb, GrB_Matrix_setElement(*new_faces, -1, i + nb_edges             , curr_face)) //edge at time tn+dt
                CHECK_GB_CALL(infogrb, GrB_Matrix_setElement(*new_faces,  1, v_edges_index + pt_index2, curr_face)) //vertical edge
                CHECK_GB_CALL(infogrb, GrB_Matrix_setElement(*new_faces, -1, v_edges_index + pt_index1, curr_face)) //vertical edge of pt_index1 between t=tn and t=tn+dt

                val = 3+i;
                set_ith_elem_vec_int(status_faces, curr_face, &val);

                CHECK_GB_CALL(infogrb, GrB_Matrix_extractElement(&fne_ptind, *(fn->faces), i, j))
                CHECK_GB_CALL(infogrb, GrB_Matrix_setElement(*new_volume, fne_ptind, curr_face, 0))

                curr_face += 1;
            }
        }
    }

    p = new_Polyhedron3D_vefvs(new_vertices, new_edges, new_faces, new_volume, status_faces);

    free(pt3D);
    GrB_Matrix_free(&emptyNE);
    GrB_Matrix_free(&emptySW);
    GrB_Matrix_free(&emptyE_N);
    GrB_Matrix_free(&emptyE_S);
    GrB_Vector_free(&ed_i);
    GrB_Vector_free(&nz_ei);
    GrB_Vector_free(&val_nz_ei);
    GrB_free(&fj);
    GrB_free(&extr_vals_fj);
    GrB_free(&edge_indices);
    dealloc_vec_int(status_faces); free(status_faces);
    dealloc_vec_pts3D(new_vertices); free(new_vertices);
    GrB_Matrix_free(new_edges); free(new_edges);
    GrB_Matrix_free(new_faces); free(new_faces);
    GrB_Matrix_free(new_volume); free(new_volume);
    free(I_index);
    free(J_index);

    return p;
}

//Compute the polyhedron in space-time given the polygon at time tn+dt.
//It supposes some intersections occur at time tn+dt and where already treated in fn (at time tn, with all the backpropagation) and at time tn+dt (with all intersection points added, edges modified, etc).
static Polyhedron3D* build_space_time_cell_with_intersection(const Polygon2D* fn, const Polygon2D* fnp1, my_real_c dt,\
                                                    const Vector_int8* pt_in_or_out_split, const Vector_int8* pt_in_or_out_fuse){
    Vector_points3D* new_vertices;
    uint64_t i, nb_pts1, nb_pts2, nb_pts_in; 
    GrB_Index j, ie;
    GrB_Index nb_edges1, nb_edges2;
    GrB_Index nb_faces1, nb_faces2;
    GrB_Index pt_index1, pt_index2, temp;
    GrB_Index nb_cols_newedges, nb_cols_newfaces, size_edge_indices, size_nz_ei;
    Point3D pt3D;
    GrB_Matrix emptyNE, emptySW, emptyE_N, emptyE_S;
    GrB_Matrix *new_edges = (GrB_Matrix*)malloc(sizeof(GrB_Matrix));
    GrB_Matrix *new_faces = (GrB_Matrix*)malloc(sizeof(GrB_Matrix));
    GrB_Matrix *new_volume = (GrB_Matrix*)malloc(sizeof(GrB_Matrix));
    Vector_int *status_faces;
    GrB_Index *I_index, *J_index;
    GrB_Index curr_edge, v_edges_index, curr_face;
    GrB_Vector fj, extr_vals_fj, edge_indices;
    GrB_Vector ed_i, nz_ei, val_nz_ei;
    long val;
    my_real_c val_r;
    int8_t in_out_f1, in_out_f2, in_out_s1, in_out_s2;
    Polyhedron3D* res_p;
    GrB_Info infogrb;
   
    nb_pts1 = fn->vertices->size;
    nb_pts2 = fnp1->vertices->size;
    GrB_Matrix_ncols(&nb_edges1, *(fn->edges));
    GrB_Matrix_ncols(&nb_edges2, *(fnp1->edges));
    GrB_Matrix_ncols(&nb_faces1, *(fn->faces));
    GrB_Matrix_ncols(&nb_faces2, *(fnp1->faces));
    new_vertices = alloc_with_capacity_vec_pts3D(nb_pts1 + nb_pts2);

    pt3D = (Point3D){0.,0.,0.};
    for(i=0; i<nb_pts1; i++){
        pt3D.x = get_ith_elem_vec_pts2D(fn->vertices, i)->x;
        pt3D.y = get_ith_elem_vec_pts2D(fn->vertices, i)->y;
        set_ith_elem_vec_pts3D(new_vertices, i, &pt3D);
    }

    pt3D.t = dt;
    for(i=0; i<nb_pts2; i++){
        pt3D.x = get_ith_elem_vec_pts2D(fnp1->vertices, i)->x;
        pt3D.y = get_ith_elem_vec_pts2D(fnp1->vertices, i)->y;
        set_ith_elem_vec_pts3D(new_vertices, i+nb_pts1, &pt3D);
    }

    nb_pts_in = 0;
    for(i=0; i<pt_in_or_out_fuse->size; i++){
        if (*get_ith_elem_vec_int8(pt_in_or_out_fuse, i)>=0){
            nb_pts_in++;
        }
        if (*get_ith_elem_vec_int8(pt_in_or_out_split, i)>=0){
            nb_pts_in++;
        }
    }
    //pt_indexes = rowvals(fn.edges)
    //rowvals_faces = rowvals(fn.faces)

    GrB_Matrix_new(new_edges, GrB_INT8, nb_pts1 + nb_pts2, nb_edges1 + nb_edges2 + nb_pts1 + nb_pts_in - 1);
    GrB_Matrix_new(&emptyNE, GrB_INT8, nb_pts1, nb_edges2);
    GrB_Matrix_new(&emptySW, GrB_INT8, nb_pts2, nb_edges1);
    GrB_Matrix_new(&emptyE_N, GrB_INT8, nb_pts1, nb_pts1 + nb_pts_in - 1);
    GrB_Matrix_new(&emptyE_S, GrB_INT8, nb_pts2, nb_pts1 + nb_pts_in - 1);

    CHECK_GB_CALL(infogrb, \
        GxB_Matrix_concat(*new_edges, \
            (GrB_Matrix[]){*(fn->edges), emptyNE, emptyE_N, emptySW, *(fnp1->edges), emptyE_S}, \
            2, 3, GrB_NULL)) //new_edges = [fn->edges, 0, 0) 0, fnp1->edges, 0]
    GrB_Matrix_ncols(&nb_cols_newedges, *new_edges);

    GrB_Matrix_new(new_faces, GrB_INT8, nb_cols_newedges, nb_faces1 + nb_faces2 + nb_edges1 + nb_edges2);
    I_index = (GrB_Index*)malloc((nb_edges1>nb_edges2? nb_edges1 : nb_edges2)*sizeof(GrB_Index));
    J_index = (GrB_Index*)malloc((nb_faces1>nb_faces2? nb_faces1 : nb_faces2)*sizeof(GrB_Index));
    for (i = 0; i<nb_edges2; i++){
        I_index[i] = nb_edges1 + i;
    }
    for (i = 0; i<nb_faces2; i++){
        J_index[i] = nb_faces1 + i;
    }
    CHECK_GB_CALL(infogrb, GxB_Matrix_subassign(*new_faces, GrB_NULL, GrB_NULL, *(fn->faces), I_index, nb_edges2, J_index, nb_faces2, GrB_NULL))
    CHECK_GB_CALL(infogrb, GrB_apply(*new_faces, GrB_NULL, GrB_NULL, GrB_AINV_INT8, *new_faces, GrB_NULL))
    for (i = 0; i<nb_edges1; i++){
        I_index[i] = i;
    }
    for (i = 0; i<nb_faces1; i++){
        J_index[i] = i;
    }
    CHECK_GB_CALL(infogrb, GxB_Matrix_subassign(*new_faces, GrB_NULL, GrB_NULL, *(fn->faces), I_index, nb_edges1, J_index, nb_faces1, GrB_NULL))
    GrB_Matrix_ncols(&nb_cols_newfaces, *new_faces);

    status_faces = alloc_with_capacity_vec_int(nb_cols_newfaces);
    val = 1;
    for(i = 0; i < nb_faces1; i++){
        set_ith_elem_vec_int(status_faces, i, &val);
    }
    val = 2;
    for(i = nb_faces1; i < nb_faces1 + nb_faces2; i++){
        set_ith_elem_vec_int(status_faces, i, &val);
    }
    val = 0;
    for(i = nb_faces1 + nb_faces2; i < nb_cols_newfaces; i++){
        set_ith_elem_vec_int(status_faces, i, &val);
    }

    GrB_Matrix_new(new_volume, GrB_INT8, nb_cols_newfaces, 1);
    for(i=0; i<nb_edges1; i++){
        CHECK_GB_CALL(infogrb, GrB_Matrix_setElement(*new_volume, -1, i, 0))
    }
    for(i=nb_edges1; i<nb_cols_newfaces; i++){
        CHECK_GB_CALL(infogrb, GrB_Matrix_setElement(*new_volume, 1, i, 0))
    }

    curr_edge = nb_edges1 + nb_edges2;
    v_edges_index = curr_edge;
    curr_face = nb_faces1 + nb_faces2;

    for(i=0; i<nb_pts1; i++){
        CHECK_GB_CALL(infogrb, GrB_Matrix_setElement(*new_edges, -1, i, curr_edge))
        CHECK_GB_CALL(infogrb, GrB_Matrix_setElement(*new_edges, 1, i + nb_pts1, curr_edge))
        curr_edge++;
    }

    //At this point, new_edges has the following organisation :
    //First nb_edges: original edges of f at t=tn
    //Following nb_edges: edges of f at t=tn+dt
    //Following nb_pts: edges linking pt at t=tn and pt at t=tn+dt
    GrB_Vector_new(&ed_i, GrB_INT8, nb_edges1);
    GrB_Vector_new(&nz_ei, GrB_UINT64, nb_edges1);
    GrB_Vector_new(&val_nz_ei, GrB_INT8, nb_edges1);
    GrB_Vector_new(&fj, GrB_INT8, nb_edges1);
    GrB_Vector_new(&edge_indices, GrB_UINT64, nb_edges1);
    GrB_Vector_new(&extr_vals_fj, GrB_INT8, nb_edges1);
    for(j=0; j<nb_faces1; j++){
        CHECK_GB_CALL(infogrb, GrB_extract(fj, GrB_NULL, GrB_NULL, *(fn->faces), GrB_ALL, 1, j, GrB_NULL)) //Get indices of edges composing face i
        CHECK_GB_CALL(infogrb, GxB_Vector_extractTuples_Vector(edge_indices, extr_vals_fj, fj, GrB_NULL))
        CHECK_GB_CALL(infogrb, GrB_Vector_size(&size_edge_indices, edge_indices))

        for (ie=0; ie<size_edge_indices; ie++){
            CHECK_GB_CALL(infogrb, GrB_Vector_extractElement(&i, edge_indices, ie))
            CHECK_GB_CALL(infogrb, GrB_extract(ed_i, GrB_NULL, GrB_NULL, *(fn->edges), GrB_ALL, 1, i, GrB_NULL)) //Get point indices of edge i
            CHECK_GB_CALL(infogrb, GxB_Vector_extractTuples_Vector(nz_ei, val_nz_ei, ed_i, GrB_NULL))
            CHECK_GB_CALL(infogrb, GrB_Vector_size(&size_nz_ei, nz_ei))

            if(size_nz_ei>0){
                infogrb = GrB_Vector_extractElement(&pt_index1, nz_ei, 0);
                infogrb = GrB_Vector_extractElement(&pt_index2, nz_ei, 1);
            
                //GrB_Matrix_extractElement(&fne_ptind, *(fn->edges), pt_index1, i);
                GrB_Vector_extractElement(&in_out_f1, val_nz_ei, 0);
                if (in_out_f1 > 0){ //pt_index1 should always be the index where the edge starts
                    temp = pt_index1;
                    pt_index1 = pt_index2;
                    pt_index2 = temp;
                }

                in_out_f1 = *get_ith_elem_vec_int8(pt_in_or_out_fuse, pt_index1);
                in_out_f2 = *get_ith_elem_vec_int8(pt_in_or_out_fuse, pt_index2);
                in_out_s1 = *get_ith_elem_vec_int8(pt_in_or_out_split, pt_index1);
                in_out_s2 = *get_ith_elem_vec_int8(pt_in_or_out_split, pt_index2);
                //if (((pt_in_or_out_fuse[pt_index1] ≥ 0) && (pt_in_or_out_fuse[pt_index2] ≥ 0)) && ((pt_in_or_out_split[pt_index1] ≥ 0) && (pt_in_or_out_split[pt_index2] ≥ 0)))
                if (((in_out_f1 >= 0) && (in_out_f2 >= 0)) && ((in_out_s1 >= 0) && (in_out_s2 >= 0))){
                    CHECK_GB_CALL(infogrb, GrB_Matrix_setElement(*new_edges,  1, pt_index2 + nb_pts1, curr_edge)) //diagonal edge
                    CHECK_GB_CALL(infogrb, GrB_Matrix_setElement(*new_edges, -1, pt_index1          , curr_edge)) //diagonal edge

                    CHECK_GB_CALL(infogrb, GrB_Matrix_setElement(*new_faces,  1, i                        , curr_face)) //edge at time tn
                    CHECK_GB_CALL(infogrb, GrB_Matrix_setElement(*new_faces,  1, v_edges_index + pt_index2, curr_face)) //vertical edge
                    CHECK_GB_CALL(infogrb, GrB_Matrix_setElement(*new_faces, -1, curr_edge                , curr_face)) //diagonal edge

                    CHECK_GB_CALL(infogrb, GrB_Matrix_setElement(*new_faces, -1, i + nb_edges1            , curr_face + 1)) //edge at time tn+dt
                    CHECK_GB_CALL(infogrb, GrB_Matrix_setElement(*new_faces, -1, v_edges_index + pt_index1, curr_face + 1)) //vertical edge of pt_index1 between t=tn and t=tn+dt
                    CHECK_GB_CALL(infogrb, GrB_Matrix_setElement(*new_faces,  1, curr_edge                , curr_face + 1)) //diagonal edge

                    val = -(3+i);
                    set_ith_elem_vec_int(status_faces, curr_face    , &val);
                    set_ith_elem_vec_int(status_faces, curr_face + 1, &val);

                    CHECK_GB_CALL(infogrb, GrB_Matrix_extractElement(&in_out_f1, *(fn->faces), i, j))
                    CHECK_GB_CALL(infogrb, GrB_Matrix_setElement(*new_volume, in_out_f1, curr_face    , 0))
                    CHECK_GB_CALL(infogrb, GrB_Matrix_setElement(*new_volume, in_out_f1, curr_face + 1, 0))

                    curr_edge += 1;
                    curr_face += 2;
                } else {
                    CHECK_GB_CALL(infogrb, GrB_Matrix_setElement(*new_faces,  1, i                        , curr_face)) //edge at time tn
                    CHECK_GB_CALL(infogrb, GrB_Matrix_setElement(*new_faces, -1, i + nb_edges1             , curr_face)) //edge at time tn+dt
                    CHECK_GB_CALL(infogrb, GrB_Matrix_setElement(*new_faces,  1, v_edges_index + pt_index2, curr_face)) //vertical edge
                    CHECK_GB_CALL(infogrb, GrB_Matrix_setElement(*new_faces, -1, v_edges_index + pt_index1, curr_face)) //vertical edge of pt_index1 between t=tn and t=tn+dt

                    val = -(3+i);
                    set_ith_elem_vec_int(status_faces, curr_face, &val);

                    CHECK_GB_CALL(infogrb, GrB_Matrix_extractElement(&in_out_f1, *(fn->faces), i, j))
                    CHECK_GB_CALL(infogrb, GrB_Matrix_setElement(*new_volume, in_out_f1, curr_face, 0))

                    curr_face += 1;
                }
            }
        }
    }

    CHECK_GB_CALL(infogrb, GrB_Matrix_resize(*new_edges, nb_pts1 + nb_pts2, curr_edge))
    CHECK_GB_CALL(infogrb, GrB_Matrix_resize(*new_faces, curr_edge, curr_face))
    CHECK_GB_CALL(infogrb, GrB_Matrix_resize(*new_volume, curr_face, 1))
    status_faces->size = curr_face;
    res_p = new_Polyhedron3D_vefvs(new_vertices, new_edges, new_faces, new_volume, status_faces);

    GrB_free(&emptyNE);
    GrB_free(&emptySW);
    GrB_free(&emptyE_N);
    GrB_free(&emptyE_S);
    GrB_free(&fj);
    GrB_free(&edge_indices);
    GrB_free(&extr_vals_fj);
    GrB_free(&ed_i);
    GrB_free(&nz_ei);
    GrB_free(&val_nz_ei);

    dealloc_vec_pts3D(new_vertices); free(new_vertices);
    GrB_free(new_edges);
    if(new_edges) free(new_edges);
    GrB_free(new_faces);
    if(new_faces) free(new_faces);
    GrB_free(new_volume);
    if(new_volume) free(new_volume);
    dealloc_vec_int(status_faces); free(status_faces);
    free(I_index);
    free(J_index);

    return res_p;
}


/// @brief Using a polygon `fn` defined at time t^n and a set of vectors `vs` defining the translation of each point of `fn` at time t^{n+1} = t^n+`dt`,
///        this function builds a 3D polyhedron (2D + time) connecting `fn` and the polygon at time t^{n+1}. 
/// @details In the resulting polyhedron `status_faces`, 1 denotes the face created at t^n, 
///          2 the face created at t^{n+1}, and i+2 the face created in time-space from edge i.
/// @param fn [IN] polygon at time t^n
/// @param vs* [IN] Vector translating points of `fn` over time `dt` to form the polygon at time t^{n+1}
/// @param dt [IN] time-step 
/// @param split [IN] flag to triangulate (or not) faces in time linking the edges at time t^n and t^{n+1}.
Polyhedron3D* build_space2D_time_cell(const Polygon2D *fn, const my_real_c *vsx, const my_real_c* vsy, uint64_t size_vs, const my_real_c dt, bool split, Array_int *list_del_pts){
    Vector_points2D* vertices_tnp1 = build_vertices_tnp1(fn, vsx, vsy, size_vs, dt, list_del_pts);
    Polyhedron3D* p = build_space_time_cell_given_tnp1_vertices(fn, vertices_tnp1, dt, split);
    
    dealloc_vec_pts2D(vertices_tnp1); free(vertices_tnp1);
    return p;
}

//Detect all edges too long and split it in the middle.
static void refine_interface(Polygon2D *p, my_real_c maximal_length){
    uint64_t i, nb_edges;
    Point2D *ext1, *ext2;
    my_real_c norm_e;
    Point2D new_pt;

    GrB_Matrix_ncols(&nb_edges, *(p->edges));
    for(i=0; i<nb_edges; i++){
        retrieve_ith_edge2D(p->vertices, p->edges, i, &ext1, &ext2);
        if (ext1 && ext2){
            if ((ext1->x != ext2->x) || (ext1->y != ext2->y)){
                norm_e = sqrt((ext1->x-ext2->x)*(ext1->x-ext2->x) + (ext1->y-ext2->y)*(ext1->y-ext2->y));
                if (norm_e > maximal_length){
                    new_pt.x = 0.5*(ext1->x + ext2->x);
                    new_pt.y = 0.5*(ext1->y + ext2->y);
                    split_edge(p, i, &new_pt);
                }
            }
        }
    }
}

//Based on list_changed_edges, listing all edges that should be suppressed and how, it suppresses all given edges (usually because they are too short or because the angle is too acute).
static void coarsen_interface(Polygon2D *p, Array_int *list_changed_edges){
    uint64_t i;
    int64_t ind_e1_fused, ind_e2_fused, ind_f;
    GrB_Index nb_pts, nb_faces;
    GrB_Matrix ef_m;

    nb_pts = p->vertices->size;
    GrB_Matrix_ncols(&nb_faces, *(p->faces));
    GrB_Matrix_new(&ef_m, GrB_FP64, nb_pts, nb_faces);

    for (i=0; i<p->vertices->size; i++){
        ind_e1_fused = *get_ijth_elem_arr_int(list_changed_edges, i, 0);
        if (ind_e1_fused > -1){
            ind_e2_fused = *get_ijth_elem_arr_int(list_changed_edges, i, 1);
            ind_f = *get_ijth_elem_arr_int(list_changed_edges, i, 2);
            fuse_points(p, ind_e1_fused, ind_e2_fused, ind_f);
        }
    }

    GrB_free(&ef_m);
}

/// @brief Using a polygon `solid` defined at time t^n and a set of vectors `vs` defining the translation of each point of `solid` at time t^{n+1} = t^n+`dt`,
///        this function builds a 3D polyhedron (2D + time) connecting `fn` and the polygon at time t^{n+1}, and updates solid to be the one at time t^{n+1}.
/// @details In the resulting polyhedron `solid3D->status_faces`, 1 denotes the face created at t^n, 
///          2 the face created at t^{n+1}, and i+2 the face created in time-space from edge i.
///          This function also deals with auto-intersection and removes them when they happen.
///          It also refine or coarsen the polygon discretization if an edge is too long, too short and an angle in the polygon is too acute.
/// @param solid [IN] polygon at time t^n
///              [OUT] polygon at time t^{n+1}
/// @param solid3D [OUT] polyhedron (2D space + time) linking solid at time t^n and at time t^{n+1}
/// @param vs* [IN] Vector translating points of `fn` over time `dt` to form the polygon at time t^{n+1}
/// @param dt [IN] time-step
/// @param minimal_length [IN] minimal length the path linking three consecutive points should have.
/// @param maximal_length [IN] maximal length an edge should have.
/// @param minimal_angle [IN] minimal value (in rad) the angle formed by three consecutive points should have.
void update_solid(Polygon2D **solid, Polyhedron3D** solid3D, const my_real_c* vec_move_solidx, const my_real_c* vec_move_solidy, my_real_c dt, \
                    my_real_c minimal_length, my_real_c maximal_length, my_real_c minimal_angle){
    Array_int *list_del_pts,  *list_changed_edges;
    Vector_points2D* vertices_tnp1;
    Polygon2D* solid_new = new_Polygon2D();
    Polygon2D* solid_tnp1 = new_Polygon2D();
    uint64_t i, j, k;
    Vector_points2D* IntersecList = NULL, *IntersecList_fuse = NULL, *IntersecList_split = NULL;
    Vector_uint* edge_intersect1 = NULL, *edge_intersect1_fuse = NULL, *edge_intersect1_split = NULL;
    Vector_uint* edge_intersect2 = NULL, *edge_intersect2_fuse = NULL, *edge_intersect2_split = NULL;
    Vector_points2D *normals_edges = NULL;
    Vector_int8 *pt_in_or_out_split, *pt_in_or_out_fuse;
    GrB_Index nb_faces, nb_edges;
    uint64_t original_nb_pts;
    int8_t zero8 = 0;
    int64_t ell, emm;
    Array_int *pair_intersection_split, *pair_intersection_fuse;
    Vector_uint **faces_to_split, **faces_to_fuse;
    uint64_t size_faces_to_split, size_faces_to_fuse;
    Point2D pt;

    detect_pts_to_delete(*solid, dt, vec_move_solidx, vec_move_solidy, minimal_length, minimal_angle, &list_del_pts, &list_changed_edges);

    original_nb_pts = (*solid)->vertices->size;
    vertices_tnp1 = build_vertices_tnp1(*solid, vec_move_solidx, vec_move_solidy, (*solid)->vertices->size, dt, list_del_pts);

    copy_Polygon2D(*solid, solid_tnp1);

    for (i=0; i<vertices_tnp1->size; i++){
        set_ith_elem_vec_pts2D(solid_tnp1->vertices, i, get_ith_elem_vec_pts2D(vertices_tnp1, i));
    }

    IntersecList = alloc_empty_vec_pts2D();
    edge_intersect1 = alloc_empty_vec_uint();
    edge_intersect2 = alloc_empty_vec_uint();
    find_all_self_intersection(solid_tnp1, IntersecList, edge_intersect1, edge_intersect2);

    if (IntersecList->size>0){
        IntersecList_fuse = alloc_empty_vec_pts2D();
        edge_intersect1_fuse = alloc_empty_vec_uint();
        edge_intersect2_fuse = alloc_empty_vec_uint();
        IntersecList_split = alloc_empty_vec_pts2D();
        edge_intersect1_split = alloc_empty_vec_uint();
        edge_intersect2_split = alloc_empty_vec_uint();
        GrB_Matrix_ncols(&nb_edges, *(solid_tnp1->edges));
        normals_edges = alloc_with_capacity_vec_pts2D(nb_edges);
        pt_in_or_out_fuse = alloc_with_capacity_vec_int8(original_nb_pts);
        pt_in_or_out_split = alloc_with_capacity_vec_int8(original_nb_pts);
        size_faces_to_fuse = 0;
        size_faces_to_split = 0;

        for(i=0; i<original_nb_pts; i++){
            set_ith_elem_vec_int8(pt_in_or_out_fuse, i, &zero8);
            set_ith_elem_vec_int8(pt_in_or_out_split, i, &zero8);
        }

        //We will use solid_new as the final result of all the intersection solving process, 
        //and solid_tnp1 as an intermediate polygon where the intersections occur but are not deleted.
        compute_all_normals2D(solid_tnp1, NULL, normals_edges, NULL);
        in_or_out_intersection(solid_tnp1, normals_edges, IntersecList, edge_intersect1, edge_intersect2, \
                            pt_in_or_out_split->data, IntersecList_split, edge_intersect1_split, edge_intersect2_split,\
                            pt_in_or_out_fuse->data, IntersecList_fuse, edge_intersect1_fuse, edge_intersect2_fuse);
        if (IntersecList_split->size > 0){
            printf("SPLITTING DONE\n");
            find_pair_intersection(solid_tnp1, edge_intersect1_split, edge_intersect2_split, pt_in_or_out_split, &pair_intersection_split);
            break_edges_split_fusion(solid_tnp1, edge_intersect1_split, edge_intersect2_split, pt_in_or_out_split->data, &solid_new);
            polygon_split(solid_new, solid_tnp1->edges,\
                IntersecList_split, edge_intersect1_split, edge_intersect2_split, pt_in_or_out_split->data, &solid_new, &faces_to_split, &size_faces_to_split);
        } else {
            copy_Polygon2D(solid_tnp1, solid_new);
            pair_intersection_split = alloc_with_capacity_arr_int(solid_tnp1->vertices->size, 2);
            ell = -1;
            for(i=0; i<solid_tnp1->vertices->size; i++){
                for(j=0; j<2; j++){
                    set_ijth_elem_arr_int(pair_intersection_split, i, j, &ell);
                }
            }
            GrB_Matrix_ncols(&nb_faces, *(solid_tnp1->faces));
            faces_to_split = (Vector_uint**)malloc(nb_faces*sizeof(Vector_uint*));
            for(i=0; i<nb_faces; i++){
                faces_to_split[i] = alloc_empty_vec_uint();
            }
        }
        if (IntersecList_fuse->size >0){
            printf("FUSION DONE\n");
            find_pair_intersection(solid_tnp1, edge_intersect1_fuse, edge_intersect2_fuse, pt_in_or_out_fuse, &pair_intersection_fuse);
            break_edges_split_fusion(solid_new, edge_intersect1_fuse, edge_intersect2_fuse, pt_in_or_out_fuse->data, &solid_new);
            polygons_fusion(solid_new, solid_tnp1->edges, IntersecList_fuse, edge_intersect1_fuse, edge_intersect2_fuse, \
                            pt_in_or_out_fuse->data, &solid_new, &faces_to_fuse, &size_faces_to_fuse);
        } else {
            pair_intersection_fuse = alloc_with_capacity_arr_int(solid_tnp1->vertices->size, 2);
            ell = -1;
            for(i=0; i<solid_tnp1->vertices->size; i++){
                for(j=0; j<2; j++){
                    set_ijth_elem_arr_int(pair_intersection_fuse, i, j, &ell);
                }
            }
            GrB_Matrix_ncols(&nb_faces, *(solid_tnp1->faces));
            faces_to_fuse = (Vector_uint**)malloc(nb_faces*sizeof(Vector_uint*));
            for(i=0; i<nb_faces; i++){
                faces_to_fuse[i] = alloc_empty_vec_uint();
            }
        }

        if (size_faces_to_split>0)
            create_new_faces_split(solid_new, faces_to_split, size_faces_to_split, &solid_new);
        if (size_faces_to_fuse>0)
            fuse_faces(solid_new, faces_to_fuse, size_faces_to_fuse, &solid_new);

        clean_Polygon2D(solid_new, &solid_new);
        detect_pts_to_delete(solid_new, dt, vec_move_solidx, vec_move_solidy, minimal_length, minimal_angle, &list_del_pts, &list_changed_edges);
        if (list_del_pts->nrows < solid_new->vertices->size){
            solid_new->vertices->size = list_del_pts->nrows;
        }

        //solid and solid_tnp1 will now be modified to be the faces of the polyhedron at tn and tn+dt
        //First: backpropagate the intersection point on the face at tn.
        backpropagate_pts(*solid, vertices_tnp1, IntersecList_split, edge_intersect1_split, edge_intersect2_split, pt_in_or_out_split);
        if(pt_in_or_out_fuse->size < pt_in_or_out_split->size){
            zero8 = 0;
            for(i=pt_in_or_out_fuse->size; i < pt_in_or_out_split->size; i++){
                set_ith_elem_vec_int8(pt_in_or_out_fuse, i, &zero8);
            }
        }
        backpropagate_pts(*solid, vertices_tnp1, IntersecList_fuse, edge_intersect1_fuse, edge_intersect2_fuse, pt_in_or_out_fuse);
        if(pt_in_or_out_fuse->size > pt_in_or_out_split->size){
            zero8 = 0;
            for(i=pt_in_or_out_split->size; i < pt_in_or_out_fuse->size; i++){
                set_ith_elem_vec_int8(pt_in_or_out_split, i, &zero8);
            }
        }

        //Second: break the edges where the intersection occurs in two parts.
        for(i=0; i<edge_intersect1_split->size; i++){
            split_edge(solid_tnp1, edge_intersect1_split->data[i], get_ith_elem_vec_pts2D(IntersecList_split, i));
            split_edge(solid_tnp1, edge_intersect2_split->data[i], get_ith_elem_vec_pts2D(IntersecList_split, i));
        }

        for(i=0; i<edge_intersect1_fuse->size; i++){
            split_edge(solid_tnp1, edge_intersect1_fuse->data[i], get_ith_elem_vec_pts2D(IntersecList_fuse,i));
            split_edge(solid_tnp1, edge_intersect2_fuse->data[i], get_ith_elem_vec_pts2D(IntersecList_fuse,i));
        }

        //Third: project all deleted points on the edge connecting the two intersection points deleting them, and create new edges.
        for(i=0; i<original_nb_pts; i++){
            ell = *get_ijth_elem_arr_int(pair_intersection_split, i, 0);
            emm = *get_ijth_elem_arr_int(pair_intersection_split, i, 1);
            if (ell > -1){
                j = (uint64_t) ell;
                k = (uint64_t) emm;
                pt = project_on_edge(get_ith_elem_vec_pts2D(solid_tnp1->vertices, i), 
                                    get_ith_elem_vec_pts2D(IntersecList_split,j),\
                                    get_ith_elem_vec_pts2D(IntersecList_split,k));
                set_ith_elem_vec_pts2D(solid_tnp1->vertices, i, &pt);
            }
        }
        for(i=0; i<original_nb_pts; i++){
            ell = *get_ijth_elem_arr_int(pair_intersection_fuse, i, 0);
            emm = *get_ijth_elem_arr_int(pair_intersection_fuse, i, 1);
            if (ell > -1){
                j = (uint64_t) ell;
                k = (uint64_t) emm;
                pt = project_on_edge(get_ith_elem_vec_pts2D(solid_tnp1->vertices, i), 
                                    get_ith_elem_vec_pts2D(IntersecList_fuse,j),\
                                    get_ith_elem_vec_pts2D(IntersecList_fuse,k));
                set_ith_elem_vec_pts2D(solid_tnp1->vertices, i, &pt);
            }
        }

        //Finally, create 3D space-time solid interface using these two polygons.
        if (*solid3D){
            dealloc_Polyhedron3D(*solid3D); free(*solid3D);
        }
        *solid3D = build_space_time_cell_with_intersection(*solid, solid_tnp1, dt, pt_in_or_out_split, pt_in_or_out_fuse);
    } else {
        copy_Polygon2D(solid_tnp1, solid_new);
        //Create 3D space-time solid interface
        if (*solid3D){
            dealloc_Polyhedron3D(*solid3D); free(*solid3D);
        }
        *solid3D = build_space_time_cell_given_tnp1_vertices(*solid, vertices_tnp1, dt, true);
        for(i=0; i<(*solid3D)->status_face->size; i++){
            if ((*solid3D)->status_face->data[i] > 2)
                (*solid3D)->status_face->data[i] = -(*solid3D)->status_face->data[i]; //Face status numbered negatively for solid space-time reconstructions
        }
    }

    coarsen_interface(solid_new, list_changed_edges);
    if (maximal_length<INFINITY)
        refine_interface(solid_new, maximal_length);

    //Clean the result if some points were suppressed
    clean_Polygon2D(solid_new, solid);

    dealloc_vec_pts2D(vertices_tnp1); free(vertices_tnp1);
    dealloc_vec_pts2D(IntersecList); free(IntersecList);
    dealloc_vec_uint(edge_intersect1); free(edge_intersect1);
    dealloc_vec_uint(edge_intersect2); free(edge_intersect2);
    dealloc_Polygon2D(solid_tnp1); free(solid_tnp1);
    dealloc_Polygon2D(solid_new);  free(solid_new);
    dealloc_arr_int(list_del_pts); free(list_del_pts);
    dealloc_arr_int(list_changed_edges); free(list_changed_edges);
    if(IntersecList_fuse){
        dealloc_vec_pts2D(IntersecList_fuse); free(IntersecList_fuse);
        dealloc_vec_uint(edge_intersect1_fuse); free(edge_intersect1_fuse);
        dealloc_vec_uint(edge_intersect2_fuse); free(edge_intersect2_fuse);
        dealloc_vec_pts2D(IntersecList_split); free(IntersecList_split);
        dealloc_vec_uint(edge_intersect1_split); free(edge_intersect1_split);
        dealloc_vec_uint(edge_intersect2_split); free(edge_intersect2_split);
        dealloc_vec_pts2D(normals_edges); free(normals_edges);
        dealloc_arr_int(pair_intersection_split); free(pair_intersection_split);
        for(i=0; i<nb_faces; i++){
            dealloc_vec_uint(faces_to_split[i]); free(faces_to_split[i]);
        }
        free(faces_to_split);
        dealloc_arr_int(pair_intersection_fuse); free(pair_intersection_fuse);
        for(i=0; i<nb_faces; i++){
            dealloc_vec_uint(faces_to_fuse[i]); free(faces_to_fuse[i]);
        }
        free(faces_to_fuse);
    }
}

