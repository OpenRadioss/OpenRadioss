//
//function Base.:isless(e1::Edge2D, e2::Edge2D)
//    return e1 < e2
//end

#include "edge_collision.h"
#include "Point.h"
#include "stdbool.h"
#include "vector_indices_intersection.h"
#include <math.h>
#include "macro_check_gb_call.h"

#define SIGN(x) ((x)>0 ? 1 : -1)

typedef struct {
    Point2D ext1;
    Point2D ext2;
} Edge2D;

typedef struct {
    Edge2D e;
    uint64_t i;
} Indexed_Edge2D;

// Defines an ordering of two edges
static int compare_edges(const void *a, const void *b){
    Indexed_Edge2D* e1 = (Indexed_Edge2D*) a;
    Indexed_Edge2D* e2 = (Indexed_Edge2D*) b;
    Point2D p1, p2;

    p1 = e1->e.ext1;
    p2 = e2->e.ext1;

    if ((p1.x<p2.x) || ((p1.x==p2.x) && (p1.y<p2.y))){
        return -1;
    } else {
        return 1;
    }
}

// Find if e1 and e2 intersects
static Point2D intersection_edges(Edge2D e1, Edge2D e2, bool all_inclusive){// all_inclusive = false
    Point2D n, pt;    
    my_real_c d1, d2;
    my_real_c lambda;
    my_real_c bottom, up, left, right;
    bool check_inside_box_e1;

    Point2D pt1_e1 = e1.ext1;
    Point2D pt2_e1 = e1.ext2;
    Point2D pt1_e2 = e2.ext1;
    Point2D pt2_e2 = e2.ext2;

    n.y = pt2_e1.x-pt1_e1.x;
    n.x = -(pt2_e1.y-pt1_e1.y);

    //Compute distance of e2 endpoints to e1
    d1 = compute_distance2D(pt1_e2, n, pt1_e1);
    d2 = compute_distance2D(pt2_e2, n, pt1_e1);

    //If both distances have the same sign : no intersection possible.
    if ((d1>0.0 && d2>0.0) || (d1<0.0 && d2<0.0) || ((d1==0.0) && (d2==0.0))){
        pt.x = nan("");
        pt.y = nan("");
        return pt;
    } else {
        //Compute the intersection between the lines supported by e1 and e2
        lambda = d2/(d2-d1);
        pt.x = lambda*pt1_e2.x + (1-lambda)*pt2_e2.x;
        pt.y = lambda*pt1_e2.y + (1-lambda)*pt2_e2.y;
       
        //Check that pt is really inside e1
        if (pt1_e1.y < pt2_e1.y) {
            bottom = pt1_e1.y;
            up = pt2_e1.y;
        } else {
            up = pt1_e1.y;
            bottom = pt2_e1.y;
        }
        if (pt1_e1.x < pt2_e1.x) {
            left = pt1_e1.x;
            right = pt2_e1.x;
        } else {
            right = pt1_e1.x;
            left = pt2_e1.x;
        }

        check_inside_box_e1 = true;
        if (left < right){
            if (all_inclusive) 
                check_inside_box_e1 = check_inside_box_e1 && (pt.x >= left) && (pt.x <= right);
            else
                check_inside_box_e1 = check_inside_box_e1 && (pt.x>left) && (pt.x<right);
        }
        if (bottom < up){
            if (all_inclusive)
                check_inside_box_e1 = check_inside_box_e1 && (pt.y > bottom) && (pt.y <= up);
            else
                check_inside_box_e1 = check_inside_box_e1 && (pt.y>bottom) && (pt.y<up);
        }

        if (!check_inside_box_e1) {
            pt.x = nan("");
            pt.y = nan("");
        }
        
        return pt;
    }
}


//It supposes that one edge can only be part of only one face => hard to ensure for more than two phases..?
//IntersecList, edge_intersect1 and edge_intersect2 must be allocated (but they can be empty)
void find_all_self_intersection(Polygon2D *p, Vector_points2D* IntersecList, Vector_uint *edge_intersect1, Vector_uint *edge_intersect2){
    GrB_Index nb_edges, nb_pts, i, j;
    Indexed_Edge2D *list_edges;
    GrB_Vector val_extr, J_vect, pt_indices;
    GrB_Index indpt1, indpt2;
    uint64_t indpermi, indpermj;
    Point2D *ext1, *ext2, pt;
    Edge2D e1, e2;
    my_real_c norm_e;

    GrB_Matrix_ncols(&nb_edges, *(p->edges));
    GrB_Matrix_nrows(&nb_pts, *(p->edges));

    //rowvals_edges = rowvals(edges)
    GrB_Vector_new(&J_vect, GrB_UINT64, nb_pts);
    GrB_Vector_new(&pt_indices, GrB_UINT64, nb_pts);
    GrB_Vector_new(&val_extr, GrB_INT8, nb_pts);
    list_edges = (Indexed_Edge2D*) malloc(nb_edges * sizeof(Indexed_Edge2D));
    for (i = 0; i<nb_edges; i++){
        GrB_extract(J_vect, GrB_NULL, GrB_NULL, *(p->edges), GrB_ALL, 1, i, GrB_NULL);
        GrB_Vector_extractTuples(pt_indices, val_extr, J_vect, GrB_NULL);
        GrB_Vector_extractElement(&indpt1, pt_indices, 0);
        GrB_Vector_extractElement(&indpt2, pt_indices, 1);
        ext1 = get_ith_elem_vec_pts2D(p->vertices, indpt1);
        ext2 = get_ith_elem_vec_pts2D(p->vertices, indpt2);
        if ((ext1->x < ext2->x) || ((ext1->x == ext2->x) && (ext1->y < ext2->y))){
            list_edges[i] = (Indexed_Edge2D){(Edge2D){*ext1, *ext2}, i};
        } else {
            list_edges[i] = (Indexed_Edge2D){(Edge2D){*ext2, *ext1}, i};
        }
    }

    qsort(list_edges, nb_edges, sizeof(Indexed_Edge2D), compare_edges);

    for (i = 0; i<nb_edges-1; i++){
        e1 = list_edges[i].e;
        indpermi = list_edges[i].i;
        pt.x = e1.ext2.x-e1.ext1.x;
        pt.y = e1.ext2.y-e1.ext1.y;
        norm_e = sqrt(pt.x*pt.x+pt.y*pt.y);
        if (norm_e>0){
            for (j = i+1; j<nb_edges; j++){
                e2 = list_edges[j].e;
                indpermj = list_edges[j].i;
                //if e2.ext1<e1.ext2 
                pt.x = e2.ext2.x-e2.ext1.x;
                pt.y = e2.ext2.y-e2.ext1.y;
                norm_e = sqrt(pt.x*pt.x+pt.y*pt.y);
                if (norm_e>0){
                    if ((e2.ext1.x < e1.ext2.x) || ((e2.ext1.x == e1.ext2.x) && (e2.ext1.y < e1.ext2.y))){//at least, they have common x-range.
                        pt = intersection_edges(e1, e2, false);
                        if (!isnan(pt.x)) {
                            push_back_vec_pts2D(&IntersecList, &pt);
                            push_back_vec_uint(&edge_intersect1, &indpermi);
                            push_back_vec_uint(&edge_intersect2, &indpermj);
                        }
                    } else {
                        break;
                    }
                }
            }
        }
    }

    GrB_Vector_free(&J_vect);
    GrB_Vector_free(&pt_indices);
    GrB_Vector_free(&val_extr);
    free(list_edges);
}

//Compute the barycentric coordinate of `middle_pt` on edge [pt1, pt2]
my_real_c compute_barycentric_coord(Point2D middle_pt, Point2D pt1, Point2D pt2){
    Point2D d_mpt1, d_pt12;

    d_mpt1.x = middle_pt.x-pt1.x; d_mpt1.y = middle_pt.y-pt1.y;
    d_pt12.x = pt2.x-pt1.x; d_pt12.y = pt2.y-pt1.y;
    return norm_pt2D(d_mpt1)/norm_pt2D(d_pt12);
}


//Checks all points in pts and finds the two points closest to `pt1` and `pt2`. `ind1` and `ind2` are the indices of the points in `pts`.
static void find_closest_point_to_edge_intersection(const Vector_points2D *pts, Point2D pt1, Point2D pt2, uint64_t *ind1, uint64_t *ind2){
    my_real_c theta;
    Point2D* pt;
    uint64_t j;
    Vector_double *thetas = alloc_with_capacity_vec_double(pts->size);
    Vector_uint* perms = alloc_with_capacity_vec_uint(pts->size);

    for (j=0; j< pts->size; j++){
        pt = get_ith_elem_vec_pts2D(pts, j);
        theta = compute_barycentric_coord(*pt, pt1, pt2);
        set_ith_elem_vec_double(thetas, j, &theta);
    }

    sort_vec_double(thetas, thetas, perms);
    
    *ind1 = *get_ith_elem_vec_uint(perms, 0);
    *ind2 = *get_ith_elem_vec_uint(perms, pts->size-1);

    dealloc_vec_double(thetas);free(thetas);
    dealloc_vec_uint(perms);free(perms);
}

//Checks if point `pt_index` is inside the face `i_face` of polygon `p`. It uses the normals_vectors to the edges of p.
//The algorithm is based on a ray tracing intersecting the polygon.
static uint64_t ray_tracing_intersect(const Polygon2D* p, uint64_t pt_index, const Vector_points2D *normal_vectors, uint64_t i_face){
    //GrB_Index size_i_edges;
    GrB_Index nb_edges, nb_pts, nvals;
    GrB_Vector i_edges, I_vec_e_k, extr_vals_e_k;
    GrB_Matrix e_k;
    Point2D *pt, e1_ext1, e1_ext2, e2_ext1, e2_ext2;
    Point2D normalVector1, normalVector2;
    GrB_Index ie1, ie2;
    GrB_Vector ej, nz_ej, extr_vals_ej;
    GrB_Index i, j, k;
    int8_t sign;
    Point2D tmp_pt;
    Edge2D ray_edge;
    bool swap;
    uint64_t nb_intersect = 0;
    my_real_c max_ray;
    my_real_c x_min, x_max, y_min, y_max;

    GrB_Matrix_ncols(&nb_edges, *(p->edges));
    GrB_Matrix_nrows(&nb_pts, *(p->edges));
    GrB_Vector_new(&ej, GrB_INT8, nb_pts);
    GrB_Vector_new(&nz_ej, GrB_UINT64, nb_pts);
    GrB_Vector_new(&extr_vals_ej, GrB_INT8, nb_pts);
    GrB_Matrix_new(&e_k, GrB_INT8, 1, nb_edges);

    GrB_extract(e_k, GrB_NULL, GrB_NULL, *(p->edges), &pt_index, 1, GrB_ALL, 1, GrB_NULL); //Get indices of edges connected with point pt_index
    GrB_Matrix_nvals(&nvals, e_k);

    GrB_Vector_new(&i_edges, GrB_UINT64, nvals);
    GrB_Vector_new(&I_vec_e_k, GrB_UINT64, nvals);
    GrB_Vector_new(&extr_vals_e_k, GrB_INT8, nvals);
    GxB_Matrix_extractTuples_Vector(I_vec_e_k, i_edges, extr_vals_e_k, e_k, GrB_NULL);
    //GrB_Vector_size(&size_i_edges, i_edges);

    pt = get_ith_elem_vec_pts2D(p->vertices, pt_index);
    GrB_Vector_extractElement(&ie1, i_edges, 0);
    GrB_Vector_extractElement(&ie2, i_edges, 1);

    GrB_extract(ej, GrB_NULL, GrB_NULL, *(p->edges), GrB_ALL, 1, ie1, GrB_NULL); 
    GxB_Vector_extractTuples_Vector(nz_ej, extr_vals_ej, ej, GrB_NULL);
    GrB_Vector_extractElement(&i, nz_ej, 0);
    GrB_Vector_extractElement(&j, nz_ej, 1);
    e1_ext1 = *get_ith_elem_vec_pts2D(p->vertices, i);
    e1_ext2 = *get_ith_elem_vec_pts2D(p->vertices, j);

    GrB_extract(ej, GrB_NULL, GrB_NULL, *(p->edges), GrB_ALL, 1, ie2, GrB_NULL); 
    GxB_Vector_extractTuples_Vector(nz_ej, extr_vals_ej, ej, GrB_NULL);
    GrB_Vector_extractElement(&i, nz_ej, 0);
    GrB_Vector_extractElement(&j, nz_ej, 1);
    e2_ext1 = *get_ith_elem_vec_pts2D(p->vertices, i);
    e2_ext2 = *get_ith_elem_vec_pts2D(p->vertices, j);

    GrB_Matrix_extractElement(&sign, *(p->faces), ie1, i_face);
    normalVector1 = *get_ith_elem_vec_pts2D(normal_vectors, ie1);
    normalVector1 = (Point2D){-sign*(normalVector1.x), -sign*(normalVector1.y)};

    GrB_Matrix_extractElement(&sign, *(p->faces), ie2, i_face);
    normalVector2 = *get_ith_elem_vec_pts2D(normal_vectors, ie2);
    normalVector2 = (Point2D){-sign*(normalVector2.x), -sign*(normalVector2.y)};

    if (normalVector1.y * normalVector2.y <= 0) { //y-direction is tangent
        if ((e1_ext1.x != pt->x) || (e1_ext1.x != pt->x)){
            tmp_pt = e1_ext1;
            e1_ext1 = e1_ext2;
            e1_ext2 = tmp_pt;
        }
        if ((e2_ext1.x != pt->x) || (e2_ext1.x != pt->x)) {
            tmp_pt = e2_ext1;
            e2_ext1 = e2_ext2;
            e2_ext2 = tmp_pt;
        }

        //We must identify the edge on top of the other. The one below is edge1, the one above is edge2.
        swap = false;
        if ((e1_ext2.x <= pt->x) && (e1_ext2.x <= pt->x)){ //both edges are on the left
            if (normalVector1.x*normalVector2.x <= 0){ //Both edges are in the same quarter
                if ((e1_ext2.y <= pt->y) && (e1_ext2.y <= pt->y)){ //both edges are on the bottom left quarter
                    if (normalVector1.x * normalVector1.y <= 0)
                        swap = true;
                } else { //both edges are on the up left quarter
                    if (normalVector1.x * normalVector1.y >= 0)
                        swap = true;
                }
            } else {//The edges span over the left half-plane
                if ((normalVector1.x >= 0) && (normalVector2.x >= 0)){
                    if (normalVector1.y >= 0)
                        swap = true;
                } else {
                    if (normalVector1.y <= 0)
                        swap = true;
                }
            }
        } else if ((e1_ext2.x >= pt->x) && (e1_ext2.x >= pt->x)){ //both edges are on the right
            if (normalVector1.x*normalVector2.x <= 0){ //Both edges are in the same quarter
                if ((e1_ext2.y <= pt->y) && (e1_ext2.y <= pt->y)){ //both edges are on the bottom right quarter
                    if (normalVector1.x * normalVector1.y >= 0)
                        swap = true;
                } else { //both edges are on the up right quarter
                    if (normalVector1.x * normalVector1.y <= 0)
                        swap = true;
                }
            } else { //The edges span over the right half-plane
                if ((normalVector1.x <= 0) && (normalVector2.x <= 0)){
                    if (normalVector1.y >= 0)
                        swap = true;
                } else {
                    if (normalVector1.y <= 0)
                        swap = true;
                }
            }
        }

        if (swap) {
            //tmp = ie1;
            //ie1 = ie2;
            //ie2 = tmp;

            //tmp_pt = e1_ext1;
            //e1_ext1 = e2_ext1;
            //e2_ext1 = tmp_pt;

            //tmp_pt = e1_ext2;
            //e1_ext2 = e2_ext2;
            //e2_ext2 = tmp_pt;

            tmp_pt = normalVector1;
            normalVector1 = normalVector2;
            normalVector2 = tmp_pt;
        }
    }

    if (normalVector2.y != 0.0){
        tmp_pt = *get_ith_elem_vec_pts2D(p->vertices, 0);
        max_ray = tmp_pt.y;
        for(i=1; i<p->vertices->size; i++){
            tmp_pt = *get_ith_elem_vec_pts2D(p->vertices, 0);
            if (max_ray < tmp_pt.y)
                max_ray = tmp_pt.y;
        }
        ray_edge = (Edge2D){*pt, (Point2D){pt->x, max_ray + 1}};
        for (i = 0; i<nb_edges; i++){
            GrB_extract(ej, GrB_NULL, GrB_NULL, *(p->edges), GrB_ALL, 1, i, GrB_NULL); 
            GxB_Vector_extractTuples_Vector(nz_ej, extr_vals_ej, ej, GrB_NULL);
            GrB_Vector_extractElement(&k, nz_ej, 0);
            GrB_Vector_extractElement(&j, nz_ej, 1);
            e1_ext1 = *get_ith_elem_vec_pts2D(p->vertices, k);
            e1_ext2 = *get_ith_elem_vec_pts2D(p->vertices, j);
            if (e1_ext1.x < e1_ext2.x){
                x_min = e1_ext1.x;
                x_max = e1_ext2.x;
            } else {
                x_max = e1_ext1.x;
                x_min = e1_ext2.x;
            }
            if (e1_ext1.y < e1_ext2.y){
                y_min = e1_ext1.y;
                y_max = e1_ext2.y;
            } else {
                y_max = e1_ext1.y;
                y_min = e1_ext2.y;
            }
            if ((x_min <= pt->x) && (pt->x < x_max)) {
                if (y_min > pt->y){
                    nb_intersect += 1;
                } else if (y_max > pt->y) {
                    e2_ext2 = intersection_edges(ray_edge, (Edge2D){e1_ext1, e1_ext2}, true);
                    if (!isnan(e2_ext2.x)) {
                        nb_intersect += 1;
                    }
                }
            }
        }
        if (normalVector2.y > 0)
            nb_intersect += 1;
    } else {
        tmp_pt = *get_ith_elem_vec_pts2D(p->vertices, 0);
        max_ray = tmp_pt.x;
        for(i=1; i<p->vertices->size; i++){
            tmp_pt = *get_ith_elem_vec_pts2D(p->vertices, 0);
            if (max_ray < tmp_pt.x)
                max_ray = tmp_pt.x;
        }
        ray_edge = (Edge2D){*pt, (Point2D){max_ray + 1, pt->y}};
        for (i = 0; i<nb_edges; i++){
            GrB_extract(ej, GrB_NULL, GrB_NULL, *(p->edges), GrB_ALL, 1, i, GrB_NULL); 
            GxB_Vector_extractTuples_Vector(nz_ej, extr_vals_ej, ej, GrB_NULL);
            GrB_Vector_extractElement(&k, nz_ej, 0);
            GrB_Vector_extractElement(&j, nz_ej, 1);
            e1_ext1 = *get_ith_elem_vec_pts2D(p->vertices, k);
            e1_ext2 = *get_ith_elem_vec_pts2D(p->vertices, j);
            if (e1_ext1.x < e1_ext2.x){
                x_min = e1_ext1.x;
                x_max = e1_ext2.x;
            } else {
                x_max = e1_ext1.x;
                x_min = e1_ext2.x;
            }
            if (e1_ext1.y < e1_ext2.y){
                y_min = e1_ext1.y;
                y_max = e1_ext2.y;
            } else {
                y_max = e1_ext1.y;
                y_min = e1_ext2.y;
            }
            if ((y_min <= pt->y) && (pt->y < y_max)){
                if (x_min > pt->x){
                    nb_intersect += 1;
                } else if (x_max > pt->x){
                    e2_ext2 = intersection_edges(ray_edge, (Edge2D){e1_ext1, e1_ext2}, true);
                    if (!isnan(e2_ext2.x)) {
                        nb_intersect += 1;
                    }
                }
            }
        }
        if (normalVector2.x > 0)
            nb_intersect += 1;
    }

    GrB_Vector_free(&ej);
    GrB_Vector_free(&nz_ej);
    GrB_Vector_free(&extr_vals_ej);
    GrB_Matrix_free(&e_k);
    GrB_Vector_free(&i_edges);
    GrB_Vector_free(&I_vec_e_k);
    GrB_Vector_free(&extr_vals_e_k);

    return nb_intersect;
}

///@brief Checks each intersection and decides wether the intersection should lead to a splitting of a face in two faces, or the fusion of two faces into one.
///@details The arrays pt_in_or_out_* are of size p->vertices->size, initialized with 0s. 
///         They mark each point as being unaffected by an intersection (0), close to an intersection but outside the overlapping region (1), close to an intersection but inside the overlapping region (-1).
///         All output arrays must be allocated before calling the function.
///         Except for both arrays pt_in_or_out_*, they can be left empty. 
///@param p [IN] Polygons
///@param normal_vectors [IN] Normal vectors to the edges of p.
///@param IntersecList [IN] List of intersection points
///@param edge_intersect1 [IN] List of intersecting edges (1st edge)
///@param edge_intersect2 [IN] List of intersecting edges (2nd edge)
///@param `*_split` [OUT] Described above, information for face splitting.
///@param `*_fuse` [OUT] Described above, information for faces fusion.
void in_or_out_intersection(const Polygon2D* p, const Vector_points2D* normal_vectors, \
                            const Vector_points2D* IntersecList, const Vector_uint* edge_intersect1, const Vector_uint* edge_intersect2,\
                            int8_t* pt_in_or_out_split, Vector_points2D* IntersecList_split, \
                            Vector_uint* edge_intersect1_split, Vector_uint* edge_intersect2_split,\
                            int8_t* pt_in_or_out_fuse, Vector_points2D* IntersecList_fuse, \
                            Vector_uint* edge_intersect1_fuse, Vector_uint* edge_intersect2_fuse
                            )
{
    GrB_Index nb_edges, nb_pts, nb_faces;
    GrB_Index i, j, i_pt1, i_pt2, i_pt_inters;
    uint64_t ei1, ei2;
    Vector_indices_intersec** edge_list_intersect;
    Vector_indices_intersec* s;
    Indices_intersection i_inters;
    GrB_Vector ej, nz_ej, extr_vals_ej;
    Point2D *pt1, *pt2;
    uint64_t i_e_inters1, i_e_inters2;
    Vector_points2D *il_subarray;
    GrB_Matrix f_k;
    GrB_Vector i_faces, I_vec_f_k, extr_vals_f_k;
    GrB_Index curr_face, i_face1, i_face2;
    uint64_t nb_intersect;
    int8_t sign;
    Point2D normalVector_inters, *pt_inters;
    my_real_c dpte;
    long int phase, phase1, phase2;
    GrB_Info infogrb;

    GrB_Matrix_ncols(&nb_edges, *(p->edges));
    GrB_Matrix_nrows(&nb_pts, *(p->edges));
    GrB_Matrix_ncols(&nb_faces, *(p->faces));
    GrB_Vector_new(&ej, GrB_INT8, nb_pts);
    GrB_Vector_new(&nz_ej, GrB_UINT64, nb_pts);
    GrB_Vector_new(&extr_vals_ej, GrB_INT8, nb_pts);
    GrB_Matrix_new(&f_k, GrB_INT8, 1, nb_faces);
    GrB_Vector_new(&i_faces, GrB_UINT64, nb_faces);
    GrB_Vector_new(&I_vec_f_k, GrB_UINT64, nb_faces);
    GrB_Vector_new(&extr_vals_f_k, GrB_INT8, nb_faces);

    il_subarray = alloc_empty_vec_pts2D();

    edge_list_intersect = malloc(nb_edges * sizeof(Vector_indices_intersec**));
    for(i=0; i<nb_edges; i++){
        edge_list_intersect[i] = alloc_empty_vec_indices_intersec();
    }
    
    //Build a list for each edge which stores all the edges intersecting with it.
    for (i=0; i<edge_intersect1->size; i++){
        ei1 = *get_ith_elem_vec_uint(edge_intersect1, i);
        ei2 = *get_ith_elem_vec_uint(edge_intersect2, i);
        i_inters = (Indices_intersection){ei2, i};
        push_back_unique_vec_indices_intersec(&(edge_list_intersect[ei1]), &i_inters);
        i_inters = (Indices_intersection){ei1, i};
        push_back_unique_vec_indices_intersec(&(edge_list_intersect[ei2]), &i_inters);
    }

    for (i=0; i<nb_edges; i++){
        s = edge_list_intersect[i];
        if (s->size>0){
            //i_inters_mix = collect(s)
            //pt_indices = nzrange(p.edges, i)
            GrB_extract(ej, GrB_NULL, GrB_NULL, *(p->edges), GrB_ALL, 1, i, GrB_NULL); 
            GxB_Vector_extractTuples_Vector(nz_ej, extr_vals_ej, ej, GrB_NULL);
            GrB_Vector_extractElement(&i_pt1, nz_ej, 0);
            GrB_Vector_extractElement(&i_pt2, nz_ej, 1);
            pt1 = get_ith_elem_vec_pts2D(p->vertices, i_pt1);
            pt2 = get_ith_elem_vec_pts2D(p->vertices, i_pt2);

            if (s->size == 1){ //Only one edge intersecting with this edge.
                i_inters = *get_ith_elem_vec_indices_intersec(s, 0);
                i_e_inters1 = i_inters.i_e;
                i_e_inters2 = i_e_inters1;
            } else { //length(s)>1 : we must find the closest edge to each point
                il_subarray->size = 0;
                for(j=0; j<s->size; j++){
                    i_inters = *get_ith_elem_vec_indices_intersec(s, j);
                    push_back_vec_pts2D(&il_subarray, get_ith_elem_vec_pts2D(IntersecList, i_inters.i_pt));
                }
                // /!\ This function only returns local indices to the subarray!
                find_closest_point_to_edge_intersection(il_subarray, *pt1, *pt2, &i_e_inters1, &i_e_inters2);

                i_inters = *get_ith_elem_vec_indices_intersec(s, i_e_inters1);
                i_e_inters1 = i_inters.i_e;//Now global!
                i_inters = *get_ith_elem_vec_indices_intersec(s, i_e_inters2);
                i_e_inters2 = i_inters.i_e;//Now global!
            }

            CHECK_GB_CALL(infogrb, GrB_extract(f_k, GrB_NULL, GrB_NULL, *(p->faces), &i, 1, GrB_ALL, 1, GrB_NULL)) //Get indices of faces using edge i
            CHECK_GB_CALL(infogrb, GxB_Matrix_extractTuples_Vector(I_vec_f_k, i_faces, extr_vals_f_k, f_k, GrB_NULL))
            CHECK_GB_CALL(infogrb, GrB_Vector_extractElement(&curr_face, i_faces, 0))

            CHECK_GB_CALL(infogrb, GrB_extract(f_k, GrB_NULL, GrB_NULL, *(p->faces), &i_e_inters1, 1, GrB_ALL, 1, GrB_NULL)) //Get indices of faces using edge i_e_inters1
            CHECK_GB_CALL(infogrb, GxB_Matrix_extractTuples_Vector(I_vec_f_k, i_faces, extr_vals_f_k, f_k, GrB_NULL))
            CHECK_GB_CALL(infogrb, GrB_Vector_extractElement(&i_face1, i_faces, 0))

            CHECK_GB_CALL(infogrb, GrB_extract(f_k, GrB_NULL, GrB_NULL, *(p->faces), &i_e_inters2, 1, GrB_ALL, 1, GrB_NULL)) //Get indices of faces using edge i_e_inters2
            CHECK_GB_CALL(infogrb, GxB_Matrix_extractTuples_Vector(I_vec_f_k, i_faces, extr_vals_f_k, f_k, GrB_NULL))
            CHECK_GB_CALL(infogrb, GrB_Vector_extractElement(&i_face2, i_faces, 0))
            
            if ((curr_face == i_face1) && (curr_face == i_face2)){ //Only one face in contact: we must split it!
                //for ind in i_inters_mix
                for (j=0; j<s->size; j++){
                    if ((!is_in_vec_uint(edge_intersect1_split, &i)) && (!is_in_vec_uint(edge_intersect2_split, &i))){
                    //(i ∉ edge_intersect1_split) && (i ∉ edge_intersect2_split)
                        i_inters = *get_ith_elem_vec_indices_intersec(s, j);
                        push_back_vec_pts2D(&IntersecList_split, get_ith_elem_vec_pts2D(IntersecList, i_inters.i_pt));
                        push_back_vec_uint(&edge_intersect1_split, &(i_inters.i_e));
                        push_back_vec_uint(&edge_intersect2_split, &i);
                    }
                }
                phase = *get_ith_elem_vec_int(p->phase_face, curr_face);
                if (pt_in_or_out_split[i_pt1] == 0){
                    nb_intersect = ray_tracing_intersect(p, i_pt1, normal_vectors, curr_face);
                    if (nb_intersect%2 == 0)
                        pt_in_or_out_split[i_pt1] = (int8_t)(SIGN(phase));
                    else
                        pt_in_or_out_split[i_pt1] = (int8_t)(-SIGN(phase));
                }
                if (pt_in_or_out_split[i_pt2] == 0) {
                    nb_intersect = ray_tracing_intersect(p, i_pt2, normal_vectors, curr_face);
                    if (nb_intersect%2 == 0)
                        pt_in_or_out_split[i_pt2] = (int8_t)(SIGN(phase));
                    else
                        pt_in_or_out_split[i_pt2] = (int8_t)(-SIGN(phase));
                }
            } else { //At least two faces in contact: we must fuse them!
                for (j=0; j<s->size; j++){
                    if ((!is_in_vec_uint(edge_intersect1_fuse, &i)) && (!is_in_vec_uint(edge_intersect2_fuse, &i))){
                    //(i ∉ edge_intersect1_fuse) && (i ∉ edge_intersect2_fuse)
                        i_inters = *get_ith_elem_vec_indices_intersec(s, j);
                        push_back_vec_pts2D(&IntersecList_fuse, get_ith_elem_vec_pts2D(IntersecList, i_inters.i_pt));
                        push_back_vec_uint(&edge_intersect1_fuse, &(i_inters.i_e));
                        push_back_vec_uint(&edge_intersect2_fuse, &i);
                    }
                }
                phase = *get_ith_elem_vec_int(p->phase_face, curr_face);
                phase1 = *get_ith_elem_vec_int(p->phase_face, i_face1);
                phase2 = *get_ith_elem_vec_int(p->phase_face, i_face2);

                CHECK_GB_CALL(infogrb, GrB_extract(ej, GrB_NULL, GrB_NULL, *(p->edges), GrB_ALL, 1, i_e_inters1, GrB_NULL)) //We get the points of the first edge intersecting with edge i (i.e. edge i_e_inters1)
                CHECK_GB_CALL(infogrb, GxB_Vector_extractTuples_Vector(nz_ej, extr_vals_ej, ej, GrB_NULL))
                CHECK_GB_CALL(infogrb, GrB_Vector_extractElement(&i_pt_inters, nz_ej, 0))
                pt_inters = get_ith_elem_vec_pts2D(p->vertices, i_pt_inters);
                GrB_Matrix_extractElement(&sign, *(p->faces), i_e_inters1, i_face1);
                normalVector_inters = *get_ith_elem_vec_pts2D(normal_vectors, i_e_inters1);
                normalVector_inters.x = sign * normalVector_inters.x;
                normalVector_inters.y = sign * normalVector_inters.y;
                dpte = compute_distance2D(*pt1, normalVector_inters, *pt_inters);//We compute the signed distance of pt1 to the line passing through pt_inters and having the given normal vector normalVector_inters
                if ((phase > 0) && (phase1 > 0) && (phase2 > 0)){
                    pt_in_or_out_fuse[i_pt1] = (int8_t)(SIGN(dpte));
                } else {
                    pt_in_or_out_fuse[i_pt1] = -(int8_t)(SIGN(dpte));
                }

                CHECK_GB_CALL(infogrb, GrB_extract(ej, GrB_NULL, GrB_NULL, *(p->edges), GrB_ALL, 1, i_e_inters2, GrB_NULL)) //We get the points of the second edge intersecting with edge i (i.e. edge i_e_inters2) -- could be the same as i_e_intersec1!
                CHECK_GB_CALL(infogrb, GxB_Vector_extractTuples_Vector(nz_ej, extr_vals_ej, ej, GrB_NULL))
                CHECK_GB_CALL(infogrb, GrB_Vector_extractElement(&i_pt_inters, nz_ej, 1))
                pt_inters = get_ith_elem_vec_pts2D(p->vertices, i_pt_inters);
                GrB_Matrix_extractElement(&sign, *(p->faces), i_e_inters2, i_face1);
                normalVector_inters = *get_ith_elem_vec_pts2D(normal_vectors, i_e_inters2);
                normalVector_inters.x = sign * normalVector_inters.x;
                normalVector_inters.y = sign * normalVector_inters.y;
                dpte = compute_distance2D(*pt2, normalVector_inters, *pt_inters);
                if ((phase > 0) && (phase1 > 0) && (phase2 > 0)){
                    pt_in_or_out_fuse[i_pt2] = (int8_t)(SIGN(dpte));
                } else {
                    pt_in_or_out_fuse[i_pt2] = -(int8_t)(SIGN(dpte));
                }
            }
        }
    }

    GrB_Vector_free(&ej);
    GrB_Vector_free(&nz_ej);
    GrB_Vector_free(&extr_vals_ej);
    GrB_Matrix_free(&f_k);
    GrB_Vector_free(&i_faces);
    GrB_Vector_free(&I_vec_f_k);
    GrB_Vector_free(&extr_vals_f_k);
    dealloc_vec_pts2D(il_subarray); free(il_subarray);
    for(i=0; i<nb_edges; i++){
        dealloc_vec_indices_intersec(edge_list_intersect[i]); free(edge_list_intersect[i]);
    }
    free(edge_list_intersect);
}