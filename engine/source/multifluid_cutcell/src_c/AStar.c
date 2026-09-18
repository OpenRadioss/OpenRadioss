#include "AStar.h"
#include "uthash.h"
#include "NodeWithCost_OrdSet.h"

/*
This file implements the AStar algorithm based on the data architecture of Polygon2D.
It is mainly used one for doing path retrieval in the Polygon between 2 intersection points.
The data structure used here are only used for the A* algorithm and are way more explained in books explaining the algorithm.
*/

typedef struct {
    uint64_t parent, edge_with_parent;
} Treenode_exploration_graph;

typedef struct {
    int id;                    /* key */
    Treenode_exploration_graph tneg;
    UT_hash_handle hh;         /* makes this structure hashable */
} Dict_Int_Treenode;



static void build_path_to_start(uint64_t start_ind, uint64_t end_node, Dict_Int_Treenode* closedList_nodes, Vector_uint* listEdges, Vector_uint* listNodes){
    uint64_t curr_node;
    Dict_Int_Treenode *s;

    push_back_vec_uint(&listNodes, &end_node);
    curr_node = end_node;
    while (curr_node != start_ind){
        HASH_FIND_INT(closedList_nodes, &curr_node, s);
        push_back_vec_uint(&listEdges, &(s->tneg.edge_with_parent));
        curr_node = s->tneg.parent;
        push_back_vec_uint(&listNodes, &curr_node);
    }
}

static void list_all_neighbours(uint64_t curr_node, const GrB_Matrix* edges, const Vector_uint* closedList_edges, Vector_uint* neighbours, Vector_uint* ind_edges){
    GrB_Index nb_pts, nb_edges, nvals, size_pot_ind_edges, i_e;
    GrB_Matrix e_k;
    GrB_Vector pot_ind_edges, I_vec_e_k, extr_vals_e_k;
    GrB_Vector ej, nz_ej, extr_vals_ej;
    uint64_t i;
    GrB_Index pot_neigh1, pot_neigh2;
    GrB_Info infogrb;

    //pot_ind_edges = findall(x->x!=0, edges[curr_node, :]);
    GrB_Matrix_ncols(&nb_edges, *edges);
    GrB_Matrix_nrows(&nb_pts, *edges);
    GrB_Matrix_new(&e_k, GrB_INT8, 1, nb_edges);
    GrB_extract(e_k, GrB_NULL, GrB_NULL, *edges, &curr_node, 1, GrB_ALL, 1, GrB_NULL); 
    GrB_Matrix_nvals(&nvals, e_k);

    GrB_Vector_new(&pot_ind_edges, GrB_UINT64, nvals);
    GrB_Vector_new(&I_vec_e_k, GrB_UINT64, nvals);
    GrB_Vector_new(&extr_vals_e_k, GrB_INT8, nvals);
    GxB_Matrix_extractTuples_Vector(I_vec_e_k, pot_ind_edges, extr_vals_e_k, e_k, GrB_NULL);
    GrB_Vector_size(&size_pot_ind_edges, pot_ind_edges);
    GrB_Vector_new(&ej, GrB_INT8, nb_pts);
    GrB_Vector_new(&nz_ej, GrB_UINT64, nb_pts);
    GrB_Vector_new(&extr_vals_ej, GrB_INT8, nb_pts);


    ind_edges->size = 0;
    neighbours->size = 0;
    for(i=0; i<size_pot_ind_edges; i++){
        GrB_Vector_extractElement(&i_e, pot_ind_edges, i);
        //if !(i_e ∈ closedList_edges)
        if (!is_in_vec_uint(closedList_edges, &i_e)){
            push_back_vec_uint(&ind_edges, &i_e);
            //pot_neigh = findall(x->x!=0, edges[:, i_e])
            infogrb = GrB_extract(ej, GrB_NULL, GrB_NULL, *edges, GrB_ALL, 1, i_e, GrB_NULL); 
            infogrb = GxB_Vector_extractTuples_Vector(nz_ej, extr_vals_ej, ej, GrB_NULL);
            infogrb = GrB_Vector_extractElement(&pot_neigh1, nz_ej, 0);
            infogrb = GrB_Vector_extractElement(&pot_neigh2, nz_ej, 1);
            if (pot_neigh1 == curr_node)
                push_back_vec_uint(&neighbours, &pot_neigh2);
            else
                push_back_vec_uint(&neighbours, &pot_neigh1);
        }
    }

    GrB_free(&e_k);
    GrB_free(&pot_ind_edges);
    GrB_free(&I_vec_e_k);
    GrB_free(&extr_vals_e_k);
    GrB_free(&ej);
    GrB_free(&nz_ej);
    GrB_free(&extr_vals_ej);
}

/// @brief Searches for a path (approx. the shortest) in a graph between start_pt and end_pt and avoiding forbidden_edges.
/// @param vertices [IN] Vertices of the graph.
/// @param edges [IN] Edges of the graph.
/// @param start_pt [IN] Index of the starting point.
/// @param end_pt [IN] Index of the ending point.
/// @param forbidden_edges [IN] Edges that can not be visited.
/// @param listEdges [OUT] List of edges visited to go from start_pt to end_pt. If of size 0 => no path found.
/// @param listNodes [OUT] List of nodes visited to go from start_pt to end_pt. If of size 0 => no path found.
void astar(const Vector_points2D *vertices, const GrB_Matrix *edges, uint64_t start_pt, uint64_t end_pt, const Vector_uint *forbidden_edges, \
            Vector_uint* listEdges, Vector_uint* listNodes){
    Node_with_cost start_node = (Node_with_cost){start_pt, 0, 0};
    Node_with_cost *curr_node;
    Dict_Int_Treenode* closedList_nodes = NULL, *s = NULL, *tmp = NULL;
    Vector_uint *closedList_edges = alloc_empty_vec_uint();
    OrdSet_Nodes *openList = NULL;
    Vector_uint *neighbours = alloc_empty_vec_uint();
    Vector_uint *ind_edges = alloc_empty_vec_uint();
    uint64_t i, i_n, i_e;
    bool isin;
    Node_with_cost *node, new_node;
    Point2D diffpt, endpt2D;


    endpt2D = *get_ith_elem_vec_pts2D(vertices, end_pt);
    copy_vec_uint(forbidden_edges, closedList_edges);
    add_node_ordset(&openList, &start_node);
    listEdges->size = 0;
    listNodes->size = 0;


    while (openList){
        curr_node = pop_node_ordset(&openList);
        if (curr_node->ind_node == end_pt){
            build_path_to_start(start_pt, end_pt, closedList_nodes, listEdges, listNodes);
            break;
        }

        list_all_neighbours(curr_node->ind_node, edges, closedList_edges, neighbours, ind_edges);
        for(i=0; i<neighbours->size; i++){
            i_n = *get_ith_elem_vec_uint(neighbours, i);
            i_e = *get_ith_elem_vec_uint(ind_edges, i);

            HASH_FIND_INT(closedList_nodes, &i_n, s);
            if (!s){
                is_in_ordset(openList, i_n, &isin, &node);
                if (!(isin && (node->cost < curr_node->cost + 1))){
                    new_node = (Node_with_cost){i_n, curr_node->cost + 1, curr_node->cost + 1};
                    diffpt = *get_ith_elem_vec_pts2D(vertices, i_n);
                    diffpt.x -= endpt2D.x; 
                    diffpt.y -= endpt2D.y; 
                    new_node.heuristic += norm_pt2D(diffpt);
                    
                    add_node_ordset(&openList, &new_node);

                    s = (Dict_Int_Treenode*)malloc(sizeof(Dict_Int_Treenode));
                    s->id = i_n;
                    s->tneg = (Treenode_exploration_graph){curr_node->ind_node, i_e};
                    HASH_ADD_INT(closedList_nodes, id, s);
                }
            }
        }

        for(i=0; i<ind_edges->size; i++){
            i_e = *get_ith_elem_vec_uint(ind_edges, i);
            push_back_unique_vec_uint(&closedList_edges, &i_e);
        }
    }

    destroy_node_ordset(&openList);
    HASH_ITER(hh, closedList_nodes, s, tmp) {
        HASH_DEL(closedList_nodes, s);  /* delete; users advances to next */
        free(s);             /* optional- if you want to free  */
    }
    dealloc_vec_uint(closedList_edges); free(closedList_edges);
    dealloc_vec_uint(neighbours); free(neighbours);
    dealloc_vec_uint(ind_edges); free(ind_edges);
}