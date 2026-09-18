#ifndef NODEWITHCOST_ORDSET_H
#define NODEWITHCOST_ORDSET_H

#include "my_real_c.inc"
#include <stdbool.h>
#include <stdint.h>

typedef struct {
    uint64_t ind_node;
    my_real_c cost, heuristic;
    //Node_with_cost() = new(-1,0.,0.)
} Node_with_cost;

typedef struct OrdSet_Nodes_t {
    Node_with_cost *data;
    struct OrdSet_Nodes_t *next;
} OrdSet_Nodes;

void add_node_ordset(OrdSet_Nodes **ordset, Node_with_cost* node);
Node_with_cost* pop_node_ordset(OrdSet_Nodes **ordset);
void is_in_ordset(const OrdSet_Nodes* ordset, uint64_t ind, bool *res, Node_with_cost **node);
void destroy_node_ordset(OrdSet_Nodes **ordset);

#endif