#include "NodeWithCost_OrdSet.h"
#include <stdlib.h>
#include <string.h>

/*
Utilities used for AStar algorithm.
Defines a node in a graph associated with a cost and a heuristic. Defines also ordered sets of nodes, sorted by heuristic.
*/

void add_node_ordset(OrdSet_Nodes **ordset, Node_with_cost* node){
    OrdSet_Nodes *currnode, *currnext, *new_elem;
    if (*ordset){
        currnode = *ordset;
        currnext = currnode->next;
        while(currnext){
            if (currnode->next->data->heuristic > node->heuristic){
                break;
            } else {
                currnode = currnext;
                currnext = currnode->next;
            }
        }
        if (currnode->data->ind_node != node->ind_node){
            new_elem = (OrdSet_Nodes*)malloc(sizeof(OrdSet_Nodes));
            new_elem->data = (Node_with_cost*)malloc(sizeof(Node_with_cost));
            memcpy(new_elem->data, node, sizeof(Node_with_cost));
            new_elem->next = currnext;
            currnode->next = new_elem;
        }
    } else {
        *ordset = (OrdSet_Nodes*)malloc(sizeof(OrdSet_Nodes));
        (*ordset)->data = (Node_with_cost*)malloc(sizeof(Node_with_cost));
        memcpy((*ordset)->data, node, sizeof(Node_with_cost));
        (*ordset)->next = NULL;
    }
}

Node_with_cost* pop_node_ordset(OrdSet_Nodes **ordset){
    OrdSet_Nodes *currnode = *ordset;
    Node_with_cost* val;
    if(currnode){
        val = currnode->data;
        *ordset = currnode->next;
        free(currnode);
    } else {
        val = NULL;
    }
    return val;
}

void is_in_ordset(const OrdSet_Nodes* ordset, uint64_t ind, bool *res, Node_with_cost **node){
    const OrdSet_Nodes *currnode = ordset;
    *res = false;
    *node = NULL;

    while(currnode){
        if (currnode->data->ind_node == ind){
            *res = true;
            *node = currnode->data;
            break;
        }
        if (currnode->data->ind_node > ind){
            break;
        }
        currnode = currnode->next;
    }
}

void destroy_node_ordset(OrdSet_Nodes **ordset){
    OrdSet_Nodes *currnode, *nextnode;
    if (ordset){
        currnode = *ordset;
        while(currnode){
            nextnode = currnode->next;
            free(currnode->data);
            currnode->next = NULL;
            free(currnode);
            currnode = nextnode;
        }
    }
    *ordset = NULL;
}