#include <iostream>
#include <vector>
#include <algorithm>
#include <set>

// define the type ghosts as a vector<vector<int>>
typedef std::vector<std::vector<int>> ghosts;

// Fortran callable functions

bool is_shell_ghost_on_proc(
    int shell_index, // Index of the shell
    int process_id,  // Process ID
    int *shells,     // Shell nodes array
    int *mask,       // Mask array for nodes
    int nspmd,       // Number of processes
    int max_node_id) // Maximum node ID
{
    // A shell should be included if at least one of its nodes has mask == 1
    for (int j = 0; j < 4; j++)
    {
        const int node_id = shells[shell_index * 4 + j];
        if (node_id > 0 && node_id <= max_node_id)
        {
            // For a Fortran array mask(nspmd,numnodes)
            // Access mask(p,node_id) as mask[(node_id-1)*nspmd + (p)]
            int mask_index = (node_id - 1) * nspmd + process_id;
            if (mask[mask_index] == 1)
            {
                return true; // Found at least one node with mask == 1
            }
        }
    }
    return false; // No nodes have mask == 1
}

extern "C"
{
    // function build_ghosts(shells,nb_shells,mask,nspmd) result(c) bind(C,name="cpp_build_ghosts")

    ghosts *cpp_build_ghosts(
        int *shells, // 4 x nb_shells array : node id of the shells
        int nb_shells,
        int *mask, // nspmd x nb_shells array: 1 => shell is to be considered
        int nspmd,
        int numnod)
    {
        ghosts *c = new ghosts(nspmd);
        // find the maximum node id in the array shells of size 4 x nb_shells
        int max_node_id = numnod;
        // find the maximum value in the shell array of size 4 x nb_shells
        for (int i = 0; i < nb_shells; i++)
        {
            for (int p = 0; p < nspmd; p++)
            {
                if (is_shell_ghost_on_proc(i, p, shells, mask, nspmd, max_node_id))
                {
                    (*c)[p].push_back(i+1);
                }
            }
        }
        if (nb_shells > 0)
        {
            // remove duplicates from the vector
            for (int p = 0; p < nspmd; p++)
            {
                std::sort((*c)[p].begin(), (*c)[p].end());
                auto new_end = std::unique((*c)[p].begin(), (*c)[p].end());
                (*c)[p].erase(new_end, (*c)[p].end());
            }
        }

        return c;
    }

    int *cpp_get_shells_list(void *c, int pc, int *n)
    {
        // p is the process id
        // n is the size of the shell list [out]
        ghosts *rc = static_cast<ghosts *>(c);


        const int p = pc - 1; // Fortran to C index conversion
        if (c == nullptr)
        {
            *n = 0;
            return nullptr;
        }
        // Check if p is within bounds
        if (p < 0 || p >= rc->size())
        {
            // Out of bounds, return empty result
            *n = 0;
            return nullptr;
        }

        *n = (*rc)[p].size();
        if (*n == 0)
        {
            return nullptr;
        }
        else
        {
            // return the pointer to data
            return (*rc)[p].data();
        }
    }

    int cpp_get_shells_list_size(void *c, int pc)
    {
        // p is the process id
        ghosts *rc = static_cast<ghosts *>(c);
        const int p = pc - 1; // Fortran to C index conversion
        if (c == nullptr)
        {
            return 0;
        }
        // Check if p is within bounds
        if (p < 0 || p >= rc->size())
        {
            return 0;
        }
        return (*rc)[p].size();
    }

    void cpp_copy_shells_list(void *c, int pc, int *shells,int n)
    {
        // p is the process id
        ghosts *rc = static_cast<ghosts *>(c);
        const int p = pc - 1; // Fortran to C index conversion
        if (c == nullptr)
        {
            return;
        }
        // Check if p is within bounds
        if (p < 0 || p >= rc->size())
        {
            return;
        }
        // copy the shells list to the array shells
        std::copy((*rc)[p].begin(), (*rc)[p].end(), shells);
    }

    void cpp_destroy_ghosts(void *c)
    {
        // destroy the ghosts object
        ghosts *rc = static_cast<ghosts *>(c);
        // free each vector in the ghosts
        for (auto &vec : *rc)
        {
            vec.clear();
        }
        // free the ghosts object
        delete rc;
    }
}