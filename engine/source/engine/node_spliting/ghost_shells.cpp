//Copyright>    OpenRadioss
//Copyright>    Copyright (C) 1986-2025 Altair Engineering Inc.
//Copyright>
//Copyright>    This program is free software: you can redistribute it and/or modify
//Copyright>    it under the terms of the GNU Affero General Public License as published by
//Copyright>    the Free Software Foundation, either version 3 of the License, or
//Copyright>    (at your option) any later version.
//Copyright>
//Copyright>    This program is distributed in the hope that it will be useful,
//Copyright>    but WITHOUT ANY WARRANTY; without even the implied warranty of
//Copyright>    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//Copyright>    GNU Affero General Public License for more details.
//Copyright>
//Copyright>    You should have received a copy of the GNU Affero General Public License
//Copyright>    along with this program.  If not, see <https://www.gnu.org/licenses/>.
//Copyright>
//Copyright>
//Copyright>    Commercial Alternative: Altair Radioss Software
//Copyright>
//Copyright>    As an alternative to this open-source version, Altair also offers Altair Radioss
//Copyright>    software under a commercial license.  Contact Altair to discuss further if the
//Copyright>    commercial version may interest you: https://www.altair.com/radioss/.
#include <iostream>
#include <vector>
#include <algorithm>

// Type alias for ghost shells data structure
using Ghosts = std::vector<std::vector<int>>;

bool is_shell_ghost_on_proc(
    int shell_index, // Index of the shell
    int process_id,  // Process ID
    int *shells,     // Shell nodes array
    int *mask,       // Mask array for nodes
    int nspmd,       // Number of processes
    int max_node_id) // Maximum node ID
{
    // A shell should be included if at least one of its nodes has mask == 1
    constexpr int nodes_per_shell = 4;

    for (int j = 0; j < nodes_per_shell; ++j)
    {
        const int node_id = shells[shell_index * nodes_per_shell + j];
        if (node_id > 0 && node_id <= max_node_id)
        {
            // For a Fortran array mask(nspmd,numnodes)
            // Access mask(p,node_id) as mask[(node_id-1)*nspmd + (p)]
            const int mask_index = (node_id - 1) * nspmd + process_id;
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
    Ghosts *cpp_build_ghosts(
        int *shells, // 4 x nb_shells array : node id of the shells
        int nb_shells,
        int *mask, // nspmd x numnod array: 1 => node belongs to process
        int nspmd,
        int numnod)
    {
        if (!shells || !mask || nb_shells < 0 || nspmd <= 0 || numnod <= 0)
        {
            return nullptr;
        }

        auto *ghost_data = new Ghosts(nspmd);
        const int max_node_id = numnod;

        // Build ghost shell lists for each process
        for (int shell_idx = 0; shell_idx < nb_shells; ++shell_idx)
        {
            for (int process_idx = 0; process_idx < nspmd; ++process_idx)
            {
                if (is_shell_ghost_on_proc(shell_idx, process_idx, shells, mask, nspmd, max_node_id))
                {
                    (*ghost_data)[process_idx].push_back(shell_idx + 1); // Convert to 1-based for Fortran
                }
            }
        }

        // Remove duplicates from each process's shell list
        if (nb_shells > 0)
        {
            for (int process_idx = 0; process_idx < nspmd; ++process_idx)
            {
                auto &shell_list = (*ghost_data)[process_idx];
                std::sort(shell_list.begin(), shell_list.end());
                auto new_end = std::unique(shell_list.begin(), shell_list.end());
                shell_list.erase(new_end, shell_list.end());
            }
        }

        return ghost_data;
    }

    int *cpp_get_shells_list(void *c, int pc, int *n)
    {
        if (!c || !n)
        {
            if (n)
                *n = 0;
            return nullptr;
        }

        auto *ghost_data = static_cast<Ghosts *>(c);
        const int process_idx = pc - 1; // Fortran to C index conversion

        // Check bounds
        if (process_idx < 0 || process_idx >= static_cast<int>(ghost_data->size()))
        {
            *n = 0;
            return nullptr;
        }

        auto &shell_list = (*ghost_data)[process_idx];
        *n = static_cast<int>(shell_list.size());

        return *n == 0 ? nullptr : shell_list.data();
    }

    int cpp_get_shells_list_size(void *c, int pc)
    {
        if (!c)
        {
            return 0;
        }

        auto *ghost_data = static_cast<Ghosts *>(c);
        const int process_idx = pc - 1; // Fortran to C index conversion

        // Check bounds
        if (process_idx < 0 || process_idx >= static_cast<int>(ghost_data->size()))
        {
            return 0;
        }

        return static_cast<int>((*ghost_data)[process_idx].size());
    }

    void cpp_copy_shells_list(void *c, int pc, int *shells, int n)
    {
        if (!c || !shells)
        {
            return;
        }

        auto *ghost_data = static_cast<Ghosts *>(c);
        const int process_idx = pc - 1; // Fortran to C index conversion

        // Check bounds
        if (process_idx < 0 || process_idx >= static_cast<int>(ghost_data->size()))
        {
            return;
        }

        // Copy the shells list to the output array
        const auto &shell_list = (*ghost_data)[process_idx];
        std::copy(shell_list.begin(), shell_list.end(), shells);
    }

    void cpp_destroy_ghosts(void *c)
    {
        if (!c)
        {
            return;
        }

        auto *ghost_data = static_cast<Ghosts *>(c);
        delete ghost_data;
    }
}
