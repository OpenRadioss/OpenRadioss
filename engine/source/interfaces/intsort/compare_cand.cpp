//Copyright>    OpenRadioss
//Copyright>    Copyright (C) 1986-2024 Altair Engineering Inc.
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
#include <vector>
#include <utility>
#include <iostream>
#include <algorithm>

extern "C"
{

    void compare_cand(int *cand_n, int *cand_e, int ii_stok, int *cand_n_ref, int *cand_e_ref, int ii_stok_ref)
    {
        std::vector<std::pair<int, int>> candidates, candidates_ref;
        for (int i = 0; i < ii_stok; ++i)
        {
            candidates.emplace_back(cand_n[i], cand_e[i]);
        }
        for (int i = 0; i < ii_stok_ref; ++i)
        {
            candidates_ref.emplace_back(cand_n_ref[i], cand_e_ref[i]);
        }

        std::sort(candidates.begin(), candidates.end());
        std::sort(candidates_ref.begin(), candidates_ref.end());

        int num_matches = 0;
        int i = 0, j = 0;
        while (i < ii_stok && j < ii_stok_ref)
        {
            if (candidates[i] == candidates_ref[j])
            {
                num_matches++;
                ++i;
                ++j; 
            }
            else if (candidates[i] < candidates_ref[j])
            {
                ++i; 
            }
            else
            {
                ++j; 
            }
        }
        std::cout << "number of candidates: " << ii_stok << "(ref: "<<ii_stok_ref<<")\n";
        if (num_matches == ii_stok_ref)
        {
            std::cout << "Success: All candidates_ref are found in candidates.\n";
        }
        else
        {
            std::cout << "Failure: Not all candidates_ref are found in candidates.\n";
        }
    }
}
