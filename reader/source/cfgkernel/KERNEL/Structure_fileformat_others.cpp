/*Copyright>    OpenRadioss
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
//Copyright>    commercial version may interest you: https://www.altair.com/radioss/.*/
#include "Structure_fileformat_others.h"
#include <KERNEL_BASE/utils.h>

extern "C" void MCDS_new_ff_cell_app_opt(ff_cell_t **cell_pfp)
{
    if (cell_pfp == NULL)
        return;
    *cell_pfp = (ff_cell_t *) new ff_append_option_cell_t;
    ((ff_formated_cell_t *)(*cell_pfp))->format = NULL;
}

extern "C" void MCDS_free_app_opt(ff_cell_t *cell_p)
{
    if (cell_p == NULL)
        return;
    delete cell_p;
    cell_p = NULL;
}
