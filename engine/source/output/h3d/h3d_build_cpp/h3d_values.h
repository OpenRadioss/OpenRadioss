//Copyright>    OpenRadioss
//Copyright>    Copyright (C) 1986-2022 Altair Engineering Inc.
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
    extern H3DFileInfo* h3d_file;
    extern bool rc;
    extern H3D_ID subcase_id;

    extern unsigned int   model_count;
    extern bool model_tabular;
    extern H3D_TRIBOOL model_adaptive ;
    extern H3D_ID model_id;

    extern H3D_ID assm_poolname_id;


    extern unsigned int assm_count;
    extern H3D_ID assm_id;
    extern H3D_ID model_as_parent;

    extern H3D_ID assm_parent;   // assm_parent = 1

    extern H3D_ID comp_poolname_id;
    extern H3D_ID node_poolname_id;

    extern unsigned int comp_count;
    extern H3D_ID comp_id;
    extern H3D_ID comp_parent_id;      // comp_parent_id = assm 1

    extern H3D_SIM_IDX sim_idx;
    extern H3D_ID rbody_poolname_id;
    extern H3D_ID rbe2_poolname_id;
    extern H3D_ID rbe3_poolname_id;
    extern H3D_ID rwall_poolname_id;
    extern H3D_ID spring_poolname_id;
    extern H3D_ID truss_poolname_id;
    extern H3D_ID elem1D_poolname_id;
    extern H3D_ID elem2D_poolname_id;
    extern H3D_ID sh4n_poolname_id;
    extern H3D_ID sh3n_poolname_id;
    extern H3D_ID shell_poolname_id;
    extern H3D_ID quad_poolname_id;
    extern H3D_ID skin_poolname_id;
    extern H3D_ID solid4n_poolname_id;
    extern H3D_ID solid10n_poolname_id;
    extern H3D_ID solid5n_poolname_id;
    extern H3D_ID solid6n_poolname_id;
    extern H3D_ID solid8n_poolname_id;
    extern H3D_ID solid16n_poolname_id;
    extern H3D_ID solid20n_poolname_id;
    extern H3D_ID rigid_poolname_id;
    extern H3D_ID solid_poolname_id;
    extern H3D_ID onedelem_poolname_id;
    extern H3D_ID beam_poolname_id;
    extern H3D_ID sph_poolname_id;
    extern H3D_ID sphcell_poolname_id;
    extern H3D_ID sphnode_poolname_id;

    extern char edata_type[50];
    extern H3D_ID dt_id;
    extern unsigned int pool_count;
    extern unsigned int layer_count;
    extern bool has_corners;
    extern H3D_TENSOR_TYPE tensor_type; // unused
    extern float poisson;		// default & unused
    extern unsigned int dt_count;
    extern H3D_ID* layername_ids;
