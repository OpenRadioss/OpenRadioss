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

#include <UTILS/win32_utils.h> 



#include <UTILS/mv_cstdlib.h> 
#include <UTILS/mv_stl_various.h> 
#include <UTILS/error.h>          
#include <UTILS/system_utils.h>   
#include <UTILS/set_utils.h>
#include <UTILS/file_utils.h>


#include "mvc_utils.h"
#include "mv_data_cfg.h"
#include "mv_data_cfg_parser.h"
#include <HCDI/hcdi_multicfgkernelmgr.h>
#include <KERNEL/cfg_kernel.h>

//#include <boost/filesystem.hpp>



//typedef const MvPreDatasHierarchy_t                     *LocPreDatasHierarchyPtr_t;
//typedef map<MvDataTreeType_e,LocPreDatasHierarchyPtr_t>  LocPreDataHierarchyMap_t;



static const string             &loc_get_cfg_file(MvDataTreeType_e tree_type);
//static LocPreDataHierarchyMap_t *loc_get_cfg_map_ptr(int option=0);


//static LocPreDataHierarchyMap_t *cfg_map_p[FF_LAST]= { NULL };

/* --------- Exported functions --------- */


int MV_get_data_tree_type_index(MvDataTreeType_e dtt) {
  int a_ind=-1;
  //
  switch(dtt) {
  case DTT_ASSIGNED:   a_ind=0; break;
  case DTT_POST:       a_ind=1; break;
  case DTT_CONNECTION: a_ind=2; break;
  case DTT_SCRIPT:     a_ind=3; break;
  default:
    throw MvError_t("MV_get_data_tree_type_index -> %d is not a valid tree type",dtt);
    //break;
  }
  //
  return a_ind;
}



int MV_get_all_data_tree_types() {
  int a_all_tree_types=0;
  //
  MvDataTreeType_e a_dtt_begin = MV_get_begin_data_tree_type();
  MvDataTreeType_e a_dtt_end   = MV_get_end_data_tree_type();
  MvDataTreeType_e a_dtt;
  for(a_dtt=a_dtt_begin;a_dtt!=a_dtt_end;a_dtt=MV_get_next_data_tree_type(a_dtt)) {
    a_all_tree_types|=a_dtt;
  }
  //
  return a_all_tree_types;
}



bool MV_is_multiple_data_tree_type(MvDataTreeType_e dtt) {
  bool a_is_multiple=(dtt==DTT_POST);
  return a_is_multiple;
}



const string &MV_get_data_tree_type_rank_skeyword(MvDataTreeType_e dtt) {
  static string a_none_rank_skw="";
  static string a_post_rank_skw="ANIM_RANK";
  //
  if(dtt==DTT_POST) return a_post_rank_skw;
  //
  return a_none_rank_skw;
}

void load_data_hiearchy_at_user_location();



void CFGKernel::init_trees_cfg() {
 // MV_get_data_cfg();
    get_tree_cfg(DTT_ASSIGNED, "Assigned data");
}



void CFGKernel::close_trees_cfg() {
   LocPreDataHierarchyMap_t *a_cfg_map_p=loc_get_cfg_map_ptr();
   //
   if(a_cfg_map_p!=NULL) {
     LocPreDataHierarchyMap_t::iterator a_it_begin = a_cfg_map_p->begin();
     LocPreDataHierarchyMap_t::iterator a_it_end   = a_cfg_map_p->end();
     LocPreDataHierarchyMap_t::iterator a_it;
     for(a_it=a_it_begin;a_it!=a_it_end;++a_it) {
       const MvPreDatasHierarchy_t *a_pdh_pf=(*a_it).second;
       delete a_pdh_pf;
     }
   }
   //
   loc_get_cfg_map_ptr(-1);
}



const MvPreDatasHierarchy_t * CFGKernel::get_tree_cfg(MvDataTreeType_e tree_type,const string &title) const {
  LocPreDataHierarchyMap_t &a_pre_data_hierarchy_map=(*loc_get_cfg_map_ptr());
  //
  LocPreDatasHierarchyPtr_t &a_pdh_p=a_pre_data_hierarchy_map[tree_type];

  if(a_pdh_p==NULL) {
    const string &a_cfg_file  = loc_get_cfg_file(tree_type);

    string  a_file_path_syninfo = mv_get_cfg_file("syntax_info.cfg");
    //
    if (a_file_path_syninfo.size())
        ((CFGKernel&)*this).setSyntaxInfo(MvDataCfgParser_t(a_file_path_syninfo).getSyntaxInfoPtr());



    const MvDirList_t &a_cfg_dirs = ((CFGKernel&)*this).get_config_dirs();
    int size = (int)a_cfg_dirs.size();
    char *user_path = getenv(USR_DIR_VAR);
    if ((size == 2) && (user_path != NULL))
    {
        // MV_USR_DIR is defined, need to append data hierarchy contents into installed data hierarchy
        MvPreDatasHierarchy_t *a_pdh_usr_p = NULL;
        string        a_file_path_usr = mv_get_cfg_file_at_pos(a_cfg_file, 0);
        string        a_file_path_install = mv_get_cfg_file_at_pos(a_cfg_file, 1);
        //
        if (a_file_path_usr.size())
            a_pdh_usr_p = MvDataCfgParser_t(a_file_path_usr).getDatasHierarchyPtr((CFGKernel&)*this, title);

        if (a_file_path_install.size())
            a_pdh_p = MvDataCfgParser_t(a_file_path_install).getDatasHierarchyPtr((CFGKernel&)*this, title);

        if (a_pdh_p && a_pdh_usr_p)
        {
            if (a_pdh_p->getNbChildren())
            {
                MvObjectTypeSet_t type_set;
                a_pdh_p->getObjTypeList(&type_set);


                const MvPreDatasHierarchyList_t& child = a_pdh_usr_p->getChildList();
                MvPreDatasHierarchyList_t::const_iterator iter_b = child.begin();
                MvPreDatasHierarchyList_t::const_iterator iter_e = child.end();
                MvPreDatasHierarchyList_t::const_iterator iter;
                for (iter = iter_b; iter != iter_e; ++iter)
                {
                    MvPreDatasHierarchy_t* a_hier = *iter;
                    MvFullType_t fulltype = a_hier->getFullType();
                    obj_type_e type_to_find = fulltype.getType();

                    if (type_to_find < type_set)
                    {
                        const MvPreDatasHierarchyList_t& child_list = a_pdh_p->getChildList();
                        MvPreDatasHierarchyList_t::const_iterator child_iter_b = child_list.begin();
                        MvPreDatasHierarchyList_t::const_iterator child_iter_e = child_list.end();
                        MvPreDatasHierarchyList_t::const_iterator child_iter;
                        for (child_iter = child_iter_b; child_iter != child_iter_e; ++child_iter)
                        {
                            MvPreDatasHierarchy_t* a_child_hier = *child_iter;
                            if (a_child_hier->findObjectType(type_to_find))
                            {
                                const MvPreDatasHierarchyList_t& newchild = a_hier->getChildList();
                                MvPreDatasHierarchyList_t::const_iterator nchild_iter_b = newchild.begin();
                                MvPreDatasHierarchyList_t::const_iterator nchild_iter_e = newchild.end();
                                MvPreDatasHierarchyList_t::const_iterator nchild_iter;
                                for (nchild_iter = nchild_iter_b; nchild_iter != nchild_iter_e; ++nchild_iter)
                                {
                                    MvPreDatasHierarchy_t* a_new_hier_p = *nchild_iter;
                                    // find in the child list
                                    const MvPreDatasHierarchyList_t& old_child_list = a_child_hier->getChildList();
                                    MvPreDatasHierarchyList_t::const_iterator old_child_iter_b = old_child_list.begin();
                                    MvPreDatasHierarchyList_t::const_iterator old_child_iter_e = old_child_list.end();
                                    MvPreDatasHierarchyList_t::const_iterator old_child_iter;
                                    bool found = false;
                                    MvFullType_t fulltype_to_search = a_new_hier_p->getFullType();
                                    const MvPreDatasHierarchy_t* found_pre_hier_p = NULL;
                                    for (old_child_iter = old_child_iter_b; old_child_iter != old_child_iter_e; ++old_child_iter)
                                    {
                                        MvPreDatasHierarchy_t* old_hier_p = *old_child_iter;
                                        const MvPreDatasHierarchy_t* a_pre_hier_p = old_hier_p->search(fulltype_to_search);
                                        if (a_pre_hier_p != NULL)
                                        {
                                            found_pre_hier_p = a_pre_hier_p;
                                            found = true;
                                            break;
                                        }
                                    }

                                    if (found)
                                    {
                                        a_child_hier->removeChild(found_pre_hier_p);
                                    }

                                    MvPreDatasHierarchy_t* copy_hier = new MvPreDatasHierarchy_t(*a_new_hier_p);
                                    copy_hier->copyChild(a_new_hier_p);
                                    a_child_hier->addChild(copy_hier);
                                }
                            }
                        }
                    }
                    else
                    {
                        MvPreDatasHierarchy_t* copy_hier = new MvPreDatasHierarchy_t(*a_hier);
                        copy_hier->copyChild(a_hier);
                        ((MvPreDatasHierarchy_t*)a_pdh_p)->addChild(copy_hier);
                    }
                }
            }
            else
            {
                MvPreDatasHierarchy_t* copy_hier = new MvPreDatasHierarchy_t(*a_pdh_usr_p);
                copy_hier->copyChild(a_pdh_usr_p);
                ((MvPreDatasHierarchy_t *)a_pdh_p)->addChild(copy_hier);
            }
        }
    }
    else
    {
        string        a_file_path = mv_get_cfg_file(a_cfg_file);
        //
        if (a_file_path.size())
            a_pdh_p = MvDataCfgParser_t(a_file_path).getDatasHierarchyPtr((CFGKernel&)*this, title);
    }
  }
  //
  return a_pdh_p;
}


const MvPreDatasHierarchy_t * CFGKernel::get_datastreehierarchy(MvDataTreeType_e tree_type) const {
  LocPreDataHierarchyMap_t &a_pre_data_hierarchy_map=(*loc_get_cfg_map_ptr());
  //
  LocPreDatasHierarchyPtr_t &a_pdh_p=a_pre_data_hierarchy_map[tree_type];
  //
  return a_pdh_p;
}


/* --------- Static functions --------- */


static const string &loc_get_cfg_file(MvDataTreeType_e tree_type) {
  typedef map<MvDataTreeType_e,string> LocFileNames_t;
  static LocFileNames_t a_file_names;
  //
  if(a_file_names.empty()) {
    a_file_names[DTT_ASSIGNED]   = "data_hierarchy.cfg";
    a_file_names[DTT_POST]       = "post_hierarchy.cfg";
    a_file_names[DTT_CONNECTION] = "connection_hierarchy.cfg";
    a_file_names[DTT_SCRIPT]     = "script_hierarchy.cfg";
  }
  //
  return a_file_names[tree_type];
}



LocPreDataHierarchyMap_t * CFGKernel::loc_get_cfg_map_ptr(int option)  const {
 // LocPreDataHierarchyMap_t *a_cfg_map_p= p_cfg_map_p;
  //
  switch(option) {
  case 1:  // Creation
    if(p_cfg_map_p !=NULL) delete p_cfg_map_p;
    p_cfg_map_p =new LocPreDataHierarchyMap_t();
    break;
  case 0:  // Access
    if(p_cfg_map_p ==NULL) p_cfg_map_p =new LocPreDataHierarchyMap_t();
    break;
  case -1: // Destruction
    if(p_cfg_map_p !=NULL) {
      delete p_cfg_map_p;
      p_cfg_map_p =NULL;
    }
    break;
  default:
    break;
  }
  //
  return p_cfg_map_p;
}

/*for testing*/
void load_data_hiearchy_at_user_location()
{
    //string filename = getenv("HM_MV_USR_DIR");
    string fullpath = "C:/Users/ksingh/Desktop/tree_hierarchy_domain.cfg";// MV_get_cfg_file2(filename);

    if (fullpath.size())
    {
        /*MvPreDatasHierarchy_t* incl_hier_p = MvDataCfgParser_t(fullpath).getDatasHierarchyPtr("included data");
        leafmapmap_t list;
        HCDIgetInfo(incl_hier_p, HCDI_OBJ_TYPE_GROUPS, "ENGG", list);

        delete incl_hier_p;*/
    	//leafmapmap_t list;
    	//HCDILoadDataHiearchyAtUserLocation(HCDI_OBJ_TYPE_GROUPS, "ENGG", false, list);// get all ENGG
        // get all DISCRETE
       // HCDILoadDataHiearchyAtUserLocation(HCDI_OBJ_TYPE_GROUPS, "DISCRETE", false, list);
    }

}

