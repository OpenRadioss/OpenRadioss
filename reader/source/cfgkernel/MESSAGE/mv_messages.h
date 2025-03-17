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
#ifndef MV_MESSSAGES_H
#define MV_MESSSAGES_H

#include <HCDI/hcdi.h>
/** @name Messages strings */
/*@{*/

/** Message types (which library?) */
enum MvMsgType_s {
  /** Unknown value */
  MSGT_UNKNOWN=-1,
  /** Utils module's messages */
  MSGT_UTILS, 
  /** Kernel module's messages */
  MSGT_KERNEL,
  /** Model module's messages */
  MSGT_MODEL,
  /** Model I/O module's messages */
  MSGT_MODEL_IO,    
  /** Read (D00, version 5.X, module's messages */
  MSGT_READ_D00_5X, 
  /** Read (D00, version 4.X and past, fixed or block) module's messages */
  MSGT_READ_D00_4X,
  /** Read (Nastran files) module's messages */
  MSGT_READ_NASTRAN,
  /** Read (Ideas files) module's messages */
  MSGT_READ_UNV,
  /** Write (D00, version 5.X, module's messages */
  MSGT_WRITE_D00_5X, 
  /** Write (D00, version 4.x block, x>=1) module's messages */
  MSGT_WRITE_D00_4XB,
  /** Write (D00, version 4.1 fix) module's messages */
  MSGT_WRITE_D00_41F,
  /** Write (Nastran files) module's messages */
  MSGT_WRITE_NASTRAN,
  /** Write (Ideas files) module's messages */
  MSGT_WRITE_UNV,
  /** Actions (action management, ModEdit batch, ...) module's messages */
  MSGT_ACTION,
  
  MSGT_BATCH,
  /** Data bases module's messages */
  MSGT_DATA_BASE,
  /** Ckeck module's messages */
  MSGT_CHECK,
  /** Remove module's messages */
  MSGT_REMOVE,
  /** Submodel to model module's messages */
   
  MSGT_SUBMODEL2MODEL,
   
  /** Application module's messages */
  MSGT_APP,
  /** Mpost module's messages */
  MSGT_MPOST, 
  
  MSGT_READ_D00_INFO,
  MSGT_READ_D00_ERROR,
  MSGT_READ_D00_WARNING,
  MSGT_WRITE_D00_INFO,
  MSGT_WRITE_D00_ERROR,
  MSGT_BOLTMAKE_INFO,
  MSGT_BOLTMAKE_ERROR,
  MSGT_BOLTMAKE_WARNING,
  MSGT_BOTTOM_ERROR,
  MSGT_BOUND_INFO,
  MSGT_BOUND_ERROR,
  MSGT_BOUND_WARNING,
  MSGT_CALLSAFETY_INFO,
  MSGT_CLOAD_INFO,
  MSGT_CLOAD_ERROR,
  MSGT_CLOAD_WARNING,
  MSGT_CONNEC_INFO,
  MSGT_CONNEC_ERROR,
  MSGT_CONNEC_WARNING,
  MSGT_CONTACT_INFO,
  MSGT_CONTACT_ERROR,
/* PM:0032:03/03/2004 */
  MSGT_MONIVOL_INFO,
  MSGT_DISCONNE_INFO,
  MSGT_DISPIMP_INFO,
  MSGT_DISPIMP_ERROR,
  MSGT_DISPIMP_WARNING,
  MSGT_DISPLAY_INFO,
  MSGT_DISPLAY_ERROR,
  MSGT_MECASPOT_ERROR,
  MSGT_ELEMENTARY_TIMESTEP, /* PM:0134:28/02/2005 */
  MSGT_EXTRACT_INFO,
  MSGT_EXTRACT_ERROR,
  MSGT_EXTRACT_WARNING,
  MSGT_GUI_OGL_ERROR,
  MSGT_GENERAL_INFO,
  MSGT_GENERAL_ERROR,
  MSGT_GENERAL_WARNING,
  MSGT_GLUEMAKE_INFO,
  MSGT_GLUEMAKE_ERROR,
  MSGT_GLUEMAKE_WARNING,
  MSGT_GRAVMAKE_INFO,
  MSGT_GRAVMAKE_ERROR,
  MSGT_GRAVMAKE_WARNING,
  MSGT_GTKGLAREA_ERROR,
  MSGT_HCMASS_LABEL,      
  MSGT_HCMASS_WARNING,
  MSGT_HCMASS_ERROR,
  MSGT_HCTRANSFORMMANAGER_LABEL,   
  MSGT_HCTRANSFORMMANAGER_MESSAGE, 
  MSGT_HCTRANSFORMMANAGER_ERROR,   
  MSGT_HELP_INFO,
  MSGT_HEMMAKE_INFO,
  MSGT_HEMMAKE_ERROR,
  MSGT_HEMMAKE_WARNING,
  MSGT_HMSCRIPT_LABEL,
  MSGT_HMSCRIPT_MESSAGE,
  MSGT_HMSCRIPT_ERROR,
  MSGT_MAINWINDOW_ERROR,
  MSGT_MASSDIST_INFO,
  MSGT_MASSDIST_ERROR,
  MSGT_MASSDIST_WARNING,
  MSGT_MATGEN_WARNING,
  /* PM:0067:22/07/2004 */
  MSGT_MATPROP_INFO,
  MSGT_MATPROP_ERROR,
  MSGT_MATPROP_WARNING,
  /* END PM:0067 */
  MSGT_MERGENAS_INFO,
  MSGT_MERGENAS_ERROR,
  MSGT_MESH2D_ERROR,
  MSGT_MESHGEN_INFO,
  MSGT_MESHGEN_ERROR,
  MSGT_MESHGEN_WARNING,
  MSGT_MESSAGEWIN_INFO,
  MSGT_MODIFYELEM_INFO,
  MSGT_MODIFYELEM_ERROR,
  MSGT_MODULE_INFO,
  MSGT_MOVE_INFO,
  MSGT_MOVE_ERROR,
  MSGT_MOVE_WARNING,

  MSGT_NEWAUTOGEN_ERROR,

  MSGT_NEWBARGEN_INFO,
  MSGT_NEWBARGEN_ERROR,
  MSGT_NEWMASSGEN_INFO,
  MSGT_NEWMASSGEN_ERROR,
  MSGT_NEWMATGEN_INFO,
  MSGT_NEWMATGEN_ERROR,
  MSGT_NEWMATGEN_WARNING,
  MSGT_NEWPROPGEN_INFO,
  MSGT_NEWPROPGEN_ERROR,
  MSGT_NEWPROPGEN_WARNING,
  MSGT_NEWMERGE_INFO,
  MSGT_NEWMERGE_ERROR,
  MSGT_NEWPOSTPRO_INFO,
  MSGT_NEWPOSTPRO_ERROR,
  MSGT_NEWPOSTPRO_WARNING,
  MSGT_NEWSPOT_INFO,
  MSGT_NEWSPOT_ERROR,
  MSGT_OBJECTCFG_INFO, /* PM:0224:09/12/2005 */
  MSGT_PURGE_INFO,
  MSGT_RADCHECK_INFO,
  MSGT_RADCHECK_ERROR,
  MSGT_RADCHECK_WARNING,
  MSGT_RBODYRIVET_INFO,
  MSGT_RBODYRIVET_ERROR,
  MSGT_RBODYRIVET_WARNING,
  MSGT_READHONDA_INFO,
  MSGT_READHONDA_ERROR,
  MSGT_READHONDA_WARNING,
  MSGT_READ_M00_ERROR,
  MSGT_READNASM00_INFO,
  MSGT_READNASM00_WARNING,
  MSGT_READUNV_INFO,
  MSGT_READUNV_ERROR,
  MSGT_READUNV_WARNING,
  MSGT_READD2M_INFO,
  MSGT_READD2M_ERROR,
  MSGT_READD2M_WARNING,
  MSGT_READP2M_INFO,
  MSGT_REN2MECA_ERROR,
  MSGT_REN2MECA_WARNING,
  MSGT_RENUM_WARNING,
  MSGT_REPLACE_INFO,
  MSGT_REPLACE_ERROR,
  MSGT_SIMPLE_INFO, 
  MSGT_SIMPLE_ERROR,
  MSGT_SIMPLE_WARNING,
  MSGT_SPOTMAKE_INFO,
  MSGT_SPOTMAKE_ERROR,
  MSGT_SPOTMAKE_WARNING,
  MSGT_SUBMODELLING_INFO, 
  MSGT_SYSGEN_INFO,
  MSGT_SYSGEN_ERROR,
  MSGT_SYSGEN_WARNING,
  MSGT_TREE_INFO,
  MSGT_TREE_ERROR,
  MSGT_TREEMODEL_LABEL, 
  MSGT_TREEMODEL_POPUP,   
  MSGT_TREEMODEL_MESSAGE,  
  MSGT_TREEMODEL_TOOLTIP, 
  MSGT_UTILITY_ERROR,
  MSGT_UTILITY_WARNING,
  MSGT_UTILITYGUI_INFO,
  MSGT_UTILITYGUI_ERROR,
  MSGT_UTILITYGUI_WARNING,
  MSGT_VECTOR_ERROR,
  MSGT_VELIMP_INFO,
  MSGT_VELIMP_ERROR,
  MSGT_VELIMP_WARNING,
  MSGT_VELOCITY_INFO,
  MSGT_VELOCITY_ERROR,
  MSGT_WELDMAKE_INFO,
  MSGT_WELDMAKE_ERROR,
  MSGT_WELDMAKE_WARNING,
  MSGT_WRITE_D01_ERROR,
  MSGT_WRITE_D01_WARNING,
  MSGT_WRITENAS_INFO,
  MSGT_WRITENAS_WARNING,
  MSGT_WRITEPAM_INFO,
  MSGT_WRITEPAM_ERROR,
  MSGT_WRITEPAM_WARNING,
  MSGT_WRITE41F_INFO,
  MSGT_WRITE41F_ERROR,
  MSGT_WRITE41F_WARNING,
  MSGT_WRITEUNV_INFO,
  MSGT_WRITEUNV_WARNING,
  MSGT_WRITEM2D_INFO,
  MSGT_WRITEM2D_ERROR,
  MSGT_WRITEM2D_WARNING,
  
  
  MSGT_READ_M00,
  
  
  MSGT_WRITE_M00_ERROR,
  
  
  /** Parser module's messages */
  MSGT_PARSER,
  /** Units module's messages */
  MSGT_MUNITS,
  
  MSGT_GENEDIT_INFO,
  /* FLG 05_10_2011 */
  MSGT_VERTICALPANEL_INFO, 
  /*rohith for GENEDIT messages*/
  MSGT_GLOBAL,
  /** Last value (for closing the enum) */
  MSGT_LAST
};

/** Message types (which library?) */
typedef enum MvMsgType_s MvMsgType_e;


#ifdef __cplusplus
extern "C" {
#endif

/** Gets the array containing the messages (for a given library) */
HC_DATA_DLL_API const char **MV_get_msg_array(MvMsgType_e type);

/** Gets the number of messages (of a given library) */
int          MV_get_nb_msg(MvMsgType_e type);

#ifdef __cplusplus
}
#endif

/*@}*/

#endif 




