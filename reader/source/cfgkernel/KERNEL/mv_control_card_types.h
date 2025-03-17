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

//

#ifndef MV_CONTROL_CARD_TYPES_H
#define MV_CONTROL_CARD_TYPES_H


/** @name Control card types */



enum MvControlCardType_s {

  CCT_UNKNOWN,
  CCT_MODEL,
  CCT_MEMORY,
  CCT_SPMD,
  CCT_IOFLAG,
  CCT_INISTA,
  CCT_ANALY,
  CCT_DEF_SOLID,
  CCT_DEF_SHELL,
  CCT_RANDOM,
  CCT_SPHGLO,
  CCT_CHECK,
  CCT_LAGMUL,
  CCT_ARCH,       
  CCT_ANIM_VERS,  
  CCT_IMPL_GRAPH, 
  CCT_UPWIND,
  CCT_INTTHICK,
  CCT_SHFRA,
  CCT_CAA,
  CCT_DEFAULT_TYPE,
  
  CCT_ANIMVERS,  
  CCT_ALE_DONEA,
  CCT_ALE_DISP,
  CCT_ALE_SPRING,
  CCT_ALE_ZERO,
  CCT_ALE_STANDARD,
  CCT_ADMESH_GLOBAL,
  CCT_AMS,
  CCT_LAST

};

enum MvEngineControlCardType_s {
  //ENGINE CARD
  ECCT_UNKNOWN,
  ECCT_KILL, 
  ECCT_ATFILE,
  ECCT_PRINT,
  ECCT_PROC,
  ECCT_RFILE, 
  ECCT_RUN, 
  ECCT_STOP,  
  ECCT_TFILE, 
  ECCT_PARITH,
  ECCT_DT, 
  ECCT_DT_ELTYPE_IFLAG,
  ECCT_DT_ELTYPE_KEYWORD_IFLAG,
  ECCT_DT_GLOB_IFLAG,
  ECCT_DT_AMS, 
  ECCT_DT1,
  ECCT_DTIX,
  ECCT_DYREL,
  ECCT_DYREL1,
  ECCT_FXINP,
  ECCT_KEREL,
  ECCT_KEREL1,
  ECCT_MON,
  ECCT_PATRAN,
  
  ECCT_RAD2RAD,
  
  ECCT_ANIM,
  ECCT_IMPL,
  ECCT_STATE,
  ECCT_BCS,
  ECCT_BCSR,
  ECCT_INIV,
  ECCT_VEL,
  ECCT_RBODY,  
  ECCT_DAMP,
  ECCT_INTER,
  ECCT_DEL,
  ECCT_TITLE,
  ECCT_TH,
  ECCT_AATFILE,
  ECCT_SHVER_V51,
  ECCT_RERUN,
  ECCT_MADYMO,
  ECCT_OUTP,
  ECCT_SHSUB, 
  ECCT_FVMBAG,
  ECCT_ABF,
  ECCT_ALE,
  ECCT_UPWIND,
  
  ECCT_VERS,  

  
  ECCT_ANIM_BRICK_TENS,
  ECCT_ANIM_DT,
  ECCT_ANIM_GZIP,
  ECCT_ANIM_ELTYPE_RESTYPE,
  ECCT_ANIM_ELTYPE_FORC, 
  ECCT_ANIM_MASS,
  ECCT_ANIM_MAT,
  ECCT_ANIM_NODA_RESTYPE,
  ECCT_ANIM_SENSOR,
  ECCT_ANIM_SHELL_EPSP_KEYWORD4,
  ECCT_ANIM_SHELL_FLDZ,
  ECCT_ANIM_SHELL_IDPLY,
  ECCT_ANIM_SHELL_IDPLY_DAMA,
  ECCT_ANIM_SHELL_IDPLY_EPSP,
  ECCT_ANIM_SHELL_IDPLY_PHI,
  ECCT_ANIM_SHELL_IDPLY_RESTYPE,
  ECCT_ANIM_SHELL_TENS_RESTYPE_KEYWORD4,
  ECCT_ANIM_VECT_RESTYPE,
  ECCT_ANIM_VERS_VERNO,
  ECCT_ANIM_GPS1,
  ECCT_ANIM_GPS2,
  ECCT_ANIM_KEEPD,
  ECCT_ANIM_GPS_TENS,
  ECCT_ANIM_TITLE,

  ECCT_ALE_GRID_DISP,
  ECCT_ALE_GRID_DONEA,
  ECCT_ALE_GRID_SPRING,
  ECCT_ALE_GRID_STANDARD,
  ECCT_ALE_GRID_ZERO,
  ECCT_IMPL_AUTOSPC,
  ECCT_IMPL_BUCKL_1,
  ECCT_IMPL_BUCKL_2,
  ECCT_IMPL_INTER_KNONL,
  ECCT_IMPL_LINEAR_SCAUCHY,
  ECCT_IMPL_RREF_OFF, 
  ECCT_IMPL_QSTAT_MRIGM,   
  ECCT_IMPL_NONLIN_SOLVINFO,

  ECCT_STATE_BRICK_AUX_FULL,
  ECCT_STATE_BRICK_STRAIN_FULL,
  ECCT_STATE_BRICK_STRES_FULL, 
  ECCT_STATE_SHELL_ORTHL,
  ECCT_STATE_DT,
  ECCT_STATE_SHELL_AUX_FULL,
  ECCT_STATE_SHELL_EPSP_FULL,
  ECCT_STATE_SHELL_STRESS_FULL, 
  ECCT_STATE_SHELL_STRAIN_FULL, 
  ECCT_STATE_SHELL_FAIL,  
  ECCT_STATE_NO_DEL,
  ECCT_RBODY_ON,
  ECCT_RBODY_OFF,
  ECCT_DEL_1,
  ECCT_DEL_INTER,
  ECCT_AATFILE_KEYWORD2,	
  ECCT_RUN_NAME_NUMBER_LETTER,
  ECCT_RFILE_N,
  ECCT_DT_THERM,
  ECCT_ENGINE_UPWIND,
  

  ECCT_LAST
  
};





typedef enum MvControlCardType_s MvControlCardType_e;
typedef enum MvEngineControlCardType_s MvEngineControlCardType_e;
//@}

#endif //MV_CONTROL_CARD_TYPES_H




