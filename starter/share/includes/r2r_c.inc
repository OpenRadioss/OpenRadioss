Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2022 Altair Engineering Inc.
Copyright>    
Copyright>        This program is free software: you can redistribute it and/or modify
Copyright>        it under the terms of the GNU Affero General Public License as published by
Copyright>        the Free Software Foundation, either version 3 of the License, or
Copyright>        (at your option) any later version.
Copyright>    
Copyright>        This program is distributed in the hope that it will be useful,
Copyright>        but WITHOUT ANY WARRANTY; without even the implied warranty of
Copyright>        MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
Copyright>        GNU Affero General Public License for more details.
Copyright>    
Copyright>        You should have received a copy of the GNU Affero General Public License
Copyright>        along with this program.  If not, see <https://www.gnu.org/licenses/>.
Copyright>    
Copyright>    
Copyright>        Commercial Alternative: Altair Radioss Software 
Copyright>    
Copyright>        As an alternative to this open-source version, Altair also offers Altair Radioss 
Copyright>        software under a commercial license.  Contact Altair to discuss further if the 
Copyright>        commercial version may interest you: https://www.altair.com/radioss/.    
C.../RAD2R/
Cmwdev004      
      COMMON /RAD2R/NB_PART_SUB,NSUBDOM,NNODN,NELCN,NELTGN, 
     .       SIZ_IPM_NEW,SIZ_PM_NEW,SIZ_IGEO_NEW,SIZ_GEO_NEW,
     .       IDDOM,IPID,DOM_NAME,NELTN,NELRN,NELPN,NPARN,
     .       NELSN,NELS10N,NELS20N,NELS16N,NELQN,NELURN,NODSUPR,
     .       R2R_FILNAM,N_PART,NEW_NRBY,NEW_NJOINT,NEW_NINTER,
     .       NEW_NLINK,NEW_NRBE3,NEW_NRBE2,NEW_NUMMPC,FLG_SPLIT,
     .       NEW_NVOLU,FLG_TIED,NTHREAD_R2R,NL_DDR2R,SFRONTB_R2R,
     .       R2R_SIU,NUMMAT0,NUMGEO0,SUB_FLAG,NSPHN,NINLETN,NEW_NGJOINT,
     .       FLG_SWALE,FLG_FSI,ROOTNAM0,IDDOM_L,R2R_FLAG_ERR_OFF,
     .       NEW_NRBYKIN,NEW_HM_NVOLU
                
      INTEGER NB_PART_SUB,NSUBDOM,NNODN,NELCN,NELTGN,NELTN     
      INTEGER SIZ_IPM_NEW,SIZ_PM_NEW,IPID,NPARN,NELRN,NELPN 
      INTEGER SIZ_IGEO_NEW,SIZ_GEO_NEW,IDDOM,NODSUPR,N_PART
      INTEGER NELSN,NELS10N,NELS20N,NELS16N,NELQN,NELURN
      INTEGER NEW_NRBY,NEW_NJOINT,NEW_NINTER,NEW_NLINK,NEW_NRBE3,NEW_NRBYKIN
      INTEGER NEW_NRBE2,NEW_NGJOINT,NEW_NUMMPC,FLG_SPLIT,NEW_NVOLU,NEW_HM_NVOLU
      INTEGER FLG_TIED(5),NTHREAD_R2R,NL_DDR2R,SFRONTB_R2R,R2R_SIU
      INTEGER NUMMAT0,NUMGEO0,SUB_FLAG,NSPHN,NINLETN,FLG_SWALE,FLG_FSI,IDDOM_L
      INTEGER R2R_FLAG_ERR_OFF  
      CHARACTER DOM_NAME*2048,R2R_FILNAM*100,ROOTNAM0*80
