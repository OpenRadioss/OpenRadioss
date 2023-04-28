Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2023 Altair Engineering Inc.
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
Chd|====================================================================
Chd|  H3D_SKIN_TSH_DIM              source/output/h3d/h3d_results/h3d_skin_ixskin.F
Chd|-- called by -----------
Chd|        H3D_SKIN_DIM                  source/output/h3d/h3d_results/h3d_skin_ixskin.F
Chd|-- calls ---------------
Chd|        INITBUF                       share/resol/initbuf.F         
Chd|        INITBUF_MOD                   share/resol/initbuf.F         
Chd|====================================================================
      SUBROUTINE H3D_SKIN_TSH_DIM(IPARG    ,TAG_SKINS6, NUMSKIN)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INITBUF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL
      INTEGER IPARG(NPARG,*),NUMSKIN, TAG_SKINS6(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C----------------------------------------------- 
      INTEGER I,NG,
     .        MLW     ,NEL     ,NFT     ,IAD     ,ITY     ,    
     .        NPT     ,JALE    ,ISMSTR  ,JEUL    ,JTUR    ,    
     .        JTHE    ,JLAG    ,JMULT   ,JHBE    ,JIVF    ,    
     .        NVAUX   ,JPOR    ,KCVT    ,JCLOSE  ,JPLASOL ,    
     .        IREP    ,IINT    ,IGTYP   ,ISRAT   ,ISROT   ,    
     .        ICSEN   ,ISORTH  ,ISORTHG ,IFAILURE,JSMS   ,
     .        NN,ICS,ISOLNOD
C-----------------------------------------------
       DO NG=1,NGROUP
        CALL INITBUF(IPARG    ,NG      ,                      
     2        MLW     ,NEL     ,NFT     ,IAD     ,ITY     ,    
     3        NPT     ,JALE    ,ISMSTR  ,JEUL    ,JTUR    ,    
     4        JTHE    ,JLAG    ,JMULT   ,JHBE    ,JIVF    ,    
     5        NVAUX   ,JPOR    ,KCVT    ,JCLOSE  ,JPLASOL ,    
     6        IREP    ,IINT    ,IGTYP   ,ISRAT   ,ISROT   ,    
     7        ICSEN   ,ISORTH  ,ISORTHG ,IFAILURE,JSMS    ) 
!
        IF(MLW == 13 .OR. MLW == 0.OR.ITY /= 1) CYCLE   
C------ possible to remove(w/ TAG_SKINS6) case of several layers in mesh (even not recommended)        
        IF (IGTYP==20 .OR. IGTYP==21 .OR. IGTYP==22) THEN
            NUMSKIN = NUMSKIN + 2*NEL
        ENDIF !IF (IGTYP==20 
       END DO ! NG=1,NGROUP
C-----------
      RETURN
      END
