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
Chd|====================================================================
Chd|  CRK_ACCELE                    source/elements/xfem/accele_crk.F
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        CRACKXFEM_MOD                 share/modules/crackxfem_mod.F 
Chd|====================================================================
      SUBROUTINE CRK_ACCELE(ADDCNE_CRK,INOD_CRK ,NODLEVXF ,NODFT    ,NODLT   ,
     .                      NODENR    ,CRKSKY   ,MS       ,IN       ,ITAB    )
C-----------------------------------------------
      USE CRACKXFEM_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com_xfem1.inc"
#include      "parit_c.inc"
#include      "task_c.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NODFT,NODLT
      INTEGER ADDCNE_CRK(*),INOD_CRK(*),NODENR(*),NODLEVXF(*),ITAB(NUMNOD)
      my_real MS(*),IN(*)
      TYPE(XFEM_SKY_), DIMENSION(*) :: CRKSKY
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,KK,N,NN,NCT_CRK,NC_CRK,ILEV,EN0,NLEV
      my_real
     .   RTMP0,RTMP
      INTEGER, DIMENSION(:),ALLOCATABLE :: FAC
C=======================================================================
      ALLOCATE(FAC(IENRNOD))
c---------------------------
      DO N = NODFT,NODLT                                                
        NN = INOD_CRK(N)                                                
        IF (NN <= 0) CYCLE                                              
c---
        NCT_CRK = ADDCNE_CRK(NN)-1                                      
        NC_CRK  = ADDCNE_CRK(NN+1)-ADDCNE_CRK(NN)                       
        FAC     = 0                                                     
        RTMP0   = ONE                                                    
c
        NLEV = NODLEVXF(NN)                                             
        DO ILEV=1,NLEV                                                  
          DO KK = NCT_CRK+1, NCT_CRK+NC_CRK                             
            EN0 = CRKLVSET(ILEV)%ENR0(2,KK)                             
            IF (EN0 > 0) FAC(EN0) = FAC(EN0) + 1                        
          END DO                                                        
        END DO                                                          
c                                                                      
        DO ILEV=1,NLEV                                                  
          DO KK = NCT_CRK+1, NCT_CRK+NC_CRK                             
            EN0 = CRKLVSET(ILEV)%ENR0(2,KK)                             
            IF (EN0 > 0) THEN                                           
              IF (FAC(EN0) > 0) RTMP0 = NC_CRK/FAC(EN0)                 
              IF (MS(N) > ZERO) THEN                                    
                RTMP = RTMP0 / MS(N)                                    
                CRKAVX(ILEV)%A(1,KK)  = CRKSKY(ILEV)%FSKY(1,KK) * RTMP  
                CRKAVX(ILEV)%A(2,KK)  = CRKSKY(ILEV)%FSKY(2,KK) * RTMP  
                CRKAVX(ILEV)%A(3,KK)  = CRKSKY(ILEV)%FSKY(3,KK) * RTMP  
              END IF                                                    
              IF (IN(N) > ZERO) THEN                                    
                RTMP = RTMP0 / IN(N)                                    
                CRKAVX(ILEV)%AR(1,KK) = CRKSKY(ILEV)%FSKY(4,KK) * RTMP  
                CRKAVX(ILEV)%AR(2,KK) = CRKSKY(ILEV)%FSKY(5,KK) * RTMP  
                CRKAVX(ILEV)%AR(3,KK) = CRKSKY(ILEV)%FSKY(6,KK) * RTMP  
              END IF                                                    
            END IF                                                      
          END DO                                                        
        END DO ! DO ILEV=1,NLEVMAX                                      
      END DO  ! N = NODFT,NODLT                                         
c-----------
      DEALLOCATE(FAC)
c-----------
      RETURN
      END
c
