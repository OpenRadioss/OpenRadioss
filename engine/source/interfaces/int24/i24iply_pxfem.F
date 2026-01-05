Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2026 Altair Engineering Inc.
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
!||====================================================================
!||    i24iply_pxfem   ../engine/source/interfaces/int24/i24iply_pxfem.F
!||--- called by ------------------------------------------------------
!||    i24mainf        ../engine/source/interfaces/int24/i24main.F
!||--- uses       -----------------------------------------------------
!||    plyxfem_mod     ../engine/share/modules/plyxfem_mod.F
!||    tri7box         ../engine/share/modules/tri7box.F
!||====================================================================
      SUBROUTINE I24IPLY_PXFEM(
     1                  JLT      ,CAND_E  ,MSEGTYP , IX1,IX2     ,
     2                  IX3     ,IX4    ,PENE,MS_PLY,INOD_PXFEM  ,
     3                  IPLY    ,ITAB) 
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TRI7BOX
      USE PLYXFEM_MOD
C-------------------------------------------f----
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
#include      "comlock.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JLT, CAND_E(*),IX1(*), IX2(*), IX3(*), 
     .        IX4(*), INOD_PXFEM(*),
     .        IPLY(4,*),MSEGTYP(*),ITAB(*)
      my_real
     .     PENE(*),MS_PLY(NPLYXFE,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J1, NN, ILY, ISEGTYP(MVSIZ)
C --------------------------------------------------------------------- 
        DO I=1,JLT
          ISEGTYP(I) = MSEGTYP(CAND_E(I))
        ENDDO       
C--------------------------------------------------------
C
C for computing contact ply forces.
C
           DO I=1,JLT
C or by node segment if it needed            
             IPLY(1,I) = 0
             IPLY(2,I) = 0
             IPLY(3,I) = 0
             IPLY(4,I) = 0
             
             IF(PENE(I) == ZERO) CYCLE 
             IF(ISEGTYP(I) == ZERO) CYCLE 
              
             IF(ISEGTYP(I) > 0) THEN
              J1 = IX1(I)
              IF(J1 > 0) THEN
                NN = INOD_PXFEM(J1)                   
                IF(NN > 0 ) THEN 
                    ILY = NPLYMAX                     
                    DO WHILE(MS_PLY(NN,ILY) == ZERO)  
                     ILY = ILY - 1                    
                    ENDDO                             
                    IPLY(1,I) = ILY     
                  ENDIF  
               ENDIF    
              J1 = IX2(I)
              IF(J1 > 0) THEN
              NN = INOD_PXFEM(J1)
              IF(NN > 0) THEN
                  ILY = NPLYMAX
                  DO WHILE(MS_PLY(NN,ILY) == ZERO)
                     ILY = ILY - 1
                  ENDDO
                  IPLY(2,I) = ILY
              ENDIF
              ENDIF
              J1 = IX3(I)
              IF(J1 > 0)THEN
              NN = INOD_PXFEM(J1)
              IF( NN > 0) THEN
                  ILY = NPLYMAX
                  DO WHILE(MS_PLY(NN,ILY) == ZERO) 
                    ILY = ILY - 1
                  ENDDO    
                  IPLY(3,I) = ILY
              ENDIF
              ENDIF
              J1 = IX4(I)
              IF(J1 > 0) THEN
               NN = INOD_PXFEM(J1)
              IF( NN > 0)  THEN
                 ILY = NPLYMAX
                 DO WHILE(MS_PLY(NN,ILY) == ZERO)
                  ILY = ILY - 1
                 ENDDO  
                 IPLY(4,I) = ILY
              ENDIF  
              ENDIF
             ELSE ! 
               J1= IX1(I)
               IF(J1 > 0) THEN
               NN = INOD_PXFEM(J1)
               IF( NN > 0)  THEN
                    ILY = 1
                    DO WHILE(MS_PLY(NN,ILY) == ZERO)
                     ILY = ILY + 1
                    ENDDO
                    IPLY(1,I) = ILY
              ENDIF
              ENDIF
              J1= IX2(I)
              IF(J1 > 0)THEN
              NN = INOD_PXFEM(J1)
              IF(NN > 0) THEN 
               ILY = 1                           
               DO WHILE(MS_PLY(NN,ILY) == ZERO)  
                  ILY = ILY + 1                  
               ENDDO                             
               IPLY(2,I) = ILY                   
              ENDIF   
             ENDIF                             
C                
              J1= IX3(I) 
              IF(J1 > 0)THEN
              NN = INOD_PXFEM(J1)
              IF( NN > 0 ) THEN  
                ILY = 1
                DO WHILE(MS_PLY(NN,ILY) == ZERO)
                  ILY = ILY + 1
                ENDDO
                IPLY(3,I) = ILY
              ENDIF 
              ENDIF
C            
              J1= IX4(I)
              IF(J1> 0)THEN
              NN = INOD_PXFEM(J1)
              IF( NN > 0) THEN
                 ILY = 1
                 DO WHILE(MS_PLY(NN,ILY) == ZERO) 
                  ILY = ILY + 1
                 ENDDO
                 IPLY(4,I) = ILY
              ENDIF 
              ENDIF
            ENDIF  
         ENDDO                
C         
         RETURN
         END
