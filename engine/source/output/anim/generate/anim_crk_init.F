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
Chd|  ANIM_XFE_INIT                 source/output/anim/generate/anim_crk_init.F
Chd|-- called by -----------
Chd|        RESOL_INIT                    source/engine/resol_init.F    
Chd|-- calls ---------------
Chd|        CRACKXFEM_MOD                 share/modules/crackxfem_mod.F 
Chd|====================================================================
      SUBROUTINE ANIM_XFE_INIT(IXC,IXTG,INOD_CRK,IEL_CRK,
     .                         IADC_CRK,IADTG_CRK)
C-----------------------------------------------
C   M o d u l e s
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
#include      "param_c.inc"
#include      "spmd_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IXC(NIXC,*),IXTG(NIXTG,*),INOD_CRK(*),
     .        IEL_CRK(*),IADC_CRK(4,*),IADTG_CRK(3,*)
C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
      INTEGER N,NN,I,II,J,ILEV,NCOUNT,IADC(4)
C=======================================================================
      DO ILEV=1,NLEVMAX
        NCOUNT = 0
c
        DO I=1,NUMELC
          II = IEL_CRK(I)        ! n  systeme xfem
          IF (II > 0) THEN
            DO J=1,4
              N  = IXC(J+1,I)
              NN = INOD_CRK(N)   ! n  systeme xfem
              IF (NN > 0) THEN
                NCOUNT  = NCOUNT  + 1
                IADC(J) = IADC_CRK(J,II)   ! adresse sky nodal xfem
                CRKNOD(ILEV)%NOD2IAD(NCOUNT) = IADC(J)
              ENDIF
            ENDDO
         ENDIF
        ENDDO
c
        DO I=1,NUMELTG
          II = IEL_CRK(I+NUMELC)
          IF (II > 0) THEN
            DO J=1,3
              N  = IXTG(J+1,I)
              NN = INOD_CRK(N)
              IADC(J) = IADTG_CRK(J,II)
              IF (NN > 0) THEN
                NCOUNT  = NCOUNT  + 1
                CRKNOD(ILEV)%NOD2IAD(NCOUNT) = IADC(J)
              ENDIF
            ENDDO
c           ! need one more additional node to reconstruct triangle cut elem
            NCOUNT  = NCOUNT  + 1
            CRKNOD(ILEV)%NOD2IAD(NCOUNT) = IADC(3)
          ENDIF
        ENDDO
      ENDDO      !  DO ILEV=1,NLEVMAX
c-----------
      RETURN
      END
c
