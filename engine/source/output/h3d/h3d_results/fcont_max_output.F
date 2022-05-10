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
Chd|  FCONT_MAX_OUTPUT              source/output/h3d/h3d_results/fcont_max_output.F
Chd|-- called by -----------
Chd|        SORTIE_MAIN                   source/output/sortie_main.F   
Chd|-- calls ---------------
Chd|        SPMD_H3D_SUM_R_NODAL_21       source/output/h3d/spmd/spmd_h3d_gather_r_node.F
Chd|====================================================================
      SUBROUTINE FCONT_MAX_OUTPUT(CONT,FCONTG,NODGLOB,FCONT_MAX,WEIGHT)
C-----------------------------s------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "task_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "scr06_c.inc"
#include      "spmd_c.inc"
#include      "intstamp_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NODGLOB(*),WEIGHT(*)
      my_real
     .   CONT(3,*),FCONTG(3,*),FCONT_MAX(3,*)
      my_real  FOLD, FNEW, FMAX
      INTEGER I,K,N  
      my_real
     .       , DIMENSION(:,:), ALLOCATABLE :: FCONT_TMP,FCONT_TMP_P
C-----------------------------------------------

      IFCONTMAX = 1
      IF(NINTSTAMP == 0 .OR.NSPMD == 1) THEN
         DO N=1,NUMNOD
            IF(WEIGHT(N) == 1) THEN
             FNEW = CONT(1,N)**2 + CONT(2,N)**2 +CONT(3,N)**2
             FOLD = FCONT_MAX(1,N)**2 + FCONT_MAX(2,N)**2 +FCONT_MAX(3,N)**2
             FMAX = MAX(FNEW,FOLD)
             IF(FNEW > FOLD) THEN
                FCONT_MAX(1:3,N) = CONT(1:3,N)   
             ENDIF
            ELSE
             CONT(1:3,N) =  ZERO
            ENDIF
         ENDDO
      ELSE

         ALLOCATE(FCONT_TMP(3,NUMNOD)) 
         ALLOCATE(FCONT_TMP_P(3,NUMNODG)) 

         DO I=1,NUMNOD
           K=NODGLOB(I)
           FCONT_TMP(1,I) = CONT(1,I) + FCONTG(1,K)
           FCONT_TMP(2,I) = CONT(2,I) + FCONTG(2,K)
           FCONT_TMP(3,I) = CONT(3,I) + FCONTG(3,K)
         ENDDO

         IF(NSPMD > 1 ) THEN
             CALL SPMD_H3D_SUM_R_NODAL_21(NODGLOB,FCONT_TMP,3*NUMNOD,FCONT_TMP_P,3*NUMNODG,FCONTG)
         ENDIF

         IF(NSPMD > 1) THEN
            IF(ISPMD == 0 ) THEN
               DO N=1,NUMNODG
                  FNEW = FCONT_TMP_P(1,N)**2 + FCONT_TMP_P(2,N)**2 +FCONT_TMP_P(3,N)**2
                  FOLD = FCONT_MAX(1,N)**2 + FCONT_MAX(2,N)**2 +FCONT_MAX(3,N)**2
                  FMAX = MAX(FNEW,FOLD)
                  IF(FNEW > FOLD) THEN
                     FCONT_MAX(1:3,N) = FCONT_TMP_P(1:3,N)   
                  ENDIF
               ENDDO
             ENDIF
         ENDIF
         DEALLOCATE( FCONT_TMP_P, FCONT_TMP)
       ENDIF
 
       RETURN
       END 
