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
C  |         I17MAIN_TRI                       /int17/i17main_lag.F
Chd|====================================================================
Chd|  I17XSAVE                      source/interfaces/int17/i17xsave.F
Chd|-- called by -----------
Chd|        I17MAIN_TRI                   source/interfaces/int17/i17main_pena.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE I17XSAVE(
     1       X     ,V     ,A     ,IXS   ,IXS16 ,
     2       NMES  ,NME_T ,ESH_T ,NELEM ,NELES ,
     3       EMINXM,ITASK ,XSAV  ,XMIN  ,YMIN  ,
     4       ZMIN  ,XMAX  ,YMAX  ,ZMAX  )
C sauvegarde des XSAV et calcul borne domaine
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NMES,NME_T,ESH_T,NMN,ITASK,
     .        NELEM(*),NELES(*),IXS(NIXS,*),IXS16(8,*)
      my_real
     .        XMAX, YMAX, ZMAX, XMIN, YMIN, ZMIN,
     .        X(3,*), V(3,*), A(3,*), EMINXM(6,*), XSAV(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NMES_F, NMES_L, I, J, K
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
C
      NMES_F = 1 + ITASK*NMES / NTHREAD
      NMES_L = (ITASK+1)*NMES / NTHREAD
C
      XMIN=EP30
      XMAX=-EP30
      YMIN=EP30
      YMAX=-EP30
      ZMIN=EP30
      ZMAX=-EP30
C
      DO K=1,8
        DO I=1+ESH_T,NME_T+ESH_T                      
          J=IXS(K+1,NELEM(I))
          XSAV(1,J) = X(1,J)+DT2*(V(1,J)+DT12*A(1,J))
          XSAV(2,J) = X(2,J)+DT2*(V(2,J)+DT12*A(2,J))
          XSAV(3,J) = X(3,J)+DT2*(V(3,J)+DT12*A(3,J))
          J=IXS16(K,NELEM(I)-NUMELS8-NUMELS10-NUMELS20)
          XSAV(1,J) = X(1,J)+DT2*(V(1,J)+DT12*A(1,J))
          XSAV(2,J) = X(2,J)+DT2*(V(2,J)+DT12*A(2,J))
          XSAV(3,J) = X(3,J)+DT2*(V(3,J)+DT12*A(3,J))
C
          XMIN = MIN( XMIN , EMINXM(1,I) )
          YMIN = MIN( YMIN , EMINXM(2,I) )
          ZMIN = MIN( ZMIN , EMINXM(3,I) )
          XMAX = MAX( XMAX , EMINXM(4,I) )
          YMAX = MAX( YMAX , EMINXM(5,I) )
          ZMAX = MAX( ZMAX , EMINXM(6,I) )
        ENDDO
        DO I=NMES_F,NMES_L
          J=IXS(K+1,NELES(I))
          XSAV(1,J) = X(1,J)+DT2*(V(1,J)+DT12*A(1,J))
          XSAV(2,J) = X(2,J)+DT2*(V(2,J)+DT12*A(2,J))
          XSAV(3,J) = X(3,J)+DT2*(V(3,J)+DT12*A(3,J))
          J=IXS16(K,NELES(I)-NUMELS8-NUMELS10-NUMELS20)
          XSAV(1,J) = X(1,J)+DT2*(V(1,J)+DT12*A(1,J))
          XSAV(2,J) = X(2,J)+DT2*(V(2,J)+DT12*A(2,J))
          XSAV(3,J) = X(3,J)+DT2*(V(3,J)+DT12*A(3,J))
        ENDDO
      ENDDO
C
      RETURN
      END
