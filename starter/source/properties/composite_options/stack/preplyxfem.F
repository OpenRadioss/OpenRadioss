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
Chd|  PREPLYXFEM                    source/properties/composite_options/stack/preplyxfem.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE PREPLYXFEM(
     1      MS_PLY0,ZI_PLY0,IEL, INOD , IXC ,MS_PLY,ZI_PLY,ADDCNE ,
     2      MSZ20,MSZ2)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  IXC(NIXC,*), ADDCNE(0:*),IEL(*),INOD(*)
      my_real 
     .        MS_PLY0(NUMNOD,*),  ZI_PLY0(NUMNOD,*),
     .        MS_PLY(NPLYXFE,*),  ZI_PLY(NPLYXFE,*),
     .        MSZ20(*),MSZ2(*) 
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,II,K,N1,J,N
      my_real
     .  MSZ
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
         ADDCNE(0)=0
         ADDCNE(1)= 0
         DO I=1,NUMNOD
            II = INOD(I)
            ADDCNE(I + 1) = 0
            MSZ = ZERO
            IF( II > 0 )THEN
              MSZ2(II) = ZERO
              DO J=1,NPLYMAX
                 MS_PLY(II,J) = MS_PLY0(I,J)
                 ZI_PLY(II,J) = ZI_PLY0(I,J)
Celementary compute
cc                 MSZ2(II) = MSZ20(I)  
c nodale compute                 
                 MSZ2(II) = MSZ2(II) + 
     .                      MS_PLY(II,J)*ZI_PLY(II,J)*ZI_PLY(II,J)
              ENDDO
               IF(MSZ2(II) == ZERO) MSZ2(II) = EP30
            ENDIF
         ENDDO    
C
         DO  I=1,NUMELC
            IF(IEL(I) > 0 ) THEN
              DO  K=2,5
                N = INOD(IXC(K,I)) + 1
                ADDCNE(N) = ADDCNE(N) + 1
              ENDDO
            ENDIF
          ENDDO 
          ADDCNE(1) = 1
          DO I=2,NPLYXFE + 1
             ADDCNE(I) = ADDCNE(I) + ADDCNE(I-1)
          END DO        
C          
          RETURN
          END
Chd|====================================================================
Chd|  FILLCNE_PXFEM                 source/properties/composite_options/stack/preplyxfem.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        MY_ORDERS                     ../common_source/tools/sort/my_orders.c
Chd|====================================================================
      SUBROUTINE FILLCNE_PXFEM(
     1   IEL , INOD, IXC,CEP, ADDCNE, CNE,CEL   )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IXC(NIXC,*),ADDCNE(0:*), CNE(*),IEL(*),INOD(*),
     .        CEP(*),CEL(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, K, N, ADSKY(0:NPLYXFE+1),N1,PROC,NG,
     .        INDX, INDEX(NUMELC),P,II,NIN,INDEXC(2*NUMELC),
     .        WORK(70000),ITRI(NUMELC)
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
C   CALCUL DE CNE ADDCNE 
C-----------------------------------------------
      DO I = 0, NPLYXFE + 1
        ADSKY(I) = ADDCNE(I)
      ENDDO
C
C
C tri des elements locaux suivants num user
C
      DO I = 1, NUMELC
        ITRI(I) = IXC(7,I)
      ENDDO
      CALL MY_ORDERS(0,WORK,ITRI,INDEXC,NUMELC,1)
      
      INDX = 0
      DO J=1,NUMELC
        I = INDEXC(J)
!!      DO   I = 1,NUMELC
       IF(IEL(I) > 0 )THEN
         INDX = INDX + 1
         DO K=1,4
           N = IXC(K+1,I)
           N1 = INOD(N)
           CNE(ADSKY(N1)) = I
           ADSKY(N1) = ADSKY(N1) + 1
         ENDDO
         INDEX(INDX) = I
       ENDIF 
      ENDDO
C     
      DO PROC = 1, NSPMD
        NIN = 0  
!!        DO I=1,INDX
          DO II=1,NUMELC
!!           II = INDEX(I)
           NG = II +  NUMELS + NUMELQ
           P = CEP(NG) + 1 
           IF (P == PROC) THEN
            J = IEL(II) 
            IF(J > 0) THEN 
              NIN = NIN + 1
              CEL(J) = NIN
            ENDIF  
           ENDIF
        ENDDO 
      ENDDO  
C      
      RETURN
      END
