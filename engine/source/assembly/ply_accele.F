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
Chd|  PLY_ACCELE                    source/assembly/ply_accele.F  
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        PLYA                          source/assembly/ply_accele.F  
Chd|        PLYXFEM_MOD                   share/modules/plyxfem_mod.F   
Chd|====================================================================
      SUBROUTINE PLY_ACCELE(INOD,MS_LAYER,ZI_LAYER,MS,NODFT,NODLT,
     .                     NPLYMAX,NPLYXFE,NDDIM,MSZ2 )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE PLYXFEM_MOD
C----6---------------------------------------------------------------7---------8
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com08_c.inc"
#include      "com06_c.inc"
#include      "comlock.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NODFT,NODLT,NDDIM,NPLYMAX,NPLYXFE,INOD(*)
C     REAL
      my_real
     .   MS_LAYER(NPLYXFE,*), ZI_LAYER(NPLYXFE,*),
     .   MS(*),MSZ2(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  
     .       J,K,N,NN,NPT
      my_real
     .         RTMP, FN1,FN2,FN3,FAC,MSI,FM1,FM2,FM3,INER,
     .         MASS,ALPHA(NDDIM),F1,F3,F2,MDT1,MDT2,MDT3,
     .         FPLY, MPLY,DF1,DF2,DF3,KZI,MASS_NODE,BETA
C----------------------------------------------------     
C 
       ALPHA = ONE 
       IF(IPLYXFEM == 2 ) THEN 
#include "vectorize.inc" 
          DO NN=NODFT,NODLT
            N = INOD(NN)
            F1 = ZERO
            F2 = ZERO
            F3 = ZERO
            MDT1 = ZERO
            MDT2 = ZERO
            MDT3 = ZERO
            IF(N > 0) THEN 
             NPT = 0
C
             MSZ2(N)=ZERO
             KZI    =ZERO
             DO J=1,NPLYMAX
              IF(MS_LAYER(N,J) > ZERO) THEN 
                  MSZ2(N) = MSZ2(N) + 
     .                       MS_LAYER(N,J)*ZI_LAYER(N,J)*ZI_LAYER(N,J)
                  KZI=KZI+PLY(J)%A(4,N)*ZI_LAYER(N,J)
              END IF
             END DO
             KZI=KZI/MSZ2(N)
C          
             MASS_NODE = ZERO
             DO J=1,NPLYMAX
                IF(MS_LAYER(N,J) > ZERO) THEN 
                   MASS_NODE  = MASS_NODE + MS_LAYER(N,J)
                ENDIF
             ENDDO
             DO J=1,NPLYMAX
                IF(MS_LAYER(N,J) > ZERO) THEN 
                   BETA = MS(NN)/MASS_NODE
                   FAC = ONE/(DTFACX*DTFACX)
                   MASS =FAC*DT2*DT2*PLY(J)%A(4,N)
clm        .                             +KZI*MS_LAYER(N,J)*ABS(ZI_LAYER(N,J)))
                   ALPHA(NN) = MAX(ALPHA(NN), MASS/MS_LAYER(N,J)/BETA) 
C                  
                  MDT1 = MDT1 + PLY(J)%A(1,N)*ZI_LAYER(N,J)
                  MDT2 = MDT2 + PLY(J)%A(2,N)*ZI_LAYER(N,J)
                  MDT3 = MDT3 + PLY(J)%A(3,N)*ZI_LAYER(N,J)
C
                  F1 = F1 + PLY(J)%A(1,N)
                  F2 = F2 + PLY(J)%A(2,N)
                  F3 = F3 + PLY(J)%A(3,N)
C for fail of ply ! is desactivated for checking                 
!to be checked                   IF(PLY(J)%A(4,N) == ZERO)THEN
!to be checked                   PLY(J)%A(4,N) = EP30
!to be checked                   PLY(J)%ITAG(N) = 1
!to be checked                  ENDIF 
C                               
                ENDIF
             ENDDO
C corre   ction des forces par plies
               DO J=1,NPLYMAX
                IF(MS_LAYER(N,J) > ZERO) THEN  
                  BETA  =  MS(NN)/MASS_NODE 
                  FPLY  =  BETA*MS_LAYER(N,J)/MS(NN)
                  MPLY  = ZI_LAYER(N,J)*MS_LAYER(N,J)/MSZ2(N)
C
                  PLY(J)%A(1,N)= PLY(J)%A(1,N) -  F1*FPLY 
                  PLY(J)%A(2,N)= PLY(J)%A(2,N) -  F2*FPLY 
                  PLY(J)%A(3,N)= PLY(J)%A(3,N) -  F3*FPLY                
                ENDIF    
               ENDDO      
             ENDIF
           ENDDO   
       ELSE !old formulation 
#include "vectorize.inc" 
          DO NN=NODFT,NODLT
            N = INOD(NN)
            F1 = ZERO
            F2 = ZERO
            F3 = ZERO
            MDT1 = ZERO
            MDT2 = ZERO
            MDT3 = ZERO
            IF(N > 0) THEN 
             NPT = 0
C
             MSZ2(N)=ZERO
             KZI    =ZERO
             DO J=1,NPLYMAX
              IF(MS_LAYER(N,J) > ZERO) THEN 
                  MSZ2(N) = MSZ2(N) + 
     .                       MS_LAYER(N,J)*ZI_LAYER(N,J)*ZI_LAYER(N,J)
                  KZI=KZI+PLY(J)%A(4,N)*ZI_LAYER(N,J)
              END IF
             END DO
             KZI=KZI/MSZ2(N)
C
             DO J=1,NPLYMAX
                IF(MS_LAYER(N,J) > ZERO) THEN 
                   FAC = ONE/(DTFACX*DTFACX)
                   MASS =FAC*DT2*DT2*PLY(J)%A(4,N)
clm        .                             +KZI*MS_LAYER(N,J)*ABS(ZI_LAYER(N,J)))
                   ALPHA(NN) = MAX(ALPHA(NN), MASS/MS_LAYER(N,J) ) 
C                  
                  MDT1 = MDT1 + PLY(J)%A(1,N)*ZI_LAYER(N,J)
                  MDT2 = MDT2 + PLY(J)%A(2,N)*ZI_LAYER(N,J)
                  MDT3 = MDT3 + PLY(J)%A(3,N)*ZI_LAYER(N,J)
C
                  F1 = F1 + PLY(J)%A(1,N)
                  F2 = F2 + PLY(J)%A(2,N)
                  F3 = F3 + PLY(J)%A(3,N)
C is desactivated for checking                 
!to be checked                  IF(PLY(J)%A(4,N) == ZERO)THEN
!to be checked                     PLY(J)%A(4,N) = EP30
!to be checked                     PLY(J)%ITAG(N) = 1
!to be checked                  ENDIF 
C                               
                ENDIF
             ENDDO
C corre   ction des forces par plies
               DO J=1,NPLYMAX
                IF(MS_LAYER(N,J) > ZERO) THEN 
                  FPLY  = MS_LAYER(N,J)/MS(NN)
                  MPLY  = ZI_LAYER(N,J)*MS_LAYER(N,J)/MSZ2(N)
                  PLY(J)%A(1,N)= PLY(J)%A(1,N) -  F1*FPLY - MPLY*MDT1 
                  PLY(J)%A(2,N)= PLY(J)%A(2,N) -  F2*FPLY - MPLY*MDT2
                  PLY(J)%A(3,N)= PLY(J)%A(3,N) -  F3*FPLY - MPLY*MDT3
                ENDIF    
               ENDDO      
             ENDIF
           ENDDO
        ENDIF ! formulation  
C     
        DO J=1,NPLYMAX 
          CALL PLYA(J,NODFT,NODLT,NPLYXFE, INOD,ALPHA,
     .              MS_LAYER, PLY(J)%A(1,1))
        ENDDO    
C--------------------------------------------
      RETURN
      END
Chd|====================================================================
Chd|  PLYA                          source/assembly/ply_accele.F  
Chd|-- called by -----------
Chd|        PLY_ACCELE                    source/assembly/ply_accele.F  
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE PLYA(J,NODFT,NODLT,NPLYXFE,INOD,ALPHA,MS,A)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
C----6---------------------------------------------------------------7---------8
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NODFT,NODLT,NPLYXFE,INOD(*),J
C     REAL
      my_real
     .   MS(NPLYXFE,*),A(4,*),ALPHA(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  K,N,NN
      my_real
     .         RTMP, FN1,FN2,FN3,FAC,MSI,FM1,FM2,FM3,INER,
     .         MASS,F1,F2,F3
C----------------------------------------------------     
C      
#include "vectorize.inc" 
      DO NN=NODFT,NODLT
         N = INOD(NN)          
         IF(N > 0) THEN   
            IF(MS(N,J) > ZERO) THEN                         
                  MASS = ALPHA(NN)*MS(N,J)
                  RTMP = ONE / MASS 
                  A(1,N) = A(1,N)  * RTMP 
                  A(2,N) = A(2,N)  * RTMP  
                  A(3,N) = A(3,N)  * RTMP
              ELSE                                                       
                  A(1,N) = ZERO                                 
                  A(2,N) = ZERO                                 
                  A(3,N) = ZERO                                  
                  A(4,N) = ZERO                 
               ENDIF
         ENDIF                                                           
      ENDDO
C                                    
C--------------------------------------------
      RETURN
      END
