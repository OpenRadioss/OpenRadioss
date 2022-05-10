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
Chd|  ALEW                          source/ale/alew.F             
Chd|-- called by -----------
Chd|        ALEWDX                        source/ale/alewdx.F           
Chd|-- calls ---------------
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|        SPMD_WVOIS                    source/mpi/fluid/spmd_cfd.F   
Chd|        ALE_CONNECTIVITY_MOD          ../common_source/modules/ale_connectivity_mod.F
Chd|====================================================================
      SUBROUTINE ALEW(
     1     X         ,D      ,V       ,W       ,WA      ,
     2     ALE_NN_CONNECT     ,NALE   ,NODFT   ,NODLT   ,ITASK   ,
     3     NBRCVOIS,NBSDVOIS,LNRCVOIS ,LNSDVOIS,ITAB         )
      USE ALE_CONNECTIVITY_MOD
C-----------------------------------------------
C     D e s c r i p t i o n
C-----------------------------------------------
C     Compute Grid for /ALE/GRID/DONEA
C-----------------------------------------------
C     I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C     D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NALE(*), NODFT, NODLT, ITASK,
     .     NBRCVOIS(*),NBSDVOIS(*),
     .     LNRCVOIS(*),LNSDVOIS(*) ,ITAB(*) 
C     REAL ou REAL*8
      my_real
     .     X(3,*), D(3,*), V(3,*), W(3,*), WA(3,*)
      TYPE(t_connectivity), INTENT(IN) :: ALE_NN_CONNECT
C-----------------------------------------------
C     C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr05_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com07_c.inc"
#include      "com08_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C     L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER N, I, NCI, K, J, L, LENCOM
      my_real
     .     LIJ, XLAGR, FIX, FIY, FIZ, SLI, WIX, WIY, WIZ, FAC,
     .     LIJ2,LIJSQR
      INTEGER :: IAD1, IAD2
C-----------------------------------------------

      DO N = NODFT,NODLT
         WA(1,N)=W(1,N)
         WA(2,N)=W(2,N)
         WA(3,N)=W(3,N)
      ENDDO
C     
C     el BARRIER indispensable (WA voisins)
C     
      CALL MY_BARRIER()
C     
C     ECHANGE SUR NOEUDS VOISINS de X, D, WA (X est deja bon si INCOMP=0)
C     
      IF(NSPMD.GT.1)THEN
C     
C     Partie non parallele smt
C     
!$OMP SINGLE

         LENCOM=NBRCVOIS(NSPMD+1)+NBSDVOIS(NSPMD+1)
         CALL SPMD_WVOIS(
     1        X	  ,D	   ,WA    ,NBRCVOIS,NBSDVOIS,
     2        LNRCVOIS,LNSDVOIS,LENCOM)

C     Fin Partie non parallele smt
!$OMP END SINGLE

      END IF
C     
      DO N = NODFT,NODLT
         XLAGR=MIN(1,IABS(IABS(NALE(N))-2))
         W(1,N)=V(1,N)*XLAGR
         W(2,N)=V(2,N)*XLAGR
         W(3,N)=V(3,N)*XLAGR
      ENDDO
C     
      IF(ALPHA.EQ.ZERO) THEN
         DO I = NODFT,NODLT
            IF(NALE(I) .NE. 0) THEN
               NCI=0
               WIX=ZERO
               WIY=ZERO
               WIZ=ZERO
C     
               IAD1 = ALE_NN_CONNECT%IAD_CONNECT(I)
               IAD2 = ALE_NN_CONNECT%IAD_CONNECT(I + 1) - 1
               DO K = IAD1, IAD2
                  J = ALE_NN_CONNECT%CONNECTED(K)
                  IF (J > 0) THEN
                     NCI = NCI + 1
                     WIX = WIX + WA(1,J)
                     WIY = WIY + WA(2,J)
                     WIZ = WIZ + WA(3,J)
                  ENDIF
               ENDDO
C     
               W(1,I) = WIX / NCI
               W(2,I) = WIY / NCI
               W(3,I) = WIZ / NCI
            ENDIF
         ENDDO
C     
      ELSE
         DO I = NODFT,NODLT
            IF(NALE(I) .NE. 0) THEN
               NCI=0
               FIX=ZERO
               FIY=ZERO
               FIZ=ZERO
               SLI=ZERO
               WIX=ZERO
               WIY=ZERO
               WIZ=ZERO
               IAD1 = ALE_NN_CONNECT%IAD_CONNECT(I)
               IAD2 = ALE_NN_CONNECT%IAD_CONNECT(I + 1) - 1
               DO K = IAD1, IAD2
                  J = ALE_NN_CONNECT%CONNECTED(K)
                  IF (J > 0) THEN
                     NCI = NCI + 1
                     LIJ2= (X(1,J)-X(1,I))*(X(1,J)-X(1,I))
     .                    + (X(2,J)-X(2,I))*(X(2,J)-X(2,I))
     .                    + (X(3,J)-X(3,I))*(X(3,J)-X(3,I))
                     LIJSQR = SQRT(LIJ2)
                     LIJ=MAX(EM20,LIJSQR)
                     IF(LIJ.LT.EP20) THEN
                        SLI=SLI+LIJ
                        FIX=FIX+(D(1,J)-D(1,I))/LIJ
                        FIY=FIY+(D(2,J)-D(2,I))/LIJ
                        FIZ=FIZ+(D(3,J)-D(3,I))/LIJ
                     ENDIF
                     WIX=WIX+WA(1,J)
                     WIY=WIY+WA(2,J)
                     WIZ=WIZ+WA(3,J)
                  ENDIF
               ENDDO
C     
               FAC=ALPHA*SLI/(NCI*NCI*DT2)
               W(1,I) = WIX/NCI + FAC*FIX
               W(2,I) = WIY/NCI + FAC*FIY
               W(3,I) = WIZ/NCI + FAC*FIZ
C     
            ENDIF
         ENDDO
      ENDIF
C     
      IF(GAMMA.LT.QUATREVINGT19)THEN
         DO I = NODFT,NODLT
            IF(NALE(I) .NE. 0) THEN
               IF(V(1,I).NE.ZERO)
     .              W(1,I)=VGX*V(1,I)*
     .              MAX((UN-GAMMA), MIN((UN+GAMMA),W(1,I)/V(1,I)))
               IF(V(2,I).NE.ZERO)
     .              W(2,I)=VGY*V(2,I)*
     .              MAX((UN-GAMMA), MIN((UN+GAMMA),W(2,I)/V(2,I)))
               IF(V(3,I).NE.ZERO)
     .              W(3,I)=VGZ*V(3,I)*
     .              MAX((UN-GAMMA), MIN((UN+GAMMA),W(3,I)/V(3,I)))
            ENDIF
         ENDDO
      ENDIF
C     
      RETURN
      END 
