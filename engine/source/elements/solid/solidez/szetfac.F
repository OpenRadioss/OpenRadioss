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
Chd|  SZETFAC                       source/elements/solid/solidez/szhour3.F
Chd|-- called by -----------
Chd|        SZHOUR3                       source/elements/solid/solidez/szhour3.F
Chd|        SZHOUR3_OR                    source/elements/solid/solidez/szhour3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SZETFAC(LFT,LLT,IKT,MTN,ET,G  )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER LFT,LLT,IKT,MTN
      my_real
     .   ET(*),G(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J,K
      my_real
     .   FAC_L,FAC_U,FAC(MVSIZ),ETE,FAC_MIN,FAC_MAX
C-------IKT=2 for the moment----------
      FAC_L=FIVEEM2
      DO I=LFT,LLT
        FAC(I) = ONE
      END DO

      IF (IKT == 2 ) THEN
       SELECT CASE (MTN)
        CASE (2,36) 
        
        CASE (42,62,69,82,88,92,94,95,111)                                          
c          FAC_MIN=EP10
c          FAC_MAX=ZERO
         DO I=LFT,LLT
          IF (ET(I)<=ONE) THEN
           FAC(I)=MAX(FAC_L,ET(I))
          ELSE
C
            FAC(I)= ZEP2+ET(I)
          END IF
c           IF (FAC_MIN>FAC(I)) FAC_MIN=FAC(I)
c           IF (FAC_MAX<FAC(I)) FAC_MAX=FAC(I)
         END DO           
        CASE(38,70,90)
          FAC_L=EM02
c          FAC_MIN=EP10
c          FAC_MAX=ZERO
         DO I=LFT,LLT
          IF (ET(I) <= ONE) THEN
           FAC(I)=MAX(FAC_L,ET(I))
          ELSE
C-----    -G(i) is already calculated with EMAX          
           FAC(I)=ONE
          END IF         
c           IF (FAC_MIN>FAC(I)) FAC_MIN=FAC(I)
c           IF (FAC_MAX<FAC(I)) FAC_MAX=FAC(I)
         END DO 
c          write(iout,*),'FAC_L,FAC(1),ET(1)=',FAC_L,FAC(1),ET(1)
        CASE (71) 
         FAC_L=EM02
         DO I=LFT,LLT
          IF (ET(I)<=ONE) THEN
           FAC(I)=MAX(FAC_L,ET(I))
          END IF
         END DO 
       END SELECT
C-------IKT>2 hide option :limited by IKT ----------
      ELSE
C---------    
        FAC_U=ONE*IKT
        SELECT CASE (MTN)
         CASE (2,36) 
        
         CASE (42,62,69,82,88,92,94,111)                                          
          DO I=LFT,LLT
           IF (ET(I)<=ONE) THEN
            FAC(I)=MAX(FAC_L,ET(I))
           ELSE
            FAC(I)=MIN(FAC_U,ET(I))
           END IF
          ENDDO           
        CASE(38,70,90)  
          FAC_L=FAC_L/IKT
          DO I=LFT,LLT
           IF (ET(I)<=ONE) THEN
            FAC(I)=MAX(FAC_L,ET(I))
           ELSE
            FAC(I)=MIN(FAC_U,ET(I))
           END IF
          ENDDO 
        END SELECT
      END IF !(IKT ==2 THEN
C       
        DO I=LFT,LLT
         G(I)=FAC(I)*G(I)
        ENDDO
C
      RETURN
      END
