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
Chd|  BCUMU2                        source/ale/bimat/bcumu2.F     
Chd|-- called by -----------
Chd|        BFORC2                        source/ale/bimat/bforc2.F     
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE BCUMU2(ALPH,ALPHC,E,FILL,
     .                  SIG,EINT,RHO,BULK,BULKT,
     .                  SIGT,EINTT,RHOT,TEMPT,TEMP,
     .                  PLAST,PLAS,BFRACT,BFRAC,
     .                  F11, F12, F21, F22, AX1,AX2,
     .                  T11,T12,T13,T14,T21,T22,T23,T24, 
     .                  B11,B12,B13,B14,B21,B22,B23,B24,
     .                  NC1,NC2,NC3,NC4,STI,STIFN,
     .                  L_TEMP, L_PLA,L_BFRAC, L_BULK,NEL)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NC1(*), NC2(*), NC3(*), NC4(*), L_TEMP, L_PLA, L_BFRAC,L_BULK,NEL
      my_real
     .   ALPH(*), ALPHC(*), E(3,*)   , FILL(*)  , SIG(NEL,6), EINT(*) ,
     .   RHO(*) , BULK(*) , SIGT(NEL,6), EINTT(*) , RHOT(*) , TEMPT(*),
     .   TEMP(*), PLAST(*), PLAS(*)  , BFRACT(*), BFRAC(*), BULKT(*)
      my_real
     .   B11(*), B12(*), B13(*), B14(*),
     .   B21(*), B22(*), B23(*), B24(*)
      my_real
     .   F11(*), F12(*), F21(*), F22(*), AX1(*), AX2(*),
     .   T11(*), T12(*), T13(*), T14(*),
     .   T21(*), T22(*), T23(*), T24(*),STI(*),STIFN(*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J
      my_real
     .   ALPHT, ALPHX     
C-----------------------------------------------
      IF(JALE+JEUL==0.OR.MTN==11) THEN
      DO I=LFT,LLT
        B11(I)=ZERO
        B12(I)=ZERO
        B13(I)=ZERO
        B14(I)=ZERO
        B21(I)=ZERO
        B22(I)=ZERO
        B23(I)=ZERO
        B24(I)=ZERO
      ENDDO
      ENDIF
C-------------------------
C     ASSEMBLE
C-------------------------
       DO I=LFT,LLT
         ALPHT=ONE
         ALPHX=ALPH(I)
         E(2,NC1(I))   = E(2,NC1(I))-AX1(I)+ALPHX*(-T11(I)-F11(I))-ALPHT*B11(I)
         E(3,NC1(I))   = E(3,NC1(I))+AX2(I)+ALPHX*(-T21(I)-F21(I))-ALPHT*B21(I)
         STIFN(NC1(I)) = STIFN(NC1(I))+STI(I)
         E(2,NC2(I))   = E(2,NC2(I))-AX1(I)+ALPHX*(-T12(I)-F12(I))-ALPHT*B12(I)
         E(3,NC2(I))   = E(3,NC2(I))+AX2(I)+ALPHX*(-T22(I)-F22(I))-ALPHT*B22(I)
         STIFN(NC2(I)) = STIFN(NC2(I))+STI(I)
         E(2,NC3(I))   = E(2,NC3(I))-AX1(I)+ALPHX*(-T13(I)+F11(I))-ALPHT*B13(I)
         E(3,NC3(I))   = E(3,NC3(I))+AX2(I)+ALPHX*(-T23(I)+F21(I))-ALPHT*B23(I)
         STIFN(NC3(I)) = STIFN(NC3(I))+STI(I)
         E(2,NC4(I))   = E(2,NC4(I))-AX1(I)+ALPHX*(-T14(I)+F12(I))-ALPHT*B14(I)
         E(3,NC4(I))   = E(3,NC4(I))+AX2(I)+ALPHX*(-T24(I)+F22(I))-ALPHT*B24(I)
         STIFN(NC4(I)) = STIFN(NC4(I))+STI(I)
       ENDDO

      DO I=LFT,LLT
        IF(ALPH(I)==ZERO)THEN
         DO J=1,6
           SIG(I,J)= ZERO
         ENDDO
         EINT(I)   = ZERO
         RHO(I)    = ZERO
         BULK(I)   = ZERO
        ENDIF
      ENDDO

      DO I=LFT,LLT
       DO J=1,6
         SIGT(I,J)=SIGT(I,J)+SIG(I,J)*ALPH(I)
       ENDDO
       EINTT(I)=EINTT(I)+EINT(I)*ALPH(I)
       RHOT(I) =RHOT(I)+RHO(I)*ALPH(I)
      ENDDO

      IF(L_TEMP>0)THEN
        DO I=LFT,LLT
         TEMPT(I)=TEMPT(I)+TEMP(I)*ALPH(I)
        ENDDO
      ENDIF
      
      IF(L_PLA>0)THEN
        DO I=LFT,LLT
         PLAST(I)=PLAST(I)+PLAS(I)*ALPH(I)
        ENDDO
      ENDIF      
      
      IF(L_BFRAC>0)THEN
        DO I=LFT,LLT
         BFRACT(I)=BFRACT(I)+BFRAC(I)*ALPH(I)
        ENDDO
      ENDIF   
      
      IF(L_BULK>0)THEN
        DO I=LFT,LLT
         BULKT(I)=BULKT(I)+BULK(I)*ALPH(I)
        ENDDO
      ENDIF             

      RETURN
      END

Chd|====================================================================
Chd|  BCUMU2P                       source/ale/bimat/bcumu2.F     
Chd|-- called by -----------
Chd|        BFORC2                        source/ale/bimat/bforc2.F     
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE BCUMU2P(ALPH,ALPHC,FILL,
     .                   SIG,EINT,RHO,BULK,BULKT,
     .                   SIGT,EINTT,RHOT,TEMPT,TEMP,
     .                   PLAST,PLAS,BFRACT,BFRAC,
     .                   F11, F12, F21, F22, AX1,AX2,
     .                   T11,T12,T13,T14,T21,T22,T23,T24, 
     .                   B11,B12,B13,B14,B21,B22,B23,B24,
     .                   FSKY,FSKYV,IADQ,STI,NC1,NC2,NC3,NC4,
     .                   L_TEMP, L_PLA, L_BFRAC,L_BULK,NEL)
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
#include      "parit_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IADQ(4,*),NC1(*),NC2(*),NC3(*),NC4(*),L_TEMP, L_PLA,L_BFRAC,
     .   L_BULK,NEL
      my_real
     .   ALPH(*), ALPHC(*), FILL(*), SIG(NEL,6), EINT(*),
     .   RHO(*), BULK(*), SIGT(NEL,6), EINTT(*), RHOT(*),TEMPT(*),
     .   TEMP(*),PLAST(*), PLAS(*), BFRACT(*), BFRAC(*), BULKT(*)
      my_real
     .   B11(*), B12(*), B13(*), B14(*),
     .   B21(*), B22(*), B23(*), B24(*)
      my_real
     .   F11(*), F12(*), F21(*), F22(*), AX1(*), AX2(*),
     .   T11(*), T12(*), T13(*), T14(*),
     .   T21(*), T22(*), T23(*), T24(*),
     .   FSKYV(LSKY,8),FSKY(8,LSKY),STI(*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, II, K
      my_real
     .   ALPHT, ALPHX     
C-----------------------------------------------
      IF(JALE+JEUL==0.OR.MTN==11) THEN
      DO I=LFT,LLT
        B11(I)=ZERO
        B12(I)=ZERO
        B13(I)=ZERO
        B14(I)=ZERO
        B21(I)=ZERO
        B22(I)=ZERO
        B23(I)=ZERO
        B24(I)=ZERO
      ENDDO
      ENDIF
C-------------------------
C     ASSEMBLE
C-------------------------
      IF(IVECTOR==1) THEN
        DO I=LFT,LLT
          ALPHT=ONE
          ALPHX=ALPH(I)
          II=I+NFT
          K = IADQ(1,II)
          FSKYV(K,2)=-AX1(I)+ALPHX*(-T11(I)-F11(I))-ALPHT*B11(I)
          FSKYV(K,3)=+AX2(I)+ALPHX*(-T21(I)-F21(I))-ALPHT*B21(I)
          FSKYV(K,7)=STI(I)

          K = IADQ(2,II)
          FSKYV(K,2)=-AX1(I)+ALPHX*(-T12(I)-F12(I))-ALPHT*B12(I)
          FSKYV(K,3)=+AX2(I)+ALPHX*(-T22(I)-F22(I))-ALPHT*B22(I)
          FSKYV(K,7)=STI(I)

          K = IADQ(3,II)
          FSKYV(K,2)=-AX1(I)+ALPHX*(-T13(I)+F11(I))-ALPHT*B13(I)
          FSKYV(K,3)=+AX2(I)+ALPHX*(-T23(I)+F21(I))-ALPHT*B23(I)
          FSKYV(K,7)=STI(I)

          K = IADQ(4,II)
          FSKYV(K,2)=-AX1(I)+ALPHX*(-T14(I)+F12(I))-ALPHT*B14(I)
          FSKYV(K,3)=+AX2(I)+ALPHX*(-T24(I)+F22(I))-ALPHT*B24(I)
          FSKYV(K,7)=STI(I)
        ENDDO
      ELSE
        DO I=LFT,LLT
          ALPHT=ONE
          ALPHX=ALPH(I)
          II=I+NFT
          K = IADQ(1,II)
          FSKY(2,K)=-AX1(I)+ALPHX*(-T11(I)-F11(I))-ALPHT*B11(I)
          FSKY(3,K)=+AX2(I)+ALPHX*(-T21(I)-F21(I))-ALPHT*B21(I)
          FSKY(7,K)=STI(I)

          K = IADQ(2,II)
          FSKY(2,K)=-AX1(I)+ALPHX*(-T12(I)-F12(I))-ALPHT*B12(I)
          FSKY(3,K)=+AX2(I)+ALPHX*(-T22(I)-F22(I))-ALPHT*B22(I)
          FSKY(7,K)=STI(I)

          K = IADQ(3,II)
          FSKY(2,K)=-AX1(I)+ALPHX*(-T13(I)+F11(I))-ALPHT*B13(I)
          FSKY(3,K)=+AX2(I)+ALPHX*(-T23(I)+F21(I))-ALPHT*B23(I)
          FSKY(7,K)=STI(I)

          K = IADQ(4,II)
          FSKY(2,K)=-AX1(I)+ALPHX*(-T14(I)+F12(I))-ALPHT*B14(I)
          FSKY(3,K)=+AX2(I)+ALPHX*(-T24(I)+F22(I))-ALPHT*B24(I)
          FSKY(7,K)=STI(I)
        ENDDO
      ENDIF

      DO I=LFT,LLT
        IF(ALPH(I)==ZERO)THEN
         DO J=1,6
           SIG(I,J)=ZERO
         ENDDO
         EINT(I)=ZERO
         RHO(I) =ZERO
         BULK(I)=ZERO
        ENDIF
      ENDDO

      DO I=LFT,LLT
       DO J=1,6
         SIGT(I,J)=SIGT(I,J)+SIG(I,J)*ALPH(I)
       ENDDO
       EINTT(I)=EINTT(I)+EINT(I)*ALPH(I)
       RHOT(I) =RHOT(I)+RHO(I)*ALPH(I)
      ENDDO

      IF(L_TEMP>0)THEN
        DO I=LFT,LLT
         TEMPT(I)=TEMPT(I)+TEMP(I)*ALPH(I)
        ENDDO
      ENDIF

      IF(L_PLA>0)THEN
        DO I=LFT,LLT
         PLAST(I)=PLAST(I)+PLAS(I)*ALPH(I)
        ENDDO
      ENDIF  
      
      IF(L_BFRAC>0)THEN
        DO I=LFT,LLT
         BFRACT(I)=BFRACT(I)+BFRAC(I)*ALPH(I)
        ENDDO
      ENDIF   
      
      IF(L_BULK>0)THEN
        DO I=LFT,LLT
         BULKT(I)=BULKT(I)+BULK(I)*ALPH(I)
        ENDDO
      ENDIF                   

      RETURN
      END

Chd|====================================================================
Chd|  BCUMU2PA                      source/ale/bimat/bcumu2.F     
Chd|-- called by -----------
Chd|        BFORC2                        source/ale/bimat/bforc2.F     
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE BCUMU2PA(ALPH,ALPHC,FILL,
     .                    SIG,EINT,RHO,BULK,BULKT,
     .                    SIGT,EINTT,RHOT,TEMPT,TEMP,
     .                    PLAST,PLAS,BFRACT,BFRAC,
     .                    F11, F12, F21, F22, AX1,AX2,
     .                    T11,T12,T13,T14,T21,T22,T23,T24, 
     .                    B11,B12,B13,B14,B21,B22,B23,B24,
     .                    FSKY,FSKYV,IADQ,STI,NC1,NC2,NC3,NC4,
     .                    L_TEMP, L_PLA,L_BFRAC,L_BULK,NEL)
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
#include      "parit_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IADQ(4,*),NC1(*),NC2(*),NC3(*),NC4(*),L_TEMP,L_PLA,L_BFRAC,
     .   L_BULK,NEL
      my_real
     .   ALPH(*), ALPHC(*), FILL(*), SIG(NEL,6), EINT(*),
     .   RHO(*), BULK(*), SIGT(NEL,6), EINTT(*), RHOT(*),TEMPT(*),
     .   TEMP(*), PLAST(*), PLAS(*), BFRACT(*), BFRAC(*),BULKT(*)
      my_real
     .   B11(*), B12(*), B13(*), B14(*),
     .   B21(*), B22(*), B23(*), B24(*)
      my_real
     .   F11(*), F12(*), F21(*), F22(*), AX1(*), AX2(*),
     .   T11(*), T12(*), T13(*), T14(*),
     .   T21(*), T22(*), T23(*), T24(*),
     .   FSKYV(LSKY,8),FSKY(8,LSKY),STI(*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, II, K
      my_real
     .   ALPHT, ALPHX
C-----------------------------------------------
      IF(JALE+JEUL==0.OR.MTN==11) THEN
      DO I=LFT,LLT
        B11(I)=ZERO
        B12(I)=ZERO
        B13(I)=ZERO
        B14(I)=ZERO
        B21(I)=ZERO
        B22(I)=ZERO
        B23(I)=ZERO
        B24(I)=ZERO
      ENDDO
      ENDIF
C-------------------------
C     ASSEMBLE
C-------------------------
      IF(IVECTOR==1) THEN
        DO I=LFT,LLT
          ALPHT=ONE
          ALPHX=ALPH(I)
          II=I+NFT
          K = IADQ(1,II)
          FSKYV(K,2)=FSKYV(K,2)
     .               -AX1(I)+ALPHX*(-T11(I)-F11(I))-ALPHT*B11(I)
          FSKYV(K,3)=FSKYV(K,3)
     .               +AX2(I)+ALPHX*(-T21(I)-F21(I))-ALPHT*B21(I)
          FSKYV(K,7)=FSKYV(K,7)+STI(I)

          K = IADQ(2,II)
          FSKYV(K,2)=FSKYV(K,2)
     .               -AX1(I)+ALPHX*(-T12(I)-F12(I))-ALPHT*B12(I)
          FSKYV(K,3)=FSKYV(K,3)
     .               +AX2(I)+ALPHX*(-T22(I)-F22(I))-ALPHT*B22(I)
          FSKYV(K,7)=FSKYV(K,7)+STI(I)

          K = IADQ(3,II)
          FSKYV(K,2)=FSKYV(K,2)
     .               -AX1(I)+ALPHX*(-T13(I)+F11(I))-ALPHT*B13(I)
          FSKYV(K,3)=FSKYV(K,3)
     .               +AX2(I)+ALPHX*(-T23(I)+F21(I))-ALPHT*B23(I)
          FSKYV(K,7)=FSKYV(K,7)+STI(I)

          K = IADQ(4,II)
          FSKYV(K,2)=FSKYV(K,2)
     .               -AX1(I)+ALPHX*(-T14(I)+F12(I))-ALPHT*B14(I)
          FSKYV(K,3)=FSKYV(K,3)
     .               +AX2(I)+ALPHX*(-T24(I)+F22(I))-ALPHT*B24(I)
          FSKYV(K,7)=FSKYV(K,7)+STI(I)
        ENDDO
      ELSE
        DO I=LFT,LLT
          ALPHT=ONE
          ALPHX=ALPH(I)
          II=I+NFT
          K = IADQ(1,II)
          FSKY(2,K)=FSKY(2,K)
     .              -AX1(I)+ALPHX*(-T11(I)-F11(I))-ALPHT*B11(I)
          FSKY(3,K)=FSKY(3,K)
     .              +AX2(I)+ALPHX*(-T21(I)-F21(I))-ALPHT*B21(I)
          FSKY(7,K)=FSKY(7,K)+STI(I)

          K = IADQ(2,II)
          FSKY(2,K)=FSKY(2,K)
     .              -AX1(I)+ALPHX*(-T12(I)-F12(I))-ALPHT*B12(I)
          FSKY(3,K)=FSKY(3,K)
     .              +AX2(I)+ALPHX*(-T22(I)-F22(I))-ALPHT*B22(I)
          FSKY(7,K)=FSKY(7,K)+STI(I)

          K = IADQ(3,II)
          FSKY(2,K)=FSKY(2,K)
     .              -AX1(I)+ALPHX*(-T13(I)+F11(I))-ALPHT*B13(I)
          FSKY(3,K)=FSKY(3,K)
     .              +AX2(I)+ALPHX*(-T23(I)+F21(I))-ALPHT*B23(I)
          FSKY(7,K)=FSKY(7,K)+STI(I)

          K = IADQ(4,II)
          FSKY(2,K)=FSKY(2,K)
     .              -AX1(I)+ALPHX*(-T14(I)+F12(I))-ALPHT*B14(I)
          FSKY(3,K)=FSKY(3,K)
     .              +AX2(I)+ALPHX*(-T24(I)+F22(I))-ALPHT*B24(I)
          FSKY(7,K)=FSKY(7,K)+STI(I)
        ENDDO
      ENDIF

      DO I=LFT,LLT
        IF(ALPH(I)==ZERO)THEN
         DO J=1,6
           SIG(I,J)=ZERO
         ENDDO
         EINT(I)=ZERO
         RHO(I) =ZERO
         BULK(I)=ZERO
        ENDIF
      ENDDO

      DO I=LFT,LLT
       DO J=1,6
         SIGT(I,J)=SIGT(I,J)+SIG(I,J)*ALPH(I)
       ENDDO
       EINTT(I)=EINTT(I)+EINT(I)*ALPH(I)
       RHOT(I) =RHOT(I)+RHO(I)*ALPH(I)
      ENDDO

      IF(L_TEMP>0)THEN
        DO I=LFT,LLT
         TEMPT(I)=TEMPT(I)+TEMP(I)*ALPH(I)
        ENDDO
      ENDIF

      IF(L_PLA>0)THEN
        DO I=LFT,LLT
         PLAST(I)=PLAST(I)+PLAS(I)*ALPH(I)
        ENDDO
      ENDIF  
      
      IF(L_BFRAC>0)THEN
        DO I=LFT,LLT
         BFRACT(I)=BFRACT(I)+BFRAC(I)*ALPH(I)
        ENDDO
      ENDIF  
      
      IF(L_BULK>0)THEN
        DO I=LFT,LLT
         BULKT(I)=BULKT(I)+BULK(I)*ALPH(I)
        ENDDO
      ENDIF                

      RETURN
      END
