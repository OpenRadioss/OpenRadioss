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
Chd|  BALPH2                        source/ale/bimat/balph2.F     
Chd|-- called by -----------
Chd|        BFORC2                        source/ale/bimat/bforc2.F     
Chd|-- calls ---------------
Chd|        IDP_FREE                      source/system/machine.F       
Chd|        IDP_LOCK                      source/system/machine.F       
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE BALPH2(PM,ALPH1,ALPH2,VOLT,FILL,
     .                  SIG1,EINT1,VOLO1,RHON1,FLUX1,FLU11,OFF1,
     .                  SIG2,EINT2,VOLO2,RHON2,FLUX2,FLU12,OFF2,
     .                  SIGT,EINTT,RHOT ,TEMPT,MTN1,MTN2,NGL,L_TEMP,
     .                  BFRACT,L_BFRAC,PLAST,L_PLAS,VOLN, BULKT, L_BULK,NEL,
     .                  AIRE, AIRES,
     .                  D1, D2, D3, D4, D5, D6,
     .                  D1S, D2S, D3S, D4S, D5S, D6S,
     .                  MAT, NC1, NC2, NC3, NC4,
     .                  DALPH1, DALPH2)
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "param_c.inc"
#include      "vect01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER MTN1, MTN2, NGL(*), L_TEMP, L_PLAS, L_BFRAC,L_BULK,NEL
      INTEGER MAT(*), NC1(*), NC2(*), NC3(*), NC4(*)
      my_real
     .   PM(NPROPM,*), ALPH1(*), ALPH2(*), VOLT(*), FILL(NUMNOD,*),
     .   SIG1(NEL,6), EINT1(*), VOLO1(*), RHON1(*), FLUX1(4,*), FLU11(*),
     .   OFF1(*), SIG2(NEL,6), EINT2(*), VOLO2(*), RHON2(*), FLUX2(4,*),
     .   FLU12(*), OFF2(*), SIGT(NEL,6), EINTT(*), RHOT(*), TEMPT(*),
     .   PLAST(*), BFRACT(*),VOLN(MVSIZ),BULKT(*),
     .   AIRE(*), AIRES(*),
     .   D1(*), D2(*), D3(*), D4(*), D5(*), D6(*),
     .   D1S(*), D2S(*), D3S(*), D4S(*), D5S(*), D6S(*),
     .   DALPH1(*), DALPH2(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, MX1, MX2
      my_real
     .   ALPH1N(MVSIZ), ALPH2N(MVSIZ), ALPN1,
     .   ALPN2, ALPN3, ALPN4, ALPD1, ALPD2, ALPD3, ALPD4, ALPD, ALPN,
     .   EPS, SOFF1, SOFF2, ALPHN, ALPHNN, C11, C12, ALPHT, DA,
     .   IALPH1, IALPH2
C-----------------------------------------------
      DO I=LFT,LLT
        SIGT(I,1)=ZERO
        SIGT(I,2)=ZERO
        SIGT(I,3)=ZERO
        SIGT(I,4)=ZERO
        SIGT(I,5)=ZERO
        SIGT(I,6)=ZERO
        EINTT(I) =ZERO
        RHOT(I)  =ZERO
        VOLT(I)  =VOLN(I)
        AIRES(I) =AIRE(I)
        D1S(I)   =D1(I)
        D2S(I)   =D2(I)
        D3S(I)   =D3(I)
        D4S(I)   =D4(I)
        D5S(I)   =D5(I)
        D6S(I)   =D6(I)
      ENDDO

      IF(L_TEMP>0)THEN
        DO I=LFT,LLT
          TEMPT(I)=ZERO
        ENDDO
      ENDIF

      IF(L_PLAS>0)THEN
        DO I=LFT,LLT
          PLAST(I)=ZERO
        ENDDO
      ENDIF

      IF(L_BFRAC>0)THEN
        DO I=LFT,LLT
          BFRACT(I)=ZERO
        ENDDO
      ENDIF

      IF(L_BULK>0)THEN
        DO I=LFT,LLT
          BULKT(I)=ZERO
        ENDDO
      ENDIF
C-----------------------------------------------
C     MULTIMATERIAUX TAUX DE REMPLISSAGE
C-----------------------------------------------
       DO I=LFT,LLT
         ALPH1N(I)=ZERO
         ALPH2N(I)=ZERO
         ALPN1= MAX(ZERO,FILL(NC1(I),1))
         ALPN2= MAX(ZERO,FILL(NC2(I),1))
         ALPN3= MAX(ZERO,FILL(NC3(I),1))
         ALPN4= MAX(ZERO,FILL(NC4(I),1))
         ALPD1=ABS(FILL(NC1(I),1))
         ALPD2=ABS(FILL(NC2(I),1))
         ALPD3=ABS(FILL(NC3(I),1))
         ALPD4=ABS(FILL(NC4(I),1))
         ALPD=ALPD1+ALPD2+ALPD3+ALPD4
         ALPN=ALPN1+ALPN2+ALPN3+ALPN4
         IF (ALPD>EM20) ALPH1N(I)=ALPN/ALPD
       ENDDO

       IF(JMULT>1)THEN
         DO I=LFT,LLT
           ALPN1= MAX(ZERO,FILL(NC1(I),2))
           ALPN2= MAX(ZERO,FILL(NC2(I),2))
           ALPN3= MAX(ZERO,FILL(NC3(I),2))
           ALPN4= MAX(ZERO,FILL(NC4(I),2))
           ALPD1=ABS(FILL(NC1(I),2))
           ALPD2=ABS(FILL(NC2(I),2))
           ALPD3=ABS(FILL(NC3(I),2))
           ALPD4=ABS(FILL(NC4(I),2))
           ALPD=ALPD1+ALPD2+ALPD3+ALPD4
           ALPN=ALPN1+ALPN2+ALPN3+ALPN4
           IF(ALPD>EM20)ALPH2N(I)=ALPN/ALPD
         ENDDO
       ENDIF

       EPS=EM15
       SOFF1=ZERO
       SOFF2=ZERO

      IF(JMULT==1)THEN
       IALPH1 = 0
       DO I=LFT,LLT
         DALPH1(I)=ZERO
         IF(ALPH1N(I)/=ZERO.AND.ALPH1N(I)/=ONE)THEN
          ALPHN=(VOLO1(I)-DT1*HALF*(FLU11(I)
     .    +FLUX1(1,I)+FLUX1(2,I)+FLUX1(3,I)+FLUX1(4,I)))/VOLN(I)
          ALPHNN=ALPHN*(ONE+(D1(I)+D2(I)+D3(I))*DT1)
          IF((SIG1(I,1)+SIG1(I,2)+SIG1(I,3))>ZERO)THEN	
           ALPHN= MIN(ALPHN,ALPHNN)
          ELSE
           ALPHN= MAX(ALPHN,ALPHNN)
          ENDIF
          IF(ALPHN<=ZEP99.AND.ALPHN>ZERO.AND.RHON1(I)<=ZERO)THEN
           IF(RHON1(I)/=ZERO)THEN
#include "lockon.inc"
            WRITE(6,*)' ***NEGATIVE RHO****ALPH1,RHON1'
            WRITE(6,*)I+NFT,ALPH1N(I),RHON1(I)
#include "lockoff.inc"
            RHON1(I)=ZERO
           ENDIF
           ALPHN=ZERO
          ENDIF
          DALPH1(I)=ALPHN-ALPH1N(I)
          ALPH1(I)= MIN(ONE,ALPHN)
          ALPH1(I)= MAX(ZERO,ALPH1(I))
         ELSEIF(ALPH1N(I)==ONE.AND.ALPH1(I)<ZEP999)THEN
          ALPHN=(VOLO1(I)-DT1*HALF*(FLU11(I)
     .    +FLUX1(1,I)+FLUX1(2,I)+FLUX1(3,I)+FLUX1(4,I)))/VOLN(I)
          ALPHNN=ALPHN*(ONE+(D1(I)+D2(I)+D3(I))*DT1)
          ALPHN= MAX(ALPHN,ALPHNN,ALPH1(I))
          IF(RHON1(I)<=ZERO)THEN
           RHON1(I)=ZERO
           ALPHN=ZERO
          ENDIF
          ALPH1(I)= MIN(ONE,ALPHN)
         ELSEIF(ALPH1N(I)==ZERO.AND.ALPH1(I)>EM3)THEN
          ALPH1(I)=ALPH1N(I)
         ELSE
          ALPH1(I)=ALPH1N(I)
         ENDIF
       ENDDO !next I

       DO I=LFT,LLT
         IF(ALPH1(I)<EPS)THEN
          ALPH1(I)=ZERO
          OFF1(I) =ZERO
          VOLO1(I)=ZERO
          EINT1(I)=ZERO	
         ELSE
          OFF1(I)=ONE
         ENDIF
         SOFF1=SOFF1+OFF1(I)
         IF(ALPH1(I)==ZERO)THEN
         DALPH1(I)=ZERO
          IALPH1=1
         ELSEIF(ALPH1(I)==ONE)THEN
          DALPH1(I)=ZERO
          IALPH1=1
         ENDIF
       ENDDO

       IF(IALPH1==1) THEN
         CALL IDP_LOCK(2)
         DO I=LFT,LLT
           IF(ALPH1(I)==ZERO)THEN
             FILL(NC1(I),1)= MIN(ZERO,FILL(NC1(I),1))
             FILL(NC2(I),1)= MIN(ZERO,FILL(NC2(I),1))
             FILL(NC3(I),1)= MIN(ZERO,FILL(NC3(I),1))
             FILL(NC4(I),1)= MIN(ZERO,FILL(NC4(I),1))
           ELSEIF(ALPH1(I)==ONE)THEN
             FILL(NC1(I),1)= MAX(ZERO,FILL(NC1(I),1))
             FILL(NC2(I),1)= MAX(ZERO,FILL(NC2(I),1))
             FILL(NC3(I),1)= MAX(ZERO,FILL(NC3(I),1))
             FILL(NC4(I),1)= MAX(ZERO,FILL(NC4(I),1))
           ENDIF
         ENDDO
         CALL IDP_FREE(2)
       ENDIF

      ELSEIF(JMULT==2)THEN
       IALPH1 = 0
       IALPH2 = 0
       DO I=LFT,LLT
         DALPH1(I)=ZERO
         DALPH2(I)=ZERO
         MX1=NINT(PM(21,MAT(I)))
         C11 =PM(32,MX1)
         MX2=NINT(PM(22,MAT(I)))
         C12 =PM(32,MX2)
         IF(ALPH1N(I)/=ZERO.AND.ALPH1N(I)/=ONE)THEN
          ALPHN=(VOLO1(I)-DT1*HALF*(FLU11(I)
     .    +FLUX1(1,I)+FLUX1(2,I)+FLUX1(3,I)+FLUX1(4,I)))/VOLN(I)
          ALPHNN=ALPHN*(ONE+(D1(I)+D2(I)+D3(I))*DT1)
          IF((SIG1(I,1)+SIG1(I,2)+SIG1(I,3))>ZERO)THEN
           ALPHN= MIN(ALPHN,ALPHNN)
          ELSE
           ALPHN= MAX(ALPHN,ALPHNN)
          ENDIF
          IF(ALPHN<=ZEP99.AND.ALPHN>ZERO.AND.RHON1(I)<=ZERO)THEN
           IF(RHON1(I)/=ZERO)THEN
#include "lockon.inc"
            WRITE(6,*)' ***NEGATIVE RHO****ALPH1,RHON1'
            WRITE(6,*)I+NFT,ALPH1N(I),RHON1(I)
#include "lockoff.inc"
            RHON1(I)=ZERO
           ENDIF
           ALPHN=ZERO
          ENDIF
          DALPH1(I)=ALPHN-ALPH1N(I)
          ALPH1(I)= MIN(ONE,ALPHN)
          ALPH1(I)= MAX(ZERO,ALPH1(I))
         ELSEIF(ALPH1N(I)==ONE.AND.ALPH1(I)<ZEP999)THEN
          ALPHN=(VOLO1(I)-DT1*HALF*(FLU11(I)
     .    +FLUX1(1,I)+FLUX1(2,I)+FLUX1(3,I)+FLUX1(4,I)))/VOLN(I)
          ALPHNN=ALPHN*(ONE + (D1(I)+D2(I)+D3(I))*DT1)
          ALPHN= MAX(ALPHN,ALPHNN,ALPH1(I))
          IF(RHON1(I)<=ZERO)THEN
           RHON1(I)=ZERO
           ALPHN=ZERO
          ENDIF
          ALPH1(I)= MIN(ONE,ALPHN)
         ELSEIF(ALPH1N(I)==ZERO.AND.ALPH1(I)>EM3)THEN
          ALPH1(I)=ALPH1N(I)
         ELSE
          ALPH1(I)=ALPH1N(I)
         ENDIF
         IF(ALPH2N(I)/=ZERO.AND.ALPH2N(I)/=ONE)THEN
          ALPHN=(VOLO2(I)-DT1*HALF*(FLU12(I)
     .    +FLUX2(1,I)+FLUX2(2,I)+FLUX2(3,I)+FLUX2(4,I)))/VOLN(I)
          ALPHNN=ALPHN*(ONE+(D1(I)+D2(I)+D3(I))*DT1)
          IF((SIG2(I,1)+SIG2(I,2)+SIG2(I,3))>ONE)THEN
           ALPHN= MIN(ALPHN,ALPHNN)
          ELSE
           ALPHN= MAX(ALPHN,ALPHNN)
          ENDIF
          IF(ALPHN<=ZEP99.AND.ALPHN>ZERO.AND.RHON2(I)<=ZERO)THEN
           IF(RHON2(I)/=ZERO)THEN
#include "lockon.inc"
            WRITE(6,*)' ***NEGATIVE RHO****ALPH2,RHON2'
            WRITE(6,*)I+NFT,ALPH2N(I),RHON2(I)
#include "lockoff.inc"
            RHON2(I)=ZERO
           ENDIF
           ALPHN=ZERO
          ENDIF
          DALPH2(I)=ALPHN-ALPH2N(I)
          ALPH2(I)= MIN(ONE,ALPHN)
          ALPH2(I)= MAX(ZERO,ALPH2(I))
         ELSEIF(ALPH2N(I)==ONE.AND.ALPH2(I)<=ZEP999)THEN
          ALPHN=(VOLO2(I)-DT1*HALF*(FLU12(I)
     .    +FLUX2(1,I)+FLUX2(2,I)+FLUX2(3,I)+FLUX2(4,I)))/VOLN(I)
          ALPHNN=ALPHN*(ONE + (D1(I)+D2(I)+D3(I))*DT1)
          ALPHN= MAX(ALPHN,ALPHNN,ALPH2(I))
          IF(RHON2(I)<=ZERO)THEN
           RHON2(I)=ZERO
           ALPHN=ZERO
          ENDIF
          ALPH2(I)= MIN(ONE,ALPHN)
         ELSEIF(ALPH2N(I)==ZERO.AND.ALPH2(I)>EM3)THEN
          ALPH2(I)=ALPH2N(I)
         ELSE
          ALPH2(I)=ALPH2N(I)
         ENDIF
         ALPHT=ALPH1(I)+ALPH2(I)
         IF(ALPHT>ONE)THEN
           DA=(ALPHT-ONE)/(C11*ALPH2(I)+C12*ALPH1(I))
           ALPH1(I)=ALPH1(I)*(ONE -C12*DA)
           ALPH2(I)=ALPH2(I)*(ONE -C11*DA)
           DALPH1(I)=DALPH1(I)-C12*DA
           DALPH2(I)=DALPH2(I)-C11*DA
         ENDIF
       ENDDO

       DO I=LFT,LLT
         IF(ALPH1(I)<EPS)THEN
          ALPH1(I)=ZERO
          OFF1(I) =ZERO
          VOLO1(I)=ZERO
          EINT1(I)=ZERO	
         ELSE
          OFF1(I) =ONE
         ENDIF
         SOFF1=SOFF1+OFF1(I)
         IF(ALPH2(I)<EPS)THEN
          ALPH2(I)=ZERO
          OFF2(I) =ZERO
          VOLO2(I)=ZERO
          EINT2(I)=ZERO
         ELSE
          OFF2(I)=ONE
         ENDIF
         SOFF2=SOFF2+OFF2(I)
         IF(ALPH1(I)==ZERO)THEN
          DALPH1(I)=ZERO
          IALPH1 = 1
         ELSEIF(ALPH1(I)==ONE)THEN
          DALPH1(I)=ZERO
          IALPH1 = 1
         ENDIF
         IF(ALPH2(I)==ZERO)THEN
          DALPH2(I)=ZERO
          IALPH2 = 1
         ELSEIF(ALPH2(I)==ONE)THEN
          DALPH2(I)=ZERO
          IALPH2 = 1
         ENDIF
       ENDDO

       IF(IALPH1==1) THEN
         CALL IDP_LOCK(2)
         DO I=LFT,LLT
           IF(ALPH1(I)==ZERO)THEN
             FILL(NC1(I),1)= MIN(ZERO,FILL(NC1(I),1))
             FILL(NC2(I),1)= MIN(ZERO,FILL(NC2(I),1))
             FILL(NC3(I),1)= MIN(ZERO,FILL(NC3(I),1))
             FILL(NC4(I),1)= MIN(ZERO,FILL(NC4(I),1))
           ELSEIF(ALPH1(I)==ONE)THEN
             FILL(NC1(I),1)= MAX(ZERO,FILL(NC1(I),1))
             FILL(NC2(I),1)= MAX(ZERO,FILL(NC2(I),1))
             FILL(NC3(I),1)= MAX(ZERO,FILL(NC3(I),1))
             FILL(NC4(I),1)= MAX(ZERO,FILL(NC4(I),1))
           ENDIF
         ENDDO
         CALL IDP_FREE(2)
       ENDIF

       IF(IALPH2==1) THEN
         CALL IDP_LOCK(2)
         DO I=LFT,LLT
           IF(ALPH2(I)==ZERO)THEN
             FILL(NC1(I),2)= MIN(ZERO,FILL(NC1(I),2))
             FILL(NC2(I),2)= MIN(ZERO,FILL(NC2(I),2))
             FILL(NC3(I),2)= MIN(ZERO,FILL(NC3(I),2))
             FILL(NC4(I),2)= MIN(ZERO,FILL(NC4(I),2))
           ELSEIF(ALPH2(I)==ONE)THEN
             FILL(NC1(I),2)= MAX(ZERO,FILL(NC1(I),2))
             FILL(NC2(I),2)= MAX(ZERO,FILL(NC2(I),2))
             FILL(NC3(I),2)= MAX(ZERO,FILL(NC3(I),2))
             FILL(NC4(I),2)= MAX(ZERO,FILL(NC4(I),2))
           ENDIF
         ENDDO
         CALL IDP_FREE(2)
       ENDIF
      ENDIF

      RETURN
      END
