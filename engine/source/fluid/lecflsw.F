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
Chd|  LECFLSW                       source/fluid/lecflsw.F        
Chd|-- called by -----------
Chd|        LECTUR                        source/input/lectur.F         
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        NINTRN                        source/fluid/nintrn.F         
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE LECFLSW (NSFLSW,NTFLSW,NEFLSW,NNFLSW,CRFLSW,
     .                    X,IXS,IPARG,ITMP)
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "units_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NSFLSW, NTFLSW
      INTEGER NEFLSW(*), NNFLSW(8,*), IXS(NIXS,*), IPARG(NPARG,*), ITMP(*)
      my_real CRFLSW(6,*), X(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IL, I, IS, NEL, J, K, IE, II, NG, ITY, LFT, LLT, NFT, NB1,
     .   N, I2, I1, NE, N1, N2, N3, N4
      my_real
     .   CRX, CRY, CRZ, SURS, X1, Y1, Z1, X2, Y2, Z2, X3, Y3, Z3, X4,
     .   Y4, Z4, SFX, SFY, SFZ, SFM, SURV
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER NINTRN
C-----------------------------------------------
      IL = 0
      WRITE (IOUT, 1000)
      DO 200 I = 1, NSFLSW
C
C     LECTURE DU NUMERO DE LA SECTION ET DU NOMBRE DE FACES
C
        READ (IIN, '(2I5,3F10.0)') IS, NEL, CRX, CRY, CRZ
C
C
C     LECTURE DES FACES PAR SECTION
C
        NEFLSW(I) = NEL
        CRFLSW(1,I) = CRX
        CRFLSW(2,I) = CRY
        CRFLSW(3,I) = CRZ
        DO 100 J = 1, NEL
          IL = IL + 1
          READ(IIN, '(6I5)') NNFLSW(7,IL),(NNFLSW(K,IL),K=2,6)
  100   CONTINUE
  200 CONTINUE
C
C     TEST SI LE NOMBRE TOTALE DE FACES EST EGALE A NTFLSW
C
      IF (IL .NE. NTFLSW) THEN
        CALL ANCMSG(MSGID=16,ANMODE=ANINFO,
     .              I1=IL,I2=NTFLSW)
        CALL ARRET(2)
      END IF
C
C     CALCUL DE LA NUMEROTATION INTERNE DES ELEMENTS ET L'ADRESSE
C     DANS LE BUFFER D'ELEMENTS
C       IL               :  NUMERO LOCALE DE LA FACE ET DE L'ELEMENT
C       IE               :  NUMERO EXTERNE DE L'ELEMENT
C       NEL=NNFLSW(1,IL) :  NOMBRE D'ELEMENTS DANS LE GROUPE D'ELEMENTS
C       IB2=NNFLSW(7,IL) :  ADRESSE DE LA PRESSION (ECRASE NUMERO EXTERNE)
C       IB3=NNFLSW(8,IL) :  ADRESSE DE L'ENERGIE
C       ITMP(II) = IL    :  NUMERATION LOCALE DES ELEMENTS DES SECTIONS
C
      DO 300 I = 1, NUMELS
        ITMP(I) = 0
  300 CONTINUE
C
      DO 310 IL = 1, NTFLSW
        IE = NNFLSW(7,IL)
        II = NINTRN(IE,IXS,11,NUMELS)
        ITMP(II) = IL
        IF (NNFLSW(6,IL) .EQ. 0) NNFLSW(6,IL) = 4
  310 CONTINUE
C
      DO 350 NG=1,NGROUP
        ITY = IPARG(5,NG)
        IF(ITY.GT.1) GO TO 350
        LFT = 1
        LLT = IPARG(2,NG)
        NFT = IPARG(3,NG)
        NB1 = IPARG(4,NG)
        DO 330 I = LFT,LLT
          N = I+NFT
          IL = ITMP(N)
          IF (IL .GT. 0) THEN
            NNFLSW(7,IL) = NB1 +   LLT + 6*I - 6
            NNFLSW(8,IL) = NB1 + 7*LLT +   I - 1
            NNFLSW(1,IL) = LLT
          END IF
  330   CONTINUE
  350 CONTINUE
C
C     CALCUL DES SURFACES ET DE LA NORMALE PAR SECTION
C
      IL = 0
      I2 = 0
      DO 500 IS = 1, NSFLSW
        SURS = 0.
        NEL = NEFLSW(IS)
        I1  = I2 + 1
        I2  = I2 + NEL
        DO 400 I = I1, I2
            NE = NNFLSW(1,I)
            N1 = NNFLSW(2,I)
            N2 = NNFLSW(3,I)
            N3 = NNFLSW(4,I)
            N4 = NNFLSW(5,I)
C
            X1 = X(1,N1)
            Y1 = X(2,N1)
            Z1 = X(3,N1)
            X2 = X(1,N2)
            Y2 = X(2,N2)
            Z2 = X(3,N2)
            X3 = X(1,N3)
            Y3 = X(2,N3)
            Z3 = X(3,N3)
            X4 = X(1,N4)
            Y4 = X(2,N4)
            Z4 = X(3,N4)
C
            SFX = UNDEMI*((Y3-Y1)*(Z4-Z2)-
     1                   (Z3-Z1)*(Y4-Y2))
            SFY = UNDEMI*((Z3-Z1)*(X4-X2)-
     1                   (X3-X1)*(Z4-Z2))
            SFZ = UNDEMI*((X3-X1)*(Y4-Y2)-
     1                   (Y3-Y1)*(X4-X2))
            SFM = SQRT(SFX*SFX+SFY*SFY+SFZ*SFZ)
C
            CRFLSW(4,IS) = CRFLSW(4,IS) + SFX
            CRFLSW(5,IS) = CRFLSW(5,IS) + SFY
            CRFLSW(6,IS) = CRFLSW(6,IS) + SFZ
            SURS         = SURS + SFM
C
  400     CONTINUE
C
          SURV = SQRT(CRFLSW(4,IS)**2+CRFLSW(5,IS)**2+CRFLSW(6,IS)**2)
          CRFLSW(4,IS) = CRFLSW(4,IS)/SURV
          CRFLSW(5,IS) = CRFLSW(5,IS)/SURV
          CRFLSW(6,IS) = CRFLSW(6,IS)/SURV
C
        WRITE (IOUT, 1100) IS, NEL, (CRFLSW(K,IS),K=1,6),SURV,SURS
        WRITE (IOUT, 1200)
     1              (J,(NNFLSW(K,IL-NEL+J),K=1,8),J=1,NEL,NEL-1)
  500 CONTINUE
C
 1000 FORMAT (///' FLUX AND SWIRL CALCULATION'/
     1           ' --------------------------'/)
 1100 FORMAT (/
     1  ' SET NUMBER . . . . . . . . . . ',I5/
     1  '    NUMBER OF ELEMENTS. . . . . ',I5/
     1  '    SWIRL CENTER. . . . . . . . ',3E12.4/
     1  '    SWIRL AXIS. . . . . . . . . ',3E12.4/
     1  '    VECTORIAL TOTAL SURFACE . . ',E12.4 /
     1  '    SCALAR TOTAL SURFACE. . . . ',E12.4 /
     1   1H ,16HFIRST/LAST NUMB.,6HI.E.N.,4X,6HNODE-1,4X,6HNODE-2,4X,
     2                      6HNODE-3,4X,6HNODE-4,4X,6H NDIV    )
 1200 FORMAT (2H  ,10I10)
      RETURN
      END
