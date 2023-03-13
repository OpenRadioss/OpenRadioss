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
Chd|  I14CAN                        source/interfaces/int14/i14can.F
Chd|-- called by -----------
Chd|        I14CMP                        source/interfaces/int14/i14cmp.F
Chd|-- calls ---------------
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|====================================================================
      SUBROUTINE I14CAN(NSI     ,KSI     ,X    ,
     2                  KSURF   ,IGRSURF ,BUFSF,
     3                  G       ,
     4                  NSC   ,KSC ,NSP ,KSP ,   
     5                  IMPACT,CIMP,NIMP,EW  )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE GROUPDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NSI, NSC, NSP, KSURF,
     .        KSI(*), IMPACT(*)
C     REAL
      my_real
     .       X(3,*), BUFSF(*), G, KSC(*), KSP(*),
     .       CIMP(3,*),NIMP(3,*),EW(*)
      TYPE (SURF_)   , DIMENSION(NSURF)   :: IGRSURF
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER ADRBUF, I, IN, IL
      INTEGER DGR
      my_real
     .   XG, YG, ZG, A, B, C, ROT(9),
     .   AN, BN, CN, HN,
     .   TRMX, TRMY, TRMZ, TRMXN, TRMYN, TRMZN,
     .   XP, YP, ZP,
     .   D, H, EG(6), EPG
C-----------------------------------------------
      ADRBUF=IGRSURF(KSURF)%IAD_BUFR
C
      DGR=BUFSF(ADRBUF+36)
      XG=BUFSF(ADRBUF+4)
      YG=BUFSF(ADRBUF+5)
      ZG=BUFSF(ADRBUF+6)
      A =BUFSF(ADRBUF+1)
      B =BUFSF(ADRBUF+2)
      C =BUFSF(ADRBUF+3)
C
       D=MIN(A,B,C)
       D=ONE/D
C---------------------------------
       AN=A**DGR
       BN=B**DGR
       CN=C**DGR
       AN=ONE/AN
       BN=ONE/BN
       CN=ONE/CN       
       DO I=1,9
        ROT(I)=BUFSF(ADRBUF+7+I-1)
       END DO
C-----------------------------------------------
       NSC=0
       NSP=0
C-----------------------------------------------
C      Calcul de EG (coefs de l'equation de l'ellipse
C      dans rep. global centre en G).
C      Warning : Xloc = [ROT] Xglo
C      EG(1)=ROT(1)*ROT(1)*A2+ROT(4)*ROT(4)*B2+ROT(7)*ROT(7)*C2
C      EG(2)=ROT(1)*ROT(2)*A2+ROT(4)*ROT(5)*B2+ROT(7)*ROT(8)*C2
C      EG(3)=ROT(1)*ROT(3)*A2+ROT(4)*ROT(6)*B2+ROT(7)*ROT(9)*C2
C      EG(4)=ROT(2)*ROT(2)*A2+ROT(5)*ROT(5)*B2+ROT(8)*ROT(8)*C2
C      EG(5)=ROT(2)*ROT(3)*A2+ROT(5)*ROT(6)*B2+ROT(8)*ROT(9)*C2
C      EG(6)=ROT(3)*ROT(3)*A2+ROT(6)*ROT(6)*B2+ROT(9)*ROT(9)*C2
C------------------------
#include "vectorize.inc"
       DO 110 I=1,NSI
        IN=KSI(I)
        XP=X(1,IN)-XG
        YP=X(2,IN)-YG
        ZP=X(3,IN)-ZG
        TRMX=ROT(1)*XP+ROT(2)*YP+ROT(3)*ZP
        TRMY=ROT(4)*XP+ROT(5)*YP+ROT(6)*ZP
        TRMZ=ROT(7)*XP+ROT(8)*YP+ROT(9)*ZP
        IF (IMPACT(I)==0) THEN
C        penetration / ellips.
C        EPG= EG(1)*XP*XP+EG(2)*XP*YP+EG(3)*XP*ZP
C    .       +EG(4)*YP*YP+EG(5)*YP*ZP
C    .       +EG(6)*ZP*ZP
         TRMXN=TRMX**DGR
         TRMYN=TRMY**DGR
         TRMZN=TRMZ**DGR
         TRMXN=ABS(TRMXN*AN)
         TRMYN=ABS(TRMYN*BN)
         TRMZN=ABS(TRMZN*CN)
         EW(IN)=TRMXN+TRMYN+TRMZN
        ELSEIF (IMPACT(I)>0) THEN
C        penetration / plan.
         EW(IN)= NIMP(1,I)*(TRMX-CIMP(1,I))
     .          +NIMP(2,I)*(TRMY-CIMP(2,I))
     .          +NIMP(3,I)*(TRMZ-CIMP(3,I))
        ENDIF
110    CONTINUE
C---------------------------------
       DO 120 I=1,NSI
        IN=KSI(I)
        IF (IMPACT(I)>0) THEN
C---------------------------------
          IF (EW(IN) <= G) THEN
           NSP=NSP+1
           KSP(NSP)=I
          ELSE
           IMPACT(I)=0
          ENDIF
        ELSE
C---------------------------------
C         tri exhaustif.
          H =ONE+G*D
          HN=H**DGR
          IF (EW(IN) <= HN) THEN
           NSC=NSC+1
           KSC(NSC)=I
          ELSE
           IMPACT(I)=0
          END IF
        END IF
120   CONTINUE
C---------------------------------
      RETURN
      END
