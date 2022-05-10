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
Cqz ------- only FIXV dans system global sont traites---------
Chd|====================================================================
Chd|  FV_IMP0                       source/constraints/general/impvel/fv_imp0.F
Chd|-- called by -----------
Chd|        UPD_GLOB_K                    source/implicit/upd_glob_k.F  
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE FV_IMP0(IDDL   ,IFIX  ,NDOF  ,IADK  ,JDIK ,
     1                   DIAG_K ,LT_K  ,UD    ,NBK   ,IAB  ,
     2                   BK     ,NDDL  ,RD    )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "impl1_c.inc"
#include      "com01_c.inc"
#include "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER MAXC
      INTEGER
     .     IDDL(*),IADK(*),JDIK(*),NDOF(*),
     .     NBK(*),IAB(NFXVEL,*) ,NDDL ,IFIX(*)
C     REAL ou REAL*8
      my_real
     .  UD(3,*), DIAG_K(*),LT_K(*),BK(NFXVEL,*),RD(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER N, I, J, K,I1,J1,ND,ID,IDUD(NFXVEL),
     .        NFV,NF,NT,JD
      my_real
     .  U(NFXVEL)
C     REAL ou REAL*8
C--------extraire kij associe avec Ud (update {b} due to {ud})-----
      NFV=0
      DO I = 1,NUMNOD
       IF (NDOF(I).GT.0) THEN
        ND = IDDL(I)
        K = NDOF(I)
        DO J =1,K
         ID = ND + J
         IF (J.LE.3) THEN
          IF (IFIX(ID).EQ.1) THEN
           UD(J,I)=ZERO
          ELSEIF (IFIX(ID).EQ.2.OR.IFIX(ID).EQ.9) THEN
           NFV=NFV+1
           IDUD(NFV)=ID
           U(NFV)=UD(J,I)
          ENDIF
         ELSE
          IF (IFIX(ID).EQ.1) THEN
           RD(J-3,I)=ZERO
          ELSEIF (IFIX(ID).EQ.2.OR.IFIX(ID).EQ.9) THEN
           NFV=NFV+1
           IDUD(NFV)=ID
           U(NFV)=RD(J-3,I)
          ENDIF
         ENDIF
        ENDDO
       ENDIF
      ENDDO
C      IF (NFV.NE.NFXVEL) WRITE(*,*)'ERROR IN FV_IMP0',NFV,NFXVEL
C
      DO I = 1,NFXVEL
       NBK(I)=0
      ENDDO
C
      DO I1=1,NFV
       ND=0
       ID=IDUD(I1)
C------------Ligne ID-------
       DO J1 = IADK(ID),IADK(ID+1)-1
        JD = JDIK(J1)
        IF (IFIX(JD).EQ.0.AND.LT_K(J1).NE.ZERO) THEN
         ND=NBK(I1)+1
         IAB(I1,ND)=JD
         BK(I1,ND)=U(I1)*LT_K(J1)
         NBK(I1)=ND
        ENDIF
       ENDDO
C------------Colonne ID-------
       IF (IKPAT.EQ.0) THEN
        NF=1
        NT=ID-1
       ELSE
        NF=ID+1
        NT=NDDL
       ENDIF
       DO I = NF,NT
         IF (IFIX(I).EQ.0) THEN
          DO K = IADK(I),IADK(I+1)-1
           J=JDIK(K)
           IF (ID.EQ.J.AND.LT_K(K).NE.ZERO) THEN
             ND=NBK(I1)+1
             IAB(I1,ND)=I
             BK(I1,ND)=U(I1)*LT_K(K)
             NBK(I1)=ND
           ENDIF
          ENDDO
         ENDIF
       ENDDO
C
       IF (ND.GT.MAXB0) THEN
        CALL ANCMSG(MSGID=103,ANMODE=ANINFO,
     .            I1=ND,I2=MAXB,I3=I1)
        CALL ARRET(2)
       ENDIF
      ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  FV_IMP1                       source/constraints/general/impvel/fv_imp0.F
Chd|-- called by -----------
Chd|        IMP_CHKM                      source/implicit/imp_solv.F    
Chd|        IMP_SOLV                      source/implicit/imp_solv.F    
Chd|-- calls ---------------
Chd|        IMP_FVBCL                     share/modules/impbufdef_mod.F 
Chd|====================================================================
      SUBROUTINE FV_IMP1(NBK   ,IAB   ,BK     ,B    )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMP_FVBCL
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER
     .        NBK(*),IAB(NFXVEL,*)
C     REAL ou REAL*8
      my_real
     .  BK(NFXVEL,*), B(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER N, I, J, K,I1,J1,K1,ND,ID
C     REAL ou REAL*8
C----------------only imposed global displacement is available---------------
C--------update {b} due to {ud}-----
      DO I = 1,NFXVEL
       DO J = 1,NBK(I)
        ID=IAB(I,J)
        B(ID)=B(ID)-BK(I,J)
       ENDDO
      ENDDO
C------Part of FV-BCS coupling----
       DO J = 1,NKUD_1
        ID=IKUD_1(J)
        B(ID)=B(ID)-BKUD_1(J)
       ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  FV_IMP                        source/constraints/general/impvel/fv_imp0.F
Chd|-- called by -----------
Chd|        IMP_BUCK                      source/implicit/imp_buck.F    
Chd|        IMP_CHKM                      source/implicit/imp_solv.F    
Chd|        IMP_SOLV                      source/implicit/imp_solv.F    
Chd|-- calls ---------------
Chd|        DINTERI                       source/constraints/general/impvel/fv_imp0.F
Chd|        FVL_MODIF                     source/constraints/general/impvel/fv_imp0.F
Chd|        VELROT                        source/constraints/general/rbe2/rbe2v.F
Chd|====================================================================
      SUBROUTINE FV_IMP(IBFV  ,NPC    ,TF   ,VEL   ,SENSOR,
     1                  LSENSOR,UD    ,RD   ,IFIX  ,IDDL  ,
     2                  SKEW   ,IFRAME,XFRAME,V    ,VR    ,
     3                  X      ,LJ    ,NDOF  ,A    ,AR    )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "param_c.inc"
#include      "task_c.inc"
#include      "scr03_c.inc"
#include      "scr05_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NPC(*),IBFV(NIFV,*),LSENSOR(NSENSI,*),
     .        IFIX(*),IDDL(*),IFRAME(LISKN,*),LJ(*),NDDL,
     .        NDOF(*)
C     REAL ou REAL*8
      my_real
     .  TF(*), VEL(LFXVELR,*), UD(3,*), SENSOR(NSENSR,*),
     .  SKEW(LSKEW,*),RD(3,*),V(3,*),VR(3,*),
     .  X(3,*),XFRAME(NXFRAME,*),A(3,*),AR(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER N, I, ISK, J, L, K1, K2, K3, ISENS,K,
     .        II, IC, NN, IDEB, NR, NSK, NFK, IFM, N0,
     .        ILENC(MVSIZ), IPOSC(MVSIZ), IADC(MVSIZ),
     .        LC(MVSIZ), INDEX(MVSIZ),I1,J1,ND,ID,J2,J3,
     .        N1,N2,N3,NVL
C     REAL ou REAL*8
      my_real
     .   FAC, STARTT, STOPT, TS,DYDX,
     .   YC(MVSIZ), TSC(MVSIZ), DYDXC(MVSIZ),
     .   RX,RY,RZ,VF,VFX,VFY,VFZ,VL(NFXVEL),
     .   TSC1(MVSIZ),FACX,A0,LMS(3),VS(3),MRV(3),VV
C IBFV(7,N):1 V;2 D ;0 A ;
      IDEB = 0
      DO NN=1,NFXVEL
        LJ(NN) = 0
      ENDDO
      NVL = 0
C
      DO NN=1,NFXVEL,NVSIZ
        IF (IBFV(8,NN).EQ.1) GOTO 100
        IC = 0
        IF (NSENSOR.GT.0) THEN
          DO 10 II = 1, MIN(NFXVEL-IDEB,NVSIZ)
            N = II+IDEB
            STARTT = VEL(2,N)
            STOPT  = VEL(3,N)
            IF(TT.LT.STARTT)GOTO 10
            IF(TT.GT.STOPT) GOTO 10
            I=IABS(IBFV(1,N))
            IF(NDOF(I).EQ.0) GOTO 10
            ISENS=0
            DO K=1,NSENSOR
              IF(IBFV(4,N).EQ.LSENSOR(1,K))ISENS=K
            ENDDO
            IF(ISENS.EQ.0)THEN
              TS=TT
            ELSE
              TS = TT-SENSOR(2,ISENS)
              IF(TS.LT.ZERO)GOTO 10
            ENDIF
            IC = IC + 1
            INDEX(IC) = N
             TSC(IC) = TS
             TSC1(IC) = TSC(IC)-DT2
 10       CONTINUE
        ELSE
          DO 20 II = 1, MIN(NFXVEL-IDEB,NVSIZ)
            N = II+IDEB
            STARTT = VEL(2,N)
            STOPT  = VEL(3,N)
            IF(TT.LT.STARTT)GOTO 20
            IF(TT.GT.STOPT) GOTO 20
            I=IABS(IBFV(1,N))
            IF(NDOF(I).EQ.0) GOTO 20
            IC = IC + 1
            INDEX(IC) = N
             TSC(IC) = TT
             TSC1(IC) = TSC(IC)-DT2
 20       CONTINUE
        ENDIF
C
        IDEB = IDEB + MIN(NFXVEL-IDEB,NVSIZ)
C
         DO II=1,IC
          N = INDEX(II)
          FACX   = VEL(5,N)
          TSC(II) = FACX*TSC(II)
          TSC1(II) = FACX*TSC1(II)
         ENDDO
        IF(NCYCLE.EQ.1)THEN
         DO II=1,IC
          N = INDEX(II)
          L = IBFV(3,N)
          LC(II) = IBFV(7,N)
          IPOSC(II) = 0
          IADC(II) = UNDEMI * NPC(L) + 1
          ILENC(II) = UNDEMI * NPC(L+1) - IADC(II) - IPOSC(II)
         ENDDO
        ELSE
         DO II=1,IC
          N = INDEX(II)
          L = IBFV(3,N)
          LC(II) = IBFV(7,N)
          IPOSC(II) = IBFV(5,N)
          IADC(II) = UNDEMI * NPC(L) + 1
          ILENC(II) = UNDEMI * NPC(L+1) - IADC(II) - IPOSC(II)
         ENDDO
        ENDIF
        CALL DINTERI(TF,IADC,IPOSC,ILENC,IC,TSC1,TSC,YC,LC)
         DO II=1,IC
          N = INDEX(II)
          IBFV(5,N) = IPOSC(II)
          FAC  = VEL(1,N)
          YC(II)  = YC(II) * FAC
          FACX   = VEL(5,N)
C IBFV(7,N):1 V;2 D ;0 A ; explict YC=A;implicit YC=D;
          IF(IBFV(7,N).LT.2) YC(II)  = YC(II) / FACX
          IF(IBFV(7,N).EQ.0) YC(II)  = YC(II) / FACX
          VL(N) = YC(II)
C          YC(II) = (YC(II)-YC1(II)) * FAC
          I=IABS(IBFV(1,N))
          ISK=IBFV(2,N)/10
          IFM = IBFV(9,N)
          J=IBFV(2,N)
          IF (IFM.LE.1) J=J-10*ISK
          IF (ISK.GT.1.OR.IFM.GT.1) THEN
           NVL = NVL + 1
           LJ(N)=J
          ENDIF
          IF(J.LE.3)THEN
            IF(ISK.LE.1.AND.IFM.LE.1)THEN
             UD(J,I)=YC(II)
             K1 = IDDL(I)+J
             IFIX(K1)=2
             A0 = A(J,I)
            ELSEIF (ISK.GT.1) THEN
              K1=3*J-2
              K2=3*J-1
              K3=3*J
              A0 = SKEW(K1,ISK)*A(1,I) +
     .             SKEW(K2,ISK)*A(2,I) +
     .             SKEW(K3,ISK)*A(3,I)
            ELSEIF (IFM.GT.1) THEN
              K1=3*J-2
              K2=3*J-1
              K3=3*J
              RX  = X(1,I) - XFRAME(10,IFM)
              RY  = X(2,I) - XFRAME(11,IFM)
              RZ  = X(3,I) - XFRAME(12,IFM)
              LMS(1)=RX
              LMS(2)=RY
              LMS(3)=RZ
              MRV(1)=XFRAME(13,IFM)*DT2
              MRV(2)=XFRAME(14,IFM)*DT2
              MRV(3)=XFRAME(15,IFM)*DT2
              CALL VELROT(MRV,LMS,VS)
              VFX = XFRAME(31,IFM)*DT2+VS(1)
              VFY = XFRAME(32,IFM)*DT2+VS(2)
              VFZ = XFRAME(33,IFM)*DT2+VS(3)
              VF = XFRAME(K1,IFM)*VFX
     .           + XFRAME(K2,IFM)*VFY
     .           + XFRAME(K3,IFM)*VFZ
              VL(N) = VL(N) + VF
              A0 = XFRAME(K1,IFM)*A(1,I)
     .           + XFRAME(K2,IFM)*A(2,I)
     .           + XFRAME(K3,IFM)*A(3,I)
            ENDIF
          ELSEIF(J.LE.6)THEN
           J1 = J
           J = J - 3
            IF(ISK.LE.1.AND.IFM.LE.1)THEN
             RD(J,I)=YC(II)
             K1 = IDDL(I)+J1
             IFIX(K1)=2
             A0   = AR(J,I)
            ELSEIF (ISK.GT.1) THEN
              K1=3*J-2
              K2=3*J-1
              K3=3*J
              A0 = SKEW(K1,ISK)*AR(1,I) +
     .             SKEW(K2,ISK)*AR(2,I) +
     .             SKEW(K3,ISK)*AR(3,I)
            ELSEIF (IFM.GT.1) THEN
              K1=3*J-2
              K2=3*J-1
              K3=3*J
              J1 = IFRAME(1,IFM)
              VF = XFRAME(K1,IFM)*XFRAME(13,IFM)
     .           + XFRAME(K2,IFM)*XFRAME(14,IFM)
     .           + XFRAME(K3,IFM)*XFRAME(15,IFM)
              VL(N) = VL(N) + VF*DT2
              A0 = XFRAME(K1,IFM)*AR(1,I)
     .           + XFRAME(K2,IFM)*AR(2,I)
     .           + XFRAME(K3,IFM)*AR(3,I)
            ENDIF
          ENDIF
C-------------SAVE Fint-----
          VEL(4,N) = A0
         ENDDO
 100    CONTINUE
      ENDDO
C-------------traitement-pour fxvel dans system local  --
      IF (NVL .GT. 0) THEN
       CALL FVL_MODIF(NVL   ,IBFV  ,UD    ,RD   ,IFIX  ,
     1                IDDL  ,SKEW  ,XFRAME,VL   ,LJ    )
      ENDIF
C
      RETURN
      END
Chd|====================================================================
Chd|  FV_IMPI                       source/constraints/general/impvel/fv_imp0.F
Chd|-- called by -----------
Chd|        UPD_INT_K                     source/implicit/upd_glob_k.F  
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE FV_IMPI(IDDL   ,IFIX  ,NDOF  ,IADK  ,JDIK ,
     1                   DIAG_K ,LT_K  ,UD    ,B     ,NDDL )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "impl1_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER
     .        IDDL(*),IFIX(*),IADK(*),JDIK(*),NDOF(*),NDDL
C     REAL ou REAL*8
      my_real
     .  UD(3,*), DIAG_K(*),LT_K(*),B(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER N, I, J, K,I1,J1,K1,ND,ID,NF,NT
C     REAL ou REAL*8
C--------update {b} due to {ud}-----
      DO I = 1,NUMNOD
      IF (NDOF(I).GT.0) THEN
       ND = IDDL(I)
       K = MIN(3,NDOF(I))
       DO J =1,K
        ID = ND + J
         IF (IFIX(ID).EQ.2.OR.IFIX(ID).EQ.9) THEN
          IF (IKPAT.EQ.0) THEN
           NF=1
           NT=ID-1
          ELSE
           NF=ID+1
           NT=NDDL
          ENDIF
          DO I1=NF,NT
           DO J1 = IADK(I1),IADK(I1+1)-1
            K1 =JDIK(J1)
            IF (K1.EQ.ID ) B(I1)=B(I1)-LT_K(J1)*UD(J,I)
           ENDDO
          ENDDO
          DO J1 = IADK(ID),IADK(ID+1)-1
            K1 =JDIK(J1)
            B(K1)=B(K1)-LT_K(J1)*UD(J,I)
          ENDDO
         ENDIF
       ENDDO
      ENDIF
      ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  FV_RW                         source/constraints/general/impvel/fv_imp0.F
Chd|-- called by -----------
Chd|        IMP_BUCK                      source/implicit/imp_buck.F    
Chd|        IMP_CHKM                      source/implicit/imp_solv.F    
Chd|        IMP_SOLV                      source/implicit/imp_solv.F    
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE FV_RW(IDDL   ,IKC   ,NDOF  ,UD    ,V )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "com08_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER
     .        IDDL(*),IKC(*),NDOF(*)
C     REAL ou REAL*8
      my_real
     .  UD(3,*), V(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER N, I, J, K,I1,J1,K1,ND,ID
C     REAL ou REAL*8
      DO I = 1,NUMNOD
       ND = IDDL(I)
       K = MIN(3,NDOF(I))
       DO J =1,K
        ID = ND + J
        IF (IKC(ID).EQ.3) UD(J,I)=V(J,I)*DT2
       ENDDO
      ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  FV_RW0                        source/constraints/general/impvel/fv_imp0.F
Chd|-- called by -----------
Chd|        UPD_GLOB_K                    source/implicit/upd_glob_k.F  
Chd|        UPD_INT_K                     source/implicit/upd_glob_k.F  
Chd|-- calls ---------------
Chd|        FV_RWL0                       source/constraints/general/rwall/srw_imp.F
Chd|====================================================================
      SUBROUTINE FV_RW0(IDDL   ,IFIX  ,NDOF  ,IADK  ,JDIK ,
     1                  DIAG_K ,LT_K  ,UD    ,B     )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER
     .        IDDL(*),IFIX(*),IADK(*),JDIK(*),NDOF(*)
C     REAL ou REAL*8
      my_real
     .  UD(3,*), DIAG_K(*),LT_K(*),B(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER N, I, J, K,I1,J1,K1,ND,ID
C     REAL ou REAL*8
      DO I = 1,NUMNOD
       ND = IDDL(I)
       K = MIN(3,NDOF(I))
       DO J =1,K
        ID = ND + J
         IF (IFIX(ID).EQ.3.OR.IFIX(ID).EQ.4.OR.
     .       IFIX(ID).EQ.10.OR.IFIX(ID).EQ.11) THEN
          DO I1=1,ID-1
           DO J1 = IADK(I1),IADK(I1+1)-1
            K1 =JDIK(J1)
            IF (K1.EQ.ID ) B(I1)=B(I1)-LT_K(J1)*UD(J,I)
           ENDDO
          ENDDO
          DO J1 = IADK(ID),IADK(ID+1)-1
            K1 =JDIK(J1)
            B(K1)=B(K1)-LT_K(J1)*UD(J,I)
          ENDDO
         ENDIF
       ENDDO
      ENDDO
      CALL FV_RWL0(IDDL   ,IFIX  ,NDOF  ,IADK  ,JDIK ,
     1             DIAG_K ,LT_K  ,UD    ,B     )
C
      RETURN
      END
Chd|====================================================================
Chd|  FV_DD0                        source/constraints/general/impvel/fv_imp0.F
Chd|-- called by -----------
Chd|        IMP_SOLV                      source/implicit/imp_solv.F    
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE FV_DD0(IDDL   ,IKC   ,NDOF  ,DD    ,DDR ,D  )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER
     .        IDDL(*),IKC(*),NDOF(*)
C     REAL ou REAL*8
      my_real
     .  DD(3,*), D(3,*),DDR(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
      INTEGER N, I, J, K,I1,J1,K1,ND,ID
C     REAL ou REAL*8
      DO I = 1,NUMNOD
       ND = IDDL(I)
       K = MIN(3,NDOF(I))
       DO J =1,K
        ID = ND + J
        IF (IKC(ID).EQ.3.OR.IKC(ID).EQ.4.OR.
     .      IKC(ID).EQ.10.OR.IKC(ID).EQ.11) THEN
         DD(J,I)=D(J,I)
c        ELSEIF (IKC(ID).EQ.2) THEN
c         DD(J,I)=ZERO
        ENDIF
       ENDDO
      ENDDO
C
      IF (IRODDL.NE.0) THEN
       DO I = 1,NUMNOD
        IF (NDOF(I).GT.3) THEN
          DO J=1,3
           ID = IDDL(I)+J+3
           IF (IKC(ID).EQ.2) DDR(J,I)=ZERO
          ENDDO
         ENDIF
       ENDDO
      ENDIF
C
      RETURN
      END
Chd|====================================================================
Chd|  DINTERI                       source/constraints/general/impvel/fv_imp0.F
Chd|-- called by -----------
Chd|        FV_IMP                        source/constraints/general/impvel/fv_imp0.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE DINTERI(TF,IAD,IPOS1,ILEN,NEL0,X1,X2,DY,ITY)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEL0,IAD(*),IPOS1(*),ILEN(*),ITY(*)
      my_real
     .  X1(*),X2(*),DY(*),TF(2,*)
C----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IPOS(NEL0)
      my_real
     .   Y1(NEL0),Y2(NEL0),DYDX,X(NEL0),Y(NEL0),
     .   V(NEL0) 
      INTEGER I,J1,J,J2,ICONT,J0,L,JJ
C------postion pour x1-----
      IF (NEL0.EQ.0) RETURN
      DO I=1,NEL0
       IF (ITY(I).LT.2) DY(I) =ZERO
       IF (ITY(I).EQ.0) V(I) = ZERO
      ENDDO
      DO I=1,NEL0
       J1 = IPOS1(I)+IAD(I)+1
       IF (X1(I).GT.TF(1,J1)) THEN
        IPOS(I) = IPOS1(I)
C--------due au divergence-----
       ELSE
        IPOS(I) = 0
        ILEN(I) = ILEN(I)+IPOS1(I)
       ENDIF
      ENDDO
      J = 0
      ICONT = 1
C
      DO WHILE(ICONT.EQ.1)
       J = J+1
       ICONT = 0
       DO I=1,NEL0
         J1 = IPOS(I)+IAD(I)+1
         IF(J.LE.ILEN(I)-1.AND.X1(I).GT.TF(1,J1))THEN
           IPOS(I)=IPOS(I)+1
           ICONT = 1
         ENDIF
       ENDDO
      ENDDO
C------interpelation pour y1--------
      DO I=1,NEL0
        J1   =IPOS(I)+IAD(I)
        J2   = J1+1
        DYDX=(TF(2,J2)-TF(2,J1))/(TF(1,J2)-TF(1,J1))
        Y1(I)   = TF(2,J1) + DYDX*(X1(I)-TF(1,J1))
        IPOS1(I) = IPOS(I)
        IF (ITY(I).LT.2) THEN
         X(I) = X1(I)
         Y(I) = Y1(I)
        ENDIF
      ENDDO
C------ Calcul de V initial quand A est imposee
      ICONT = 1
C
      JJ = 0
      DO WHILE(ICONT.EQ.1)
        JJ = JJ+1
        ICONT = 0
        DO I=1,NEL0
          J1 = IAD(I)-1+JJ
          IF (ITY(I).EQ.0) THEN
            IF (X1(I).GT.TF(1,J1+1)) THEN
              V(I) = V(I) + UNDEMI*(TF(2,J1)+TF(2,J1+1))*
     .                             (TF(1,J1+1)-TF(1,J1))
              ICONT = 1
            ELSE
              DYDX = (TF(2,J1+1)-TF(2,J1))/(TF(1,J1+1)-TF(1,J1))
              Y2(I) = TF(2,J1) + DYDX*(X2(I)-TF(1,J1))
              V(I) = V(I) + UNDEMI*(TF(2,J1)+Y1(I))*
     .                             (X1(I)-TF(1,J1))
            END IF
          ELSE
          END IF
        ENDDO
      ENDDO        
C------postion pour x2-----
      ICONT = 1
C
      DO WHILE(ICONT.EQ.1)
       J = J+1
       ICONT = 0
       DO I=1,NEL0
         J1 = IPOS(I)+IAD(I)+1
         IF(J.LE.ILEN(I).AND.X2(I).GT.TF(1,J1))THEN
           IPOS(I)=IPOS(I)+1
           ICONT = 1
          IF (ITY(I).EQ.1) THEN
           DY(I) = DY(I) + UNDEMI*(TF(2,J1)+Y(I))*
     .                            (TF(1,J1)-X(I))
           X(I) = TF(1,J1)
           Y(I) = TF(2,J1)
          ELSEIF (ITY(I).EQ.0) THEN
           DY(I) = DY(I) + V(I)*(TF(1,J1)-X(I)) +
     .     UNSIX*(DEUX*Y(I)+TF(2,J1))*(TF(1,J1)-X(I))*(TF(1,J1)-X(I))
           V(I) = V(I) + UNDEMI*(Y(I)+TF(2,J1))*(TF(1,J1)-X(I))
           X(I) = TF(1,J1)
           Y(I) = TF(2,J1)
          ENDIF
         ENDIF
       ENDDO
C
      ENDDO    
C------interpelation pour (d,v,a)--------
      DO I=1,NEL0
        J1   =IPOS(I)+IAD(I)
        J2   = J1+1      
        DYDX=(TF(2,J2)-TF(2,J1))/(TF(1,J2)-TF(1,J1))
        Y2(I)   = TF(2,J1) + DYDX*(X2(I)-TF(1,J1))
          IF (ITY(I).EQ.2) THEN
           DY(I) = Y2(I) - Y1(I)
          ELSEIF (ITY(I).EQ.1) THEN
           DY(I) = DY(I) + UNDEMI*(Y(I)+Y2(I))*(X2(I)-X(I))
          ELSEIF (ITY(I).EQ.0) THEN
           DY(I) = DY(I) + V(I)*(X2(I)-X(I)) +
     .     UNSIX*(DEUX*Y(I)+Y2(I))*(X2(I)-X(I))*(X2(I)-X(I))
          ENDIF
      ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  FV_IMPL                       source/constraints/general/impvel/fv_imp0.F
Chd|-- called by -----------
Chd|        UPD_GLOB_K                    source/implicit/upd_glob_k.F  
Chd|        UPD_INT_K                     source/implicit/upd_glob_k.F  
Chd|-- calls ---------------
Chd|        FV_UPDK                       source/constraints/general/impvel/fv_imp0.F
Chd|====================================================================
      SUBROUTINE FV_IMPL(IBFV   ,SKEW  ,XFRAME ,LJ   ,IDDL   ,
     1                   IFIX   ,NDOF  ,IADK   ,JDIK ,DIAG_K ,
     2                   LT_K   ,UD    ,RD     ,LB   )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IBFV(NIFV,*),LJ(*)
      INTEGER
     .     IDDL(*),IADK(*),JDIK(*),NDOF(*),IFIX(*)
C     REAL ou REAL*8
      my_real
     .  UD(*),RD(*), DIAG_K(*),LT_K(*),SKEW(LSKEW,*),LB(*),
     .  XFRAME(NXFRAME,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER N, I, ISK, J, L, K1, K2, K3, K,
     .        IFM, J2,J3,I1,J1,ND,ID,IR,NN
      my_real
     .   EJ(3),S
C--------
      DO N = 1,NFXVEL
        J1=LJ(N)
        IF (J1.GT.0) THEN
          I=IABS(IBFV(1,N))
          ISK=IBFV(2,N)/10
          IFM = IBFV(9,N)
          J=IBFV(2,N)
          IF (IFM.LE.1) J=J-10*ISK
          K1=3*J-2
          K2=3*J-1
          K3=3*J
            IF (ISK.GT.1) THEN
              EJ(1)=SKEW(K1,ISK)
              EJ(2)=SKEW(K2,ISK)
              EJ(3)=SKEW(K3,ISK)
            ELSE
              EJ(1)=XFRAME(K1,IFM)
              EJ(2)=XFRAME(K2,IFM)
              EJ(3)=XFRAME(K3,IFM)
            ENDIF
         IF (J1.LE.3) THEN
          IR =0
          S = UN/EJ(J1)
          DO NN =1,3
           EJ(NN) = EJ(NN)*S
          ENDDO
          CALL FV_UPDK(I     ,IDDL  ,EJ    ,J1    ,IR    ,
     1                 IADK  ,JDIK  ,DIAG_K,LT_K  ,LB    ,UD     )
         ELSE
          IR =1
          J1 = J1 -3
          S = UN/EJ(J1)
          DO NN =1,3
           EJ(NN) = EJ(NN)*S
          ENDDO
          CALL FV_UPDK(I     ,IDDL  ,EJ    ,J1    ,IR    ,
     1                 IADK  ,JDIK  ,DIAG_K,LT_K  ,LB    ,RD     )
         ENDIF
        ENDIF
      ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  FV_UPDK                       source/constraints/general/impvel/fv_imp0.F
Chd|-- called by -----------
Chd|        FV_IMPL                       source/constraints/general/impvel/fv_imp0.F
Chd|        FV_RWL0                       source/constraints/general/rwall/srw_imp.F
Chd|-- calls ---------------
Chd|        BC_UPDK                       source/constraints/general/bcs/bc_imp0.F
Chd|====================================================================
      SUBROUTINE FV_UPDK(N     ,IDDL  ,EJ    ,JJ    ,IR    ,
     1                   IADK  ,JDIK  ,DIAG_K,LT_K  ,LB    ,
     2                   UD    )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "impl1_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER N,JJ,IDDL(*),IR,IADK(*) ,JDIK(*)
      my_real
     .   EJ(*),DIAG_K(*),LT_K(*),LB(*),UD(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,ND,K,L,J1,K1,L1,ID,SHF,JFT,KFT,LFT,NL,NJ,
     .        IT(6),KK
      my_real
     .   S
C-----------------------------------------------
      K = JJ + 1
      L = JJ + 2
      IF (K.GT.3) K = K - 3
      IF (L.GT.3) L = L - 3
      IF (EJ(K).EQ.ZERO.AND.EJ(L).EQ.ZERO) RETURN
      CALL BC_UPDK(N     ,IDDL  ,EJ    ,JJ    ,IR    ,
     1             IADK  ,JDIK  ,DIAG_K,LT_K  )
      IF (IR.EQ.0) THEN
       J1 = JJ
       K1 = K
       L1 = L
      ELSE
       J1 = JJ + 3
       K1 = K + 3
       L1 = L + 3
      ENDIF
      IF (IMCONV.NE.1) RETURN
      ID = IDDL(N)
      S = -DIAG_K(ID+J1)*UD(JJ,N)
      LB(ID+K1)=LB(ID+K1)-EJ(K)*S
      LB(ID+L1)=LB(ID+L1)-EJ(L)*S
C
      RETURN
      END
Chd|====================================================================
Chd|  FV_IMPD                       source/constraints/general/impvel/fv_imp0.F
Chd|-- called by -----------
Chd|        FR_U2DD                       source/mpi/implicit/imp_fri.F 
Chd|        IMP3_U2X                      source/airbag/monv_imp0.F     
Chd|        RECUKIN                       source/implicit/recudis.F     
Chd|-- calls ---------------
Chd|        BC_UPDD                       source/constraints/general/bcs/bc_imp0.F
Chd|        BC_UPDD2                      source/constraints/general/bcs/bc_imp0.F
Chd|====================================================================
      SUBROUTINE FV_IMPD(IBFV  ,LJ    ,SKEW  ,XFRAME,UD    ,
     1                   RD     )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IBFV(NIFV,*),LJ(*)
C     REAL ou REAL*8
      my_real
     .  UD(3,*),RD(3,*),SKEW(LSKEW,*),XFRAME(NXFRAME,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER N, I, ISK, J, L, K1, K2, K3, ISENS,K,
     .        II, NN, NR, NSK, NFK, IFM,
     .        I1,J1,ND,ID,ITAG(NFXVEL),N2,J2,N1,NL
C     REAL ou REAL*8
      my_real
     .   EJ(3),EJ1(3),S,S1,S2
C---------------------------------------------------------------
      NL = 0
      DO N=1,NFXVEL
       ITAG(N)=0
      ENDDO
      DO N=1,NFXVEL
       IF (LJ(N).GT.0.AND.ITAG(N).GE.0) THEN
        NL = 1
        ITAG(N)=N
        I=IABS(IBFV(1,N))
        DO N1=N+1,NFXVEL
         IF (LJ(N1).GT.0.AND.IABS(IBFV(1,N1)).EQ.I) THEN
           NN =IABS(LJ(N1)-LJ(N))
           IF (NN.GT.0.AND.NN.LT.3) THEN
            ITAG(N)=N1
            ITAG(N1)=-N
           ENDIF
         ENDIF
        ENDDO
       ENDIF
      ENDDO
C
      IF (NL.EQ.0) RETURN
C
      DO N=1,NFXVEL
       IF (ITAG(N).EQ.N) THEN
          I=IABS(IBFV(1,N))
          ISK=IBFV(2,N)/10
          IFM = IBFV(9,N)
          J=IBFV(2,N)
          IF (IFM.LE.1) J=J-10*ISK
          IF(J.LE.3) THEN
           ND = 0
          ELSEIF(J.LE.6) THEN
           ND = 3
           J = J- 3
          ENDIF
           K1=3*J-2
           K2=3*J-1
           K3=3*J
            IF (ISK.GT.1) THEN
              EJ(1)=SKEW(K1,ISK)
              EJ(2)=SKEW(K2,ISK)
              EJ(3)=SKEW(K3,ISK)
            ELSE
              EJ(1)=XFRAME(K1,IFM)
              EJ(2)=XFRAME(K2,IFM)
              EJ(3)=XFRAME(K3,IFM)
            ENDIF
            J1 = LJ(N)
            S = UN/EJ(J1)
            DO NN =1,3
             EJ(NN) = EJ(NN)*S
            ENDDO
            IF (ND .EQ.0 ) THEN
C	     UD(J1,I)=UD(J1,I)*S*S
             CALL BC_UPDD(I     ,EJ   ,J1     ,UD     )
            ELSE
c	     RD(J1,I)=RD(J1,I)*S*S
             CALL BC_UPDD(I     ,EJ   ,J1     ,RD     )
            ENDIF
       ELSEIF (ITAG(N).GT.0) THEN
C-------traite le cas ou il y a deux directions sont imposees-----
          N1 = ITAG(N)
          I=IABS(IBFV(1,N))
           ISK=IBFV(2,N)/10
           IFM = IBFV(9,N)
           J=IBFV(2,N)
           IF (IFM.LE.1) J=J-10*ISK
           IF(J.LE.3) THEN
            ND = 0
           ELSEIF(J.LE.6) THEN
            ND = 3
            J = J- 3
           ENDIF
           K1=3*J-2
           K2=3*J-1
           K3=3*J
            IF (ISK.GT.1) THEN
              EJ(1)=SKEW(K1,ISK)
              EJ(2)=SKEW(K2,ISK)
              EJ(3)=SKEW(K3,ISK)
            ELSE
              EJ(1)=XFRAME(K1,IFM)
              EJ(2)=XFRAME(K2,IFM)
              EJ(3)=XFRAME(K3,IFM)
            ENDIF
            J1 = LJ(N)
            S1 = UN/EJ(J1)
            DO NN =1,3
             EJ(NN) = EJ(NN)*S1
            ENDDO
C
           ISK = IBFV(2,N1)/10
           IFM = IBFV(9,N1)
           J = IBFV(2,N1)
           IF (IFM.LE.1) J=J-10*ISK
           IF(J.GT.3) J = J- 3
           K1=3*J-2
           K2=3*J-1
           K3=3*J
            IF (ISK.GT.1) THEN
              EJ1(1)=SKEW(K1,ISK)
              EJ1(2)=SKEW(K2,ISK)
              EJ1(3)=SKEW(K3,ISK)
            ELSE
              EJ1(1)=XFRAME(K1,IFM)
              EJ1(2)=XFRAME(K2,IFM)
              EJ1(3)=XFRAME(K3,IFM)
            ENDIF
            J2 = LJ(N1)
            S2 = UN/EJ1(J2)
            DO NN =1,3
             EJ1(NN) = EJ1(NN)*S2
            ENDDO
           IF (ND.EQ.0) THEN
C	     UD(J1,I)=UD(J1,I)*S1*S1
C	     UD(J2,I)=UD(J2,I)*S2*S2
            CALL BC_UPDD2(I   ,EJ    ,J1   ,EJ1    ,J2   ,UD    )
           ELSEIF (ND.EQ.3) THEN
C	     RD(J1,I)=RD(J1,I)*S1*S1
C	     RD(J2,I)=RD(J2,I)*S2*S2
            CALL BC_UPDD2(I   ,EJ    ,J1   ,EJ1    ,J2   ,RD    )
           ENDIF
       ENDIF
      ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  FVL_MODIF                     source/constraints/general/impvel/fv_imp0.F
Chd|-- called by -----------
Chd|        FV_IMP                        source/constraints/general/impvel/fv_imp0.F
Chd|-- calls ---------------
Chd|        L_DIR02                       source/constraints/general/impvel/fv_imp0.F
Chd|====================================================================
      SUBROUTINE FVL_MODIF(NVL   ,IBFV  ,UD    ,RD   ,IFIX  ,
     2                     IDDL  ,SKEW  ,XFRAME,VL   ,LJ    )
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
      INTEGER NVL,IBFV(NIFV,*),IFIX(*),IDDL(*),LJ(*)
C     REAL ou REAL*8
      my_real
     .  UD(3,*), SKEW(LSKEW,*),RD(3,*),VL(*),XFRAME(NXFRAME,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IVL(NVL),I,J,II,N,ND,JJ,ISK,IFM,
     .        NN,K,L,J1,K1,K2,K3,L1,N1,N2,N3,NL
      INTEGER NLFV(NVL),ITAG(NUMNOD)
      my_real
     .   EJ(3)
C-----------------------------------------------
       N1 = 0
       DO N=1,NFXVEL
        IF (LJ(N).GT.0) THEN
         N1 = N1 + 1
         IVL(N1) = N
        ENDIF
       ENDDO
       IF (N1.NE.NVL) WRITE(*,*)'ERROR IN FVL_MODIF',N1,NVL
C
C------determine NL (raison for NL=3)save into NLFV
C----first translation
       DO I=1,NUMNOD
        ITAG(I)=0
       END DO
       DO I=1,NVL
        N = IVL(I)
        J = LJ(N)
        II=IABS(IBFV(1,N))
        IF (J.GT.3) CYCLE
        ITAG(II)= ITAG(II)+1
       ENDDO
       DO I=1,NVL
        N = IVL(I)
        J = LJ(N)
        II=IABS(IBFV(1,N))
        IF (J.GT.3) CYCLE
        NLFV(I)= ITAG(II)
       ENDDO
C----rotational---------
       DO I=1,NUMNOD
        ITAG(I)=0
       END DO
       DO I=1,NVL
        N = IVL(I)
        J = LJ(N)
        II=IABS(IBFV(1,N))
        IF (J.LE.3) CYCLE
        ITAG(II)= ITAG(II)+1
       ENDDO
       DO I=1,NVL
        N = IVL(I)
        J = LJ(N)
        II=IABS(IBFV(1,N))
        IF (J.LE.3) CYCLE
        NLFV(I)= ITAG(II)
       ENDDO
C---------------------------------
       N2 = 0
       N3 = 0
       DO I=1,NVL
        N = IVL(I)
        J = LJ(N)
        II=IABS(IBFV(1,N))
        ND = IDDL(II)
        IF (J.GT.3) ND = ND +3
        NL = NLFV(I)
        DO NN =1,3
         K1 = ND+NN
         IF (IFIX(K1).EQ.9) IFIX(K1)=0
        ENDDO
        IF (NL.EQ.1) THEN
C          IFIX(ND+J)=0
          ISK=IBFV(2,N)/10
          IFM = IBFV(9,N)
          IF(J.LE.3)THEN
            IF (ISK.GT.1) THEN
              K1=3*J-2
              K2=3*J-1
              K3=3*J
              EJ(1)=SKEW(K1,ISK)
              EJ(2)=SKEW(K2,ISK)
              EJ(3)=SKEW(K3,ISK)
            ELSEIF (IFM.GT.1) THEN
              K1=3*J-2
              K2=3*J-1
              K3=3*J
              EJ(1)=XFRAME(K1,IFM)
              EJ(2)=XFRAME(K2,IFM)
              EJ(3)=XFRAME(K3,IFM)
            ENDIF
            CALL L_DIR02(EJ,J1,J,IFIX(ND+1))
            UD(J1,II)=VL(N)/EJ(J1)
            ND = IDDL(II)+J1
            IFIX(ND)=9
            LJ(N) = J1
            ND=IDDL(II)
          ELSEIF(J.LE.6)THEN
           J = J - 3
            IF (ISK.GT.1) THEN
              K1=3*J-2
              K2=3*J-1
              K3=3*J
              EJ(1)=SKEW(K1,ISK)
              EJ(2)=SKEW(K2,ISK)
              EJ(3)=SKEW(K3,ISK)
            ELSEIF (IFM.GT.1) THEN
              K1=3*J-2
              K2=3*J-1
              K3=3*J
              EJ(1)=XFRAME(K1,IFM)
              EJ(2)=XFRAME(K2,IFM)
              EJ(3)=XFRAME(K3,IFM)
            ENDIF
            CALL L_DIR02(EJ,J1,J,IFIX(ND+4))
            RD(J1,II)=VL(N)/EJ(J1)
            ND = IDDL(II)+J1+3
            IFIX(ND)=9
            LJ(N) = J1 + 3
          ENDIF
        ELSEIF (NL.EQ.3) THEN
          DO NN =1,3
           K1 = ND+NN
           IFIX(K1)=2
          ENDDO
          IF (J.LE.3) THEN
           DO NN =1,3
            UD(NN,II)=ZERO
           ENDDO
          ELSE
           DO NN =1,3
            RD(NN,II)=ZERO
           ENDDO
          ENDIF
          LJ(N)=-J
          N3 = N3 + 1
        ELSE
C--------NL=2 to be traited later---------------------
         N2 = N2 + 1
        ENDIF
       ENDDO
C--------change to global system ---------------------
      IF (N3 .GE. 3) THEN
       DO I=1,NVL
        N = IVL(I)
        J = -LJ(N)
        IF (J.GT.0) THEN
          II=IABS(IBFV(1,N))
          ISK=IBFV(2,N)/10
          IFM = IBFV(9,N)
          J1=J
          IF (J1 > 3) J1=J1-3
          K1=3*J1-2
          K2=3*J1-1
          K3=3*J1
          IF (ISK.GT.1) THEN
              EJ(1)=SKEW(K1,ISK)
              EJ(2)=SKEW(K2,ISK)
              EJ(3)=SKEW(K3,ISK)
          ELSE
              EJ(1)=XFRAME(K1,IFM)
              EJ(2)=XFRAME(K2,IFM)
              EJ(3)=XFRAME(K3,IFM)
          ENDIF
          IF (J.LE.3) THEN
           DO NN =1,3
            UD(NN,II)=UD(NN,II)+VL(N)*EJ(NN)
           ENDDO
          ELSE
           DO NN =1,3
            RD(NN,II)=RD(NN,II)+VL(N)*EJ(NN)
           ENDDO
          ENDIF
          LJ(N)=0
        ENDIF
       ENDDO
      ENDIF
C--------VL dans 2 directions--------
      IF (N2 .EQ. 0) RETURN
       N2 =0
       DO I=1,NVL
        N = IVL(I)
        J = LJ(N)
        IF (J.GT.0) THEN
         II=IABS(IBFV(1,N))
         ND = IDDL(II)
        IF (J.GT.3) ND = ND +3
        NL = NLFV(I)
C         DO NN =1,3
C          K1 = ND+NN
C          IF (IFIX(K1).EQ.9) NL=NL+1
C         ENDDO
         IF (NL.EQ.2) THEN
C---------premier--------
C          IFIX(ND+J)=0
          ISK=IBFV(2,N)/10
          IFM = IBFV(9,N)
          IF(J.LE.3)THEN
            IF (ISK.GT.1) THEN
              K1=3*J-2
              K2=3*J-1
              K3=3*J
              EJ(1)=SKEW(K1,ISK)
              EJ(2)=SKEW(K2,ISK)
              EJ(3)=SKEW(K3,ISK)
            ELSEIF (IFM.GT.1) THEN
              K1=3*J-2
              K2=3*J-1
              K3=3*J
              EJ(1)=XFRAME(K1,IFM)
              EJ(2)=XFRAME(K2,IFM)
              EJ(3)=XFRAME(K3,IFM)
            ENDIF
            CALL L_DIR02(EJ,J1,J,IFIX(ND+1))
            UD(J1,II)=VL(N)/EJ(J1)
            IFIX(IDDL(II)+J1)=9
            LJ(N) = J1
          ELSEIF(J.LE.6)THEN
           J = J - 3
            IF (ISK.GT.1) THEN
              K1=3*J-2
              K2=3*J-1
              K3=3*J
              EJ(1)=SKEW(K1,ISK)
              EJ(2)=SKEW(K2,ISK)
              EJ(3)=SKEW(K3,ISK)
            ELSEIF (IFM.GT.1) THEN
              K1=3*J-2
              K2=3*J-1
              K3=3*J
              EJ(1)=XFRAME(K1,IFM)
              EJ(2)=XFRAME(K2,IFM)
              EJ(3)=XFRAME(K3,IFM)
            ENDIF
            CALL L_DIR02(EJ,J1,J,IFIX(ND+4))
            RD(J1,II)=VL(N)/EJ(J1)
            J1 = J1 + 3
            IFIX(IDDL(II)+J1)=9
            LJ(N) = J1
C----------for the second
           J = J - 3
          ENDIF
C---------seconde--------
           DO K=I+1,NVL
            N1 = IVL(K)
            NN=IABS(IBFV(1,N1))
            JJ = IABS(LJ(N1)-J)
            IF (LJ(N1)>0.AND.II.EQ.NN.AND.JJ.LT.3) GOTO 100
           ENDDO
 100       CONTINUE
          J = LJ(N1)
          IF (J.NE.J1) IFIX(ND+J)=0
          ISK=IBFV(2,N1)/10
          IFM = IBFV(9,N1)
          IF(J.LE.3)THEN
            IF (ISK.GT.1) THEN
              K1=3*J-2
              K2=3*J-1
              K3=3*J
              EJ(1)=SKEW(K1,ISK)
              EJ(2)=SKEW(K2,ISK)
              EJ(3)=SKEW(K3,ISK)
            ELSEIF (IFM.GT.1) THEN
              K1=3*J-2
              K2=3*J-1
              K3=3*J
              EJ(1)=XFRAME(K1,IFM)
              EJ(2)=XFRAME(K2,IFM)
              EJ(3)=XFRAME(K3,IFM)
            ENDIF
            CALL L_DIR02(EJ,JJ,J,IFIX(ND+1))
            UD(JJ,NN)=VL(N1)/EJ(JJ)
            IFIX(IDDL(NN)+JJ)=9
            LJ(N1) = -JJ
          ELSEIF(J.LE.6)THEN
           J = J - 3
            IF (ISK.GT.1) THEN
              K1=3*J-2
              K2=3*J-1
              K3=3*J
              EJ(1)=SKEW(K1,ISK)
              EJ(2)=SKEW(K2,ISK)
              EJ(3)=SKEW(K3,ISK)
            ELSEIF (IFM.GT.1) THEN
              K1=3*J-2
              K2=3*J-1
              K3=3*J
              EJ(1)=XFRAME(K1,IFM)
              EJ(2)=XFRAME(K2,IFM)
              EJ(3)=XFRAME(K3,IFM)
            ENDIF
            CALL L_DIR02(EJ,JJ,J,IFIX(ND+4))
            RD(JJ,NN)=VL(N)/EJ(JJ)
            JJ = JJ + 3
            IFIX(IDDL(NN)+JJ)=9
            LJ(N1) = -JJ
          ENDIF
         ENDIF
        ENDIF
       ENDDO
C
       DO I=1,NVL
        N = IVL(I)
        IF (LJ(N).LT.0) LJ(N) = -LJ(N)
       ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  FV_UPDF                       source/constraints/general/impvel/fv_imp0.F
Chd|-- called by -----------
Chd|        IMP3_A2B                      source/airbag/monv_imp0.F     
Chd|        UPD_FR                        source/mpi/implicit/imp_fri.F 
Chd|-- calls ---------------
Chd|        KIN_UPDF                      source/constraints/general/impvel/fv_imp0.F
Chd|====================================================================
      SUBROUTINE FV_UPDF(NFX   ,IFX   ,IBFV  ,SKEW  ,XFRAME,
     1                   A     )
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
      INTEGER NFX   ,IFX(2,*)  ,IBFV(NIFV,*)
      my_real
     .  A(3,*),SKEW(LSKEW,*),XFRAME(NXFRAME,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,N,K,L,J1,K1,L1,K2,K3,II,ISK,IFM,NN
      my_real
     .   EJ(3),S
C-----------------------------------------------
        DO II=1,NFX
         I = IFX(1,II)
         N = IABS(IBFV(1,I))
         ISK=IBFV(2,I)/10
         IFM = IBFV(9,I)
         J=IBFV(2,I)
         IF (IFM.LE.1) J=J-10*ISK
          K1=3*J-2
          K2=3*J-1
          K3=3*J
            IF (ISK.GT.1) THEN
              EJ(1)=SKEW(K1,ISK)
              EJ(2)=SKEW(K2,ISK)
              EJ(3)=SKEW(K3,ISK)
            ELSE
              EJ(1)=XFRAME(K1,IFM)
              EJ(2)=XFRAME(K2,IFM)
              EJ(3)=XFRAME(K3,IFM)
            ENDIF
          J1 = IFX(2,II)
          S = UN/EJ(J1)
          DO NN =1,3
           EJ(NN) = EJ(NN)*S
          ENDDO
          IF(J.LE.3)THEN
            CALL KIN_UPDF(N    ,EJ    ,J1    ,A     )
          ENDIF
        ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  KIN_UPDF                      source/constraints/general/impvel/fv_imp0.F
Chd|-- called by -----------
Chd|        FV_UPDF                       source/constraints/general/impvel/fv_imp0.F
Chd|        IMP3_A2B                      source/airbag/monv_imp0.F     
Chd|        UPD_FR                        source/mpi/implicit/imp_fri.F 
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE KIN_UPDF(N    ,EJ    ,J1    ,A     )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER N   ,J1
      my_real
     .  EJ(3),A(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER K,L
C-----------------------------------------------
          K = J1 + 1
          IF (K.GT.3) K = K - 3
          L = J1 + 2
          IF (L.GT.3) L = L - 3
          A(K,N)=A(K,N)-EJ(K)*A(J1,N)
          A(L,N)=A(L,N)-EJ(L)*A(J1,N)
C
      RETURN
      END
Chd|====================================================================
Chd|  FV_UPDKD                      source/constraints/general/impvel/fv_imp0.F
Chd|-- called by -----------
Chd|        BCL_IMPKD                     source/constraints/general/bcs/bc_imp0.F
Chd|        DIAG_INT                      source/mpi/implicit/imp_fri.F 
Chd|        UPDK_MV                       source/airbag/monv_imp0.F     
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE FV_UPDKD(EJ    ,J     ,KDD   ,DIAG_K)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER J
      my_real
     .   EJ(3),DIAG_K(3)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,ND,K,L,J1,K1,L1
      my_real
     .   KDD(3,3)
C-----------------------------------------------
      K = J + 1
      L = J + 2
      IF (K.GT.3) K = K - 3
      IF (L.GT.3) L = L - 3
      DIAG_K(K)=DIAG_K(K)-(DEUX*KDD(K,J)-KDD(J,J)*EJ(K))*EJ(K)
      DIAG_K(L)=DIAG_K(L)-(DEUX*KDD(L,J)-KDD(J,J)*EJ(L))*EJ(L)
C
      RETURN
      END
Chd|====================================================================
Chd|  FV_IMPRL                      source/constraints/general/impvel/fv_imp0.F
Chd|-- called by -----------
Chd|        EXT_RHS                       source/implicit/upd_glob_k.F  
Chd|        UPD_RHS                       source/implicit/upd_glob_k.F  
Chd|-- calls ---------------
Chd|        BC_UPDB                       source/constraints/general/bcs/bc_imp0.F
Chd|====================================================================
      SUBROUTINE FV_IMPRL(IBFV   ,SKEW  ,XFRAME ,LJ   ,IDDL   ,
     1                    NDOF   ,LB    )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IBFV(NIFV,*),LJ(*),IDDL(*),NDOF(*)
C     REAL ou REAL*8
      my_real
     .  SKEW(LSKEW,*),LB(*),XFRAME(NXFRAME,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER N, I, ISK, J, L, K1, K2, K3, K,
     .        IFM, J2,J3,I1,J1,ND,ID,IR,NN
      my_real
     .   EJ(3),S
C--------
      DO N = 1,NFXVEL
        J1=LJ(N)
        IF (J1.GT.0) THEN
          I=IABS(IBFV(1,N))
          ISK=IBFV(2,N)/10
          IFM = IBFV(9,N)
          J=IBFV(2,N)
          IF (IFM.LE.1) J=J-10*ISK
          K1=3*J-2
          K2=3*J-1
          K3=3*J
          ID=IDDL(I)
            IF (ISK.GT.1) THEN
              EJ(1)=SKEW(K1,ISK)
              EJ(2)=SKEW(K2,ISK)
              EJ(3)=SKEW(K3,ISK)
            ELSE
              EJ(1)=XFRAME(K1,IFM)
              EJ(2)=XFRAME(K2,IFM)
              EJ(3)=XFRAME(K3,IFM)
            ENDIF
         IF (J1.LE.3) THEN
          IR =0
          S = UN/EJ(J1)
          DO NN =1,3
           EJ(NN) = EJ(NN)*S
          ENDDO
          CALL BC_UPDB(ID    ,EJ    ,J1    ,IR    ,LB    )
         ELSE
          IR =1
          J1 = J1 -3
          S = UN/EJ(J1)
          DO NN =1,3
           EJ(NN) = EJ(NN)*S
          ENDDO
          CALL BC_UPDB(ID    ,EJ    ,J1    ,IR    ,LB    )
         ENDIF
        ENDIF
      ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  FVL_FRK                       source/constraints/general/impvel/fv_imp0.F
Chd|-- called by -----------
Chd|        UPD_KML                       source/mpi/implicit/imp_fri.F 
Chd|        UPD_KSL                       source/mpi/implicit/imp_fri.F 
Chd|-- calls ---------------
Chd|        FV_UPDFR                      source/constraints/general/impvel/fv_imp0.F
Chd|====================================================================
      SUBROUTINE FVL_FRK(J1     ,N     ,IBFV  ,SKEW  ,XFRAME ,
     1                  IDDL    ,IDDLM ,IKC   ,IADK  ,JDIK   ,
     2                  DIAG_K  ,LT_K  ,UD    ,LB    ,A      ,
     3                  KSS     ,KSM   ,IDLM  ,IFSS  ,IFSM  )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER N,IBFV(NIFV,*),J1,IDLM  ,IFSS  ,IFSM
      INTEGER
     .     IDDL(*),IDDLM(*),IADK(*),JDIK(*),IKC(*)
C     REAL ou REAL*8
      my_real
     .  UD(3,*),DIAG_K(*),LT_K(*),SKEW(LSKEW,*),LB(*),
     .  XFRAME(NXFRAME,*),A(3,*),KSS(6),KSM(9)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, ISK, J, L, K1, K2, K3, K,
     .        IFM, J2,J3,I1,ND,ID,IR,NN
      my_real
     .   EJ(3),S
C--------
          I=IABS(IBFV(1,N))
          ISK=IBFV(2,N)/10
          IFM = IBFV(9,N)
          J=IBFV(2,N)
          IF (IFM.LE.1) J=J-10*ISK
          K1=3*J-2
          K2=3*J-1
          K3=3*J
            IF (ISK.GT.1) THEN
              EJ(1)=SKEW(K1,ISK)
              EJ(2)=SKEW(K2,ISK)
              EJ(3)=SKEW(K3,ISK)
            ELSE
              EJ(1)=XFRAME(K1,IFM)
              EJ(2)=XFRAME(K2,IFM)
              EJ(3)=XFRAME(K3,IFM)
            ENDIF
          S = UN/EJ(J1)
          DO NN =1,3
           EJ(NN) = EJ(NN)*S
          ENDDO
          CALL FV_UPDFR(I     ,EJ    ,J1    ,IDDL  ,IDDLM  ,
     1                  IKC   ,IADK  ,JDIK  ,DIAG_K,LT_K   ,
     2                  UD    ,LB    ,A     ,KSS   ,KSM    ,
     3                  IDLM  ,IFSS  ,IFSM  )
C
      RETURN
      END
Chd|====================================================================
Chd|  FV_UPDFR                      source/constraints/general/impvel/fv_imp0.F
Chd|-- called by -----------
Chd|        FVL_FRK                       source/constraints/general/impvel/fv_imp0.F
Chd|        UPD_KML                       source/mpi/implicit/imp_fri.F 
Chd|        UPD_KSL                       source/mpi/implicit/imp_fri.F 
Chd|-- calls ---------------
Chd|        BC_UPDFR                      source/constraints/general/bcs/bc_imp0.F
Chd|====================================================================
      SUBROUTINE FV_UPDFR(N      ,EJ    ,J1     ,IDDL    ,IDDLM ,
     1                   IKC     ,IADK  ,JDIK   ,DIAG_K  ,LT_K  ,
     2                   UD      ,LB    ,A      ,KSS     ,KSM   ,
     3                   IDLM    ,IFSS  ,IFSM)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER N,J1,IDLM    ,IFSS  ,IFSM
      INTEGER
     .     IDDL(*),IDDLM(*),IADK(*),JDIK(*),IKC(*)
C     REAL ou REAL*8
      my_real
     .  UD(3,*),DIAG_K(*),LT_K(*),LB(*),EJ(3),
     .  A(3,*),KSS(6),KSM(9)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER L, K,ID,IDM
      my_real
     .   S
C--------
          CALL BC_UPDFR(N     ,IDDL  ,EJ    ,J1    ,IDDLM  ,
     1                  IKC   ,IADK  ,JDIK  ,DIAG_K,LT_K   ,
     2                  LB    ,A     ,KSS   ,KSM   ,IDLM   ,
     3                  IFSS  ,IFSM  )
       IF (IFSS.GT.0) THEN
        S = -KSS(J1)*UD(J1,N)
        ID = IDDL(N)
        IDM = IDDLM(N)
        K = J1 + 1
        L = J1 + 2
        IF (K.GT.3) K = K - 3
        IF (L.GT.3) L = L - 3
        IF(IKC(ID+K).EQ.0) LB(IDM+K)=LB(IDM+K)-EJ(K)*S
        IF(IKC(ID+L).EQ.0) LB(IDM+L)=LB(IDM+L)-EJ(L)*S
       ENDIF
C
      RETURN
      END
Chd|====================================================================
Chd|  WFV_IMP                       source/constraints/general/impvel/fv_imp0.F
Chd|-- called by -----------
Chd|        DYNA_WEX                      source/implicit/imp_dyna.F    
Chd|-- calls ---------------
Chd|        DINTERA                       source/constraints/general/impvel/fv_imp0.F
Chd|        IMP_DYNA                      share/modules/impbufdef_mod.F 
Chd|====================================================================
      SUBROUTINE WFV_IMP(IBFV  ,NPC    ,TF   ,VEL   ,SENSOR,
     1                  LSENSOR,UD     ,RD   ,IFIX  ,IDDL  ,
     2                  SKEW   ,IFRAME ,XFRAME,A    ,AR    ,
     3                  X      ,NDOF  ,MS   ,IN    ,WEIGHT ,
     4                  RBY    ,DW    )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMP_DYNA
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "param_c.inc"
#include      "task_c.inc"
#include      "scr03_c.inc"
#include      "scr05_c.inc"
Ctmp+1
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NPC(*),IBFV(NIFV,*),LSENSOR(NSENSI,*),
     .        IFIX(*),IDDL(*),IFRAME(LISKN,*),NDOF(*),WEIGHT(*)
C     REAL ou REAL*8
      my_real
     .  TF(*), VEL(LFXVELR,*), UD(3,*), SENSOR(NSENSR,*),
     .  SKEW(LSKEW,*),RD(3,*),A(3,*),AR(3,*),IN(*),
     .  X(3,*),XFRAME(NXFRAME,*),DW,MS(*),RBY(NRBY,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER N, I, ISK, J, L, K1, K2, K3, ISENS,K,
     .        II, IC, NN, IDEB, NR, NSK, NFK, IFM, N0,
     .        INDEX(MVSIZ),I1,J1,ND,ID,J2,J3,
     .        N1,N2,N3,NVL
C     REAL ou REAL*8
      my_real
     .   FAC, STARTT, STOPT, TS,
     .   RX,RY,RZ,VF,VFX,VFY,VFZ,
     .   FACX,FINT,A0,IN0,DD
      INTEGER ILENC(MVSIZ), IPOSC(MVSIZ), IADC(MVSIZ),
     .        LC(MVSIZ)
      my_real
     .   YC(MVSIZ), TSC(MVSIZ), DYDXC(MVSIZ),
     .   TSC1(MVSIZ)
C IBFV(7,N):1 V;2 D ;0 A ;
C-------------------------------
C--A =DY_AR, AR=DY_AR as input ;
C---DY_A is not precise w/ imposed (u,v,a) Correction for DY_V,DY_D, DY_A
      IDEB = 0
C
      DW = 0
      DO NN=1,NFXVEL,NVSIZ
        IF (IBFV(8,NN).EQ.1) GOTO 100
        IC = 0
        IF (NSENSOR.GT.0) THEN
          DO 10 II = 1, MIN(NFXVEL-IDEB,NVSIZ)
            N = II+IDEB
            STARTT = VEL(2,N)
            STOPT  = VEL(3,N)
            IF(TT.LT.STARTT)GOTO 10
            IF(TT.GT.STOPT) GOTO 10
            I=IABS(IBFV(1,N))
            IF(NDOF(I).EQ.0) GOTO 10
            ISENS=0
            DO K=1,NSENSOR
              IF(IBFV(4,N).EQ.LSENSOR(1,K))ISENS=K
            ENDDO
            IF(ISENS.EQ.0)THEN
              TS=TT
            ELSE
              TS = TT-SENSOR(2,ISENS)
              IF(TS.LT.ZERO)GOTO 10
            ENDIF
            IC = IC + 1
            INDEX(IC) = N
             TSC(IC) = TS
             TSC1(IC) = TSC(IC)-DT2
 10       CONTINUE
        ELSE
          DO 20 II = 1, MIN(NFXVEL-IDEB,NVSIZ)
            N = II+IDEB
            STARTT = VEL(2,N)
            STOPT  = VEL(3,N)
            IF(TT.LT.STARTT)GOTO 20
            IF(TT.GT.STOPT) GOTO 20
            I=IABS(IBFV(1,N))
            IF(NDOF(I).EQ.0) GOTO 20
            IC = IC + 1
            INDEX(IC) = N
             TSC(IC) = TT
             TSC1(IC) = TSC(IC)-DT2
 20       CONTINUE
        ENDIF
C
        IDEB = IDEB + MIN(NFXVEL-IDEB,NVSIZ)
C
         DO II=1,IC
          N = INDEX(II)
          FACX   = VEL(5,N)
          TSC(II) = FACX*TSC(II)
          TSC1(II) = FACX*TSC1(II)
         ENDDO
C
         DO II=1,IC
          N = INDEX(II)
          L = IBFV(3,N)
          LC(II) = IBFV(7,N)
C
          IPOSC(II) = IBFV(5,N)
          IADC(II) = UNDEMI * NPC(L) + 1
          ILENC(II) = UNDEMI * NPC(L+1) - IADC(II) - IPOSC(II)
         ENDDO
C
        CALL DINTERA(TF,IADC,IPOSC,ILENC,IC,TSC1,TSC,YC,LC)
         DO II=1,IC
          N = INDEX(II)
          FAC  = VEL(1,N)
          YC(II)  = YC(II) * FAC
          FACX   = VEL(5,N)
          IF(IBFV(7,N).LT.2) YC(II)  = YC(II) / FACX
          IF(IBFV(7,N).EQ.0) YC(II)  = YC(II) / FACX
          I=IABS(IBFV(1,N))
          ISK=IBFV(2,N)/10
          IFM = IBFV(9,N)
          J=IBFV(2,N)
          IF (IFM.LE.1) J=J-10*ISK
          FINT = VEL(4,N)
          IF(J.LE.3)THEN
            IF(ISK.LE.1.AND.IFM.LE.1)THEN
            ELSEIF (ISK.GT.1) THEN
            ELSEIF (IFM.GT.1) THEN
            ENDIF
C---Correction for DY_V,DY_D
            DY_V(J,I) = DY_V(J,I)+(YC(II)-A(J,I))*DT2
            DY_D(J,I) =DT2*(DY_V(J,I)+(DY_G-DY_B-UNDEMI)*YC(II)*DT2)
	        A(J,I) = YC(II)
            IF (TT.LE.DT2) THEN
             DY_V(J,I) = UD(J,I)/DT2
             DY_D(J,I) = UD(J,I)
             YC(II) = UNDEMI*DY_V(J,I)/DT2
	        END IF
            DW   = DW + UD(J,I)*(MS(I)*WEIGHT(I)*YC(II)-FINT)
          ELSEIF(J.LE.6)THEN
           J1 = J
           J = J - 3
c
            IF(ISK.LE.1.AND.IFM.LE.1)THEN
              IF(IBFV(6,N).EQ.0)THEN
                IN0=IN(I)*WEIGHT(I)
              ELSE
                NR = IBFV(6,N)
                IN0= WEIGHT(I)*
     .          (RBY(16+J,NR) + RBY(19+J,NR) + RBY(22+J,NR))
              ENDIF
            ELSEIF (ISK.GT.1) THEN
              IF(IBFV(6,N).EQ.0)THEN
                IN0=IN(I)*WEIGHT(I)
              ELSE
                NR = IBFV(6,N)
                K1=3*J-2
                K2=3*J-1
                K3=3*J
                IN0=WEIGHT(I)*
     .             ((RBY(17,NR)*SKEW(K1,ISK)
     .              +RBY(18,NR)*SKEW(K2,ISK)
     .              +RBY(19,NR)*SKEW(K3,ISK))*SKEW(K1,ISK) +
     .              (RBY(20,NR)*SKEW(K1,ISK)
     .              +RBY(21,NR)*SKEW(K2,ISK)
     .              +RBY(22,NR)*SKEW(K3,ISK))*SKEW(K2,ISK) +
     .              (RBY(23,NR)*SKEW(K1,ISK)
     .              +RBY(24,NR)*SKEW(K2,ISK)
     .              +RBY(25,NR)*SKEW(K3,ISK))*SKEW(K3,ISK))
              ENDIF
            ELSEIF (IFM.GT.1) THEN
              IF(IBFV(6,N).EQ.0)THEN
                IN0=IN(I)*WEIGHT(I)
              ELSE
                NR = IBFV(6,N)
                K1=3*J-2
                K2=3*J-1
                K3=3*J
                IN0= WEIGHT(I)*
     .             ((RBY(17,NR)*XFRAME(K1,IFM)
     .              +RBY(18,NR)*XFRAME(K2,IFM)
     .              +RBY(19,NR)*XFRAME(K3,IFM))*XFRAME(K1,IFM) +
     .              (RBY(20,NR)*XFRAME(K1,IFM)
     .              +RBY(21,NR)*XFRAME(K2,IFM)
     .              +RBY(22,NR)*XFRAME(K3,IFM))*XFRAME(K2,IFM) +
     .              (RBY(23,NR)*XFRAME(K1,IFM)
     .              +RBY(24,NR)*XFRAME(K2,IFM)
     .              +RBY(25,NR)*XFRAME(K3,IFM))*XFRAME(K3,IFM))
              ENDIF
            ENDIF
C---Correction for DY_VR,DY_DR, DY_AR
            DY_VR(J,I) = DY_VR(J,I)+(YC(II)-AR(J,I))*DT2
            DY_DR(J,I) =DT2*(DY_VR(J,I)+(DY_G-DY_B-UNDEMI)*YC(II)*DT2)
	    AR(J,I) = YC(II)
            IF (TT.LE.DT2) THEN
             DY_VR(J,I) = RD(J,I)/DT2
             DY_DR(J,I) = RD(J,I)
             YC(II) = UNDEMI*DY_VR(J,I)/DT2
	    END IF
            DW   = DW + RD(J,I)*(IN0*YC(II)-FINT)
          ENDIF
         ENDDO
 100    CONTINUE
      ENDDO
C
      RETURN
      END
C------- choice of codensated direction with prefer direction j0--
Chd|====================================================================
Chd|  L_DIR02                       source/constraints/general/impvel/fv_imp0.F
Chd|-- called by -----------
Chd|        FVL_MODIF                     source/constraints/general/impvel/fv_imp0.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        L_DIR0                        source/constraints/general/bcs/bc_imp0.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE L_DIR02(EJ ,J   ,J0  ,IKC )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER J,J0,IKC(3)
      my_real
     .   EJ(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
      my_real
     .   EJ1(3)
C------------EJ is not modified-----------------------
      DO I=1,3
       EJ1(I)= EJ(I)
C-----becomes obsolet
C       IF (IKC(I).GT.0) EJ1(I)=ZERO
      END DO
C------add user's id
       IF ((ABS(EJ1(1))+ABS(EJ1(2))+ABS(EJ1(3))).EQ.ZERO) THEN
        CALL ANCMSG(MSGID=104,ANMODE=ANINFO)
        CALL ARRET(2)
       ENDIF
C---using J0 creat numerical error when EJ1(J0) is small---
c      IF (J0.GT.0 )THEN
c       IF (ABS(EJ1(J0)).GT.EM6) THEN
c        J=J0
c       ELSE
c        CALL L_DIR0(EJ1 ,J)
c       ENDIF
c      ELSE
       CALL L_DIR0(EJ1 ,J)
c      ENDIF
C
      RETURN
      END
Chd|====================================================================
Chd|  DINTERA                       source/constraints/general/impvel/fv_imp0.F
Chd|-- called by -----------
Chd|        WFV_IMP                       source/constraints/general/impvel/fv_imp0.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE DINTERA(TF,IAD,IPOS1,ILEN,NEL0,X1,X2,AY,ITY)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEL0,IAD(*),IPOS1(*),ILEN(*),ITY(*)
      my_real
     .  X1(*),X2(*),AY(*),TF(2,*)
C----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C-----calculate acceleration at (a(X1)+a(X2))/2
      INTEGER IPOS(NEL0)
      my_real
     .   Y1(NEL0),Y2(NEL0),DYDX,X(NEL0),Y(NEL0),
     .   VY1(NEL0),VY2(NEL0)
      INTEGER I,J1,J,J2,ICONT,J0,L
C------postion pour x1-----
      IF (NEL0.EQ.0) RETURN
      DO I=1,NEL0
       J1 = IPOS1(I)+IAD(I)+1
       IF (X1(I).GT.TF(1,J1)) THEN
        IPOS(I) = IPOS1(I)
C--------due au divergence-----
       ELSE
        IPOS(I) = 0
        ILEN(I) = ILEN(I)+IPOS1(I)
       ENDIF
      ENDDO
      J = 0
      ICONT = 1
C
      DO WHILE(ICONT.EQ.1)
       J = J+1
       ICONT = 0
       DO I=1,NEL0
         J1 = IPOS(I)+IAD(I)+1
         IF(J.LE.ILEN(I)-1.AND.X1(I).GT.TF(1,J1))THEN
           IPOS(I)=IPOS(I)+1
           ICONT = 1
         ENDIF
       ENDDO
      ENDDO
C------interpelation pour y1--------
      DO I=1,NEL0
        J1   =IPOS(I)+IAD(I)
        J2   = J1+1
        DYDX=(TF(2,J2)-TF(2,J1))/(TF(1,J2)-TF(1,J1))
        Y1(I)   = TF(2,J1) + DYDX*(X1(I)-TF(1,J1))
C        IPOS1(I) = IPOS(I)
        IF (ITY(I).EQ.2) VY1(I) = DYDX
C--------to be consisting w/ explicit--
c        IF (X1(I).LE.ZERO) Y1(I)   =ZERO
      ENDDO
C------postion pour x2-----
      ICONT = 1
C
      DO WHILE(ICONT.EQ.1)
       J = J+1
       ICONT = 0
       DO I=1,NEL0
         J1 = IPOS(I)+IAD(I)+1
         IF(J.LE.ILEN(I).AND.X2(I).GT.TF(1,J1))THEN
           IPOS(I)=IPOS(I)+1
           ICONT = 1
         ENDIF
       ENDDO
      ENDDO
C------interpelation pour y2--------
      DO I=1,NEL0
        J1   =IPOS(I)+IAD(I)
        J2   = J1+1
        DYDX=(TF(2,J2)-TF(2,J1))/(TF(1,J2)-TF(1,J1))
        Y2(I)   = TF(2,J1) + DYDX*(X2(I)-TF(1,J1))
C        IPOS1(I) = IPOS(I)
        IF (ITY(I).EQ.2) VY2(I) = DYDX
      ENDDO
C------interpelation in function of (d,v,a)--------
      DO I=1,NEL0
          IF (ITY(I).EQ.2) THEN
           AY(I) = (VY2(I) - VY1(I))/(X2(I)-X1(I))
          ELSEIF (ITY(I).EQ.1) THEN
           AY(I) = (Y2(I)-Y1(I))/(X2(I)-X1(I))
          ELSEIF (ITY(I).EQ.0) THEN
C----------------takes average value since it's used only for energy compute
           AY(I) = (Y2(I)+Y1(I))*UNDEMI
          ENDIF
      ENDDO
      I=1
C
      RETURN
      END
Chd|====================================================================
Chd|  FV_FINT0                      source/constraints/general/impvel/fv_imp0.F
Chd|-- called by -----------
Chd|        IMP_SOLV                      source/implicit/imp_solv.F    
Chd|-- calls ---------------
Chd|        IMP_DYNA                      share/modules/impbufdef_mod.F 
Chd|====================================================================
      SUBROUTINE FV_FINT0(IBFV  ,NPC    ,TF   ,VEL   ,SENSOR,
     1                   LSENSOR,UD     ,RD   ,IFIX  ,IDDL  ,
     2                   SKEW   ,IFRAME ,XFRAME,A    ,AR    ,
     3                   X      ,NDOF  ,MS   ,IN    ,WEIGHT ,
     4                   RBY    )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMP_DYNA
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "param_c.inc"
#include      "task_c.inc"
#include      "scr03_c.inc"
#include      "scr05_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NPC(*),IBFV(NIFV,*),LSENSOR(NSENSI,*),
     .        IFIX(*),IDDL(*),IFRAME(LISKN,*),NDOF(*),WEIGHT(*)
C     REAL ou REAL*8
      my_real
     .  TF(*), VEL(LFXVELR,*), UD(3,*), SENSOR(NSENSR,*),
     .  SKEW(LSKEW,*),RD(3,*),A(3,*),AR(3,*),IN(*),
     .  X(3,*),XFRAME(NXFRAME,*),DW,MS(*),RBY(NRBY,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER N, I, ISK, J, L, K1, K2, K3, ISENS,K,
     .        II, IC, NN, IDEB, NR, NSK, NFK, IFM, N0,
     .        INDEX(MVSIZ),I1,J1,ND,ID,J2,J3,
     .        N1,N2,N3,NVL
C     REAL ou REAL*8
      my_real
     .   FAC, STARTT, STOPT, TS,
     .   RX,RY,RZ,VF,VFX,VFY,VFZ,
     .   FACX,FOLD,A0,IN0,DD,MS0,DVR
C IBFV(7,N):1 V;2 D ;0 A ;
C-------------------------------
C  calculate average value (Fint(t+dt)+Fint(t)) for W_ext compute stored at VEL(4,N)
C---Fint(t) has been calculated and stored at begining of imp_solv(even t+dt) ; input A,AR : residual
C-----------------------------------------------
      IDEB = 0
C
      DO NN=1,NFXVEL,NVSIZ
        IF (IBFV(8,NN).EQ.1) GOTO 100
        IC = 0
        IF (NSENSOR.GT.0) THEN
          DO 10 II = 1, MIN(NFXVEL-IDEB,NVSIZ)
            N = II+IDEB
            STARTT = VEL(2,N)
            STOPT  = VEL(3,N)
            IF(TT.LT.STARTT)GOTO 10
            IF(TT.GT.STOPT) GOTO 10
            I=IABS(IBFV(1,N))
            IF(NDOF(I).EQ.0) GOTO 10
            ISENS=0
            DO K=1,NSENSOR
              IF(IBFV(4,N).EQ.LSENSOR(1,K))ISENS=K
            ENDDO
            IF(ISENS.EQ.0)THEN
              TS=TT
            ELSE
              TS = TT-SENSOR(2,ISENS)
              IF(TS.LT.ZERO)GOTO 10
            ENDIF
            IC = IC + 1
            INDEX(IC) = N
 10       CONTINUE
        ELSE
          DO 20 II = 1, MIN(NFXVEL-IDEB,NVSIZ)
            N = II+IDEB
            STARTT = VEL(2,N)
            STOPT  = VEL(3,N)
            IF(TT.LT.STARTT)GOTO 20
            IF(TT.GT.STOPT) GOTO 20
            I=IABS(IBFV(1,N))
            IF(NDOF(I).EQ.0) GOTO 20
            IC = IC + 1
            INDEX(IC) = N
 20       CONTINUE
        ENDIF
C
        IDEB = IDEB + MIN(NFXVEL-IDEB,NVSIZ)
         DO II=1,IC
          N = INDEX(II)
          FAC  = VEL(1,N)
          I=IABS(IBFV(1,N))
          ISK=IBFV(2,N)/10
          IFM = IBFV(9,N)
          J=IBFV(2,N)
          IF (IFM.LE.1) J=J-10*ISK
	  MS0=ABS(MS(I))*WEIGHT(I)
          IF(J.LE.3)THEN
            IF(ISK.LE.1.AND.IFM.LE.1)THEN
              A0   = A(J,I)+MS0*DY_A(J,I)
            ELSEIF (ISK.GT.1) THEN
              K1=3*J-2
              K2=3*J-1
              K3=3*J
              A0 = SKEW(K1,ISK)*(A(1,I)+MS0*DY_A(1,I)) +
     .             SKEW(K2,ISK)*(A(2,I)+MS0*DY_A(2,I)) +
     .             SKEW(K3,ISK)*(A(3,I)+MS0*DY_A(3,I))
            ELSEIF (IFM.GT.1) THEN
              K1=3*J-2
              K2=3*J-1
              K3=3*J
              DD = XFRAME(K1,IFM)*UD(1,I)
     .           + XFRAME(K2,IFM)*UD(2,I)
     .           + XFRAME(K3,IFM)*UD(3,I)
              A0 = XFRAME(K1,IFM)*(A(1,I)+MS0*DY_A(1,I))
     .           + XFRAME(K2,IFM)*(A(2,I)+MS0*DY_A(2,I))
     .           + XFRAME(K3,IFM)*(A(3,I)+MS0*DY_A(3,I))
            ENDIF
          ELSEIF(J.LE.6)THEN
           J1 = J
           J = J - 3
            IN0=IN(I)*WEIGHT(I)
            IF(ISK.LE.1.AND.IFM.LE.1)THEN
              A0  = AR(J,I)+IN0*DY_AR(J,I)
            ELSEIF (ISK.GT.1) THEN
              K1=3*J-2
              K2=3*J-1
              K3=3*J
              A0 = SKEW(K1,ISK)*(AR(1,I)+IN0*DY_AR(1,I))+
     .             SKEW(K2,ISK)*(AR(2,I)+IN0*DY_AR(2,I))+
     .             SKEW(K3,ISK)*(AR(3,I)+IN0*DY_AR(3,I))
C
            ELSEIF (IFM.GT.1) THEN
              K1=3*J-2
              K2=3*J-1
              K3=3*J
              A0 = XFRAME(K1,IFM)*(AR(1,I)+IN0*DY_AR(1,I))+
     .             XFRAME(K2,IFM)*(AR(2,I)+IN0*DY_AR(2,I))+
     .             XFRAME(K3,IFM)*(AR(3,I)+IN0*DY_AR(3,I))
            ENDIF
          ENDIF
          VEL(4,N) = UNDEMI*(VEL(4,N)+A0)
         ENDDO
 100    CONTINUE
      ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  DIM_FVBCL                     source/constraints/general/impvel/fv_imp0.F
Chd|-- called by -----------
Chd|        UPD_GLOB_K                    source/implicit/upd_glob_k.F  
Chd|-- calls ---------------
Chd|        DIR_FVBC                      source/constraints/general/impvel/fv_imp0.F
Chd|        FVBC_COMPA0                   source/constraints/general/impvel/fv_imp0.F
Chd|        FVBC_COMPA1                   source/constraints/general/impvel/fv_imp0.F
Chd|        FVBC_COMPA2                   source/constraints/general/impvel/fv_imp0.F
Chd|        GETBCL_J                      source/constraints/general/impvel/fv_imp0.F
Chd|        GFVBC2_IND                    source/constraints/general/impvel/fv_imp0.F
Chd|        UPDFVBC_L                     source/constraints/general/impvel/fv_imp0.F
Chd|====================================================================
      SUBROUTINE DIM_FVBCL(IBFV   ,LJ    ,ISKEW  ,ICODT ,ICODR  ,
     1                     NDDL   ,IDDL  ,IFIX   ,IADK  ,JDIK   ,
     2                     SKEW   ,NFVBCL ,NBKUD )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "param_c.inc"
#include      "impl1_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IBFV(NIFV,*),LJ(*),ISKEW(*),ICODT(*),ICODR(*),NFVBCL,
     .        NBKUD,NDDL   ,IADK(*)  ,JDIK(*),IDDL(*),IFIX(*)
C     REAL ou REAL*8
      my_real
     .   SKEW(LSKEW,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER N, I, ISK, J, L, K1, K2, K3, K,II,N1,
     .        IFM, J2,J3,I1,J1,ND,NUD,ITAG(NFXVEL),ICTR,
     .        JBC,JJ,J11,J1_1,ISKBC
      my_real
     .   EJ(3),S
C--------LJ()=1,6----- LJ<0
C-1) impose disp in global system
C-2) change CDT,CDR and call BC1 for [K] update
C-3) when there is conflit, change FV or BCS global dir(UD will be updated in FVBC_IMPL)
C-4) adding FVBC_IMPD in RECUKIN as FV_IMP is called several times
C  if treated before
       DO N = 1,NFXVEL
        J1=-LJ(N)
        IF (J1.GT.0) LJ(N)=J1
       END DO
C
      DO N = 1,NFXVEL
        J1=LJ(N)
        ITAG(N)=0
        IF (J1.GT.0) THEN
          I=IABS(IBFV(1,N))
          ISK=IBFV(2,N)/10
          IFM = IBFV(9,N)
          J=IBFV(2,N)
          IF (IFM.LE.1) J=J-10*ISK
          ISKBC=ISKEW(I)
         IF (ISK.EQ.ISKBC) THEN
          IF (J.GT.3) THEN
           IF (ICODR(I).GT.0) THEN
            NFVBCL=NFVBCL+1
                LJ(N)=-J1
           END IF
          ELSE
           IF (ICODT(I).GT.0) THEN
            NFVBCL=NFVBCL+1
                LJ(N)=-J1
           END IF
          END IF !IF (J.GT.3) THEN
         ELSE
          IF (ISKBC.GT.0) THEN
C----------error out---
          END IF
         END IF
        ENDIF
      ENDDO
C--------------NBKUD compter----
      NBKUD =0
      DO N = 1,NFXVEL
        J1=-LJ(N)
        IF (J1.GT.0.AND.ITAG(N).GE.0) THEN
          I=IABS(IBFV(1,N))
          ISK=IBFV(2,N)/10
          IFM = IBFV(9,N)
          J=IBFV(2,N)
          IF (IFM.LE.1) J=J-10*ISK
          NUD=0
          ND =IDDL(I)
              IF (J.GT.3) THEN
                ICTR=ICODR(I)
                ND = ND +3
              ELSE
                ICTR=ICODT(I)
              END IF
          DO K=1,3
           IF (IFIX(ND+K)==9) NUD = NUD + 1
          END DO
C    case 2 Ud	 ---search only ICT 1,2,4
          IF (NUD > 1) THEN
C---------look for another Ud in LJ()<0
           DO N1=N+1,NFXVEL
            JJ = IABS(LJ(N1)-J1)
            II = IABS(IBFV(1,N1))
            IF (LJ(N1) < 0.AND.JJ < 3.AND.II==I) THEN
             ITAG(N)=N1
             ITAG(N1)=-N
            ENDIF
           ENDDO
            N1 = ITAG(N)
            J11 = IBFV(2,N1)
            J1_1 = -LJ(N1)
C-----suppose ICT =(1,2,4) otherwise starter does not passs
           CALL GETBCL_J(ICTR ,ISK   ,SKEW  ,JBC    ,J    )
           CALL FVBC_COMPA2(J1  ,J1_1 ,JBC ,IFIX(IDDL(I)+1) )
C---------
           CALL DIR_FVBC(J1    ,J1_1    ,K     )
           ND=IDDL(I)+K
           CALL UPDFVBC_L(ND   ,IFIX  ,NDDL  ,IADK  ,JDIK ,
     1                    NBKUD )
          ELSEIF (NUD == 1) THEN
C-----case 1 Ud	 ---search first ICT 3,5,6  +2BCS=3global UD-> no change for BCS--
           IF (ICTR==3 .OR.ICTR==5.OR.ICTR==6) THEN
            CALL GETBCL_J(ICTR ,ISK   ,SKEW  ,K    ,J)
C--------check compatibility--
            CALL FVBC_COMPA1(J1  ,K ,IFIX(ND+1))
C---------
           J1_1 = 0
           CALL DIR_FVBC(J1    ,J1_1    ,K     )
           ND=IDDL(I)+J1_1
           CALL UPDFVBC_L(ND   ,IFIX  ,NDDL  ,IADK  ,JDIK ,
     1                    NBKUD )
           ND=IDDL(I)+K
           CALL UPDFVBC_L(ND    ,IFIX  ,NDDL  ,IADK  ,JDIK ,
     1                    NBKUD )
C-----case 1 Ud	 ---search first ICT 1,2,4  -> 2d ud, change for BCS--
          ELSEIF (ICTR==1 .OR.ICTR==2.OR.ICTR==4) THEN
C--------termine independent dof K w/ fixing j1
            CALL GFVBC2_IND(J,ICTR,SKEW(1,ISK),K ,L )
            CALL GETBCL_J(ICTR  ,ISK   ,SKEW  ,J1_1    ,J   )
            CALL FVBC_COMPA0(J1  ,J1_1   ,IFIX(ND+1) ,K )
C---------
            ND=IDDL(I)+J1_1
            CALL UPDFVBC_L(ND    ,IFIX  ,NDDL  ,IADK  ,JDIK ,
     1                     NBKUD )
C---------change of IFIX, update {B} is done also with NKUD1
           END IF !(ICTR==3 .OR.ICTR==5.OR.ICTR==6)
          END IF !IF (NUD > 1) THEN
        ENDIF
      ENDDO
      RETURN
      END
Chd|====================================================================
Chd|  FVBC_IMPL                     source/constraints/general/impvel/fv_imp0.F
Chd|-- called by -----------
Chd|        UPD_GLOB_K                    source/implicit/upd_glob_k.F  
Chd|-- calls ---------------
Chd|        DIR_FVBC                      source/constraints/general/impvel/fv_imp0.F
Chd|        FVBC2_BUP                     source/constraints/general/impvel/fv_imp0.F
Chd|        FVBC_COMPA0                   source/constraints/general/impvel/fv_imp0.F
Chd|        FVBC_COMPA1                   source/constraints/general/impvel/fv_imp0.F
Chd|        FVBC_COMPA2                   source/constraints/general/impvel/fv_imp0.F
Chd|        GETBCL_J                      source/constraints/general/impvel/fv_imp0.F
Chd|        GFVBC2_IND                    source/constraints/general/impvel/fv_imp0.F
Chd|        RECU_UL                       source/constraints/general/impvel/fv_imp0.F
Chd|        UDL2_UG                       source/constraints/general/impvel/fv_imp0.F
Chd|        UDL2_UG2                      source/constraints/general/impvel/fv_imp0.F
Chd|        UPDFVBC_B                     source/constraints/general/impvel/fv_imp0.F
Chd|====================================================================
      SUBROUTINE FVBC_IMPL(IBFV   ,SKEW  ,XFRAME ,LJ   ,IDDL   ,
     1                     IFIX   ,NDOF  ,IADK   ,JDIK ,DIAG_K ,
     2                     LT_K   ,UD    ,RD     ,LB   ,NDDL   ,
     3                     ICODT  ,ICODR ,ICODT1 ,ICODR1,NKUD1 ,
     4                     IKUD   ,BKUD  )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "param_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IBFV(NIFV,*),LJ(*),NFVBCL,NDDL,
     .        NKUD1,IKUD(*)
      INTEGER
     .     IDDL(*),IADK(*),JDIK(*),NDOF(*),IFIX(*),
     .     ICODT(*),ICODR(*),ICODT1(*),ICODR1(*)
C     REAL ou REAL*8
      my_real
     .  UD(3,*),RD(3,*), DIAG_K(*),LT_K(*),SKEW(LSKEW,*),LB(*),
     .  XFRAME(NXFRAME,*),BKUD(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C-1) impose disp in global system
C-2) change CDT,CDR and call BC1 for [K] update
C-3) when there is conflit, change FV or BCS global dir
C-4) adding FVBC_IMPD in RECUKIN as FV_IMP is called several times
      INTEGER N, I, ISK, J, L, K1, K2, K3, K,II,N1,
     .        IFM, J2,J3,I1,J1,ND,NUD,ITAG(NFXVEL),ICTR,
     .        JBC,JJ,J11,J1_1,ICTR1
      my_real
     .   EJ(3),S,UDL(3),UDG(3)
C--------
      NKUD1=0
      DO N = 1,NUMNOD
       ICODT1(N) = ICODT(N)
      END DO
      IF (IRODDL.NE.0) THEN
       DO N = 1,NUMNOD
       ICODR1(N) = ICODR(N)
       END DO
      END IF !(IRODDL.NE.0) THEN
      DO N = 1,NFXVEL
        ITAG(N)=0
      END DO
      DO N = 1,NFXVEL
        J1=-LJ(N)
        IF (J1.GT.0.AND.ITAG(N).GE.0) THEN
          I=IABS(IBFV(1,N))
          ISK=IBFV(2,N)/10
          IFM = IBFV(9,N)
          J=IBFV(2,N)
          IF (IFM.LE.1) J=J-10*ISK
          NUD=0
          ND =IDDL(I)
              IF (J.GT.3) THEN
                ICTR=ICODR(I)
                ND = ND +3
              ELSE
                ICTR=ICODT(I)
              END IF
          DO K=1,3
           IF (IFIX(ND+K)==9) NUD = NUD + 1
           UDL(K)=ZERO
          END DO
C      case 2 Ud	 ---search only ICT 1,2,4
          IF (NUD > 1) THEN
C---------look for another Ud in LJ()<0
           DO N1=N+1,NFXVEL
            JJ = IABS(-LJ(N1)-J1)
            II = IABS(IBFV(1,N1))
            IF (LJ(N1) < 0.AND.JJ < 3.AND.II==I) THEN
             ITAG(N)=N1
             ITAG(N1)=-N
            ENDIF
           ENDDO
            N1 = ITAG(N)
            J11 = IBFV(2,N1)
            J1_1 = -LJ(N1)
            IF (N1==0) THEN
C -------error out
                 WRITE(ISTDO,'(A,I4)')
     +            ' ** ERROR IN IMPVEL(OR IMPDISP) IN SKEW:',ISK
            END IF
C--------starter done aleady  CALL FVBC_COMP1(J,J11,ICTR)
            IF (J.GT.3) THEN
             CALL RECU_UL(ISK,IFM,SKEW,XFRAME,J,J1,UDL,RD(1,I),N)
             CALL RECU_UL(ISK,IFM,SKEW,XFRAME,J11,J1_1,UDL,RD(1,I),N1)
            ELSE
             CALL RECU_UL(ISK,IFM,SKEW,XFRAME,J,J1,UDL,UD(1,I),N)
             CALL RECU_UL(ISK,IFM,SKEW,XFRAME,J11,J1_1,UDL,UD(1,I),N1)
            END IF
C--------calculate Ud in global system (only SKEW is available w/ BCS
            CALL UDL2_UG(SKEW(1,ISK),UDL,UDG)
C----------compatibility FV-BCS-(condensation components)----- ;
C-----suppose ICT =(1,2,4) otherwise starter does not passs
            CALL GETBCL_J(ICTR ,ISK   ,SKEW  ,JBC    ,J    )
            CALL FVBC_COMPA2(J1  ,J1_1 ,JBC ,IFIX(IDDL(I)+1) )
C----------update 2 ud J1,J1_1 first ;
            IF (J.GT.3) THEN
             RD(J1-3,I)=UDG(J1)
             RD(J1_1-3,I)=UDG(J1_1)
            ELSE
             UD(J1,I)=UDG(J1)
             UD(J1_1,I)=UDG(J1_1)
            END IF
C---------update {B} manually for the last ud
           CALL DIR_FVBC(J1    ,J1_1    ,K     )
           ND=IDDL(I)+K
           IF (K >3 ) K = K -3
           CALL UPDFVBC_B(ND   ,IFIX  ,NDDL  ,IADK  ,JDIK ,
     1                   LT_K  ,UDG(K),NKUD1 ,IKUD  ,BKUD )
          ELSEIF (NUD == 1) THEN
            IF (J.GT.3) THEN
	         CALL RECU_UL(ISK,IFM,SKEW,XFRAME,J,J1,UDL,RD(1,I),N)
            ELSE
	         CALL RECU_UL(ISK,IFM,SKEW,XFRAME,J,J1,UDL,UD(1,I),N)
            END IF
C-----case 1 Ud	 ---search first ICT 3,5,6  -> no change for BCS--
           IF (ICTR==3 .OR.ICTR==5.OR.ICTR==6) THEN
C--------calculate Ud in global system (only SKEW is available w/ BCS
            CALL UDL2_UG(SKEW(1,ISK),UDL,UDG)
            CALL GETBCL_J(ICTR ,ISK   ,SKEW  ,K    ,J)
C--------check compatibility--
            CALL FVBC_COMPA1(J1  ,K ,IFIX(IDDL(I)+1))
C----------update ud first
            IF (J.GT.3) THEN 
             DO K1=1,3
              RD(K1,I)=UDG(K1)
             END DO
             ICODR1(I) = 0
            ELSE
             DO K1=1,3
              UD(K1,I)=UDG(K1)
             END DO
C--------no need for update [K] w/ BCL
             ICODT1(I) = 0
            END IF
C---------update {B} of manually for the 2 other ud
           J1_1 = 0
           CALL DIR_FVBC(J1    ,J1_1    ,K     )
           ND=IDDL(I)+J1_1
           IFIX(ND)=8
           IF (J1_1 >3 ) J1_1 = J1_1 -3
           CALL UPDFVBC_B(ND   ,IFIX  ,NDDL  ,IADK  ,JDIK ,
     1                   LT_K   ,UDG(J1_1),NKUD1 ,IKUD  ,BKUD )
           ND=IDDL(I)+K
           IFIX(ND)=8
           IF (K >3 ) K = K -3
           CALL UPDFVBC_B(ND    ,IFIX  ,NDDL  ,IADK  ,JDIK ,
     1                   LT_K   ,UDG(K),NKUD1 ,IKUD  ,BKUD )
C-----case 1 Ud	 ---search first ICT 1,2,4  -> 2d ud, change for BCS--
           ELSEIF (ICTR==1 .OR.ICTR==2.OR.ICTR==4) THEN
C--------termine independent dof K w/ so that DET=DET_max
            CALL GFVBC2_IND(J,ICTR,SKEW(1,ISK),K ,ICTR1)
            CALL GETBCL_J(ICTR  ,ISK   ,SKEW  ,J1_1    ,J   )
            ND = IDDL(I)+J1_1
            IFIX(ND)=8
            ND = IDDL(I)+1
            CALL FVBC_COMPA0(J1  ,J1_1   ,IFIX(ND) ,K )
C--------calculate Ud() free of K in global system update ud_i
            CALL UDL2_UG2(J,ICTR,SKEW(1,ISK),UDL,UDG,K)
C---------and update {B} of term diag_k
            CALL FVBC2_BUP(J     ,ICTR   ,SKEW(1,ISK),J1   ,J1_1   ,
     1                     UDG   ,DIAG_K ,LB    ,IDDL(I))
C----------update ud first
            IF (J.GT.3) THEN
             RD(J1-1,I)=UDG(J1-1)
             RD(J1_1-1,I)=UDG(J1_1-1)
            ELSE
             UD(J1,I)=UDG(J1)
             UD(J1_1,I)=UDG(J1_1)
            END IF
C---------update {B} of manually for the ud_j1
            ND=IDDL(I)+J1_1
            IF (J1_1 >3 ) J1_1 = J1_1 -3
            CALL UPDFVBC_B(ND    ,IFIX  ,NDDL  ,IADK  ,JDIK ,
     1                    LT_K   ,UDG(J1_1),NKUD1 ,IKUD  ,BKUD )
C---------attention IFIX is changed also in BC_IMP1([K] update)
C---------due to this, update {B} of manually for the ud_j1
C---------update ICODT1,ICODR1	for dof j1,j1_1-> [K] update (consisting w/ BC_IMP1
C-------------negative value (taged) to avoid the change of IFIX by BC_IMP1
            IF (J.GT.3) THEN
             ICODR1(I) = -ICTR1
            ELSE
             ICODT1(I) = -ICTR1
            END IF
           END IF !(ICTR==3 .OR.ICTR==5.OR.ICTR==6)
          END IF !IF (NUD > 1) THEN
        ENDIF
      ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  RECU_UL                       source/constraints/general/impvel/fv_imp0.F
Chd|-- called by -----------
Chd|        FVBC_IMPD                     source/constraints/general/impvel/fv_imp0.F
Chd|        FVBC_IMPL                     source/constraints/general/impvel/fv_imp0.F
Chd|        FVBC_IMPL1                    source/constraints/general/impvel/fv_imp0.F
Chd|-- calls ---------------
Chd|        IMP_FVBCL                     share/modules/impbufdef_mod.F 
Chd|====================================================================
      SUBROUTINE RECU_UL(ISK,IFM,SKEW,XFRAME,J,J1,UDL,UD,N)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMP_FVBCL
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "param_c.inc"
#include      "impl1_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ISK,IFM,J,J1,N
C     REAL ou REAL*8
      my_real
     .  UD(*),UDL(*), SKEW(LSKEW,*),XFRAME(NXFRAME,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C-----reget Ud initialized in FVL_MODIF
      INTEGER J0,J01,K1,K2,K3
      my_real
     .  EJ(3)
C
        J0=J
        J01=J1
        IF (J > 3) J0 = J0-3
        IF (J1> 3) J01 = J01-3
C
        IF (IMCONV == 1) THEN
            IF (ISK.GT.1) THEN
              K1=3*J0-2
              K2=3*J0-1
              K3=3*J0
              EJ(1)=SKEW(K1,ISK)
              EJ(2)=SKEW(K2,ISK)
              EJ(3)=SKEW(K3,ISK)
            ELSEIF (IFM.GT.1) THEN
              K1=3*J0-2
              K2=3*J0-1
              K3=3*J0
              EJ(1)=XFRAME(K1,IFM)
              EJ(2)=XFRAME(K2,IFM)
              EJ(3)=XFRAME(K3,IFM)
            ENDIF
         UDL(J0)=UD(J01)*EJ(J01)
         FVBCUDL(N) = UDL(J0)
        ELSE
         UDL(J0)=FVBCUDL(N)
        END IF !(IMCONV == 1) THEN
C
      RETURN
      END
Chd|====================================================================
Chd|  UDL2_UG                       source/constraints/general/impvel/fv_imp0.F
Chd|-- called by -----------
Chd|        FVBC_IMPD                     source/constraints/general/impvel/fv_imp0.F
Chd|        FVBC_IMPL                     source/constraints/general/impvel/fv_imp0.F
Chd|        FVBC_IMPL1                    source/constraints/general/impvel/fv_imp0.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE UDL2_UG(SKEW,UDL,UDG)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL ou REAL*8
      my_real
     .  UDL(3),UDG(3), SKEW(3,3)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C-----Ud_g=[SKEW]Ud_l
      INTEGER I,J
       UDG(1)=SKEW(1,1)*UDL(1)+SKEW(1,2)*UDL(2)+SKEW(1,3)*UDL(3)
       UDG(2)=SKEW(2,1)*UDL(1)+SKEW(2,2)*UDL(2)+SKEW(2,3)*UDL(3)
       UDG(3)=SKEW(3,1)*UDL(1)+SKEW(3,2)*UDL(2)+SKEW(3,3)*UDL(3)
C
      RETURN
      END
Chd|====================================================================
Chd|  DIR_FVBC                      source/constraints/general/impvel/fv_imp0.F
Chd|-- called by -----------
Chd|        DIM_FVBCL                     source/constraints/general/impvel/fv_imp0.F
Chd|        FVBC_COMPA2                   source/constraints/general/impvel/fv_imp0.F
Chd|        FVBC_IMPL                     source/constraints/general/impvel/fv_imp0.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE DIR_FVBC(J    ,J1    ,K     )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER K,J,J1
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER J0,J01
C------J,J1 (1-3).or.(4-6)
        J0=J
        J01=J1
            IF (J > 3) J0 = J0-3
            IF (J1> 3) J01 = J01-3
          K = J0 + 1
          IF (K.GT.3) K = K - 3
          IF (J1.EQ.0) THEN
           J1 = J0 + 2
           IF (J1.GT.3) J1 = J1 - 3
          ELSEIF (K.EQ.J01) THEN
           K = J0 + 2
           IF (K.GT.3) K = K - 3
          ENDIF
          IF (J > 3) THEN
           K = K + 3
           IF (J01.EQ.0) J1 = J1 + 3
          END IF
C
      RETURN
      END
Chd|====================================================================
Chd|  UPDFVBC_L                     source/constraints/general/impvel/fv_imp0.F
Chd|-- called by -----------
Chd|        DIM_FVBCL                     source/constraints/general/impvel/fv_imp0.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE UPDFVBC_L(ID     ,IFIX  ,NDDL  ,IADK  ,JDIK ,
     1                     NB    )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "impl1_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER
     .     ID,IADK(*),JDIK(*),NDDL,IFIX(*) ,NB
C     REAL ou REAL*8
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER N, I, J, K,I1,J1,ND,NFV,NF,NT,JD
C     REAL ou REAL*8
C------------Ligne ID-------
       DO J1 = IADK(ID),IADK(ID+1)-1
        JD = JDIK(J1)
        IF (IFIX(JD).EQ.0)NB = NB+1
       ENDDO
C------------Colonne ID-------
       IF (IKPAT.EQ.0) THEN
        NF=1
        NT=ID-1
       ELSE
        NF=ID+1
        NT=NDDL
       ENDIF
       DO I = NF,NT
         IF (IFIX(I).EQ.0) THEN
          DO K = IADK(I),IADK(I+1)-1
           J=JDIK(K)
           IF (ID.EQ.J) NB = NB+1
          ENDDO
         ENDIF
       ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  UPDFVBC_B                     source/constraints/general/impvel/fv_imp0.F
Chd|-- called by -----------
Chd|        FVBC_IMPL                     source/constraints/general/impvel/fv_imp0.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE UPDFVBC_B(ID     ,IFIX  ,NDDL  ,IADK  ,JDIK ,
     1                     LT_K   ,UD    ,NB    ,IB    ,KB   )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "impl1_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER
     .     ID,IADK(*),JDIK(*),NDDL,IFIX(*) ,NB  ,IB(*)
C     REAL ou REAL*8
      my_real
     .  UD,LT_K(*),KB(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER N, I, J, K,I1,J1,ND,NFV,NF,NT,JD
C     REAL ou REAL*8
C------------Ligne ID-------
       DO J1 = IADK(ID),IADK(ID+1)-1
        JD = JDIK(J1)
        IF (IFIX(JD).EQ.0.AND.LT_K(J1).NE.ZERO) THEN
         NB = NB+1
         IB(NB)=JD
         KB(NB)=UD*LT_K(J1)
        ENDIF
       ENDDO
C------------Colonne ID-------
       IF (IKPAT.EQ.0) THEN
        NF=1
        NT=ID-1
       ELSE
        NF=ID+1
        NT=NDDL
       ENDIF
       DO I = NF,NT
         IF (IFIX(I).EQ.0) THEN
          DO K = IADK(I),IADK(I+1)-1
           J=JDIK(K)
           IF (ID.EQ.J.AND.LT_K(K).NE.ZERO) THEN
             NB = NB+1
             IB(NB)=I
             KB(NB)=UD*LT_K(K)
           ENDIF
          ENDDO
         ENDIF
       ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  GETBCL_J                      source/constraints/general/impvel/fv_imp0.F
Chd|-- called by -----------
Chd|        DIM_FVBCL                     source/constraints/general/impvel/fv_imp0.F
Chd|        FVBC_IMPD                     source/constraints/general/impvel/fv_imp0.F
Chd|        FVBC_IMPL                     source/constraints/general/impvel/fv_imp0.F
Chd|        FVBC_IMPL1                    source/constraints/general/impvel/fv_imp0.F
Chd|-- calls ---------------
Chd|        BC_C2D                        source/constraints/general/bcs/bc_imp0.F
Chd|        DIR_RBE2                      source/constraints/general/rbe2/rbe2v.F
Chd|        L_DIR2                        source/constraints/general/bcs/bc_imp0.F
Chd|====================================================================
      SUBROUTINE GETBCL_J(ICT  ,ISK   ,SKEW  ,J    ,IR    )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ICT,ISK,J,IR
      my_real
     .   SKEW(LSKEW,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER K,J1,L,KC
      my_real
     .   EJ(3),EJ1(3),S,EA,EB
C-----GET BCS w/ local skew condensation component(global) info----
C----For ICT=1,2,4-: return to depedent component j
C----For ICT=3,5,6-: return to indepedent component j=k
         IF (ICT .EQ. 4 ) THEN
          EJ(1)=SKEW(1,ISK)
          EJ(2)=SKEW(2,ISK)
          EJ(3)=SKEW(3,ISK)
          CALL L_DIR2(EJ,J,1)
         ELSEIF (ICT .EQ. 2) THEN
          EJ(1)=SKEW(4,ISK)
          EJ(2)=SKEW(5,ISK)
          EJ(3)=SKEW(6,ISK)
          CALL L_DIR2(EJ,J,2)
         ELSEIF (ICT .EQ. 1) THEN
          EJ(1)=SKEW(7,ISK)
          EJ(2)=SKEW(8,ISK)
          EJ(3)=SKEW(9,ISK)
          CALL L_DIR2(EJ,J,3)
         ELSEIF (ICT .EQ. 3) THEN
          EJ(1)=SKEW(7,ISK)
          EJ(2)=SKEW(8,ISK)
          EJ(3)=SKEW(9,ISK)
c          CALL L_DIR(EJ,J)
          EJ1(1)=SKEW(4,ISK)
          EJ1(2)=SKEW(5,ISK)
          EJ1(3)=SKEW(6,ISK)
          CALL BC_C2D(EJ,EJ1, J, J1  ,EA, EB  )
          CALL DIR_RBE2(J    ,J1    ,K     )
	     J = K
         ELSEIF (ICT .EQ. 5) THEN
          EJ(1)=SKEW(7,ISK)
          EJ(2)=SKEW(8,ISK)
          EJ(3)=SKEW(9,ISK)
c          CALL L_DIR(EJ,J)
          EJ1(1)=SKEW(1,ISK)
          EJ1(2)=SKEW(2,ISK)
          EJ1(3)=SKEW(3,ISK)
          CALL BC_C2D(EJ,EJ1, J, J1  ,EA, EB  )
          CALL DIR_RBE2(J    ,J1    ,K     )
	      J = K
         ELSEIF (ICT .EQ. 6) THEN
C
          EJ(1)=SKEW(4,ISK)
          EJ(2)=SKEW(5,ISK)
          EJ(3)=SKEW(6,ISK)
c          CALL L_DIR(EJ,J)
          EJ1(1)=SKEW(1,ISK)
          EJ1(2)=SKEW(2,ISK)
          EJ1(3)=SKEW(3,ISK)
          CALL BC_C2D(EJ,EJ1, J, J1  ,EA, EB  )
          CALL DIR_RBE2(J    ,J1    ,K     )
	      J = K
         ELSEIF (ICT .EQ. 7) THEN
          J=0
         ENDIF
C---------rotational id----------
	  IF (IR > 3) J = J + 3
C
      RETURN
      END
Chd|====================================================================
Chd|  FVBC_COMPA0                   source/constraints/general/impvel/fv_imp0.F
Chd|-- called by -----------
Chd|        DIM_FVBCL                     source/constraints/general/impvel/fv_imp0.F
Chd|        FVBC_IMPD                     source/constraints/general/impvel/fv_imp0.F
Chd|        FVBC_IMPL                     source/constraints/general/impvel/fv_imp0.F
Chd|        FVBC_IMPL1                    source/constraints/general/impvel/fv_imp0.F
Chd|-- calls ---------------
Chd|        DIR_RBE2                      source/constraints/general/rbe2/rbe2v.F
Chd|====================================================================
      SUBROUTINE FVBC_COMPA0(J1  ,JBC ,IFIX  ,K  )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER J1  ,JBC ,IFIX(*),K
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,IKC(3),J01,J,J02
C---------JBC is the fix component of BCS
       J01 = J1
       IF (J01>3) J01 = J01 -3
       J02 = JBC
       IF (J02>3) J02 = J02 -3
C	   
       IF (J02==K) THEN
        IF (J01==K) THEN
C       change JBC,J1 to -->
         J02=K+1
		 IF (J02>3) J02 = J02 -3
		 IF (JBC>3) J02 = J02 +3
         IFIX(J02)=8
         IFIX(JBC)=0
         JBC = J02		 
C		 
         J01=K+2
		 IF (J01>3) J01 = J01 -3
		 IF (J1>3) J01 = J01 +3
         IFIX(J01)=9
         IFIX(J1)=0	
         J1 = J01
        ELSE
C       change JBC to -->
         CALL DIR_RBE2(J01    ,K    ,J02     )
		 IF (JBC>3) J02 = J02 +3
         IFIX(J02)=8
         IFIX(JBC)=0
         JBC = J02		 
        END IF
       ELSEIF (J01==K) THEN
C       change J1 to -->
         CALL DIR_RBE2(J02    ,K    ,J01     )
		 IF (J1>3) J01 = J01 +3
         IFIX(J01)=9
         IFIX(J1)=0	
         J1 = J01
       END IF !(J02==K) THEN
      RETURN
      END
Chd|====================================================================
Chd|  FVBC_COMPA1                   source/constraints/general/impvel/fv_imp0.F
Chd|-- called by -----------
Chd|        DIM_FVBCL                     source/constraints/general/impvel/fv_imp0.F
Chd|        FVBC_IMPD                     source/constraints/general/impvel/fv_imp0.F
Chd|        FVBC_IMPL                     source/constraints/general/impvel/fv_imp0.F
Chd|        FVBC_IMPL1                    source/constraints/general/impvel/fv_imp0.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE FVBC_COMPA1(J1  ,K ,IFIX  )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER J1  ,K ,IFIX(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C---------K is the free component of BCS
       IF (J1 /= K) THEN
C---------change J1
c        print *,'change FV condensation component', J1,'to',k
C------should change only one times
        IFIX(K)=9
        IFIX(J1)=0
		J1 = K
       END IF
C
      RETURN
      END
Chd|====================================================================
Chd|  FVBC_COMPA2                   source/constraints/general/impvel/fv_imp0.F
Chd|-- called by -----------
Chd|        DIM_FVBCL                     source/constraints/general/impvel/fv_imp0.F
Chd|        FVBC_IMPD                     source/constraints/general/impvel/fv_imp0.F
Chd|        FVBC_IMPL                     source/constraints/general/impvel/fv_imp0.F
Chd|        FVBC_IMPL1                    source/constraints/general/impvel/fv_imp0.F
Chd|-- calls ---------------
Chd|        DIR_FVBC                      source/constraints/general/impvel/fv_imp0.F
Chd|====================================================================
      SUBROUTINE FVBC_COMPA2(J1  ,J2 ,JBC ,IFIX  )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER J1  ,J2 ,JBC ,IFIX(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,K
C---------change J1
       IF (JBC == J1) THEN
C---------change J1
        CALL DIR_FVBC(JBC   ,J2    ,K     )
c        print *,'change FV condensation component', J1,'to',k
        IFIX(K)=9
        IFIX(J1)=0
		J1 = K
       ELSEIF (JBC == J2) THEN
        CALL DIR_FVBC(JBC   ,J1    ,K     )
c        print *,'change FV condensation component', J2,'to',k
        IFIX(K)=9
        IFIX(J2)=0
		J2 =K
       END IF
C
      RETURN
      END
Chd|====================================================================
Chd|  UDL2_UG2                      source/constraints/general/impvel/fv_imp0.F
Chd|-- called by -----------
Chd|        FVBC_IMPD                     source/constraints/general/impvel/fv_imp0.F
Chd|        FVBC_IMPL                     source/constraints/general/impvel/fv_imp0.F
Chd|        FVBC_IMPL1                    source/constraints/general/impvel/fv_imp0.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE UDL2_UG2(FVJ,ICT,SKEW,UDL,UDG,K)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER FVJ,ICT,K
C     REAL ou REAL*8
      my_real
     .  UDL(3),UDG(3), SKEW(3,3)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,L
      my_real
     .  DET,UDI,UDJ,EI(3),EJ(3)
C-----Ud_g=[A(2,2)]Ud_l, [A] =reduced (condense k) [SKEW]^-t
       J = FVJ
       IF (J>3) J=J-3
       DO L=1,3
        EI(L)=SKEW(L,J)
       END DO
       UDI=UDL(J)
       IF (ICT==4) THEN
        I=1
       ELSEIF(ICT==2) THEN
        I=2
       ELSEIF(ICT==1) THEN
        I=3
       END IF
       DO J=1,3
        EJ(J)=SKEW(J,I)
       END DO
       UDJ=UDL(I)
C	  
       IF (K == 1) THEN
        DET = EI(2)*EJ(3)-EI(3)*EJ(2)
        UDG(2)=(EJ(3)*UDI-EI(3)*UDJ)/DET
        UDG(3)=(-EJ(2)*UDI+EI(2)*UDJ)/DET
       ELSEIF (K == 2) THEN        
        DET = EI(1)*EJ(3)-EI(3)*EJ(1)
        UDG(1)=(EJ(3)*UDI-EI(3)*UDJ)/DET
        UDG(3)=(-EJ(1)*UDI+EI(1)*UDJ)/DET
       ELSEIF (K == 3) THEN
        DET = EI(1)*EJ(2)-EI(2)*EJ(1)
        UDG(1)=(EJ(2)*UDI-EI(2)*UDJ)/DET
        UDG(2)=(-EJ(1)*UDI+EI(1)*UDJ)/DET
       END IF
C
      RETURN
      END
Chd|====================================================================
Chd|  FVBC_ALLO                     source/constraints/general/impvel/fv_imp0.F
Chd|-- called by -----------
Chd|        UPD_GLOB_K                    source/implicit/upd_glob_k.F  
Chd|-- calls ---------------
Chd|        IMP_FVBCL                     share/modules/impbufdef_mod.F 
Chd|====================================================================
      SUBROUTINE FVBC_ALLO
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMP_FVBCL
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "impl1_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER K
C     REAL ou REAL*8
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      my_real
     .  DET
      INTEGER I,J
C-----
      IF (NCYCLE /= 1 .OR.INCONV /=1) RETURN

       ALLOCATE(ICT_1(NUMNOD))
       IF (IRODDL >0 ) ALLOCATE(ICR_1(NUMNOD))
       ALLOCATE(FVBCUDL(NFXVEL))
       IF (NKUD_L.GT.0) THEN
        ALLOCATE(IKUD_1(NKUD_L),BKUD_1(NKUD_L))
       ENDIF
C
      RETURN
      END
Chd|====================================================================
Chd|  FVBC_DEALLO                   source/constraints/general/impvel/fv_imp0.F
Chd|-- called by -----------
Chd|        DEALLOCM_IMP                  source/implicit/imp_solv.F    
Chd|-- calls ---------------
Chd|        IMP_FVBCL                     share/modules/impbufdef_mod.F 
Chd|====================================================================
      SUBROUTINE FVBC_DEALLO
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMP_FVBCL
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER K
C     REAL ou REAL*8
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      my_real
     .  DET
      INTEGER I,J
C-----
      IF (NFVBCL > 0) THEN

       DEALLOCATE(ICT_1)
       IF (IRODDL >0 ) DEALLOCATE(ICR_1)
       DEALLOCATE(FVBCUDL)
       IF (NKUD_L.GT.0) THEN
        DEALLOCATE(IKUD_1)
        DEALLOCATE(BKUD_1)
       ENDIF
      END IF !(NFVBCL > 0) THEN
C
      RETURN
      END
Chd|====================================================================
Chd|  FVBC_IMPL1                    source/constraints/general/impvel/fv_imp0.F
Chd|-- called by -----------
Chd|        IMP_SOLV                      source/implicit/imp_solv.F    
Chd|-- calls ---------------
Chd|        FVBC_COMPA0                   source/constraints/general/impvel/fv_imp0.F
Chd|        FVBC_COMPA1                   source/constraints/general/impvel/fv_imp0.F
Chd|        FVBC_COMPA2                   source/constraints/general/impvel/fv_imp0.F
Chd|        GETBCL_J                      source/constraints/general/impvel/fv_imp0.F
Chd|        GFVBC2_IND                    source/constraints/general/impvel/fv_imp0.F
Chd|        RECU_UL                       source/constraints/general/impvel/fv_imp0.F
Chd|        UDL2_UG                       source/constraints/general/impvel/fv_imp0.F
Chd|        UDL2_UG2                      source/constraints/general/impvel/fv_imp0.F
Chd|====================================================================
      SUBROUTINE FVBC_IMPL1(IBFV   ,SKEW  ,XFRAME ,LJ   ,IDDL   ,
     1                      IFIX   ,NDOF  ,UD     ,RD   ,ICODT  ,
     3                      ICODR  ,ISKEW )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "param_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IBFV(NIFV,*),LJ(*)
      INTEGER
     .     IDDL(*),NDOF(*),IFIX(*),ICODT(*),ICODR(*),ISKEW(*)
C     REAL ou REAL*8
      my_real
     .  UD(3,*),RD(3,*), SKEW(LSKEW,*), XFRAME(NXFRAME,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C-1) impose disp in global system
C-2) change CDT,CDR and call BC1
      INTEGER N, I, ISK, J, L, K1, K2, K3, K,II,N1,
     .        IFM, J2,J3,I1,J1,ND,NUD,ITAG(NFXVEL),ICTR,
     .        JBC,JJ,J11,J1_1,ICTR1,ISKBC,IDONE
      my_real
     .   EJ(3),S,UDL(3),UDG(3)
C--------
C  if treated before, skip
       IDONE=0
       DO N = 1,NFXVEL
        J1=-LJ(N)
        IF (J1.GT.0) IDONE=1
       END DO
       IF (IDONE == 1) RETURN
C  Tag coulping FV-BCS
      DO N = 1,NFXVEL
        ITAG(N)=0
      END DO
      DO N = 1,NFXVEL
        J1=LJ(N)
        IF (J1.GT.0) THEN
          I=IABS(IBFV(1,N))
          ISK=IBFV(2,N)/10
          IFM = IBFV(9,N)
          J=IBFV(2,N)
          IF (IFM.LE.1) J=J-10*ISK
          ISKBC=ISKEW(I)
          IF (ISK.EQ.ISKBC) THEN
           IF (J.GT.3) THEN
            IF (ICODR(I).GT.0) LJ(N)=-J1
           ELSE
            IF (ICODT(I).GT.0) LJ(N)=-J1
           END IF !IF (J.GT.3) THEN
        
          END IF
        ENDIF
      ENDDO
C
      DO N = 1,NFXVEL
        J1= -LJ(N)
        IF (J1.GT.0.AND.ITAG(N).GE.0) THEN
          I=IABS(IBFV(1,N))
          ISK=IBFV(2,N)/10
          IFM = IBFV(9,N)
          J=IBFV(2,N)
          IF (IFM.LE.1) J=J-10*ISK
          NUD=0
          ND =IDDL(I)
          IF (J.GT.3) THEN
            ICTR=ICODR(I)
            ND = ND +3
          ELSE
            ICTR=ICODT(I)
          END IF
         DO K=1,3
          IF (IFIX(ND+K)==9) NUD = NUD + 1
          UDL(K)=ZERO
         END DO
C-----case 2 Ud	 ---search only ICT 1,2,4
         IF (NUD > 1) THEN
C---------look for another Ud in LJ()>0
           DO N1=N+1,NFXVEL
            JJ = IABS(-LJ(N1)-J1)
            II = IABS(IBFV(1,N1))
            IF (LJ(N1) < 0.AND.JJ < 3.AND.II==I) THEN
             ITAG(N)=N1
             ITAG(N1)=-N
            ENDIF
           ENDDO
            N1 = ITAG(N)
            J11 = IBFV(2,N1)
            J1_1 = -LJ(N1)
            IF (N1==0) THEN
C--------------error out
             WRITE(ISTDO,'(A,I4)')
     .        ' ** ERROR IN IMPVEL(OR IMPDISP) IN SKEW:',ISK
            END IF
C--------starter done aleady  CALL FVBC_COMP1(J,J11,ICTR)
            IF (J.GT.3) THEN
             CALL RECU_UL(ISK,IFM,SKEW,XFRAME,J,J1,UDL,RD(1,I),N)
             CALL RECU_UL(ISK,IFM,SKEW,XFRAME,J11,J1_1,UDL,RD(1,I),N1)
            ELSE
             CALL RECU_UL(ISK,IFM,SKEW,XFRAME,J,J1,UDL,UD(1,I),N)
             CALL RECU_UL(ISK,IFM,SKEW,XFRAME,J11,J1_1,UDL,UD(1,I),N1)
            END IF
C--------calculate Ud in global system (only SKEW is available w/ BCS
            CALL UDL2_UG(SKEW(1,ISK),UDL,UDG)
C----------compatibility FV-BCS-(condensation components)----- ;
C-----suppose ICT =(1,2,4) otherwise starter does not passs
            CALL GETBCL_J(ICTR ,ISK   ,SKEW  ,JBC    ,J    )
            CALL FVBC_COMPA2(J1  ,J1_1 ,JBC ,IFIX(IDDL(I)+1) )
C----------update 2 ud J1,J1_1 ;
            IF (J.GT.3) THEN
             RD(J1,I)=UDG(J1)
             RD(J1_1,I)=UDG(J1_1)
            ELSE
             UD(J1,I)=UDG(J1)
             UD(J1_1,I)=UDG(J1_1)
            END IF
         ELSEIF (NUD == 1) THEN
C-----case 1 Ud	 ---search first ICT 3,5,6  -> no change for BCS--
            IF (J.GT.3) THEN
	         CALL RECU_UL(ISK,IFM,SKEW,XFRAME,J,J1,UDL,RD(1,I),N)
            ELSE
	         CALL RECU_UL(ISK,IFM,SKEW,XFRAME,J,J1,UDL,UD(1,I),N)
            END IF
           IF (ICTR==3 .OR.ICTR==5.OR.ICTR==6) THEN
C--------calculate Ud in global system (only SKEW is available w/ BCS
            CALL UDL2_UG(SKEW(1,ISK),UDL,UDG)
            CALL GETBCL_J(ICTR ,ISK   ,SKEW  ,K    ,J)
C--------check compatibility--
            CALL FVBC_COMPA1(J1  ,K ,IFIX(IDDL(I)+1))
C----------update ud
            IF (J.GT.3) THEN
             RD(J1,I)=UDG(J1)
            ELSE
             UD(J1,I)=UDG(J1)
            END IF
C-----case 1 Ud	 ---search first ICT 1,2,4  -> 2d ud, change for BCS--
           ELSEIF (ICTR==1 .OR.ICTR==2.OR.ICTR==4) THEN
C--------termine independent dof K w/ fixing j1
            CALL GFVBC2_IND(J,ICTR,SKEW(1,ISK),K ,L )
            CALL GETBCL_J(ICTR  ,ISK   ,SKEW  ,J1_1    ,J   )
	        ND = IDDL(I)+1
            CALL FVBC_COMPA0(J1  ,J1_1   ,IFIX(ND) ,K )
C--------calculate Ud() free of K in global system update ud_i
c	        IF (K >3 ) K = K -3
            CALL UDL2_UG2(J,ICTR,SKEW(1,ISK),UDL,UDG,K)
C----------update ud
             IF (J.GT.3) THEN
              RD(J1-1,I)=UDG(J1-1)
              RD(J1_1-1,I)=UDG(J1_1-1)
             ELSE
              UD(J1,I)=UDG(J1)
              UD(J1_1,I)=UDG(J1_1)
             END IF
           END IF !(ICTR==3 .OR.ICTR==5.OR.ICTR==6)
          END IF !IF (NUD > 1) THEN
	     ENDIF
      ENDDO
C
      RETURN
      END
C-------definitively, using different system (nodal) seting [K] makes coding more simple
C-----will do in V12, should:
C-1). create ITSKEW(inode),IRSKEW(inode)
C-2). change [K] to each nodal system once assemblage elementary
C-3). if slave,master node has not the same system:
C      condensation getKij -> change to global system
C      change to system of master node -> putKij
C      do all kinematic constraints w/o exception.
C-4) once resolved change U to global system for all local nodal one
Chd|====================================================================
Chd|  GDIR2_IND                     source/constraints/general/impvel/fv_imp0.F
Chd|-- called by -----------
Chd|        BC_C2D                        source/constraints/general/bcs/bc_imp0.F
Chd|        FVBC2_BUP                     source/constraints/general/impvel/fv_imp0.F
Chd|        GFVBC2_IND                    source/constraints/general/impvel/fv_imp0.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE GDIR2_IND(EI,EJ,K)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER K
C     REAL ou REAL*8
      my_real
     .  EI(3),EJ(3)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
      my_real
     .  DET(3),DETMAX
C-----determine independent direction (global) with two local constraints
       DET(1) = ABS(EI(2)*EJ(3)-EI(3)*EJ(2))
       DET(2) = ABS(EI(1)*EJ(3)-EI(3)*EJ(1))
       DET(3) = ABS(EI(1)*EJ(2)-EI(2)*EJ(1))
       DETMAX =ZERO
        K= 1
        DO I= 1,3
         IF (DET(I)>DETMAX) THEN
          DETMAX = DET(I)
          K = I
         END IF
        END DO
C
      RETURN
      END
Chd|====================================================================
Chd|  GFVBC2_IND                    source/constraints/general/impvel/fv_imp0.F
Chd|-- called by -----------
Chd|        DIM_FVBCL                     source/constraints/general/impvel/fv_imp0.F
Chd|        FVBC_IMPD                     source/constraints/general/impvel/fv_imp0.F
Chd|        FVBC_IMPL                     source/constraints/general/impvel/fv_imp0.F
Chd|        FVBC_IMPL1                    source/constraints/general/impvel/fv_imp0.F
Chd|-- calls ---------------
Chd|        DIR_RBE2                      source/constraints/general/rbe2/rbe2v.F
Chd|        GDIR2_IND                     source/constraints/general/impvel/fv_imp0.F
Chd|====================================================================
      SUBROUTINE GFVBC2_IND(FVJ,ICT,SKEW,K ,ICT1)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER FVJ,K,ICT,ICT1
C     REAL ou REAL*8
      my_real
     .  SKEW(3,3)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,L
      my_real
     .  EI(3),EJ(3)
C-----determine independent direction (global) K, ICT1 is used to call BC_IMP1
      IF (ICT==4) THEN
	   I=1
      ELSEIF(ICT==2) THEN
	   I=2
      ELSEIF(ICT==1) THEN
	   I=3
      END IF
      DO J=1,3
       EI(J)=SKEW(J,I)
      END DO
	  J = FVJ
      IF (J>3) J=J-3
      DO L=1,3
       EJ(L)=SKEW(L,J)
      END DO
      CALL DIR_RBE2(I,J,L)
      IF (L==1) THEN
	   ICT1=3
      ELSEIF(L==2) THEN
	   ICT1=5
      ELSEIF(L==3) THEN
	   ICT1=6
      END IF
      CALL GDIR2_IND(EI,EJ,K)
C
      RETURN
      END
Chd|====================================================================
Chd|  FVBC_IMPD                     source/constraints/general/impvel/fv_imp0.F
Chd|-- called by -----------
Chd|        RECUKIN                       source/implicit/recudis.F     
Chd|-- calls ---------------
Chd|        FVBC_COMPA0                   source/constraints/general/impvel/fv_imp0.F
Chd|        FVBC_COMPA1                   source/constraints/general/impvel/fv_imp0.F
Chd|        FVBC_COMPA2                   source/constraints/general/impvel/fv_imp0.F
Chd|        GETBCL_J                      source/constraints/general/impvel/fv_imp0.F
Chd|        GFVBC2_IND                    source/constraints/general/impvel/fv_imp0.F
Chd|        RECU_UL                       source/constraints/general/impvel/fv_imp0.F
Chd|        UDL2_UG                       source/constraints/general/impvel/fv_imp0.F
Chd|        UDL2_UG2                      source/constraints/general/impvel/fv_imp0.F
Chd|====================================================================
      SUBROUTINE FVBC_IMPD(IBFV   ,SKEW  ,XFRAME ,LJ   ,NDOF  ,
     1                     UD     ,RD    ,ICODT  ,ICODR,ISKEW ,
     2                     ICODT1 ,ICODR1)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "param_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IBFV(NIFV,*),LJ(*)
      INTEGER
     .     NDOF(*),ICODT(*),ICODR(*),ISKEW(*),
     .     ICODT1(*),ICODR1(*)
C     REAL ou REAL*8
      my_real
     .  UD(3,*),RD(3,*), SKEW(LSKEW,*), XFRAME(NXFRAME,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C-1) impose disp in global system
C-2) change CDT,CDR and call BC1
      INTEGER N, I, ISK, J, L, K1, K2, K3, K,II,N1,
     .        IFM, J2,J3,I1,J1,ND,NUD,ITAG(NFXVEL),ICTR,
     .        JBC,JJ,J11,J1_1,ICTR1,ISKBC,IDONE,
     .        ITAG1(numnod),NLFV(NFXVEL),IFIX(9)
      my_real
     .   EJ(3),S,UDL(3),UDG(3)
C----LJ()>0 ->IDONE=1 ->FV_IMP (FVL_MODIF) has been called
       IDONE = 1
       DO N=1,NFXVEL
        J = LJ(N)
        IF (J < 0) THEN
         LJ(N) =-J
         IDONE = 0
        END IF
       END DO
C------determine Num of FVL of the same node
C----first translation
       DO I=1,NUMNOD
        ITAG1(I)=0
       END DO
       DO N=1,NFXVEL
        J = LJ(N)
        II=IABS(IBFV(1,N))
        NLFV(N)= 0
        IF (J==0.OR.J > 3) CYCLE
        ITAG1(II)= ITAG1(II)+1
       ENDDO
       DO N=1,NFXVEL
        J = LJ(N)
        II=IABS(IBFV(1,N))
        IF (J==0.OR.J > 3) CYCLE
        NLFV(N)= ITAG1(II)
C------if ITAG1(II)>1, the second one will not be traited		
        ITAG1(II)=0
       ENDDO
C----rotational---can use the same NLFV as FV is defined per dir------
c       DO I=1,NUMNOD
c        ITAG1(I)=0
c       END DO
       DO N=1,NFXVEL
        J = LJ(N)
        II=IABS(IBFV(1,N))
        IF (J < 3) CYCLE
        ITAG1(II)= ITAG1(II)+1
       ENDDO
       DO N=1,NFXVEL
        J = LJ(N)
        II=IABS(IBFV(1,N))
        IF (J < 3) CYCLE
        NLFV(N)= ITAG1(II)
C------if ITAG1(II)>1, the second one will not be traited		
        ITAG1(II)=0
       ENDDO
C	   
      DO N = 1,NUMNOD
       ICODT1(N) = ICODT(N)
      END DO
      IF (IRODDL.NE.0) THEN
       DO N = 1,NUMNOD
       ICODR1(N) = ICODR(N)
       END DO
      END IF !(IRODDL.NE.0) THEN
C-----put ICODT,R 0 for coupling bcs	  
C  Tag coulping FV-BCS
      DO N = 1,NFXVEL
        ITAG(N)=0
      END DO
      DO N = 1,NFXVEL
        J1=LJ(N)
        IF (J1/=0) THEN
          I=IABS(IBFV(1,N))
          ISK=IBFV(2,N)/10
          IFM = IBFV(9,N)
          J=IBFV(2,N)
          IF (IFM.LE.1) J=J-10*ISK
          ISKBC=ISKEW(I)
          IF (ISK.EQ.ISKBC) THEN
           IF (J.GT.3) THEN
            IF (ICODR(I).GT.0) LJ(N)=-J1
           ELSE
            IF (ICODT(I).GT.0) LJ(N)=-J1
           END IF !IF (J.GT.3) THEN        
          END IF
        ENDIF
      ENDDO
C
      DO N = 1,NFXVEL
        J1= -LJ(N)
        IF (J1.GT.0.AND.ITAG(N).GE.0) THEN
          I=IABS(IBFV(1,N))
          ISK=IBFV(2,N)/10
          IFM = IBFV(9,N)
          J=IBFV(2,N)
          IF (IFM.LE.1) J=J-10*ISK
          NUD=0
          IF (J.GT.3) THEN
            ICTR=ICODR(I)
            ND = ND +3
          ELSE
            ICTR=ICODT(I)
          END IF
         NUD = NLFV(N)
         DO K=1,3
c          IF (IFIX(ND+K)==9) NUD = NUD + 1
          UDL(K)=ZERO
         END DO
C-----case 2 Ud	 ---search only ICT 1,2,4
         IF (NUD > 1 .AND. IDONE ==1) THEN
C---------look for another Ud in LJ()>0
           DO N1=N+1,NFXVEL
            JJ = IABS(-LJ(N1)-J1)
            II = IABS(IBFV(1,N1))
            IF (LJ(N1) < 0.AND.JJ < 3.AND.II==I) THEN
             ITAG(N)=N1
             ITAG(N1)=-N
            ENDIF
           ENDDO
            N1 = ITAG(N)
            J11 = IBFV(2,N1)
            J1_1 = -LJ(N1)
            IF (N1==0) THEN
C--------------error out
             WRITE(ISTDO,'(A,I4)')
     .        ' ** ERROR IN IMPVEL(OR IMPDISP) IN SKEW:',ISK
            END IF
C--------starter done aleady  CALL FVBC_COMP1(J,J11,ICTR)
            IF (J.GT.3) THEN
             CALL RECU_UL(ISK,IFM,SKEW,XFRAME,J,J1,UDL,RD(1,I),N)
             CALL RECU_UL(ISK,IFM,SKEW,XFRAME,J11,J1_1,UDL,RD(1,I),N1)
            ELSE
             CALL RECU_UL(ISK,IFM,SKEW,XFRAME,J,J1,UDL,UD(1,I),N)
             CALL RECU_UL(ISK,IFM,SKEW,XFRAME,J11,J1_1,UDL,UD(1,I),N1)
            END IF
C--------calculate Ud in global system (only SKEW is available w/ BCS
            CALL UDL2_UG(SKEW(1,ISK),UDL,UDG)
C----------compatibility FV-BCS-(condensation components)----- ;
C-----suppose ICT =(1,2,4) otherwise starter does not passs
            CALL GETBCL_J(ICTR ,ISK   ,SKEW  ,JBC    ,J    )
            CALL FVBC_COMPA2(J1  ,J1_1 ,JBC ,IFIX )
C----------update 2 ud J1,J1_1 ;
            IF (J.GT.3) THEN
             RD(J1,I)=UDG(J1)
             RD(J1_1,I)=UDG(J1_1)
            ELSE
             UD(J1,I)=UDG(J1)
             UD(J1_1,I)=UDG(J1_1)
            END IF
         ELSEIF (NUD == 1) THEN
C-----case 1 Ud	 ---search first ICT 3,5,6  -> no change for BCS--
            IF (J.GT.3) THEN
	         CALL RECU_UL(ISK,IFM,SKEW,XFRAME,J,J1,UDL,RD(1,I),N)
            ELSE
	         CALL RECU_UL(ISK,IFM,SKEW,XFRAME,J,J1,UDL,UD(1,I),N)
            END IF
           IF (ICTR==3 .OR.ICTR==5.OR.ICTR==6) THEN
            IF (IDONE == 1) THEN
C--------calculate Ud in global system (only SKEW is available w/ BCS
             CALL UDL2_UG(SKEW(1,ISK),UDL,UDG)
             CALL GETBCL_J(ICTR ,ISK   ,SKEW  ,K    ,J)
C--------check compatibility--
             CALL FVBC_COMPA1(J1  ,K ,IFIX)
C----------update ud
             IF (J.GT.3) THEN 
              DO K1=1,3
               RD(K1,I)=UDG(K1)
              END DO
             ELSE
              DO K1=1,3
               UD(K1,I)=UDG(K1)
              END DO
             END IF
            END IF
            IF (J.GT.3) THEN 
             ICODR1(I) = 0
            ELSE
             ICODT1(I) = 0
            END IF 
C-----case 1 Ud	 ---search first ICT 1,2,4  -> 2d ud, change for BCS--
           ELSEIF (ICTR==1 .OR.ICTR==2.OR.ICTR==4) THEN
C--------termine independent dof K w/ fixing j1
             CALL GFVBC2_IND(J,ICTR,SKEW(1,ISK),K ,ICTR1 )
             CALL GETBCL_J(ICTR  ,ISK   ,SKEW  ,J1_1    ,J   )
             CALL FVBC_COMPA0(J1  ,J1_1   ,IFIX ,K )
C--------calculate Ud() free of K in global system update ud_i
            IF (IDONE == 1) THEN
             CALL UDL2_UG2(J,ICTR,SKEW(1,ISK),UDL,UDG,K)			 
C----------update ud
             IF (J.GT.3) THEN
              RD(J1-1,I)=UDG(J1-1)
              RD(J1_1-1,I)=UDG(J1_1-1)
             ELSE
              UD(J1,I)=UDG(J1)
              UD(J1_1,I)=UDG(J1_1)
             END IF
            END IF 
C-----------different than real BCS _local			
            IF (J.GT.3) THEN 
             ICODR1(I) = -ICTR1
            ELSE
             ICODT1(I) = -ICTR1
            END IF 
           END IF !(ICTR==3 .OR.ICTR==5.OR.ICTR==6)
         END IF !IF (NUD > 1) THEN
        ENDIF
      ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  FVBC2_BUP                     source/constraints/general/impvel/fv_imp0.F
Chd|-- called by -----------
Chd|        FVBC_IMPL                     source/constraints/general/impvel/fv_imp0.F
Chd|-- calls ---------------
Chd|        GDIR2_IND                     source/constraints/general/impvel/fv_imp0.F
Chd|====================================================================
      SUBROUTINE FVBC2_BUP(FVJ    ,ICT   ,SKEW   ,J1   ,J1_1   ,
     1                     UD     ,DIAG_K,LB     ,ND   )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "impl1_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER FVJ    ,ICT   ,J1   ,J1_1   ,ND
C     REAL ou REAL*8
      my_real
     .  SKEW(3,3),UD(3),DIAG_K(*),LB(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,L,K,J2
      my_real
     .  EJ(3),EJ1(3),DET,EA,EB
C-----update LB of terme diag()*ud
      IF (IMCONV/=1) RETURN
C	  
      IF (ICT==4) THEN
	   I=1
      ELSEIF(ICT==2) THEN
	   I=2
      ELSEIF(ICT==1) THEN
	   I=3
      END IF
      DO J=1,3
       EJ1(J)=SKEW(J,I)
      END DO
	  J = FVJ
      IF (J>3) J=J-3
      DO L=1,3
       EJ(L)=SKEW(L,J)
      END DO
      CALL GDIR2_IND(EJ,EJ1,K)
	  J  = J1
	  IF (J >3) J= J-3
      J2  = J1_1
	  IF (J2 >3) J2= J2-3
      DET = UN/(EJ(J)*EJ1(J2)-EJ(J2)*EJ1(J))
	  EA = -DET*(EJ1(J2)*EJ(K)-EJ(J2)*EJ1(K))
	  EB = -DET*(EJ(J)*EJ1(K)-EJ1(J)*EJ(K))
C	  
      LB(ND+K)=LB(ND+K)-EA*DIAG_K(ND+J)*UD(J)
      LB(ND+K)=LB(ND+K)-EB*DIAG_K(ND+J2)*UD(J2)
C
      RETURN
      END
