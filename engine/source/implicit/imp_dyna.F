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
Chd|  DYNA_INI                      source/implicit/imp_dyna.F    
Chd|-- called by -----------
Chd|        IMP_INIT                      source/implicit/imp_init.F    
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        IMP_DYNA                      share/modules/impbufdef_mod.F 
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE DYNA_INI(NODFT ,NODLT ,D_AL  ,NM_A  ,NM_B ,
     1                    V     ,VR    )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMP_DYNA
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include "com01_c.inc"
#include "com04_c.inc"
#include "units_c.inc"
#include "impl1_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL ou REAL*8
      INTEGER NODFT,NODLT
      my_real
     .   D_AL,NM_A  ,NM_B,V(3,*),VR(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER N, IER1,IER2,IER3,IER4
C------------------------------------------
C---DY_DAM0(t)  Rayleigh damping force; used for energy compute
      IF (IDY_DAMP.GT.0) THEN
       ALLOCATE(DY_DAM(3,NUMNOD),DY_DAM0(3,NUMNOD),STAT=IER1)
       DY_DAM=ZERO
       DY_DAM0=ZERO
      ENDIF
      IF(ALLOCATED(DY_D)) DEALLOCATE(DY_D)
      ALLOCATE(DY_D(3,NUMNOD),STAT=IER1)
      IF(ALLOCATED(DY_V)) DEALLOCATE(DY_V)
      ALLOCATE(DY_V(3,NUMNOD),STAT=IER2)
      IF(ALLOCATED(DY_A)) THEN
        IER3=0
      ELSE
       ALLOCATE(DY_A(3,NUMNOD),STAT=IER3)
       DY_A=ZERO
      ENDIF
         IF ((IER1+IER2+IER3).NE.0) THEN
             CALL ANCMSG(MSGID=19,ANMODE=ANINFO,
     .            C1='FOR IMPLICIT DYNAMIC')
             CALL ARRET(2)
         ENDIF
C-------------for sortie at T=0----
        DO N=NODFT,NODLT
         DY_V(1,N) = V(1,N)
         DY_V(2,N) = V(2,N)
         DY_V(3,N) = V(3,N)
         DY_D(1,N) = ZERO
         DY_D(2,N) = ZERO
         DY_D(3,N) = ZERO
        ENDDO
C
      IF (IRODDL.NE.0) THEN
       IF (IDY_DAMP.GT.0) THEN
        ALLOCATE(DY_DAMR(3,NUMNOD),DY_DAMR0(3,NUMNOD),STAT=IER4)
        DY_DAMR=ZERO
        DY_DAMR0=ZERO
       ENDIF
       IF(ALLOCATED(DY_DR)) DEALLOCATE(DY_DR)
       ALLOCATE(DY_DR(3,NUMNOD),STAT=IER1)
       IF(ALLOCATED(DY_VR)) DEALLOCATE(DY_VR)
       ALLOCATE(DY_VR(3,NUMNOD),STAT=IER2)
       IF(ALLOCATED(DY_AR)) THEN
        IER3=0
       ELSE
        ALLOCATE(DY_AR(3,NUMNOD),STAT=IER3)
        DY_AR=ZERO
       ENDIF
C
         IF ((IER1+IER2+IER3).NE.0) THEN
             CALL ANCMSG(MSGID=19,ANMODE=ANINFO,
     .            C1='FOR IMPLICIT DYNAMIC')
             CALL ARRET(2)
         ENDIF
C
          DO N=NODFT,NODLT
           DY_VR(1,N) = VR(1,N)
           DY_VR(2,N) = VR(2,N)
           DY_VR(3,N) = VR(3,N)
           DY_DR(1,N) = ZERO
           DY_DR(2,N) = ZERO
           DY_DR(3,N) = ZERO
          ENDDO
      ENDIF
C------gama,beta----
      IF (IDYNA.EQ.2) THEN
       DY_G = NM_A
       DY_B = NM_B
       D_AL = ZERO
      ELSE
       DY_G = UNDEMI-D_AL
       DY_B = QUART*(UN-D_AL)*(UN-D_AL)
      ENDIF
      DY_EDAMP = ZERO
C
      RETURN
      END
Chd|====================================================================
Chd|  DYNA_IN0                      source/implicit/imp_dyna.F    
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        IMP_DYNA                      share/modules/impbufdef_mod.F 
Chd|====================================================================
      SUBROUTINE DYNA_IN0(NODFT ,NODLT )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMP_DYNA
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include "com01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL ou REAL*8
      INTEGER NODFT,NODLT
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER N
C------------------------------------------
      IF (IRODDL.NE.0) THEN
          DO N=NODFT,NODLT
           DY_IN(1,N) = ZERO
           DY_IN(2,N) = ZERO
           DY_IN(3,N) = ZERO
          ENDDO
      ENDIF
C
      RETURN
      END
Chd|====================================================================
Chd|  INTE_DYNA                     source/implicit/imp_dyna.F    
Chd|-- called by -----------
Chd|        INTEGRATOR                    source/implicit/integrator.F  
Chd|        INTEGRATOR_HP                 source/implicit/integrator.F  
Chd|-- calls ---------------
Chd|        IMP_DYNA                      share/modules/impbufdef_mod.F 
Chd|====================================================================
      SUBROUTINE INTE_DYNA(NODFT,NODLT ,D    ,DR    ,V    ,VR    )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMP_DYNA
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com08_c.inc"
#include      "impl1_c.inc"
#include      "impl2_c.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NODFT,NODLT,NDT
C     REAL ou REAL*8
      my_real
     . D(3,*),DR(3,*),V(3,*),VR(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,K
      my_real
     .  ADT,ADT1,BDT,BDTI,DT3
C------- integrateur ----------
       IF (IMCONV.LE.-2.AND..NOT.(NINVEL.GT.0.AND.NCYCLE.EQ.0)) THEN
C------- ->a(t) --add something there for initial velocity at t=0--------
        DT3 = DT1_IMP
        BDT = UN/(UNDEMI-DY_G+DY_B)/DT3
        BDTI = BDT/DT3
        DO I=NODFT,NODLT
         DY_A(1,I)=BDT*DY_V(1,I)-BDTI*DY_D(1,I)
         DY_A(2,I)=BDT*DY_V(2,I)-BDTI*DY_D(2,I)
         DY_A(3,I)=BDT*DY_V(3,I)-BDTI*DY_D(3,I)
        ENDDO
        IF (IRODDL.NE.0) THEN
         DO I=NODFT,NODLT
          DY_AR(1,I)=BDT*DY_VR(1,I)-BDTI*DY_DR(1,I)
          DY_AR(2,I)=BDT*DY_VR(2,I)-BDTI*DY_DR(2,I)
          DY_AR(3,I)=BDT*DY_VR(3,I)-BDTI*DY_DR(3,I)
         ENDDO
        ENDIF
       ELSE
        BDTI = UN/DT2/DT2/DY_B
        DO I=NODFT,NODLT
         DY_A(1,I)=BDTI*(D(1,I)-DY_D(1,I))
         DY_A(2,I)=BDTI*(D(2,I)-DY_D(2,I))
         DY_A(3,I)=BDTI*(D(3,I)-DY_D(3,I))
        ENDDO
        IF (IRODDL.NE.0) THEN
         DO I=NODFT,NODLT
          DY_AR(1,I)=BDTI*(DR(1,I)-DY_DR(1,I))
          DY_AR(2,I)=BDTI*(DR(2,I)-DY_DR(2,I))
          DY_AR(3,I)=BDTI*(DR(3,I)-DY_DR(3,I))
         ENDDO
        ENDIF
       ENDIF
C------- actualise V',D',(t) --V->v(t+dt)-------
       IF (IMCONV.EQ.1) THEN
         ADT1 = DY_G*DT2
c         CALL IMP_DT2(DT3)
         DT1_IMP = DT_IMP
	 DT3 = DT2
         ADT = (UN-DY_G)*DT3
         BDT = (UNDEMI-DY_B)*DT3*DT3
         DO I=NODFT,NODLT
          V(1,I)=DY_V(1,I)+ADT1*DY_A(1,I)
          V(2,I)=DY_V(2,I)+ADT1*DY_A(2,I)
          V(3,I)=DY_V(3,I)+ADT1*DY_A(3,I)
C
          DY_V(1,I)=V(1,I)+ADT*DY_A(1,I)
          DY_V(2,I)=V(2,I)+ADT*DY_A(2,I)
          DY_V(3,I)=V(3,I)+ADT*DY_A(3,I)
C
          DY_D(1,I)=DT3*V(1,I)+BDT*DY_A(1,I)
          DY_D(2,I)=DT3*V(2,I)+BDT*DY_A(2,I)
          DY_D(3,I)=DT3*V(3,I)+BDT*DY_A(3,I)
         ENDDO
         IF (IRODDL.NE.0) THEN
          DO I=NODFT,NODLT
           VR(1,I)=DY_VR(1,I)+ADT1*DY_AR(1,I)
           VR(2,I)=DY_VR(2,I)+ADT1*DY_AR(2,I)
           VR(3,I)=DY_VR(3,I)+ADT1*DY_AR(3,I)
C
           DY_VR(1,I)=VR(1,I)+ADT*DY_AR(1,I)
           DY_VR(2,I)=VR(2,I)+ADT*DY_AR(2,I)
           DY_VR(3,I)=VR(3,I)+ADT*DY_AR(3,I)
C
           DY_DR(1,I)=DT3*VR(1,I)+BDT*DY_AR(1,I)
           DY_DR(2,I)=DT3*VR(2,I)+BDT*DY_AR(2,I)
           DY_DR(3,I)=DT3*VR(3,I)+BDT*DY_AR(3,I)
          ENDDO
         ENDIF
       ENDIF
c       I = 2
c       print *,'V=',IDYNA
c       print *,V(1,I),V(2,I),V(3,I)
C--------------------------------------------
      RETURN
      END
Chd|====================================================================
Chd|  IMP_DYNAM                     source/implicit/imp_dyna.F    
Chd|-- called by -----------
Chd|        IMP_CHKM                      source/implicit/imp_solv.F    
Chd|        IMP_SOLV                      source/implicit/imp_solv.F    
Chd|-- calls ---------------
Chd|        IMP_DYNA                      share/modules/impbufdef_mod.F 
Chd|====================================================================
      SUBROUTINE IMP_DYNAM(NODFT  ,NODLT   ,IDDL   ,NDOF   ,
     .                     DIAG_K ,MS      ,IN     ,D_AL   ,WEIGHT,
     .                     IADK   ,LT_K    )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMP_DYNA
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com08_c.inc"
#include      "scr05_c.inc"
#include      "impl1_c.inc"
#include      "impl2_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NODFT,NODLT,IDDL(*) ,NDOF(*),WEIGHT(*),IADK(*)
C     REAL ou REAL*8
      my_real
     . DIAG_K(*),MS(*),IN(*),D_AL,LT_K(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,ND
      my_real
     .  BDT, MKF,MKM,S,S0
C------- add to diag_[k] ----------
      IF (IQSTAT.GT.0) THEN
       D_AL = -ZEP05
       DY_G = UNDEMI-D_AL
       DY_B = QUART*(UN-D_AL)*(UN-D_AL)
      ENDIF
      S0 = ZERO
      IF (IDY_DAMP.GT.0) THEN
       S = (UN+D_AL)*DY_B*DT2
       BDT = (UN/DT2+(UN+D_AL)*DAMPA_IMP*DY_G)/S
       ND = IADK(NDDL_L+1)-IADK(1)
       S0 = (UN+D_AL)*DAMPB_IMP*DY_G/S
       IF (S0.NE.ZERO) THEN
        DO I = 1,NDDL_L
         DIAG_K(I)=DIAG_K(I)+S0*DIAG_K(I)
        ENDDO
        DO I = 1,ND
         LT_K(I) = LT_K(I)+S0*LT_K(I)
        ENDDO
       ENDIF
      ELSE
      S = (UN+D_AL)*DY_B*DT2*DT2
      IF (IQSTAT.GT.0.AND.(ILINTF.EQ.0.OR.ILINTF.EQ.NCYCLE)) THEN
       S = S *SCAL_DTQ*SCAL_DTQ
      ENDIF
      BDT = UN/S
Cq52d4+1
      ENDIF
C
       IF (IRODDL.EQ.0) THEN
        DO I = NODFT,NODLT
         MKF = ABS(MS(I))*BDT*WEIGHT(I)
         DO J=1,NDOF(I)
          ND = IDDL(I)+J
          DIAG_K(ND)=DIAG_K(ND)+MKF
         ENDDO
        ENDDO
       ELSE
        DO I = NODFT,NODLT
         MKF = ABS(MS(I))*BDT*WEIGHT(I)
         MKM = ABS(IN(I))*BDT*WEIGHT(I)
        DO J=1,NDOF(I)
         ND = IDDL(I)+J
          IF (J.LE.3) THEN
           DIAG_K(ND)=DIAG_K(ND)+MKF
          ELSE
           DIAG_K(ND)=DIAG_K(ND)+MKM
          ENDIF
        ENDDO
        ENDDO
       END IF !(IRODDL.EQ.0) THEN
C--------------------------------------------
      RETURN
      END
Chd|====================================================================
Chd|  IMP_DYNAR                     source/implicit/imp_dyna.F    
Chd|-- called by -----------
Chd|        UPD_RHS                       source/implicit/upd_glob_k.F  
Chd|-- calls ---------------
Chd|        IMP_DYNA                      share/modules/impbufdef_mod.F 
Chd|====================================================================
      SUBROUTINE IMP_DYNAR(DY_AC,DY_ACR,MS    ,IN    ,FINT ,MINT  ,
     .                      V    ,VR    )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMP_DYNA
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "impl1_c.inc"
#include      "impl2_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL ou REAL*8
      my_real
     . DY_AC(3,*),DY_ACR(3,*),MS(*),IN(*),FINT(3,*),MINT(3,*),
     . V(3,*),VR(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K
      my_real
     .  BETASDT
C-------  ----------
      DO I=1,NUMNOD
        DY_AC(1,I)=-ABS(MS(I))*DY_A(1,I)
        DY_AC(2,I)=-ABS(MS(I))*DY_A(2,I)
        DY_AC(3,I)=-ABS(MS(I))*DY_A(3,I)
      ENDDO
      IF (IRODDL.NE.0) THEN
       DO I=1,NUMNOD
         DY_ACR(1,I)=-ABS(IN(I))*DY_AR(1,I)
         DY_ACR(2,I)=-ABS(IN(I))*DY_AR(2,I)
         DY_ACR(3,I)=-ABS(IN(I))*DY_AR(3,I)
       ENDDO
      ENDIF
C---add Cv in Fint---------------
      IF (IDY_DAMP.GT.0) THEN
       DO I=1,NUMNOD
        FINT(1,I)=FINT(1,I)-DY_DAM(1,I)
        FINT(2,I)=FINT(2,I)-DY_DAM(2,I)
        FINT(3,I)=FINT(3,I)-DY_DAM(3,I)
       ENDDO
       IF (IRODDL.NE.0) THEN
        DO I=1,NUMNOD
        MINT(1,I)=MINT(1,I)-DY_DAMR(1,I)
        MINT(2,I)=MINT(2,I)-DY_DAMR(2,I)
        MINT(3,I)=MINT(3,I)-DY_DAMR(3,I)
        ENDDO
       ENDIF
      ENDIF
C--------------------------------------------
      RETURN
      END
Chd|====================================================================
Chd|  IMP_FHHT                      source/implicit/imp_dyna.F    
Chd|-- called by -----------
Chd|        UPD_RHS                       source/implicit/upd_glob_k.F  
Chd|-- calls ---------------
Chd|        IMP_DYNA                      share/modules/impbufdef_mod.F 
Chd|====================================================================
      SUBROUTINE IMP_FHHT(NDDL   ,LB    )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMP_DYNA
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "impl1_c.inc"
#include      "impl2_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NDDL,IFLAG
C     REAL ou REAL*8
      my_real
     .  LB(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K
      my_real
     .  A,B
C-------LB() =((UN+HHT_A)LB(t+dt)-HHT_A*LB(t))/(UN+HHT_A)
C--------/(UN+HHT_A) is due to the Jacobien [K]'=[K]/(UN+HHT_A) used to reduce [K] modifications
C----- store  db in  DY_R0; add  db in lb in IMP_FHHT; this is du to SPMD version---------
      IF (HHT_A.EQ.ZERO) RETURN
      A = UN+HHT_A
      B = -HHT_A/A
      DO I=1,NDDL
       DY_R1(I) = LB(I)
       DY_R0(I) = B*DY_R0(I)
      ENDDO
C--------------------------------------------
      RETURN
      END
Chd|====================================================================
Chd|  DYNA_CPR0                     source/implicit/imp_dyna.F    
Chd|-- called by -----------
Chd|        IMP_SOLV                      source/implicit/imp_solv.F    
Chd|-- calls ---------------
Chd|        CP_REAL                       source/implicit/produt_v.F    
Chd|        IMP_DYNA                      share/modules/impbufdef_mod.F 
Chd|====================================================================
      SUBROUTINE DYNA_CPR0(NDDL  )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMP_DYNA
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "impl2_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL ou REAL*8
      INTEGER NDDL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C------------------------------------------
      IF (HHT_A.EQ.ZERO) RETURN
      CALL CP_REAL(NDDL,DY_R1,DY_R0)
C--------------------------------------------
      RETURN
      END
C      SUBROUTINE CINER_DY(JFT    ,JLT     ,PM     ,MAT     ,GEO   ,
C     1                   PID     ,AREA    ,VOLG   ,THK02   ,NPT   ,
C     2                   IGTYP   ,IGEO    ,VQ     ,IXC     ,IXTG  ,
C     3                   NNOD    )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
C      USE IMP_DYNA
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
C#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C#include      "param_c.inc"
C#include      "com01_c.inc"
C#include      "com04_c.inc"
C#include      "impl1_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C      INTEGER JFT, JLT  ,NPT,IGTYP,NNOD
C      INTEGER MAT(*), PID(*),IGEO(NPROPGI,*),IXC(NIXC,*),IXTG(NIXTG,*)
C     REAL ou REAL*8
C      my_real
C     .   GEO(NPROPG,*), PM(NPROPM,*), AREA(*),
C     .   VOLG(*),THK02(*),VQ(3,3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C      INTEGER I,MX,J,J2,JJ,N1,N2,N3,N4,MATLY(MVSIZ*100)
C     REAL ou REAL*8
C      my_real
C     .   RHO(MVSIZ),INE(MVSIZ),FAC,EMS,INZ(MVSIZ),
C     .   IN_X,IN_Y,IN_Z
C      my_real
C     .   THKLY(MVSIZ*100),POSLY(MVSIZ,100)
C-----------------------------------------------
Ctmp      IF (IDYNA.EQ.0.OR.IRODDL.EQ.0) RETURN
C
C      DO I=JFT,JLT
C        MX  =MAT(I)
C        RHO(I)=PM(1,MX)
C        INE(I)=ZERO
C      ENDDO
C      IF (NNOD.EQ.4) THEN
C       FAC = QUART
C      ELSEIF (NNOD.EQ.3) THEN
C       FAC = THIRD
C      ENDIF
C----------------------------------------------
C     MASSES,INERTIES ELEMENTAIRES /NNOD
C----------------------------------------------
C      IF (IGTYP.EQ.11 ) THEN
C         CALL LAYINI (JFT    ,JLT    ,NPT    ,GEO    ,IGEO   ,
C     .                MAT    ,PID    ,THKLY  ,MATLY  ,POSLY  ,
C     .                IGTYP  )
C         DO J=1,NPT
C          J2=1+(J-1)*JLT
C          MX  =MATLY(J2)
C          DO I=JFT,JLT
C           JJ = J2 - 1 + I
C           EMS = PM(1,MX)*THKLY(JJ)*AREA(I)*FAC
C           INE(I)=INE(I)+EMS*((AREA(I)+THKLY(JJ)*THKLY(JJ))*UNDOUZE+
C     .                        POSLY(I,J)*POSLY(I,J))
C          ENDDO
C         ENDDO
C      ELSE
C        DO I=JFT,JLT
C          EMS = RHO(I)*VOLG(I)*FAC
C          INE(I)=EMS*(AREA(I)+THK02(I))*UNDOUZE
C          INZ(I)=DEUX*INE(I)
C        ENDDO
C      ENDIF
C
C      IF (NNOD.EQ.4) THEN
C       DO I=JFT,JLT
C        IN_X = INE(I)*VQ(1,1,I)*VQ(1,1,I)+INE(I)*VQ(2,1,I)*VQ(2,1,I)+
C     .         INZ(I)*VQ(3,1,I)*VQ(3,1,I)
C        IN_Y = INE(I)*VQ(1,2,I)*VQ(1,2,I)+INE(I)*VQ(2,2,I)*VQ(2,2,I)+
C     .         INZ(I)*VQ(3,2,I)*VQ(3,2,I)
C        IN_Z = INE(I)*VQ(1,3,I)*VQ(1,3,I)+INE(I)*VQ(2,3,I)*VQ(2,3,I)+
C     .         INZ(I)*VQ(3,3,I)*VQ(3,3,I)
C        N1 = IXC(2,I)
C        N2 = IXC(3,I)
C        N3 = IXC(4,I)
C        N4 = IXC(5,I)
C        DY_IN(1,N1) = DY_IN(1,N1) + IN_X
C        DY_IN(2,N1) = DY_IN(2,N1) + IN_Y
C        DY_IN(3,N1) = DY_IN(3,N1) + IN_Z
C
C        DY_IN(1,N2) = DY_IN(1,N2) + IN_X
C        DY_IN(2,N2) = DY_IN(2,N2) + IN_Y
C        DY_IN(3,N2) = DY_IN(3,N2) + IN_Z
C
C        DY_IN(1,N3) = DY_IN(1,N3) + IN_X
C        DY_IN(2,N3) = DY_IN(2,N3) + IN_Y
C        DY_IN(3,N3) = DY_IN(3,N3) + IN_Z
C
C        DY_IN(1,N4) = DY_IN(1,N4) + IN_X
C        DY_IN(2,N4) = DY_IN(2,N4) + IN_Y
C        DY_IN(3,N4) = DY_IN(3,N4) + IN_Z
C       ENDDO
C      ELSEIF (NNOD.EQ.3) THEN
C       DO I=JFT,JLT
C        IN_X = INE(I)*VQ(1,1,I)*VQ(1,1,I)+INE(I)*VQ(2,1,I)*VQ(2,1,I)+
C     .         INZ(I)*VQ(3,1,I)*VQ(3,1,I)
C        IN_Y = INE(I)*VQ(1,2,I)*VQ(1,2,I)+INE(I)*VQ(2,2,I)*VQ(2,2,I)+
C     .         INZ(I)*VQ(3,2,I)*VQ(3,2,I)
C        IN_Z = INE(I)*VQ(1,3,I)*VQ(1,3,I)+INE(I)*VQ(2,3,I)*VQ(2,3,I)+
C     .         INZ(I)*VQ(3,3,I)*VQ(3,3,I)
C        N1 = IXTG(2,I)
C        N2 = IXTG(3,I)
C        N3 = IXTG(4,I)
C        DY_IN(1,N1) = DY_IN(1,N1) + IN_X
C        DY_IN(2,N1) = DY_IN(2,N1) + IN_Y
C        DY_IN(3,N1) = DY_IN(3,N1) + IN_Z
C
C        DY_IN(1,N2) = DY_IN(1,N2) + IN_X
C        DY_IN(2,N2) = DY_IN(2,N2) + IN_Y
C        DY_IN(3,N2) = DY_IN(3,N2) + IN_Z
C
C        DY_IN(1,N3) = DY_IN(1,N3) + IN_X
C        DY_IN(2,N3) = DY_IN(2,N3) + IN_Y
C        DY_IN(3,N3) = DY_IN(3,N3) + IN_Z
C       ENDDO
C      ENDIF
C
C      RETURN
C      END
Cq52d5-------------nouvelle routine-------
Chd|====================================================================
Chd|  QSTAT_INI                     source/implicit/imp_dyna.F    
Chd|-- called by -----------
Chd|        IMP_SOLV                      source/implicit/imp_solv.F    
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        IMP_QSTAT                     share/modules/impbufdef_mod.F 
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE QSTAT_INI(NDDL   ,INLOC  ,IDDL    ,NDOF   ,IKC    ,
     .                     MS     ,IN      )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMP_QSTAT
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "units_c.inc"
#include      "impl1_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NDDL,INLOC(*)  ,IDDL(*) ,NDOF(*),IKC(*)
C     REAL ou REAL*8
      my_real
     . MS(*),IN(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,N,ID,ND,NKC,IER1
      my_real
     .  BDT, MKF,MKM,S,B
C-----------------
      IF(ILINE.EQ.0) THEN
       IF(NCYCLE.EQ.1.AND.INCONV.EQ.1) THEN
       ALLOCATE(D_N_1(3*NUMNOD),STAT=IER1)
       IF (IRODDL.NE.0) ALLOCATE(DR_N_1(3*NUMNOD),STAT=IER1)
       IF (IER1.NE.0) THEN
        CALL ANCMSG(MSGID=19,ANMODE=ANINFO,
     .              C1='FOR IMPLICIT QUASI-STATIC')
        CALL ARRET(2)
       ENDIF
       END IF
C      
      ELSE
C 
      IF(NCYCLE == 1)ALLOCATE(QS_D(NDDL),QS_U(NDDL),STAT=IER1)
      IF (IER1.NE.0) THEN
        CALL ANCMSG(MSGID=19,ANMODE=ANINFO,
     .              C1='FOR IMPLICIT QUASI-STATIC')
        CALL ARRET(2)
      ENDIF
      DO I=1,NDDL
        QS_U(I)=ZERO
      ENDDO
      B = QUART*(UN+ZEP05)*(UN+ZEP05)
      S = (UN-ZEP05)*B*DT2*DT2
      BDT = UN/S
      NKC = 0
      IF (IRODDL == 0) THEN
       DO N = 1,NUMNOD
        I=INLOC(N)
        MKF = ABS(MS(I))*BDT
        DO J=1,NDOF(I)
         ND = IDDL(I)+J
         ID = ND-NKC
         IF (IKC(ND).LT.1) THEN
          QS_D(ID)=MKF
         ELSE
          NKC=NKC+1
         ENDIF
        ENDDO
       ENDDO
      ELSE
      DO N = 1,NUMNOD
       I=INLOC(N)
       MKF = ABS(MS(I))*BDT
       MKM = ABS(IN(I))*BDT
       DO J=1,NDOF(I)
        ND = IDDL(I)+J
        ID = ND-NKC
         IF (IKC(ND).LT.1) THEN
          IF (J.LE.3) THEN
           QS_D(ID)=MKF
          ELSE
           QS_D(ID)=MKM
          ENDIF
         ELSE
          NKC=NKC+1
         ENDIF
       ENDDO
      ENDDO
      END IF !(IRODDL == 0) THEN
C      
      END IF !(ILINE.EQ.0) THEN
C--------------------------------------------
      RETURN
      END
Cq52d5-------------nouvelle routine-------
Chd|====================================================================
Chd|  QSTAT_IT                      source/implicit/imp_dyna.F    
Chd|-- called by -----------
Chd|        LIN_SOLV                      source/implicit/lin_solv.F    
Chd|-- calls ---------------
Chd|        IMP_QSTAT                     share/modules/impbufdef_mod.F 
Chd|====================================================================
      SUBROUTINE QSTAT_IT(NDDL   ,F      ,U      )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMP_QSTAT
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NDDL
C     REAL ou REAL*8
      my_real
     . F(*),U(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C-----------------
        DO I=1,NDDL
         F(I)    = QS_D(I)*U(I)
         QS_U(I) = QS_U(I)+U(I)
        ENDDO
C--------------------------------------------
      RETURN
      END
Cq52d5-------------nouvelle routine-------
Chd|====================================================================
Chd|  QSTAT_END                     source/implicit/imp_dyna.F    
Chd|-- called by -----------
Chd|        LIN_SOLV                      source/implicit/lin_solv.F    
Chd|-- calls ---------------
Chd|        IMP_QSTAT                     share/modules/impbufdef_mod.F 
Chd|====================================================================
      SUBROUTINE QSTAT_END(NDDL   ,U      )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMP_QSTAT
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NDDL
C     REAL ou REAL*8
      my_real
     . U(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C-----------------
      DO I=1,NDDL
        U(I) = QS_U(I)
      ENDDO
      DEALLOCATE(QS_U,QS_D)
C--------------------------------------------
      RETURN
      END
Chd|====================================================================
Chd|  DYNA_INA                      source/implicit/imp_dyna.F    
Chd|-- called by -----------
Chd|        IMP_SOLV                      source/implicit/imp_solv.F    
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        FORCE                         source/loads/general/force.F  
Chd|        GRAVIT_IMP                    source/loads/general/grav/gravit_imp.F
Chd|        SPMD_MAX_I                    source/mpi/implicit/imp_spmd.F
Chd|        SPMD_SUMF_A                   source/mpi/implicit/imp_spmd.F
Chd|        STARTIME                      source/system/timer.F         
Chd|        STOPTIME                      source/system/timer.F         
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|        IMP_DYNA                      share/modules/impbufdef_mod.F 
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE DYNA_INA(IBCL  ,FORC   ,NPC   ,TF    ,A     ,
     2                    V     ,X      ,SKEW  ,AR    ,VR    ,
     3                    SENSOR,LSENSOR,WEIGHT,TFEXC ,IADS_F ,
     4                    FSKY  ,IGRV   ,AGRV  ,MS    ,IN    ,
     5                    LGRAV ,ITASK ,NRBYAC,IRBYAC ,NPBY  ,
     6                    RBY   ,FR_ELEM,IAD_ELEM  ,NDDL  ,NNZK,
     7                    IDIV  ,H3D_DATA,CPTREAC,FTHREAC,NODREAC)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMP_DYNA
      USE MESSAGE_MOD
      USE H3D_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include "com01_c.inc"
#include "com04_c.inc"
#include "com08_c.inc"
#include "impl1_c.inc"
#include "param_c.inc"
#include "timeri_c.inc"
#include "scr05_c.inc"
#include "units_c.inc"
#include "impl2_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ITASK,NPC(*), IBCL(*), NPBY(NNPBY,*), IADS_F(*),
     .        LSENSOR(NSENSI,*),IGRV(*),WEIGHT(*),LGRAV(*),IDIV,
     .        CPTREAC,NODREAC(*)
      INTEGER FR_ELEM(*),IAD_ELEM(2,*),NRBYAC,IRBYAC(*),NDDL ,NNZK
C     REAL ou REAL*8
      my_real
     .   X(3,*)    ,V(3,*)   ,VR(3,*),MS(*)   ,AGRV(*),
     .   SKEW(LSKEW,*),TF(*) ,FORC(*)  ,RBY(NRBY,*),IN(*),
     .   A(3,*) ,AR(3,*),TFEXC,FSKY(*),SENSOR(NSENSR,*),FTHREAC(6,*)
      TYPE(H3D_DATABASE) :: H3D_DATA
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,N, NCL_MAX,LBAND,NTMP,NODFT,NODLT,IER1,IER2
      my_real
     .   TMP,ADT,BDT,BDTI,RBID
C--------------------------------
       NODFT = 1
       NODLT = NUMNOD
C-----allocation supp----
      IF (IDIV ==0) THEN
       IF (IDY_DAMP.GT.0) THEN
        ALLOCATE(DY_DIAK0(NDDL),DY_LTK0(NNZK),STAT=IER1)
        DY_DIAK0=ZERO
        DY_LTK0=ZERO
        ALLOCATE(DY_IADK0(NDDL+1),DY_JDIK0(NNZK),STAT=IER2)
         IF ((IER1+IER2).NE.0) THEN
             CALL ANCMSG(MSGID=19,ANMODE=ANINFO,
     .            C1='FOR IMPLICIT DYNAMIC')
             CALL ARRET(2)
         ENDIF
       ENDIF
       IF (HHT_A.NE.ZERO) THEN
        ALLOCATE(DY_R0(NDDL),DY_R1(NDDL),STAT=IER1)
        DY_R0=ZERO
         IF (IER1.NE.0) THEN
             CALL ANCMSG(MSGID=19,ANMODE=ANINFO,
     .            C1='FOR IMPLICIT DYNAMIC')
             CALL ARRET(2)
         ENDIF
       ENDIF
C-----------divergence at first step--------
      ELSE
         ADT = (UN-DY_G)
         BDT = (UNDEMI-DY_B)*DT0_IMP
	 BDTI =UN/(BDT-ADT*DT0_IMP)
         DO N=NODFT,NODLT
           DY_A(1,N) = ZERO
           DY_A(2,N) = ZERO
           DY_A(3,N) = ZERO
C
           V(1,N) = BDTI*(DY_V(1,N)*BDT-DY_D(1,N)*ADT)
           V(2,N) = BDTI*(DY_V(2,N)*BDT-DY_D(2,N)*ADT)
           V(3,N) = BDTI*(DY_V(3,N)*BDT-DY_D(3,N)*ADT)
         ENDDO
         IF (IRODDL.NE.0) THEN
          DO N=NODFT,NODLT
           DY_AR(1,N) = ZERO
           DY_AR(2,N) = ZERO
           DY_AR(3,N) = ZERO
C
           VR(1,N) = BDTI*(DY_VR(1,N)*BDT-DY_DR(1,N)*ADT)
           VR(2,N) = BDTI*(DY_VR(2,N)*BDT-DY_DR(2,N)*ADT)
           VR(3,N) = BDTI*(DY_VR(3,N)*BDT-DY_DR(3,N)*ADT)
          ENDDO
         ENDIF
      END IF !(IDIV ==0) THEN
C
       IF (TT.EQ.DT2.OR.IMPL_S0.EQ.0) THEN
C---estimation a(t=0)----
        NCL_MAX = 0
        IF (TT.EQ.DT2) TT = ZERO
        IF(NCONLD.NE.0) THEN
          IF (IMON.GT.0) CALL STARTIME(4,ITASK+1)
          CALL FORCE(IBCL  ,FORC  ,NPC   ,TF  ,DY_A  ,
     2               V     ,X     ,SKEW  ,DY_AR,VR    ,
     3               SENSOR,LSENSOR,WEIGHT,TFEXC,
     4               IADS_F,FSKY  ,FSKY,RBID,H3D_DATA,
     5               CPTREAC,FTHREAC,NODREAC)
         IF (NSPMD.GT.1) THEN
          DO I=IAD_ELEM(1,1),IAD_ELEM(1,NSPMD+1)-1
            J = FR_ELEM(I)
            TMP = ABS(DY_A(1,J))+ABS(DY_A(2,J))+ABS(DY_A(3,J))+
     .            ABS(DY_AR(1,J))+ABS(DY_AR(2,J))+ABS(DY_AR(3,J))
            IF (TMP.GT.ZERO) NCL_MAX = NCL_MAX + 1
          ENDDO
         ENDIF
          IF (IMON.GT.0) CALL STOPTIME(4,ITASK+1)
        ENDIF
        IF (NSPMD.GT.1) THEN
         CALL SPMD_MAX_I(NCL_MAX)
         IF (NCL_MAX.GT.0) THEN
          LBAND = IAD_ELEM(1,NSPMD+1)-IAD_ELEM(1,1)
          IF (IRODDL.NE.0) THEN
           NTMP = 6
          ELSE
           NTMP = 3
          ENDIF
          CALL SPMD_SUMF_A(DY_A,DY_AR,IAD_ELEM,FR_ELEM,NTMP,LBAND)
         ENDIF
        ENDIF
        IF(NGRAV.NE.0) THEN
          IF (IMON.GT.0) CALL STARTIME(4,ITASK+1)
          CALL GRAVIT_IMP(IGRV  ,AGRV  ,NPC   ,TF    ,DY_A,
     2                    V     ,X     ,SKEW  ,MS,TFEXC,
     3                    SENSOR,LSENSOR,WEIGHT,LGRAV,ITASK,
     4                    NRBYAC,IRBYAC,NPBY  ,RBY    )
          IF (IMON.GT.0) CALL STOPTIME(4,ITASK+1)
        ENDIF
         DO N=NODFT,NODLT
          IF(MS(N).GT.0.) THEN
            TMP = UN / MS(N)
            DY_A(1,N) = (DY_A(1,N) + A(1,N))* TMP
            DY_A(2,N) = (DY_A(2,N) + A(2,N))* TMP
            DY_A(3,N) = (DY_A(3,N) + A(3,N))* TMP
          ENDIF
         ENDDO
        IF (IRODDL.NE.0) THEN
         DO N=NODFT,NODLT
          IF(IN(N).GT.0.) THEN
            TMP = UN / IN(N)
            DY_AR(1,N) = (DY_AR(1,N) + AR(1,N))* TMP
            DY_AR(2,N) = (DY_AR(2,N) + AR(2,N))* TMP
            DY_AR(3,N) = (DY_AR(3,N) + AR(3,N))* TMP
          ENDIF
         ENDDO
        ENDIF
        IF (TT.EQ.ZERO) TT = DT2
       ELSE     !restart
       ENDIF     ! (TT.EQ.0) THEN
C
         ADT = (UN-DY_G)*DT2
         BDT = (UNDEMI-DY_B)*DT2*DT2
         DO N=NODFT,NODLT
          DY_V(1,N)=V(1,N)+ADT*DY_A(1,N)
          DY_V(2,N)=V(2,N)+ADT*DY_A(2,N)
          DY_V(3,N)=V(3,N)+ADT*DY_A(3,N)
C
          DY_D(1,N)=DT2*V(1,N)+BDT*DY_A(1,N)
          DY_D(2,N)=DT2*V(2,N)+BDT*DY_A(2,N)
          DY_D(3,N)=DT2*V(3,N)+BDT*DY_A(3,N)
         ENDDO
         IF (IRODDL.NE.0) THEN
          DO N=NODFT,NODLT
           DY_VR(1,N)=VR(1,N)+ADT*DY_AR(1,N)
           DY_VR(2,N)=VR(2,N)+ADT*DY_AR(2,N)
           DY_VR(3,N)=VR(3,N)+ADT*DY_AR(3,N)
C
           DY_DR(1,N)=DT2*VR(1,N)+BDT*DY_AR(1,N)
           DY_DR(2,N)=DT2*VR(2,N)+BDT*DY_AR(2,N)
           DY_DR(3,N)=DT2*VR(3,N)+BDT*DY_AR(3,N)
          ENDDO
         ENDIF
       IF (TT.EQ.DT2) THEN
C----predictor for a(t+dt)-------
c    keep continuity with GM validation: -initialize later-
        IF (NINVEL.GT.0) THEN
         BDTI = UN/DT2/DT2/DY_B
         BDTI = ZERO
         DO N=NODFT,NODLT
            DY_A(1,N) = -BDTI*DY_D(1,N)
            DY_A(2,N) = -BDTI*DY_D(2,N)
            DY_A(3,N) = -BDTI*DY_D(3,N)
         ENDDO
         IF (IRODDL.NE.0) THEN
          DO N=NODFT,NODLT
            DY_AR(1,N) = -BDTI*DY_DR(1,N)
            DY_AR(2,N) = -BDTI*DY_DR(2,N)
            DY_AR(3,N) = -BDTI*DY_DR(3,N)
          ENDDO
         END IF
        ELSE
         BDTI = UN/DT2/DY_B
         DO N=NODFT,NODLT
            DY_A(1,N) = -BDTI*V(1,N)
            DY_A(2,N) = -BDTI*V(2,N)
            DY_A(3,N) = -BDTI*V(3,N)
         ENDDO
         IF (IRODDL.NE.0) THEN
          DO N=NODFT,NODLT
            DY_AR(1,N) = -BDTI*VR(1,N)
            DY_AR(2,N) = -BDTI*VR(2,N)
            DY_AR(3,N) = -BDTI*VR(3,N)
          ENDDO
         ENDIF
        END IF !IF (NINVEL.GT.0)
       ENDIF
C
      RETURN
      END
Chd|====================================================================
Chd|  DYNA_WEX                      source/implicit/imp_dyna.F    
Chd|-- called by -----------
Chd|        IMP_SOLV                      source/implicit/imp_solv.F    
Chd|-- calls ---------------
Chd|        FORCE                         source/loads/general/force.F  
Chd|        GRAVIT_IMP                    source/loads/general/grav/gravit_imp.F
Chd|        STARTIME                      source/system/timer.F         
Chd|        STOPTIME                      source/system/timer.F         
Chd|        WFV_IMP                       source/constraints/general/impvel/fv_imp0.F
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|        IMP_DYNA                      share/modules/impbufdef_mod.F 
Chd|====================================================================
      SUBROUTINE DYNA_WEX(IBCL  ,FORC   ,NPC   ,TF    ,A     ,
     2                    V     ,X      ,SKEW  ,AR    ,VR    ,
     3                    SENSOR,LSENSOR,WEIGHT,TFEXT ,IADS_F ,
     4                    FSKY  ,IGRV   ,AGRV  ,MS    ,IN    ,
     5                    LGRAV ,ITASK ,NRBYAC,IRBYAC ,NPBY  ,
     6                    RBY   ,IBFV  ,VEL   ,D      ,DR    ,
     7                    IKC   ,IDDL  ,IFRAME,XFRAME ,NDOF  ,
     8                    H3D_DATA,CPTREAC,FTHREAC,NODREAC)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMP_DYNA
      USE H3D_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include "com01_c.inc"
#include "com04_c.inc"
#include "com08_c.inc"
#include "param_c.inc"
#include "timeri_c.inc"
#include "impl1_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ITASK,NPC(*), IBCL(*), NPBY(NNPBY,*), IADS_F(*),
     .        LSENSOR(NSENSI,*),IGRV(*),WEIGHT(*),LGRAV(*),
     .        IFRAME(LISKN,*),IBFV(*),IKC(*) ,IDDL(*),NDOF(*),
     .        CPTREAC,NODREAC(*)
      INTEGER NRBYAC,IRBYAC(*)
C     REAL ou REAL*8
      my_real
     .   X(3,*)    ,V(3,*)   ,VR(3,*),MS(*)   ,AGRV(*),
     .   SKEW(LSKEW,*),TF(*) ,FORC(*)  ,RBY(NRBY,*),IN(*),
     .   A(3,*) ,AR(3,*),TFEXT,FSKY(*),SENSOR(NSENSR,*),
     .   VEL(LFXVELR,*), D(3,*), DR(3,*),XFRAME(NXFRAME,*),
     .   FTHREAC(6,*)
      TYPE(H3D_DATABASE) :: H3D_DATA
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER N,IBID,IW ,I,J,ND,J1
      my_real
     .   TFEXC,RBID
C--------------------------------
       IF (IMON.GT.0) CALL STARTIME(4,ITASK+1)
        IF(NCONLD.NE.0) THEN
          CALL FORCE(IBCL  ,FORC  ,NPC   ,TF  ,A     ,
     2               V     ,X     ,SKEW  ,AR  ,VR    ,
     3               SENSOR,LSENSOR,WEIGHT,TFEXC,
     4               IADS_F,FSKY  ,FSKY, RBID,H3D_DATA,
     5               CPTREAC,FTHREAC,NODREAC)
          TFEXT = TFEXT + TFEXC*DT2
        ENDIF
        IF(NGRAV.NE.0) THEN
          CALL GRAVIT_IMP(IGRV  ,AGRV  ,NPC   ,TF    ,A    ,
     2                    V     ,X     ,SKEW  ,MS,TFEXC,
     3                    SENSOR,LSENSOR,WEIGHT,LGRAV,ITASK,
     5                    NRBYAC,IRBYAC,NPBY  ,RBY    )
          TFEXT = TFEXT + TFEXC*DT2
        ENDIF
        IF(NFXVEL.NE.0) THEN
          CALL WFV_IMP(IBFV  ,NPC    ,TF     ,VEL   ,SENSOR,
     1                LSENSOR,D      ,DR     ,IKC   ,IDDL  ,
     2                SKEW  ,IFRAME ,XFRAME  ,DY_A  ,DY_AR ,
     3                X     ,NDOF    ,MS     ,IN    ,WEIGHT,
     4                RBY   ,TFEXC   )
          TFEXT = TFEXT + TFEXC
        ENDIF
        IF(IDY_DAMP.GT.0) THEN
	 TFEXC = ZERO
C----------DY_DAM has been already condensed---
         DO I = 1 ,NUMNOD
	  DO J=1,3
           ND = IDDL(I)+J
	   IF (IKC(ND) == 0 )
     .      TFEXC = TFEXC +D(J,I)*(DY_DAM(J,I)+DY_DAM0(J,I))*UNDEMI
	  END DO
         ENDDO
         DO I = 1 ,NUMNOD
           DY_DAM0(1,I) = DY_DAM(1,I)
           DY_DAM0(2,I) = DY_DAM(2,I)
           DY_DAM0(3,I) = DY_DAM(3,I)
         ENDDO
         IF (IRODDL.NE.0) THEN
          DO I = 1 ,NUMNOD
	   DO J=4,NDOF(I)
            ND = IDDL(I)+J
	    J1=J-3
	    IF (IKC(ND) == 0 )
     .      TFEXC = TFEXC +DR(J,I)*(DY_DAMR(J,I)+DY_DAMR0(J,I))*UNDEMI
           ENDDO
          ENDDO
          DO I = 1 ,NUMNOD
           DY_DAMR0(1,I) = DY_DAMR(1,I)
           DY_DAMR0(2,I) = DY_DAMR(2,I)
           DY_DAMR0(3,I) = DY_DAMR(3,I)
          ENDDO
         END IF
         DY_EDAMP = DY_EDAMP + TFEXC
C     DY_EDAMP will added in Eint instead of W_ext (as explicit)
C     which makes high error (energy) in case of high Rayleigh damping
        ENDIF
       IF (IMON.GT.0) CALL STOPTIME(4,ITASK+1)
C
      RETURN
      END
Chd|====================================================================
Chd|  IMP_QIFAM                     source/implicit/imp_dyna.F    
Chd|-- called by -----------
Chd|        IMP_SOLV                      source/implicit/imp_solv.F    
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE IMP_QIFAM(NODFT  ,NODLT   ,IDDL   ,NDOF   ,INLOC ,
     .                     IKC    ,DIAG_K  ,MS     ,IN     ,WEIGHT)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com08_c.inc"
#include      "scr05_c.inc"
#include      "impl1_c.inc"
#include      "impl2_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NODFT,NODLT,IDDL(*) ,NDOF(*),WEIGHT(*),INLOC(*),
     .        IKC(*)
C     REAL ou REAL*8
      my_real
     . DIAG_K(*),MS(*),IN(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,ND,N,NKC,ID
      my_real
     .  BDT, MKF,MKM,S,S0,D_AL,DY_G,DY_B,FAC,MM
C------- IQSTAT >0 add to diag_[k] condens   e----------
       IF (SCAL_DTQ.EQ.UN) RETURN
C
       D_AL = -ZEP05
       DY_B = QUART*(UN-D_AL)*(UN-D_AL)
       S = (UN+D_AL)*DY_B*DT2*DT2
       FAC = UN/SCAL_DTQ/SCAL_DTQ
       BDT = -UN/S + FAC/S
      NKC=0
      IF (NSPMD.GT.1) THEN
       DO N = NODFT,NODLT
       I=INLOC(N)
       MKF = ABS(MS(I))*BDT*WEIGHT(I)
       MKM = ABS(IN(I))*BDT*WEIGHT(I)
        DO J=1,NDOF(I)
         ND = IDDL(I)+J
         ID = ND-NKC
          IF (J.LE.3) THEN
	   MM = MKF
          ELSE
           MM = MKM
          ENDIF
          IF (IKC(ND).LT.1) THEN
           DIAG_K(ID)=DIAG_K(ID)+MM
          ELSE
            NKC=NKC+1
          ENDIF
        ENDDO
       ENDDO
      ELSE
       DO N = NODFT,NODLT
       I=INLOC(N)
       MKF = ABS(MS(I))*BDT
       MKM = ABS(IN(I))*BDT
        DO J=1,NDOF(I)
         ND = IDDL(I)+J
         ID = ND-NKC
          IF (J.LE.3) THEN
	   MM = MKF
          ELSE
           MM = MKM
          ENDIF
          IF (IKC(ND).LT.1) THEN
           DIAG_K(ID)=DIAG_K(ID)+MM
          ELSE
            NKC=NKC+1
          ENDIF
        ENDDO
       ENDDO
      ENDIF
C--------------------------------------------
      RETURN
      END
Chd|====================================================================
Chd|  IMP_DYCRB                     source/implicit/imp_dyna.F    
Chd|-- called by -----------
Chd|        UPD_RHS                       source/implicit/upd_glob_k.F  
Chd|-- calls ---------------
Chd|        ROTBMR                        source/tools/skew/rotbmr.F    
Chd|        IMP_DYNA                      share/modules/impbufdef_mod.F 
Chd|====================================================================
      SUBROUTINE IMP_DYCRB(AM    ,IN     ,VR     ,NBY    ,RBY0   ,
     .                     WEIGHT,ICODR  ,ISKEW  ,SKEW   )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMP_DYNA
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "scr05_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL ou REAL*8
      INTEGER NBY(*), WEIGHT(*),ICODR(*),ISKEW(*)
      my_real
     . AM(3,*),IN(*),VR(3,*),RBY0(*),SKEW(LSKEW,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER M, NSN, I, N, J, K, LCOD, ISK
      my_real
     .   WA1, WA2, WA3, DD, VI(3),II1,II2,II3,II4,II5,II6,II7,II8,II9,
     .   VID(3),RBYD(9),VJ(3),RBZ(9),
     .   DET, IL1,IL2,IL3,IL4,IL5,IL6,IL7,IL8,IL9
      my_real
     . RBY(25)
C-------  ----------
      M   =NBY(1)
      IF (IMACH.EQ.3.AND.M.LT.0) RETURN
      NSN =NBY(2)
C petit traitement pour le spmd ou M est replique sur les processeurs
      IF (IMACH.EQ.3.AND.NSPMD.GT.1) THEN
        AM(1,M)  = AM(1,M)  * WEIGHT(M)
        AM(2,M)  = AM(2,M)  * WEIGHT(M)
        AM(3,M)  = AM(3,M)  * WEIGHT(M)
      ENDIF
C------annule in const-----
      AM(1,M)  = AM(1,M) + ABS(IN(M))*DY_AR(1,M)
      AM(2,M)  = AM(2,M) + ABS(IN(M))*DY_AR(2,M)
      AM(3,M)  = AM(3,M) + ABS(IN(M))*DY_AR(3,M)
      DO I = 1, 25
       RBY(I) = RBY0(I)
      ENDDO
C CORRECTION DE L'INERTIE DU RIGID BODY POUR DT NODAL
      RBY(10) = MAX(RBY(10),IN(M))
      RBY(11) = MAX(RBY(11),IN(M))
      RBY(12) = MAX(RBY(12),IN(M))
C
      ISK =ISKEW(M)
      LCOD=ICODR(M)
      IF(LCOD.NE.0)THEN
C       rotation de la matrice d'orientation (directions principales)
        VI(1)=RBY(1)*VR(1,M)+RBY(2)*VR(2,M)+RBY(3)*VR(3,M)
        VI(2)=RBY(4)*VR(1,M)+RBY(5)*VR(2,M)+RBY(6)*VR(3,M)
        VI(3)=RBY(7)*VR(1,M)+RBY(8)*VR(2,M)+RBY(9)*VR(3,M)
        CALL ROTBMR(VI,RBY,DT1)
C
C       matrice d'inertie en repere global
        II1=RBY(10)*RBY(1)
        II2=RBY(10)*RBY(2)
        II3=RBY(10)*RBY(3)
        II4=RBY(11)*RBY(4)
        II5=RBY(11)*RBY(5)
        II6=RBY(11)*RBY(6)
        II7=RBY(12)*RBY(7)
        II8=RBY(12)*RBY(8)
        II9=RBY(12)*RBY(9)
C
        RBY(17)=RBY(1)*II1 + RBY(4)*II4 + RBY(7)*II7
        RBY(18)=RBY(1)*II2 + RBY(4)*II5 + RBY(7)*II8
        RBY(19)=RBY(1)*II3 + RBY(4)*II6 + RBY(7)*II9
        RBY(20)=RBY(2)*II1 + RBY(5)*II4 + RBY(8)*II7
        RBY(21)=RBY(2)*II2 + RBY(5)*II5 + RBY(8)*II8
        RBY(22)=RBY(2)*II3 + RBY(5)*II6 + RBY(8)*II9
        RBY(23)=RBY(3)*II1 + RBY(6)*II4 + RBY(9)*II7
        RBY(24)=RBY(3)*II2 + RBY(6)*II5 + RBY(9)*II8
        RBY(25)=RBY(3)*II3 + RBY(6)*II6 + RBY(9)*II9
C       ajout des termes [Iglobal] vr ^ vr
        WA1=RBY(17)*VR(1,M)+RBY(18)*VR(2,M)+RBY(19)*VR(3,M)
        WA2=RBY(20)*VR(1,M)+RBY(21)*VR(2,M)+RBY(22)*VR(3,M)
        WA3=RBY(23)*VR(1,M)+RBY(24)*VR(2,M)+RBY(25)*VR(3,M)
C
        AM(1,M)=AM(1,M)-(WA2*VR(3,M)-WA3*VR(2,M))*WEIGHT(M)
        AM(2,M)=AM(2,M)-(WA3*VR(1,M)-WA1*VR(3,M))*WEIGHT(M)
        AM(3,M)=AM(3,M)-(WA1*VR(2,M)-WA2*VR(1,M))*WEIGHT(M)
C
       IF(ISK.EQ.1)THEN
C------------------
C       REPERE GLOBAL :
C       Resolution [Iglobal] gama = M, compte-tenu des conditions aux limites
C       Ex : gamaz=0
C           | Ixx Ixy Ixz | { gamax }   { Mx }
C           | Iyx Iyy Iyz | { gamay } = { My }
C           | Izx Izy Izz | {   0   }   { Mz + DMz }  DMz inconnue
C       equivaut a
C           | Ixx Ixy | { gamax }   { Mx }
C           | Iyx Iyy | { gamay } = { My }
C           et gamaz=0
C------------------
        IF(LCOD.EQ.1)THEN
          DET=UN/(RBY(17)*RBY(21)-RBY(18)*RBY(20))
          WA1=-DY_AR(1,M)
          WA2=-DY_AR(2,M)
          AM(1,M)=( RBY(21)*WA1-RBY(20)*WA2)*DET
          AM(2,M)=(-RBY(18)*WA1+RBY(17)*WA2)*DET
          AM(3,M)=ZERO
        ELSEIF(LCOD.EQ.2)THEN
          DET=UN/(RBY(17)*RBY(25)-RBY(19)*RBY(23))
          WA1=-DY_AR(1,M)
          WA2=-DY_AR(3,M)
          AM(1,M)=( RBY(25)*WA1-RBY(23)*WA2)*DET
          AM(2,M)=ZERO
          AM(3,M)=(-RBY(19)*WA1+RBY(17)*WA2)*DET
        ELSEIF(LCOD.EQ.3)THEN
          AM(1,M)=-DY_AR(1,M)/RBY(17)
          AM(2,M)=ZERO
          AM(3,M)=ZERO
        ELSEIF(LCOD.EQ.4)THEN
          DET=UN/(RBY(21)*RBY(25)-RBY(22)*RBY(24))
          WA1=-DY_AR(2,M)
          WA2=-DY_AR(3,M)
          AM(1,M)=ZERO
          AM(2,M)=( RBY(25)*WA1-RBY(24)*WA2)*DET
          AM(3,M)=(-RBY(22)*WA1+RBY(21)*WA2)*DET
        ELSEIF(LCOD.EQ.5)THEN
          AM(1,M)=ZERO
          AM(2,M)=-DY_AR(2,M)/RBY(21)
          AM(3,M)=ZERO
        ELSEIF(LCOD.EQ.6)THEN
          AM(1,M)=ZERO
          AM(2,M)=ZERO
          AM(3,M)=-DY_AR(3,M)/RBY(25)
        ELSEIF(LCOD.EQ.7)THEN
          AM(1,M)=ZERO
          AM(2,M)=ZERO
          AM(3,M)=ZERO
        ENDIF
       ELSE
C-------------------
C       REPERE OBLIQUE
C------------------
C
C       Passage dans le skew : (vitesse), moments, matrice d'inertie.
C
C
        WA1=-DY_AR(1,M)
        WA2=-DY_AR(2,M)
        WA3=-DY_AR(3,M)
C
        AM(1,M)=SKEW(1,ISK)*WA1+SKEW(2,ISK)*WA2+SKEW(3,ISK)*WA3
        AM(2,M)=SKEW(4,ISK)*WA1+SKEW(5,ISK)*WA2+SKEW(6,ISK)*WA3
        AM(3,M)=SKEW(7,ISK)*WA1+SKEW(8,ISK)*WA2+SKEW(9,ISK)*WA3
C
C       Resolution ds le repere local, compte-tenu des conditions aux limites
C       Ex : v3+gama3*dt12=0
C           | IL1 IL2 IL3 | { gama1    }   { M1 }
C           | IL4 IL5 IL6 | { gama2    } = { M2 }
C           | IL7 IL8 IL9 | { -v3/dt12 }   { M3 + DM3  }  DM3 inconnue
C       equivaut a
C           | IL1 IL2 IL3 | { gama1 }   { M1 }          | IL1 IL2 IL3 | { 0 }
C           | IL4 IL5 IL6 | { gama2 } = { M2 }        + | IL4 IL5 IL6 | { 0 }
C           | IL7 IL8 IL9 | { 0     }   { M3 + DM3  }   | IL7 IL8 IL9 | { v3/dt12 }
C
C       pas de solution => gama3=0, v'3=0 (reimpose dans la condition limite)
C
C           | IL1 IL2 IL3 | { gama1 }   { M1 }            | IL1 IL2 IL3 | { gama1 }   { M1 }
C           | IL4 IL5 IL6 | { gama2 } = { M2 }       <==> | IL4 IL5 IL6 | { gama2 } = { M2 }
C           | IL7 IL8 IL9 | { 0     }   { M3 + DM3  }
C
        IF(LCOD.EQ.1)THEN
C
         II1=RBY(17)*SKEW(1,ISK)+RBY(18)*SKEW(2,ISK)+RBY(19)*SKEW(3,ISK)
         II2=RBY(17)*SKEW(4,ISK)+RBY(18)*SKEW(5,ISK)+RBY(19)*SKEW(6,ISK)
         II4=RBY(20)*SKEW(1,ISK)+RBY(21)*SKEW(2,ISK)+RBY(22)*SKEW(3,ISK)
         II5=RBY(20)*SKEW(4,ISK)+RBY(21)*SKEW(5,ISK)+RBY(22)*SKEW(6,ISK)
         II7=RBY(23)*SKEW(1,ISK)+RBY(24)*SKEW(2,ISK)+RBY(25)*SKEW(3,ISK)
         II8=RBY(23)*SKEW(4,ISK)+RBY(24)*SKEW(5,ISK)+RBY(25)*SKEW(6,ISK)
         IL1=SKEW(1,ISK)*II1+SKEW(2,ISK)*II4+SKEW(3,ISK)*II7
         IL2=SKEW(1,ISK)*II2+SKEW(2,ISK)*II5+SKEW(3,ISK)*II8
         IL4=SKEW(4,ISK)*II1+SKEW(5,ISK)*II4+SKEW(6,ISK)*II7
         IL5=SKEW(4,ISK)*II2+SKEW(5,ISK)*II5+SKEW(6,ISK)*II8
C
         DET=UN/(IL1*IL5-IL2*IL4)
         WA1=-DY_AR(1,M)
         WA2=-DY_AR(2,M)
         AM(1,M)=( IL5*WA1-IL4*WA2)*DET
         AM(2,M)=(-IL2*WA1+IL1*WA2)*DET
         AM(3,M)=ZERO
        ELSEIF(LCOD.EQ.2)THEN
         II1=RBY(17)*SKEW(1,ISK)+RBY(18)*SKEW(2,ISK)+RBY(19)*SKEW(3,ISK)
         II3=RBY(17)*SKEW(7,ISK)+RBY(18)*SKEW(8,ISK)+RBY(19)*SKEW(9,ISK)
         II4=RBY(20)*SKEW(1,ISK)+RBY(21)*SKEW(2,ISK)+RBY(22)*SKEW(3,ISK)
         II6=RBY(20)*SKEW(7,ISK)+RBY(21)*SKEW(8,ISK)+RBY(22)*SKEW(9,ISK)
         II7=RBY(23)*SKEW(1,ISK)+RBY(24)*SKEW(2,ISK)+RBY(25)*SKEW(3,ISK)
         II9=RBY(23)*SKEW(7,ISK)+RBY(24)*SKEW(8,ISK)+RBY(25)*SKEW(9,ISK)
         IL1=SKEW(1,ISK)*II1+SKEW(2,ISK)*II4+SKEW(3,ISK)*II7
         IL3=SKEW(1,ISK)*II3+SKEW(2,ISK)*II6+SKEW(3,ISK)*II9
         IL7=SKEW(7,ISK)*II1+SKEW(8,ISK)*II4+SKEW(9,ISK)*II7
         IL9=SKEW(7,ISK)*II3+SKEW(8,ISK)*II6+SKEW(9,ISK)*II9
C
         DET=UN/(IL1*IL9-IL3*IL7)
         WA1=-DY_AR(1,M)
         WA2=-DY_AR(3,M)
         AM(1,M)=( IL9*WA1-IL7*WA2)*DET
         AM(2,M)=ZERO
         AM(3,M)=(-IL3*WA1+IL1*WA2)*DET
        ELSEIF(LCOD.EQ.3)THEN
         II1=RBY(17)*SKEW(1,ISK)+RBY(18)*SKEW(2,ISK)+RBY(19)*SKEW(3,ISK)
         II4=RBY(20)*SKEW(1,ISK)+RBY(21)*SKEW(2,ISK)+RBY(22)*SKEW(3,ISK)
         II7=RBY(23)*SKEW(1,ISK)+RBY(24)*SKEW(2,ISK)+RBY(25)*SKEW(3,ISK)
         IL1=SKEW(1,ISK)*II1+SKEW(2,ISK)*II4+SKEW(3,ISK)*II7
C
         AM(1,M)=-DY_AR(1,M)/IL1
         AM(2,M)=ZERO
         AM(3,M)=ZERO
        ELSEIF(LCOD.EQ.4)THEN
         II2=RBY(17)*SKEW(4,ISK)+RBY(18)*SKEW(5,ISK)+RBY(19)*SKEW(6,ISK)
         II3=RBY(17)*SKEW(7,ISK)+RBY(18)*SKEW(8,ISK)+RBY(19)*SKEW(9,ISK)
         II5=RBY(20)*SKEW(4,ISK)+RBY(21)*SKEW(5,ISK)+RBY(22)*SKEW(6,ISK)
         II6=RBY(20)*SKEW(7,ISK)+RBY(21)*SKEW(8,ISK)+RBY(22)*SKEW(9,ISK)
         II8=RBY(23)*SKEW(4,ISK)+RBY(24)*SKEW(5,ISK)+RBY(25)*SKEW(6,ISK)
         II9=RBY(23)*SKEW(7,ISK)+RBY(24)*SKEW(8,ISK)+RBY(25)*SKEW(9,ISK)
         IL5=SKEW(4,ISK)*II2+SKEW(5,ISK)*II5+SKEW(6,ISK)*II8
         IL6=SKEW(4,ISK)*II3+SKEW(5,ISK)*II6+SKEW(6,ISK)*II9
         IL8=SKEW(7,ISK)*II2+SKEW(8,ISK)*II5+SKEW(9,ISK)*II8
         IL9=SKEW(7,ISK)*II3+SKEW(8,ISK)*II6+SKEW(9,ISK)*II9
C
         DET=UN/(IL5*IL9-IL6*IL8)
         WA1=-DY_AR(2,M)
         WA2=-DY_AR(3,M)
         AM(1,M)=ZERO
         AM(2,M)=( IL9*WA1-IL8*WA2)*DET
         AM(3,M)=(-IL6*WA1+IL5*WA2)*DET
        ELSEIF(LCOD.EQ.5)THEN
         II2=RBY(17)*SKEW(4,ISK)+RBY(18)*SKEW(5,ISK)+RBY(19)*SKEW(6,ISK)
         II5=RBY(20)*SKEW(4,ISK)+RBY(21)*SKEW(5,ISK)+RBY(22)*SKEW(6,ISK)
         II8=RBY(23)*SKEW(4,ISK)+RBY(24)*SKEW(5,ISK)+RBY(25)*SKEW(6,ISK)
         IL5=SKEW(4,ISK)*II2+SKEW(5,ISK)*II5+SKEW(6,ISK)*II8
C
         AM(1,M)=ZERO
         AM(2,M)=-DY_AR(2,M)/IL5
         AM(3,M)=ZERO
        ELSEIF(LCOD.EQ.6)THEN
         II3=RBY(17)*SKEW(7,ISK)+RBY(18)*SKEW(8,ISK)+RBY(19)*SKEW(9,ISK)
         II6=RBY(20)*SKEW(7,ISK)+RBY(21)*SKEW(8,ISK)+RBY(22)*SKEW(9,ISK)
         II9=RBY(23)*SKEW(7,ISK)+RBY(24)*SKEW(8,ISK)+RBY(25)*SKEW(9,ISK)
         IL9=SKEW(7,ISK)*II3+SKEW(8,ISK)*II6+SKEW(9,ISK)*II9
C
         AM(1,M)=ZERO
         AM(2,M)=ZERO
         AM(3,M)=-DY_AR(3,M)/IL9
        ELSEIF(LCOD.EQ.7)THEN
         AM(1,M)=ZERO
         AM(2,M)=ZERO
         AM(3,M)=ZERO
        ENDIF
        WA1=AM(1,M)
        WA2=AM(2,M)
        WA3=AM(3,M)
C
        AM(1,M)=SKEW(1,ISK)*WA1+SKEW(4,ISK)*WA2+SKEW(7,ISK)*WA3
        AM(2,M)=SKEW(2,ISK)*WA1+SKEW(5,ISK)*WA2+SKEW(8,ISK)*WA3
        AM(3,M)=SKEW(3,ISK)*WA1+SKEW(6,ISK)*WA2+SKEW(9,ISK)*WA3
       ENDIF       ! IF(ISK.EQ.1)THEN
C
      ELSE
C
       WA1=-DY_AR(1,M)
       WA2=-DY_AR(2,M)
       WA3=-DY_AR(3,M)
C repere globale -> repere d'inertie principale
       AM(1,M)=RBY(1)*WA1+RBY(2)*WA2+RBY(3)*WA3
       AM(2,M)=RBY(4)*WA1+RBY(5)*WA2+RBY(6)*WA3
       AM(3,M)=RBY(7)*WA1+RBY(8)*WA2+RBY(9)*WA3
C les contributions des vr ne sont ajoutees que sur le processeur maitre
       VI(1)=RBY(1)*VR(1,M)+RBY(2)*VR(2,M)+RBY(3)*VR(3,M)
       VI(2)=RBY(4)*VR(1,M)+RBY(5)*VR(2,M)+RBY(6)*VR(3,M)
       VI(3)=RBY(7)*VR(1,M)+RBY(8)*VR(2,M)+RBY(9)*VR(3,M)
       DO K=1,9
         RBZ(K)=RBY(K)
       ENDDO
       CALL ROTBMR(VI,RBY,DT1)
C
       AM(1,M) = AM(1,M) - (RBY(11)-RBY(12))*VI(2)*VI(3)*WEIGHT(M)
       AM(2,M) = AM(2,M) - (RBY(12)-RBY(10))*VI(3)*VI(1)*WEIGHT(M)
       AM(3,M) = AM(3,M) - (RBY(10)-RBY(11))*VI(1)*VI(2)*WEIGHT(M)
C
C CALCUL D'UN PSEUDO MOMENT:
C EN FAIT L'ACCELERATION DE ROTATION * INERTIE DU NOEUD MAITRE (IMIN DU RB)
C--------
        WA1 = AM(1,M)*RBY(10)
        WA2 = AM(2,M)*RBY(11)
        WA3 = AM(3,M)*RBY(12)
C repere d'inertie principale -> repere globale
       AM(1,M)=RBY(1)*WA1+RBY(4)*WA2+RBY(7)*WA3
       AM(2,M)=RBY(2)*WA1+RBY(5)*WA2+RBY(8)*WA3
       AM(3,M)=RBY(3)*WA1+RBY(6)*WA2+RBY(9)*WA3
      ENDIF
C--------------------------------------------
      RETURN
      END
Chd|====================================================================
Chd|  DYNA_INIV                     source/implicit/imp_dyna.F    
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        IMP_DYNA                      share/modules/impbufdef_mod.F 
Chd|====================================================================
      SUBROUTINE DYNA_INIV(NODFT ,NODLT ,MS   ,IN   ,D  ,DR  )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMP_DYNA
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include "com01_c.inc"
#include "com04_c.inc"
#include "com08_c.inc"
#include "impl1_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL ou REAL*8
      INTEGER NODFT,NODLT,IDIV
      my_real
     .   D(3,*),DR(3,*),MS(*) ,IN(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER N,I
      my_real
     .   ADT,BDT,TMP,TMP1
C------------------------------------------
C-------------seems more stable with predictor d=0 when there is DY_DAM
        IF (IDY_DAMP.EQ.0) THEN
         DO I=NODFT,NODLT
          D(1,I)=DY_D(1,I)
          D(2,I)=DY_D(2,I)
          D(3,I)=DY_D(3,I)
         ENDDO
         IF (IRODDL.NE.0) THEN
          DO I=NODFT,NODLT
           DR(1,I)=DY_DR(1,I)
           DR(2,I)=DY_DR(2,I)
           DR(3,I)=DY_DR(3,I)
          ENDDO
         ENDIF
C
        ELSE
         DO I=NODFT,NODLT
          D(1,I)=DY_D(1,I)*EM3
          D(2,I)=DY_D(2,I)*EM3
          D(3,I)=DY_D(3,I)*EM3
         ENDDO
         IF (IRODDL.NE.0) THEN
          DO I=NODFT,NODLT
           DR(1,I)=DY_DR(1,I)*EM3
           DR(2,I)=DY_DR(2,I)*EM3
           DR(3,I)=DY_DR(3,I)*EM3
          ENDDO
         ENDIF
         ADT = (UN-DY_G)*DT2
         BDT = (UNDEMI-DY_B)*DT2*DT2
         DO I=NODFT,NODLT
          IF(MS(I).GT.0.) THEN
           TMP = ADT / MS(I)
           DY_V(1,I)=DY_V(1,I)-TMP*DY_DAM(1,I)
           DY_V(2,I)=DY_V(2,I)-TMP*DY_DAM(2,I)
           DY_V(3,I)=DY_V(3,I)-TMP*DY_DAM(3,I)
C
           TMP1 = BDT / MS(I)
           DY_D(1,I)=DY_D(1,I)-TMP1*DY_DAM(1,I)
           DY_D(2,I)=DY_D(2,I)-TMP1*DY_DAM(2,I)
           DY_D(3,I)=DY_D(3,I)-TMP1*DY_DAM(3,I)
          ENDIF
         ENDDO
         IF (IRODDL.NE.0) THEN
         DO I=NODFT,NODLT
          IF(IN(I).GT.0.) THEN
           TMP = ADT / IN(I)
           DY_VR(1,I)=DY_VR(1,I)-TMP*DY_DAMR(1,I)
           DY_VR(2,I)=DY_VR(2,I)-TMP*DY_DAMR(2,I)
           DY_VR(3,I)=DY_VR(3,I)-TMP*DY_DAMR(3,I)
C
           TMP1 = BDT / IN(I)
           DY_DR(1,I)=DY_DR(1,I)-TMP1*DY_DAMR(1,I)
           DY_DR(2,I)=DY_DR(2,I)-TMP1*DY_DAMR(2,I)
           DY_DR(3,I)=DY_DR(3,I)-TMP1*DY_DAMR(3,I)
          ENDIF
         ENDDO
         ENDIF
        END IF !IF (IDY_DAMP.EQ.0)
C--------------------------------------------
      RETURN
      END
Chd|====================================================================
Chd|  DYNA_IVFAC                    source/implicit/imp_dyna.F    
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|        SPMD_SUM_S                    source/mpi/implicit/imp_spmd.F
Chd|        IMP_DYNA                      share/modules/impbufdef_mod.F 
Chd|====================================================================
      SUBROUTINE DYNA_IVFAC(NODFT ,NODLT ,MS  ,IN   ,WEIGHT,
     .                      EN_I  ,EFAC  ,ITASK)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMP_DYNA
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
#include "comlock.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com08_c.inc"
#include      "impl2_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL ou REAL*8
      INTEGER NODFT,NODLT,WEIGHT(*),ITASK
      my_real
     .   MS(*)  ,IN(*),EFAC,EN_I
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
      my_real
     .   DT05,VX,VY,VZ,MAS,EN_K
C------------------------------------------
        IF (ITASK.EQ.0) R_N2 = ZERO
C----------------------
      CALL MY_BARRIER
C---------------------
         DT05=ZERO
	 EN_K=ZERO
         DO I = NODFT ,NODLT
          MAS=MS(I)*WEIGHT(I)
          VX = DY_V(1,I) - DT05*DY_A(1,I)
          VY = DY_V(2,I) - DT05*DY_A(2,I)
          VZ = DY_V(3,I) - DT05*DY_A(3,I)
          EN_K= EN_K+ ( VX*VX + VY*VY + VZ*VZ)*UNDEMI*MAS
         ENDDO
        IF(IRODDL.NE.0)THEN
         DO I = NODFT ,NODLT
          MAS=IN(I)*WEIGHT(I)
          VX = DY_VR(1,I) - DT05*DY_AR(1,I)
          VY = DY_VR(2,I) - DT05*DY_AR(2,I)
          VZ = DY_VR(3,I) - DT05*DY_AR(3,I)
          EN_K=EN_K + (VX*VX + VY*VY + VZ*VZ)*UNDEMI*MAS

         ENDDO
        ENDIF
#include "lockon.inc"
          R_N2= R_N2 + EN_K
#include "lockoff.inc"
C----------------------
      CALL MY_BARRIER
C---------------------
       IF (ITASK.EQ.0) CALL SPMD_SUM_S(R_N2)
C----------------------
      CALL MY_BARRIER
C---------------------
	EFAC = R_N2/MAX(EM20,EN_I)
	EFAC = MIN(UN,EFAC)
C--------------------------------------------
      RETURN
      END
Chd|====================================================================
Chd|  DYNA_CPK0                     source/implicit/imp_dyna.F    
Chd|-- called by -----------
Chd|        IMP_SOLV                      source/implicit/imp_solv.F    
Chd|-- calls ---------------
Chd|        CP_INT                        source/implicit/produt_v.F    
Chd|        CP_REAL                       source/implicit/produt_v.F    
Chd|        IMP_DYNA                      share/modules/impbufdef_mod.F 
Chd|====================================================================
      SUBROUTINE DYNA_CPK0(NDDL  ,NNZK  ,IADK  ,JDIK   ,DIAG_K ,
     .                     LT_K  )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMP_DYNA
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include  "impl1_c.inc"
#include  "com01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL ou REAL*8
      INTEGER NDDL  ,NNZK  ,IADK(*)  ,JDIK(*)
      my_real
     .   DIAG_K(*)  ,LT_K(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C------------------------------------------
       IF (ISMDISP.GT.0.AND.(NCYCLE.GT.1.OR.INCONV.NE.1)) RETURN
C
       CALL CP_INT(NDDL+1,IADK,DY_IADK0)
       CALL CP_INT(NNZK,JDIK,DY_JDIK0)
       CALL CP_REAL(NDDL,DIAG_K,DY_DIAK0)
       CALL CP_REAL(NNZK,LT_K,DY_LTK0)
C--------------------------------------------
      RETURN
      END
Chd|====================================================================
Chd|  IMP_DYKV                      source/implicit/imp_dyna.F    
Chd|-- called by -----------
Chd|        IMP_SOLV                      source/implicit/imp_solv.F    
Chd|-- calls ---------------
Chd|        I2_IMPR1                      source/interfaces/interf/i2_imp1.F
Chd|        I2_IMPR2                      source/interfaces/interf/i2_imp1.F
Chd|        IMP_SETB                      source/implicit/imp_setb.F    
Chd|        MAV_LT                        source/implicit/produt_v.F    
Chd|        RBE2_IMPR1                    source/constraints/general/rbe2/rbe2_imp0.F
Chd|        RBE3_IMPR1                    source/constraints/general/rbe3/rbe3_imp0.F
Chd|        RBE3_IMPR2                    source/constraints/general/rbe3/rbe3_imp0.F
Chd|        RBY_IMPR1                     source/constraints/general/rbody/rby_imp0.F
Chd|        RBY_IMPR2                     source/constraints/general/rbody/rby_imp0.F
Chd|        SPMD_SUMF_A                   source/mpi/implicit/imp_spmd.F
Chd|        IMP_DYNA                      share/modules/impbufdef_mod.F 
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE IMP_DYKV(NODFT  ,NODLT   ,IDDL   ,NDOF   ,IKC    ,
     .                    DIAG_K ,IADK    ,JDIK   ,LT_K   ,WEIGHT ,
     1                    RBY    ,X       ,SKEW   ,LPBY   ,NPBY   ,
     2                    NRBYAC ,IRBYAC  ,NINT2  ,IINT2  ,IPARI  ,
     3                    INTBUF_TAB      ,IRBE3  ,LRBE3  ,FRBE3  ,
     4                    IRBE2  ,LRBE2   ,V      ,VR     ,NDDL   ,
     5                    FR_ELEM,IAD_ELEM,MS     ,IN     )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMP_DYNA
      USE INTBUFDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "scr05_c.inc"
#include      "param_c.inc"
#include      "com08_c.inc"
#include      "impl2_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NODFT,NODLT,IDDL(*) ,NDOF(*),IADK(*),
     .        IKC(*),JDIK(*),NDDL ,FR_ELEM(*),IAD_ELEM(2,*)
      INTEGER NINT2 ,IINT2(*),LPBY(*),NPBY(NNPBY,*),
     .        IPARI(NPARI,*), NRBYAC,IRBYAC(*)
      INTEGER WEIGHT(*),IRBE3(*),LRBE3(*),IRBE2(*),LRBE2(*)
C     REAL ou REAL*8
      my_real
     . DIAG_K(*),V(3,*),VR(3,*),LT_K(*),FRBE3(*),
     . RBY(NRBY,*) ,X(3,*) ,SKEW(*),MS(*),IN(*)
      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,ND,NNZ,NKC,N,ID,JR,JI,JB,K1
      my_real
     .  U(NDDL),W(NDDL),ADT1,MKF,MKM,TMP
C------- using v,than v(t+dt) is more stable-especially at beginning-----
        CALL IMP_SETB(V  ,VR    ,IDDL   ,NDOF  ,U    )
c       CALL IMP_SETB(DY_V  ,DY_VR  ,IDDL   ,NDOF  ,U    )
        NNZ=DY_IADK0(NDDL+1)-DY_IADK0(1)
        DO I=1,NDDL
         W(I)=ZERO
        ENDDO
       CALL MAV_LT(
     1            NDDL   ,NNZ   ,DY_IADK0,DY_JDIK0 ,DY_DIAK0,
     2            DY_LTK0,U     ,W     )
C----add aM ------
      IF (IRODDL.EQ.0) THEN
       DO I = NODFT ,NODLT
        ND = IDDL(I)
        MKF = ABS(MS(I))*WEIGHT(I)
        DO J =1,NDOF(I)
         ID = ND + J
         TMP=DAMPA_IMP*MKF*V(J,I)+DAMPB_IMP*W(ID)
         DY_DAM(J,I)=TMP
         W(ID) = TMP
        ENDDO
       ENDDO
      ELSE
       DO I = NODFT ,NODLT
        ND = IDDL(I)
        MKF = ABS(MS(I))*WEIGHT(I)
        MKM = ABS(IN(I))*WEIGHT(I)
        DO J =1,NDOF(I)
         ID = ND + J
         IF (J.GT.3) THEN
          JR=J-3
          TMP=DAMPA_IMP*MKM*VR(JR,I)+DAMPB_IMP*W(ID)
          DY_DAMR(JR,I)=TMP
          W(ID) = TMP
         ELSE
          TMP=DAMPA_IMP*MKF*V(J,I)+DAMPB_IMP*W(ID)
          DY_DAM(J,I)=TMP
          W(ID) = TMP
         ENDIF
        ENDDO
       ENDDO
      END IF
C-------int2,RBE3,rby condense----------
         DO I=1,NINT2
          N=IINT2(I)
          CALL I2_IMPR1(IPARI(1,N),INTBUF_TAB(N) ,
     .                  X  ,NDOF ,IDDL    ,W  )
         ENDDO
         IF (NRBE2.GT.0) THEN
          CALL RBE2_IMPR1(
     1                    IRBE2  ,LRBE2 ,X     ,SKEW   ,NDOF   ,
     2                    IDDL   ,W     ,WEIGHT)
         ENDIF
         IF (NRBE3.GT.0) THEN
          CALL RBE3_IMPR1(
     1                    IRBE3  ,LRBE3 ,FRBE3  ,X     ,SKEW   ,
     2                    NDOF   ,IDDL  ,W      ,WEIGHT)
         ENDIF
         DO I=1,NRBYAC
          N=IRBYAC(I)
          K1=IRBYAC(I+NRBYKIN)+1
          CALL RBY_IMPR1(X, RBY(1,N),LPBY(K1),NPBY(1,N),
     1                   NDOF  ,IDDL   ,W    )
         ENDDO
C-------int2,rby speciale (elems deleted)----------
         DO I=1,NINT2
          N=IINT2(I)
          CALL I2_IMPR2(IPARI(1,N),INTBUF_TAB(N) ,DY_DAM ,DY_DAMR,
     .                  X  ,NDOF ,IDDL    ,W  )
         ENDDO
         IF (NRBE3.GT.0) THEN
          CALL RBE3_IMPR2(
     1                    IRBE3  ,LRBE3 ,FRBE3  ,X     ,SKEW   ,
     2                    NDOF   ,IDDL  ,W      ,WEIGHT,DY_DAM ,
     3                    DY_DAMR)
         ENDIF
         DO I=1,NRBYAC
          N=IRBYAC(I)
          K1=IRBYAC(I+NRBYKIN)+1
          CALL RBY_IMPR2(X, RBY(1,N),LPBY(K1),NPBY(1,N),
     1                   NDOF  ,IDDL   ,W    ,DY_DAM ,DY_DAMR)
         ENDDO
C
       DO I = NODFT ,NODLT
        ND = IDDL(I)
        DO J =1,NDOF(I)
         ID = ND + J
         IF (J.GT.3) THEN
          JR=J-3
          DY_DAMR(JR,I)=W(ID)
         ELSE
          DY_DAM(J,I)=W(ID)
         ENDIF
        ENDDO
       ENDDO
C
       IF (NSPMD.GT.1) THEN
          ND = IAD_ELEM(1,NSPMD+1)-IAD_ELEM(1,1)
          IF (IRODDL.NE.0) THEN
           JR = 6
          ELSE
           JR = 3
          ENDIF
          CALL SPMD_SUMF_A(DY_DAM,DY_DAMR,IAD_ELEM,FR_ELEM,JR,ND)
       ENDIF
C--------------------------------------------
      RETURN
      END
Chd|====================================================================
Chd|  GETDYNA_A                     source/implicit/imp_dyna.F    
Chd|-- called by -----------
Chd|        RGWAL0_IMP                    source/constraints/general/rwall/rgwal0.F
Chd|-- calls ---------------
Chd|        IMP_DYNA                      share/modules/impbufdef_mod.F 
Chd|====================================================================
      SUBROUTINE GETDYNA_A(NODFT,NODLT,A )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMP_DYNA
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include  "impl1_c.inc"
#include  "com01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL ou REAL*8
      INTEGER NODFT,NODLT
      my_real
     .   A(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C------------------------------------------
         DO I=NODFT,NODLT
          A(1,I)=DY_A(1,I)
          A(2,I)=DY_A(2,I)
          A(3,I)=DY_A(3,I)
         ENDDO
C--------------------------------------------
      RETURN
      END
Chd|====================================================================
Chd|  IMP_DYKV0                     source/implicit/imp_dyna.F    
Chd|-- called by -----------
Chd|        IMP_SOLV                      source/implicit/imp_solv.F    
Chd|-- calls ---------------
Chd|        I2_IMPR1                      source/interfaces/interf/i2_imp1.F
Chd|        I2_IMPR2                      source/interfaces/interf/i2_imp1.F
Chd|        IMP_SETB                      source/implicit/imp_setb.F    
Chd|        MAV_LT                        source/implicit/produt_v.F    
Chd|        RBE2_IMPR1                    source/constraints/general/rbe2/rbe2_imp0.F
Chd|        RBE3_IMPR1                    source/constraints/general/rbe3/rbe3_imp0.F
Chd|        RBE3_IMPR2                    source/constraints/general/rbe3/rbe3_imp0.F
Chd|        RBY_IMPR1                     source/constraints/general/rbody/rby_imp0.F
Chd|        RBY_IMPR2                     source/constraints/general/rbody/rby_imp0.F
Chd|        SPMD_SUMF_A                   source/mpi/implicit/imp_spmd.F
Chd|        IMP_DYNA                      share/modules/impbufdef_mod.F 
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE IMP_DYKV0(NODFT  ,NODLT   ,IDDL   ,NDOF   ,IKC    ,
     .                    DIAG_K ,IADK    ,JDIK   ,LT_K   ,WEIGHT ,
     1                    RBY    ,X       ,SKEW   ,LPBY   ,NPBY   ,
     2                    NRBYAC ,IRBYAC  ,NINT2  ,IINT2  ,IPARI  ,
     3                    INTBUF_TAB      ,IRBE3  ,LRBE3  ,FRBE3  ,
     4                    IRBE2  ,LRBE2   ,V      ,VR     ,NDDL   ,
     5                    FR_ELEM,IAD_ELEM,MS     ,IN     )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMP_DYNA
      USE INTBUFDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "scr05_c.inc"
#include      "param_c.inc"
#include      "com08_c.inc"
#include      "impl2_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NODFT,NODLT,IDDL(*) ,NDOF(*),IADK(*),
     .        IKC(*),JDIK(*),NDDL ,FR_ELEM(*),IAD_ELEM(2,*)
      INTEGER NINT2 ,IINT2(*),LPBY(*),NPBY(NNPBY,*),
     .        IPARI(NPARI,*), NRBYAC,IRBYAC(*)
      INTEGER WEIGHT(*),IRBE3(*),LRBE3(*),IRBE2(*),LRBE2(*)
C     REAL ou REAL*8
      my_real
     . DIAG_K(*),V(3,*),VR(3,*),LT_K(*),FRBE3(*),
     . RBY(NRBY,*) ,X(3,*) ,SKEW(*),MS(*),IN(*)

      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,ND,NNZ,NKC,N,ID,JR,JI,JB,K1
      my_real
     .  U(NDDL),W(NDDL),ADT1,MKF,MKM,TMP
C------- estimation of A(t+dt) w/ initial velocity------
        CALL IMP_SETB(DY_D  ,DY_DR  ,IDDL   ,NDOF  ,U    )
        DO I=1,NDDL
         W(I)=ZERO
        ENDDO
	NNZ=IADK(NDDL+1)-IADK(1)
       CALL MAV_LT(
     1             NDDL   ,NNZ   ,IADK,JDIK ,DIAG_K,
     2             LT_K   ,U     ,W     )
C-------int2,RBE3,rby condense----------
         DO I=1,NINT2
          N=IINT2(I)
          JI=IPARI(1,N)
          JB=IPARI(2,N)
          CALL I2_IMPR1(IPARI(1,N),INTBUF_TAB(N) ,
     .                  X  ,NDOF ,IDDL    ,W  )
         ENDDO
         IF (NRBE2.GT.0) THEN
          CALL RBE2_IMPR1(
     1                    IRBE2  ,LRBE2 ,X     ,SKEW   ,NDOF   ,
     2                    IDDL   ,W     ,WEIGHT)
         ENDIF
         IF (NRBE3.GT.0) THEN
          CALL RBE3_IMPR1(
     1                    IRBE3  ,LRBE3 ,FRBE3  ,X     ,SKEW   ,
     2                    NDOF   ,IDDL  ,W      ,WEIGHT)
         ENDIF
         DO I=1,NRBYAC
          N=IRBYAC(I)
          K1=IRBYAC(I+NRBYKIN)+1
          CALL RBY_IMPR1(X, RBY(1,N),LPBY(K1),NPBY(1,N),
     1                   NDOF  ,IDDL   ,W    )
         ENDDO
C-------int2,rby speciale (elems deleted)----------
         DO I=1,NINT2
          N=IINT2(I)
          CALL I2_IMPR2(IPARI(1,N),INTBUF_TAB(N) ,DY_A ,DY_AR,
     .                  X  ,NDOF ,IDDL    ,W  )
         ENDDO
         IF (NRBE3.GT.0) THEN
          CALL RBE3_IMPR2(
     1                    IRBE3  ,LRBE3 ,FRBE3  ,X     ,SKEW   ,
     2                    NDOF   ,IDDL  ,W      ,WEIGHT,DY_A   ,
     3                    DY_AR  )
         ENDIF
         DO I=1,NRBYAC
          N=IRBYAC(I)
          K1=IRBYAC(I+NRBYKIN)+1
          CALL RBY_IMPR2(X, RBY(1,N),LPBY(K1),NPBY(1,N),
     1                   NDOF  ,IDDL   ,W    ,DY_A ,DY_AR)
         ENDDO
C---- ------
      IF (IRODDL.EQ.0) THEN
       DO I = NODFT ,NODLT
        IF(MS(I).GT.0.) THEN
         TMP = UN / MS(I)
         ND = IDDL(I)
         DO J =1,NDOF(I)
          ID = ND + J
          DY_A(J,I)=-TMP*W(ID)
         ENDDO
        END IF !(MS(I).GT.0.) THEN
       ENDDO
      ELSE
       DO I = NODFT ,NODLT
        ND = IDDL(I)
        IF(MS(I).GT.0.) THEN
         TMP = UN / MS(I)
         DO J =1,MIN(3,NDOF(I))
          ID = ND + J
          DY_A(J,I)=-TMP*W(ID)
         ENDDO
        END IF !(MS(I).GT.0.) THEN
C
        IF(IN(I).GT.0.) THEN
         TMP = UN / IN(I)
         DO J =4,NDOF(I)
          ID = ND + J
          DY_AR(J-3,I)=-TMP*W(ID)
         ENDDO
        END IF !(IN(I).GT.0.) THEN
       ENDDO
      END IF
C
       IF (NSPMD.GT.1) THEN
          ND = IAD_ELEM(1,NSPMD+1)-IAD_ELEM(1,1)
          IF (IRODDL.NE.0) THEN
           JR = 6
          ELSE
           JR = 3
          ENDIF
          CALL SPMD_SUMF_A(DY_A,DY_AR,IAD_ELEM,FR_ELEM,JR,ND)
       ENDIF
C--------------------------------------------
      RETURN
      END
Chd|====================================================================
Chd|  IMP_FHHT1                     source/implicit/imp_dyna.F    
Chd|-- called by -----------
Chd|        UPD_RHS                       source/implicit/upd_glob_k.F  
Chd|-- calls ---------------
Chd|        CONDENS_B                     source/implicit/upd_glob_k.F  
Chd|        IMP_DYNA                      share/modules/impbufdef_mod.F 
Chd|====================================================================
      SUBROUTINE IMP_FHHT1(NDDL0   ,NDDL    ,LB    ,IKC   )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMP_DYNA
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "impl1_c.inc"
#include      "impl2_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NDDL0   ,NDDL,IKC(*)
C     REAL ou REAL*8
      my_real
     .  LB(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K
      my_real
     .  DB(NDDL0)
C-----  db in lb-;du to SPMD version---------
      IF (HHT_A.EQ.ZERO) RETURN
C
       DO I=1,NDDL0
        DB(I) = DY_R0(I)
       ENDDO
       CALL CONDENS_B(NDDL0 ,IKC   ,DB   )
       DO I=1,NDDL
        LB(I) = LB(I)+ DB(I)
       ENDDO
C--------------------------------------------
      RETURN
      END

