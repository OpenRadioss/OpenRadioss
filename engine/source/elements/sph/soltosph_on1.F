Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
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
Chd|  SOLTOSPH_ON1                  source/elements/sph/soltosph_on1.F
Chd|-- called by -----------
Chd|        SPHPREP                       source/elements/sph/sphprep.F 
Chd|-- calls ---------------
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|        STARTIMEG                     source/system/timer.F         
Chd|        STOPTIMEG                     source/system/timer.F         
Chd|        ELBUFDEF_MOD                  ../common_source/modules/mat_elem/elbufdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SPHBOX                        share/modules/sphbox.F        
Chd|====================================================================
      SUBROUTINE SOLTOSPH_ON1(
     .   X      ,SPBUF    ,KXSP   ,IXSP     ,IPARTSP ,
     .   IPARG  ,NGROUNC ,IGROUNC ,ELBUF_TAB,ITASK   ,
     .   NOD2SP ,SOL2SPH ,SPH2SOL ,IXS      ,MS      ,
     .   PM     ,IADS    ,ADDCNE  ,FSKYD    ,DMSPH   ,
     .   V      ,ICONTACT)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE SPHBOX
      USE ELBUFDEF_MOD         
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com06_c.inc"
#include      "param_c.inc"
#include      "parit_c.inc"
#include      "scr17_c.inc"
#include      "sphcom.inc"
#include      "task_c.inc"
#include      "units_c.inc"
#include      "vect01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER KXSP(NISP,*),
     .        IPARTSP(*), IPARG(NPARG,*), NGROUNC, 
     .        IGROUNC(*), ITASK, IXSP(KVOISPH,*), NOD2SP(*), 
     .        SOL2SPH(2,*), SPH2SOL(*), IXS(NIXS,*),
     .        IADS(8,*), ADDCNE(*), ICONTACT(*)
      my_real
     .        X(3,*), SPBUF(NSPBUF,*), MS(*), PM(NPROPM,*), FSKYD(*),
     .        DMSPH(*), V(3,*)
      TYPE (ELBUF_STRUCT_), TARGET, DIMENSION(NGROUP) :: ELBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,  N, IP, KP, NG, MG, J, NP, KFT, IG, NELEM,
     .        NEL, OFFSET, NVOIS, M, INOD, JNOD, NN, IPRT, IMAT,
     .        N1, N2, N3, N4, N5, N6, N7, N8,
     .        K1, K2, K3, K4, K5, K6, K7, K8, IERROR,
     .        NODFT, NODLT
      my_real
     .        DM, RHO0, EHOURT, EK, VI2, VXI, VYI, VZI,
     .        VX1, VX2, VX3, VX4, VX5, VX6, VX7, VX8,
     .        VY1, VY2, VY3, VY4, VY5, VY6, VY7, VY8,
     .        VZ1, VZ2, VZ3, VZ4, VZ5, VZ6, VZ7, VZ8
C                                                                    
C-----
      TYPE(G_BUFEL_) ,POINTER :: GBUF, GBUFSP
      TYPE(L_BUFEL_) ,POINTER :: LBUF     
      TYPE(BUF_MAT_) ,POINTER :: MBUF  
C-----------------------------------------------
!$OMP DO SCHEDULE(DYNAMIC,1)
       DO IG = 1, NGROUNC
        NG = IGROUNC(IG)         
        IF(IPARG(8,NG)==1)GOTO 50
        IF (IDDW>0) CALL STARTIMEG(NG)
        DO NELEM = 1,IPARG(2,NG),NVSIZ
          OFFSET = NELEM - 1
          NEL   =IPARG(2,NG)
          NFT   =IPARG(3,NG) + OFFSET
          IAD   =IPARG(4,NG)
          ITY   =IPARG(5,NG)
          IPARTSPH=IPARG(69,NG)
          LFT=1
          LLT=MIN(NVSIZ,NEL-NELEM+1)
          IF(ITY==1.AND.IPARTSPH/=0) THEN
C-----------
            GBUF => ELBUF_TAB(NG)%GBUF
            LBUF => ELBUF_TAB(NG)%BUFLY(1)%LBUF(1,1,1)
            MBUF => ELBUF_TAB(NG)%BUFLY(1)%MAT(1,1,1)
C-----
            DO I=LFT,LLT
              IF(GBUF%OFF(I)/=ZERO) THEN
                N=NFT+I
                DO KP=1,SOL2SPH(2,N)-SOL2SPH(1,N)
                  NP=SOL2SPH(1,N)+KP
                  INOD=KXSP(3,NP)
                  IF(ICONTACT(INOD)/=0)THEN
C
C Solid will be deleted at next cycle
                    GBUF%OFF(I)=FOUR_OVER_5
                    IDEL7NOK=1
#include "lockon.inc"
          WRITE(IOUT,*)
     .' -- PARTICLE INTO CONTACT => DELETE SOLID ELEMENT AT NEXT CYCLE',
     . IXS(NIXS,N)
          WRITE(ISTDO,*)
     .' -- PARTICLE INTO CONTACT => DELETE SOLID ELEMENT AT NEXT CYCLE',
     . IXS(NIXS,N)
#include "lockoff.inc"
                    EXIT
                  END IF
                END DO
              END IF
            ENDDO
          END IF
        END DO
        IF (IDDW>0) CALL STOPTIMEG(NG)
C--------
 50     CONTINUE
       END DO
!$OMP END DO
C-----------------------------------------------
      EHOURT=ZERO
      IF(IPARIT==0)THEN
C-----------------------------------------------
C     PARITH/OFF
C-----------------------------------------------
!$OMP DO SCHEDULE(DYNAMIC,1)
       DO IG = 1, NGROUNC
        NG = IGROUNC(IG)         
        IF(IPARG(8,NG)==1)GOTO 100
        IF (IDDW>0) CALL STARTIMEG(NG)
        DO NELEM = 1,IPARG(2,NG),NVSIZ
          OFFSET = NELEM - 1
          NEL   =IPARG(2,NG)
          NFT   =IPARG(3,NG) + OFFSET
          IAD   =IPARG(4,NG)
          ITY   =IPARG(5,NG)
          IPARTSPH=IPARG(69,NG)
          LFT=1
          LLT=MIN(NVSIZ,NEL-NELEM+1)
          IF(ITY==1.AND.IPARTSPH/=0) THEN
C-----------
            GBUF => ELBUF_TAB(NG)%GBUF
            LBUF => ELBUF_TAB(NG)%BUFLY(1)%LBUF(1,1,1)
            MBUF => ELBUF_TAB(NG)%BUFLY(1)%MAT(1,1,1)
C-----
            DO I=LFT,LLT
              IF(GBUF%OFF(I)==ZERO) THEN
C
C               SOL2SPH(1,N)+1<=I<=SOLSPH(2,N) <=> N==SPH2SOL(I)
                N=NFT+I
                NP=SOL2SPH(1,N)+1
                IF(KXSP(2,NP)<0)THEN
C
C                 Solid must have passed to deleted within THIS cycle !
                  EK=ZERO
                  DO KP=1,SOL2SPH(2,N)-SOL2SPH(1,N)
                    NP=SOL2SPH(1,N)+KP
                    MG =MOD(-KXSP(2,NP),NGROUP+1)
                    KFT=IPARG(3,MG)
                    GBUFSP => ELBUF_TAB(MG)%GBUF
                    KXSP(2,NP)        =ABS(KXSP(2,NP))
                    GBUFSP%OFF(NP-KFT)=ONE
                    SPH2SOL(NP)       =0
C
                    INOD=KXSP(3,NP)
                    VI2= V(1,INOD)*V(1,INOD)
     .                  +V(2,INOD)*V(2,INOD)
     .                  +V(3,INOD)*V(3,INOD)
                    EK=EK+HALF*MS(INOD)*VI2
                  ENDDO
                  N1=IXS(2,N)
                  N2=IXS(3,N)
                  N3=IXS(4,N)
                  N4=IXS(5,N)
                  N5=IXS(6,N)
                  N6=IXS(7,N)
                  N7=IXS(8,N)
                  N8=IXS(9,N)
                  IMAT=IXS(1,N)
                  RHO0=PM(1,IMAT)
                  DM=ONE_OVER_8*GBUF%VOL(I)*RHO0
C lockon.. & echange spmd
                  DMSPH(N1)=DMSPH(N1)+DM
                  DMSPH(N2)=DMSPH(N2)+DM
                  DMSPH(N3)=DMSPH(N3)+DM
                  DMSPH(N4)=DMSPH(N4)+DM
                  DMSPH(N5)=DMSPH(N5)+DM
                  DMSPH(N6)=DMSPH(N6)+DM
                  DMSPH(N7)=DMSPH(N7)+DM
                  DMSPH(N8)=DMSPH(N8)+DM
C----
                  N1=IXS(2,N)
                  VX1=V(1,N1)
                  VY1=V(2,N1)
                  VZ1=V(3,N1)
                  N2=IXS(3,N)
                  VX2=V(1,N2)
                  VY2=V(2,N2)
                  VZ2=V(3,N2)
                  N3=IXS(4,N)
                  VX3=V(1,N3)
                  VY3=V(2,N3)
                  VZ3=V(3,N3)
                  N4=IXS(5,N)
                  VX4=V(1,N4)
                  VY4=V(2,N4)
                  VZ4=V(3,N4)
                  N5=IXS(6,N)
                  VX5=V(1,N5)
                  VY5=V(2,N5)
                  VZ5=V(3,N5)
                  N6=IXS(7,N)
                  VX6=V(1,N6)
                  VY6=V(2,N6)
                  VZ6=V(3,N6)
                  N7=IXS(8,N)
                  VX7=V(1,N7)
                  VY7=V(2,N7)
                  VZ7=V(3,N7)
                  N8=IXS(9,N)
                  VX8=V(1,N8)
                  VY8=V(2,N8)
                  VZ8=V(3,N8)
                  VXI=VX1+VX2+VX3+VX4+VX5+VX6+VX7+VX8
                  VYI=VY1+VY2+VY3+VY4+VY5+VY6+VY7+VY8
                  VZI=VZ1+VZ2+VZ3+VZ4+VZ5+VZ6+VZ7+VZ8
                  VI2=VX1*VX1+VX2*VX2+VX3*VX3+VX4*VX4
     1               +VX5*VX5+VX6*VX6+VX7*VX7+VX8*VX8
     2               +VY1*VY1+VY2*VY2+VY3*VY3+VY4*VY4
     3               +VY5*VY5+VY6*VY6+VY7*VY7+VY8*VY8
     4               +VZ1*VZ1+VZ2*VZ2+VZ3*VZ3+VZ4*VZ4
     5               +VZ5*VZ5+VZ6*VZ6+VZ7*VZ7+VZ8*VZ8
C
C absorbed energy due to remeshing
                  EHOURT=EHOURT+HALF*DM*VI2-EK
                END IF
              END IF
            ENDDO
          END IF
        END DO
        IF (IDDW>0) CALL STOPTIMEG(NG)
C--------
 100      CONTINUE
       END DO
!$OMP END DO
      ELSE ! IPARIT==0
C-----------------------------------------------
C     PARITH/ON
C-----------------------------------------------
       NODFT   = 1+ITASK*NUMNOD/ NTHREAD
       NODLT   = (ITASK+1)*NUMNOD/NTHREAD
       DO N = NODFT, NODLT
         FSKYD(ADDCNE(N):ADDCNE(N+1)-1)=ZERO
       ENDDO
C
       CALL MY_BARRIER
C
!$OMP DO SCHEDULE(DYNAMIC,1)
       DO IG = 1, NGROUNC
        NG = IGROUNC(IG)         
        IF(IPARG(8,NG)==1)GOTO 200
        IF (IDDW>0) CALL STARTIMEG(NG)
        DO NELEM = 1,IPARG(2,NG),NVSIZ
          OFFSET = NELEM - 1
          NEL   =IPARG(2,NG)
          NFT   =IPARG(3,NG) + OFFSET
          IAD   =IPARG(4,NG)
          ITY   =IPARG(5,NG)
          IPARTSPH=IPARG(69,NG)
          LFT=1
          LLT=MIN(NVSIZ,NEL-NELEM+1)
          IF(ITY==1.AND.IPARTSPH/=0) THEN
C-----------
            GBUF => ELBUF_TAB(NG)%GBUF
            LBUF => ELBUF_TAB(NG)%BUFLY(1)%LBUF(1,1,1)
            MBUF => ELBUF_TAB(NG)%BUFLY(1)%MAT(1,1,1)
C-----
            DO I=LFT,LLT
              IF(GBUF%OFF(I)==ZERO) THEN
C
C               SOL2SPH(1,N)+1<=I<=SOLSPH(2,N) <=> N==SPH2SOL(I)
                N=NFT+I
                NP=SOL2SPH(1,N)+1
                IF(KXSP(2,NP)<0)THEN
C
C                 Solid must have passed to deleted within THIS cycle !
                  EK=ZERO
                  DO KP=1,SOL2SPH(2,N)-SOL2SPH(1,N)
                    NP=SOL2SPH(1,N)+KP
                    MG =MOD(-KXSP(2,NP),NGROUP+1)
                    KFT=IPARG(3,MG)
                    GBUFSP => ELBUF_TAB(MG)%GBUF
                    KXSP(2,NP)        =ABS(KXSP(2,NP))
                    GBUFSP%OFF(NP-KFT)=ONE
                    SPH2SOL(NP)       =0
C
                    INOD=KXSP(3,NP)
                    VI2= V(1,INOD)*V(1,INOD)
     .                  +V(2,INOD)*V(2,INOD)
     .                  +V(3,INOD)*V(3,INOD)
                    EK=EK+HALF*MS(INOD)*VI2
                  ENDDO
                  IMAT=IXS(1,N)
                  RHO0=PM(1,IMAT)
                  DM=ONE_OVER_8*GBUF%VOL(I)*RHO0
C lockon.. & echange spmd
                  K1=IADS(1,N)
                  FSKYD(K1)=DM
                  K2=IADS(2,N)
                  FSKYD(K2)=DM
                  K3=IADS(3,N)
                  FSKYD(K3)=DM
                  K4=IADS(4,N)
                  FSKYD(K4)=DM
                  K5=IADS(5,N)
                  FSKYD(K5)=DM
                  K6=IADS(6,N)
                  FSKYD(K6)=DM
                  K7=IADS(7,N)
                  FSKYD(K7)=DM
                  K8=IADS(8,N)
                  FSKYD(K8)=DM
C----
                  N1=IXS(2,N)
                  VX1=V(1,N1)
                  VY1=V(2,N1)
                  VZ1=V(3,N1)
                  N2=IXS(3,N)
                  VX2=V(1,N2)
                  VY2=V(2,N2)
                  VZ2=V(3,N2)
                  N3=IXS(4,N)
                  VX3=V(1,N3)
                  VY3=V(2,N3)
                  VZ3=V(3,N3)
                  N4=IXS(5,N)
                  VX4=V(1,N4)
                  VY4=V(2,N4)
                  VZ4=V(3,N4)
                  N5=IXS(6,N)
                  VX5=V(1,N5)
                  VY5=V(2,N5)
                  VZ5=V(3,N5)
                  N6=IXS(7,N)
                  VX6=V(1,N6)
                  VY6=V(2,N6)
                  VZ6=V(3,N6)
                  N7=IXS(8,N)
                  VX7=V(1,N7)
                  VY7=V(2,N7)
                  VZ7=V(3,N7)
                  N8=IXS(9,N)
                  VX8=V(1,N8)
                  VY8=V(2,N8)
                  VZ8=V(3,N8)
                  VXI=VX1+VX2+VX3+VX4+VX5+VX6+VX7+VX8
                  VYI=VY1+VY2+VY3+VY4+VY5+VY6+VY7+VY8
                  VZI=VZ1+VZ2+VZ3+VZ4+VZ5+VZ6+VZ7+VZ8
                  VI2=VX1*VX1+VX2*VX2+VX3*VX3+VX4*VX4
     1               +VX5*VX5+VX6*VX6+VX7*VX7+VX8*VX8
     2               +VY1*VY1+VY2*VY2+VY3*VY3+VY4*VY4
     3               +VY5*VY5+VY6*VY6+VY7*VY7+VY8*VY8
     4               +VZ1*VZ1+VZ2*VZ2+VZ3*VZ3+VZ4*VZ4
     5               +VZ5*VZ5+VZ6*VZ6+VZ7*VZ7+VZ8*VZ8
C
C absorbed energy due to remeshing
                  EHOURT=EHOURT+HALF*DM*VI2-EK
                END IF
              END IF
            ENDDO
          END IF
        END DO
        IF (IDDW>0) CALL STOPTIMEG(NG)
C--------
 200      CONTINUE
       END DO
!$OMP END DO
C--------
      END IF
C-----------------------------------------------
#include "lockon.inc"
      EHOUR=EHOUR+EHOURT
#include "lockoff.inc"
C-----------------------------------------------
      RETURN
      END SUBROUTINE SOLTOSPH_ON1
Chd|====================================================================
Chd|  SOLTOSPH_ON12                 source/elements/sph/soltosph_on1.F
Chd|-- called by -----------
Chd|        SPHPREP                       source/elements/sph/sphprep.F 
Chd|-- calls ---------------
Chd|        STARTIMEG                     source/system/timer.F         
Chd|        STOPTIMEG                     source/system/timer.F         
Chd|        ELBUFDEF_MOD                  ../common_source/modules/mat_elem/elbufdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SPHBOX                        share/modules/sphbox.F        
Chd|====================================================================
      SUBROUTINE SOLTOSPH_ON12(
     .   X      ,SPBUF    ,KXSP   ,IXSP     ,IPARTSP ,
     .   IPARG  ,NGROUNC ,IGROUNC ,ELBUF_TAB,ITASK   ,
     .   NOD2SP ,SOL2SPH ,SPH2SOL ,IXS      ,MS      ,
     .   PM     ,IADS    ,ADDCNE  ,FSKYD    ,DMSPH   ,
     .   V      ,ICONTACT,IPART)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE SPHBOX
      USE ELBUFDEF_MOD         
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "scr17_c.inc"
#include      "sphcom.inc"
#include      "task_c.inc"
#include      "units_c.inc"
#include      "vect01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER KXSP(NISP,*),
     .        IPARTSP(*), IPARG(NPARG,*), NGROUNC, 
     .        IGROUNC(*), ITASK, IXSP(KVOISPH,*), NOD2SP(*), 
     .        SOL2SPH(2,*), SPH2SOL(*), IXS(NIXS,*),
     .        IADS(8,*), ADDCNE(*), ICONTACT(*), IPART(LIPART1,*)
      my_real
     .        X(3,*), SPBUF(NSPBUF,*), MS(*), PM(NPROPM,*), FSKYD(*),
     .        DMSPH(*), V(3,*)
      TYPE (ELBUF_STRUCT_), TARGET, DIMENSION(NGROUP) :: ELBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,  N, IP, KP, NG, MG, J, NP, KFT, IG, NELEM,
     .        NEL, OFFSET, NVOIS, M, INOD, JNOD, NN, IPRT, IMAT
C                                                                    
C-----
      TYPE(G_BUFEL_) ,POINTER :: GBUF
      TYPE(L_BUFEL_) ,POINTER :: LBUF     
      TYPE(BUF_MAT_) ,POINTER :: MBUF  
C-----------------------------------------------
!$OMP DO SCHEDULE(DYNAMIC,1)
       DO IG = 1, NGROUNC
        NG = IGROUNC(IG)         
        IF(IPARG(8,NG)==1)GOTO 50
        IF (IDDW>0) CALL STARTIMEG(NG)
        DO NELEM = 1,IPARG(2,NG),NVSIZ
          OFFSET = NELEM - 1
          NEL   =IPARG(2,NG)
          NFT   =IPARG(3,NG) + OFFSET
          IAD   =IPARG(4,NG)
          ITY   =IPARG(5,NG)
          IPARTSPH=IPARG(69,NG)
          LFT=1
          LLT=MIN(NVSIZ,NEL-NELEM+1)
          IF(ITY==1.AND.IPARTSPH/=0) THEN
C-----------
            GBUF => ELBUF_TAB(NG)%GBUF
            LBUF => ELBUF_TAB(NG)%BUFLY(1)%LBUF(1,1,1)
            MBUF => ELBUF_TAB(NG)%BUFLY(1)%MAT(1,1,1)
C-----
            IF ((ITSOL2SPH==1).OR.(NSUBS==0)) THEN
C           Desactivation of solid element if id of sph part is different
C-----
            DO I=LFT,LLT
              IF(GBUF%OFF(I)/=ZERO) THEN
                N=NFT+I
                DO KP=1,SOL2SPH(2,N)-SOL2SPH(1,N)
                  NP=SOL2SPH(1,N)+KP
                  INOD=KXSP(3,NP)
                  NVOIS=KXSP(4,NP)
                  DO J=1,NVOIS
                    JNOD=IXSP(J,NP)
                    IF(JNOD>0)THEN
                      M=NOD2SP(JNOD)
                      IF(IPARTSP(M)/=IPARTSP(NP))THEN
C
C Solid will be deleted at next cycle
                          GBUF%OFF(I)=FOUR_OVER_5
                          IDEL7NOK=1
#include "lockon.inc"
                          WRITE(IOUT,5000) IXS(NIXS,N)
                          WRITE(ISTDO,5000) IXS(NIXS,N)
#include "lockoff.inc"
                          GOTO 100
                      END IF
                    ELSE
                      NN = -JNOD
                      IF(NINT(XSPHR(14,NN))/=IPARTSP(NP))THEN
C
C Solid will be deleted at next cycle
                          GBUF%OFF(I)=FOUR_OVER_5
                          IDEL7NOK=1
#include "lockon.inc"
                          WRITE(IOUT,5000) IXS(NIXS,N)
                          WRITE(ISTDO,5000) IXS(NIXS,N)
#include "lockoff.inc"
                          GOTO 100
                        END IF
                    END IF
                  END DO
                END DO
              END IF
 100          CONTINUE
            ENDDO
C-----
            ELSEIF (ITSOL2SPH==2) THEN
C           Desactivation of solid element if id of subset is different
C-----
            DO I=LFT,LLT
              IF(GBUF%OFF(I)/=ZERO) THEN
                N=NFT+I
                DO KP=1,SOL2SPH(2,N)-SOL2SPH(1,N)
                  NP=SOL2SPH(1,N)+KP
                  INOD=KXSP(3,NP)
                  NVOIS=KXSP(4,NP)
                  DO J=1,NVOIS
                    JNOD=IXSP(J,NP)
                    IF(JNOD>0)THEN
                      M=NOD2SP(JNOD)    
                      IF((IPART(3,IPARTSP(M))/=IPART(3,IPARTSP(NP))).OR.
     .                  (((IPART(3,IPARTSP(M))+IPART(3,IPARTSP(NP)))==2*NSUBS).
     .                  AND.(IPARTSP(M)/=IPARTSP(NP)))) THEN
C
C Solid will be deleted at next cycle
                          GBUF%OFF(I)=FOUR_OVER_5
                          IDEL7NOK=1
#include "lockon.inc"
                          WRITE(IOUT,6000) IXS(NIXS,N)
                          WRITE(ISTDO,6000) IXS(NIXS,N)
#include "lockoff.inc"
                          GOTO 200
                      END IF
                    ELSE
                      NN = -JNOD
                      IF((IPART(3,NINT(XSPHR(14,NN)))/=IPART(3,IPARTSP(NP))).OR.
     .                  (((IPART(3,IPARTSP(NP))+IPART(3,NINT(XSPHR(14,NN)))==2*NSUBS).
     .                  AND.(NINT(XSPHR(14,NN))/=IPARTSP(NP))))) THEN
C
C Solid will be deleted at next cycle
                          GBUF%OFF(I)=FOUR_OVER_5
                          IDEL7NOK=1
#include "lockon.inc"
                          WRITE(IOUT,6000) IXS(NIXS,N)
                          WRITE(ISTDO,6000) IXS(NIXS,N)
#include "lockoff.inc"
                          GOTO 200
                        END IF
                    END IF
                  END DO
                END DO
              END IF
 200          CONTINUE
            ENDDO
C-----
            ENDIF
C-----
          END IF
        END DO
        IF (IDDW>0) CALL STOPTIMEG(NG)
C--------
 50     CONTINUE
       END DO
!$OMP END DO
C-----------------------------------------------
 5000 FORMAT(
     & ' -- PARTICLE INTERACTING W/OTHER SPH PART',
     . ' => DELETE SOLID ELEMENT AT NEXT CYCLE',I10)
 6000 FORMAT(
     & ' -- PARTICLE INTERACTING W/OTHER SPH PART OR SUBSET',
     . ' => DELETE SOLID ELEMENT AT NEXT CYCLE',I10)    
C-----------------------------------------------
      RETURN
      END SUBROUTINE SOLTOSPH_ON12
