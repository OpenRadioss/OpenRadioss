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
Chd|  SOLTOSPHA                     source/elements/sph/soltospha.F
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|        STARTIMEG                     source/system/timer.F         
Chd|        STOPTIMEG                     source/system/timer.F         
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SOLTOSPH_MOD                  share/modules/soltosph_mod.F  
Chd|====================================================================
      SUBROUTINE SOLTOSPHA(
     1   ITASK   ,V       ,A        ,MS      ,PM      ,
     2   IPART   ,IXS     ,IPARTS   ,KXSP    ,IPARTSP ,
     3   IRST    ,SPBUF   ,PARTSAV  ,SOL2SPH ,IPARG   ,
     4   NGROUNC ,IGROUNC ,ELBUF_TAB,IGEO)
C-----------------------------------------------
      USE ELBUFDEF_MOD         
      USE SOLTOSPH_MOD         
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com06_c.inc"
#include      "com08_c.inc"
#include      "param_c.inc"
#include      "sphcom.inc"
#include      "scr11_c.inc"
#include      "scr17_c.inc"
#include      "task_c.inc"
#include      "units_c.inc"
#include      "vect01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IXS(NIXS,*), KXSP(NISP,*), ITASK,
     .        IPARTSP(*), IRST(3,*), NGROUNC, IGROUNC(*), 
     .        IPARTS(*), IPARG(NPARG,*), SOL2SPH(2,*), IPART(LIPART1,*),
     .        IGEO(NPROPGI,*)
      my_real
     .        V(3,*), A(3,*), SPBUF(NSPBUF,*), MS(*), PARTSAV(NPSAV,*),
     .        PM(NPROPM,*)
      TYPE (ELBUF_STRUCT_), TARGET, DIMENSION(NGROUP) :: ELBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,  J, IR, IS, IT, KP, NP, N, INOD, IG, NG, MG, NEL,
     .        N1, N2, N3, N4, N5, N6, N7, N8, IPRT, IMAT,
     .        NELEM, OFFSET, NSPHDIR, KFT, IERROR
C                                                                    
      my_real
     .   VX1,VX2,VX3,VX4,VX5,VX6,VX7,VX8,
     .   VY1,VY2,VY3,VY4,VY5,VY6,VY7,VY8,
     .   VZ1,VZ2,VZ3,VZ4,VZ5,VZ6,VZ7,VZ8,
     .   PHI1,PHI2,PHI3,PHI4,PHI5,PHI6,PHI7,PHI8,
     .   KSI, ETA, ZETA, WI,
     .   VXI, VYI, VZI, USDT, DT05,
     .   XMASS_T, XMOMT_T, YMOMT_T, ZMOMT_T, ENCIN_T,
     .   RHO0, MASS, VI2
C-----
      TYPE(G_BUFEL_) ,POINTER :: GBUF, GBUFSP
      TYPE(L_BUFEL_) ,POINTER :: LBUF     
      TYPE(BUF_MAT_) ,POINTER :: MBUF  
C-----------------------------------------------
      my_real
     .  A_GAUSS(9,9),A_GAUSS_TETRA(9,9)
      DATA A_GAUSS /
     1 0.               ,0.               ,0.               ,
     1 0.               ,0.               ,0.               ,
     1 0.               ,0.               ,0.               ,
     2 -.5              ,0.5              ,0.               ,
     2 0.               ,0.               ,0.               ,
     2 0.               ,0.               ,0.               ,
     3 -.666666666666666,0.               ,0.666666666666666,
     3 0.               ,0.               ,0.               ,
     3 0.               ,0.               ,0.               ,
     4 -.75             ,-.25             ,0.25             ,
     4 0.75             ,0.               ,0.               ,
     4 0.               ,0.               ,0.               ,
     5 -.8              ,-.4              ,0.               ,
     5 0.4              ,0.8              ,0.               ,
     5 0.               ,0.               ,0.               ,
     6 -.833333333333333,-.5              ,-.166666666666666,
     6 0.166666666666666,0.5              ,0.833333333333333,
     6 0.               ,0.               ,0.               ,
     7 -.857142857142857,-.571428571428571,-.285714285714285,
     7 0.               ,0.285714285714285,0.571428571428571,
     7 0.857142857142857,0.               ,0.               ,
     8 -.875            ,-.625            ,-.375            ,
     8 -.125            ,0.125            ,0.375,
     8 0.625            ,0.875            ,0.               ,
     9 -.888888888888888,-.666666666666666,-.444444444444444,
     9 -.222222222222222,0.               ,0.222222222222222,
     9 0.444444444444444,0.666666666666666,0.888888888888888/
C-----------------------------------------------
      DATA A_GAUSS_TETRA /
     1 0.250000000000000,0.000000000000000,0.000000000000000,
     1 0.000000000000000,0.000000000000000,0.000000000000000,
     1 0.000000000000000,0.000000000000000,0.000000000000000,
     2 0.166666666666667,0.500000000000000,0.000000000000000,
     2 0.000000000000000,0.000000000000000,0.000000000000000,
     2 0.000000000000000,0.000000000000000,0.000000000000000,
     3 0.125000000000000,0.375000000000000,0.625000000000000,
     3 0.000000000000000,0.000000000000000,0.000000000000000,
     3 0.000000000000000,0.000000000000000,0.000000000000000,
     4 0.100000000000000,0.300000000000000,0.500000000000000,
     4 0.700000000000000,0.000000000000000,0.000000000000000,
     4 0.000000000000000,0.000000000000000,0.000000000000000,
     5 0.083333333333333,0.250000000000000,0.416666666666667,
     5 0.583333333333333,0.750000000000000,0.000000000000000,
     5 0.000000000000000,0.000000000000000,0.000000000000000,
     6 0.071428571428571,0.214285714285714,0.357142857142857,
     6 0.500000000000000,0.642857142857143,0.785714285714286,
     6 0.000000000000000,0.000000000000000,0.000000000000000,
     7 0.062500000000000,0.187500000000000,0.312500000000000,
     7 0.437500000000000,0.562500000000000,0.687500000000000,
     7 0.812500000000000,0.000000000000000,0.000000000000000,
     8 0.055555555555556,0.166666666666667,0.277777777777778,
     8 0.388888888888889,0.500000000000000,0.611111111111111,
     8 0.722222222222222,0.833333333333333,0.000000000000000,
     9 0.050000000000000,0.150000000000000,0.250000000000000,
     9 0.350000000000000,0.450000000000000,0.550000000000000,
     9 0.650000000000000,0.750000000000000,0.850000000000000/
C-----------------------------------------------
      IF(ITASK==0)THEN
        ALLOCATE(WPARTSAV(NTHREAD,NPSAV,NPART),STAT=IERROR)
        IF(IERROR/=0) THEN
          CALL ANCMSG(MSGID=19,ANMODE=ANINFO,
     .                C1='(SOLIDS to SPH)')
          CALL ARRET(2)
        ENDIF
      END IF
C
      CALL MY_BARRIER
C
      USDT=ONE/DT12
      DT05=HALF*DT1
      XMASS_T=ZERO
      XMOMT_T=ZERO
      YMOMT_T=ZERO
      ZMOMT_T=ZERO
      ENCIN_T=ZERO
      DO IPRT=ITASK+1,NPART,NTHREAD
      DO J=1,NPSAV
      DO I=1,NTHREAD
          WPARTSAV(I,J,IPRT)=ZERO
      END DO
      END DO
      END DO
C
!$OMP DO SCHEDULE(DYNAMIC,1)
      DO IG = 1, NGROUNC
        NG = IGROUNC(IG)         
        IF(IPARG(8,NG)==1)GOTO 250
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
C----TETRA---     
C-----
            IF (IPARG(28,NG)==4) THEN
C-----
              DO I=LFT,LLT
              N=NFT+I
              IF(GBUF%OFF(I)==ZERO) CYCLE
C
C             SOL2SPH(1,N)+1<=I<=SOLSPH(2,N) <=> N==SPH2SOL(I)
              N1=IXS(2,N)
              VX1=V(1,N1)+DT12*A(1,N1)
              VY1=V(2,N1)+DT12*A(2,N1)
              VZ1=V(3,N1)+DT12*A(3,N1)
              N2=IXS(4,N)
              VX2=V(1,N2)+DT12*A(1,N2)
              VY2=V(2,N2)+DT12*A(2,N2)
              VZ2=V(3,N2)+DT12*A(3,N2)
              N3=IXS(7,N)
              VX3=V(1,N3)+DT12*A(1,N3)
              VY3=V(2,N3)+DT12*A(2,N3)
              VZ3=V(3,N3)+DT12*A(3,N3)
              N4=IXS(6,N)
              VX4=V(1,N4)+DT12*A(1,N4)
              VY4=V(2,N4)+DT12*A(2,N4)
              VZ4=V(3,N4)+DT12*A(3,N4)
C
              NSPHDIR=IGEO(37,IXS(10,N))
C-----------------------------------------------
              DO KP=1,SOL2SPH(2,N)-SOL2SPH(1,N)
                NP  =SOL2SPH(1,N)+KP
                IR=IRST(1,NP-FIRST_SPHSOL+1)
                IS=IRST(2,NP-FIRST_SPHSOL+1)
                IT=IRST(3,NP-FIRST_SPHSOL+1)
                KSI  = A_GAUSS_TETRA(IR,NSPHDIR)
                ETA  = A_GAUSS_TETRA(IS,NSPHDIR)
                ZETA = A_GAUSS_TETRA(IT,NSPHDIR)
C
                PHI1=KSI
                PHI2=ETA
                PHI3=ZETA
                PHI4=1-KSI-ETA-ZETA
C
                VXI=PHI1*VX1+PHI2*VX2+PHI3*VX3+PHI4*VX4
                VYI=PHI1*VY1+PHI2*VY2+PHI3*VY3+PHI4*VY4
                VZI=PHI1*VZ1+PHI2*VZ2+PHI3*VZ3+PHI4*VZ4
C
                INOD=KXSP(3,NP)
                A(1,INOD)=(VXI-V(1,INOD))*USDT
                A(2,INOD)=(VYI-V(2,INOD))*USDT
                A(3,INOD)=(VZI-V(3,INOD))*USDT
              ENDDO
              ENDDO
C-----
            ELSE
C-----
C----HEXA---     
C-----
            DO I=LFT,LLT
              N=NFT+I
              IF(GBUF%OFF(I)==ZERO) CYCLE
C
C             SOL2SPH(1,N)+1<=I<=SOLSPH(2,N) <=> N==SPH2SOL(I)
              N1=IXS(2,N)
              VX1=V(1,N1)+DT12*A(1,N1)
              VY1=V(2,N1)+DT12*A(2,N1)
              VZ1=V(3,N1)+DT12*A(3,N1)
              N2=IXS(3,N)
              VX2=V(1,N2)+DT12*A(1,N2)
              VY2=V(2,N2)+DT12*A(2,N2)
              VZ2=V(3,N2)+DT12*A(3,N2)
              N3=IXS(4,N)
              VX3=V(1,N3)+DT12*A(1,N3)
              VY3=V(2,N3)+DT12*A(2,N3)
              VZ3=V(3,N3)+DT12*A(3,N3)
              N4=IXS(5,N)
              VX4=V(1,N4)+DT12*A(1,N4)
              VY4=V(2,N4)+DT12*A(2,N4)
              VZ4=V(3,N4)+DT12*A(3,N4)
              N5=IXS(6,N)
              VX5=V(1,N5)+DT12*A(1,N5)
              VY5=V(2,N5)+DT12*A(2,N5)
              VZ5=V(3,N5)+DT12*A(3,N5)
              N6=IXS(7,N)
              VX6=V(1,N6)+DT12*A(1,N6)
              VY6=V(2,N6)+DT12*A(2,N6)
              VZ6=V(3,N6)+DT12*A(3,N6)
              N7=IXS(8,N)
              VX7=V(1,N7)+DT12*A(1,N7)
              VY7=V(2,N7)+DT12*A(2,N7)
              VZ7=V(3,N7)+DT12*A(3,N7)
              N8=IXS(9,N)
              VX8=V(1,N8)+DT12*A(1,N8)
              VY8=V(2,N8)+DT12*A(2,N8)
              VZ8=V(3,N8)+DT12*A(3,N8)
C
C
              NSPHDIR=NINT((SOL2SPH(2,N)-SOL2SPH(1,N))**THIRD)
C-----------------------------------------------
              DO KP=1,SOL2SPH(2,N)-SOL2SPH(1,N)
                NP  =SOL2SPH(1,N)+KP
                IR=IRST(1,NP-FIRST_SPHSOL+1)
                IS=IRST(2,NP-FIRST_SPHSOL+1)
                IT=IRST(3,NP-FIRST_SPHSOL+1)
                KSI  = A_GAUSS(IR,NSPHDIR)
                ETA  = A_GAUSS(IS,NSPHDIR)
                ZETA = A_GAUSS(IT,NSPHDIR)
C
                PHI1=(ONE-KSI)*(ONE-ETA)*(ONE-ZETA)
                PHI2=(ONE-KSI)*(ONE-ETA)*(ONE+ZETA)
                PHI3=(ONE+KSI)*(ONE-ETA)*(ONE+ZETA)
                PHI4=(ONE+KSI)*(ONE-ETA)*(ONE-ZETA)
                PHI5=(ONE-KSI)*(ONE+ETA)*(ONE-ZETA)
                PHI6=(ONE-KSI)*(ONE+ETA)*(ONE+ZETA)
                PHI7=(ONE+KSI)*(ONE+ETA)*(ONE+ZETA)
                PHI8=(ONE+KSI)*(ONE+ETA)*(ONE-ZETA)
                VXI=ONE_OVER_8*(PHI1*VX1+PHI2*VX2+PHI3*VX3+PHI4*VX4+
     .                      PHI5*VX5+PHI6*VX6+PHI7*VX7+PHI8*VX8)
                VYI=ONE_OVER_8*(PHI1*VY1+PHI2*VY2+PHI3*VY3+PHI4*VY4+
     .                      PHI5*VY5+PHI6*VY6+PHI7*VY7+PHI8*VY8)
                VZI=ONE_OVER_8*(PHI1*VZ1+PHI2*VZ2+PHI3*VZ3+PHI4*VZ4+
     .                      PHI5*VZ5+PHI6*VZ6+PHI7*VZ7+PHI8*VZ8)
C
                INOD=KXSP(3,NP)
                A(1,INOD)=(VXI-V(1,INOD))*USDT
                A(2,INOD)=(VYI-V(2,INOD))*USDT
                A(3,INOD)=(VZI-V(3,INOD))*USDT
              ENDDO
            ENDDO
C-----
          ENDIF
C-----
          ENDIF
          IF (IDDW>0) CALL STOPTIMEG(NG)
        END DO
C--------
 250    CONTINUE
      END DO
!$OMP END DO
C
!$OMP DO SCHEDULE(DYNAMIC,1)
      DO IG = 1, NGROUNC
        NG = IGROUNC(IG)         
        IF(IPARG(8,NG)==1)GOTO 350
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
              N=NFT+I
              IF(GBUF%OFF(I)/=ZERO) THEN
C
C               SOL2SPH(1,N)+1<=I<=SOLSPH(2,N) <=> N==SPH2SOL(I)
C
                DO KP=1,SOL2SPH(2,N)-SOL2SPH(1,N)
C
                  NP=SOL2SPH(1,N)+KP
                  MG =MOD(-KXSP(2,NP),NGROUP+1)
C
                  INOD=KXSP(3,NP)
                  VXI=V(1,INOD)+DT05*A(1,INOD)
                  VYI=V(2,INOD)+DT05*A(2,INOD)
                  VZI=V(3,INOD)+DT05*A(3,INOD)
                  XMASS_T=XMASS_T-MS(INOD)
                  XMOMT_T=XMOMT_T-MS(INOD)*VXI
                  YMOMT_T=YMOMT_T-MS(INOD)*VYI
                  ZMOMT_T=ZMOMT_T-MS(INOD)*VZI
                  ENCIN_T=ENCIN_T
     .                   -HALF*MS(INOD)*(VXI*VXI+VYI*VYI+VZI*VZI)
C
C                 IPARG(8,MG)==1 <=> group was not computed 
C                 (there is no cloud active particle within the group)
                  IF(IPARG(8,MG)==1)CYCLE
C
                  KFT=IPARG(3,MG)
                  GBUFSP => ELBUF_TAB(MG)%GBUF
                  IPRT=IPARTSP(NP)
                  WPARTSAV(ITASK+1,1,IPRT)=WPARTSAV(ITASK+1,1,IPRT)
     .              -GBUFSP%VOL(NP-KFT)*GBUFSP%EINT(NP-KFT)
                  WPARTSAV(ITASK+1,2,IPRT)=WPARTSAV(ITASK+1,2,IPRT)
     .              -HALF*MS(INOD)*(VXI*VXI+VYI*VYI+VZI*VZI)
                  WPARTSAV(ITASK+1,3,IPRT)=WPARTSAV(ITASK+1,3,IPRT)
     .              -MS(INOD)*VXI
                  WPARTSAV(ITASK+1,4,IPRT)=WPARTSAV(ITASK+1,4,IPRT)
     .              -MS(INOD)*VYI
                  WPARTSAV(ITASK+1,5,IPRT)=WPARTSAV(ITASK+1,5,IPRT)
     .              -MS(INOD)*VZI
                  WPARTSAV(ITASK+1,6,IPRT)=WPARTSAV(ITASK+1,6,IPRT)
     .              -MS(INOD)
C
                ENDDO
              END IF
            END DO
          END IF
        END DO
        IF (IDDW>0) CALL STOPTIMEG(NG)
 350    CONTINUE
      END DO
!$OMP END DO
C
#include "lockon.inc"
      ENCIN=ENCIN + ENCIN_T     
      XMASS=XMASS + XMASS_T
      XMOMT=XMOMT + XMOMT_T
      YMOMT=YMOMT + YMOMT_T
      ZMOMT=ZMOMT + ZMOMT_T
#include "lockoff.inc"
      DO IPRT=ITASK+1,NPART,NTHREAD
      DO J=1,NPSAV
      DO I=1,NTHREAD
          PARTSAV(J,IPRT)=PARTSAV(J,IPRT)+WPARTSAV(I,J,IPRT)
      END DO
      END DO
      END DO
C
      CALL MY_BARRIER
C
      IF(ITASK==0) DEALLOCATE(WPARTSAV)
C-----------------------------------------------
      RETURN
      END SUBROUTINE SOLTOSPHA
