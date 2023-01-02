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
Chd|  MOD_SPLISSV                   source/elements/sph/splissv.F 
Chd|-- called by -----------
Chd|        SPLISSV                       source/elements/sph/splissv.F 
Chd|        SPONFV                        source/elements/sph/sponfv.F  
Chd|-- calls ---------------
Chd|====================================================================
      MODULE MOD_SPLISSV
      implicit none
#include      "my_real.inc"
      DOUBLE PRECISION, DIMENSION(:,:,:), ALLOCATABLE :: AS6, A6
      my_real
     .       , DIMENSION(:,:), ALLOCATABLE :: AS
      my_real
     .       , DIMENSION(:,:), ALLOCATABLE :: ASPHR
      INTEGER, DIMENSION(:), ALLOCATABLE ::  ITAG
      END MODULE MOD_SPLISSV
Chd|====================================================================
Chd|  SPLISSV                       source/elements/sph/splissv.F 
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        FOAT_TO_6_FLOAT               source/system/parit.F         
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|        SPMD_EXCH_A_SOL2SPH           source/mpi/elements/spmd_sph.F
Chd|        SPMD_SPHGETF                  source/mpi/elements/spmd_sph.F
Chd|        STARTIMEG                     source/system/timer.F         
Chd|        STOPTIMEG                     source/system/timer.F         
Chd|        WEIGHT0                       source/elements/sph/weight.F  
Chd|        GET_U_GEO                     source/user_interface/upidmid.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        MOD_SPLISSV                   source/elements/sph/splissv.F 
Chd|        SPHBOX                        share/modules/sphbox.F        
Chd|====================================================================
      SUBROUTINE SPLISSV(
     1    X       ,V       ,MS      ,A       ,SPBUF   ,
     2    WA      ,ITAB    ,KXSP    ,IXSP    ,NOD2SP  ,
     3    D       ,ISPSYM  ,XSPSYM  ,VSPSYM  ,BUFMAT  ,
     4    BUFGEO  ,NPC     ,PLD     ,PM      ,GEO     ,
     5    ISPCOND ,XFRAME  ,WASPSYM ,IPARTSP ,PARTSAV ,
     6    WACOMP  ,WSMCOMP ,WASPACT ,IPART   ,ITASK   ,
     7    SPH2SOL ,SOL2SPH ,IRST    ,IXS     ,IPARG   ,
     8    NGROUNC ,IGROUNC ,ELBUF_TAB,IAD_ELEM,FR_ELEM,
     9    IGEO    ,SOL2SPH_TYP)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD         
      USE MESSAGE_MOD
      USE SPHBOX
      USE MOD_SPLISSV
C----6---------------------------------------------------------------7---------8
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
#include      "vect01_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com06_c.inc"
#include      "com08_c.inc"
#include      "sphcom.inc"
#include      "param_c.inc"
#include      "parit_c.inc"
#include      "scr05_c.inc"
#include      "scr17_c.inc"
#include      "scr18_c.inc"
#include      "task_c.inc"
#include      "units_c.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER KXSP(NISP,*),IXSP(KVOISPH,*),NOD2SP(*),ITAB(*),
     .        ISPSYM(NSPCOND,*),NPC(*),ISPCOND(NISPCOND,*),
     .        IPARTSP(*),WASPACT(*),IPART(LIPART1,*), ITASK,
     .        SPH2SOL(*),IXS(NIXS,*),IRST(3,*),SOL2SPH(2,*),
     .        IPARG(NPARG,*), NGROUNC, IGROUNC(*),
     .        IAD_ELEM(2,*),FR_ELEM(*),IGEO(NPROPGI,*),SOL2SPH_TYP(*)
C     REAL
      my_real
     .   X(3,*)    ,V(3,*)   ,MS(*)   ,
     .   A(3,*)    ,SPBUF(NSPBUF,*)  ,WA(*),
     .   D(3,*)    ,XSPSYM(3,*)   ,VSPSYM(3,*),
     .   PM(NPROPM,*), GEO(NPROPG,*),BUFMAT(*),BUFGEO(*),PLD(*),
     .   XFRAME(NXFRAME,*), WASPSYM(3,*), PARTSAV(NPSAV,*),
     .   WACOMP(16,*), WSMCOMP(6,*)
      TYPE (ELBUF_STRUCT_), TARGET, DIMENSION(NGROUP) :: ELBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER N,INOD,JNOD,J,NVOIS,M,NN,
     .        IPROP,IMAT,I,
     .        NVOISS,SM,NC,JS,
     .        IS,IC,ISLIDE,IPRT,NS,MYADRN, 
     .        NSOL, NSKI, N1, N2, N3, N4, N5, N6, N7, N8, 
     .        IR, IT, NSPHDIR, KP, NP,NELEM, OFFSET, NEL, IG, NG, 
     .        II, II1, SIZE, LENR, IERROR
      my_real
     .       XI,YI,ZI,DI,RHOI,PRESI,XJ,YJ,ZJ,DJ,PRESJ,RHOJ,DIJ,
     .       VXI,VYI,VZI,VXJ,VYJ,VZJ,
     .       VJ,VJX,VJY,VJZ,
     .       DRHO,WGHT,DIVV,
     .       MUIJ,MUMAX,
     .       ALPCI,ALPCJ,FACT,FACI,FACJ,
     .       WAX,WAY,WAZ,AXI,AYI,AZI,AXJ,AYJ,AZJ,AN,
     .       VV,KV,EHOURT,DTINV,
     .       OX,OY,OZ,NX,NY,NZ,AXS,AYS,AZS,
     .       ALPHAI,BETAXI,BETAYI,BETAZI,BETAI,
     .       ALPHAJ,BETAXJ,BETAYJ,BETAZJ,BETAJ,UNM,
     .       VX1,VX2,VX3,VX4,VX5,VX6,VX7,VX8,
     .       VY1,VY2,VY3,VY4,VY5,VY6,VY7,VY8,
     .       VZ1,VZ2,VZ3,VZ4,VZ5,VZ6,VZ7,VZ8,USDT,
     .       PHI1,PHI2,PHI3,PHI4,PHI5,PHI6,PHI7,PHI8,
     .       KSI, ETA, ZETA
C-----------------------------------------------
      my_real  GET_U_GEO
      EXTERNAL GET_U_GEO
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
C
      IF(SOL2SPH_FLAG/=0)THEN
        IF(ITASK==0)THEN
C
          ALLOCATE(AS(3,8*NSPHACT),STAT=IERROR)
          IF(IERROR/=0) THEN
            CALL ANCMSG(MSGID=19,ANMODE=ANINFO,
     .           C1='(SOL2SPH)')
            CALL ARRET(2)
          END IF
C
          ALLOCATE(AS6(6,3,8*NSPHACT),STAT=IERROR)
          IF(IERROR/=0) THEN
            CALL ANCMSG(MSGID=19,ANMODE=ANINFO,
     .           C1='(SOL2SPH)')
            CALL ARRET(2)
          END IF
C
          ALLOCATE(A6(6,3,NUMNOD),STAT=IERROR)
          IF(IERROR/=0) THEN
            CALL ANCMSG(MSGID=19,ANMODE=ANINFO,
     .           C1='(SOL2SPH)')
            CALL ARRET(2)
          END IF
          A6  = ZERO
C
          ALLOCATE(ITAG(NUMNOD),STAT=IERROR)
          IF(IERROR/=0) THEN
            CALL ANCMSG(MSGID=19,ANMODE=ANINFO,
     .           C1='(SOL2SPH)')
            CALL ARRET(2)
          END IF
          ITAG= 0
        END IF
      ENDIF
C
      IF (NUMSPH > 0) THEN
C
C-----------------------------------------------
C       mean accelerations on cloud active particles 
C       a optimiser : traiter NSPHACT only !!!
C-----------------------------------------------

      IF (NSPHSOL > 0) THEN
        USDT=ONE/DT12
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
             IF (IPARG(28,NG)==4) THEN
C-----
C----TETRA---
C-----
              DO I=LFT,LLT
                N=NFT+I
                IF(GBUF%OFF(I)==ZERO) CYCLE
C
C               SOL2SPH(1,N)+1<=I<=SOLSPH(2,N) <=> N==SPH2SOL(I)
                N1=IXS(2,N)
                VX1=V(1,N1)+DT12*A(1,N1)/MS(N1)
                VY1=V(2,N1)+DT12*A(2,N1)/MS(N1)
                VZ1=V(3,N1)+DT12*A(3,N1)/MS(N1)
                N2=IXS(4,N)
                VX2=V(1,N2)+DT12*A(1,N2)/MS(N2)
                VY2=V(2,N2)+DT12*A(2,N2)/MS(N2)
                VZ2=V(3,N2)+DT12*A(3,N2)/MS(N2)
                N3=IXS(7,N)
                VX3=V(1,N3)+DT12*A(1,N3)/MS(N3)
                VY3=V(2,N3)+DT12*A(2,N3)/MS(N3)
                VZ3=V(3,N3)+DT12*A(3,N3)/MS(N3)
                N4=IXS(6,N)
                VX4=V(1,N4)+DT12*A(1,N4)/MS(N4)
                VY4=V(2,N4)+DT12*A(2,N4)/MS(N4)
                VZ4=V(3,N4)+DT12*A(3,N4)/MS(N4)
C
                NSPHDIR=IGEO(37,IXS(10,N))
C-----------------------------------------------
                DO KP=1,SOL2SPH(2,N)-SOL2SPH(1,N)
                  NP  =SOL2SPH(1,N)+KP
                  IR=IRST(1,NP-FIRST_SPHSOL+1)
                  IS=IRST(2,NP-FIRST_SPHSOL+1)
                  IT=IRST(3,NP-FIRST_SPHSOL+1)
C
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
                  A(1,INOD)=MS(INOD)*(VXI-V(1,INOD))*USDT
                  A(2,INOD)=MS(INOD)*(VYI-V(2,INOD))*USDT
                  A(3,INOD)=MS(INOD)*(VZI-V(3,INOD))*USDT
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
C               SOL2SPH(1,N)+1<=I<=SOLSPH(2,N) <=> N==SPH2SOL(I)
                N1=IXS(2,N)
                VX1=V(1,N1)+DT12*A(1,N1)/MS(N1)
                VY1=V(2,N1)+DT12*A(2,N1)/MS(N1)
                VZ1=V(3,N1)+DT12*A(3,N1)/MS(N1)
                N2=IXS(3,N)
                VX2=V(1,N2)+DT12*A(1,N2)/MS(N2)
                VY2=V(2,N2)+DT12*A(2,N2)/MS(N2)
                VZ2=V(3,N2)+DT12*A(3,N2)/MS(N2)
                N3=IXS(4,N)
                VX3=V(1,N3)+DT12*A(1,N3)/MS(N3)
                VY3=V(2,N3)+DT12*A(2,N3)/MS(N3)
                VZ3=V(3,N3)+DT12*A(3,N3)/MS(N3)
                N4=IXS(5,N)
                VX4=V(1,N4)+DT12*A(1,N4)/MS(N4)
                VY4=V(2,N4)+DT12*A(2,N4)/MS(N4)
                VZ4=V(3,N4)+DT12*A(3,N4)/MS(N4)
                N5=IXS(6,N)
                VX5=V(1,N5)+DT12*A(1,N5)/MS(N5)
                VY5=V(2,N5)+DT12*A(2,N5)/MS(N5)
                VZ5=V(3,N5)+DT12*A(3,N5)/MS(N5)
                N6=IXS(7,N)
                VX6=V(1,N6)+DT12*A(1,N6)/MS(N6)
                VY6=V(2,N6)+DT12*A(2,N6)/MS(N6)
                VZ6=V(3,N6)+DT12*A(3,N6)/MS(N6)
                N7=IXS(8,N)
                VX7=V(1,N7)+DT12*A(1,N7)/MS(N7)
                VY7=V(2,N7)+DT12*A(2,N7)/MS(N7)
                VZ7=V(3,N7)+DT12*A(3,N7)/MS(N7)
                N8=IXS(9,N)
                VX8=V(1,N8)+DT12*A(1,N8)/MS(N8)
                VY8=V(2,N8)+DT12*A(2,N8)/MS(N8)
                VZ8=V(3,N8)+DT12*A(3,N8)/MS(N8)
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
     .                        PHI5*VX5+PHI6*VX6+PHI7*VX7+PHI8*VX8)
                  VYI=ONE_OVER_8*(PHI1*VY1+PHI2*VY2+PHI3*VY3+PHI4*VY4+
     .                        PHI5*VY5+PHI6*VY6+PHI7*VY7+PHI8*VY8)
                  VZI=ONE_OVER_8*(PHI1*VZ1+PHI2*VZ2+PHI3*VZ3+PHI4*VZ4+
     .                        PHI5*VZ5+PHI6*VZ6+PHI7*VZ7+PHI8*VZ8)
                  INOD=KXSP(3,NP)
                  A(1,INOD)=MS(INOD)*(VXI-V(1,INOD))*USDT
                  A(2,INOD)=MS(INOD)*(VYI-V(2,INOD))*USDT
                  A(3,INOD)=MS(INOD)*(VZI-V(3,INOD))*USDT
                ENDDO
              ENDDO
C-----
             ENDIF
C-----
            ENDIF
            IF (IDDW>0) CALL STOPTIMEG(NG)
          END DO
C--------
 250      CONTINUE
        END DO
!$OMP END DO
      END IF

C-----------------------------------------------
C Comm H et A sur cellules remotes
C-----------------------------------------------
      IF(NSPMD>1)THEN
        IF(ITASK==0) THEN
          ALLOCATE(ASPHR(4,NSPHR))
          CALL SPMD_SPHGETF(KXSP,SPBUF,A,MS,ASPHR)
c          IF(NSPHIO/=0.AND.NSPHACT/=0)THEN 
c               CALL SPMD_SPHGETV(KXSP,SPBUF,V,MS)
c          ENDIF
        END IF
        CALL MY_BARRIER()
      END IF
C-----------------------------------------------
C     compute accelerations of ghost particles (temporary storage).
C-----------------------------------------------
      DO NC=1,NSPCOND
        IS=ISPCOND(3,NC)
        IC=ISPCOND(2,NC)
        ISLIDE=ISPCOND(5,NC)
        OX=XFRAME(10,IS)
        OY=XFRAME(11,IS)
        OZ=XFRAME(12,IS)
        NX=XFRAME(3*(IC-1)+1,IS)
        NY=XFRAME(3*(IC-1)+2,IS)
        NZ=XFRAME(3*(IC-1)+3,IS)
        DO NS =ITASK+1,NSPHACT,NTHREAD
         N=WASPACT(NS)
         JS=ISPSYM(NC,N)
         IF(JS>0)THEN
          INOD=KXSP(3,N)
          XI =X(1,INOD)
          YI =X(2,INOD)
          ZI =X(3,INOD)
          AXI=A(1,INOD)
          AYI=A(2,INOD)
          AZI=A(3,INOD)
          IF(ISLIDE==0)THEN
           AXS=-AXI
           AYS=-AYI
           AZS=-AZI
          ELSE
           AN=AXI*NX+AYI*NY+AZI*NZ
           AXS=AXI-TWO*AN*NX
           AYS=AYI-TWO*AN*NY
           AZS=AZI-TWO*AN*NZ	   
          ENDIF
          WASPSYM(1,JS)=AXS
          WASPSYM(2,JS)=AYS
          WASPSYM(3,JS)=AZS
         ENDIF
        ENDDO
C
C Particules symetriques de particules remotes
C
        DO NS = ITASK+1,NSPHR,NTHREAD
         JS=ISPSYMR(NC,NS)
         IF(JS>0)THEN
          XI =XSPHR(3,NS)
          YI =XSPHR(4,NS)
          ZI =XSPHR(5,NS)
          AXI=ASPHR(1,NS)
          AYI=ASPHR(2,NS)
          AZI=ASPHR(3,NS)
          IF(ISLIDE==0)THEN
           AXS=-AXI
           AYS=-AYI
           AZS=-AZI
          ELSE
           AN=AXI*NX+AYI*NY+AZI*NZ
           AXS=AXI-TWO*AN*NX
           AYS=AYI-TWO*AN*NY
           AZS=AZI-TWO*AN*NZ	   
          ENDIF
          WASPSYM(1,JS)=AXS
          WASPSYM(2,JS)=AYS
          WASPSYM(3,JS)=AZS
         ENDIF
        END DO
      ENDDO
C
C    /---------------/
      CALL MY_BARRIER
C    /---------------/
C-------------------------------------------
      EHOURT=ZERO
      DTINV=ONE/DT12
      DO NS =ITASK+1,NSPHACT,NTHREAD
        N=WASPACT(NS)
        INOD =KXSP(3,N)
        UNM=ONE/MAX(EM30,MS(INOD))
        VXI=V(1,INOD)+DT12*A(1,INOD)*UNM
        VYI=V(2,INOD)+DT12*A(2,INOD)*UNM
        VZI=V(3,INOD)+DT12*A(3,INOD)*UNM
        VV=VXI*VXI+VYI*VYI+VZI*VZI
        KV=HALF*MS(INOD)*VV
        EHOURT=EHOURT+KV
        IPRT=IPARTSP(N)
        PARTSAV(8,IPRT)=PARTSAV(8,IPRT)+KV
      ENDDO
C-----------------------------------------------
C      Conservative smoothing of velocities.
C-----------------------------------------------
C
      DO NS =ITASK+1,NSPHACT,NTHREAD
        N     =WASPACT(NS)
        MYADRN=3*(N-1)
        WA(MYADRN+1)=ZERO
        WA(MYADRN+2)=ZERO
        WA(MYADRN+3)=ZERO     
      ENDDO
C
      DO NS =ITASK+1,NSPHACT,NTHREAD
        N=WASPACT(NS)
        MYADRN=3*(N-1)
        IPRT =IPARTSP(N)
        IMAT =IPART(1,IPRT)
        IPROP=IPART(2,IPRT)
        ALPCI=GET_U_GEO(4,IPROP)
C------
        INOD =KXSP(3,N)
        NVOIS=KXSP(4,N)
        XI=X(1,INOD)
        YI=X(2,INOD)
        ZI=X(3,INOD)
        DI=SPBUF(1,N)
        VXI=V(1,INOD)
        VYI=V(2,INOD)
        VZI=V(3,INOD)
        AXI=A(1,INOD)
        AYI=A(2,INOD)
        AZI=A(3,INOD)
        RHOI=SPBUF(2,N)
C------
        ALPHAI=WACOMP(1,N)
        BETAXI=WACOMP(2,N)
        BETAYI=WACOMP(3,N)
        BETAZI=WACOMP(4,N)
C-----
        DO J=1,NVOIS
         JNOD=IXSP(J,N)
         IF(JNOD>0)THEN
          M=NOD2SP(JNOD)
C
C Solids to SPH, no interaction if both particles are inactive
          IF(KXSP(2,N)<0.AND.KXSP(2,M)<0)CYCLE
          XJ=X(1,JNOD)
          YJ=X(2,JNOD)
          ZJ=X(3,JNOD)
          RHOJ=SPBUF(2,M)
          VXJ=V(1,JNOD)
          VYJ=V(2,JNOD)
          VZJ=V(3,JNOD)
          AXJ=A(1,JNOD)
          AYJ=A(2,JNOD)
          AZJ=A(3,JNOD)
          DJ=SPBUF(1,M)
          DIJ=HALF*(DI+DJ)
          CALL WEIGHT0(XI,YI,ZI,XJ,YJ,ZJ,DIJ,WGHT)
          BETAI=ONE+BETAXI*(XI-XJ)+BETAYI*(YI-YJ)+BETAZI*(ZI-ZJ)
          ALPHAJ=WACOMP(1,M)
          BETAXJ=WACOMP(2,M)
          BETAYJ=WACOMP(3,M)
          BETAZJ=WACOMP(4,M)
          BETAJ=ONE+BETAXJ*(XJ-XI)+BETAYJ*(YJ-YI)+BETAZJ*(ZJ-ZI)
          WGHT=WGHT*(ALPHAI*BETAI+ALPHAJ*BETAJ)*HALF
          FACT=TWO*WGHT/(RHOI+RHOJ)
          WAX=AXJ-AXI+MS(INOD)*(VXJ-VXI)*DTINV
          WAY=AYJ-AYI+MS(INOD)*(VYJ-VYI)*DTINV
          WAZ=AZJ-AZI+MS(INOD)*(VZJ-VZI)*DTINV
          FACI= ALPCI*SPBUF(12,M)*FACT
         ELSE                           ! cellule remote
          NN = -JNOD
C
C Solids to SPH, no interaction if both particles are inactive
          IF(KXSP(2,N)<=0.AND.XSPHR(13,NN)<=0)CYCLE
          XJ=XSPHR(3,NN)
          YJ=XSPHR(4,NN)
          ZJ=XSPHR(5,NN)
          RHOJ=XSPHR(7,NN)
          VXJ=XSPHR(9,NN)
          VYJ=XSPHR(10,NN)
          VZJ=XSPHR(11,NN)
          AXJ=ASPHR(1,NN)
          AYJ=ASPHR(2,NN)
          AZJ=ASPHR(3,NN)
          DJ=XSPHR(2,NN)
          DIJ=HALF*(DI+DJ)
          CALL WEIGHT0(XI,YI,ZI,XJ,YJ,ZJ,DIJ,WGHT)
          BETAI=ONE+BETAXI*(XI-XJ)+BETAYI*(YI-YJ)+BETAZI*(ZI-ZJ)
          ALPHAJ=WACOMPR(1,NN)
          BETAXJ=WACOMPR(2,NN)
          BETAYJ=WACOMPR(3,NN)
          BETAZJ=WACOMPR(4,NN)
          BETAJ=ONE+BETAXJ*(XJ-XI)+BETAYJ*(YJ-YI)+BETAZJ*(ZJ-ZI)
          WGHT=WGHT*(ALPHAI*BETAI+ALPHAJ*BETAJ)*HALF
          FACT=TWO*WGHT/(RHOI+RHOJ)
          WAX=AXJ-AXI+MS(INOD)*(VXJ-VXI)*DTINV
          WAY=AYJ-AYI+MS(INOD)*(VYJ-VYI)*DTINV
          WAZ=AZJ-AZI+MS(INOD)*(VZJ-VZI)*DTINV
          FACI= ALPCI*XSPHR(8,NN)*FACT
         END IF
         WA(MYADRN+1)=WA(MYADRN+1)+FACI*WAX
         WA(MYADRN+2)=WA(MYADRN+2)+FACI*WAY
         WA(MYADRN+3)=WA(MYADRN+3)+FACI*WAZ
        END DO
C-----
C       partie symetrique.
        NVOISS=KXSP(6,N)
        DO J=KXSP(5,N)+1,KXSP(5,N)+NVOISS
         JS=IXSP(J,N)
         IF(JS>0)THEN
          SM=JS/(NSPCOND+1)
C
C Solids to SPH, no interaction if both particles are inactive
          IF(KXSP(2,N)<=0.AND.KXSP(2,SM)<=0)CYCLE
          NC=MOD(JS,NSPCOND+1)
          JS=ISPSYM(NC,SM)
          XJ=XSPSYM(1,JS)
          YJ=XSPSYM(2,JS)
          ZJ=XSPSYM(3,JS)
          VXJ=VSPSYM(1,JS)
          VYJ=VSPSYM(2,JS)
          VZJ=VSPSYM(3,JS)
          AXJ=WASPSYM(1,JS)
          AYJ=WASPSYM(2,JS)
          AZJ=WASPSYM(3,JS)
          RHOJ=SPBUF(2,SM)
          DJ  =SPBUF(1,SM)
          DIJ =HALF*(DI+DJ)
          CALL WEIGHT0(XI,YI,ZI,XJ,YJ,ZJ,DIJ,WGHT)
          JNOD=KXSP(3,SM)
          BETAI=ONE +BETAXI*(XI-XJ)+BETAYI*(YI-YJ)+BETAZI*(ZI-ZJ)
          ALPHAJ=WACOMP(1,SM)
C         BETAXJ=WACOMP(2,SM)
C         BETAYJ=WACOMP(3,SM)
C         BETAZJ=WACOMP(4,SM)
          BETAXJ=WSMCOMP(1,JS)
          BETAYJ=WSMCOMP(2,JS)
          BETAZJ=WSMCOMP(3,JS)
          BETAJ=ONE+BETAXJ*(XJ-XI)+BETAYJ*(YJ-YI)+BETAZJ*(ZJ-ZI)
          WGHT=WGHT*(ALPHAI*BETAI+ALPHAJ*BETAJ)*HALF
          FACT=ALPCI*TWO*SPBUF(12,SM)*WGHT/(RHOI+RHOJ)
          WAX=AXJ-AXI+MS(INOD)*(VXJ-VXI)*DTINV
          WAY=AYJ-AYI+MS(INOD)*(VYJ-VYI)*DTINV
          WAZ=AZJ-AZI+MS(INOD)*(VZJ-VZI)*DTINV
         ELSE                     ! particule symetrique de particule remote
          SM=-JS/(NSPCOND+1)
C
C Solids to SPH, no interaction if both particles are inactive
          IF(KXSP(2,N)<=0.AND.XSPHR(13,SM)<=0)CYCLE
          NC=MOD(-JS,NSPCOND+1)
          JS=ISPSYMR(NC,SM)
          XJ=XSPSYM(1,JS)
          YJ=XSPSYM(2,JS)
          ZJ=XSPSYM(3,JS)
          VXJ=VSPSYM(1,JS)
          VYJ=VSPSYM(2,JS)
          VZJ=VSPSYM(3,JS)
          AXJ=WASPSYM(1,JS)
          AYJ=WASPSYM(2,JS)
          AZJ=WASPSYM(3,JS)
          RHOJ=XSPHR(7,SM)
          DJ  =XSPHR(2,SM)
          DIJ =HALF*(DI+DJ)
          CALL WEIGHT0(XI,YI,ZI,XJ,YJ,ZJ,DIJ,WGHT)
          BETAI=ONE +BETAXI*(XI-XJ)+BETAYI*(YI-YJ)+BETAZI*(ZI-ZJ)
          ALPHAJ=WACOMPR(1,SM)
C         BETAXJ=WACOMPR(2,SM)
C         BETAYJ=WACOMPR(3,SM)
C         BETAZJ=WACOMPR(4,SM)
          BETAXJ=WSMCOMP(1,JS)
          BETAYJ=WSMCOMP(2,JS)
          BETAZJ=WSMCOMP(3,JS)
          BETAJ=ONE+BETAXJ*(XJ-XI)+BETAYJ*(YJ-YI)+BETAZJ*(ZJ-ZI)
          WGHT=WGHT*(ALPHAI*BETAI+ALPHAJ*BETAJ)*HALF
          FACT=ALPCI*TWO*XSPHR(8,SM)*WGHT/(RHOI+RHOJ)
          WAX=AXJ-AXI+MS(INOD)*(VXJ-VXI)*DTINV
          WAY=AYJ-AYI+MS(INOD)*(VYJ-VYI)*DTINV
          WAZ=AZJ-AZI+MS(INOD)*(VZJ-VZI)*DTINV
         END IF
         WA(MYADRN+1)=WA(MYADRN+1)+FACT*WAX
         WA(MYADRN+2)=WA(MYADRN+2)+FACT*WAY
         WA(MYADRN+3)=WA(MYADRN+3)+FACT*WAZ
        END DO
      END DO
C-----

C barrier sur A & ASPHR
C    /---------------/
      CALL MY_BARRIER
C    /---------------/

C     Assemblage des forces dans A
      IF(NSPHSOL==0)THEN
        DO NS=ITASK+1,NSPHACT,NTHREAD
          N=WASPACT(NS)
          MYADRN=3*(N-1)
          INOD=KXSP(3,N)
          A(1,INOD)=A(1,INOD)+WA(MYADRN+1)
          A(2,INOD)=A(2,INOD)+WA(MYADRN+2)
          A(3,INOD)=A(3,INOD)+WA(MYADRN+3)
        END DO
      ELSEIF(ITASK==0)THEN
C
C a paralleliser smp
        II=0
        DO NS=1,NSPHACT
          N=WASPACT(NS)
          MYADRN=3*(N-1)
          IF(SPH2SOL(N)==0)THEN
            INOD=KXSP(3,N)
            A(1,INOD)=A(1,INOD)+WA(MYADRN+1)
            A(2,INOD)=A(2,INOD)+WA(MYADRN+2)
            A(3,INOD)=A(3,INOD)+WA(MYADRN+3)
C
          ELSEIF (SOL2SPH_TYP(SPH2SOL(N))==4) THEN
C---------------
C------ TETRA -- 
C---------------
C           for computing Ehour only
            INOD=KXSP(3,N)
            A(1,INOD)=A(1,INOD)+WA(MYADRN+1)
            A(2,INOD)=A(2,INOD)+WA(MYADRN+2)
            A(3,INOD)=A(3,INOD)+WA(MYADRN+3)
C----------
            NSOL=SPH2SOL(N)
C
            N1=IXS(2,NSOL)
            N2=IXS(4,NSOL)
            N3=IXS(7,NSOL)
            N4=IXS(6,NSOL)
C
            IR=IRST(1,N-FIRST_SPHSOL+1)
            IS=IRST(2,N-FIRST_SPHSOL+1)
            IT=IRST(3,N-FIRST_SPHSOL+1)
            NSPHDIR=IGEO(37,IXS(10,NSOL))
C
            KSI  = A_GAUSS(IR,NSPHDIR)
            ETA  = A_GAUSS(IS,NSPHDIR)
            ZETA = A_GAUSS(IT,NSPHDIR)
C
            PHI1=ONE_OVER_8*(ONE-KSI)*(ONE-ETA)*(ONE-ZETA)
            PHI2=ONE_OVER_8*(ONE-KSI)*(ONE-ETA)*(ONE+ZETA)
            PHI3=ONE_OVER_8*(ONE+KSI)*(ONE-ETA)*(ONE+ZETA)
            PHI4=ONE_OVER_8*(ONE+KSI)*(ONE-ETA)*(ONE-ZETA)
C
            II=II+1
            AS(1,II)=PHI1*WA(MYADRN+1)
            AS(2,II)=PHI1*WA(MYADRN+2)
            AS(3,II)=PHI1*WA(MYADRN+3)

            II=II+1
            AS(1,II)=PHI2*WA(MYADRN+1)
            AS(2,II)=PHI2*WA(MYADRN+2)
            AS(3,II)=PHI2*WA(MYADRN+3)

            II=II+1
            AS(1,II)=PHI3*WA(MYADRN+1)
            AS(2,II)=PHI3*WA(MYADRN+2)
            AS(3,II)=PHI3*WA(MYADRN+3)

            II=II+1
            AS(1,II)=PHI4*WA(MYADRN+1)
            AS(2,II)=PHI4*WA(MYADRN+2)
            AS(3,II)=PHI4*WA(MYADRN+3)
C
          ELSE
C---------------
C------ HEXA- -- 
C---------------
C           for computing Ehour only
            INOD=KXSP(3,N)
            A(1,INOD)=A(1,INOD)+WA(MYADRN+1)
            A(2,INOD)=A(2,INOD)+WA(MYADRN+2)
            A(3,INOD)=A(3,INOD)+WA(MYADRN+3)
C----------
            NSOL=SPH2SOL(N)
C
            N1=IXS(2,NSOL)
            N2=IXS(3,NSOL)
            N3=IXS(4,NSOL)
            N4=IXS(5,NSOL)
            N5=IXS(6,NSOL)
            N6=IXS(7,NSOL)
            N7=IXS(8,NSOL)
            N8=IXS(9,NSOL)
C
            IR=IRST(1,N-FIRST_SPHSOL+1)
            IS=IRST(2,N-FIRST_SPHSOL+1)
            IT=IRST(3,N-FIRST_SPHSOL+1)
            NSPHDIR=NINT((SOL2SPH(2,NSOL)-SOL2SPH(1,NSOL))**THIRD)
C
            KSI  = A_GAUSS(IR,NSPHDIR)
            ETA  = A_GAUSS(IS,NSPHDIR)
            ZETA = A_GAUSS(IT,NSPHDIR)
C
            PHI1=ONE_OVER_8*(ONE-KSI)*(ONE-ETA)*(ONE-ZETA)
            PHI2=ONE_OVER_8*(ONE-KSI)*(ONE-ETA)*(ONE+ZETA)
            PHI3=ONE_OVER_8*(ONE+KSI)*(ONE-ETA)*(ONE+ZETA)
            PHI4=ONE_OVER_8*(ONE+KSI)*(ONE-ETA)*(ONE-ZETA)
            PHI5=ONE_OVER_8*(ONE-KSI)*(ONE+ETA)*(ONE-ZETA)
            PHI6=ONE_OVER_8*(ONE-KSI)*(ONE+ETA)*(ONE+ZETA)
            PHI7=ONE_OVER_8*(ONE+KSI)*(ONE+ETA)*(ONE+ZETA)
            PHI8=ONE_OVER_8*(ONE+KSI)*(ONE+ETA)*(ONE-ZETA)
C
            II=II+1
            AS(1,II)=PHI1*WA(MYADRN+1)
            AS(2,II)=PHI1*WA(MYADRN+2)
            AS(3,II)=PHI1*WA(MYADRN+3)

            II=II+1
            AS(1,II)=PHI2*WA(MYADRN+1)
            AS(2,II)=PHI2*WA(MYADRN+2)
            AS(3,II)=PHI2*WA(MYADRN+3)

            II=II+1
            AS(1,II)=PHI3*WA(MYADRN+1)
            AS(2,II)=PHI3*WA(MYADRN+2)
            AS(3,II)=PHI3*WA(MYADRN+3)

            II=II+1
            AS(1,II)=PHI4*WA(MYADRN+1)
            AS(2,II)=PHI4*WA(MYADRN+2)
            AS(3,II)=PHI4*WA(MYADRN+3)

            II=II+1
            AS(1,II)=PHI5*WA(MYADRN+1)
            AS(2,II)=PHI5*WA(MYADRN+2)
            AS(3,II)=PHI5*WA(MYADRN+3)

            II=II+1
            AS(1,II)=PHI6*WA(MYADRN+1)
            AS(2,II)=PHI6*WA(MYADRN+2)
            AS(3,II)=PHI6*WA(MYADRN+3)

            II=II+1
            AS(1,II)=PHI7*WA(MYADRN+1)
            AS(2,II)=PHI7*WA(MYADRN+2)
            AS(3,II)=PHI7*WA(MYADRN+3)

            II=II+1
            AS(1,II)=PHI8*WA(MYADRN+1)
            AS(2,II)=PHI8*WA(MYADRN+2)
            AS(3,II)=PHI8*WA(MYADRN+3)
C
          END IF
        ENDDO
C------
        CALL FOAT_TO_6_FLOAT(1,3*II,AS,AS6)
C------
        II=0
        DO NS=1,NSPHACT
          N=WASPACT(NS)
          IF(SPH2SOL(N)/=0)THEN
           IF (SOL2SPH_TYP(SPH2SOL(N))==4) THEN
C---------------
C------ TETRA -- 
C---------------
C
            NSOL=SPH2SOL(N)
C
            N1=IXS(2,NSOL)
            N2=IXS(3,NSOL)
            N3=IXS(4,NSOL)
            N4=IXS(5,NSOL)
C
            ITAG(N1)=1
            ITAG(N2)=1
            ITAG(N3)=1
            ITAG(N4)=1
C
            II1=II
            DO J=1,6
              II=II1+1
              A6(J,1,N1)=A6(J,1,N1)+AS6(J,1,II)
              A6(J,2,N1)=A6(J,2,N1)+AS6(J,2,II)
              A6(J,3,N1)=A6(J,3,N1)+AS6(J,3,II)
C
              II=II+1
              A6(J,1,N2)=A6(J,1,N2)+AS6(J,1,II)
              A6(J,2,N2)=A6(J,2,N2)+AS6(J,2,II)
              A6(J,3,N2)=A6(J,3,N2)+AS6(J,3,II)
C
              II=II+1
              A6(J,1,N3)=A6(J,1,N3)+AS6(J,1,II)
              A6(J,2,N3)=A6(J,2,N3)+AS6(J,2,II)
              A6(J,3,N3)=A6(J,3,N3)+AS6(J,3,II)
C
              II=II+1
              A6(J,1,N4)=A6(J,1,N4)+AS6(J,1,II)
              A6(J,2,N4)=A6(J,2,N4)+AS6(J,2,II)
              A6(J,3,N4)=A6(J,3,N4)+AS6(J,3,II)
C
            END DO
C
           ELSE
C---------------
C------ HEXA --- 
C---------------
C
            NSOL=SPH2SOL(N)
C
            N1=IXS(2,NSOL)
            N2=IXS(3,NSOL)
            N3=IXS(4,NSOL)
            N4=IXS(5,NSOL)
            N5=IXS(6,NSOL)
            N6=IXS(7,NSOL)
            N7=IXS(8,NSOL)
            N8=IXS(9,NSOL)
C
            ITAG(N1)=1
            ITAG(N2)=1
            ITAG(N3)=1
            ITAG(N4)=1
            ITAG(N5)=1
            ITAG(N6)=1
            ITAG(N7)=1
            ITAG(N8)=1
C
            II1=II
            DO J=1,6
              II=II1+1
              A6(J,1,N1)=A6(J,1,N1)+AS6(J,1,II)
              A6(J,2,N1)=A6(J,2,N1)+AS6(J,2,II)
              A6(J,3,N1)=A6(J,3,N1)+AS6(J,3,II)
C
              II=II+1
              A6(J,1,N2)=A6(J,1,N2)+AS6(J,1,II)
              A6(J,2,N2)=A6(J,2,N2)+AS6(J,2,II)
              A6(J,3,N2)=A6(J,3,N2)+AS6(J,3,II)
C
              II=II+1
              A6(J,1,N3)=A6(J,1,N3)+AS6(J,1,II)
              A6(J,2,N3)=A6(J,2,N3)+AS6(J,2,II)
              A6(J,3,N3)=A6(J,3,N3)+AS6(J,3,II)
C
              II=II+1
              A6(J,1,N4)=A6(J,1,N4)+AS6(J,1,II)
              A6(J,2,N4)=A6(J,2,N4)+AS6(J,2,II)
              A6(J,3,N4)=A6(J,3,N4)+AS6(J,3,II)
C
              II=II+1
              A6(J,1,N5)=A6(J,1,N5)+AS6(J,1,II)
              A6(J,2,N5)=A6(J,2,N5)+AS6(J,2,II)
              A6(J,3,N5)=A6(J,3,N5)+AS6(J,3,II)
C
              II=II+1
              A6(J,1,N6)=A6(J,1,N6)+AS6(J,1,II)
              A6(J,2,N6)=A6(J,2,N6)+AS6(J,2,II)
              A6(J,3,N6)=A6(J,3,N6)+AS6(J,3,II)
C
              II=II+1
              A6(J,1,N7)=A6(J,1,N7)+AS6(J,1,II)
              A6(J,2,N7)=A6(J,2,N7)+AS6(J,2,II)
              A6(J,3,N7)=A6(J,3,N7)+AS6(J,3,II)
C
              II=II+1
              A6(J,1,N8)=A6(J,1,N8)+AS6(J,1,II)
              A6(J,2,N8)=A6(J,2,N8)+AS6(J,2,II)
              A6(J,3,N8)=A6(J,3,N8)+AS6(J,3,II)
C
            END DO
C
           ENDIF
C
          END IF
        END DO
      END IF
C
      ENDIF !(IF (NUMSPH > 0)
C-----
      IF ((SOL2SPH_FLAG > 0).AND.(ITASK==0)) THEN
        IF(NSPMD > 1)THEN
          SIZE=19
          LENR = IAD_ELEM(1,NSPMD+1)-IAD_ELEM(1,1)
          CALL SPMD_EXCH_A_SOL2SPH(
     1   A6       ,ITAG   ,IAD_ELEM ,FR_ELEM,SIZE,
     2   LENR     )
        END IF
C-----
        DO N=1,NUMNOD
          IF(ITAG(N)/=0)THEN
            A(1,N)=A(1,N)+A6(1,1,N)+A6(2,1,N)+A6(3,1,N)
     .                   +A6(4,1,N)+A6(5,1,N)+A6(6,1,N)
            A(2,N)=A(2,N)+A6(1,2,N)+A6(2,2,N)+A6(3,2,N)
     .                   +A6(4,2,N)+A6(5,2,N)+A6(6,2,N)
            A(3,N)=A(3,N)+A6(1,3,N)+A6(2,3,N)+A6(3,3,N)
     .                   +A6(4,3,N)+A6(5,3,N)+A6(6,3,N)
          END IF
        END DO
      ENDIF

C-------------------------------------------
C    /---------------/
      CALL MY_BARRIER
C    /---------------/
C
      IF (NUMSPH > 0) THEN
        DO NS =ITASK+1,NSPHACT,NTHREAD
          N    =WASPACT(NS)
          INOD =KXSP(3,N)
          UNM=ONE/MAX(EM30,MS(INOD))
          VXI=V(1,INOD)+DT12*A(1,INOD)*UNM
          VYI=V(2,INOD)+DT12*A(2,INOD)*UNM
          VZI=V(3,INOD)+DT12*A(3,INOD)*UNM
          VV=VXI*VXI+VYI*VYI+VZI*VZI
          KV=HALF*MS(INOD)*VV
          EHOURT=EHOURT-KV
          IPRT=IPARTSP(N)
          PARTSAV(8,IPRT)=PARTSAV(8,IPRT)-KV
        ENDDO
#include "atomic.inc"
        EHOUR=EHOUR+EHOURT
#include "atomend.inc"
      ENDIF
C
      IF(NSPMD>1 .AND. ITASK==0.AND.ALLOCATED(ASPHR)) DEALLOCATE(ASPHR)
      IF(ITASK==0.AND.SOL2SPH_FLAG/=0) DEALLOCATE(AS,AS6,A6,ITAG)
C-------------------------------------------
      RETURN
      END
