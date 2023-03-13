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
Chd|  SOLTOSPHF                     source/elements/sph/soltosph.F
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
      SUBROUTINE SOLTOSPHF(
     1   A      ,SPBUF   ,IXS     ,KXSP    ,IPARTSP ,
     2   NOD2SP ,IRST   ,NGROUNC  ,IGROUNC ,IPARG   ,
     3   STIFN  ,SOL2SPH,SPH2SOL  ,ELBUF_TAB,ITASK  ,
     4   NODFT  ,NODLT  ,ISKY     ,FSKYI    ,IGEO   ,
     5   SOL2SPH_TYP)
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
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "parit_c.inc"
#include      "sphcom.inc"
#include      "task_c.inc"
#include      "vect01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IXS(NIXS,*), KXSP(NISP,*), IPARTSP(*), NOD2SP(*),
     .        IRST(3,*), NGROUNC, IGROUNC(*), IPARG(NPARG,*),
     .        SOL2SPH(2,*), SPH2SOL(*), ITASK, NODFT, NODLT, ISKY(*),
     .        IGEO(NPROPGI,*),SOL2SPH_TYP(*)
      my_real
     .        A(3,*), SPBUF(NSPBUF,*), STIFN(*), FSKYI(LSKYI,NFSKYI)
      TYPE (ELBUF_STRUCT_), TARGET, DIMENSION(NGROUP) :: ELBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,  J, IR, IS, IT, KP, NP, N, INOD, MG, NG, NEL,
     .        KFT, N1, N2, N3, N4, N5, N6, N7, N8,
     .        LENR, IG , NELEM, OFFSET, NSPHDIR, IPRTSPH, IAW,
     .        MY_IAW, IERROR, NISK, NIFTSK, NILTSK, NISKYL, NSOL
C                                                                    
      my_real
     .   PHI1,PHI2,PHI3,PHI4,PHI5,PHI6,PHI7,PHI8,
     .   KSI, ETA, ZETA
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
C     MAPPING OF CONTACT FORCES APPLYING TO SLEEPING PARTICLES
C-----------------------------------------------
      IF(IPARIT==0)THEN
       IF(ITASK==0)THEN
        ALLOCATE(AWORK(4,NTHREAD*NUMNOD),STAT=IERROR)
        IF(IERROR/=0) THEN
          CALL ANCMSG(MSGID=19,ANMODE=ANINFO,
     .                C1='(SOLIDS to SPH)')
          CALL ARRET(2)
        ENDIF
       END IF
C
       CALL MY_BARRIER
C
       DO IAW=ITASK*NUMNOD+1,(ITASK+1)*NUMNOD
         AWORK(1,IAW)=ZERO
         AWORK(2,IAW)=ZERO
         AWORK(3,IAW)=ZERO
         AWORK(4,IAW)=ZERO
       END DO
C
       CALL MY_BARRIER
C
       MY_IAW=ITASK*NUMNOD
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
          IPRTSPH=IPARG(69,NG)
          LFT=1
          LLT=MIN(NVSIZ,NEL-NELEM+1)
          IF(ITY==1.AND.IPRTSPH/=0) THEN
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
              IF(GBUF%OFF(I)/=ZERO) THEN
C
                N1=IXS(2,N)
                N2=IXS(3,N)
                N3=IXS(4,N)
                N4=IXS(5,N)
C
                NSPHDIR=IGEO(37,IXS(10,N))
C
C-----------------------------------------------
C               SOL2SPH(1,N)+1<=I<=SOLSPH(2,N) <=> N==SPH2SOL(I)
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
                  INOD=KXSP(3,NP)
                  AWORK(1,MY_IAW+N1)=AWORK(1,MY_IAW+N1)+PHI1*A(1,INOD)
                  AWORK(2,MY_IAW+N1)=AWORK(2,MY_IAW+N1)+PHI1*A(2,INOD)
                  AWORK(3,MY_IAW+N1)=AWORK(3,MY_IAW+N1)+PHI1*A(3,INOD)
                  AWORK(4,MY_IAW+N1)=AWORK(4,MY_IAW+N1)+PHI1*STIFN(INOD)
                  AWORK(1,MY_IAW+N2)=AWORK(1,MY_IAW+N2)+PHI2*A(1,INOD)
                  AWORK(2,MY_IAW+N2)=AWORK(2,MY_IAW+N2)+PHI2*A(2,INOD)
                  AWORK(3,MY_IAW+N2)=AWORK(3,MY_IAW+N2)+PHI2*A(3,INOD)
                  AWORK(4,MY_IAW+N2)=AWORK(4,MY_IAW+N2)+PHI2*STIFN(INOD)
                  AWORK(1,MY_IAW+N3)=AWORK(1,MY_IAW+N3)+PHI3*A(1,INOD)
                  AWORK(2,MY_IAW+N3)=AWORK(2,MY_IAW+N3)+PHI3*A(2,INOD)
                  AWORK(3,MY_IAW+N3)=AWORK(3,MY_IAW+N3)+PHI3*A(3,INOD)
                  AWORK(4,MY_IAW+N3)=AWORK(4,MY_IAW+N3)+PHI3*STIFN(INOD)
                  AWORK(1,MY_IAW+N4)=AWORK(1,MY_IAW+N4)+PHI4*A(1,INOD)
                  AWORK(2,MY_IAW+N4)=AWORK(2,MY_IAW+N4)+PHI4*A(2,INOD)
                  AWORK(3,MY_IAW+N4)=AWORK(3,MY_IAW+N4)+PHI4*A(3,INOD)
                  AWORK(4,MY_IAW+N4)=AWORK(4,MY_IAW+N4)+PHI4*STIFN(INOD)
C
                  A(1,INOD)=ZERO
                  A(2,INOD)=ZERO
                  A(3,INOD)=ZERO
                  STIFN(INOD)=EM20
                ENDDO
              END IF
            ENDDO
C-----
            ELSE
C-----
C----HEXA---     
C-----
            DO I=LFT,LLT
              N=NFT+I
              IF(GBUF%OFF(I)/=ZERO) THEN
C
                N1=IXS(2,N)
                N2=IXS(3,N)
                N3=IXS(4,N)
                N4=IXS(5,N)
                N5=IXS(6,N)
                N6=IXS(7,N)
                N7=IXS(8,N)
                N8=IXS(9,N)
C
                NSPHDIR=NINT((SOL2SPH(2,N)-SOL2SPH(1,N))**THIRD)
C
C-----------------------------------------------
C               SOL2SPH(1,N)+1<=I<=SOLSPH(2,N) <=> N==SPH2SOL(I)
                DO KP=1,SOL2SPH(2,N)-SOL2SPH(1,N)
                  NP  =SOL2SPH(1,N)+KP
                  IR=IRST(1,NP-FIRST_SPHSOL+1)
                  IS=IRST(2,NP-FIRST_SPHSOL+1)
                  IT=IRST(3,NP-FIRST_SPHSOL+1)
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
                  INOD=KXSP(3,NP)
                  AWORK(1,MY_IAW+N1)=AWORK(1,MY_IAW+N1)+PHI1*A(1,INOD)
                  AWORK(2,MY_IAW+N1)=AWORK(2,MY_IAW+N1)+PHI1*A(2,INOD)
                  AWORK(3,MY_IAW+N1)=AWORK(3,MY_IAW+N1)+PHI1*A(3,INOD)
                  AWORK(4,MY_IAW+N1)=AWORK(4,MY_IAW+N1)+PHI1*STIFN(INOD)
                  AWORK(1,MY_IAW+N2)=AWORK(1,MY_IAW+N2)+PHI2*A(1,INOD)
                  AWORK(2,MY_IAW+N2)=AWORK(2,MY_IAW+N2)+PHI2*A(2,INOD)
                  AWORK(3,MY_IAW+N2)=AWORK(3,MY_IAW+N2)+PHI2*A(3,INOD)
                  AWORK(4,MY_IAW+N2)=AWORK(4,MY_IAW+N2)+PHI2*STIFN(INOD)
                  AWORK(1,MY_IAW+N3)=AWORK(1,MY_IAW+N3)+PHI3*A(1,INOD)
                  AWORK(2,MY_IAW+N3)=AWORK(2,MY_IAW+N3)+PHI3*A(2,INOD)
                  AWORK(3,MY_IAW+N3)=AWORK(3,MY_IAW+N3)+PHI3*A(3,INOD)
                  AWORK(4,MY_IAW+N3)=AWORK(4,MY_IAW+N3)+PHI3*STIFN(INOD)
                  AWORK(1,MY_IAW+N4)=AWORK(1,MY_IAW+N4)+PHI4*A(1,INOD)
                  AWORK(2,MY_IAW+N4)=AWORK(2,MY_IAW+N4)+PHI4*A(2,INOD)
                  AWORK(3,MY_IAW+N4)=AWORK(3,MY_IAW+N4)+PHI4*A(3,INOD)
                  AWORK(4,MY_IAW+N4)=AWORK(4,MY_IAW+N4)+PHI4*STIFN(INOD)
                  AWORK(1,MY_IAW+N5)=AWORK(1,MY_IAW+N5)+PHI5*A(1,INOD)
                  AWORK(2,MY_IAW+N5)=AWORK(2,MY_IAW+N5)+PHI5*A(2,INOD)
                  AWORK(3,MY_IAW+N5)=AWORK(3,MY_IAW+N5)+PHI5*A(3,INOD)
                  AWORK(4,MY_IAW+N5)=AWORK(4,MY_IAW+N5)+PHI5*STIFN(INOD)
                  AWORK(1,MY_IAW+N6)=AWORK(1,MY_IAW+N6)+PHI6*A(1,INOD)
                  AWORK(2,MY_IAW+N6)=AWORK(2,MY_IAW+N6)+PHI6*A(2,INOD)
                  AWORK(3,MY_IAW+N6)=AWORK(3,MY_IAW+N6)+PHI6*A(3,INOD)
                  AWORK(4,MY_IAW+N6)=AWORK(4,MY_IAW+N6)+PHI6*STIFN(INOD)
                  AWORK(1,MY_IAW+N7)=AWORK(1,MY_IAW+N7)+PHI7*A(1,INOD)
                  AWORK(2,MY_IAW+N7)=AWORK(2,MY_IAW+N7)+PHI7*A(2,INOD)
                  AWORK(3,MY_IAW+N7)=AWORK(3,MY_IAW+N7)+PHI7*A(3,INOD)
                  AWORK(4,MY_IAW+N7)=AWORK(4,MY_IAW+N7)+PHI7*STIFN(INOD)
                  AWORK(1,MY_IAW+N8)=AWORK(1,MY_IAW+N8)+PHI8*A(1,INOD)
                  AWORK(2,MY_IAW+N8)=AWORK(2,MY_IAW+N8)+PHI8*A(2,INOD)
                  AWORK(3,MY_IAW+N8)=AWORK(3,MY_IAW+N8)+PHI8*A(3,INOD)
                  AWORK(4,MY_IAW+N8)=AWORK(4,MY_IAW+N8)+PHI8*STIFN(INOD)
C
                  A(1,INOD)=ZERO
                  A(2,INOD)=ZERO
                  A(3,INOD)=ZERO
                  STIFN(INOD)=EM20
                ENDDO
              END IF
            ENDDO
C--------
            ENDIF
          ENDIF
          IF (IDDW>0) CALL STOPTIMEG(NG)
        END DO
C--------
 350    CONTINUE
       END DO
!$OMP END DO
C
       CALL MY_BARRIER
C
       DO IT=0,NTHREAD-1
        DO N=NODFT,NODLT
          IAW=N+IT*NUMNOD
          A(1,N)=A(1,N)+AWORK(1,IAW)
          A(2,N)=A(2,N)+AWORK(2,IAW)
          A(3,N)=A(3,N)+AWORK(3,IAW)
          STIFN(N)=STIFN(N)+AWORK(4,IAW)
        END DO
       END DO
C
       CALL MY_BARRIER
C
       IF(ITASK==0) DEALLOCATE(AWORK)
      ELSE ! IPARIT==0
C-----------------------------------------------
        NIFTSK   = 1+ITASK*NISKY/ NTHREAD
        NILTSK   = (ITASK+1)*NISKY/NTHREAD
C
        CALL MY_BARRIER
C
        DO NISK=NIFTSK,NILTSK
          INOD=ISKY(NISK)
          NP  =NOD2SP(INOD)
          IF(NP/=0)THEN
            N=SPH2SOL(NP)
            IF(N/=0)THEN
#include "lockon.inc"
              NISKYL = NISKY
              NISKY = NISKY + 8
#include "lockoff.inc"
              IF (NISKYL+8 > LSKYI) THEN
                 CALL ANCMSG(MSGID=243,ANMODE=ANINFO_BLIND)
                 CALL ARRET(2)
              ENDIF
C
              IF (SOL2SPH_TYP(SPH2SOL(NP))==4) THEN
C---------------
C------ Tetra -- 
C---------------
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
              KSI  = A_GAUSS_TETRA(IR,NSPHDIR)
              ETA  = A_GAUSS_TETRA(IS,NSPHDIR)
              ZETA = A_GAUSS_TETRA(IT,NSPHDIR)
C
              PHI1=KSI
              PHI2=ETA
              PHI3=ZETA
              PHI4=1-KSI-ETA-ZETA
C
              NISKYL=NISKYL+1
              ISKY(NISKYL)=N1
              DO J=1,NFSKYI
                FSKYI(NISKYL,J)=PHI1*FSKYI(NISK,J)
              END DO
              NISKYL=NISKYL+1
              ISKY(NISKYL)=N2
              DO J=1,NFSKYI
                FSKYI(NISKYL,J)=PHI2*FSKYI(NISK,J)
              END DO
              NISKYL=NISKYL+1
              ISKY(NISKYL)=N3
              DO J=1,NFSKYI
                FSKYI(NISKYL,J)=PHI3*FSKYI(NISK,J)
              END DO
              NISKYL=NISKYL+1
              ISKY(NISKYL)=N4
              DO J=1,NFSKYI
                FSKYI(NISKYL,J)=PHI4*FSKYI(NISK,J)
              END DO
C
            ELSE
C---------------
C------ Hexa -- 
C---------------
              N1=IXS(2,N)
              N2=IXS(3,N)
              N3=IXS(4,N)
              N4=IXS(5,N)
              N5=IXS(6,N)
              N6=IXS(7,N)
              N7=IXS(8,N)
              N8=IXS(9,N)
C
              NSPHDIR=NINT((SOL2SPH(2,N)-SOL2SPH(1,N))**THIRD)
C
C-----------------------------------------------
              IR=IRST(1,NP-FIRST_SPHSOL+1)
              IS=IRST(2,NP-FIRST_SPHSOL+1)
              IT=IRST(3,NP-FIRST_SPHSOL+1)
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
              NISKYL=NISKYL+1
              ISKY(NISKYL)=N1
              DO J=1,NFSKYI
                FSKYI(NISKYL,J)=PHI1*FSKYI(NISK,J)
              END DO
              NISKYL=NISKYL+1
              ISKY(NISKYL)=N2
              DO J=1,NFSKYI
                FSKYI(NISKYL,J)=PHI2*FSKYI(NISK,J)
              END DO
              NISKYL=NISKYL+1
              ISKY(NISKYL)=N3
              DO J=1,NFSKYI
                FSKYI(NISKYL,J)=PHI3*FSKYI(NISK,J)
              END DO
              NISKYL=NISKYL+1
              ISKY(NISKYL)=N4
              DO J=1,NFSKYI
                FSKYI(NISKYL,J)=PHI4*FSKYI(NISK,J)
              END DO
              NISKYL=NISKYL+1
              ISKY(NISKYL)=N5
              DO J=1,NFSKYI
                FSKYI(NISKYL,J)=PHI5*FSKYI(NISK,J)
              END DO
              NISKYL=NISKYL+1
              ISKY(NISKYL)=N6
              DO J=1,NFSKYI
                FSKYI(NISKYL,J)=PHI6*FSKYI(NISK,J)
              END DO
              NISKYL=NISKYL+1
              ISKY(NISKYL)=N7
              DO J=1,NFSKYI
                FSKYI(NISKYL,J)=PHI7*FSKYI(NISK,J)
              END DO
              NISKYL=NISKYL+1
              ISKY(NISKYL)=N8
              DO J=1,NFSKYI
                FSKYI(NISKYL,J)=PHI8*FSKYI(NISK,J)
              END DO
              DO J=1,NFSKYI
                FSKYI(NISK,J)=ZERO
              END DO
C
              ENDIF
            END IF
          END IF
        END DO
      END IF
      RETURN
      END SUBROUTINE SOLTOSPHF
Chd|====================================================================
Chd|  SOLTOSPHP                     source/elements/sph/soltosph.F
Chd|-- called by -----------
Chd|        FORINTP                       source/elements/forintp.F     
Chd|-- calls ---------------
Chd|        INITBUF                       share/resol/initbuf.F         
Chd|        SIG_HEPH1                     source/elements/sph/soltosph_hour.F
Chd|        SREP2GLO                      source/elements/sph/srep2glo.F
Chd|        STARTIMEG                     source/system/timer.F         
Chd|        STOPTIMEG                     source/system/timer.F         
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        INITBUF_MOD                   share/resol/initbuf.F         
Chd|====================================================================
      SUBROUTINE SOLTOSPHP(
     .   X      ,SPBUF   ,IXS     ,KXSP    ,IPARTSP ,
     .   IRST   ,ELBUF_TAB,IPARG  ,NGROUNC ,IGROUNC ,
     .   SOL2SPH,WA       ,PM)
C-----------------------------------------------
      USE INITBUF_MOD
      USE ELBUFDEF_MOD         
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
#include      "com01_c.inc"
#include      "com08_c.inc"
#include      "param_c.inc"
#include      "sphcom.inc"
#include      "task_c.inc"
#include      "vect01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IXS(NIXS,*), KXSP(NISP,*),
     .        IPARTSP(*), IRST(3,*), IPARG(NPARG,*), NGROUNC, 
     .        IGROUNC(*), SOL2SPH(2,*)
      my_real
     .        X(3,*), SPBUF(NSPBUF,*), WA(KWASPH,*),PM(NPROPM,*)
      TYPE (ELBUF_STRUCT_), TARGET, DIMENSION(NGROUP) :: ELBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,  N, IP, KP, NG, MG, J, NP, KFT, IG, NELEM,
     .        NEL, OFFSET, MLW, IPLA,NELSP,K,IR,IS,IT,NSPHDIR,
     .        NPTR,NPTS,NPTT,II(6),JJ(6)
      my_real
     .   RHON, RHOO, DIVV, SM,
     .   R11(MVSIZ),R12(MVSIZ),R13(MVSIZ),
     .   R21(MVSIZ),R22(MVSIZ),R23(MVSIZ),
     .   R31(MVSIZ),R32(MVSIZ),R33(MVSIZ),
     .   T11(MVSIZ),T12(MVSIZ),T13(MVSIZ),
     .   T21(MVSIZ),T22(MVSIZ),T23(MVSIZ),
     .   T31(MVSIZ),T32(MVSIZ),T33(MVSIZ),
     .   RX(MVSIZ),SX(MVSIZ),TX(MVSIZ),
     .   RY(MVSIZ),SY(MVSIZ),TY(MVSIZ),
     .   RZ(MVSIZ),SZ(MVSIZ),TZ(MVSIZ),
     .   G11,G22,G33,G12,G21,G23,G32,G13,G31,
     .   S11,S22,S33,S12,S21,S23,S32,S13,S31,
     .   L11,L22,L33,L12,L21,L23,L32,L13,L31,
     .   SIGLO(MVSIZ,6), STRAGLO(MVSIZ,6), ANGL(MVSIZ,6), 
     .   DGLO24(MVSIZ,6),SIG_HEPH(MVSIZ,6,7),
     .   JR0(MVSIZ),JS0(MVSIZ),JT0(MVSIZ),NU(MVSIZ),SIG_HEPH_GLO(MVSIZ,6,7),
     .   RBID(6,MVSIZ),ZETA,ETA,KSI,SIG_HA8(MVSIZ,3,3,3,6)
C                                                                    
C-----
      TYPE(G_BUFEL_) ,POINTER :: GBUF, GBUFSP
      TYPE(L_BUFEL_) ,POINTER :: LBUF, LBUFSP, LBUF2     
      TYPE(BUF_MAT_) ,POINTER :: MBUF, MBUFSP
C-----------------------------------------------
      my_real A_GAUSS(9,9)
      DATA A_GAUSS /
     1 0.               ,0.               ,0.               ,
     1 0.               ,0.               ,0.               ,
     1 0.               ,0.               ,0.               ,
     2 -.577350269189626,0.577350269189626,0.               ,
     2 0.               ,0.               ,0.               ,
     2 0.               ,0.               ,0.               ,
     3 -.774596669241483,0.               ,0.774596669241483,
     3 0.               ,0.               ,0.               ,
     3 0.               ,0.               ,0.               ,
     4 -.861136311594053,-.339981043584856,0.339981043584856,
     4 0.861136311594053,0.               ,0.               ,
     4 0.               ,0.               ,0.               ,
     5 -.906179845938664,-.538469310105683,0.               ,
     5 0.538469310105683,0.906179845938664,0.               ,
     5 0.               ,0.               ,0.               ,
     6 -.932469514203152,-.661209386466265,-.238619186083197,
     6 0.238619186083197,0.661209386466265,0.932469514203152,
     6 0.               ,0.               ,0.               ,
     7 -.949107912342759,-.741531185599394,-.405845151377397,
     7 0.               ,0.405845151377397,0.741531185599394,
     7 0.949107912342759,0.               ,0.               ,
     8 -.960289856497536,-.796666477413627,-.525532409916329,
     8 -.183434642495650,0.183434642495650,0.525532409916329,
     8 0.796666477413627,0.960289856497536,0.               ,
     9 -.968160239507626,-.836031107326636,-.613371432700590,
     9 -.324253423403809,0.               ,0.324253423403809,
     9 0.613371432700590,0.836031107326636,0.968160239507626/  
C-----------------------------------------------
!$OMP DO SCHEDULE(DYNAMIC,1)
      DO IG = 1, NGROUNC
        NG = IGROUNC(IG)         
        IF(IPARG(8,NG)==1)GOTO 300
        IF (IDDW>0) CALL STARTIMEG(NG)
        OFFSET  = 0
        ITY     = IPARG(5,NG)
        IPARTSPH= IPARG(69,NG)
        IF(ITY==1.AND.IPARTSPH/=0) THEN
C
C---
          CALL INITBUF (IPARG    ,NG      ,                              
     2       MLW     ,NEL     ,NFT     ,IAD     ,ITY     ,               
     3       NPT     ,JALE    ,ISMSTR  ,JEUL    ,JTUR    ,               
     4       JTHE    ,JLAG    ,JMULT   ,JHBE    ,JIVF    ,               
     5       NVAUX   ,JPOR    ,JCVT    ,JCLOSE  ,IPLA    ,               
     6       IREP    ,IINT    ,IGTYP   ,ISRAT   ,ISROT   ,               
     7       ICSEN   ,ISORTH  ,ISORTHG ,IFAILURE,JSMS    )               
          LFT   = 1                  
          LLT   = MIN(NVSIZ,NEL)     
!
          DO I=1,6
            II(I) = NEL*(I-1)
          ENDDO
!
C-----------
          GBUF => ELBUF_TAB(NG)%GBUF
          LBUF => ELBUF_TAB(NG)%BUFLY(1)%LBUF(1,1,1)
          MBUF => ELBUF_TAB(NG)%BUFLY(1)%MAT(1,1,1)
C-----------
          CALL SREP2GLO(X,IXS(1,NFT+1),GBUF%GAMA,
     .                  RX ,RY ,RZ, SX ,SY ,SZ ,TX ,TY ,TZ ,
     .                  R11,R12,R13,R21,R22,R23,R31,R32,R33,
     .                  T11,T12,T13,T21,T22,T23,T31,T32,T33,
     .                  JR0,JS0,JT0,NEL)
C
C----------- HEPH -------
          IF (JHBE==24) THEN
C------------------------
          SIG_HEPH(1:MVSIZ,1:6,1:7) = ZERO
          CALL SIG_HEPH1(JR0     ,JS0  ,JT0  ,GBUF%SIG  ,GBUF%HOURG,
     .                   SIG_HEPH,PM   ,IXS  ,II        ,NEL       )          
C
          IF(ISORTH==0)THEN
           DO J=1,7
           DO I=LFT,LLT
C hourglass stress in corotational system
            L11    =SIG_HEPH(I,1,J)
            L22    =SIG_HEPH(I,2,J)
            L33    =SIG_HEPH(I,3,J)
            L12    =SIG_HEPH(I,4,J)
            L23    =SIG_HEPH(I,5,J)
            L13    =SIG_HEPH(I,6,J)
            S11    =L11*R11(I)+L12*R12(I)+L13*R13(I) 
            S12    =L11*R21(I)+L12*R22(I)+L13*R23(I) 
            S13    =L11*R31(I)+L12*R32(I)+L13*R33(I)
            S21    =L12*R11(I)+L22*R12(I)+L23*R13(I)
            S22    =L12*R21(I)+L22*R22(I)+L23*R23(I)
            S23    =L12*R31(I)+L22*R32(I)+L23*R33(I) 
            S31    =L13*R11(I)+L23*R12(I)+L33*R13(I)
            S32    =L13*R21(I)+L23*R22(I)+L33*R23(I)
            S33    =L13*R31(I)+L23*R32(I)+L33*R33(I)
            SIG_HEPH_GLO(I,1,J)=R11(I)*S11+R12(I)*S21+R13(I)*S31
            SIG_HEPH_GLO(I,2,J)=R21(I)*S12+R22(I)*S22+R23(I)*S32
            SIG_HEPH_GLO(I,3,J)=R31(I)*S13+R32(I)*S23+R33(I)*S33
            SIG_HEPH_GLO(I,4,J)=R11(I)*S12+R12(I)*S22+R13(I)*S32
            SIG_HEPH_GLO(I,5,J)=R21(I)*S13+R22(I)*S23+R23(I)*S33
            SIG_HEPH_GLO(I,6,J)=R11(I)*S13+R12(I)*S23+R13(I)*S33
           END DO
           END DO
          ELSE
           DO J=1,7
           DO I=LFT,LLT
C hourglass stress in orthotropic system
            L11    =SIG_HEPH(I,1,J)
            L22    =SIG_HEPH(I,2,J)
            L33    =SIG_HEPH(I,3,J)
            L12    =SIG_HEPH(I,4,J)
            L23    =SIG_HEPH(I,5,J)
            L13    =SIG_HEPH(I,6,J)
            S11    =L11*T11(I)+L12*T12(I)+L13*T13(I) 
            S12    =L11*T21(I)+L12*T22(I)+L13*T23(I) 
            S13    =L11*T31(I)+L12*T32(I)+L13*T33(I)
            S21    =L12*T11(I)+L22*T12(I)+L23*T13(I)
            S22    =L12*T21(I)+L22*T22(I)+L23*T23(I)
            S23    =L12*T31(I)+L22*T32(I)+L23*T33(I) 
            S31    =L13*T11(I)+L23*T12(I)+L33*T13(I)
            S32    =L13*T21(I)+L23*T22(I)+L33*T23(I)
            S33    =L13*T31(I)+L23*T32(I)+L33*T33(I)
            SIG_HEPH_GLO(I,1,J)=T11(I)*S11+T12(I)*S21+T13(I)*S31
            SIG_HEPH_GLO(I,2,J)=T21(I)*S12+T22(I)*S22+T23(I)*S32
            SIG_HEPH_GLO(I,3,J)=T31(I)*S13+T32(I)*S23+T33(I)*S33
            SIG_HEPH_GLO(I,4,J)=T11(I)*S12+T12(I)*S22+T13(I)*S32
            SIG_HEPH_GLO(I,5,J)=T21(I)*S13+T22(I)*S23+T23(I)*S33
            SIG_HEPH_GLO(I,6,J)=T11(I)*S13+T12(I)*S23+T13(I)*S33
           END DO
           END DO
           ENDIF
C----------- HA8  -------
          ELSEIF (JHBE==14) THEN
C------------------------
          NPTR   = ELBUF_TAB(NG)%NPTR
          NPTS   = ELBUF_TAB(NG)%NPTS
          NPTT   = ELBUF_TAB(NG)%NPTT
          IF(ISORTH==0)THEN
           DO IR=1,NPTR
           DO IS=1,NPTS
           DO IT=1,NPTT
C--------- convention HA8 : ETA-> r / ZETA -> s / KSI -> t
C--------- convention SOL2SPH : KSI-> r / ETA -> s / ZETA -> t
C--------- on permute KSI,ETA,ZETA
           LBUF2 => ELBUF_TAB(NG)%BUFLY(1)%LBUF(IT,IR,IS)
C---------
           DO I=LFT,LLT
C hourglass stress in corotational system
            L11    =LBUF2%SIG(II(1)+I)
            L22    =LBUF2%SIG(II(2)+I)
            L33    =LBUF2%SIG(II(3)+I)
            L12    =LBUF2%SIG(II(4)+I)
            L23    =LBUF2%SIG(II(5)+I)
            L13    =LBUF2%SIG(II(6)+I)
            S11    =L11*R11(I)+L12*R12(I)+L13*R13(I) 
            S12    =L11*R21(I)+L12*R22(I)+L13*R23(I) 
            S13    =L11*R31(I)+L12*R32(I)+L13*R33(I)
            S21    =L12*R11(I)+L22*R12(I)+L23*R13(I)
            S22    =L12*R21(I)+L22*R22(I)+L23*R23(I)
            S23    =L12*R31(I)+L22*R32(I)+L23*R33(I) 
            S31    =L13*R11(I)+L23*R12(I)+L33*R13(I)
            S32    =L13*R21(I)+L23*R22(I)+L33*R23(I)
            S33    =L13*R31(I)+L23*R32(I)+L33*R33(I)
            SIG_HA8(I,IR,IS,IT,1)=R11(I)*S11+R12(I)*S21+R13(I)*S31
            SIG_HA8(I,IR,IS,IT,2)=R21(I)*S12+R22(I)*S22+R23(I)*S32
            SIG_HA8(I,IR,IS,IT,3)=R31(I)*S13+R32(I)*S23+R33(I)*S33
            SIG_HA8(I,IR,IS,IT,4)=R11(I)*S12+R12(I)*S22+R13(I)*S32
            SIG_HA8(I,IR,IS,IT,5)=R21(I)*S13+R22(I)*S23+R23(I)*S33
            SIG_HA8(I,IR,IS,IT,6)=R11(I)*S13+R12(I)*S23+R13(I)*S33
           END DO
           END DO
           END DO
           END DO
          ELSE
           DO IR=1,NPTR
           DO IS=1,NPTS
           DO IT=1,NPTT
           LBUF2 => ELBUF_TAB(NG)%BUFLY(1)%LBUF(IR,IS,IT)
           DO I=LFT,LLT
C hourglass stress in orthotropic system
            L11    =LBUF2%SIG(II(1)+I)
            L22    =LBUF2%SIG(II(2)+I)
            L33    =LBUF2%SIG(II(3)+I)
            L12    =LBUF2%SIG(II(4)+I)
            L23    =LBUF2%SIG(II(5)+I)
            L13    =LBUF2%SIG(II(6)+I)
            S11    =L11*T11(I)+L12*T12(I)+L13*T13(I) 
            S12    =L11*T21(I)+L12*T22(I)+L13*T23(I) 
            S13    =L11*T31(I)+L12*T32(I)+L13*T33(I)
            S21    =L12*T11(I)+L22*T12(I)+L23*T13(I)
            S22    =L12*T21(I)+L22*T22(I)+L23*T23(I)
            S23    =L12*T31(I)+L22*T32(I)+L23*T33(I) 
            S31    =L13*T11(I)+L23*T12(I)+L33*T13(I)
            S32    =L13*T21(I)+L23*T22(I)+L33*T23(I)
            S33    =L13*T31(I)+L23*T32(I)+L33*T33(I)
            SIG_HA8(I,IR,IS,IT,1)=T11(I)*S11+T12(I)*S21+T13(I)*S31
            SIG_HA8(I,IR,IS,IT,2)=T21(I)*S12+T22(I)*S22+T23(I)*S32
            SIG_HA8(I,IR,IS,IT,3)=T31(I)*S13+T32(I)*S23+T33(I)*S33
            SIG_HA8(I,IR,IS,IT,4)=T11(I)*S12+T12(I)*S22+T13(I)*S32
            SIG_HA8(I,IR,IS,IT,5)=T21(I)*S13+T22(I)*S23+T23(I)*S33
            SIG_HA8(I,IR,IS,IT,6)=T11(I)*S13+T12(I)*S23+T13(I)*S33
           END DO
           END DO
           END DO
           END DO
           ENDIF
C----------- Standard formulation  -------
          ELSEIF (JCVT == 0)THEN
C------------------------------------------
           DO I=LFT,LLT
C mean stress in global system
            SIGLO(I,1)  =GBUF%SIG(II(1)+I)
            SIGLO(I,2)  =GBUF%SIG(II(2)+I)
            SIGLO(I,3)  =GBUF%SIG(II(3)+I)
            SIGLO(I,4)  =GBUF%SIG(II(4)+I)
            SIGLO(I,5)  =GBUF%SIG(II(5)+I)
            SIGLO(I,6)  =GBUF%SIG(II(6)+I)
           END DO
C----------- Corotational formulation  -------
          ELSE
C--------------------------------------------
C
          IF (ISORTH== 0) THEN
           DO I=LFT,LLT
C mean stress in corotational system
            L11    =GBUF%SIG(II(1)+I)
            L22    =GBUF%SIG(II(2)+I)
            L33    =GBUF%SIG(II(3)+I)
            L12    =GBUF%SIG(II(4)+I)
            L23    =GBUF%SIG(II(5)+I)
            L13    =GBUF%SIG(II(6)+I)
            S11    =L11*R11(I)+L12*R12(I)+L13*R13(I) 
            S12    =L11*R21(I)+L12*R22(I)+L13*R23(I) 
            S13    =L11*R31(I)+L12*R32(I)+L13*R33(I)
            S21    =L12*R11(I)+L22*R12(I)+L23*R13(I)
            S22    =L12*R21(I)+L22*R22(I)+L23*R23(I)
            S23    =L12*R31(I)+L22*R32(I)+L23*R33(I) 
            S31    =L13*R11(I)+L23*R12(I)+L33*R13(I)
            S32    =L13*R21(I)+L23*R22(I)+L33*R23(I)
            S33    =L13*R31(I)+L23*R32(I)+L33*R33(I)
            SIGLO(I,1)=R11(I)*S11+R12(I)*S21+R13(I)*S31
            SIGLO(I,2)=R21(I)*S12+R22(I)*S22+R23(I)*S32
            SIGLO(I,3)=R31(I)*S13+R32(I)*S23+R33(I)*S33
            SIGLO(I,4)=R11(I)*S12+R12(I)*S22+R13(I)*S32
            SIGLO(I,5)=R21(I)*S13+R22(I)*S23+R23(I)*S33
            SIGLO(I,6)=R11(I)*S13+R12(I)*S23+R13(I)*S33
           END DO
          ELSE
           DO I=LFT,LLT
C mean stress in orthotropic system
            L11    =GBUF%SIG(II(1)+I)
            L22    =GBUF%SIG(II(2)+I)
            L33    =GBUF%SIG(II(3)+I)
            L12    =GBUF%SIG(II(4)+I)
            L23    =GBUF%SIG(II(5)+I)
            L13    =GBUF%SIG(II(6)+I)
            S11    =L11*T11(I)+L12*T12(I)+L13*T13(I) 
            S12    =L11*T21(I)+L12*T22(I)+L13*T23(I) 
            S13    =L11*T31(I)+L12*T32(I)+L13*T33(I)
            S21    =L12*T11(I)+L22*T12(I)+L23*T13(I)
            S22    =L12*T21(I)+L22*T22(I)+L23*T23(I)
            S23    =L12*T31(I)+L22*T32(I)+L23*T33(I) 
            S31    =L13*T11(I)+L23*T12(I)+L33*T13(I)
            S32    =L13*T21(I)+L23*T22(I)+L33*T23(I)
            S33    =L13*T31(I)+L23*T32(I)+L33*T33(I)
            SIGLO(I,1)=T11(I)*S11+T12(I)*S21+T13(I)*S31
            SIGLO(I,2)=T21(I)*S12+T22(I)*S22+T23(I)*S32
            SIGLO(I,3)=T31(I)*S13+T32(I)*S23+T33(I)*S33
            SIGLO(I,4)=T11(I)*S12+T12(I)*S22+T13(I)*S32
            SIGLO(I,5)=T21(I)*S13+T22(I)*S23+T23(I)*S33
            SIGLO(I,6)=T11(I)*S13+T12(I)*S23+T13(I)*S33
           END DO
          END IF
C------------------------
          ENDIF
C-----------------------
          IF(ELBUF_TAB(NG)%BUFLY(1)%L_STRA > 0)THEN
            IF(JCVT == 0)THEN
             DO I=LFT,LLT
              STRAGLO(I,1)=LBUF%STRA(II(1)+I)
              STRAGLO(I,2)=LBUF%STRA(II(2)+I)
              STRAGLO(I,3)=LBUF%STRA(II(3)+I)
              STRAGLO(I,4)=LBUF%STRA(II(4)+I)
              STRAGLO(I,5)=LBUF%STRA(II(5)+I)
              STRAGLO(I,6)=LBUF%STRA(II(6)+I)
             END DO
            ELSEIF(ISORTH==0)THEN
             DO I=LFT,LLT
C
C strain in corotational system
              L11    =LBUF%STRA(II(1)+I)
              L22    =LBUF%STRA(II(2)+I)
              L33    =LBUF%STRA(II(3)+I)
              L12    =HALF*LBUF%STRA(II(4)+I)
              L23    =HALF*LBUF%STRA(II(5)+I)
              L13    =HALF*LBUF%STRA(II(6)+I)
              S11    =L11*R11(I)+L12*R12(I)+L13*R13(I) 
              S12    =L11*R21(I)+L12*R22(I)+L13*R23(I) 
              S13    =L11*R31(I)+L12*R32(I)+L13*R33(I)
              S21    =L12*R11(I)+L22*R12(I)+L23*R13(I)
              S22    =L12*R21(I)+L22*R22(I)+L23*R23(I)
              S23    =L12*R31(I)+L22*R32(I)+L23*R33(I) 
              S31    =L13*R11(I)+L23*R12(I)+L33*R13(I)
              S32    =L13*R21(I)+L23*R22(I)+L33*R23(I)
              S33    =L13*R31(I)+L23*R32(I)+L33*R33(I)
              STRAGLO(I,1)=R11(I)*S11+R12(I)*S21+R13(I)*S31
              STRAGLO(I,2)=R21(I)*S12+R22(I)*S22+R23(I)*S32
              STRAGLO(I,3)=R31(I)*S13+R32(I)*S23+R33(I)*S33
              STRAGLO(I,4)=TWO*(R11(I)*S12+R12(I)*S22+R13(I)*S32)
              STRAGLO(I,5)=TWO*(R21(I)*S13+R22(I)*S23+R23(I)*S33)
              STRAGLO(I,6)=TWO*(R11(I)*S13+R12(I)*S23+R13(I)*S33)
             END DO
            ELSE
             DO I=LFT,LLT
C
C strain in orthotropic system
              L11    =LBUF%STRA(II(1)+I)
              L22    =LBUF%STRA(II(2)+I)
              L33    =LBUF%STRA(II(3)+I)
              L12    =HALF*LBUF%STRA(II(4)+I)
              L23    =HALF*LBUF%STRA(II(5)+I)
              L13    =HALF*LBUF%STRA(II(6)+I)
              S11    =L11*T11(I)+L12*T12(I)+L13*T13(I) 
              S12    =L11*T21(I)+L12*T22(I)+L13*T23(I) 
              S13    =L11*T31(I)+L12*T32(I)+L13*T33(I)
              S21    =L12*T11(I)+L22*T12(I)+L23*T13(I)
              S22    =L12*T21(I)+L22*T22(I)+L23*T23(I)
              S23    =L12*T31(I)+L22*T32(I)+L23*T33(I) 
              S31    =L13*T11(I)+L23*T12(I)+L33*T13(I)
              S32    =L13*T21(I)+L23*T22(I)+L33*T23(I)
              S33    =L13*T31(I)+L23*T32(I)+L33*T33(I)
              STRAGLO(I,1)=T11(I)*S11+T12(I)*S21+T13(I)*S31
              STRAGLO(I,2)=T21(I)*S12+T22(I)*S22+T23(I)*S32
              STRAGLO(I,3)=T31(I)*S13+T32(I)*S23+T33(I)*S33
              STRAGLO(I,4)=TWO*(T11(I)*S12+T12(I)*S22+T13(I)*S32)
              STRAGLO(I,5)=TWO*(T21(I)*S13+T22(I)*S23+T23(I)*S33)
              STRAGLO(I,6)=TWO*(T11(I)*S13+T12(I)*S23+T13(I)*S33)
             END DO
            END IF
          END IF
C-----------

          IF(ELBUF_TAB(NG)%BUFLY(1)%L_ANG > 0)THEN
            IF(JCVT == 0 .AND. ISORTH == 0)THEN
             DO I=LFT,LLT
              G11=LBUF%ANG(II(1)+I)
              G21=LBUF%ANG(II(2)+I)
              G31=LBUF%ANG(II(3)+I)
              G12=LBUF%ANG(II(4)+I)
              G22=LBUF%ANG(II(5)+I)
              G32=LBUF%ANG(II(6)+I)
              G13=G21*G32-G31*G22
              G23=G31*G12-G11*G32
              G33=G11*G22-G21*G12
C             MATRICE DE PASSAGE GLOBAL -> ANG
              S11=RX(I)*G11+SX(I)*G21+TX(I)*G31
              S12=RX(I)*G12+SX(I)*G22+TX(I)*G32
              S13=RX(I)*G13+SX(I)*G23+TX(I)*G33
              S21=RY(I)*G11+SY(I)*G21+TY(I)*G31
              S22=RY(I)*G12+SY(I)*G22+TY(I)*G32
              S23=RY(I)*G13+SY(I)*G23+TY(I)*G33
              S31=RZ(I)*G11+SZ(I)*G21+TZ(I)*G31
              S32=RZ(I)*G12+SZ(I)*G22+TZ(I)*G32
              S33=RZ(I)*G13+SZ(I)*G23+TZ(I)*G33
              ANGL(I,1)=S11
              ANGL(I,2)=S21
              ANGL(I,3)=S31
              ANGL(I,4)=S12
              ANGL(I,5)=S22
              ANGL(I,6)=S32
             END DO
            ELSEIF(JCVT /=0 .AND. ISORTH == 0)THEN
             DO I=LFT,LLT
              G11=LBUF%ANG(II(1)+I)
              G21=LBUF%ANG(II(2)+I)
              G31=LBUF%ANG(II(3)+I)
              G12=LBUF%ANG(II(4)+I)
              G22=LBUF%ANG(II(5)+I)
              G32=LBUF%ANG(II(6)+I)
              G13=G21*G32-G31*G22
              G23=G31*G12-G11*G32
              G33=G11*G22-G21*G12
C             MATRICE DE PASSAGE GLOBAL -> ANG
              S11=R11(I)*G11+R12(I)*G21+R13(I)*G31
              S12=R11(I)*G12+R12(I)*G22+R13(I)*G32
              S13=R11(I)*G13+R12(I)*G23+R13(I)*G33
              S21=R21(I)*G11+R22(I)*G21+R23(I)*G31
              S22=R21(I)*G12+R22(I)*G22+R23(I)*G32
              S23=R21(I)*G13+R22(I)*G23+R23(I)*G33
              S31=R31(I)*G11+R32(I)*G21+R33(I)*G31
              S32=R31(I)*G12+R32(I)*G22+R33(I)*G32
              S33=R31(I)*G13+R32(I)*G23+R33(I)*G33
              ANGL(I,1)=S11
              ANGL(I,2)=S21
              ANGL(I,3)=S31
              ANGL(I,4)=S12
              ANGL(I,5)=S22
              ANGL(I,6)=S32
             END DO
            ELSE
             DO I=LFT,LLT
C
C ISorth /=0 (ANG is given both for solid & sph wrt orthotropic system)
C             MATRICE DE PASSAGE ORTHOTROPE -> ANG
              ANGL(I,1)=LBUF%ANG(II(1)+I)
              ANGL(I,2)=LBUF%ANG(II(2)+I)
              ANGL(I,3)=LBUF%ANG(II(3)+I)
              ANGL(I,4)=LBUF%ANG(II(4)+I)
              ANGL(I,5)=LBUF%ANG(II(5)+I)
              ANGL(I,6)=LBUF%ANG(II(6)+I)
             END DO
            END IF
          END IF
C-----------
          IF(ELBUF_TAB(NG)%BUFLY(1)%L_DGLO > 0)THEN

            IF(JCVT == 0 .AND. ISORTH == 0)THEN
             DO I=LFT,LLT
C             TENSOR wrt ISOPARAMETRIC SYSTEM
              G11=LBUF%DGLO(II(1)+I)
              G22=LBUF%DGLO(II(2)+I)
              G33=LBUF%DGLO(II(3)+I)
              G12=LBUF%DGLO(II(4)+I)
              G23=LBUF%DGLO(II(5)+I)
              G13=LBUF%DGLO(II(6)+I)
              S11=G11*RX(I)+G12*SX(I)+G13*TX(I)
              S12=G11*RY(I)+G12*SY(I)+G13*TY(I)
              S13=G11*RZ(I)+G12*SZ(I)+G13*TZ(I)
              S21=G12*RX(I)+G22*SX(I)+G23*TX(I)
              S22=G12*RY(I)+G22*SY(I)+G23*TY(I)
              S23=G12*RZ(I)+G22*SZ(I)+G23*TZ(I)
              S31=G13*RX(I)+G23*SX(I)+G33*TX(I)
              S32=G13*RY(I)+G23*SY(I)+G33*TY(I)
              S33=G13*RZ(I)+G23*SZ(I)+G33*TZ(I)
C             TENSOR wrt GLOBAL SYSTEM
              DGLO24(I,1)=RX(I)*S11+SX(I)*S21+TX(I)*S31
              DGLO24(I,2)=RY(I)*S12+SY(I)*S22+TY(I)*S32
              DGLO24(I,3)=RZ(I)*S13+SZ(I)*S23+TZ(I)*S33
              DGLO24(I,4)=RX(I)*S12+SX(I)*S22+TX(I)*S32
              DGLO24(I,5)=RY(I)*S13+SY(I)*S23+TY(I)*S33
              DGLO24(I,6)=RX(I)*S13+SX(I)*S23+TX(I)*S33
             END DO
            ELSEIF(JCVT /=0 .AND. ISORTH == 0)THEN
             DO I=LFT,LLT
C             TENSOR wrt COROTATIONAL SYSTEM
              G11=LBUF%DGLO(II(1)+I)
              G22=LBUF%DGLO(II(2)+I)
              G33=LBUF%DGLO(II(3)+I)
              G12=LBUF%DGLO(II(4)+I)
              G23=LBUF%DGLO(II(5)+I)
              G13=LBUF%DGLO(II(6)+I)
              S11=G11*R11(I)+G12*R12(I)+G13*R13(I)
              S12=G11*R21(I)+G12*R22(I)+G13*R23(I)
              S13=G11*R31(I)+G12*R32(I)+G13*R33(I)
              S21=G12*R11(I)+G22*R12(I)+G23*R13(I)
              S22=G12*R21(I)+G22*R22(I)+G23*R23(I)
              S23=G12*R31(I)+G22*R32(I)+G23*R33(I)
              S31=G13*R11(I)+G23*R12(I)+G33*R13(I)
              S32=G13*R21(I)+G23*R22(I)+G33*R23(I)
              S33=G13*R31(I)+G23*R32(I)+G33*R33(I)
C             TENSOR wrt GLOBAL SYSTEM
              DGLO24(I,1)=R11(I)*S11+R12(I)*S21+R13(I)*S31
              DGLO24(I,2)=R21(I)*S12+R22(I)*S22+R23(I)*S32
              DGLO24(I,3)=R31(I)*S13+R32(I)*S23+R33(I)*S33
              DGLO24(I,4)=R11(I)*S12+R12(I)*S22+R13(I)*S32
              DGLO24(I,5)=R21(I)*S13+R22(I)*S23+R23(I)*S33
              DGLO24(I,6)=R11(I)*S13+R12(I)*S23+R13(I)*S33
             END DO
            ELSE
C
C ISorth /=0 (orthotropic system is the same for solid & sph)
C             TENSOR wrt ORTHOTROPIC SYSTEM
             DO I=LFT,LLT
              DGLO24(I,1)=LBUF%DGLO(II(1)+I)
              DGLO24(I,2)=LBUF%DGLO(II(2)+I)
              DGLO24(I,3)=LBUF%DGLO(II(3)+I)
              DGLO24(I,4)=LBUF%DGLO(II(4)+I)
              DGLO24(I,5)=LBUF%DGLO(II(5)+I)
              DGLO24(I,6)=LBUF%DGLO(II(6)+I)
             END DO
            END IF
          END IF
C-----------
          DO I=LFT,LLT
            IF(GBUF%OFF(I)==ZERO) CYCLE
            N=NFT+I
C
C           SOL2SPH(1,N)+1<=I<=SOLSPH(2,N) <=> N==SPH2SOL(I)
            NSPHDIR=NINT((SOL2SPH(2,N)-SOL2SPH(1,N))**THIRD)
            DO KP=1,SOL2SPH(2,N)-SOL2SPH(1,N)
C
              NP=SOL2SPH(1,N)+KP
              MG =MOD(-KXSP(2,NP),NGROUP+1)
              NELSP=IPARG(2,MG)
              KFT=IPARG(3,MG)
              GBUFSP => ELBUF_TAB(MG)%GBUF
              LBUFSP => ELBUF_TAB(MG)%BUFLY(1)%LBUF(1,1,1)
              MBUFSP => ELBUF_TAB(MG)%BUFLY(1)%MAT(1,1,1)
              J=NP-KFT
              RHON = GBUF%RHO(I)
              RHOO = WA(10,NP)
              DIVV = (RHOO-RHON)/MAX(EM30,RHOO*DT1)
              WA(13,NP)     = DIVV
              WA(14,NP)     = ZERO
              SPBUF(2,NP)   = RHON
              GBUFSP%RHO(J) = RHON
C
C             group was not computed 
C             (there is no cloud active particle within the group)
C             IF(IPARG(8,MG)==1)CYCLE
C
              GBUFSP%EINT(J)        =GBUF%EINT(I)
C
!
              DO K=1,6
                JJ(K) = NELSP*(K-1)
              ENDDO
!
C-----------
              IF (JHBE==14) THEN
C HA8 stress
                IR=IRST(1,NP-FIRST_SPHSOL+1)
                IS=IRST(2,NP-FIRST_SPHSOL+1)
                IT=IRST(3,NP-FIRST_SPHSOL+1)
                DO K=1,6
                  GBUFSP%SIG(JJ(K)+J)=SIG_HA8(I,IR,IS,IT,K)
                ENDDO
              ELSEIF (JHBE==24) THEN
C Hourglass stress contribution for HEPH
                IR=IRST(1,NP-FIRST_SPHSOL+1)
                IS=IRST(2,NP-FIRST_SPHSOL+1)
                IT=IRST(3,NP-FIRST_SPHSOL+1)
C--- Permutation (KSI-> ETA;ETA->ZETA;ZETA->KSI) for consistencvy with HEPH convention
                ETA  = A_GAUSS(IR,NSPHDIR)
                ZETA = A_GAUSS(IS,NSPHDIR)
                KSI  = A_GAUSS(IT,NSPHDIR)
C-----------
                DO K=1,6
                  GBUFSP%SIG(JJ(K)+J) = SIG_HEPH_GLO(I,K,1)
     .                            +ZETA*SIG_HEPH_GLO(I,K,2)
     .                             +ETA*SIG_HEPH_GLO(I,K,3)
     .                             +KSI*SIG_HEPH_GLO(I,K,4)
     .                        +ZETA*ETA*SIG_HEPH_GLO(I,K,5)
     .                        +ZETA*KSI*SIG_HEPH_GLO(I,K,6)
     .                         +ETA*KSI*SIG_HEPH_GLO(I,K,7)            
                END DO
              ELSE
                GBUFSP%SIG(JJ(1)+J) = SIGLO(I,1)
                GBUFSP%SIG(JJ(2)+J) = SIGLO(I,2)
                GBUFSP%SIG(JJ(3)+J) = SIGLO(I,3)
                GBUFSP%SIG(JJ(4)+J) = SIGLO(I,4)
                GBUFSP%SIG(JJ(5)+J) = SIGLO(I,5)
                GBUFSP%SIG(JJ(6)+J) = SIGLO(I,6)
              ENDIF
C-----------
              WA(1,NP)=GBUFSP%SIG(JJ(1)+J)
              WA(2,NP)=GBUFSP%SIG(JJ(2)+J)
              WA(3,NP)=GBUFSP%SIG(JJ(3)+J)
              WA(4,NP)=GBUFSP%SIG(JJ(4)+J)
              WA(5,NP)=GBUFSP%SIG(JJ(5)+J)
              WA(6,NP)=GBUFSP%SIG(JJ(6)+J)
C
C               (particles ARE computed with rho_old, Eold...)
C-----------
              IF(GBUF%G_PLA > 0) GBUFSP%PLA(J) = GBUF%PLA(I)
              IF(GBUF%G_EPSD> 0) GBUFSP%EPSD(J)= GBUF%EPSD(I)
              IF(GBUF%G_EPSQ> 0) GBUFSP%EPSQ(J)= GBUF%EPSQ(I)
C-----------
              IF(GBUF%G_GAMA > 0)THEN
C
C TIJ stores global to orthotropic system
                GBUFSP%GAMA(JJ(1)+J)=T11(I)
                GBUFSP%GAMA(JJ(2)+J)=T21(I)
                GBUFSP%GAMA(JJ(3)+J)=T31(I)
                GBUFSP%GAMA(JJ(4)+J)=T12(I)
                GBUFSP%GAMA(JJ(5)+J)=T22(I)
                GBUFSP%GAMA(JJ(6)+J)=T32(I)
              END IF
C-----------
              IF(ELBUF_TAB(NG)%BUFLY(1)%L_STRA > 0.AND.
     .           ELBUF_TAB(MG)%BUFLY(1)%L_STRA > 0)THEN
                LBUFSP%STRA(JJ(1)+J)=STRAGLO(I,1)
                LBUFSP%STRA(JJ(2)+J)=STRAGLO(I,2)
                LBUFSP%STRA(JJ(3)+J)=STRAGLO(I,3)
                LBUFSP%STRA(JJ(4)+J)=STRAGLO(I,4)
                LBUFSP%STRA(JJ(5)+J)=STRAGLO(I,5)
                LBUFSP%STRA(JJ(6)+J)=STRAGLO(I,6)
              END IF
C-----------
              IF(ELBUF_TAB(NG)%BUFLY(1)%L_ANG > 0)THEN
                LBUFSP%ANG(JJ(1)+J)=ANGL(I,1)
                LBUFSP%ANG(JJ(2)+J)=ANGL(I,2)
                LBUFSP%ANG(JJ(3)+J)=ANGL(I,3)
                LBUFSP%ANG(JJ(4)+J)=ANGL(I,4)
                LBUFSP%ANG(JJ(5)+J)=ANGL(I,5)
                LBUFSP%ANG(JJ(6)+J)=ANGL(I,6)
              END IF
C-----------
              IF(ELBUF_TAB(NG)%BUFLY(1)%L_SF > 0)THEN
                LBUFSP%SF(JJ(1)+J)=LBUF%SF(II(1)+I)
                LBUFSP%SF(JJ(2)+J)=LBUF%SF(II(2)+I)
                LBUFSP%SF(JJ(3)+J)=LBUF%SF(II(3)+I)
              END IF
C-----------
              IF(ELBUF_TAB(NG)%BUFLY(1)%L_DAM > 0)THEN
                DO K=1,ELBUF_TAB(NG)%BUFLY(1)%L_DAM
                  LBUFSP%DAM(JJ(K)+J)=LBUF%DAM(II(K)+I)
                ENDDO
              END IF
C-----------
              IF(ELBUF_TAB(NG)%BUFLY(1)%L_DSUM > 0)
     .          LBUFSP%DSUM(J)=LBUF%DSUM(I)
C-----------
              IF(ELBUF_TAB(NG)%BUFLY(1)%L_DGLO > 0)THEN
                LBUFSP%DGLO(JJ(1)+J)=DGLO24(I,1)
                LBUFSP%DGLO(JJ(2)+J)=DGLO24(I,2)
                LBUFSP%DGLO(JJ(3)+J)=DGLO24(I,3)
                LBUFSP%DGLO(JJ(4)+J)=DGLO24(I,4)
                LBUFSP%DGLO(JJ(5)+J)=DGLO24(I,5)
                LBUFSP%DGLO(JJ(6)+J)=DGLO24(I,6)
              END IF
C-----------
              IF(ELBUF_TAB(NG)%BUFLY(1)%L_ROB > 0)
     .          LBUFSP%ROB(J)=LBUF%ROB(I)
C-----------
              IF(ELBUF_TAB(NG)%BUFLY(1)%L_SIGC > 0)THEN
C
C Law24 / Stress in crack frame (same for SPH and solids)
                LBUFSP%SIGC(JJ(1)+J)=LBUF%SIGC(II(1)+I)
                LBUFSP%SIGC(JJ(2)+J)=LBUF%SIGC(II(2)+I)
                LBUFSP%SIGC(JJ(3)+J)=LBUF%SIGC(II(3)+I)
                LBUFSP%SIGC(JJ(4)+J)=LBUF%SIGC(II(4)+I)
                LBUFSP%SIGC(JJ(5)+J)=LBUF%SIGC(II(5)+I)
                LBUFSP%SIGC(JJ(6)+J)=LBUF%SIGC(II(6)+I)
              END IF
C-----------
              IF(ELBUF_TAB(NG)%BUFLY(1)%L_CRAK > 0)THEN
                LBUFSP%CRAK(JJ(1)+J)=LBUF%CRAK(II(1)+I)
                LBUFSP%CRAK(JJ(2)+J)=LBUF%CRAK(II(2)+I)
                LBUFSP%CRAK(JJ(3)+J)=LBUF%CRAK(II(3)+I)
              END IF
C-----------
              IF(ELBUF_TAB(NG)%BUFLY(1)%L_EPSA > 0)THEN
                LBUFSP%EPSA(JJ(1)+J)=LBUF%EPSA(II(1)+I)
                LBUFSP%EPSA(JJ(2)+J)=LBUF%EPSA(II(2)+I)
                LBUFSP%EPSA(JJ(3)+J)=LBUF%EPSA(II(3)+I)
              END IF
C-----------
              IF(ELBUF_TAB(NG)%BUFLY(1)%L_SIGA > 0)THEN
                LBUFSP%SIGA(JJ(1)+J)=LBUF%SIGA(II(1)+I)
                LBUFSP%SIGA(JJ(2)+J)=LBUF%SIGA(II(2)+I)
                LBUFSP%SIGA(JJ(3)+J)=LBUF%SIGA(II(3)+I)
              END IF
C-----------
C Stress in orthotropic skew for mulaw
              IF(ELBUF_TAB(NG)%BUFLY(1)%L_SIGL > 0)THEN
                LBUFSP%SIGL(JJ(1)+J)=LBUF%SIGL(II(1)+I)
                LBUFSP%SIGL(JJ(2)+J)=LBUF%SIGL(II(2)+I)
                LBUFSP%SIGL(JJ(3)+J)=LBUF%SIGL(II(3)+I)
                LBUFSP%SIGL(JJ(4)+J)=LBUF%SIGL(II(4)+I)
                LBUFSP%SIGL(JJ(5)+J)=LBUF%SIGL(II(5)+I)
                LBUFSP%SIGL(JJ(6)+J)=LBUF%SIGL(II(6)+I)
              END IF
C-----------
C User variables (same for SPH and solids)
              IF(ELBUF_TAB(NG)%BUFLY(1)%NVAR_MAT > 0)THEN
                DO K=1,ELBUF_TAB(NG)%BUFLY(1)%NVAR_MAT
                  MBUFSP%VAR(NELSP*(K-1)+J) = MBUF%VAR(NEL*(K-1)+I)
                END DO
              ENDIF
C-----------
            ENDDO
          ENDDO
        END IF
        IF (IDDW>0) CALL STOPTIMEG(NG)
C--------
 300      CONTINUE
      END DO
!$OMP END DO
C-----------------------------------------------
      RETURN
      END SUBROUTINE SOLTOSPHP
