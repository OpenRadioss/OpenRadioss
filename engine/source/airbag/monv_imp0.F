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
Chd|  MONV_PREM                     source/airbag/monv_imp0.F
Chd|-- called by -----------
Chd|        DIM_GLOB_K                    source/implicit/ind_glob_k.F
Chd|-- calls ---------------
Chd|        DIM_KINMV                     source/airbag/monv_imp0.F
Chd|        INI_KINMV                     source/airbag/monv_imp0.F
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        IMP_MONV                      share/modules/impbufdef_mod.F
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE MONV_PREM(
     1    NMONV     ,IMONV     ,MONVOL    ,IGRSURF   ,
     2    FR_MV     ,ITAG      ,NPBY      ,LPBY      ,NRBYAC    ,
     3    IRBYAC    ,NINT2     ,IINT2     ,IPARI     ,INTBUF_TAB,
     4    NDOF      ,IPREC0    ,IRBE3     ,IRBE2     ,LRBE2     )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
        USE IMP_MONV
        USE INTBUFDEF_MOD
        USE GROUPDEF_MOD
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
#include      "scr05_c.inc"
#include      "task_c.inc"
#include      "units_c.inc"
#include      "impl1_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER NMONV,IMONV(*),MONVOL(*),
     .          FR_MV(NSPMD+2,NVOLU),ITAG(*),NDOF(*),IPREC0
        INTEGER NPBY(NNPBY,*),LPBY(*),NRBYAC,IRBYAC(*),
     .          NINT2,IINT2(*),IPARI(NPARI,*),IRBE3(NRBE3L,*),
     .          IRBE2(NRBE2L,*),LRBE2(*)
C     REAL

        TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
        TYPE (SURF_)   , DIMENSION(NSURF)   :: IGRSURF
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER I, NTYP,NN,K1,IAD,IS,J,N,NOD,NUMN,ID,NMT,IROT,
     .          IERR1,IERR2,IERR3,IERR4,IERR5,IERR6
C-------IMONV(NMONV)--nb de noeuds dans chaque mv.----------------------
        NMONV = 0
        IF(NVOLU>0.AND.IMPMV>0) THEN
          DO N=1,NUMNOD
            ITAG(N) = 0
          ENDDO
          NUMN_MV = 0
          NNMAX_MV = 0
          K1 = 1
          DO I=1,NVOLU
            NTYP=MONVOL(K1+1)
            IMONV(I)=0
            IF(NTYP==3) THEN
              IF(IMACH==3.AND.FR_MV(ISPMD+1,I)==0) GO TO 100
              IS   = MONVOL(K1+3)
              NN = IGRSURF(IS)%NSEG
              NUMN = NUMN_MV
              IF(NN>0) THEN
                DO J=1,NN
                  DO N=1,4
                    NOD = IGRSURF(IS)%NODES(J,N)
                    IF (ITAG(NOD)==0) THEN
                      NUMN_MV = NUMN_MV + 1
                      ITAG(NOD) = NUMN_MV
                    ENDIF
                  ENDDO
                ENDDO
                NNMAX_MV = MAX(NNMAX_MV,NN)
              ENDIF
              IMONV(I)=NUMN_MV - NUMN
              IF (IMONV(I)>0) NMONV = NMONV+1
            ELSEIF (NEIG==ZERO) THEN
              IF(IMACH/=3.OR.ISPMD==0) THEN
                WRITE(IOUT,1001)NTYP
                WRITE(ISTDO,1001)NTYP
              ENDIF
            ENDIF
 100        CONTINUE
            K1 = K1 + NIMV
          ENDDO
        ENDIF
C--------allocation------
        IF (NMONV == 0) RETURN
C
        ALLOCATE(IN_MV(NUMN_MV),ID_MV(3,NUMN_MV),STAT=IERR1)
        DO I=1,NUMNOD
          J = ITAG(I)
          IF (J>0) IN_MV(J) = I
        ENDDO
        CALL DIM_KINMV(
     1      NPBY      ,LPBY      ,NRBYAC    ,IRBYAC    ,NINT2     ,
     2      IINT2     ,IPARI     ,INTBUF_TAB,ITAG      ,NRB_MV    ,
     3      NI2_MV    ,IRBE3     ,NRBE3_MV  ,IRBE2     ,LRBE2     ,
     4      NRBE2_MV  )
        IF (NI2_MV>0) THEN
          ALLOCATE(II2_MV(2,NI2_MV),ID_MVM2(6,4,NI2_MV),STAT=IERR2)
        ENDIF
        IF (NRB_MV>0) THEN
          ALLOCATE(IRB_MV(2,NRB_MV),ID_MVM(6,NRB_MV),STAT=IERR4)
        ENDIF
        IF (NRBE3_MV>0) THEN
          ALLOCATE(IRBE3_MV(NRBE3_MV),STAT=IERR3)
        ENDIF
        IF (NRBE2_MV>0) THEN
          ALLOCATE(IRBE2_MV(2,NRBE2_MV),ID_MVM4(6,NRBE2_MV),STAT=IERR3)
          ID_MVM4=0
        ENDIF
        CALL INI_KINMV(
     1      NPBY      ,LPBY      ,NRBYAC    ,IRBYAC    ,NINT2     ,
     2      IINT2     ,IPARI     ,INTBUF_TAB,ITAG      ,NRB_MV    ,
     3      IRB_MV    ,NI2_MV    ,II2_MV    ,IRBE3     ,NRBE3_MV  ,
     4      IRBE3_MV  ,IRBE2     ,LRBE2     ,NRBE2_MV  ,IRBE2_MV  )
C-----------ini RBE3
        IF (NRBE3_MV>0) THEN
          IAD=0
          NMT = 0
          IROT=0
          DO I=1,NRBE3_MV
            N=IRBE3_MV(I)
            NUMN = IRBE3(5,N)
            IAD=MAX(IAD,NUMN)
            NMT = NMT + NUMN
            IROT=MAX(IROT,IRBE3(6,N))
          ENDDO
          ALLOCATE(ID_MVM3(6,IAD,NRBE3_MV),STAT=IERR3)
          ID_MVM3=0
          R3M_MAX=IAD
          ALLOCATE(FCDI_MV(18*NMT),STAT=IERR5)
          FCDI_MV=ZERO
          IF (IROT>0) THEN
            ALLOCATE(MCDI_MV(18*NMT),STAT=IERR5)
            MCDI_MV=ZERO
          ENDIF
        ENDIF
        IF (IPREC >= 2) THEN
          ALLOCATE(DIAG_MV(3,NUMN_MV),STAT=IERR2)
          DIAG_MV=ZERO
          IF (NI2_MV>0) ALLOCATE(DIAG_MVM2(6,4,NI2_MV),STAT=IERR5)
          IF (NRB_MV>0) ALLOCATE(DIAG_MVM(6,NRB_MV),STAT=IERR6)
          IF (NRBE3_MV>0) THEN
            ALLOCATE(DIAG_MVM3(6,R3M_MAX,NRBE3_MV),STAT=IERR5)
            DIAG_MVM3=ZERO
          ENDIF
          IF (NRBE2_MV>0) THEN
            ALLOCATE(DIAG_MVM4(6,NRBE2_MV),STAT=IERR5)
            DIAG_MVM4=ZERO
          ENDIF
        ENDIF
C
 1001   FORMAT(5X,'*****WARNING : IMPLICIT OPTION IS NOT AVAILABLE',
     .         ' WITH MONITORED VOLUME TYPE:',I3/,5X,
     .         '******  IT WILL BE IGNORED  *****')
        RETURN
      END
Chd|====================================================================
Chd|  DIM_KINMV                     source/airbag/monv_imp0.F
Chd|-- called by -----------
Chd|        MONV_PREM                     source/airbag/monv_imp0.F
Chd|-- calls ---------------
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE DIM_KINMV(
     1    NPBY      ,LPBY      ,NRBYAC    ,IRBYAC    ,NINT2     ,
     2    IINT2     ,IPARI     ,INTBUF_TAB,INLOC     ,LNS       ,
     3    LNS2      ,IRBE3     ,LNS3      ,IRBE2     ,LRBE2     ,
     4    LNS4      )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
        USE INTBUFDEF_MOD
C----6------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER NPBY(NNPBY,*),LPBY(*),NRBYAC,IRBYAC(*),
     .          NINT2,IINT2(*),IPARI(NPARI,*)
        INTEGER
     .     INLOC(*),LNS  ,LNS2,IRBE3(NRBE3L,*) ,LNS3,
     .     IRBE2(NRBE2L,*),LRBE2(*),LNS4
C     REAL
        TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER
     .          I,J,K,N,L,NL,NJ,NI,J1,M,NSN,N1,N2,NK,ID,
     .          JI,K10,K11,K12,K13,K14,KFI,NS
C----------------------------
        LNS2=0
        DO J=1,NINT2
          N=IINT2(J)
          NSN = IPARI(5,N)
          JI=IPARI(1,N)
          K10=JI-1
          K11=K10+4*IPARI(3,N)
C------IRECT(4,NSN)-----
          K12=K11+4*IPARI(4,N)
C------NSV(NSN)--node number---
          K13=K12+NSN
C------MSR(NMN)-----
          K14=K13+IPARI(6,N)
C------IRTL(NSN)--main el number---
          KFI=K14+NSN
          DO I=1,NSN
            NI=INTBUF_TAB(N)%NSV(I)
            IF (INLOC(NI)>0) THEN
              LNS2=LNS2+1
            ENDIF
          ENDDO
        ENDDO
C-----RBE2------
        LNS4=0
        DO N=1,NRBE2
          K =IRBE2(1,N)
          M =IRBE2(3,N)
          NSN  =IRBE2(5,N)
          DO I=1,NSN
            ID = I+K
            NI=LRBE2(ID)
            IF (INLOC(NI)>0) THEN
              LNS4=LNS4+1
              IF (INLOC(M)==0) INLOC(M) = 2
            ENDIF
          ENDDO
        ENDDO
C--------RBE3--------------------
        LNS3=0
        DO N=1,NRBE3
          NI = IRBE3(3,N)
          IF (NI==0) CYCLE
          IF (INLOC(NI)>0) THEN
            LNS3=LNS3+1
          ENDIF
        ENDDO
C-----active rigid body main nodes------
        LNS=0
        DO J=1,NRBYAC
          N=IRBYAC(J)
          K=IRBYAC(J+NRBYKIN)
          M  =NPBY(1,N)
          NSN  =NPBY(2,N)
          DO I=1,NSN
            ID = I+K
            NI=LPBY(ID)
            IF (INLOC(NI)>0) THEN
              LNS=LNS+1
              IF (INLOC(M)==0) INLOC(M) = 1
            ENDIF
          ENDDO
        ENDDO
C----6---------------------------------------------------------------7---------8
        RETURN
      END
Chd|====================================================================
Chd|  INI_KINMV                     source/airbag/monv_imp0.F
Chd|-- called by -----------
Chd|        MONV_PREM                     source/airbag/monv_imp0.F
Chd|-- calls ---------------
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE INI_KINMV(
     1    NPBY      ,LPBY      ,NRBYAC    ,IRBYAC    ,NINT2     ,
     2    IINT2     ,IPARI     ,INTBUF_TAB,INLOC     ,NRB_MV    ,
     3    IRB_MV    ,NI2_MV    ,II2_MV    ,IRBE3     ,NRBE3_MV  ,
     4    IRBE3_MV  ,IRBE2     ,LRBE2     ,NRBE2_MV  ,IRBE2_MV  )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
        USE INTBUFDEF_MOD
C----6------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER NPBY(NNPBY,*),LPBY(*),NRBYAC,IRBYAC(*),
     .          NINT2,IINT2(*),IPARI(NPARI,*)
        INTEGER
     .     INLOC(*),NRB_MV,NI2_MV,IRB_MV(2,*),II2_MV(2,*),
     .     IRBE3(NRBE3L,*),NRBE3_MV  ,IRBE3_MV(*),
     .     IRBE2(NRBE2L,*),LRBE2(*),NRBE2_MV  ,IRBE2_MV(2,*)
C     REAL

        TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER
     .          I,J,K,N,L,NL,NJ,NI,J1,M,NSN,N1,N2,NK,ID,
     .          JI,K10,K11,K12,K13,K14,KFI,NI2,NRB,NR3,NR2
c----------------------
        NI2=0
        IF (NI2_MV>0) THEN
          DO J=1,NINT2
            N=IINT2(J)
            NSN = IPARI(5,N)
            JI=IPARI(1,N)
            K10=JI-1
            K11=K10+4*IPARI(3,N)
C------IRECT(4,NSN)-----
            K12=K11+4*IPARI(4,N)
C------NSV(NSN)--node number---
            K13=K12+NSN
C------MSR(NMN)-----
            K14=K13+IPARI(6,N)
C------IRTL(NSN)--main el number---
            KFI=K14+NSN
            DO I=1,NSN
              NI=INTBUF_TAB(N)%NSV(I)
              IF (INLOC(NI)>0) THEN
                NI2=NI2+1
                II2_MV(1,NI2)=N
                II2_MV(2,NI2)=I
              ENDIF
            ENDDO
          ENDDO
          IF (NI2/=NI2_MV) WRITE(*,*)'pb cal NI2_MV'
        ENDIF
C-----RBE2-----
        NR2=0
        IF (NRBE2_MV>0) THEN
          DO N=1,NRBE2
            K =IRBE2(1,N)
            M =IRBE2(3,N)
            IF (INLOC(M)>0) THEN
              NSN  =IRBE2(5,N)
              DO I=1,NSN
                ID = I+K
                NI=LRBE2(ID)
                IF (INLOC(NI)>0) THEN
                  NR2=NR2+1
                  IRBE2_MV(1,NR2)=N
                  IRBE2_MV(2,NR2)=NI
                ENDIF
              ENDDO
            ENDIF
          ENDDO
          IF (NR2/=NRBE2_MV) WRITE(*,*)'pb cal NRBE2_MV'
        ENDIF
C--------RBE3--------------------
        IF (NRBE3_MV>0) THEN
          NR3=0
          DO N=1,NRBE3
            NI = IRBE3(3,N)
            IF (NI==0) CYCLE
            IF (INLOC(NI)>0) THEN
              NR3=NR3+1
              IRBE3_MV(NR3)=N
            ENDIF
          ENDDO
          IF (NR3/=NRBE3_MV) WRITE(*,*)'pb cal NRBE3_MV'
        ENDIF
C-----active rigid body main nodes------
        NRB=0
        IF (NRB_MV>0) THEN
          DO J=1,NRBYAC
            N=IRBYAC(J)
            K=IRBYAC(J+NRBYKIN)
            M  =NPBY(1,N)
            IF (INLOC(M)>0) THEN
              NSN  =NPBY(2,N)
              DO I=1,NSN
                ID = I+K
                NI=LPBY(ID)
                IF (INLOC(NI)>0) THEN
                  NRB=NRB+1
                  IRB_MV(1,NRB)=M
                  IRB_MV(2,NRB)=NI
                ENDIF
              ENDDO
            ENDIF
          ENDDO
          IF (NRB/=NRB_MV) WRITE(*,*)'pb cal NRB_MV'
        ENDIF
C----6---------------------------------------------------------------7---------8
        RETURN
      END
Chd|====================================================================
Chd|  MONV_FVL                      source/airbag/monv_imp0.F
Chd|-- called by -----------
Chd|        MONV_IMP                      source/airbag/monv_imp0.F
Chd|-- calls ---------------
Chd|        IMP_ASPC                      share/modules/impbufdef_mod.F
Chd|        IMP_MONV                      share/modules/impbufdef_mod.F
Chd|        IMP_RWL                       share/modules/impbufdef_mod.F
Chd|====================================================================
      SUBROUTINE MONV_FVL(IBFV      ,LJ      ,ISKEW   ,ICODT  )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
        USE IMP_MONV
        USE IMP_RWL
        USE IMP_ASPC
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER IBFV(NIFV,*),LJ(*),ISKEW(*) ,ICODT(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER I,J,K,N,L,IERR1,IERR2,IERR3,ITAG(NUMNOD)
C-----------------------------------------------
        DO N=1,NUMNOD
          ITAG(N) = 0
        ENDDO
        DO I=1,NUMN_MV
          N = IN_MV(I)
          ITAG(N) = I
        ENDDO
        NBC_MV = 0
        DO N=1,NUMNOD
          IF (ISKEW(N)>1.AND.ICODT(N)/=7) THEN
            IF (ITAG(N)>0)NBC_MV = NBC_MV + 1
          ENDIF
        ENDDO
        IF (NBC_MV>0) THEN
          IF(ALLOCATED(IBC_MV)) DEALLOCATE(IBC_MV)
          ALLOCATE(IBC_MV(3,NBC_MV),STAT=IERR1)
          NBC_MV = 0
          DO N=1,NUMNOD
            IF (ISKEW(N)>1.AND.ICODT(N)/=7) THEN
              IF (ITAG(N)>0) THEN
                NBC_MV = NBC_MV + 1
                IBC_MV(1,NBC_MV) = N
                IBC_MV(2,NBC_MV) = ISKEW(N)
                IBC_MV(3,NBC_MV) = ICODT(N)
              ENDIF
            ENDIF
          ENDDO
        ENDIF
C-----AUTOSPC----
        NSPC_MV = 0
        DO N = 1, NSPCL
          IF (ITAG(N)>0)NSPC_MV = NSPC_MV + 1
        ENDDO
        IF (NSPC_MV>0) THEN
          IF(ALLOCATED(ISPC_MV)) DEALLOCATE(ISPC_MV)
          ALLOCATE(ISPC_MV(NSPC_MV),STAT=IERR1)
          NSPC_MV = 0
          DO N=1,NSPCL
            IF (ITAG(N)>0) THEN
              NSPC_MV = NSPC_MV + 1
              ISPC_MV(NSPC_MV) = N
            ENDIF
          ENDDO
        ENDIF
C
        NFX_MV = 0
        DO J=1,NFXVEL
          IF (LJ(J)>0.AND.LJ(J)<=3) THEN
            N=IABS(IBFV(1,J))
            IF (ITAG(N)>0)NFX_MV = NFX_MV + 1
          ENDIF
        ENDDO
        IF (NFX_MV>0) THEN
          IF(ALLOCATED(IFX_MV)) DEALLOCATE(IFX_MV)
          ALLOCATE(IFX_MV(2,NFX_MV),STAT=IERR2)
          NFX_MV = 0
          DO J=1,NFXVEL
            IF (LJ(J)>0.AND.LJ(J)<=3) THEN
              N=IABS(IBFV(1,J))
              IF (ITAG(N)>0) THEN
                NFX_MV = NFX_MV + 1
                IFX_MV(1,NFX_MV) = J
                IFX_MV(2,NFX_MV) = LJ(J)
              ENDIF
            ENDIF
          ENDDO
        ENDIF
        NRW_MV = 0
        DO J=1,N_RWL
          N=IN_RWL(J)
          IF (ITAG(N)>0) NRW_MV = NRW_MV + 1
        ENDDO
        IF (NRW_MV>0) THEN
          IF(ALLOCATED(IRW_MV)) DEALLOCATE(IRW_MV)
          ALLOCATE(IRW_MV(NRW_MV),STAT=IERR3)
          NRW_MV = 0
          DO J=1,N_RWL
            N=IN_RWL(J)
            IF (ITAG(N)>0) THEN
              NRW_MV = NRW_MV + 1
              IRW_MV(NRW_MV) = J
            ENDIF
          ENDDO
        ENDIF
C
        RETURN
      END
Chd|====================================================================
Chd|  MONV_IMP                      source/airbag/monv_imp0.F
Chd|-- called by -----------
Chd|        IMP_SOLV                      source/implicit/imp_solv.F
Chd|-- calls ---------------
Chd|        MONV_FVL                      source/airbag/monv_imp0.F
Chd|        MONV_M3                       source/airbag/monv_imp0.F
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE MONV_IMP(MONVOL ,VOLMON  ,X      ,IGRSURF  ,
     1                    NMONV  ,IMONV   ,IPARI  ,INTBUF_TAB      ,
     2                    A_MV   ,AR_MV   ,NDOF   ,IDDL    ,IKC    ,
     3                    INLOC  ,IPREC   ,IBFV   ,SKEW    ,XFRAME ,
     4                    LJ     ,ISKEW   ,ICODT  ,IRBE3   ,LRBE3  ,
     5                    FRBE3  ,IRBE2   ,LRBE2  ,NSURF   )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
        USE INTBUFDEF_MOD
        USE GROUPDEF_MOD
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER NMONV,IMONV(*),MONVOL(*),
     .          IPARI(*), NDOF(*),IDDL(*),IKC(*),
     .          INLOC(*),IPREC,IBFV(*),LJ(*),ISKEW(*),ICODT(*),
     .          IRBE3(*),LRBE3(*),IRBE2(*),LRBE2(*),NSURF
C     REAL
        my_real
     .     X(3,*),A_MV(3,*),AR_MV(3,*), VOLMON(*) ,
     .     SKEW(*) ,XFRAME(*),FRBE3(*)
        TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
        TYPE(SURF_)   ,DIMENSION(NSURF)   :: IGRSURF
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER I,J
C-----------------------------------------------
        CALL MONV_FVL(IBFV      ,LJ    ,ISKEW   ,ICODT  )
        CALL MONV_M3(MONVOL ,VOLMON  ,X      ,IGRSURF  ,
     1               NMONV  ,IMONV   ,IPARI  ,INTBUF_TAB,
     2               A_MV   ,AR_MV   ,NDOF   ,IDDL    ,IKC    ,
     3               INLOC  ,IPREC   ,IBFV   ,SKEW    ,XFRAME ,
     4               IRBE3  ,LRBE3   ,FRBE3  ,IRBE2   ,LRBE2  )
C
        RETURN
      END
Chd|====================================================================
Chd|  MONV_M3                       source/airbag/monv_imp0.F
Chd|-- called by -----------
Chd|        MONV_IMP                      source/airbag/monv_imp0.F
Chd|-- calls ---------------
Chd|        ID_MVINI                      source/airbag/monv_imp0.F
Chd|        MONV_KD                       source/airbag/monv_imp0.F
Chd|        UPDK_MV                       source/airbag/monv_imp0.F
Chd|        ZEROR                         source/system/zero.F
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        IMP_MONV                      share/modules/impbufdef_mod.F
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE MONV_M3(MONVOL ,VOLMON  ,X      ,IGRSURF  ,
     1                   NMONV  ,IMONV   ,IPARI  ,INTBUF_TAB,
     2                   A_MV   ,AR_MV   ,NDOF   ,IDDL    ,IKC    ,
     3                   INLOC  ,IPREC   ,IBFV   ,SKEW    ,XFRAME ,
     4                   IRBE3  ,LRBE3   ,FRBE3  ,IRBE2   ,LRBE2  )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
        USE IMP_MONV
        USE INTBUFDEF_MOD
        USE GROUPDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER NMONV,IMONV(*),MONVOL(*),
     .          IPARI(NPARI,*), NDOF(*),IDDL(*),IKC(*),
     .          INLOC(*),IPREC,IBFV(*),IRBE3(NRBE3L,*),LRBE3(*),
     .          IRBE2(NRBE2L,*),LRBE2(*)
C     REAL
        my_real
     .     X(3,*),A_MV(3,*),AR_MV(3,*), VOLMON(*) ,
     .     SKEW(*) ,XFRAME(*),FRBE3(*)

        TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
        TYPE(SURF_)   ,DIMENSION(NSURF)   :: IGRSURF
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER I,J,IDDLM(NUMNOD),NKC,N,ND,ID,NKIN,IAD
        INTEGER M,NSN,JI,K10,K11,K12,K13,K14,L,NNOD,NJ,NL,NI
C----------------
        CALL ID_MVINI(IPARI  ,INTBUF_TAB,NDOF   ,IDDL    ,IKC    ,
     1                INLOC  ,X      ,SKEW   ,IRBE3   ,LRBE3  ,
     2                FRBE3  ,IRBE2  ,LRBE2  )
        IF (IPREC<2) RETURN
        CALL ZEROR(A_MV,NUMNOD)
        CALL MONV_KD(MONVOL ,VOLMON  ,X      ,IGRSURF ,
     1               NMONV  ,IMONV   ,A_MV   ,NNMAX_MV)
        NKIN = NRB_MV+NI2_MV+NFX_MV+NBC_MV+NRW_MV+NRBE3_MV+NSPC_MV
     .         +NRBE2_MV
        IF (NKIN>0) THEN
          IF ((NRB_MV+NI2_MV+NRBE3_MV+NSPC_MV+NRBE2_MV)>0)
     .        CALL ZEROR(AR_MV,NUMNOD)
          CALL UPDK_MV(NDOF    ,IPARI  ,INTBUF_TAB,NI2_MV ,
     .                 II2_MV  ,NRB_MV ,IRB_MV  ,NFX_MV,IFX_MV ,
     .                 NBC_MV  ,IBC_MV ,NRW_MV  ,IRW_MV,IBFV   ,
     .                 SKEW    ,XFRAME ,X       ,A_MV  ,AR_MV  ,
     .                 NRBE3_MV,IRBE3_MV,IRBE3  ,LRBE3 ,FCDI_MV,
     .                 MCDI_MV ,DIAG_MVM3,R3M_MAX,NSPC_MV,ISPC_MV,
     .                 NRBE2_MV,IRBE2_MV,IRBE2   ,LRBE2  )
        ENDIF
C-----initialise diag_mv-----------
        ND = 0
        DO I = 1, NUMN_MV
          N = IN_MV(I)
          DO J = 1, MIN(3,NDOF(N))
            ID = ID_MV(J,I)
            IF (ID>0) DIAG_MV(J,I)=A_MV(J,N)
          ENDDO
        ENDDO
C
        DO I = 1, NRB_MV
          N = IRB_MV(1,I)
          DO J = 1, NDOF(N)
            ID = ID_MVM(J,I)
            IF (ID>0) THEN
              IF (J<=3) THEN
                DIAG_MVM(J,I)=A_MV(J,N)
              ELSE
                DIAG_MVM(J,I)=AR_MV(J-3,N)
              ENDIF
            ENDIF
          ENDDO
        ENDDO
C
        DO I=1,NI2_MV
          N=II2_MV(1,I)
          NI=II2_MV(2,I)
          JI=IPARI(1,N)
          NSN=IPARI(5,N)
          K10=JI-1
          K11=K10+4*IPARI(3,N)
C------IRECT(4,NSN)-----
          K12=K11+4*IPARI(4,N)
C------NSV(NSN)--node number---
          K13=K12+NSN
C------MSR(NMN)-----
          K14=K13+IPARI(6,N)
          L=INTBUF_TAB(N)%IRTLM(NI)
          NL=4*(L-1)
          IF (INTBUF_TAB(N)%IRECTM(NL+3)==INTBUF_TAB(N)%IRECTM(NL+4)) THEN
            NNOD=3
          ELSE
            NNOD=4
          ENDIF
C-------si noeud main est dependant aussi-----
          DO M=1,NNOD
            NJ=INTBUF_TAB(N)%IRECTM(NL+M)
            DO J = 1, NDOF(NJ)
              ID = ID_MVM2(J,M,I)
              IF (ID>0) THEN
                IF (J<=3) THEN
                  DIAG_MVM2(J,M,I)=A_MV(J,NJ)
                ELSE
                  DIAG_MVM2(J,M,I)=AR_MV(J-3,NJ)
                ENDIF
              ENDIF
            ENDDO
          ENDDO
        ENDDO
C-------RBE3-----------
        DO I=1,NRBE3_MV
          N=IRBE3_MV(I)
          NNOD=IRBE3(5,N)
          IAD=IRBE3(1,N)
C-------
          DO M=1,NNOD
            NJ=LRBE3(IAD+M)
            DO J = 1 , NDOF(NJ)
              ID = ID_MVM3(J,M,I)
              IF (ID>0) THEN
                IF (J<=3) THEN
                  DIAG_MVM3(J,M,I)=A_MV(J,NJ)
                ELSE
                  DIAG_MVM3(J,M,I)=AR_MV(J-3,NJ)
                ENDIF
              ENDIF
            ENDDO
          ENDDO
        ENDDO
C--------RBE2----------
        DO I = 1, NRBE2_MV
          N = IRBE2_MV(1,I)
          M = IRBE2(3,N)
          DO J = 1, NDOF(M)
            ID = ID_MVM4(J,I)
            IF (ID>0) THEN
              IF (J<=3) THEN
                DIAG_MVM4(J,I)=A_MV(J,M)
              ELSE
                DIAG_MVM4(J,I)=AR_MV(J-3,M)
              ENDIF
            ENDIF
          ENDDO
        ENDDO
C
        RETURN
      END
Chd|====================================================================
Chd|  ID_MVINI                      source/airbag/monv_imp0.F
Chd|-- called by -----------
Chd|        MONV_M3                       source/airbag/monv_imp0.F
Chd|-- calls ---------------
Chd|        RBE3CL                        source/constraints/general/rbe3/rbe3f.F
Chd|        IMP_MONV                      share/modules/impbufdef_mod.F
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE ID_MVINI(IPARI  ,INTBUF_TAB,NDOF   ,IDDL    ,IKC    ,
     1                    INLOC  ,X      ,SKEW   ,IRBE3   ,LRBE3  ,
     2                    FRBE3  ,IRBE2  ,LRBE2  )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
        USE IMP_MONV
        USE INTBUFDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "tabsiz_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER IPARI(NPARI,*), NDOF(*),IDDL(*),IKC(*),
     .          INLOC(*),IRBE3(NRBE3L,*),LRBE3(*),
     .          IRBE2(NRBE2L,*),LRBE2(*)
C     REAL
        my_real
     .     X(3,*),SKEW(*) ,FRBE3(*)

        TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER I,J,IDDLM(NUMNOD),NKC,N,ND,ID,NND,IROT,NMT,IAD,IADS
        INTEGER M,NSN,JI,K10,K11,K12,K13,K14,L,NNOD,NJ,NL,NI
C-----initialise iddl_mv-----------
        NKC = 0
        DO N =1,NUMNOD
          I=INLOC(N)
          IDDLM(I)=IDDL(I)-NKC
          DO J=1,NDOF(I)
            ND = IDDL(I)+J
            IF (IKC(ND)/=0) NKC = NKC + 1
          ENDDO
        ENDDO
C
        DO I = 1, NUMN_MV
          N = IN_MV(I)
          IF (NDOF(N)==0) THEN
            DO J = 1 , 3
              ID_MV(J,I) = -7
            ENDDO
          ELSE
            ND = 0
            DO J = 1 , MIN(3,NDOF(N))
              ID = IDDL(N) + J
              IF (IKC(ID)<1) THEN
                ND = ND + 1
                ID_MV(J,I) = IDDLM(N) + ND
              ELSE
                ID_MV(J,I) = -IKC(ID)
              ENDIF
            ENDDO
          ENDIF
        ENDDO
C
        DO I = 1, NRB_MV
          N = IRB_MV(1,I)
          ND = 0
          DO J = 1 , NDOF(N)
            ID = IDDL(N) + J
            IF (IKC(ID)<1) THEN
              ND = ND + 1
              ID_MVM(J,I) = IDDLM(N) + ND
            ELSE
              ID_MVM(J,I) = -IKC(ID)
            ENDIF
          ENDDO
        ENDDO
C
        DO I=1,NI2_MV
          N=II2_MV(1,I)
          NI=II2_MV(2,I)
          JI=IPARI(1,N)
          NSN=IPARI(5,N)
          K10=JI-1
          K11=K10+4*IPARI(3,N)
C------IRECT(4,NSN)-----
          K12=K11+4*IPARI(4,N)
C------NSV(NSN)--node number---
          K13=K12+NSN
C------MSR(NMN)-----
          K14=K13+IPARI(6,N)
          L=INTBUF_TAB(N)%IRTLM(NI)
          NL=4*(L-1)
          IF (INTBUF_TAB(N)%IRECTM(NL+3)==INTBUF_TAB(N)%IRECTM(NL+4)) THEN
            NNOD=3
          ELSE
            NNOD=4
          ENDIF
C-------si noeud main est dependant aussi-----
          DO M=1,NNOD
            NJ=INTBUF_TAB(N)%IRECTM(NL+M)
            ND = 0
            DO J = 1 , NDOF(NJ)
              ID = IDDL(NJ) + J
              IF (IKC(ID)<1) THEN
                ND = ND + 1
                ID_MVM2(J,M,I) = IDDLM(NJ) + ND
              ELSE
                ID_MVM2(J,M,I) = -IKC(ID)
              ENDIF
            ENDDO
          ENDDO
        ENDDO
C-------RBE3-----------
        IF (NRBE3_MV>0) THEN
          DO I=1,NRBE3_MV
            N=IRBE3_MV(I)
            NNOD=IRBE3(5,N)
            IAD=IRBE3(1,N)
C-------
            DO M=1,NNOD
              NJ=LRBE3(IAD+M)
              ND = 0
              DO J = 1 , NDOF(NJ)
                ID = IDDL(NJ) + J
                IF (IKC(ID)<1) THEN
                  ND = ND + 1
                  ID_MVM3(J,M,I) = IDDLM(NJ) + ND
                ELSE
                  ID_MVM3(J,M,I) = -IKC(ID)
                ENDIF
              ENDDO
            ENDDO
          ENDDO
C------- init FCDI_MV,MCDI_MV
          NMT = SLRBE3/2
          IADS =1
          DO I=1,NRBE3_MV
            N=IRBE3_MV(I)
            NI=IRBE3(3,N)
            NNOD=IRBE3(5,N)
            IAD=IRBE3(1,N)
            IROT=IRBE3(6,N)
            CALL RBE3CL(LRBE3(IAD+1),LRBE3(NMT+IAD+1),NI     ,X    ,
     .                  FRBE3(IAD+1),SKEW    ,NNOD   ,IROT   ,
     .                  FCDI_MV(IADS),MCDI_MV(IADS) ,IRBE3(2,N) )
C-------
            IADS = IADS + NNOD
          ENDDO
        ENDIF
C---------RBE2------------
        DO I = 1, NRBE2_MV
          N = IRBE2_MV(1,I)
          M = IRBE2(3,N)
          ND = 0
          DO J = 1 , NDOF(M)
            ID = IDDL(M) + J
            IF (IKC(ID)<1) THEN
              ND = ND + 1
              ID_MVM4(J,I) = IDDLM(M) + ND
            ELSE
              ID_MVM4(J,I) = -IKC(ID)
            ENDIF
          ENDDO
        ENDDO
C
        RETURN
      END
Chd|====================================================================
Chd|  MONV_KD                       source/airbag/monv_imp0.F
Chd|-- called by -----------
Chd|        MONV_M3                       source/airbag/monv_imp0.F
Chd|-- calls ---------------
Chd|        MONV_KEDI                     source/airbag/monv_imp0.F
Chd|        MONV_KEDJ                     source/airbag/monv_imp0.F
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|====================================================================
      SUBROUTINE MONV_KD(MONVOL ,VOLMON  ,X      ,IGRSURF ,
     1                   NMONV  ,IMONV   ,K_DIAG ,NNMAX_MV)
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
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER NMONV,IMONV(*),MONVOL(*),
     .          NNMAX_MV
C     REAL
        my_real
     .     X(3,*), VOLMON(*)  ,K_DIAG(3,*)
        TYPE(SURF_)   ,DIMENSION(NSURF)   :: IGRSURF
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER I, NTYP,NN,K1,IAD,IS,J,N,NMV,
     .          N1,N2,N3,N4,KK1,J1,ID,ID1,M1,M2,M3,M4
        my_real
     .     VOL(NMONV),NOR(3,NNMAX_MV),DVD1(3),DVD2(3),GAMAV(NMONV)
        my_real
     .     XX,YY,ZZ,X12,Y12,Z12,X13,Y13,Z13,X24,Y24,Z24,V,
     .     GAMAV2
C-----IDDL(3,*) est apres upd_k-,si dependant-:comme interface remote---------
C-----------reprend vol,gama------
        NMV = 0
        K1 = 1
        KK1 = 0
        DO I=1,NVOLU
          IF(IMONV(I)>0) THEN
            NMV = NMV+1
C-----------VOL-VINC---------
            VOL(NMV) = VOLMON(KK1+16)- VOLMON(KK1+5)
            GAMAV(NMV) = (VOLMON(KK1+1)-ONE)*VOLMON(KK1+13) /VOL(NMV)
          ENDIF
          K1 = K1 + NIMV
          KK1 = KK1 + NRVOLU
        ENDDO
C-----------DIAG_K------
        NMV = 0
        K1 = 1
        DO I=1,NVOLU
          IF(IMONV(I)>0) THEN
            IS   = MONVOL(K1+3)
            NN = IGRSURF(IS)%NSEG
            NMV = NMV+1
            DO J=1,NN
              N1 = IGRSURF(IS)%NODES(J,1)
              N2 = IGRSURF(IS)%NODES(J,2)
              N3 = IGRSURF(IS)%NODES(J,3)
              N4 = IGRSURF(IS)%NODES(J,4)
              X13=X(1,N3)-X(1,N1)
              Y13=X(2,N3)-X(2,N1)
              Z13=X(3,N3)-X(3,N1)
              X24=X(1,N4)-X(1,N2)
              Y24=X(2,N4)-X(2,N2)
              Z24=X(3,N4)-X(3,N2)
              NOR(1,J)=HALF*(Y13*Z24-Y24*Z13)
              NOR(2,J)=HALF*(Z13*X24-Z24*X13)
              NOR(3,J)=HALF*(X13*Y24-X24*Y13)
            ENDDO
            DO J=1,NN
              N1 = IGRSURF(IS)%NODES(J,1)
              N2 = IGRSURF(IS)%NODES(J,2)
              N3 = IGRSURF(IS)%NODES(J,3)
              N4 = IGRSURF(IS)%NODES(J,4)
              XX=HALF*(X(1,N1)+X(1,N2))
              YY=HALF*(X(2,N1)+X(2,N2))
              ZZ=HALF*(X(3,N1)+X(3,N2))
              X13=X(1,N3)-X(1,N1)
              Y13=X(2,N3)-X(2,N1)
              Z13=X(3,N3)-X(3,N1)
              X24=X(1,N4)-X(1,N2)
              Y24=X(2,N4)-X(2,N2)
              Z24=X(3,N4)-X(3,N2)
              GAMAV2=GAMAV(NMV)/VOL(NMV)
C-----------K-elememtaire J1--Kij(i dans ele J;j dans ele J1 ---)
              CALL MONV_KEDI(N1     ,N2      ,N3     ,N4     ,XX      ,
     1                       YY     ,ZZ      ,X13    ,Y13    ,Z13     ,
     2                       X24    ,Y24     ,Z24   ,NOR(1,J),VOL(NMV),
     3                       GAMAV2 ,DVD1    ,DVD2   ,K_DIAG)
C-----------K-elememtaire J1--Kij(i dans ele J;j dans ele J1 ---)
              DO J1=1,NN
                IF (J1/=J) THEN
                  M1 = IGRSURF(IS)%NODES(J1,1)
                  M2 = IGRSURF(IS)%NODES(J1,2)
                  M3 = IGRSURF(IS)%NODES(J1,3)
                  M4 = IGRSURF(IS)%NODES(J1,4)
                  IF (M1==N1.OR.M1==N2.OR.M1==N3.OR.M1==N4
     1            .OR.M2==N1.OR.M2==N2.OR.M2==N3.OR.M2==N4
     2            .OR.M3==N1.OR.M3==N2.OR.M3==N3.OR.M3==N4
     3            .OR.M4==N1.OR.M4==N2.OR.M4==N3.OR.M4==N4) THEN
                    CALL MONV_KEDJ(N1     ,N2      ,N3     ,N4     ,M1      ,
     1                             M2     ,M3      ,M4   ,NOR(1,J),NOR(1,J1),
     2                             VOL(NMV),GAMAV2 ,DVD1   ,DVD2   ,K_DIAG  )
                  ENDIF
                ENDIF
              ENDDO
            ENDDO
          ENDIF
          K1 = K1 + NIMV
        ENDDO
C
        RETURN
      END
Chd|====================================================================
Chd|  MONV_KEDI                     source/airbag/monv_imp0.F
Chd|-- called by -----------
Chd|        MONV_KD                       source/airbag/monv_imp0.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE MONV_KEDI(N1     ,N2      ,N3     ,N4     ,XX      ,
     1                     YY     ,ZZ      ,X13    ,Y13    ,Z13     ,
     2                     X24    ,Y24     ,Z24    ,N      ,VOL     ,
     3                     GAMAV2 ,DVD1    ,DVD2   ,K_DIAG )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER N1,N2,N3,N4
C     REAL
        my_real
     .     XX,YY,ZZ,X12,Y12,Z12,X13,Y13,Z13,X24,Y24,Z24,VOL,
     .     DVD1(*),DVD2(*),N(3),GAMAV2,K_DIAG(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER I, J,NNOD,ND,ID
        my_real
     .     DNDX1(3),DNDY1(3),DNDZ1(3),DNDX2(3),DNDY2(3),DNDZ2(3),
     .     KEV1(3),KEV2(3),KEVS(3),FAC,FACV(3)
C---------DNDX:NX,x,NY,x,NZ,x,------------------------------------
        ND = 3
        IF (N3==N4) THEN
          NNOD = 3
        ELSE
          NNOD = 4
        ENDIF
        DNDX1(1)=ZERO
        DNDX1(2)=Z24
        DNDX1(3)=-Y24
        DNDY1(1)=-Z24
        DNDY1(2)=ZERO
        DNDY1(3)=X24
        DNDZ1(1)=Y24
        DNDZ1(2)=-X24
        DNDZ1(3)=ZERO
C
        DNDX2(1)=ZERO
        DNDX2(2)=-Z13
        DNDX2(3)=Y13
        DNDY2(1)=Z13
        DNDY2(2)=ZERO
        DNDY2(3)=-X13
        DNDZ2(1)=-Y13
        DNDZ2(2)=X13
        DNDZ2(3)=ZERO
C
        DVD1(1) = DNDX1(1)*XX+DNDX1(2)*YY+DNDX1(3)*ZZ
        DVD1(2) = DNDY1(1)*XX+DNDY1(2)*YY+DNDY1(3)*ZZ
        DVD1(3) = DNDZ1(1)*XX+DNDZ1(2)*YY+DNDZ1(3)*ZZ
        DVD2(1) = DNDX2(1)*XX+DNDX2(2)*YY+DNDX2(3)*ZZ
        DVD2(2) = DNDY2(1)*XX+DNDY2(2)*YY+DNDY2(3)*ZZ
        DVD2(3) = DNDZ2(1)*XX+DNDZ2(2)*YY+DNDZ2(3)*ZZ
C---------terme n---zero----------------
        FAC = GAMAV2/NNOD
C---------terme v-------------------
        DO J=1,ND
          FACV(J) = FAC*N(J)
        ENDDO
        DO I=1,ND
C---------K11,K13=-K11-------------------
          KEV1(I) = FACV(I)*DVD1(I)
C---------K12,K22-------------------
          KEV2(I) = FACV(I)*DVD2(I)
C---------terme sup pour (xx,xj j=1,2)-------------------
          KEVS(I) = FACV(I)*N(I)
        ENDDO
C---------KE11-------------------
        DO I=1,ND
          K_DIAG(I,N1) = K_DIAG(I,N1)-KEV1(I)-KEVS(I)
        ENDDO
C---------KE33-------------------
        DO I=1,ND
          K_DIAG(I,N3) = K_DIAG(I,N3)+KEV1(I)
        ENDDO
C---------K22-------------------
        DO I=1,ND
          K_DIAG(I,N2) = K_DIAG(I,N2)-KEV2(I)-KEVS(I)
        ENDDO
C---------K44-------------------
        IF (NNOD==4) THEN
          DO I=1,ND
            K_DIAG(I,N4) = K_DIAG(I,N4)+KEV2(I)
          ENDDO
        ENDIF
C
        RETURN
      END
Chd|====================================================================
Chd|  MONV_KEDJ                     source/airbag/monv_imp0.F
Chd|-- called by -----------
Chd|        MONV_KD                       source/airbag/monv_imp0.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE MONV_KEDJ(N1     ,N2      ,N3     ,N4     ,M1      ,
     1                     M2     ,M3      ,M4     ,N      ,NJ      ,
     2                     VOL    ,GAMAV2  ,DVD1   ,DVD2   ,K_DIAG  )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER N1,N2,N3,N4,M1,M2,M3,M4
C     REAL
        my_real
     .     VOL,DVD1(*),DVD2(*),N(3),NJ(3),GAMAV2,K_DIAG(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER I, J,NNOD,NNOD1,ND,NM(4)
        my_real
     .     KEV1(3),KEV2(3),KEVS(3),FAC,FACV(3)
C---------DNDX:NX,x,NY,x,NZ,x,------------------------------------
        ND = 3
        NM(1) = M1
        NM(2) = M2
        NM(3) = M3
        IF (N3==N4) THEN
          NNOD = 3
        ELSE
          NNOD = 4
        ENDIF
        IF (M3==M4) THEN
          NNOD1 = 3
        ELSE
          NNOD1 = 4
          NM(4) = M4
        ENDIF
C---------terme v-------------------
        DO J=1,ND
          FACV(J) = GAMAV2*NJ(J)
        ENDDO
        DO I=1,ND
          KEV1(I) = FACV(I)*DVD1(I)
          KEV2(I) = FACV(I)*DVD2(I)
          KEVS(I) = FACV(I)*N(I)
        ENDDO
C---------KEIJ---J=N1,I=NM(j)----------------
        DO I=1,NNOD1
          IF (N1==NM(I)) THEN
            DO J=1,ND
              K_DIAG(J,N1) = K_DIAG(J,N1)-KEV1(J)-KEVS(J)
            ENDDO
          ELSEIF (N2==NM(I)) THEN
            DO J=1,ND
              K_DIAG(J,N2) = K_DIAG(J,N2)-KEV2(J)-KEVS(J)
            ENDDO
          ELSEIF (N3==NM(I)) THEN
            DO J=1,ND
              K_DIAG(J,N3) = K_DIAG(J,N3)+KEV1(J)
            ENDDO
          ELSEIF (NNOD==4.AND.N4==NM(I)) THEN
            DO J=1,ND
              K_DIAG(J,N4) = K_DIAG(J,N4)+KEV2(J)
            ENDDO
          ENDIF
        ENDDO
C
        RETURN
      END
Chd|====================================================================
Chd|  UPDK_MV                       source/airbag/monv_imp0.F
Chd|-- called by -----------
Chd|        MONV_M3                       source/airbag/monv_imp0.F
Chd|-- calls ---------------
Chd|        BCL_IMPKD                     source/constraints/general/bcs/bc_imp0.F
Chd|        FV_UPDKD                      source/constraints/general/impvel/fv_imp0.F
Chd|        FV_UPDKD2                     source/constraints/general/bcs/bc_imp0.F
Chd|        I2_FRUP0                      source/interfaces/interf/i2_imp1.F
Chd|        I2_FRUP1                      source/interfaces/interf/i2_imp1.F
Chd|        L_DIR                         source/constraints/general/bcs/bc_imp0.F
Chd|        PRERBE2FR                     source/constraints/general/rbe2/rbe2f.F
Chd|        PRERBE3FR                     source/constraints/general/rbe3/rbe3f.F
Chd|        RBE2_IMPKD                    source/constraints/general/rbe2/rbe2_imp0.F
Chd|        RBE3_FRUPD                    source/constraints/general/rbe3/rbe3_imp0.F
Chd|        IMP_ASPC                      share/modules/impbufdef_mod.F
Chd|        IMP_RWL                       share/modules/impbufdef_mod.F
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE UPDK_MV(NDOF    ,IPARI  ,INTBUF_TAB,NI2_MV ,
     .                   II2_MV  ,NRB_MV ,IRB_MV  ,NFX_MV,IFX_MV ,
     .                   NBC_MV  ,IBC_MV ,NRW_MV  ,IRW_MV,IBFV   ,
     .                   SKEW    ,XFRAME ,X       ,A     ,AR     ,
     .                   NRBE3_MV,IRBE3_MV,IRBE3  ,LRBE3 ,FCDI_MV,
     .                   MCDI_MV ,DIAG_M3 ,MAXR3  ,NSPC_MV,ISPC_MV,
     .                   NRBE2_MV,IRBE2_MV,IRBE2  ,LRBE2 )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
        USE IMP_RWL
        USE IMP_ASPC
        USE INTBUFDEF_MOD
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
        INTEGER   NRB_MV , NI2_MV ,NDOF(*),II2_MV(2,*),IRB_MV(2,*),
     .           IPARI(NPARI,*),NFX_MV,IFX_MV(2,*),
     .           NBC_MV,IBC_MV(3,*),IBFV(NIFV,*),NRW_MV,IRW_MV(*),
     .           NRBE3_MV,IRBE3_MV(*),IRBE3(NRBE3L,*),LRBE3(*),MAXR3,
     .           NSPC_MV,ISPC_MV(*),NRBE2_MV,IRBE2_MV(2,*),
     .           IRBE2(NRBE2L,*),LRBE2(*)
        my_real
     .    A(3,*),AR(3,*),X(3,*),SKEW(LSKEW,*),XFRAME(NXFRAME,*),
     .    FCDI_MV(*),MCDI_MV(*),DIAG_M3(6,MAXR3,*)

        TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
        INTEGER I,ID,N,J,NDD,IS,NS,ILEV,J10,J11,J12,J21
        INTEGER M,NSN,JI,L,NNOD,NJ,NL,NI,
     .          I1,J1,ISK,IFM,K1,K2,K3,ICT,NN,IROT,IAD,IADS,
     .          JT(3),JR(3),IR,IRAD,K,IC
        my_real
     .    XS,YS,ZS,XS2,YS2,ZS2,KSS(6),KJJ(6,4),KII(3,3),EJ(3),S,
     .    KDD(6,6),KMM(6)
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
C--------local secnd node-,d'abord independant----
        DO I=1,NI2_MV
          N=II2_MV(1,I)
          NI=II2_MV(2,I)
          JI=IPARI(1,N)
          NSN=IPARI(5,N)
          L=INTBUF_TAB(N)%IRTLM(NI)
          NL=4*(L-1)
          NS=INTBUF_TAB(N)%NSV(NI)
          ILEV  =IPARI(20,N)
          J10=IPARI(2,N)
          J11=J10+1
          J12=J11+NPARIR
          J21=J12+2*NSN
          DO J=1,3
            KSS(J) = A(J,NS)
            KSS(J+3) = ZERO
          ENDDO
          DO M=1,4
            DO J=1,6
              KJJ(J,M) = ZERO
            ENDDO
          ENDDO
          IF (ILEV==1) THEN
            CALL I2_FRUP1(X   ,INTBUF_TAB(N)%IRECTM,INTBUF_TAB(N)%DPARA,
     .                   INTBUF_TAB(N)%NSV ,
     1                   INTBUF_TAB(N)%IRTLM ,NS  ,KSS,KJJ )
          ELSE
            CALL I2_FRUP0(X   ,INTBUF_TAB(N)%IRECTM,INTBUF_TAB(N)%CSTS ,
     .               INTBUF_TAB(N)%NSV ,
     1               INTBUF_TAB(N)%IRTLM,NS,NDOF,KSS   ,KJJ   )
          ENDIF
          DO M=1,4
            NJ=INTBUF_TAB(N)%IRECTM(NL+M)
            DO J = 1 , 3
              A(J,NJ) = A(J,NJ) + KJJ(J,M)
            ENDDO
            IF (NDOF(NJ)>3) THEN
              DO J = 1 , 3
                AR(J,NJ) = AR(J,NJ) + KJJ(J+3,M)
              ENDDO
            ENDIF
          ENDDO
        ENDDO
C--------RBE2-----------
        DO I = 1, NRBE2_MV
          N = IRBE2_MV(1,I)
          M = IRBE2(3,N)
          NS = IRBE2_MV(2,I)
          ISK = IRBE2(7,N)
          IRAD =IRBE2(11,N)
          IC = IRBE2(4,N)
          IC =(IC/512)*512
          CALL PRERBE2FR(IC    ,JT  ,JR   )
          DO K=1,6
            DO J=1,6
              KDD(K,J) = ZERO
            ENDDO
            KMM(K)=  ZERO
            KSS(K)=  ZERO
          ENDDO
          DO J=1,3
            KDD(J,J) = A(J,NS)
          ENDDO
          IF (NDOF(NS)>3) THEN
            DO J = 1 , 3
              KDD(J+3,J+3) = AR(J,NS)
            ENDDO
          ENDIF
          CALL RBE2_IMPKD(M     ,NS    ,X     ,ISK    ,JT     ,
     2                   JR     ,NDOF  ,SKEW(1,ISK),KDD    ,KMM ,
     3                   KSS    ,IRAD  )
C--------for the moment, A(J,NS) will be not transfert
          DO J = 1 , 3
            A(J,M) = A(J,M) + KMM(J)
            A(J,NS) = A(J,NS) + KSS(J)
          ENDDO
          IF (NDOF(M)>3) THEN
            DO J = 1 , 3
              AR(J,M) = AR(J,M) + KMM(J+3)
            ENDDO
          ENDIF
          IF (NDOF(NS)>3) THEN
            DO J = 1 , 3
              AR(J,NS) = AR(J,NS) + KSS(J+3)
            ENDDO
          ENDIF
        ENDDO
C------RBE3---
        IADS=1
        DO I=1,NRBE3_MV
          N=IRBE3_MV(I)
          NS=IRBE3(3,N)
          NNOD=IRBE3(5,N)
          IROT=IRBE3(6,N)
          IAD=IRBE3(1,N)
C--------
          DO J=1,3
            KSS(J) = A(J,NS)
            KSS(J+3) = ZERO
          ENDDO
          CALL PRERBE3FR(IRBE3 ,N    ,JT  ,JR   )
          CALL RBE3_FRUPD(NNOD   ,LRBE3(IAD+1) ,FCDI_MV(IADS),
     1                    MCDI_MV(IADS),NDOF  ,JT   ,IROT  ,
     2                    KSS   ,DIAG_M3(1,1,I))
          IADS=IADS+NNOD
          DO M=1,NNOD
            NJ=LRBE3(IAD+M)
            DO J = 1 , 3
              A(J,NJ) = A(J,NJ) + DIAG_M3(J,M,I)
            ENDDO
            IF (IROT>0.AND.NDOF(NJ)>3) THEN
              DO J = 1 , 3
                AR(J,NJ) = AR(J,NJ) + DIAG_M3(J+3,M,I)
              ENDDO
            ENDIF
          ENDDO
        ENDDO
C
        DO I = 1, NRB_MV
          M = IRB_MV(1,I)
          N = IRB_MV(2,I)
          XS=X(1,N)-X(1,M)
          YS=X(2,N)-X(2,M)
          ZS=X(3,N)-X(3,M)
          DO J=1,3
            A(J,M) = A(J,M)+A(J,N)
          ENDDO
          XS2=XS*XS
          YS2=YS*YS
          ZS2=ZS*ZS
          AR(1,M) = AR(1,M)+A(2,N)*ZS2+A(3,N)*YS2
          AR(2,M) = AR(2,M)+A(1,N)*ZS2+A(3,N)*XS2
          AR(3,M) = AR(3,M)+A(1,N)*YS2+A(2,N)*XS2
        ENDDO
        DO I1 = 1,NBC_MV
          N = IBC_MV(1,I1)
          ISK= IBC_MV(2,I1)
          ICT= IBC_MV(3,I1)
          KII(1,1)=A(1,N)
          KII(2,2)=A(2,N)
          KII(3,3)=A(3,N)
          KII(1,2)=ZERO
          KII(1,3)=ZERO
          KII(2,3)=ZERO
          KII(2,1)=KII(1,2)
          KII(3,1)=KII(1,3)
          KII(3,2)=KII(2,3)
          CALL BCL_IMPKD(ICT  ,ISK   ,SKEW  ,KII   ,A(1,N) )
        ENDDO
        DO I1 = 1,NSPC_MV
          N = ISPC_MV(I1)
          I = IN_SPC(N)
          IR = 0
          IAD = 6*(N-1)+1
          NN = IC_SPC(N)
          IF (NN>3) THEN
            NN= NN-3
            IR = 1
          ENDIF
          IF (IR==0) THEN
            KII(1,1)=A(1,I)
            KII(2,2)=A(2,I)
            KII(3,3)=A(3,I)
            KII(1,2)=ZERO
            KII(1,3)=ZERO
            KII(2,3)=ZERO
            KII(2,1)=KII(1,2)
            KII(3,1)=KII(1,3)
            KII(3,2)=KII(2,3)
            IF (NN==1) THEN
              EJ(1)=SKEW_SPC(IAD)
              EJ(2)=SKEW_SPC(IAD+1)
              EJ(3)=SKEW_SPC(IAD+2)
              CALL L_DIR(EJ,J)
              CALL FV_UPDKD(EJ    ,J    ,KII   ,A(1,I))
            ELSEIF (NN==2) THEN
              CALL FV_UPDKD2(SKEW_SPC(IAD),SKEW_SPC(IAD+3),KII  ,A(1,I))
            END IF
          ELSE
            KII(1,1)=AR(1,I)
            KII(2,2)=AR(2,I)
            KII(3,3)=AR(3,I)
            KII(1,2)=ZERO
            KII(1,3)=ZERO
            KII(2,3)=ZERO
            KII(2,1)=KII(1,2)
            KII(3,1)=KII(1,3)
            KII(3,2)=KII(2,3)
            IF (NN==1) THEN
              EJ(1)=SKEW_SPC(IAD)
              EJ(2)=SKEW_SPC(IAD+1)
              EJ(3)=SKEW_SPC(IAD+2)
              CALL L_DIR(EJ,J)
              CALL FV_UPDKD(EJ    ,J    ,KII   ,AR(1,I))
            ELSEIF (NN==2) THEN
              CALL FV_UPDKD2(SKEW_SPC(IAD),SKEW_SPC(IAD+3),KII  ,AR(1,I))
            END IF
          ENDIF
        ENDDO
C
        DO I1 = 1,NFX_MV
          N = IFX_MV(1,I1)
          J1= IFX_MV(2,I1)
          I=IABS(IBFV(1,N))
          ISK=IBFV(2,N)/10
          IFM = IBFV(9,N)
          J=IBFV(2,N)
          IF (IFM<=1) J=J-10*ISK
          K1=3*J-2
          K2=3*J-1
          K3=3*J
          IF (ISK>1) THEN
            EJ(1)=SKEW(K1,ISK)
            EJ(2)=SKEW(K2,ISK)
            EJ(3)=SKEW(K3,ISK)
          ELSE
            EJ(1)=XFRAME(K1,IFM)
            EJ(2)=XFRAME(K2,IFM)
            EJ(3)=XFRAME(K3,IFM)
          ENDIF
          S = ONE/EJ(J1)
          DO NN =1,3
            EJ(NN) = EJ(NN)*S
          ENDDO
          KII(1,1)=A(1,I)
          KII(2,2)=A(2,I)
          KII(3,3)=A(3,I)
          KII(1,2)=ZERO
          KII(1,3)=ZERO
          KII(2,3)=ZERO
          KII(2,1)=KII(1,2)
          KII(3,1)=KII(1,3)
          KII(3,2)=KII(2,3)
          CALL FV_UPDKD(EJ    ,J1    ,KII   ,A(1,I))
        ENDDO
        DO I1 = 1,NRW_MV
          N = IRW_MV(I1)
          I = IN_RWL(N)
          KII(1,1)=A(1,I)
          KII(2,2)=A(2,I)
          KII(3,3)=A(3,I)
          KII(1,2)=ZERO
          KII(1,3)=ZERO
          KII(2,3)=ZERO
          KII(2,1)=KII(1,2)
          KII(3,1)=KII(1,3)
          KII(3,2)=KII(2,3)
          EJ(1)=NOR_RWL(1,N)
          EJ(2)=NOR_RWL(2,N)
          EJ(3)=NOR_RWL(3,N)
          CALL L_DIR(EJ,J1)
          CALL FV_UPDKD(EJ    ,J1    ,KII   ,A(1,I))
        ENDDO
C
        RETURN
      END
Chd|====================================================================
Chd|  MONV_DIAG                     source/airbag/monv_imp0.F
Chd|-- called by -----------
Chd|        LIN_SOLVH1                    source/implicit/lin_solv.F
Chd|        LIN_SOLVIH2                   source/implicit/lin_solv.F
Chd|-- calls ---------------
Chd|        IMP_MONV                      share/modules/impbufdef_mod.F
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE MONV_DIAG(DIAG_K,NDOF,IPARI,INTBUF_TAB,IRBE3,LRBE3,
     .                     IRBE2  ,IFLAG )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
        USE IMP_MONV
        USE INTBUFDEF_MOD
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
        INTEGER IPARI(NPARI,*), NDOF(*),IFLAG,
     .          IRBE3(NRBE3L,*),LRBE3(*),IRBE2(NRBE2L,*)
C     REAL
        my_real
     .     DIAG_K(*)

        TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER I,J,NKC,N,ND,ID,IAD
        INTEGER M,NSN,JI,K10,K11,K12,K13,K14,L,NNOD,NJ,NL,NI
C---------IFLAG=0: add; IFLAG=1:remove-------
        IF (IFLAG==0) THEN
          DO I = 1, NUMN_MV
            N = IN_MV(I)
            DO J = 1, MIN(3,NDOF(N))
              ID = ID_MV(J,I)
              IF (ID>0) DIAG_K(ID)=DIAG_K(ID)+DIAG_MV(J,I)
            ENDDO
          ENDDO
C
          DO I = 1, NRB_MV
            N = IRB_MV(1,I)
            DO J = 1, NDOF(N)
              ID = ID_MVM(J,I)
              IF (ID>0) DIAG_K(ID)=DIAG_K(ID)+DIAG_MVM(J,I)
            ENDDO
          ENDDO
C
          DO I=1,NI2_MV
            N=II2_MV(1,I)
            NI=II2_MV(2,I)
            JI=IPARI(1,N)
            NSN=IPARI(5,N)
            K10=JI-1
            K11=K10+4*IPARI(3,N)
C------IRECT(4,NSN)-----
            K12=K11+4*IPARI(4,N)
C------NSV(NSN)--node number---
            K13=K12+NSN
C------MSR(NMN)-----
            K14=K13+IPARI(6,N)
            L=INTBUF_TAB(N)%IRTLM(NI)
            NL=4*(L-1)
            IF (INTBUF_TAB(N)%IRECTM(NL+3)==INTBUF_TAB(N)%IRECTM(NL+4)) THEN
              NNOD=3
            ELSE
              NNOD=4
            ENDIF
C-------si noeud main est dependant aussi-----
            DO M=1,NNOD
              NJ=INTBUF_TAB(N)%IRECTM(NL+M)
              DO J = 1, NDOF(NJ)
                ID = ID_MVM2(J,M,I)
                IF (ID>0) DIAG_K(ID)=DIAG_K(ID)+DIAG_MVM2(J,M,I)
              ENDDO
            ENDDO
          ENDDO
C---------RBE3--------------
          DO I=1,NRBE3_MV
            N=IRBE3_MV(I)
            NNOD=IRBE3(5,N)
            IAD=IRBE3(1,N)
C--------
            DO M=1,NNOD
              NJ=LRBE3(IAD+M)
              DO J = 1, NDOF(NJ)
                ID = ID_MVM3(J,M,I)
                IF (ID>0) DIAG_K(ID)=DIAG_K(ID)+DIAG_MVM3(J,M,I)
              ENDDO
            ENDDO
          ENDDO
C----------RBE2---maybe the order is important---------
          DO I = 1, NRBE2_MV
            N = IRBE2_MV(1,I)
            M = IRBE2(3,N)
            DO J = 1, NDOF(M)
              ID = ID_MVM(J,I)
              IF (ID>0) DIAG_K(ID)=DIAG_K(ID)+DIAG_MVM4(J,I)
            ENDDO
          ENDDO
C---------on enleve-----
        ELSE
          DO I = 1, NUMN_MV
            N = IN_MV(I)
            DO J = 1, MIN(3,NDOF(N))
              ID = ID_MV(J,I)
              IF (ID>0) DIAG_K(ID)=DIAG_K(ID)-DIAG_MV(J,I)
            ENDDO
          ENDDO
C
          DO I = 1, NRB_MV
            N = IRB_MV(1,I)
            DO J = 1, NDOF(N)
              ID = ID_MVM(J,I)
              IF (ID>0) DIAG_K(ID)=DIAG_K(ID)-DIAG_MVM(J,I)
            ENDDO
          ENDDO
C
          DO I=1,NI2_MV
            N=II2_MV(1,I)
            NI=II2_MV(2,I)
            JI=IPARI(1,N)
            NSN=IPARI(5,N)
            K10=JI-1
            K11=K10+4*IPARI(3,N)
C------IRECT(4,NSN)-----
            K12=K11+4*IPARI(4,N)
C------NSV(NSN)--node number---
            K13=K12+NSN
C------MSR(NMN)-----
            K14=K13+IPARI(6,N)
            L=INTBUF_TAB(N)%IRTLM(NI)
            NL=4*(L-1)
            IF (INTBUF_TAB(N)%IRECTM(NL+3)==INTBUF_TAB(N)%IRECTM(NL+4)) THEN
              NNOD=3
            ELSE
              NNOD=4
            ENDIF
C-------si noeud main est dependant aussi-----
            DO M=1,NNOD
              NJ=INTBUF_TAB(N)%IRECTM(NL+M)
              DO J = 1, NDOF(NJ)
                ID = ID_MVM2(J,M,I)
                IF (ID>0) DIAG_K(ID)=DIAG_K(ID)-DIAG_MVM2(J,M,I)
              ENDDO
            ENDDO
          ENDDO
C---------RBE3--------------
          DO I=1,NRBE3_MV
            N=IRBE3_MV(I)
            NNOD=IRBE3(5,N)
            IAD=IRBE3(1,N)
C--------
            DO M=1,NNOD
              NJ=LRBE3(IAD+M)
              DO J = 1, NDOF(NJ)
                ID = ID_MVM3(J,M,I)
                IF (ID>0) DIAG_K(ID)=DIAG_K(ID)-DIAG_MVM3(J,M,I)
              ENDDO
            ENDDO
          ENDDO
C----------RBE2------------
          DO I = 1, NRBE2_MV
            N = IRBE2_MV(1,I)
            M = IRBE2(3,N)
            DO J = 1, NDOF(M)
              ID = ID_MVM(J,I)
              IF (ID>0) DIAG_K(ID)=DIAG_K(ID)-DIAG_MVM4(J,I)
            ENDDO
          ENDDO
        ENDIF
C
        RETURN
      END
Chd|====================================================================
Chd|  MV_MATV                       source/airbag/monv_imp0.F
Chd|-- called by -----------
Chd|        MAV_LT2                       source/implicit/produt_v.F
Chd|        MAV_LTH                       source/implicit/produt_v.F
Chd|        MAV_LTH0                      source/implicit/produt_v.F
Chd|        MAV_LTP                       source/implicit/produt_v.F
Chd|-- calls ---------------
Chd|        IMP3_A2B                      source/airbag/monv_imp0.F
Chd|        IMP3_U2X                      source/airbag/monv_imp0.F
Chd|        IMP_PVGA                      source/airbag/monv_imp0.F
Chd|        SPMD_FR_POFF                  source/mpi/kinematic_conditions/spmd_fr_poff.F
Chd|        ZEROR                         source/system/zero.F
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        IMP_MONV                      share/modules/impbufdef_mod.F
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE MV_MATV(MONVOL ,VOLMON  ,X      ,IGRSURF,
     1                   FR_MV  ,NMONV  ,IMONV   ,U      ,F      ,
     2                   NDOF   ,IPARI  ,INTBUF_TAB,A      ,
     3                   AR     ,X_IMP  ,IBFV    ,SKEW   ,XFRAME ,
     4                   IRBE3  ,LRBE3  ,IRBE2   ,LRBE2  )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
        USE IMP_MONV
        USE INTBUFDEF_MOD
        USE GROUPDEF_MOD
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
#include      "scr05_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER NMONV,IMONV(*),MONVOL(*),
     .          IPARI(*)  ,NDOF(*),FR_MV(NSPMD+2,*),
     .          IBFV(*),IRBE3(*)  ,LRBE3(*),IRBE2(*)  ,LRBE2(*)
        my_real
     .     X(3,*),A(3,*),AR(3,*), VOLMON(*)  ,F(*), U(*),
     .     X_IMP(3,*),SKEW(*) ,XFRAME(*)

        TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
        TYPE (SURF_)   , DIMENSION(NSURF)   :: IGRSURF
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER I, J,N,K,K1,KK1,N1,N2,N3,N4,ID,IAD,IS,NN,NTY
        my_real
     .     TEMP,VOL ,NOR(3,NNMAX_MV),
     .     XX,YY,ZZ,X12,Y12,Z12,X13,Y13,Z13,X24,Y24,Z24,
     .     FNI(3),DPI,DPRES
C----------------actualise X---------------------
        CALL IMP3_U2X(X     ,IPARI  ,INTBUF_TAB,NDOF  ,
     .                U     ,A      ,AR     ,X_IMP ,NUMN_MV,
     .                IN_MV ,ID_MV  ,NRB_MV ,IRB_MV,ID_MVM ,
     .                NI2_MV,II2_MV ,ID_MVM2,NFX_MV,IFX_MV ,
     .                NBC_MV,IBC_MV ,NRW_MV,IRW_MV ,IBFV   ,
     .                SKEW  ,XFRAME ,IRBE3 ,LRBE3  ,NRBE3_MV,
     .                IRBE3_MV,ID_MVM3,R3M_MAX,FCDI_MV,MCDI_MV,
     .                NSPC_MV,ISPC_MV,IRBE2 ,LRBE2 ,NRBE2_MV,
     .                IRBE2_MV,ID_MVM4)
        CALL ZEROR(A,NUMNOD)
        IF ((NRB_MV+NI2_MV+NRBE3_MV+NSPC_MV+NRBE2_MV)>0)
     .     CALL ZEROR(AR,NUMNOD)
C-----------calcul VOL,PRESS------
        K1 = 1
        KK1 = 1
        DO I=1,NVOLU
          IF(IMONV(I)>0) THEN
            IS   = MONVOL(K1+3)
            NN = IGRSURF(IS)%NSEG
            VOL = 0
            DO J=1,NN
              N1 = IGRSURF(IS)%NODES(J,1)
              N2 = IGRSURF(IS)%NODES(J,2)
              N3 = IGRSURF(IS)%NODES(J,3)
              N4 = IGRSURF(IS)%NODES(J,4)
              XX=HALF*(X_IMP(1,N1)+X_IMP(1,N2))
              YY=HALF*(X_IMP(2,N1)+X_IMP(2,N2))
              ZZ=HALF*(X_IMP(3,N1)+X_IMP(3,N2))
              X13=X_IMP(1,N3)-X_IMP(1,N1)
              Y13=X_IMP(2,N3)-X_IMP(2,N1)
              Z13=X_IMP(3,N3)-X_IMP(3,N1)
              X24=X_IMP(1,N4)-X_IMP(1,N2)
              Y24=X_IMP(2,N4)-X_IMP(2,N2)
              Z24=X_IMP(3,N4)-X_IMP(3,N2)
              NOR(1,J)=HALF*(Y13*Z24-Y24*Z13)
              NOR(2,J)=HALF*(Z13*X24-Z24*X13)
              NOR(3,J)=HALF*(X13*Y24-X24*Y13)
              VOL= VOL+THIRD*(NOR(1,J)*XX+NOR(2,J)*YY+NOR(3,J)*ZZ)
            ENDDO
            IF(IMACH==3) THEN
cow51g8+1
              IF (NSPMD > 1) THEN
                TEMP = VOL
                CALL SPMD_FR_POFF(FR_MV(1,I),TEMP,1)
                VOL = TEMP
cow51g8+1
              ENDIF
            ENDIF
            CALL IMP_PVGA(MONVOL(K1),VOLMON(KK1),VOL ,DPRES)
C-----------noeud independant -> W------
            IF (DPRES/=ZERO) THEN
              DO J=1,NN
                N1 = IGRSURF(IS)%NODES(J,1)
                N2 = IGRSURF(IS)%NODES(J,2)
                N3 = IGRSURF(IS)%NODES(J,3)
                N4 = IGRSURF(IS)%NODES(J,4)
                NTY = IGRSURF(IS)%ELTYP(J)
                IF (NTY==7) THEN
                  DPI = DPRES*THIRD
                  DO K = 1,3
                    FNI(K)=DPI*NOR(K,J)
                  ENDDO
                  DO K = 1,3
                    A(K,N1) = A(K,N1)+FNI(K)
                    A(K,N2) = A(K,N2)+FNI(K)
                    A(K,N3) = A(K,N3)+FNI(K)
                  ENDDO
                ELSE
                  DPI = DPRES*FOURTH
                  DO K = 1,3
                    FNI(K)=DPI*NOR(K,J)
                  ENDDO
                  DO K = 1,3
                    A(K,N1) = A(K,N1)+FNI(K)
                    A(K,N2) = A(K,N2)+FNI(K)
                    A(K,N3) = A(K,N3)+FNI(K)
                    A(K,N4) = A(K,N4)+FNI(K)
                  ENDDO
                ENDIF
              ENDDO
            ENDIF
          ENDIF
          K1 = K1 + NIMV
          KK1 = KK1 + NRVOLU
        ENDDO
        CALL IMP3_A2B(IPARI  ,INTBUF_TAB,NDOF  ,X_IMP  ,
     .                A      ,AR     ,NUMN_MV,IN_MV,ID_MV  ,
     .                NRB_MV ,IRB_MV ,ID_MVM ,NI2_MV,II2_MV,
     .                ID_MVM2,NFX_MV ,IFX_MV ,NBC_MV,IBC_MV,
     .                NRW_MV ,IRW_MV ,IBFV   ,SKEW  ,XFRAME,
     .                F      ,IRBE3  ,LRBE3  ,NRBE3_MV,IRBE3_MV,
     .                ID_MVM3,R3M_MAX,FCDI_MV,MCDI_MV ,NSPC_MV,
     .                ISPC_MV,IRBE2 ,LRBE2 ,NRBE2_MV,IRBE2_MV,
     .                ID_MVM4)
        RETURN
      END
Chd|====================================================================
Chd|  IMP3_U2X                      source/airbag/monv_imp0.F
Chd|-- called by -----------
Chd|        INT_MATV                      source/implicit/imp_int_k.F
Chd|        INT_MATVP                     source/implicit/imp_int_k.F
Chd|        MV_MATV                       source/airbag/monv_imp0.F
Chd|-- calls ---------------
Chd|        BCL_IMPD                      source/constraints/general/bcs/bc_imp0.F
Chd|        BC_UPD2D                      source/constraints/general/bcs/bc_imp0.F
Chd|        BC_UPDD                       source/constraints/general/bcs/bc_imp0.F
Chd|        FV_IMPD                       source/constraints/general/impvel/fv_imp0.F
Chd|        I2_FRRD0                      source/interfaces/interf/i2_imp2.F
Chd|        I2_FRRD1                      source/interfaces/interf/i2_imp2.F
Chd|        L_DIR                         source/constraints/general/bcs/bc_imp0.F
Chd|        PRERBE2FR                     source/constraints/general/rbe2/rbe2f.F
Chd|        PRERBE3FR                     source/constraints/general/rbe3/rbe3f.F
Chd|        RBE2_FRD                      source/constraints/general/rbe2/rbe2v.F
Chd|        RBE3_FRD                      source/constraints/general/rbe3/rbe3v.F
Chd|        IMP_ASPC                      share/modules/impbufdef_mod.F
Chd|        IMP_RWL                       share/modules/impbufdef_mod.F
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE IMP3_U2X(X     ,IPARI  ,INTBUF_TAB,NDOF  ,
     .                   LX     ,A      ,AR     ,X_IMP ,NUMN  ,
     .                   INL    ,IDDL   ,NRB    ,IRB   ,IDDLM ,
     .                   NI2    ,II2    ,IDDLM2 ,NFX   ,IFX   ,
     .                   NBC    ,IBC    ,NRW    ,IRW   ,IBFV  ,
     .                   SKEW   ,XFRAME ,IRBE3  ,LRBE3 ,NR3   ,
     .                   IR3    ,IDDLM3 ,R3_MAX ,FCDI  ,MCDI  ,
     .                   NSPC   ,ISPC   ,IRBE2  ,LRBE2 ,NR2   ,
     .                   IR2    ,IDDLM4)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
        USE IMP_RWL
        USE IMP_ASPC
        USE INTBUFDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER R3_MAX
        INTEGER NUMN,INL(*),NRB,IRB(2,*) ,NI2,II2(2,*),
     .          IDDL(3,*),IDDLM(6,*),IDDLM2(6,4,*),
     .          IPARI(NPARI,*), NDOF(*),NFX ,IFX(2,*),
     .          NBC ,IBC(3,*),NRW ,IRW(*),IBFV(NIFV,*),
     .          NR3,IR3(*),IDDLM3(6,R3_MAX,*),IRBE3(NRBE3L,*),LRBE3(*),
     .          NR2,IR2(2,*),IDDLM4(6,*),IRBE2(NRBE2L,*),LRBE2(*),
     .          NSPC ,ISPC(*)
        my_real
     .    X(3,*) ,LX(*),A(3,*),AR(3,*),X_IMP(3,*),
     .    SKEW(LSKEW,*)  ,XFRAME(*),FCDI(*)  ,MCDI(*)

        TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER I,J,N,M,NS,NI,NSN,ILEV,JT(3),JR(3),IADS,IAD,
     .          NN,L,NNOD,NJ,ND,NL,ISK,IFM,LJFR(NFXVEL),IROT,IRAD,IC
        my_real
     .    XS,YS,ZS,EJ(3)
C------utilise A,AR pour actualise U------
        DO I = 1, NUMN
          N = INL(I)
          DO J = 1, 3
            ND = IDDL(J,I)
            IF (ND>0) THEN
              A(J,N) = LX(ND)
            ELSE
              A(J,N) = ZERO
            ENDIF
          ENDDO
        ENDDO
C------si il y a rb hierachic
C------BCS-----------
        DO L=NBC,1,-1
          I = IBC(1,L)
          ISK =IBC(2,L)
          IFM =IBC(3,L)
          CALL BCL_IMPD(IFM  ,ISK   ,SKEW  ,I     ,A     )
        ENDDO
C
        IF (NFX>0) THEN
          DO N=1,NFXVEL
            LJFR(N)=0
          ENDDO
          DO L=1,NFX
            I = IFX(1,L)
            LJFR(I) = IFX(2,L)
          ENDDO
          CALL FV_IMPD(IBFV  ,LJFR  ,SKEW  ,XFRAME,A     ,
     1                 AR    )
        ENDIF
        DO L=NSPC,1,-1
          N = ISPC(L)
          I = IN_SPC(N)
          IROT = 0
          IAD = 6*(N-1)+1
          NN = IC_SPC(N)
          IF (NN>3) THEN
            NN= NN-3
            IROT = 1
          ENDIF
          IF (NN==1) THEN
            EJ(1)=SKEW_SPC(IAD)
            EJ(2)=SKEW_SPC(IAD+1)
            EJ(3)=SKEW_SPC(IAD+2)
            CALL L_DIR(EJ,J)
          END IF
          IF (IROT==0) THEN
            IF (NN==1) THEN
              A(J,I) = ZERO
              CALL BC_UPDD(I     ,EJ   ,J     ,A     )
            ELSEIF (NN==2) THEN
              CALL BC_UPD2D(I     ,SKEW_SPC(IAD),SKEW_SPC(IAD+3),A     )
            END IF
          ELSE
            IF (NN==1) THEN
              AR(J,I) = ZERO
              CALL BC_UPDD(I     ,EJ   ,J     ,A     )
            ELSEIF (NN==2) THEN
              CALL BC_UPD2D(I     ,SKEW_SPC(IAD),SKEW_SPC(IAD+3),AR    )
            END IF
          ENDIF
        ENDDO
        DO L = 1,NRW
          I = IRW(L)
          N=IN_RWL(I)
          EJ(1)=NOR_RWL(1,I)
          EJ(2)=NOR_RWL(2,I)
          EJ(3)=NOR_RWL(3,I)
          CALL L_DIR(EJ,J)
          CALL BC_UPDD(N     ,EJ   ,J     ,A     )
        ENDDO
C------Rigid bodies-------
        DO I=1,NRB
          M=IRB(1,I)
          DO J = 1 , NDOF(M)
            ND = IDDLM(J,I)
            IF (ND<=0) THEN
              IF (J<=3) THEN
                A(J,M)=ZERO
              ELSE
                AR(J-3,M)=ZERO
              ENDIF
            ENDIF
          ENDDO
        ENDDO
C------esperons le hierachic est dans l'ordre
        DO I=NRB,1,-1
          M=IRB(1,I)
          DO J = 1 , MIN(3,NDOF(M))
            ND = IDDLM(J,I)
            IF (ND>0) A(J,M)=LX(ND)
          ENDDO
          DO J = 4 , NDOF(M)
            ND = IDDLM(J,I)
            IF (ND>0) AR(J-3,M)=LX(ND)
          ENDDO
          NS=IRB(2,I)
          XS=X(1,NS)-X(1,M)
          YS=X(2,NS)-X(2,M)
          ZS=X(3,NS)-X(3,M)
          A(1,NS)=A(1,M)+AR(2,M)*ZS-AR(3,M)*YS
          A(2,NS)=A(2,M)-AR(1,M)*ZS+AR(3,M)*XS
          A(3,NS)=A(3,M)+AR(1,M)*YS-AR(2,M)*XS
        ENDDO
C--------RBE3-----
        DO I=1,NR3
          N=IR3(I)
          IAD=IRBE3(1,N)
          NNOD=IRBE3(5,N)
          IROT=IRBE3(6,N)
          DO M=1,NNOD
            NJ=LRBE3(IAD+M)
            DO J=1,NDOF(NJ)
              ND = IDDLM3(J,M,I)
              IF (J<=3.AND.ND>0) THEN
                A(J,NJ)=ZERO
              ELSEIF(ND>0) THEN
                AR(J-3,NJ)=ZERO
              ENDIF
            ENDDO
          ENDDO
        ENDDO
        IADS=1
        DO I=NR3,1,-1
          N=IR3(I)
          IAD=IRBE3(1,N)
          NS=IRBE3(3,N)
          NNOD=IRBE3(5,N)
          IROT=IRBE3(6,N)
          DO M=1,NNOD
            NJ=LRBE3(IAD+M)
            DO J=1,NDOF(NJ)
              ND = IDDLM3(J,M,I)
              IF (J<=3.AND.ND>0) THEN
                A(J,NJ)=LX(ND)
              ELSEIF(ND>0) THEN
                AR(J-3,NJ)=LX(ND)
              ENDIF
            ENDDO
          ENDDO
          CALL PRERBE3FR(IRBE3 ,N    ,JT  ,JR   )
          CALL RBE3_FRD(NNOD  ,LRBE3(IAD+1),NS    ,A     ,AR    ,
     1                  FCDI(IADS),MCDI(IADS)    ,JT    ,JR    ,
     2                  IROT  )
          IADS=IADS+NNOD
        ENDDO
C------RBE2---add jt&skew
        DO I=1,NR2
          N=IR2(1,I)
          M=IRBE2(3,N)
          DO J = 1 , NDOF(M)
            ND = IDDLM4(J,I)
            IF (ND<=0) THEN
              IF (J<=3) THEN
                A(J,M)=ZERO
              ELSE
                AR(J-3,M)=ZERO
              ENDIF
            ENDIF
          ENDDO
        ENDDO
        DO I=NR2,1,-1
          N=IR2(1,I)
          M=IRBE2(3,N)
          NS = IR2(2,I)
          ISK = IRBE2(7,N)
          IRAD =IRBE2(11,N)
          IC  = IRBE2(4,N)
          IC =(IC/512)*512
          DO J = 1 , MIN(3,NDOF(M))
            ND = IDDLM4(J,I)
            IF (ND>0) A(J,M)=LX(ND)
          ENDDO
          DO J = 4 , NDOF(M)
            ND = IDDLM4(J,I)
            IF (ND>0) AR(J-3,M)=LX(ND)
          ENDDO
          CALL PRERBE2FR(IC    ,JT  ,JR   )
          CALL RBE2_FRD(NS    ,M     ,X     ,A     ,AR    ,
     1                 JT    ,JR     ,SKEW(1,ISK),ISK   ,IRAD  )
        ENDDO
C------int2-------
        DO I=1,NI2
          N=II2(1,I)
          NI=II2(2,I)
          NSN=IPARI(5,N)
          L=INTBUF_TAB(N)%IRTLM(NI)
          NL=4*(L-1)
          IF (INTBUF_TAB(N)%IRECTM(NL+3)==INTBUF_TAB(N)%IRECTM(NL+4)) THEN
            NNOD=3
          ELSE
            NNOD=4
          ENDIF
C-------si noeud main est dependant aussi-----
          DO M=1,NNOD
            NJ=INTBUF_TAB(N)%IRECTM(NL+M)
            DO J=1,NDOF(NJ)
              ND = IDDLM2(J,M,I)
              IF (ND<=0) THEN
                IF (J<=3) THEN
                  A(J,NJ)=ZERO
                ELSE
                  AR(J-3,NJ)=ZERO
                ENDIF
              ENDIF
            ENDDO
          ENDDO
        ENDDO
C
        DO I=NI2,1,-1
          N=II2(1,I)
          NI=II2(2,I)
          NSN=IPARI(5,N)
          L=INTBUF_TAB(N)%IRTLM(NI)
          NL=4*(L-1)
          IF (INTBUF_TAB(N)%IRECTM(NL+3)==INTBUF_TAB(N)%IRECTM(NL+4)) THEN
            NNOD=3
          ELSE
            NNOD=4
          ENDIF
C-------si noeud main est dependant aussi-----
          DO M=1,NNOD
            NJ=INTBUF_TAB(N)%IRECTM(NL+M)
            DO J=1,NDOF(NJ)
              ND = IDDLM2(J,M,I)
              IF (J<=3.AND.ND>0) THEN
                A(J,NJ)=LX(ND)
              ELSEIF(ND>0) THEN
                AR(J-3,NJ)=LX(ND)
              ENDIF
            ENDDO
          ENDDO

          ILEV  =IPARI(20,N)
          IF (ILEV==1) THEN
            CALL I2_FRRD1(X   ,INTBUF_TAB(N)%IRECTM,INTBUF_TAB(N)%DPARA ,
     .                   INTBUF_TAB(N)%NSV ,
     1                   INTBUF_TAB(N)%IRTLM ,A   ,NI    )
          ELSE
            CALL I2_FRRD0(X   ,INTBUF_TAB(N)%IRECTM,INTBUF_TAB(N)%CSTS ,
     .                   INTBUF_TAB(N)%NSV ,
     1                   INTBUF_TAB(N)%IRTLM,A   ,AR    ,NI  ,NDOF    )
          ENDIF
        ENDDO
C
C------Actuallise-X(*) dans X_IMP-----
        DO I = 1, NUMN
          N = INL(I)
          DO J = 1, 3
            X_IMP(J,N) = A(J,N) + X(J,N)
          ENDDO
        ENDDO
C
        RETURN
      END
Chd|====================================================================
Chd|  IMP_PVGA                      source/airbag/monv_imp0.F
Chd|-- called by -----------
Chd|        MV_MATV                       source/airbag/monv_imp0.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE IMP_PVGA(IVOLU ,RVOLU   ,VOL     ,DPRES    )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com08_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER IVOLU(*)
C     REAL
        my_real
     .     RVOLU(*),DPRES
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER IDEF,IV
C     REAL
        my_real
     .     VOL,VINC,GAMA,PRES,PMAX,VEPS,POLD,VOLD,PEXT,
     .     DV,ENERGY,ENERG_OLD,DEOUT,FAC
C-----------------------------------------------
        PEXT   =RVOLU(3)
        POLD   =RVOLU(12)
C------------------------
        IDEF   =IVOLU(14)
        GAMA   =RVOLU(1)
C     P0V0G  =RVOLU(4)
        VINC   =RVOLU(5)
        PMAX   =RVOLU(6)
        ENERG_OLD=RVOLU(13)
        VOLD   =RVOLU(16)
        VEPS   =RVOLU(17)
        VOL    =VOL + VEPS
        DEOUT  =RVOLU(22)
        DV     = VOL-VOLD
C
        IF(IDEF==1)THEN
          PRES  = PEXT
        ELSE
C       CALCUL DE L ENERGIE PUIS DE LA PRESSION
          FAC  = HALF*(GAMA-ONE)*DV
          ENERGY= ((ONE-FAC/(VOLD-VINC))*ENERG_OLD-DEOUT*DT1 ) /
     .                               (ONE+FAC/(VOL-VINC))
          ENERGY = MAX(ENERGY,ZERO)
C
          PRES=(GAMA-ONE)*ENERGY/(VOL-VINC)
C
          IF(PRES>PMAX)THEN
            IDEF=1
            PRES = PEXT
          ENDIF
        ENDIF
C
        DPRES=PRES-POLD
C
C
        RETURN
      END
C
Chd|====================================================================
Chd|  IMP3_A2B                      source/airbag/monv_imp0.F
Chd|-- called by -----------
Chd|        INT_MATV                      source/implicit/imp_int_k.F
Chd|        INT_MATVP                     source/implicit/imp_int_k.F
Chd|        MV_MATV                       source/airbag/monv_imp0.F
Chd|-- calls ---------------
Chd|        BC_FI                         source/constraints/general/bcs/bc_imp0.F
Chd|        BC_FI2                        source/constraints/general/bcs/bc_imp0.F
Chd|        BC_UPDF                       source/constraints/general/bcs/bc_imp0.F
Chd|        FV_UPDF                       source/constraints/general/impvel/fv_imp0.F
Chd|        I2_FRFM0                      source/interfaces/interf/i2_imp1.F
Chd|        I2_FRFM1                      source/interfaces/interf/i2_imp1.F
Chd|        KIN_UPDF                      source/constraints/general/impvel/fv_imp0.F
Chd|        L_DIR                         source/constraints/general/bcs/bc_imp0.F
Chd|        PRERBE2FR                     source/constraints/general/rbe2/rbe2f.F
Chd|        PRERBE3FR                     source/constraints/general/rbe3/rbe3f.F
Chd|        RBE2FRF                       source/constraints/general/rbe2/rbe2f.F
Chd|        RBE3FRF                       source/constraints/general/rbe3/rbe3f.F
Chd|        RBY_IMPF                      source/constraints/general/rbody/rby_imp0.F
Chd|        IMP_ASPC                      share/modules/impbufdef_mod.F
Chd|        IMP_RWL                       share/modules/impbufdef_mod.F
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE IMP3_A2B(IPARI  ,INTBUF_TAB,NDOF  ,X_IMP  ,
     .                    A      ,AR     ,NUMN  ,INL   ,IDDL   ,
     .                    NRB    ,IRB    ,IDDLM ,NI2   ,II2    ,
     .                    IDDLM2 ,NFX    ,IFX   ,NBC   ,IBC    ,
     .                    NRW    ,IRW    ,IBFV  ,SKEW  ,XFRAME ,
     .                    LB     ,IRBE3  ,LRBE3 ,NR3   ,IR3    ,
     .                    IDDLM3 ,R3_MAX ,FCDI  ,MCDI  ,NSPC   ,
     .                    ISPC   ,IRBE2  ,LRBE2 ,NR2   ,IR2    ,
     .                    IDDLM4 )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
        USE IMP_RWL
        USE IMP_ASPC
        USE INTBUFDEF_MOD
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
        INTEGER NUMN,INL(*),NRB,IRB(2,*) ,NI2,II2(2,*),
     .          IDDL(3,*),IDDLM(6,*),IDDLM2(6,4,*),IBFV(*),
     .          IPARI(NPARI,*), NDOF(*),NFX,IFX(2,*),
     .          NBC,IBC(3,*),NRW,IRW(*),R3_MAX,NSPC,ISPC(*)
        INTEGER NR3,IR3(*),IDDLM3(6,R3_MAX,*),IRBE3(NRBE3L,*),LRBE3(*),
     .          NR2,IR2(2,*),IDDLM4(6,*),IRBE2(NRBE2L,*),LRBE2(*)
        my_real
     .    A(3,*),AR(3,*),X_IMP(3,*),LB(*),SKEW(LSKEW,*),XFRAME(*),
     .    FCDI(*) ,MCDI(*)

        TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
C----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER I,J,K,ID,ND,M,N,NS,NI,NSN,ILEV,JT(3),JR(3),
     .          JI,K10,K11,K12,K13,K14,J10,J11,J12,J21,
     .          L,NNOD,NJ,NL,IAD,IADS,IROT,ISK,IRAD,NN,IC
        my_real
     .    EJ(3)
C------noeuds independants------
        DO I = 1,NUMN
          N=INL(I)
          DO J=1,MIN(3,NDOF(N))
            ND = IDDL(J,I)
            IF (ND>0) LB(ND)=LB(ND)+A(J,N)
          ENDDO
        ENDDO
C------int2-------
        DO I=1,NI2
          N=II2(1,I)
          NI=II2(2,I)
          JI=IPARI(1,N)
          NSN=IPARI(5,N)
          K10=JI
          K11=K10+4*IPARI(3,N)
C------IRECT(4,NSN)-----
          K12=K11+4*IPARI(4,N)
C------NSV(NSN)--node number---
          K13=K12+NSN
C------MSR(NMN)-----
          K14=K13+IPARI(6,N)
          L=INTBUF_TAB(N)%IRTLM(NI)
          NL=4*(L-1)
C------IRTL(NSN)--main el number---
          J10=IPARI(2,N)
          J11=J10+1
          J12=J11+NPARIR
          J21=J12+2*NSN
          ILEV  =IPARI(20,N)
          IF (ILEV==1) THEN
            CALL I2_FRFM1(X_IMP ,INTBUF_TAB(N)%IRECTM,INTBUF_TAB(N)%DPARA ,
     .                   INTBUF_TAB(N)%NSV ,
     1                   INTBUF_TAB(N)%IRTLM ,A   ,NI   )
          ELSE
            CALL I2_FRFM0(X_IMP ,INTBUF_TAB(N)%IRECTM,INTBUF_TAB(N)%CSTS ,
     .                   INTBUF_TAB(N)%NSV ,
     1                   INTBUF_TAB(N)%IRTLM ,A   ,AR    ,NI  ,NDOF    )
          ENDIF
          IF (INTBUF_TAB(N)%IRECTM(NL+3)==INTBUF_TAB(N)%IRECTM(NL+4)) THEN
            NNOD=3
          ELSE
            NNOD=4
          ENDIF
C-------si noeud main est dependant aussi-----
          DO M=1,NNOD
            NJ=INTBUF_TAB(N)%IRECTM(NL+M)
            DO J=1,NDOF(NJ)
              ND = IDDLM2(J,M,I)
              IF (J<=3.AND.ND>0) THEN
                LB(ND) = LB(ND)+A(J,NJ)
                A(J,NJ)=ZERO
              ELSEIF(ND>0) THEN
                LB(ND) = LB(ND)+AR(J-3,NJ)
                AR(J-3,NJ)=ZERO
              ENDIF
            ENDDO
          ENDDO
        ENDDO
C---------RBE2 -------------
        DO I=1,NR2
          N=IR2(1,I)
          M=IRBE2(3,N)
          NS = IR2(2,I)
          ISK = IRBE2(7,N)
          IRAD =IRBE2(11,N)
          IC  = IRBE2(4,N)
          IC =(IC/512)*512
          CALL PRERBE2FR(IC    ,JT  ,JR   )
          CALL RBE2FRF(NS    ,M     ,A     ,AR    ,JT    ,
     1                 JR    ,X_IMP ,ISK   ,SKEW(1,ISK),IRAD  )
          DO J = 1 , NDOF(M)
            ND = IDDLM(J,I)
            IF (J<=3.AND.ND>0) THEN
              LB(ND)=LB(ND)+A(J,M)
              A(J,M)=ZERO
            ELSEIF (ND>0) THEN
              LB(ND)=LB(ND)+AR(J-3,M)
              AR(J-3,M)=ZERO
            ENDIF
          ENDDO
        ENDDO
C--------RBE3-----
        IADS=1
        DO I=1,NR3
          N=IR3(I)
          IAD=IRBE3(1,N)
          NS=IRBE3(3,N)
          NNOD=IRBE3(5,N)
          IROT=IRBE3(6,N)
          CALL PRERBE3FR(IRBE3 ,N    ,JT  ,JR   )
          CALL RBE3FRF(NNOD  ,LRBE3(IAD+1),NS    ,A     ,AR    ,
     1                 FCDI(IADS),MCDI(IADS),JT  ,JR    ,IROT  )
          IADS=IADS+NNOD
          DO M=1,NNOD
            NJ=LRBE3(IAD+M)
            DO J=1,NDOF(NJ)
              ND = IDDLM3(J,M,I)
              IF (J<=3.AND.ND>0) THEN
                LB(ND) = LB(ND)+A(J,NJ)
                A(J,NJ)=ZERO
              ELSEIF(ND>0.AND.IROT>0) THEN
                LB(ND) = LB(ND)+AR(J-3,NJ)
                AR(J-3,NJ)=ZERO
              ENDIF
            ENDDO
          ENDDO
        ENDDO
C------Rigid bodies-------
        DO I=1,NRB
          M=IRB(1,I)
          NS=IRB(2,I)
          CALL RBY_IMPF(X_IMP ,M      ,NS     ,NDOF   ,A     ,
     .                  AR    )
          DO J = 1 , NDOF(M)
            ND = IDDLM(J,I)
            IF (J<=3.AND.ND>0) THEN
              LB(ND)=LB(ND)+A(J,M)
              A(J,M)=ZERO
            ELSEIF (ND>0) THEN
              LB(ND)=LB(ND)+AR(J-3,M)
              AR(J-3,M)=ZERO
            ENDIF
          ENDDO
        ENDDO
        IF (NBC>0) THEN
          CALL BC_UPDF(NBC ,IBC ,SKEW  ,A      )
        ENDIF
        DO L=1,NSPC
          N = ISPC(L)
          I = IN_SPC(N)
          IF (NDOF(I)==0) CYCLE
          IROT = 0
          IAD = 6*(N-1)+1
          NN = IC_SPC(N)
          IF (NN>3) THEN
            NN= NN-3
            IROT = 1
          ENDIF
          IF (NN==1) THEN
            EJ(1)=SKEW_SPC(IAD)
            EJ(2)=SKEW_SPC(IAD+1)
            EJ(3)=SKEW_SPC(IAD+2)
            CALL L_DIR(EJ,J)
          ENDIF
          IF (IROT==0) THEN
            IF (NN==1) THEN
              CALL BC_FI(I     ,EJ   ,J     ,A     )
            ELSE
              CALL BC_FI2(I   ,SKEW_SPC(IAD),SKEW_SPC(IAD+3),A     )
            END IF
          ELSE
            IF (NN==1) THEN
              CALL BC_FI(I     ,EJ   ,J     ,AR    )
            ELSE
              CALL BC_FI2(I   ,SKEW_SPC(IAD),SKEW_SPC(IAD+3),AR    )
            END IF
          ENDIF
        ENDDO
C
        IF (NFX>0) THEN
          CALL FV_UPDF(NFX    ,IFX   ,IBFV  ,SKEW  ,XFRAME,
     1                 A      )
        ENDIF
        DO L = 1,NRW
          I = IRW(L)
          N = IN_RWL(I)
          EJ(1)=NOR_RWL(1,I)
          EJ(2)=NOR_RWL(2,I)
          EJ(3)=NOR_RWL(3,I)
          CALL L_DIR(EJ,J)
          CALL KIN_UPDF(N    ,EJ    ,J    ,A     )
        ENDDO
C
        RETURN
      END
Chd|====================================================================
Chd|  RECU_KDIS                     source/airbag/monv_imp0.F
Chd|-- called by -----------
Chd|        LIN_SOLV                      source/implicit/lin_solv.F
Chd|-- calls ---------------
Chd|        IMP_MONV                      share/modules/impbufdef_mod.F
Chd|====================================================================
      SUBROUTINE RECU_KDIS(NDOF   ,D     )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
        USE IMP_MONV
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER NDOF(*)
        my_real D(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER I,J,N,ND
C---------------------------------
        DO I = 1, NUMN_MV
          N = IN_MV(I)
          DO J = 1, 3
            ND = ID_MV(J,I)
            IF (ND==-1) THEN
              D(J,N) = ZERO
            ENDIF
          ENDDO
        ENDDO
C
        RETURN
      END

