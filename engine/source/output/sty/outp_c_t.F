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
Chd|  OUTP_C_T                      source/output/sty/outp_c_t.F  
Chd|-- called by -----------
Chd|        GENOUTP                       source/output/sty/genoutp.F   
Chd|-- calls ---------------
Chd|        SPMD_RGATHER9_1COMM           source/mpi/interfaces/spmd_outp.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE OUTP_C_T(ITENS ,KEY   ,TEXT  ,ELBUF_TAB,
     .                    IPARG ,EPSDOT,DD_IAD,SIZLOC   ,SIZP0,SIZ_WR)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD
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
#include      "units_c.inc"
#include      "scr01_c.inc"
#include      "scr05_c.inc"
#include      "task_c.inc"
#include      "scr16_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      CHARACTER*10 KEY
      CHARACTER*40 TEXT
      INTEGER IPARG(NPARG,*),DD_IAD(NSPMD+1,*)
      INTEGER ITENS,SIZLOC,SIZP0,SIZ_WR
      my_real
     .   EPSDOT(6,*)
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP), TARGET :: ELBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,JJ
      INTEGER NG,NEL,NFT,ITY,LFT,NPT,IPT,N0,
     .        LLT,MLW,ISTRAIN,N,NS1,NS2,
     .        I1,I2,ISTRE,IHBE,JJ_OLD,NGF,NGL,NN,LEN,
     .        IR,IS,NPTR,NPTS,NLAY,MPT,NPT_ALL,IGTYP,NPTT,II(8)
      INTEGER RESP0,WRTLEN,RES,COMPTEUR,L,K
      INTEGER, DIMENSION(NSPGROUP) :: JJ_LOC
      INTEGER, DIMENSION(NSPGROUP+1,NSPMD) :: ADRESS 
      my_real
     .   WA(SIZLOC),WAP0(SIZ_WR),WAP0_LOC(SIZP0)
      my_real A1,A2,THK,FUNC(6)
C
      TYPE(G_BUFEL_) , POINTER :: GBUF
C-----------------------------------------------
        IF (ISPMD == 0) THEN
          WRITE(IUGEO,'(2A)')'/SHELL     /TENSOR    /',KEY
          WRITE(IUGEO,'(A)')TEXT
          IF (OUTYY_FMT == 2) THEN
            WRITE(IUGEO,'(A)')
     .      '#FORMAT: (1P6E12.5) (TX(I),TY(I),TXY(I),I=1,NUMSHL)'
          ELSE
            WRITE(IUGEO,'(A)')
     .      '#FORMAT: (1P6E20.13) (TX(I),TY(I),TXY(I),I=1,NUMSHL)'
          ENDIF
        ENDIF
C
        JJ_OLD = 1
        RESP0 = 1
        NGF = 1
        NGL = 0
        JJ = 0
        COMPTEUR = 0
        DO NN=1,NSPGROUP
          NGL = NGL + DD_IAD(ISPMD+1,NN)
          DO NG = NGF,NGL
            ITY = IPARG(5,NG)
            IF (ITY == 3 .OR. ITY == 7) THEN
              MLW = IPARG(1,NG)
              NEL = IPARG(2,NG)
              NFT = IPARG(3,NG)
              LFT = 1
              LLT = NEL
              NPT     = IPARG(6,NG)
              MPT     = MAX(1,NPT)
              ISTRAIN = IPARG(44,NG)
              IHBE    = IPARG(23,NG)
              IGTYP   = IPARG(38,NG)
              NLAY    = ELBUF_TAB(NG)%NLAY
              GBUF    =>ELBUF_TAB(NG)%GBUF
!
              DO I=1,8  ! lenght max of GBUF%G_STRA = 8
                II(I) = NEL*(I-1)
              ENDDO

!
              IF (MLW == 0 .OR. MLW == 13) CYCLE   
              IF (MLW == 27 .OR. MLW == 25 .OR.
     .            MLW == 32 .OR. MLW == 15) ISTRAIN=1
C
              A1 = ZERO
              A2 = ZERO
              ISTRE = 1
C
              IF (IGTYP == 51 .OR. IGTYP == 52) THEN
                NPT_ALL = 0
                DO IPT=1,NLAY
                  NPT_ALL = NPT_ALL + ELBUF_TAB(NG)%BUFLY(IPT)%NPTT
                ENDDO
                IF (NLAY == 1) MPT  = MAX(1,NPT_ALL)
              ENDIF
C------------------------
C        STRESS
C------------------------
              IF (ITENS == 1) THEN
                NS1 = 5
                NS2 = 3
                A1 = ONE
                A2 = ZERO
              ELSEIF (ITENS == 2) THEN
                NS1 = 5
                NS2 = 3
                A1 = ZERO
                A2 = ONE
              ELSEIF (ITENS == 3) THEN
                NS1 = 5
                NS2 = 3
                IF (MLW == 1) THEN
                  A1 = ONE
                  A2 = SIX
                ELSEIF (MLW == 3 .OR. MLW == 23) THEN
                  A1 = ONE
                  A2 = ZERO
                ELSE
                  A1 = ONE
                  A2 = ZERO
                ENDIF
              ELSEIF (ITENS == 4) THEN
                NS1 = 5
                NS2 = 3
                IF (MLW == 1) THEN
                  A1 = ONE
                  A2 = -SIX
                ELSEIF (MLW == 3 .OR. MLW == 23) THEN
                  A1 = ONE
                  A2 = ZERO
                ELSE
                  A1 = ONE
                  A2 = ZERO
                ENDIF
              ELSEIF (ITENS >= 11  .and.  ITENS <= 40) THEN
                NS1 = 5
                NS2 = 3
                IF (MLW == 1 .OR. MLW == 3 .OR. MLW == 23) THEN
                  A1 = ONE
                  A2 = ZERO
                ELSE
                  IPT = MIN(MPT,ITENS-10)
                  A1 = ONE
                  A2 = ZERO
                ENDIF
C------------------------
C        STRAIN
C------------------------
              ELSEIF (ITENS == 5) THEN
                ISTRE = 0
                NS1 = 8
                NS2 = 8
                IF (ISTRAIN == 1) THEN
                  A1 = ONE
                  A2 = ZERO
                ELSE
                  A1 = ZERO
                  A2 = ZERO
                ENDIF
              ELSEIF (ITENS == 6) THEN
                ISTRE = 0
                NS1 = 8
                NS2 = 8
                IF (ISTRAIN == 1) THEN
                  A1 = ZERO
                  A2 = ONE
                ELSE
                  A1 = ZERO
                  A2 = ZERO
                ENDIF
              ELSEIF (ITENS == 7) THEN
                ISTRE = 0
                NS1 = 8
                NS2 = 8
                IF (ISTRAIN == 1) THEN
                  A1 = ONE
                  A2 = HALF
                ELSE
                  A1 = ZERO
                  A2 = ZERO
                ENDIF
              ELSEIF (ITENS == 8) THEN
                ISTRE = 0
                NS1 = 8
                NS2 = 8
                IF (ISTRAIN == 1) THEN
                  A1 = ONE
                  A2 = -HALF
                ELSE
                  A1 = ZERO
                  A2 = ZERO
                ENDIF
              ELSEIF (ITENS >= 51  .and.  ITENS <= 80) THEN
                ISTRE = 0
                NS1 = 8
                NS2 = 8
                IF (ISTRAIN == 1  .and.  MPT /= 0) THEN
                  IPT = MIN(MPT,ITENS-50)
                  A1 = ONE
                  A2 = HALF*(((2*IPT-ONE)/MPT)- ONE)
                ELSE
                  A1 = ZERO
                  A2 = ZERO
                ENDIF
C------------------------
C        STRAIN RATE
C------------------------
              ELSEIF (ITENS == 91) THEN
                ISTRE = 2
                A1 = ONE
                A2 = ZERO
              ELSEIF (ITENS == 92) THEN
                ISTRE = 2
                A1 = ZERO
                A2 = ONE
              ELSEIF (ITENS == 93) THEN
                ISTRE = 2
                A1 = ONE
                A2 = HALF
              ELSEIF (ITENS == 94) THEN
                ISTRE = 2
                A1 = ONE
                A2 = -HALF
              ELSEIF (ITENS >= 101  .and.  ITENS <= 130) THEN
                ISTRE = 2
                IPT = MIN(MPT,ITENS-100)
                A1 = ONE
                A2 = HALF*(((2*IPT-ONE)/MPT)-ONE)
              ENDIF
C
              IF (ISTRE == 1) THEN
C------------------------
C          STRESS
C------------------------
                DO I=LFT,LLT
                  DO J=1,3
                    JJ = JJ + 1
                    WA(JJ) = A1 * GBUF%FOR(II(J)+I) + A2 * GBUF%MOM(II(J)+I)
                  ENDDO
                ENDDO
              ELSEIF (ISTRE == 0  .and.  GBUF%G_STRA > 0) THEN
C------------------------
C          STRAIN
C------------------------
                DO I=LFT,LLT
                  N = I + NFT
                  THK = GBUF%THK(I)
                  JJ = JJ + 3
                  WA(JJ-2)= A1*GBUF%STRA(II(1)+I)+ A2*GBUF%STRA(II(1)+I)*THK
                  WA(JJ-1)= A1*GBUF%STRA(II(2)+I)+ A2*GBUF%STRA(II(2)+I)*THK
                  WA(JJ)  = HALF*(GBUF%STRA(II(3)+I)+ A2*GBUF%STRA(II(3)+I)*THK)
                ENDDO
              ELSEIF (ISTRE == 2) THEN
C------------------------
C          STRAIN RATE
C------------------------
                IF (ITY == 3) THEN
                  N0 = 0
                ELSE
                  N0 = NUMELC
                ENDIF
                DO I=LFT,LLT
                  THK = GBUF%THK(I)
                  N = I + NFT
                  JJ = JJ + 3
                  WA(JJ-2)= A1 * EPSDOT(1,N+N0) + A2 * EPSDOT(4,N+N0)*THK 
                  WA(JJ-1)= A1 * EPSDOT(2,N+N0) + A2 * EPSDOT(5,N+N0)*THK 
                  WA(JJ)= HALF*(A1*EPSDOT(3,N+N0)+A2*EPSDOT(6,N+N0)*THK) 
                ENDDO
              ENDIF  !  IF (ISTRE == 1)
            ENDIF  !  IF (ITY == 3 .OR. ITY == 7)
          ENDDO  !  DO NG = NGF,NGL
          NGF = NGL + 1
        JJ_LOC(NN) = JJ - COMPTEUR            ! size of each group
        COMPTEUR = JJ                         
       ENDDO
!     ++++++++++
       IF( NSPMD>1 ) THEN
        CALL SPMD_RGATHER9_1COMM(WA,JJ,JJ_LOC,WAP0_LOC,SIZP0,ADRESS)
       ELSE
        WAP0_LOC(1:JJ) = WA(1:JJ)
        ADRESS(1,1) = 1
        DO NN = 2,NSPGROUP+1
         ADRESS(NN,1) = JJ_LOC(NN-1) + ADRESS(NN-1,1)
        ENDDO
       ENDIF
!     ++++++++++
       IF(ISPMD==0) THEN
         RESP0 = 0
         DO NN=1,NSPGROUP
          COMPTEUR = 0
          DO K = 1,NSPMD
           IF((ADRESS(NN+1,K)-1-ADRESS(NN,K))>=0) THEN
            DO L = ADRESS(NN,K),ADRESS(NN+1,K)-1
             COMPTEUR = COMPTEUR + 1
             WAP0(COMPTEUR+RESP0) = WAP0_LOC(L)
            ENDDO  ! l=... , ...
           ENDIF   !if(size_loc>0)
          ENDDO    ! k=1,nspmd

          JJ_OLD = COMPTEUR+RESP0
          IF(JJ_OLD>0) THEN
            RES=MOD(JJ_OLD,6)
            WRTLEN=JJ_OLD-RES
            IF (WRTLEN > 0) THEN
              IF (OUTYY_FMT == 2) THEN
                WRITE(IUGEO,'(1P6E12.5)')(WAP0(J),J=1,WRTLEN)
               ELSE
                WRITE(IUGEO,'(1P6E20.13)')(WAP0(J),J=1,WRTLEN)
              ENDIF
            ENDIF
            DO I=1,RES
               WAP0(I)=WAP0(WRTLEN+I)
            ENDDO
            RESP0=RES
          ENDIF
        ENDDO  !  DO NN=1,NSPGROUP
C
        IF (RESP0 > 0) THEN
          IF (OUTYY_FMT == 2) THEN
            WRITE(IUGEO,'(1P6E12.5)')(WAP0(J),J=1,RESP0)
          ELSE
            WRITE(IUGEO,'(1P6E20.13)')(WAP0(J),J=1,RESP0)
          ENDIF
        ENDIF
      ENDIF  !  IF (NSPMD == 1)
C---
      RETURN
      END

Chd|====================================================================
Chd|  OUTP_C_TF                     source/output/sty/outp_c_t.F  
Chd|-- called by -----------
Chd|        GENOUTP                       source/output/sty/genoutp.F   
Chd|-- calls ---------------
Chd|        C_TF_NE                       source/output/sty/c_tf_ne.F   
Chd|        SPMD_RGATHER9_1COMM           source/mpi/interfaces/spmd_outp.F
Chd|        DRAPE_MOD                     share/modules/drape_mod.F     
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        STACK_MOD                     share/modules/stack_mod.F     
Chd|====================================================================
      SUBROUTINE OUTP_C_TF(ITENS ,KEY   ,TEXT  ,ELBUF_TAB,IPARG,
     .                     DD_IAD,SIZLOC,SIZP0 ,THKE     ,GEO  ,
     .                     IGEO  ,STACK ,DRAPE_Q4, DRAPE_T3 ,IXC  ,
     .                     IXTG  ,SIZ_WR)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD
      USE STACK_MOD
      USE DRAPE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "units_c.inc"
#include      "task_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "scr01_c.inc"
#include      "scr05_c.inc"
#include      "scr16_c.inc"
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      CHARACTER*10 KEY
      CHARACTER*40 TEXT
      INTEGER ITENS
      INTEGER IPARG(NPARG,*), DD_IAD(NSPMD+1,*),SIZLOC,SIZP0,
     .   IGEO(NPROPGI,*),IXC(NIXC,*),IXTG(NIXTG,*),
     .   SIZ_WR
      my_real
     .   THKE(*),GEO(NPROPG,*)
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP), TARGET :: ELBUF_TAB
      TYPE (STACK_PLY) :: STACK
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NG,NEL,NFT,ITY,LFT,NPT,IPT,N0,
     .   LLT,MLW,ISTRAIN,N,NS1,NS2,
     .   I1,I2,I3,I4,I5,I11,ISTRE,IHBE,I,J,JJ,
     .   JJ_OLD,NGF,NGL,NN,LEN,K,NPG,IPG,NLAY,NPTS,NPTR,IL,ITHK,NPTT,IT,
     .   IGTYP,IXFEM,ISUBSTACK,NPT_ALL,MPT,COMPTEUR,L,II(8)
      INTEGER, DIMENSION(NSPGROUP) :: JJ_LOC
      INTEGER, DIMENSION(NSPGROUP+1,NSPMD) :: ADRESS 
      my_real
     .   WA(SIZLOC),WAP0(SIZ_WR),WAP0_LOC(SIZP0)

      my_real
     .   A1,A2,THK,FUNC(6),HOURG(5),PLA(MVSIZ)
C
      TYPE(BUF_LAY_) ,POINTER :: BUFLY
      TYPE(G_BUFEL_) ,POINTER :: GBUF
      TYPE(L_BUFEL_) ,POINTER :: LBUF
      TYPE(DRAPE_) :: DRAPE_Q4, DRAPE_T3
C-----------------------------------------------
        IF (ISPMD == 0) THEN
          WRITE(IUGEO,'(2A)')'/SHELL     /TENSOR    /',KEY
          WRITE(IUGEO,'(A)')TEXT
          IF (ITENS == 95) THEN
            WRITE(IUGEO,'(A)')
     .      '#(NPG=Surface Quadratue Points; For QEPH,QBAT,DKT18: NPG>1) '
            IF (OUTYY_FMT == 2) THEN
              WRITE(IUGEO,'(A)')
     .        '#FORMAT: (IF NPT>0) (2I8/1P6E12.5/6E12.5) '
              WRITE(IUGEO,'(A)')
     .        '#NPT,NPG,THICK,EM,EB,H1,H2,H3'
              WRITE(IUGEO,'(2A)')
     .        '#(TX,TY,TXY,TXZ,TYZ,EPSP(K,J,I)',
     .        'K=1,NPG),J=1,NPT),I=1,NUMSHL)'
              WRITE(IUGEO,'(A)')
     .        '#FORMAT: (IF NPT == 0) ((2I8/1P6E12.5/6E12.5/3E12.5)) '
              WRITE(IUGEO,'(A)')
     .        '#0,NPG,THICK,EM,EB,H1,H2,H3'
              WRITE(IUGEO,'(2A)')
     .        '#(NX,NY,NXY,NXZ,NYZ,EPSP,MX,MY,MXY(K,I)',
     .        'K=1,NPG),I=1,NUMSHL)'
            ELSE
              WRITE(IUGEO,'(A)')
     .        '#FORMAT: (IF NPT>0) (2I10/1P6E20.13/6E20.13) '
              WRITE(IUGEO,'(A)')
     .        '#NPT,NPG,THICK,EM,EB,H1,H2,H3'
              WRITE(IUGEO,'(2A)')
     .        '#(TX,TY,TXY,TXZ,TYZ,EPSP(K,J,I)',
     .        'K=1,NPG),J=1,NPT),I=1,NUMSHL)'
              WRITE(IUGEO,'(A)')
     .        '#FORMAT: (IF NPT == 0) ((2I10/1P6E20.13/6E20.13/3E20.13)) '
              WRITE(IUGEO,'(A)')
     .        '#0,NPG,THICK,EM,EB,H1,H2,H3'
              WRITE(IUGEO,'(2A)')
     .        '#(NX,NY,NXY,NXZ,NYZ,EPSP,MX,MY,MXY(K,I)',
     .        'K=1,NPG),I=1,NUMSHL)'
            ENDIF
          ELSEIF (ITENS == 96) THEN
            IF (OUTYY_FMT == 2) THEN
              WRITE(IUGEO,'(A)')
     .        '#FORMAT: (1P6E12.5/3E12.5) '
              WRITE(IUGEO,'(2A)')
     .        '#(EX(I),EY(I),EXY(I),EXZ(I),EYZ(I),',
     .        'EPSP(I),KX(I),KY(I),KXY(I),I=1,NUMSHL)'
            ELSE
              WRITE(IUGEO,'(A)')
     .        '#FORMAT: (1P6E20.13/3E20.13) '
              WRITE(IUGEO,'(2A)')
     .        '#(EX(I),EY(I),EXY(I),EXZ(I),EYZ(I),',
     .        'EPSP(I),KX(I),KY(I),KXY(I),I=1,NUMSHL)'
            ENDIF
          ENDIF  !  IF (ITENS == 95)
        ENDIF  !  IF (ISPMD == 0)
C
        JJ_OLD = 1
        NGF = 1
        NGL = 0 
        JJ = 0
        COMPTEUR = 0
        DO NN=1,NSPGROUP
          NGL = NGL + DD_IAD(ISPMD+1,NN)
          DO NG = NGF, NGL
            ITY = IPARG(5,NG)
            IF (ITY == 3 .OR. ITY == 7) THEN
              MLW   = IPARG(1,NG)
              NEL   = IPARG(2,NG)
              NFT   = IPARG(3,NG)
              LFT = 1
              LLT = NEL
              NPT    = IPARG(6,NG)
              ISTRAIN= IPARG(44,NG)
              IHBE   = IPARG(23,NG)
              ITHK   = IPARG(28,NG)
              IGTYP  = IPARG(38,NG)
              IXFEM  = IPARG(54,NG)
              ISUBSTACK=IPARG(71,NG)
C---
              GBUF => ELBUF_TAB(NG)%GBUF
              NLAY = ELBUF_TAB(NG)%NLAY
              NPTR = ELBUF_TAB(NG)%NPTR
              NPTS = ELBUF_TAB(NG)%NPTS
              NPG  = NPTR*NPTS
!
              DO I=1,8  ! lenght max of GBUF%G_STRA = 8
                II(I) = NEL*(I-1)
              ENDDO
!
C
C pre counting of all NPTT (especialy for PID_51)
C
              MPT = IABS(NPT)
              IF (IGTYP == 51 .OR. IGTYP == 52 ) THEN
                NPT_ALL = 0
                DO IL=1,NLAY
                  NPT_ALL = NPT_ALL + ELBUF_TAB(NG)%BUFLY(IL)%NPTT
                ENDDO
                MPT  = MAX(1,NPT_ALL)
              ENDIF
C---
              IF (MLW == 27 .OR. MLW == 25 .OR.
     .            MLW == 32 .OR. MLW == 15) ISTRAIN=1
C------------------------
C        STRESS
C------------------------
              IF (ITENS == 95) THEN
                IF (IHBE == 0) THEN
                  DO I=LFT,LLT
                    WA(JJ+1) = IHBE
                    JJ=JJ+1
                  ENDDO
                ELSEIF (IHBE >= 11) THEN
                  CALL C_TF_NE(ELBUF_TAB(NG),IHBE     ,NEL  ,NPT   ,MLW   ,
     .                         ITY          ,ISTRAIN  ,JJ   ,WA    ,1     ,
     .                         NLAY         ,NPTR     ,NPTS ,ITHK  ,NFT   ,
     .                         THKE         ,NPG      ,IGTYP,GEO   ,IGEO  ,
     .                         IXFEM        ,ISUBSTACK,STACK,DRAPE_Q4, DRAPE_T3,
     .                         IXC          ,IXTG     ,MPT  )
                ELSE
                  IF (MLW == 1 .OR. MLW == 3 .OR. MLW == 23) THEN
                    DO I=LFT,LLT
                      IF (GBUF%G_HOURG == 0) THEN
                        HOURG(1) = ZERO
                        HOURG(2) = ZERO
                        HOURG(3) = ZERO
                      ELSE
                        HOURG(1) = GBUF%HOURG(II(1)+I)
                        HOURG(2) = GBUF%HOURG(II(2)+I)
                        HOURG(3) = GBUF%HOURG(II(3)+I)
                      ENDIF
                      WA(JJ+1) = IHBE
                      JJ=JJ+1
                      WA(JJ+1) = 0
                      IF (ITHK >0 ) THEN
                        WA(JJ+2) = GBUF%THK(I)
                      ELSE
                        WA(JJ+2) = THKE(I+NFT)
                      END IF
c                      WA(JJ+2) = GBUF%THK(I)
                      WA(JJ+3) = GBUF%EINT(I)
                      WA(JJ+4) = GBUF%EINT(I+LLT)
                      WA(JJ+5) = HOURG(1)
                      WA(JJ+6) = HOURG(2)
                      WA(JJ+7) = HOURG(3)
                      WA(JJ+8)  = GBUF%FORPG(II(1)+I)
                      WA(JJ+9)  = GBUF%FORPG(II(2)+I)
                      WA(JJ+10) = GBUF%FORPG(II(3)+I)
                      WA(JJ+11) = GBUF%FORPG(II(4)+I)
                      WA(JJ+12) = GBUF%FORPG(II(5)+I)
                      IF (GBUF%G_PLA > 0) THEN       
                        WA(JJ+13) = GBUF%PLA(I)
                      ELSE                           
                        WA(JJ+13)=ZERO                 
                      ENDIF                          
                      WA(JJ+14) = GBUF%MOMPG(II(1)+I)
                      WA(JJ+15) = GBUF%MOMPG(II(2)+I)
                      WA(JJ+16) = GBUF%MOMPG(II(3)+I)
                      JJ = JJ + 16
                    ENDDO
                  ELSE   ! MLW 
                    DO I=LFT,LLT
                      IF (GBUF%G_HOURG == 0) THEN
                        HOURG(1) = ZERO
                        HOURG(2) = ZERO
                        HOURG(3) = ZERO
                      ELSE
                        HOURG(1) = GBUF%HOURG(II(1)+I)
                        HOURG(2) = GBUF%HOURG(II(2)+I)
                        HOURG(3) = GBUF%HOURG(II(3)+I)
                      ENDIF
                      WA(JJ+1) = IHBE
                      JJ=JJ+1
                      WA(JJ+1) = MPT
                      IF (ITHK > 0) THEN
                        WA(JJ+2) = GBUF%THK(I)
                      ELSE
                        WA(JJ+2) = THKE(I+NFT)
                      END IF
                      WA(JJ+3) = GBUF%EINT(I)
                      WA(JJ+4) = GBUF%EINT(I+LLT)
                      WA(JJ+5) = HOURG(1)
                      WA(JJ+6) = HOURG(2)
                      WA(JJ+7) = HOURG(3)
                      JJ = JJ+7
C
                      IF (NPT == 0) THEN
C
                        WA(JJ+1) = GBUF%FORPG(II(1)+I)
                        WA(JJ+2) = GBUF%FORPG(II(2)+I)
                        WA(JJ+3) = GBUF%FORPG(II(3)+I)
                        WA(JJ+4) = GBUF%FORPG(II(4)+I)
                        WA(JJ+5) = GBUF%FORPG(II(5)+I)
                        IF (GBUF%G_PLA > 0) THEN       
                          WA(JJ+6) = GBUF%PLA(I)
                        ELSE                           
                          WA(JJ+6)=ZERO                
                        ENDIF                          
                        WA(JJ+7) = GBUF%MOMPG(II(1)+I)
                        WA(JJ+8) = GBUF%MOMPG(II(2)+I)
                        WA(JJ+9) = GBUF%MOMPG(II(3)+I)
                        JJ = JJ+9
C
                      ELSE  ! NPT > 0     
C
                        IF (NLAY == 1) THEN
                          BUFLY => ELBUF_TAB(NG)%BUFLY(1)
                          NPTT = BUFLY%NPTT
                          DO IT = 1,NPTT
                            LBUF => BUFLY%LBUF(1,1,IT)
                            WA(JJ+1) = LBUF%SIG(II(1)+I)
                            WA(JJ+2) = LBUF%SIG(II(2)+I)
                            WA(JJ+3) = LBUF%SIG(II(3)+I)
                            WA(JJ+4) = LBUF%SIG(II(4)+I)
                            WA(JJ+5) = LBUF%SIG(II(5)+I)
                            IF (BUFLY%L_PLA > 0) THEN
                              WA(JJ+6) = LBUF%PLA(I)
                            ELSE
                              WA(JJ+6) = ZERO
                            ENDIF
                            JJ = JJ+6
                          ENDDO
                        ELSEIF (NLAY > 1) THEN
                          DO IL = 1,NLAY
                            BUFLY => ELBUF_TAB(NG)%BUFLY(IL)
                            NPTT = BUFLY%NPTT  ! need for PID51 (new shell prop)
                            DO IT=1,NPTT
                              LBUF  => BUFLY%LBUF(1,1,IT)
                              WA(JJ+1) = LBUF%SIG(II(1)+I)
                              WA(JJ+2) = LBUF%SIG(II(2)+I)
                              WA(JJ+3) = LBUF%SIG(II(3)+I)
                              WA(JJ+4) = LBUF%SIG(II(4)+I)
                              WA(JJ+5) = LBUF%SIG(II(5)+I)
                              IF (BUFLY%L_PLA > 0) THEN
                                WA(JJ+6) = LBUF%PLA(I)
                              ELSE
                                WA(JJ+6) = ZERO
                              ENDIF
                              JJ = JJ+6
                            ENDDO ! DO IT=1,NPTT
                          ENDDO ! DO IL = 1,NLAY
                        ENDIF  !  IF (NLAY ==1) THEN
                      ENDIF  !  NPT > 0
                    ENDDO  !  DO I=LFT,LLT
                  ENDIF  !  MLW 
                ENDIF  !  IF (IHBE == 0)
C------------------------
C        STRAIN
C------------------------
              ELSEIF (ITENS == 96) THEN
cc                IF (ISTRAIN > 0) THEN
                IF (GBUF%G_STRA > 0) THEN
                  DO I=LFT,LLT
                    WA(JJ+1) = GBUF%STRA(II(1)+I)
                    WA(JJ+2) = GBUF%STRA(II(2)+I)
                    WA(JJ+3) = GBUF%STRA(II(3)+I)
                    WA(JJ+4) = GBUF%STRA(II(4)+I)
                    WA(JJ+5) = GBUF%STRA(II(5)+I)
C--------------
  ! plastic strain (mid layer : npt/2 + 1)
C--------------
                    IF (GBUF%G_PLA > 0) THEN
                      IF (NLAY > 1) THEN
                        IL = IABS(NLAY)/2 + 1
                        BUFLY => ELBUF_TAB(NG)%BUFLY(IL)
                       IF (BUFLY%L_PLA > 0) THEN
                        NPTT = BUFLY%NPTT
                        IF (IHBE /= 11) THEN
                          FUNC(6) = ZERO
                          DO IT=1,NPTT
                            LBUF  => BUFLY%LBUF(1,1,IT)
                            FUNC(6) = FUNC(6) + LBUF%PLA(I)/NPTT
                          ENDDO
                          WA(JJ+6) = FUNC(6)
                        ELSE
                          WA(JJ+6) = BUFLY%PLAPT(I)
                        ENDIF
                       ENDIF
                      ELSE
                        BUFLY => ELBUF_TAB(NG)%BUFLY(1)
                       IF (BUFLY%L_PLA > 0) THEN
                        NPTT = BUFLY%NPTT
                        IL = IABS(NPTT)/2 + 1
                        IF (IHBE /= 11) THEN
                          WA(JJ+6) = BUFLY%LBUF(1,1,IL)%PLA(I)
                        ELSE
                          I3 = (IL-1)*NEL
                          WA(JJ+6) = BUFLY%PLAPT(I3+I)
                        ENDIF
                       ENDIF
                      ENDIF
                    ELSE
                      WA(JJ+6) = ZERO
                    ENDIF
                    WA(JJ+7) = GBUF%STRA(II(6)+I)
                    WA(JJ+8) = GBUF%STRA(II(7)+I)
                    WA(JJ+9) = GBUF%STRA(II(8)+I)
                    JJ = JJ+9
                  ENDDO
                ELSE  ! (GBUF%G_STRA = 0)
                  DO I=LFT,LLT
                    I1 = 8*(I-1)
                    WA(JJ+1) = ZERO
                    WA(JJ+2) = ZERO
                    WA(JJ+3) = ZERO
                    WA(JJ+4) = ZERO
                    WA(JJ+5) = ZERO
C--------------
  ! plastic strain (mid layer : npt/2 + 1)
C--------------
                    IF (GBUF%G_PLA > 0) THEN
                      IF (NLAY > 1) THEN
                        IL = IABS(NLAY)/2 + 1
                        BUFLY => ELBUF_TAB(NG)%BUFLY(IL)
                       IF (BUFLY%L_PLA > 0) THEN
                        NPTT = BUFLY%NPTT
                        IF (IHBE /= 11) THEN
                          FUNC(6) = ZERO
                          DO IT=1,NPTT
                            LBUF  => BUFLY%LBUF(1,1,IT)
                            FUNC(6) = FUNC(6) + LBUF%PLA(I)/NPTT
                          ENDDO
                          WA(JJ+6) = FUNC(6)
                        ELSE
                          WA(JJ+6) = BUFLY%PLAPT(I)
                        ENDIF
                       ENDIF
                      ELSE
                        BUFLY => ELBUF_TAB(NG)%BUFLY(1)
                       IF (BUFLY%L_PLA > 0) THEN
                        NPTT = BUFLY%NPTT
                        IL = IABS(NPTT)/2 + 1
                        IF (IHBE /= 11) THEN
                          WA(JJ+6) = BUFLY%LBUF(1,1,IL)%PLA(I)
                        ELSE
                          I3 = (IL-1)*NEL
                          WA(JJ+6) = BUFLY%PLAPT(I3+I)
                        ENDIF
                       ENDIF
                      ENDIF
                    ELSE
                      WA(JJ+6) = ZERO
                    ENDIF
                    WA(JJ+7) = ZERO
                    WA(JJ+8) = ZERO
                    WA(JJ+9) = ZERO
                    JJ = JJ+9
                  ENDDO
                ENDIF !  IF (GBUF%G_STRA > 0)
C
              ENDIF  !  IF (ITENS == 95)
            ENDIF  !  IF (ITY == 3 .OR. ITY == 7)
          ENDDO  !  DO NG = NGF, NGL
C-----------------------------------------------------------------------
          NGF = NGL + 1
        JJ_LOC(NN) = JJ - COMPTEUR            ! size of each group
        COMPTEUR = JJ                         
       ENDDO
!     ++++++++++
       IF( NSPMD>1 ) THEN
        CALL SPMD_RGATHER9_1COMM(WA,JJ,JJ_LOC,WAP0_LOC,SIZP0,ADRESS)
       ELSE
        WAP0_LOC(1:JJ) = WA(1:JJ)
        ADRESS(1,1) = 1
        DO NN = 2,NSPGROUP+1
         ADRESS(NN,1) = JJ_LOC(NN-1) + ADRESS(NN-1,1)
        ENDDO
       ENDIF
!     ++++++++++
       IF(ISPMD==0) THEN
         DO NN=1,NSPGROUP
          COMPTEUR = 0
          DO K = 1,NSPMD
           IF((ADRESS(NN+1,K)-1-ADRESS(NN,K))>=0) THEN
            DO L = ADRESS(NN,K),ADRESS(NN+1,K)-1
             COMPTEUR = COMPTEUR + 1
             WAP0(COMPTEUR) = WAP0_LOC(L)
            ENDDO  ! l=... , ...
           ENDIF   !if(size_loc>0)
          ENDDO    ! k=1,nspmd
           

          JJ_OLD = COMPTEUR
          IF(JJ_OLD>0) THEN

            IF (ITENS == 95) THEN
              J = 1
              DO WHILE (J < JJ_OLD+1)
                IHBE=NINT(WAP0(J))
                J=J+1
                IF (IHBE == 0) THEN
                  IF (OUTYY_FMT == 2) THEN
                    NPT = 0
                    NPG = 0
                    WRITE(IUGEO,'(2I8/,1P6E12.5)')
     .                NPT,NPG,ZERO,ZERO,ZERO,
     .                ZERO,ZERO,ZERO
                    WRITE(IUGEO,'(1P6E12.5)')
     .                ZERO,ZERO,ZERO,
     .                ZERO,ZERO,ZERO
                    WRITE(IUGEO,'(1P3E12.5)')
     .                ZERO,ZERO,ZERO
                  ELSE
                    NPT = 0
                    NPG = 0
                    WRITE(IUGEO,'(2I10/,1P6E20.13)')
     .                NPT,NPG,ZERO,ZERO,ZERO,
     .                ZERO,ZERO,ZERO
                    WRITE(IUGEO,'(1P6E20.13)')
     .                ZERO,ZERO,ZERO,
     .                ZERO,ZERO,ZERO
                    WRITE(IUGEO,'(1P3E20.13)')
     .                ZERO,ZERO,ZERO
                  ENDIF 
                ELSEIF (IHBE >= 11) THEN
                  IF (OUTYY_FMT == 2) THEN
                    NPT = NINT(WAP0(J))
                    NPG = NINT(WAP0(J+1))
                    WRITE(IUGEO,'(2I8/,1P3E12.5)')NPT,NPG,
     .                                           (WAP0(J+K),K=2,4)
                    J = J + 5    
                    IF (NPT == 0) THEN
                      DO IPG=1,NPG
                        WRITE(IUGEO,'(1P6E12.5/1P3E12.5)')
     .                                   (WAP0(J+K),K=0,8)
                        J = J + 9
                      ENDDO 
                    ELSE
                      DO I=1,NPT
                        DO IPG=1,NPG
                          WRITE(IUGEO,'(1P6E12.5)')(WAP0(J+K),K=0,5)
                          J = J + 6
                        ENDDO
                      ENDDO
                    ENDIF
                  ELSE
                    NPT = NINT(WAP0(J))
                    NPG = NINT(WAP0(J+1))
                    WRITE(IUGEO,'(2I10/,1P3E20.13)')NPT,NPG,
     .                                          (WAP0(J+K),K=2,4)          
                    J = J + 5    
                    IF (NPT == 0) THEN
                      DO IPG=1,NPG
                        WRITE(IUGEO,'(1P6E20.13/1P3E20.13)')
     .                                          (WAP0(J+K),K=0,8)
                        J = J + 9    
                      ENDDO 
                    ELSE
                      DO I=1,NPT
                        DO IPG=1,NPG
                          WRITE(IUGEO,'(1P6E20.13)')(WAP0(J+K),K=0,5)
                          J = J + 6
                        ENDDO
                      ENDDO
                    ENDIF
                  ENDIF 
                ELSE
                  NPT = NINT(WAP0(J))
                  IF (NPT == 0) THEN
                    IF (OUTYY_FMT == 2) THEN
                      WRITE(IUGEO,'(I8/,1P6E12.5)')NPT,(WAP0(J+K),K=1,6)
                      WRITE(IUGEO,'(1P6E12.5/1P3E12.5)')(WAP0(J+K),K=7,15)
                    ELSE
                      WRITE(IUGEO,'(I10/,1P6E20.13)')NPT,(WAP0(J+K),K=1,6)
                      WRITE(IUGEO,'(1P6E20.13/1P3E20.13)')(WAP0(J+K),K=7,15)
                    ENDIF
                    J = J + 16    
                  ELSE
                    IF (OUTYY_FMT == 2) THEN
                      WRITE(IUGEO,'(I8/,1P6E12.5)')NPT,(WAP0(J+K),K=1,6)
                    ELSE
                      WRITE(IUGEO,'(I10/,1P6E20.13)')NPT,(WAP0(J+K),K=1,6)
                    ENDIF
                    J = J + 7
                    IF (OUTYY_FMT == 2) THEN
                      DO I=1,NPT
                        WRITE(IUGEO,'(1P6E12.5)')(WAP0(J-1+K),K=1,6)
                        J = J + 6
                      ENDDO
                    ELSE
                      DO I=1,NPT
                        WRITE(IUGEO,'(1P6E20.13)')(WAP0(J-1+K),K=1,6)
                        J = J + 6
                      ENDDO
                    ENDIF
                  ENDIF
                ENDIF  !  IF (IHBE == 0)
              ENDDO  !  DO WHILE (J < JJ_OLD)
            ELSEIF (ITENS == 96) THEN
              J = 1
              IF (OUTYY_FMT == 2) THEN
                DO WHILE (J < JJ_OLD)
                  WRITE(IUGEO,'(1P6E12.5)')(WAP0(J-1+K),K=1,6)
                  WRITE(IUGEO,'(1P3E12.5)')(WAP0(J-1+K),K=7,9)
                  J = J + 9
                ENDDO
              ELSE
                DO WHILE (J < JJ_OLD)
                  WRITE(IUGEO,'(1P6E20.13)')(WAP0(J-1+K),K=1,6)
                  WRITE(IUGEO,'(1P3E20.13)')(WAP0(J-1+K),K=7,9)
                  J = J + 9
                ENDDO
              ENDIF
            ENDIF  !  IF (ITENS == 95)
          ENDIF  !  IF (JJ_OLD > 0)
        ENDDO  !  DO NN=1,NSPGROUP
      ENDIF  !  IF(ISPMD == 0)THEN
C---
      RETURN
      END
Chd|====================================================================
Chd|  COUNT_ARSZ_CT                 source/output/sty/outp_c_t.F  
Chd|-- called by -----------
Chd|        GENOUTP                       source/output/sty/genoutp.F   
Chd|        OUTP_ARSZ_CT                  source/mpi/interfaces/spmd_outp.F
Chd|-- calls ---------------
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE COUNT_ARSZ_CT(IPARG,DD_IAD,WASZ,SIZ_WRITE,ELBUF_TAB)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "task_c.inc"
#include      "com01_c.inc"
#include      "scr16_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER IPARG(NPARG,*),DD_IAD(NSPMD+1,*),WASZ(3),SIZ_WRITE(3*NSPGROUP+3)
       TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP), TARGET :: ELBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER NGF,NGL,NN,ITY,MLW,NEL,NPT,IHBE,NPG,MPT,NG,JJ,COUNT,I,
     .          WASZ1,WASZ2,WASZ3,WASZ_95,WASZ4,
     .          IL,NLAY,IGTYP,NPT_ALL,
     .          NPTR,NPTS,SIZ_WRITE_LOC(4*NSPGROUP)
C-----------------------------------------------
      SIZ_WRITE_LOC = 0
      WASZ1 = 0

      COUNT = 0
      DO I=1,30
        COUNT = COUNT + OUTP_CT(10+I)+OUTP_CT(50+I)+OUTP_CT(100+I)
      ENDDO

      IF ( OUTP_CT( 1) == 1.OR.OUTP_CT( 2) == 1.OR.OUTP_CT( 3) == 1
     . .OR.OUTP_CT( 4) == 1.OR.OUTP_CT( 5) == 1.OR.OUTP_CT( 6) == 1
     . .OR.OUTP_CT( 7) == 1.OR.OUTP_CT( 8) == 1.OR.OUTP_CT(91) == 1
     . .OR.OUTP_CT(92) == 1.OR.OUTP_CT(93) == 1.OR.OUTP_CT(94) == 1
     . .OR.COUNT>0 )   THEN

        NGF = 1
        NGL = 0
        DO NN=1,NSPGROUP
         JJ = 0
         NGL = NGL + DD_IAD(ISPMD+1,NN)
           DO NG = NGF, NGL
            ITY   =IPARG(5,NG)
            IF(ITY == 3.OR.ITY == 7) THEN
              NEL = IPARG(2,NG)
              JJ = JJ + 3 * NEL
            ENDIF
          ENDDO
          NGF = NGL + 1
          WASZ1 = WASZ1+JJ
          SIZ_WRITE_LOC(NN) = JJ
        ENDDO
      ENDIF


      WASZ2 = 0
C--------

      IF (OUTP_CT(95) == 1) THEN
        NGF = 1
        NGL = 0
        DO NN=1,NSPGROUP
          JJ = 0
          NGL = NGL + DD_IAD(ISPMD+1,NN)
          DO NG = NGF, NGL
            ITY   =IPARG(5,NG)
            IF (ITY == 3.OR.ITY == 7) THEN
              MLW   = IPARG(1,NG)
              NEL   = IPARG(2,NG)
              NPT   = IPARG(6,NG)
              IHBE  = IPARG(23,NG)            
              IGTYP = IPARG(38,NG)
              JJ=JJ+NEL
              NLAY  = ELBUF_TAB(NG)%NLAY
              NPTR  = ELBUF_TAB(NG)%NPTR    
              NPTS  = ELBUF_TAB(NG)%NPTS 
              NPG   = NPTR*NPTS
              MPT=IABS(NPT)
              IF (IGTYP == 51 .OR. IGTYP == 52) THEN
                NPT_ALL = 0
                DO IL=1,NLAY
                  NPT_ALL = NPT_ALL + ELBUF_TAB(NG)%BUFLY(IL)%NPTT
                ENDDO
                MPT = MAX(1,NPT_ALL)
              ENDIF
C
              IF (IHBE >= 11) THEN
C------------------------
C        STRESS
C------------------------
                IF (MLW == 1.OR.MLW == 3.OR.MLW == 23) MPT=0
C---QEPH:------
                IF (IHBE == 23) THEN
                  NPG=4
C---Transfert to QBAT------
                  IF (MPT == 0) THEN
                    JJ = JJ + ((9*NPG)+5)*NEL
                  ELSE
                    JJ = JJ + (6*NPG*MPT+5)*NEL
                  ENDIF
                ELSEIF (IHBE == 11) THEN
C-------QBAT,DKT18-----
                  IF (MPT == 0) THEN
                    JJ = JJ + ((9*NPG)+5)*NEL
                  ELSE
                    JJ = JJ + ((6*NPG*MPT)+5)*NEL
                  ENDIF
                ENDIF ! IF (IHBE == 23)
              ELSE
                IF (MLW == 1.OR.MLW == 3.OR.MLW == 23) THEN
                  JJ = JJ + 16*NEL
                ELSE
                  IF (MPT == 0) THEN 
                    JJ = JJ + 16*NEL
                  ELSE
                    JJ = JJ + (6*MPT+7)*NEL
                  ENDIF
                ENDIF 
              ENDIF ! IF (IHBE >= 11)
            ELSE
C----error message------
            ENDIF ! IF (ITY == 3.OR.ITY == 7)
          ENDDO ! DO NG = NGF, NGL

          WASZ2 = WASZ2+JJ
          NGF = NGL + 1
          SIZ_WRITE_LOC(NSPGROUP+NN) = JJ
        ENDDO ! DO NN=1,NSPGROUP
      ENDIF !IF (OUTP_CT(95) == 1)

C--------

      WASZ3 = 0
      IF (OUTP_CT(95) == 1) THEN
        NGF = 1
        NGL = 0
        DO NN=1,NSPGROUP
         JJ = 0
         NGL = NGL + DD_IAD(ISPMD+1,NN)
         DO NG = NGF, NGL
          ITY   =IPARG(5,NG)
          NEL   =IPARG(2,NG)
          IF(ITY == 3.OR.ITY == 7) THEN
           JJ = JJ + 9*NEL
          ENDIF
         ENDDO
         WASZ3 = WASZ3+JJ
         NGF = NGL + 1
         SIZ_WRITE_LOC(2*NSPGROUP+NN) = JJ
        ENDDO
      END IF


      WASZ4 = 0
      IF (OUTP_CT(96) == 1) THEN
        NGF = 1
        NGL = 0
        DO NN=1,NSPGROUP
         JJ = 0
         NGL = NGL + DD_IAD(ISPMD+1,NN)
         DO NG = NGF, NGL
          NEL   =IPARG(2,NG)
           JJ = JJ + 9*NEL
         ENDDO
         WASZ4 = WASZ4+JJ
         NGF = NGL + 1
         SIZ_WRITE_LOC(3*NSPGROUP+NN) = JJ
        ENDDO
      END IF
      WASZ_95 = MAX(WASZ2,WASZ3)
      WASZ(1) = WASZ1
      WASZ(2) = WASZ_95
      WASZ(3) = WASZ4
      DO NN=1,NSPGROUP
       SIZ_WRITE(NN) = SIZ_WRITE_LOC(NN)
       SIZ_WRITE(NSPGROUP+NN) = MAX(SIZ_WRITE_LOC(NSPGROUP+NN),SIZ_WRITE_LOC(2*NSPGROUP+NN))
       SIZ_WRITE(2*NSPGROUP+NN) = SIZ_WRITE_LOC(3*NSPGROUP+NN)
      ENDDO
      DO NN=1,3
        SIZ_WRITE(3*NSPGROUP+NN) = WASZ(NN)   
      ENDDO
c-----------
      RETURN
      END
