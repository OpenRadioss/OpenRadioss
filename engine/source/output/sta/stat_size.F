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
Chd|  STAT_SIZE_C                   source/output/sta/stat_size.F 
Chd|-- called by -----------
Chd|        GENSTAT                       source/output/sta/genstat.F   
Chd|-- calls ---------------
Chd|        SPMD_GLOB_ISUM9               source/mpi/interfaces/spmd_th.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE STAT_SIZE_C(IPARG ,IXC  ,IXTG ,IGEO ,IPM      ,
     .                       P0ARS ,WASZ ,IXS  ,GEO  ,ELBUF_TAB,
     .                       IXR   ,IXP  ,IXT  )
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
#include      "scr16_c.inc"
#include      "param_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IXC(NIXC,*),IXTG(NIXTG,*),
     .        IPARG(NPARG,*),IPM(NPROPMI,*),IGEO(NPROPGI,*),
     .        WASZ,P0ARS,IXS(NIXS,*),IXR(NIXR,*),IXP(NIXP,*),
     .        IXT(NIXT,*)
      my_real
     .   GEO(NPROPG,*)
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP), TARGET :: ELBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER JJ,NN,ITY,IAD,NFT,LFT,LLT,NPT,NPTM,ISTRAIN,IHBE,ISH3N,
     .        NUVAR,MLW,NG,NEL,I,NPG,MPT,ISROT,
     .        RWASZ,WASZ2,P0ARSZ2,RWASZ2,JHBE,ISOLNOD,
     .        NLAY,NPTR,NPTS,NPTT,NPTG,IGTYP,ICSIG,
     .        NPTS0,NPTR0,NPTT0,NUVARR,NVARF,IL,NPT_ALL,IPROP,
     .        EL_FIX,EL_VAR,ISMSTR,NFAIL
      TYPE(BUF_FAIL_) ,POINTER :: FBUF
C======================================================================|
      P0ARS = 0
      WASZ = 0
C
      IF (STAT_C(1) == 1.OR.STAT_C(2) == 1.OR.STAT_R(1) == 1.OR.
     .    STAT_P(1) == 1.OR.STAT_P(3) == 1.OR.STAT_T(1) == 1) THEN
C       1: off, 2: thk
        WASZ  = 3*MAX(STAT_NUMELC,STAT_NUMELTG,STAT_NUMELR,STAT_NUMELP,
     .                STAT_NUMELT)
        RWASZ = 3*MAX(STAT_NUMELC_G,STAT_NUMELTG_G,STAT_NUMELR_G,STAT_NUMELP_G,
     .                STAT_NUMELT_G)
        IF (ISPMD == 0) P0ARS = RWASZ
      ENDIF
C
      P0ARSZ2 = 0
      WASZ2   = 0
C
      IF (STAT_C(3) == 1) THEN
C------------------------------
C       3: epsp/full
C------------------------------
        DO NG=1,NGROUP
          ITY   = IPARG(5,NG)
          NEL   = IPARG(2,NG)
          IGTYP = IPARG(38,NG)
          IF (ITY == 3 .OR. ITY == 7) THEN
            NPT   = IPARG(6,NG)
            IHBE  = IPARG(23,NG)
            NPTM  = MAX(1,IABS(NPT))
            NLAY  = ELBUF_TAB(NG)%NLAY
            NPTR  = ELBUF_TAB(NG)%NPTR    
            NPTS  = ELBUF_TAB(NG)%NPTS    
c            NPTT  = ELBUF_TAB(NG)%NPTT    
            NPG   = NPTR*NPTS
            IF (ITY==3 .AND. IHBE==23) NPG=4
C
C pre counting of all NPTT (especially for PID_51)
C
            IF (IGTYP == 51 .OR. IGTYP == 52) THEN
              NPT_ALL = 0
              DO IL=1,NLAY
                NPT_ALL = NPT_ALL + ELBUF_TAB(NG)%BUFLY(IL)%NPTT
              ENDDO
              NPTM = MAX(1,NPT_ALL)
            ENDIF
            WASZ2 = WASZ2+(6+NPTM*NPG)*NEL
          ENDIF
        ENDDO
C
        RWASZ2= WASZ2
        IF (NSPMD > 1) CALL SPMD_GLOB_ISUM9(RWASZ2,1)
        IF (ISPMD == 0) P0ARSZ2 = RWASZ2
      ENDIF  !  IF (STAT_C(3) == 1)
C
      P0ARS= MAX (P0ARS,P0ARSZ2)+6
      WASZ = MAX (WASZ,WASZ2)
C
      P0ARSZ2 = 0
      WASZ2   = 0
C
      IF (STAT_C(4) == 1) THEN
C------------------------------
C       4: stress/full
C------------------------------
        DO NG=1,NGROUP
          ITY   = IPARG(5,NG)
          NEL   = IPARG(2,NG)
          IGTYP = IPARG(38,NG)
          IF (ITY == 3 .OR. ITY == 7) THEN
            NPT =IPARG(6,NG)
            MPT =IABS(NPT)
            MLW =IPARG(1,NG)
            IHBE  = IPARG(23,NG)
            NLAY  = ELBUF_TAB(NG)%NLAY
            NPTR  = ELBUF_TAB(NG)%NPTR    
            NPTS  = ELBUF_TAB(NG)%NPTS    
c            NPTT  = ELBUF_TAB(NG)%NPTT    
            NPG   = NPTR*NPTS
            IF (ITY==3.AND.IHBE==23) NPG=4
C
C pre counting of all NPTT (especially for PID_51)
C
            IF (IGTYP == 51 .OR. IGTYP == 52) THEN
              NPT_ALL = 0
              DO IL=1,NLAY
                NPT_ALL = NPT_ALL + ELBUF_TAB(NG)%BUFLY(IL)%NPTT
              ENDDO
              MPT = MAX(1,NPT_ALL)
            ENDIF
            IF (MLW == 1 .OR. MLW == 3 .OR. MLW == 23) MPT=0
C
            WASZ2 = WASZ2 + 5*NEL
            IF (MPT == 0) THEN
              WASZ2 = WASZ2 + (9*NPG+7)*NEL
            ELSE
              WASZ2 = WASZ2 + (6*NPG*MPT+7)*NEL
            ENDIF
          ENDIF
        ENDDO
C
        RWASZ2= WASZ2
        IF (NSPMD > 1) CALL SPMD_GLOB_ISUM9(RWASZ2,1)
        IF (ISPMD == 0) P0ARSZ2 = RWASZ2
      ENDIF  !  IF (STAT_C(4) == 1)
C
      P0ARS= MAX (P0ARS,P0ARSZ2)+6
      WASZ = MAX (WASZ,WASZ2)
C
      P0ARSZ2 = 0
      WASZ2   = 0
C
      IF (STAT_C(10) == 1) THEN
C------------------------------
C       10: stress/globfull
C------------------------------
        DO NG=1,NGROUP
          ITY   = IPARG(5,NG)
          NEL   = IPARG(2,NG)
          IGTYP = IPARG(38,NG)
          IF (ITY == 3 .OR. ITY == 7) THEN
            NPT =IPARG(6,NG)
            MPT =IABS(NPT)
            MLW =IPARG(1,NG)
            IHBE  = IPARG(23,NG)
            NLAY  = ELBUF_TAB(NG)%NLAY
            NPTR  = ELBUF_TAB(NG)%NPTR    
            NPTS  = ELBUF_TAB(NG)%NPTS    
c            NPTT  = ELBUF_TAB(NG)%NPTT    
            NPG   = NPTR*NPTS
            IF (IHBE==23) NPG=4
C
C pre counting of all NPTT (especially for PID_51)
C
              NPT_ALL = 0
              DO IL=1,NLAY
                NPT_ALL = NPT_ALL + ELBUF_TAB(NG)%BUFLY(IL)%NPTT
              ENDDO
              MPT = MAX(1,NPT_ALL)
            IF (MLW == 1 .OR. MLW == 3 .OR. MLW == 23) MPT=0
C
            WASZ2 = WASZ2 + 5*NEL
            IF (MPT == 0) THEN
              WASZ2 = WASZ2 + (13*NPG+7)*NEL
            ELSE
              WASZ2 = WASZ2 + (8*NPG*MPT+7)*NEL
            ENDIF
          ENDIF
        ENDDO
C
        RWASZ2= WASZ2
        IF (NSPMD > 1) CALL SPMD_GLOB_ISUM9(RWASZ2,1)
        IF (ISPMD == 0) P0ARSZ2 = RWASZ2
      ENDIF  !  IF (STAT_C(4) == 1)
C
      P0ARS= MAX (P0ARS,P0ARSZ2)+6
      WASZ = MAX (WASZ,WASZ2)
C
      P0ARSZ2 = 0
      WASZ2   = 0
C
      IF (STAT_C(11) == 1) THEN
C------------------------------
C       1: strain/globfull  shells
C------------------------------
        DO NG=1,NGROUP
         ITY = IPARG(5,NG)
         NEL = IPARG(2,NG)
         IF (ITY == 3.OR.ITY == 7) THEN
           NPT =IPARG(6,NG)
           MPT =IABS(NPT)
           MLW =IPARG(1,NG)
           NLAY  = ELBUF_TAB(NG)%NLAY
              NPT_ALL = 0
              DO IL=1,NLAY
                NPT_ALL = NPT_ALL + ELBUF_TAB(NG)%BUFLY(IL)%NPTT
              ENDDO
              MPT = MAX(1,NPT_ALL)
           IF (NPT==0) MPT=2
C
           IHBE	 =IPARG(23,NG)
           NPG   =IPARG(48,NG)
           IF (IHBE==23) NPG=4
C
           WASZ2 = WASZ2 + (7*NPG*MPT+6)*NEL
         ENDIF
        ENDDO
C
        RWASZ2= WASZ2
        IF (NSPMD > 1) CALL SPMD_GLOB_ISUM9(RWASZ2,1)
        IF (ISPMD == 0) P0ARSZ2 = RWASZ2
      ENDIF
C
      P0ARS= MAX (P0ARS,P0ARSZ2)+6
      WASZ = MAX (WASZ,WASZ2)
C
      P0ARSZ2 = 0
      WASZ2 = 0
C
      IF (STAT_C(5) == 1) THEN
C------------------------------
C       5: strain/full  shells
C------------------------------
        DO NG=1,NGROUP
         ITY = IPARG(5,NG)
         NEL = IPARG(2,NG)
         IF (ITY == 3.OR.ITY == 7) THEN
           NPT =IPARG(6,NG)
           MPT =IABS(NPT)
           MLW =IPARG(1,NG)
           IF (MLW == 1.OR.MLW == 3.OR.MLW == 23) MPT=0
C
           IHBE	 =IPARG(23,NG)
           NPG   =IPARG(48,NG)
CC         IF(ITY==3.AND.IHBE==23) NPG=4
C
           WASZ2 = WASZ2 + 5*NEL
           WASZ2 = WASZ2 + (8*NPG+1)*NEL
         ENDIF
        ENDDO
C
        RWASZ2= WASZ2
        IF (NSPMD > 1) CALL SPMD_GLOB_ISUM9(RWASZ2,1)
        IF (ISPMD == 0) P0ARSZ2 = RWASZ2
      ENDIF
C
      P0ARS= MAX (P0ARS,P0ARSZ2)+6
      WASZ = MAX (WASZ,WASZ2)
C
      P0ARSZ2 = 0
      WASZ2 = 0
      IF (STAT_C(6) == 1) THEN
C------------------------------
C       6: /inishe/aux/full
C------------------------------
        DO NG=1,NGROUP
          ITY = IPARG(5,NG)
          NEL = IPARG(2,NG)
          NFT = IPARG(3,NG)
C
          IF (ITY == 3.OR.ITY == 7) THEN
            LFT=1
            LLT=NEL
            MLW    = IPARG(1,NG)
            NPT    = IPARG(6,NG)
            IHBE   = IPARG(23,NG)
            IGTYP  = IPARG(38,NG)
            ISTRAIN= IPARG(44,NG)
            NLAY = ELBUF_TAB(NG)%NLAY
            NPTR = ELBUF_TAB(NG)%NPTR
            NPTS = ELBUF_TAB(NG)%NPTS
            NPG  = NPTR*NPTS
            IF (MLW == 25.OR.MLW == 27.OR.MLW == 32) ISTRAIN=1
C
            NUVAR = 0
            IF (ITY == 3) THEN
              DO I=LFT,LLT
                NUVAR = MAX(NUVAR,IPM(8,IXC(1,I+NFT)))
              ENDDO
            ELSE
              DO I=LFT,LLT
                NUVAR = MAX(NUVAR,IPM(8,IXTG(1,I+NFT)))
              ENDDO
            ENDIF
            IF (MLW == 78) THEN
              NUVAR = NUVAR + 18 ! backstress will be printed in /AUX
            ELSEIF (MLW == 87) THEN
              NUVAR = ELBUF_TAB(NG)%BUFLY(1)%NVAR_MAT + 12 ! backstress will be printed in /AUX
            ELSE IF (MLW == 36) THEN
              NUVAR = 6  ! backstress will be printed in /AUX
            END IF
C
            MPT = IABS(NPT)
C
C pre counting of all NPTT (especially for PID_51)
C
            IF (IGTYP == 51 .OR. IGTYP == 52) THEN
              NPT_ALL = 0
              DO IL=1,NLAY
                NPT_ALL = NPT_ALL + ELBUF_TAB(NG)%BUFLY(IL)%NPTT
              ENDDO
              MPT = MAX(1,NPT_ALL)
            ENDIF
C
            NPTM = MAX(1,MPT)
            WASZ2 = WASZ2 + NEL*(6+NPG*NPTM*NUVAR)
          ENDIF
        ENDDO
C
        RWASZ2= WASZ2
        IF (NSPMD > 1) CALL SPMD_GLOB_ISUM9(RWASZ2,1)
        IF (ISPMD == 0) P0ARSZ2 = RWASZ2
      ENDIF
C
      P0ARS= MAX (P0ARS,P0ARSZ2)+6
      WASZ = MAX (WASZ,WASZ2)
C
      P0ARSZ2 = 0
      WASZ2 = 0
C
      IF (STAT_C(7) == 1) THEN
C------------------------------
C       7: orth_loc    shells
C------------------------------
        DO NG=1,NGROUP
          ITY = IPARG(5,NG)
          NEL = IPARG(2,NG)
          NFT = IPARG(3,NG)
C
          IF (ITY == 3.OR.ITY == 7) THEN
            LFT=1
            LLT=NEL
            NPT    =IPARG(6,NG)
            ISTRAIN=IPARG(44,NG)
            IHBE   =IPARG(23,NG)
cc            WASZ2 = WASZ2 + NEL*(8+NPT*4)
            WASZ2 = WASZ2 + NEL*(9+NPT*5)
          END IF
        ENDDO
C
        RWASZ2= WASZ2
        IF (NSPMD > 1) CALL SPMD_GLOB_ISUM9(RWASZ2,1)
        IF (ISPMD == 0) P0ARSZ2 = RWASZ2
      ENDIF
C
      P0ARS= MAX (P0ARS,P0ARSZ2)+6
      WASZ = MAX (WASZ,WASZ2)
C
      P0ARSZ2 = 0
      WASZ2 = 0
C
      IF (STAT_C(8) == 1) THEN
C------------------------------
C       8: fail
C------------------------------
        DO NG=1,NGROUP
          ITY	=IPARG(5,NG)
          NEL	=IPARG(2,NG)
          NFT	=IPARG(3,NG)

          IF (ITY == 3.OR.ITY == 7) THEN
            LFT=1
            LLT=NEL
            MLW    = IPARG(1,NG)
            NPT    = IPARG(6,NG)
            IGTYP  = IPARG(38,NG)
            ISTRAIN= IPARG(44,NG)
            IHBE   = IPARG(23,NG)
            NPTR = ELBUF_TAB(NG)%NPTR
            NPTS = ELBUF_TAB(NG)%NPTS
            NPG  = NPTR*NPTS
            IF (MLW == 25.OR.MLW  == 27.OR.MLW == 32) ISTRAIN=1
C
            NUVARR   = 0
            IF (ITY == 3) THEN
              DO I=LFT,LLT
                NUVARR  = MAX(NUVARR,IPM(221,IXC(1,I+NFT)) + 1) 
              ENDDO
            ELSE
              DO I=LFT,LLT
                NUVARR  = MAX(NUVARR,IPM(221,IXTG(1,I+NFT)) + 1) 
              ENDDO
            ENDIF
C
            MPT=IABS(NPT)
C
C
C pre counting of all NPTT (especially for PID_51)
C
            IF (IGTYP == 51 .OR. IGTYP == 52) THEN
              NPT_ALL = 0
              DO IL=1,NLAY
                NPT_ALL = NPT_ALL + ELBUF_TAB(NG)%BUFLY(IL)%NPTT
              ENDDO
              MPT = MAX(1,NPT_ALL)
            ENDIF
C
            NPTM=MAX(1,MPT)
            WASZ2 = WASZ2 + NEL*(7 + NPG*(3+NPT * MAX(1,NUVARR)* 15))
          END IF
        ENDDO
C
        RWASZ2= WASZ2
        IF (NSPMD > 1) CALL SPMD_GLOB_ISUM9(RWASZ2,1)
        IF (ISPMD == 0) P0ARSZ2 = RWASZ2
      ENDIF
C
      P0ARS= MAX (P0ARS,P0ARSZ2)+6
      WASZ = MAX (WASZ,WASZ2)
C
      P0ARSZ2 = 0
      WASZ2 = 0
C------------------------------
C       8: /inibri/stres/full
C------------------------------
      IF (STAT_S(4)==1 .OR. STAT_S(8)==1) THEN
        DO NG=1,NGROUP
          ITY	   =IPARG(5,NG)
          IF(ITY==1) THEN
            NEL	   =IPARG(2,NG)
            NLAY = ELBUF_TAB(NG)%NLAY
            NPTR = ELBUF_TAB(NG)%NPTR
            NPTS = ELBUF_TAB(NG)%NPTS
            NPTT = ELBUF_TAB(NG)%NPTT
            NPT  = NLAY*NPTR*NPTS*NPTT
c
            WASZ2 = WASZ2 + NEL * (12 + 9 * NPT)
          ENDIF
        ENDDO
        RWASZ2= WASZ2
        IF(NSPMD > 1)CALL SPMD_GLOB_ISUM9(RWASZ2,1)
        IF (ISPMD== 0) P0ARSZ2 = RWASZ2
      ENDIF

      P0ARS= MAX (P0ARS,P0ARSZ2)+8
      WASZ = MAX (WASZ,WASZ2)
      P0ARSZ2 = 0
      WASZ2 = 0
C------------------------------
C       9: /inibri/strai/full
C------------------------------
      IF (STAT_S(5)==1 .OR. STAT_S(9)==1) THEN
        DO NG=1,NGROUP
          ITY	 =IPARG(5,NG)
          IF(ITY==1) THEN
            NEL	 =IPARG(2,NG)
            NLAY = ELBUF_TAB(NG)%NLAY
            NPTR = ELBUF_TAB(NG)%NPTR
            NPTS = ELBUF_TAB(NG)%NPTS
            NPTT = ELBUF_TAB(NG)%NPTT
            NPT  = NLAY*NPTR*NPTS*NPTT
c
            WASZ2 = WASZ2 + NEL * (11 + 6 * NPT)
          ENDIF
        ENDDO
        RWASZ2= WASZ2
        IF(NSPMD > 1)CALL SPMD_GLOB_ISUM9(RWASZ2,1)
        IF (ISPMD==0 ) P0ARSZ2 = RWASZ2
      ENDIF

      P0ARS= MAX (P0ARS,P0ARSZ2)+6
      WASZ = MAX (WASZ,WASZ2)
      P0ARSZ2 = 0
      WASZ2 = 0
      NUVAR = 0
C------------------------------
C      10: /inibri/aux/full
C------------------------------
      IF (STAT_S(6)==1) THEN
        DO NG=1,NGROUP
          ITY =IPARG(5,NG)
          IF(ITY==1) THEN
            NEL	 =IPARG(2,NG)
            NFT	 =IPARG(3,NG)
            NLAY = ELBUF_TAB(NG)%NLAY
            NPTR = ELBUF_TAB(NG)%NPTR
            NPTS = ELBUF_TAB(NG)%NPTS
            NPTT = ELBUF_TAB(NG)%NPTT
            NPT  = NLAY*NPTR*NPTS*NPTT
c
            DO I=1,NEL
              NUVAR  = MAX(NUVAR,IPM(8,IXS(1,I+NFT)))
            ENDDO
            WASZ2 = WASZ2 + NEL * (11 + NPT * NUVAR)
          ENDIF
        ENDDO
        RWASZ2= WASZ2
        IF(NSPMD > 1)CALL SPMD_GLOB_ISUM9(RWASZ2,1)
        IF (ISPMD==0 ) P0ARSZ2 = RWASZ2
      ENDIF

      P0ARS= MAX (P0ARS,P0ARSZ2)+6
      WASZ = MAX (WASZ,WASZ2)

      P0ARSZ2 = 0
      WASZ2   = 0
C------------------------------
C      11: /inibri/ortho
C------------------------------
      IF (STAT_S(7)==1.OR.STAT_S(10)==1) THEN
        DO NG=1,NGROUP
          ITY =IPARG(5,NG)
          IF(ITY==1) THEN
            NEL	 =IPARG(2,NG)
            NLAY = ELBUF_TAB(NG)%NLAY
            NPTR = ELBUF_TAB(NG)%NPTR
            NPTS = ELBUF_TAB(NG)%NPTS
            NPTT = ELBUF_TAB(NG)%NPTT
            NPT  = NLAY*NPTR*NPTS*NPTT
c
            WASZ2 = WASZ2 + NEL * (11 + NLAY * 6)
          ENDIF
        ENDDO
        RWASZ2= WASZ2
        IF(NSPMD > 1)CALL SPMD_GLOB_ISUM9(RWASZ2,1)
        IF (ISPMD ==0) P0ARSZ2 = RWASZ2
      ENDIF

      P0ARS= MAX (P0ARS,P0ARSZ2)+14
      WASZ = MAX (WASZ,WASZ2)

      P0ARSZ2 = 0
      WASZ2   = 0
C------------------------------
C      12: /inibri/fail
C------------------------------
      IF (STAT_S(11)==1) THEN
        NVARF = 0
        NFAIL = 0
        DO NG=1,NGROUP
         NFAIL = MAX(NFAIL,ELBUF_TAB(NG)%BUFLY(1)%NFAIL)
        ENDDO
!
        IF (NFAIL > 0) THEN
          DO NG=1,NGROUP
            FBUF => ELBUF_TAB(NG)%BUFLY(1)%FAIL(1,1,1)
            NVARF  = MAX(NVARF,FBUF%FLOC(1)%NVAR) 
          ENDDO
        ENDIF ! IF (NFAIL > 0)
!
        DO NG=1,NGROUP
          ITY =IPARG(5,NG)
          IF(ITY==1) THEN
            NEL	 =IPARG(2,NG)
            NFT	 =IPARG(3,NG)
            NLAY = ELBUF_TAB(NG)%NLAY
            NPTR = ELBUF_TAB(NG)%NPTR
            NPTS = ELBUF_TAB(NG)%NPTS
            NPTT = ELBUF_TAB(NG)%NPTT
            NPT  = NLAY*NPTR*NPTS*NPTT
c
            WASZ2 = WASZ2 + NEL * (10 + 5 * (NPT * (NVARF+1) + 4) )
          ENDIF
        ENDDO
        RWASZ2= WASZ2
        IF(NSPMD > 1)CALL SPMD_GLOB_ISUM9(RWASZ2,1)
        IF (ISPMD==0 ) P0ARSZ2 = RWASZ2
      ENDIF

      P0ARS= MAX (P0ARS,P0ARSZ2)+6
      WASZ = MAX (WASZ,WASZ2)

      P0ARSZ2 = 0
      WASZ2   = 0
C------------------------------
C      13: /state/spring/full
C------------------------------
      IF (STAT_R(1) == 1) THEN
        DO NG=1,NGROUP
          EL_VAR = 0
          ITY = IPARG(5,NG)
          IF (ITY == 6) THEN
            NEL = IPARG(2,NG)
            NFT = IPARG(3,NG)
            IPROP = IXR(1,NFT+1)
            IGTYP = IGEO(11,IPROP)
            EL_FIX = 5
C
            IF (IGTYP == 4) THEN
              EL_VAR = 7 + EL_FIX
            ELSEIF (IGTYP == 12) THEN
              EL_VAR = 8 + EL_FIX
            ELSEIF (IGTYP == 8 .OR. IGTYP == 13 .OR. IGTYP == 25
     .                                          .OR. IGTYP == 23 ) THEN
              EL_VAR = 40 + EL_FIX
            ELSEIF (IGTYP == 26) THEN
              EL_VAR = 6 + EL_FIX
            ELSEIF (IGTYP == 29 .OR. IGTYP == 30 .OR. IGTYP == 31 .OR.
     .              IGTYP == 32 .OR. IGTYP == 33 .OR. IGTYP == 35 .OR.
     .              IGTYP == 36 .OR. IGTYP == 44 .OR. IGTYP == 45 .OR.
     .              IGTYP == 46) THEN
              EL_VAR = 13 + EL_FIX
!!              IF (IGTYP /= 32 .AND. IGTYP /= 33 .AND. IGTYP /= 45) THEN
!!                EL_VAR = EL_VAR + 2  !     MOM1Y, MOM1Z
!!              ENDIF
              NUVAR = NINT(GEO(25,IPROP))         
              EL_VAR = EL_VAR + NUVAR
            ENDIF ! IF(IGTYP)
C
            WASZ2 = WASZ2 + NEL * EL_VAR
          ENDIF ! IF (ITY)
        ENDDO
        RWASZ2= WASZ2
        IF (NSPMD > 1) CALL SPMD_GLOB_ISUM9(RWASZ2,1)
        IF (ISPMD == 0) P0ARSZ2 = RWASZ2
      ENDIF

      P0ARS= MAX (P0ARS,P0ARSZ2)+6
      WASZ = MAX (WASZ,WASZ2)

      P0ARSZ2 = 0
      WASZ2   = 0
C------------------------------
C      14: /state/beam/full
C------------------------------
      IF (STAT_P(1) == 1) THEN
        DO NG=1,NGROUP
          EL_VAR = 0
          ITY = IPARG(5,NG)
          NPT = IPARG(6,NG)
          IF (ITY == 5) THEN
            NEL = IPARG(2,NG)
            NFT = IPARG(3,NG)
            IPROP = IXP(5,NFT+1)
            IGTYP = IGEO(11,IPROP)
            EL_FIX = 13
!
            IF (IGTYP == 3) THEN
              EL_VAR = 1 + EL_FIX
            ELSEIF (IGTYP == 18) THEN
              EL_VAR = 4*NPT + EL_FIX
              NUVAR = IPARG(46,NG)         
              EL_VAR = EL_VAR + NUVAR
            ENDIF ! IF(IGTYP)
!
            WASZ2 = WASZ2 + NEL * EL_VAR
          ENDIF ! IF (ITY)
        ENDDO
        RWASZ2= WASZ2
        IF (NSPMD > 1) CALL SPMD_GLOB_ISUM9(RWASZ2,1)
        IF (ISPMD == 0) P0ARSZ2 = RWASZ2
      ENDIF ! IF (STAT_P(1) == 1)
!
      P0ARS= MAX (P0ARS,P0ARSZ2)+6
      WASZ = MAX (WASZ,WASZ2)
!
      P0ARSZ2 = 0
      WASZ2   = 0
C------------------------------
C      15: /state/beam/aux
C------------------------------
      IF (STAT_P(3) == 1) THEN
        DO NG=1,NGROUP
          ITY = IPARG(5,NG)
          NEL = IPARG(2,NG)
          NFT = IPARG(3,NG)
          IF (ITY == 5) THEN
            LFT=1
            LLT=NEL
            MLW    = IPARG(1,NG)
            NPT    = IPARG(6,NG)
            IGTYP  = IPARG(38,NG)
!
            NUVAR = 0
            IF (IGTYP == 18 .AND. MLW == 36) THEN
              DO I=LFT,LLT
                NUVAR = MAX(NUVAR,IPM(8,IXP(1,I+NFT)))
              ENDDO
            ENDIF
!
            WASZ2 = WASZ2 + NEL*(6+NPT*NUVAR)
          ENDIF
        ENDDO
!
        RWASZ2= WASZ2
        IF (NSPMD > 1) CALL SPMD_GLOB_ISUM9(RWASZ2,1)
        IF (ISPMD == 0) P0ARSZ2 = RWASZ2
      ENDIF ! IF (STAT_P(3) == 1)
!
      P0ARS= MAX (P0ARS,P0ARSZ2)+6
      WASZ = MAX (WASZ,WASZ2)
!
      P0ARSZ2 = 0
      WASZ2 = 0
C------------------------------
C      16: /state/truss/full
C------------------------------
      IF (STAT_T(1) == 1) THEN
        DO NG=1,NGROUP
          EL_FIX = 0
          ITY = IPARG(5,NG)
          IF (ITY == 4) THEN
            NEL = IPARG(2,NG)
!
            EL_FIX = 8
!
            WASZ2 = WASZ2 + NEL * EL_FIX
          ENDIF ! IF (ITY)
        ENDDO
        RWASZ2= WASZ2
        IF (NSPMD > 1) CALL SPMD_GLOB_ISUM9(RWASZ2,1)
        IF (ISPMD == 0) P0ARSZ2 = RWASZ2
      ENDIF ! IF (STAT_T(1) == 1)
!
      P0ARS= MAX (P0ARS,P0ARSZ2)+6
      WASZ = MAX (WASZ,WASZ2)
!
      P0ARSZ2 = 0
      WASZ2   = 0
C------------------------------
C      17: /inibri/eref
C------------------------------
      IF (STAT_S(13)==1) THEN
        DO NG=1,NGROUP
          ITY	 =IPARG(5,NG)
          ISOLNOD=IPARG(28,NG)
          ISMSTR =IPARG(9,NG)
          ISROT  =IPARG(41,NG)
          IF(ITY==1) THEN
            NEL	 =IPARG(2,NG)
c
            WASZ2 = WASZ2 + NEL * 7
            IF (ISMSTR==1.OR.ISMSTR>=10) THEN
              WASZ2 = WASZ2 + NEL * ISOLNOD*3
              IF (ISOLNOD==4 .AND. ISROT>0) WASZ2 = WASZ2 + NEL * ISOLNOD*10
            END IF
          ENDIF
        ENDDO
        RWASZ2= WASZ2
        IF(NSPMD > 1)CALL SPMD_GLOB_ISUM9(RWASZ2,1)
        IF (ISPMD==0 ) P0ARSZ2 = RWASZ2
      ENDIF

      P0ARS= MAX (P0ARS,P0ARSZ2)+6
      WASZ = MAX (WASZ,WASZ2)
      P0ARSZ2 = 0
      WASZ2 = 0
!-----------
      RETURN
      END
