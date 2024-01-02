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
Chd|  R2R_GETDATA                   source/coupling/rad2rad/r2r_getdata.F
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        GET_DISPL_C                   source/coupling/rad2rad/rad2rad_c.c
Chd|        GET_DISPL_SPMD                source/coupling/rad2rad/r2r_getdata.F
Chd|        GET_FORCE_C                   source/coupling/rad2rad/rad2rad_c.c
Chd|        GET_FORCE_NL_C                source/coupling/rad2rad/rad2rad_c.c
Chd|        GET_FORCE_SPMD                source/coupling/rad2rad/r2r_getdata.F
Chd|        SPMD_EXCH_R2R_2               source/mpi/r2r/spmd_r2r.F     
Chd|        SPMD_EXCH_R2R_NL              source/mpi/r2r/spmd_exch_r2r_nl.F
Chd|        SPMD_EXCH_WORK                source/mpi/r2r/spmd_r2r.F     
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        NLOCAL_REG_MOD                ../common_source/modules/nlocal_reg_mod.F
Chd|        RAD2R_MOD                     share/modules/rad2r.F         
Chd|====================================================================
      SUBROUTINE R2R_GETDATA(
     .               IEXLNK  ,IGRNOD  ,X       ,V       ,
     .               VR      ,A       ,AR      ,MS      ,IN      ,
     .               XDP     ,DX      ,R2R_ON  ,DD_R2R  ,WEIGHT  ,
     .               IAD_ELEM,FR_ELEM ,STIFN   , STIFR  , DD_R2R_ELEM,
     .               SDD_R2R_ELEM,NLOC_DMG)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
        USE RAD2R_MOD
        USE GROUPDEF_MOD
        USE NLOCAL_REG_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com06_c.inc"
#include      "com08_c.inc"
#include      "param_c.inc"
#include      "scr05_c.inc"
#include      "scr11_c.inc"
#include      "task_c.inc"
#include      "rad2r_c.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER R2R_ON
        INTEGER IEXLNK(NR2R,NR2RLNK),
     .          WEIGHT(*), DD_R2R(NSPMD+1,*), IAD_ELEM(2,*), FR_ELEM(*),
     .          DD_R2R_ELEM(*),SDD_R2R_ELEM
C     REAL
        my_real
     .     X(3,*),V(3,*),VR(3,*),A(3,*),AR(3,*),MS(*),IN(*),
     .     STIFN(*),STIFR(*),DX(3,*)
C
        DOUBLE PRECISION XDP(3,*)
!
        TYPE (GROUP_)  , TARGET, DIMENSION(NGRNOD)  :: IGRNOD
        TYPE(NLOCAL_STR_), TARGET, INTENT(IN)  :: NLOC_DMG
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER I, IEX, IDP, IDG, NNG, NB,NGLOB,LENR,SIZE,BID
        INTEGER NBD,NL_FLAG,SBUF_SIZE,RBUF_SIZE,PSP
        my_real
     .          WF, WM, WF2, WM2, WFB, WMB, WF2B, WM2B,ANN,VNN,ARN,VRN
C
        INTEGER, DIMENSION(:), POINTER :: GRNOD
        my_real, POINTER, DIMENSION(:) :: MSNL,FNL
C=======================================================================
        WF = ZERO
        WM = ZERO
        WF2= ZERO
        WM2= ZERO
        NL_FLAG = 0

        IF ((R2R_SIU==1).OR.(NSPMD==1)) THEN
C-----------------------------------------------------------------------
          DO IEX = 1, NR2RLNK
            IDG  = IEXLNK(1,IEX)
            IDP  = IEXLNK(2,IEX)
            NNG  = IGRNOD(IDG)%NENTITY
            GRNOD => IGRNOD(IDG)%ENTITY

            IF (NLLNK(IEX)==1) THEN
C-------Non local coupling interface--------------------------
              NL_FLAG = 1
              MSNL  => NLOC_DMG%MASS(1:NLOC_DMG%L_NLOC)
              FNL  => NLOC_DMG%FNL(1:NLOC_DMG%L_NLOC,1)
              CALL GET_FORCE_NL_C(IDP    ,NBDOF_NL(IEX)  ,IADD_NL  ,FNL ,MSNL ,
     .                            IEX)
            ELSE
              CALL GET_FORCE_C(IDP    ,NNG    ,GRNOD  ,WF     ,WM      ,
     .                         WF2    ,WM2    ,V      ,VR     ,A      ,AR      ,
     .                         MS     ,IN     ,X      ,XDP    ,DX     ,TYPLNK(IEX),
     .                         KINLNK(IEX),WEIGHT  ,IEX    ,IRESP, TFEXT)
            ENDIF
C
            IF (R2R_ON == 1)  THEN
              CALL GET_DISPL_C(IDP,NNG,GRNOD,X)
            ENDIF
          END DO

C----------New rad2rad HMPP - synchro SPMD-----------------------------
          IF (NSPMD>1) THEN
            IF (SDD_R2R_ELEM>0) THEN
              IF (NL_FLAG == 0) THEN
                SIZE =  3+FLAG_KINE + IRODDL*(3+FLAG_KINE)
                LENR = IAD_ELEM(1,NSPMD+1)-IAD_ELEM(1,1)
                CALL SPMD_EXCH_R2R_2(A ,AR,V , VR  ,MS   ,IN,
     2                               IAD_ELEM,FR_ELEM,SIZE , WF, WF2,
     3                               LENR    ,DD_R2R,DD_R2R_ELEM,WEIGHT,FLAG_KINE)
              ELSE
                SIZE =  3+FLAG_KINE + IRODDL*(3+FLAG_KINE)
                LENR = IAD_ELEM(1,NSPMD+1)-IAD_ELEM(1,1)
                SBUF_SIZE = SIZE*LENR + DD_R2R_NL(1)
                RBUF_SIZE = SIZE*LENR + DD_R2R_NL(2)
                CALL SPMD_EXCH_R2R_NL(A ,AR,V , VR  ,MS   ,
     2                                IN,IAD_ELEM,FR_ELEM,SIZE ,
     3                                SBUF_SIZE,RBUF_SIZE,WF, WF2,DD_R2R,
     4                                DD_R2R_ELEM,WEIGHT,FLAG_KINE,NLOC_DMG)
              ENDIF
            ENDIF
            CALL SPMD_EXCH_WORK(WF, WF2)
            CALL SPMD_EXCH_WORK(WM, WM2)
          END IF
C
        ELSE
C
          DO IEX = 1, NR2RLNK
            IDG  = IEXLNK(1,IEX)
            IDP  = IEXLNK(2,IEX)
            NNG  = IGRNOD(IDG)%NENTITY
            GRNOD => IGRNOD(IDG)%ENTITY
C-
            WFB = ZERO
            WMB = ZERO
            WF2B= ZERO
            WM2B= ZERO
C-
            IF (ISPMD==0) THEN
              NGLOB=DD_R2R(NSPMD+1,IEX)+DBNO(IEX)
              NB = DBNO(IEX)
            ELSE
              NGLOB=NNG
              NB = 0
            ENDIF
C-
            NB = DBNO(IEX)
            NBD = DD_R2R(NSPMD+1,IEX)

            CALL GET_FORCE_SPMD(
     1        IDP     ,NNG      ,GRNOD,WFB,WMB                   ,
     2        WF2B    ,WM2B     ,V            ,VR,A                    ,
     3        AR     ,MS      ,IN,DD_R2R(1,IEX),NGLOB,
     4        WEIGHT ,IAD_ELEM,FR_ELEM,NB,IEX,TYPLNK(IEX),ROTLNK(IEX),NBD)
C-
            WF = WF + WFB
            WM = WM + WMB
            WF2 = WF2 + WF2B
            WM2 = WM2 + WM2B
            IF (R2R_ON == 1)  THEN
              CALL GET_DISPL_SPMD(
     1          IDP,NNG              ,GRNOD,X       ,DD_R2R(1,IEX),
     2          NGLOB,WEIGHT       ,IAD_ELEM,FR_ELEM,IEX)
C-
            ENDIF
          END DO
C
        END IF

C----- Count the work of external process forces
        IF(IMACH/=3.OR.ISPMD==0) THEN
          TFEXT_MD = TFEXT_MD + R2RFX1 + (WF + WM) * DT1
          R2RFX1 = WF  + WM
          R2RFX2 = WF2 + WM2
        END IF
C
C-----------------------------------------------------------------
        RETURN
      END SUBROUTINE R2R_GETDATA
C
Chd|====================================================================
Chd|  GET_FORCE_SPMD                source/coupling/rad2rad/r2r_getdata.F
Chd|-- called by -----------
Chd|        R2R_GETDATA                   source/coupling/rad2rad/r2r_getdata.F
Chd|-- calls ---------------
Chd|        GET_FORCE_SPMD_C              source/coupling/rad2rad/rad2rad_c.c
Chd|        SPMD_R2R_RSET3                source/mpi/r2r/spmd_r2r.F     
Chd|        SPMD_R2R_RSET3B               source/mpi/r2r/spmd_r2r.F     
Chd|====================================================================
      SUBROUTINE GET_FORCE_SPMD(
     1   IDP    ,NNG     ,GRNOD  ,WF    ,WM    ,
     2   WF2    ,WM2     ,V      ,VR    ,A     ,
     3   AR     ,MS      ,IN     ,DD_R2R ,NGLOB,
     4   WEIGHT ,IAD_ELEM,FR_ELEM,NB,IEX,TYP,FLAG_ROT,NBD)
C----6------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER IDP, NNG, NGLOB, GRNOD(*),IEX,NB,TYP,FLAG_ROT,
     .          WEIGHT(*), DD_R2R(*), IAD_ELEM(2,*), FR_ELEM(*),NBD
C     REAL
        my_real
     .          V(3,*),VR(3,*),A(3,*),AR(3,*),MS(*),IN(*),
     .          WF, WM, WF2, WM2
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER LRBUF,i
        my_real
     .          BUFR1(3,NGLOB),BUFR2(3,NGLOB),
     .          BUFR3(3,NGLOB),BUFR4(3,NGLOB),WTMP(4)
        INTEGER POP0,POP1,RATE
        my_real
     .     POP2,POP3,SECS
C
C******************************************************************************C

        IF(ISPMD==0) THEN
          CALL GET_FORCE_SPMD_C(IDP,NBD,BUFR1,BUFR2,BUFR3,BUFR4,TYP,IEX,NGLOB)
        ENDIF
        LRBUF = 2*4*(IAD_ELEM(1,NSPMD+1)-IAD_ELEM(1,1))+2*NSPMD

        IF (TYP/=7) THEN
          IF(FLAG_ROT /= 0)THEN
            IF(TYP<4)THEN
              CALL SPMD_R2R_RSET3(VR  ,NNG     ,GRNOD,DD_R2R,WEIGHT,
     .                              BUFR4,IAD_ELEM,FR_ELEM,LRBUF,IEX)
            ENDIF
            CALL SPMD_R2R_RSET3B(AR  ,NNG     ,GRNOD,DD_R2R,WEIGHT,
     .                      BUFR2,IAD_ELEM,FR_ELEM,LRBUF, IN, VR, WM, WM2,IEX)
          END IF

          CALL SPMD_R2R_RSET3B(A   ,NNG     ,GRNOD,DD_R2R,WEIGHT,
     .                    BUFR1,IAD_ELEM,FR_ELEM,LRBUF, MS, V,  WF, WF2,IEX)
          IF(TYP<4)THEN
            CALL SPMD_R2R_RSET3(V    ,NNG     ,GRNOD,DD_R2R,WEIGHT,
     .                          BUFR3,IAD_ELEM,FR_ELEM,LRBUF,IEX)
          ENDIF

          WTMP(1) = WF
          WTMP(2) = WF2
          WTMP(3) = WM
          WTMP(4) = WM2
          WF  = WTMP(1)
          WF2 = WTMP(2)
          WM  = WTMP(3)
          WM2 = WTMP(4)
        ENDIF
C-----------------------------------------------------------------
        RETURN
      END SUBROUTINE GET_FORCE_SPMD
C
Chd|====================================================================
Chd|  GET_DISPL_SPMD                source/coupling/rad2rad/r2r_getdata.F
Chd|-- called by -----------
Chd|        R2R_GETDATA                   source/coupling/rad2rad/r2r_getdata.F
Chd|-- calls ---------------
Chd|        GET_DISPL_SPMD_C              source/coupling/rad2rad/rad2rad_c.c
Chd|        SPMD_R2R_RSET3                source/mpi/r2r/spmd_r2r.F     
Chd|====================================================================
      SUBROUTINE GET_DISPL_SPMD(
     1   IDP  ,NNG   ,GRNOD ,X      ,DD_R2R ,
     4   NGLOB,WEIGHT,IAD_ELEM,FR_ELEM,IEX        )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER IDP, NNG, NGLOB,IEX,GRNOD(*),
     .          WEIGHT(*), DD_R2R(*), IAD_ELEM(2,*), FR_ELEM(*)
C     REAL
        my_real
     .          X(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER LRBUF
        my_real
     .          BUFR1(3,NGLOB)
C
C******************************************************************************C
        IF(ISPMD==0)
     .    CALL GET_DISPL_SPMD_C(IDP,NGLOB,BUFR1)
        LRBUF = 2*4*IAD_ELEM(1,NSPMD+1)-IAD_ELEM(1,1)+2*NSPMD
        CALL SPMD_R2R_RSET3(X    ,NNG     ,GRNOD,DD_R2R,WEIGHT,
     .                     BUFR1,IAD_ELEM,FR_ELEM,LRBUF,IEX)

C-----------------------------------------------------------------
        RETURN
      END SUBROUTINE GET_DISPL_SPMD
C
Chd|====================================================================
Chd|  R2R_SENDKINE                  source/coupling/rad2rad/r2r_getdata.F
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        SEND_MASS_KINE_C              source/coupling/rad2rad/rad2rad_c.c
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        RAD2R_MOD                     share/modules/rad2r.F         
Chd|====================================================================
      SUBROUTINE R2R_SENDKINE(
     .               IEXLNK  ,IGRNOD ,MS      ,IN)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
        USE RAD2R_MOD
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
#include      "rad2r_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER IEXLNK(NR2R,NR2RLNK)
        my_real MS(*),IN(*)
!
        TYPE (GROUP_)  , TARGET, DIMENSION(NGRNOD)  :: IGRNOD
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER I, IEX, IDP, IDG, NNG, OFF
C
        INTEGER, DIMENSION(:), POINTER :: GRNOD
C=======================================================================

        FLAG_KINE = 0
        OFF = 0

        IF (R2R_SIU==1) THEN
C----------Send of new mass---------------------------------------
          DO IEX = 1, NR2RLNK
            IDP  = IEXLNK(2,IEX)
            IDG  = IEXLNK(1,IEX)
            NNG  = IGRNOD(IDG)%NENTITY
            GRNOD => IGRNOD(IDG)%ENTITY
            IF ((TYPLNK(IEX)==5).AND.(KINLNK(IEX)==1)) THEN
              FLAG_KINE = 1
              CALL SEND_MASS_KINE_C(IDP,NNG,GRNOD,MS,IN,IEX,OFF)
            ENDIF
            OFF = OFF + NNG
          END DO
        ENDIF

C-----------------------------------------------------------------
        RETURN
      END SUBROUTINE R2R_SENDKINE
