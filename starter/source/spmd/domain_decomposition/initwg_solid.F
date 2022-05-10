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
Chd|  INITWG_SOLID                  source/spmd/domain_decomposition/initwg_solid.F
Chd|-- called by -----------
Chd|        INITWG                        source/spmd/domain_decomposition/initwg.F
Chd|-- calls ---------------
Chd|        BIDON                         source/system/machine.F       
Chd|        DDWEIGHTS_MOD                 share/modules1/ddweights_mod.F
Chd|        MID_PID_MOD                   share/modules1/mid_pid_mod.F  
Chd|====================================================================
      SUBROUTINE INITWG_SOLID(WD,PM,GEO,IXS,IGEO,ISOLNOD,
     .            NUMELS,IPM, SIZE_IRUP,
     .            NUMMAT,NUMGEO,
     .            POIN_PART_SOL,MID_PID_SOL,IPARTS,BUFMAT,
     .            MID_OLD,PID_OLD,MLN_OLD,RECHERCHE,ISOL_OLD,
     .            TELT_PRO,TABMP_L,NPART)
C-----------------------------------------------
C            M o d u l e s
C-----------------------------------------------
      USE DDWEIGHTS_MOD
      USE MID_PID_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com01_c.inc"
#include      "com_xfem1.inc"
#include      "scr21_c.inc"
#include      "scr05_c.inc"
#include      "tablen_c.inc"
#include      "scr23_c.inc"
#include      "spmd_c.inc" 
#include      "ddspmd_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  
     .  NUMELS,
     .  NUMMAT,NUMGEO,
     .  IXS(NIXS,*),IGEO(NPROPGI,NUMGEO),ISOLNOD(*),
     .  IPM(NPROPMI,*),TABMP_L,NPART
      INTEGER, INTENT(IN) :: SIZE_IRUP
C     REAL OU REAL*8
      my_real
     .    PM(NPROPM,*), GEO(NPROPG,*),BUFMAT(*)
      REAL WD(*)
      INTEGER MID_OLD,PID_OLD,MLN_OLD,RECHERCHE,ISOL_OLD
      my_real TELT_PRO

      INTEGER, DIMENSION(2,NPART,*), INTENT(IN) :: POIN_PART_SOL
      INTEGER, DIMENSION(*), INTENT(IN) :: IPARTS
      TYPE(MID_PID_TYPE), DIMENSION(NUMMAT,*), INTENT(INOUT) :: MID_PID_SOL
C-----------------------------------------------
      INTEGER OFF, NPN, MID, PID, JHBE, IGT, MLN,
     .    ISTRAIN, ITHK, IHBE, IPLA, ISSN, MTN, I, J, K,L,
     .    NFUNC,MPT,NPTS,NPTT,NPTR,NPTOT,IFLAG,JSROT,
     .    I_MID,I_PID,I_MID_OLD,I_PID_OLD,PUID,MUID,
     .    ELM_TYP,ELM_TYP_OLD,ILAW,ILAW_OLD,TEST_MAT,
     .    I_PRO,ISOL2,MUID_OLD,PUID_OLD,
     .    TEST,NFUNC1,NFUNC2,IRUP,IRUP2,
     .    ISOL,INDI,IAD,INDI2,MULT
      INTEGER :: INDI3,ADD_OPTION,INDI_OPT_1,INDI_OPT_2
      INTEGER :: IRUP_TAB(SIZE_IRUP)
      my_real :: OPT_1,OPT_2
     
      REAL
     .   WTYPE(9),FWIHBE,FAC8,
     .   TABMAT(3),TABX(3),TIMMAT,NPT,TELT,POIDS,W,
     .   BATOZMULT,TMAT,TRUP,TMATADD,WD_LOCAL
      INTEGER :: FLAG_NICE_NEWTON,FLAG_GURSON,FLAG_NON_LOCAL
      INTEGER :: SPECIAL_OPTION,SPE_I_1,SPE_I_2,SPE_I_3
      my_real
     .        INVTREF,MULT_SPE
      INTEGER :: INDI4,POIN_PID,POIN_MID,POIN_PART,COST_CHECK,POIN_ELM_TYP

      my_real INVTELT_PRO
      
      my_real
     .        CC,A,B,A1,A2
       DATA WTYPE /1.6 ,1. ,1. ,.9 ,1.1 ,1.4 ,0.65 ,.9 ,2.0/     
C-----------------------------------------------
       CALL BIDON()
!     DD_OPTIMIZATION = 0 --> default case, DD optimized for Broadwell processor
!     DD_OPTIMIZATION = 1 --> DD optimized for Skylake processor 
!     DD_OPTIMIZATION = 2 --> DD optimized for Sandy Bridge processor   
!     DD_OPTIMIZATION = 3 --> default case for ARM processor, DD optimized for ThunderX2 processor (ARM)    
      IF(DD_OPTIMIZATION==1) THEN
!       Skylake processor 
#include "weights_p4linux964_spmd_avx512.inc"
      ELSEIF(DD_OPTIMIZATION==2) THEN
!       Sandy Bridge processor   
#include "weights_p4linux964_spmd_sse3.inc"
      ELSEIF(DD_OPTIMIZATION==3) THEN
!       ThunderX2 processor (ARMV8.0)      
#include "weights_p4linuxa964_spmd.inc"
      ELSE
!       DEFAULT CASE
#if ARCH_CPU
!       ThunderX2 processor (ARMV8.0)      
#include "weights_p4linuxa964_spmd.inc"
#elif 1
!       Broadwell processor
#include "weights_p4linux964_spmd.inc"
#endif
      ENDIF
       INVTREF = UN/TPSREF
       DO I = 1, NUMELS
C -------------------------------
C Element Property initialization
C -------------------------------
        NPN = 1
        JHBE=IHBE_D
        MID= IXS(1,I)
        PID= IXS(10,I)
        
        MLN = NINT(PM(19,ABS(MID)))
        ISOL = ISOLNOD(I)
        WD_LOCAL = WD(I)
        ! -----------------
        IF(RECHERCHE==1) THEN
         MID = MID_OLD
         PID = PID_OLD
         MLN = MLN_OLD
         ISOL = ISOL_OLD
         WD_LOCAL = ZERO
        ENDIF
        ! ----------------- 
        IF(ISOL==8) THEN
          INDI3 = 3
        ELSEIF(ISOL==10) THEN
          INDI3 = 4
        ELSEIF(ISOL==16) THEN
          INDI3 = 5
        ELSEIF(ISOL==20) THEN
          INDI3 = 6
        ELSEIF(ISOL==6) THEN
          INDI3 = 7
        ELSEIF(ISOL==4) THEN
          INDI3 = 8
        ELSE
          INDI3 = 9
        ENDIF
        ! -----------------
        IF (PID.NE.0) THEN
         JHBE  = IGEO(10,PID)
         IGT   = IGEO(11,PID)
         NPN   = IGEO(4,PID)
         JSROT = IGEO(20,PID)
        ENDIF
        IRUP = IPM(220,ABS(MID))
        IRUP_TAB(1:IRUP) = 0
        IF(IRUP/=0) THEN          ! up to 6 failure models per material
         DO J=1,IRUP
          IRUP_TAB(J) = IPM(111+15*(J-1),ABS(MID))
         ENDDO
        ENDIF
        TMAT = 0.
        TRUP = 0.
        TMATADD = 0.
        OPT_1 = ZERO
        OPT_2 = ZERO
        ADD_OPTION = 0
        MULT = 0 
        FLAG_NON_LOCAL = 0  
        SPECIAL_OPTION = 0
        SPE_I_1 = 1
        SPE_I_2 = 1
        IF((MLN<28).OR.(MLN==49).OR.(MLN==59)) THEN
         IRUP2 = 1
        ELSE
         IRUP2 = 2
        ENDIF
        IF (MLN.EQ.2) THEN
           CC = PM(43,MID)
           IF (CC.NE.0) THEN
            INDI = 2
           ELSE
            INDI = 1
           ENDIF
        ELSEIF (MLN == 36)THEN
           NFUNC = MAX(IPM(10,MID) - 3,1)
           IF (NFUNC.LE.2) THEN
            INDI = 1
           ELSEIF (NFUNC.GT.2.AND.NFUNC.LE.7) THEN
            INDI = 2
           ELSEIF (NFUNC.GT.7) THEN
            INDI = 3
           ENDIF
        ELSEIF (MLN==33) THEN
           NFUNC1 = IPM(11,MID)
           NFUNC2 = IPM(12,MID)
           IF((NFUNC1/=0).OR.(NFUNC2/=0)) THEN
            INDI = 2
           ELSE
            INDI = 1
           ENDIF
        ELSEIF((MLN==42).OR.(MLN==62).OR.(MLN==69)) THEN
!       check the NPRONY model
           IAD=IPM(7,ABS(MID))-1
           IF(MLN==42) NFUNC = NINT(BUFMAT(IAD+16))
           IF(MLN==62) NFUNC = NINT(BUFMAT(IAD+3))
           IF(MLN==69) THEN
            NFUNC = 0
            IF(IPM(222,ABS(MID)) > 0) THEN 
             IAD = IPM(223,ABS(MID))
             NFUNC =  NINT(BUFMAT(IAD +1 ))
            ENDIF
           ENDIF
           IF(NFUNC==0) THEN
            INDI = 1
           ELSEIF(NFUNC==1) THEN
            INDI = 2
           ELSEIF(NFUNC==2) THEN
            INDI = 3
           ELSEIF(NFUNC>2) THEN
            INDI = 3
            MULT = NFUNC - 2
            INDI2 = 2            
           ENDIF
        ELSEIF((MLN==82)) THEN
           IAD=IPM(7,ABS(MID))-1
           NFUNC=NINT(BUFMAT(IAD+1))
           IF(NFUNC<=1) THEN
            INDI = 1
           ELSEIF(NFUNC==2) THEN
            INDI = 2
           ELSEIF(NFUNC==3) THEN
            INDI = 3
           ELSEIF(NFUNC>3) THEN
            INDI = 3
            MULT = NFUNC - 3
            INDI2 = 2            
           ENDIF
        ELSEIF(MLN==100) THEN
        !       SPECIAL TREATMENT :
        !       LAW 100 : material cost = base cost + viscosity cost + N * network cost
        !                                              (optionnal)        (optionnal)
        !                                   INDI        INDI_OPT_1         INDI_OPT_2
           INDI=1
           IAD=IPM(7,ABS(MID))-1

           ADD_OPTION = 0
           OPT_1 = ZERO
           INDI_OPT_1 = 2
           OPT_2 = ZERO   
           INDI_OPT_2 = 2

        !  viscosity flag
           IF(NINT(BUFMAT(IAD+5))>0) THEN
                OPT_1 = UN
                INDI_OPT_1 = 2
                ADD_OPTION = 1
           ENDIF
        !  network flag
           IF(NINT(BUFMAT(IAD+1))>0) THEN
                OPT_2 = NINT(BUFMAT(IAD+1))         
                INDI_OPT_2 = 3
                ADD_OPTION = 1
                !   if network is used, then, viscosity is also used
                OPT_1 = UN
                INDI_OPT_1 = 2
                ADD_OPTION = 1
           ENDIF
        ELSEIF(MLN==104) THEN
           IAD=IPM(7,ABS(MID))-1
           FLAG_NICE_NEWTON=NINT(BUFMAT(IAD+11))
           IF(FLAG_NICE_NEWTON==2) THEN !   Newtow algo
             INDI = 2
           ELSE     !   Nice algo
             INDI = 1
           ENDIF
           FLAG_GURSON=NINT(BUFMAT(IAD+30))
           IF(FLAG_GURSON/=0) THEN
             SPECIAL_OPTION=1
             SPE_I_1 = 1
             SPE_I_2 = 1
           ENDIF
           IF(FLAG_GURSON==1) THEN
             SPE_I_2 = 1
           ELSEIF(FLAG_GURSON==2) THEN
             SPE_I_2 = 2
           ELSEIF(FLAG_GURSON==3) THEN
             SPE_I_2 = 3
           ENDIF                
           FLAG_NON_LOCAL = IPM(253,ABS(MID))
        ELSE
           INDI = 1
        ENDIF
        MULT_SPE = 0.
        SPE_I_3 = 1
        IF(FLAG_NON_LOCAL/=0) THEN
            SPE_I_3 = 1
            MULT_SPE = 1.
        ENDIF
        COST_CHECK = 0
!****************************************
!       ---------------------------
!       TETRA 4
!       ---------------------------       
        IF (ISOL.EQ.4.AND. (JSROT .NE. 1)) THEN
        ! check if the (mid,pid) cost must be initialized from a previous run
         IF(RECHERCHE==0.AND.TEST_POIDS/=0) THEN
           POIN_PART = IPARTS(I)
           POIN_MID = POIN_PART_SOL(1,POIN_PART,6)
           POIN_PID = POIN_PART_SOL(2,POIN_PART,6)
           IF(POIN_MID/=0.AND.POIN_PID/=0) THEN
            IF(MID_PID_SOL(POIN_MID,6)%COST1D(POIN_PID)/=ZERO) THEN
                COST_CHECK = 1
                POIN_ELM_TYP = 6
                TELT = MID_PID_SOL(POIN_MID,POIN_ELM_TYP)%COST1D(POIN_PID)
            ENDIF
          ENDIF
         ENDIF
        ! the (mid,pid) cost must be initialized from .inc file
         IF(COST_CHECK==0) THEN
           IF( DDWEIGHTS(1,1,IABS(MID))/=0)THEN
            TMAT = DDWEIGHTS(1,1,IABS(MID)) * TPSREF
           ELSE
              IF(MULT/=0) TMATADD = MULT * (TET4TNL(MLN,INDI)-TET4TNL(MLN,INDI2))
              IF(ADD_OPTION/=0) TMATADD = OPT_1 * TET4TNL(MLN,INDI_OPT_1) + OPT_2 * TET4TNL(MLN,INDI_OPT_2)
              IF(SPECIAL_OPTION/=0) TMATADD = TMATADD + SOL_OPTION(SPE_I_1,SPE_I_2) 
              TMAT = TET4TNL(MLN,INDI) + TMATADD
           ENDIF
!          --------------
!          Failure
           IF(IRUP/=0) THEN
            DO J=1,IRUP
             TRUP = TRUP + RUPTURE_TET4(IRUP_TAB(J),IRUP2)
            ENDDO
           ENDIF
!          --------------            
           TELT = TMAT + TET4TELT(1)  + TRUP + MULT_SPE*NLOCAL_OPTION(SPE_I_3)
         ENDIF
!****************************************
!       ---------------------------
!       TETRA 10 or TETRA4 + JSROT
!       ---------------------------
        ELSEIF ((ISOL.EQ.10).OR.(ISOL.EQ.4.AND. JSROT==1)) THEN
        ! check if the (mid,pid) cost must be initialized from a previous run
         IF(RECHERCHE==0.AND.TEST_POIDS/=0) THEN
           IF(ISOL.EQ.10) THEN
            POIN_PART = IPARTS(I)
            POIN_MID = POIN_PART_SOL(1,POIN_PART,2)
            POIN_PID = POIN_PART_SOL(2,POIN_PART,2)
        ! if POIN_MID==0 and POIN_PID == 0, the element cost in the .ddw file is 0 --> must be initialized 
        ! from the .inc file 
            IF(POIN_MID/=0.AND.POIN_PID/=0) THEN
             IF(MID_PID_SOL(POIN_MID,2)%COST1D(POIN_PID)/=ZERO) THEN
                 COST_CHECK = 1
                 POIN_ELM_TYP = 2
                 TELT = MID_PID_SOL(POIN_MID,POIN_ELM_TYP)%COST1D(POIN_PID)
             ENDIF
            ENDIF
           ELSEIF(ISOL.EQ.4.AND. JSROT==1) THEN
            POIN_PART = IPARTS(I)
            POIN_MID = POIN_PART_SOL(1,POIN_PART,6)
            POIN_PID = POIN_PART_SOL(2,POIN_PART,6)
        ! if POIN_MID==0 and POIN_PID == 0, the element cost in the .ddw file is 0 --> must be initialized 
        ! from the .inc file 
            IF(POIN_MID/=0.AND.POIN_PID/=0) THEN
             IF(MID_PID_SOL(POIN_MID,6)%COST1D(POIN_PID)/=ZERO) THEN
                 COST_CHECK = 1
                 POIN_ELM_TYP = 6
                 TELT = MID_PID_SOL(POIN_MID,POIN_ELM_TYP)%COST1D(POIN_PID)
             ENDIF
            ENDIF
           ENDIF
         ENDIF
        ! the (mid,pid) cost must be initialized from .inc file
         IF(COST_CHECK==0) THEN
            IF( DDWEIGHTS(1,1,IABS(MID))/=0)THEN
              TMAT = DDWEIGHTS(1,1,IABS(MID)) * TPSREF
            ELSE
              IF(MULT/=0) TMATADD = MULT * (TET10TNL(MLN,INDI)-TET10TNL(MLN,INDI2))
              IF(ADD_OPTION/=0) TMATADD = OPT_1 * TET10TNL(MLN,INDI_OPT_1) + OPT_2 * TET10TNL(MLN,INDI_OPT_2)
              IF(SPECIAL_OPTION/=0) TMATADD = TMATADD + SOL_OPTION(SPE_I_1,SPE_I_2) 
              TMAT = TET10TNL(MLN,INDI) + TMATADD
            ENDIF
!           --------------
!           Failure
            IF(IRUP/=0) THEN
             DO J=1,IRUP
              TRUP = TRUP + RUPTURE_TET10(IRUP_TAB(J),IRUP2)
             ENDDO
            ENDIF
!           --------------
             IF(ISOL.EQ.10) TELT = TET10TELT(1)
             IF(ISOL.EQ.4.AND. JSROT==1) TELT = TET4TELT(2)
             TELT = TMAT + TELT + TRUP + MULT_SPE*NLOCAL_OPTION(SPE_I_3)
         ENDIF
        ELSE
!****************************************
!       ---------------------------
!       SOLID ELEMENT
!       ---------------------------
        ! check if the (mid,pid) cost must be initialized from a previous run
           IF(RECHERCHE==0.AND.TEST_POIDS/=0) THEN
               IF(ISOL==6) THEN
                POIN_ELM_TYP = 5
               ELSEIF(ISOL==8) THEN
                POIN_ELM_TYP = 7
               ELSEIF(ISOL==16) THEN
                POIN_ELM_TYP = 3
               ELSEIF(ISOL==20) THEN
                POIN_ELM_TYP = 4
               ELSE
                POIN_ELM_TYP = 1
             ENDIF
             POIN_PART = IPARTS(I)
             POIN_MID = POIN_PART_SOL(1,POIN_PART,POIN_ELM_TYP)
             POIN_PID = POIN_PART_SOL(2,POIN_PART,POIN_ELM_TYP)
        ! if POIN_MID==0 and POIN_PID == 0, the element cost in the .ddw file is 0 --> must be initialized 
        ! from the .inc file 
             IF(POIN_MID/=0.AND.POIN_PID/=0) THEN
               IF(MID_PID_SOL(POIN_MID,POIN_ELM_TYP)%COST1D(POIN_PID)/=ZERO) THEN
                  COST_CHECK = 1
                  TELT = MID_PID_SOL(POIN_MID,POIN_ELM_TYP)%COST1D(POIN_PID)
               ENDIF
               ENDIF
           ENDIF
        ! the (mid,pid) cost must be initialized from .inc file
           IF(COST_CHECK==0) THEN
            IF (JHBE.EQ.1) THEN
C ow        Solides ISOLD1
              IF( DDWEIGHTS(1,1,IABS(MID))/=0)THEN
               TMAT = DDWEIGHTS(1,1,IABS(MID)) * TPSREF
              ELSE
               IF(MULT/=0) TMATADD = MULT * (SOL1TNL(MLN,INDI)-SOL1TNL(MLN,INDI2))
               IF(ADD_OPTION/=0) TMATADD = OPT_1 * SOL1TNL(MLN,INDI_OPT_1) + OPT_2 * SOL1TNL(MLN,INDI_OPT_2)
               IF(SPECIAL_OPTION/=0) TMATADD = TMATADD + SOL_OPTION(SPE_I_1,SPE_I_2) 
               TMAT = SOL1TNL(MLN,INDI) + TMATADD
              ENDIF
!             --------------
!             Failure
              IF(IRUP/=0) THEN
               DO J=1,IRUP
                TRUP = TRUP + RUPTURE_SOL(IRUP_TAB(J),IRUP2)
               ENDDO
              ENDIF
!             -------------- 
              TELT = TMAT + SOLTELT(1)  + TRUP + MULT_SPE*NLOCAL_OPTION(SPE_I_3)
            ELSEIF (JHBE.EQ.2) THEN
C ow        Solides ISOLD2
              IF( DDWEIGHTS(1,1,IABS(MID))/=0)THEN
                TMAT = DDWEIGHTS(1,1,IABS(MID)) * TPSREF
              ELSE
               IF(MULT/=0) TMATADD = MULT * (SOL1TNL(MLN,INDI)-SOL1TNL(MLN,INDI2))
               IF(ADD_OPTION/=0) TMATADD = OPT_1 * SOL1TNL(MLN,INDI_OPT_1) + OPT_2 * SOL1TNL(MLN,INDI_OPT_2)
               IF(SPECIAL_OPTION/=0) TMATADD = TMATADD + SOL_OPTION(SPE_I_1,SPE_I_2) 
               TMAT = SOL1TNL(MLN,INDI) + TMATADD
              ENDIF
!             --------------
!             Failure
              IF(IRUP/=0) THEN
               DO J=1,IRUP
                TRUP = TRUP + RUPTURE_SOL(IRUP_TAB(J),IRUP2)
               ENDDO
              ENDIF
!             -------------- 
              TELT = TMAT + SOLTELT(2) + TRUP + MULT_SPE*NLOCAL_OPTION(SPE_I_3)
C
            ELSEIF (JHBE.EQ.24.OR.JHBE.EQ.104) THEN
C ow        Solides ISOLD24  - HEPH
              IF( DDWEIGHTS(1,1,IABS(MID))/=0)THEN
                TMAT = DDWEIGHTS(1,1,IABS(MID)) * TPSREF
              ELSE
               IF(MULT/=0) TMATADD = MULT * (SOL1TNL(MLN,INDI)-SOL1TNL(MLN,INDI2))
               IF(ADD_OPTION/=0) TMATADD = OPT_1 * SOL1TNL(MLN,INDI_OPT_1) + OPT_2 * SOL1TNL(MLN,INDI_OPT_2)
               IF(SPECIAL_OPTION/=0) TMATADD = TMATADD + SOL_OPTION(SPE_I_1,SPE_I_2) 
               TMAT = SOL1TNL(MLN,INDI) + TMATADD
              ENDIF
!             --------------
!             Failure
              IF(IRUP/=0) THEN
               DO J=1,IRUP
                TRUP = TRUP + RUPTURE_SOL(IRUP_TAB(J),IRUP2)
               ENDDO
              ENDIF
!             -------------- 
              TELT = TMAT + SOLTELT(3) + TRUP + MULT_SPE*NLOCAL_OPTION(SPE_I_3)
C
            ELSEIF (JHBE.EQ.12) THEN
C ow        Solides ISOLD12 - std 8 node integ point
              IF( DDWEIGHTS(1,1,IABS(MID))/=0)THEN
                TMAT = DDWEIGHTS(1,1,IABS(MID)) * TPSREF
              ELSE
               IF(MULT/=0) TMATADD = MULT * (SOL8TNL(MLN,INDI)-SOL8TNL(MLN,INDI2))
               IF(ADD_OPTION/=0) TMATADD = OPT_1 * SOL8TNL(MLN,INDI_OPT_1) + OPT_2 * SOL8TNL(MLN,INDI_OPT_2)
               IF(SPECIAL_OPTION/=0) TMATADD = TMATADD + SOL_OPTION(SPE_I_1,SPE_I_2) 
               TMAT = SOL8TNL(MLN,INDI) + TMATADD
              ENDIF
!             --------------
!             Failure
              IF(IRUP/=0) THEN
               DO J=1,IRUP
                TRUP = TRUP + RUPTURE_SOL12(IRUP_TAB(J),IRUP2)
               ENDDO
              ENDIF
!             -------------- 
              TELT = TMAT + SOLTELT(4) + TRUP + MULT_SPE*NLOCAL_OPTION(SPE_I_3)
C
            ELSEIF (JHBE.EQ.14.OR.(JHBE.GE.222.AND.JHBE.LE.999)) THEN
C ow        Solides HA8
               MPT = ABS (NPN)
               NPTS = MPT/100
               NPTT = MOD(MPT/10,10)
               NPTR = MOD(MPT,10)
               NPTOT = NPTS*NPTT*NPTR

               IF( DDWEIGHTS(1,1,IABS(MID))/=0)THEN
                TMAT = DDWEIGHTS(1,1,IABS(MID)) * TPSREF
               ELSE
                IF(MULT/=0) TMATADD = MULT * (SOL1TNL(MLN,INDI)-SOL1TNL(MLN,INDI2))
                IF(ADD_OPTION/=0) TMATADD = OPT_1 * SOL1TNL(MLN,INDI_OPT_1) + OPT_2 * SOL1TNL(MLN,INDI_OPT_2)
                IF(SPECIAL_OPTION/=0) TMATADD = TMATADD + SOL_OPTION(SPE_I_1,SPE_I_2) 
                TMAT = SOL1TNL(MLN,INDI) + TMATADD
               ENDIF
!              --------------
!              Failure
               IF(IRUP/=0) THEN
                DO J=1,IRUP
                 TRUP = TRUP + RUPTURE_SOL(IRUP_TAB(J),IRUP2)
                ENDDO
               ENDIF
!              --------------
               ! 8 NPT = 222 = reference weight 
               ! if NPT > 8, element weight = reference weight + (NPT-8) * overweight 
               TELT = NPTOT*(TMAT+TRUP)+SOLTELT(5) +(NPTOT-8)*SOLTELT(6) + MULT_SPE*NLOCAL_OPTION(SPE_I_3)
     
            ELSEIF (JHBE.EQ.17) THEN   
c am        ISOLIDE=17 H8C Standard 8-nodes compatible 
c           solid full integration formulation 2*2*2 integration points, no hourglass
               IF( DDWEIGHTS(1,1,IABS(MID))/=0)THEN
                TMAT = DDWEIGHTS(1,1,IABS(MID)) * TPSREF
               ELSE
                IF(MULT/=0) TMATADD = MULT * (SOL1TNL(MLN,INDI)-SOL1TNL(MLN,INDI2))
                IF(ADD_OPTION/=0) TMATADD = OPT_1 * SOL1TNL(MLN,INDI_OPT_1) + OPT_2 * SOL1TNL(MLN,INDI_OPT_2)
                IF(SPECIAL_OPTION/=0) TMATADD = TMATADD + SOL_OPTION(SPE_I_1,SPE_I_2) 
                TMAT = SOL1TNL(MLN,INDI) + TMATADD
               ENDIF
!              --------------
!              Failure
               IF(IRUP/=0) THEN
                DO J=1,IRUP
                 TRUP = TRUP + RUPTURE_SOL(IRUP_TAB(J),IRUP2)
                ENDDO
               ENDIF
!              -------------- 
               TELT =  (TMAT+TRUP)*8 + SOLTELT(7) + MULT_SPE*NLOCAL_OPTION(SPE_I_3)
            ELSEIF (JHBE.EQ.18) THEN   
c am        ISOLIDE=18
               IF( DDWEIGHTS(1,1,IABS(MID))/=0)THEN
                TMAT = DDWEIGHTS(1,1,IABS(MID)) * TPSREF
               ELSE
                IF(MULT/=0) TMATADD = MULT * (SOL1TNL(MLN,INDI)-SOL1TNL(MLN,INDI2))
                IF(ADD_OPTION/=0) TMATADD = OPT_1 * SOL1TNL(MLN,INDI_OPT_1) + OPT_2 * SOL1TNL(MLN,INDI_OPT_2)
                IF(SPECIAL_OPTION/=0) TMATADD = TMATADD + SOL_OPTION(SPE_I_1,SPE_I_2) 
                TMAT = SOL1TNL(MLN,INDI) + TMATADD
               ENDIF
!              --------------
!              Failure
               IF(IRUP/=0) THEN
                DO J=1,IRUP
                 TRUP = TRUP + RUPTURE_SOL(IRUP_TAB(J),IRUP2)
                ENDDO
               ENDIF
!              -------------- 
               TELT =  (TMAT+TRUP)*8 + SOLTELT(9) + MULT_SPE*NLOCAL_OPTION(SPE_I_3)
C              
C           Solides pas encore pris en compte : PA6, HSEPH (deja repertories)
            ELSE
!             --------------
!             Failure
              IF(IRUP/=0) THEN
               DO J=1,IRUP
                TRUP = TRUP + RUPTURE_SOL(IRUP_TAB(J),IRUP2)
               ENDDO
              ENDIF
!             -------------- 
              TELT = SOL1TNL(MLN,1) + SOLTELT(1) + TRUP + MULT_SPE*NLOCAL_OPTION(SPE_I_3)
            ENDIF
           ENDIF
        ENDIF      
C
          POIDS = TELT * INVTREF
C el       test elem delete
          ! --------------------
          IF(RECHERCHE==0) THEN
           IF (WD_LOCAL.EQ.0..AND.MLN.NE.0)THEN
             WD(I) = POIDS
             POIN_PART = IPARTS(I)
             IF (ISOL.EQ.4.AND. (JSROT .NE. 1)) THEN
                POIN_ELM_TYP = 6
             ELSEIF( (ISOL.EQ.10).OR.(ISOL.EQ.4.AND. JSROT==1) ) THEN
                IF(ISOL==10) THEN
                        POIN_ELM_TYP = 2
                ELSE
                        POIN_ELM_TYP = 6
                ENDIF
             ELSE
                IF(ISOL==6) THEN
                        POIN_ELM_TYP = 5
                ELSEIF(ISOL==8) THEN
                        POIN_ELM_TYP = 7
                ELSEIF(ISOL==16) THEN
                        POIN_ELM_TYP = 3
                ELSEIF(ISOL==20) THEN
                        POIN_ELM_TYP = 4
                ELSE
                        POIN_ELM_TYP = 1
                ENDIF
             ENDIF
             POIN_PART = IPARTS(I)
             POIN_MID = POIN_PART_SOL(1,POIN_PART,POIN_ELM_TYP)
             POIN_PID = POIN_PART_SOL(2,POIN_PART,POIN_ELM_TYP)
             IF(POIN_MID/=0.AND.POIN_PID/=0) MID_PID_SOL(POIN_MID,POIN_ELM_TYP)%COST1D(POIN_PID) = TELT
           ELSEIF(MLN.EQ.0)THEN
             WD(I) = 0.0001
           END IF
          ELSE
           TELT_PRO = TELT
          ENDIF
          ! --------------------               
      ENDDO
      RETURN
      END
