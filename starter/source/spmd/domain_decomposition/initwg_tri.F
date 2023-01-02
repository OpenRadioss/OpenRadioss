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
Chd|  INITWG_TRI                    source/spmd/domain_decomposition/initwg_tri.F
Chd|-- called by -----------
Chd|        INITWG                        source/spmd/domain_decomposition/initwg.F
Chd|-- calls ---------------
Chd|        BIDON                         source/system/machine.F       
Chd|        INTERLAGRAN                   source/spmd/domain_decomposition/grid2mat.F
Chd|        DDWEIGHTS_MOD                 share/modules1/ddweights_mod.F
Chd|        MID_PID_MOD                   share/modules1/mid_pid_mod.F  
Chd|====================================================================
      SUBROUTINE INITWG_TRI(WD,PM,GEO,IXTG,IGEO,NUMELTG,IPM ,SIZE_IRUP,
     .            NUMMAT,NUMGEO,POIN_PART_TRI,MID_PID_TRI,IPARTG,
     .            OFF,BUFMAT,MID_OLD,PID_OLD,MLN_OLD,RECHERCHE,TELT_PRO,
     .            TABMP_L)
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
      INTEGER  OFF,NUMELTG,TABMP_L,
     .  NUMMAT,NUMGEO, IXTG(NIXTG,*),IGEO(NPROPGI,*),
     .  IPM(NPROPMI,*)
      INTEGER, INTENT(IN) :: SIZE_IRUP

C     REAL OU REAL*8
      my_real
     .    PM(NPROPM,*), GEO(NPROPG,*),BUFMAT(*)

      REAL WD(*)
      INTEGER MID_OLD,PID_OLD,MLN_OLD,RECHERCHE
      my_real TELT_PRO

      INTEGER, DIMENSION(*), INTENT(IN) :: IPARTG
      INTEGER, DIMENSION(2,*), INTENT(IN) :: POIN_PART_TRI
      TYPE(MID_PID_TYPE), DIMENSION(*), INTENT(INOUT) :: MID_PID_TRI
C-----------------------------------------------
      INTEGER  NPN, MID, PID, JHBE, IGT, MLN,
     .    ISTRAIN, ITHK, IHBE, IPLA, ISSN, MTN, I, J, K,L,
     .    NFUNC,MPT,NPTS,NPTT,NPTR,NPTOT,IFLAG,JSROT,
     .    I_MID,I_PID,I_MID_OLD,I_PID_OLD,PUID,MUID,
     .    ELM_TYP,ELM_TYP_OLD,ILAW,ILAW_OLD,TEST_MAT,
     .    I_PRO,ISOL2,MUID_OLD,PUID_OLD,
     .    TEST,NFUNC1,NFUNC2,IRUP,IRUP2,II,IRUP_TAB(SIZE_IRUP),
     .    INDI,IAD,INDI2,MULT
      INTEGER :: INDI3,COST_CHECK,POIN_PART,POIN_MID,POIN_PID
      INTEGER :: FLAG_NICE_NEWTON,FLAG_GURSON,FLAG_NON_LOCAL
      INTEGER :: SPECIAL_OPTION,SPE_I_1,SPE_I_2,SPE_I_3
     
      REAL
     .   WTYPE(9),FWIHBE,FAC8,
     .   TABMAT(3),TABX(3),TIMMAT,NPT,TELT,POIDS,W,
     .   BATOZMULT,TMAT,TRUP,TABRUP(3),TRUP_LOCAL,TMATADD,
     .   WD_LOCAL,MULT_SPE
      
      my_real
     .        CC, A,B,A1,A2
      my_real
     .       INVTREF
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
      INVTREF = ONE/TPSREF
      INDI3 = 2
      DO I = 1, NUMELTG
        MID= IXTG(1,I)
        PID= IXTG(5,I)
        MLN = NINT(PM(19,ABS(MID)))
        WD_LOCAL = WD(I+OFF)
        ! -----------------
        IF(RECHERCHE==1) THEN
         MID = MID_OLD
         PID = PID_OLD
         MLN = MLN_OLD
         WD_LOCAL = ZERO
        ENDIF
        ! -----------------
        NPN = NINT(GEO(6,PID))
        IHBE = NINT(GEO(171,PID))
        ITHK = NINT(GEO(35,PID))
        IPLA = NINT(GEO(39,PID))
        NPT = MAX(ABS(NPN),1)
        FLAG_NON_LOCAL = 0  
        SPECIAL_OPTION = 0
        SPE_I_1 = 1
        SPE_I_2 = 1
        IRUP = IPM(220,ABS(MID))
        IRUP_TAB = 0
        IF(IRUP/=0) THEN          ! up to 6 failure models per material
         DO J=1,IRUP
          IRUP_TAB(J) = IPM(111+15*(J-1),ABS(MID))
         ENDDO
        ENDIF
        TIMMAT = 0.
        TRUP = 0.
        TMATADD = 0.
        MULT = 0
        IF((MLN<28).OR.(MLN==32)) THEN
         IRUP2 = 0
        ELSE
         IRUP2 = 3
        ENDIF

        IF (MLN==2.OR.MLN==3) THEN
          CC = PM(43,MID)
          IF (CC/=0) THEN
           INDI = 2
          ELSE
           INDI = 1
          ENDIF
        ELSEIF ((MLN==25).AND.(ABS(NPN)>0)) THEN
          IFLAG = NINT( PM(40,MID))
          IF (IFLAG/=0) THEN
           INDI = 2
          ELSE
           INDI = 1
          ENDIF
C loi 36+86 en fonction des fct
        ELSEIF (MLN==36.OR.(MLN==86).AND.(ABS(NPN)>0)) THEN
          NFUNC = NINT(PM(40,MID))
          IF (NFUNC<=2) THEN
           INDI = 1
          ELSEIF (NFUNC>2.AND.NFUNC<=7) THEN
           INDI = 2
          ELSEIF (NFUNC>7) THEN
           INDI = 3
          ENDIF
        ELSEIF((MLN==42).OR.(MLN==62).OR.(MLN==69)) THEN
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
            SPE_I_3 = 2
            MULT_SPE = NPT
        ENDIF

        COST_CHECK = 0
!****************************************
!       ---------------------------
!       SHELL3N ELEMENT
!       ---------------------------
        ! check if the (mid,pid) cost must be initialized from a previous run
        IF(RECHERCHE==0.AND.TEST_POIDS/=0) THEN
          POIN_PART = IPARTG(I)
          POIN_MID = POIN_PART_TRI(1,POIN_PART)
          POIN_PID = POIN_PART_TRI(2,POIN_PART)
        ! if POIN_MID==0 and POIN_PID == 0, the element cost in the .ddw file is 0 --> must be initialized 
        ! from the .inc file 
          IF(POIN_MID/=0.AND.POIN_PID/=0) THEN
           IF(MID_PID_TRI(POIN_MID)%COST1D(POIN_PID)/=ZERO) THEN
                COST_CHECK = 1
                TELT = MID_PID_TRI(POIN_MID)%COST1D(POIN_PID)
           ENDIF
          ENDIF
        ENDIF
        ! the (mid,pid) cost must be initialized from .inc file
        IF(COST_CHECK==0) THEN
         IF( DDWEIGHTS(1,2,MID)/=ZERO)THEN
!       Compute time according to integration points:
            A1 = DDWEIGHTS(1,2,MID) * TPSREF 
            A2 = DDWEIGHTS(2,2,MID) * TPSREF 

            IF (A2 /=ZERO)THEN
!       Compute line function A1 ="Time for 1 int Point" - A2="time for 5 int points"
              A = (A2-A1)/4
              B = A1-A
              TIMMAT = A*NPT + B
            ELSE
              TIMMAT = A1*NPT
            ENDIF
!           --------------
!           Failure
            IF(IRUP/=0) THEN
             DO J=1,IRUP
              A1 = RUPTURE_SHELL(IRUP_TAB(J),IRUP2+1)
              A2 = RUPTURE_SHELL(IRUP_TAB(J),IRUP2+3)
              IF (A2 /=ZERO)THEN
               A = (A2-A1)/4
               B = A1-A
               TRUP = TRUP + A*NPT + B
              ELSE
               TRUP = TRUP + A1*NPT
              ENDIF
             ENDDO
            ENDIF
!           --------------
         ELSE
          IF(ITHK==2)THEN
            ITHK = 0
          ELSEIF(MLN==32)THEN
            ITHK = 1
          ENDIF
          ISTRAIN = NINT(GEO(11,PID))
          IF(MLN==19.OR.MLN>=25)ISTRAIN = 1
          ISSN = NINT(GEO(3,PID))
!       ow test elem delete
          IF (WD_LOCAL==0.) THEN
           IF(ABS(NPN)>0) THEN
              TABX(1) = 1.
              TABX(2) = 3.
              TABX(3) = 5.
              ! *******--------*******
              DO J=1,3
                IF(MULT/=0) TMATADD = MULT * 
     .              (TRITNL(MIN(MLN,MAXLAW),J,INDI) - TRITNL(MIN(MLN,MAXLAW),J,INDI2) )
                  IF(SPECIAL_OPTION/=0) TMATADD = TMATADD + SHTNL_OPTION(SPE_I_1,SPE_I_2) 
                TABMAT(J) = TRITNL(MIN(MLN,MAXLAW),J,INDI) + TMATADD
              ENDDO
              ! *******--------*******
              NPT = ABS(NPN)
              CALL INTERLAGRAN(TABMAT,TABX,3,NPT,TIMMAT)
!              ----------------
!               Failure 
                IF(IRUP/=0) THEN
                 DO J=1,IRUP
                  DO II=1,3
                   TABRUP(II) = RUPTURE_SHELL(IRUP_TAB(J),IRUP2+II)
                  ENDDO
                  CALL INTERLAGRAN(TABRUP,TABX,3,NPT,TRUP_LOCAL)
                  TRUP = TRUP + TRUP_LOCAL
                 ENDDO
                ENDIF          !   <--- fin IRUP/=0
!               ---------------- 

            ELSE
!       0pt d integration doit etre traite a part
              ! *******--------*******
              IF(MULT/=0) TMATADD = MULT * 
     .              (TRITNL(MIN(MLN,MAXLAW),0,INDI) - TRITNL(MIN(MLN,MAXLAW),0,INDI2) )
              TIMMAT =  TRITNL(MIN(MLN,MAXLAW),0,INDI)  + TMATADD
              ! *******--------*******
!             ----------------
!             Failure incompatible N=0
              IF(IRUP/=0) THEN
               TRUP = 0.
              ENDIF          !   <--- fin IRUP/=0
!             ----------------  
            ENDIF
          ENDIF
         ENDIF
         IF(MLN/=0)THEN
              TELT = TRITELT(1) 
         ENDIF
        ENDIF   !   fin TRITNL_OLD(MID,PID)
!****************************************

        ! --------------------
        IF(RECHERCHE==0) THEN
         IF((WD_LOCAL==0.).AND.(MLN/=0))THEN
          POIDS = (TELT + TIMMAT + TRUP + MULT_SPE*NLOCAL_OPTION(SPE_I_3) ) * INVTREF
          WD(I+OFF) = POIDS

          POIN_PART = IPARTG(I)
          POIN_MID = POIN_PART_TRI(1,POIN_PART)
          POIN_PID = POIN_PART_TRI(2,POIN_PART)
          IF(POIN_MID/=0.AND.POIN_PID/=0) 
     .    MID_PID_TRI(POIN_MID)%COST1D(POIN_PID) = TELT + TIMMAT + TRUP +
     .           MULT_SPE*NLOCAL_OPTION(SPE_I_3)
         ELSE
          WD(I+OFF) = 0.0001
         END IF
        ELSE
         TELT_PRO = TELT + TIMMAT + TRUP + MULT_SPE*NLOCAL_OPTION(SPE_I_3)
        ENDIF
        ! --------------------
      ENDDO
      RETURN
      END
