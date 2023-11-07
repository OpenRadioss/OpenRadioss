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
Chd|  INITWG_SHELL                  source/spmd/domain_decomposition/initwg_shell.F
Chd|-- called by -----------
Chd|        INITWG                        source/spmd/domain_decomposition/initwg.F
Chd|-- calls ---------------
Chd|        BIDON                         source/system/machine.F       
Chd|        INTERLAGRAN                   source/spmd/domain_decomposition/grid2mat.F
Chd|        DDWEIGHTS_MOD                 share/modules1/ddweights_mod.F
Chd|        MID_PID_MOD                   share/modules1/mid_pid_mod.F  
Chd|====================================================================
      SUBROUTINE INITWG_SHELL(WD,PM,GEO,IXC,IGEO,SIZE_IRUP,
     .            NUMELC,IPM,NUMMAT,NUMGEO,POIN_PART_SHELL,
     .            MID_PID_SHELL,IPARTC,OFF,BUFMAT,
     .            MID_OLD,PID_OLD,MLN_OLD,RECHERCHE,TELT_PRO,
     .            TABMP_L,MAT_PARAM)
C-----------------------------------------------
C            M o d u l e s
C-----------------------------------------------
      USE DDWEIGHTS_MOD
      USE MID_PID_MOD
      USE MATPARAM_DEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com01_c.inc"
#include      "tablen_c.inc"
#include      "ddspmd_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  NUMELC,OFF,
     .  NUMMAT,NUMGEO,IXC(NIXC,*),
     .  IGEO(NPROPGI,*),IPM(NPROPMI,*),TABMP_L
      INTEGER, INTENT(IN) :: SIZE_IRUP ! maximum number of rupture criteria

C     REAL OU REAL*8
      my_real
     .    PM(NPROPM,*), GEO(NPROPG,*),BUFMAT(*)
      REAL WD(*)
      INTEGER MID_OLD,PID_OLD,MLN_OLD,RECHERCHE
      my_real TELT_PRO

      INTEGER, DIMENSION(*), INTENT(IN) :: IPARTC
      INTEGER, DIMENSION(2,*), INTENT(IN) :: POIN_PART_SHELL
      TYPE(MID_PID_TYPE), DIMENSION(*), INTENT(INOUT) :: MID_PID_SHELL
      TYPE(MATPARAM_STRUCT_) ,DIMENSION(NUMMAT), INTENT(IN) :: MAT_PARAM
C-----------------------------------------------
      INTEGER  NPN, MID, PID, JHBE, IGT, MLN,
     .    ISTRAIN, ITHK, IHBE, IPLA, ISSN, MTN, I, J, K,L,
     .    NFUNC,MPT,NPTS,NPTT,NPTR,NPTOT,IFLAG,JSROT,
     .    I_MID,I_PID,I_MID_OLD,I_PID_OLD,PUID,MUID,
     .    ELM_TYP,ELM_TYP_OLD,ILAW,ILAW_OLD,TEST_MAT,
     .    I_PRO,ISOL2,MUID_OLD,PUID_OLD,
     .    TEST,NFUNC1,NFUNC2,NFAIL,IRUP2,II,IRUP_TAB(SIZE_IRUP),
     .    INDI,IAD,INDI2,MULT,IDRIL
      INTEGER :: INDI3,IGTYP,INDI4,INDI5

      INTEGER :: POIN_PID,POIN_MID,POIN_PART,COST_CHECK
      INTEGER :: FLAG_NICE_NEWTON,FLAG_GURSON,FLAG_NON_LOCAL
      INTEGER :: SPECIAL_OPTION,SPE_I_1,SPE_I_2,SPE_I_3
           
      REAL
     .   WTYPE(9),FWIHBE,FAC8,
     .   TABMAT(3),TABX(3),TIMMAT,NPT,TELT,POIDS,W,
     .   BATOZMULT,TMAT,TRUP,TABRUP(3),TRUP_LOCAL,TMATADD,
     .   WD_LOCAL,MULT_SPE
           
      my_real
     .        CC,A,B,A1,A2
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
      INDI3 = 1
      DO I = 1, NUMELC
        MID= IXC(1,I)
        PID= IXC(6,I)
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
        NPT = ABS(NPN)
        IHBE = NINT(GEO(171,PID))
        ITHK = NINT(GEO(35,PID))
        IPLA = NINT(GEO(39,PID))
        IGTYP = IGEO(11,PID)
        NFAIL = MAT_PARAM(MID)%NFAIL
        IDRIL = IGEO(20,PID)
        FLAG_NON_LOCAL = 0  
        SPECIAL_OPTION = 0
        SPE_I_1 = 1
        SPE_I_2 = 1
        IRUP_TAB = 0
        IF (NFAIL > 0) THEN          ! up to 6 failure models per material
         DO J=1,NFAIL
          IRUP_TAB(J) = MAT_PARAM(MID)%FAIL(J)%IRUPT
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
        ! ----------------
        ! law 2 : 2 sub-option
        IF (MLN==2) THEN
           CC = PM(43,MID)
           IF (CC/=0) THEN
            INDI = 2
           ELSE
            INDI = 1
           ENDIF
        ! ----------------
        ! law 25 : 2 sub-option
        ELSEIF(MLN==25) THEN
           IF (IGTYP/=9) THEN
            INDI = 2
           ELSE
            INDI = 1
           ENDIF
        ! ----------------
        ! law 36 : the number of function is taken into account
        ELSEIF ((MLN==36)) THEN
           IF(ABS(NPN)>0) THEN
            NFUNC = IPM(10,MID)
           ELSE
            NFUNC = NINT(PM(40,MID))
           ENDIF
           IF (NFUNC<=2) THEN
            INDI = 1
           ELSEIF (NFUNC>2.AND.NFUNC<=7) THEN
            INDI = 2
           ELSEIF (NFUNC>7) THEN
            INDI = 3
           ENDIF
        ! ----------------
        ! law 86 : prony option
        ELSEIF((MLN==86).AND.(ABS(NPN)==0)) THEN
           NFUNC = NINT(PM(40,MID))
           IF (NFUNC<=2) THEN
            INDI = 1
           ELSEIF (NFUNC>2.AND.NFUNC<=7) THEN
            INDI = 2
           ELSEIF (NFUNC>7) THEN
            INDI = 3
           ENDIF
        ! ----------------
        ! law 42,62 and 69 : prony option
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
        ! ----------------
        ! law 82 : prony option
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
        ! ----------------
        ! law 104 : 2 different algo + sub-option
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
           FLAG_NON_LOCAL = MAT_PARAM(ABS(MID))%NLOC
        ! ----------------
        ! other laws
        ELSE
           INDI = 1
        ENDIF
        ! ----------------
        MULT_SPE = 0.
        SPE_I_3 = 1
        IF(FLAG_NON_LOCAL/=0) THEN
            SPE_I_3 = 2
            MULT_SPE = NPT
        ENDIF
        COST_CHECK = 0
!****************************************
!       ---------------------------
!       SHELL ELEMENT
!       ---------------------------
        ! check if the (mid,pid) cost must be initialized from a previous run
        IF(RECHERCHE==0.AND.TEST_POIDS/=0) THEN
          POIN_PART = IPARTC(I)
          POIN_MID = POIN_PART_SHELL(1,POIN_PART)
          POIN_PID = POIN_PART_SHELL(2,POIN_PART)
        ! if POIN_MID==0 and POIN_PID == 0, the element cost in the .ddw file is 0 --> must be initialized 
        ! from the .inc file 
          IF(POIN_MID/=0.AND.POIN_PID/=0) THEN
           IF(MID_PID_SHELL(POIN_MID)%COST1D(POIN_PID)/=ZERO) THEN
                COST_CHECK = 1
                TELT = MID_PID_SHELL(POIN_MID)%COST1D(POIN_PID)
           ENDIF
          ENDIF
        ENDIF
        ! the (mid,pid) cost must be initialized from .inc file
        IF(COST_CHECK==0) THEN
!       ----------------------------------------
!       user material cost
!       ----------------------------------------
         IF(DDWEIGHTS(1,2,MID)/=ZERO)THEN
C           Compute time according to integration points:
            A1 = DDWEIGHTS(1,2,MID) * TPSREF 
            A2 = DDWEIGHTS(2,2,MID) * TPSREF 

            IF (A2 /=ZERO)THEN
C           Compute line function A1 ="Time for 1 int Point" - A2="time for 5 int points"
             A = (A2-A1)/4
             B = A1-A
             TIMMAT = A*NPT + B
            ELSE
             TIMMAT = A1*NPT
            ENDIF
!           --------------
!           Failure
            IF(NFAIL/=0) THEN
             DO J=1,NFAIL
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
!       ----------------------------------------
!       Radioss material cost
!       ----------------------------------------
            IF(ITHK==2)THEN
              ITHK = 0
            ELSEIF(MLN==32)THEN
              ITHK = 1
            ENDIF
            ISTRAIN = NINT(GEO(11,PID))
            IF(MLN==19.OR.MLN>=25)ISTRAIN = 1
             ISSN = NINT(GEO(3,PID))
C ow        test elem delete             
             IF (WD_LOCAL==0.) THEN
               IF(ABS(NPN)>0) THEN
                TABX(1) = 1.
                TABX(2) = 3.
                TABX(3) = 5.
                ! *******--------*******
                DO J = 1,3
                  IF(MULT/=0) TMATADD = MULT * 
     .              (SHTNL(MIN(MLN,MAXLAW),J,INDI) - SHTNL(MIN(MLN,MAXLAW),J,INDI2) )
                  IF(SPECIAL_OPTION/=0) TMATADD = TMATADD + SHTNL_OPTION(SPE_I_1,SPE_I_2) 
                  TABMAT(J) = SHTNL(MIN(MLN,MAXLAW),J,INDI) + TMATADD
                ENDDO
                ! *******--------*******
                NPT = ABS(NPN)
                CALL INTERLAGRAN(TABMAT,TABX,3,NPT,TIMMAT)
!               ----------------
!               Failure 
                IF(NFAIL/=0) THEN
                 DO J=1,NFAIL
                  DO II=1,3
                   TABRUP(II) = RUPTURE_SHELL(IRUP_TAB(J),IRUP2+II)
                  ENDDO
                  TRUP_LOCAL = 0.
                  CALL INTERLAGRAN(TABRUP,TABX,3,NPT,TRUP_LOCAL)
                  TRUP = TRUP + TRUP_LOCAL
                 ENDDO
                ENDIF          !   <--- fin NFAIL/=0
!               ----------------      
               ELSE
C           0pt d integration doit etre traite a part
                ! *******--------*******
                IF(MULT/=0) TMATADD = MULT * 
     .              (SHTNL(MIN(MLN,MAXLAW),0,INDI) - SHTNL(MIN(MLN,MAXLAW),0,INDI2) )
                TIMMAT =  SHTNL(MIN(MLN,MAXLAW),0,INDI) + TMATADD
                ! *******--------*******
!               ----------------
!               Failure incompatible N=0
                IF(NFAIL/=0) THEN
                 TRUP = 0.
                ENDIF          !   <--- fin NFAIL/=0
!               ----------------  
               ENDIF
            ENDIF
         ENDIF

         IF(IGTYP==10) THEN
                INDI4 = 1
         ELSEIF(IGTYP==11) THEN
                INDI4 = 2
         ELSEIF(IGTYP==9) THEN
                INDI4 = 3
         ELSEIF(IGTYP==16) THEN
                INDI4 = 4
         ELSEIF(IGTYP==51) THEN
                INDI4 = 5
         ELSE
                INDI4 = 0
         ENDIF

         INDI5 = 0
         IF(IDRIL==1) INDI5=2

         IF(MLN/=0)THEN
           IF (IHBE>=11.AND.IHBE<=19) THEN
C ow            BATOZ
                 TELT = SHTELT(INDI4*5+3+INDI5)+BATOZMULT*(TIMMAT + TRUP) + MULT_SPE*NLOCAL_OPTION(SPE_I_3)
           ELSEIF (IHBE>=21.AND.IHBE<=29) THEN
C ow            QEPH
                 TELT = SHTELT(INDI4*5+2+INDI5) + TIMMAT + TRUP + MULT_SPE*NLOCAL_OPTION(SPE_I_3)
           ELSE
C ow            Q4
                 TELT = SHTELT(INDI4*5+1)  + TIMMAT + TRUP + MULT_SPE*NLOCAL_OPTION(SPE_I_3)
           ENDIF
         ENDIF
        ENDIF       !   fin SHTNL_OLD
!****************************************

        ! --------------------
        IF(RECHERCHE==0) THEN
         IF((WD_LOCAL==0.).AND.(MLN/=0)) THEN
           POIDS = TELT * INVTREF
           WD(I+OFF) = POIDS
           POIN_PART = IPARTC(I)
           POIN_MID = POIN_PART_SHELL(1,POIN_PART)
           POIN_PID = POIN_PART_SHELL(2,POIN_PART)
           IF(POIN_MID/=0.AND.POIN_PID/=0) MID_PID_SHELL(POIN_MID)%COST1D(POIN_PID) = TELT
         ELSEIF((WD_LOCAL==0.).AND.(MLN==0)) THEN
           WD(I+OFF) = 0.0001
         ENDIF
        ELSE
         TELT_PRO = TELT
        ENDIF
        ! --------------------
      ENDDO

      RETURN
      END
