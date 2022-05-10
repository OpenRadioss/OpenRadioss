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
C NOUVELLE ROUTINE POUR REMPLIR LE TABLEAU DES POIDS
Chd|====================================================================
Chd|  INITWG                        source/spmd/domain_decomposition/initwg.F
Chd|-- called by -----------
Chd|        DOMETIS                       source/spmd/domain_decomposition/grid2mat.F
Chd|        DOMETIS2                      source/spmd/domain_decomposition/domdecs.F
Chd|-- calls ---------------
Chd|        INITWG_POUTRE                 source/spmd/domain_decomposition/initwg_poutre.F
Chd|        INITWG_QUAD                   source/spmd/domain_decomposition/initwg_quad.F
Chd|        INITWG_RESSORT                source/spmd/domain_decomposition/initwg_ressort.F
Chd|        INITWG_SHELL                  source/spmd/domain_decomposition/initwg_shell.F
Chd|        INITWG_SOLID                  source/spmd/domain_decomposition/initwg_solid.F
Chd|        INITWG_TRI                    source/spmd/domain_decomposition/initwg_tri.F
Chd|        INITWG_TRUSS                  source/spmd/domain_decomposition/initwg_truss.F
Chd|        INITWG_UR                     source/spmd/domain_decomposition/initwg_ur.F
Chd|        INITWG_X                      source/spmd/domain_decomposition/initwg_x.F
Chd|        INIT_MID_PID_ARRAY            source/spmd/tools/init_mid_pid_array.F
Chd|        DDWEIGHTS_MOD                 share/modules1/ddweights_mod.F
Chd|        MID_PID_MOD                   share/modules1/mid_pid_mod.F  
Chd|====================================================================
      SUBROUTINE INITWG(WD,PM,GEO,IXS,IXQ,
     .            IXC,IXT,IXP,IXR,IXTG,
     .            IXUR,KXX,IGEO,ISOLNOD,IDARCH,
     .            NUMELS,NUMELQ,NUMELC,NUMELT,NUMELP,
     .            NUMELR,NUMELTG,NUMELUR,NUMELX,IPM ,
     .            BUFMAT,NUMMAT,NUMGEO,TAILLE,POIN_UMP,
     .            TAB_UMP,POIN_UMP_OLD,TAB_UMP_OLD,CPUTIME_MP_OLD,
     .            TABMP_L,IPART,IPARTC,IPARTG,
     .            IPARTS,NPART,POIN_PART_SHELL,POIN_PART_TRI,POIN_PART_SOL,
     .            MID_PID_SHELL,MID_PID_TRI,MID_PID_SOL,IDDLEVEL)
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
#include      "scr17_c.inc"
#include      "scr21_c.inc"
#include      "scr05_c.inc"
#include      "tablen_c.inc"
#include      "scr23_c.inc"
#include      "spmd_c.inc" 
#include      "ddspmd_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  IDARCH,
     .  NUMELS,NUMELQ,NUMELC,NUMELT,NUMELP,
     .  NUMELR,NUMELTG,NUMELUR,NUMELX,
     .  NUMMAT,NUMGEO,TAILLE,
     .  IXS(NIXS,*),IXQ(NIXQ,*),IXC(NIXC,*),
     .  IXT(NIXT,*), IXP(NIXP,*), IXR(NIXR,*), IXTG(NIXTG,*),
     .  IXUR(NIXUR,*),  KXX(NIXX,*),IGEO(NPROPGI,*),ISOLNOD(*),
     .  IPM(NPROPMI,*),TABMP_L,NPART,IDDLEVEL
      INTEGER, DIMENSION(NUMMAT_OLD) :: POIN_UMP_OLD
      INTEGER, DIMENSION(7,TAILLE_OLD) :: TAB_UMP_OLD
      INTEGER, DIMENSION(NUMMAT) :: POIN_UMP
      INTEGER, DIMENSION(7,TAILLE) :: TAB_UMP
      INTEGER, DIMENSION(LIPART1,*), INTENT(IN) :: IPART
      INTEGER, DIMENSION(*), INTENT(IN) :: IPARTC,IPARTG,IPARTS

      INTEGER, DIMENSION(2,NPART), INTENT(INOUT) :: POIN_PART_SHELL,POIN_PART_TRI
      INTEGER, DIMENSION(2,NPART,7), INTENT(INOUT) :: POIN_PART_SOL
      TYPE(MID_PID_TYPE), DIMENSION(NUMMAT), INTENT(INOUT) :: MID_PID_SHELL,MID_PID_TRI
      TYPE(MID_PID_TYPE), DIMENSION(NUMMAT,7), INTENT(INOUT) :: MID_PID_SOL

C     REAL OU REAL*8
      my_real
     .    PM(NPROPM,*), GEO(NPROPG,*),BUFMAT(*)
      my_real, DIMENSION(TAILLE_OLD) :: CPUTIME_MP_OLD
      REAL WD(*)
C-----------------------------------------------
      INTEGER OFF, NPN, MID, PID, JHBE, IGT, MLN,
     .    ISTRAIN, ITHK, IHBE, IPLA, ISSN, MTN, I, J, K,L,
     .    NFUNC,MPT,NPTS,NPTT,NPTR,NPTOT,IFLAG,JSROT,
     .    NFUNC1,NFUNC2,IRUP,II,IRUP2,IRUP_TAB(6),
     .    I_MID,I_PID,I_MID_OLD,I_PID_OLD,PUID,MUID,
     .    ELM_TYP,ELM_TYP_OLD,ILAW,ILAW_OLD,TEST_MAT,
     .    I_PRO,ISOL,MID_OLD,PID_OLD,MUID_OLD,PUID_OLD,
     .    TEST,RECHERCHE,NUMEL_RE,IAD,INDI,
     .    MAX_ELM_OLD,MAX_ELM,MAX_ELM_OLD_36_2,MAX_ELM_36_2,
     .    K_36_2,I_PRO_36_2,NBR_ELM
     
      INTEGER, DIMENSION(TAILLE) :: CONCORDANCE_MAT
      REAL
     .   WTYPE(9),FWIHBE,FAC8,
     .   TABMAT(3),TABX(3),TIMMAT,NPT,TELT,POIDS,W,
     .   BATOZMULT,TMAT,TABRUP(3),TRUP_LOCAL,TRUP
      my_real INVTELT_PRO,TELT_PRO

      INTEGER, DIMENSION(NUMMAT) :: PID_SHELL,PID_TRI
      INTEGER, DIMENSION(NUMMAT,7) :: PID_SOL
      INTEGER :: IPID,ID,IGTYP,MODE
      
      my_real
     .        CC, INVTREF,A,B,A1,A2
      my_real, DIMENSION(TAILLE_OLD) :: CPUTIME_MP_OLD_2
      INTEGER :: SIZE_IRUP !Maximum number of rupture criteria


       DATA WTYPE /1.6 ,1. ,1. ,.9 ,1.1 ,1.4 ,0.65 ,.9 ,2.0/
!     ---------------------------------------------------------------------


      SIZE_IRUP = 0
      DO I = 1, NUMMAT
        SIZE_IRUP = MAX(SIZE_IRUP, IPM(220,I))
      ENDDO

      IF(IDDLEVEL==0) THEN
        POIN_PART_SHELL(1:2,1:NPART) = 0
        POIN_PART_TRI(1:2,1:NPART) = 0
        POIN_PART_SOL(1:2,1:NPART,1:7) = 0
        PID_SHELL(1:NUMMAT) = 0
        PID_TRI(1:NUMMAT) = 0
        PID_SOL(1:NUMMAT,1:7) = 0


        MODE = 0
        CALL INIT_MID_PID_ARRAY(MODE           ,TAILLE       ,NUMMAT     ,NPART  ,CONCORDANCE_MAT,
     1                        TAB_UMP        ,PID_SHELL    ,PID_TRI    ,PID_SOL,
     2                        MID_PID_SHELL  ,MID_PID_TRI  ,MID_PID_SOL,
     3                        IPART          ,IPM          ,GEO          ,CPUTIME_MP_OLD_2,
     4                        POIN_PART_SHELL,POIN_PART_TRI,POIN_PART_SOL)
         ENDIF
      CONCORDANCE_MAT(1:TAILLE) = 0
C-----------------------------------------------
      IF(DOMDEC_TUNING/=0)         WRITE(IOUT,'(A)')
     .  ' DOMAIN DECOMPOSITION : MANUAL TUNING'
      IF(DD_OPTIMIZATION==1) THEN
!       Skylake processor 
        WRITE(IOUT,'(A)')
     .  ' DOMAIN DECOMPOSITION : OPTIMIZED FOR SKYLAKE PROCESSOR'
      ELSEIF(DD_OPTIMIZATION==2) THEN
!       Sandy Bridge processor   
        WRITE(IOUT,'(A)')
     .  ' DOMAIN DECOMPOSITION : OPTIMIZED FOR SANDY BRIDGE PROCESSOR'
      ELSEIF(DD_OPTIMIZATION==3) THEN
!       ThunderX2 processor (ARMV8.0)      
        WRITE(IOUT,'(A)')
     .  ' DOMAIN DECOMPOSITION : OPTIMIZED FOR ARM64 PROCESSOR'
      ELSEIF(DD_OPTIMIZATION==0.OR.DD_OPTIMIZATION==4) THEN
!       Win64 machine --> AVX-2 Broadwell processor
        DD_OPTIMIZATION = 0
        WRITE(IOUT,'(A)')
     .  ' DOMAIN DECOMPOSITION : OPTIMIZED FOR BROADWELL PROCESSOR'
       ENDIF
C-----------------------------------------------
      I_PRO = 0
      I_PRO_36_2 = 0
      IF(  (TEST_POIDS==1).AND.
     .     (NUMMAT_OLD/=0).AND.
     .     (NUMGEO_OLD/=0).AND.
     .     (TAILLE_OLD/=0)      ) THEN


        MAX_ELM = -1
        MAX_ELM_OLD = -1
        MAX_ELM_36_2 = -1
        MAX_ELM_OLD_36_2 = -1
        DO I=1,TAILLE_OLD
         ILAW_OLD = TAB_UMP_OLD(6,I)
         MUID_OLD = TAB_UMP_OLD(1,I)
         MID_OLD  = TAB_UMP_OLD(3,I)
         PUID_OLD = TAB_UMP_OLD(2,I)
         ELM_TYP_OLD = TAB_UMP_OLD(7,I)
         PID_OLD  = TAB_UMP_OLD(4,I)

         IF(CPUTIME_MP_OLD(I)>ZERO) THEN
          DO J=1,TAILLE
           ILAW = TAB_UMP(6,J)
           MUID = TAB_UMP(1,J)
           MID  = TAB_UMP(3,J)
           PUID = TAB_UMP(2,J)
           ELM_TYP = TAB_UMP(7,J)
           PID  = TAB_UMP(4,J)

           IF((ILAW==ILAW_OLD).AND.(MUID_OLD==MUID).AND.
     .        (PUID_OLD==PUID).AND.(ELM_TYP==ELM_TYP_OLD) ) THEN
                 CONCORDANCE_MAT(J) = I
                 ! Check the material/property couple with the higher number of element
                 ! ILAW must be different from 0, 29, 30, 31 and 99 (user routines)
                 IF((ILAW/=0).OR.(ILAW/=29).OR.(ILAW/=30).OR.
     .              (ILAW/=31).OR.(ILAW<99))  THEN
                  MAX_ELM_OLD = MAX_ELM
                  MAX_ELM = MAX(MAX_ELM,TAB_UMP_OLD(5,I))
                  ! Material 2 or 36 are favoured  
                  IF((ILAW==2).OR.(ILAW==36)) THEN
                   MAX_ELM_OLD_36_2 = MAX_ELM_36_2
                   MAX_ELM_36_2 = MAX(MAX_ELM_36_2,TAB_UMP_OLD(5,I))
                   IF( (MAX_ELM_OLD_36_2<MAX_ELM_36_2) ) I_PRO_36_2 = J
                  ENDIF                   
                  IF( (MAX_ELM_OLD<MAX_ELM) ) I_PRO = J
                 ENDIF     
               ENDIF                 
          ENDDO
           ENDIF
         ENDDO

        ! --------------------------
        ! find the weight reference TELT_PRO
        I=0
        TEST_MAT = 0
        RECHERCHE = 1
        NUMEL_RE = 1
        K = 0
        OFF = 0
        IF(I_PRO_36_2>0) THEN
          K_36_2 = CONCORDANCE_MAT(I_PRO_36_2)
          NBR_ELM = TAB_UMP_OLD(5,K_36_2)
          IF(NBR_ELM>1024) I_PRO = I_PRO_36_2
        ENDIF
        IF(I_PRO>0) K = CONCORDANCE_MAT(I_PRO)
        IF(K/=0) THEN
          ELM_TYP_OLD = TAB_UMP_OLD(7,K)
          MLN = TAB_UMP_OLD(6,K)
          MID = TAB_UMP_OLD(3,K)
          PID = TAB_UMP_OLD(4,K)
! --------------------------
!         SHELL        
           IF(ELM_TYP_OLD==3) THEN
            TEST_MAT = 1
            CALL INITWG_SHELL(WD,PM,GEO,IXC,IGEO,SIZE_IRUP,
     .            NUMEL_RE,IPM,NUMMAT,NUMGEO,POIN_PART_SHELL,
     .            MID_PID_SHELL,IPARTC,OFF,BUFMAT,
     .            MID,PID,MLN,RECHERCHE,TELT_PRO,
     .            TABMP_L)
! --------------------------
!       TRI             
           ELSEIF(ELM_TYP_OLD==7) THEN
            TEST_MAT = 1
            CALL INITWG_TRI(WD,PM,GEO,IXTG,IGEO,NUMEL_RE,IPM ,SIZE_IRUP,
     .            NUMMAT,NUMGEO,POIN_PART_TRI,MID_PID_TRI,IPARTG,
     .            OFF,BUFMAT,MID,PID,MLN,RECHERCHE,TELT_PRO,
     .            TABMP_L)

! --------------------------
!       OTHER        
            ELSEIF((ELM_TYP_OLD==1004).OR.(ELM_TYP_OLD==1010).OR.
     .             (ELM_TYP_OLD==1)   .OR.(ELM_TYP_OLD==1006).OR.
     .             (ELM_TYP_OLD==1008).OR.(ELM_TYP_OLD==1016).OR.
     .             (ELM_TYP_OLD==1020))                          THEN
            TEST_MAT = 1
            IF(ELM_TYP_OLD>1000) THEN
             ISOL = ELM_TYP_OLD - 1000
            ELSE
             ISOL = 1
            ENDIF
            CALL  INITWG_SOLID(WD,PM,GEO,IXS,IGEO,ISOLNOD,
     .            NUMEL_RE,IPM ,SIZE_IRUP,
     .            NUMMAT,NUMGEO,
     .            POIN_PART_SOL,MID_PID_SOL,IPARTS,BUFMAT,
     .            MID,PID,MLN,RECHERCHE,ISOL,
     .            TELT_PRO,TABMP_L,NPART)
           ENDIF
           ! --------------------------
           ! Convert the old weight : new_weight = old_weight*reference_weight/reference_old_weight
           ! weight = 0 if new material/property couple
           ! --------------------------
           INVTELT_PRO = TELT_PRO/CPUTIME_MP_OLD(K)
           DO I=1,TAILLE_OLD
            IF(CPUTIME_MP_OLD(I)>ZERO) THEN
             CPUTIME_MP_OLD_2(I) = CPUTIME_MP_OLD(I) * INVTELT_PRO
            ELSE
             CPUTIME_MP_OLD_2(I) = ZERO
            ENDIF
           ENDDO
           MODE = 1
           CALL INIT_MID_PID_ARRAY(MODE           ,TAILLE       ,NUMMAT     ,NPART  ,CONCORDANCE_MAT,
     1                             TAB_UMP        ,PID_SHELL    ,PID_TRI    ,PID_SOL,
     2                             MID_PID_SHELL  ,MID_PID_TRI  ,MID_PID_SOL,
     3                             IPART          ,IPM          ,GEO          ,CPUTIME_MP_OLD_2,
     4                             POIN_PART_SHELL,POIN_PART_TRI,POIN_PART_SOL)
          ! --------------------------     
        ENDIF   ! K/=0
      ! --------------------------               
      ENDIF      ! (TEST_POIDS==1).AND. ...
C-----------------------------------------------
      RECHERCHE = 0
      MID = 0
      PID = 0
      MLN = 0
      ISOL = 0
C -------------------------------
C Element Property initialization
C -------------------------------
       CALL  INITWG_SOLID(WD,PM,GEO,IXS,IGEO,ISOLNOD,
     .            NUMELS,IPM ,SIZE_IRUP,
     .            NUMMAT,NUMGEO,
     .            POIN_PART_SOL,MID_PID_SOL,IPARTS,BUFMAT,
     .            MID,PID,MLN,RECHERCHE,ISOL,
     .            TELT_PRO,TABMP_L,NPART)
C      
      OFF = NUMELS
! -------------------- 
      CALL INITWG_QUAD(WD,PM,GEO,IXQ,IGEO,
     .                 NUMELQ,IPM,OFF)
C
      OFF = OFF + NUMELQ
! -------------------- 
       CALL INITWG_SHELL(WD,PM,GEO,IXC,IGEO,SIZE_IRUP,
     .            NUMELC,IPM,NUMMAT,NUMGEO,POIN_PART_SHELL,
     .            MID_PID_SHELL,IPARTC,OFF,BUFMAT,
     .            MID,PID,MLN,RECHERCHE,TELT_PRO,
     .            TABMP_L)
C
      OFF = OFF + NUMELC
! -------------------- 
      CALL INITWG_TRUSS(WD,PM,GEO,IXT,IGEO,
     .            NUMELT,IPM,NUMMAT,NUMGEO,OFF)
C     
      OFF = OFF + NUMELT
! -------------------- 
      CALL INITWG_POUTRE(WD,PM,GEO,IXP,IGEO,
     .            NUMELP,IPM,NUMMAT,NUMGEO,OFF)
C
      OFF = OFF + NUMELP
! -------------------- 
      CALL INITWG_RESSORT(WD,PM,GEO,IXR,IGEO,
     .            NUMELR,IPM,NUMMAT,NUMGEO,OFF)
C
      OFF = OFF + NUMELR
! -------------------- 
      CALL INITWG_TRI(WD,PM,GEO,IXTG,IGEO,NUMELTG,IPM , SIZE_IRUP,
     .            NUMMAT,NUMGEO,POIN_PART_TRI,MID_PID_TRI,IPARTG,
     .            OFF,BUFMAT,MID,PID,MLN,RECHERCHE,TELT_PRO,
     .            TABMP_L)
C
      OFF = OFF + NUMELTG
! -------------------- 
      CALL INITWG_UR(WD,PM,GEO,IXUR,IGEO,
     .            NUMELUR,IPM,NUMMAT,NUMGEO,OFF)
C
      OFF = OFF + NUMELUR
! -------------------- 
      CALL INITWG_X(WD,PM,GEO,KXX,IGEO,
     .            NUMELX,IPM,NUMMAT,NUMGEO,OFF)
C
      OFF = OFF + NUMELX
! -------------------- 

      RETURN
      END
