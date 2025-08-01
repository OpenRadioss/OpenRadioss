Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
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
!||====================================================================
!||    hm_read_perturb              ../starter/source/general_controls/computation/hm_read_perturb.F
!||--- called by ------------------------------------------------------
!||    lectur                       ../starter/source/starter/lectur.F
!||--- calls      -----------------------------------------------------
!||    hm_option_count              ../starter/source/devtools/hm_reader/hm_option_count.F
!||    hm_read_perturb_fail         ../starter/source/general_controls/computation/hm_read_perturb_fail.F
!||    hm_read_perturb_part_shell   ../starter/source/general_controls/computation/hm_read_perturb_part_shell.F
!||    hm_read_perturb_part_solid   ../starter/source/general_controls/computation/hm_read_perturb_part_solid.F
!||    udouble                      ../starter/source/system/sysfus.F
!||--- uses       -----------------------------------------------------
!||    hm_option_read_mod           ../starter/share/modules1/hm_option_read_mod.F
!||    message_mod                  ../starter/share/message_module/message_mod.F
!||    submodel_mod                 ../starter/share/modules1/submodel_mod.F
!||====================================================================
      SUBROUTINE HM_READ_PERTURB(MAT_PARAM,
     1      IPART   ,RNOISE  ,IPARTC   ,IPARTG    ,IPARTSP  ,
     2      IGRPART ,IPM     ,IPARTS   ,PERTURB   ,QP_IPERTURB,
     3      QP_RPERTURB      ,LSUBMODEL,UNITAB  )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE GROUPDEF_MOD
      USE UNITAB_MOD
      USE SUBMODEL_MOD
      USE HM_OPTION_READ_MOD
      USE MAT_ELEM_MOD    
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "scr17_c.inc"
#include      "param_c.inc"
#include      "sphcom.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB
      my_real
     .   RNOISE(NPERTURB,NUMELC+NUMELTG+NUMELS+NUMSPH),
     .   QP_RPERTURB(NPERTURB,4)
c     . RNOISE(*)
      INTEGER IPART(LIPART1,*),IPARTC(*),IPARTSP(*),IPARTG(*),IPARTS(*),
     .   IPM(NPROPMI,*),PERTURB(NPERTURB),QP_IPERTURB(NPERTURB,6)
      TYPE(SUBMODEL_DATA) LSUBMODEL(*)
      TYPE (MATPARAM_STRUCT_) ,DIMENSION(NUMMAT) ,INTENT(INOUT) :: MAT_PARAM
C-----------------------------------------------
      TYPE (GROUP_)  , DIMENSION(NGRPART)  :: IGRPART
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER FLAG_FMT,I,J,K,ICOUNT,
     .   NUMA,I_METHOD,UID,
     .   CPT_PART,NB_RANDOM,I_SEED,DISTRIB(50),
     .   II,NB_INTERV,SIZEY,IGRPRT,N,
     .   IOK,SEED,SEED_RANDOM,IFAILCRIT,ITYP,IFAILMAT,
     .   L,IFAILTYPE,I_PERTURB_VAR,EMPTY,IDPERTURB(NPERTURB),
     .   IGRPRTS,NPART_SHELL,NPART_SOLID,OFFS
      INTEGER, DIMENSION(:), ALLOCATABLE :: INDEX,INDEX_ITYP
      my_real :: MEAN,SD,MEAN_INPUT,SD_INPUT,MAX_DISTRIB,TEMP,MIN_VALUE,
     . MAX_VALUE,INTERV,VALUE,MAX_VALUE1,MINVAL,MAXVAL,BID
      my_real, DIMENSION(:), ALLOCATABLE :: ARRAY
      CHARACTER MESS*40
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      DATA MESS/'PERTURBATION DEFINITION            '/
C=======================================================================
      ! Initialization and allocation of tables
      ITYP     = 0
      OFFS     = 0
      BID = ZERO
      IDPERTURB(1:NPERTURB) = 0
c
      ALLOCATE(INDEX(NUMELC+NUMELTG+NUMELS+NUMSPH))
      ALLOCATE(INDEX_ITYP(NUMELC+NUMELTG+NUMELS+NUMSPH))
      INDEX(:) = 0
      INDEX_ITYP(:) = 0
c
c     Counting PERTURB type
      CALL HM_OPTION_COUNT('/PERTURB/PART/SHELL',NPART_SHELL)  
      CALL HM_OPTION_COUNT('/PERTURB/PART/SOLID',NPART_SOLID)  
c
c     /PERTURB/PART/SHELL
c
      IF (NPART_SHELL > 0) THEN 
        ! Reading routine
        CALL HM_READ_PERTURB_PART_SHELL(
     .      IPART   ,RNOISE   ,IPARTC ,IPARTG    ,IGRPART   ,
     .      IPM     ,PERTURB  ,LSUBMODEL,UNITAB  ,IDPERTURB ,
     .      INDEX   ,INDEX_ITYP,NPART_SHELL,OFFS,QP_IPERTURB,
     .      QP_RPERTURB)
        ! Computing the offset
        OFFS = OFFS + NPART_SHELL
      ENDIF
c
c     /PERTURB/PART/SOLID
c
      IF (NPART_SOLID > 0) THEN 
        ! Reading routine
        CALL HM_READ_PERTURB_PART_SOLID(
     .      IPART   ,RNOISE   ,IGRPART ,IPM     ,IPARTS   ,
     .      PERTURB,LSUBMODEL ,UNITAB  ,IDPERTURB,INDEX   ,
     .      INDEX_ITYP,NPART_SOLID     ,OFFS  ,QP_IPERTURB,
     .      QP_RPERTURB)
        ! Computing the offset
        OFFS = OFFS + NPART_SOLID
      ENDIF
c
c     /PERTURB/FAIL/BIQUAD
c
      CALL HM_READ_PERTURB_FAIL(MAT_PARAM,
     .     IPART    ,RNOISE   ,IPARTC ,IPARTG    ,IPARTSP  ,
     .     IGRPART  ,IPARTS   ,PERTURB   ,IDPERTURB,
     .     INDEX   ,INDEX_ITYP,NPART_SHELL,OFFS ,QP_IPERTURB,
     .     QP_RPERTURB,LSUBMODEL,UNITAB)
c
c-------------------------------------------------------------
      ! Checking for doubled IDs
      CALL UDOUBLE(IDPERTURB,1,NPERTURB,MESS,0,BID)
c-------------------------------------------------------------
      IF (ALLOCATED(INDEX))      DEALLOCATE(INDEX)
      IF (ALLOCATED(INDEX_ITYP)) DEALLOCATE(INDEX_ITYP)
c-----------
      RETURN
      END


