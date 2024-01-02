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
Chd|  LAGM_INI                      source/tools/lagmul/lagm_ini.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        LGMINI_BC                     source/tools/lagmul/lgmini_bc.F
Chd|        LGMINI_FXV                    source/tools/lagmul/lgmini_fxv.F
Chd|        LGMINI_GJ                     source/tools/lagmul/lgmini_gj.F
Chd|        LGMINI_I2                     source/tools/lagmul/lgmini_i2.F
Chd|        LGMINI_MPC                    source/tools/lagmul/lgmini_mpc.F
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE LAGM_INI(NHF    ,IADHF       ,IADLL  ,JLL    ,LLL    ,
     2                    IPARI  ,INTBUF_TAB  ,IGRNOD ,        IBCSLAG,
     3                    MASS   ,INER        ,GJBUFI ,IBUFNC ,IBUFNN ,
     4                    IBUFDL ,IBUFSK      ,IBFV   ,VEL    ,ITAB   ,
     5                 NOM_OPT,PTR_NOPT_INTER,PTR_NOPT_FXV,PTR_NOPT_BCS,
     6                 PTR_NOPT_MPC,PTR_NOPT_GJOINT)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE INTBUFDEF_MOD
      USE GROUPDEF_MOD
C----------------------------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com04_c.inc"
#include      "lagmult.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NHF
      INTEGER IADHF(*), IADLL(*), JLL(*), LLL(*),
     .        IPARI(NPARI,NINTER),IBCSLAG(5,*),
     .        GJBUFI(LKJNI,*),IBFV(NIFV,*),
     .        IBUFNC(*),IBUFNN(*),IBUFDL(*),IBUFSK(*),ITAB(*)
      my_real
     .        VEL(LFXVELR,*),MASS(*),INER(*)
      INTEGER NOM_OPT(LNOPT1,*),PTR_NOPT_INTER,PTR_NOPT_FXV,
     .        PTR_NOPT_BCS,PTR_NOPT_MPC,PTR_NOPT_GJOINT
      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
C-----------------------------------------------
      TYPE (GROUP_)  , DIMENSION(NGRNOD)  :: IGRNOD
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER, ALLOCATABLE, DIMENSION(:,:) :: LTSM
      INTEGER IC, IK, J, JC, NCF, HIJ, ERR
C======================================================================|
      ALLOCATE(LTSM(6,NUMNOD),  STAT=ERR)
      IF (ERR /= 0) THEN
        CALL ANCMSG(MSGID=268,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .                         C1='LTSM')
      ENDIF
      NCF      = 0
      LTSM     = 0
      NHF      = 1
      IADLL(1) = 1
      IADHF(1) = 1
c---
      IF (NBCSLAG > 0)
     .  CALL LGMINI_BC(IADLL  ,JLL    ,LLL   ,IGRNOD   ,IBCSLAG,
     .                 MASS   ,INER   ,NCF   ,NOM_OPT(1,PTR_NOPT_BCS+1))
      CALL LGMINI_I2(IADLL  ,JLL    ,LLL    ,INTBUF_TAB  ,IPARI  ,NCF,
     .               MASS, ITAB    ,NOM_OPT(1,PTR_NOPT_INTER+1))
      IF (NGJOINT > 0)
     .  CALL LGMINI_GJ (IADLL  ,JLL    ,LLL    ,GJBUFI ,NCF, MASS,
     .                  INER, ITAB    ,NOM_OPT(1,PTR_NOPT_GJOINT+1))
      IF (NUMMPC > 0)
     .  CALL LGMINI_MPC(IADLL  ,JLL    ,LLL    ,IBUFNC ,IBUFNN ,
     .                  IBUFDL ,IBUFSK ,NCF, MASS, INER, ITAB    ,
     .                  NOM_OPT(1,PTR_NOPT_MPC+1))
      IF (NFVLAG > 0)
     .  CALL LGMINI_FXV(IADLL  ,JLL    ,LLL    ,IBFV   ,VEL    ,
     .                  NCF, MASS, INER, ITAB,NOM_OPT(1,PTR_NOPT_FXV+1))
c---
      DO IC=1,NCF
        DO IK=IADLL(IC),IADLL(IC+1)-1
          LTSM(JLL(IK),LLL(IK)) = 1
        ENDDO
        DO JC=IC+1,NCF
          HIJ = 0
          DO IK=IADLL(JC),IADLL(JC+1)-1
            HIJ = HIJ + LTSM(JLL(IK),LLL(IK))
          ENDDO
          IF (HIJ > 0) NHF = NHF + 1
        ENDDO
        IADHF(IC+1) = NHF
        DO IK=IADLL(IC),IADLL(IC+1)-1
          LTSM(JLL(IK),LLL(IK)) = 0
        ENDDO
      ENDDO
      NHF = NHF-1
      IF (ALLOCATED(LTSM)) DEALLOCATE(LTSM)
C-----------
      RETURN
      END

