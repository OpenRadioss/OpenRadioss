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
Chd|  HM_READ_FAIL_CONNECT          source/materials/fail/connect/hm_read_fail_connect.F
Chd|-- called by -----------
Chd|        HM_READ_FAIL                  source/materials/fail/hm_read_fail.F
Chd|-- calls ---------------
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_OPTION_IS_ENCRYPTED        source/devtools/hm_reader/hm_option_is_encrypted.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_FAIL_CONNECT(
     .           UPARAM   ,MAXUPARAM,NUPARAM  ,NUVAR  ,IFUNC    ,
     .           MAXFUNC  ,NFUNC    ,UNITAB   ,ID     ,FAIL_ID  ,
     .           LSUBMODEL,UNIT_ID)
C-----------------------------------------------
C   ROUTINE DESCRIPTION :
C   ===================
C   READ FAILURE CONNECT 
C-----------------------------------------------
C   DUMMY ARGUMENTS DESCRIPTION:
C   ===================
C
C     NAME            DESCRIPTION                         
C
C     UNITAB          UNITS ARRAY
C     FAIL_ID         FAILURE ID(INTEGER)
C     TITR            MATERIAL TITLE
C     LSUBMODEL       SUBMODEL STRUCTURE    
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE UNITAB_MOD
      USE MESSAGE_MOD 
      USE SUBMODEL_MOD
      USE HM_OPTION_READ_MOD 
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C INPUT ARGUMENTS
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB 
      INTEGER,INTENT(IN)             ::ID,FAIL_ID,UNIT_ID
      TYPE(SUBMODEL_DATA),INTENT(IN) ::LSUBMODEL(*)
C MODIFIED ARGUMENT
      INTEGER,INTENT(INOUT)          ::IFUNC(MAXFUNC),NFUNC,MAXFUNC,MAXUPARAM
      my_real,INTENT(INOUT)          ::UPARAM(MAXUPARAM)
      INTEGER,INTENT(INOUT)            ::NUPARAM,NUVAR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C     REAL
      INTEGER    ::     IFUNN,IFUNT,IFAIL,IFAILS,IFAILE,ISOLID,NFAIL,ISYM,IUNIT,IFLAGUNIT
      my_real    ::     MAXDN,MAXDT,EXPN,EXPT,FACN,FACT,ASCALE,MAXEI,MAXEN,MAXET,NN,NT,
     .                  DTIME,DSOFT,AREASCALE, UNIT_T
C-----------------------------------------------
      LOGICAL    ::     IS_AVAILABLE,IS_ENCRYPTED
C=======================================================================
      IS_ENCRYPTED   = .FALSE.
      IS_AVAILABLE = .FALSE.
C--------------------------------------------------
C EXTRACT DATA (IS OPTION CRYPTED)
C--------------------------------------------------
      CALL HM_OPTION_IS_ENCRYPTED(IS_ENCRYPTED)
C--------------------------------------------------
C EXTRACT DATAS (REAL VALUES)
C--------------------------------------------------
Card1
      CALL HM_GET_FLOATV         ('Epsilon_maxN',MAXDN ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV         ('Exponent_N'  ,EXPN  ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV         ('Alpha_N'     ,FACN  ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_INTV           ('R_fct_ID_N'  ,IFUNN ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV           ('Ifail'       ,IFAIL ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV           ('Ifail_so'    ,ISOLID,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV           ('ISYM'        ,ISYM  ,IS_AVAILABLE,LSUBMODEL)
Card2
      CALL HM_GET_FLOATV         ('Epsilon_maxT',MAXDT  ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV         ('Exponent_T'  ,EXPT   ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV         ('Alpha_T'     ,FACT   ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_INTV           ('R_fct_ID_T'  ,IFUNT  ,IS_AVAILABLE,LSUBMODEL)
Card3
      CALL HM_GET_FLOATV         ('EI_max'      ,MAXEI  ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV         ('EN_max'      ,MAXEN  ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV         ('ET_max'      ,MAXET  ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV         ('N_n'         ,NN     ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV         ('N_t'         ,NT     ,IS_AVAILABLE,LSUBMODEL,UNITAB)
Card4
      CALL HM_GET_FLOATV         ('T_max'       ,DTIME     ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV         ('N_soft'      ,DSOFT     ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV         ('AREA_scale'  ,AREASCALE ,IS_AVAILABLE,LSUBMODEL,UNITAB)
C-------------------------------------------- 
      NFAIL = 1
      IF (ISOLID == 0) THEN
        ISOLID = 1
      ELSE IF (ISOLID == 2) THEN 
        NFAIL = 4
      END IF 
      IF (MAXDN == ZERO) MAXDN = INFINITY
      IF (MAXDT == ZERO) MAXDT = INFINITY
      IF (MAXEI == ZERO) MAXEI = INFINITY
      IF (MAXEN == ZERO) MAXEN = INFINITY
      IF (MAXET == ZERO) MAXET = INFINITY
      IF (FACN == ZERO)  FACN  = ONE
      IF (FACT == ZERO)  FACT  = ONE
      IF (EXPN == ZERO)  EXPN  = ONE
      IF (EXPT == ZERO)  EXPT  = ONE
      IF (NN    == ZERO) NN    = ONE
      IF (NT    == ZERO) NT    = ONE
c-----------------------------------------------------------------------
c     Rupture model flag 
c 
c     IFAILS =  1 => strain based unidimensional rupture
c     IFAILS =  2 => strain based multidimensional rupture
c     IFAILE =  1 => total energy based rupture
c     IFAILE =  2 => component energy based rupture
c     IFAILE =  3 => total and component energy based rupture
c
      IFAILS = 0
      IFAILE = 0
      IF (MAXDN < INFINITY .OR. MAXDT < INFINITY) IFAILS = IFAIL + 1
      IF (MAXEI < INFINITY) IFAILE = IFAILE + 1
      IF (MAXEN < INFINITY .OR. MAXET < INFINITY) IFAILE = IFAILE + 2
c-----------------------------------------------------------------------
      IF (UNIT_ID > 0) THEN
        UNIT_T = UNITAB%FAC_T(UNIT_ID)
      ELSE
        UNIT_T = ONE
      ENDIF
      ASCALE = ONE / UNIT_T
c-----------------------------------------------------------------------
      UPARAM(1)  = MAXDN
      UPARAM(2)  = MAXDT
      UPARAM(3)  = EXPN
      UPARAM(4)  = EXPT
      UPARAM(5)  = FACN
      UPARAM(6)  = FACT
      UPARAM(7)  = ASCALE
      UPARAM(8)  = IFAILS
      UPARAM(9)  = IFAILE
      UPARAM(10) = NFAIL
      UPARAM(11) = MAXEI
      UPARAM(12) = MAXEN
      UPARAM(13) = MAXET
      UPARAM(14) = NN
      UPARAM(15) = NT
      UPARAM(16) = DTIME
      UPARAM(17) = DSOFT
      UPARAM(18) = ISYM
      UPARAM(19) = AREASCALE
C    
      IFUNC(1) = IFUNN
      IFUNC(2) = IFUNT
C    
      NUPARAM = 19
      NFUNC   = 2
      NUVAR   = 13
C--------------------------------------------------
      IF (IS_ENCRYPTED) THEN
        WRITE(IOUT, 1000)
      ELSE
        WRITE(IOUT, 1100) MAXDN,MAXDT,EXPN,EXPT,IFAIL,ISOLID,ISYM,
     .                    IFUNN,IFUNT,FACN,FACT,
     .                    MAXEI,MAXEN,MAXET,NN,NT,DTIME,DSOFT,AREASCALE

      ENDIF
C--------------------------------------------------
      RETURN
 1000 FORMAT(
     & 5X,'    CRYPTED DATA IN FAILURE MODEL             '/,
     & 5X,'    -----------------------------             '/)
 1100 FORMAT(
     & 5X,'    SOLID CONNECTION RUPTURE MODEL            ',/
     & 5X,'    ------------------------------            ',/
     & 5X,'Max deformation in normal direction  . . . . . . . =',E16.9/
     & 5X,'Max deformation in tangent direction . . . . . . . =',E16.9/
     & 5X,'Deformation exponent in normal direction . . . . . =',E16.9/
     & 5X,'Deformation exponent in tangent direction. . . . . =',E16.9/
     & 5X,'Multi-directional rupture flag                 . . =',I10/
     & 5X,'Integration point rupture flag                 . . =',I10/
     & 5X,'Rupture deactivation flag in compression       . . =',I10/
     & 5X,'Strain rate scale function in normal direction . . =',I10/
     & 5X,'Strain rate scale function in tangent direction. . =',I10/
     & 5X,'Function scale factor in normal direction  . . . . =',E16.9/
     & 5X,'Function scale factor in tangent direction . . . . =',E16.9/
     & 5X,'Maximum total internal energy    . . . . . . . . . =',E16.9/
     & 5X,'Maximum energy in normal direction       . . . . . =',E16.9/
     & 5X,'Maximum energy in tangent direction      . . . . . =',E16.9/
     & 5X,'Energy exponent in normal direction      . . . . . =',E16.9/
     & 5X,'Energy exponent in tangent direction     . . . . . =',E16.9/
     & 5X,'Damage duration parameter      . . . . . . . . . . =',E16.9/
     & 5X,'Damage softening parameter     . . . . . . . . . . =',E16.9/ 
     & 5X,'Element deformation parameter  . . . . . . . . . . =',E16.9/)
C-----------       
      RETURN
      END
