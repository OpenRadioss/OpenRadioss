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
!||    hm_read_monvol_type1_mod   ../starter/source/airbag/hm_read_monvol_type1.F
!||--- called by ------------------------------------------------------
!||    read_monvol                ../starter/source/airbag/read_monvol.F
!||====================================================================
      MODULE HM_READ_MONVOL_TYPE1_MOD
      CONTAINS
!||====================================================================
!||    hm_read_monvol_type1     ../starter/source/airbag/hm_read_monvol_type1.F
!||--- called by ------------------------------------------------------
!||    read_monvol              ../starter/source/airbag/read_monvol.F
!||--- calls      -----------------------------------------------------
!||    ancmsg                   ../starter/source/output/message/message.F
!||    freerr                   ../starter/source/starter/freform.F
!||    hm_get_intv              ../starter/source/devtools/hm_reader/hm_get_intv.F
!||    monvol_check_surfclose   ../starter/share/modules1/monvol_struct_mod.F
!||    monvol_compute_volume    ../starter/share/modules1/monvol_struct_mod.F
!||    monvol_orient_surf       ../starter/share/modules1/monvol_struct_mod.F
!||    monvol_reverse_normals   ../starter/share/modules1/monvol_struct_mod.F
!||--- uses       -----------------------------------------------------
!||    message_mod              ../starter/share/message_module/message_mod.F
!||    monvol_struct_mod        ../starter/share/modules1/monvol_struct_mod.F
!||    submodel_mod             ../starter/share/modules1/submodel_mod.F
!||====================================================================
      SUBROUTINE HM_READ_MONVOL_TYPE1(T_MONVOLN,
     .     UNITAB, LUID, IGRSURF,
     .     ITAB, X, PM, GEO, IXC, IXTG,LSUBMODEL)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE UNITAB_MOD
      USE GROUPDEF_MOD
      USE MESSAGE_MOD
      USE MONVOL_STRUCT_MOD
      USE SUBMODEL_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"      
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C     NSURF
#include      "com04_c.inc"
C     KMONVO, IREC
C     NIMV, NRVOLU
#include      "param_c.inc"
C     IIN
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE(UNIT_TYPE_), INTENT(IN) :: UNITAB
      INTEGER, INTENT(IN) :: LUID
      INTEGER, INTENT(IN) :: ITAB(*), IXC(NIXC, *), IXTG(NIXTG, *)
      my_real, INTENT(IN) :: X(3, *), GEO(NPROPG, *), PM(NPROPM, *)
      TYPE (SURF_), INTENT(INOUT), DIMENSION(NSURF) :: IGRSURF
      TYPE(MONVOL_STRUCT_), INTENT(INOUT) :: T_MONVOLN
      TYPE(SUBMODEL_DATA), DIMENSION(NSUBMOD), INTENT(IN) :: LSUBMODEL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: II
      INTEGER :: SURFID
      my_real :: FAC_M, FAC_L, FAC_T, FAC_C
      my_real :: SCAL_T, SCAL_P, SCAL_S, SCAL_A, SCAL_D
      LOGICAL :: FOUND
      my_real :: SA, ROT, VOL, VMIN, VEPS, AMU, SV
      LOGICAL :: IS_AVAILABLE
C-----------------------------------------------
C     B e g i n n i n g   o f   s o u r c e
C-----------------------------------------------

      IS_AVAILABLE = .FALSE.
C     =======
C     Reading
C     =======
C     Line 1
      CALL HM_GET_INTV('surf_IDex', SURFID, IS_AVAILABLE, LSUBMODEL)
C     ================
C     Check operations
C     ================
C     External surface check
      T_MONVOLN%IVOLU(4) = 0
      FOUND = .FALSE.
      DO II = 1, NSURF
         IF (SURFID == IGRSURF(II)%ID) THEN
            T_MONVOLN%IVOLU(4) = II
            T_MONVOLN%EXT_SURFID = II
            FOUND = .TRUE.
            EXIT
         ENDIF
      ENDDO
      IF (.NOT. FOUND) THEN
         CALL FREERR(3)
      ELSEIF (IGRSURF(T_MONVOLN%IVOLU(4))%ISH4N3N == 0) THEN
         CALL ANCMSG(MSGID = 18, ANMODE = ANINFO, MSGTYPE = MSGERROR,
     .        I1 = T_MONVOLN%ID, C1 = T_MONVOLN%TITLE, I2 = SURFID)
         CALL FREERR(3)
      ENDIF
      
C     Check surface closure
      CALL MONVOL_CHECK_SURFCLOSE(T_MONVOLN, ITAB, IGRSURF(T_MONVOLN%EXT_SURFID), X)
C     Set all normal on same side
      CALL MONVOL_ORIENT_SURF(T_MONVOLN, T_MONVOLN%TITLE, T_MONVOLN%IVOLU, ITAB, 
     .     IGRSURF(T_MONVOLN%EXT_SURFID),IXC, IXTG, X, 1)
C     Compute Monvon volume
      CALL MONVOL_COMPUTE_VOLUME(T_MONVOLN, T_MONVOLN%TITLE, T_MONVOLN%IVOLU, IGRSURF(T_MONVOLN%EXT_SURFID), 
     .     ITAB, X, PM, GEO, IXC, IXTG, 
     .     SA, ROT, VOL, VMIN, VEPS, SV)
C     Reverse all normals to ensure positive volume
      CALL MONVOL_REVERSE_NORMALS(T_MONVOLN, T_MONVOLN%TITLE, T_MONVOLN%IVOLU, ITAB, 
     .     IGRSURF(T_MONVOLN%EXT_SURFID),IXC,IXTG,VOL, X, 1)
     
C     =====
C     Units
C     =====
      FAC_M = UNITAB%FAC_M(LUID)
      FAC_L = UNITAB%FAC_L(LUID)
      FAC_T = UNITAB%FAC_T(LUID)
      FAC_C = FAC_M / (FAC_L * FAC_T * FAC_T)

C     Default values
      SCAL_T = ONE * FAC_T
      SCAL_P = ONE * FAC_C
      SCAL_S = ONE * FAC_L * FAC_L
      SCAL_A = ONE 
      SCAL_D = ONE * FAC_L

C     =====
C     Store
C     ===== 
      T_MONVOLN%RVOLU(26) = ONE / SCAL_T
      T_MONVOLN%RVOLU(27) = ONE / SCAL_P
      T_MONVOLN%RVOLU(28) = ONE / SCAL_S
      T_MONVOLN%RVOLU(29) = ONE / SCAL_A
      T_MONVOLN%RVOLU(30) = ONE / SCAL_D
C
      AMU = ZERO
      T_MONVOLN%RVOLU(2) = AMU
      T_MONVOLN%RVOLU(16) = VOL + VEPS
      T_MONVOLN%RVOLU(18) = SA
      T_MONVOLN%RVOLU(21) = ROT
      T_MONVOLN%RVOLU(22:24) = ZERO

C     =========
C     Print out
C     =========
      WRITE(IOUT, 1005) SURFID
      WRITE(IOUT, 1003) SCAL_T, SCAL_P, SCAL_S, SCAL_A, SCAL_D
      WRITE(IOUT, 1002) SA, SV, VOL
C-----------------------------------------------
C     E n d   o f   s o u r c e
C-----------------------------------------------
      
      RETURN
 1002 FORMAT(
     .     /5X,'INITIAL SURFACE OF MONITORED VOLUME . .=',1PG20.13,
     .     /5X,'SURFACE ERROR(NE.0 FOR NON CLOSED SURF)=',1PG20.13,
     .     /5X,'INITIAL VOLUME OF MONITORED VOLUME. . .=',1PG20.13)     
 1003 FORMAT(
     .     5X,'UNIT SCALE FOR TIME FUNCTIONS          =',1PG20.13,
     .     /5X,'UNIT SCALE FOR PRESSURE FUNCTIONS      =',1PG20.13,
     .     /5X,'UNIT SCALE FOR AREA FUNCTIONS          =',1PG20.13,
     .     /5X,'UNIT SCALE FOR ANGLE FUNCTIONS         =',1PG20.13,
     .     /5X,'UNIT SCALE FOR DISTANCE FUNCTIONS      =',1PG20.13)
 1005 FORMAT( 5X,'EXTERNAL SURFACE ID . . . . . . . . . .=',I10)
      END SUBROUTINE HM_READ_MONVOL_TYPE1
      END MODULE HM_READ_MONVOL_TYPE1_MOD
