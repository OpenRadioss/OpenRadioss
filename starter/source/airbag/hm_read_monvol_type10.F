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
!||    hm_read_monvol_type10_mod   ../starter/source/airbag/hm_read_monvol_type10.F
!||--- called by ------------------------------------------------------
!||    read_monvol                 ../starter/source/airbag/read_monvol.F
!||====================================================================
      MODULE HM_READ_MONVOL_TYPE10_MOD
      CONTAINS
!||====================================================================
!||    hm_read_monvol_type10    ../starter/source/airbag/hm_read_monvol_type10.F
!||--- called by ------------------------------------------------------
!||    read_monvol              ../starter/source/airbag/read_monvol.F
!||--- calls      -----------------------------------------------------
!||    ancmsg                   ../starter/source/output/message/message.F
!||    check_function_id        ../starter/source/tools/curve/check_function.F
!||    freerr                   ../starter/source/starter/freform.F
!||    hm_get_floatv            ../starter/source/devtools/hm_reader/hm_get_floatv.F
!||    hm_get_floatv_dim        ../starter/source/devtools/hm_reader/hm_get_floatv_dim.F
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
      SUBROUTINE HM_READ_MONVOL_TYPE10(T_MONVOLN, UNITAB, NPC, IGRSURF, ITAB, X, PM, GEO, IXC, IXTG, LSUBMODEL)
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
C     NSURF, NFUNCT
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
      INTEGER, INTENT(IN) :: NPC(*), ITAB(*), IXC(NIXC, *), IXTG(NIXTG, *)
      my_real, INTENT(IN) :: X(3, *), GEO(NPROPG, *), PM(NPROPM, *)
      TYPE (SURF_), INTENT(INOUT), DIMENSION(NSURF) :: IGRSURF
      TYPE(MONVOL_STRUCT_), INTENT(INOUT) :: T_MONVOLN
      TYPE(SUBMODEL_DATA), DIMENSION(NSUBMOD), INTENT(IN) :: LSUBMODEL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: II
      INTEGER :: SURFID, IFUNCT_ID(6), IFBULK, IFMIN, IFMOUTT, IFMOUTP, IFP0, IFPMAX
      my_real :: FAC_GEN
      my_real :: SCAL_T, SCAL_P
      my_real :: SA, ROT, VOL, VMIN, VEPS, AMU, SV
      my_real :: RHOI, SFBULK, SFMIN, SFMOUTT, SFMOUTP, SFP0, SFPMAX
      LOGICAL :: FOUND
      LOGICAL :: IS_AVAILABLE
C-----------------------------------------------
C     B e g i n n i n g   o f   s o u r c e
C-----------------------------------------------
C     =======
C     Reading
C     =======
C     Line 1
      CALL HM_GET_INTV('surf_IDex', SURFID, IS_AVAILABLE, LSUBMODEL)
C     Line 2
      CALL HM_GET_FLOATV('Ascalet', SCAL_T, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('AscaleP', SCAL_P, IS_AVAILABLE, LSUBMODEL, UNITAB)
C     Line 3
      CALL HM_GET_FLOATV('Rho', RHOI, IS_AVAILABLE, LSUBMODEL, UNITAB)
C     Line 4
      CALL HM_GET_INTV('fct_K', IFBULK, IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_INTV('fct_Mtin', IFMIN, IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_FLOATV('Fscale_K', SFBULK, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('Fscale_Mtin', SFMIN, IS_AVAILABLE, LSUBMODEL, UNITAB)
C     Line 5 
      CALL HM_GET_INTV('fct_Mtout', IFMOUTT, IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_INTV('fct_Mpout', IFMOUTP, IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_FLOATV('Fscale_Mtout', SFMOUTT, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('Fscale_Mpout', SFMOUTP, IS_AVAILABLE, LSUBMODEL, UNITAB)
C     Line 6
      CALL HM_GET_INTV('fct_Padd', IFP0, IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_INTV('fct_Pmax', IFPMAX, IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_FLOATV('Fscale_Padd', SFP0, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('Fscale_Pmax', SFPMAX, IS_AVAILABLE, LSUBMODEL, UNITAB)
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
      ELSEIF (IGRSURF(T_MONVOLN%EXT_SURFID)%ISH4N3N == 0) THEN
         CALL ANCMSG(MSGID = 18, ANMODE = ANINFO, MSGTYPE = MSGERROR,
     .        I1 = T_MONVOLN%ID, C1 = T_MONVOLN%TITLE, I2 = SURFID)
         CALL FREERR(3)
      ENDIF
      
C     Check surface closure
      CALL MONVOL_CHECK_SURFCLOSE(T_MONVOLN, ITAB, IGRSURF(T_MONVOLN%EXT_SURFID), X)
C     Set all normal on same side
      CALL MONVOL_ORIENT_SURF(T_MONVOLN, T_MONVOLN%TITLE, T_MONVOLN%IVOLU, ITAB, 
     .     IGRSURF(T_MONVOLN%EXT_SURFID),IXC, IXTG, X, 10)
C     Compute Monvon volume
      CALL MONVOL_COMPUTE_VOLUME(T_MONVOLN, T_MONVOLN%TITLE, T_MONVOLN%IVOLU, IGRSURF(T_MONVOLN%EXT_SURFID), 
     .     ITAB, X, PM, GEO, IXC, IXTG, 
     .     SA, ROT, VOL, VMIN, VEPS, SV)
C     Reverse all normals to ensure positive volume
      CALL MONVOL_REVERSE_NORMALS(T_MONVOLN, T_MONVOLN%TITLE, T_MONVOLN%IVOLU, ITAB, 
     .     IGRSURF(T_MONVOLN%EXT_SURFID),IXC,IXTG,VOL, X, 10)
     

      IFUNCT_ID(1:6) = 0
      IF (IFBULK > 0) THEN
         CALL CHECK_FUNCTION_ID(NPC, NFUNCT, IFBULK, IFUNCT_ID(1), FOUND)
         IF (.NOT. FOUND) THEN
            CALL ANCMSG(MSGID = 9, ANMODE = ANINFO, MSGTYPE = MSGERROR,
     .           I2 = IFBULK, I1 = T_MONVOLN%ID, C1 = T_MONVOLN%TITLE, C2 = 'BULK')
         ENDIF
      ENDIF
      IF (IFMIN > 0) THEN
         CALL CHECK_FUNCTION_ID(NPC, NFUNCT, IFMIN, IFUNCT_ID(2), FOUND)
         IF (.NOT. FOUND) THEN
            CALL ANCMSG(MSGID = 9, ANMODE = ANINFO, MSGTYPE = MSGERROR,
     .           I2 = IFMIN, I1 = T_MONVOLN%ID, C1 = T_MONVOLN%TITLE, C2 = 'MASS FLOW')
         ENDIF
      ENDIF
      IF (IFMOUTT > 0) THEN
         CALL CHECK_FUNCTION_ID(NPC, NFUNCT, IFMOUTT, IFUNCT_ID(3), FOUND)
         IF (.NOT. FOUND) THEN
            CALL ANCMSG(MSGID = 9, ANMODE = ANINFO, MSGTYPE = MSGERROR,
     .           I2 = IFMOUTT, I1 = T_MONVOLN%ID, C1 = T_MONVOLN%TITLE, C2 = 'MASS FLOW')
         ENDIF
      ENDIF
      IF (IFMOUTP > 0) THEN
         CALL CHECK_FUNCTION_ID(NPC, NFUNCT, IFMOUTP, IFUNCT_ID(4), FOUND)
         IF (.NOT. FOUND) THEN
            CALL ANCMSG(MSGID = 9, ANMODE = ANINFO, MSGTYPE = MSGERROR,
     .           I2 = IFMOUTP, I1 = T_MONVOLN%ID, C1 = T_MONVOLN%TITLE, C2 = 'MASS FLOW')
         ENDIF
      ENDIF
      IF (IFP0 > 0) THEN
         CALL CHECK_FUNCTION_ID(NPC, NFUNCT, IFP0, IFUNCT_ID(5), FOUND)
         IF (.NOT. FOUND) THEN
            CALL ANCMSG(MSGID = 9, ANMODE = ANINFO, MSGTYPE = MSGERROR,
     .           I2 = IFP0, I1 = T_MONVOLN%ID, C1 = T_MONVOLN%TITLE, C2 = 'PRESSURE')
         ENDIF
      ENDIF
      IF (IFPMAX > 0) THEN
         CALL CHECK_FUNCTION_ID(NPC, NFUNCT, IFPMAX, IFUNCT_ID(6), FOUND)
         IF (.NOT. FOUND) THEN
            CALL ANCMSG(MSGID = 9, ANMODE = ANINFO, MSGTYPE = MSGERROR,
     .           I2 = IFPMAX, I1 = T_MONVOLN%ID, C1 = T_MONVOLN%TITLE, C2 = 'PRESSURE')
         ENDIF
      ENDIF


C     Default value for time scale factor
      IF (SCAL_T == ZERO) THEN
         CALL HM_GET_FLOATV_DIM('Ascalet', FAC_GEN, IS_AVAILABLE, LSUBMODEL, UNITAB)     
         SCAL_T = ONE * FAC_GEN 
      ENDIF
      IF (SCAL_P == ZERO) THEN
         CALL HM_GET_FLOATV_DIM('AscaleP', FAC_GEN, IS_AVAILABLE, LSUBMODEL, UNITAB)     
         SCAL_P = ONE * FAC_GEN 
      ENDIF
C     Default value
      IF (IFBULK > 0) THEN
         IF (SFBULK == ZERO) THEN
            CALL HM_GET_FLOATV_DIM('Fscale_K', FAC_GEN, IS_AVAILABLE, LSUBMODEL, UNITAB)   
            SFBULK = ONE * FAC_GEN
         ENDIF
      ENDIF
      IF (IFMIN > 0) THEN
         IF (SFMIN == ZERO) THEN
            CALL HM_GET_FLOATV_DIM('Fscale_Mtin', FAC_GEN, IS_AVAILABLE, LSUBMODEL, UNITAB)   
            SFMIN = ONE * FAC_GEN
         ENDIF
      ENDIF
      IF (IFMOUTT > 0) THEN
         IF (SFMOUTT == ZERO) THEN
            CALL HM_GET_FLOATV_DIM('Fscale_Mtout', FAC_GEN, IS_AVAILABLE, LSUBMODEL, UNITAB)   
            SFMOUTT = ONE * FAC_GEN
         ENDIF
      ENDIF
      IF (IFMOUTP > 0) THEN
         IF (SFMOUTP == ZERO) THEN
            CALL HM_GET_FLOATV_DIM('Fscale_Mpout', FAC_GEN, IS_AVAILABLE, LSUBMODEL, UNITAB)   
            SFMOUTP = ONE * FAC_GEN
         ENDIF
      ENDIF
      IF (IFP0 > 0) THEN
         IF (SFP0 == ZERO) THEN
            CALL HM_GET_FLOATV_DIM('Fscale_Padd', FAC_GEN, IS_AVAILABLE, LSUBMODEL, UNITAB)   
            SFP0 = ONE * FAC_GEN
         ENDIF
      ENDIF
      CALL HM_GET_FLOATV_DIM('Fscale_Pmax', FAC_GEN, IS_AVAILABLE, LSUBMODEL, UNITAB)   
      IF (IFPMAX > 0) THEN
         IF (SFPMAX == ZERO) THEN
            SFPMAX = ONE * FAC_GEN
         ENDIF
      ELSE
         SFPMAX = INFINITY * FAC_GEN
      ENDIF
C     =====
C     Store
C     =====
C     Store in data structure
      T_MONVOLN%IVOLU(21) = IFUNCT_ID(1)
      T_MONVOLN%RVOLU(35) = SFBULK
      T_MONVOLN%IVOLU(22) = IFUNCT_ID(2)
      T_MONVOLN%RVOLU(36) = SFMIN
      T_MONVOLN%IVOLU(23) = IFUNCT_ID(3)
      T_MONVOLN%RVOLU(37) = SFMOUTT
      T_MONVOLN%IVOLU(24) = IFUNCT_ID(4)
      T_MONVOLN%RVOLU(38) = SFMOUTP
      T_MONVOLN%IVOLU(25) = IFUNCT_ID(5)
      T_MONVOLN%RVOLU(39) = SFP0
      T_MONVOLN%IVOLU(26) = IFUNCT_ID(6)
      T_MONVOLN%RVOLU(40) = SFPMAX

      T_MONVOLN%RVOLU(26) = ONE / SCAL_T
      T_MONVOLN%RVOLU(27) = ONE / SCAL_P
      T_MONVOLN%RVOLU(28) = ONE
      T_MONVOLN%RVOLU(29) = ONE
      T_MONVOLN%RVOLU(30) = ONE

      T_MONVOLN%RVOLU(34) = RHOI
C     
      VEPS = MAX(ZERO, VMIN - VOL)
      T_MONVOLN%RVOLU(4) = VOL + VEPS 
      T_MONVOLN%RVOLU(17) = VEPS 
      T_MONVOLN%RVOLU(20)= RHOI*VOL
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
      WRITE(IOUT, 1003) SCAL_T, SCAL_P
      WRITE(IOUT, 1002) SA, SV, VOL
      WRITE(IOUT,1800) RHOI, IFBULK, SFBULK, IFMIN, SFMIN, IFMOUTT, SFMOUTT, 
     .     IFMOUTP, SFMOUTP, IFP0, SFP0, IFPMAX, SFPMAX
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
     .     /5X,'UNIT SCALE FOR PRESSURE FUNCTIONS      =',1PG20.13)
 1005 FORMAT( 5X,'EXTERNAL SURFACE ID . . . . . . . . . .=',I10)

 1800 FORMAT(
     .     5X,'FLUID DENSITY. . . . . . . . . . . . . . . . . . . . . =',1PG20.13,
     .     /5X,'BULK TIME FUNCTION. . . . . . . . . . . . . . . . . . .=',I10,
     .     /5X,'BULK TIME FUNCTION SCALE FACTOR. . . . . . . . . . . . =',1PG20.13,
     .     /5X,'INPUT MASS FLOW RATE TIME FUNCTION. . . . . . . . . . .=',I10,
     .     /5X,'INPUT MASS FLOW RATE TIME FUNCTION SCALE FACTOR. . . . =',1PG20.13,
     .     /5X,'OUTPUT MASS FLOW RATE TIME FUNCTION. . . . . . . . . . =',I10,
     .     /5X,'OUTPUT MASS FLOW RATE TIME FUNCTION SCALE FACTOR. . . .=',1PG20.13,
     .     /5X,'OUTPUT MASS FLOW RATE PRESSURE FUNCTION. . . . . . . . =',I10,
     .     /5X,'OUTPUT MASS FLOW RATE PRESSURE FUNCTION SCALE FACTOR. .=',1PG20.13,
     .     /5X,'ADDITIONAL PRESSURE TIME FUNCTION. . . . . . . . . . . =',I10,
     .     /5X,'ADDITIONAL PRESSURE TIME FUNCTION SCALE FACTOR. . . . .=',1PG20.13,
     .     /5X,'MAXIMUM PRESSURE TIME FUNCTION. . . . . . . . . . . . .=',I10,
     .     /5X,'MAXIMUM PRESSURE TIME FUNCTION SCALE FACTOR. . . . . . =',1PG20.13)
      END SUBROUTINE HM_READ_MONVOL_TYPE10
      END MODULE HM_READ_MONVOL_TYPE10_MOD