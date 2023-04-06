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
Chd|  READ_RWALL                    source/constraints/general/rwall/read_rwall.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        HM_OPTION_COUNT               source/devtools/hm_reader/hm_option_count.F
Chd|        HM_READ_RWALL_CYL             source/constraints/general/rwall/hm_read_rwall_cyl.F
Chd|        HM_READ_RWALL_LAGMUL          source/constraints/general/rwall/hm_read_rwall_lagmul.F
Chd|        HM_READ_RWALL_PARAL           source/constraints/general/rwall/hm_read_rwall_paral.F
Chd|        HM_READ_RWALL_PLANE           source/constraints/general/rwall/hm_read_rwall_plane.F
Chd|        HM_READ_RWALL_SPHER           source/constraints/general/rwall/hm_read_rwall_spher.F
Chd|        HM_READ_RWALL_THERM           source/constraints/general/rwall/hm_read_rwall_therm.F
Chd|        UDOUBLE                       source/system/sysfus.F        
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        R2R_MOD                       share/modules1/r2r_mod.F      
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE READ_RWALL(RWL     ,NPRW    ,LPRW    ,IFI     ,MS      ,
     .                  V       ,ITAB    ,ITABM1  ,X       ,IXS     ,
     .                  IXQ     ,NPC     ,IKINE   ,IGRNOD  ,
     .                  MFI     ,IMERGE  ,UNITAB  ,
     .                  IKINE1LAG,IDDLEVEL,LSUBMODEL,RTRANS,NOM_OPT ,
     .                  ITAGND ) 
C-------------------------------------
C     LECTURE MUR RIGIDE
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE UNITAB_MOD
      USE SUBMODEL_MOD
      USE MESSAGE_MOD
      USE R2R_MOD
      USE GROUPDEF_MOD
      USE HM_OPTION_READ_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "units_c.inc"
#include      "scr17_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB 
      INTEGER IFI,MFI,IDDLEVEL 
      INTEGER NPRW(*), LPRW(*), ITAB(*), ITABM1(*),IXS(NIXS,*),
     .        IXQ(NIXQ,*), NPC(*), IKINE(*),
     .        IMERGE(*),
     .        IKINE1LAG(*),ITAGND(*)
      TYPE(SUBMODEL_DATA) LSUBMODEL(*)
C     REAL
      my_real
     .   RWL(NRWLP,*), MS(*), V(3,*), X(3,*),
     .   RTRANS(NTRANSF,*)
      INTEGER NOM_OPT(LNOPT1,*)
C-----------------------------------------------
      TYPE (GROUP_)  ,TARGET, DIMENSION(NGRNOD)  :: IGRNOD
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER K,I,NCHPLAN,NCHCYL,NCHSPHER,NCHPARAL,
     .   IKINE1(3*NUMNOD),OFFS,NCHLAGM,NCHTHERM
      my_real
     .   BID
      CHARACTER MESS*40
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      DATA MESS/'STANDARD RIGID WALL DEFINITION          '/
C=======================================================================
C-----------------------------------------------
C     B e g i n n i n g   o f   s o u r c e
C-----------------------------------------------
C     Beginning of RWALL cards in the IIN file
      WRITE(IOUT,1000)
C
C     Initialization of variable
      K    = 0
      ! Offset
      OFFS = 0
      ! Flag for therm rigid wall
      DO I=1,3*NUMNOD
        IKINE1(I) = 0
      ENDDO
C
C-----------------------------------------------      
!     **************************    !
!     RWALL read with hm reader     !
!     **************************    !
C-----------------------------------------------
C     Counting RWALL type
      CALL HM_OPTION_COUNT('/RWALL/PLANE' ,NCHPLAN  )
      CALL HM_OPTION_COUNT('/RWALL/CYL'   ,NCHCYL   )
      CALL HM_OPTION_COUNT('/RWALL/SPHER' ,NCHSPHER )
      CALL HM_OPTION_COUNT('/RWALL/PARAL' ,NCHPARAL )
      CALL HM_OPTION_COUNT('/RWALL/LAGMUL',NCHLAGM  )
      CALL HM_OPTION_COUNT('/RWALL/THERM' ,NCHTHERM )
C     
C     /RWALL/PLANE
      IF (NCHPLAN > 0) THEN 
        CALL HM_READ_RWALL_PLANE(RWL     ,NPRW    ,LPRW    ,IFI     ,MS      ,
     .                           V       ,ITAB    ,ITABM1  ,X       ,IKINE   ,
     .                           IGRNOD  ,MFI     ,IMERGE  ,UNITAB  ,IDDLEVEL,
     .                          LSUBMODEL,RTRANS  ,NOM_OPT ,ITAGND  ,NCHPLAN ,
     .                           K       ,OFFS    ,IKINE1  )
      ENDIF
C
C     /RWALL/CYL
      IF (NCHCYL > 0) THEN
        CALL HM_READ_RWALL_CYL(RWL     ,NPRW    ,LPRW    ,IFI     ,MS      ,
     .                         V       ,ITAB    ,ITABM1  ,X       ,IKINE   ,
     .                         IGRNOD  ,MFI     ,IMERGE  ,UNITAB  ,IDDLEVEL,
     .                        LSUBMODEL,RTRANS  ,NOM_OPT ,ITAGND  ,NCHCYL  ,
     .                         K       ,OFFS    ,IKINE1  )  
      ENDIF
C
C     /RWALL/SPHER
      IF (NCHSPHER > 0) THEN
        CALL HM_READ_RWALL_SPHER(RWL     ,NPRW    ,LPRW    ,IFI     ,MS      ,
     .                           V       ,ITAB    ,ITABM1  ,X       ,IKINE   ,
     .                           IGRNOD  ,MFI     ,IMERGE  ,UNITAB  ,IDDLEVEL,
     .                          LSUBMODEL,RTRANS  ,NOM_OPT ,ITAGND  ,NCHSPHER,
     .                           K       ,OFFS    ,IKINE1  ) 
      ENDIF
C
C     /RWALL/PARAL
      IF (NCHPARAL > 0) THEN
        CALL HM_READ_RWALL_PARAL(RWL     ,NPRW    ,LPRW    ,IFI     ,MS      ,
     .                           V       ,ITAB    ,ITABM1  ,X       ,IKINE   ,
     .                           IGRNOD  ,MFI     ,IMERGE  ,UNITAB  ,IDDLEVEL,
     .                          LSUBMODEL,RTRANS  ,NOM_OPT ,ITAGND  ,NCHPARAL,
     .                           K       ,OFFS    ,IKINE1  ) 
      ENDIF
C
C     /RWALL/LAGMUL
      IF (NCHLAGM > 0) THEN 
        CALL HM_READ_RWALL_LAGMUL(RWL     ,NPRW    ,LPRW    ,IFI     ,MS      ,
     .                            V       ,ITAB    ,ITABM1  ,X       ,IKINE   ,
     .                            IGRNOD  ,MFI     ,IMERGE  ,UNITAB  ,IDDLEVEL,
     .                           LSUBMODEL,RTRANS  ,NOM_OPT ,ITAGND  ,NCHLAGM ,
     .                            K       ,OFFS    ,IKINE1LAG)
      ENDIF
C
C     /RWALL/THERM
      IF (NCHTHERM > 0) THEN 
        CALL HM_READ_RWALL_THERM(RWL      ,NPRW    ,LPRW    ,IFI     ,MS      ,
     .                            V       ,ITAB    ,ITABM1  ,X       ,IKINE   ,
     .                            IGRNOD  ,MFI     ,IMERGE  ,UNITAB  ,IDDLEVEL,
     .                           LSUBMODEL,RTRANS  ,NOM_OPT ,ITAGND  ,NCHTHERM,
     .                            K       ,OFFS    ,IKINE1  ,IXS     ,IXQ     ,
     .                            NPC     )
      ENDIF
C
C-------------------------------------
C Recherche des ID doubles
C-------------------------------------
      CALL UDOUBLE(NOM_OPT,LNOPT1,NRWALL,MESS,0,BID)
      RETURN
 1000 FORMAT(
     . '      RIGID WALL DEFINITIONS '/
     . '      ---------------------- '/)
      END
