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
Chd|  LEC_INISTATE                  source/elements/initia/lec_inistate.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        HM_READ_INISTATE_D00          source/elements/initia/hm_read_inistate_d00.F
Chd|        LEC_INISTATE_TRI              source/elements/initia/lec_inistate_tri.F
Chd|        LEC_INISTATE_YFILE            source/initial_conditions/inista/lec_inistate_yfile.F
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        MAPPING_OPTION_MOD            share/modules1/dichotomy_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE LEC_INISTATE(      IXS          ,IXQ          ,IXC          ,IXT     ,
     1                  IXP        ,IXR          ,GEO          ,PM           ,KXSP    ,
     2                  IXTG       ,INDEX        ,ITRI         ,
     3                  NSIGSH     ,IGEO         ,IPM          ,NSIGS        ,NSIGSPH ,
     4                  KSYSUSR    ,PTSHEL       ,PTSH3N       ,PTSOL        ,PTQUAD  ,
     5                  PTSPH      ,NUMEL        ,NSIGRS       ,UNITAB       ,ISOLNODD00,
     6                  LSUBMODEL  ,RTRANS       ,IDRAPE       ,NSIGI        ,
     7                  PTSPRI     ,NSIGBEAM     ,PTBEAM       ,NSIGTRUSS    ,PTTRUSS  ,
     8                  SIGI       ,SIGSH        ,SIGSP        ,SIGSPH       ,SIGRS    ,
     9                  SIGBEAM    ,SIGTRUSS     ,STRSGLOB     ,STRAGLOB     ,ORTHOGLOB,
     A                  ISIGSH     ,IYLDINI      ,KSIGSH3      ,FAIL_INI     ,IUSOLYLD ,
     B                  IUSER      ,IGRBRIC      ,MAP_TABLES   ,IPARG        )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------                     
      USE UNITAB_MOD
      USE GROUPDEF_MOD
      USE SUBMODEL_MOD
      USE MAPPING_OPTION_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "scry_c.inc"
#include      "scr16_c.inc"
#include      "scr17_c.inc"
#include      "sysunit.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB 
      INTEGER IXS(NIXS,*), IXQ(NIXQ,*),IXC(NIXC,*),
     .   IGEO(NPROPGI,*), IXT(NIXT,*),IXP(NIXP,*), IXR(NIXR,*),
     .   IXTG(NIXTG,*),INDEX(*),ITRI(*),KXSP(*),IPM(NPROPMI,*),
     .   KSYSUSR(*),PTSHEL(*),PTSH3N(*),PTSOL(*),PTQUAD(*),PTSPH(*),
     .   IDRAPE(NPLYMAX,*),PTSPRI(*),PTBEAM(*),PTTRUSS(*)
      INTEGER NSIGI,NSIGSH,NSIGS, NSIGSPH, NSIGRS,
     .        NUMEL,ISOLNODD00(*),NSIGBEAM,NSIGTRUSS,STRSGLOB(*),
     .        STRAGLOB(*),ORTHOGLOB(*),ISIGSH,IYLDINI,KSIGSH3,FAIL_INI(5),
     .        IUSOLYLD,IUSER,VARMAX
      INTEGER, DIMENSION(NPARG,NGROUP) ,INTENT(IN):: IPARG
      my_real
     .   GEO(*),PM(NPROPM,*),RTRANS(NTRANSF,*),
     .   SIGI(NSIGS,*),SIGSH(MAX(1,NSIGSH),*),SIGTRUSS(NSIGTRUSS,*),
     .   SIGSP(NSIGI,*),SIGSPH(NSIGSPH,*),SIGRS(NSIGRS,*),SIGBEAM(NSIGBEAM,*)
C
      TYPE(SUBMODEL_DATA) LSUBMODEL(*)
      TYPE (GROUP_)  , DIMENSION(NGRBRIC) :: IGRBRIC
C
      TYPE(MAPPING_STRUCT_) :: MAP_TABLES
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  I, J, N, stat
      INTEGER NIBRICK, NIQUAD, NISHELL, NISH3N, NISPRING, NIBEAM, NITRUSS
      INTEGER, DIMENSION(:), ALLOCATABLE :: ID_SIGSH
      INTEGER, DIMENSION(:), ALLOCATABLE :: ID_SOLID_SIGI
      INTEGER, DIMENSION(:), ALLOCATABLE :: ID_QUAD_SIGI
      INTEGER, DIMENSION(:), ALLOCATABLE :: ID_SIGSPRI
      INTEGER, DIMENSION(:), ALLOCATABLE :: ID_SIGBEAM
      INTEGER, DIMENSION(:), ALLOCATABLE :: ID_SIGTRUSS
      INTEGER, DIMENSION(:), ALLOCATABLE :: WORK
C=======================================================================
C
C -- LECTURE OF INITIAL STATE DATA - EXTRACTED FROM INITIA.F
C
C=======================================================================
      IUSER = 0
      IYLDINI = 0
      IUSOLYLD = 0 
      FAIL_INI(1:5) = 0
      ISIGSH =0
      KSIGSH3 = 0
      IORTSHEL = 0
      INISPRI = 0
      VARMAX = MAX(NSIGSH,NSIGI,NSIGI,NSIGTRUSS,NSIGBEAM,NSIGRS)
C
      ALLOCATE (ID_SIGSH(NUMSHEL+NUMSH3N)     ,STAT=stat)
      ALLOCATE (ID_SOLID_SIGI(NUMSOL)         ,STAT=stat)
      ALLOCATE (ID_QUAD_SIGI(NUMQUAD)         ,STAT=stat)
      ALLOCATE (ID_SIGSPRI(NUMSPRI)           ,STAT=stat)
      ALLOCATE (ID_SIGBEAM(NUMBEAM)           ,STAT=stat)
      ALLOCATE (ID_SIGTRUSS(NUMTRUS)          ,STAT=stat)
      ALLOCATE (WORK(70000)                   ,STAT=stat)
C
      ID_SIGSH(1:NUMSHEL+NUMSH3N) = 0
      ID_SOLID_SIGI(1:NUMSOL) = 0
      ID_QUAD_SIGI(1:NUMQUAD) = 0
      ID_SIGSPRI(1:NUMSPRI)  = 0
      ID_SIGBEAM(1:NUMBEAM)  = 0
      ID_SIGTRUSS(1:NUMTRUS) = 0
      WORK(1:70000) = 0
C
      IF (ABS(ISIGI) == 3.OR.ABS(ISIGI) == 4.OR.ABS(ISIGI) == 5) THEN
         DO I=1,NUMSHEL+NUMSH3N
            DO J=1,NSIGSH
              SIGSH(J,I)=ZERO
            ENDDO
         ENDDO
      ENDIF

C--------------------------------------------------------
C     CONTRAINTES INITIALES + ENERGIES DENSITES EPS-PLAST
C     SOLIDE-QUAD-SPRING      READ ON FILE
C--------------------------------------------------------

      IF (ISIGI == 1.OR.ISIGI == 2) THEN
C
C      FICHIER S00 (Obsolete)
C
       IF (IOUTP_FMT == 2) THEN
         DO I=1,NUMELS+NUMELQ
           READ(IIN4,'(I8,3F16.0/8X,3F16.0)') N,(SIGI(J,I),J=1,6)
           SIGI(7,I) = N
         ENDDO
       ELSE
         DO I=1,NUMELS+NUMELQ
           READ(IIN4,'(I10,3F20.0/8X,3F20.0)') N,(SIGI(J,I),J=1,6)
           SIGI(7,I) = N
         ENDDO
       ENDIF

      ELSEIF (ISIGI == 3.OR.ISIGI == 4.OR.ISIGI == 5) THEN
C
C      FICHIER Y000
C
         CALL LEC_INISTATE_YFILE( 
     1                  NSIGSH    ,NSIGS     ,NSIGSPH   ,NSIGRS    ,NSIGI     ,                 
     2                  SIGSH     ,SIGI      ,SIGSPH    ,SIGRS     ,SIGSP     ,                  
     3                  ISIGSH    ,IUSER     ,
     4                  ID_SIGSH  , ID_SOLID_SIGI, ID_QUAD_SIGI  )

      ENDIF

C-----------------------------------------
C     CONTRAINTES INITIALES FICHIER D00
C-----------------------------------------
      IF (ISIGI == -3.OR.ISIGI == -4.OR.ISIGI == -5) THEN
         ISIGI = -ISIGI

!         CALL LEC_INISTATE_D00 (
!     1                  IXS        ,IXQ          ,IXC          ,IXT          ,IXP       ,
!     2                  IXR        ,GEO          ,PM           ,IXTG         ,INDEX     ,
!     3                  ITRI       ,NSIGSH       ,IGEO      ,
!     4                  IPM        ,NSIGS        ,NSIGSPH      ,KSYSUSR      ,NSIGRS    ,
!     5                  UNITAB     ,ISOLNODD00   ,LSUBMODEL    ,RTRANS       ,IDRAPE    ,
!     6                  NSIGI      ,NSIGBEAM     ,NSIGTRUSS    ,
!     7                  SIGI       ,SIGSH        ,SIGSP        ,SIGSPH       ,SIGRS     ,
!     8                  SIGBEAM    ,SIGTRUSS     ,STRSGLOB     ,STRAGLOB     ,ORTHOGLOB ,
!     9                  ISIGSH     ,IYLDINI      ,FAIL_INI     ,IUSOLYLD     ,IUSER     , 
!     A                  ID_SIGSH   ,ID_SOLID_SIGI,ID_QUAD_SIGI ,ID_SIGSPRI   ,ID_SIGBEAM,
!     B                  ID_SIGTRUSS,WORK         ,IGRBRIC )

         CALL HM_READ_INISTATE_D00 (
     1                  IXS        ,IXQ          ,IXC          ,IXT          ,IXP       ,
     2                  IXR        ,GEO          ,PM           ,IXTG         ,INDEX     ,
     3                  ITRI       ,NSIGSH       ,IGEO      ,
     4                  IPM        ,NSIGS        ,NSIGSPH      ,KSYSUSR      ,NSIGRS    ,
     5                  UNITAB     ,ISOLNODD00   ,LSUBMODEL    ,RTRANS       ,IDRAPE    ,
     6                  NSIGI      ,NSIGBEAM     ,NSIGTRUSS    ,
     7                  SIGI       ,SIGSH        ,SIGSP        ,SIGSPH       ,SIGRS     ,
     8                  SIGBEAM    ,SIGTRUSS     ,STRSGLOB     ,STRAGLOB     ,ORTHOGLOB ,
     9                  ISIGSH     ,IYLDINI      ,FAIL_INI     ,IUSOLYLD     ,IUSER     , 
     A                  ID_SIGSH   ,ID_SOLID_SIGI,ID_QUAD_SIGI ,ID_SIGSPRI   ,ID_SIGBEAM,
     B                  ID_SIGTRUSS,WORK         ,IGRBRIC      ,NIBRICK      ,NIQUAD    ,
     C                  NISHELL    ,NISH3N       ,NISPRING     ,NIBEAM       ,NITRUSS   ,
     D                  MAP_TABLES ,VARMAX       ,IPARG        )

      ENDIF
C------------------------------------------------------------------------------------------

         CALL LEC_INISTATE_TRI(
     1                  IXS        ,IXQ          ,IXC          ,IXT          ,IXP       ,
     2                  IXR        ,KXSP         ,IXTG         ,INDEX        ,ITRI      ,
     3                  NSIGSH     ,NSIGS        ,NSIGSPH      ,KSYSUSR      ,KSIGSH3   ,
     4                  NSIGRS     ,NSIGI        ,NSIGBEAM     ,NSIGTRUSS    ,
     5                  PTSHEL     ,PTSH3N       ,PTSOL        ,PTQUAD       ,PTSPH     ,
     6                  PTSPRI     ,PTBEAM       ,PTTRUSS      ,SIGI         ,SIGSH     ,
     7                  SIGSP      ,SIGSPH       ,SIGRS        ,SIGBEAM      ,SIGTRUSS  ,
     8                  ID_SIGSH   ,ID_SOLID_SIGI,ID_QUAD_SIGI ,ID_SIGSPRI ,ID_SIGBEAM  ,
     9                  ID_SIGTRUSS,WORK )

C
      DEALLOCATE (ID_SOLID_SIGI)
      DEALLOCATE (ID_QUAD_SIGI)
      DEALLOCATE (ID_SIGSH)
      DEALLOCATE (ID_SIGSPRI)
      DEALLOCATE (ID_SIGBEAM)
      DEALLOCATE (ID_SIGTRUSS)
      DEALLOCATE (WORK)
C
      RETURN
      END
