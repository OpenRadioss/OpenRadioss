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
Chd|  ALEWDX                        source/ale/grid/alewdx.F      
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        ALELIN                        source/ale/grid/alelin.F      
Chd|        ALEW                          source/ale/grid/alew.F        
Chd|        ALEW1                         source/ale/grid/alew1.F       
Chd|        ALEW2                         source/ale/grid/alew2.F       
Chd|        ALEW4                         source/ale/grid/alew4.F       
Chd|        ALEW5                         source/ale/grid/alew5.F       
Chd|        ALEW6                         source/ale/grid/alew6.F       
Chd|        ALEW7                         source/ale/grid/alew7.F       
Chd|        BCS2V                         source/constraints/general/bcs/bcs2.F
Chd|        BCS3V                         source/ale/inter/bcs3v.F      
Chd|        INTAL3                        source/ale/inter/intal3.F     
Chd|        LASER1                        source/loads/laser/laser1.F   
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|        RGWAL1                        source/ale/grid/rgwal1.F      
Chd|        STARTIME                      source/system/timer.F         
Chd|        STOPTIME                      source/system/timer.F         
Chd|        WLAG                          source/ale/grid/wlag.F        
Chd|        WPOR                          source/ale/grid/wpor.F        
Chd|        ALE_CONNECTIVITY_MOD          ../common_source/modules/ale/ale_connectivity_mod.F
Chd|        ALE_MOD                       ../common_source/modules/ale/ale_mod.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/mat_elem/elbufdef_mod.F
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|        MULTI_FVM_MOD                 ../common_source/modules/ale/multi_fvm_mod.F
Chd|====================================================================
      SUBROUTINE ALEWDX(
     1     GEO             ,X             ,D          ,V          ,VR         ,
     2     W               ,WA            ,WB         ,RBY        ,SKEW       ,
     3     PM              ,XLAS          ,MS         ,FSAV       ,
     4     A               ,TF            ,RWBUF      ,
     5     DT2SAVE         ,
     6     IPARG           ,IXS           ,IXQ        ,NODPOR     ,
     7     ISKEW           ,ICODT         ,ELBUF_TAB  ,
     8     NPF             ,LINALE        ,NPRW       ,LAS        ,
     9     IPARI           ,NODFT         ,NODLT      ,ITASK      ,
     A     IAD_ELEM        ,FR_ELEM       ,NBRCVOIS   ,NBSDVOIS   ,LNRCVOIS   ,
     B     LNSDVOIS        ,WEIGHT        ,ADSKY      ,FSKY       ,IADS       ,
     C     FR_WALL         ,NPORGEO       ,PROCNE     ,
     D     FR_NBCC         ,IADQ          ,XDP        ,IGRNOD     ,
     E     DR              ,INTBUF_TAB    ,ITAB       ,MULTI_FVM  ,
     F     ALE_CONNECTIVITY,DDP           ,NE_NERCVOIS,NE_NESDVOIS,
     G     NE_LERCVOIS     ,NE_LESDVOIS   ,XCELL      ,XFACE      )
C-----------------------------------------------
C     D e s c r i p t i o n
C-----------------------------------------------
! --- NWALE : INTEGER SET IN [0,7] depending on /ALE/GRID/Keyword option
!
!   NWALE   FORMULATION (/ALE/GRID/...)
!   0       'DONEA'
!   1       'DISP'
!   2       'SPRING'
!   3       'ZERO'
!   4       'STANDARD'
!   5       'LAPLACIAN'
!   6       'VOLUME'
!   7       'MASSFLOW'
!
! This subroutine is switching to grid formulation depending on NWALE value.
C
C     X,D,V are allocated to SX,SD,DV=3*(NUMNOD_L+NUMVVOIS_L)
C      in grid subroutine it may needed to access nodes which
C      are connected to a remote elem. They are sored in X(1:3,NUMNOD+1:)
C      Consequently X is defined here X(3,SX/3) instead of X(3,NUMNOD) as usually
C-----------------------------------------------
C     M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD
      USE INTBUFDEF_MOD
      USE GROUPDEF_MOD
      USE ALE_CONNECTIVITY_MOD
      USE MULTI_FVM_MOD
      USE ALE_MOD
C-----------------------------------------------
C     I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C     C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com06_c.inc"
#include      "com08_c.inc"
#include      "param_c.inc"
#include      "scr03_c.inc"
#include      "scr05_c.inc"
#include      "timeri_c.inc"
#include      "parit_c.inc"
#include      "tabsiz_c.inc"
C-----------------------------------------------
C     D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE(MULTI_FVM_STRUCT) :: MULTI_FVM
      INTEGER ISKEW(*), IXS(NIXS,NUMELS),IXQ(NIXQ,NUMELQ),
     .     NPF(*),LAS(*), IPARG(NPARG,NGROUP), IPARI(NPARI,NINTER),
     .     NPRW(*), ICODT(*), LINALE(*),
     .     NODPOR(*), NBRCVOIS(*),NBSDVOIS(*), PROCNE(*),FR_NBCC(*),
     .     LNRCVOIS(*),LNSDVOIS(*), NODFT ,NODLT, ITASK,
     .     WEIGHT(*), FR_ELEM(*), IAD_ELEM(*), ADSKY(*), NPORGEO(*),
     .     IADS(*) ,FR_WALL(*),
     .     IADQ(*), NIADX
      INTEGER, INTENT(IN) :: ITAB(NUMNOD)
      DOUBLE PRECISION    :: XDP(3,*),DDP(3,*)
      my_real X(3,SX/3) ,D(3,SD/3), V(3,SV/3) ,VR(3,SVR/3) ,A(3,SA/3) , FSKY(*),
     .        MS(*)   ,PM(NPROPM,NUMMAT),SKEW(LSKEW,*),GEO(NPROPG,NUMGEO),
     .        W(3,SW/3), WB(*), TF(*), FSAV(NTHVKI,*) ,XLAS(*),
     .        RWBUF(NRWLP,*),RBY(NRBY,*),WA(3,*),DR(3,SDR/3),XCELL(3,SXCELL), XFACE(3,6,*)
      my_real DT2SAVE
      TYPE(ELBUF_STRUCT_), DIMENSION(NGROUP) :: ELBUF_TAB
      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
      TYPE (GROUP_)  , DIMENSION(NGRNOD)  :: IGRNOD
      TYPE(t_ale_connectivity), INTENT(IN) :: ALE_CONNECTIVITY
      INTEGER, INTENT(IN) :: NE_NERCVOIS(*), NE_NESDVOIS(*), NE_LERCVOIS(*), NE_LESDVOIS(*)
C-----------------------------------------------
C     L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  I, N, ISK, LCOD, NINDX1, NINDX2,INDX1(1024), INDX2(1024), SIZEN
      DOUBLE PRECISION VDT2
      my_real VDT
C-----------------------------------------------
C     S o u r c e   L i n e s
C-----------------------------------------------
      IF(IMON > 0 .AND. ITASK == 0) THEN
         CALL STARTIME(5,1)
      ENDIF
C----------------------------------------
C     MESH VELOCITIES DEPENDING ON USER FORMULATION
C----------------------------------------
      IF(ALE%SUB%IFSUB == 0) THEN

        IF(ITASK == 0)THEN
          DT2SAVE=DT2
          IF(ALE%SUB%IALESUB /= 0)DT2=INT(DT2S/DT2SAVE)*DT2SAVE !DT2=DT2S
          SIZEN=0
          IF(IPARIT == 0)SIZEN = NUMNOD
        ENDIF

        SELECT CASE (ALE%GRID%NWALE)

          !-------------------------!
          !---/ALE/GRID/DONEA (0)
          !-------------------------!
          CASE(0)
            CALL MY_BARRIER
            CALL ALEW(
     1           X         ,D      ,V       ,W       ,WA      ,
     2           ALE_CONNECTIVITY%NN_CONNECT     ,ALE_CONNECTIVITY%NALE   ,NODFT   ,NODLT   ,ITASK   ,
     3           NBRCVOIS,NBSDVOIS,LNRCVOIS ,LNSDVOIS,ITAB         )
            CALL MY_BARRIER

          !-------------------------!
          !---/ALE/GRID/DISP (1)
          !-------------------------!
          CASE(1)
            CALL ALEW1(
     1           X         ,D       ,V       ,W       ,
     2           ALE_CONNECTIVITY%NN_CONNECT     ,ALE_CONNECTIVITY%NALE    ,NODFT   ,NODLT   ,ITASK   ,
     3           NBRCVOIS  ,NBSDVOIS,LNRCVOIS,LNSDVOIS,ITAB         )
            CALL MY_BARRIER

          !-------------------------!
          !---/ALE/GRID/SPRING (2)
          !-------------------------!
          CASE(2)
            IF (ITASK == 0) THEN
              CALL ALEW2(
     1            X       ,D      ,V       ,W    ,WA   ,
     2            ALE_CONNECTIVITY%NALE    ,IPARG  ,IXS     ,WB   ,
     3            IAD_ELEM,FR_ELEM,FR_NBCC ,SIZEN,ADSKY,
     4            PROCNE  ,FSKY   ,FSKY    ,IADS ,ITAB      )
            ENDIF

          !-------------------------!
          !---/ALE/GRID/ZERO (3)
          !-------------------------!
          CASE(3)
            IF (ITASK == 0) THEN
              CALL WLAG(V,W,ALE_CONNECTIVITY%NALE)
            ENDIF

          !-------------------------!
          !---/ALE/GRID/STANDARD (4)
          !-------------------------!
          CASE(4)
            IF (ITASK == 0) THEN
              IF (N2D == 0) THEN
                NIADX=8
                CALL ALEW4(
     1               X       ,D      ,V       ,W    ,WA            ,
     2               ALE_CONNECTIVITY%NALE    ,IPARG  ,IXS     ,WB   ,
     3               IAD_ELEM,FR_ELEM,FR_NBCC ,SIZEN,ADSKY         ,
     4               PROCNE  ,FSKY   ,FSKY    ,IADS ,WB(1+3*NUMNOD),
     5               NIXS    ,NIADX  ,ITAB )
              ELSE
                NIADX=4
                CALL ALEW4(
     1               X       ,D      ,V       ,W    ,WA            ,
     2               ALE_CONNECTIVITY%NALE    ,IPARG  ,IXQ     ,WB   ,
     3               IAD_ELEM,FR_ELEM,FR_NBCC ,SIZEN,ADSKY         ,
     4               PROCNE  ,FSKY   ,FSKY    ,IADQ ,WB(1+3*NUMNOD),
     5               NIXQ    ,NIADX  ,ITAB  )
              ENDIF
            ENDIF

          !-------------------------!
          !---/ALE/GRID/LAPLACIAN (5)
          !-------------------------!
          CASE(5)
            CALL MY_BARRIER
            CALL ALEW5(
     1           X         ,D       ,V       ,W       ,WA       ,
     2           ALE_CONNECTIVITY%NN_CONNECT     ,ALE_CONNECTIVITY%NALE    ,NODFT   ,NODLT   ,ITASK   ,
     3           NBRCVOIS  ,NBSDVOIS,LNRCVOIS,LNSDVOIS,ITAB       ,
     4           SKEW, ISKEW,ICODT)
            CALL MY_BARRIER

          !-------------------------!
          !---/ALE/GRID/VOLUME (6)
          !-------------------------!
          CASE(6)
            CALL MY_BARRIER
            CALL ALEW6(
     1           X         ,D       ,V       ,W       ,WA       ,
     .           XCELL, XFACE,
     2           ALE_CONNECTIVITY%NN_CONNECT     ,ALE_CONNECTIVITY%NE_CONNECT,ALE_CONNECTIVITY%NALE    ,NODFT   ,NODLT   ,ITASK   ,
     3           NBRCVOIS  ,NBSDVOIS,LNRCVOIS,LNSDVOIS,ITAB,NE_NERCVOIS, NE_NESDVOIS, NE_LERCVOIS, NE_LESDVOIS,
     4           ELBUF_TAB, IPARG, IXS, IXQ)
            CALL MY_BARRIER

          !-------------------------!
          !---/ALE/GRID/MASSFLOW (7)
          !-------------------------!
          CASE(7)
            CALL MY_BARRIER
            CALL ALEW7(
     1           X       ,V       ,W      ,MS      ,ALE_CONNECTIVITY%NALE,
     2           NODFT   ,NODLT   ,WEIGHT ,NUMNOD  ,DT1                  ,
     3           SX      ,SV      ,SW     ,NSPMD   )
            CALL MY_BARRIER


        END SELECT !CASE (ALE%GRID%NWALE)

        IF (ITASK == 0) THEN
          DT2=DT2SAVE
        ENDIF
      ELSE
         IF (ITASK == 0) CALL WLAG(V,W,ALE_CONNECTIVITY%NALE)
      ENDIF
C
      CALL MY_BARRIER
C
      IF(IMON > 0 .AND. ITASK == 0) CALL STOPTIME(5,1)
      IF (ITASK == 0) THEN
C----------------------------------------
         IF(NUMPOR > 0)
     1        CALL WPOR(GEO,NODPOR,X                    ,V       ,VR  ,
     2                  W  ,RBY   ,ALE_CONNECTIVITY%NALE,NPORGEO       )
C----------------------------------------
C     BOUNDARY CONDITIONS AT NODES
C----------------------------------------
         IF(IMON > 0 .AND. ITASK == 0) CALL STARTIME(4,1)
         DO I=1,NUMNOD,1024
            NINDX1 = 0
            NINDX2 = 0
            DO  N = I,MIN(NUMNOD,I+1023)
               IF(ALE_CONNECTIVITY%NALE(N) /= 0)THEN
                  ISK=ISKEW(N)
                  LCOD=ICODT(N+NUMNOD)
                  IF(LCOD /= 0) THEN
                     NINDX1 = NINDX1 + 1
                     INDX1(NINDX1) = N
                  ENDIF
                  LCOD=ICODT(N+NUMNOD+NUMNOD)
                  IF(LCOD /= 0) THEN
                     NINDX2 = NINDX2 + 1
                     INDX2(NINDX2) = N
                  ENDIF
               ENDIF
            ENDDO
            IF(NINDX1 /= 0)THEN
              CALL BCS2V(NINDX1,INDX1,ISKEW,ICODT(NUMNOD+1),W,SKEW)
            ENDIF
            IF(NINDX2 /= 0)THEN
              CALL BCS3V(NINDX2,INDX2,ISKEW,ICODT(2*NUMNOD+1),W,V,SKEW)
            ENDIF
         ENDDO
         IF(IMON > 0 .AND. ITASK == 0) CALL STOPTIME(4,1)
C--------------------------------
C     ALE/LAGRANGE INTERFACES
C--------------------------------
         IF(NINTER /= 0) THEN
            IF(IMON > 0 .AND. ITASK == 0) CALL STARTIME(2,1)
            CALL INTAL3(
     1           IPARI,X     ,V      ,
     2           A    ,W    ,ISKEW ,SKEW  ,ICODT(1+NUMNOD),
     3           IXS  ,IXQ  ,ELBUF_TAB ,IPARG ,
     4           PM   ,ALE_CONNECTIVITY%NALE ,INTBUF_TAB)
            IF(IMON > 0 .AND. ITASK == 0) CALL STOPTIME(2,1)
         ENDIF
C-----------------------------------
C     2D GRID VELOCITIES
C-----------------------------------
         IF(N2D == 1)THEN
            IF(IMON > 0 .AND. ITASK == 0) CALL STARTIME(5,1)
            DO I=1,NUMNOD
               IF(X(2,I)+DT2*W(2,I) >= ZERO)CYCLE
               W(2,I)=ZERO
               V(2,I)=ZERO
            ENDDO
            IF(IMON > 0 .AND. ITASK == 0) CALL STOPTIME(5,1)
         ENDIF
C--------------------------------------
C     /ALE/LINK/VEL
C--------------------------------------
         IF(NALELK /= 0) THEN
            IF(IMON > 0 .AND. ITASK == 0) CALL STARTIME(4,1)
            CALL ALELIN(NALELK,LINALE,W,WEIGHT,IGRNOD,ITAB)
            IF(IMON > 0 .AND. ITASK == 0) CALL STOPTIME(4,1)
         ENDIF
C-----------------------------------
C     2D GRID VELOCITY
C-----------------------------------
         IF(N2D == 1)THEN
            IF(IMON > 0 .AND. ITASK == 0) CALL STARTIME(5,1)
            DO I=1,NUMNOD
               IF(X(2,I)+DT2*W(2,I) >= ZERO)CYCLE
               W(2,I)=ZERO
               V(2,I)=ZERO
            ENDDO!next I
            IF(IMON > 0 .AND. ITASK == 0) CALL STOPTIME(5,1)
         ENDIF
C-----------------------
C     RIGID WALL ALE or ALE-TH
C-----------------------
         IF(NRWALL > 0) THEN
            IF(IMON > 0 .AND. ITASK == 0) CALL STARTIME(4,1)
            CALL RGWAL1(
     1           X                   ,A                      ,V          ,W       ,RWBUF           ,
     2           NPRW(1+NNPRW*NRWALL),NPRW                   ,
     3           ICODT               ,ISKEW                  ,SKEW       ,MS      ,FSAV(1,NINTER+1),
     4           IXS                 ,IXQ                    ,ELBUF_TAB  ,IPARG   ,
     5           PM                  ,ALE_CONNECTIVITY%NALE  ,TF         ,NPF     ,WEIGHT          ,
     6           IAD_ELEM            ,FR_ELEM                ,FR_WALL         )
            IF(IMON > 0.AND. ITASK == 0) CALL STOPTIME(4,1)
         ENDIF
C-----------------------
C     LASER
C-----------------------
         IF (NLASER > 0) THEN
            CALL LASER1(LAS ,XLAS ,MS       ,X    ,V    ,
     .                  W   ,WA   ,IPARG    ,IXQ  ,PM   ,
     .                  TF  ,NPF  ,ELBUF_TAB)
         ENDIF
C-----------------------
      ENDIF                     ! ITASK == 0

C-----------------------------------
C     LAGRANGIAN ROTATIONS
C-----------------------------------
      IF(IMPOSE_DR /= 0 .AND. IRODDL /= 0) THEN
#include      "vectorize.inc"
         DO N=NODFT,NODLT
            DR(1,N)=DR(1,N)+DT2*VR(1,N)
            DR(2,N)=DR(2,N)+DT2*VR(2,N)
            DR(3,N)=DR(3,N)+DT2*VR(3,N)
         ENDDO
      ENDIF

C-----------------------------------
C     GRID DISPLACEMENT
C-----------------------------------
      IF(IMON > 0 .AND. ITASK == 0) CALL STARTIME(5,1)
C
      CALL MY_BARRIER
C
      IF (.NOT. MULTI_FVM%IS_USED) THEN
         IF (IRESP == 1) THEN
#include      "vectorize.inc"
            DO N=NODFT,NODLT
               VDT2 = DT2*W(1,N)
               DDP(1,N) = DDP(1,N)+VDT2
               D(1,N) = D(1,N)+VDT2
               XDP(1,N) = XDP(1,N)+VDT2
               X(1,N) = XDP(1,N)

               VDT2 = DT2*W(2,N)
               DDP(2,N) = DDP(2,N)+VDT2
               D(2,N) = D(2,N)+VDT2
               XDP(2,N) = XDP(2,N)+VDT2
               X(2,N) = XDP(2,N)

               VDT2 = DT2*W(3,N)
               DDP(3,N) = DDP(3,N)+VDT2
               D(3,N) = D(3,N)+VDT2
               XDP(3,N) = XDP(3,N)+VDT2
               X(3,N) = XDP(3,N)
            ENDDO
         ELSE
#include      "vectorize.inc"
            DO N=NODFT,NODLT
               VDT = DT2*W(1,N)
               D(1,N) = D(1,N)+VDT
               X(1,N) = X(1,N)+VDT

               VDT = DT2*W(2,N)
               D(2,N) = D(2,N)+VDT
               X(2,N) = X(2,N)+VDT

               VDT = DT2*W(3,N)
               D(3,N) = D(3,N)+VDT
               X(3,N) = X(3,N)+VDT
            ENDDO
         ENDIF
      ENDIF
C-----------------------------------------------
      CALL MY_BARRIER
C-----------------------------------------------
      IF(IMON > 0 .AND. ITASK == 0) THEN
         CALL STOPTIME(5,1)
      ENDIF
C-----------------------------------------------
      RETURN
      END SUBROUTINE ALEWDX
