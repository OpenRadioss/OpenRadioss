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
!||    pblast_load_computation   ../engine/source/loads/pblast/pblast.F
!||--- called by ------------------------------------------------------
!||    resol                     ../engine/source/engine/resol.F
!||--- calls      -----------------------------------------------------
!||    my_barrier                ../engine/source/system/machine.F
!||    pblast_1                  ../engine/source/loads/pblast/pblast_1.F
!||    pblast_2                  ../engine/source/loads/pblast/pblast_2.F
!||    pblast_3                  ../engine/source/loads/pblast/pblast_3.F
!||--- uses       -----------------------------------------------------
!||    groupdef_mod              ../common_source/modules/groupdef_mod.F
!||    h3d_mod                   ../engine/share/modules/h3d_mod.F
!||    pblast_mod                ../common_source/modules/loads/pblast_mod.F90
!||    th_surf_mod               ../common_source/modules/interfaces/th_surf_mod.F
!||====================================================================
      SUBROUTINE PBLAST_LOAD_COMPUTATION (PBLAST,ILOADP  ,FAC   ,A  , V   , X        ,
     1                        IADC    ,FSKY    ,LLOADP   ,FEXT  ,NODA_SURF, NODA_PEXT,
     2                        ITAB    ,H3D_DATA,TH_SURF  ,WFEXT)
C-----------------------------------------------
C   M o d u l e s
C----------------------------------------------- 
      USE H3D_MOD 
      USE PBLAST_MOD
      USE GROUPDEF_MOD  
      USE TH_SURF_MOD , ONLY : TH_SURF_
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "com06_c.inc"
#include      "com08_c.inc"
#include      "mvsiz_p.inc"
#include      "tabsiz_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE(PBLAST_),INTENT(INOUT) :: PBLAST
      INTEGER,INTENT(IN) :: LLOADP(SLLOADP)
      INTEGER,INTENT(INOUT) :: ILOADP(SIZLOADP,NLOADP)
      INTEGER,INTENT(IN) :: IADC(*)
      INTEGER, INTENT(IN) :: ITAB(NUMNOD)
      my_real,INTENT(INOUT) :: FAC(LFACLOAD,NLOADP)
      my_real,INTENT(IN) :: V(3,NUMNOD),X(3,NUMNOD)
      my_real,INTENT(INOUT) :: A(3,NUMNOD),FSKY(8,SFSKY/8), FEXT(3,NUMNOD)
      my_real,INTENT(INOUT) :: NODA_SURF(NUMNOD)
      my_real,INTENT(INOUT) :: NODA_PEXT(NUMNOD)
      TYPE(H3D_DATABASE),INTENT(IN) :: H3D_DATA
      TYPE (TH_SURF_) , INTENT(INOUT) :: TH_SURF
      DOUBLE PRECISION,INTENT(INOUT) :: WFEXT
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
       INTEGER :: NL, ABAC_ID, ID, II, IJK, NN(4), NNOD, IAD , IL, NSEGPL
       my_real :: DTMIN_LOC, T_STOP, TA_FIRST
       DOUBLE PRECISION :: WFEXT_LOC
       LOGICAL :: IS_RESET

C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C this subroutines is applying pressure load to
C a segment submitted to a blast wave.
C Preussre time histories are built from "UFC 3-340-02, Dec. 5th 2008" tables which are hardcoded in unit system {g, cm, mus}
C 3 different models are available to build the pressure time history :
C    FREE AIR      (ABAC_ID = 1)
C    SURFACE BURST (ABAC_ID = 2)
C    AIR BURST     (ABAC_ID = 3)
C-----------------------------------------------
C   P r e - C o n d i t i o n
C-----------------------------------------------
       PBLAST%PBLAST_DT%DT = EP20
       PBLAST%PBLAST_DT%IDT = 0

       IF(PBLAST%NLOADP_B==0)THEN
         RETURN
       ENDIF     
       NSEGPL = TH_SURF%NSEGLOADPF
C-----------------------------------------------,
C   S o u r c e   C o d e
C-----------------------------------------------
!$OMP PARALLEL
!$OMP+ SHARED(FAC,A,V,X,WFEXT,IADC,FSKY,LLOADP,FEXT,ITAB,H3D_DATA,TT,ILOADP)
!$OMP+ PRIVATE(ABAC_ID,ID,DTMIN_LOC,NL,WFEXT_LOC,TA_FIRST,IJK)

       !-----------------------------------------------
       !   LOOP OVER all /LOAD/PBLAST options
       !-----------------------------------------------

       DO NL=NLOADP_F+1, NLOADP_F+PBLAST%NLOADP_B

         ABAC_ID  = ILOADP(07,NL)
         ID       = ILOADP(08,NL) !user_id
         TA_FIRST = FAC(07,NL)
         T_STOP   = FAC(13,NL)
         IL       = NL-NLOADP_F
         IS_RESET = PBLAST%PBLAST_TAB(IL)%IS_RESET
         WFEXT_LOC = ZERO
         DTMIN_LOC = EP20

         IF(TT <= T_STOP)THEN
           SELECT CASE(ABAC_ID)
             CASE(1)
               !--- LOADING MODEL : FREE AIR, SPHERICAL CHARGE
               CALL PBLAST_1(PBLAST,
     1                       ILOADP  ,FAC      ,A        ,V         ,X        ,
     2                       IADC    ,FSKY     ,LLOADP   ,FEXT      ,NODA_SURF, NODA_PEXT,
     3                       ITAB    ,H3D_DATA ,NL       ,DTMIN_LOC ,WFEXT_LOC,
     4                       TH_SURF ,NSEGPL   )
     
             CASE(2)
               !--- LOADING MODEL : GROUND REFLECTION, HEMISPHERICAL CHARGE
               CALL PBLAST_2(PBLAST,
     1                       ILOADP  ,FAC      ,A        ,V         ,X        ,
     2                       IADC    ,FSKY     ,LLOADP   ,FEXT      ,NODA_SURF, NODA_PEXT,
     3                       ITAB    ,H3D_DATA ,NL       ,DTMIN_LOC ,WFEXT_LOC,
     4                       TH_SURF ,NSEGPL   )
             CASE(3)
               !--- LOADING MODEL : SURFACE BURST, SPHERICAL CHARGE
               CALL PBLAST_3(PBLAST,
     1                       ILOADP  ,FAC      ,A        ,V         ,X        ,
     2                       IADC    ,FSKY     ,LLOADP   ,FEXT      ,NODA_SURF, NODA_PEXT,
     3                       ITAB    ,H3D_DATA ,NL       ,DTMIN_LOC ,WFEXT_LOC,
     4                       TH_SURF ,NSEGPL   )
     
             END SELECT 
             
         ELSEIF(TT > T_STOP)THEN         
           DTMIN_LOC = EP20
           IF(.NOT. IS_RESET)THEN
            ! FLUSH fsky array to 0.
!$OMP DO SCHEDULE(GUIDED,MVSIZ)           
             DO II = 1,ILOADP(1,NL)/4
               !nodes of structural face : N1,N2,N3,N4
               NN(1)=LLOADP(ILOADP(4,NL)+4*(II-1))
               NN(2)=LLOADP(ILOADP(4,NL)+4*(II-1)+1)
               NN(3)=LLOADP(ILOADP(4,NL)+4*(II-1)+2)
               NN(4)=LLOADP(ILOADP(4,NL)+4*(II-1)+3)
               IF(NN(4) /= 0 .AND.NN(1) /= NN(2) .AND. NN(1) /= NN(3) .AND. NN(1) /= NN(4) .AND.
     .            NN(2) /= NN(3) .AND. NN(2) /= NN(4) .AND. NN(3) /= NN(4) )THEN
                 NNOD=4
               ELSE
                 NNOD=3
               ENDIF
               DO IJK=1,NNOD
                 IAD = IADC(ILOADP(4,NL)+4*(II-1)+(IJK-1))  ! node indexes in FSKY related to current option /LOAD/PBLAST
                 FSKY(1:3,IAD) = ZERO
               ENDDO
               PBLAST%PBLAST_TAB(IL)%IS_RESET = .TRUE.
             ENDDO! next II
!$OMP END DO          
           ENDIF!(.NOT. IS_RESET)
         
         ENDIF

#include "lockon.inc"
         WFEXT = WFEXT + WFEXT_LOC
         ! Time Step (DT_PBLAST) and corresponding option id (IDT_PBLAST)
         ! if nothing was loaded : set time step to go on arrival time (no CPU time wasted in nothing happen elsewhere)
         ! if load has started, use positive impulse to determine a time step for this current option

           IF(DTMIN_LOC < PBLAST%PBLAST_DT%DT)THEN
              PBLAST%PBLAST_DT%IDT  = ID
              PBLAST%PBLAST_DT%DT = DTMIN_LOC
           ENDIF

#include  "lockoff.inc"

           CALL MY_BARRIER()
                
       ENDDO !next NL

!$OMP END PARALLEL
                     

      END SUBROUTINE
