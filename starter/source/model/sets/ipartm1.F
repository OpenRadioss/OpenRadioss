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
!||    create_ipartm1   ../starter/source/model/sets/ipartm1.F
!||--- calls      -----------------------------------------------------
!||====================================================================
      SUBROUTINE CREATE_IPARTM1 (NPART,IPART,IPARTM1 )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s 
C-----------------------------------------------
      INTEGER, INTENT(IN) :: NPART
      INTEGER, INTENT(IN), DIMENSION(LIPART1,NPART)  :: IPART
      INTEGER, INTENT(INOUT),DIMENSION(NPART,2) :: IPARTM1
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
      INTEGER, DIMENSION(:),ALLOCATABLE :: IPARTSORT
      INTEGER, DIMENSION(:),ALLOCATABLE :: INDEX_SORT
      INTEGER, DIMENSION(70000) :: IWORK
C-----------------------------------------------
      ALLOCATE(IPARTSORT(NPART))
      ALLOCATE(INDEX_SORT(2*NPART))

      DO I=1,NPART
         IPARTSORT(I)=IPART(4,I)
         INDEX_SORT(I)=I
      ENDDO
      CALL MY_ORDERS(0,IWORK,IPARTSORT,INDEX_SORT,NPART,1)

      DO I=1,NPART
         IPARTM1(I,1)=IPARTSORT(INDEX_SORT(I))
         IPARTM1(I,2)=INDEX_SORT(I)
      ENDDO

      END
!||====================================================================
!||    part_usrtos   ../starter/source/model/sets/ipartm1.F
!||====================================================================
      INTEGER FUNCTION PART_USRTOS(IU,IPARTM1,NPART)
C      IDENTIQUE A USR2SYS, SANS GENERER D'ERREUR
C      FONCTION DONNE N0 SYSTEME DU NOEUD USER IU
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IU
      INTEGER NPART
      INTEGER IPARTM1(NPART,2)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER JINF, JSUP, J,I
      JINF=1
      JSUP=NPART
      J=MAX(1,NPART/2)
   10 IF(JSUP<=JINF.AND.(IU-IPARTM1(J,1))/=0) THEN
C      
        PART_USRTOS=0
        RETURN
      ENDIF
      IF((IU-IPARTM1(J,1))==0)THEN
C     >CAS IU=TABM FIN DE LA RECHERCHE
         PART_USRTOS=J
         RETURN
      ELSE IF (IU-IPARTM1(J,1)<0) THEN
C     >CAS IU<TABM
         JSUP=J-1
      ELSE
C     >CAS IU>TABM
         JINF=J+1
      ENDIF
      J=(JSUP+JINF)/2
      GO TO 10
      END
C
C-----------------------------------------------
!||====================================================================
!||    set_usrtos             ../starter/source/model/sets/ipartm1.F
!||--- called by ------------------------------------------------------
!||    create_elt_list        ../starter/source/model/sets/create_element_clause.F
!||    create_node_list       ../starter/source/model/sets/create_node_clause.F
!||    create_nodens_clause   ../starter/source/model/sets/create_nodens_clause.F90
!||    create_part_list       ../starter/source/model/sets/create_part_clause.F
!||    create_rbody_list      ../starter/source/model/sets/create_rbody_clause.F
!||    create_seg_clause      ../starter/source/model/sets/create_seg_clause.F
!||    create_set_list        ../starter/source/model/sets/create_set_clause.F
!||    create_setcol_list     ../starter/source/model/sets/create_setcol_clause.F
!||    create_subm_list       ../starter/source/model/sets/create_subm_clause.F
!||    create_subs_list       ../starter/source/model/sets/create_subs_clause.F
!||    hm_read_inistate_d00   ../starter/source/elements/initia/hm_read_inistate_d00.F
!||    hm_read_thgrne         ../starter/source/output/th/hm_read_thgrne.F
!||    hm_tagpart2            ../starter/source/groups/hm_tagpart2.F
!||    lectur                 ../starter/source/starter/lectur.F
!||====================================================================
      INTEGER FUNCTION SET_USRTOS(IU,IPARTM1,NPART)
C-----------------------------------------------
C   ROUTINE DESCRIPTION :
C   ===================
C   Dichotomy Over sorted array to obtain Local id from 
C   Global ID
C-----------------------------------------------
C   DUMMY ARGUMENTS DESCRIPTION:
C   ===================
C
C     NAME                                 DESCRIPTION                         
C
C     UI, INTEGER           : User ID
C     MAP(SZ,2)             : UID,LOCAL ID Map
C     SZ                    : Size of Option
C     Returns : indice in ipartm1 to get nearest local ID
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IU
      INTEGER NPART
      INTEGER IPARTM1(NPART,2)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER JINF, JSUP, J,I
      JINF=1
      JSUP=NPART
      J=MAX(1,NPART/2)
   10 IF(J == 0)THEN
C      
        SET_USRTOS=0
        RETURN
      ELSEIF(JSUP<=JINF.AND.(IU-IPARTM1(J,1))/=0) THEN
C      
        SET_USRTOS=0
        RETURN
      ENDIF
      IF((IU-IPARTM1(J,1))==0)THEN
C     >CAS IU=TABM FIN DE LA RECHERCHE
         SET_USRTOS=J
         RETURN
      ELSE IF (IU-IPARTM1(J,1)<0) THEN
C     >CAS IU<TABM
         JSUP=J-1
      ELSE
C     >CAS IU>TABM
         JINF=J+1
      ENDIF
      J=(JSUP+JINF)/2
      GO TO 10
      END
C
!||====================================================================
!||    set_usrtos_nearest     ../starter/source/model/sets/ipartm1.F
!||--- called by ------------------------------------------------------
!||    create_elt_list_g      ../starter/source/model/sets/create_element_clause.F
!||    create_node_list_g     ../starter/source/model/sets/create_node_clause.F
!||    create_part_list_g     ../starter/source/model/sets/create_part_clause.F
!||    create_rbody_list_g    ../starter/source/model/sets/create_rbody_clause.F
!||    create_set_list_g      ../starter/source/model/sets/create_set_clause.F
!||    create_setcol_list_g   ../starter/source/model/sets/create_setcol_clause.F
!||    create_subm_list_g     ../starter/source/model/sets/create_subm_clause.F
!||    create_subs_list_g     ../starter/source/model/sets/create_subs_clause.F
!||====================================================================
      INTEGER FUNCTION SET_USRTOS_NEAREST(UI,MAP,SZ,UPLOW)
C-----------------------------------------------
C   ROUTINE DESCRIPTION :
C   ===================
C   Dichotomy Over sorted array to obtain Local id from 
C   Global ID
C-----------------------------------------------
C   DUMMY ARGUMENTS DESCRIPTION:
C   ===================
C
C     NAME                                 DESCRIPTION                         
C
C     UI, INTEGER           : User ID
C     MAP(SZ,2)             : UID,LOCAL ID Map
C     SZ                    : Size of Option
C     UPLOW                 : 1 UP (take a majorant), 2 LOW (take a minorant)
C     Returns : indice in ipartm1 to get nearest local ID
C
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER UI
      INTEGER SZ, UPLOW
      INTEGER MAP(SZ,2)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER JINF, JSUP, J,I
      JINF=1
      JSUP=SZ

      IF ( UI >= MAP(SZ,1) ) THEN
         SET_USRTOS_NEAREST = SZ
         RETURN
      ENDIF

      IF ( UI <= MAP(1,1) ) THEN
         SET_USRTOS_NEAREST = 1
         RETURN
      ENDIF

      J=MAX(1,SZ/2)

   10 IF(JSUP<=JINF.AND.(UI-MAP(J,1))/=0) THEN
C       
        SET_USRTOS_NEAREST=0
        IF (UPLOW == 1) THEN

           DO WHILE (MAP(JINF,1) < UI)     ! FIRST Entity higher then UI
               JINF=JINF+1
           ENDDO
           SET_USRTOS_NEAREST=JINF

        ELSEIF (UPLOW == 2) THEN

           DO WHILE (MAP(JSUP,1) > UI)     ! FIRST Entity lower then UI
               JSUP=JSUP-1
           ENDDO
           SET_USRTOS_NEAREST=JSUP
           
        ENDIF
        RETURN
      ENDIF
      IF((UI-MAP(J,1))==0)THEN
C     >CAS IU=TABM FIN DE LA RECHERCHE
         SET_USRTOS_NEAREST=J
         RETURN
      ELSE IF (UI-MAP(J,1)<0) THEN
C     >CAS IU<TABM
         JSUP=J-1
      ELSE
C     >CAS IU>TABM
         JINF=J+1
      ENDIF
      J=(JSUP+JINF)/2
      GO TO 10
      END
C

!||====================================================================
!||    print_ipartm1   ../starter/source/model/sets/ipartm1.F
!||====================================================================
      SUBROUTINE PRINT_IPARTM1(NPART,IPARTM1 )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s 
C-----------------------------------------------
         INTEGER NPART,I
         INTEGER IPARTM1(NPART,2)
         DO I=1,NPART
            print*,I,'IPART=',IPARTM1(I,1),'--',IPARTM1(I,2)
         ENDDO
      END

