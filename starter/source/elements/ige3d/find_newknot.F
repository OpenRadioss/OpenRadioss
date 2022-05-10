Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2022 Altair Engineering Inc.
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
Chd|  FIND_NEWKNOT                  source/elements/ige3d/find_newknot.F
Chd|-- called by -----------
Chd|        PRERAFIG3D                    source/elements/ige3d/prerafig3d.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE FIND_NEWKNOT(IEL,KXIG3D,KNOT,DIR,IAD_KNOT,
     .                        NKNOT1,NKNOT2,NKNOT3,IDCUT,NEWKNOT)
C----------------------------------------------------------------------
C   ROUTINE QUI RENVOIE EN FONCTION DU NUMERO DE LA COUPE D'ELEMENT
C   A REALISER LE KNOT CORRESPONDANT A INSERER
C----------------------------------------------------------------------
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
#include      "analyse_name.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "tabsiz_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER KXIG3D(NIXIG3D,*)
      INTEGER IEL,DIR,IDCUT,IAD_KNOT,NKNOT1,NKNOT2,NKNOT3
      my_real KNOT(*),NEWKNOT
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IDNBCUT,IDKNOT1,IDKNOT2  
      INTEGER OFFSET_KNOT
C=======================================================================
C----------------------------------------------------------
C     ELEMENT ISO GEOMETRIQUE
C-----------------------------------------------
C     KXIG3D(6,*)   :   index of 1st knot in the Xknot vector corresponding to the element
C     KXIG3D(7,*)   :   index of 1st knot in the Yknot vector corresponding to the element
C     KXIG3D(8,*)   :   index of 1st knot in the Zknot vector corresponding to the element
C     KXIG3D(9,*)   :   index of 2nd knot in the Xknot vector corresponding to the element
C     KXIG3D(10,*)  :   index of 2nd knot in the Yknot vector corresponding to the element
C     KXIG3D(11,*)  :   index of 2nd knot in the Zknot vector corresponding to the element
C     KXIG3D(12,*)  :   number of element's cuts needed in the X direction
C     KXIG3D(13,*)  :   number of element's cuts needed in the Y direction
C     KXIG3D(14,*)  :   number of element's cuts needed in the Z direction
C=======================================================================                                         
C----------------------------------------------------------------------
C     INITIALISATION DES VARIABLES EN FONCTION DE LA DIRECTION DE COUPE
C----------------------------------------------------------------------
C
      IF(DIR.EQ.1) THEN
        IDNBCUT=12
        IDKNOT1=6  ! index of 1nd knot in the Xknot vector corresponding to the element
        IDKNOT2=9  ! index of 2nd knot in the Xknot vector corresponding to the element
        OFFSET_KNOT = IAD_KNOT
      ELSEIF(DIR.EQ.2) THEN
        IDNBCUT=13
        IDKNOT1=7  ! index of 1st knot in the Yknot vector corresponding to the element
        IDKNOT2=10 ! index of 1st knot in the Yknot vector corresponding to the element
        OFFSET_KNOT = IAD_KNOT + NKNOT1
      ELSEIF(DIR.EQ.3) THEN
        IDNBCUT=14
        IDKNOT1=8  ! index of 1st knot in the Zknot vector corresponding to the element
        IDKNOT2=11 ! index of 1st knot in the Zknot vector corresponding to the element
        OFFSET_KNOT = IAD_KNOT + NKNOT1 + NKNOT2
      ENDIF
C
C----------------------------------------------------------------------
C  NE PERMET QUE LES COUPES PAR 2,4,8... (DES COUPES PAIRES), ET
C  BORNE LE NOMBRE MAXIMAL DE COUPE PAR 8
C----------------------------------------------------------------------
C
      SELECT CASE (IDCUT)
       CASE(1) ! COUPE AU MILIEU DE L'ELEMENT (1/2)

        NEWKNOT=KNOT(OFFSET_KNOT+KXIG3D(IDKNOT1,IEL))+
     +       (KNOT(OFFSET_KNOT+KXIG3D(IDKNOT2,IEL))-KNOT(OFFSET_KNOT+KXIG3D(IDKNOT1,IEL)))/2

       CASE(2) ! COUPE AU PREMIER QUART DE L'ELEMENT (1/4)

        NEWKNOT=KNOT(OFFSET_KNOT+KXIG3D(IDKNOT1,IEL))+
     +       1*(KNOT(OFFSET_KNOT+KXIG3D(IDKNOT2,IEL))-KNOT(OFFSET_KNOT+KXIG3D(IDKNOT1,IEL)))/4

       CASE(3) ! COUPE AU DERNIER QUART DE L'ELEMENT (3/4)

        NEWKNOT=KNOT(OFFSET_KNOT+KXIG3D(IDKNOT1,IEL))+
     +       3*(KNOT(OFFSET_KNOT+KXIG3D(IDKNOT2,IEL))-KNOT(OFFSET_KNOT+KXIG3D(IDKNOT1,IEL)))/4

       CASE(4) ! COUPE AU PREMIER HUITIEME DE L'ELEMENT (1/8)

        NEWKNOT=KNOT(OFFSET_KNOT+KXIG3D(IDKNOT1,IEL))+
     +       1*(KNOT(OFFSET_KNOT+KXIG3D(IDKNOT2,IEL))-KNOT(OFFSET_KNOT+KXIG3D(IDKNOT1,IEL)))/8

       CASE(5) ! COUPE AU TROISIEME HUITIEME DE L'ELEMENT (3/8)

        NEWKNOT=KNOT(OFFSET_KNOT+KXIG3D(IDKNOT1,IEL))+
     +       3*(KNOT(OFFSET_KNOT+KXIG3D(IDKNOT2,IEL))-KNOT(OFFSET_KNOT+KXIG3D(IDKNOT1,IEL)))/8

       CASE(6) ! COUPE AU CINQUIEME HUITIEME DE L'ELEMENT (5/8)

        NEWKNOT=KNOT(OFFSET_KNOT+KXIG3D(IDKNOT1,IEL))+
     +       5*(KNOT(OFFSET_KNOT+KXIG3D(IDKNOT2,IEL))-KNOT(OFFSET_KNOT+KXIG3D(IDKNOT1,IEL)))/8

       CASE(7) ! COUPE AU DERNIER HUITIEME DE L'ELEMENT (7/8)

        NEWKNOT=KNOT(OFFSET_KNOT+KXIG3D(IDKNOT1,IEL))+
     +       7*(KNOT(OFFSET_KNOT+KXIG3D(IDKNOT2,IEL))-KNOT(OFFSET_KNOT+KXIG3D(IDKNOT1,IEL)))/8

      END SELECT
C
      RETURN
      END
C
C
Chd|====================================================================
Chd|  FIND_NEIGHBOURKNOT            source/elements/ige3d/find_newknot.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE FIND_NEIGHBOURKNOT(IDNEIGHBOUR,KXIG3D,TAB_OLDIDCUT,IDNBCUT,DIR,IDCUT_VOISIN)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
#include      "analyse_name.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER KXIG3D(NIXIG3D,*),TAB_OLDIDCUT(3,*)
      INTEGER IDNEIGHBOUR,IDNBCUT,IDCUT_VOISIN,DIR
C=======================================================================
C----------------------------------------------------------
C     ELEMENT ISO GEOMETRIQUE
C-----------------------------------------------
C     KXIG3D(6,*)   :   index of 1st knot in the Xknot vector corresponding to the element
C     KXIG3D(7,*)   :   index of 1st knot in the Yknot vector corresponding to the element
C     KXIG3D(8,*)   :   index of 1st knot in the Zknot vector corresponding to the element
C     KXIG3D(9,*)   :   index of 2nd knot in the Xknot vector corresponding to the element
C     KXIG3D(10,*)  :   index of 2nd knot in the Yknot vector corresponding to the element
C     KXIG3D(11,*)  :   index of 2nd knot in the Zknot vector corresponding to the element
C     KXIG3D(12,*)  :   number of element's cuts needed in the X direction
C     KXIG3D(13,*)  :   number of element's cuts needed in the Y direction
C     KXIG3D(14,*)  :   number of element's cuts needed in the Z direction
C=======================================================================
C    

cc LES ID VOISINS NE CORRESPONDENT PAS AU INITIAL CUT

      IF(KXIG3D(IDNBCUT,IDNEIGHBOUR).EQ.1) THEN 
        IDCUT_VOISIN = 0
      ELSE
        IDCUT_VOISIN = TAB_OLDIDCUT(DIR,IDNEIGHBOUR)-KXIG3D(IDNBCUT,IDNEIGHBOUR)+1
      ENDIF 
C
C
      RETURN
      END
