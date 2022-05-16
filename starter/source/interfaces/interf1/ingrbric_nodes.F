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
Cts
Cts Node definition for secnd side of Itnerface Type18.
Cts Input for secnd side is a GRBRIC. Nodes are extracted 
Cts from it into a sorted list
Chd|====================================================================
Chd|  INGRBRIC_NODES                source/interfaces/interf1/ingrbric_nodes.F
Chd|-- called by -----------
Chd|        LECINS                        source/interfaces/interf1/lecins.F
Chd|        LECINT                        source/interfaces/interf1/lecint.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE INGRBRIC_NODES(MSN,NOINT,IBUFSSG,ITAB,MSV, IXS, NBRIC,IFLAG, NALE, IPM, BUFMAT, S_MSV)
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "scr03_c.inc"
#include      "warn_c.inc"
#include      "scr17_c.inc"
#include      "alesolver.inc"
#include      "tabsiz_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER,INTENT(IN)    :: NOINT,NBRIC,S_MSV
      INTEGER,INTENT(INOUT) :: MSN
      INTEGER,INTENT(IN)    :: IBUFSSG(NBRIC), ITAB(NUMNOD), IXS(NIXS,NUMELS)
      INTEGER,INTENT(IN)    ::IFLAG
      INTEGER,INTENT(INOUT) :: MSV(1:S_MSV)
      INTEGER,INTENT(IN) :: NALE(NUMNOD)
      INTEGER,INTENT(IN) :: IPM(NPROPMI,NUMMAT)
      my_real,INTENT(IN),TARGET :: BUFMAT(SBUFMAT)
C-----------------------------------------------
C   L o c a l   V a r a i b l e s
C-----------------------------------------------
      INTEGER :: I, J, VALMIN, TMP, POSMIN, INOD
      INTEGER, DIMENSION(:),ALLOCATABLE,TARGET :: MSVTMP
      my_real, DIMENSION(:), POINTER :: UPARAM
      INTEGER NPAR, IADBUF, IFORM, IMAT, IELEM, ILAW
      LOGICAL IS_ELEM_A_BOUNDARY_MAT
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
            
      ALLOCATE(MSVTMP(1:NUMNOD))
      MSVTMP(1:NUMNOD)=0

      !TAG NODES - remove lagrangian nodes
      DO I=1,NBRIC
        IELEM = IBUFSSG(I)
        DO J=2,9
          INOD = IXS(J,IELEM)
          IF(NALE(INOD)/=0 .AND. INOD>0)MSVTMP(INOD)=1   !NALE(node_i)==0 <=> lagrangian node_i
        ENDDO      
      ENDDO
      !TAG NODES - remove nodes from boundary materials (law11, & law51 iform=2,3,4,5,6)
      ! (do not loop unnecessarily otherwise)
      IF(IS_BOUNDARY_MATERIAL)THEN
        DO I=1,NBRIC
          IELEM = IBUFSSG(I)
          IS_ELEM_A_BOUNDARY_MAT = .FALSE.
          IMAT = IXS(1,IELEM)
          ILAW = IPM(2,IMAT)
          IFORM = 0
          IF(ILAW==51)THEN
            NPAR   = IPM(9,IMAT)                
            IADBUF = IPM(7,IMAT)                
            IADBUF = MAX(1,IADBUF)                
            UPARAM => BUFMAT(IADBUF:IADBUF+NPAR)  
            IFORM  = NINT(UPARAM(31))
            IF(IFORM>=2 .AND. IFORM<=6)IS_ELEM_A_BOUNDARY_MAT=.TRUE.
          ELSEIF(IFORM==11)THEN   
            IS_ELEM_A_BOUNDARY_MAT=.TRUE.
          ENDIF  
          IF(IS_ELEM_A_BOUNDARY_MAT)THEN
            DO J=2,9
              INOD = IXS(J,IELEM)
              MSVTMP(INOD)=0
            ENDDO
          ENDIF
        END DO
      ENDIF

      !ORDERING
      TMP=0
      DO I=1,NUMNOD
        IF(MSVTMP(I)==1)THEN
          MSVTMP(TMP+1)=I
          TMP=TMP+1
        ENDIF     
      END DO      
      MSN = TMP ! size of ordered list
      
      DO I=1,S_MSV
        MSV(I)=MSVTMP(I)
      ENDDO   
      DEALLOCATE(MSVTMP)

      !PRINTOUT
      IF(IPRI.GE.1) THEN
        WRITE(IOUT,'(/,A,/)')' NODES EXTRACTED FROM SOLID ELEMENTS'
        WRITE(IOUT,FMT=FMW_10I)(ITAB(MSV(I)),I=1,MSN)
      ENDIF
C------------------------------------------------------------
      RETURN
      END

