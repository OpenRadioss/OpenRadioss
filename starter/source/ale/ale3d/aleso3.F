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
Chd|  ALESO3                        source/ale/ale3d/aleso3.F     
Chd|-- called by -----------
Chd|        ALELEC                        source/ale/alelec.F           
Chd|-- calls ---------------
Chd|        ALE_CONNECTIVITY_MOD          ../common_source/modules/ale_connectivity_mod.F
Chd|====================================================================
      SUBROUTINE ALESO3(ALE_CONNECTIVITY,PM,IXS,GEO)
      USE ALE_CONNECTIVITY_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "units_c.inc"
#include      "scr03_c.inc"
#include      "param_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  :: IXS(NIXS,*)
      my_real  :: PM(NPROPM,*), GEO(NPROPG,*)
      TYPE(t_ale_connectivity), INTENT(INOUT) :: ALE_CONNECTIVITY
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IVPR(6), M, IAL, I, IAD1, LGTH

      IF(IPRI.GE.3)THEN
       WRITE(IOUT,1200)
       DO M=1,NUMELS
        IAL=NINT(PM(71,IABS(IXS(1,M)))+PM(72,IABS(IXS(1,M))))
        IF(IAL.EQ.0)CYCLE
        IAD1 = ALE_CONNECTIVITY%ee_connect%iad_connect(M)
        LGTH = ALE_CONNECTIVITY%ee_connect%iad_connect(M+1)-ALE_CONNECTIVITY%ee_connect%iad_connect(M) 
        DO  I=1,LGTH
            IVPR(I)=ALE_CONNECTIVITY%ee_connect%connected(IAD1 + I - 1)
            IF(IVPR(I).NE.0)IVPR(I)=IXS(11,IVPR(I))
          ENDDO
          WRITE(IOUT,'(7I10)')IXS(11,M),(IVPR(I),I=1,6)
       ENDDO
      ENDIF

 1200 FORMAT(//
     & 6X,'A.L.E.  ELEMENT CONNECTIVITIES                ',/
     & 6X,'------------------------------                ',/
     & 6X,'ELEM      ELE1      ELE2      ELE3      ELE4      ELE5',
     & 6X,'ELE6')
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7--
      END
