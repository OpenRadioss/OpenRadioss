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
Chd|  CUTMAIN                       source/tools/sect/cutmain.F   
Chd|-- called by -----------
Chd|        GENANI                        source/output/anim/generate/genani.F
Chd|-- calls ---------------
Chd|        CUTCON                        source/tools/sect/cutcon.F    
Chd|====================================================================
      SUBROUTINE CUTMAIN(ICUT,XCUT,IXS,XYZ0,D,
     .                       NODCUT,NELCUT,ICBUF,CBUF,LEN,NBF)
C------------------------------------------
C COMPTAGE DU NOMBRE D'ELEMENTS COUPES
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scrcut_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real
     .   XCUT(7,*),XYZ0(3,*),D(3,*),CBUF(*)
      INTEGER ICUT(44,*),IXS(NIXS,*),ICBUF(*),NODCUT,NELCUT,LEN,NBF
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IC,ITYP,NUMTOT,NUMEL,
     .MIC1,MIC2,MIC3,MIC4,MIC5,MIC6,MAC1,MAC2,MAC3
      MIC1=1
      MIC2=MIC1+10*LEN
      MIC3=MIC2+12*LEN
      MIC4=MIC3+6*LEN
      MIC5=MIC4+NCUTS
      MIC6=MIC5+NCUTS
      MAC1=1
      MAC2=MAC1+18*LEN
      MAC3=MAC2+6*LEN
      DO IC=1,NCUTS
        ITYP=ICUT(1,IC)
        CALL CUTCON(ITYP,XCUT(1,IC),XCUT(4,IC),XYZ0,IXS,D,
     .              NUMTOT,NUMEL,
     .              ICBUF(MIC1),ICBUF(MIC2),ICBUF(MIC3),
     .              CBUF(MAC1),CBUF(MAC2),NODCUT) 
        NODCUT=NODCUT+NUMTOT
        NELCUT=NELCUT+NUMEL
        ICBUF(MIC4)=NELCUT+NBF
        ICBUF(MIC5)=NODCUT          
        MIC1=MIC1+5*NUMEL
        MIC2=MIC2+NUMTOT*2
        MIC4=MIC4+1
        MIC5=MIC5+1
        MAC1=MAC1+3*NUMTOT
        MAC2=MAC2+NUMTOT
      ENDDO
      RETURN
      END
