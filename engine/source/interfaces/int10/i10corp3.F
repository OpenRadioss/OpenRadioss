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
Chd|  I10CORP3                      source/interfaces/int10/i10corp3.F
Chd|-- called by -----------
Chd|        I10FORCF3                     source/interfaces/int10/i10ke3.F
Chd|-- calls ---------------
Chd|        IMP_INTM                      share/modules/imp_intm.F      
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE I10CORP3(JLT   ,X     ,IRECT ,CAND_E  ,CAND_N  ,
     1                   STIF  ,H1    ,H2    ,H3      ,H4       ,
     2                   N1    ,N2    ,N3    ,NT1     ,NT2      ,
     3                   NT3   ,IX1   ,IX2   ,IX3     ,IX4      ,
     4                   NSVG  ,VXI   ,VYI   ,VZI     ,MSI      ,
     5                   DXI   ,DYI   ,DZI   ,NSN     ,NIN      ,
     6                   JLT_NEW,LREM )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TRI7BOX
      USE IMP_INTM
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IRECT(4,*), CAND_E(*), CAND_N(*),
     .         JLT,JLT_NEW,NIN,NSN,LREM
      INTEGER IX1(MVSIZ), IX2(MVSIZ), IX3(MVSIZ), IX4(MVSIZ),
     .        NSVG(MVSIZ)
C     REAL
C     REAL
      my_real
     .   VXI(MVSIZ), VYI(MVSIZ), VZI(MVSIZ), MSI(MVSIZ),
     .   X(3,*), STIF(*),N1(MVSIZ), N2(MVSIZ), N3(MVSIZ),
     .   NT1(MVSIZ), NT2(MVSIZ), NT3(MVSIZ),
     .   H1(MVSIZ), H2(MVSIZ), H3(MVSIZ), H4(MVSIZ),
     .   DXI(MVSIZ),DYI(MVSIZ),DZI(MVSIZ)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I ,J  ,NI, L, NN, NS ,NE
      my_real
     .   T1,T2,T3,NORM
C-----------------------------------------------
C
C
        DO I=1,JLT
         NI = CAND_N(I)
         L  = CAND_E(I)
         IF(NI.GT.NSN)THEN
           NN = NI - NSN
           JLT_NEW = JLT_NEW + 1
           VXI(JLT_NEW)= VFI(NIN)%P(1,NN)
           VYI(JLT_NEW)= VFI(NIN)%P(2,NN)
           VZI(JLT_NEW)= VFI(NIN)%P(3,NN)
           MSI(JLT_NEW)= MSFI(NIN)%P(NN)
           IX1(JLT_NEW)  = IRECT(1,L)
           IX2(JLT_NEW)  = IRECT(2,L)
           IX3(JLT_NEW)  = IRECT(3,L)
           IX4(JLT_NEW)  = IRECT(4,L)
	   NS=IND_INT(NIN)%P(NN)
 	   NE=SHF_INT(NIN)+JLT_NEW+LREM
           NSVG(JLT_NEW) = NS
           H1(JLT_NEW)  = H_E(1,NE)
           H2(JLT_NEW)  = H_E(2,NE)
           H3(JLT_NEW)  = H_E(3,NE)
           H4(JLT_NEW)  = H_E(4,NE)
	   STIF(JLT_NEW)=STIFS(NE)
	   N1(JLT_NEW)=N_E(1,NE)
	   N2(JLT_NEW)=N_E(2,NE)
	   N3(JLT_NEW)=N_E(3,NE)
C----------displacement
	   DXI(JLT_NEW)=DFI(1,NS)
	   DYI(JLT_NEW)=DFI(2,NS)
	   DZI(JLT_NEW)=DFI(3,NS)
           T1 = X(1,IX3(JLT_NEW)) - X(1,IX1(JLT_NEW))
           T2 = X(2,IX3(JLT_NEW)) - X(2,IX1(JLT_NEW))
           T3 = X(3,IX3(JLT_NEW)) - X(3,IX1(JLT_NEW))
           NORM = ONE/SQRT(T1*T1+T2*T2+T3*T3)
           NT1(JLT_NEW) = T1*NORM
           NT2(JLT_NEW) = T2*NORM
           NT3(JLT_NEW) = T3*NORM
C
         END IF
C
        END DO
C
      RETURN
      END
