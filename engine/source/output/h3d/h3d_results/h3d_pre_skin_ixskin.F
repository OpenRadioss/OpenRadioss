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
Chd|  H3D_PRE_SKIN_IXSKIN           source/output/h3d/h3d_results/h3d_skin_ixskin.F
Chd|-- called by -----------
Chd|        H3D_SKIN_IXSKIN               source/output/h3d/h3d_results/h3d_skin_ixskin.F
Chd|-- calls ---------------
Chd|        H3D_INC_MOD                   share/modules/h3d_inc_mod.F   
Chd|        PINCHTYPE_MOD                 ../common_source/modules/pinchtype_mod.F
Chd|====================================================================
      SUBROUTINE H3D_PRE_SKIN_IXSKIN(NODAL_IPART,IB ,ILOADP,LLOADP,
     .                               IXSKIN  ,NSKIN ,IMAPSKP)
C-----------------------------------------------
C   M o d u l e s 
C-----------------------------------------------
      USE PINCHTYPE_MOD 
      USE H3D_INC_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "tabsiz_c.inc"
C-----------------------------------------------
C   E x t e r n a l  F u n c t i o n s
C-----------------------------------------------
C-----------------------------------------------,
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER , DIMENSION(NUMSKINP0), INTENT(IN) ::  IMAPSKP
      INTEGER LLOADP(SLLOADP),NSKIN,IXSKIN(NIXQ,*)
      INTEGER ILOADP(SIZLOADP,*),IB(NIBCLD,*),NODAL_IPART(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NL, N1, ISK, N2, N3, N4, N5, 
     .        IAD ,NP ,IFUNC ,NPRES,NSKIN0,NSKIN1,N,I,PID0
C=======================================================================
C-----IXSKIN(1,*):ipart;IXSKIN(2:5,*):N1-4,IXSKIN(6,*):pid;IXSKIN(7,*):usr_id
       PID0 =0
       NSKIN0 = NSKIN
       NP = 0
C-----Force (pressure) first 
       DO NL=1,NCONLD-NPLOADPINCH
         N1      = IB(1,NL)
         N2      = IB(2,NL)
         N3      = IB(3,NL)
         N4      = IB(4,NL)
		 IF (N1==0.OR.N2==0.OR.N3==0.OR.N4==-1) CYCLE
C        
		 NP = NP + 1
         NSKIN = NSKIN0+ IMAPSKP(NP)
		 IXSKIN(2,NSKIN)=N1
		 IXSKIN(3,NSKIN)=N2
		 IXSKIN(4,NSKIN)=N3
		 IF (N4==0) THEN
		  IXSKIN(5,NSKIN)=N3
		 ELSEIF(N4>0) THEN
		  IXSKIN(5,NSKIN)=N4
		 END IF
		 IXSKIN(1,NSKIN)=NODAL_IPART(N1)
		 IXSKIN(6,NSKIN)=NSKIN0+NL
       END DO 
       NSKIN1 = NSKIN
C----------load_pressure	   
       DO NL=1,NLOADP_HYD
         IAD = ILOADP(4,NL)
         DO N=1, ILOADP(1,NL)/4
           N1 = LLOADP(IAD+4*(N-1))
           N2 = LLOADP(IAD+4*(N-1)+1)
           N3 = LLOADP(IAD+4*(N-1)+2)
           N4 = LLOADP(IAD+4*(N-1)+3)
		   IF (N1==0.OR.N2==0.OR.N3==0) CYCLE
C          
		   NP = NP + 1
		   IF (IMAPSKP(NP) < NP) CYCLE
           NSKIN = NSKIN0+ IMAPSKP(NP)
		   IXSKIN(2,NSKIN)=N1
		   IXSKIN(3,NSKIN)=N2
		   IXSKIN(4,NSKIN)=N3
		   IF (N4==0) THEN
		    IXSKIN(5,NSKIN)=N3
		   ELSEIF(N4>0) THEN
		    IXSKIN(5,NSKIN)=N4
		   END IF
		   IXSKIN(1,NSKIN)=NODAL_IPART(N1)
		   IXSKIN(6,NSKIN)=NSKIN1+NL
         ENDDO
       END DO !NP=1,NLOADP_HYD
C---------pfluid	   
       NSKIN1 = NSKIN
       DO NL=1,NLOADP_F
         IAD = ILOADP(4,NL)
         DO I = 1,ILOADP(1,NL)/4
           N1=LLOADP(IAD+4*(I-1))
           N2=LLOADP(IAD+4*(I-1)+1)
           N3=LLOADP(IAD+4*(I-1)+2)
           N4=LLOADP(IAD+4*(I-1)+3)
		   IF (N1==0.OR.N2==0.OR.N3==0) CYCLE
C          
		   NP = NP + 1
		   IF (IMAPSKP(NP) < NP) CYCLE
           NSKIN = NSKIN0+ IMAPSKP(NP)
		   IXSKIN(2,NSKIN)=N1
		   IXSKIN(3,NSKIN)=N2
		   IXSKIN(4,NSKIN)=N3
		   IF (N4==0) THEN
		    IXSKIN(5,NSKIN)=N3
		   ELSEIF(N4>0) THEN
		    IXSKIN(5,NSKIN)=N4
		   END IF
		   IXSKIN(1,NSKIN)=NODAL_IPART(N1)
		   IXSKIN(6,NSKIN)=NSKIN1+NL
         END DO 
       END DO 
C---------pblast	   
       NSKIN1 = NSKIN
       DO NL=1+NLOADP_F,NLOADP_F+NLOADP_B
         IAD = ILOADP(4,NL)
         DO I = 1,ILOADP(1,NL)/4
           N1=LLOADP(IAD+4*(I-1))
           N2=LLOADP(IAD+4*(I-1)+1)
           N3=LLOADP(IAD+4*(I-1)+2)
           N4=LLOADP(IAD+4*(I-1)+3)
		   IF (N1==0.OR.N2==0.OR.N3==0) CYCLE
C          
		   NP = NP + 1
		   IF (IMAPSKP(NP) < NP) CYCLE
           NSKIN = NSKIN0+ IMAPSKP(NP)
		   IXSKIN(2,NSKIN)=N1
		   IXSKIN(3,NSKIN)=N2
		   IXSKIN(4,NSKIN)=N3
		   IF (N4==0) THEN
		    IXSKIN(5,NSKIN)=N3
		   ELSEIF(N4>0) THEN
		    IXSKIN(5,NSKIN)=N4
		   END IF
		   IXSKIN(1,NSKIN)=NODAL_IPART(N1)
		   IXSKIN(6,NSKIN)=NSKIN1+NL
         END DO 
       END DO 
C
      RETURN
      END
