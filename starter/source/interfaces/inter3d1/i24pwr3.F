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
Chd|  I24PWR3                       source/interfaces/inter3d1/i24pwr3.F
Chd|-- called by -----------
Chd|        ININT3                        source/interfaces/inter3d1/inint3.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE I24PWR3(ITAB  ,INACTI,CAND_E,CAND_N ,PMIN     ,
     1                   PENMIN,I_STOK,NSV   ,IWPENE ,PENE_OLD,
     2                   NOINT ,NTY   ,MSR   ,IRTLM  ,IRECT    ,
     4                   NSN   ,ID    ,TITR   ,ICONT_I,IWPENE0 )
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
#ifndef HYPERMESH_LIB
      USE MESSAGE_MOD
#endif
C
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr03_c.inc"
#include      "scr17_c.inc"
#include      "units_c.inc"
#include      "vect07_c.inc"
#include      "com04_c.inc"
#include      "scr05_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ITAB(*),CAND_E(*),CAND_N(*), IRECT(4,*), IRTLM(2,*),
     .        ICONT_I(*)
      INTEGER I_STOK,NSV(*),MSR(*),IWPENE,INACTI,NOINT,NTY,NSN,IWPENE0
C     REAL ou REAL*8
      my_real
     .   PMIN(*),PENMIN,PENE_OLD(5,*)
      INTEGER ID
      CHARACTER*nchartitle,
     .   TITR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, L,NI
      INTEGER IX1, IX2, IX3, IX4, NSVG
      my_real
     .   PEN,TOL
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      IF (IRESP==1.AND.PENMIN<=EM06) PENMIN = DEUX*EM06
      TOL = PENMIN
C      
       DO I=1,I_STOK
         NI=CAND_N(I)
         L=CAND_E(I)
C---Cas INACTI=0,1----
          PEN =ZERO
         IF(ICONT_I(NI)==-L)THEN
C-------Cas INACTI=-1,5	  
	   PEN = PMIN(NI)
         ELSEIF(IRTLM(1,NI)==L)THEN
	   PEN = PENE_OLD(5,NI)
	 ELSE
	  CYCLE
	 END IF
           IX1=IRECT(1,L)
           IX2=IRECT(2,L)
           IX3=IRECT(3,L)
           IX4=IRECT(4,L)
           NSVG=NSV(NI)
C
          IF(PEN/=ZERO)THEN
C            True initial penetration
             IWPENE=IWPENE+1
             IF(IPRI>=5.AND.NSVG<=NUMNOD) THEN
#ifndef HYPERMESH_LIB
                CALL ANCMSG(MSGID=1164,
     .                           MSGTYPE=MSGWARNING,
     .                           ANMODE=ANINFO_BLIND_1,
     .                           I1=ITAB(NSVG),
     .                           I2=ITAB(IX1),
     .                           I3=ITAB(IX2),
     .                           I4=ITAB(IX3),
     .                           I5=ITAB(IX4),
     .                           R1=PEN      ,
     .                           PRMOD=MSG_CUMU)
#endif
             END IF
            IF (PEN<=TOL)IWPENE0=IWPENE0+1
          END IF
       END DO !I=1,I_STOK
C
      RETURN
      END
