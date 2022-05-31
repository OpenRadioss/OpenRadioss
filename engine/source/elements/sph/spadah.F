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
Chd|  SPADAH                        source/elements/sph/spadah.F  
Chd|-- called by -----------
Chd|        FORINTP                       source/elements/forintp.F     
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        GET_U_GEO                     source/user_interface/upidmid.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE SPADAH(
     1    X       ,V       ,MS      ,SPBUF   ,ITAB    ,
     2    KXSP    ,IXSP    ,NOD2SP  ,WA      ,WASPACT ,
     3    ITASK   ,IPARTSP ,IPART)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
C----6---------------------------------------------------------------7---------8
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com08_c.inc"
#include      "sphcom.inc"
#include      "param_c.inc"
#include      "units_c.inc"
#include      "task_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER KXSP(NISP,*),IXSP(KVOISPH,*),NOD2SP(*),ITAB(*),
     .        WASPACT(*),ITASK,IPARTSP(*),IPART(LIPART1,*)
C     REAL ou REAL*8
      my_real
     .   X(3,*) ,V(3,*) ,MS(*) ,SPBUF(NSPBUF,*) ,WA(KWASPH,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER N,INOD,NVOIS,NS,IPRT,IPROP
      my_real
     .       XI,YI,ZI,DI,DIVV,COEFF,GET_U_GEO
C-----------------------------------------------
      EXTERNAL GET_U_GEO      
C-----------------------------------------------
      DO NS=ITASK+1,NSPHACT,NTHREAD
        N=WASPACT(NS)
        INOD =KXSP(3,N)
        NVOIS=KXSP(4,N)
        XI=X(1,INOD)
        YI=X(2,INOD)
        ZI=X(3,INOD)
        DI   =SPBUF(1,N)
        DIVV =WA(13,N)
        IPRT =IPARTSP(N)
        IPROP=IPART(2,IPRT)
        COEFF = GET_U_GEO(8,IPROP)
C--     3D dilatation of H : GET_U_GEO(8,IPROP) = 1/3
C--     1D dilatation of H : GET_U_GEO(8,IPROP) = 1
C--     Constant H : GET_U_GEO(8,IPROP) = 0        
        SPBUF(1,N)=DI*(ONE+DIVV*DT1*COEFF)
        IF(SPBUF(1,N).LT.EM20)THEN
          CALL ANCMSG(MSGID=174,ANMODE=ANINFO,
     .            I1=KXSP(NISP,N))
          CALL ARRET(2)
        ENDIF
      ENDDO
C-----------------------------------------------
      RETURN
      END
