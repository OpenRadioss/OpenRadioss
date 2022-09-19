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
Chd|  ADMBCS                        source/model/remesh/admbcs.F  
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE ADMBCS(IXC,IPARTC,IXTG,IPARTTG,IPART,
     .                  ICODE,ISKEW,ITAB,SH4TREE,SH3TREE)
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "scr17_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "remesh_c.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IXC(NIXC,*),IPARTC(*),IXTG(NIXTG,*),IPARTTG(*),
     .        IPART(LIPART1,*),ICODE(*),ISKEW(*),ITAB(*),
     .        SH4TREE(KSH4TREE,*),SH3TREE(KSH3TREE,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IP,NLEV,N1,N2,IC1,IC2,ICOD,IS1,IS2,
     .        DIR, N
C-----------------------------------------------
      INTEGER MY_AND
      EXTERNAL MY_AND
C-----------------------------------------------
C
      DO N=1,NUMELC0
        IP   =IPARTC(N)
        NLEV =IPART(10,IP)
        IF(NLEV>0)THEN
          DO DIR=0,3
            N1=IXC(DIR+2,N)
            N2=IXC(MOD(DIR+1,4)+2,N)
            IC1 =ICODE(N1)
            IC2 =ICODE(N2)
            ICOD=MY_AND(IC1,IC2)
            IF(ICOD/=0)THEN
              IS1=ISKEW(N1)
              IS2=ISKEW(N2)
              IF(IS1/=IS2)THEN
                CALL ANCMSG(MSGID=650,
     .                      MSGTYPE=MSGERROR,
     .                      ANMODE=ANINFO,
     .                      I2=ITAB(N2),
     .                      I1=ITAB(N1),
     .                      I3=IXC(NIXC,N))
              END IF
              CALL ADMBCS4(DIR,ICOD,IS1,N ,IXC,
     .                     IPARTC,IPART,ICODE,ISKEW,SH4TREE)
            END IF
          END DO
        END IF
      END DO
C
      DO N=1,NUMELTG0
        IP   =IPARTTG(N)
        NLEV =IPART(10,IP)
        IF(NLEV>0)THEN
          DO DIR=0,2
            N1=IXTG(DIR+2,N)
            N2=IXTG(MOD(DIR+1,3)+2,N)
            IC1 =ICODE(N1)
            IC2 =ICODE(N2)
            ICOD=MY_AND(IC1,IC2)
            IF(ICOD/=0)THEN
              IS1=ISKEW(N1)
              IS2=ISKEW(N2)
              IF(IS1/=IS2)THEN
                CALL ANCMSG(MSGID=650,
     .                      MSGTYPE=MSGERROR,
     .                      ANMODE=ANINFO,
     .                      I2=ITAB(N2),
     .                      I1=ITAB(N1),
     .                      I3=IXTG(NIXTG,N))
              END IF
              CALL ADMBCS3(DIR,ICOD,IS1,N ,IXTG,
     .                     IPARTTG,IPART,ICODE,ISKEW,SH3TREE)
            END IF
          END DO
        END IF
      END DO

      RETURN
      END


      RECURSIVE SUBROUTINE ADMBCS4(DIR,ICOD ,ISK  ,N    ,IXC    ,
     .                          IPARTC,IPART,ICODE,ISKEW,SH4TREE)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "scr17_c.inc"
#include      "param_c.inc"
#include      "remesh_c.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER DIR,ICOD,ISK,N,IXC(NIXC,*),
     .        IPARTC(*),IPART(LIPART1,*),ICODE(*),ISKEW(*),
     .        SH4TREE(KSH4TREE,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER LEVEL,IP,NLEV
      INTEGER SON,M1,M2
C-----------------------------------------------
      INTEGER MY_OR
      EXTERNAL MY_OR
C-----------------------------------------------
      LEVEL=SH4TREE(3,N)
      IF(LEVEL<0)THEN
        LEVEL=-(LEVEL+1)
      END IF
      IP   =IPARTC(N)
      NLEV =IPART(10,IP)

      IF(LEVEL<NLEV)THEN
        SON=SH4TREE(2,N)+DIR
        CALL ADMBCS4(DIR,ICOD,ISK,SON,IXC,
     .              IPARTC,IPART,ICODE,ISKEW,SH4TREE)
        SON=SH4TREE(2,N)+MOD(DIR+1,4)
        CALL ADMBCS4(DIR,ICOD,ISK,SON,IXC,
     .              IPARTC,IPART,ICODE,ISKEW,SH4TREE)
      ELSE
        M1=IXC(DIR+2,N)
        M2=IXC(MOD(DIR+1,4)+2,N)
        ICODE(M1)=MY_OR(ICOD,ICODE(M1))
        ICODE(M2)=MY_OR(ICOD,ICODE(M2))
        ISKEW(M1)=ISK
        ISKEW(M2)=ISK
      END IF

      RETURN
      END
      RECURSIVE SUBROUTINE ADMBCS3(DIR,ICOD,ISK,N,IXTG,
     .                         IPARTTG,IPART,ICODE,ISKEW,SH3TREE)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "scr17_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "remesh_c.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER DIR,ICOD,ISK,N,IXTG(NIXTG,*),
     .        IPARTTG(*),IPART(LIPART1,*),ICODE(*),ISKEW(*),
     .        SH3TREE(KSH3TREE,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER LEVEL,IP,NLEV
      INTEGER SON,M1,M2,J
C-----------------------------------------------
      INTEGER MY_OR
      EXTERNAL MY_OR
C-----------------------------------------------
      LEVEL=SH3TREE(3,N)
      IF(LEVEL<0)THEN
        LEVEL=-(LEVEL+1)
      END IF
      IP   =IPARTTG(N)
      NLEV =IPART(10,IP)

      IF(LEVEL<NLEV)THEN
        SON=SH3TREE(2,N)+DIR
        CALL ADMBCS3(DIR,ICOD,ISK,SON,IXTG,
     .              IPARTTG,IPART,ICODE,ISKEW,SH3TREE)
        SON=SH3TREE(2,N)+MOD(DIR+1,3)
        CALL ADMBCS3(DIR,ICOD,ISK,SON,IXTG,
     .              IPARTTG,IPART,ICODE,ISKEW,SH3TREE)
      ELSE
        M1=IXTG(DIR+2,N)
        M2=IXTG(MOD(DIR+1,3)+2,N)
        ICODE(M1)=MY_OR(ICOD,ICODE(M1))
        ICODE(M2)=MY_OR(ICOD,ICODE(M2))
        ISKEW(M1)=ISK
        ISKEW(M2)=ISK
      END IF

      RETURN
      END
