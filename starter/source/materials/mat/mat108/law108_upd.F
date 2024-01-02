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
Chd|  LAW108_UPD                    source/materials/mat/mat108/law108_upd.F
Chd|-- called by -----------
Chd|        UPDMAT                        source/materials/updmat.F     
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        TABLE_MOD                     share/modules1/table_mod.F    
Chd|====================================================================
      SUBROUTINE LAW108_UPD(IOUT   ,TITR   ,UPARAM ,NPC    ,PLD    ,  
     .                     NFUNC  ,IFUNC  ,MAT_ID ,FUNC_ID,
     .                     PM     )
      USE MESSAGE_MOD
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE TABLE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com04_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      CHARACTER*nchartitle  :: TITR
      INTEGER MAT_ID,IOUT,NFUNC
      INTEGER NPC(*), FUNC_ID(*),IFUNC(NFUNC)
      my_real UPARAM(*),PLD(*),PM(NPROPM)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,K,FUNC,NUPAR,NPT, J,J1,NUPARAM,
     . IF1,IF2,IF3,IF4,IC1,IC2,II,JJ,LOAD,UNLOAD,
     . NP1,NP2,ILENG2,I7,I11,I13,I5,I4,K1,
     . K2,I14,IFUN
      my_real
     .   XK,  HARD,X1,X2,Y1,Y2,LSCALE,XK_INI,YFAC,
     .   X0,EMAX,DX,DY,Y0,DERI,H,XSCALE,ALPHA1,ALPHA2,
     .   S1,S2,T1,T2,TY,SX,XX1,YY1,DYDX,DTDS
      CHARACTER*nchartitle  :: TITR1
C=======================================================================
c     Transform FUNC_ID ->  Function number , leakmat only
cc
c---------------------------------------------------------------
c
c     traction X 
c
      I7  = 40  ! 4 + 6*6
      I11 = 64  ! 4 + 10*6
      I13 = 76  ! 4 + 12*6
      FUNC = IFUNC(1)
      LSCALE = UPARAM(I7 + 1)
      XK   = UPARAM(I11 + 1) 
      HARD = UPARAM(I13 + 1)
      XK_INI = XK
      IF (FUNC > 0 ) THEN     
        NPT=(NPC(FUNC+1)-NPC(FUNC))/2
        DO  J=2,NPT
           J1 =2*(J-2)
           X1 = PLD(NPC(FUNC)  + J1)
           Y1 = PLD(NPC(FUNC)  + J1 + 1)
           X2 = PLD(NPC(FUNC)  + J1 + 2)
           Y2 = PLD(NPC(FUNC)  + J1 + 3)
           XK = MAX(XK,LSCALE*(Y2 - Y1)/(X2 - X1))
        ENDDO
        IF(HARD/=0)THEN
            IF(XK_INI<XK)THEN
!!            CALL FRETITL2(TITR1,NOM_OPT(LNOPT1-LTITR+1,FUNC),LTITR)
            CALL ANCMSG(MSGID=1640, ! 
     .                  MSGTYPE=MSGWARNING,
     .                  ANMODE=ANINFO_BLIND_1,
     .                  I1=MAT_ID,
     .                  C1=TITR,
     .                  I2=NPC(NFUNCT+FUNC+1),
!!     .                  C2=TITR1,
     .                  R1=XK_INI,
     .                  R2=XK,
     .                  R3=XK)
            ENDIF
         ENDIF
         UPARAM(I11 + 1)= XK   
      ENDIF
!-----------------
      ! Traction Y
!-----------------   
      FUNC = IFUNC(2)
      LSCALE = UPARAM(I7 + 2)
      XK     = UPARAM(I11 + 2) 
      HARD   = UPARAM(I13 + 2)
      XK_INI = XK
      IF (FUNC > 0 ) THEN     
        NPT=(NPC(FUNC+1)-NPC(FUNC))/2
        DO  J=2,NPT
           J1 =2*(J-2)
           X1 = PLD(NPC(FUNC)  + J1)
           Y1 = PLD(NPC(FUNC)  + J1 + 1)
           X2 = PLD(NPC(FUNC)  + J1 + 2)
           Y2 = PLD(NPC(FUNC)  + J1 + 3)
           XK = MAX(XK,LSCALE*(Y2 - Y1)/(X2 - X1))
        ENDDO
        IF(HARD/=0)THEN
            IF(XK_INI<XK)THEN
!!            CALL FRETITL2(TITR1,NOM_OPT(LNOPT1-LTITR+1,FUNC),LTITR)
            CALL ANCMSG(MSGID=1640,  
     .                  MSGTYPE=MSGWARNING,
     .                  ANMODE=ANINFO_BLIND_1,
     .                  I1=MAT_ID,
     .                  C1=TITR,
     .                  I2=NPC(NFUNCT+FUNC+1),
!!     .                  C2=TITR1,
     .                  R1=XK_INI,
     .                  R2=XK,
     .                  R3=XK)
            ENDIF
         ENDIF
         UPARAM(I11 + 2) = XK   
      ENDIF
!-----------------
      ! Traction Z
!-----------------   
      FUNC = IFUNC(3)
      LSCALE = UPARAM(I7 +  3)
      XK     = UPARAM(I11 + 3) 
      HARD   = UPARAM(I13 + 3)
      XK_INI = XK
      IF (FUNC > 0 ) THEN     
        NPT=(NPC(FUNC+1)-NPC(FUNC))/2
        DO  J=2,NPT
           J1 =2*(J-2)
           X1 = PLD(NPC(FUNC)  + J1)
           Y1 = PLD(NPC(FUNC)  + J1 + 1)
           X2 = PLD(NPC(FUNC)  + J1 + 2)
           Y2 = PLD(NPC(FUNC)  + J1 + 3)
           XK = MAX(XK,LSCALE*(Y2 - Y1)/(X2 - X1))
        ENDDO
        IF(HARD/=0)THEN
            IF(XK_INI<XK)THEN
!!            CALL FRETITL2(TITR1,NOM_OPT(LNOPT1-LTITR+1,FUNC),LTITR)
            CALL ANCMSG(MSGID=1640, ! 
     .                  MSGTYPE=MSGWARNING,
     .                  ANMODE=ANINFO_BLIND_1,
     .                  I1=MAT_ID,
     .                  C1=TITR,
     .                  I2=NPC(NFUNCT+FUNC+1),
!!     .                  C2=TITR1,
     .                  R1=XK_INI,
     .                  R2=XK,
     .                  R3=XK)
            ENDIF
         ENDIF
         UPARAM(I11 + 3)= XK   
      ENDIF
!-----------------
      ! Torsion xx
!-----------------   
      FUNC = IFUNC(4)
      LSCALE = UPARAM(I7 + 4)
      XK     = UPARAM(I11 + 4) 
      HARD   = UPARAM(I13 + 4)
      XK_INI = XK
      IF (FUNC > 0 ) THEN     
        NPT=(NPC(FUNC+1)-NPC(FUNC))/2
        DO  J=2,NPT
           J1 =2*(J-2)
           X1 = PLD(NPC(FUNC)  + J1)
           Y1 = PLD(NPC(FUNC)  + J1 + 1)
           X2 = PLD(NPC(FUNC)  + J1 + 2)
           Y2 = PLD(NPC(FUNC)  + J1 + 3)
           XK = MAX(XK,LSCALE*(Y2 - Y1)/(X2 - X1))
        ENDDO
        IF(HARD/=0)THEN
            IF(XK_INI<XK)THEN
!!            CALL FRETITL2(TITR1,NOM_OPT(LNOPT1-LTITR+1,FUNC),LTITR)
            CALL ANCMSG(MSGID=1640, ! 
     .                  MSGTYPE=MSGWARNING,
     .                  ANMODE=ANINFO_BLIND_1,
     .                  I1=MAT_ID,
     .                  C1=TITR,
     .                  I2=NPC(NFUNCT+FUNC+1),
!!     .                  C2=TITR1,
     .                  R1=XK_INI,
     .                  R2=XK,
     .                  R3=XK)
            ENDIF
         ENDIF
         UPARAM(I11 + 4)= XK   
      ENDIF
!-----------------
      ! Rotation YY
!-----------------   
      FUNC = IFUNC(5)
      LSCALE = UPARAM(I7 + 5)
      XK     = UPARAM(I11 + 5) 
      HARD   = UPARAM(I13 + 5)
      XK_INI = XK
      IF (FUNC > 0 ) THEN     
        NPT=(NPC(FUNC+1)-NPC(FUNC))/2
        DO  J=2,NPT
           J1 =2*(J-2)
           X1 = PLD(NPC(FUNC)  + J1)
           Y1 = PLD(NPC(FUNC)  + J1 + 1)
           X2 = PLD(NPC(FUNC)  + J1 + 2)
           Y2 = PLD(NPC(FUNC)  + J1 + 3)
           XK = MAX(XK,LSCALE*(Y2 - Y1)/(X2 - X1))
        ENDDO
        IF(HARD/=0)THEN
            IF(XK_INI<XK)THEN
!!            CALL FRETITL2(TITR1,NOM_OPT(LNOPT1-LTITR+1,FUNC),LTITR)
            CALL ANCMSG(MSGID=1640, !  
     .                  MSGTYPE=MSGWARNING,
     .                  ANMODE=ANINFO_BLIND_1,
     .                  I1=MAT_ID,
     .                  C1=TITR,
     .                  I2=NPC(NFUNCT+FUNC+1),
!!     .                  C2=TITR1,
     .                  R1=XK_INI,
     .                  R2=XK,
     .                  R3=XK)

            ENDIF
         ENDIF
         UPARAM(I11  + 5)= XK   
      ENDIF
!-----------------
      ! Rotation ZZ
!-----------------   
      FUNC = IFUNC(6)
      LSCALE = UPARAM(I7 + 6)
      XK     = UPARAM(I11 + 6) 
      HARD   = UPARAM(I13 + 6)
      XK_INI = XK
      IF (FUNC > 0 ) THEN     
        NPT=(NPC(FUNC+1)-NPC(FUNC))/2
        DO  J=2,NPT
           J1 =2*(J-2)
           X1 = PLD(NPC(FUNC)  + J1)
           Y1 = PLD(NPC(FUNC)  + J1 + 1)
           X2 = PLD(NPC(FUNC)  + J1 + 2)
           Y2 = PLD(NPC(FUNC)  + J1 + 3)
           XK = MAX(XK,LSCALE*(Y2 - Y1)/(X2 - X1))
        ENDDO
        IF(HARD/=0)THEN
            IF(XK_INI<XK)THEN
!!            CALL FRETITL2(TITR1,NOM_OPT(LNOPT1-LTITR+1,FUNC),LTITR)
            CALL ANCMSG(MSGID=1640, !  
     .                  MSGTYPE=MSGWARNING,
     .                  ANMODE=ANINFO_BLIND_1,
     .                  I1=MAT_ID,
     .                  C1=TITR,
     .                  I2=NPC(NFUNCT+FUNC+1),
!!     .                  C2=TITR1,
     .                  R1=XK_INI,
     .                  R2=XK,
     .                  R3=XK)
            ENDIF
         ENDIF
         UPARAM(I11 + 6)= XK   
      ENDIF
C
   ! update of max slope
C   
c -------- 
!compute max slope for ifunc3
          I5  = 44 ! 4 + 4*10
          I7  = 40 ! 4 + 4*6
          I13 = 76 ! 4 + 12*6
          I14 = 82 ! 4 + 13*6
          IF1 = 0
          IF3 = 12
          IF4 = 18
          DO  J=1, 6
            YFAC  = UPARAM(I5 + J) ! 
            IFUN = IFUNC(IF4 + J) ! 
            IF (IFUN /= 0)THEN
              IC1 = NPC(IFUN)           
              IC2 = NPC(IFUN+1)         
              X0 = PLD(IC1)
              EMAX = ZERO
              DO II = IC1,IC2-4,2                   
                 JJ = II+2                          
                 DX = PLD(JJ)   - X0                
                 DY = PLD(JJ+1) - PLD(II+1)         
                 Y0 =  PLD(II+1)               
                 Y1 =  PLD(JJ+1)               
                 DERI = YFAC * DY / DX              
                 X1 =  PLD(JJ)                      
                 EMAX = MAX(EMAX, DERI)             
                 X0 =  PLD(JJ)                      
              ENDDO  
              UPARAM(I14+J) =  EMAX      
            ENDIF                        
          ENDDO
C
          DO 100 J=1, 6 
            H= UPARAM(I13 + J )      
            IF (H == 7)THEN
              XSCALE=UPARAM(7+J)
              LOAD  =IFUNC(IF1 + J)
              UNLOAD=IFUNC(IF3 + J)
              NP1  = (NPC(LOAD+1)-NPC(LOAD))*HALF
              NP2  = (NPC(UNLOAD+1)-NPC(UNLOAD))*HALF
              ALPHA1=ZERO  
              ALPHA2=ZERO             
c---
              DO JJ=2,NP1
                J1=2*(JJ-2)
                S1=PLD(NPC(LOAD)+J1)*XSCALE
                S2=PLD(NPC(LOAD)+J1+2)*XSCALE
                T1=PLD(NPC(LOAD)+J1+1)
                T2=PLD(NPC(LOAD)+J1+3)
                TY=ZERO
                SX=ZERO
                IF ( S1<=ZERO .AND.S2> ZERO)ALPHA1=(T2-T1)/(S2-S1)
                DO K=2,NP2
                 K1=2*(K-2)
                 XX1=PLD(NPC(UNLOAD)+K1)*XSCALE
                 X2=PLD(NPC(UNLOAD)+K1+2)*XSCALE
                 YY1=PLD(NPC(UNLOAD)+K1+1)
                 Y2=PLD(NPC(UNLOAD)+K1+3)
                 IF ( XX1<=ZERO .AND.X2> ZERO)ALPHA2=(Y2-YY1)/(X2-XX1)  
                 IF (Y2>=T1 .AND.YY1<=T2.AND.X2>=S1.AND.XX1<=S2)THEN
                   DYDX = (Y2-YY1) / (X2-XX1)
                   DTDS = (T2-T1) / (S2-S1)
                   IF (DYDX > DTDS) THEN
                     SX = (T1-YY1-DTDS*S1+DYDX*XX1) / (DYDX-DTDS)
                     TY =  T1 + DTDS*(SX - S1)
                   ENDIF
                   IF (TY/=ZERO .AND. SX/=ZERO )THEN  
                    IF (TY>=YY1.AND.TY<=Y2.AND.SX>=XX1.AND.SX<=X2
     .                      .AND.SX>=S2.AND.TY<=T2)THEN        
                      CALL ANCMSG(MSGID=982,
     .                            MSGTYPE=MSGERROR,
     .                            ANMODE=ANINFO_BLIND_1,
     .                            C1=TITR,
     .                            I1=UNLOAD,
     .                            I2=LOAD)
                     GOTO 100         
                    ENDIF     
                   ENDIF                                
                 ENDIF
                ENDDO
              ENDDO
              IF(ALPHA2>=ALPHA1)THEN
                CALL ANCMSG(MSGID=982,
     .                      MSGTYPE=MSGERROR,
     .                      ANMODE=ANINFO_BLIND_1,
     .                      C1=TITR,
     .                      I1=UNLOAD,
     .                      I2=LOAD)
              ENDIF
           ENDIF      
 100      CONTINUE           
c-----------
      RETURN
      END
