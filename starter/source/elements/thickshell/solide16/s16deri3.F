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
Chd|  S16DERI3                      source/elements/thickshell/solide16/s16deri3.F
Chd|-- called by -----------
Chd|        S16INIT3                      source/elements/thickshell/solide16/s16init3.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE S16DERI3(NGL,OFF,R,S,T    ,W     ,
     2   DNIDR ,DNIDS ,DNIDT ,DXDR  ,DYDR  ,DZDR  ,
     3   DXDS  ,DYDS  ,DZDS  ,DXDT  ,DYDT  ,DZDT  ,
     4   XX    ,YY    ,ZZ    ,PX    ,PY    ,PZ    ,
     5   VOL   ,DELTAX,KXX   ,NI    ,VOLG  ,UL    ,VOLDP)
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NGL(*)
C     REAL ou REAL*8
      my_real
     .   R,S,T,W,
     .   DNIDR(16),DNIDS(16),DNIDT(16),DXDR(*),DYDR(*),DZDR(*),
     .   DXDS(*),DYDS(*),DZDS(*),DXDT(*),DYDT(*),DZDT(*),
     .   DRDX(MVSIZ), DSDX(MVSIZ), DTDX(MVSIZ),
     .   DRDY(MVSIZ), DSDY(MVSIZ), DTDY(MVSIZ),
     .   DRDZ(MVSIZ), DSDZ(MVSIZ), DTDZ(MVSIZ),
     .   XX(MVSIZ,16),YY(MVSIZ,16),ZZ(MVSIZ,16),
     .   PX(MVSIZ,16),PY(MVSIZ,16),PZ(MVSIZ,16),
     .   VOL(*),OFF(*),DELTAX(*),VOLG(MVSIZ)  , 
     .   KXX(MVSIZ,16),NI(16),UL(MVSIZ,16)
      DOUBLE PRECISION
     .   VOLDP(*),DETDP
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,N
      my_real
     .   D, AA, BB, DET(MVSIZ),R9 ,R13 ,S9 ,S10 ,S11 ,S12 ,T10 ,T14
C-----------------------------------------------
C
C
C
C                                      ^ S               _ T
C                                      |                 /|
C                                      |                /
C                         7            |      14       /
C                          O-----------|-----O-----------------O 6
C                         /.           |             /        /|
C                        / .           |            /        / |
C                       /  .           |                    /  |
C                      /   .           |          /        /   |
C                     /    .           |         /        /    |
C                 15 O     .    *      +   *          *  O     |
C                   /      .           |       /        / 13   |
C                  /       .                  /        /       |
C                 /                    |     +        /        | 
C                /         .#          #    /     #  /         |
C               /          .    * 16   |   *        / *        |
C            8 O-----------------O-----------------O 5         |
C              |           .           | /         |           |
C              |        @  .       @          @    |           |
C     R <------|- - -+ - - -#- - - - - # - - - - -#|- - -+     |
C              |           .    *     /    *       |  *        |
C              |           . 3         |      10   |           |
C              |           O......../.. .....O.....|...........O 2
C              |        @ '        @   |      @    |          /
C              |         '  #     /    #          #|         /
C              |        '        +     |           |        / 
C              |       '                           |       /    
C              |   11 '                |           |      /    
C              |     O  @          @   +      @    |     O     
C              |    '                              |    / 9    
C              |   '                               |   /    
C              |  '                                |  /    
C              | '                                 | /    
C              |'                                  |/    
C              O-----------------O-----------------O    
C             4                 12                  1
C
C
C
C
C-----------------------------------------------
C
C
C                                      ^ S               _ T
C                                      |                 /|
C                                      |                /
C                                      |               /
C                        ( 7)==========|===(14)===============( 6)
C                        //|           |             /        //|
C                       // |           |            /        //||
C                      //  |           |                    // ||
C                     //   |           |          /        //  ||
C                    //    |           |         /        //   ||
C                  (15)    |    *- - - + - * - - - - -* (13)   ||
C                  //      |   /|      |  /|   /     /| //     ||
C                 //       |                  /        //      ||
C                //        | /  |      |/  | +     /  //       ||
C               //         |#- - - - - # - -/- - -#  //        ||
C              //          |    * - - /|- -*- - -/ -//*        ||
C            ( 8)===============(16)==============( 5)         ||
C             ||         / || / |   /  | / |   /  ||| |        ||
C             ||        @- | - - - @ - - - - -@    ||          ||
C     R <-----||- - -+ -|- -# - - -| - # - - -|- -#|| - -+     ||
C             ||           |    * - - /| - *- - -/-|| *        ||
C             ||        |  ||  /   |      /   |   |||/         ||
C             ||         ( 3)-------/--|---(10)----||---------( 2)
C             ||        @ /- / - - @ - -/ - - @    ||         //
C             ||        |/  #- - -/| - # - - -|- -#||        //
C             ||        /  /     +    /|         / ||       // 
C             ||       /|          |          |    ||      //    
C             ||      /  /          /  |       /   ||     //    
C             ||   (11) @- - - - - @ - + - - -@    ||   ( 9)    
C             ||    /                              ||   //     
C             ||   /                               ||  //    
C             ||  /                                || //    
C             || /                                 ||//    
C             ||/                                  ||/    
C            ( 4)===============(12)==============( 1)    
C                                                
C
C
C
C
C*/
C-----------------------------------------------
C
C         _ 
C        \      
C    f = /_ (fi * Ni)
C             _ 
C            \      
C    df/dx = /_ (fi * dNi/dx)
C
C    dNi/dx =  dNi/dr dr/dx + dNi/ds ds/dx + dNi/dt dt/dx
C
C-----------------------------------------------
C                _ 
C               \      
C    x(r,s,t) = /_ (xi * Ni(r,s,t))
C                _ 
C               \      
C    y(r,s,t) = /_ (yi * Ni(r,s,t))
C                _ 
C               \      
C    z(r,s,t) = /_ zi * Ni(r,s,t))
C        
C                _ 
C               \      
C    dx/dr    = /_ (xi * dNi/dr)
C    ...        
C
C          [dx/dr dy/dr dz/dr]            
C    [J] = |dx/ds dy/ds dz/ds|                
C          [dx/dt dy/dt dz/dt]               
C                                     
C    |dNi/dx|      -1  |dNi/dr|
C    {dNi/dy} = [J]    {dNi/ds}
C    |dNi/dz|          |dNi/dt|
C
C
C           [dNi/dx    0       0   ]
C           |  0     dNi/dy    0   |
C    [Bi] = |  0       0     dNi/dz|
C           |dNi/dy  dNi/dx    0   |
C           |  0     dNi/dz  dNi/dy|
C           [dNi/dz    0     dNi/dx]          
C                                            
C                                          
C            [dNi/dx    0       0     dNi/dy    0     dNi/dz]
C    [Bi]t = |  0     dNi/dy    0     dNi/dx  dNi/dz    0   |
C            [  0       0     dNi/dz    0     dNi/dy  dNi/dx]
C                                          
C          [D11 D12 D12  0   0   0 ]
C          |D12 D11 D12  0   0   0 |
C    [D] = |D12 D12 D11  0   0   0 |
C          | 0   0   0   G   0   0 |
C          | 0   0   0   0   G   0 |
C          [ 0   0   0   0   0   G ]
C
C                [D11*dNi/dx D12*dNi/dx D12*dNi/dx G*dNi/dy     0    G*dNi/dz]
C    [Bi]t [D] = |D12*dNi/dy D11*dNi/dy D12*dNi/dy G*dNi/dx G*dNi/dz     0   |
C                [D12*dNi/dz D12*dNi/dz D11*dNi/dz     0    G*dNi/dy G*dNi/dx]
C                                             _
C                                            /   t
C     eps = [B] u     sig = [D] eps    F = _/ [B]  sig dvol
C 
C                _
C               /    t
C     [Kij] = _/ [Bi]  [D] [Bj] dvol
C
C
C
C        [ D11*dNi/dx*dNj/dx  D12*dNi/dx*dNj/dy  D12*dNi/dx*dNj/dz ]
C        | + G*dNi/dy*dNj/dy  + G*dNi/dy*dNj/dx  + G*dNi/dz*dNj/dx |
C        | + G*dNi/dz*dNj/dz                                       |
C [Kij]  |                    D11*dNi/dy*dNj/dy  D12*dNi/dy*dNj/dz |
C ---- = | D12*dNi/dy*dNj/dx  + G*dNi/dx*dNj/dx  + G*dNi/dz*dNj/dy |
C Volp   | + G*dNi/dx*dNj/dy  + G*dNi/dz*dNj/dz                    |
C        |                                       D11*dNi/dz*dNj/dz |
C        | D12*dNi/dz*dNj/dx  D12*dNi/dz*dNj/dy  + G*dNi/dy*dNj/dy |
C        [ + G*dNi/dx*dNj/dz  + G*dNi/dy*dNj/dz  + G*dNi/dx*dNj/dx ]
C
C-----------------------------------------------
C-----------------------------------------------------------------------
C     dx/dr; dx/ds; dx/dt
C-----------------------------------------------------------------------
      DO I=LFT,LLT
C
        DXDR(I) = DNIDR(1)*XX(I,1) + DNIDR(2)*XX(I,2) + DNIDR(3)*XX(I,3)
     +          + DNIDR(4)*XX(I,4) + DNIDR(5)*XX(I,5) + DNIDR(6)*XX(I,6)
     +          + DNIDR(7)*XX(I,7) + DNIDR(8)*XX(I,8)  
     +    + DNIDR(9)*(XX(I,9) - XX(I,11)) + DNIDR(10)*XX(I,10)
     +    + DNIDR(12)*XX(I,12) + DNIDR(13)*(XX(I,13) - XX(I,15))
     +    + DNIDR(14)*XX(I,14) + DNIDR(16)*XX(I,16)
C  
        DXDS(I) = DNIDS(1)*XX(I,1) + DNIDS(2)*XX(I,2) + DNIDS(3)*XX(I,3)
     +          + DNIDS(4)*XX(I,4) + DNIDS(5)*XX(I,5) + DNIDS(6)*XX(I,6)
     +          + DNIDS(7)*XX(I,7) + DNIDS(8)*XX(I,8)  
     +    + DNIDS(9)* (XX(I,9)  - XX(I,13))
     +    + DNIDS(10)*(XX(I,10) - XX(I,14)) 
     +    + DNIDS(11)*(XX(I,11) - XX(I,15))
     +    + DNIDS(12)*(XX(I,12) - XX(I,16)) 
C 
        DXDT(I) = DNIDT(1)*XX(I,1) + DNIDT(2)*XX(I,2) + DNIDT(3)*XX(I,3)
     +          + DNIDT(4)*XX(I,4) + DNIDT(5)*XX(I,5) + DNIDT(6)*XX(I,6)
     +          + DNIDT(7)*XX(I,7) + DNIDT(8)*XX(I,8)  
     +    + DNIDT(9)*XX(I,9)   + DNIDT(10)*(XX(I,10) - XX(I,12))
     +    + DNIDT(11)*XX(I,11) + DNIDT(13)*XX(I,13)
     +    + DNIDT(14)*(XX(I,14) - XX(I,16)) + DNIDT(15)*XX(I,15) 
C-----------------------------------------------------------------------
C     dy/dr; dy/ds; dy/dt
C-----------------------------------------------------------------------
        DYDR(I) = DNIDR(1)*YY(I,1) + DNIDR(2)*YY(I,2) + DNIDR(3)*YY(I,3)
     +          + DNIDR(4)*YY(I,4) + DNIDR(5)*YY(I,5) + DNIDR(6)*YY(I,6)
     +          + DNIDR(7)*YY(I,7) + DNIDR(8)*YY(I,8)  
     +    + DNIDR(9)*(YY(I,9) - YY(I,11))   + DNIDR(10)*YY(I,10)
     +    + DNIDR(12)*YY(I,12) + DNIDR(13)*(YY(I,13) - YY(I,15))
     +    + DNIDR(14)*YY(I,14) + DNIDR(16)*YY(I,16)
C  
        DYDS(I) = DNIDS(1)*YY(I,1) + DNIDS(2)*YY(I,2) + DNIDS(3)*YY(I,3)
     +          + DNIDS(4)*YY(I,4) + DNIDS(5)*YY(I,5) + DNIDS(6)*YY(I,6)
     +          + DNIDS(7)*YY(I,7) + DNIDS(8)*YY(I,8)  
     +    + DNIDS(9)* (YY(I,9)  - YY(I,13))   
     +    + DNIDS(10)*(YY(I,10) - YY(I,14)) 
     +    + DNIDS(11)*(YY(I,11) - YY(I,15))
     +    + DNIDS(12)*(YY(I,12) - YY(I,16))
C 
        DYDT(I) = DNIDT(1)*YY(I,1) + DNIDT(2)*YY(I,2) + DNIDT(3)*YY(I,3)
     +          + DNIDT(4)*YY(I,4) + DNIDT(5)*YY(I,5) + DNIDT(6)*YY(I,6)
     +          + DNIDT(7)*YY(I,7) + DNIDT(8)*YY(I,8)  
     +    + DNIDT(9)*YY(I,9)   + DNIDT(10)*(YY(I,10) - YY(I,12))
     +    + DNIDT(11)*YY(I,11) + DNIDT(13)*YY(I,13)
     +    + DNIDT(14)*(YY(I,14) - YY(I,16)) + DNIDT(15)*YY(I,15) 
C-----------------------------------------------------------------------
C     dz/dr; dz/ds; dz/dt
C-----------------------------------------------------------------------
        DZDR(I) = DNIDR(1)*ZZ(I,1) + DNIDR(2)*ZZ(I,2) + DNIDR(3)*ZZ(I,3)
     +          + DNIDR(4)*ZZ(I,4) + DNIDR(5)*ZZ(I,5) + DNIDR(6)*ZZ(I,6)
     +          + DNIDR(7)*ZZ(I,7) + DNIDR(8)*ZZ(I,8)  
     +    + DNIDR(9)*(ZZ(I,9) - ZZ(I,11))   + DNIDR(10)*ZZ(I,10)
     +    + DNIDR(12)*ZZ(I,12) + DNIDR(13)*(ZZ(I,13) - ZZ(I,15))
     +    + DNIDR(14)*ZZ(I,14) + DNIDR(16)*ZZ(I,16)
C  
        DZDS(I) = DNIDS(1)*ZZ(I,1) + DNIDS(2)*ZZ(I,2) + DNIDS(3)*ZZ(I,3)
     +          + DNIDS(4)*ZZ(I,4) + DNIDS(5)*ZZ(I,5) + DNIDS(6)*ZZ(I,6)
     +          + DNIDS(7)*ZZ(I,7) + DNIDS(8)*ZZ(I,8)  
     +    + DNIDS(9)* (ZZ(I,9)  - ZZ(I,13))
     +    + DNIDS(10)*(ZZ(I,10) - ZZ(I,14))
     +    + DNIDS(11)*(ZZ(I,11) - ZZ(I,15))
     +    + DNIDS(12)*(ZZ(I,12) - ZZ(I,16)) 
C 
        DZDT(I) = DNIDT(1)*ZZ(I,1) + DNIDT(2)*ZZ(I,2) + DNIDT(3)*ZZ(I,3)
     +          + DNIDT(4)*ZZ(I,4) + DNIDT(5)*ZZ(I,5) + DNIDT(6)*ZZ(I,6)
     +          + DNIDT(7)*ZZ(I,7) + DNIDT(8)*ZZ(I,8)  
     +    + DNIDT(9)*ZZ(I,9)   + DNIDT(10)*(ZZ(I,10) - ZZ(I,12))
     +    + DNIDT(11)*ZZ(I,11) + DNIDT(13)*ZZ(I,13)
     +    + DNIDT(14)*(ZZ(I,14) - ZZ(I,16)) + DNIDT(15)*ZZ(I,15) 
c         print *,'dx y z/dr'
c         print *,DXDR(1),DYDR(1),DZDR(1)
c         print *,'dx y z/ds'
c         print *,DXDS(1),DYDS(1),DZDS(1)
c         print *,'dx y z/dt='
c         print *,DXDT(1),DYDT(1),DZDT(1)
C-----------------------------------------------------------------------
C          -1
C       [J]          Inversion du jacobien
C-----------------------------------------------------------------------
        DRDX(I)=DYDS(I)*DZDT(I)-DZDS(I)*DYDT(I)
        DRDY(I)=DZDS(I)*DXDT(I)-DXDS(I)*DZDT(I)
        DRDZ(I)=DXDS(I)*DYDT(I)-DYDS(I)*DXDT(I)
C
        DSDZ(I)=DXDT(I)*DYDR(I)-DYDT(I)*DXDR(I)
        DSDY(I)=DZDT(I)*DXDR(I)-DXDT(I)*DZDR(I)
        DSDX(I)=DYDT(I)*DZDR(I)-DZDT(I)*DYDR(I)
C
        DTDX(I)=DYDR(I)*DZDS(I)-DZDR(I)*DYDS(I)
        DTDY(I)=DZDR(I)*DXDS(I)-DXDR(I)*DZDS(I)
        DTDZ(I)=DXDR(I)*DYDS(I)-DYDR(I)*DXDS(I)
C
        DETDP = DXDR(I) * DRDX(I)
     .         + DYDR(I) * DRDY(I)
     .         + DZDR(I) * DRDZ(I)
        DET(I) = DETDP
        VOLDP(I) = W * DETDP
        VOL(I) = VOLDP(I)
C
        IF(VOL(I).LE.ZERO)THEN
         CALL ANCMSG(MSGID=483,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO,
     .               I1=NGL(I))
        ENDIF
      ENDDO
C
C      IF(IDTMIN(1).EQ.1)THEN
C ...
C      ENDIF
C
      DO I=LFT,LLT
C-----------------------------------------------------------------------
C          -1            Inversion du jacobien suite
C       [J]              et repere local r,s,t
C-----------------------------------------------------------------------
        D = 1./DET(I)
        DRDX(I)=D*DRDX(I)
        DSDX(I)=D*DSDX(I)
        DTDX(I)=D*DTDX(I)
C
        DRDY(I)=D*DRDY(I)
        DSDY(I)=D*DSDY(I)
        DTDY(I)=D*DTDY(I)
C
        DRDZ(I)=D*DRDZ(I)
        DSDZ(I)=D*DSDZ(I)
        DTDZ(I)=D*DTDZ(I)
C-----------------------------------------------------------------------
C       |dNi/dx|      -1  |dNi/dr|
C       {dNi/dy} = [J]    {dNi/ds}
C       |dNi/dz|          |dNi/dt|
C-----------------------------------------------------------------------
        PX(I,1) = DNIDR(1)*DRDX(I) + DNIDS(1)*DSDX(I) + DNIDT(1)*DTDX(I)  
        PX(I,2) = DNIDR(2)*DRDX(I) + DNIDS(2)*DSDX(I) + DNIDT(2)*DTDX(I)  
        PX(I,3) = DNIDR(3)*DRDX(I) + DNIDS(3)*DSDX(I) + DNIDT(3)*DTDX(I)  
        PX(I,4) = DNIDR(4)*DRDX(I) + DNIDS(4)*DSDX(I) + DNIDT(4)*DTDX(I)  
        PX(I,5) = DNIDR(5)*DRDX(I) + DNIDS(5)*DSDX(I) + DNIDT(5)*DTDX(I)  
        PX(I,6) = DNIDR(6)*DRDX(I) + DNIDS(6)*DSDX(I) + DNIDT(6)*DTDX(I)  
        PX(I,7) = DNIDR(7)*DRDX(I) + DNIDS(7)*DSDX(I) + DNIDT(7)*DTDX(I)  
        PX(I,8) = DNIDR(8)*DRDX(I) + DNIDS(8)*DSDX(I) + DNIDT(8)*DTDX(I)
	R9  = DNIDR(9) *DRDX(I)  
	R13 = DNIDR(13)*DRDX(I)  
	S9  = DNIDS(9) *DSDX(I)  
	S10 = DNIDS(10)*DSDX(I)  
	S11 = DNIDS(11)*DSDX(I)  
	S12 = DNIDS(12)*DSDX(I)  
	T10 = DNIDT(10)*DTDX(I)  
	T14 = DNIDT(14)*DTDX(I) 
        PX(I,9) = R9               + S9  + DNIDT(9)*DTDX(I)  
        PX(I,10)= DNIDR(10)*DRDX(I)+ S10 + T10  
        PX(I,11)=-R9               + S11 + DNIDT(11)*DTDX(I)  
        PX(I,12)= DNIDR(12)*DRDX(I)+ S12 - T10  
        PX(I,13)= R13              - S9  + DNIDT(13)*DTDX(I)  
        PX(I,14)= DNIDR(14)*DRDX(I)- S10 + T14  
        PX(I,15)=-R13              - S11 + DNIDT(15)*DTDX(I)  
        PX(I,16)= DNIDR(16)*DRDX(I)- S12 - T14  
C
        PY(I,1) = DNIDR(1)*DRDY(I) + DNIDS(1)*DSDY(I) + DNIDT(1)*DTDY(I)  
        PZ(I,1) = DNIDR(1)*DRDZ(I) + DNIDS(1)*DSDZ(I) + DNIDT(1)*DTDZ(I)  
        PY(I,2) = DNIDR(2)*DRDY(I) + DNIDS(2)*DSDY(I) + DNIDT(2)*DTDY(I)  
        PY(I,3) = DNIDR(3)*DRDY(I) + DNIDS(3)*DSDY(I) + DNIDT(3)*DTDY(I)  
        PY(I,4) = DNIDR(4)*DRDY(I) + DNIDS(4)*DSDY(I) + DNIDT(4)*DTDY(I)  
        PY(I,5) = DNIDR(5)*DRDY(I) + DNIDS(5)*DSDY(I) + DNIDT(5)*DTDY(I)  
        PY(I,6) = DNIDR(6)*DRDY(I) + DNIDS(6)*DSDY(I) + DNIDT(6)*DTDY(I)  
        PY(I,7) = DNIDR(7)*DRDY(I) + DNIDS(7)*DSDY(I) + DNIDT(7)*DTDY(I)  
        PY(I,8) = DNIDR(8)*DRDY(I) + DNIDS(8)*DSDY(I) + DNIDT(8)*DTDY(I)  
	R9  = DNIDR(9) *DRDY(I)  
	R13 = DNIDR(13)*DRDY(I)  
	S9  = DNIDS(9) *DSDY(I)  
	S10 = DNIDS(10)*DSDY(I)  
	S11 = DNIDS(11)*DSDY(I)  
	S12 = DNIDS(12)*DSDY(I)  
	T10 = DNIDT(10)*DTDY(I)  
	T14 = DNIDT(14)*DTDY(I) 
        PY(I,9) = R9               + S9  + DNIDT(9)*DTDY(I)  
        PY(I,10)= DNIDR(10)*DRDY(I)+ S10 + T10  
        PY(I,11)=-R9               + S11 + DNIDT(11)*DTDY(I)  
        PY(I,12)= DNIDR(12)*DRDY(I)+ S12 - T10 
        PY(I,13)= R13              - S9  + DNIDT(13)*DTDY(I)  
        PY(I,14)= DNIDR(14)*DRDY(I)- S10 + T14  
        PY(I,15)=-R13              - S11 + DNIDT(15)*DTDY(I)  
        PY(I,16)= DNIDR(16)*DRDY(I)- S12 - T14  
C
        PZ(I,1) = DNIDR(1)*DRDZ(I) + DNIDS(1)*DSDZ(I) + DNIDT(1)*DTDZ(I)  
        PZ(I,2) = DNIDR(2)*DRDZ(I) + DNIDS(2)*DSDZ(I) + DNIDT(2)*DTDZ(I)  
        PZ(I,3) = DNIDR(3)*DRDZ(I) + DNIDS(3)*DSDZ(I) + DNIDT(3)*DTDZ(I)  
        PZ(I,4) = DNIDR(4)*DRDZ(I) + DNIDS(4)*DSDZ(I) + DNIDT(4)*DTDZ(I)  
        PZ(I,5) = DNIDR(5)*DRDZ(I) + DNIDS(5)*DSDZ(I) + DNIDT(5)*DTDZ(I)  
        PZ(I,6) = DNIDR(6)*DRDZ(I) + DNIDS(6)*DSDZ(I) + DNIDT(6)*DTDZ(I)  
        PZ(I,7) = DNIDR(7)*DRDZ(I) + DNIDS(7)*DSDZ(I) + DNIDT(7)*DTDZ(I)  
        PZ(I,8) = DNIDR(8)*DRDZ(I) + DNIDS(8)*DSDZ(I) + DNIDT(8)*DTDZ(I)  
	R9  = DNIDR(9) *DRDZ(I)  
	R13 = DNIDR(13)*DRDZ(I)  
	S9  = DNIDS(9) *DSDZ(I)  
	S10 = DNIDS(10)*DSDZ(I)  
	S11 = DNIDS(11)*DSDZ(I)  
	S12 = DNIDS(12)*DSDZ(I)  
	T10 = DNIDT(10)*DTDZ(I)  
	T14 = DNIDT(14)*DTDZ(I) 
        PZ(I,9) = R9               + S9  + DNIDT(9)*DTDZ(I)  
        PZ(I,10)= DNIDR(10)*DRDZ(I)+ S10 + T10  
        PZ(I,11)=-R9               + S11 + DNIDT(11)*DTDZ(I)  
        PZ(I,12)= DNIDR(12)*DRDZ(I)+ S12 - T10  
        PZ(I,13)= R13              - S9  + DNIDT(13)*DTDZ(I)  
        PZ(I,14)= DNIDR(14)*DRDZ(I)- S10 + T14  
        PZ(I,15)=-R13              - S11 + DNIDT(15)*DTDZ(I)  
        PZ(I,16)= DNIDR(16)*DRDZ(I)- S12 - T14 
C
      ENDDO
C
C
C
      DO N=1,16
        DO I=LFT,LLT
          KXX(I,N) = VOL(I)*
     .     (PX(I,N)*PX(I,N) + PY(I,N)*PY(I,N) + PZ(I,N)*PZ(I,N))
          UL(I,N) = UL(I,N) + KXX(I,N)
        ENDDO
      ENDDO
      DO I=LFT,LLT
        VOLG(I) =VOLG(I) + VOL(I)
      ENDDO
C
c      AA = 0
c      DO N=1,16
c        AA = AA + NI(N)*NI(N)
c      ENDDO
c      DO I=LFT,LLT
c        BB = 1.E20
c        DO N=1,16
c          BB = MIN(BB,NI(N)*NI(N)/UL(I,N))
c        ENDDO
c        DELTAX(I) = SQRT(2.*BB/AA)
c      ENDDO
C
C
 1000 FORMAT(/' ZERO OR NEGATIVE VOLUME : 3D-ELEMENT NB',I10/)
 2000 FORMAT(/' ZERO OR NEGATIVE VOLUME : DELETE 3D-ELEMENT NB',I10/)
C
      RETURN
      END

Chd|====================================================================
Chd|  S16RST                        source/elements/thickshell/solide16/s16deri3.F
Chd|-- called by -----------
Chd|        S16INIT3                      source/elements/thickshell/solide16/s16init3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE S16RST(R,S,T,
     2   NI   ,DNIDR ,DNIDS ,DNIDT )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real
     1   R,S,T,
     2   NI(16) ,DNIDR(16),DNIDS(16),DNIDT(16)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
      my_real
     .  U_M_R,U_P_R,U_M_S,U_P_S,U_M_T,U_P_T,
     .  UMS_UMT,UMS_UPT,UPS_UMT,UPS_UPT,
     .  UMR_UMS,UMR_UPS,UPR_UMS,UPR_UPS,
     .  UMT_UMR,UMT_UPR,UPT_UMR,UPT_UPR,
     .  A
C-----------------------------------------------
C
C
C
C                            ^ S       _ T
C                            |         /|
C                            |        /
C                   7        | 14    /
C                    O-------|--O----------O 6
C                   /.       |     /      /|
C                  / .       |           / |
C              15 O  .           /   13 O  |
C                /   .       |         /   |
C               /    .    16   /    5 /    |
C            8 O----------O----------O     |
C     R <------|- - -.- - - -+  10   |     |
C              |     O..........O....|.....O 2
C              |    . 3              |    / 
C              |   .                 |   /    
C              |  O 11               |  O     
C              | .                   | / 9    
C              |.                    |/    
C              O----------O----------O     
C             4          12           1
C
C
C
C
C-----------------------------------------------
C ro = r ri    so = s si    to = t ti
C
C i=1,8  
C    ri=+-1  si=+-1 ti=+-1
C    Ni = 1/8 (1+ro)(1+so)(1+to)(ro+to-1)
C    dNi/dr = ri/8 (1+so)(1+to)(2ro+to)
C    dNi/ds = si/8 (1+to)(1+ro)(to+ro-1)
C    dNi/dt = ti/8 (1+ro)(1+so)(2to+ro)
C
C i=10;12;14;16
C    ri=0    si=+-1 ti=+-1
C    Ni = 1/4 (1-r^2)(1+so)(1+to)
C    dNi/dr = -r/2 (1+so)(1+to)
C    dNi/ds = si/4 (1-r^2)(1+to)
C    dNi/dt = ti/4 (1-r^2)(1+so)
C
C
C i=9;11;13;15
C    ri=+-1  si=+-1 ti=0
C    Ni = 1/4 (1-t^2)(1+ro)(1+so)
C    dNi/dr = ri/4 (1-t^2)(1+so)
C    dNi/ds = si/4 (1-t^2)(1+ro)
C    dNi/dt = -t/2 (1+ro)(1+so)
C-----------------------------------------------
C  i    ri   si   ti       Ni                                  
C--------------------------------------------------------------------
C  1   -1   -1   -1     1/8(1-r)(1-s)(1-t)(-r-t-1)
C  2   -1   -1   +1     1/8(1-r)(1-s)(1+t)(-r+t-1)
C  3   +1   -1   +1     1/8(1+r)(1-s)(1+t)(+r+t-1)
C  4   +1   -1   -1     1/8(1+r)(1-s)(1-t)(+r-t-1)
C  5   -1   +1   -1     1/8(1-r)(1+s)(1-t)(-r-t-1)
C  6   -1   +1   +1     1/8(1-r)(1+s)(1+t)(-r+t-1)
C  7   +1   +1   +1     1/8(1+r)(1+s)(1+t)(+r+t-1)    
C  8   +1   +1   -1     1/8(1+r)(1+s)(1-t)(+r-t-1)
C  9   -1   -1    0     1/4(1-t^2)(1-r)(1-s)
C 10    0   -1   +1     1/4(1-r^2)(1-s)(1+t)
C 11   +1   -1    0     1/4(1-t^2)(1+r)(1-s)
C 12    0   -1   -1     1/4(1-r^2)(1-s)(1-t)
C 13   -1   +1    0     1/4(1-t^2)(1-r)(1+s)
C 14    0   +1   +1     1/4(1-r^2)(1+s)(1+t)
C 15   +1   +1    0     1/4(1-t^2)(1+r)(1+s)
C 16    0   +1   -1     1/4(1-r^2)(1+s)(1-t)
C-----------------------------------------------
C  i    ri   si   ti       dNi/dr                  dNi/ds                               
C--------------------------------------------------------------------
C  1   -1   -1   -1     -1/8(1-s)(1-t)(-2r-t)     -1/8 (1-t)(1-r)(-t-r-1)
C  2   -1   -1   +1     -1/8(1-s)(1+t)(-2r+t)     -1/8 (1+t)(1-r)( t-r-1)
C  3   +1   -1   +1      1/8(1-s)(1+t)(+2r+t)     -1/8 (1+t)(1+r)( t+r-1)
C  4   +1   -1   -1      1/8(1-s)(1-t)(+2r-t)     -1/8 (1-t)(1+r)(-t+r-1)
C  5   -1   +1   -1     -1/8(1+s)(1-t)(-2r-t)      1/8 (1-t)(1-r)(-t-r-1)
C  6   -1   +1   +1     -1/8(1+s)(1+t)(-2r+t)      1/8 (1+t)(1-r)( t-r-1)
C  7   +1   +1   +1      1/8(1+s)(1+t)(+2r+t)      1/8 (1+t)(1+r)( t+r-1)   
C  8   +1   +1   -1      1/8(1+s)(1-t)(+2r-t)      1/8 (1-t)(1+r)(-t+r-1)
C  9   -1   -1    0     -1/4 (1-t^2)(1-s)         -1/4 (1-t^2)(1-r)
C 10    0   -1   +1     -1/2 r(1-s)(1+t)          -1/4 (1-r^2)(1+t)
C 11   +1   -1    0      1/4 (1-t^2)(1-s)         -1/4 (1-t^2)(1+r)
C 12    0   -1   -1     -1/2 r(1-s)(1-t)          -1/4 (1-r^2)(1-t)
C 13   -1   +1    0     -1/4 (1-t^2)(1+s)          1/4 (1-t^2)(1-r)
C 14    0   +1   +1     -1/2 r(1+s)(1+t)           1/4 (1-r^2)(1+t)
C 15   +1   +1    0      1/4 (1-t^2)(1+s)          1/4 (1-t^2)(1+r)
C 16    0   +1   -1     -1/2 r(1+s)(1-t)           1/4 (1-r^2)(1-t)
C-----------------------------------------------
C
       U_M_R = HALF*(1.-R)
       U_P_R = HALF*(1.+R)
      
C
      U_M_S = HALF*(1.-S)
      U_P_S = HALF*(1.+S)
C
      U_M_T = HALF*(1.-T)
      U_P_T = HALF*(1.+T)
      UMS_UMT = U_M_S * U_M_T
      UMS_UPT = U_M_S * U_P_T
      UPS_UMT = U_P_S * U_M_T
      UPS_UPT = U_P_S * U_P_T
C
      UMR_UMS = U_M_R * U_M_S
      UMR_UPS = U_M_R * U_P_S
      UPR_UMS = U_P_R * U_M_S
      UPR_UPS = U_P_R * U_P_S
C
      UMT_UMR = U_M_T * U_M_R
      UMT_UPR = U_M_T * U_P_R
      UPT_UMR = U_P_T * U_M_R
      UPT_UPR = U_P_T * U_P_R
C
      NI(1) = U_M_R * UMS_UMT * (-R-T-1.)
      NI(2) = U_M_R * UMS_UPT * (-R+T-1.)
      NI(3) = U_P_R * UMS_UPT * ( R+T-1.)
      NI(4) = U_P_R * UMS_UMT * ( R-T-1.)
      NI(5) = U_M_R * UPS_UMT * (-R-T-1.)
      NI(6) = U_M_R * UPS_UPT * (-R+T-1.)
      NI(7) = U_P_R * UPS_UPT * ( R+T-1.)
      NI(8) = U_P_R * UPS_UMT * ( R-T-1.)
C
C
C
      DNIDR(1) = -UMS_UMT * (-HALF*T - R) 
      DNIDR(2) = -UMS_UPT * ( HALF*T - R)
      DNIDR(3) =  UMS_UPT * ( HALF*T + R)
      DNIDR(4) =  UMS_UMT * (-HALF*T + R)
      DNIDR(5) = -UPS_UMT * (-HALF*T - R)
      DNIDR(6) = -UPS_UPT * ( HALF*T - R)
      DNIDR(7) =  UPS_UPT * ( HALF*T + R)
      DNIDR(8) =  UPS_UMT * (-HALF*T + R)
C
      DNIDS(1) = -UMT_UMR * (-R-T-ONE)*HALF
      DNIDS(2) = -UPT_UMR * (-R+T-ONE)*HALF
      DNIDS(3) = -UPT_UPR * ( R+T-ONE)*HALF
      DNIDS(4) = -UMT_UPR * ( R-T-ONE)*HALF
      DNIDS(5) =  UMT_UMR * (-R-T-ONE)*HALF
      DNIDS(6) =  UPT_UMR * (-R+T-ONE)*HALF
      DNIDS(7) =  UPT_UPR * ( R+T-ONE)*HALF
      DNIDS(8) =  UMT_UPR * ( R-T-ONE)*HALF
C
      DNIDT(1) = -UMR_UMS * (-HALF*R - T)
      DNIDT(2) =  UMR_UMS * (-HALF*R + T)
      DNIDT(3) =  UPR_UMS * (+HALF*R + T)
      DNIDT(4) = -UPR_UMS * (+HALF*R - T)
      DNIDT(5) = -UMR_UPS * (-HALF*R - T)
      DNIDT(6) =  UMR_UPS * (-HALF*R + T)
      DNIDT(7) =  UPR_UPS * (+HALF*R + T)
      DNIDT(8) = -UPR_UPS * (+HALF*R - T)      
C------------------------------------
      A = (ONE - R*R)
      NI(10) = A * UMS_UPT
      NI(12) = A * UMS_UMT
      NI(14) = A * UPS_UPT
      NI(16) = A * UPS_UMT
C    
      A = HALF*A
      DNIDT(10) =  A * U_M_S
      DNIDT(14) =  A * U_P_S
C      DNIDT(12) = -DNIDT(10)
C      DNIDT(16) = -DNIDT(14)
C    
C      DNIDS(14) =  A * U_P_T
C      DNIDS(16) =  A * U_M_T
C      DNIDS(10) = -DNIDS(14)
C      DNIDS(12) = -DNIDS(16)
      DNIDS(10) = -A * U_P_T
      DNIDS(12) = -A * U_M_T
C    
      A = -TWO*R
      DNIDR(10) = A * UMS_UPT
      DNIDR(12) = A * UMS_UMT
      DNIDR(14) = A * UPS_UPT
      DNIDR(16) = A * UPS_UMT
C------------------------------------
      A = (ONE - T*T)
      NI(9)  = A * UMR_UMS
      NI(11) = A * UPR_UMS
      NI(13) = A * UMR_UPS
      NI(15) = A * UPR_UPS
C
      A = HALF*A
C      DNIDR(11) =  A * U_M_S
C      DNIDR(15) =  A * U_P_S
C      DNIDR(9)  = -DNIDR(11)
C      DNIDR(13) = -DNIDR(15)
      DNIDR(9)  = -A * U_M_S
      DNIDR(13) = -A * U_P_S
C
C      DNIDS(13) =  A * U_M_R
C      DNIDS(15) =  A * U_P_R
C      DNIDS(9)  = -DNIDS(13)
C      DNIDS(11) = -DNIDS(15)
      DNIDS(9)  = -A * U_M_R
      DNIDS(11) = -A * U_P_R
C
      A = -TWO*T
      DNIDT(9)  = A * UMR_UMS
      DNIDT(11) = A * UPR_UMS
      DNIDT(13) = A * UMR_UPS
      DNIDT(15) = A * UPR_UPS
C
C-----------------------------------------------
      RETURN
      END
