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
Chd|  S20DERI3                      source/elements/solid/solide20/s20deri3.F
Chd|-- called by -----------
Chd|        S20FORC3                      source/elements/solid/solide20/s20forc3.F
Chd|        S20KE3                        source/elements/solid/solide20/s20ke3.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE S20DERI3(
     1   NGL,     OFF,     R,       S,
     2   T,       W,       DNIDR,   DNIDS,
     3   DNIDT,   DXDR,    DYDR,    DZDR,
     4   DXDS,    DYDS,    DZDS,    DXDT,
     5   DYDT,    DZDT,    XX,      YY,
     6   ZZ,      PX,      PY,      PZ,
     7   VOL,     DELTAX,  KXX,     NI,
     8   VOLG,    UL,      IR,      IS,
     9   IT,      VOLDP,   NEL)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: NEL
      INTEGER NGL(*),IR,IS,IT
C     REAL
      my_real
     .   R,S,T,W,
     .   DNIDR(20),DNIDS(20),DNIDT(20),DXDR(*),DYDR(*),DZDR(*),
     .   DXDS(*),DYDS(*),DZDS(*),DXDT(*),DYDT(*),DZDT(*),
     .   DRDX(MVSIZ), DSDX(MVSIZ), DTDX(MVSIZ),
     .   DRDY(MVSIZ), DSDY(MVSIZ), DTDY(MVSIZ),
     .   DRDZ(MVSIZ), DSDZ(MVSIZ), DTDZ(MVSIZ),
     .   XX(MVSIZ,20),YY(MVSIZ,20),ZZ(MVSIZ,20),
     .   PX(MVSIZ,20),PY(MVSIZ,20),PZ(MVSIZ,20),
     .   VOL(*),OFF(*),DELTAX(*),VOLG(MVSIZ)  , 
     .   KXX(MVSIZ,20),NI(20),UL(MVSIZ,20)
      DOUBLE PRECISION
     .  VOLDP(MVSIZ),DETDP
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,N, ICOR
      my_real
     .   D, AA, BB, DET(MVSIZ)
C-----------------------------------------------
C
C
C
C                                      ^ S               _ T
C                                      |                 /|
C                                      |                /
C                         7            |      18       /
C                          O-----------|-----O-----------------O 6
C                         /.           |             /        /|
C                        / .           |            /        / |
C                       /  .           |                    /  |
C                      /   .           |          /        /   |
C                     /    .           |         /        /    |
C                 19 O     .    *      +   *          *  O     |
C                   /      .           |       /        / 17   |
C                  /       .                  /        /       |
C                 /     15 O           |     +        /        O 14
C                /         .#          #    /     #  /         |
C               /          .    * 20   |   *        / *        |
C            8 O-----------------O-----------------O 5         |
C              |           .           | /         |           |
C              |        @  .       @          @    |           |
C     R <------|- - -+ - - -#- - - - - # - - - - -#|- - -+     |
C              |           .    *     /    *       |  *        |
C              |           . 3         |      10   |           |
C              |           O......../.. .....O.....|...........O 2
C              |        @ '        @   |      @    |          /
C              |         '  #     /    #          #|         /
C           16 O        '        +     |           O 13     / 
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
C                        ( 7)==========|===(18)===============( 6)
C                        //|           |             /        //|
C                       // |           |            /        //||
C                      //  |           |                    // ||
C                     //   |           |          /        //  ||
C                    //    |           |         /        //   ||
C                  (19)    |    *- - - + - * - - - - -* (17)   ||
C                  //      |   /|      |  /|   /     /| //     ||
C                 //       |                  /        //      ||
C                //      (15)/  |      |/  | +     /  //      (14)
C               //         |#- - - - - # - -/- - -#  //        ||
C              //          |    * - - /|- -*- - -/ -//*        ||
C            ( 8)===============(20)==============( 5)         ||
C             ||         / || / |   /  | / |   /  ||| |        ||
C             ||        @- | - - - @ - - - - -@    ||          ||
C     R <-----||- - -+ -|- -# - - -| - # - - -|- -#|| - -+     ||
C             ||           |    * - - /| - *- - -/-|| *        ||
C             ||        |  ||  /   |      /   |   |||/         ||
C             ||         ( 3)-------/--|---(10)----||---------( 2)
C             ||        @ /- / - - @ - -/ - - @    ||         //
C             ||        |/  #- - -/| - # - - -|- -#||        //
C            (16)       /  /     +    /|         /(13)      // 
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
      DO I=1,NEL
C
        DXDR(I) = DNIDR(1)*XX(I,1) + DNIDR(2)*XX(I,2) + DNIDR(3)*XX(I,3)
     +          + DNIDR(4)*XX(I,4) + DNIDR(5)*XX(I,5) + DNIDR(6)*XX(I,6)
     +          + DNIDR(7)*XX(I,7) + DNIDR(8)*XX(I,8)  
     +    + DNIDR(9)*XX(I,9)   + DNIDR(10)*XX(I,10) + DNIDR(11)*XX(I,11)
     +    + DNIDR(12)*XX(I,12) + DNIDR(13)*XX(I,13) + DNIDR(14)*XX(I,14)
     +    + DNIDR(15)*XX(I,15) + DNIDR(16)*XX(I,16) + DNIDR(17)*XX(I,17)
     +    + DNIDR(18)*XX(I,18) + DNIDR(19)*XX(I,19) + DNIDR(20)*XX(I,20)
C  
        DXDS(I) = DNIDS(1)*XX(I,1) + DNIDS(2)*XX(I,2) + DNIDS(3)*XX(I,3)
     +          + DNIDS(4)*XX(I,4) + DNIDS(5)*XX(I,5) + DNIDS(6)*XX(I,6)
     +          + DNIDS(7)*XX(I,7) + DNIDS(8)*XX(I,8)  
     +    + DNIDS(9)*XX(I,9)   + DNIDS(10)*XX(I,10) + DNIDS(11)*XX(I,11)
     +    + DNIDS(12)*XX(I,12) + DNIDS(13)*XX(I,13) + DNIDS(14)*XX(I,14)
     +    + DNIDS(15)*XX(I,15) + DNIDS(16)*XX(I,16) + DNIDS(17)*XX(I,17)  
     +    + DNIDS(18)*XX(I,18) + DNIDS(19)*XX(I,19) + DNIDS(20)*XX(I,20) 
C 
        DXDT(I) = DNIDT(1)*XX(I,1) + DNIDT(2)*XX(I,2) + DNIDT(3)*XX(I,3)
     +          + DNIDT(4)*XX(I,4) + DNIDT(5)*XX(I,5) + DNIDT(6)*XX(I,6)
     +          + DNIDT(7)*XX(I,7) + DNIDT(8)*XX(I,8)  
     +    + DNIDT(9)*XX(I,9)   + DNIDT(10)*XX(I,10) + DNIDT(11)*XX(I,11)
     +    + DNIDT(12)*XX(I,12) + DNIDT(13)*XX(I,13) + DNIDT(14)*XX(I,14)
     +    + DNIDT(15)*XX(I,15) + DNIDT(16)*XX(I,16) + DNIDT(17)*XX(I,17)
     +    + DNIDT(18)*XX(I,18) + DNIDT(19)*XX(I,19) + DNIDT(20)*XX(I,20) 
C-----------------------------------------------------------------------
C     dy/dr; dy/ds; dy/dt
C-----------------------------------------------------------------------
        DYDR(I) = DNIDR(1)*YY(I,1) + DNIDR(2)*YY(I,2) + DNIDR(3)*YY(I,3)
     +          + DNIDR(4)*YY(I,4) + DNIDR(5)*YY(I,5) + DNIDR(6)*YY(I,6)
     +          + DNIDR(7)*YY(I,7) + DNIDR(8)*YY(I,8)  
     +    + DNIDR(9)*YY(I,9)   + DNIDR(10)*YY(I,10) + DNIDR(11)*YY(I,11)
     +    + DNIDR(12)*YY(I,12) + DNIDR(13)*YY(I,13) + DNIDR(14)*YY(I,14)
     +    + DNIDR(15)*YY(I,15) + DNIDR(16)*YY(I,16) + DNIDR(17)*YY(I,17)
     +    + DNIDR(18)*YY(I,18) + DNIDR(19)*YY(I,19) + DNIDR(20)*YY(I,20)
C  
        DYDS(I) = DNIDS(1)*YY(I,1) + DNIDS(2)*YY(I,2) + DNIDS(3)*YY(I,3)
     +          + DNIDS(4)*YY(I,4) + DNIDS(5)*YY(I,5) + DNIDS(6)*YY(I,6)
     +          + DNIDS(7)*YY(I,7) + DNIDS(8)*YY(I,8)  
     +    + DNIDS(9)*YY(I,9)   + DNIDS(10)*YY(I,10) + DNIDS(11)*YY(I,11)
     +    + DNIDS(12)*YY(I,12) + DNIDS(13)*YY(I,13) + DNIDS(14)*YY(I,14)
     +    + DNIDS(15)*YY(I,15) + DNIDS(16)*YY(I,16) + DNIDS(17)*YY(I,17)
     +    + DNIDS(18)*YY(I,18) + DNIDS(19)*YY(I,19) + DNIDS(20)*YY(I,20) 
C 
        DYDT(I) = DNIDT(1)*YY(I,1) + DNIDT(2)*YY(I,2) + DNIDT(3)*YY(I,3)
     +          + DNIDT(4)*YY(I,4) + DNIDT(5)*YY(I,5) + DNIDT(6)*YY(I,6)
     +          + DNIDT(7)*YY(I,7) + DNIDT(8)*YY(I,8)  
     +    + DNIDT(9)*YY(I,9)   + DNIDT(10)*YY(I,10) + DNIDT(11)*YY(I,11)
     +    + DNIDT(12)*YY(I,12) + DNIDT(13)*YY(I,13) + DNIDT(14)*YY(I,14)
     +    + DNIDT(15)*YY(I,15) + DNIDT(16)*YY(I,16) + DNIDT(17)*YY(I,17)
     +    + DNIDT(18)*YY(I,18) + DNIDT(19)*YY(I,19) + DNIDT(20)*YY(I,20) 
C-----------------------------------------------------------------------
C     dz/dr; dz/ds; dz/dt
C-----------------------------------------------------------------------
        DZDR(I) = DNIDR(1)*ZZ(I,1) + DNIDR(2)*ZZ(I,2) + DNIDR(3)*ZZ(I,3)
     +          + DNIDR(4)*ZZ(I,4) + DNIDR(5)*ZZ(I,5) + DNIDR(6)*ZZ(I,6)
     +          + DNIDR(7)*ZZ(I,7) + DNIDR(8)*ZZ(I,8)  
     +    + DNIDR(9)*ZZ(I,9)   + DNIDR(10)*ZZ(I,10) + DNIDR(11)*ZZ(I,11)
     +    + DNIDR(12)*ZZ(I,12) + DNIDR(13)*ZZ(I,13) + DNIDR(14)*ZZ(I,14)
     +    + DNIDR(15)*ZZ(I,15) + DNIDR(16)*ZZ(I,16) + DNIDR(17)*ZZ(I,17)
     +    + DNIDR(18)*ZZ(I,18) + DNIDR(19)*ZZ(I,19) + DNIDR(20)*ZZ(I,20)
C  
        DZDS(I) = DNIDS(1)*ZZ(I,1) + DNIDS(2)*ZZ(I,2) + DNIDS(3)*ZZ(I,3)
     +          + DNIDS(4)*ZZ(I,4) + DNIDS(5)*ZZ(I,5) + DNIDS(6)*ZZ(I,6)
     +          + DNIDS(7)*ZZ(I,7) + DNIDS(8)*ZZ(I,8)  
     +    + DNIDS(9)*ZZ(I,9)   + DNIDS(10)*ZZ(I,10) + DNIDS(11)*ZZ(I,11)
     +    + DNIDS(12)*ZZ(I,12) + DNIDS(13)*ZZ(I,13) + DNIDS(14)*ZZ(I,14)
     +    + DNIDS(15)*ZZ(I,15) + DNIDS(16)*ZZ(I,16) + DNIDS(17)*ZZ(I,17)
     +    + DNIDS(18)*ZZ(I,18) + DNIDS(19)*ZZ(I,19) + DNIDS(20)*ZZ(I,20) 
C 
        DZDT(I) = DNIDT(1)*ZZ(I,1) + DNIDT(2)*ZZ(I,2) + DNIDT(3)*ZZ(I,3)
     +          + DNIDT(4)*ZZ(I,4) + DNIDT(5)*ZZ(I,5) + DNIDT(6)*ZZ(I,6)
     +          + DNIDT(7)*ZZ(I,7) + DNIDT(8)*ZZ(I,8)  
     +    + DNIDT(9)*ZZ(I,9)   + DNIDT(10)*ZZ(I,10) + DNIDT(11)*ZZ(I,11)
     +    + DNIDT(12)*ZZ(I,12) + DNIDT(13)*ZZ(I,13) + DNIDT(14)*ZZ(I,14)
     +    + DNIDT(15)*ZZ(I,15) + DNIDT(16)*ZZ(I,16) + DNIDT(17)*ZZ(I,17)
     +    + DNIDT(18)*ZZ(I,18) + DNIDT(19)*ZZ(I,19) + DNIDT(20)*ZZ(I,20) 
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
      ENDDO
C
C      IF(IDTMIN(1)==1)THEN
C ...
C      ENDIF
C
C
      ICOR=0
      DO I=1,NEL
          IF(OFF(I)==ZERO)THEN
              VOL(I)=ONE
              VOLDP(I) = ONE
          ELSEIF(VOL(I)<=ZERO)THEN
              ICOR=1
          ENDIF
      ENDDO
      IF(ICOR/=0)THEN
       DO I=1,NEL
          IF(VOL(I)<=ZERO)THEN
            CALL ANCMSG(MSGID=171,ANMODE=ANINFO,
     .                  I1=NGL(I),I2=IR,I3=IS,I4=IT)
            CALL ARRET(2)
          ENDIF
       ENDDO
      ENDIF
C
      DO I=1,NEL
C-----------------------------------------------------------------------
C          -1            Inversion du jacobien suite
C       [J]              et repere local r,s,t
C-----------------------------------------------------------------------
        D = ONE/DET(I)
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
        PX(I,9) = DNIDR(9)*DRDX(I) + DNIDS(9)*DSDX(I) + DNIDT(9)*DTDX(I)  
        PX(I,10)= DNIDR(10)*DRDX(I)+ DNIDS(10)*DSDX(I)+DNIDT(10)*DTDX(I)  
        PX(I,11)= DNIDR(11)*DRDX(I)+ DNIDS(11)*DSDX(I)+DNIDT(11)*DTDX(I)  
        PX(I,12)= DNIDR(12)*DRDX(I)+ DNIDS(12)*DSDX(I)+DNIDT(12)*DTDX(I)  
        PX(I,13)= DNIDR(13)*DRDX(I)+ DNIDS(13)*DSDX(I)+DNIDT(13)*DTDX(I)  
        PX(I,14)= DNIDR(14)*DRDX(I)+ DNIDS(14)*DSDX(I)+DNIDT(14)*DTDX(I)  
        PX(I,15)= DNIDR(15)*DRDX(I)+ DNIDS(15)*DSDX(I)+DNIDT(15)*DTDX(I)  
        PX(I,16)= DNIDR(16)*DRDX(I)+ DNIDS(16)*DSDX(I)+DNIDT(16)*DTDX(I)  
        PX(I,17)= DNIDR(17)*DRDX(I)+ DNIDS(17)*DSDX(I)+DNIDT(17)*DTDX(I)  
        PX(I,18)= DNIDR(18)*DRDX(I)+ DNIDS(18)*DSDX(I)+DNIDT(18)*DTDX(I)  
        PX(I,19)= DNIDR(19)*DRDX(I)+ DNIDS(19)*DSDX(I)+DNIDT(19)*DTDX(I)  
        PX(I,20)= DNIDR(20)*DRDX(I)+ DNIDS(20)*DSDX(I)+DNIDT(20)*DTDX(I)  
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
        PY(I,9) = DNIDR(9)*DRDY(I) + DNIDS(9)*DSDY(I) + DNIDT(9)*DTDY(I)  
        PY(I,10)= DNIDR(10)*DRDY(I)+ DNIDS(10)*DSDY(I)+DNIDT(10)*DTDY(I)  
        PY(I,11)= DNIDR(11)*DRDY(I)+ DNIDS(11)*DSDY(I)+DNIDT(11)*DTDY(I)  
        PY(I,12)= DNIDR(12)*DRDY(I)+ DNIDS(12)*DSDY(I)+DNIDT(12)*DTDY(I)  
        PY(I,13)= DNIDR(13)*DRDY(I)+ DNIDS(13)*DSDY(I)+DNIDT(13)*DTDY(I)  
        PY(I,14)= DNIDR(14)*DRDY(I)+ DNIDS(14)*DSDY(I)+DNIDT(14)*DTDY(I)  
        PY(I,15)= DNIDR(15)*DRDY(I)+ DNIDS(15)*DSDY(I)+DNIDT(15)*DTDY(I)  
        PY(I,16)= DNIDR(16)*DRDY(I)+ DNIDS(16)*DSDY(I)+DNIDT(16)*DTDY(I)  
        PY(I,17)= DNIDR(17)*DRDY(I)+ DNIDS(17)*DSDY(I)+DNIDT(17)*DTDY(I)  
        PY(I,18)= DNIDR(18)*DRDY(I)+ DNIDS(18)*DSDY(I)+DNIDT(18)*DTDY(I)  
        PY(I,19)= DNIDR(19)*DRDY(I)+ DNIDS(19)*DSDY(I)+DNIDT(19)*DTDY(I)  
        PY(I,20)= DNIDR(20)*DRDY(I)+ DNIDS(20)*DSDY(I)+DNIDT(20)*DTDY(I)  
C
        PZ(I,1) = DNIDR(1)*DRDZ(I) + DNIDS(1)*DSDZ(I) + DNIDT(1)*DTDZ(I)  
        PZ(I,2) = DNIDR(2)*DRDZ(I) + DNIDS(2)*DSDZ(I) + DNIDT(2)*DTDZ(I)  
        PZ(I,3) = DNIDR(3)*DRDZ(I) + DNIDS(3)*DSDZ(I) + DNIDT(3)*DTDZ(I)  
        PZ(I,4) = DNIDR(4)*DRDZ(I) + DNIDS(4)*DSDZ(I) + DNIDT(4)*DTDZ(I)  
        PZ(I,5) = DNIDR(5)*DRDZ(I) + DNIDS(5)*DSDZ(I) + DNIDT(5)*DTDZ(I)  
        PZ(I,6) = DNIDR(6)*DRDZ(I) + DNIDS(6)*DSDZ(I) + DNIDT(6)*DTDZ(I)  
        PZ(I,7) = DNIDR(7)*DRDZ(I) + DNIDS(7)*DSDZ(I) + DNIDT(7)*DTDZ(I)  
        PZ(I,8) = DNIDR(8)*DRDZ(I) + DNIDS(8)*DSDZ(I) + DNIDT(8)*DTDZ(I)  
        PZ(I,9) = DNIDR(9)*DRDZ(I) + DNIDS(9)*DSDZ(I) + DNIDT(9)*DTDZ(I)  
        PZ(I,10)= DNIDR(10)*DRDZ(I)+ DNIDS(10)*DSDZ(I)+DNIDT(10)*DTDZ(I)  
        PZ(I,11)= DNIDR(11)*DRDZ(I)+ DNIDS(11)*DSDZ(I)+DNIDT(11)*DTDZ(I)  
        PZ(I,12)= DNIDR(12)*DRDZ(I)+ DNIDS(12)*DSDZ(I)+DNIDT(12)*DTDZ(I)  
        PZ(I,13)= DNIDR(13)*DRDZ(I)+ DNIDS(13)*DSDZ(I)+DNIDT(13)*DTDZ(I)  
        PZ(I,14)= DNIDR(14)*DRDZ(I)+ DNIDS(14)*DSDZ(I)+DNIDT(14)*DTDZ(I)  
        PZ(I,15)= DNIDR(15)*DRDZ(I)+ DNIDS(15)*DSDZ(I)+DNIDT(15)*DTDZ(I)  
        PZ(I,16)= DNIDR(16)*DRDZ(I)+ DNIDS(16)*DSDZ(I)+DNIDT(16)*DTDZ(I)  
        PZ(I,17)= DNIDR(17)*DRDZ(I)+ DNIDS(17)*DSDZ(I)+DNIDT(17)*DTDZ(I)  
        PZ(I,18)= DNIDR(18)*DRDZ(I)+ DNIDS(18)*DSDZ(I)+DNIDT(18)*DTDZ(I)  
        PZ(I,19)= DNIDR(19)*DRDZ(I)+ DNIDS(19)*DSDZ(I)+DNIDT(19)*DTDZ(I)  
        PZ(I,20)= DNIDR(20)*DRDZ(I)+ DNIDS(20)*DSDZ(I)+DNIDT(20)*DTDZ(I)  
C
      ENDDO
C
C
C
      DO N=1,20
        DO I=1,NEL
          KXX(I,N) = VOL(I)*
     .     (PX(I,N)*PX(I,N) + PY(I,N)*PY(I,N) + PZ(I,N)*PZ(I,N))
          UL(I,N) = UL(I,N) + KXX(I,N)
        ENDDO
      ENDDO
      DO I=1,NEL
        VOLG(I) =VOLG(I) + VOL(I)
      ENDDO
C
c      AA = 0
c      DO N=1,20
c        AA = AA + NI(N)*NI(N)
c      ENDDO
c      DO I=1,NEL
c        BB = 1.E20
c        DO N=1,20
c          BB = MIN(BB,NI(N)*NI(N)/UL(I,N))
c        ENDDO
c        DELTAX(I) = SQRT(2.*BB/AA)
c      ENDDO
C
C
 2000 FORMAT(/' ZERO OR NEGATIVE VOLUME : DELETE 3D-ELEMENT NB',I10/)
C
      RETURN
      END
