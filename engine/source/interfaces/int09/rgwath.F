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
Chd|  RGWATH                        source/interfaces/int09/rgwath.F
Chd|-- called by -----------
Chd|        RGWAL1                        source/ale/grid/rgwal1.F      
Chd|-- calls ---------------
Chd|        RGWAT2                        source/interfaces/int09/rgwat2.F
Chd|        RGWAT3                        source/interfaces/int09/rgwat3.F
Chd|        SPMD_EXCH_FR6                 source/mpi/kinematic_conditions/spmd_exch_fr6.F
Chd|        SUM_6_FLOAT                   source/system/parit.F         
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE RGWATH(
     1               X      ,V      ,W        ,RWL     ,NSW     ,
     2               NSN    ,MSR    ,MS       ,FSAV    ,IXS     ,
     3               IXQ    ,ELBUF_TAB,IPARG   ,PM      ,
     4               NTAG   ,NELW   ,NE       ,TEMP    ,TSTIF   ,
     5               E      ,A      ,ITIED    ,WEIGHT  ,IAD_ELEM,
     6               FR_ELEM,FR_WALL)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD            
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "param_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NSN, ITIED, MSR, NE
      INTEGER IPARG(NPARG,*), NSW(*) ,IXS(NIXS,*),IXQ(NIXQ,*),
     .     NTAG(*), NELW(*), WEIGHT(*),
     .        IAD_ELEM(*), FR_ELEM(*), FR_WALL(*)
      my_real
     .  PM(NPROPM,*), X(*), RWL(*), MS(*), FSAV(*), V(*), W(*), 
     .  E(*), A(*),
     .  TEMP,TSTIF,FHEAT, SAVE(3)
      TYPE(ELBUF_STRUCT_), DIMENSION(NGROUP) :: ELBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER M3, M2, M1, I, N, N3, N2, N1, K, PMAIN
      my_real
     .   XWL, YWL, ZWL, VXW, VYW, VZW, FXN, FYN, FZN, FXT, FYT, FZT,
     .   VX, VY, VZ, UX, UY, UZ, XC, YC, ZC, DP, DV, DA, DVT,
     .   FNXN,FNYN, FNZN, FNXT, FNYT, FNZT, FNDFN, FTDFT, FRIC, FRIC2,
     .   FCOE,
     .   F1(NSN), F2(NSN), F3(NSN), F4(NSN), F5(NSN), F6(NSN)
      DOUBLE PRECISION 
     .   FRWL6(6,6)
C
      IF(MSR==0)THEN
       XWL=RWL(4)
       YWL=RWL(5)
       ZWL=RWL(6)
       VXW=ZERO
       VYW=ZERO
       VZW=ZERO      
      ELSE
       M3=3*MSR
       M2=M3-1
       M1=M2-1
       VXW=V(M1)
       VYW=V(M2)
       VZW=V(M3)
       XWL=X(M1)+VXW*DT2
       YWL=X(M2)+VYW*DT2
       ZWL=X(M3)+VZW*DT2
      ENDIF
C-----------------------
C     VITESSE DE MATIERE ET GRILLE
C-----------------------
C
      DO 10 N=1,NUMNOD
        NTAG(N) = 0
        E(N)	= ZERO
 10   CONTINUE
C
c      FXN = ZERO
c      FYN = ZERO
c      FZN = ZERO
c      FXT = ZERO
c      FYT = ZERO
c      FZT = ZERO
C
      DO 20 I=1,NSN
      N=IABS(NSW(I))
      N3=3*N
      N2=N3-1
      N1=N2-1
      VX=V(N1)
      VY=V(N2)
      VZ=V(N3)
      UX=X(N1)+VX*DT2
      UY=X(N2)+VY*DT2
      UZ=X(N3)+VZ*DT2
      XC=UX-XWL
      YC=UY-YWL
      ZC=UZ-ZWL
      DP=XC*RWL(1)+YC*RWL(2)+ZC*RWL(3)
      NSW(I) = N
      IF(DP>ZERO)GOTO 20
       NTAG(N) = 1
       DV=(V(N1)-VXW)*RWL(1)+(V(N2)-VYW)*RWL(2)+(V(N3)-VZW)*RWL(3)
       DVT=DV
       FNXN=DVT*RWL(1)*MS(N)
       FNYN=DVT*RWL(2)*MS(N)
       FNZN=DVT*RWL(3)*MS(N)
       F1(I) = FNXN*WEIGHT(N)
       F2(I) = FNYN*WEIGHT(N)
       F3(I) = FNZN*WEIGHT(N)
c       FXN=FXN+FNXN*WEIGHT(N)
c       FYN=FYN+FNYN*WEIGHT(N)
c       FZN=FZN+FNZN*WEIGHT(N)
       IF(ITIED/=0)THEN
        FNXT=((V(N1)-VXW))*MS(N)-FNXN
        FNYT=((V(N2)-VYW))*MS(N)-FNYN
        FNZT=((V(N3)-VZW))*MS(N)-FNZN
        FNDFN=FNXN**2+FNYN**2+FNZN**2
        FTDFT=FNXT**2+FNYT**2+FNZT**2
        FHEAT=RWL(12)
        FRIC =RWL(13)
        FRIC2=FRIC**2
        IF(FTDFT<=FRIC2*FNDFN.OR.ITIED==1) THEN
C         POINT SECND TIED
          V(N1)=VXW
          V(N2)=VYW
          V(N3)=VZW
        ELSE
C         POINT SECND SLIDING
          FCOE=FRIC*SQRT(FNDFN/FTDFT)
          FNXT=FCOE*FNXT
          FNYT=FCOE*FNYT
          FNZT=FCOE*FNZT
          V(N1)=V(N1)-DV*RWL(1)-FNXT/MS(N)
          V(N2)=V(N2)-DV*RWL(2)-FNYT/MS(N)
          V(N3)=V(N3)-DV*RWL(3)-FNZT/MS(N)
          E(N) = FHEAT * 
     .        ((V(N1)-VXW)*FNXT+(V(N2)-VYW)*FNYT+(V(N3)-VZW)*FNZT)
        ENDIF
        F4(I) = FNXT*WEIGHT(N)
        F5(I) = FNYT*WEIGHT(N)
        F6(I) = FNZT*WEIGHT(N)
c        FXT=FXT+FNXT*WEIGHT(N)
c        FYT=FYT+FNYT*WEIGHT(N)
c        FZT=FZT+FNZT*WEIGHT(N)
       ELSE
c        FXT=ZERO
c        FYT=ZERO
c        FZT=ZERO
        F4(I) = ZERO
        F5(I) = ZERO
        F6(I) = ZERO
        V(N1)=V(N1)-DV*RWL(1)
        V(N2)=V(N2)-DV*RWL(2)
        V(N3)=V(N3)-DV*RWL(3)
       ENDIF
       DV=(W(N1)-VXW)*RWL(1)+(W(N2)-VYW)*RWL(2)+(W(N3)-VZW)*RWL(3)
       W(N1)=W(N1)-DV*RWL(1)
       W(N2)=W(N2)-DV*RWL(2)
       W(N3)=W(N3)-DV*RWL(3)
 20   CONTINUE
C
C Traitement Parith/ON
C
      IF (MSR/=0) THEN
        DO K = 1, 6
          FRWL6(1,K) = ZERO
          FRWL6(2,K) = ZERO
          FRWL6(3,K) = ZERO
          FRWL6(4,K) = ZERO
          FRWL6(5,K) = ZERO
          FRWL6(6,K) = ZERO
        END DO
        CALL SUM_6_FLOAT(1, NSN, F1, FRWL6(1,1), 6)
        CALL SUM_6_FLOAT(1, NSN, F2, FRWL6(2,1), 6)
        CALL SUM_6_FLOAT(1, NSN, F3, FRWL6(3,1), 6)
        CALL SUM_6_FLOAT(1, NSN, F4, FRWL6(4,1), 6)
        CALL SUM_6_FLOAT(1, NSN, F5, FRWL6(5,1), 6)
        CALL SUM_6_FLOAT(1, NSN, F6, FRWL6(6,1), 6)
        
        IF(NSPMD > 1) THEN
Cel si proc concerne par le rgwall
          IF(FR_WALL(ISPMD+1)/=0) THEN
            CALL SPMD_EXCH_FR6(FR_WALL,FRWL6,6*6)
          ENDIF
          PMAIN = FR_WALL(NSPMD+2)
        ELSE
          PMAIN = 1
        ENDIF

        FXN = FRWL6(1,1)+FRWL6(1,2)+FRWL6(1,3)+
     .        FRWL6(1,4)+FRWL6(1,5)+FRWL6(1,6)
        FYN = FRWL6(2,1)+FRWL6(2,2)+FRWL6(2,3)+
     .        FRWL6(2,4)+FRWL6(2,5)+FRWL6(2,6)
        FZN = FRWL6(3,1)+FRWL6(3,2)+FRWL6(3,3)+
     .        FRWL6(3,4)+FRWL6(3,5)+FRWL6(3,6)
        FXT = FRWL6(4,1)+FRWL6(4,2)+FRWL6(4,3)+
     .        FRWL6(4,4)+FRWL6(4,5)+FRWL6(4,6)
        FYT = FRWL6(5,1)+FRWL6(5,2)+FRWL6(5,3)+
     .        FRWL6(5,4)+FRWL6(5,5)+FRWL6(5,6)
        FZT = FRWL6(6,1)+FRWL6(6,2)+FRWL6(6,3)+
     .        FRWL6(6,4)+FRWL6(6,5)+FRWL6(6,6)
        IF(MS(MSR)/=ZERO)THEN
          A(M1)=(FXT+FXN) / DT12
          A(M2)=(FYT+FYN) / DT12
          A(M3)=(FZT+FZN) / DT12
        ENDIF

        IF(ISPMD+1==PMAIN)THEN
C
CGW   TFEXT=TFEXT 
CCG   L' ENERGIE DE FROTTEMENT EST MISE DS LES ELEMENTS
C
          FSAV(1)=FSAV(1)+FXN
          FSAV(2)=FSAV(2)+FYN
          FSAV(3)=FSAV(3)+FZN
          FSAV(4)=FSAV(4)+FXT
          FSAV(5)=FSAV(5)+FYT
          FSAV(6)=FSAV(6)+FZT
        ENDIF
      ENDIF  ! fin si MSR /= 0

C----------------------
C     PONT THERMIQUE
C----------------------
      IF(N2D==0)THEN
        CALL RGWAT3(
     1               X        ,NELW ,NE      ,IXS    ,
     4               ELBUF_TAB,IPARG,PM      ,NTAG   ,TEMP   ,
     5               TSTIF    ,E    ,IAD_ELEM,FR_ELEM        )
      ELSE
        CALL RGWAT2(
     1               X        ,NELW ,NE      ,IXQ    ,
     4               ELBUF_TAB,IPARG,PM      ,NTAG   ,TEMP   ,
     5               TSTIF    ,E    ,IAD_ELEM,FR_ELEM        )
      ENDIF
C-----------
      RETURN
      END
