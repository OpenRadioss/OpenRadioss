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
Chd|  I7LAGM                        source/interfaces/int07/i7lagm.F
Chd|-- called by -----------
Chd|        I7MAIN_LMULT                  source/interfaces/int07/i7main_lmult.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE I7LAGM(LLL     ,JLL     ,SLL    ,XLL   ,IADLL  ,
     2                  N_MUL_MX,ITASK   ,NINT   ,NKMAX , 
     3                  JLT     ,A       ,V      ,ITAG  ,XTAG   ,
     4                  GAP     ,NOINT   ,STFN   ,ITAB  ,CN_LOC ,
     5                  NX1     ,NX2     ,NX3    ,NX4   ,NY1    ,
     6                  NY2     ,NY3     ,NY4    ,NZ1   ,NZ2    ,
     7                  NZ3     ,NZ4     ,LB1    ,LB2   ,LB3    ,
     8                  LB4     ,LC1     ,LC2    ,LC3   ,LC4    ,
     9                  P1      ,P2      ,P3     ,P4    ,
     A                  IX1     ,IX2     ,IX3    ,IX4   ,NSVG   ,
     B                  GAPV    ,NEWFRONT,IBAG   ,ICONTACT,STIF ,
     C                  COMNTAG ,IADM    )
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
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "task_c.inc"
#include      "warn_c.inc"
#include      "scr05_c.inc"
#include      "scr07_c.inc"
#include      "units_c.inc"
#include      "com08_c.inc"
#include      "param_c.inc"
#include      "couple_c.inc"
      COMMON /LAGGLOB/N_MULT
      INTEGER N_MULT
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER N_MUL_MX,ITASK,ITIED,NINT,NKMAX ,
     .  LLL(*),JLL(*),SLL(*),IADLL(*),COMNTAG(*)
C     REAL
      my_real
     .  V(3,*),XLL(*),A(3,*),XTAG(*)
      INTEGER JLT, IBAG  ,NOINT,NEWFRONT, IADM
      INTEGER ITAB(*),ICONTACT(*),ITAG(*)
      INTEGER IX1(MVSIZ), IX2(MVSIZ), IX3(MVSIZ), IX4(MVSIZ),
     .        NSVG(MVSIZ), CN_LOC(MVSIZ)
      my_real
     .   GAP, STFN(*)
      my_real
     .     NX1(MVSIZ), NX2(MVSIZ), NX3(MVSIZ), NX4(MVSIZ),
     .     NY1(MVSIZ), NY2(MVSIZ), NY3(MVSIZ), NY4(MVSIZ),
     .     NZ1(MVSIZ), NZ2(MVSIZ), NZ3(MVSIZ), NZ4(MVSIZ),
     .     LB1(MVSIZ), LB2(MVSIZ), LB3(MVSIZ), LB4(MVSIZ),
     .     LC1(MVSIZ), LC2(MVSIZ), LC3(MVSIZ), LC4(MVSIZ),
     .   P1(MVSIZ), P2(MVSIZ), P3(MVSIZ), P4(MVSIZ), STIF(MVSIZ),
     .   GAPV(MVSIZ)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,IK,IE,IS,IC,NK,III(MVSIZ,17),LLT,NFT,LE,FIRST,LAST,
     .        I16,IAD,LL
      my_real
     .   AA,XMIN,YMIN,ZMIN,XMAX,YMAX,ZMAX
      INTEGER IG 
      my_real
     .   NX(MVSIZ), NY(MVSIZ), NZ(MVSIZ), PENE(MVSIZ),
     .   H1(MVSIZ), H2(MVSIZ), H3(MVSIZ), H4(MVSIZ),
     .   VX(MVSIZ), VY(MVSIZ), VZ(MVSIZ), VN(MVSIZ), 
     .   H0, LA1, LA2, LA3, LA4,D1,D2,D3,D4,A1,A2,A3,A4
C-----------------------------------------------
C
C
C       | M | Lt| | a    | M ao
C       |---+---| |    = |
C       | L | 0 | | la   | bo
C
C        [M] a + [L]t la = [M] ao
C        [L] a = bo
C
C         a = -[M]-1[L]t la + ao
C        [L][M]-1[L]t la  = [L] ao - bo
C
C on pose:
C        [H] = [L][M]-1[L]t
C        b  = [L] ao - bo
C
C        [H] la = b
C
C        a = ao - [M]-1[L]t la
C-----------------------------------------------
C
C        la              : LAMBDA(N_MULT)
C        ao              : A(NUMNOD)
C        L               : XLL(NK,N_MULT)
C        M               : MAS(NUMNOD)
C        [L][M]-1[L]t la : HLA(N_MULT)
C        [L] ao - b      : B(N_MULT)
C        [M]-1[L]t la    : LTLA(NUMNOD)
C
C        N_MULT : nombre de contact 
C        NK : nombre de noeud pour un contact (8+1,16+1,8+8,16+16)
C
C        IC : numero du contact (1,N_MULT)
C        IK : numero de noeud local a un contact (1,NK)
C        I  : numero global du noeud (1,NUMNOD)
C
C        IADLL(N_MULT)        : IAD = IADLL(IC)
C        LLL(N_MULT*(17,51))  : I  = LLL(IAD+1,2...IADNEXT-1)
C-----------------------------------------------
C  evaluation de b:
C
C         Vs = Somme(Ni Vi)
C         Vs_ + dt As = Somme(Ni Vi_) + Somme(dt Ni Ai)
C         Somme(dt Ni Ai) - dt As =  Vs_ -Somme(Ni Vi_)
C         [L] = dt {N1,N2,..,N15,-1}
C         bo = [L] a = -[L]/dt v_
C         b  = [L] ao - bo
C         b  = [L] ao + [L]/dt v_ = [L] (v_ + ao dt)/dt
C-----------------------------------------------
C                 b  = [L] vo+/dt   +   vout
C-----------------------------------------------
#include "sync_dir.inc"

C--------------------------------------------------------
C  SEUL CAS RESTANT : PAQUETS MIXTES
C--------------------------------------------------------

      DO I=1,JLT
C
        D1 = SQRT(P1(I))
        P1(I) = MAX(ZERO, GAPV(I) - D1)
C
        D2 = SQRT(P2(I))
        P2(I) = MAX(ZERO, GAPV(I) - D2)
C
        D3 = SQRT(P3(I))
        P3(I) = MAX(ZERO, GAPV(I) - D3)
C
        D4 = SQRT(P4(I))
        P4(I) = MAX(ZERO, GAPV(I) - D4)
C
        A1 = P1(I)/MAX(EM20,D1)
        A2 = P2(I)/MAX(EM20,D2)
        A3 = P3(I)/MAX(EM20,D3)
        A4 = P4(I)/MAX(EM20,D4)
        NX(I) = A1*NX1(I) + A2*NX2(I) + A3*NX3(I) + A4*NX4(I) 
        NY(I) = A1*NY1(I) + A2*NY2(I) + A3*NY3(I) + A4*NY4(I) 
        NZ(I) = A1*NZ1(I) + A2*NZ2(I) + A3*NZ3(I) + A4*NZ4(I) 
      ENDDO
C
      DO I=1,JLT
        IF(IX3(I)/=IX4(I))THEN
         PENE(I) = MAX(P1(I),P2(I),P3(I),P4(I))
C
         LA1 = ONE - LB1(I) - LC1(I)
         LA2 = ONE - LB2(I) - LC2(I)
         LA3 = ONE - LB3(I) - LC3(I)
         LA4 = ONE - LB4(I) - LC4(I)         
C
         H0    = FOURTH * 
     .         (P1(I)*LA1 + P2(I)*LA2 + P3(I)*LA3 + P4(I)*LA4)
         H1(I) = H0 + P1(I) * LB1(I) + P4(I) * LC4(I)
         H2(I) = H0 + P2(I) * LB2(I) + P1(I) * LC1(I)
         H3(I) = H0 + P3(I) * LB3(I) + P2(I) * LC2(I)
         H4(I) = H0 + P4(I) * LB4(I) + P3(I) * LC3(I)
         H0    = 1./MAX(EM20,H1(I) + H2(I) + H3(I) + H4(I))
         H1(I) = H1(I) * H0
         H2(I) = H2(I) * H0
         H3(I) = H3(I) * H0
         H4(I) = H4(I) * H0
C
        ELSE
         PENE(I) = P1(I)
         NX(I) = NX1(I)
         NY(I) = NY1(I)
         NZ(I) = NZ1(I)
         H1(I) = LB1(I)
         H2(I) = LC1(I)
         H3(I) = ONE - LB1(I) - LC1(I)
         H4(I) = ZERO
        ENDIF
      ENDDO

C
c      DO I=1,JLT
cC       correction hourglass
c         H0 = -.25*(H1(I) - H2(I) + H3(I) - H4(I))
c         H0 = MIN(H0,H2(I),H4(I))
c         H0 = MAX(H0,-H1(I),-H3(I))
c         IF(IX3(I)==IX4(I))H0 = 0.0
c         H1(I) = H1(I) + H0
c         H2(I) = H2(I) - H0
c         H3(I) = H3(I) + H0
c         H4(I) = H4(I) - H0
c      ENDDO
C
C---------------------
C
      DO I=1,JLT
       IF( (GAPV(I)-PENE(I))/GAPV(I) <EM10 .AND. 
     .     STIF(I)>ZERO) THEN
         STIF(I) = ZERO
         IF (IMACH==3) NEWFRONT = -1
#include "lockon.inc"
            STFN(CN_LOC(I)) = -ABS(STFN(CN_LOC(I)))

         WRITE(ISTDO,'(A,I8)')' WARNING INTERFACE ',NOINT
         WRITE(ISTDO,'(A,I8,A)')' NODE ',ITAB(NSVG(I)),
     .                   ' DE-ACTIVATED FROM INTERFACE'                 
         WRITE(IOUT ,'(A,I8)')' WARNING INTERFACE ',NOINT
         WRITE(IOUT ,'(A,I8,A)')' NODE ',ITAB(NSVG(I)),
     .                   ' DE-ACTIVATED FROM INTERFACE'   
#include "lockoff.inc"
       ENDIF
      ENDDO
C
      DO I=1,JLT
        IG=NSVG(I)
        VX(I) = V(1,IG)+DT12*A(1,IG)
     .                  - H1(I)*(V(1,IX1(I))+DT12*A(1,IX1(I)))
     .                  - H2(I)*(V(1,IX2(I))+DT12*A(1,IX2(I)))
     .                  - H3(I)*(V(1,IX3(I))+DT12*A(1,IX3(I)))
     .                  - H4(I)*(V(1,IX4(I))+DT12*A(1,IX4(I)))
        VY(I) = V(2,IG)+DT12*A(2,IG)
     .                  - H1(I)*(V(2,IX1(I))+DT12*A(2,IX1(I)))
     .                  - H2(I)*(V(2,IX2(I))+DT12*A(2,IX2(I)))
     .                  - H3(I)*(V(2,IX3(I))+DT12*A(2,IX3(I)))
     .                  - H4(I)*(V(2,IX4(I))+DT12*A(2,IX4(I)))
        VZ(I) = V(3,IG)+DT12*A(3,IG)  
     .                  - H1(I)*(V(3,IX1(I))+DT12*A(3,IX1(I)))
     .                  - H2(I)*(V(3,IX2(I))+DT12*A(3,IX2(I)))
     .                  - H3(I)*(V(3,IX3(I))+DT12*A(3,IX3(I)))
     .                  - H4(I)*(V(3,IX4(I))+DT12*A(3,IX4(I)))
        VN(I) = NX(I)*VX(I) + NY(I)*VY(I) + NZ(I)*VZ(I)
#include "lockon.inc"
       IF(STIF(I)/=ZERO.AND.PENE(I)>ZERO.AND.VN(I)<XTAG(IG))THEN
            AA = ONE/SQRT(NX(I)*NX(I)+NY(I)*NY(I)+NZ(I)*NZ(I))
            NX(I) = NX(I)*AA
            NY(I) = NY(I)*AA
            NZ(I) = NZ(I)*AA
            IF(ITAG(NSVG(I))==0)then
             N_MULT = N_MULT+1
             ITAG(NSVG(I)) = N_MULT
             XTAG(NSVG(I)) = VN(I)
             IF(N_MULT > N_MUL_MX)THEN
#include "lockoff.inc"
               CALL ANCMSG(MSGID=95,ANMODE=ANINFO)
               CALL ARRET(2)
             ENDIF
             IADLL(N_MULT+1)=IADLL(N_MULT) + 15
             IF(IADLL(N_MULT+1)-1 > NKMAX)THEN
#include "lockoff.inc"
               CALL ANCMSG(MSGID=96,ANMODE=ANINFO,
     .                     I1=IADLL(N_MULT+1)-1,
     .                     I2=NKMAX)
               CALL ARRET(2)
             ENDIF
             IAD = IADLL(N_MULT) - 1
            else
             xtag(NSVG(I)) = VN(I)
             IAD = IADLL(itag(NSVG(I))) - 1
             LL = LLL(IAD+1)
             COMNTAG(LL)= COMNTAG(LL) - 1
             LL = LLL(IAD+2)
             COMNTAG(LL)= COMNTAG(LL) - 1
             LL = LLL(IAD+3)
             COMNTAG(LL)= COMNTAG(LL) - 1
             LL = LLL(IAD+4)
             COMNTAG(LL)= COMNTAG(LL) - 1
             LL = LLL(IAD+5)
             COMNTAG(LL)= COMNTAG(LL) - 1
            ENDIF
C
            LLL(IAD+1)    = IX1(I)
            JLL(IAD+1)    = 1
            SLL(IAD+1)    = 0
            XLL(IAD+1)    = NX(I)*H1(I)
C---
            LLL(IAD+2)    = IX2(I)
            JLL(IAD+2)    = 1
            SLL(IAD+2)    = 0
            XLL(IAD+2)    = NX(I)*H2(I)
C---
            LLL(IAD+3)    = IX3(I)
            JLL(IAD+3)    = 1
            SLL(IAD+3)    = 0
            XLL(IAD+3)    = NX(I)*H3(I)
C---
            LLL(IAD+4)    = IX4(I)
            JLL(IAD+4)    = 1
            SLL(IAD+4)    = 0
            XLL(IAD+4)    = NX(I)*H4(I)
C---
            LLL(IAD+5)    = NSVG(I)
            JLL(IAD+5)    = 1
            SLL(IAD+5)    = NINT
            XLL(IAD+5)    = -NX(I)
C-----------------------------
            LLL(IAD+6)    = IX1(I)
            JLL(IAD+6)    = 2
            SLL(IAD+6)    = 0
            XLL(IAD+6)    = NY(I)*H1(I)
C---
            LLL(IAD+7)    = IX2(I)
            JLL(IAD+7)    = 2
            SLL(IAD+7)    = 0
            XLL(IAD+7)    = NY(I)*H2(I)
C---
            LLL(IAD+8)    = IX3(I)
            JLL(IAD+8)    = 2
            SLL(IAD+8)    = 0
            XLL(IAD+8)    = NY(I)*H3(I)
C---
            LLL(IAD+9)    = IX4(I)
            JLL(IAD+9)    = 2
            SLL(IAD+9)    = 0
            XLL(IAD+9)    = NY(I)*H4(I)
C---
            LLL(IAD+10)    = NSVG(I)
            JLL(IAD+10)    = 2
            SLL(IAD+10)    = NINT
            XLL(IAD+10)    = -NY(I)
C------------------------------------
            LLL(IAD+11)    = IX1(I)
            JLL(IAD+11)    = 3
            SLL(IAD+11)    = 0
            XLL(IAD+11)    = NZ(I)*H1(I)
C---
            LLL(IAD+12)    = IX2(I)
            JLL(IAD+12)    = 3
            SLL(IAD+12)    = 0
            XLL(IAD+12)    = NZ(I)*H2(I)
C---
            LLL(IAD+13)    = IX3(I)
            JLL(IAD+13)    = 3
            SLL(IAD+13)    = 0
            XLL(IAD+13)    = NZ(I)*H3(I)
C---
            LLL(IAD+14)    = IX4(I)
            JLL(IAD+14)    = 3
            SLL(IAD+14)    = 0
            XLL(IAD+14)    = NZ(I)*H4(I)
C---
            LLL(IAD+15)    = NSVG(I)
            JLL(IAD+15)    = 3
            SLL(IAD+15)    = NINT
            XLL(IAD+15)    = -NZ(I)
C--------------------------------------
            LL = IX1(I)
            COMNTAG(LL) = COMNTAG(LL) + 1
            LL = IX2(I)
            COMNTAG(LL) = COMNTAG(LL) + 1
            LL = IX3(I)
            COMNTAG(LL) = COMNTAG(LL) + 1
            LL = IX4(I)
            COMNTAG(LL) = COMNTAG(LL) + 1
            LL = NSVG(I)
            COMNTAG(LL) = COMNTAG(LL) + 1
C
        ENDIF
#include "lockoff.inc"
      ENDDO
C
      IF(IBAG/=0.OR.IADM/=0)THEN
       DO I=1,JLT
        IF(PENE(I)/=ZERO)THEN
          ICONTACT(NSVG(I))=1
          ICONTACT(IX1(I))=1
          ICONTACT(IX2(I))=1
          ICONTACT(IX3(I))=1
          ICONTACT(IX4(I))=1
        ENDIF
       ENDDO
      ENDIF
C
C-----------------------------------------------
      RETURN
      END
