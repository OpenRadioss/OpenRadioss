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
Chd|  R3DEF3                        source/elements/spring/r3def3.F
Chd|-- called by -----------
Chd|        RFORC3                        source/elements/spring/rforc3.F
Chd|-- calls ---------------
Chd|        REDEF3                        source/elements/spring/redef3.F
Chd|        TABLE_INTERP                  source/tools/curve/table_tools.F
Chd|        INTERFACE_TABLE_MOD           share/modules/table_mod.F     
Chd|        TABLE_MOD                     share/modules/table_mod.F     
Chd|====================================================================
      SUBROUTINE R3DEF3(
     1                  GEO    ,F     ,AL0    ,E     ,DL     ,
     2                  NPF    ,TF    ,OFF    ,DPL   ,FEP    ,
     3                  DPX2   ,DFS   ,V      ,IXR   ,DF     ,
     4                  ANIM   ,IPOS  ,FR_WAVE,IGEO  ,AL0_ERR,
     5                  X1DP   ,X2DP  ,X3DP   ,YIELD ,TABLE  ,
     6                  INIFRIC,NGL   ,MGN    ,EX    ,EY     ,
     7                  EZ     ,XK    ,XM     ,XC    ,AK     ,
     8                  EX2    ,EY2   ,EZ2    ,NC1   ,NC2    ,
     9                  NC3    ,NUVAR ,UVAR   ,DL0   )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TABLE_MOD
      USE INTERFACE_TABLE_MOD
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
#include      "units_c.inc"
#include      "scr17_c.inc"
#include      "param_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "scr03_c.inc"
#include      "scr14_c.inc"
#include      "vect01_c.inc"
#include      "com01_c.inc"
#include      "impl1_c.inc" 
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NPF(*),IXR(NIXR,*),IGEO(NPROPGI,*),NGL(*),MGN(*),
     .        NC1(*),NC2(*),NC3(*),NUVAR
C     REAL
      my_real
     .   GEO(NPROPG,*), F(*), AL0(*), E(*), DL(*), TF(*), OFF(*),
     .   DPL(*), FEP(*), DPX2(*),DFS(*),V(3,*),DF(*),ANIM(*),
     .   IPOS(*),FR_WAVE(*),AL0_ERR(*),YIELD(*),INIFRIC(*),
     .   EX(MVSIZ),EY(MVSIZ),EZ(MVSIZ),XK(MVSIZ),
     .   XM(MVSIZ),XC(MVSIZ),AK(MVSIZ),EX2(MVSIZ),EY2(MVSIZ),EZ2(MVSIZ),
     .   UVAR(NUVAR,*),DL0(*)
      DOUBLE PRECISION X1DP(3,*),X2DP(3,*),X3DP(3,*)
      TYPE(TTABLE) TABLE(*)
C-----------------------------------------------
c   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IFUNC2(MVSIZ),
     .        IECROU(MVSIZ), IFUNC(MVSIZ), IFV(MVSIZ),
     .        NINDX, INDX(MVSIZ),I,ILENG,IPID,J, 
     .        IFUNC3(MVSIZ),ITABF(MVSIZ),IFRIC(MVSIZ)
C     REAL
      my_real
     .     XL0(MVSIZ),DMN(MVSIZ),DMX(MVSIZ),
     .     DLOLD(MVSIZ),B(MVSIZ), D(MVSIZ), DV(MVSIZ),
     .     FF(MVSIZ),LSCALE(MVSIZ),EE(MVSIZ),
     .     GF3(MVSIZ),FRIC(MVSIZ),XSCALF(MVSIZ),GX(MVSIZ),YSCALF(MVSIZ),
     .     F_MIN(MVSIZ),F_MAX(MVSIZ),EPLA(MVSIZ)
      my_real
     .     BETA, MU,FMAX, DDX ,VX21,VY21,VZ21,VL21,
     .     VX32,VY32,VZ32,VL32,NOT_USED
      my_real,
     .        DIMENSION(1) :: XX
      DOUBLE PRECISION EXDP(MVSIZ) ,EYDP(MVSIZ) ,EZDP(MVSIZ),
     .                 EX2DP(MVSIZ),EY2DP(MVSIZ),EZ2DP(MVSIZ),
     .                 AL1DP(MVSIZ),AL2DP(MVSIZ),ALDP(MVSIZ),
     .                 AL0DP(MVSIZ)
C-----------------------------------------------
C
      NOT_USED = ZERO
C
      DO I=LFT,LLT
        EPLA(I) = ZERO
        IPID = MGN(I)
        XM(I)  =GEO(1,IPID)
        XK(I)  =GEO(2,IPID)
        XC(I)  =GEO(3,IPID) 
        IECROU(I)=NINT(GEO(7,IPID))
        AK(I)    = GEO(10,IPID)
        B(I)     = GEO(11,IPID)
        D(I)     = GEO(13,IPID)
        EE(I)    = GEO(40,IPID)
        GF3(I)   = GEO(132,IPID)
        FF(I)    = GEO(18,IPID)
        DMN(I)   = GEO(15,IPID)
        DMX(I)   = GEO(16,IPID)
        FRIC(I)  = GEO(17,IPID)
        XSCALF(I)= GEO(20,IPID)
        LSCALE(I)= GEO(39,IPID)
        IFUNC(I) = IGEO(101,IPID)
        IFV(I)   = IGEO(102,IPID)
        IFUNC2(I)= IGEO(103,IPID)
        IFUNC3(I)= IGEO(119,IPID)
        ITABF(I) = IGEO(201,IPID)
        IFRIC(I) = IGEO(202,IPID)
        F_MIN(I) = GEO(138,IPID)
        F_MAX(I) = GEO(139,IPID)
        YSCALF(I)= GEO(140,IPID)
      ENDDO
C
      DO I=LFT,LLT
        EXDP(I)=X2DP(1,I)-X1DP(1,I)
        EYDP(I)=X2DP(2,I)-X1DP(2,I)
        EZDP(I)=X2DP(3,I)-X1DP(3,I)
        EX2DP(I)=X2DP(1,I)-X3DP(1,I)
        EY2DP(I)=X2DP(2,I)-X3DP(2,I)
        EZ2DP(I)=X2DP(3,I)-X3DP(3,I)
        DLOLD(I)=DL(I)
      ENDDO
C
      IF (INISPRI /= 0 .and. TT == ZERO) THEN
        DO I=LFT,LLT
          DLOLD(I)=DL0(I)
        ENDDO
      ENDIF
C
      DO I=LFT,LLT
        AL1DP(I)=SQRT(EXDP(I)*EXDP(I)+EYDP(I)*EYDP(I)+EZDP(I)*EZDP(I))
        AL2DP(I)=SQRT(EX2DP(I)*EX2DP(I)+EY2DP(I)*EY2DP(I)+
     .                                  EZ2DP(I)*EZ2DP(I))
        ALDP(I) =AL1DP(I) + AL2DP(I)
        AL1DP(I)= MAX(AL1DP(I),EM15)
        EXDP(I) = EXDP(I)/AL1DP(I)
        EYDP(I) = EYDP(I)/AL1DP(I)
        EZDP(I) = EZDP(I)/AL1DP(I)
        AL2DP(I)= MAX(AL2DP(I),EM15)
        EX2DP(I)= EX2DP(I)/AL2DP(I)
        EY2DP(I)= EY2DP(I)/AL2DP(I)
        EZ2DP(I)= EZ2DP(I)/AL2DP(I)
      ENDDO
C
      DO I=LFT,LLT
        EX(I) = EXDP(I) 
        EY(I) = EYDP(I) 
        EZ(I) = EZDP(I) 
        EX2(I)= EX2DP(I)
        EY2(I)= EY2DP(I)
        EZ2(I)= EZ2DP(I)
      ENDDO
!
      IF (INISPRI /= 0 .and. TT == ZERO) THEN
        DO I=LFT,LLT
          XL0(I)= AL0(I)
! if not initialized length
          IF (XL0(I) == ZERO) XL0(I) = ALDP(I)
        ENDDO
      ENDIF
!
      IF (TT == ZERO) THEN
        DO I=LFT,LLT
          AL0(I)= ALDP(I)               ! cast double vers My_real
        ENDDO
      ENDIF
C
      IF (SCODVER >= 101) THEN
        IF (TT == ZERO) THEN
          DO I=LFT,LLT
            AL0_ERR(I)=ALDP(I)-AL0(I)     ! difference entre double et My_real
          ENDDO
        ENDIF
      ENDIF
!
      IF ( INISPRI /= 0 .and. TT == ZERO) THEN
        DO I=LFT,LLT
          AL0(I)= XL0(I)
        ENDDO
      ENDIF
!
      DO I=LFT,LLT
        AL0DP(I)= AL0(I)                ! cast My_real en double
      ENDDO
!
      IF (SCODVER >= 101) THEN
        DO I=LFT,LLT
          AL0DP(I)= AL0DP(I)+AL0_ERR(I)   ! AL_DP doit   tre recalcul   ainsi afin de garantir la coh  rence absolue entre AL0_DP et AL_DP
        ENDDO
      ENDIF
C
      IF (ISMDISP > 0) THEN
        DO I=LFT,LLT
          VX21= V(1,NC2(I)) - V(1,NC1(I)) 
          VY21= V(2,NC2(I)) - V(2,NC1(I)) 
          VZ21= V(3,NC2(I)) - V(3,NC1(I)) 
          VL21 = VX21*EX(I)+VY21*EY(I)+VZ21*EZ(I)
          VX32= V(1,NC2(I)) - V(1,NC3(I)) 
          VY32= V(2,NC2(I)) - V(2,NC3(I)) 
          VZ32= V(3,NC2(I)) - V(3,NC3(I)) 
          VL32 = VX32*EX2(I)+VY32*EY2(I)+VZ32*EZ2(I)
          DL(I)= DLOLD(I)+(VL21+VL32)*DT1
        ENDDO
      ELSE
        DO I=LFT,LLT
          DL(I)= (ALDP(I)-AL0DP(I))
        ENDDO
      ENDIF !(ISMDISP.GT.0) THEN
C
      DO I=LFT,LLT
        ILENG=NINT(GEO(93,MGN(I)))
        IF (ILENG /= 0) THEN
          XL0(I)=AL0DP(I)
        ELSE
          XL0(I) = ONE
        ENDIF
      ENDDO
C
      CALL REDEF3(F       ,XK     ,DL    ,FEP        ,DLOLD ,
     .            DPL     ,TF     ,NPF   ,XC         ,OFF   ,
     .            E       ,DPX2   ,ANIM  ,ANIM_FE(11),IPOS  ,
     .            FR_WAVE ,XL0    ,DMN   ,DMX        ,DV    ,
     .            FF      ,LSCALE ,EE    ,GF3        ,IFUNC3,
     .            YIELD   ,ALDP   ,AK    ,B          ,D     ,
     .            IECROU  ,IFUNC  ,IFV   ,IFUNC2     ,EPLA  ,
     .            UVAR(1,1),NOT_USED,NOT_USED,NOT_USED)
      NINDX = 0
      DO I=LFT,LLT
        IF (OFF(I) == ONE .AND. DMX(I) /= ZERO .AND. DMN(I) /= ZERO) THEN
          IF (DL(I) > DMX(I) .OR. DL(I) < DMN(I)) THEN
            OFF(I)=ZERO
            NINDX = NINDX + 1
            INDX(NINDX) = I
            IDEL7NOK = 1
          ENDIF
        ENDIF
      ENDDO
C
      DO J=1,NINDX
        I = INDX(J)
#include "lockon.inc"
        WRITE(IOUT, 1000) NGL(I)
        WRITE(ISTDO,1100) NGL(I),TT
#include "lockoff.inc"
      ENDDO
C
      DO I=LFT,LLT
        XM(I)=XM(I)*XL0(I)
        XK(I)=XK(I)/XL0(I)
C--- for time step compute adding +max slope of h
        XC(I)=(XC(I)+GEO(141,MGN(I)))/XL0(I)
      ENDDO
C
      DO I=LFT,LLT
        IF (IFRIC(I) == 0) THEN ! old approach
          IF (ITABF(I) > ZERO) THEN
            DDX =  EXDP(I) * (V(1,NC2(I)) - V(1,NC1(I)))
     .           + EYDP(I) * (V(2,NC2(I)) - V(2,NC1(I)))
     .           + EZDP(I) * (V(3,NC2(I)) - V(3,NC1(I)))
     .           + EX2DP(I)* (V(1,NC3(I)) - V(1,NC2(I)))
     .           + EY2DP(I)* (V(2,NC3(I)) - V(2,NC2(I)))
     .           + EZ2DP(I)* (V(3,NC3(I)) - V(3,NC2(I)))
            DFS(I)= DFS(I) + DDX * DT1 * XK(I)
            BETA  = PI - ACOS(EXDP(I)*EX2DP(I)+EYDP(I)*EY2DP(I)+
     .                                         EZDP(I)*EZ2DP(I))
            XX(1) = ABS(DFS(I)*XSCALF(I))
            CALL TABLE_INTERP(TABLE(ITABF(I)),XX,MU)
            MU = MU*YSCALF(I)
            FMAX  = MAX(ZERO,F(I) * TANH(HALF*MU*BETA))
            FMAX  = MIN(FMAX,ABS(DFS(I)))
            DFS(I)= SIGN(FMAX,DFS(I))
            DF(I) = DFS(I)
          ELSEIF (FRIC(I) > ZERO) THEN
            DDX =  EXDP(I) * (V(1,NC2(I)) - V(1,NC1(I)))
     .           + EYDP(I) * (V(2,NC2(I)) - V(2,NC1(I)))
     .           + EZDP(I) * (V(3,NC2(I)) - V(3,NC1(I)))
     .           + EX2DP(I)* (V(1,NC3(I)) - V(1,NC2(I)))
     .           + EY2DP(I)* (V(2,NC3(I)) - V(2,NC2(I)))
     .           + EZ2DP(I)* (V(3,NC3(I)) - V(3,NC2(I)))
            DFS(I)= DFS(I) + DDX * DT1 * XK(I)
            BETA  = PI - ACOS(EXDP(I)*EX2DP(I)+EYDP(I)*EY2DP(I)+
     .                                         EZDP(I)*EZ2DP(I))
            FMAX  = MAX(ZERO,F(I) * TANH(HALF*FRIC(I)*BETA))
            FMAX  = MIN(FMAX,ABS(DFS(I)))
            DFS(I)= SIGN(FMAX,DFS(I))
            DF(I) = DFS(I)
C         df=dfs pour compatibilite des restarts si fric=0 
C         et dfs=ev(nb9) non existant avant 23f et ancien 23f
          ELSE
            DF(I)= ZERO
          ENDIF ! IF (ITABF(I) > ZERO)
        ELSEIF (IFRIC(I) > 0) THEN ! new approach
          IF (ITABF(I) > ZERO) THEN
            DDX =  EXDP(I) * (V(1,NC2(I)) - V(1,NC1(I)))
     .           + EYDP(I) * (V(2,NC2(I)) - V(2,NC1(I)))
     .           + EZDP(I) * (V(3,NC2(I)) - V(3,NC1(I)))
     .           + EX2DP(I)* (V(1,NC3(I)) - V(1,NC2(I)))
     .           + EY2DP(I)* (V(2,NC3(I)) - V(2,NC2(I)))
     .           + EZ2DP(I)* (V(3,NC3(I)) - V(3,NC2(I)))
            DFS(I)= DFS(I) + DDX * DT1 * XK(I)
            BETA  = PI - ACOS(EXDP(I)*EX2DP(I)+EYDP(I)*EY2DP(I)+
     .                                         EZDP(I)*EZ2DP(I))
cc          XX(1) = ABS(DFS(I)*XSCALF(I)) --> wrong only positive (abscisa) side function used
            XX(1) = DFS(I)*XSCALF(I)
            CALL TABLE_INTERP(TABLE(ITABF(I)),XX,MU)
            MU = MU*YSCALF(I)
C---
C  force limit reached (positive and negative)
C---
cc            IF (F_MIN(I) /= ZERO .OR. F_MAX(I) /= 0) THEN
cc              IF (DFS(I) <= F_MIN(I) .OR. DFS(I) >= F_MAX(I)) MU = FRIC(I)
cc            ELSE  ! no limit force defined
cc            ENDIF
            IF (DFS(I) <= F_MIN(I) .OR. DFS(I) >= F_MAX(I)) INIFRIC(I) = ONE
            IF (INIFRIC(I) == ONE) MU = FRIC(I) ! reset friction to initial
C---
            FMAX  = MAX(ZERO,F(I) * TANH(HALF*MU*BETA))
            FMAX  = MIN(FMAX,ABS(DFS(I)))
            DFS(I)= SIGN(FMAX,DFS(I))
            DF(I) = DFS(I)
          ELSEIF (FRIC(I) > ZERO .AND. ITABF(I) == 0) THEN
            DDX =  EXDP(I) * (V(1,NC2(I)) - V(1,NC1(I)))
     .           + EYDP(I) * (V(2,NC2(I)) - V(2,NC1(I)))
     .           + EZDP(I) * (V(3,NC2(I)) - V(3,NC1(I)))
     .           + EX2DP(I)* (V(1,NC3(I)) - V(1,NC2(I)))
     .           + EY2DP(I)* (V(2,NC3(I)) - V(2,NC2(I)))
     .           + EZ2DP(I)* (V(3,NC3(I)) - V(3,NC2(I)))
            DFS(I)= DFS(I) + DDX * DT1 * XK(I)
            BETA  = PI - ACOS(EXDP(I)*EX2DP(I)+EYDP(I)*EY2DP(I)+
     .                                         EZDP(I)*EZ2DP(I))
            FMAX  = MAX(ZERO,F(I) * TANH(HALF*FRIC(I)*BETA))
            FMAX  = MIN(FMAX,ABS(DFS(I)))
            DFS(I)= SIGN(FMAX,DFS(I))
            DF(I) = DFS(I)
          ELSE
            DF(I)= ZERO
          ENDIF ! IF (ITABF(I) > ZERO)
        ENDIF ! IF (IFRIC(I) == 0)
      ENDDO ! DO I=LFT,LLT
C---
 1000 FORMAT(1X,'-- RUPTURE OF SPRING ELEMENT NUMBER ',I10)
 1100 FORMAT(1X,'-- RUPTURE OF SPRING ELEMENT :',I10,' AT TIME :',G11.4)
C---
      RETURN
      END
