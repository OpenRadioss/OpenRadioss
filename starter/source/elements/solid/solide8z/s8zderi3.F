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
Chd|  S8ZDERI3                      source/elements/solid/solide8z/s8zderi3.F
Chd|-- called by -----------
Chd|        S8CINIT3                      source/elements/thickshell/solide8c/s8cinit3.F
Chd|        S8ZINIT3                      source/elements/solid/solide8z/s8zinit3.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE S8ZDERI3(VOL,VEUL,GEO,
     .    KSI,ETA,ZETA,WI,
     .    HX, HY, HZ,   
     .    CJ1,CJ2,CJ3,
     .    CJ4,CJ5,CJ6,
     .    CJ7,CJ8,CJ9,
     .    SMAX, DELTAX, NGL,VOLDP)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
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
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real
     .   VOL(*), VEUL(LVEUL,*),GEO(NPROPG,*)
      my_real
     .   KSI,ETA,ZETA,WI,
     .   HX(4,*), HY(4,*), HZ(4,*),  
     .   CJ1(*),CJ2(*),CJ3(*),
     .   CJ4(*),CJ5(*),CJ6(*),
     .   CJ7(*),CJ8(*),CJ9(*),
     .   SMAX(*), DELTAX(*)
      INTEGER NGL(*)
      DOUBLE PRECISION
     .   VOLDP(*),DETDP
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J
C                                                                     12
      my_real
     .   JAC1(MVSIZ),JAC2(MVSIZ),JAC3(MVSIZ),
     .   JAC4(MVSIZ),JAC5(MVSIZ),JAC6(MVSIZ),
     .   JAC7(MVSIZ),JAC8(MVSIZ),JAC9(MVSIZ),
     .   JAC_59_68(MVSIZ), JAC_67_49(MVSIZ), JAC_48_57(MVSIZ), DET(MVSIZ)
C-----------------------------------------------
C Jacobian matrix
      DO I=LFT,LLT
       JAC1(I)=CJ1(I)+HX(3,I)*ETA+(HX(2,I)+HX(4,I)*ETA)*ZETA
       JAC2(I)=CJ2(I)+HY(3,I)*ETA+(HY(2,I)+HY(4,I)*ETA)*ZETA
       JAC3(I)=CJ3(I)+HZ(3,I)*ETA+(HZ(2,I)+HZ(4,I)*ETA)*ZETA
C
       JAC4(I)=CJ4(I)+HX(1,I)*ZETA+(HX(3,I)+HX(4,I)*ZETA)*KSI
       JAC5(I)=CJ5(I)+HY(1,I)*ZETA+(HY(3,I)+HY(4,I)*ZETA)*KSI
       JAC6(I)=CJ6(I)+HZ(1,I)*ZETA+(HZ(3,I)+HZ(4,I)*ZETA)*KSI
C
       JAC7(I)=CJ7(I)+HX(2,I)*KSI+(HX(1,I)+HX(4,I)*KSI)*ETA
       JAC8(I)=CJ8(I)+HY(2,I)*KSI+(HY(1,I)+HY(4,I)*KSI)*ETA
       JAC9(I)=CJ9(I)+HZ(2,I)*KSI+(HZ(1,I)+HZ(4,I)*KSI)*ETA
      ENDDO
C
      DO I=LFT,LLT
       JAC_59_68(I)=JAC5(I)*JAC9(I)-JAC6(I)*JAC8(I)
       JAC_67_49(I)=JAC6(I)*JAC7(I)-JAC4(I)*JAC9(I)
       JAC_48_57(I)=JAC4(I)*JAC8(I)-JAC5(I)*JAC7(I)
      ENDDO
C
      DO I=LFT,LLT
      DETDP=ONE_OVER_512*(JAC1(I)*JAC_59_68(I)+JAC2(I)*JAC_67_49(I)+JAC3(I)*JAC_48_57(I))
      DET(I)=DETDP
      VOLDP(I)=WI*DETDP
      VOL(I)= VOLDP(I)
      ENDDO
      DO I=LFT,LLT
      IF(DET(I)>0.0) CYCLE
      CALL ANCMSG(MSGID=245,
     .            MSGTYPE=MSGERROR,
     .            ANMODE=ANINFO,
     .            I1=NGL(I))
      END DO
C
C
      DO I=LFT,LLT
        DELTAX(I)=MIN(DELTAX(I),HUNDRED28*DET(I)*SMAX(I))
      ENDDO
      RETURN
      END
Chd|====================================================================
Chd|  S8ZDERIC3                     source/elements/solid/solide8z/s8zderi3.F
Chd|-- called by -----------
Chd|        S8CINIT3                      source/elements/thickshell/solide8c/s8cinit3.F
Chd|        S8ZINIT3                      source/elements/solid/solide8z/s8zinit3.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE S8ZDERIC3(VOL,
     .     HX, HY, HZ,   
     .     JAC1,JAC2,JAC3,
     .     JAC4,JAC5,JAC6,
     .     JAC7,JAC8,JAC9,SMAX, DET, NGL, 
     .     XD1, XD2, XD3, XD4, XD5, XD6, XD7, XD8, 
     .     YD1, YD2, YD3, YD4, YD5, YD6, YD7, YD8, 
     .     ZD1, ZD2, ZD3, ZD4, ZD5, ZD6, ZD7, ZD8)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
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
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real
     .   VOL(*),HX(4,*), HY(4,*), HZ(4,*),  
     .   JAC1(*),JAC2(*),JAC3(*),
     .   JAC4(*),JAC5(*),JAC6(*),
     .   JAC7(*),JAC8(*),JAC9(*),SMAX(*), DET(*)
      DOUBLE PRECISION
     .   XD1(MVSIZ), XD2(MVSIZ), XD3(MVSIZ), XD4(MVSIZ), 
     .   XD5(MVSIZ), XD6(MVSIZ), XD7(MVSIZ), XD8(MVSIZ), 
     .   YD1(MVSIZ), YD2(MVSIZ), YD3(MVSIZ), YD4(MVSIZ),
     .   YD5(MVSIZ), YD6(MVSIZ), YD7(MVSIZ), YD8(MVSIZ),
     .   ZD1(MVSIZ), ZD2(MVSIZ), ZD3(MVSIZ), ZD4(MVSIZ),
     .   ZD5(MVSIZ), ZD6(MVSIZ), ZD7(MVSIZ), ZD8(MVSIZ)
      INTEGER NGL(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,  J
      
C                                                                     12
      my_real
     .   X17(MVSIZ) , X28(MVSIZ) , X35(MVSIZ) , X46(MVSIZ),
     .   Y17(MVSIZ) , Y28(MVSIZ) , Y35(MVSIZ) , Y46(MVSIZ),
     .   Z17(MVSIZ) , Z28(MVSIZ) , Z35(MVSIZ) , Z46(MVSIZ),
     .   JAC_59_68(MVSIZ), JAC_67_49(MVSIZ), JAC_48_57(MVSIZ),
     .   JAC_38_29(MVSIZ), JAC_19_37(MVSIZ), JAC_27_18(MVSIZ),
     .   JAC_26_35(MVSIZ), JAC_34_16(MVSIZ), JAC_15_24(MVSIZ),
     .   X_17_46(MVSIZ) , X_28_35(MVSIZ) ,
     .   Y_17_46(MVSIZ) , Y_28_35(MVSIZ) ,
     .   Z_17_46(MVSIZ) , Z_28_35(MVSIZ) 
C-----------------------------------------------
      DO I=LFT,LLT
      X17(I)=XD7(I)-XD1(I)
      X28(I)=XD8(I)-XD2(I)
      X35(I)=XD5(I)-XD3(I)
      X46(I)=XD6(I)-XD4(I)
      Y17(I)=YD7(I)-YD1(I)
      Y28(I)=YD8(I)-YD2(I)
      Y35(I)=YD5(I)-YD3(I)
      Y46(I)=YD6(I)-YD4(I)
      Z17(I)=ZD7(I)-ZD1(I)
      Z28(I)=ZD8(I)-ZD2(I)
      Z35(I)=ZD5(I)-ZD3(I)
      Z46(I)=ZD6(I)-ZD4(I)
      END DO
C
      DO I=LFT,LLT
      JAC4(I)=X17(I)+X28(I)-X35(I)-X46(I)
      JAC5(I)=Y17(I)+Y28(I)-Y35(I)-Y46(I)
      JAC6(I)=Z17(I)+Z28(I)-Z35(I)-Z46(I)
      X_17_46(I)=X17(I)+X46(I)
      X_28_35(I)=X28(I)+X35(I)
      Y_17_46(I)=Y17(I)+Y46(I)
      Y_28_35(I)=Y28(I)+Y35(I)
      Z_17_46(I)=Z17(I)+Z46(I)
      Z_28_35(I)=Z28(I)+Z35(I)
      END DO
C
C Jacobian matrix
      DO I=LFT,LLT
      JAC7(I)=X_17_46(I)+X_28_35(I)
      JAC8(I)=Y_17_46(I)+Y_28_35(I)
      JAC9(I)=Z_17_46(I)+Z_28_35(I)
      JAC1(I)=X_17_46(I)-X_28_35(I)
      JAC2(I)=Y_17_46(I)-Y_28_35(I)
      JAC3(I)=Z_17_46(I)-Z_28_35(I)
      END DO
C
      DO I=LFT,LLT
      JAC_59_68(I)=JAC5(I)*JAC9(I)-JAC6(I)*JAC8(I)
      JAC_67_49(I)=JAC6(I)*JAC7(I)-JAC4(I)*JAC9(I)
      JAC_38_29(I)=(-JAC2(I)*JAC9(I)+JAC3(I)*JAC8(I))
      JAC_19_37(I)=( JAC1(I)*JAC9(I)-JAC3(I)*JAC7(I))
      JAC_27_18(I)=(-JAC1(I)*JAC8(I)+JAC2(I)*JAC7(I))
      JAC_26_35(I)=( JAC2(I)*JAC6(I)-JAC3(I)*JAC5(I))
      JAC_34_16(I)=(-JAC1(I)*JAC6(I)+JAC3(I)*JAC4(I))
      JAC_15_24(I)=( JAC1(I)*JAC5(I)-JAC2(I)*JAC4(I))
      JAC_48_57(I)=JAC4(I)*JAC8(I)-JAC5(I)*JAC7(I)
      END DO
C
      DO I=LFT,LLT
      DET(I)=ONE_OVER_64*(JAC1(I)*JAC_59_68(I)+JAC2(I)*JAC_67_49(I)+JAC3(I)*JAC_48_57(I))
      VOL(I)=DET(I)
      END DO
C
      DO I=LFT,LLT
      IF(DET(I)>0.0) CYCLE
      CALL ANCMSG(MSGID=245,
     .            MSGTYPE=MSGERROR,
     .            ANMODE=ANINFO,
     .            I1=NGL(I))
      END DO
C
C
C mode 1
C 1 1 -1 -1 -1 -1 1 1
       DO I=LFT,LLT
         HX(1,I)=(XD1(I)+XD2(I)-XD3(I)-XD4(I)-XD5(I)-XD6(I)+XD7(I)+XD8(I))
         HY(1,I)=(YD1(I)+YD2(I)-YD3(I)-YD4(I)-YD5(I)-YD6(I)+YD7(I)+YD8(I))
         HZ(1,I)=(ZD1(I)+ZD2(I)-ZD3(I)-ZD4(I)-ZD5(I)-ZD6(I)+ZD7(I)+ZD8(I))
       ENDDO
C mode 2
C 1 -1 -1 1 -1 1 1 -1
       DO I=LFT,LLT
         HX(2,I)=(XD1(I)-XD2(I)-XD3(I)+XD4(I)-XD5(I)+XD6(I)+XD7(I)-XD8(I))
         HY(2,I)=(YD1(I)-YD2(I)-YD3(I)+YD4(I)-YD5(I)+YD6(I)+YD7(I)-YD8(I))
         HZ(2,I)=(ZD1(I)-ZD2(I)-ZD3(I)+ZD4(I)-ZD5(I)+ZD6(I)+ZD7(I)-ZD8(I))
       ENDDO
C mode 3
C 1 -1 1 -1 1 -1 1 -1
       DO I=LFT,LLT
         HX(3,I)=(XD1(I)-XD2(I)+XD3(I)-XD4(I)+XD5(I)-XD6(I)+XD7(I)-XD8(I))
         HY(3,I)=(YD1(I)-YD2(I)+YD3(I)-YD4(I)+YD5(I)-YD6(I)+YD7(I)-YD8(I))
         HZ(3,I)=(ZD1(I)-ZD2(I)+ZD3(I)-ZD4(I)+ZD5(I)-ZD6(I)+ZD7(I)-ZD8(I))
       ENDDO
C mode 4
C -1 1 -1 1 1 -1 1 -1
       DO I=LFT,LLT
         HX(4,I)=(-XD1(I)+XD2(I)-XD3(I)+XD4(I)+XD5(I)-XD6(I)+XD7(I)-XD8(I))
         HY(4,I)=(-YD1(I)+YD2(I)-YD3(I)+YD4(I)+YD5(I)-YD6(I)+YD7(I)-YD8(I))
         HZ(4,I)=(-ZD1(I)+ZD2(I)-ZD3(I)+ZD4(I)+ZD5(I)-ZD6(I)+ZD7(I)-ZD8(I))
       ENDDO
C----surface max mediane-- *16
       DO I=LFT,LLT
         SMAX(I)= JAC_59_68(I)*JAC_59_68(I)+JAC_67_49(I)*JAC_67_49(I)
     .            +JAC_48_57(I)*JAC_48_57(I)
         SMAX(I)= MAX(SMAX(I),JAC_38_29(I)*JAC_38_29(I)+JAC_19_37(I)*JAC_19_37(I)
     .            +JAC_27_18(I)*JAC_27_18(I))
         SMAX(I)= MAX(SMAX(I),JAC_26_35(I)*JAC_26_35(I)+JAC_34_16(I)*JAC_34_16(I)
     .            +JAC_15_24(I)*JAC_15_24(I))
       ENDDO
       DO I=LFT,LLT
          IF(SMAX(I)<=ZERO)THEN
           CALL ANCMSG(MSGID=245,
     .                 MSGTYPE=MSGERROR,
     .                 ANMODE=ANINFO,
     .                 I1=NGL(I))
          ENDIF
          SMAX(I)= ONE/SQRT(SMAX(I))
       ENDDO
      RETURN
C
      END
Chd|====================================================================
Chd|  S8ZJAC_IC                     source/elements/solid/solide8z/s8zderi3.F
Chd|-- called by -----------
Chd|        S8ZINIT3                      source/elements/solid/solide8z/s8zinit3.F
Chd|-- calls ---------------
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
        SUBROUTINE S8ZJAC_IC(
     .              XD1  ,XD2  ,XD3  ,XD4  ,XD5  ,XD6  ,XD7  ,XD8   ,
     .              YD1  ,YD2  ,YD3  ,YD4  ,YD5  ,YD6  ,YD7  ,YD8   ,
     .              ZD1  ,ZD2  ,ZD3  ,ZD4  ,ZD5  ,ZD6  ,ZD7  ,ZD8   ,
     .              JAC1  ,JAC2  ,JAC3  ,
     .              JAC4  ,JAC5  ,JAC6  ,
     .              JAC7  ,JAC8  ,JAC9  ,
     .              HX, HY, HZ,
     .              JAC_I)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
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
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real
     .   HX(4,*), HY(4,*), HZ(4,*),  
     .   JAC1(*),JAC2(*),JAC3(*),
     .   JAC4(*),JAC5(*),JAC6(*),
     .   JAC7(*),JAC8(*),JAC9(*),JAC_I(10,*)
      DOUBLE PRECISION
     .   XD1(MVSIZ), XD2(MVSIZ), XD3(MVSIZ), XD4(MVSIZ), 
     .   XD5(MVSIZ), XD6(MVSIZ), XD7(MVSIZ), XD8(MVSIZ), 
     .   YD1(MVSIZ), YD2(MVSIZ), YD3(MVSIZ), YD4(MVSIZ),
     .   YD5(MVSIZ), YD6(MVSIZ), YD7(MVSIZ), YD8(MVSIZ),
     .   ZD1(MVSIZ), ZD2(MVSIZ), ZD3(MVSIZ), ZD4(MVSIZ),
     .   ZD5(MVSIZ), ZD6(MVSIZ), ZD7(MVSIZ), ZD8(MVSIZ)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,  J
      
      my_real
     .   DET(MVSIZ) ,DETT(MVSIZ)
C                                                                     12
      my_real
     .   X17(MVSIZ) , X28(MVSIZ) , X35(MVSIZ) , X46(MVSIZ),
     .   Y17(MVSIZ) , Y28(MVSIZ) , Y35(MVSIZ) , Y46(MVSIZ),
     .   Z17(MVSIZ) , Z28(MVSIZ) , Z35(MVSIZ) , Z46(MVSIZ),
     .   JAC_59_68(MVSIZ), JAC_67_49(MVSIZ), JAC_48_57(MVSIZ),
     .   JAC_38_29(MVSIZ), JAC_19_37(MVSIZ), JAC_27_18(MVSIZ),
     .   JAC_26_35(MVSIZ), JAC_34_16(MVSIZ), JAC_15_24(MVSIZ),
     .   X_17_46(MVSIZ) , X_28_35(MVSIZ) ,
     .   Y_17_46(MVSIZ) , Y_28_35(MVSIZ) ,
     .   Z_17_46(MVSIZ) , Z_28_35(MVSIZ) 

C-----------------------------------------------
      DO I=LFT,LLT
      X17(I)=XD7(I)-XD1(I)
      X28(I)=XD8(I)-XD2(I)
      X35(I)=XD5(I)-XD3(I)
      X46(I)=XD6(I)-XD4(I)
      Y17(I)=YD7(I)-YD1(I)
      Y28(I)=YD8(I)-YD2(I)
      Y35(I)=YD5(I)-YD3(I)
      Y46(I)=YD6(I)-YD4(I)
      Z17(I)=ZD7(I)-ZD1(I)
      Z28(I)=ZD8(I)-ZD2(I)
      Z35(I)=ZD5(I)-ZD3(I)
      Z46(I)=ZD6(I)-ZD4(I)
      END DO
C
      DO I=LFT,LLT
      JAC4(I)=X17(I)+X28(I)-X35(I)-X46(I)
      JAC5(I)=Y17(I)+Y28(I)-Y35(I)-Y46(I)
      JAC6(I)=Z17(I)+Z28(I)-Z35(I)-Z46(I)
      X_17_46(I)=X17(I)+X46(I)
      X_28_35(I)=X28(I)+X35(I)
      Y_17_46(I)=Y17(I)+Y46(I)
      Y_28_35(I)=Y28(I)+Y35(I)
      Z_17_46(I)=Z17(I)+Z46(I)
      Z_28_35(I)=Z28(I)+Z35(I)
      END DO
C
      DO I=LFT,LLT
      JAC7(I)=X_17_46(I)+X_28_35(I)
      JAC8(I)=Y_17_46(I)+Y_28_35(I)
      JAC9(I)=Z_17_46(I)+Z_28_35(I)
      JAC1(I)=X_17_46(I)-X_28_35(I)
      JAC2(I)=Y_17_46(I)-Y_28_35(I)
      JAC3(I)=Z_17_46(I)-Z_28_35(I)
      END DO
C
      DO I=LFT,LLT
      JAC_59_68(I)=JAC5(I)*JAC9(I)-JAC6(I)*JAC8(I)
      JAC_67_49(I)=JAC6(I)*JAC7(I)-JAC4(I)*JAC9(I)
      JAC_38_29(I)=(-JAC2(I)*JAC9(I)+JAC3(I)*JAC8(I))
      JAC_19_37(I)=( JAC1(I)*JAC9(I)-JAC3(I)*JAC7(I))
      JAC_27_18(I)=(-JAC1(I)*JAC8(I)+JAC2(I)*JAC7(I))
      JAC_26_35(I)=( JAC2(I)*JAC6(I)-JAC3(I)*JAC5(I))
      JAC_34_16(I)=(-JAC1(I)*JAC6(I)+JAC3(I)*JAC4(I))
      JAC_15_24(I)=( JAC1(I)*JAC5(I)-JAC2(I)*JAC4(I))
      JAC_48_57(I)=JAC4(I)*JAC8(I)-JAC5(I)*JAC7(I)
      END DO
C
      DO I=LFT,LLT
      DET(I)=ONE_OVER_64*(JAC1(I)*JAC_59_68(I)+JAC2(I)*JAC_67_49(I)+JAC3(I)*JAC_48_57(I))
      DETT(I)=ONE_OVER_64/DET(I)
      END DO
C
      DO I=LFT,LLT
       JAC_I(1,I)=DETT(I)*JAC_59_68(I)
       JAC_I(4,I)=DETT(I)*JAC_67_49(I)
       JAC_I(7,I)=DETT(I)*JAC_48_57(I)
       JAC_I(2,I)=DETT(I)*JAC_38_29(I)
       JAC_I(5,I)=DETT(I)*JAC_19_37(I)
       JAC_I(8,I)=DETT(I)*JAC_27_18(I)
       JAC_I(3,I)=DETT(I)*JAC_26_35(I)
       JAC_I(6,I)=DETT(I)*JAC_34_16(I)
       JAC_I(9,I)=DETT(I)*JAC_15_24(I)
       JAC_I(10,I)=DET(I)
      ENDDO
C mode 1
C 1 1 -1 -1 -1 -1 1 1
       DO I=LFT,LLT
         HX(1,I)=(XD1(I)+XD2(I)-XD3(I)-XD4(I)-XD5(I)-XD6(I)+XD7(I)+XD8(I))
         HY(1,I)=(YD1(I)+YD2(I)-YD3(I)-YD4(I)-YD5(I)-YD6(I)+YD7(I)+YD8(I))
         HZ(1,I)=(ZD1(I)+ZD2(I)-ZD3(I)-ZD4(I)-ZD5(I)-ZD6(I)+ZD7(I)+ZD8(I))
       ENDDO
C mode 2
C 1 -1 -1 1 -1 1 1 -1
       DO I=LFT,LLT
         HX(2,I)=(XD1(I)-XD2(I)-XD3(I)+XD4(I)-XD5(I)+XD6(I)+XD7(I)-XD8(I))
         HY(2,I)=(YD1(I)-YD2(I)-YD3(I)+YD4(I)-YD5(I)+YD6(I)+YD7(I)-YD8(I))
         HZ(2,I)=(ZD1(I)-ZD2(I)-ZD3(I)+ZD4(I)-ZD5(I)+ZD6(I)+ZD7(I)-ZD8(I))
       ENDDO
C mode 3
C 1 -1 1 -1 1 -1 1 -1
       DO I=LFT,LLT
         HX(3,I)=(XD1(I)-XD2(I)+XD3(I)-XD4(I)+XD5(I)-XD6(I)+XD7(I)-XD8(I))
         HY(3,I)=(YD1(I)-YD2(I)+YD3(I)-YD4(I)+YD5(I)-YD6(I)+YD7(I)-YD8(I))
         HZ(3,I)=(ZD1(I)-ZD2(I)+ZD3(I)-ZD4(I)+ZD5(I)-ZD6(I)+ZD7(I)-ZD8(I))
       ENDDO
C mode 4
C -1 1 -1 1 1 -1 1 -1
       DO I=LFT,LLT
         HX(4,I)=(-XD1(I)+XD2(I)-XD3(I)+XD4(I)+XD5(I)-XD6(I)+XD7(I)-XD8(I))
         HY(4,I)=(-YD1(I)+YD2(I)-YD3(I)+YD4(I)+YD5(I)-YD6(I)+YD7(I)-YD8(I))
         HZ(4,I)=(-ZD1(I)+ZD2(I)-ZD3(I)+ZD4(I)+ZD5(I)-ZD6(I)+ZD7(I)-ZD8(I))
       ENDDO
C       
      RETURN
C
      END
Chd|====================================================================
Chd|  S8ZPIJ_IC                     source/elements/solid/solide8z/s8zderi3.F
Chd|-- called by -----------
Chd|        S8ZINIT3                      source/elements/solid/solide8z/s8zinit3.F
Chd|-- calls ---------------
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
        SUBROUTINE S8ZPIJ_IC(
     .              XD1  ,XD2  ,XD3  ,XD4  ,XD5  ,XD6  ,XD7  ,XD8   ,
     .              YD1  ,YD2  ,YD3  ,YD4  ,YD5  ,YD6  ,YD7  ,YD8   ,
     .              ZD1  ,ZD2  ,ZD3  ,ZD4  ,ZD5  ,ZD6  ,ZD7  ,ZD8   ,
     .              JAC1  ,JAC2  ,JAC3  ,
     .              JAC4  ,JAC5  ,JAC6  ,
     .              JAC7  ,JAC8  ,JAC9  ,
     .              HX, HY, HZ,
     .              PXC1, PXC2, PXC3, PXC4, 
     .              PYC1, PYC2, PYC3, PYC4, 
     .              PZC1, PZC2, PZC3, PZC4)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
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
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real
     .   HX(4,*), HY(4,*), HZ(4,*),  
     .   PXC1(*), PXC2(*), PXC3(*), PXC4(*), 
     .   PYC1(*), PYC2(*), PYC3(*), PYC4(*), 
     .   PZC1(*), PZC2(*), PZC3(*), PZC4(*), 
     .   JAC1(*),JAC2(*),JAC3(*),
     .   JAC4(*),JAC5(*),JAC6(*),
     .   JAC7(*),JAC8(*),JAC9(*)
      DOUBLE PRECISION
     .   XD1(MVSIZ), XD2(MVSIZ), XD3(MVSIZ), XD4(MVSIZ), 
     .   XD5(MVSIZ), XD6(MVSIZ), XD7(MVSIZ), XD8(MVSIZ), 
     .   YD1(MVSIZ), YD2(MVSIZ), YD3(MVSIZ), YD4(MVSIZ),
     .   YD5(MVSIZ), YD6(MVSIZ), YD7(MVSIZ), YD8(MVSIZ),
     .   ZD1(MVSIZ), ZD2(MVSIZ), ZD3(MVSIZ), ZD4(MVSIZ),
     .   ZD5(MVSIZ), ZD6(MVSIZ), ZD7(MVSIZ), ZD8(MVSIZ)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,  J
      
      my_real
     .   DET(MVSIZ) ,DETT(MVSIZ)
C                                                                     12
      my_real
     .   X17(MVSIZ) , X28(MVSIZ) , X35(MVSIZ) , X46(MVSIZ),
     .   Y17(MVSIZ) , Y28(MVSIZ) , Y35(MVSIZ) , Y46(MVSIZ),
     .   Z17(MVSIZ) , Z28(MVSIZ) , Z35(MVSIZ) , Z46(MVSIZ),
     .   JAC_59_68(MVSIZ), JAC_67_49(MVSIZ), JAC_48_57(MVSIZ),
     .   JAC_38_29(MVSIZ), JAC_19_37(MVSIZ), JAC_27_18(MVSIZ),
     .   JAC_26_35(MVSIZ), JAC_34_16(MVSIZ), JAC_15_24(MVSIZ),
     .   JACI1, JACI2, JACI3,JACI4, JACI5, JACI6,JACI7, JACI8, JACI9,
     .   JACI12, JACI45, JACI78,JACI12P, JACI45P, JACI78P,
     .   X_17_46(MVSIZ) , X_28_35(MVSIZ) ,
     .   Y_17_46(MVSIZ) , Y_28_35(MVSIZ) ,
     .   Z_17_46(MVSIZ) , Z_28_35(MVSIZ) 

C-----------------------------------------------
      DO I=LFT,LLT
      X17(I)=XD7(I)-XD1(I)
      X28(I)=XD8(I)-XD2(I)
      X35(I)=XD5(I)-XD3(I)
      X46(I)=XD6(I)-XD4(I)
      Y17(I)=YD7(I)-YD1(I)
      Y28(I)=YD8(I)-YD2(I)
      Y35(I)=YD5(I)-YD3(I)
      Y46(I)=YD6(I)-YD4(I)
      Z17(I)=ZD7(I)-ZD1(I)
      Z28(I)=ZD8(I)-ZD2(I)
      Z35(I)=ZD5(I)-ZD3(I)
      Z46(I)=ZD6(I)-ZD4(I)
      END DO
C
      DO I=LFT,LLT
      JAC4(I)=X17(I)+X28(I)-X35(I)-X46(I)
      JAC5(I)=Y17(I)+Y28(I)-Y35(I)-Y46(I)
      JAC6(I)=Z17(I)+Z28(I)-Z35(I)-Z46(I)
      X_17_46(I)=X17(I)+X46(I)
      X_28_35(I)=X28(I)+X35(I)
      Y_17_46(I)=Y17(I)+Y46(I)
      Y_28_35(I)=Y28(I)+Y35(I)
      Z_17_46(I)=Z17(I)+Z46(I)
      Z_28_35(I)=Z28(I)+Z35(I)
      END DO
C
      DO I=LFT,LLT
      JAC7(I)=X_17_46(I)+X_28_35(I)
      JAC8(I)=Y_17_46(I)+Y_28_35(I)
      JAC9(I)=Z_17_46(I)+Z_28_35(I)
      JAC1(I)=X_17_46(I)-X_28_35(I)
      JAC2(I)=Y_17_46(I)-Y_28_35(I)
      JAC3(I)=Z_17_46(I)-Z_28_35(I)
      END DO
C
      DO I=LFT,LLT
      JAC_59_68(I)=JAC5(I)*JAC9(I)-JAC6(I)*JAC8(I)
      JAC_67_49(I)=JAC6(I)*JAC7(I)-JAC4(I)*JAC9(I)
      JAC_38_29(I)=(-JAC2(I)*JAC9(I)+JAC3(I)*JAC8(I))
      JAC_19_37(I)=( JAC1(I)*JAC9(I)-JAC3(I)*JAC7(I))
      JAC_27_18(I)=(-JAC1(I)*JAC8(I)+JAC2(I)*JAC7(I))
      JAC_26_35(I)=( JAC2(I)*JAC6(I)-JAC3(I)*JAC5(I))
      JAC_34_16(I)=(-JAC1(I)*JAC6(I)+JAC3(I)*JAC4(I))
      JAC_15_24(I)=( JAC1(I)*JAC5(I)-JAC2(I)*JAC4(I))
      JAC_48_57(I)=JAC4(I)*JAC8(I)-JAC5(I)*JAC7(I)
      END DO
C
      DO I=LFT,LLT
      DET(I)=ONE_OVER_64*(JAC1(I)*JAC_59_68(I)+JAC2(I)*JAC_67_49(I)+JAC3(I)*JAC_48_57(I))
      DETT(I)=ONE_OVER_64/DET(I)
      END DO
C
       DO I=LFT,LLT
         JACI1=DETT(I)*JAC_59_68(I)
         JACI4=DETT(I)*JAC_67_49(I)
         JACI7=DETT(I)*JAC_48_57(I)
         JACI2=DETT(I)*JAC_38_29(I)
         JACI5=DETT(I)*JAC_19_37(I)
         JACI8=DETT(I)*JAC_27_18(I)
         JACI3=DETT(I)*JAC_26_35(I)
         JACI6=DETT(I)*JAC_34_16(I)
         JACI9=DETT(I)*JAC_15_24(I)
         JACI12=JACI1-JACI2
         JACI45=JACI4-JACI5
         JACI78=JACI7-JACI8
         PXC2(I)= JACI12-JACI3
         PYC2(I)= JACI45-JACI6
         PZC2(I)= JACI78-JACI9
         PXC4(I)=-JACI12-JACI3
         PYC4(I)=-JACI45-JACI6
         PZC4(I)=-JACI78-JACI9
         JACI12P=JACI1+JACI2
         JACI45P=JACI4+JACI5
         JACI78P=JACI7+JACI8
         PXC1(I)=-JACI12P-JACI3
         PYC1(I)=-JACI45P-JACI6
         PZC1(I)=-JACI78P-JACI9
         PXC3(I)= JACI12P-JACI3
         PYC3(I)= JACI45P-JACI6
         PZC3(I)= JACI78P-JACI9
       ENDDO
C mode 1
C 1 1 -1 -1 -1 -1 1 1
       DO I=LFT,LLT
         HX(1,I)=(XD1(I)+XD2(I)-XD3(I)-XD4(I)-XD5(I)-XD6(I)+XD7(I)+XD8(I))
         HY(1,I)=(YD1(I)+YD2(I)-YD3(I)-YD4(I)-YD5(I)-YD6(I)+YD7(I)+YD8(I))
         HZ(1,I)=(ZD1(I)+ZD2(I)-ZD3(I)-ZD4(I)-ZD5(I)-ZD6(I)+ZD7(I)+ZD8(I))
       ENDDO
C mode 2
C 1 -1 -1 1 -1 1 1 -1
       DO I=LFT,LLT
         HX(2,I)=(XD1(I)-XD2(I)-XD3(I)+XD4(I)-XD5(I)+XD6(I)+XD7(I)-XD8(I))
         HY(2,I)=(YD1(I)-YD2(I)-YD3(I)+YD4(I)-YD5(I)+YD6(I)+YD7(I)-YD8(I))
         HZ(2,I)=(ZD1(I)-ZD2(I)-ZD3(I)+ZD4(I)-ZD5(I)+ZD6(I)+ZD7(I)-ZD8(I))
       ENDDO
C mode 3
C 1 -1 1 -1 1 -1 1 -1
       DO I=LFT,LLT
         HX(3,I)=(XD1(I)-XD2(I)+XD3(I)-XD4(I)+XD5(I)-XD6(I)+XD7(I)-XD8(I))
         HY(3,I)=(YD1(I)-YD2(I)+YD3(I)-YD4(I)+YD5(I)-YD6(I)+YD7(I)-YD8(I))
         HZ(3,I)=(ZD1(I)-ZD2(I)+ZD3(I)-ZD4(I)+ZD5(I)-ZD6(I)+ZD7(I)-ZD8(I))
       ENDDO
C mode 4
C -1 1 -1 1 1 -1 1 -1
       DO I=LFT,LLT
         HX(4,I)=(-XD1(I)+XD2(I)-XD3(I)+XD4(I)+XD5(I)-XD6(I)+XD7(I)-XD8(I))
         HY(4,I)=(-YD1(I)+YD2(I)-YD3(I)+YD4(I)+YD5(I)-YD6(I)+YD7(I)-YD8(I))
         HZ(4,I)=(-ZD1(I)+ZD2(I)-ZD3(I)+ZD4(I)+ZD5(I)-ZD6(I)+ZD7(I)-ZD8(I))
       ENDDO
C       
      RETURN
C
      END
Chd|====================================================================
Chd|  S8ZJAC_I3                     source/elements/solid/solide8z/s8zderi3.F
Chd|-- called by -----------
Chd|        S8ZINIT3                      source/elements/solid/solide8z/s8zinit3.F
Chd|-- calls ---------------
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE S8ZJAC_I3(
     .    KSI,ETA,ZETA,WI,
     .    HX, HY, HZ,   
     .    CJ1,CJ2,CJ3,
     .    CJ4,CJ5,CJ6,
     .    CJ7,CJ8,CJ9,
     .    JAC_I,L_PIJ,PIJ,NEL)
C-----------------------------------------------
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
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER L_PIJ,NEL
      my_real
     .   KSI,ETA,ZETA,WI,
     .   HX(4,*), HY(4,*), HZ(4,*),  
     .   CJ1(*),CJ2(*),CJ3(*),
     .   CJ4(*),CJ5(*),CJ6(*),
     .   CJ7(*),CJ8(*),CJ9(*),
     .   JAC_I(10,*),PIJ(NEL,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,  J
      my_real
     .   DET(MVSIZ) ,  DETT(MVSIZ) ,
     .   JAC1(MVSIZ),JAC2(MVSIZ),JAC3(MVSIZ),
     .   JAC4(MVSIZ),JAC5(MVSIZ),JAC6(MVSIZ),
     .   JAC7(MVSIZ),JAC8(MVSIZ),JAC9(MVSIZ),
     .   JAC_59_68(MVSIZ), JAC_67_49(MVSIZ), JAC_48_57(MVSIZ),
     .   JAC_38_29(MVSIZ), JAC_19_37(MVSIZ), JAC_27_18(MVSIZ),
     .   JAC_26_35(MVSIZ), JAC_34_16(MVSIZ), JAC_15_24(MVSIZ)
      my_real
     .   JACI1(MVSIZ), JACI2(MVSIZ), JACI3(MVSIZ),
     .   JACI4(MVSIZ), JACI5(MVSIZ), JACI6(MVSIZ),
     .   JACI7(MVSIZ), JACI8(MVSIZ), JACI9(MVSIZ),
     .   A1PR1(MVSIZ),A1PR3(MVSIZ),A1PR5(MVSIZ),A1PR7(MVSIZ),
     .   A4PR1(MVSIZ),A4PR3(MVSIZ),A4PR5(MVSIZ),A4PR7(MVSIZ),
     .   A7PR1(MVSIZ),A7PR3(MVSIZ),A7PR5(MVSIZ),A7PR7(MVSIZ),
     .   A2PS1(MVSIZ),A2PS2(MVSIZ),A2PS5(MVSIZ),A2PS6(MVSIZ),
     .   A5PS1(MVSIZ),A5PS2(MVSIZ),A5PS5(MVSIZ),A5PS6(MVSIZ),
     .   A8PS1(MVSIZ),A8PS2(MVSIZ),A8PS5(MVSIZ),A8PS6(MVSIZ),
     .   A3PT1(MVSIZ),A3PT2(MVSIZ),A3PT3(MVSIZ),A3PT4(MVSIZ),
     .   A6PT1(MVSIZ),A6PT2(MVSIZ),A6PT3(MVSIZ),A6PT4(MVSIZ),
     .   A9PT1(MVSIZ),A9PT2(MVSIZ),A9PT3(MVSIZ),A9PT4(MVSIZ),
     .   PR(8),PS(8),PT(8),RP, SP, TP, RM, SM, TM,
     .   PX1, PX2, PX3, PX4,PX5, PX6, PX7, PX8,
     .   PY1, PY2, PY3, PY4,PY5, PY6, PY7, PY8,
     .   PZ1, PZ2, PZ3, PZ4,PZ5, PZ6, PZ7, PZ8
C-----------------------------------------------
      DO I=LFT,LLT
       JAC1(I)=CJ1(I)+HX(3,I)*ETA+(HX(2,I)+HX(4,I)*ETA)*ZETA
       JAC2(I)=CJ2(I)+HY(3,I)*ETA+(HY(2,I)+HY(4,I)*ETA)*ZETA
       JAC3(I)=CJ3(I)+HZ(3,I)*ETA+(HZ(2,I)+HZ(4,I)*ETA)*ZETA
C
       JAC4(I)=CJ4(I)+HX(1,I)*ZETA+(HX(3,I)+HX(4,I)*ZETA)*KSI
       JAC5(I)=CJ5(I)+HY(1,I)*ZETA+(HY(3,I)+HY(4,I)*ZETA)*KSI
       JAC6(I)=CJ6(I)+HZ(1,I)*ZETA+(HZ(3,I)+HZ(4,I)*ZETA)*KSI
C
       JAC7(I)=CJ7(I)+HX(2,I)*KSI+(HX(1,I)+HX(4,I)*KSI)*ETA
       JAC8(I)=CJ8(I)+HY(2,I)*KSI+(HY(1,I)+HY(4,I)*KSI)*ETA
       JAC9(I)=CJ9(I)+HZ(2,I)*KSI+(HZ(1,I)+HZ(4,I)*KSI)*ETA
      ENDDO
C
C     JACOBIAN
C
      DO I=LFT,LLT
      JAC_59_68(I)=JAC5(I)*JAC9(I)-JAC6(I)*JAC8(I)
      JAC_67_49(I)=JAC6(I)*JAC7(I)-JAC4(I)*JAC9(I)
      JAC_38_29(I)=(-JAC2(I)*JAC9(I)+JAC3(I)*JAC8(I))
      JAC_19_37(I)=( JAC1(I)*JAC9(I)-JAC3(I)*JAC7(I))
      JAC_27_18(I)=(-JAC1(I)*JAC8(I)+JAC2(I)*JAC7(I))
      JAC_26_35(I)=( JAC2(I)*JAC6(I)-JAC3(I)*JAC5(I))
      JAC_34_16(I)=(-JAC1(I)*JAC6(I)+JAC3(I)*JAC4(I))
      JAC_15_24(I)=( JAC1(I)*JAC5(I)-JAC2(I)*JAC4(I))
      JAC_48_57(I)=JAC4(I)*JAC8(I)-JAC5(I)*JAC7(I)
      ENDDO
C
      DO I=LFT,LLT
      DET(I)=ONE_OVER_512*(JAC1(I)*JAC_59_68(I)+JAC2(I)*JAC_67_49(I)+JAC3(I)*JAC_48_57(I))
      DETT(I)=ONE_OVER_512/DET(I)
      ENDDO
C      
      IF (L_PIJ ==0) THEN
      DO I=LFT,LLT
       JAC_I(1,I)=DETT(I)*JAC_59_68(I)
       JAC_I(4,I)=DETT(I)*JAC_67_49(I)
       JAC_I(7,I)=DETT(I)*JAC_48_57(I)
       JAC_I(2,I)=DETT(I)*JAC_38_29(I)
       JAC_I(5,I)=DETT(I)*JAC_19_37(I)
       JAC_I(8,I)=DETT(I)*JAC_27_18(I)
       JAC_I(3,I)=DETT(I)*JAC_26_35(I)
       JAC_I(6,I)=DETT(I)*JAC_34_16(I)
       JAC_I(9,I)=DETT(I)*JAC_15_24(I)
       JAC_I(10,I)=DET(I)
      ENDDO
      ELSE
C--------calculate Pij----      
      DO I=LFT,LLT
       JACI1(I)=DETT(I)*JAC_59_68(I)
       JACI4(I)=DETT(I)*JAC_67_49(I)
       JACI7(I)=DETT(I)*JAC_48_57(I)
       JACI2(I)=DETT(I)*JAC_38_29(I)
       JACI5(I)=DETT(I)*JAC_19_37(I)
       JACI8(I)=DETT(I)*JAC_27_18(I)
       JACI3(I)=DETT(I)*JAC_26_35(I)
       JACI6(I)=DETT(I)*JAC_34_16(I)
       JACI9(I)=DETT(I)*JAC_15_24(I)
      ENDDO
C------NI,ksi-------
      RP = ONE + KSI
      SP = ONE + ETA
      TP = ONE + ZETA
      RM = ONE - KSI
      SM = ONE - ETA
      TM = ONE - ZETA
      PR(1)=-SM*TM
      PR(2)=-PR(1)
      PR(3)= SP*TM
      PR(4)=-PR(3)
      PR(5)=-SM*TP
      PR(6)=-PR(5)
      PR(7)= SP*TP
      PR(8)=-PR(7)
      PS(1)=-RM*TM
      PS(2)=-RP*TM
      PS(3)=-PS(2)
      PS(4)=-PS(1)
      PS(5)=-RM*TP
      PS(6)=-RP*TP
      PS(7)=-PS(6)
      PS(8)=-PS(5)
      PT(1)=-RM*SM
      PT(2)=-RP*SM
      PT(3)=-RP*SP
      PT(4)=-RM*SP
      PT(5)=-PT(1)
      PT(6)=-PT(2)
      PT(7)=-PT(3)
      PT(8)=-PT(4)
cc
      DO I=LFT,LLT
        A1PR1(I)=JACI1(I)*PR(1)
        A1PR3(I)=JACI1(I)*PR(3)
        A1PR5(I)=JACI1(I)*PR(5)
        A1PR7(I)=JACI1(I)*PR(7)
        A4PR1(I)=JACI4(I)*PR(1)
        A4PR3(I)=JACI4(I)*PR(3)
        A4PR5(I)=JACI4(I)*PR(5)
        A4PR7(I)=JACI4(I)*PR(7)
        A7PR1(I)=JACI7(I)*PR(1)
        A7PR3(I)=JACI7(I)*PR(3)
        A7PR5(I)=JACI7(I)*PR(5)
        A7PR7(I)=JACI7(I)*PR(7)
      ENDDO
      DO I=LFT,LLT
        A2PS1(I)=JACI2(I)*PS(1)
        A2PS2(I)=JACI2(I)*PS(2)
        A2PS5(I)=JACI2(I)*PS(5)
        A2PS6(I)=JACI2(I)*PS(6)
        A5PS1(I)=JACI5(I)*PS(1)
        A5PS2(I)=JACI5(I)*PS(2)
        A5PS5(I)=JACI5(I)*PS(5)
        A5PS6(I)=JACI5(I)*PS(6)
        A8PS1(I)=JACI8(I)*PS(1)
        A8PS2(I)=JACI8(I)*PS(2)
        A8PS5(I)=JACI8(I)*PS(5)
        A8PS6(I)=JACI8(I)*PS(6)
      ENDDO
C
      DO I=LFT,LLT
        A3PT1(I)=JACI3(I)*PT(1)
        A3PT2(I)=JACI3(I)*PT(2)
        A3PT3(I)=JACI3(I)*PT(3)
        A3PT4(I)=JACI3(I)*PT(4)
        A6PT1(I)=JACI6(I)*PT(1)
        A6PT2(I)=JACI6(I)*PT(2)
        A6PT3(I)=JACI6(I)*PT(3)
        A6PT4(I)=JACI6(I)*PT(4)
        A9PT1(I)=JACI9(I)*PT(1)
        A9PT2(I)=JACI9(I)*PT(2)
        A9PT3(I)=JACI9(I)*PT(3)
        A9PT4(I)=JACI9(I)*PT(4)
      ENDDO
C
      DO I=LFT,LLT
       PX1= A1PR1(I)+A2PS1(I)+A3PT1(I)       
       PX2=-A1PR1(I)+A2PS2(I)+A3PT2(I)
       PX3= A1PR3(I)-A2PS2(I)+A3PT3(I)
       PX4=-A1PR3(I)-A2PS1(I)+A3PT4(I)
       PX5= A1PR5(I)+A2PS5(I)-A3PT1(I)
       PX6=-A1PR5(I)+A2PS6(I)-A3PT2(I)
       PX7= A1PR7(I)-A2PS6(I)-A3PT3(I)
       PX8=-A1PR7(I)-A2PS5(I)-A3PT4(I)
C
       PY1= A4PR1(I)+A5PS1(I)+A6PT1(I)
       PY2=-A4PR1(I)+A5PS2(I)+A6PT2(I)
       PY3= A4PR3(I)-A5PS2(I)+A6PT3(I)
       PY4=-A4PR3(I)-A5PS1(I)+A6PT4(I)
       PY5= A4PR5(I)+A5PS5(I)-A6PT1(I)
       PY6=-A4PR5(I)+A5PS6(I)-A6PT2(I)
       PY7= A4PR7(I)-A5PS6(I)-A6PT3(I)
       PY8=-A4PR7(I)-A5PS5(I)-A6PT4(I)
C
       PZ1= A7PR1(I)+A8PS1(I)+A9PT1(I)
       PZ2=-A7PR1(I)+A8PS2(I)+A9PT2(I)
       PZ3= A7PR3(I)-A8PS2(I)+A9PT3(I)
       PZ4=-A7PR3(I)-A8PS1(I)+A9PT4(I)
       PZ5= A7PR5(I)+A8PS5(I)-A9PT1(I)
       PZ6=-A7PR5(I)+A8PS6(I)-A9PT2(I)
       PZ7= A7PR7(I)-A8PS6(I)-A9PT3(I)
       PZ8=-A7PR7(I)-A8PS5(I)-A9PT4(I)
C
       PIJ(I,1) = PX1
       PIJ(I,2) = PY1
       PIJ(I,3) = PZ1
       PIJ(I,4) = PX2
       PIJ(I,5) = PY2
       PIJ(I,6) = PZ2
       PIJ(I,7) = PX3
       PIJ(I,8) = PY3
       PIJ(I,9) = PZ3
       PIJ(I,10) = PX4
       PIJ(I,11) = PY4
       PIJ(I,12) = PZ4
       PIJ(I,13) = PX5
       PIJ(I,14) = PY5
       PIJ(I,15) = PZ5
       PIJ(I,16) = PX6
       PIJ(I,17) = PY6
       PIJ(I,18) = PZ6
       PIJ(I,19) = PX7
       PIJ(I,20) = PY7
       PIJ(I,21) = PZ7
       PIJ(I,22) = PX8
       PIJ(I,23) = PY8
       PIJ(I,24) = PZ8
      ENDDO
      END IF !(L_JAC >0) THEN
C
      RETURN
      END
Chd|====================================================================
Chd|  S8E_PIJ                       source/elements/solid/solide8z/s8zderi3.F
Chd|-- called by -----------
Chd|        S8ZINIT3                      source/elements/solid/solide8z/s8zinit3.F
Chd|-- calls ---------------
Chd|        S8EDERIL2G                    source/elements/solid/solide8z/s8zderi3.F
Chd|        S8EDERISH2                    source/elements/solid/solide8z/s8zderi3.F
Chd|        S8EDERI_BIJ                   source/elements/solid/solide8z/s8zderi3.F
Chd|        S8EGETPIJ                     source/elements/solid/solide8z/s8zderi3.F
Chd|        S8ESELECSH                    source/elements/solid/solide8z/s8zderi3.F
Chd|        SETMATRANS                    source/elements/solid/solide8z/s8zderi3.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/mat_elem/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE S8E_PIJ(NPTR,NPTS,NPTT,NNPT,NEL,
     .                   PXC1, PXC2, PXC3, PXC4, 
     .                   PYC1, PYC2, PYC3, PYC4, 
     .                   PZC1, PZC2, PZC3, PZC4, 
     .                  E1X  ,E1Y  ,E1Z  ,E2X  ,E2Y  ,E2Z  ,E3X  ,E3Y  ,E3Z  ,
     .                  NU   ,ELBUF_STR)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD            
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NPTR,NPTS,NPTT,NNPT,NEL
      my_real
     .   PXC1(*), PXC2(*), PXC3(*), PXC4(*), 
     .   PYC1(*), PYC2(*), PYC3(*), PYC4(*), 
     .   PZC1(*), PZC2(*), PZC3(*), PZC4(*), 
     .   E1X(MVSIZ),E1Y(MVSIZ),E1Z(MVSIZ),E2X(MVSIZ),
     .   E2Y(MVSIZ),E2Z(MVSIZ),E3X(MVSIZ),E3Y(MVSIZ),E3Z(MVSIZ),NU(*)
      TYPE(ELBUF_STRUCT_), TARGET :: ELBUF_STR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IP,IR,IS,IT,IL,i
C-----------------------------------------------
      TYPE(L_BUFEL_) ,POINTER :: LBUF     
      TYPE(G_BUFEL_) ,POINTER :: GBUF     
c-----------------------------------------------------
      my_real
     .   BXY1(MVSIZ),BXY2(MVSIZ),BXY3(MVSIZ),BXY4(MVSIZ),
     .   BXY5(MVSIZ),BXY6(MVSIZ),BXY7(MVSIZ),BXY8(MVSIZ),
     .   BYX1(MVSIZ),BYX2(MVSIZ),BYX3(MVSIZ),BYX4(MVSIZ),
     .   BYX5(MVSIZ),BYX6(MVSIZ),BYX7(MVSIZ),BYX8(MVSIZ),
     .   BXZ1(MVSIZ),BXZ2(MVSIZ),BXZ3(MVSIZ),BXZ4(MVSIZ),
     .   BXZ5(MVSIZ),BXZ6(MVSIZ),BXZ7(MVSIZ),BXZ8(MVSIZ),
     .   BZX1(MVSIZ),BZX2(MVSIZ),BZX3(MVSIZ),BZX4(MVSIZ),
     .   BZX5(MVSIZ),BZX6(MVSIZ),BZX7(MVSIZ),BZX8(MVSIZ),
     .   BYZ1(MVSIZ),BYZ2(MVSIZ),BYZ3(MVSIZ),BYZ4(MVSIZ),
     .   BYZ5(MVSIZ),BYZ6(MVSIZ),BYZ7(MVSIZ),BYZ8(MVSIZ),
     .   BZY1(MVSIZ),BZY2(MVSIZ),BZY3(MVSIZ),BZY4(MVSIZ),
     .   BZY5(MVSIZ),BZY6(MVSIZ),BZY7(MVSIZ),BZY8(MVSIZ),
     .   BXX1(MVSIZ),BXX2(MVSIZ),BXX3(MVSIZ),BXX4(MVSIZ),
     .   BXX5(MVSIZ),BXX6(MVSIZ),BXX7(MVSIZ),BXX8(MVSIZ),
     .   BYY1(MVSIZ),BYY2(MVSIZ),BYY3(MVSIZ),BYY4(MVSIZ),
     .   BYY5(MVSIZ),BYY6(MVSIZ),BYY7(MVSIZ),BYY8(MVSIZ),
     .   BZZ1(MVSIZ),BZZ2(MVSIZ),BZZ3(MVSIZ),BZZ4(MVSIZ),
     .   BZZ5(MVSIZ),BZZ6(MVSIZ),BZZ7(MVSIZ),BZZ8(MVSIZ)
      my_real
     .   PXY1(MVSIZ),PXY2(MVSIZ),PXY3(MVSIZ),PXY4(MVSIZ),
     .   PXY5(MVSIZ),PXY6(MVSIZ),PXY7(MVSIZ),PXY8(MVSIZ),
     .   PYX1(MVSIZ),PYX2(MVSIZ),PYX3(MVSIZ),PYX4(MVSIZ),
     .   PYX5(MVSIZ),PYX6(MVSIZ),PYX7(MVSIZ),PYX8(MVSIZ),
     .   PXZ1(MVSIZ),PXZ2(MVSIZ),PXZ3(MVSIZ),PXZ4(MVSIZ),
     .   PXZ5(MVSIZ),PXZ6(MVSIZ),PXZ7(MVSIZ),PXZ8(MVSIZ),
     .   PZX1(MVSIZ),PZX2(MVSIZ),PZX3(MVSIZ),PZX4(MVSIZ),
     .   PZX5(MVSIZ),PZX6(MVSIZ),PZX7(MVSIZ),PZX8(MVSIZ),
     .   PYZ1(MVSIZ),PYZ2(MVSIZ),PYZ3(MVSIZ),PYZ4(MVSIZ),
     .   PYZ5(MVSIZ),PYZ6(MVSIZ),PYZ7(MVSIZ),PYZ8(MVSIZ),
     .   PZY1(MVSIZ),PZY2(MVSIZ),PZY3(MVSIZ),PZY4(MVSIZ),
     .   PZY5(MVSIZ),PZY6(MVSIZ),PZY7(MVSIZ),PZY8(MVSIZ),
     .  PX1(MVSIZ,NNPT),PX2(MVSIZ,NNPT),PX3(MVSIZ,NNPT),PX4(MVSIZ,NNPT),
     .  PX5(MVSIZ,NNPT),PX6(MVSIZ,NNPT),PX7(MVSIZ,NNPT),PX8(MVSIZ,NNPT),
     .  PY1(MVSIZ,NNPT),PY2(MVSIZ,NNPT),PY3(MVSIZ,NNPT),PY4(MVSIZ,NNPT),
     .  PY5(MVSIZ,NNPT),PY6(MVSIZ,NNPT),PY7(MVSIZ,NNPT),PY8(MVSIZ,NNPT),
     .  PZ1(MVSIZ,NNPT),PZ2(MVSIZ,NNPT),PZ3(MVSIZ,NNPT),PZ4(MVSIZ,NNPT),
     .  PZ5(MVSIZ,NNPT),PZ6(MVSIZ,NNPT),PZ7(MVSIZ,NNPT),PZ8(MVSIZ,NNPT),
     .   P0XY1(MVSIZ,2),P0XY2(MVSIZ,2),P0XY3(MVSIZ,2),P0XY4(MVSIZ,2),
     .   P0XY5(MVSIZ,2),P0XY6(MVSIZ,2),P0XY7(MVSIZ,2),P0XY8(MVSIZ,2),
     .   P0YX1(MVSIZ,2),P0YX2(MVSIZ,2),P0YX3(MVSIZ,2),P0YX4(MVSIZ,2),
     .   P0YX5(MVSIZ,2),P0YX6(MVSIZ,2),P0YX7(MVSIZ,2),P0YX8(MVSIZ,2),
     .   P0XZ1(MVSIZ,2),P0XZ2(MVSIZ,2),P0XZ3(MVSIZ,2),P0XZ4(MVSIZ,2),
     .   P0XZ5(MVSIZ,2),P0XZ6(MVSIZ,2),P0XZ7(MVSIZ,2),P0XZ8(MVSIZ,2),
     .   P0ZX1(MVSIZ,2),P0ZX2(MVSIZ,2),P0ZX3(MVSIZ,2),P0ZX4(MVSIZ,2),
     .   P0ZX5(MVSIZ,2),P0ZX6(MVSIZ,2),P0ZX7(MVSIZ,2),P0ZX8(MVSIZ,2),
     .   P0YZ1(MVSIZ,2),P0YZ2(MVSIZ,2),P0YZ3(MVSIZ,2),P0YZ4(MVSIZ,2),
     .   P0YZ5(MVSIZ,2),P0YZ6(MVSIZ,2),P0YZ7(MVSIZ,2),P0YZ8(MVSIZ,2),
     .   P0ZY1(MVSIZ,2),P0ZY2(MVSIZ,2),P0ZY3(MVSIZ,2),P0ZY4(MVSIZ,2),
     .   P0ZY5(MVSIZ,2),P0ZY6(MVSIZ,2),P0ZY7(MVSIZ,2),P0ZY8(MVSIZ,2)
      my_real
     .   QC(MVSIZ,3,3),QGC(MVSIZ,3,3),QT(MVSIZ,3,3)
C-----------------------------------------------
      IL = 1
      GBUF => ELBUF_STR%GBUF
      LBUF => ELBUF_STR%BUFLY(IL)%LBUF(1,1,1)
        DO IR=1,NPTR
        DO IS=1,NPTS
        DO IT=1,NPTT
C-----------
          LBUF => ELBUF_STR%BUFLY(IL)%LBUF(IR,IS,IT)
         IP = IR + ( (IS-1) + (IT-1)*NPTS )*NPTR
C
         CALL S8EGETPIJ(
     .    PX1(1,IP), PX2(1,IP), PX3(1,IP), PX4(1,IP),
     .    PX5(1,IP), PX6(1,IP), PX7(1,IP), PX8(1,IP),
     .    PY1(1,IP), PY2(1,IP), PY3(1,IP), PY4(1,IP),
     .    PY5(1,IP), PY6(1,IP), PY7(1,IP), PY8(1,IP),
     .    PZ1(1,IP), PZ2(1,IP), PZ3(1,IP), PZ4(1,IP),
     .    PZ5(1,IP), PZ6(1,IP), PZ7(1,IP), PZ8(1,IP),
     .    LBUF%PIJ,LLT)
c
        ENDDO
        ENDDO
        ENDDO
C        
        CALL S8ESELECSH(NPTR,NPTS,NPTT,NNPT,
     .    PX1, PX2, PX3, PX4, PX5, PX6, PX7, PX8,
     .    PY1, PY2, PY3, PY4, PY5, PY6, PY7, PY8,
     .    PZ1, PZ2, PZ3, PZ4, PZ5, PZ6, PZ7, PZ8,
     .   P0XY1,P0XY2,P0XY3,P0XY4,P0XY5,P0XY6,P0XY7,P0XY8,
     .   P0YX1,P0YX2,P0YX3,P0YX4,P0YX5,P0YX6,P0YX7,P0YX8,
     .   P0XZ1,P0XZ2,P0XZ3,P0XZ4,P0XZ5,P0XZ6,P0XZ7,P0XZ8,
     .   P0ZX1,P0ZX2,P0ZX3,P0ZX4,P0ZX5,P0ZX6,P0ZX7,P0ZX8,
     .   P0YZ1,P0YZ2,P0YZ3,P0YZ4,P0YZ5,P0YZ6,P0YZ7,P0YZ8,
     .   P0ZY1,P0ZY2,P0ZY3,P0ZY4,P0ZY5,P0ZY6,P0ZY7,P0ZY8)
C--------conctruction trandfert matrix QQ,Q        
       CALL SETMATRANS(
     .                  E1X  ,E1Y  ,E1Z  ,E2X  ,E2Y  ,E2Z  ,E3X  ,E3Y  ,E3Z  ,
     .                  QT   ,QC   ,QGC  ) 
      DO IR=1,NPTR
       DO IS=1,NPTS
        DO IT=1,NPTT
          LBUF => ELBUF_STR%BUFLY(IL)%LBUF(IR,IS,IT)
         IP = IR + ( (IS-1) + (IT-1)*NPTS )*NPTR
          CALL S8EDERISH2(
     .   PXY1,PXY2,PXY3,PXY4,PXY5,PXY6,PXY7,PXY8,
     .   PYX1,PYX2,PYX3,PYX4,PYX5,PYX6,PYX7,PYX8,
     .   PXZ1,PXZ2,PXZ3,PXZ4,PXZ5,PXZ6,PXZ7,PXZ8,
     .   PZX1,PZX2,PZX3,PZX4,PZX5,PZX6,PZX7,PZX8,
     .   PYZ1,PYZ2,PYZ3,PYZ4,PYZ5,PYZ6,PYZ7,PYZ8,
     .   PZY1,PZY2,PZY3,PZY4,PZY5,PZY6,PZY7,PZY8,
     .   P0XY1(1,IR),P0XY2(1,IR),P0XY3(1,IR),P0XY4(1,IR),
     .   P0XY5(1,IR),P0XY6(1,IR),P0XY7(1,IR),P0XY8(1,IR),
     .   P0YX1(1,IR),P0YX2(1,IR),P0YX3(1,IR),P0YX4(1,IR),
     .   P0YX5(1,IR),P0YX6(1,IR),P0YX7(1,IR),P0YX8(1,IR),
     .   P0XZ1(1,IT),P0XZ2(1,IT),P0XZ3(1,IT),P0XZ4(1,IT),
     .   P0XZ5(1,IT),P0XZ6(1,IT),P0XZ7(1,IT),P0XZ8(1,IT),
     .   P0ZX1(1,IT),P0ZX2(1,IT),P0ZX3(1,IT),P0ZX4(1,IT),
     .   P0ZX5(1,IT),P0ZX6(1,IT),P0ZX7(1,IT),P0ZX8(1,IT),
     .   P0YZ1(1,IS),P0YZ2(1,IS),P0YZ3(1,IS),P0YZ4(1,IS),
     .   P0YZ5(1,IS),P0YZ6(1,IS),P0YZ7(1,IS),P0YZ8(1,IS),
     .   P0ZY1(1,IS),P0ZY2(1,IS),P0ZY3(1,IS),P0ZY4(1,IS),
     .   P0ZY5(1,IS),P0ZY6(1,IS),P0ZY7(1,IS),P0ZY8(1,IS))
         CALL S8EDERI_BIJ(
     .    PX1(1,IP), PX2(1,IP), PX3(1,IP), PX4(1,IP),
     .    PX5(1,IP), PX6(1,IP), PX7(1,IP), PX8(1,IP),
     .    PY1(1,IP), PY2(1,IP), PY3(1,IP), PY4(1,IP),
     .    PY5(1,IP), PY6(1,IP), PY7(1,IP), PY8(1,IP),
     .    PZ1(1,IP), PZ2(1,IP), PZ3(1,IP), PZ4(1,IP),
     .    PZ5(1,IP), PZ6(1,IP), PZ7(1,IP), PZ8(1,IP),
     .    PXC1, PXC2, PXC3, PXC4,
     .    PYC1, PYC2, PYC3, PYC4,
     .    PZC1, PZC2, PZC3, PZC4,
     .    PXY1,PXY2,PXY3,PXY4,PXY5,PXY6,PXY7,PXY8,
     .    PYX1,PYX2,PYX3,PYX4,PYX5,PYX6,PYX7,PYX8,
     .    PXZ1,PXZ2,PXZ3,PXZ4,PXZ5,PXZ6,PXZ7,PXZ8,
     .    PZX1,PZX2,PZX3,PZX4,PZX5,PZX6,PZX7,PZX8,
     .    PYZ1,PYZ2,PYZ3,PYZ4,PYZ5,PYZ6,PYZ7,PYZ8,
     .    PZY1,PZY2,PZY3,PZY4,PZY5,PZY6,PZY7,PZY8,
     .    BXY1, BXY2, BXY3, BXY4, BXY5, BXY6, BXY7, BXY8,
     .    BYX1, BYX2, BYX3, BYX4, BYX5, BYX6, BYX7, BYX8,
     .    BXZ1, BXZ2, BXZ3, BXZ4, BXZ5, BXZ6, BXZ7, BXZ8,
     .    BZX1, BZX2, BZX3, BZX4, BZX5, BZX6, BZX7, BZX8,
     .    BYZ1, BYZ2, BYZ3, BYZ4, BYZ5, BYZ6, BYZ7, BYZ8,
     .    BZY1, BZY2, BZY3, BZY4, BZY5, BZY6, BZY7, BZY8,
     .    NU  )
C ------------return to global system for PIJ and store in PIJ(1:24->PX,PY,PZ)    
         CALL S8EDERIL2G(
     .    PX1(1,IP), PX2(1,IP), PX3(1,IP), PX4(1,IP),
     .    PX5(1,IP), PX6(1,IP), PX7(1,IP), PX8(1,IP),
     .    PY1(1,IP), PY2(1,IP), PY3(1,IP), PY4(1,IP),
     .    PY5(1,IP), PY6(1,IP), PY7(1,IP), PY8(1,IP),
     .    PZ1(1,IP), PZ2(1,IP), PZ3(1,IP), PZ4(1,IP),
     .    PZ5(1,IP), PZ6(1,IP), PZ7(1,IP), PZ8(1,IP),
     .    PXY1,PXY2,PXY3,PXY4,PXY5,PXY6,PXY7,PXY8,
     .    PYX1,PYX2,PYX3,PYX4,PYX5,PYX6,PYX7,PYX8,
     .    PXZ1,PXZ2,PXZ3,PXZ4,PXZ5,PXZ6,PXZ7,PXZ8,
     .    PZX1,PZX2,PZX3,PZX4,PZX5,PZX6,PZX7,PZX8,
     .    PYZ1,PYZ2,PYZ3,PYZ4,PYZ5,PYZ6,PYZ7,PYZ8,
     .    PZY1,PZY2,PZY3,PZY4,PZY5,PZY6,PZY7,PZY8,
     .    BXY1, BXY2, BXY3, BXY4, BXY5, BXY6, BXY7, BXY8,
     .    BYX1, BYX2, BYX3, BYX4, BYX5, BYX6, BYX7, BYX8,
     .    BXZ1, BXZ2, BXZ3, BXZ4, BXZ5, BXZ6, BXZ7, BXZ8,
     .    BZX1, BZX2, BZX3, BZX4, BZX5, BZX6, BZX7, BZX8,
     .    BYZ1, BYZ2, BYZ3, BYZ4, BYZ5, BYZ6, BYZ7, BYZ8,
     .    BZY1, BZY2, BZY3, BZY4, BZY5, BZY6, BZY7, BZY8,
     .    QT  , QC  , QGC , LBUF%PIJ,LLT)
C
        ENDDO 
       ENDDO 
      ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  S8EGETPIJ                     source/elements/solid/solide8z/s8zderi3.F
Chd|-- called by -----------
Chd|        S8E_PIJ                       source/elements/solid/solide8z/s8zderi3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE S8EGETPIJ(
     .    PX1, PX2, PX3, PX4,
     .    PX5, PX6, PX7, PX8,
     .    PY1, PY2, PY3, PY4,
     .    PY5, PY6, PY7, PY8,
     .    PZ1, PZ2, PZ3, PZ4,
     .    PZ5, PZ6, PZ7, PZ8,
     .    PIJ,NEL)
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
      INTEGER NEL
C     REAL
      my_real
     .  PX1(MVSIZ),PX2(MVSIZ),PX3(MVSIZ),PX4(MVSIZ),
     .  PX5(MVSIZ),PX6(MVSIZ),PX7(MVSIZ),PX8(MVSIZ),
     .  PY1(MVSIZ),PY2(MVSIZ),PY3(MVSIZ),PY4(MVSIZ),
     .  PY5(MVSIZ),PY6(MVSIZ),PY7(MVSIZ),PY8(MVSIZ),
     .  PZ1(MVSIZ),PZ2(MVSIZ),PZ3(MVSIZ),PZ4(MVSIZ),
     .  PZ5(MVSIZ),PZ6(MVSIZ),PZ7(MVSIZ),PZ8(MVSIZ),
     .  PIJ(NEL,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K

C-----------------------------------------------
       DO I=1,NEL
        PX1(I) = PIJ(I,1) 
        PY1(I) = PIJ(I,2) 
        PZ1(I) = PIJ(I,3) 
        PX2(I) = PIJ(I,4) 
        PY2(I) = PIJ(I,5) 
        PZ2(I) = PIJ(I,6) 
        PX3(I) = PIJ(I,7) 
        PY3(I) = PIJ(I,8) 
        PZ3(I) = PIJ(I,9) 
        PX4(I) = PIJ(I,10) 
        PY4(I) = PIJ(I,11) 
        PZ4(I) = PIJ(I,12) 
        PX5(I) = PIJ(I,13) 
        PY5(I) = PIJ(I,14) 
        PZ5(I) = PIJ(I,15) 
        PX6(I) = PIJ(I,16) 
        PY6(I) = PIJ(I,17) 
        PZ6(I) = PIJ(I,18) 
        PX7(I) = PIJ(I,19) 
        PY7(I) = PIJ(I,20) 
        PZ7(I) = PIJ(I,21) 
        PX8(I) = PIJ(I,22) 
        PY8(I) = PIJ(I,23) 
        PZ8(I) = PIJ(I,24) 
       END DO
C-----------
      RETURN
      END
Chd|====================================================================
Chd|  S8ESELECSH                    source/elements/solid/solide8z/s8zderi3.F
Chd|-- called by -----------
Chd|        S8E_PIJ                       source/elements/solid/solide8z/s8zderi3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE S8ESELECSH(NPTR,NPTS,NPTT,NNPT,
     .    PX1, PX2, PX3, PX4, PX5, PX6, PX7, PX8,
     .    PY1, PY2, PY3, PY4, PY5, PY6, PY7, PY8,
     .    PZ1, PZ2, PZ3, PZ4, PZ5, PZ6, PZ7, PZ8,
     .   P0XY1,P0XY2,P0XY3,P0XY4,P0XY5,P0XY6,P0XY7,P0XY8,
     .   P0YX1,P0YX2,P0YX3,P0YX4,P0YX5,P0YX6,P0YX7,P0YX8,
     .   P0XZ1,P0XZ2,P0XZ3,P0XZ4,P0XZ5,P0XZ6,P0XZ7,P0XZ8,
     .   P0ZX1,P0ZX2,P0ZX3,P0ZX4,P0ZX5,P0ZX6,P0ZX7,P0ZX8,
     .   P0YZ1,P0YZ2,P0YZ3,P0YZ4,P0YZ5,P0YZ6,P0YZ7,P0YZ8,
     .   P0ZY1,P0ZY2,P0ZY3,P0ZY4,P0ZY5,P0ZY6,P0ZY7,P0ZY8)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NPTR,NPTS,NPTT,NNPT 
C     REAL
      my_real
     .  PX1(MVSIZ,NNPT),PX2(MVSIZ,NNPT),PX3(MVSIZ,NNPT),PX4(MVSIZ,NNPT),
     .  PX5(MVSIZ,NNPT),PX6(MVSIZ,NNPT),PX7(MVSIZ,NNPT),PX8(MVSIZ,NNPT),
     .  PY1(MVSIZ,NNPT),PY2(MVSIZ,NNPT),PY3(MVSIZ,NNPT),PY4(MVSIZ,NNPT),
     .  PY5(MVSIZ,NNPT),PY6(MVSIZ,NNPT),PY7(MVSIZ,NNPT),PY8(MVSIZ,NNPT),
     .  PZ1(MVSIZ,NNPT),PZ2(MVSIZ,NNPT),PZ3(MVSIZ,NNPT),PZ4(MVSIZ,NNPT),
     .  PZ5(MVSIZ,NNPT),PZ6(MVSIZ,NNPT),PZ7(MVSIZ,NNPT),PZ8(MVSIZ,NNPT),
     .   P0XY1(MVSIZ,2),P0XY2(MVSIZ,2),P0XY3(MVSIZ,2),P0XY4(MVSIZ,2),
     .   P0XY5(MVSIZ,2),P0XY6(MVSIZ,2),P0XY7(MVSIZ,2),P0XY8(MVSIZ,2),
     .   P0YX1(MVSIZ,2),P0YX2(MVSIZ,2),P0YX3(MVSIZ,2),P0YX4(MVSIZ,2),
     .   P0YX5(MVSIZ,2),P0YX6(MVSIZ,2),P0YX7(MVSIZ,2),P0YX8(MVSIZ,2),
     .   P0XZ1(MVSIZ,2),P0XZ2(MVSIZ,2),P0XZ3(MVSIZ,2),P0XZ4(MVSIZ,2),
     .   P0XZ5(MVSIZ,2),P0XZ6(MVSIZ,2),P0XZ7(MVSIZ,2),P0XZ8(MVSIZ,2),
     .   P0ZX1(MVSIZ,2),P0ZX2(MVSIZ,2),P0ZX3(MVSIZ,2),P0ZX4(MVSIZ,2),
     .   P0ZX5(MVSIZ,2),P0ZX6(MVSIZ,2),P0ZX7(MVSIZ,2),P0ZX8(MVSIZ,2),
     .   P0YZ1(MVSIZ,2),P0YZ2(MVSIZ,2),P0YZ3(MVSIZ,2),P0YZ4(MVSIZ,2),
     .   P0YZ5(MVSIZ,2),P0YZ6(MVSIZ,2),P0YZ7(MVSIZ,2),P0YZ8(MVSIZ,2),
     .   P0ZY1(MVSIZ,2),P0ZY2(MVSIZ,2),P0ZY3(MVSIZ,2),P0ZY4(MVSIZ,2),
     .   P0ZY5(MVSIZ,2),P0ZY6(MVSIZ,2),P0ZY7(MVSIZ,2),P0ZY8(MVSIZ,2)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J ,IR,IS,IT,IP,I1,I2,I3,I4
      INTEGER IXY(4,2),IXZ(4,2),IYZ(4,2)
      DATA IXY/1,5,3,7,2,6,4,8/
      DATA IXZ/1,3,2,4,5,7,6,8/
      DATA IYZ/1,5,2,6,3,7,4,8/
C------ moyen in 3 directions
      DO IR=1,NPTR
        I1 = IXY(1,IR)
        I2 = IXY(2,IR)
        I3 = IXY(3,IR)
        I4 = IXY(4,IR)
       DO I=LFT,LLT
       P0YX1(I,IR)=FOURTH*(PX1(I,I1)+PX1(I,I2)+PX1(I,I3)+PX1(I,I4))
       P0YX2(I,IR)=FOURTH*(PX2(I,I1)+PX2(I,I2)+PX2(I,I3)+PX2(I,I4))
       P0YX3(I,IR)=FOURTH*(PX3(I,I1)+PX3(I,I2)+PX3(I,I3)+PX3(I,I4))
       P0YX4(I,IR)=FOURTH*(PX4(I,I1)+PX4(I,I2)+PX4(I,I3)+PX4(I,I4))
       P0YX5(I,IR)=FOURTH*(PX5(I,I1)+PX5(I,I2)+PX5(I,I3)+PX5(I,I4))
       P0YX6(I,IR)=FOURTH*(PX6(I,I1)+PX6(I,I2)+PX6(I,I3)+PX6(I,I4))
       P0YX7(I,IR)=FOURTH*(PX7(I,I1)+PX7(I,I2)+PX7(I,I3)+PX7(I,I4))
       P0YX8(I,IR)=FOURTH*(PX8(I,I1)+PX8(I,I2)+PX8(I,I3)+PX8(I,I4))
       ENDDO
       DO I=LFT,LLT
       P0XY1(I,IR)=FOURTH*(PY1(I,I1)+PY1(I,I2)+PY1(I,I3)+PY1(I,I4))
       P0XY2(I,IR)=FOURTH*(PY2(I,I1)+PY2(I,I2)+PY2(I,I3)+PY2(I,I4))
       P0XY3(I,IR)=FOURTH*(PY3(I,I1)+PY3(I,I2)+PY3(I,I3)+PY3(I,I4))
       P0XY4(I,IR)=FOURTH*(PY4(I,I1)+PY4(I,I2)+PY4(I,I3)+PY4(I,I4))
       P0XY5(I,IR)=FOURTH*(PY5(I,I1)+PY5(I,I2)+PY5(I,I3)+PY5(I,I4))
       P0XY6(I,IR)=FOURTH*(PY6(I,I1)+PY6(I,I2)+PY6(I,I3)+PY6(I,I4))
       P0XY7(I,IR)=FOURTH*(PY7(I,I1)+PY7(I,I2)+PY7(I,I3)+PY7(I,I4))
       P0XY8(I,IR)=FOURTH*(PY8(I,I1)+PY8(I,I2)+PY8(I,I3)+PY8(I,I4))
       ENDDO
      ENDDO
C      
      DO IT=1,NPTT
        I1 = IXZ(1,IT)
        I2 = IXZ(2,IT)
        I3 = IXZ(3,IT)
        I4 = IXZ(4,IT)
       DO I=LFT,LLT
       P0ZX1(I,IT)=FOURTH*(PX1(I,I1)+PX1(I,I2)+PX1(I,I3)+PX1(I,I4))
       P0ZX2(I,IT)=FOURTH*(PX2(I,I1)+PX2(I,I2)+PX2(I,I3)+PX2(I,I4))
       P0ZX3(I,IT)=FOURTH*(PX3(I,I1)+PX3(I,I2)+PX3(I,I3)+PX3(I,I4))
       P0ZX4(I,IT)=FOURTH*(PX4(I,I1)+PX4(I,I2)+PX4(I,I3)+PX4(I,I4))
       P0ZX5(I,IT)=FOURTH*(PX5(I,I1)+PX5(I,I2)+PX5(I,I3)+PX5(I,I4))
       P0ZX6(I,IT)=FOURTH*(PX6(I,I1)+PX6(I,I2)+PX6(I,I3)+PX6(I,I4))
       P0ZX7(I,IT)=FOURTH*(PX7(I,I1)+PX7(I,I2)+PX7(I,I3)+PX7(I,I4))
       P0ZX8(I,IT)=FOURTH*(PX8(I,I1)+PX8(I,I2)+PX8(I,I3)+PX8(I,I4))
       ENDDO
       DO I=LFT,LLT
       P0XZ1(I,IT)=FOURTH*(PZ1(I,I1)+PZ1(I,I2)+PZ1(I,I3)+PZ1(I,I4))
       P0XZ2(I,IT)=FOURTH*(PZ2(I,I1)+PZ2(I,I2)+PZ2(I,I3)+PZ2(I,I4))
       P0XZ3(I,IT)=FOURTH*(PZ3(I,I1)+PZ3(I,I2)+PZ3(I,I3)+PZ3(I,I4))
       P0XZ4(I,IT)=FOURTH*(PZ4(I,I1)+PZ4(I,I2)+PZ4(I,I3)+PZ4(I,I4))
       P0XZ5(I,IT)=FOURTH*(PZ5(I,I1)+PZ5(I,I2)+PZ5(I,I3)+PZ5(I,I4))
       P0XZ6(I,IT)=FOURTH*(PZ6(I,I1)+PZ6(I,I2)+PZ6(I,I3)+PZ6(I,I4))
       P0XZ7(I,IT)=FOURTH*(PZ7(I,I1)+PZ7(I,I2)+PZ7(I,I3)+PZ7(I,I4))
       P0XZ8(I,IT)=FOURTH*(PZ8(I,I1)+PZ8(I,I2)+PZ8(I,I3)+PZ8(I,I4))
       ENDDO
      ENDDO
C     
      DO IS=1,NPTS
        I1 = IYZ(1,IS)
        I2 = IYZ(2,IS)
        I3 = IYZ(3,IS)
        I4 = IYZ(4,IS)
       DO I=LFT,LLT
       P0ZY1(I,IS)=FOURTH*(PY1(I,I1)+PY1(I,I2)+PY1(I,I3)+PY1(I,I4))
       P0ZY2(I,IS)=FOURTH*(PY2(I,I1)+PY2(I,I2)+PY2(I,I3)+PY2(I,I4))
       P0ZY3(I,IS)=FOURTH*(PY3(I,I1)+PY3(I,I2)+PY3(I,I3)+PY3(I,I4))
       P0ZY4(I,IS)=FOURTH*(PY4(I,I1)+PY4(I,I2)+PY4(I,I3)+PY4(I,I4))
       P0ZY5(I,IS)=FOURTH*(PY5(I,I1)+PY5(I,I2)+PY5(I,I3)+PY5(I,I4))
       P0ZY6(I,IS)=FOURTH*(PY6(I,I1)+PY6(I,I2)+PY6(I,I3)+PY6(I,I4))
       P0ZY7(I,IS)=FOURTH*(PY7(I,I1)+PY7(I,I2)+PY7(I,I3)+PY7(I,I4))
       P0ZY8(I,IS)=FOURTH*(PY8(I,I1)+PY8(I,I2)+PY8(I,I3)+PY8(I,I4))
       ENDDO
       DO I=LFT,LLT
       P0YZ1(I,IS)=FOURTH*(PZ1(I,I1)+PZ1(I,I2)+PZ1(I,I3)+PZ1(I,I4))
       P0YZ2(I,IS)=FOURTH*(PZ2(I,I1)+PZ2(I,I2)+PZ2(I,I3)+PZ2(I,I4))
       P0YZ3(I,IS)=FOURTH*(PZ3(I,I1)+PZ3(I,I2)+PZ3(I,I3)+PZ3(I,I4))
       P0YZ4(I,IS)=FOURTH*(PZ4(I,I1)+PZ4(I,I2)+PZ4(I,I3)+PZ4(I,I4))
       P0YZ5(I,IS)=FOURTH*(PZ5(I,I1)+PZ5(I,I2)+PZ5(I,I3)+PZ5(I,I4))
       P0YZ6(I,IS)=FOURTH*(PZ6(I,I1)+PZ6(I,I2)+PZ6(I,I3)+PZ6(I,I4))
       P0YZ7(I,IS)=FOURTH*(PZ7(I,I1)+PZ7(I,I2)+PZ7(I,I3)+PZ7(I,I4))
       P0YZ8(I,IS)=FOURTH*(PZ8(I,I1)+PZ8(I,I2)+PZ8(I,I3)+PZ8(I,I4))
       ENDDO
      ENDDO
C
      RETURN
       END
Chd|====================================================================
Chd|  S8EDERISH2                    source/elements/solid/solide8z/s8zderi3.F
Chd|-- called by -----------
Chd|        S8E_PIJ                       source/elements/solid/solide8z/s8zderi3.F
Chd|-- calls ---------------
Chd|        S8EA2BP8                      source/elements/solid/solide8z/s8zderi3.F
Chd|====================================================================
      SUBROUTINE S8EDERISH2(
     .   PXY1,PXY2,PXY3,PXY4,PXY5,PXY6,PXY7,PXY8,
     .   PYX1,PYX2,PYX3,PYX4,PYX5,PYX6,PYX7,PYX8,
     .   PXZ1,PXZ2,PXZ3,PXZ4,PXZ5,PXZ6,PXZ7,PXZ8,
     .   PZX1,PZX2,PZX3,PZX4,PZX5,PZX6,PZX7,PZX8,
     .   PYZ1,PYZ2,PYZ3,PYZ4,PYZ5,PYZ6,PYZ7,PYZ8,
     .   PZY1,PZY2,PZY3,PZY4,PZY5,PZY6,PZY7,PZY8,
     .   P0XY1,P0XY2,P0XY3,P0XY4,P0XY5,P0XY6,P0XY7,P0XY8,
     .   P0YX1,P0YX2,P0YX3,P0YX4,P0YX5,P0YX6,P0YX7,P0YX8,
     .   P0XZ1,P0XZ2,P0XZ3,P0XZ4,P0XZ5,P0XZ6,P0XZ7,P0XZ8,
     .   P0ZX1,P0ZX2,P0ZX3,P0ZX4,P0ZX5,P0ZX6,P0ZX7,P0ZX8,
     .   P0YZ1,P0YZ2,P0YZ3,P0YZ4,P0YZ5,P0YZ6,P0YZ7,P0YZ8,
     .   P0ZY1,P0ZY2,P0ZY3,P0ZY4,P0ZY5,P0ZY6,P0ZY7,P0ZY8)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL
      my_real
     .   PXY1(*),PXY2(*),PXY3(*),PXY4(*),
     .   PXY5(*),PXY6(*),PXY7(*),PXY8(*),
     .   PYX1(*),PYX2(*),PYX3(*),PYX4(*),
     .   PYX5(*),PYX6(*),PYX7(*),PYX8(*),
     .   PXZ1(*),PXZ2(*),PXZ3(*),PXZ4(*),
     .   PXZ5(*),PXZ6(*),PXZ7(*),PXZ8(*),
     .   PZX1(*),PZX2(*),PZX3(*),PZX4(*),
     .   PZX5(*),PZX6(*),PZX7(*),PZX8(*),
     .   PYZ1(*),PYZ2(*),PYZ3(*),PYZ4(*),
     .   PYZ5(*),PYZ6(*),PYZ7(*),PYZ8(*),
     .   PZY1(*),PZY2(*),PZY3(*),PZY4(*),
     .   PZY5(*),PZY6(*),PZY7(*),PZY8(*),
     .   P0XY1(*),P0XY2(*),P0XY3(*),P0XY4(*),
     .   P0XY5(*),P0XY6(*),P0XY7(*),P0XY8(*),
     .   P0YX1(*),P0YX2(*),P0YX3(*),P0YX4(*),
     .   P0YX5(*),P0YX6(*),P0YX7(*),P0YX8(*),
     .   P0XZ1(*),P0XZ2(*),P0XZ3(*),P0XZ4(*),
     .   P0XZ5(*),P0XZ6(*),P0XZ7(*),P0XZ8(*),
     .   P0ZX1(*),P0ZX2(*),P0ZX3(*),P0ZX4(*),
     .   P0ZX5(*),P0ZX6(*),P0ZX7(*),P0ZX8(*),
     .   P0YZ1(*),P0YZ2(*),P0YZ3(*),P0YZ4(*),
     .   P0YZ5(*),P0YZ6(*),P0YZ7(*),P0YZ8(*),
     .   P0ZY1(*),P0ZY2(*),P0ZY3(*),P0ZY4(*),
     .   P0ZY5(*),P0ZY6(*),P0ZY7(*),P0ZY8(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J 
C                                                                     12
          CALL S8EA2BP8(
     .    PXY1, PXY2, PXY3, PXY4, PXY5, PXY6, PXY7, PXY8,
     .    P0XY1, P0XY2, P0XY3, P0XY4,
     .    P0XY5, P0XY6, P0XY7, P0XY8)
          CALL S8EA2BP8(
     .    PYX1, PYX2, PYX3, PYX4, PYX5, PYX6, PYX7, PYX8,
     .    P0YX1,P0YX2,P0YX3,P0YX4,
     .    P0YX5,P0YX6,P0YX7,P0YX8)
          CALL S8EA2BP8(
     .    PXZ1, PXZ2, PXZ3, PXZ4, PXZ5, PXZ6, PXZ7, PXZ8,
     .    P0XZ1,P0XZ2,P0XZ3,P0XZ4,
     .    P0XZ5,P0XZ6,P0XZ7,P0XZ8)
          CALL S8EA2BP8(
     .    PZX1, PZX2, PZX3, PZX4, PZX5, PZX6, PZX7, PZX8,
     .    P0ZX1,P0ZX2,P0ZX3,P0ZX4,
     .    P0ZX5,P0ZX6,P0ZX7,P0ZX8)
          CALL S8EA2BP8(
     .    PYZ1, PYZ2, PYZ3, PYZ4, PYZ5, PYZ6, PYZ7, PYZ8,
     .    P0YZ1,P0YZ2,P0YZ3,P0YZ4,
     .    P0YZ5,P0YZ6,P0YZ7,P0YZ8)
          CALL S8EA2BP8(
     .    PZY1, PZY2, PZY3, PZY4, PZY5, PZY6, PZY7, PZY8,
     .    P0ZY1,P0ZY2,P0ZY3,P0ZY4,
     .    P0ZY5,P0ZY6,P0ZY7,P0ZY8)
C
      RETURN
       END
Chd|====================================================================
Chd|  S8EA2BP8                      source/elements/solid/solide8z/s8zderi3.F
Chd|-- called by -----------
Chd|        S8EDERISH2                    source/elements/solid/solide8z/s8zderi3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE S8EA2BP8(
     .    PX1, PX2, PX3, PX4, PX5, PX6, PX7, PX8,
     .    P0X1, P0X2, P0X3, P0X4, P0X5, P0X6, P0X7, P0X8)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL
      my_real
     .   PX1(*), PX2(*), PX3(*), PX4(*),
     .   PX5(*), PX6(*), PX7(*), PX8(*),
     .   P0X1(*), P0X2(*), P0X3(*), P0X4(*),
     .   P0X5(*), P0X6(*), P0X7(*), P0X8(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C      
        PX1(LFT:LLT) = P0X1(LFT:LLT)  
        PX2(LFT:LLT) = P0X2(LFT:LLT)  
        PX3(LFT:LLT) = P0X3(LFT:LLT)  
        PX4(LFT:LLT) = P0X4(LFT:LLT)  
        PX5(LFT:LLT) = P0X5(LFT:LLT)  
        PX6(LFT:LLT) = P0X6(LFT:LLT)  
        PX7(LFT:LLT) = P0X7(LFT:LLT)  
        PX8(LFT:LLT) = P0X8(LFT:LLT)  
C
      RETURN
       END
Chd|====================================================================
Chd|  S8EDERI_BIJ                   source/elements/solid/solide8z/s8zderi3.F
Chd|-- called by -----------
Chd|        S8E_PIJ                       source/elements/solid/solide8z/s8zderi3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE S8EDERI_BIJ(
     .    PX1, PX2, PX3, PX4, PX5, PX6, PX7, PX8,
     .    PY1, PY2, PY3, PY4, PY5, PY6, PY7, PY8,
     .    PZ1, PZ2, PZ3, PZ4, PZ5, PZ6, PZ7, PZ8,
     .    PXC1, PXC2, PXC3, PXC4,
     .    PYC1, PYC2, PYC3, PYC4,
     .    PZC1, PZC2, PZC3, PZC4,
     .    PXY1,PXY2,PXY3,PXY4,PXY5,PXY6,PXY7,PXY8,
     .    PYX1,PYX2,PYX3,PYX4,PYX5,PYX6,PYX7,PYX8,
     .    PXZ1,PXZ2,PXZ3,PXZ4,PXZ5,PXZ6,PXZ7,PXZ8,
     .    PZX1,PZX2,PZX3,PZX4,PZX5,PZX6,PZX7,PZX8,
     .    PYZ1,PYZ2,PYZ3,PYZ4,PYZ5,PYZ6,PYZ7,PYZ8,
     .    PZY1,PZY2,PZY3,PZY4,PZY5,PZY6,PZY7,PZY8,
     .    BXY1, BXY2, BXY3, BXY4, BXY5, BXY6, BXY7, BXY8,
     .    BYX1, BYX2, BYX3, BYX4, BYX5, BYX6, BYX7, BYX8,
     .    BXZ1, BXZ2, BXZ3, BXZ4, BXZ5, BXZ6, BXZ7, BXZ8,
     .    BZX1, BZX2, BZX3, BZX4, BZX5, BZX6, BZX7, BZX8,
     .    BYZ1, BYZ2, BYZ3, BYZ4, BYZ5, BYZ6, BYZ7, BYZ8,
     .    BZY1, BZY2, BZY3, BZY4, BZY5, BZY6, BZY7, BZY8,
     .    NU  )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL
      my_real
     .   PX1(*), PX2(*), PX3(*), PX4(*),
     .   PX5(*), PX6(*), PX7(*), PX8(*),
     .   PY1(*), PY2(*), PY3(*), PY4(*),
     .   PY5(*), PY6(*), PY7(*), PY8(*),
     .   PZ1(*), PZ2(*), PZ3(*), PZ4(*), 
     .   PZ5(*), PZ6(*), PZ7(*), PZ8(*),
     .   PXY1(*),PXY2(*),PXY3(*),PXY4(*),
     .   PXY5(*),PXY6(*),PXY7(*),PXY8(*),
     .   PYX1(*),PYX2(*),PYX3(*),PYX4(*),
     .   PYX5(*),PYX6(*),PYX7(*),PYX8(*),
     .   PXZ1(*),PXZ2(*),PXZ3(*),PXZ4(*),
     .   PXZ5(*),PXZ6(*),PXZ7(*),PXZ8(*),
     .   PZX1(*),PZX2(*),PZX3(*),PZX4(*),
     .   PZX5(*),PZX6(*),PZX7(*),PZX8(*),
     .   PYZ1(*),PYZ2(*),PYZ3(*),PYZ4(*),
     .   PYZ5(*),PYZ6(*),PYZ7(*),PYZ8(*),
     .   PZY1(*),PZY2(*),PZY3(*),PZY4(*),
     .   PZY5(*),PZY6(*),PZY7(*),PZY8(*),
     .   PXC1(*), PXC2(*), PXC3(*), PXC4(*),
     .   PYC1(*), PYC2(*), PYC3(*), PYC4(*),
     .   PZC1(*), PZC2(*), PZC3(*), PZC4(*),
     .   BXY1(*),BXY2(*),BXY3(*),BXY4(*),
     .   BXY5(*),BXY6(*),BXY7(*),BXY8(*),
     .   BYX1(*),BYX2(*),BYX3(*),BYX4(*),
     .   BYX5(*),BYX6(*),BYX7(*),BYX8(*),
     .   BXZ1(*),BXZ2(*),BXZ3(*),BXZ4(*),
     .   BXZ5(*),BXZ6(*),BXZ7(*),BXZ8(*),
     .   BZX1(*),BZX2(*),BZX3(*),BZX4(*),
     .   BZX5(*),BZX6(*),BZX7(*),BZX8(*),
     .   BYZ1(*),BYZ2(*),BYZ3(*),BYZ4(*),
     .   BYZ5(*),BYZ6(*),BYZ7(*),BYZ8(*),
     .   BZY1(*),BZY2(*),BZY3(*),BZY4(*),
     .   BZY5(*),BZY6(*),BZY7(*),BZY8(*),
     .   NU(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  I, J ,ICOR
C     REAL
C                                                                     12
      my_real
     .   NU1(MVSIZ),X1,X3,Y1,Y2,Z2,Z3,X24,Y34,Z14
C-----------------------------------------------
C---PX_I=X1234_I+PXC_I; PYX_I=X23_I+PXC_I; PZX_I=X12_I+PXC_I
C-----X2_I=0->X1_I = X12_I ; X3_I = X23_I X24_I =X1234_I-X1_I-X3_I
C-----verifie valeur de X4_I otherwise to be calculated
C---PY_I=Y1234_I+PYC_I; PXY_I=Y23_I+PYC_I; PZY_I=Y13_I+PYC_I
C-----Y3_I=0->Y1_I = Y13_I ; Y2_I = Y23_I Y34_I =Y1234_I-Y1_I-Y2_I
C---PZ_I=Z1234_I+PZC_I; PXZ_I=Z12_I+PZC_I; PYZ_I=Z13_I+PZC_I
C-----Z1_I=0->Z2_I = Z12_I ; Z3_I = Z13_I Z14_I =Z1234_I-Z2_I-Z3_I
C-------|0 BYX BZX |
C-------|BXY 0 BZY |  same than H8A
C-------|BXZ BYZ 0 |
C-BXY_I=-nu1*X1_I-nu*X24_I
C-BXZ_I=-nu1*X3_I-nu*X24_I
C-BYX_I=-nu1*Y1_I-nu*Y34_I
C-BYZ_I=-nu1*Y2_I-nu*Y34_I
C-BZX_I=-nu1*Z2_I-nu*Z14_I
C-BZY_I=-nu1*Z3_I-nu*Z14_I
      DO I=LFT,LLT
       NU1(I) = NU(I)/(ONE - NU(I))
      ENDDO
      DO I=LFT,LLT
       X1 = PZX1(I)-PXC1(I)
       X3 = PYX1(I)-PXC1(I)
       Y1 = PZY1(I)-PYC1(I)
       Y2 = PXY1(I)-PYC1(I)
       Z2 = PXZ1(I)-PZC1(I)
       Z3 = PYZ1(I)-PZC1(I)
       X24= NU(I)*(PX1(I)-PXC1(I)-X1-X3)
       Y34= NU(I)*(PY1(I)-PYC1(I)-Y1-Y2)
       Z14= NU(I)*(PZ1(I)-PZC1(I)-Z2-Z3)
       BXY1(I)=-NU1(I)*X1-X24
       BXZ1(I)=-NU1(I)*X3-X24
       BYX1(I)=-NU1(I)*Y1-Y34
       BYZ1(I)=-NU1(I)*Y2-Y34
       BZX1(I)=-NU1(I)*Z3-Z14
       BZY1(I)=-NU1(I)*Z2-Z14
      ENDDO
C      
      DO I=LFT,LLT
       X1 = PZX2(I)-PXC2(I)
       X3 = PYX2(I)-PXC2(I)
       Y1 = PZY2(I)-PYC2(I)
       Y2 = PXY2(I)-PYC2(I)
       Z2 = PXZ2(I)-PZC2(I)
       Z3 = PYZ2(I)-PZC2(I)
       X24= NU(I)*(PX2(I)-PXC2(I)-X1-X3)
       Y34= NU(I)*(PY2(I)-PYC2(I)-Y1-Y2)
       Z14= NU(I)*(PZ2(I)-PZC2(I)-Z2-Z3)
       BXY2(I)=-NU1(I)*X1-X24
       BXZ2(I)=-NU1(I)*X3-X24
       BYX2(I)=-NU1(I)*Y1-Y34
       BYZ2(I)=-NU1(I)*Y2-Y34
       BZX2(I)=-NU1(I)*Z3-Z14
       BZY2(I)=-NU1(I)*Z2-Z14
      ENDDO
C      
      DO I=LFT,LLT
       X1 = PZX3(I)-PXC3(I)
       X3 = PYX3(I)-PXC3(I)
       Y1 = PZY3(I)-PYC3(I)
       Y2 = PXY3(I)-PYC3(I)
       Z2 = PXZ3(I)-PZC3(I)
       Z3 = PYZ3(I)-PZC3(I)
       X24= NU(I)*(PX3(I)-PXC3(I)-X1-X3)
       Y34= NU(I)*(PY3(I)-PYC3(I)-Y1-Y2)
       Z14= NU(I)*(PZ3(I)-PZC3(I)-Z2-Z3)
       BXY3(I)=-NU1(I)*X1-X24
       BXZ3(I)=-NU1(I)*X3-X24
       BYX3(I)=-NU1(I)*Y1-Y34
       BYZ3(I)=-NU1(I)*Y2-Y34
       BZX3(I)=-NU1(I)*Z3-Z14
       BZY3(I)=-NU1(I)*Z2-Z14
      ENDDO
C      
      DO I=LFT,LLT
       X1 = PZX4(I)-PXC4(I)
       X3 = PYX4(I)-PXC4(I)
       Y1 = PZY4(I)-PYC4(I)
       Y2 = PXY4(I)-PYC4(I)
       Z2 = PXZ4(I)-PZC4(I)
       Z3 = PYZ4(I)-PZC4(I)
       X24= NU(I)*(PX4(I)-PXC4(I)-X1-X3)
       Y34= NU(I)*(PY4(I)-PYC4(I)-Y1-Y2)
       Z14= NU(I)*(PZ4(I)-PZC4(I)-Z2-Z3)
       BXY4(I)=-NU1(I)*X1-X24
       BXZ4(I)=-NU1(I)*X3-X24
       BYX4(I)=-NU1(I)*Y1-Y34
       BYZ4(I)=-NU1(I)*Y2-Y34
       BZX4(I)=-NU1(I)*Z3-Z14
       BZY4(I)=-NU1(I)*Z2-Z14
      ENDDO
C      
      DO I=LFT,LLT
       X1 = PZX5(I)+PXC3(I)
       X3 = PYX5(I)+PXC3(I)
       Y1 = PZY5(I)+PYC3(I)
       Y2 = PXY5(I)+PYC3(I)
       Z2 = PXZ5(I)+PZC3(I)
       Z3 = PYZ5(I)+PZC3(I)
       X24= NU(I)*(PX5(I)+PXC3(I)-X1-X3)
       Y34= NU(I)*(PY5(I)+PYC3(I)-Y1-Y2)
       Z14= NU(I)*(PZ5(I)+PZC3(I)-Z2-Z3)
       BXY5(I)=-NU1(I)*X1-X24
       BXZ5(I)=-NU1(I)*X3-X24
       BYX5(I)=-NU1(I)*Y1-Y34
       BYZ5(I)=-NU1(I)*Y2-Y34
       BZX5(I)=-NU1(I)*Z3-Z14
       BZY5(I)=-NU1(I)*Z2-Z14
      ENDDO
C      
      DO I=LFT,LLT
       X1 = PZX6(I)+PXC4(I)
       X3 = PYX6(I)+PXC4(I)
       Y1 = PZY6(I)+PYC4(I)
       Y2 = PXY6(I)+PYC4(I)
       Z2 = PXZ6(I)+PZC4(I)
       Z3 = PYZ6(I)+PZC4(I)
       X24= NU(I)*(PX6(I)+PXC4(I)-X1-X3)
       Y34= NU(I)*(PY6(I)+PYC4(I)-Y1-Y2)
       Z14= NU(I)*(PZ6(I)+PZC4(I)-Z2-Z3)
       BXY6(I)=-NU1(I)*X1-X24
       BXZ6(I)=-NU1(I)*X3-X24
       BYX6(I)=-NU1(I)*Y1-Y34
       BYZ6(I)=-NU1(I)*Y2-Y34
       BZX6(I)=-NU1(I)*Z3-Z14
       BZY6(I)=-NU1(I)*Z2-Z14
      ENDDO
C      
      DO I=LFT,LLT
       X1 = PZX7(I)+PXC1(I)
       X3 = PYX7(I)+PXC1(I)
       Y1 = PZY7(I)+PYC1(I)
       Y2 = PXY7(I)+PYC1(I)
       Z2 = PXZ7(I)+PZC1(I)
       Z3 = PYZ7(I)+PZC1(I)
       X24= NU(I)*(PX7(I)+PXC1(I)-X1-X3)
       Y34= NU(I)*(PY7(I)+PYC1(I)-Y1-Y2)
       Z14= NU(I)*(PZ7(I)+PZC1(I)-Z2-Z3)
       BXY7(I)=-NU1(I)*X1-X24
       BXZ7(I)=-NU1(I)*X3-X24
       BYX7(I)=-NU1(I)*Y1-Y34
       BYZ7(I)=-NU1(I)*Y2-Y34
       BZX7(I)=-NU1(I)*Z3-Z14
       BZY7(I)=-NU1(I)*Z2-Z14
      ENDDO
C      
      DO I=LFT,LLT
       X1 = PZX8(I)+PXC2(I)
       X3 = PYX8(I)+PXC2(I)
       Y1 = PZY8(I)+PYC2(I)
       Y2 = PXY8(I)+PYC2(I)
       Z2 = PXZ8(I)+PZC2(I)
       Z3 = PYZ8(I)+PZC2(I)
       X24= NU(I)*(PX8(I)+PXC2(I)-X1-X3)
       Y34= NU(I)*(PY8(I)+PYC2(I)-Y1-Y2)
       Z14= NU(I)*(PZ8(I)+PZC2(I)-Z2-Z3)
       BXY8(I)=-NU1(I)*X1-X24
       BXZ8(I)=-NU1(I)*X3-X24
       BYX8(I)=-NU1(I)*Y1-Y34
       BYZ8(I)=-NU1(I)*Y2-Y34
       BZX8(I)=-NU1(I)*Z3-Z14
       BZY8(I)=-NU1(I)*Z2-Z14
      ENDDO
C      
      RETURN
      END
Chd|====================================================================
Chd|  SETMATRANS                    source/elements/solid/solide8z/s8zderi3.F
Chd|-- called by -----------
Chd|        S8E_PIJ                       source/elements/solid/solide8z/s8zderi3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SETMATRANS(
     .                    E1X  ,E1Y  ,E1Z  ,E2X  ,E2Y  ,E2Z  ,E3X  ,E3Y  ,E3Z  ,
     .                    QT   ,QC   ,QGC  ) 
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL
      my_real
     .   E1X(MVSIZ),E1Y(MVSIZ),E1Z(MVSIZ),E2X(MVSIZ),
     .   E2Y(MVSIZ),E2Z(MVSIZ),E3X(MVSIZ),E3Y(MVSIZ),E3Z(MVSIZ),
     .   QT(MVSIZ,3,3),QC(MVSIZ,3,3),QGC(MVSIZ,3,3)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K
C     REAL
      my_real
     .   E1(MVSIZ,3),E2(MVSIZ,3),E3(MVSIZ,3)
C-----------------------------------------------
       DO I=LFT,LLT
         QT(I,1,1)=E1X(I)
         QT(I,2,1)=E2X(I)
         QT(I,3,1)=E3X(I)
         QT(I,1,2)=E1Y(I)
         QT(I,2,2)=E2Y(I)
         QT(I,3,2)=E3Y(I)
         QT(I,1,3)=E1Z(I)
         QT(I,2,3)=E2Z(I)
         QT(I,3,3)=E3Z(I)
       ENDDO
       DO J= 1,3
        DO I=LFT,LLT
          E1(I,J)=QT(I,1,J)
          E2(I,J)=QT(I,2,J)
          E3(I,J)=QT(I,3,J)
        ENDDO
       ENDDO
C
       DO J= 1,3
        DO I=LFT,LLT
         QC(I,1,J)=E1(I,J)*E1(I,J)
         QC(I,2,J)=E2(I,J)*E2(I,J)
         QC(I,3,J)=E3(I,J)*E3(I,J)
C
         QGC(I,1,J)=E1(I,J)*E2(I,J)
         QGC(I,2,J)=E2(I,J)*E3(I,J)
         QGC(I,3,J)=E1(I,J)*E3(I,J)
        ENDDO
       ENDDO
C      
      RETURN
      END
Chd|====================================================================
Chd|  BMTRL2G                       source/elements/solid/solide8z/s8zderi3.F
Chd|-- called by -----------
Chd|        S8EL2GNJ                      source/elements/solid/solide8z/s8zderi3.F
Chd|-- calls ---------------
Chd|        CKKTRAN3V                     source/elements/solid/solide8z/s8zderi3.F
Chd|====================================================================
      SUBROUTINE BMTRL2G(BM   ,BC   ,QC  ,QGC    ,QT    ) 
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL
      my_real
     .   QC(MVSIZ,3,3),QGC(MVSIZ,3,3),BM(MVSIZ,3,3),
     .   BC(MVSIZ,3,3),QT(MVSIZ,3,3)
C----------[BM]g=Qc^t[BM]Qt+QGc^t[BC]Qt
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,EP
C     REAL
C----------Qc^t[BM]Qt;  QGc^t[BC]Qt
      CALL CKKTRAN3V(LFT   ,LLT    ,QC   ,BM ,QT ) 
      CALL CKKTRAN3V(LFT   ,LLT    ,QGC  ,BC ,QT ) 
        DO I=1,3 
        DO J=1,3 
         DO EP=LFT,LLT 
          BM(EP,I,J)= BM(EP,I,J)+BC(EP,I,J)
         ENDDO
        ENDDO
        ENDDO
C      
      RETURN
      END
Chd|====================================================================
Chd|  S8EDERIL2G                    source/elements/solid/solide8z/s8zderi3.F
Chd|-- called by -----------
Chd|        S8E_PIJ                       source/elements/solid/solide8z/s8zderi3.F
Chd|-- calls ---------------
Chd|        S8EL2GNJ                      source/elements/solid/solide8z/s8zderi3.F
Chd|====================================================================
      SUBROUTINE S8EDERIL2G(
     .    PX1, PX2, PX3, PX4, PX5, PX6, PX7, PX8,
     .    PY1, PY2, PY3, PY4, PY5, PY6, PY7, PY8,
     .    PZ1, PZ2, PZ3, PZ4, PZ5, PZ6, PZ7, PZ8,
     .    PXY1,PXY2,PXY3,PXY4,PXY5,PXY6,PXY7,PXY8,
     .    PYX1,PYX2,PYX3,PYX4,PYX5,PYX6,PYX7,PYX8,
     .    PXZ1,PXZ2,PXZ3,PXZ4,PXZ5,PXZ6,PXZ7,PXZ8,
     .    PZX1,PZX2,PZX3,PZX4,PZX5,PZX6,PZX7,PZX8,
     .    PYZ1,PYZ2,PYZ3,PYZ4,PYZ5,PYZ6,PYZ7,PYZ8,
     .    PZY1,PZY2,PZY3,PZY4,PZY5,PZY6,PZY7,PZY8,
     .    BXY1, BXY2, BXY3, BXY4, BXY5, BXY6, BXY7, BXY8,
     .    BYX1, BYX2, BYX3, BYX4, BYX5, BYX6, BYX7, BYX8,
     .    BXZ1, BXZ2, BXZ3, BXZ4, BXZ5, BXZ6, BXZ7, BXZ8,
     .    BZX1, BZX2, BZX3, BZX4, BZX5, BZX6, BZX7, BZX8,
     .    BYZ1, BYZ2, BYZ3, BYZ4, BYZ5, BYZ6, BYZ7, BYZ8,
     .    BZY1, BZY2, BZY3, BZY4, BZY5, BZY6, BZY7, BZY8,
     .    QT  , QC  , QGC , PIJ , NEL )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  NEL
C     REAL
      my_real
     .   PX1(*), PX2(*), PX3(*), PX4(*),
     .   PX5(*), PX6(*), PX7(*), PX8(*),
     .   PY1(*), PY2(*), PY3(*), PY4(*),
     .   PY5(*), PY6(*), PY7(*), PY8(*),
     .   PZ1(*), PZ2(*), PZ3(*), PZ4(*), 
     .   PZ5(*), PZ6(*), PZ7(*), PZ8(*),
     .   PXY1(*),PXY2(*),PXY3(*),PXY4(*),
     .   PXY5(*),PXY6(*),PXY7(*),PXY8(*),
     .   PYX1(*),PYX2(*),PYX3(*),PYX4(*),
     .   PYX5(*),PYX6(*),PYX7(*),PYX8(*),
     .   PXZ1(*),PXZ2(*),PXZ3(*),PXZ4(*),
     .   PXZ5(*),PXZ6(*),PXZ7(*),PXZ8(*),
     .   PZX1(*),PZX2(*),PZX3(*),PZX4(*),
     .   PZX5(*),PZX6(*),PZX7(*),PZX8(*),
     .   PYZ1(*),PYZ2(*),PYZ3(*),PYZ4(*),
     .   PYZ5(*),PYZ6(*),PYZ7(*),PYZ8(*),
     .   PZY1(*),PZY2(*),PZY3(*),PZY4(*),
     .   PZY5(*),PZY6(*),PZY7(*),PZY8(*),
     .   BXY1(*),BXY2(*),BXY3(*),BXY4(*),
     .   BXY5(*),BXY6(*),BXY7(*),BXY8(*),
     .   BYX1(*),BYX2(*),BYX3(*),BYX4(*),
     .   BYX5(*),BYX6(*),BYX7(*),BYX8(*),
     .   BXZ1(*),BXZ2(*),BXZ3(*),BXZ4(*),
     .   BXZ5(*),BXZ6(*),BXZ7(*),BXZ8(*),
     .   BZX1(*),BZX2(*),BZX3(*),BZX4(*),
     .   BZX5(*),BZX6(*),BZX7(*),BZX8(*),
     .   BYZ1(*),BYZ2(*),BYZ3(*),BYZ4(*),
     .   BYZ5(*),BYZ6(*),BYZ7(*),BYZ8(*),
     .   BZY1(*),BZY2(*),BZY3(*),BZY4(*),
     .   BZY5(*),BZY6(*),BZY7(*),BZY8(*),
     .   QT(MVSIZ,9)  , QC(MVSIZ,9) , QGC(MVSIZ,9) , PIJ(NEL,*)  
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  I, J ,ICOR
C     REAL
C                                                                     12
      my_real
     .  XA
C-----------------------------------------------
      CALL S8EL2GNJ(
     .    PX1,  PY1,  PZ1, 
     .    PXY1, PYX1, PXZ1, PZX1, PYZ1, PZY1,
     .    BXY1, BYX1, BXZ1, BZX1, BYZ1, BZY1,
     .    QT  , QC  , QGC )
      CALL S8EL2GNJ(
     .     PX2,  PY2,  PZ2, 
     .    PXY2, PYX2, PXZ2, PZX2, PYZ2, PZY2,
     .    BXY2, BYX2, BXZ2, BZX2, BYZ2, BZY2,
     .    QT  , QC  , QGC )
      CALL S8EL2GNJ(
     .     PX3,  PY3,  PZ3, 
     .    PXY3, PYX3, PXZ3, PZX3, PYZ3, PZY3,
     .    BXY3, BYX3, BXZ3, BZX3, BYZ3, BZY3,
     .    QT  , QC  , QGC )
      CALL S8EL2GNJ(
     .     PX4,  PY4,  PZ4, 
     .    PXY4, PYX4, PXZ4, PZX4, PYZ4, PZY4,
     .    BXY4, BYX4, BXZ4, BZX4, BYZ4, BZY4,
     .    QT  , QC  , QGC )
      CALL S8EL2GNJ(
     .     PX5,  PY5,  PZ5, 
     .    PXY5, PYX5, PXZ5, PZX5, PYZ5, PZY5,
     .    BXY5, BYX5, BXZ5, BZX5, BYZ5, BZY5,
     .    QT  , QC  , QGC )
      CALL S8EL2GNJ(
     .     PX6,  PY6,  PZ6, 
     .    PXY6, PYX6, PXZ6, PZX6, PYZ6, PZY6,
     .    BXY6, BYX6, BXZ6, BZX6, BYZ6, BZY6,
     .    QT  , QC  , QGC )
      CALL S8EL2GNJ(
     .     PX7,  PY7,  PZ7, 
     .    PXY7, PYX7, PXZ7, PZX7, PYZ7, PZY7,
     .    BXY7, BYX7, BXZ7, BZX7, BYZ7, BZY7,
     .    QT  , QC  , QGC )
      CALL S8EL2GNJ(
     .     PX8,  PY8,  PZ8, 
     .    PXY8, PYX8, PXZ8, PZX8, PYZ8, PZY8,
     .    BXY8, BYX8, BXZ8, BZX8, BYZ8, BZY8,
     .    QT  , QC  , QGC )
C
      DO I=LFT,LLT
       PIJ(I,1) = PX1(I)
       PIJ(I,2) = PY1(I)
       PIJ(I,3) = PZ1(I)
       PIJ(I,4) = PX2(I)
       PIJ(I,5) = PY2(I)
       PIJ(I,6) = PZ2(I)
       PIJ(I,7) = PX3(I)
       PIJ(I,8) = PY3(I)
       PIJ(I,9) = PZ3(I)
       PIJ(I,10) = PX4(I)
       PIJ(I,11) = PY4(I)
       PIJ(I,12) = PZ4(I)
       PIJ(I,13) = PX5(I)
       PIJ(I,14) = PY5(I)
       PIJ(I,15) = PZ5(I)
       PIJ(I,16) = PX6(I)
       PIJ(I,17) = PY6(I)
       PIJ(I,18) = PZ6(I)
       PIJ(I,19) = PX7(I)
       PIJ(I,20) = PY7(I)
       PIJ(I,21) = PZ7(I)
       PIJ(I,22) = PX8(I)
       PIJ(I,23) = PY8(I)
       PIJ(I,24) = PZ8(I)
C       
       PIJ(I,25) = BXY1(I)
       PIJ(I,26) = BYX1(I)
       PIJ(I,27) = BXZ1(I)
       PIJ(I,28) = BZX1(I)
       PIJ(I,29) = BYZ1(I)
       PIJ(I,30) = BZY1(I)
C       
       PIJ(I,31) = BXY2(I)
       PIJ(I,32) = BYX2(I)
       PIJ(I,33) = BXZ2(I)
       PIJ(I,34) = BZX2(I)
       PIJ(I,35) = BYZ2(I)
       PIJ(I,36) = BZY2(I)
C       
       PIJ(I,37) = BXY3(I)
       PIJ(I,38) = BYX3(I)
       PIJ(I,39) = BXZ3(I)
       PIJ(I,40) = BZX3(I)
       PIJ(I,41) = BYZ3(I)
       PIJ(I,42) = BZY3(I)
C       
       PIJ(I,43) = BXY4(I)
       PIJ(I,44) = BYX4(I)
       PIJ(I,45) = BXZ4(I)
       PIJ(I,46) = BZX4(I)
       PIJ(I,47) = BYZ4(I)
       PIJ(I,48) = BZY4(I)
C       
       PIJ(I,49) = BXY5(I)
       PIJ(I,50) = BYX5(I)
       PIJ(I,51) = BXZ5(I)
       PIJ(I,52) = BZX5(I)
       PIJ(I,53) = BYZ5(I)
       PIJ(I,54) = BZY5(I)
C       
       PIJ(I,55) = BXY6(I)
       PIJ(I,56) = BYX6(I)
       PIJ(I,57) = BXZ6(I)
       PIJ(I,58) = BZX6(I)
       PIJ(I,59) = BYZ6(I)
       PIJ(I,60) = BZY6(I)
C       
       PIJ(I,61) = BXY7(I)
       PIJ(I,62) = BYX7(I)
       PIJ(I,63) = BXZ7(I)
       PIJ(I,64) = BZX7(I)
       PIJ(I,65) = BYZ7(I)
       PIJ(I,66) = BZY7(I)
C       
       PIJ(I,67) = BXY8(I)
       PIJ(I,68) = BYX8(I)
       PIJ(I,69) = BXZ8(I)
       PIJ(I,70) = BZX8(I)
       PIJ(I,71) = BYZ8(I)
       PIJ(I,72) = BZY8(I)
      ENDDO
C      
      RETURN
      END
Chd|====================================================================
Chd|  S8EL2GNJ                      source/elements/solid/solide8z/s8zderi3.F
Chd|-- called by -----------
Chd|        S8EDERIL2G                    source/elements/solid/solide8z/s8zderi3.F
Chd|-- calls ---------------
Chd|        BMTRL2G                       source/elements/solid/solide8z/s8zderi3.F
Chd|====================================================================
      SUBROUTINE S8EL2GNJ(
     .    PX1,  PY1,  PZ1, 
     .    PXY1, PYX1, PXZ1, PZX1, PYZ1, PZY1,
     .    BXY1, BYX1, BXZ1, BZX1, BYZ1, BZY1,
     .    QT  , QC  , QGC )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL
      my_real
     .    PX1(*),  PY1(*),  PZ1(*), 
     .    PXY1(*), PYX1(*), PXZ1(*), PZX1(*), PYZ1(*), PZY1(*),
     .    BXY1(*), BYX1(*), BXZ1(*), BZX1(*), BYZ1(*), BZY1(*),
     .    QT(MVSIZ,9)  , QC(MVSIZ,9) , QGC(MVSIZ,9)   
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  I, J ,ICOR
C     REAL
C                                                                     12
      my_real
     .  BM(MVSIZ,3,3)  , BC(MVSIZ,3,3)
C-----------------------------------------------
C-------|PX BYX BZX |     |PXY PYX  0 |
C-------|BXY PY BZY | :BM |PXZ  0  PZX| :BC
C-------|BXZ BYZ PZ |     | 0  PYZ PZY|
C --- n1     
      DO I=LFT,LLT
       BM(I,1,1) = PX1(I)
       BM(I,2,2) = PY1(I)
       BM(I,3,3) = PZ1(I)
       BM(I,1,2) = BYX1(I)
       BM(I,1,3) = BZX1(I)
       BM(I,2,1) = BXY1(I)
       BM(I,2,3) = BZY1(I)
       BM(I,3,1) = BXZ1(I)
       BM(I,3,2) = BYZ1(I)
      ENDDO
      DO I=LFT,LLT
       BC(I,1,1) = PY1(I)
       BC(I,1,2) = PX1(I)
       BC(I,1,3) = ZERO
       BC(I,3,1) = PZ1(I)
       BC(I,3,2) = ZERO
       BC(I,3,3) = PX1(I)
       BC(I,2,1) = ZERO
       BC(I,2,2) = PZ1(I)
       BC(I,2,3) = PY1(I)
      ENDDO
      CALL BMTRL2G(BM   ,BC   ,QC  ,QGC    ,QT    ) 
      DO I=LFT,LLT
       PX1(I) = BM(I,1,1)  
       PY1(I) = BM(I,2,2)  
       PZ1(I) = BM(I,3,3)  
       BYX1(I)= BM(I,1,2)  
       BZX1(I)= BM(I,1,3)  
       BXY1(I)= BM(I,2,1)  
       BZY1(I)= BM(I,2,3)  
       BXZ1(I)= BM(I,3,1)  
       BYZ1(I)= BM(I,3,2)  
      ENDDO
C      
      RETURN
      END
Chd|====================================================================
Chd|  CKKTRAN3V                     source/elements/solid/solide8z/s8zderi3.F
Chd|-- called by -----------
Chd|        BMTRL2G                       source/elements/solid/solide8z/s8zderi3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE CKKTRAN3V(JFT   ,JLT    ,VQI   ,KK,VQJ) 
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
#include      "implicit_f.inc"
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D U M M Y   A R G U M E N T S
C-----------------------------------------------
      INTEGER JFT,JLT
      my_real 
     .   VQI(MVSIZ,3,3), VQJ(MVSIZ,3,3),KK(MVSIZ,3,3)
C-----------------------------------------------
C   LOCAL  A R G U M E N T S
C-----------------------------------------------
      INTEGER I,J,EP
      my_real
     .    K(MVSIZ,3,3)
C-----------------------------------------------
        DO I=1,3 
        DO J=1,3 
         DO EP=JFT,JLT 
          K(EP,I,J)=VQI(EP,1,I)*(KK(EP,1,1)*VQJ(EP,1,J)+
     1                  KK(EP,1,2)*VQJ(EP,2,J)+KK(EP,1,3)*VQJ(EP,3,J))+
     2              VQI(EP,2,I)*(KK(EP,2,1)*VQJ(EP,1,J)+
     3                  KK(EP,2,2)*VQJ(EP,2,J)+KK(EP,2,3)*VQJ(EP,3,J))+
     4              VQI(EP,3,I)*(KK(EP,3,1)*VQJ(EP,1,J)+
     5                  KK(EP,3,2)*VQJ(EP,2,J)+KK(EP,3,3)*VQJ(EP,3,J))
         ENDDO
        ENDDO
        ENDDO
C
        DO I=1,3 
        DO J=1,3 
         DO EP=JFT,JLT 
          KK(EP,I,J)= K(EP,I,J)
         ENDDO
        ENDDO
        ENDDO
C      
      RETURN
      END
Chd|====================================================================
Chd|  S8EJACIP3                     source/elements/solid/solide8z/s8zderi3.F
Chd|-- called by -----------
Chd|        S8ZINIT3                      source/elements/solid/solide8z/s8zinit3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE S8EJACIP3(
     .    HX,    HY,    HZ,   
     .    CJ1,CJ2,CJ3,
     .    CJ4,CJ5,CJ6,
     .    CJ7,CJ8,CJ9,
     .    JAC1,JAC2,JAC3,
     .    JAC4,JAC5,JAC6,
     .    JAC7,JAC8,JAC9)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL
      my_real
     .   HX(4,MVSIZ),  HY(4,MVSIZ), HZ(4,MVSIZ),   
     .   CJ1(*),CJ2(*),CJ3(*),
     .   CJ4(*),CJ5(*),CJ6(*),
     .   CJ7(*),CJ8(*),CJ9(*),
     .   JAC1(MVSIZ,8),JAC2(MVSIZ,8),JAC3(MVSIZ,8),
     .   JAC4(MVSIZ,8),JAC5(MVSIZ,8),JAC6(MVSIZ,8),
     .   JAC7(MVSIZ,8),JAC8(MVSIZ,8),JAC9(MVSIZ,8)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J ,IP
C     REAL
C                                                                     12
      my_real
     .   HX1PG(MVSIZ), HX2PG(MVSIZ), HX3PG(MVSIZ),HX4PG2(MVSIZ),
     .   HY1PG(MVSIZ), HY2PG(MVSIZ), HY3PG(MVSIZ),HY4PG2(MVSIZ),
     .   HZ1PG(MVSIZ), HZ2PG(MVSIZ), HZ3PG(MVSIZ),HZ4PG2(MVSIZ),
     .   PG2,PG
       PARAMETER (PG=.577350269189625D0)
C-----------------------------------------------
      PG2=PG*PG
C
      DO I=LFT,LLT
       HX1PG(I)=HX(1,I)*PG
       HX2PG(I)=HX(2,I)*PG
       HX3PG(I)=HX(3,I)*PG
       HX4PG2(I)=HX(4,I)*PG2
       HY1PG(I)=HY(1,I)*PG
       HY2PG(I)=HY(2,I)*PG
       HY3PG(I)=HY(3,I)*PG
       HY4PG2(I)=HY(4,I)*PG2
       HZ1PG(I)=HZ(1,I)*PG
       HZ2PG(I)=HZ(2,I)*PG
       HZ3PG(I)=HZ(3,I)*PG
       HZ4PG2(I)=HZ(4,I)*PG2
      ENDDO
      IP=1
C - - -    
      DO I=LFT,LLT
       JAC1(I,IP)=CJ1(I)-HX3PG(I)-HX2PG(I)+HX4PG2(I)
       JAC2(I,IP)=CJ2(I)-HY3PG(I)-HY2PG(I)+HY4PG2(I)
       JAC3(I,IP)=CJ3(I)-HZ3PG(I)-HZ2PG(I)+HZ4PG2(I)
C
       JAC4(I,IP)=CJ4(I)-HX1PG(I)-HX3PG(I)+HX4PG2(I)
       JAC5(I,IP)=CJ5(I)-HY1PG(I)-HY3PG(I)+HY4PG2(I)
       JAC6(I,IP)=CJ6(I)-HZ1PG(I)-HZ3PG(I)+HZ4PG2(I)
C
       JAC7(I,IP)=CJ7(I)-HX2PG(I)-HX1PG(I)+HX4PG2(I)
       JAC8(I,IP)=CJ8(I)-HY2PG(I)-HY1PG(I)+HY4PG2(I)
       JAC9(I,IP)=CJ9(I)-HZ2PG(I)-HZ1PG(I)+HZ4PG2(I)
      ENDDO
      IP=2    
C + - -    
      DO I=LFT,LLT
       JAC1(I,IP)=CJ1(I)-HX3PG(I)-HX2PG(I)+HX4PG2(I)
       JAC2(I,IP)=CJ2(I)-HY3PG(I)-HY2PG(I)+HY4PG2(I)
       JAC3(I,IP)=CJ3(I)-HZ3PG(I)-HZ2PG(I)+HZ4PG2(I)
C
       JAC4(I,IP)=CJ4(I)-HX1PG(I)+HX3PG(I)-HX4PG2(I)
       JAC5(I,IP)=CJ5(I)-HY1PG(I)+HY3PG(I)-HY4PG2(I)
       JAC6(I,IP)=CJ6(I)-HZ1PG(I)+HZ3PG(I)-HZ4PG2(I)
C
       JAC7(I,IP)=CJ7(I)+HX2PG(I)-HX1PG(I)-HX4PG2(I)
       JAC8(I,IP)=CJ8(I)+HY2PG(I)-HY1PG(I)-HY4PG2(I)
       JAC9(I,IP)=CJ9(I)+HZ2PG(I)-HZ1PG(I)-HZ4PG2(I)
      ENDDO
      IP=3    
C - + -    
      DO I=LFT,LLT
       JAC1(I,IP)=CJ1(I)+HX3PG(I)-HX2PG(I)-HX4PG2(I)
       JAC2(I,IP)=CJ2(I)+HY3PG(I)-HY2PG(I)-HY4PG2(I)
       JAC3(I,IP)=CJ3(I)+HZ3PG(I)-HZ2PG(I)-HZ4PG2(I)
C
       JAC4(I,IP)=CJ4(I)-HX1PG(I)-HX3PG(I)+HX4PG2(I)
       JAC5(I,IP)=CJ5(I)-HY1PG(I)-HY3PG(I)+HY4PG2(I)
       JAC6(I,IP)=CJ6(I)-HZ1PG(I)-HZ3PG(I)+HZ4PG2(I)
C
       JAC7(I,IP)=CJ7(I)-HX2PG(I)+HX1PG(I)-HX4PG2(I)
       JAC8(I,IP)=CJ8(I)-HY2PG(I)+HY1PG(I)-HY4PG2(I)
       JAC9(I,IP)=CJ9(I)-HZ2PG(I)+HZ1PG(I)-HZ4PG2(I)
      ENDDO
      IP=4    
C + + -    
      DO I=LFT,LLT
       JAC1(I,IP)=CJ1(I)+HX3PG(I)-HX2PG(I)-HX4PG2(I)
       JAC2(I,IP)=CJ2(I)+HY3PG(I)-HY2PG(I)-HY4PG2(I)
       JAC3(I,IP)=CJ3(I)+HZ3PG(I)-HZ2PG(I)-HZ4PG2(I)
C
       JAC4(I,IP)=CJ4(I)-HX1PG(I)+HX3PG(I)-HX4PG2(I)
       JAC5(I,IP)=CJ5(I)-HY1PG(I)+HY3PG(I)-HY4PG2(I)
       JAC6(I,IP)=CJ6(I)-HZ1PG(I)+HZ3PG(I)-HZ4PG2(I)
C
       JAC7(I,IP)=CJ7(I)+HX2PG(I)+HX1PG(I)+HX4PG2(I)
       JAC8(I,IP)=CJ8(I)+HY2PG(I)+HY1PG(I)+HY4PG2(I)
       JAC9(I,IP)=CJ9(I)+HZ2PG(I)+HZ1PG(I)+HZ4PG2(I)
      ENDDO
      IP=5    
C - - +    
      DO I=LFT,LLT
       JAC1(I,IP)=CJ1(I)-HX3PG(I)+HX2PG(I)-HX4PG2(I)
       JAC2(I,IP)=CJ2(I)-HY3PG(I)+HY2PG(I)-HY4PG2(I)
       JAC3(I,IP)=CJ3(I)-HZ3PG(I)+HZ2PG(I)-HZ4PG2(I)
C
       JAC4(I,IP)=CJ4(I)+HX1PG(I)-HX3PG(I)-HX4PG2(I)
       JAC5(I,IP)=CJ5(I)+HY1PG(I)-HY3PG(I)-HY4PG2(I)
       JAC6(I,IP)=CJ6(I)+HZ1PG(I)-HZ3PG(I)-HZ4PG2(I)
C
       JAC7(I,IP)=CJ7(I)-HX2PG(I)-HX1PG(I)+HX4PG2(I)
       JAC8(I,IP)=CJ8(I)-HY2PG(I)-HY1PG(I)+HY4PG2(I)
       JAC9(I,IP)=CJ9(I)-HZ2PG(I)-HZ1PG(I)+HZ4PG2(I)
      ENDDO
      IP=6    
C + - +    
      DO I=LFT,LLT
       JAC1(I,IP)=CJ1(I)-HX3PG(I)+HX2PG(I)-HX4PG2(I)
       JAC2(I,IP)=CJ2(I)-HY3PG(I)+HY2PG(I)-HY4PG2(I)
       JAC3(I,IP)=CJ3(I)-HZ3PG(I)+HZ2PG(I)-HZ4PG2(I)
C
       JAC4(I,IP)=CJ4(I)+HX1PG(I)+HX3PG(I)+HX4PG2(I)
       JAC5(I,IP)=CJ5(I)+HY1PG(I)+HY3PG(I)+HY4PG2(I)
       JAC6(I,IP)=CJ6(I)+HZ1PG(I)+HZ3PG(I)+HZ4PG2(I)
C
       JAC7(I,IP)=CJ7(I)+HX2PG(I)-HX1PG(I)-HX4PG2(I)
       JAC8(I,IP)=CJ8(I)+HY2PG(I)-HY1PG(I)-HY4PG2(I)
       JAC9(I,IP)=CJ9(I)+HZ2PG(I)-HZ1PG(I)-HZ4PG2(I)
      ENDDO
      IP=7    
C - + +    
      DO I=LFT,LLT
       JAC1(I,IP)=CJ1(I)+HX3PG(I)+HX2PG(I)+HX4PG2(I)
       JAC2(I,IP)=CJ2(I)+HY3PG(I)+HY2PG(I)+HY4PG2(I)
       JAC3(I,IP)=CJ3(I)+HZ3PG(I)+HZ2PG(I)+HZ4PG2(I)
C
       JAC4(I,IP)=CJ4(I)+HX1PG(I)-HX3PG(I)-HX4PG2(I)
       JAC5(I,IP)=CJ5(I)+HY1PG(I)-HY3PG(I)-HY4PG2(I)
       JAC6(I,IP)=CJ6(I)+HZ1PG(I)-HZ3PG(I)-HZ4PG2(I)
C
       JAC7(I,IP)=CJ7(I)-HX2PG(I)+HX1PG(I)-HX4PG2(I)
       JAC8(I,IP)=CJ8(I)-HY2PG(I)+HY1PG(I)-HY4PG2(I)
       JAC9(I,IP)=CJ9(I)-HZ2PG(I)+HZ1PG(I)-HZ4PG2(I)
      ENDDO
      IP=8    
C + + +    
      DO I=LFT,LLT
       JAC1(I,IP)=CJ1(I)+HX3PG(I)+HX2PG(I)+HX4PG2(I)
       JAC2(I,IP)=CJ2(I)+HY3PG(I)+HY2PG(I)+HY4PG2(I)
       JAC3(I,IP)=CJ3(I)+HZ3PG(I)+HZ2PG(I)+HZ4PG2(I)
C
       JAC4(I,IP)=CJ4(I)+HX1PG(I)+HX3PG(I)+HX4PG2(I)
       JAC5(I,IP)=CJ5(I)+HY1PG(I)+HY3PG(I)+HY4PG2(I)
       JAC6(I,IP)=CJ6(I)+HZ1PG(I)+HZ3PG(I)+HZ4PG2(I)
C
       JAC7(I,IP)=CJ7(I)+HX2PG(I)+HX1PG(I)+HX4PG2(I)
       JAC8(I,IP)=CJ8(I)+HY2PG(I)+HY1PG(I)+HY4PG2(I)
       JAC9(I,IP)=CJ9(I)+HZ2PG(I)+HZ1PG(I)+HZ4PG2(I)
      ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  S8EDERI3                      source/elements/solid/solide8z/s8zderi3.F
Chd|-- called by -----------
Chd|        S8ZINIT3                      source/elements/solid/solide8z/s8zinit3.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE S8EDERI3(VOL,VEUL,GEO,WI,
     .    JACP1,JACP2,JACP3,
     .    JACP4,JACP5,JACP6,
     .    JACP7,JACP8,JACP9,
     .    SMAX, DELTAX, NGL,VOLDP)
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
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real
     .   VOL(*), VEUL(LVEUL,*),GEO(NPROPG,*),WI
      my_real
     .   JACP1(*),JACP2(*),JACP3(*),
     .   JACP4(*),JACP5(*),JACP6(*),
     .   JACP7(*),JACP8(*),JACP9(*),
     .   SMAX(*), DELTAX(*)
      INTEGER NGL(*)
      DOUBLE PRECISION
     .   VOLDP(*), DETDP
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J
C                                                                     12
      my_real
     .   JAC1(MVSIZ),JAC2(MVSIZ),JAC3(MVSIZ),
     .   JAC4(MVSIZ),JAC5(MVSIZ),JAC6(MVSIZ),
     .   JAC7(MVSIZ),JAC8(MVSIZ),JAC9(MVSIZ),
     .   JACI1(MVSIZ), JACI2(MVSIZ), JACI3(MVSIZ),
     .   JACI4(MVSIZ), JACI5(MVSIZ), JACI6(MVSIZ),
     .   JACI7(MVSIZ), JACI8(MVSIZ), JACI9(MVSIZ),
     .   JAC_59_68(MVSIZ), JAC_67_49(MVSIZ), JAC_48_57(MVSIZ), DET(MVSIZ)
C-----------------------------------------------
       JAC1(LFT:LLT)=JACP1(LFT:LLT)
       JAC2(LFT:LLT)=JACP2(LFT:LLT)
       JAC3(LFT:LLT)=JACP3(LFT:LLT)
       JAC4(LFT:LLT)=JACP4(LFT:LLT)
       JAC5(LFT:LLT)=JACP5(LFT:LLT)
       JAC6(LFT:LLT)=JACP6(LFT:LLT)
       JAC7(LFT:LLT)=JACP7(LFT:LLT)
       JAC8(LFT:LLT)=JACP8(LFT:LLT)
       JAC9(LFT:LLT)=JACP9(LFT:LLT)
C
C     JACOBIAN
C
      DO I=LFT,LLT
       JAC_59_68(I)=JAC5(I)*JAC9(I)-JAC6(I)*JAC8(I)
       JAC_67_49(I)=JAC6(I)*JAC7(I)-JAC4(I)*JAC9(I)
       JAC_48_57(I)=JAC4(I)*JAC8(I)-JAC5(I)*JAC7(I)
      ENDDO
C
      DO I=LFT,LLT
      DETDP=ONE_OVER_512*(JAC1(I)*JAC_59_68(I)+JAC2(I)*JAC_67_49(I)+JAC3(I)*JAC_48_57(I))
      DET(I)=DETDP
      VOLDP(I)= WI*DETDP
      VOL(I)= VOLDP(I)
      ENDDO
      DO 130 I=LFT,LLT
      IF(DET(I)>0.0) GO TO 130
      CALL ANCMSG(MSGID=245,
     .            MSGTYPE=MSGERROR,
     .            ANMODE=ANINFO,
     .            I1=NGL(I))
  130 CONTINUE
C
C
      DO I=LFT,LLT
        DELTAX(I)=MIN(DELTAX(I),HUNDRED28*DET(I)*SMAX(I))
      ENDDO
      RETURN
      END
      
