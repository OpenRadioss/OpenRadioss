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
C
Chd|====================================================================
Chd|  I24PEN3                       source/interfaces/inter3d1/i24pen3.F
Chd|-- called by -----------
Chd|        ININT3                        source/interfaces/inter3d1/inint3.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        I24FIC_GETN                   source/interfaces/inter3d1/i24surfi.F
Chd|        I24PENMAX                     source/interfaces/inter3d1/i24pen3.F
Chd|        ICONNET                       source/interfaces/inter3d1/i24pen3.F
Chd|        INI_ST3                       source/interfaces/inter3d1/i24pen3.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE I24PEN3(X    ,IRECT ,GAPV ,CAND_E,CAND_N,
     2                   NSV  ,INACTI,ITAB ,TAG   ,IWPENE,
     3                   NSN  ,IRTLM ,MSEGTYP   ,IWPENE0 ,
     4                   PMIN ,GAP_N ,MVOISN    ,IXS     ,
     5                   IXS10,IXS16 ,IXS20,PENMAX,PENMIN,
     6                   ID,TITR ,ILEV ,PEN_OLD,KNOD2ELS,
     7                   NOD2ELS,IPARTNS,IPEN0 ,ICONT_I ,
     8                   XFIC   ,NRTM ,IRTSE   ,IS2SE)
#ifndef HYPERMESH_LIB
      USE MESSAGE_MOD
#endif
C
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
#include      "warn_c.inc"
#include      "vect07_c.inc"
#include      "scr03_c.inc"
#include      "scr17_c.inc"
#include      "param_c.inc"
#include      "scr05_c.inc"
#include      "com04_c.inc"
C-----------------------------------------------
      INTEGER IWPENE,TAG(*),INACTI,NSV(*),NSN,MSEGTYP(*),IWPENE0,
     .        MVOISN(4,*),ILEV,KNOD2ELS(*),NOD2ELS(*),IPARTNS(*),NRTM
C----  IWPENE0 : nb of tiny inital pene;  IWPENE nb of inital pene  
C     REAL
      my_real
     .   GAPV(*)
      INTEGER IRECT(4,*), ITAB(*),CAND_E(*),CAND_N(*),IRTLM(2,*)
      INTEGER IXS(NIXS,*),IXS10(6,*), IXS16(8,*), IXS20(12,*),ICONT_I(*),
     .        IRTSE(*) ,IS2SE(*)
C     REAL
      my_real
     .   X(3,*),PMIN(*),GAP_N(12,*),PENMAX,PENMIN,PEN_OLD(5,NSN),
     .   XFIC(3,*)
C--------GAP_N(1,*) stock temporaly characristic length for Pen_max
      INTEGER ID,IPEN0
      CHARACTER*nchartitle,
     .   TITR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER II, I, J, K, L, JJ, NJ, IER,NS,IC,I0,IELIM,NI,ICONN,ip,NS1,
     .        IDEL,NN1,NN2,IE
C     REAL
      my_real
     .   PEN, ALP,XX(4),YY(4),ZZ(4),SSC,TTC,DIST,DIST0,
     .   XI,YI,ZI,XC,YC,ZC,NN(3),TOL,PEN0,DPEN,NORM,MAXPEN
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
C---  MVOISN,IRTLM(2,*) is used temporaly for Pen_ini MVOISN(1,*) -> MTYPE(solid),MVOISN(2,*) -> E_id
C-----MVOISN(3,*) -> part_id, PEN_OLD(1-3,*)->SECONDARY nodal normal, reput PEN_OLD(1-2,*)=0, 3,)=5,)after
C-----to be consiting with engine for PENMIN
      IF (IRESP==1.AND.PENMIN<=EM06) PENMIN = TWO*EM06
          TOL = PENMIN
      ALP = TWO*EM06
      IF (IRESP==1) ALP = TWO*EM05
      DO I=LFT,LLT
        L  = CAND_E(I)
        NI = CAND_N(I)         
        NS = NSV(NI)
        IF (NS >NUMNOD) THEN
         NS1 = NS -NUMNOD
         XI=XFIC(1,NS1)
         YI=XFIC(2,NS1)
         ZI=XFIC(3,NS1)
        ELSE
         XI=X(1,NS)
         YI=X(2,NS)
         ZI=X(3,NS)
        END IF
        DO JJ=1,4
         NJ=IRECT(JJ,L)
         XX(JJ)=X(1,NJ)
         YY(JJ)=X(2,NJ)
         ZZ(JJ)=X(3,NJ)
        END DO
C--------
        CALL INI_ST3(XX,YY,ZZ,XI,YI,ZI,NN,SSC,TTC,IER,ALP,
     2               XC,YC,ZC)
        IF(IER==-1)THEN
#ifndef HYPERMESH_LIB
         CALL ANCMSG(MSGID=85,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO,
     .               I1=ID,
     .               C1=TITR,
     .               I2=ITAB(NS),
     .               I3=L,
     .               I4=L,
     .               I5=ITAB(IRECT(1,L)),
     .               I6=ITAB(IRECT(2,L)),
     .               I7=ITAB(IRECT(3,L)),
     .               I8=ITAB(IRECT(4,L)))
#endif
C
        ELSE IF(IER==1.AND.(MSEGTYP(L)/=0.AND.MSEGTYP(L)<=NRTM))THEN
C                             shells except coating shells
C---------outside        
c         IF(IPRI>=1)WRITE(IOUT,FMT=FMT_6I)ITAB(NS),L,
c     .                    (ITAB(IRECT(JJ,L)),JJ=1,4)
        ELSE
C------initial penatration case 1) |PEN|<TOL : on, 2) Inacti<0 on,3)Inacti=3,4
C-------warnning out for 2),3),4)
C --------MSEGTYP /=0 ->shell ---
          PEN0=NN(1)*(XI-XC)+NN(2)*(YI-YC)+NN(3)*(ZI-ZC)
          IF(IER==1) THEN
           DIST = SQRT((XI-XC)*(XI-XC)+(YI-YC)*(YI-YC)+(ZI-ZC)*(ZI-ZC))
          ELSE
           DIST = ABS(PEN0)
          END IF
C--------for exception of elimination---           
          IDEL = 1
C----------coating shell is like solid now----           
          IF (MSEGTYP(L)/=0.AND.MSEGTYP(L)<=NRTM) THEN
           PEN=GAPV(I)-ABS(PEN0)
           IF (PEN > PENMAX ) IDEL = 0
C-debug sandwish shell : avoid elimination with high thicknees outside           
           IF (PEN > ZERO) DIST = ABS(GAPV(I)-PEN0)
C----------give up the the wrong one (normal direction, and)           
           IF (PEN0 < ZERO .OR. PEN > PENMAX) PEN=-ABS(PEN)-TOL
C----------distance after shifted           
          ELSE
           PEN=GAPV(I)-PEN0
C------------used only for eliminating wrong contact w/ smaller distance            
           IF(IER==1) PEN=-ABS(PEN)-TOL
           IF (PEN > ZERO .OR. ABS(PEN) < TOL) THEN
            MAXPEN = GAP_N(1,L)
            IF (INACTI /= 0) MAXPEN = PENMAX
            CALL I24PENMAX(PEN,MAXPEN ,MVOISN(1,L),MVOISN(2,L),
     +                    NS  ,IXS,   IXS10,     IXS16, IXS20   ,
     +                    IELIM)
            ICONN = 0
C            IF (INACTI/=0 .AND. NS<=NUMNOD) THEN
            IF (NS>NUMNOD) THEN
              CALL I24FIC_GETN(NS1     ,IRTSE   ,IS2SE   ,IE    ,NN1     ,
     4                       NN2     )
             CALL ICONNET(IRECT(1,L),IXS   ,KNOD2ELS,NOD2ELS,
     .                    IXS10 ,IXS16 ,IXS20 ,NN1  ,ICONN   )
             IF (ICONN == 0)
     .       CALL ICONNET(IRECT(1,L),IXS   ,KNOD2ELS,NOD2ELS,
     .                    IXS10 ,IXS16 ,IXS20 ,NN2  ,ICONN   )
            ELSE
             CALL ICONNET(IRECT(1,L),IXS   ,KNOD2ELS,NOD2ELS,
     .                    IXS10 ,IXS16 ,IXS20 ,NS   ,ICONN   )
            END IF
             IF ((IELIM+ICONN) > 0) PEN = -ABS(PEN)-TOL
             IF (PEN < ZERO ) IDEL = 0
           END IF
C------Elimine the impact take into account to SECONDARY nodal normal          
           IF (INACTI/=0.AND.(PEN > ZERO .OR. ABS(PEN) < TOL).AND.ILEV/=3) THEN
            NORM = NN(1)*PEN_OLD(1,NI)+NN(2)*PEN_OLD(2,NI)
     +             +NN(3)*PEN_OLD(3,NI)
             IF (NORM >= ZERO) THEN
              PEN = -ABS(PEN)-TOL
c             print *,'impact pair eliminated due to N_SECONDARY'
             END IF
           END IF
          END IF !(MSEGTYP(L)/=0 ) THEN
C------Elimine the impact between the same part
          IF (IPEN0==0) THEN           
           IF (INACTI/=0.AND.(PEN > ZERO .OR. ABS(PEN) < TOL)) THEN
            IF (IPARTNS(NI) == MVOISN(3,L)) THEN
             PEN = -ABS(PEN)-TOL
            END IF
           END IF
          END IF !(IPEN0==0) THEN
C------don't take into account auto-impact case for elimination          
          IF (IPARTNS(NI) == MVOISN(3,L)) IDEL = 0
C--------exception for SECONDARY shell (test TV)----          
          IF (GAPV(I)>ZERO.AND.(MSEGTYP(L)==0.OR.MSEGTYP(L)>NRTM))IDEL=0
C--------PMIN() has been changed from PENE to dist excepting (INACTI ==0,1)        
C--------PMIN() = -dist for  ABS(PEN) < TOL .OR. PEN<ZERO       
C------------ cas 1  this part is removed in Engine at T=0 for consisting
          IF(ABS(PEN) < TOL .OR. (PEN<ZERO.AND.IDEL>0)) THEN
C---------only used to calculate Dist_min and to eliminate wrong contact (higher)            
           IF (TAG(NS)==0) THEN
            PMIN(NI)=-DIST
            TAG(NS)=NI
           ELSE
            I0=TAG(NS)
            PEN0=PMIN(I0)
            IF (DIST <ABS(PEN0)) THEN
C----------only update dist_min            
              PMIN(NI)=-DIST
              TAG(NS)=NI
             IF (PEN0 > ZERO) THEN
C----------elimine wrong contact            
              IRTLM(1,I0)=0
              IRTLM(2,I0)=0
              PEN_OLD(5,I0)=ZERO
             END IF
            END IF
           END IF
C------------shell          
c           IF (MSEGTYP(L)/=0 ) THEN
c            IF (TAG(NS)==0.AND.PEN > ZERO) THEN
c             IF(INACTI /= 1) THEN
c              IRTLM(1,NI)=L
c              IRTLM(2,NI)=1
c             END IF!(INACTI /= 1) THEN
c             IWPENE0=IWPENE0+1
c             IWPENE=IWPENE+1
c             TAG(NS)=NI
c            END IF
c           ELSE
c            IF (TAG(NS)==0) THEN
c             IF(INACTI /= 1) THEN
c              IRTLM(1,NI)=L
c              IRTLM(2,NI)=1
c             END IF!(INACTI /= 1) THEN
c             IWPENE0=IWPENE0+1
c             IWPENE=IWPENE+1
c             TAG(NS)=NI
c            END IF
c           END IF !(MSEGTYP(L)/=0 ) THEN
          ELSEIF(PEN > PENMAX) THEN
C----------warning w/o treatement                  
#ifndef HYPERMESH_LIB
            WRITE(IOUT,1200)PEN
#endif
          ELSEIF(PEN > ZERO) THEN
C------Warning anyway-------------
           IF (TAG(NS)==0) IWPENE=IWPENE+1
C-----------add output of fictive nodes after           
c           IF(IPRI>=1.AND.NS<=NUMNOD) THEN
c            CALL ANCMSG(MSGID=346,
c     .                  MSGTYPE=MSGWARNING,
c     .                  ANMODE=ANINFO_BLIND_2,
c     .                  I1=ID,I2=ITAB(NS),
c     .                  C1=TITR,
c     .                  R1=PEN)
c            WRITE(IOUT,FMT=FMT_6I_2F)
c     .      ITAB(NS),L,(ITAB(IRECT(JJ,L)),JJ=1,4),SSC,TTC
c           END IF
C------------PMIN has been changed from PENE to dist excepting (INACTI ==0,1)        
           IF(INACTI ==0 .OR. INACTI ==1) THEN
C------------use IRTLM(2,NI)-> ICONT_I<0 for initial penetration
            IF (TAG(NS)>0) THEN
             I0=TAG(NS)
             PEN0=PMIN(I0)
C----------exclude case of PMIN(I0)<0 : -dist            
             IF (PEN < PEN0) THEN
              ICONT_I(NI)=-L
              PMIN(NI)=PEN
              TAG(NS) = NI
#ifdef HYPERMESH_LIB
             PEN_OLD(1:3,NI) = NN(1:3)
#endif
             ENDIF             
            ELSE
             ICONT_I(NI)=-L
             PMIN(NI)=PEN
             TAG(NS) = NI
#ifdef HYPERMESH_LIB
             PEN_OLD(1:3,NI) = NN(1:3)
#endif
            END IF
           ELSEIF(INACTI ==-1) THEN
C------------multi-cont-> single by overwriting with min_pene
            IF (TAG(NS)>0) THEN
             I0=TAG(NS)
             PEN0=PMIN(I0)
             DIST0 = ABS(PMIN(I0))
             IF (DIST < DIST0) THEN
C             IF (PEN < PEN0) THEN
              IRTLM(1,NI)=L
              IRTLM(2,NI)=1
              PMIN(NI)=DIST
              PEN_OLD(5,NI)=PEN
#ifdef HYPERMESH_LIB
              PEN_OLD(1:3,NI) = NN(1:3)
#endif
             ENDIF             
            ELSE
             IRTLM(1,NI)=L
             IRTLM(2,NI)=1
             PMIN(NI)=DIST
             PEN_OLD(5,NI)=PEN
#ifdef HYPERMESH_LIB
             PEN_OLD(1:3,NI) = NN(1:3)
#endif
            END IF
C--------hide option------           
           ELSEIF(INACTI ==3 ) THEN
            IF (ILEV ==3) THEN
             DPEN = PEN + TOL
            ELSE
             DPEN = HALF*(PEN + TOL)
            END IF
C-------change SECONDARY node
            IF (TAG(NS)==0) THEN
              IRTLM(1,NI)=L
              IRTLM(2,NI)=1
              IWPENE=IWPENE+1
              TAG(NS)=NI
#ifndef HYPERMESH_LIB
             WRITE(IOUT,1000)PEN
#endif
             IF (NS >NUMNOD) THEN
               NS1 = NS -NUMNOD
               XFIC(1,NS1) = XI + DPEN*NN(1)
               XFIC(2,NS1) = YI + DPEN*NN(2)
               XFIC(3,NS1) = ZI + DPEN*NN(3)
#ifndef HYPERMESH_LIB
              WRITE(IOUT,FMT=FMT_I_3F)(ITAB(NUMNOD)+NS1),
     +                        XFIC(1,NS1),XFIC(2,NS1),XFIC(3,NS1)
#endif
             ELSE
              X(1,NS) = XI + DPEN*NN(1)
              X(2,NS) = YI + DPEN*NN(2)
              X(3,NS) = ZI + DPEN*NN(3)
#ifndef HYPERMESH_LIB
              WRITE(IOUT,FMT=FMT_I_3F)ITAB(NS),X(1,NS),X(2,NS),X(3,NS)
#endif
             END IF !(NS >NUMNOD) THEN
            END IF
           ELSEIF(INACTI ==5) THEN
C------------multi-cont-> single by overwriting with min_pene         
            IF (TAG(NS)>0) THEN
             I0=TAG(NS)
             PEN0=PEN_OLD(5,I0)
             DIST0 = ABS(PMIN(I0))
             IF (DIST < DIST0) THEN
C             IF (PEN < PEN0) THEN
              IRTLM(1,NI)=L
              IRTLM(2,NI)=1
              PEN_OLD(5,NI)=PEN
              PMIN(NI)=DIST
#ifdef HYPERMESH_LIB
              PEN_OLD(1:3,NI) = NN(1:3)
#endif
             ENDIF             
            ELSE
             IRTLM(1,NI)=L
             IRTLM(2,NI)=1
             PEN_OLD(5,NI)=PEN
             PMIN(NI)=DIST
#ifdef HYPERMESH_LIB
             PEN_OLD(1:3,NI) = NN(1:3)
#endif
            END IF
           END IF !IF(INACTI ==-1) THEN
           TAG(NS)=CAND_N(I)
          END IF !(ABS(PEN)<=TOL) THEN
        END IF !(IER==-1)THEN
      END DO !I=LFT,LLT              
C      
      RETURN
 1000 FORMAT(2X,'** INITIAL PENETRATION =',1PG20.13,
     . ' CHANGE COORDINATES OF SECONDARY NODE TO:')
 1100 FORMAT(2X,'** INITIAL PENETRATION =',1PG20.13,
     . ' CHANGE COORDINATES OF MAIN NODE TO:')
 1200 FORMAT(2X,'** TOO HIGH INITIAL PENETRATION=, WILL BE IGNORED',
     .       1PG20.13)
C
      END
Chd|====================================================================
Chd|  INI_ST3                       source/interfaces/inter3d1/i24pen3.F
Chd|-- called by -----------
Chd|        I24PEN3                       source/interfaces/inter3d1/i24pen3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE INI_ST3(XX,YY,ZZ,XI,YI,ZI,NN,SSC,TTC,IER,ALP,
     1                   XC,YC,ZC)
C
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IER
C     REAL
      my_real
     .   XX(4),YY(4),ZZ(4),NN(3), SSC, TTC, ALP,XI,YI,ZI,XC,YC,ZC
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C     REAL
      my_real
     .   H(4), X0, Y0, Z0, XL1, XL2, XL3, XL4, YY1, YY2, YY3, YY4,
     .   ZZ1, ZZ2, ZZ3, ZZ4, XI1, XI2, XI3, XI4, YI1, YI2, YI3, YI4,
     .   ZI1, ZI2, ZI3, ZI4, XN1, YN1, ZN1, XN2, YN2, ZN2, XN3, YN3,
     .   ZN3, XN4, YN4, ZN4, AN, AREA, A12, A23, A34, A41, B12, B23,
     .   B34, B41, AB1, AB2, TP, TM, SP, SM, X1,X2,X3,X4,
     .   Y1,Y2,Y3,Y4,Z1,Z2,Z3,Z4,N1,N2,N3,LA,LB,LC,LBS,LCS,TT1,SS1
C
      X1=XX(1)
      X2=XX(2)
      X3=XX(3)
      X4=XX(4)
      Y1=YY(1)
      Y2=YY(2)
      Y3=YY(3)
      Y4=YY(4)
      Z1=ZZ(1)
      Z2=ZZ(2)
      Z3=ZZ(3)
      Z4=ZZ(4)
C
      X0 = FOURTH*(X1+X2+X3+X4)
      Y0 = FOURTH*(Y1+Y2+Y3+Y4)
      Z0 = FOURTH*(Z1+Z2+Z3+Z4)
C
      XL1 = X1-X0
      XL2 = X2-X0
      XL3 = X3-X0
      XL4 = X4-X0
      YY1 = Y1-Y0
      YY2 = Y2-Y0
      YY3 = Y3-Y0
      YY4 = Y4-Y0
      ZZ1 = Z1-Z0
      ZZ2 = Z2-Z0
      ZZ3 = Z3-Z0
      ZZ4 = Z4-Z0
C
      XI1 = X1-XI
      XI2 = X2-XI
      XI3 = X3-XI
      XI4 = X4-XI
      YI1 = Y1-YI
      YI2 = Y2-YI
      YI3 = Y3-YI
      YI4 = Y4-YI
      ZI1 = Z1-ZI
      ZI2 = Z2-ZI
      ZI3 = Z3-ZI
      ZI4 = Z4-ZI
C
      XN1 = YY1*ZZ2 - YY2*ZZ1
      YN1 = ZZ1*XL2 - ZZ2*XL1
      ZN1 = XL1*YY2 - XL2*YY1
      N1=XN1
      N2=YN1
      N3=ZN1
C
      XN2 = YY2*ZZ3 - YY3*ZZ2
      YN2 = ZZ2*XL3 - ZZ3*XL2
      ZN2 = XL2*YY3 - XL3*YY2
      N1=N1+XN2
      N2=N2+YN2
      N3=N3+ZN2
C
      XN3 = YY3*ZZ4 - YY4*ZZ3
      YN3 = ZZ3*XL4 - ZZ4*XL3
      ZN3 = XL3*YY4 - XL4*YY3
      N1=N1+XN3
      N2=N2+YN3
      N3=N3+ZN3
C
      XN4 = YY4*ZZ1 - YY1*ZZ4
      YN4 = ZZ4*XL1 - ZZ1*XL4
      ZN4 = XL4*YY1 - XL1*YY4
      N1=N1+XN4
      N2=N2+YN4
      N3=N3+ZN4
C
      AN= MAX(EM20,SQRT(N1*N1+N2*N2+N3*N3))
      N1=N1/AN
      N2=N2/AN
      N3=N3/AN
      NN(1)=N1
      NN(2)=N2
      NN(3)=N3
      IF(AN<=EM19) THEN
        IER=-1
        RETURN
      ENDIF
      AREA=HALF*AN
C
      A12=(N1*XN1+N2*YN1+N3*ZN1)
      A23=(N1*XN2+N2*YN2+N3*ZN2)
      A34=(N1*XN3+N2*YN3+N3*ZN3)
      A41=(N1*XN4+N2*YN4+N3*ZN4)
C
      XN1 = YI1*ZI2 - YI2*ZI1
      YN1 = ZI1*XI2 - ZI2*XI1
      ZN1 = XI1*YI2 - XI2*YI1
      B12=(N1*XN1+N2*YN1+N3*ZN1)
C
      XN2 = YI2*ZI3 - YI3*ZI2
      YN2 = ZI2*XI3 - ZI3*XI2
      ZN2 = XI2*YI3 - XI3*YI2
      B23=(N1*XN2+N2*YN2+N3*ZN2)
C
      XN3 = YI3*ZI4 - YI4*ZI3
      YN3 = ZI3*XI4 - ZI4*XI3
      ZN3 = XI3*YI4 - XI4*YI3
      B34=(N1*XN3+N2*YN3+N3*ZN3)
C
      XN4 = YI4*ZI1 - YI1*ZI4
      YN4 = ZI4*XI1 - ZI1*XI4
      ZN4 = XI4*YI1 - XI1*YI4
      B41=(N1*XN4+N2*YN4+N3*ZN4)
C
      AB1=A23*B41
      AB2=B23*A41
C
      IF(ABS(AB1+AB2)/AREA>EM10)THEN
       SSC=(AB1-AB2)/(AB1+AB2)
      ELSE
       SSC=ZERO
      ENDIF
      IF(ABS(A34/AREA)>EM10)THEN
       AB1=B12*A34
       AB2=B34*A12
       IF(ABS(AB1+AB2)/AREA>EM10)THEN
        TTC=(AB1-AB2)/(AB1+AB2)
       ELSE
        TTC=ZERO
       END IF
      ELSE
       TTC=B12/A12-ONE
       IF(B23<=ZERO.AND.B41<=ZERO)THEN
         IF(-B23/A12<=ALP.AND.-B41/A12<=ALP)SSC=ZERO
       ELSEIF(B23<=ZERO)THEN
         IF(-B23/A12<=ALP) THEN
          SSC=ONE
         ELSE
          SSC=TWO
         END IF
       ELSEIF(B41<=ZERO)THEN
         IF(-B41/A12<=ALP) THEN
          SSC=-ONE
         ELSE
          SSC=-TWO
         END IF
       ENDIF
      ENDIF
C-------------out of seg
      IF(ABS(SSC)>ONE+ALP.OR.ABS(TTC)>ONE+ALP) THEN
        IER=1
C------case tria  re-compute      
        IF (A34==ZERO.AND.TTC< ONE) THEN
             LB=FOURTH*(ONE - TTC)*(ONE - SSC)
             LC=FOURTH*(ONE - TTC)*(ONE + SSC)
             LA = ONE - LB - LC
             IF (LA>=ZERO) THEN
               LB= MIN(ONE,MAX(ZERO,LB))
               LC= MIN(ONE,MAX(ZERO,LC))
             ELSEIF(LC>LB.AND.LC >= ONE) THEN
               LC = ONE
               LB = ZERO
             ELSEIF(LB >= ONE) THEN
               LC = ZERO
               LB = ONE
             ELSE             
               LBS = HALF*(ONE+LB-LC)
               LCS = HALF*(ONE-LB+LC)
               LB= MIN(ONE,MAX(ZERO,LBS))
               LC= MIN(ONE,MAX(ZERO,LCS))
             ENDIF
             SSC= (LC-LB)/(LC+LB)
             TTC= ONE - TWO*LB - TWO*LC 
        END IF
        IF(ABS(SSC)>ONE)SSC=SSC/ABS(SSC)
        IF(ABS(TTC)>ONE)TTC=TTC/ABS(TTC)
      ELSE
        IER=0
        IF(ABS(SSC)>ONE)SSC=SSC/ABS(SSC)
        IF(ABS(TTC)>ONE)TTC=TTC/ABS(TTC)
      ENDIF
C
      TP=FOURTH*(ONE+TTC)
      TM=FOURTH*(ONE-TTC)
C
      SP=ONE+SSC
      SM=ONE-SSC
      H(1)=TM*SM
      H(2)=TM*SP
      H(3)=TP*SP
      H(4)=TP*SM
C
      XC =H(1)*X1+H(2)*X2+H(3)*X3+H(4)*X4
      YC =H(1)*Y1+H(2)*Y2+H(3)*Y3+H(4)*Y4
      ZC =H(1)*Z1+H(2)*Z2+H(3)*Z3+H(4)*Z4
      RETURN
      END
Chd|====================================================================
Chd|  I24PENMAX                     source/interfaces/inter3d1/i24pen3.F
Chd|-- called by -----------
Chd|        I24PEN3                       source/interfaces/inter3d1/i24pen3.F
Chd|-- calls ---------------
Chd|        INTAB                         source/interfaces/inter3d1/i24tools.F
Chd|====================================================================
      SUBROUTINE I24PENMAX(PEN  ,PENMAX,ETYP  ,EL    ,NS   ,
     +                     IXS   ,IXS10 ,IXS16 , IXS20  ,IELIM )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "param_c.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IXS(NIXS,*),IXS10(6,*), IXS16(8,*), IXS20(12,*)
      INTEGER ETYP  ,EL    ,NS,IELIM
C     REAL
      my_real
     .   PEN  ,PENMAX
C-----------------------------------------------
C   External function
C-----------------------------------------------
      LOGICAL INTAB
      EXTERNAL INTAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C     REAL
      INTEGER  EL2
      my_real
     .   S
C----add commun ID--at end--------------------------
C--------eliminier some initial penetrations---------
C------automatic for self_contact
       IELIM=0
       SELECT CASE (ETYP)
         CASE(1)
           IF (INTAB(8,IXS(2,EL),NS)) IELIM=1
         CASE(10)
           EL2=EL-NUMELS8
           IF (INTAB(8,IXS(2,EL),NS).OR.INTAB(6,IXS10(1,EL2),NS))
     +        IELIM=1
         CASE(20)
           EL2=EL-NUMELS8-NUMELS10
           IF (INTAB(8,IXS(2,EL),NS).OR.INTAB(12,IXS20(1,EL2),NS))
     +        IELIM=1
         CASE(16)
           EL2=EL-NUMELS8-NUMELS10-NUMELS20
           IF (INTAB(8,IXS(2,EL),NS).OR.INTAB(8,IXS16(1,EL2),NS))
     +        IELIM=1
       END SELECT
C-------
       IF (PEN >= PENMAX ) IELIM = 1

      RETURN
      END
Chd|====================================================================
Chd|  ICONNET                       source/interfaces/inter3d1/i24pen3.F
Chd|-- called by -----------
Chd|        I24PEN3                       source/interfaces/inter3d1/i24pen3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE ICONNET(IRECT ,IXS   ,KNOD2ELS,NOD2ELS,
     .                   IXS10 ,IXS16 ,IXS20 ,NS  ,ICONN   )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL
      INTEGER IRECT(4), IXS(NIXS,*), KNOD2ELS(*), NOD2ELS(*), 
     .        IXS10(6,*), IXS16(8,*), IXS20(12,*),ICONN,NS
C     REAL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER N, JJ, II, K, NN, KK, IC, IAD
C     REAL
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
C
      ICONN = 0
      IF(NUMELS==0) RETURN
       DO 230 IAD=KNOD2ELS(NS)+1,KNOD2ELS(NS+1)
        N = NOD2ELS(IAD)
        IF(N <= NUMELS8)THEN
          DO JJ=1,4
            II=IRECT(JJ)
            DO K=1,8
              IF(IXS(K+1,N)==II) ICONN = 1
            ENDDO
          ENDDO
        ELSEIF(N <= NUMELS8+NUMELS10)THEN
          DO JJ=1,4
            II=IRECT(JJ)
            DO K=1,8
              IF(IXS(K+1,N)==II) ICONN = 1
            ENDDO
            DO K=1,6
              IF(IXS10(K,N-NUMELS8)==II) ICONN = 1
            ENDDO
          ENDDO
        ELSEIF(N <= NUMELS8+NUMELS10+NUMELS20)THEN
          DO JJ=1,4
            II=IRECT(JJ)
            DO K=1,8
              IF(IXS(K+1,N)==II) ICONN = 1
            ENDDO
            DO K=1,12
              IF(IXS20(K,N-NUMELS8-NUMELS10)==II) ICONN = 1
            ENDDO
          ENDDO
        ELSEIF(N <= NUMELS8+NUMELS10+NUMELS20+NUMELS16)THEN
          DO JJ=1,4
            II=IRECT(JJ)
            DO K=1,8
              IF(IXS(K+1,N)==II) ICONN = 1
            ENDDO
            DO K=1,8
              IF(IXS16(K,N-NUMELS8-NUMELS10-NUMELS20)==II) ICONN = 1
            ENDDO
          ENDDO
        END IF
  230  CONTINUE
      RETURN
      END
C-----------------------------------------------
Chd|====================================================================
Chd|  I24CAND                       source/interfaces/inter3d1/i24pen3.F
Chd|-- called by -----------
Chd|        ININT3                        source/interfaces/inter3d1/inint3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE I24CAND(CAND_E,CAND_N,NSN  ,IRTLM ,II_STOK ,
     *                    MSEGTYP)
C
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
      INTEGER CAND_E(*),CAND_N(*),NSN,IRTLM(2,*),II_STOK,
     *                    MSEGTYP(*)
C     REAL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER E, I,ISH
     .        
C     REAL
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
            II_STOK = 0
            DO I=1,NSN
             E = IRTLM(1,I)
             IF (E >0) THEN
              II_STOK =II_STOK + 1
              CAND_N(II_STOK) = I
              CAND_E(II_STOK) = E

              ISH = ABS(MSEGTYP(E))
              IF (ISH > 0)THEN
                 II_STOK =II_STOK + 1
                 CAND_N(II_STOK) = I
                 CAND_E(II_STOK) = ISH
              ENDIF

             END IF
            END DO          
C      
      RETURN
      END
