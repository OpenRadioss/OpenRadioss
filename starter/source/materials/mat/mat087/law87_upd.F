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
Chd|  LAW87_UPD                     source/materials/mat/mat087/law87_upd.F
Chd|-- called by -----------
Chd|        UPDMAT                        source/materials/updmat.F     
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        CRITYLD2000                   source/materials/mat/mat087/law87_upd.F
Chd|        HM_OPTION_IS_ENCRYPTED        source/devtools/hm_reader/hm_option_is_encrypted.F
Chd|        INVERT                        source/constraints/general/rbe3/hm_read_rbe3.F
Chd|        PRODMATVECT                   source/materials/mat/mat087/law87_upd.F
Chd|        R_YLD2000                     source/materials/mat/mat087/law87_upd.F
Chd|        FINTER                        source/tools/curve/finter.F   
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        TABLE_MOD                     share/modules1/table_mod.F    
Chd|====================================================================
      SUBROUTINE LAW87_UPD(IOUT, TITR,MAT_ID,UPARAM,NFUNC,NFUNCT,
     .                        IFUNC, FUNC_ID , NPC   , PLD , PM)
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
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      CHARACTER*nchartitle  :: TITR
      INTEGER MAT_ID,IOUT
      INTEGER ,INTENT(IN) :: NFUNCT
      INTEGER ,INTENT(INOUT) :: NFUNC
      INTEGER, DIMENSION(NFUNC) :: IFUNC
      INTEGER, DIMENSION(NFUNCT):: FUNC_ID
      INTEGER NPC(*)
      my_real :: FINTER,UPARAM(*),PLD(*),PM(NPROPM)
      EXTERNAL FINTER
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER K,J, EXPA,EXPAM2,ERROR,IFLAG,NITER,IOK
 
      my_real 
     .         GAMMA, DELTA ,FCT,YIELD,DX,DY,SCALE,
     .      G1,G2,G3,G13,G23,G33,G15,G25,G35,DYDX,
     .       DF1,DF2,DF3,DF13,DF23,DF33,DF15,DF25,DF35,F1,F2,
     .      AL1, AL2 , AL3 , AL4 ,
     .      AL5, AL6 , AL7 , AL8 ,
     .      S00, S45 , S90 , SB  ,
     .      R00,R45  , R90 , RB  ,X1,X2,X11,X22 ,V1,V11,W11,TAL7,TAL8,
     .      ASWIFT ,EPSO,QVOCE,BETA,KO,ALPHA,NEXP,CONJTF4A8,TF4A8,DAL78(2),
     .      EXPV,KSWIFT,KVOCE,PUIS,PLA,RESIDU,G(2),DAL(8),DFTEST(6,6),DG(2,2),
     .      AL(8),F(8),DF(6,6),DFINV(6,6),RES(6,6),test(3,3),testinv(3,3),DGINV(2,2)
!      
      LOGICAL  IS_ENCRYPTED
C=======================================================================
      IS_ENCRYPTED = .FALSE.
      CALL HM_OPTION_IS_ENCRYPTED(IS_ENCRYPTED)
      S00  = UPARAM(3)
      S45  = UPARAM(4)
      S90  = UPARAM(5)
      SB   = UPARAM(6)
      R00  = UPARAM(7)
      R45  = UPARAM(8)
      R90  = UPARAM(9)
      RB   = UPARAM(10)
      EXPA = INT(UPARAM(15))
      EXPAM2 = EXPA - 2 
      ALPHA   =  UPARAM(16) 
      NEXP    =  UPARAM(17)
      EPSO    =  UPARAM(18) 
      ASWIFT  =  UPARAM(19) 
      IFLAG    =  INT(UPARAM(24)) 
      NFUNC   = NINT(UPARAM(25))
      QVOCE = UPARAM(25+2*NFUNC+1)
      BETA  = UPARAM(25+2*NFUNC+2)
      KO    = UPARAM(25+2*NFUNC+3)

      PLA = TWO*EM03
      IF (IFLAG == 1) THEN 
c-----    Yield                                                    
          PUIS  = EXP(NEXP*LOG((PLA + EPSO)))
!         KSWIFT = ASWIFT*(PLA(I) + EPSO)**NEXP                                       
          KSWIFT = ASWIFT*PUIS
!         VOCE = K0 + Q *(1-EXP (-B*PLA))
          EXPV   = EXP(-BETA*PLA)
          KVOCE  = KO + QVOCE*(ONE - EXPV)                                       
          YIELD  = ALPHA*KSWIFT + (ONE-ALPHA)*KVOCE 
      ELSE !IF TABULATED
          YIELD = FINTER(IFUNC(1),PLA,NPC,PLD,DYDX)
          SCALE = UPARAM(25+1+NFUNC)
          YIELD = YIELD * SCALE

      ENDIF  

      !YIELD TO BE COMPUTED FROM VOCE OR TABULATED FOR 0.05 PLASTIC STRAIN
      AL(1:8) = ONE

          !g1=2/3;
          !d1=-1/3;

          !g2=-1/3;
          !d2=2/3;

          !g3=-1/3;
          !d3=-1/3;
      GAMMA = TWO_THIRD
      DELTA = -THIRD


      ! START NEWTON LOOPS TO FIND AL PARAMETERS FROM 1 TO 6
      RESIDU = EP30
      NITER  = 0
      IOK = 0
      DO WHILE (ABS(RESIDU) > EM06 .AND. NITER < 100. AND . IOK == 0) 

       
      CALL CRITYLD2000(FCT,GAMMA, DELTA, EXPA, AL)      
      F(1) = FCT - TWO * (YIELD /S00 )**EXPA

      CALL CRITYLD2000(FCT,DELTA,GAMMA,  EXPA, AL)
      F(2) = FCT - TWO * (YIELD /S90 )**EXPA

      CALL CRITYLD2000(FCT,DELTA,DELTA,  EXPA, AL)
      F(3) = FCT - TWO * (YIELD /SB )**EXPA

      CALL R_YLD2000(DX,DY,GAMMA, DELTA, EXPA, AL)
      F(4) = (ONE-R00  )*DX - (TWO + R00)*DY

      CALL R_YLD2000(DX,DY, DELTA,GAMMA, EXPA, AL)
      F(5) = (TWO+R90)*DX + (R90 -ONE )*DY

      CALL R_YLD2000(DX,DY,DELTA, DELTA, EXPA, AL)
      F(6) = (ONE+TWO *RB)*DX - (TWO + RB )*DY
 
      !JACOBIAN MATRIX
      DF1 = (AL(1)*TWO_THIRD + AL(2)*THIRD)*ABS(AL(1)*TWO_THIRD + AL(2)*THIRD)**EXPAM2
      DF(1,1) = TWO_THIRD*EXPA*DF1
      DF(1,2) = THIRD*EXPA *DF1
      DF13 = (AL(3)*TWO_THIRD - TWO*AL(4)*THIRD)*ABS(AL(3)*TWO_THIRD - TWO*AL(4)*THIRD)**EXPAM2
      DF(1,3) = TWO_THIRD*EXPA*DF13
      DF(1,4) = -TWO_THIRD*EXPA*DF13
      DF15 = (TWO*AL(5)*TWO_THIRD - AL(6)*THIRD)*ABS(TWO*AL(5)*TWO_THIRD - AL(6)*THIRD)**EXPAM2
      DF(1,5) = TWO*TWO_THIRD*EXPA*DF15
      DF(1,6) = -THIRD*EXPA*DF15

      DF2 = (-AL(1)*THIRD - AL(2)*TWO_THIRD)*ABS(-AL(1)*THIRD - AL(2)*TWO_THIRD)**EXPAM2
      DF(2,1) = -THIRD *EXPA *DF2
      DF(2,2) = -TWO_THIRD*EXPA *DF2
      DF23 = (-AL(3)*THIRD + TWO*AL(4)*TWO_THIRD)*ABS(-AL(3)*THIRD + TWO*AL(4)*TWO_THIRD)**EXPAM2
      DF(2,3) = -THIRD      *EXPA*DF23
      DF(2,4) = TWO*TWO_THIRD *EXPA*DF23
      DF25 = (-TWO*AL(5)*THIRD + AL(6)*TWO_THIRD)*ABS(-TWO*AL(5)*THIRD + AL(6)*TWO_THIRD)**EXPAM2
      DF(2,5) = -TWO_THIRD*EXPA*DF25
      DF(2,6) =  TWO_THIRD*EXPA*DF25

      DF3 = (-AL(1)*THIRD + AL(2)*THIRD)*ABS(-AL(1)*THIRD + AL(2)*THIRD)**EXPAM2
      DF(3,1) = -THIRD*EXPA*DF3
      DF(3,2) = THIRD*EXPA *DF3
      DF33 = (-AL(3)*THIRD - TWO*AL(4)*THIRD)*ABS(-AL(3)*THIRD - TWO*AL(4)*THIRD)**EXPAM2
      DF(3,3) = -THIRD  *EXPA*DF33
      DF(3,4) = -TWO_THIRD *EXPA*DF33
      DF35 = (-TWO*AL(5)*THIRD - AL(6)*THIRD)*ABS(-TWO*AL(5)*THIRD - AL(6)*THIRD)**EXPAM2
      DF(3,5) = -TWO*THIRD*EXPA*DF35
      DF(3,6) = -THIRD     *EXPA*DF35

      G1 = AL(1)*TWO_THIRD+AL(2)*THIRD
      DF(4,1) = ( (ONE-R00  )*G1 + TWO_THIRD*(EXPA-ONE)*( AL(1)*(ONE-R00)+AL(2)*(TWO+R00) )) *ABS(G1)**EXPAM2
      DF(4,2) = ( (TWO+R00)*G1 + THIRD *(EXPA-ONE)*( AL(1)*(ONE-R00)+AL(2)*(TWO+R00) )) *ABS(G1)**EXPAM2
      G13 = AL(3)*TWO_THIRD-TWO*AL(4)*THIRD
      DF(4,3) = ( (ONE-R00  )*G13      + TWO_THIRD*(EXPA-ONE)*( AL(3)*(ONE-R00)-TWO*AL(4)*(TWO+R00) )) *ABS(G13)**EXPAM2
      DF(4,4) = (-TWO*(TWO+R00)*G13 - THIRD *(EXPA-ONE)*( AL(3)*(ONE-R00)-TWO*AL(4)*(TWO+R00) )) *ABS(G13)**EXPAM2
      G15 = TWO*AL(5)*TWO_THIRD-AL(6)*THIRD
      DF(4,5) = ( TWO*(ONE-R00)*G15 + TWO*TWO_THIRD*(EXPA-ONE)*( TWO*AL(5)*(ONE-R00)-AL(6)*(TWO+R00) )) *ABS(G15)**EXPAM2
      DF(4,6) = ( -(TWO+R00)*G15   - THIRD*(EXPA-ONE)      *( TWO*AL(5)*(ONE-R00)-AL(6)*(TWO+R00) )) *ABS(G15)**EXPAM2
C
      G2 = -AL(1)*THIRD-AL(2)*TWO_THIRD
      DF(5,1) = ( (TWO+R90)*G2 -THIRD  *(EXPA-ONE)*( AL(1)*(TWO+R90)+AL(2)*(ONE-R90) )) *ABS(G2)**EXPAM2
      DF(5,2) = ( (ONE-R90  )*G2 -TWO_THIRD *(EXPA-ONE)*( AL(1)*(TWO+R90)+AL(2)*(ONE-R90) )) *ABS(G2)**EXPAM2
      G23 = -AL(3)*THIRD+TWO*AL(4)*TWO_THIRD
      DF(5,3) = ( (TWO+R90)   *G23  -THIRD    *(EXPA-ONE)*( AL(3)*(TWO+R90)-TWO*AL(4)*(ONE-R90) )) *ABS(G23)**EXPAM2
      DF(5,4) = (-TWO*(ONE-R90)*G23  + TWO_THIRD  *(EXPA-ONE)*( AL(3)*(TWO+R90)-TWO*AL(4)*(ONE-R90) )) *ABS(G23)**EXPAM2
      G25 = -TWO*AL(5)*THIRD + AL(6)*TWO_THIRD
      DF(5,5) = ( TWO*(TWO+R90)*G25 - TWO*THIRD*(EXPA-ONE)*( TWO*AL(5)*(TWO+R90)-AL(6)*(ONE-R90) )) *ABS(G25)**EXPAM2
      DF(5,6) = ( -      (ONE-R90)*G25 + TWO_THIRD*(EXPA-ONE)    *( TWO*AL(5)*(TWO+R90)-AL(6)*(ONE-R90) )) *ABS(G25)**EXPAM2

C
      G3 = -AL(1)*THIRD + AL(2)*THIRD
      DF(6,1) = ( (ONE+TWO*RB)*G3 -THIRD  *(EXPA-ONE)*( AL(1)*(ONE+TWO*RB)+AL(2)*(TWO+RB) )) *ABS(G3)**EXPAM2
      DF(6,2) = ( (TWO+RB   )*G3 +THIRD *(EXPA-ONE)* ( AL(1)*(ONE+TWO*RB)+AL(2)*(TWO+RB) )) *ABS(G3)**EXPAM2
      G33 = -AL(3)*THIRD-TWO*AL(4)*THIRD
      DF(6,3) = ( (ONE+TWO*RB)   *G33  -THIRD    *(EXPA-ONE)*( AL(3)*(ONE+TWO*RB)-TWO*AL(4)*(TWO+RB) )) *ABS(G33)**EXPAM2
      DF(6,4) = (-TWO*(TWO+RB) *G33  -THIRD    *(EXPA-ONE)*( AL(3)*(ONE+TWO*RB)-TWO*AL(4)*(TWO+RB) )) *ABS(G33)**EXPAM2
      G35 = -TWO*AL(5)*THIRD - AL(6)*THIRD
      DF(6,5) = ( TWO*(ONE+TWO*RB)*G35 - TWO*THIRD*(EXPA-ONE)*( TWO*AL(5)*(ONE+TWO*RB)-AL(6)*(TWO+RB) )) *ABS(G35)**EXPAM2
      DF(6,6) = ( -      (TWO+RB) *G35 - THIRD     *(EXPA-ONE)*( TWO*AL(5)*(ONE+TWO*RB)-AL(6)*(TWO+RB) )) *ABS(G35)**EXPAM2

      
      CALL INVERT (DF,DFINV,6,ERROR)

      CALL PRODMATVECT(DFINV, F, DAL, 6)
     
      DO K = 1, 6
        AL(K) = AL(K) - DAL(K)
        IF (AL(K) > EP05)THEN
            IOK = 1
            CALL ANCMSG(MSGID=1608 ,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO_BLIND_2,
     .                I1=MAT_ID,
     .                C1=TITR)
           EXIT
        ENDIF
      ENDDO
      RESIDU = SQRT(F(1)**2+F(2)**2+F(3)**2+F(4)**2+F(5)**2+F(6)**2)/6
      NITER = NITER + 1 
      ENDDO ! WHILE ITERATIONS

      !COMPUTE AL(7) AND AL(8)
           X1=(AL(1)+AL(2))/3;
           X2=(AL(1)-AL(2))/3;
           X11=(AL(3)     + TWO*AL(4) + TWO*AL(5)+AL(6)      ) /NINE
           X22=(TWO*AL(5)+      AL(6) - AL(3)     -TWO *AL(4))/THREE


      !BOUCLE
      RESIDU = EP30
      NITER  = 0

      DO WHILE (ABS(RESIDU) > EM06 .AND. NITER < 100 .AND. IOK == 0) 

      TAL7 = SQRT(MAX(ZERO,X2**2  + FOUR * AL(7)**2) )/TWO
      TAL8 = SQRT(MAX(ZERO,X22**2 + FOUR * AL(8)**2) )
      TF4A8=      (THREE*X11 + TAL8 ) / FOUR 
      CONJTF4A8 = (THREE*X11 - TAL8 ) / FOUR 
           V1 =EXPA*(TAL7)**(EXPA-1)
           V11=EXPA*(CONJTF4A8)*(ABS(CONJTF4A8))**(EXPA-2)
           W11=EXPA*(TF4A8)*(ABS(TF4A8))**(EXPA-2)

      F1= TAL7**EXPA + ABS(CONJTF4A8)**EXPA + ABS(TF4A8)**EXPA
      F2 =v1* (X2**2/(TWO*TAL7) )   +  THREE_HALF*X11*(v11+w11) + HALF*(X22**2/TAL8) * (w11-v11)

            
           G(1) = F1 - TWO*(YIELD/S45)**EXPA
           G(2) = F2 - (TWO*EXPA/(ONE+R45))*(YIELD/S45)**EXPA
      !DF(4,7)
      DG(1,1) = EXPA * (AL(7)/(TWO*TAL7) )* ABS(TAL7)**(EXPA-1)
      !DF(4,8)
      DG(1,2) = EXPA *( AL(8) /TAL8) * ( TF4A8 * ABS (TF4A8)**(EXPA-2) 
     .                                - CONJTF4A8 *ABS (CONJTF4A8)**(EXPA-2)  )
      DG(2,1) = (AL(7) * X2**2)/(TWO*TAL7) * (EXPA * (EXPA - ONE) * TAL7**(EXPA-3)
     .                                        - V1 /TAL7/TAL7 )
      DG(2,2) = (EXPA*(EXPA-ONE)*AL(8)/TAL8)
     .         *(THREE_HALF*X11*(TF4A8-CONJTF4A8)+HALF*(X22**2/TAL8)*(TF4A8+CONJTF4A8))
     .         - TWO*(W11-V11)*AL(8)*X22**2/TAL8**3

      CALL INVERT (DG,DGINV,2,ERROR)
      CALL PRODMATVECT(DGINV, G, DAL78, 2)
      DO K = 1, 2
        AL(6+K) = AL(6+K) - DAL78(K)
        IF (AL(6+K) > EP05)THEN
            IOK = 1
            CALL ANCMSG(MSGID=1608 ,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO_BLIND_2,
     .                I1=MAT_ID,
     .                C1=TITR)
           EXIT
        ENDIF
      ENDDO
      RESIDU = SQRT(G(1)**2+G(2)**2)/2
      NITER = NITER + 1 
      ENDDO ! WHILE ITERATIONS

      IF (IOK ==0) THEN
       DO K=1,8
        IF (AL(K) <= ZERO .OR. AL(K) > TEN)THEN

            IOK = 1
            CALL ANCMSG(MSGID=1608 ,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO_BLIND_2,
     .                I1=MAT_ID,
     .                C1=TITR)
           EXIT
        ENDIF
       ENDDO
      ENDIF

      UPARAM(3) = AL(1)
      UPARAM(4) = AL(2)    
      UPARAM(5) = AL(3)
      UPARAM(6) = AL(4)
      UPARAM(7) = AL(5)
      UPARAM(8) = AL(6)
      UPARAM(9) = AL(7)
      UPARAM(10)= AL(8)
      !!
       IF(IS_ENCRYPTED)THEN
          WRITE(IOUT,'(5X,A,//)')'CONFIDENTIAL DATA'
       ELSE  
         IF (IOK == 0) THEN
           WRITE(IOUT,1000) 
           WRITE(IOUT,1001)TRIM(TITR),MAT_ID
           WRITE(IOUT,1200)AL(1), AL(2), AL(3), AL(4 ),
     .     AL(5), AL(6), AL(7), AL(8)
         ENDIF
       ENDIF  
c----------------
c     end of optimization loop
c----------------
      RETURN
 1000 FORMAT
     & (//5X, 'FITTED ALPHA PARAMETERS FOR MATERIAL LAW87 : ' ,/,
     &    5X, ' -----------------------------------------------') 
 1001 FORMAT(
     & 5X,A,/,
     &    5X, 'MATERIAL NUMBER =',I10,//)
 1200 FORMAT(
     & 5X,'ANISOTROPY COEFFICIENT ALPHA1. . . . . .=',1PG20.13/
     & 5X,'ANISOTROPY COEFFICIENT ALPHA2. . . . . .=',1PG20.13/
     & 5X,'ANISOTROPY COEFFICIENT ALPHA3. . . . . .=',1PG20.13/
     & 5X,'ANISOTROPY COEFFICIENT ALPHA4. . . . . .=',1PG20.13/
     & 5X,'ANISOTROPY COEFFICIENT ALPHA5. . . . . .=',1PG20.13/
     & 5X,'ANISOTROPY COEFFICIENT ALPHA6. . . . . .=',1PG20.13/
     & 5X,'ANISOTROPY COEFFICIENT ALPHA7. . . . . .=',1PG20.13/
     & 5X,'ANISOTROPY COEFFICIENT ALPHA8. . . . . .=',1PG20.13/)
c-----------
      RETURN
      END
cc
cc


Chd|====================================================================
Chd|  CRITYLD2000                   source/materials/mat/mat087/law87_upd.F
Chd|-- called by -----------
Chd|        LAW87_UPD                     source/materials/mat/mat087/law87_upd.F
Chd|-- calls ---------------
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE CRITYLD2000(F,G, D, AA, AL)
                  !(FCT,GAMMA, DELTA, EXPA, AL)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C     A r g u m e n t s
C-----------------------------------------------
      INTEGER,  INTENT(IN) ::       AA
C
      my_real, DIMENSION(8), INTENT(IN) ::  AL 
C
      my_real , INTENT(IN) ::  G, D
C
      my_real , INTENT(OUT) ::  F
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------

C=======================================================================
      F =  ( ABS(G*AL(1)      - D*AL(2) ) ) **AA 
     .   + ( ABS(G*AL(3)      + D*TWO*AL(4)) )**AA 
     .   + ( ABS(G*TWO*AL(5)+ D*AL(6)))**AA

      !PRINT*, ' FK ', F
      RETURN
      END
cc
Chd|====================================================================
Chd|  R_YLD2000                     source/materials/mat/mat087/law87_upd.F
Chd|-- called by -----------
Chd|        LAW87_UPD                     source/materials/mat/mat087/law87_upd.F
Chd|-- calls ---------------
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE R_YLD2000(DX,DY,GAMMA, DELTA, A, AL)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C     A r g u m e n t s
C-----------------------------------------------
      INTEGER,  INTENT(IN) ::       A
C
      my_real, DIMENSION(8), INTENT(IN) ::  AL 
C
      my_real , INTENT(IN) ::  GAMMA, DELTA
C
      my_real , INTENT(OUT) ::  DX,DY
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IDFC,IDFD ,AA
 
      my_real 
     .      S00, S45 , S90 , SB  ,
     .      R00,R45  , R90 , RB  

C=======================================================================
       AA = A - 2 
       DX =   AL(1)*(GAMMA*AL(1)-DELTA*AL(2))     *(ABS(GAMMA*AL(1)     - DELTA*AL(2)     ))**AA 
     . +      AL(3)*(GAMMA*AL(3)+DELTA*TWO*AL(4))*(ABS(GAMMA*AL(3)     + DELTA*TWO*AL(4)))**AA 
     . + TWO*AL(5)*(GAMMA*TWO*AL(5)+DELTA*AL(6))*(ABS(GAMMA*TWO*AL(5)+DELTA*AL(6)))**AA

       DY = - AL(2)*(GAMMA*AL(1)-DELTA*AL(2))     *(ABS(GAMMA*AL(1)-DELTA*AL(2)  ))**AA
     .  +   2*AL(4)*(GAMMA*AL(3)+DELTA*2*AL(4))   *(ABS(GAMMA*AL(3)+DELTA*2*AL(4)))**AA 
     .  +     AL(6)*(GAMMA*2*AL(5)+DELTA*AL(6))   *(ABS(GAMMA*2*AL(5)+DELTA*AL(6)))**AA
      RETURN
      END
C=======================================================================
Chd|====================================================================
Chd|  TETA45_YLD2000                source/materials/mat/mat087/law87_upd.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE TETA45_YLD2000(F1,F2,AL,AA,X1,X2,X11,X22)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C     A r g u m e n t s
C-----------------------------------------------
      INTEGER,  INTENT(IN) ::       AA
C
      my_real, DIMENSION(8), INTENT(IN) ::  AL
C
      my_real , INTENT(IN) ::  X1,X2,X11,X22 
C
      my_real , INTENT(OUT) ::  F1,F2
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      my_real 
     .      T1,T2,T3,V1,V11,W11
C-----------------------------------------------

           T1=SQRT(  MAX(ZERO, (X2**2  + FOUR *AL(7)**2) ) )   /TWO
           T2=(THREE*X11 - SQRT( MAX( ZERO,(X22**2 + FOUR*AL(8)**2 ) )   ) )/FOUR
           T3=(THREE*X11 + SQRT( MAX( ZERO,(X22**2 + FOUR*AL(8)**2 ) )   ) )/FOUR
      F1= T1**AA + ABS(T2)**AA + ABS(T3)**AA

           v1 =AA*(T1)**(AA-1);
           v11=AA*(T2)*(ABS(T2))**(AA-2)
           w11=AA*(T3)*(ABS(T3))**(AA-2)
           F2 =v1* (X2**2/(TWO*T1) )   +  THREE_HALF*X11*(v11+w11) + HALF*(X22**2/ (SQRT(X22**2+FOUR*AL(8)**2)  )) * (w11-v11)
      RETURN
      END
C=======================================================================
C=======================================================================
Chd|====================================================================
Chd|  PRODMAT                       source/materials/mat/mat087/law87_upd.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE PRODMAT(A, B, C, N)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
      INTEGER I, J, K, N
      my_real   A(N,N), B(N,N), C(N,N)
C-----------------------------------------------
C
      DO I = 1, N
         DO J = 1, N
            C(I,J) = ZERO
         ENDDO
      ENDDO
      DO I = 1, N
         DO J = 1, N
            DO K = 1, N
               C(I,J) = C(I,J) + A(I,K) * B(K,J)
            ENDDO
         ENDDO
      ENDDO
C
      RETURN
      END 
C
C=======================================================================
C=======================================================================
Chd|====================================================================
Chd|  PRODMATVECT                   source/materials/mat/mat087/law87_upd.F
Chd|-- called by -----------
Chd|        LAW87_UPD                     source/materials/mat/mat087/law87_upd.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE PRODMATVECT(A, B, C, N)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
      INTEGER I, J, K, N
      my_real   A(N,N), B(N), C(N)
C-----------------------------------------------
C
      DO I = 1, N
         C(I) = ZERO
      ENDDO
      DO I = 1, N
         DO J = 1, N
             C(I) = C(I) + A(I,J) * B(J)
         ENDDO
      ENDDO
C
      RETURN
      END 
C
C
C
C
