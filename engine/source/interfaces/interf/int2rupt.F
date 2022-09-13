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
Chd|  INT2RUPT                      source/interfaces/interf/int2rupt.F
Chd|-- called by -----------
Chd|        INTTI1                        source/interfaces/interf/intti1.F
Chd|-- calls ---------------
Chd|        I2RUPT                        source/interfaces/interf/int2rupt.F
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE INT2RUPT(IPARI   ,MS      ,IN      ,
     .                    X       ,V       ,A       ,STIFN   ,IGEO    ,
     .                    WEIGHT  ,FSAV    ,ILEV    ,NPF     ,TF      ,
     .                    ITAB    ,FNCONT  ,PDAMA2  ,INTBUF_TAB,H3D_DATA,
     .                    FNCONTP ,FTCONTP )         
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INTBUFDEF_MOD               
      USE H3D_MOD                     
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ILEV
      INTEGER IPARI(*),WEIGHT(*),IGEO(NPROPGI,*),NPF(*),ITAB(*)
C     REAL
      my_real
     .   MS(*),IN(*), X(*), V(*), A(*),STIFN(*),FSAV(*),TF(*),
     .   FNCONT(*),PDAMA2(*), FNCONTP(*)  ,FTCONTP(* )         

      TYPE(INTBUF_STRUCT_) INTBUF_TAB
      TYPE (H3D_DATABASE) :: H3D_DATA
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com01_c.inc"
#include      "scr05_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER
     .   NUVAR,NSN,NMN,NRTS,NRTM,IGTYP,PID,ISYM
C=======================================================================
      NRTS  = IPARI(3)
      NRTM  = IPARI(4)
      NSN   = IPARI(5)
      NMN   = IPARI(6)
      NUVAR = IPARI(35)
      ISYM  = IPARI(44)
c      
      IF (ILEV == 10 .OR. ILEV == 11 .OR. ILEV == 12) THEN
        PID   = IPARI(43)
        IF(PID > 0) THEN ! on test test_i2_10_0001.rad , PID = 0
          IGTYP = IGEO(11,PID)
        ELSE
          IGTYP = -1
        ENDIF
      ELSE
        PID   = 0 
        IGTYP =-1
      ENDIF      
C
      CALL I2RUPT(
     1     X         ,V          ,A         ,MS        ,IN     ,        
     2     STIFN     ,FSAV       ,WEIGHT          ,INTBUF_TAB%IRECTM,INTBUF_TAB%NSV,               
     3     INTBUF_TAB%MSR        ,INTBUF_TAB%IRTLM,INTBUF_TAB%IRUPT ,INTBUF_TAB%CSTS ,INTBUF_TAB%NMAS(1),          
     4     INTBUF_TAB%NMAS(1+NMN),INTBUF_TAB%SMAS ,INTBUF_TAB%SINER ,INTBUF_TAB%AREAS2,INTBUF_TAB%UVAR   ,       
     5     INTBUF_TAB%XM0        ,INTBUF_TAB%DSM  ,INTBUF_TAB%FSM   ,INTBUF_TAB%RUPT ,IPARI             ,          
     6     NSN       ,NMN       ,NUVAR     ,IGTYP   ,PID       ,          
     7     NPF       ,TF        ,ITAB      ,FNCONT  ,PDAMA2    ,
     8     ISYM      ,INTBUF_TAB%INORM,H3D_DATA,FNCONTP  ,FTCONTP )
C-----------
      RETURN
      END SUBROUTINE INT2RUPT
C
Chd|====================================================================
Chd|  I2RUPT                        source/interfaces/interf/int2rupt.F
Chd|-- called by -----------
Chd|        INT2RUPT                      source/interfaces/interf/int2rupt.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        ENG_USERLIB_UINTBUF_VAR       source/user_interface/dyn_userlib.c
Chd|        ENG_USERLIB_USERINT           source/user_interface/dyn_userlib.c
Chd|        RUPTINT2                      source/interfaces/interf/ruptint2.F
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        UINTBUF_MOD                   source/user_interface/uintbuf_mod.F
Chd|====================================================================
      SUBROUTINE I2RUPT(X      ,V       ,A       ,MS      ,IN      ,
     2                 STIFN   ,FSAV    ,WEIGHT  ,IRECT   ,NSV     , 
     3                 MSR     ,IRTL    ,IRUPT   ,CRST    ,MMASS   , 
     4                 MINER   ,SMASS   ,SINER   ,AREA    ,UVAR    , 
     5                 XSM0    ,DSM     ,FSM     ,PROP    ,IPARI   , 
     6                 NSN     ,NMN     ,NUVAR   ,IGTYP   ,PID     ,
     7                 NPF     ,TF      ,ITAB    ,FNCONT  ,PDAMA2  ,
     8                 ISYM    ,INORM   ,H3D_DATA,FNCONTP ,FTCONTP )                                     
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE UINTBUF_MOD
      USE H3D_MOD
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NSN, NMN,NUVAR,PID,IGTYP,ISYM
      INTEGER IRECT(4,*),MSR(*),NSV(*),IRTL(*),IRUPT(*),IPARI(*),
     .   WEIGHT(*),NPF(*),ITAB(*),PDAMA2(*),INORM(*)
C     REAL
      my_real
     .   X(3,*),V(3,*),A(3,*),XSM0(3,*),DSM(3,*),FSM(3,*),PROP(*),
     .   AREA(*),STIFN(*),SINER(*),SMASS(*),FNCONT(3,*),
     .   MS(*),IN(*),MMASS(*),MINER(*),CRST(2,*),FSAV(*),TF(*),
     .   FNCONTP(3,*)  ,FTCONTP(3,* ) 
      my_real, DIMENSION(NUVAR,NSN) ::
     .   UVAR
      TYPE (H3D_DATABASE) :: H3D_DATA
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
#include      "param_c.inc"
#include      "com01_c.inc"
#include      "com08_c.inc"
#include      "scr05_c.inc"
#include      "scr14_c.inc"
#include      "comlock.inc"
#include      "userlib.inc"
#include      "chara_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NIR,I,J,II,JJ,L,N1,N2,N3,N4,RFLAG,W,IONE,
     .        IMOD,IFILTR,IFUNS,IFUNN,IFUNT,NOINT,IDBG
C     REAL
      my_real
     .   S,T,SP,SM,TP,TM,AA,INS,MXI,MYI,MZI,XSM,YSM,ZSM,XC,YC,ZC,
     .   X0,X1,X2,X3,X4,Y0,Y1,Y2,Y3,Y4,Z0,Z1,Z2,Z3,Z4,SUM,DX,DY,DZ,
     .   FX,FY,FZ,FXI,FYI,FZI,SX,SY,SZ,TX,TY,TZ,DT12M,DTIME,
     .   DXNORM,DXTANG,DXTAN2,FNORM,FTANG,FTAN2,MCDG,
     .   VX,VY,VZ,VNX,VNY,VNZ,VTX,VTY,VTZ,VUX,VUY,VUZ,FACN,FACT,
     .   FSAV1,FSAV2,FSAV3,FSAV4,FSAV5,FSAV6,FSAV7,FSAV8,FSAV9,FSAV10, 
     .   FSAV11,FSAV12,FSAV13,FSAV14,FSAV15,IMPX,IMPY,IMPZ 
      my_real
     .   H(4),DXN(3),DXT(3),DXU(3),FN(3),FT(3),FU(3),FNAREA,FTAREA,ADXTANG
C----
      type(UINTBUF) :: USERBUF
!
      CHARACTER OPTION*256
      INTEGER SIZE
C=======================================================================
      NIR =4
      IONE=1
C         
      IF (TT == ZERO) THEN   
        DO II=1,NSN
          I=NSV(II)
          L=IRTL(II)
          IRUPT(II) = 0
          INORM(II) = 0  
C         initial distance main-secnd
          S = CRST(1,II)                                         
          T = CRST(2,II)                                         
          SP=ONE + S                                              
          SM=ONE - S                                              
          TP=FOURTH*(ONE + T)                                      
          TM=FOURTH*(ONE - T)                                      
          H(1)=TM*SM                                             
          H(2)=TM*SP                                             
          H(3)=TP*SP                                             
          H(4)=TP*SM                                             
          N1  = IRECT(1,L)                                       
          N2  = IRECT(2,L)                                       
          N3  = IRECT(3,L)                                       
          N4  = IRECT(4,L)                                       
          X0  = X(1,I)                                           
          Y0  = X(2,I)                                           
          Z0  = X(3,I)                                           
          X1  = X(1,N1)                                          
          Y1  = X(2,N1)                                          
          Z1  = X(3,N1)                                          
          X2  = X(1,N2)                                          
          Y2  = X(2,N2)                                          
          Z2  = X(3,N2)                                          
          X3  = X(1,N3)                                          
          Y3  = X(2,N3)                                          
          Z3  = X(3,N3)                                          
          X4  = X(1,N4)                                          
          Y4  = X(2,N4)                                          
          Z4  = X(3,N4)                                          
          XC  = X1 * H(1) + X2 * H(2) + X3 * H(3) + X4 * H(4)    
          YC  = Y1 * H(1) + Y2 * H(2) + Y3 * H(3) + Y4 * H(4)    
          ZC  = Z1 * H(1) + Z2 * H(2) + Z3 * H(3) + Z4 * H(4)    
          XSM = X0 - XC
          YSM = Y0 - YC
          ZSM = Z0 - ZC
C
          XSM0(1,II) = XSM
          XSM0(2,II) = YSM
          XSM0(3,II) = ZSM 
          DSM(1,II)  = ZERO                                       
          DSM(2,II)  = ZERO                                        
          DSM(3,II)  = ZERO
          IF (ISYM == 1) THEN                                      
            SX = -(X2 + X3 - X1 - X4)
            SY = -(Y2 + Y3 - Y1 - Y4)
            SZ = -(Z2 + Z3 - Z1 - Z4)
            TX = -(X3 + X4 - X1 - X2)
            TY = -(Y3 + Y4 - Y1 - Y2)
            TZ = -(Z3 + Z4 - Z1 - Z2)
            VNX = SY * TZ - SZ * TY
            VNY = SZ * TX - SX * TZ
            VNZ = SX * TY - SY * TX
            SUM = ONE / SQRT(VNX*VNX + VNY*VNY + VNZ*VNZ)
            VNX = VNX * SUM
            VNY = VNY * SUM
            VNZ = VNZ * SUM
            SUM = VNX*XSM + VNY*YSM + VNZ*ZSM
            INORM(II) = SIGN(IONE,  NINT(SUM))
          ELSE
            INORM(II) = IONE
          ENDIF
        ENDDO
        DT12M = ONE/DT2
      ELSE
        DT12M = ONE/DT12
      ENDIF                 
C---------------
C     Reset initial main & secnd masse /inertia
C---------------
      DO II=1,NMN        
        I=MSR(II)        
        MS(I) = MMASS(II)
        IF (IRODDL /= 0) IN(I) = MINER(II)
      ENDDO              
      DO II=1,NSN                          
        I=NSV(II)                          
        MS(I) = SMASS(II)                   
        IF (IRODDL /= 0) IN(I)= SINER(II)   
      ENDDO                                
C
C------------------------------------------------                  
      DO II=1,NSN
        I=NSV(II)
        L=IRTL(II)
        RFLAG = IRUPT(II)
C
        IF (I > 0) THEN
         IF (IRUPT(II) == 1) THEN
c          rupture totale
           FN(1) = ZERO     
           FN(2) = ZERO     
           FN(3) = ZERO     
           FT(1) = ZERO     
           FT(2) = ZERO     
           FT(3) = ZERO     
           FU(1) = ZERO     
           FU(2) = ZERO     
           FU(3) = ZERO     
           FNORM = ZERO     
           FTANG = ZERO     
         ELSE
           S = CRST(1,II)
           T = CRST(2,II)
           SP=ONE + S
           SM=ONE - S
           TP=FOURTH*(ONE + T)
           TM=FOURTH*(ONE - T)
           H(1)=TM*SM
           H(2)=TM*SP
           H(3)=TP*SP
           H(4)=TP*SM      
C
           N1  = IRECT(1,L)
           N2  = IRECT(2,L)
           N3  = IRECT(3,L)
           N4  = IRECT(4,L)
C------------------------------------------------                  
C         relative deplacement main-secnd
C------------------------------------------------                  
           X0  = X(1,I)
           Y0  = X(2,I)
           Z0  = X(3,I)
           X1  = X(1,N1)
           Y1  = X(2,N1)
           Z1  = X(3,N1)
           X2  = X(1,N2)
           Y2  = X(2,N2)
           Z2  = X(3,N2)
           X3  = X(1,N3)
           Y3  = X(2,N3)
           Z3  = X(3,N3)
           X4  = X(1,N4)
           Y4  = X(2,N4)
           Z4  = X(3,N4)
           XC  = X1 * H(1) + X2 * H(2) + X3 * H(3) + X4 * H(4)  
           YC  = Y1 * H(1) + Y2 * H(2) + Y3 * H(3) + Y4 * H(4)  
           ZC  = Z1 * H(1) + Z2 * H(2) + Z3 * H(3) + Z4 * H(4) 
           XSM = X0 - XC
           YSM = Y0 - YC
           ZSM = Z0 - ZC
C--------- depl relatif total
           DX = XSM - XSM0(1,II)
           DY = YSM - XSM0(2,II)
           DZ = ZSM - XSM0(3,II)
           DSM(1,II) = DX
           DSM(2,II) = DY
           DSM(3,II) = DZ
C------------------------------------------------                  
C          direction normale facette main
C------------------------------------------------                  
           SX = -(X2 + X3 - X1 - X4)
           SY = -(Y2 + Y3 - Y1 - Y4)
           SZ = -(Z2 + Z3 - Z1 - Z4)
           TX = -(X3 + X4 - X1 - X2)
           TY = -(Y3 + Y4 - Y1 - Y2)
           TZ = -(Z3 + Z4 - Z1 - Z2)
           VNX = SY * TZ - SZ * TY
           VNY = SZ * TX - SX * TZ
           VNZ = SX * TY - SY * TX
           SUM = INORM(II) / SQRT(VNX*VNX + VNY*VNY + VNZ*VNZ)
           VNX = VNX * SUM 
           VNY = VNY * SUM
           VNZ = VNZ * SUM
           SUM = ONE / SQRT(SX*SX + SY*SY + SZ*SZ)
           VTX = SX * SUM
           VTY = SY * SUM
           VTZ = SZ * SUM
C------------------------------------------------                  
C         composantes N/T du deplacement
C------------------------------------------------                  
           DXNORM = VNX*DX + VNY*DY + VNZ*DZ
           DXN(1) = VNX*DXNORM
           DXN(2) = VNY*DXNORM
           DXN(3) = VNZ*DXNORM
           DXT(1) = DX - DXN(1)
           DXT(2) = DY - DXN(2)
           DXT(3) = DZ - DXN(3)
           DXTANG = SQRT(DXT(1)**2 + DXT(2)**2 + DXT(3)**2)
C          repere local
           IF (DXTANG > ZERO) THEN
             SUM = ONE / DXTANG
             VTX = DXT(1) * SUM
             VTY = DXT(2) * SUM
             VTZ = DXT(3) * SUM
           ENDIF
           VUX = VNY * VTZ - VNZ * VTY
           VUY = VNZ * VTX - VNX * VTZ
           VUZ = VNX * VTY - VNY * VTX
C------------------------------------------------                  
C         force main/secnd
C------------------------------------------------                  
           IF (IRUPT(II) == 0) THEN
C            pas de rupture - interface cinematique          
             MCDG = ONE/(MS(I)+MS(N1)+MS(N2)+MS(N3)+MS(N4))
             VX = V(1,I)*MS(I)+V(1,N1)*MS(N1)
     .           +V(1,N2)*MS(N2)+V(1,N3)*MS(N3)+V(1,N4)*MS(N4)
             VY = V(2,I)*MS(I)+V(2,N1)*MS(N1)
     .           +V(2,N2)*MS(N2)+V(2,N3)*MS(N3)+V(2,N4)*MS(N4)
             VZ = V(3,I)*MS(I)+V(3,N1)*MS(N1)
     .           +V(3,N2)*MS(N2)+V(3,N3)*MS(N3)+V(3,N4)*MS(N4)
             VX = VX * MCDG
             VY = VY * MCDG
             VZ = VZ * MCDG
             FX = A(1,I) + (V(1,I) - VX)*MS(I)*DT12M
             FY = A(2,I) + (V(2,I) - VY)*MS(I)*DT12M 
             FZ = A(3,I) + (V(3,I) - VZ)*MS(I)*DT12M
           ELSEIF (IRUPT(II) == -1) THEN
C            rupture partielle = spring main/secnd        
             FX  = STIFN(I)*DX
             FY  = STIFN(I)*DY              
             FZ  = STIFN(I)*DZ              
           ENDIF
C          composantes N/T de la forces nodale                 
           FNORM = VNX*FX + VNY*FY + VNZ*FZ                    
           FTANG = VTX*FX + VTY*FY + VTZ*FZ                    
           FTAN2 = VUX*FX + VUY*FY + VUZ*FZ                    
           FN(1) = VNX*FNORM                                   
           FN(2) = VNY*FNORM                                   
           FN(3) = VNZ*FNORM                                   
           FT(1) = VTX*FTANG                                   
           FT(2) = VTY*FTANG                                   
           FT(3) = VTZ*FTANG                                   
           FU(1) = VUX*FTAN2                                   
           FU(2) = VUY*FTAN2                                   
           FU(3) = VUZ*FTAN2                                   
C------------------------------------------------                  
C         Call user subroutine   -  test rupture   
C------------------------------------------------                  
           DTIME  = MAX(DT1,EM20)
           IF(USERL_AVAIL==1.AND.IGTYP /= -1)THEN
              ADXTANG = ABS(DXTANG)   
              FNAREA=FNORM / AREA(II) 
              FTAREA=MAX(ABS(FTANG),ABS(FTAN2)) / AREA(II)
              CALL ENG_USERLIB_UINTBUF_VAR(I,AREA(II),DXNORM,ADXTANG,FNAREA,FTAREA,DTIME,RFLAG)

           ELSE
             USERBUF%ISECND = I                                     
             USERBUF%AREA   = AREA(II)	        			
             USERBUF%DXN    = DXNORM         			     
             USERBUF%DXT    = ABS(DXTANG)       			     
             USERBUF%SIGN   = FNORM / AREA(II)           			     
             USERBUF%SIGT   = MAX(ABS(FTANG),ABS(FTAN2)) / AREA(II)           			     
             USERBUF%DT     = DTIME
             USERBUF%RUPT   = RFLAG
           ENDIF
C
           IF (IGTYP == -1) THEN                                        
             NOINT = IPARI(15)      
             IMOD  = IPARI(43)      
             IFILTR= IPARI(59)      
             IFUNS = IPARI(48)      
             IFUNN = IPARI(49)      
             IFUNT = IPARI(50) 
             IDBG  = 0
               CALL RUPTINT2(                                             
     .            NSN       ,II        ,NUVAR     ,UVAR(1,II),USERBUF  , 
     .            PROP      ,IFUNS     ,IFUNN     ,IFUNT     ,IMOD     ,
     .            IFILTR    ,IDBG      ,NPF       ,TF        ,NOINT    ,
     .            ITAB      ,PDAMA2    ,ISYM      ,H3D_DATA  )    
           ELSEIF (IGTYP == 29) THEN    
             IF(USERL_AVAIL==1)THEN
               CALL ENG_USERLIB_USERINT(IGTYP,
     .            NSN       ,II       ,PID      ,NUVAR     ,  
     .            UVAR(1,II),USERBUF  )      
             ELSE
               ! ----------------
               ! ERROR to be printed & exit
               OPTION='INTERFACE TYPE2 RUPTURE MODEL'
               SIZE=LEN_TRIM(OPTION)
               CALL ANCMSG(MSGID=257,C1=OPTION(1:SIZE),ANMODE=ANINFO)
               CALL ARRET(2)
               ! ----------------
             ENDIF                             
                           
           ELSEIF (IGTYP == 30) THEN                                    
             IF(USERL_AVAIL==1)THEN
               CALL ENG_USERLIB_USERINT(IGTYP,
     .            NSN       ,II       ,PID      ,NUVAR     ,  
     .            UVAR(1,II),USERBUF  )      
             ELSE
               ! ----------------
               ! ERROR to be printed & exit
               OPTION='INTERFACE TYPE2 RUPTURE MODEL'
               SIZE=LEN_TRIM(OPTION)
               CALL ANCMSG(MSGID=257,C1=OPTION(1:SIZE),ANMODE=ANINFO)
               CALL ARRET(2)
               ! ----------------
             ENDIF                           
           ELSEIF (IGTYP == 31) THEN                                    
             IF(USERL_AVAIL==1)THEN
               CALL ENG_USERLIB_USERINT(IGTYP,
     .            NSN       ,II       ,PID      ,NUVAR     ,  
     .            UVAR(1,II),USERBUF  )      
             ELSE
               ! ----------------
               ! ERROR to be printed & exit
               OPTION='INTERFACE TYPE2 RUPTURE MODEL'
               SIZE=LEN_TRIM(OPTION)
               CALL ANCMSG(MSGID=257,C1=OPTION(1:SIZE),ANMODE=ANINFO)
               CALL ARRET(2)
               ! ----------------
             ENDIF                           
           ELSEIF (IGTYP == 37) THEN                                    
             IF(USERL_AVAIL==1)THEN
               CALL ENG_USERLIB_USERINT(IGTYP,
     .            NSN       ,II       ,PID      ,NUVAR     ,  
     .            UVAR(1,II),USERBUF  )      
             ELSE
               ! ----------------
               ! ERROR to be printed & exit
               OPTION='INTERFACE TYPE2 RUPTURE MODEL'
               SIZE=LEN_TRIM(OPTION)
               CALL ANCMSG(MSGID=257,C1=OPTION(1:SIZE),ANMODE=ANINFO)
               CALL ARRET(2)
               ! ----------------
             ENDIF                          
           ELSEIF (IGTYP == 38) THEN                                    
             IF(USERL_AVAIL==1)THEN
               CALL ENG_USERLIB_USERINT(IGTYP,
     .            NSN       ,II       ,PID      ,NUVAR     ,  
     .            UVAR(1,II),USERBUF  )      
             ELSE
               ! ----------------
               ! ERROR to be printed & exit
               OPTION='INTERFACE TYPE2 RUPTURE MODEL'
               SIZE=LEN_TRIM(OPTION)
               CALL ANCMSG(MSGID=257,C1=OPTION(1:SIZE),ANMODE=ANINFO)
               CALL ARRET(2)
               ! ----------------
             ENDIF                          
           ELSEIF (IGTYP == 39) THEN                                    
             IF(USERL_AVAIL==1)THEN
               CALL ENG_USERLIB_USERINT(IGTYP,
     .            NSN       ,II       ,PID      ,NUVAR     ,  
     .            UVAR(1,II),USERBUF  )      
             ELSE
               ! ----------------
               ! ERROR to be printed & exit
               OPTION='INTERFACE TYPE2 RUPTURE MODEL'
               SIZE=LEN_TRIM(OPTION)
               CALL ANCMSG(MSGID=257,C1=OPTION(1:SIZE),ANMODE=ANINFO)
               CALL ARRET(2)
               ! ----------------
             ENDIF                          
           ELSEIF (IGTYP == 40) THEN                                    
             IF(USERL_AVAIL==1)THEN
               CALL ENG_USERLIB_USERINT(IGTYP,
     .            NSN       ,II       ,PID      ,NUVAR     ,  
     .            UVAR(1,II),USERBUF  )      
             ELSE
               ! ----------------
               ! ERROR to be printed & exit
               OPTION='INTERFACE TYPE2 RUPTURE MODEL'
               SIZE=LEN_TRIM(OPTION)
               CALL ANCMSG(MSGID=257,C1=OPTION(1:SIZE),ANMODE=ANINFO)
               CALL ARRET(2)
               ! ----------------
            ENDIF                             
           ELSEIF (IGTYP == 41) THEN                                    
             IF(USERL_AVAIL==1)THEN
               CALL ENG_USERLIB_USERINT(IGTYP,
     .            NSN       ,II       ,PID      ,NUVAR     ,  
     .            UVAR(1,II),USERBUF  )      
             ELSE
               ! ----------------
               ! ERROR to be printed & exit
               OPTION='INTERFACE TYPE2 RUPTURE MODEL'
               SIZE=LEN_TRIM(OPTION)
               CALL ANCMSG(MSGID=257,C1=OPTION(1:SIZE),ANMODE=ANINFO)
               CALL ARRET(2)
               ! ----------------
             ENDIF                             
           ELSEIF (IGTYP == 42) THEN                                    
             IF(USERL_AVAIL==1)THEN
               CALL ENG_USERLIB_USERINT(IGTYP,
     .            NSN       ,II       ,PID      ,NUVAR     ,  
     .            UVAR(1,II),USERBUF  )      
             ELSE
               ! ----------------
               ! ERROR to be printed & exit
               OPTION='INTERFACE TYPE2 RUPTURE MODEL'
               SIZE=LEN_TRIM(OPTION)
               CALL ANCMSG(MSGID=257,C1=OPTION(1:SIZE),ANMODE=ANINFO)
               CALL ARRET(2)
               ! ----------------
             ENDIF                             
           ENDIF                                                        
C
           FACN = USERBUF%FACN                                           
           FACT = USERBUF%FACT                                           
           IRUPT(II) = NINT(USERBUF%RUPT) 
           FN(1) = FN(1) * FACN
           FN(2) = FN(2) * FACN
           FN(3) = FN(3) * FACN
           FT(1) = FT(1) * FACT
           FT(2) = FT(2) * FACT
           FT(3) = FT(3) * FACT
           FU(1) = FU(1) * FACT
           FU(2) = FU(2) * FACT
           FU(3) = FU(3) * FACT
           FNORM = FNORM*FACN
           FTANG = FTANG*FACT
           FTAN2 = FTAN2*FACT
           FSM(1,II) = VNX*FNORM + VTX*FTANG + VUX*FTAN2
           FSM(2,II) = VNY*FNORM + VTY*FTANG + VUY*FTAN2
           FSM(3,II) = VNZ*FNORM + VTZ*FTANG + VUZ*FTAN2
C
         ENDIF
C------------------------------------------------                  
C         Save TH
C------------------------------------------------                  
          W = WEIGHT(I)
          IMPX=FN(1)
          IMPY=FN(2)
          IMPZ=FN(3)
          FSAV1 = IMPX*DT12
          FSAV2 = IMPY*DT12
          FSAV3 = IMPZ*DT12
          FSAV8 = ABS(IMPX)
          FSAV9 = ABS(IMPY)
          FSAV10= ABS(IMPZ)
          FSAV11= FNORM
          IMPX=FT(1)
          IMPY=FT(2)
          IMPZ=FT(3)
          FSAV4 = IMPX*DT12
          FSAV5 = IMPY*DT12
          FSAV6 = IMPZ*DT12
          FSAV12= ABS(IMPX)
          FSAV13= ABS(IMPY)
          FSAV14= ABS(IMPZ)
          FSAV15= FTANG
#include "lockon.inc"
          FSAV(1) = FSAV(1) + FSAV1*W
          FSAV(2) = FSAV(2) + FSAV2*W
          FSAV(3) = FSAV(3) + FSAV3*W
          FSAV(4) = FSAV(4) + FSAV4*W
          FSAV(5) = FSAV(5) + FSAV5*W
          FSAV(6) = FSAV(6) + FSAV6*W
          FSAV(8) = FSAV(8) + FSAV8*W
          FSAV(9) = FSAV(9) + FSAV9*W
          FSAV(10)= FSAV(10)+ FSAV10*W
          FSAV(11)= FSAV(11)+ FSAV11*W 
          FSAV(12)= FSAV(12)+ FSAV12*W  
          FSAV(13)= FSAV(13)+ FSAV13*W  
          FSAV(14)= FSAV(14)+ FSAV14*W  
          FSAV(15)= FSAV(15)+ FSAV15*W  
#include "lockoff.inc"
C---
         IF(ANIM_V(13)+H3D_DATA%N_VECT_CONT2>0) THEN
           FNCONT(1,I) = - (FN(1)+FT(1))  * W
           FNCONT(2,I) = - (FN(2)+FT(2))  * W
           FNCONT(3,I) = - (FN(3)+FT(3))  * W
           DO JJ=1,NIR
             J=IRECT(JJ,L)
             FNCONT(1,J) = FNCONT(1,J) + W *(FN(1)+FT(1))/NIR
             FNCONT(2,J) = FNCONT(2,J) + W *(FN(2)+FT(2))/NIR
             FNCONT(3,J) = FNCONT(3,J) + W *(FN(3)+FT(3))/NIR
           ENDDO
         ENDIF

        IF(ANIM_V(27)+H3D_DATA%N_VECT_PCONT2>0) THEN ! Normal/Tangential forces output
           FNCONTP(1,I) = FN(1) * W
           FNCONTP(2,I) = FN(2) * W
           FNCONTP(3,I) = FN(3) * W
           DO JJ=1,NIR
              J=IRECT(JJ,L)
              FNCONTP(1,J) = FNCONTP(1,J) + FNCONTP(1,I)/NIR
              FNCONTP(2,J) = FNCONTP(2,J) + FNCONTP(2,I)/NIR
              FNCONTP(3,J) = FNCONTP(3,J) + FNCONTP(3,I)/NIR
           ENDDO

           FTCONTP(1,I) = FT(1) * W
           FTCONTP(2,I) = FT(2) * W
           FTCONTP(3,I) = FT(3) * W
           DO JJ=1,NIR
               J=IRECT(JJ,L)
               FTCONTP(1,J) = FTCONTP(1,J) + FTCONTP(1,I)/NIR
               FTCONTP(2,J) = FTCONTP(2,J) + FTCONTP(2,I)/NIR
               FTCONTP(3,J) = FTCONTP(3,J) + FTCONTP(3,I)/NIR
           ENDDO
         ENDIF   
        ENDIF
      ENDDO                              
C-----------
      RETURN
      END SUBROUTINE I2RUPT
