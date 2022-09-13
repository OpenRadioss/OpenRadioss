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
Chd|  DELAMINATION                  source/properties/composite_options/stack/delamination.F
Chd|-- called by -----------
Chd|        CMAIN3                        source/materials/mat_share/cmain3.F
Chd|-- calls ---------------
Chd|        DELM01LAW                     source/properties/composite_options/stack/delm01law.F
Chd|        DELM02LAW                     source/properties/composite_options/stack/delm02law.F
Chd|        DELM24LAW                     source/properties/composite_options/stack/delm24law.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE DELAMINATION(ELBUF_STR,
     1           JFT      ,JLT     ,IR     ,IS     ,NPT      ,
     2           MAT_IPLY ,IPM     ,PM     ,BUFMAT ,NPF      ,
     3           TF       ,DT1C    ,NGL    ,OFF    ,TH_IPLY  ,
     4           DEL_PLY  ,SIG     ,OFFI   ,A11    ,FOR      ,
     5           MOM      ,PLY_F   ,THK0   ,SHF    ,EXZ      ,
     6           EYZ      ,AREA    ,PID    ,GEO    ,SSP      ,
     7           POSLY    ,THKLY   ,KXX    ,KYY    ,KXY      ,
     .           DEXZ     ,DEYZ    ,EINT   ,GSTR   ,NEL      )   
C----------------------------------------------
C   M o d u l e s
C----------------------------------------------- 
      USE ELBUFDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include    "implicit_f.inc"
#include    "comlock.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com08_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JFT,JLT,IR,IS,NPT,NEL
      INTEGER MAT_IPLY(MVSIZ,*),IPM(NPROPMI,*),NGL(*),NPF(*),PID(*)
C     REAL
      my_real  
     .   PM(NPROPM,*),  TH_IPLY(MVSIZ,NPT),
     .   DEL_PLY(MVSIZ,3,NPT),OFF(*),TF(*),DT1C(*),
     .   BUFMAT(*), SIG(MVSIZ,3,NPT),OFFI(MVSIZ,*),A11(MVSIZ,NPT),
     .   THK0(*),FOR(NEL,5),PLY_F(MVSIZ,5,NPT),
     .   EXZ(*),EYZ(*),SHF(*),AREA(*),MOM(NEL,3),SSP(*),GEO(NPROPG,*),
     .   POSLY(*),THKLY(*), KXX(*), KYY(*), KXY(*), DEXZ(*), DEYZ(*),
     .   EINT(JLT,2), GSTR(NEL,8)
      TYPE(ELBUF_STRUCT_), TARGET :: ELBUF_STR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,JJ,IG,IC,II,IFL,IPLY,NVARF,IP,NFAIL,JVAR,NVARF_MAX, 
     .   IPMAT_IPLY,JINF,JSUP,NUPAR,NFUNC,
     .   IADBUF,ILAW,IRUPT,IMAT,JJINF,JJSUP,NBDEL,NF,NL,KK(5)
       INTEGER  MAT(MVSIZ),IFAIL(MVSIZ),MATF(MVSIZ),
     .   IFUNC(100),NBIPLY_DEL(MVSIZ),NIPLY_DEL(MVSIZ,NPT),JLY,
     .   NINDX,INDX(MVSIZ)
C     REAL
      my_real
     .  SYZ(MVSIZ),SXZ(MVSIZ),SZZ(MVSIZ) , DU_IPLY(3,MVSIZ),
     .  SYZ0(MVSIZ),SXZ0(MVSIZ),SZZ0(MVSIZ),
     .  EPSYZ_IP(MVSIZ),EPSXZ_IP(MVSIZ),EPSZZ_IP(MVSIZ),REDUC(MVSIZ,NPT),
     .  DEGMB(MVSIZ),DEGFX(MVSIZ)
       my_real
     . NU, G, E33,F4,F5,VOL,G0,DD,FAC,DTINV,SSPI,VISC,
     . WMC,THKY,SCALE,TH(NPT),
     . DEZZ_IP(MVSIZ),DEYZ_IP(MVSIZ),DEXZ_IP(MVSIZ),THI(MVSIZ)
c
      my_real,
     .  DIMENSION(:) ,POINTER    :: UVARF 
      TYPE(BUF_INTLAY_) ,POINTER :: INTLAY
      TYPE(BUF_INTLOC_) ,POINTER :: ILBUF
      TYPE(BUF_FAIL_)   ,POINTER :: FBUF
      TYPE(L_BUFEL_)    ,POINTER :: LBUF     
C======================================================================|
C     EPS POINT EQUIVALENT (au sens energetique)
C-----------------------------------------------------------
C     interface
C----------------------------------------------------------- 
      SIG(JFT:JLT,1:3,1:ELBUF_STR%NINTLAY)=ZERO
!
      DO I=1,5
        KK(I) = NEL*(I-1)
      ENDDO
!
C
      NINDX=0
      DO I=JFT,JLT
        THI(I)= ZERO
        IF(OFF(I)/=ZERO)THEN
           NINDX=NINDX+1
           INDX(NINDX)=I
        END IF
      ENDDO    
             
      DO K=1,NINDX
        I=INDX(K)
        NBIPLY_DEL(I) = 0
        DO IPLY = 1,ELBUF_STR%NINTLAY
          NIPLY_DEL(I,IPLY) = 0
          THI(I) = THI(I) + TH_IPLY(I,IPLY)
        ENDDO
      ENDDO    

      DO IPLY  = 1,ELBUF_STR%NINTLAY
        JINF   = IPLY
        JSUP   = IPLY + 1
        INTLAY => ELBUF_STR%INTLAY(IPLY)
        ILBUF  => ELBUF_STR%INTLAY(IPLY)%ILBUF(IR,IS)
        FBUF   => ELBUF_STR%INTLAY(IPLY)%FAIL(IR,IS)
        NFAIL  = INTLAY%NFAIL
        IMAT   = INTLAY%IMAT
        ILAW   = INTLAY%ILAW
c
        DO K=1,NINDX
          I=INDX(K)
C
C Actuallement on utilise qu'une interface elastique isotrope. (Law01)
C  
c         ILAW = IPM(2,MAT_IPLY(I,IPLY))                                
C         Only with  law59 (K33, K13, K23)                              
c
          E33 = PM(20, MAT_IPLY(I, IPLY))*OFF(I)                        
          G   = PM(22, MAT_IPLY(I, IPLY))*OFF(I)                        
          G0 =  G*TH_IPLY(I,IPLY)*OFF(I)                                
!!          FAC = TH_IPLY(I,IPLY)/THK0(I)
           FAC=   TH_IPLY(I,IPLY)/THI(I)
C                                                                       
          A11(I,IPLY) = E33 + G                                         
          IF(ILAW /= 59) THEN                                           
            G   =  TWO*G/TH_IPLY(I,IPLY)*OFF(I)                        
            E33 =  E33/TH_IPLY(I,IPLY)*OFF(I)                           
            A11(I,IPLY) = E33 + G                                       
            G0 = PM(22, MAT_IPLY(I, IPLY))*OFF(I)                       
          ENDIF                                                         
          SXZ0(I) = G*(DEL_PLY(I,1,JSUP) - DEL_PLY(I,1,JINF))           
          SYZ0(I) = G*(DEL_PLY(I,2,JSUP) - DEL_PLY(I,2,JINF))           
C
C   warns G*GSTR <=> G shell or G interply ? ::
          SIG(I,2,IPLY)=  G*(DEL_PLY(I,1,JSUP) - DEL_PLY(I,1,JINF))
     .                  + G0*GSTR(I,5)  
          SIG(I,1,IPLY)=  G*(DEL_PLY(I,2,JSUP) - DEL_PLY(I,2,JINF))
     .                  + G0*GSTR(I,4) 
          SIG(I,3,IPLY)=  E33*(DEL_PLY(I,3,JSUP) - DEL_PLY(I,3,JINF))         
C                                                                       
          MAT(I) = MAT_IPLY(I,IPLY)                                     
          SYZ(I) =  SIG(I,1,IPLY) ! syz                                 
          SXZ(I) =  SIG(I,2,IPLY) ! sxz                                 
          SZZ(I) =  SIG(I,3,IPLY) ! szz                                 
          SZZ0(I) = SZZ(I)                                              
C
C         compute ply sliding                                           
C 
          DU_IPLY(1,I) = (DEL_PLY(I,1,JSUP) - DEL_PLY(I,1,JINF))        
          DU_IPLY(2,I) = (DEL_PLY(I,2,JSUP) - DEL_PLY(I,2,JINF))        
          DU_IPLY(3,I) = (DEL_PLY(I,3,JSUP) - DEL_PLY(I,3,JINF))        
C                                                                       
C         compute strain                                                
          EPSZZ_IP(I)= DU_IPLY(3,I)/TH_IPLY(I,IPLY)                        
          EPSYZ_IP(I)= DU_IPLY(2,I)/TH_IPLY(I,IPLY)                        
          EPSXZ_IP(I)= DU_IPLY(1,I)/TH_IPLY(I,IPLY)
C 
          II = 3*(I-1)
          DEZZ_IP(I) = EPSZZ_IP(I) - ILBUF%EPS(II + 1)
          DEYZ_IP(I) = EPSYZ_IP(I) - ILBUF%EPS(II + 2)
          DEXZ_IP(I) = EPSXZ_IP(I) - ILBUF%EPS(II + 3)
C
          ILBUF%EPS(II + 1) = EPSZZ_IP(I)
          ILBUF%EPS(II + 2) = EPSYZ_IP(I)
          ILBUF%EPS(II + 3) = EPSXZ_IP(I)
       ENDDO
C----------------------- -----------------------
C              delamination damage model 
C-----------------------------------------------
         DO IFL = 1, NFAIL
           IP = (IFL - 1)*15    
           UVARF  => FBUF%FLOC(IFL)%VAR
           NVARF  =  FBUF%FLOC(IFL)%NVAR
           IRUPT  =  FBUF%FLOC(IFL)%ILAWF  
           NUPAR  = IPM(112+IP, IMAT)                            
           IADBUF = IPM(114+IP, IMAT)                          
           NFUNC  = IPM(115+IP, IMAT) 
           DO I=1,NFUNC                                             
             IFUNC(I)=IPM(115+IP+I,IMAT)                        
           ENDDO
           
!           IFAIL(IC) = IPM(111 + IP ,MAT(1))
!           MATF(IC) = MAT(1)
!           DO I=JFT +1 ,JLT
!             IFAIL(IC) = IPM(111 + IP,MAT(I-1))
!             MATF(IC) = MAT(I-1)
!             IF(IPM(111 + IP ,MAT(I))/=IPM(111 + IP,MAT(I-1)))THEN
!               IC = IC+1
!               MATF(IC)  = MAT(I)
!               IFAIL(IC) = IPM(111 + IP,MAT(I))
!             ENDIF
!           ENDDO
C 
!          NVARF_MAX = 0
!          DO II = 1,IC
!            NUPAR   = IPM(112 + IP,MATF(II))
!            NFUNC   = IPM(115 + IP,MATF(II))
!            IADBUF  = IPM(114 + IP ,MATF(II))
!            NVARF   = IPM(113 + IP ,MATF(II))
!            NVARF_MAX = MAX(NVARF_MAX, NVARF)
!            DO I=1,NFUNC                
!              IFUNC(I)=IPM(115+ IP + I,MATF(II))
!            ENDDO 
C     
          IF (IRUPT == 18) THEN 
C-- ladeveze delamination model
           CALL DELM01LAW(
     1                JLT  ,NUPAR ,NVARF ,NFUNC ,IFUNC,
     2                NPF  ,TF    ,TT    ,DT1C  ,BUFMAT ,
     3                NGL  ,IPLY  ,IPM   ,MAT   ,IP     ,
     4                OFF  ,SYZ0  ,SXZ0  ,SZZ   ,UVARF  , 
     5                OFFI(1,IPLY),REDUC(1,IPLY),INTLAY%COUNT,
     .                                               SYZ, SXZ)
C--- power law
          ELSEIF (IRUPT == 19) THEN
            CALL DELM02LAW(
     1                JLT  ,NUPAR ,NVARF ,NFUNC ,IFUNC,
     2                NPF  ,TF    ,TT    ,DT1C  ,BUFMAT ,
     3                NGL  ,IPLY  ,IPM   ,MAT   ,IP     ,
     4                OFF  ,SYZ0   ,SXZ0   ,SZZ   ,DU_IPLY,
     5                UVARF  ,OFFI(1,IPLY),REDUC(1,IPLY),
     .                INTLAY%COUNT)
C--- total strain per direction
          ELSEIF(IRUPT == 24) THEN
            CALL DELM24LAW(
     1                JLT  ,NUPAR ,NVARF ,NFUNC ,IFUNC,
     2                NPF  ,TF    ,TT    ,DT1C  ,BUFMAT ,
     3                NGL  ,IPLY  ,IPM   ,MAT   ,IP     ,
     4                OFF  ,SYZ0  ,SXZ0  ,SZZ   ,EPSYZ_IP,
     5                EPSXZ_IP,EPSZZ_IP,UVARF ,OFFI(1,IPLY),REDUC(1,IPLY),
     6                INTLAY%COUNT)
          ENDIF 
c
        ENDDO !  IFL = 1, NFAIL
c----------------------------
C          
           DO K=1,NINDX
             I=INDX(K)
C            
            IF (INT(INTLAY%COUNT(I)) == 4) THEN
                FAC = TH_IPLY(I,IPLY)/THK0(I)
                IF(REDUC(I,IPLY) < 0 ) REDUC(I,IPLY) = ZERO
                SIG(I,1,IPLY) =  SYZ0(I) *REDUC(I,IPLY)
                SIG(I,2,IPLY) =  SXZ0(I) *REDUC(I,IPLY)
                SIG(I,3,IPLY) =  SZZ(I)  *REDUC(I,IPLY)
                IF(SZZ0(I) < ZERO)SIG(I,3,IPLY) = SZZ0(I)
C
                NBIPLY_DEL(I) = NBIPLY_DEL(I) + 1 
                NBDEL = NBIPLY_DEL(I)
                NIPLY_DEL(I,NBDEL) = IPLY
C
                OFFI(I,IPLY) = REDUC(I,IPLY)
C
c                DEGMB(I) = - FOR(I,4)*DEXZ(I) - FOR(I,5)*DEYZ(I)                 
c                FAC = TH_IPLY(I,IPLY)/THK0(I) 
c                F4=FOR(I,4)-FAC*G0*EXZ(I)*SHF(I)*OFF(I)                                                         
c                F5=FOR(I,5)-FAC*G0*EYZ(I)*SHF(I)*OFF(I)  
c                IF(F4*FOR(I,4)<=ZERO)THEN
c                  FOR(I,4)=ZERO
c                ELSE
c                  FOR(I,4)=F4
c                END IF                                                       
c                IF(F5*FOR(I,5)<=ZERO)THEN
c                  FOR(I,5)=ZERO
c                ELSE
c                  FOR(I,5)=F5
c                END IF                                                       
c                DEGMB(I) = DEGMB(I) + FOR(I,4)*DEXZ(I)+FOR(I,5)*DEYZ(I)               
c                EINT(I,1)= EINT(I,1) + DEGMB(I)*HALF*THK0(I)*AREA(I)               
C            
            ELSEIF(OFFI(I,IPLY) <  ONE) THEN
C             
               SIG(I,1,IPLY) =  SYZ0(I) *OFFI(I,IPLY)
               SIG(I,2,IPLY) =  SXZ0(I) *OFFI(I,IPLY)
               SIG(I,3,IPLY) =  SZZ(I)  *OFFI(I,IPLY)
               IF(SZZ0(I) < ZERO)SIG(I,3,IPLY) = SZZ0(I)
C                                 
           ELSE
               SIG(I,1,IPLY) =  SYZ0(I)
               SIG(I,2,IPLY) =  SXZ0(I)
               SIG(I,3,IPLY) =  SZZ(I) 
           ENDIF  
C viscosity 
            VISC    =GEO(20,PID(I))
!!          VISC    =GEO(16,PID(I))
!!          IF(VISC == ZERO) VISC = FIVEEM2
            FAC = VISC + SQRT(VISC**2 + ONE)
            DTINV   =DT1C(I)/MAX(DT1C(I)**2,EM20)
            SSPI=SQRT(A11(I,IPLY)*TH_IPLY(I,IPLY)/PM(1,MAT_IPLY(I,IPLY)))
            VISC    =ONEP414*VISC*PM(1,MAT_IPLY(I,IPLY))*SSPI
            VISC    =VISC*SQRT(AREA(I))*DTINV
C           
            SIG(I,1,IPLY)= SIG(I,1,IPLY) + VISC*DEYZ_IP(I)*OFFI(I,IPLY)
            SIG(I,2,IPLY)= SIG(I,2,IPLY) + VISC*DEXZ_IP(I)*OFFI(I,IPLY)
            IF(SIG(I,3,IPLY) < ZERO)THEN
              SIG(I,3,IPLY)= SIG(I,3,IPLY) + VISC*DEZZ_IP(I)
            ELSE
              SIG(I,3,IPLY)= SIG(I,3,IPLY) + VISC*DEZZ_IP(I)*OFFI(I,IPLY)
            END IF
C
C  stress interply (output only)      
!!            II = 3*(I-1)
            ILBUF%SIG(KK(1) + I) = SIG(I,3,IPLY) 
            ILBUF%SIG(KK(2) + I) = SIG(I,1,IPLY) 
            ILBUF%SIG(KK(3) + I) = SIG(I,2,IPLY) 
C
C stability condition  
            A11(I,IPLY) =   A11(I,IPLY) * FAC*FAC	 
 
C  stress       
!!           ILBUF%SIG(KK(1) + I) = SIG(3,I,IPLY)
!!           ILBUF%SIG(KK(2) + I) = SIG(1,I,IPLY)
!!           ILBUF%SIG(KK(3) + I) = SIG(2,I,IPLY)
!!           ILBUF%EPS(II + 1) = EPSZZ_IP(I)
!!           ILBUF%EPS(II + 2) = EPSYZ_IP(I)
!!           ILBUF%EPS(II + 3) = EPSXZ_IP(I)
c           
C Energie          
            IF (IR == 1 .and. IS == 1) INTLAY%EINT(I) = ZERO
            VOL  =   AREA(I)*TH_IPLY(I,IPLY)
            INTLAY%EINT(I) = INTLAY%EINT(I) + HALF*(
     .         SIG(I,3,IPLY)*EPSZZ_IP(I) +
     .         SIG(I,1,IPLY)*EPSYZ_IP(I)+ SIG(I,2,IPLY)*EPSXZ_IP(I))*VOL
c
        ENDDO   ! I=JFT,JLT 
       ENDDO     ! inter ply
C         
C reduction of mom  for shell
       DO K=1,NINDX
           I=INDX(K)
           NBDEL = NBIPLY_DEL(I)
           IF(NBDEL > 0 ) THEN
              DO JJ  =1,NBDEL + 1
                  TH(JJ) = ZERO   
              ENDDO 
              NF = 1                                          
              DO JJ  =1,NBDEL                                 
                 NL = NIPLY_DEL(I,JJ)             
                 DO J =NF,NL  
                     JLY = (J-1)*JLT + I                               
                     TH(JJ) = TH(JJ) + THKLY(JLY)             
                 ENDDO                                        
                 NF = NL                                  
              ENDDO                                           
              JJ = NBDEL + 1                                               
              NL = ELBUF_STR%NINTLAY + 1                               
              DO J = NF,NL 
                  JLY = (J-1)*JLT + I                         
                  TH(JJ) = TH(JJ) + THKLY(JLY)                             
              ENDDO                                                        
              SCALE =ZERO                                                  
              DO JJ  =1,NBDEL + 1                                          
                 SCALE = SCALE + TH(JJ)**3                                 
              ENDDO   
              SCALE = MAX(SCALE, 0.1)
C
              DEGFX(I) = - MOM(I,1)*KXX(I) - MOM(I,2)*KYY(I)                   
     +              - MOM(I,3)*KXY(I)                                     
              MOM(I,1) = SCALE*MOM(I,1) 					    
              MOM(I,2) = SCALE*MOM(I,2) 					    
              MOM(I,3) = SCALE*MOM(I,3) 		   
C
              DEGFX(I) = DEGFX(I) + MOM(I,1)*KXX(I)+MOM(I,2)*KYY(I)                 
     +                  + MOM(I,3)*KXY(I)                                            
              EINT(I,2)= EINT(I,2) + DEGFX(I)*HALF*THK0(I)*THK0(I)*AREA(I) 
         ENDIF                
        ENDDO   ! I=JFT,JLT
C -----------------------------------------------------------------
      RETURN
      END
