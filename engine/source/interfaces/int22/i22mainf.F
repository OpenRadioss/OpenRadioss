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
Chd|  I22MAINF                      source/interfaces/int22/i22mainf.F
Chd|-- called by -----------
Chd|        INTFOP2                       source/interfaces/interf/intfop2.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        I22COR3                       source/interfaces/int22/i22cor3.F
Chd|        I22FOR3                       source/interfaces/int22/i22for3.F
Chd|        I22WETSURF                    source/interfaces/int22/i22wetsurf.F
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|        STARTIME                      source/system/timer.F         
Chd|        STOPTIME                      source/system/timer.F         
Chd|        SUM_6_FLOAT_SENS              source/system/parit.F         
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|        I22TRI_MOD                    ../common_source/modules/cut-cell-search_mod.F
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE I22MAINF(
     1                   IPARI     ,INTBUF_TAB ,X          ,A      ,
     2                   ICODT     ,FSAV       ,V          ,MS        ,DT2T     ,  
     3                   NELTST    ,ITYPTST    ,ITAB       ,STIFN     ,FSKYI    ,  
     4                   ISKY      ,FCONT      ,NIN        ,LINDMAX   ,KINET    ,  
     5                   JTASK     ,NB_JLT     ,NB_JLT_NEW ,NB_STOK_N ,          
     6                   NISKYFI   ,NEWFRONT   ,NSTRF      ,SECFCUM   ,ICONTACT ,
     7                   VISCN     ,NUM_IMP,
     9                   NS_IMP    ,NE_IMP     ,IND_IMP    ,FSAVSUB   ,NRTMDIM  ,  
     A                   FSAVBAG   ,IGRBRIC    ,
     B                   EMINX     ,IXS        ,IXS16      ,IXS20     ,FNCONT   ,  
     C                   FTCONT    ,IAD_ELEM   ,FR_ELEM    ,RCONTACT  ,ACONTACT , 
     D                   PCONTACT  ,TEMP       ,FTHE       ,FTHESKYI,
     E                   PM        ,IPARG      ,IAD17      ,MSKYI_SMS ,ISKYI_SMS,
     F                   NODNX_SMS ,MS0        ,ELBUF_TAB  ,NV46      ,FBSAV6   ,
     G                   ISENSINT  ,DIMFB      ,H3D_DATA   )
C=======================================================================
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C Interface Type22 (/INTER/TYPE22) is an FSI coupling method based on cut cell method. 
C   This experimental cut cell method is not completed, abandonned, and is not an official option.
C
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD 
      USE MOD_NODNORM
      USE I22TRI_MOD
      USE INTBUFDEF_MOD
      USE H3D_MOD
      USE MESSAGE_MOD
      USE GROUPDEF_MOD
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
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "param_c.inc"
#include      "units_c.inc"
#include      "scr03_c.inc"
#include      "scr04_c.inc"
#include      "scr05_c.inc"
#include      "scr07_c.inc"
#include      "warn_c.inc"
#include      "task_c.inc"
#include      "parit_c.inc"
#include      "timeri_c.inc"
#include      "impl1_c.inc"
#include      "scr18_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (ELBUF_STRUCT_), TARGET, DIMENSION(NGROUP) :: ELBUF_TAB
      
      INTEGER NELTST,ITYPTST,NIN,NEWFRONT,NSTRF(*),
     .        NRTMDIM, IAD17,NV46, ISENSINT(*), DIMFB
      INTEGER IPARI(NPARI,NINTER), ICODT(*),ICONTACT(*),
     .        ITAB(*), ISKY(*), KINET(*), 
     .        IPARG(NPARG,*)
      INTEGER NB_JLT,NB_JLT_NEW,NB_STOK_N,JTASK,
     .        NISKYFI, LINDMAX
      INTEGER NUM_IMP,NS_IMP(*),NE_IMP(*),IND_IMP(*)
      INTEGER IXS(*)  ,IXS16(*) ,IXS20(*)
      INTEGER IAD_ELEM(2,*),FR_ELEM(*), 
     .        ISKYI_SMS(*), NODNX_SMS(*)
      my_real 
     .        EMINX(*)
      my_real DT2T,
     .   X(*), A(3,*), FSAV(*), V(3,*),FSAVBAG(*),
     .   MS(*),STIFN(*),FSKYI(LSKYI,4),FCONT(3,*),MS0(*),
     .   SECFCUM(7,NUMNOD,NSECT),VISCN(*), FSAVSUB(*),
     .   FNCONT(3,*), FTCONT(3,*), RCONTACT(*), ACONTACT(*),
     .   PCONTACT(*),
     .   TEMP(*),FTHE(*),FTHESKYI(LSKYI),PM(NPROPM,*),
     .   MSKYI_SMS(*)

      DOUBLE PRECISION FBSAV6(12,6,DIMFB)

      TYPE(INTBUF_STRUCT_) INTBUF_TAB
      TYPE(H3D_DATABASE) :: H3D_DATA
C-----------------------------------------------
      TYPE (GROUP_)  , DIMENSION(NGRBRIC) :: IGRBRIC
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, I_STOK, JLT_NEW, JLT , NFT, IVIS2,
     .        IBC, NOINT, NSEG, ISECIN, IBAG, IADM,
     .        IGAP, INACTI, IFQ, MFROT, IGSTI, NISUB,
     .        NB_LOC, I_STOK_LOC,DEBUT,
     .        ILAGM, LENR, LENT, MAXCC,INTTH,I22GRSH3N,SFSAVPARIT,
     .        IERROR,ISU1
      INTEGER IX1(MVSIZ), IX2(MVSIZ), IX3(MVSIZ), IX4(MVSIZ),
     .        NSVG(MVSIZ), CB_LOC(MVSIZ),CE_LOC(MVSIZ),
     .        CAND_B_N(MVSIZ),CAND_E_N(MVSIZ),KINI(MVSIZ),
     .        INDEX2(LINDMAX),
     .        ISDSIZ(NSPMD+1),IRCSIZ(NSPMD+1),ITAG(NUMNOD),
     .        IELECI(MVSIZ), NSMS(MVSIZ), IAD, J, H
      my_real
     .   STARTT, FRIC, GAP, STOPT,
     .   VISC,VISCF,STIGLO,GAPMIN,
     .   KMIN, KMAX, GAPMAX,RSTIF,FHEAT,TINT,RHOH
C-----------------------------------------------
      my_real
     .     NX1(MVSIZ), NX2(MVSIZ), NX3(MVSIZ), NX4(MVSIZ),
     .     NY1(MVSIZ), NY2(MVSIZ), NY3(MVSIZ), NY4(MVSIZ),
     .     NZ1(MVSIZ), NZ2(MVSIZ), NZ3(MVSIZ), NZ4(MVSIZ),
     .     LB1(MVSIZ), LB2(MVSIZ), LB3(MVSIZ), LB4(MVSIZ),
     .     LC1(MVSIZ), LC2(MVSIZ), LC3(MVSIZ), LC4(MVSIZ),
     .     P1(MVSIZ), P2(MVSIZ), P3(MVSIZ), P4(MVSIZ),
     .   X1(MVSIZ), X2(MVSIZ), X3(MVSIZ), X4(MVSIZ),
     .   Y1(MVSIZ), Y2(MVSIZ), Y3(MVSIZ), Y4(MVSIZ),
     .   Z1(MVSIZ), Z2(MVSIZ), Z3(MVSIZ), Z4(MVSIZ),
     .   XI(MVSIZ), YI(MVSIZ), ZI(MVSIZ), STIF(MVSIZ),
     .   N1(MVSIZ), N2(MVSIZ), N3(MVSIZ), PENE(MVSIZ),
     .   H1(MVSIZ), H2(MVSIZ), H3(MVSIZ), H4(MVSIZ),
     .   GAPV(MVSIZ),VXI(MVSIZ),VYI(MVSIZ),VZI(MVSIZ),MSI(MVSIZ),
     .   TEMPI(MVSIZ),PHI(MVSIZ),AREASI(MVSIZ)
      my_real
     .     , DIMENSION(:,:),ALLOCATABLE :: SURF
      my_real
     .     , DIMENSION(:), ALLOCATABLE ::  PRES
      SAVE SURF,PRES
      my_real
     .      ANGLT, PADM
      INTEGER NRTMFT, NRTMLT, NMNFT, NMNLT, NRADM
      my_real
     .    NNX1(MVSIZ), NNX2(MVSIZ), NNX3(MVSIZ), NNX4(MVSIZ),
     .    NNY1(MVSIZ), NNY2(MVSIZ), NNY3(MVSIZ), NNY4(MVSIZ),
     .    NNZ1(MVSIZ), NNZ2(MVSIZ), NNZ3(MVSIZ), NNZ4(MVSIZ),
     .    N_SCUT(1:3,NBCUT_MAX,MVSIZ),                                !normal for cut surface (1:nbcut<8)
     .    CoG(3,NBCUT_MAX,MVSIZ),
     .    Seff(NBCUT_MAX,MVSIZ), Delta(4,NBCUT_MAX,MVSIZ)
      INTEGER ICURV
      my_real, DIMENSION(:,:,:), ALLOCATABLE,TARGET  :: FSAVPARIT
      my_real, POINTER                               :: pFSAVparit

      IADM      = 0
      
      NRTM      = IPARI(4,NIN)
      NSN       = IPARI(5,NIN)
      NTY       = IPARI(7,NIN)
      NOINT     = IPARI(15,NIN)
      INACTI    = IPARI(22,NIN)    ! unused
      IBAG      = IPARI(32,NIN)     ! Pressure correction 
      NISUB     = IPARI(36,NIN)    
      ISU1      = IPARI(45,NIN)
      INTTH     = IPARI(47,NIN)

      !Geometrical tracking
      I22GRSH3N = IPARI(48,NIN)
C      
      STIGLO    = -INTBUF_TAB%STFAC(1)
      STARTT    = INTBUF_TAB%VARIABLES(3)
      STOPT     = INTBUF_TAB%VARIABLES(11)
      FRIC      = INTBUF_TAB%VARIABLES(1)
      GAP       = INTBUF_TAB%VARIABLES(2)
      GAPMIN    = INTBUF_TAB%VARIABLES(13)
      VISC      = INTBUF_TAB%VARIABLES(14)
      VISCF     = INTBUF_TAB%VARIABLES(15)
      GAPMAX    = INTBUF_TAB%VARIABLES(16)
      KMIN      = INTBUF_TAB%VARIABLES(17)
      KMAX      = INTBUF_TAB%VARIABLES(18)
      RSTIF     = INTBUF_TAB%VARIABLES(20)
      FHEAT     = INTBUF_TAB%VARIABLES(21)
      TINT      = INTBUF_TAB%VARIABLES(22)
      I_STOK    = INTBUF_TAB%I_STOK(1)

      DEBUT     = 0

      !-------------------------------!
      !UNPLUG CONDITIONS              !
      !-------------------------------!      
      IF(STARTT.GT.TT) RETURN
      IF(TT.GT.STOPT)  RETURN

      !-------------------------------!
      !STATIC CUTTING                 !
      !-------------------------------!
          NB_LOC = I_STOK / NTHREAD
          IF (JTASK.EQ.NTHREAD) THEN
            I_STOK_LOC = I_STOK-NB_LOC*(NTHREAD-1)
          ELSE
            I_STOK_LOC = NB_LOC
          ENDIF
          DEBUT = (JTASK-1)*NB_LOC
        I_STOK = 0
      
      !-------------------------------!
      !RECALCULATE DU I_STOK          !
      !-------------------------------!
          DO I = DEBUT+1, DEBUT+I_STOK_LOC
            IF(INTBUF_TAB%CAND_N(I).NE.0) THEN
              I_STOK = I_STOK + 1
              INDEX2(I_STOK) = I
            ENDIF
          ENDDO
          
      !-------------------------------!
      !/DEBUG/CAND/NN                 !
      !-------------------------------!          
        IF (DEBUG(3).GE.1) THEN
          NB_JLT    = NB_JLT + I_STOK_LOC
          NB_STOK_N = NB_STOK_N + I_STOK
        ENDIF
        
c------------------------------------------------
      CALL MY_BARRIER()
      IF(JTASK.EQ.1)THEN
       ALLOCATE(SURF(3,NRTMDIM))
       ALLOCATE(PRES(NRTMDIM))
       DO I = 1, NRTM
         PRES(I)   = ZERO
         SURF(1,I) = ZERO
         SURF(2,I) = ZERO
         SURF(3,I) = ZERO
       ENDDO

!PARITH/ON    TO BE DONE LATER

!       IF(IPARIT.NE.0)THEN
!       !C sort depending on user node ids
!       !C calculate parith/on pressure
!       !C for a given face, node order must be the same one
!         CALL I18TRI(
!     1     INDEX2       ,I_STOK,INTBUF_TAB%CAND_N,ITAB ,NSN ,
!     2     INTBUF_TAB%NSV,NIN   )
!       END IF
      ENDIF
      CALL MY_BARRIER()
c------------------------------------------------
C
        SFSAVPARIT = 0
        NULLIFY(pFSAVparit)
        DO I=1,NISUB+1
          IF(ISENSINT(I)/=0) THEN
            SFSAVPARIT = SFSAVPARIT + 1
          ENDIF
        ENDDO
        IF (SFSAVPARIT .NE. 0) THEN
          ALLOCATE(FSAVPARIT(NISUB+1,11,I_STOK),STAT=IERROR)
          IF(IERROR/=0) THEN
            CALL ANCMSG(MSGID=19,ANMODE=ANINFO,
     .           C1='(/INTER/TYPE22)')
           CALL ARRET(2)
          ENDIF
          DO J=1,I_STOK
            DO I=1,11
              DO H=1,NISUB+1
                FSAVPARIT(H,I,J) = ZERO
              ENDDO
            ENDDO
          ENDDO
        ELSE
          ALLOCATE(FSAVPARIT(0,0,0),STAT=IERROR)
          IF(IERROR/=0) THEN
            CALL ANCMSG(MSGID=19,ANMODE=ANINFO,
     .           C1='(/INTER/TYPE22)')
           CALL ARRET(2)
          ENDIF
        ENDIF
C
      DO NFT = 0 , I_STOK - 1 , NVSIZ
      
        JLT = MIN( NVSIZ, I_STOK - NFT )
        
!        if(ibug22_trivox==1)then
!          print *, "================================================"
!          print *, "  I22MAINF : "
!          print *, "    NFT = ", NFT
!          print *, "    JLT = ", JLT        
!          print *, "================================================"        
!        endif
	       
        ! cand_b & cand_e replaced with cand_b_n & cand_e_n
        CALL I22COR3(
     1  JLT              ,X                 , INTBUF_TAB%IRECTM         ,INTBUF_TAB%NSV            ,CAND_E_N         ,
     2  CAND_B_N         ,INTBUF_TAB%STFM   , INTBUF_TAB%STFNS          ,X1                        ,X2               ,
     3  X3               ,X4                , Y1                        ,Y2                        ,Y3               ,
     4  Y4               ,Z1                , Z2                        ,Z3                        ,Z4               ,
     5  XI               ,YI                , ZI                        ,STIF                      ,IX1              ,
     6  IX2              ,IX3               , IX4                       ,NSVG                      ,IGAP             ,
     7  GAP              ,INTBUF_TAB%GAP_S  , INTBUF_TAB%GAP_M          ,GAPV                      ,MS               ,
     8  VXI              ,VYI      ,
     A  VZI              ,MSI               , NSN                       ,V                         ,KINET            ,
     B  KINI             ,NTY               , NIN                       ,IGSTI                     ,KMIN             ,  
     C  KMAX             ,GAPMAX            , GAPMIN                    ,IADM                      ,INDEX2(NFT+1)    ,
     D  INTTH            ,TEMP              , INTBUF_TAB%CAND_E(1)      ,INTBUF_TAB%CAND_N(1)      ,
     E  TEMPI            ,PHI               , INTBUF_TAB%AREAS          ,INTBUF_TAB%IELEC          ,AREASI           ,
     F  IELECI           ,NODNX_SMS         , NSMS                      ,INTBUF_TAB%GAP_SL         ,INTBUF_TAB%GAP_ML,
     G  IGRBRIC(ISU1)%ENTITY,JTASK)

        CALL I22WETSURF(
     1  JLT              ,CAND_B_N          ,CAND_E_N                   ,CB_LOC                    ,CE_LOC           , 
     2  X1               ,X2                ,X3                         ,X4                        ,Y1               , 
     3  Y2               ,Y3                ,Y4                         ,Z1                        ,Z2               , 
     4  Z3               ,Z4                ,XI                         ,YI                        ,ZI               , 
     5  NX1              ,NX2               ,NX3                        ,NX4                       ,NY1              , 
     6  NY2              ,NY3               ,NY4                        ,NZ1                       ,NZ2              , 
     7  NZ3              ,NZ4               ,LB1                        ,LB2                       ,LB3              , 
     8  LB4              ,LC1               ,LC2                        ,LC3                       ,LC4              , 
     9  P1               ,P2                ,P3                         ,P4                        ,IX1              , 
     A  IX2              ,IX3               ,IX4                        ,NSVG                      ,STIF             , 
     B  JLT_NEW          ,GAPV              ,INACTI                     ,INTBUF_TAB%CAND_P         ,N_SCUT           , 
     C  INDEX2(NFT+1)    ,VXI               ,VYI                        , 
     D  VZI              ,MSI               ,KINI                       ,SURF                      ,IBAG             , 
     E  ITAB             ,INTBUF_TAB%IRECTM ,INTBUF_TAB%I_STOK(1)       ,IXS                       ,NFT              , 
     F  CoG              ,Seff              ,Delta                      ,X)                                        

        !JLT = JLT_NEW
        
        
        !tmp
        JLT_NEW =1
        !-------!
        
        IF (IMONM > 0 .AND. JTASK == 1) CALL STARTIME(20,1)
        IF(JLT_NEW.NE.0) THEN
          IPARI(29,NIN) = 1
          IF (DEBUG(3).GE.1)NB_JLT_NEW = NB_JLT_NEW + JLT_NEW
          IF (SFSAVPARIT .NE. 0) pFSAVparit => FSAVPARIT(1,1,NFT+1)
          CALL I22FOR3(
     1    JLT                ,A                 ,V                 ,IBC               ,ICODT              ,
     2    FSAV               ,GAP               ,FRIC              ,MS                ,VISC               ,
     3    VISCF              ,NOINT             ,INTBUF_TAB%STFNS  ,ITAB              ,CB_LOC             ,
     4    STIGLO             ,STIFN             ,STIF              ,FSKYI             ,ISKY               ,
     5    NX1                ,NX2               ,NX3               ,NX4               ,NY1                ,
     6    NY2                ,NY3               ,NY4               ,NZ1               ,NZ2                ,
     7    NZ3                ,NZ4               ,LB1               ,LB2               ,LB3                ,
     8    LB4                ,LC1               ,LC2               ,LC3               ,LC4                ,
     9    P1                 ,P2                ,P3                ,P4                ,FCONT              ,
     B    IX1                ,IX2               ,IX3               ,IX4               ,NSVG               ,      
     C    IVIS2              ,NELTST            ,ITYPTST           ,DT2T              ,INTTH              ,              
     D    GAPV               ,INACTI            ,INTBUF_TAB%CAND_P ,INDEX2(NFT+1)     ,NISKYFI            ,      
     E    KINET              ,NEWFRONT          ,ISECIN            ,NSTRF             ,SECFCUM            ,      
     F    X                  ,INTBUF_TAB%IRECTM ,CE_LOC            ,MFROT             ,IFQ                ,
     G    INTBUF_TAB%FRIC_P  ,INTBUF_TAB%FTSAVX ,INTBUF_TAB%FTSAVY ,INTBUF_TAB%FTSAVZ ,
     +    INTBUF_TAB%XFILTR  ,
     H    INTBUF_TAB%IFPEN   ,IBAG              ,ICONTACT          ,       
     J    VISCN              ,VXI               ,VYI               ,VZI               ,MSI                ,      
     K    KINI               ,NIN               ,NISUB             ,INTBUF_TAB%LISUB  ,INTBUF_TAB%ADDSUBS ,
     L    INTBUF_TAB%ADDSUBM ,INTBUF_TAB%LISUBS ,INTBUF_TAB%LISUBM ,FSAVSUB           ,
     .    INTBUF_TAB%CAND_N  ,
     M    IPARI(33,NIN)      ,IPARI(39,NIN)     ,PRES              ,FNCONT            ,MS0                , 
     N    N_SCUT             ,SURF              ,CoG               ,CAND_E_N          ,SEFF               , 
     O    ELBUF_TAB          ,X1                ,X2                ,X3                ,X4                 , 
     3    Y1                 ,Y2                ,Y3                ,Y4                ,Z1                 , 
     4    Z2                 ,Z3                ,Z4                ,IXS               ,NV46               ,
     5    Delta              ,ISENSINT          ,pFSAVparit        ,IPARG             ,H3D_DATA           )                 
        ENDIF
        IF (IMONM > 0 .AND. JTASK == 1) CALL STOPTIME(20,1)
      ENDDO
c
c
      IF (SFSAVPARIT /= 0)THEN
          CALL SUM_6_FLOAT_SENS(FSAVPARIT, NISUB+1, 11, I_STOK,1,I_STOK,
     .  			FBSAV6, 12, 6, DIMFB, ISENSINT )
      ENDIF
      IF (ALLOCATED(FSAVPARIT)) DEALLOCATE (FSAVPARIT)
C
      IF(INACTI==7.AND.IBAG.NE.0)THEN                         
        CALL MY_BARRIER()                                     
        IF(JTASK.EQ.1) THEN                                   
!            CALL ASS18(NRTM  ,INTBUF_TAB%IRECTM,A   ,X   ,SURF   ,
!     2                 PRES  ,FCONT        ,IBAG,FSAV,FSAVBAG,
!     3                 FR_I18,ISKY         ,FSKYI    )      
        END IF                                                
        CALL MY_BARRIER()                                     
       IF(JTASK == 1) DEALLOCATE(SURF,PRES)                   
      ENDIF                                                   


      CALL MY_BARRIER()  
	      
      IF(JTASK == 1) THEN
        IF(ALLOCATED(SURF))DEALLOCATE(SURF)
        IF(ALLOCATED(PRES))DEALLOCATE(PRES)        
      ENDIF

C
      RETURN
      END

