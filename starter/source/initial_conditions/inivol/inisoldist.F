Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
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
!||====================================================================
!||    inisoldist       ../starter/source/initial_conditions/inivol/inisoldist.F
!||--- called by ------------------------------------------------------
!||    init_inivol      ../starter/source/initial_conditions/inivol/init_inivol.F90
!||--- calls      -----------------------------------------------------
!||    qcoor2           ../starter/source/elements/solid_2d/quad/qcoor2.F
!||    ratio_fill       ../starter/source/initial_conditions/inivol/ratio_fill.F
!||    s4coor3          ../starter/source/elements/solid/solide4/s4coor3.F
!||    scoor3           ../starter/source/elements/solid/solide/scoor3.F
!||    srcoor3          ../starter/source/elements/solid/solide/srcoor3.F
!||--- uses       -----------------------------------------------------
!||    inivol_def_mod   ../starter/share/modules1/inivol_mod.F
!||====================================================================
       SUBROUTINE INISOLDIST(
     .          IFILL      ,IXS       ,X         ,GEO       ,NG        ,
     .          IPARG      ,IDP       ,IPART_    ,XREFS     ,GLOB_THERM,
     .          NTRACE     ,NTRACE0   ,DIS       ,NSOLTOSF  ,NBIP      ,
     .          NNOD2SURF  ,INOD2SURF ,KNOD2SURF ,SURF_ELTYP,SURF_NODES,
     .          JMID       ,IPHASE    ,INPHASE   ,KVOL      ,SURF_TYPE ,
     .          IAD_BUFR   ,BUFSF     ,NOD_NORMAL,ISOLNOD   ,NBSUBMAT  ,
     .          FILL_RATIO ,ICUMU     ,IDC       ,NBCONTY   ,NSEG      ,
     .          IDSURF     ,SWIFTSURF ,SEGTOSURF ,IGRSURF   ,IVOLSURF  ,
     .          NSURF_INVOL,IXQ       ,IXTG      ,ITYP      ,NEL       , NUMEL_TOT,
     .          NUM_INIVOL ,INIVOL    ,I_INIVOL)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE GROUPDEF_MOD
      USE INIVOL_DEF_MOD , ONLY : INIVOL_STRUCT_
      use glob_therm_mod
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
#include      "param_c.inc"
#include      "vect01_c.inc"
#include      "tabsiz_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER,INTENT(IN) :: I_INIVOL !< inivol identifier
      INTEGER,INTENT(IN) :: NUM_INIVOL !< number of inivol options
      TYPE (INIVOL_STRUCT_), DIMENSION(NUM_INIVOL), INTENT(INOUT) :: INIVOL        !< inivol data structure
      INTEGER,INTENT(IN) :: NG
      INTEGER NTRACE,NTRACE0,IDC,NBCONTY,NSEG, IVOLSURF(NSURF),NUMEL_TOT,NEL
      INTEGER,TARGET :: IXS(NIXS,NUMELS),IXQ(NIXQ,NUMELQ),IXTG(NIXTG,NUMELTG)
      INTEGER IPARG(NPARG,NGROUP),IPART_(*),
     .        IDP,IFILL,NSOLTOSF(NBCONTY,NUMNOD),
     .        NNOD2SURF,KNOD2SURF(NUMNOD+1),JMID,
     .        IPHASE(NBSUBMAT+1,NUMEL_TOT),INPHASE(NTRACE,NEL),
     .        INOD2SURF(NNOD2SURF,NUMNOD),ISOLNOD,ICUMU,SURF_TYPE,IAD_BUFR,
     .        SURF_ELTYP(NSEG),SURF_NODES(NSEG,4),NBIP(NBSUBMAT,NUMEL_TOT),
     .        IDSURF,SWIFTSURF(NSURF),SEGTOSURF(*),NSURF_INVOL,
     .        ITYP
      my_real X(3,NUMNOD),GEO(NPROPG,*),XREFS(8,3,*),
     .        DIS(NSURF_INVOL,NUMNOD),KVOL(NBSUBMAT,NUMEL_TOT),BUFSF(*),
     .        NOD_NORMAL(3,NUMNOD),FILL_RATIO
      INTEGER, INTENT(IN) :: NBSUBMAT
      TYPE (SURF_)   , DIMENSION(NSURF)   :: IGRSURF
      TYPE(glob_therm_) ,intent(in)      :: glob_therm
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER,POINTER :: pIXQ,pIXTG,pIXS
      INTEGER NF1,I,II,JHBE
      INTEGER MAT(MVSIZ), PID(MVSIZ), NGL(MVSIZ),
     .   IX1(MVSIZ),IX2(MVSIZ),IX3(MVSIZ),IX4(MVSIZ),
     .   IX5(MVSIZ),IX6(MVSIZ),IX7(MVSIZ),IX8(MVSIZ)       

      INTEGER IBID
      my_real
     .   X1(MVSIZ),X2(MVSIZ),X3(MVSIZ),X4(MVSIZ),X5(MVSIZ),X6(MVSIZ),
     .   X7(MVSIZ),X8(MVSIZ),Y1(MVSIZ),Y2(MVSIZ),Y3(MVSIZ),Y4(MVSIZ),
     .   Y5(MVSIZ),Y6(MVSIZ),Y7(MVSIZ),Y8(MVSIZ),Z1(MVSIZ),Z2(MVSIZ), 
     .   Z3(MVSIZ),Z4(MVSIZ),Z5(MVSIZ),Z6(MVSIZ),Z7(MVSIZ),Z8(MVSIZ),
     .   RX(MVSIZ) ,RY(MVSIZ),RZ(MVSIZ) ,S_X(MVSIZ) ,
     .   S_Y(MVSIZ) ,S_Z(MVSIZ),TX(MVSIZ) ,TY(MVSIZ) ,TZ(MVSIZ) ,
     .   E1X(MVSIZ),E1Y(MVSIZ),E1Z(MVSIZ),
     .   E2X(MVSIZ),E2Y(MVSIZ),E2Z(MVSIZ),
     .   E3X(MVSIZ),E3Y(MVSIZ),E3Z(MVSIZ),
     .   F1X(MVSIZ),F1Y(MVSIZ),F1Z(MVSIZ),
     .   F2X(MVSIZ),F2Y(MVSIZ),F2Z(MVSIZ)

      my_real RBID
      DOUBLE PRECISION 
     .   XD1(MVSIZ), XD2(MVSIZ), XD3(MVSIZ), XD4(MVSIZ),
     .   XD5(MVSIZ), XD6(MVSIZ), XD7(MVSIZ), XD8(MVSIZ),
     .   YD1(MVSIZ), YD2(MVSIZ), YD3(MVSIZ), YD4(MVSIZ),
     .   YD5(MVSIZ), YD6(MVSIZ), YD7(MVSIZ), YD8(MVSIZ),
     .   ZD1(MVSIZ), ZD2(MVSIZ), ZD3(MVSIZ), ZD4(MVSIZ),
     .   ZD5(MVSIZ), ZD6(MVSIZ), ZD7(MVSIZ), ZD8(MVSIZ)     
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      RBID = ZERO
      IBID = 0

      JHBE  = IPARG(23,NG)
      JCVT  = IPARG(37,NG)

      !common variables required for s*coor3 subroutine
      NFT=IPARG(3,NG)
      NF1=NFT+1
      LFT=1
      LLT=NEL
      ITY=ITYP



      IF(N2D == 0)THEN
        IF ( ISOLNOD == 4 )THEN
          CALL S4COOR3(X    ,XREFS(1,1,NF1),IXS(1,NF1),NGL  ,
     .                 MAT  ,PID  ,IX1  ,IX2  ,IX3  ,IX4  ,
     .                 X1   ,X2   ,X3   ,X4   ,Y1   ,Y2   ,
     .                 Y3   ,Y4   ,Z1   ,Z2   ,Z3   ,Z4   )
        ELSEIF (ISOLNOD == 8) THEN
          IF (JCVT == 0) THEN
            CALL SCOOR3( X   ,XREFS(1,1,NF1) ,IXS(1,NF1)  ,GEO  ,MAT  ,PID  ,NGL  ,  
     .                   IX1 ,IX2            ,IX3         ,IX4  ,IX5  ,IX6  ,IX7  ,IX8,
     .                   X1  ,X2             ,X3          ,X4   ,X5   ,X6   ,X7   ,X8 ,
     .                   Y1  ,Y2             ,Y3          ,Y4   ,Y5   ,Y6   ,Y7   ,Y8  ,
     .                   Z1  ,Z2             ,Z3          ,Z4   ,Z5   ,Z6   ,Z7   ,Z8  ,
     .                   RX  ,RY             ,RZ          ,S_X   ,S_Y   ,S_Z   ,TX   ,TY  ,TZ ,
     .                   E1X ,E1Y            ,E1Z         ,E2X  ,E2Y  ,E2Z  ,E3X  ,E3Y ,E3Z ,
     .                   F1X ,F1Y            ,F1Z         ,F2X  ,F2Y  ,F2Z  ,RBID ,RBID,GLOB_THERM%NINTEMP,
     .                   XD1 ,XD2            ,XD3         ,XD4  ,XD5  ,XD6  ,XD7  ,XD8   ,
     .                   YD1 ,YD2            ,YD3         ,YD4  ,YD5  ,YD6  ,YD7  ,YD8   ,
     .                   ZD1 ,ZD2            ,ZD3         ,ZD4  ,ZD5  ,ZD6  ,ZD7  ,ZD8   )
          ELSE
            CALL SRCOOR3(  X  ,XREFS(1,1,NF1) ,IXS(1,NF1) ,GEO  ,MAT  ,PID  ,NGL  ,JHBE ,
     .                   IX1  ,IX2            ,IX3        ,IX4  ,IX5  ,IX6  ,IX7  ,IX8  ,
     .                   X1   ,X2             ,X3         ,X4   ,X5   ,X6   ,X7   ,X8   ,  
     .                   Y1   ,Y2             ,Y3         ,Y4   ,Y5   ,Y6   ,Y7   ,Y8   ,  
     .                   Z1   ,Z2             ,Z3         ,Z4   ,Z5   ,Z6   ,Z7   ,Z8   ,  
     .                   RX   ,RY             ,RZ         ,S_X   ,S_Y   ,S_Z   ,TX   ,TY   ,TZ  ,
     .                   E1X  ,E1Y            ,E1Z        ,E2X  ,E2Y  ,E2Z  ,E3X  ,E3Y  ,E3Z ,
     .                   F1X  ,F1Y            ,F1Z        ,F2X  ,F2Y  ,F2Z  ,RBID ,RBID  ,GLOB_THERM%NINTEMP,
     .                   XD1  ,XD2            ,XD3        ,XD4  ,XD5  ,XD6  ,XD7  ,XD8   ,
     .                   YD1  ,YD2            ,YD3        ,YD4  ,YD5  ,YD6  ,YD7  ,YD8   ,
     .                   ZD1  ,ZD2            ,ZD3        ,ZD4  ,ZD5  ,ZD6  ,ZD7  ,ZD8   )
          ENDIF
        ENDIF
      ELSEIF(N2D > 0)THEN
        IF(ITYP == 7)THEN
          DO II = 1, NEL
            I = II + NFT                                                                      
            IX1(II) = IXTG(1 + 1, I)                                                          
            IX2(II) = IXTG(1 + 2, I)                                                          
            IX3(II) = IXTG(1 + 3, I)
            X1(II) = ZERO            
            X2(II) = ZERO
            X3(II) = ZERO                                                                      
            Y1(II) = X(2, IXTG(1 + 1, I))                                                 
            Z1(II) = X(3, IXTG(1 + 1, I))                                                 
            Y2(II) = X(2, IXTG(1 + 2, I))                                                 
            Z2(II) = X(3, IXTG(1 + 2, I))                                                 
            Y3(II) = X(2, IXTG(1 + 3, I))                                                 
            Z3(II) = X(3, IXTG(1 + 3, I))                                                 
            NGL(II) = IXTG(6, I)                                                                      
          ENDDO
        ELSEIF(ITYP == 2)THEN
          DO II = 1, NEL
            X1(II) = ZERO            
            X2(II) = ZERO
            X3(II) = ZERO
            X4(II) = ZERO                                    
          ENDDO
          CALL QCOOR2(X, IXQ(1, NF1), NGL, MAT, PID,
     .         IX1, IX2, IX3, IX4, 
     .         Y1, Y2, Y3, Y4, 
     .         Z1, Z2, Z3, Z4,
     .         S_Y, S_Z, TY, TZ)        
        ENDIF
      ENDIF
C-----------
      NULLIFY(pIXS)
      NULLIFY(pIXQ)
      NULLIFY(pIXTG)
      IF(NUMELS>0)  pIXS  => IXS(1,NF1)            
      IF(NUMELQ>0)  pIXQ  => IXQ(1,NF1)    
      IF(N2D>0 .AND. NUMELTG>0) pIXTG => IXTG(1,NF1)

      CALL RATIO_FILL(
     . X1         ,X2         ,X3        ,X4         ,X5            ,X6            ,X7            ,X8         ,
     . Y1         ,Y2         ,Y3        ,Y4         ,Y5            ,Y6            ,Y7            ,Y8         , 
     . Z1         ,Z2         ,Z3        ,Z4         ,Z5            ,Z6            ,Z7            ,Z8         ,
     . IDP        ,X          ,
     . pIXS       ,IPART_(NF1),IFILL     ,NTRACE     ,NTRACE0       ,DIS           ,NSOLTOSF      ,
     . NNOD2SURF  ,INOD2SURF  ,KNOD2SURF ,JMID       ,IPHASE(1,NF1) ,INPHASE       ,KVOL(1,NF1)   ,
     . SURF_TYPE  ,IAD_BUFR   ,BUFSF     ,NOD_NORMAL ,ISOLNOD       ,NBSUBMAT      ,FILL_RATIO    ,ICUMU      ,
     . NSEG       ,SURF_ELTYP ,SURF_NODES,NBCONTY    ,IDC           ,NBIP(1,NF1)   ,IDSURF        ,SWIFTSURF  ,
     . SEGTOSURF  ,IGRSURF    ,IVOLSURF  ,NSURF_INVOL,pIXQ          ,pIXTG         ,ITYP          ,NEL        ,
     . NUMEL_TOT  ,NUM_INIVOL ,INIVOL    ,I_INIVOL)
C-----------------------------------------------------------------
      RETURN
      END
