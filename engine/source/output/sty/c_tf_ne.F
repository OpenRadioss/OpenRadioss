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
Chd|  C_TF_NE                       source/output/sty/c_tf_ne.F   
Chd|-- called by -----------
Chd|        OUTP_C_TF                     source/output/sty/outp_c_t.F  
Chd|-- calls ---------------
Chd|        LAYINI                        source/elements/shell/coque/layini.F
Chd|        DRAPE_MOD                     share/modules/drape_mod.F     
Chd|        ELBUFDEF_MOD                  ../common_source/modules/mat_elem/elbufdef_mod.F
Chd|        STACK_MOD                     share/modules/stack_mod.F     
Chd|====================================================================
      SUBROUTINE C_TF_NE(ELBUF_STR,IHBE     ,NEL  ,NPT   ,MLW   ,
     .                   ITY      ,ISTRAIN  ,JJ   ,WA    ,IW    ,
     .                   NLAY     ,NPTR     ,NPTS ,ITHK  ,NFT   ,
     .                   THKE     ,NPG      ,IGTYP,GEO   ,IGEO  ,
     .                   IXFEM    ,ISUBSTACK,STACK,DRAPE_SH4N, DRAPE_SH3N,
     .                   IXC      ,IXTG     ,MPT ,DRAPEG )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD
      USE STACK_MOD
      USE DRAPE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "units_c.inc"
#include      "scr16_c.inc"
C
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IHBE,NEL,NPT,JJ,MLW,ITY,ISTRAIN,IW,NLAY,
     .  NPTR,NPTS,ITHK,NFT,NPG,IGTYP,IGEO(NPROPGI,*),
     .  IXFEM,ISUBSTACK,IXC(NIXC,*),
     .  IXTG(NIXTG,*),MPT
      my_real
     .   WA(*),THKE(*),GEO(NPROPG,*),
     .   THK_LY(NEL,MPT)
      TYPE (ELBUF_STRUCT_), TARGET :: ELBUF_STR
      TYPE (STACK_PLY) :: STACK
      TYPE(DRAPE_) :: DRAPE_SH4N(NUMELC_DRAPE), DRAPE_SH3N(NUMELTG_DRAPE)
      TYPE(DRAPEG_) :: DRAPEG
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,IPT,I1,I2,I3,I4,I5,II(12),
     .        PTF,PTM,PTEP,PTS,NG,IR,IS,LENF,LENM,
     .        LENS,MAT_1,PID_1,LAYNPT_MAX,LAY_MAX,IXLAY,IPT_ALL,
     .        JPOS,IT,NPTT,ILAY,SHIFT,SEDRAPE,NUMEL_DRAPE
      INTEGER MAT(MVSIZ),PID(MVSIZ),MATLY(MVSIZ*NPT)
      my_real
     .   THKLY(MVSIZ*MPT),POSLY(MVSIZ,MPT)
      my_real
     .   FUNC(6),QPG(2,4),PG,MPG,
     .   SIG0(6,MVSIZ),EPS(MVSIZ),MOM0(3,MVSIZ),
     .   SK(2,MVSIZ),ST(2,MVSIZ),MK(2,MVSIZ),MT(2,MVSIZ),
     .   SHK(2,MVSIZ),SHT(2,MVSIZ),Z01(11,11),ZZ
      PARAMETER (PG = .577350269189626)
      PARAMETER (MPG=-.577350269189626)
      DATA QPG/MPG,MPG,PG,MPG,PG,PG,MPG,PG/
      DATA  Z01/
     1 0.       ,0.       ,0.       ,0.       ,0.       ,
     1 0.       ,0.       ,0.       ,0.       ,0.       ,0.       ,
     2 -.5      ,0.5      ,0.       ,0.       ,0.       ,
     2 0.       ,0.       ,0.       ,0.       ,0.       ,0.       ,
     3 -.5      ,0.       ,0.5      ,0.       ,0.       ,
     3 0.       ,0.       ,0.       ,0.       ,0.       ,0.       ,
     4 -.5      ,-.1666667,0.1666667,0.5      ,0.       ,
     4 0.       ,0.       ,0.       ,0.       ,0.       ,0.       ,
     5 -.5      ,-.25     ,0.       ,0.25     ,0.5      ,
     5 0.       ,0.       ,0.       ,0.       ,0.       ,0.       ,
     6 -.5      ,-.3      ,-.1      ,0.1      ,0.3      ,
     6 0.5      ,0.       ,0.       ,0.       ,0.       ,0.       ,
     7 -.5      ,-.3333333,-.1666667,0.0      ,0.1666667,
     7 0.3333333,0.5      ,0.       ,0.       ,0.       ,0.       ,
     8 -.5      ,-.3571429,-.2142857,-.0714286,0.0714286,
     8 0.2142857,0.3571429,0.5      ,0.       ,0.       ,0.       ,
     9 -.5      ,-.375    ,-.25     ,-.125    ,0.0      ,
     9 0.125    ,0.25     ,0.375    ,0.5      ,0.       ,0.       ,
     A -.5      ,-.3888889,-.2777778,-.1666667,0.0555555,
     A 0.0555555,0.1666667,0.2777778,0.3888889,0.5      ,0.       ,
     B -.5      ,-.4      ,-.3      ,-.2      ,-.1      ,
     B 0.       ,0.1      ,0.2      ,0.3      ,0.4      ,0.5      /
C
      TYPE(BUF_LAY_) ,POINTER :: BUFLY
      TYPE(G_BUFEL_) ,POINTER :: GBUF
      TYPE(L_BUFEL_) ,POINTER :: LBUF
C-----------------------------------------------
      GBUF => ELBUF_STR%GBUF
!
      DO I=1,12  ! length max of GBUF%G_HOURG = 12
        II(I) = NEL*(I-1)
      ENDDO
!
C
      SHIFT = 1+NFT
      IF (ITY == 7) SHIFT = SHIFT + NUMELC
C
      IF (ITY == 3) THEN
        MAT_1 = IXC(1,1+NFT)
        PID_1 = IXC(6,1+NFT)
      ELSEIF (ITY == 7) THEN
        MAT_1 = IXTG(1,1+NFT)
        PID_1 = IXTG(5,1+NFT)
      ENDIF
      DO I=1,NEL
        MAT(I)= MAT_1
        PID(I)= PID_1
      ENDDO
C
      IXLAY = 0
C
      IF(ITY == 7) THEN
         NUMEL_DRAPE = NUMELTG_DRAPE
         SEDRAPE = STDRAPE
        CALL LAYINI(
     .      ELBUF_STR,1    ,NEL             ,GEO              ,IGEO    ,
     .      MAT      ,PID  ,THKLY           ,MATLY            ,POSLY   ,
     .      IGTYP    ,IXFEM,IXLAY           ,NLAY             ,NPT     ,
     .      ISUBSTACK,STACK,DRAPE_SH3N        ,NFT            ,THKE    ,
     .      NEL      ,THK_LY  ,DRAPEG%INDX_SH3N ,SEDRAPE,NUMEL_DRAPE   )
      ELSE
        NUMEL_DRAPE = NUMELC_DRAPE  
        SEDRAPE = SCDRAPE
       CALL LAYINI(
     .      ELBUF_STR,1    ,NEL             ,GEO              ,IGEO    ,
     .      MAT      ,PID  ,THKLY           ,MATLY            ,POSLY   ,
     .      IGTYP    ,IXFEM,IXLAY           ,NLAY             ,NPT     ,
     .      ISUBSTACK,STACK,DRAPE_SH4N        ,NFT            ,THKE    ,
     .      NEL      ,THK_LY  ,DRAPEG%INDX_SH4N,SEDRAPE,NUMEL_DRAPE     )      
      ENDIF  
C------------------------
C        STRESS
C------------------------
      IF (MLW == 1 .OR. MLW == 3 .OR. MLW == 23) MPT=0
C
      IF (IHBE == 23) THEN
        NPG = 4
        DO I=1,NEL
          ST(1,I)=   GBUF%HOURG(II(1)+I)
          ST(2,I)=  -GBUF%HOURG(II(2)+I)
          MT(1,I)=   GBUF%HOURG(II(3)+I)
          MT(2,I)=  -GBUF%HOURG(II(4)+I)
          SK(1,I)=  -GBUF%HOURG(II(7)+I)
          SK(2,I)=   GBUF%HOURG(II(8)+I)
          MK(1,I)=  -GBUF%HOURG(II(9)+I)
          MK(2,I)=   GBUF%HOURG(II(10)+I)
          SHT(1,I)=  GBUF%HOURG(II(5)+I)
          SHT(2,I)= -GBUF%HOURG(II(6)+I)
          SHK(1,I)= -GBUF%HOURG(II(11)+I)
          SHK(2,I)=  GBUF%HOURG(II(12)+I)
        ENDDO
      ENDIF 
C
      IF (IW == 0) THEN
C
C------SMP-USE--------------
C
        IF (IHBE == 23) THEN     ! QEPH 
          IF (MPT == 0) THEN
            DO I=1,NEL
              SIG0(1,I) = GBUF%FOR(II(1)+I)  
              SIG0(2,I) = GBUF%FOR(II(2)+I)  
              SIG0(3,I) = GBUF%FOR(II(3)+I)  
              SIG0(4,I) = GBUF%FOR(II(4)+I)  
              SIG0(5,I) = GBUF%FOR(II(5)+I)  
              IF (GBUF%G_PLA > 0) THEN
                EPS(I) = GBUF%PLA(I)
              ELSE
                EPS(I) = ZERO
              ENDIF
              MOM0(1,I) = GBUF%MOM(II(1)+I)  
              MOM0(2,I) = GBUF%MOM(II(2)+I)  
              MOM0(3,I) = GBUF%MOM(II(3)+I)
            ENDDO
c
            DO I=1,NEL
              IF (OUTYY_FMT == 2) THEN
                IF (ITHK > 0) THEN
                  WRITE(IUGEO,'(2I8/,1P3E12.5)')
     .            MPT,NPG,GBUF%THK(I),GBUF%EINT(I),GBUF%EINT(I+NEL)
                ELSE
                  WRITE(IUGEO,'(2I8/,1P3E12.5)')
     .            MPT,NPG,THKE(I+NFT),GBUF%EINT(I),GBUF%EINT(I+NEL)
                ENDIF
              ELSE
                IF (ITHK > 0) THEN
                  WRITE(IUGEO,'(2I10/,1P3E20.13)')
     .            MPT,NPG,GBUF%THK(I),GBUF%EINT(I),GBUF%EINT(I+NEL)
                ELSE
                  WRITE(IUGEO,'(2I10/,1P3E20.13)')
     .            MPT,NPG,THKE(I+NFT),GBUF%EINT(I),GBUF%EINT(I+NEL)
                ENDIF
              ENDIF 
              DO K=1,NPG
                FUNC(1)=SIG0(1,I)+ST(1,I)*QPG(2,K)+SK(1,I)*QPG(1,K)
                FUNC(2)=SIG0(2,I)+ST(2,I)*QPG(2,K)+SK(2,I)*QPG(1,K)
                FUNC(3)=SIG0(3,I)
                FUNC(4)=SIG0(4,I)+SHT(2,I)*QPG(2,K)+SHK(2,I)*QPG(1,K)
                FUNC(5)=SIG0(5,I)+SHT(1,I)*QPG(2,K)+SHK(1,I)*QPG(1,K)
                FUNC(6)=EPS(I)
                IF (OUTYY_FMT == 2) THEN
                  WRITE(IUGEO,'(1P6E12.5)')(FUNC(J),J=1,6)
                ELSE
                  WRITE(IUGEO,'(1P6E20.13)')(FUNC(J),J=1,6)
                ENDIF 
                FUNC(1)=MOM0(1,I)+MT(1,I)*QPG(2,K)+MK(1,I)*QPG(1,K)
                FUNC(2)=MOM0(2,I)+MT(2,I)*QPG(2,K)+MK(2,I)*QPG(1,K)
                FUNC(3)=MOM0(3,I)
                IF (OUTYY_FMT == 2) THEN
                  WRITE(IUGEO,'(1P3E12.5)')(FUNC(J),J=1,3)
                ELSE
                  WRITE(IUGEO,'(1P3E20.13)')(FUNC(J),J=1,3)
                ENDIF 
              ENDDO ! DO K=1,NPG
            ENDDO ! DO I=1,NEL
c---
          ELSE  ! QEPH, MPT > 0
c---
            DO I=1,NEL
!!              I5 = (I-1) * 2
              IF (OUTYY_FMT == 2) THEN
                IF (ITHK > 0) THEN
                  WRITE(IUGEO,'(2I8/,1P3E12.5)')
     .            MPT,NPG,GBUF%THK(I),GBUF%EINT(I),GBUF%EINT(I+NEL)
                ELSE
                  WRITE(IUGEO,'(2I8/,1P3E12.5)')
     .            MPT,NPG,THKE(I+NFT),GBUF%EINT(I),GBUF%EINT(I+NEL)
                ENDIF
              ELSE
                IF (ITHK > 0) THEN
                  WRITE(IUGEO,'(2I10/,1P3E20.13)')
     .            MPT,NPG,GBUF%THK(I),GBUF%EINT(I),GBUF%EINT(I+NEL)
                ELSE
                  WRITE(IUGEO,'(2I10/,1P3E20.13)')
     .            MPT,NPG,THKE(I+NFT),GBUF%EINT(I),GBUF%EINT(I+NEL)
                ENDIF
              ENDIF 
C
              IF (NLAY == 1) THEN
                BUFLY => ELBUF_STR%BUFLY(1)
                NPTT  = BUFLY%NPTT   ! MPT = NPTT
                DO IT = 1,NPTT
                  LBUF => BUFLY%LBUF(1,1,IT)
                  ZZ = GBUF%THK(I)*Z01(IT,NPTT)
                  SIG0(1,I) = LBUF%SIG(II(1)+I)
                  SIG0(2,I) = LBUF%SIG(II(2)+I)
                  SIG0(3,I) = LBUF%SIG(II(3)+I)
                  SIG0(4,I) = LBUF%SIG(II(4)+I)
                  SIG0(5,I) = LBUF%SIG(II(5)+I)
                  DO K=1,NPG
                    FUNC(1)=SIG0(1,I)+(ST(1,I)+ZZ*MT(1,I))*QPG(2,K)+
     .                                (SK(1,I)+ZZ*MK(1,I))*QPG(1,K)
                    FUNC(2)=SIG0(2,I)+(ST(2,I)+ZZ*MT(2,I))*QPG(2,K)+
     .                                (SK(2,I)+ZZ*MK(2,I))*QPG(1,K)
                    FUNC(3)=SIG0(3,I)
                    FUNC(4)=SIG0(4,I)+SHT(2,I)*QPG(2,K)+SHK(2,I)*QPG(1,K)
                    FUNC(5)=SIG0(5,I)+SHT(1,I)*QPG(2,K)+SHK(1,I)*QPG(1,K)
                    IF (BUFLY%L_PLA > 0) THEN
                      FUNC(6)=LBUF%PLA(I)
                    ELSE
                      FUNC(6)=ZERO
                    ENDIF
                    IF (OUTYY_FMT == 2) THEN
                      WRITE(IUGEO,'(1P6E12.5)')(FUNC(J),J=1,6)
                    ELSE
                      WRITE(IUGEO,'(1P6E20.13)')(FUNC(J),J=1,6)
                    ENDIF 
                  ENDDO ! DO K=1,NPG
                ENDDO ! DO IT = 1,NPTT
              ELSEIF (NLAY > 1) THEN
                IPT_ALL = 0
                DO ILAY = 1,NLAY
                  BUFLY => ELBUF_STR%BUFLY(ILAY)
                  NPTT  = BUFLY%NPTT
                  DO IT=1,NPTT
                    IPT = IPT_ALL + IT        ! count all NPTT through all layers
                    ZZ = GBUF%THK(I)*POSLY(I,IPT)
                    LBUF => BUFLY%LBUF(1,1,IT)
                    SIG0(1,I) = LBUF%SIG(II(1)+I)
                    SIG0(2,I) = LBUF%SIG(II(2)+I)
                    SIG0(3,I) = LBUF%SIG(II(3)+I)
                    SIG0(4,I) = LBUF%SIG(II(4)+I)
                    SIG0(5,I) = LBUF%SIG(II(5)+I)
                    DO K=1,NPG
                      FUNC(1)=SIG0(1,I)+(ST(1,I)+ZZ*MT(1,I))*QPG(2,K)+
     .                                  (SK(1,I)+ZZ*MK(1,I))*QPG(1,K)
                      FUNC(2)=SIG0(2,I)+(ST(2,I)+ZZ*MT(2,I))*QPG(2,K)+
     .                                  (SK(2,I)+ZZ*MK(2,I))*QPG(1,K)
                      FUNC(3)=SIG0(3,I)
                      FUNC(4)=SIG0(4,I)+SHT(2,I)*QPG(2,K)+SHK(2,I)*QPG(1,K)
                      FUNC(5)=SIG0(5,I)+SHT(1,I)*QPG(2,K)+SHK(1,I)*QPG(1,K)
C
                      IF (BUFLY%L_PLA > 0) THEN
                        FUNC(6)=LBUF%PLA(I)
                      ELSE
                        FUNC(6)=ZERO
                      ENDIF
                      IF (OUTYY_FMT == 2) THEN
                        WRITE(IUGEO,'(1P6E12.5)')(FUNC(J),J=1,6)
                      ELSE
                        WRITE(IUGEO,'(1P6E20.13)')(FUNC(J),J=1,6)
                      ENDIF 
                    ENDDO ! DO K=1,NPG
                  ENDDO ! DO IT=1,NPTT
                  IPT_ALL = IPT_ALL + NPTT
                ENDDO ! DO ILAY = 1,NLAY
              ENDIF ! IF (NLAY == 1)
            ENDDO ! DO I=1,NEL
          ENDIF ! IF (MPT == 0)
c------
        ELSEIF (IHBE == 11) THEN    ! QBAT,DKT18
c------
          LENF = NEL*GBUF%G_FORPG/NPG
          LENM = NEL*GBUF%G_MOMPG/NPG
          LENS = NEL*GBUF%G_STRPG/NPG
          IF (MPT == 0) THEN
            DO I=1,NEL
              IF (OUTYY_FMT == 2) THEN
                IF (ITHK > 0) THEN
                  WRITE(IUGEO,'(2I8/,1P3E12.5)')
     .            MPT,NPG,GBUF%THK(I),GBUF%EINT(I),GBUF%EINT(I+NEL)
                ELSE
                  WRITE(IUGEO,'(2I8/,1P3E12.5)')
     .            MPT,NPG,THKE(I+NFT),GBUF%EINT(I),GBUF%EINT(I+NEL)
                ENDIF
              ELSE
                IF (ITHK > 0) THEN
                  WRITE(IUGEO,'(2I10/,1P3E20.13)')
     .            MPT,NPG,GBUF%THK(I),GBUF%EINT(I),GBUF%EINT(I+NEL)
                ELSE
                  WRITE(IUGEO,'(2I10/,1P3E20.13)')
     .            MPT,NPG,THKE(I+NFT),GBUF%EINT(I),GBUF%EINT(I+NEL)
                ENDIF
              ENDIF 
C---
              BUFLY => ELBUF_STR%BUFLY(1)
              DO  IS=1,NPTS  
                DO  IR=1,NPTR  
                  LBUF => ELBUF_STR%BUFLY(1)%LBUF(IR,IS,1)
                  NG = NPTR*(IS-1) + IR
                  PTF = (NG-1)*LENF
                  PTM = (NG-1)*LENM
                  FUNC(1) = GBUF%FORPG(PTF+II(1)+I)
                  FUNC(2) = GBUF%FORPG(PTF+II(2)+I)
                  FUNC(3) = GBUF%FORPG(PTF+II(3)+I)
                  FUNC(4) = GBUF%FORPG(PTF+II(4)+I)
                  FUNC(5) = GBUF%FORPG(PTF+II(5)+I)
                  IF (BUFLY%L_PLA > 0) THEN    
                    FUNC(6) = LBUF%PLA(I)
                  ELSE                             
                    FUNC(6) = ZERO                  
                  ENDIF                            
                  IF (OUTYY_FMT == 2) THEN
                    WRITE(IUGEO,'(1P6E12.5)')(FUNC(J),J=1,6)
                  ELSE
                    WRITE(IUGEO,'(1P6E20.13)')(FUNC(J),J=1,6)
                  ENDIF
                  FUNC(1) = GBUF%MOMPG(PTM+II(1)+I)
                  FUNC(2) = GBUF%MOMPG(PTM+II(2)+I)
                  FUNC(3) = GBUF%MOMPG(PTM+II(3)+I)
                  IF (OUTYY_FMT == 2) THEN
                   WRITE(IUGEO,'(1P3E12.5)')(FUNC(J),J=1,3)
                  ELSE
                   WRITE(IUGEO,'(1P3E20.13)')(FUNC(J),J=1,3)
                  ENDIF
                ENDDO ! DO  IR=1,NPTR
              ENDDO ! DO  IS=1,NPTS 
            ENDDO  !  DO I=1,NEL
c---
          ELSE  ! MPT > 0
            DO I=1,NEL
!!              I5 = (I-1) * 2
              IF (OUTYY_FMT == 2) THEN
                IF (ITHK > 0) THEN
                  WRITE(IUGEO,'(2I8/,1P3E12.5)')
     .            MPT,NPG,GBUF%THK(I),GBUF%EINT(I),GBUF%EINT(I+NEL)
                ELSE
                  WRITE(IUGEO,'(2I8/,1P3E12.5)')
     .            MPT,NPG,THKE(I+NFT),GBUF%EINT(I),GBUF%EINT(I+NEL)
                ENDIF
              ELSE
                IF (ITHK > 0) THEN
                  WRITE(IUGEO,'(2I10/,1P3E20.13)')
     .            MPT,NPG,GBUF%THK(I),GBUF%EINT(I),GBUF%EINT(I+NEL)
                ELSE
                  WRITE(IUGEO,'(2I10/,1P3E20.13)')
     .            MPT,NPG,THKE(I+NFT),GBUF%EINT(I),GBUF%EINT(I+NEL)
                ENDIF
              ENDIF 
c---
              IF (NLAY == 1) THEN
                BUFLY => ELBUF_STR%BUFLY(1)
                NPTT = BUFLY%NPTT  !  MPT = NPTT
                DO IT = 1,NPTT
                  DO IS = 1,NPTS
                    DO IR = 1,NPTR
                      LBUF => BUFLY%LBUF(IR,IS,IT)
                      FUNC(1) = LBUF%SIG(II(1)+I)
                      FUNC(2) = LBUF%SIG(II(2)+I)
                      FUNC(3) = LBUF%SIG(II(3)+I)
                      FUNC(4) = LBUF%SIG(II(4)+I)
                      FUNC(5) = LBUF%SIG(II(5)+I)
                      IF (BUFLY%L_PLA > 0) THEN
                        FUNC(6)=LBUF%PLA(I)
                      ELSE
                        FUNC(6)=ZERO
                      ENDIF
                      IF (OUTYY_FMT == 2) THEN
                        WRITE(IUGEO,'(1P6E12.5)')(FUNC(J),J=1,6)
                      ELSE
                        WRITE(IUGEO,'(1P6E20.13)')(FUNC(J),J=1,6)
                      ENDIF
                    ENDDO ! DO IR = 1,NPTR
                  ENDDO ! DO IS = 1,NPTS
                ENDDO ! IT = 1,NPTT
              ELSEIF (NLAY > 1) THEN
                DO ILAY=1,NLAY
                  BUFLY => ELBUF_STR%BUFLY(ILAY)
                  NPTT  = BUFLY%NPTT
                  DO IT = 1,NPTT
                    DO IS = 1,NPTS
                      DO IR = 1,NPTR
                        LBUF => ELBUF_STR%BUFLY(ILAY)%LBUF(IR,IS,IT)
                        FUNC(1) = LBUF%SIG(II(1)+I)
                        FUNC(2) = LBUF%SIG(II(2)+I)
                        FUNC(3) = LBUF%SIG(II(3)+I)
                        FUNC(4) = LBUF%SIG(II(4)+I)
                        FUNC(5) = LBUF%SIG(II(5)+I)
                        IF (BUFLY%L_PLA > 0) THEN
                          FUNC(6) = LBUF%PLA(I)
                        ELSE
                          FUNC(6)=ZERO
                        ENDIF
                        IF (OUTYY_FMT == 2) THEN
                          WRITE(IUGEO,'(1P6E12.5)')(FUNC(J),J=1,6)
                        ELSE
                          WRITE(IUGEO,'(1P6E20.13)')(FUNC(J),J=1,6)
                        ENDIF
                      ENDDO ! DO IR = 1,NPTR
                    ENDDO ! DO IS = 1,NPTS
                  ENDDO ! DO IT = 1,NPTT
                ENDDO ! DO ILAY=1,NLAY
              ENDIF ! IF (NLAY == 1)
c---
            ENDDO ! DO I=1,NEL
          ENDIF ! IF (MPT == 0)
        ELSE ! IF (IHBE == 23)
C----error message------
        ENDIF
C
C------SPMD-USE--------------
C
      ELSE  ! IF (IW == 1)
C---QEPH:------
        IF (IHBE == 23) THEN
C---Transfer to QBAT------
          IF (MPT == 0) THEN
            DO I=1,NEL
              SIG0(1,I) = GBUF%FOR(II(1)+I)  
              SIG0(2,I) = GBUF%FOR(II(2)+I)  
              SIG0(3,I) = GBUF%FOR(II(3)+I)  
              SIG0(4,I) = GBUF%FOR(II(4)+I)  
              SIG0(5,I) = GBUF%FOR(II(5)+I)  
              IF (GBUF%G_PLA > 0) THEN
                EPS(I) = GBUF%PLA(I)  
              ELSE
                EPS(I) = ZERO
              ENDIF
              MOM0(1,I) = GBUF%MOM(II(1)+I)  
              MOM0(2,I) = GBUF%MOM(II(2)+I)  
              MOM0(3,I) = GBUF%MOM(II(3)+I)
            ENDDO
C
            DO I=1,NEL
              WA(JJ+1) = IHBE
              JJ=JJ+1
              WA(JJ+1) = MPT
              WA(JJ+2) = NPG
              IF (ITHK > 0) THEN
                WA(JJ+3) = GBUF%THK(I)
              ELSE
                WA(JJ+3) = THKE(I+NFT)
              ENDIF
              WA(JJ+4) = GBUF%EINT(I)
              WA(JJ+5) = GBUF%EINT(I+NEL)
              JJ = JJ + 5
              DO K=1,NPG
                WA(JJ+1)=SIG0(1,I)+ST(1,I)*QPG(2,K)+SK(1,I)*QPG(1,K)
                WA(JJ+2)=SIG0(2,I)+ST(2,I)*QPG(2,K)+SK(2,I)*QPG(1,K)
                WA(JJ+3)=SIG0(3,I)
                WA(JJ+4)=SIG0(4,I)+SHT(2,I)*QPG(2,K)+SHK(2,I)*QPG(1,K)
                WA(JJ+5)=SIG0(5,I)+SHT(1,I)*QPG(2,K)+SHK(1,I)*QPG(1,K)
                WA(JJ+6)=EPS(I)
                WA(JJ+7)=MOM0(1,I)+MT(1,I)*QPG(2,K)+MK(1,I)*QPG(1,K)
                WA(JJ+8)=MOM0(2,I)+MT(2,I)*QPG(2,K)+MK(2,I)*QPG(1,K)
                WA(JJ+9)=MOM0(3,I)
                JJ = JJ + 9
              ENDDO
            ENDDO 
          ELSE ! IF (MPT /= 0)
            DO I=1,NEL
!!              I5 = (I-1) * 2
              WA(JJ+1) = IHBE
              JJ=JJ+1
              WA(JJ+1) = MPT
              WA(JJ+2) = NPG
              IF (ITHK > 0) THEN
                WA(JJ+3) = GBUF%THK(I)
              ELSE
                WA(JJ+3) = THKE(I+NFT)
              ENDIF
              WA(JJ+4) = GBUF%EINT(I)
              WA(JJ+5) = GBUF%EINT(I+NEL)
              JJ = JJ + 5
C
              IF (NLAY == 1) THEN
                BUFLY => ELBUF_STR%BUFLY(1)
                NPTT  = BUFLY%NPTT  !  MPT = NPTT
                DO IT=1,NPTT
                  LBUF => BUFLY%LBUF(1,1,IT)
                  ZZ = GBUF%THK(I)*Z01(IT,NPTT)
                  SIG0(1,I) = LBUF%SIG(II(1)+I)
                  SIG0(2,I) = LBUF%SIG(II(2)+I)
                  SIG0(3,I) = LBUF%SIG(II(3)+I)
                  SIG0(4,I) = LBUF%SIG(II(4)+I)
                  SIG0(5,I) = LBUF%SIG(II(5)+I)
C
                  DO K=1,NPG
                    WA(JJ+1)=SIG0(1,I)+(ST(1,I)+ZZ*MT(1,I))*QPG(2,K)+
     .                                 (SK(1,I)+ZZ*MK(1,I))*QPG(1,K)
                    WA(JJ+2)=SIG0(2,I)+(ST(2,I)+ZZ*MT(2,I))*QPG(2,K)+
     .                                 (SK(2,I)+ZZ*MK(2,I))*QPG(1,K)
                    WA(JJ+3)=SIG0(3,I)
                    WA(JJ+4)=SIG0(4,I)+SHT(2,I)*QPG(2,K)+SHK(2,I)*QPG(1,K)
                    WA(JJ+5)=SIG0(5,I)+SHT(1,I)*QPG(2,K)+SHK(1,I)*QPG(1,K)
C
                    IF (BUFLY%L_PLA > 0) THEN
                      WA(JJ+6)=LBUF%PLA(I)
                    ELSE
                      WA(JJ+6)=ZERO
                    ENDIF
                    JJ = JJ + 6
                  ENDDO ! DO K=1,NPG
                ENDDO ! DO IT=1,NPTT
              ELSEIF (NLAY > 1) THEN
                IPT_ALL = 0
                DO ILAY=1,NLAY
                  BUFLY => ELBUF_STR%BUFLY(ILAY)
                  NPTT  = BUFLY%NPTT
                  DO IT=1,NPTT
                    IPT = IPT_ALL + IT        ! count all NPTT through all layers
                    ZZ = GBUF%THK(I)*POSLY(I,IPT)
                    LBUF => BUFLY%LBUF(1,1,IT)
                    SIG0(1,I) = LBUF%SIG(II(1)+I)
                    SIG0(2,I) = LBUF%SIG(II(2)+I)
                    SIG0(3,I) = LBUF%SIG(II(3)+I)
                    SIG0(4,I) = LBUF%SIG(II(4)+I)
                    SIG0(5,I) = LBUF%SIG(II(5)+I)
                    DO K=1,NPG
                      WA(JJ+1)=SIG0(1,I)+(ST(1,I)+ZZ*MT(1,I))*QPG(2,K)+
     .                                   (SK(1,I)+ZZ*MK(1,I))*QPG(1,K)
                      WA(JJ+2)=SIG0(2,I)+(ST(2,I)+ZZ*MT(2,I))*QPG(2,K)+
     .                                   (SK(2,I)+ZZ*MK(2,I))*QPG(1,K)
                      WA(JJ+3)=SIG0(3,I)
                      WA(JJ+4)=SIG0(4,I)+SHT(2,I)*QPG(2,K)+SHK(2,I)*QPG(1,K)
                      WA(JJ+5)=SIG0(5,I)+SHT(1,I)*QPG(2,K)+SHK(1,I)*QPG(1,K)
C
                      IF (BUFLY%L_PLA > 0) THEN
                        WA(JJ+6)=LBUF%PLA(I)
                      ELSE
                        WA(JJ+6)=ZERO
                      ENDIF
                      JJ = JJ + 6
                    ENDDO ! DO K=1,NPG
                  ENDDO ! DO IT=1,NPTT
                  IPT_ALL = IPT_ALL + NPTT
                ENDDO ! DO ILAY=1,NLAY
              ENDIF ! IF (NLAY == 1)
            ENDDO ! DO I=1,NEL
          ENDIF ! IF (MPT == 0)
        ELSEIF (IHBE == 11) THEN
C-------QBAT,DKT18-----
          LENF = NEL*GBUF%G_FORPG/NPG
          LENM = NEL*GBUF%G_MOMPG/NPG
          LENS = NEL*GBUF%G_STRPG/NPG
          IF (MPT == 0) THEN
            DO I=1,NEL
              WA(JJ+1) = IHBE
              JJ=JJ+1
              WA(JJ+1) = MPT
              WA(JJ+2) = NPG
              IF (ITHK > 0) THEN
                WA(JJ+3) = GBUF%THK(I)
              ELSE
                WA(JJ+3) = THKE(I+NFT)
              ENDIF
              WA(JJ+4) = GBUF%EINT(I)
              WA(JJ+5) = GBUF%EINT(I+NEL)
              JJ = JJ + 5
C---
              DO IR=1,NPTR  
                DO IS=1,NPTS  
                  NG = NPTR*(IS-1) + IR
                  PTF = (NG-1)*LENF
                  PTM = (NG-1)*LENM
                  PTS = (NG-1)*LENS
!!                  I3 = PTS + I
                  WA(JJ+1) = GBUF%FORPG(PTF+II(1)+I)
                  WA(JJ+2) = GBUF%FORPG(PTF+II(2)+I)
                  WA(JJ+3) = GBUF%FORPG(PTF+II(3)+I)
                  WA(JJ+4) = GBUF%FORPG(PTF+II(4)+I)
                  WA(JJ+5) = GBUF%FORPG(PTF+II(5)+I)
                  WA(JJ+6) = GBUF%STRPG(PTS+II(1)+I)
                  WA(JJ+7) = GBUF%MOMPG(PTM+II(1)+I)
                  WA(JJ+8) = GBUF%MOMPG(PTM+II(2)+I)
                  WA(JJ+9) = GBUF%MOMPG(PTM+II(3)+I)
                  JJ = JJ + 9
                ENDDO
              ENDDO
            ENDDO ! DO I=1,NEL
c---
          ELSE  ! IF (MPT /= 0)
            DO I=1,NEL
              WA(JJ+1) = IHBE
              JJ=JJ+1
              WA(JJ+1) = MPT
              WA(JJ+2) = NPG
              IF (ITHK > 0) THEN
                WA(JJ+3) = GBUF%THK(I)
              ELSE
                WA(JJ+3) = THKE(I+NFT)
              ENDIF
              WA(JJ+4) = GBUF%EINT(I)
              WA(JJ+5) = GBUF%EINT(I+NEL)
              JJ = JJ + 5
c---
              IF (NLAY == 1) THEN
                BUFLY => ELBUF_STR%BUFLY(1)
                NPTT = BUFLY%NPTT  !  MPT = NPTT
                DO IT = 1,NPTT
                  DO IS = 1,NPTS
                    DO IR = 1,NPTR
                      LBUF => BUFLY%LBUF(IR,IS,IT)
                      WA(JJ+1) = LBUF%SIG(II(1)+I)
                      WA(JJ+2) = LBUF%SIG(II(2)+I)
                      WA(JJ+3) = LBUF%SIG(II(3)+I)
                      WA(JJ+4) = LBUF%SIG(II(4)+I)
                      WA(JJ+5) = LBUF%SIG(II(5)+I)
                      IF (BUFLY%L_PLA > 0) THEN
                        WA(JJ+6) = LBUF%PLA(I)
                      ELSE
                        WA(JJ+6)=ZERO
                      ENDIF
                      JJ = JJ + 6
                    ENDDO ! DO IR = 1,NPTR
                  ENDDO ! DO IR = 1,NPTR
                ENDDO !IT = 1,NPTT
              ELSEIF (NLAY > 1) THEN
                DO ILAY=1,NLAY
                  BUFLY => ELBUF_STR%BUFLY(ILAY)
                  NPTT  = BUFLY%NPTT
                  DO IT = 1,NPTT
                    DO IS = 1,NPTS
                      DO IR = 1,NPTR
                        LBUF => ELBUF_STR%BUFLY(ILAY)%LBUF(IR,IS,IT)
                        WA(JJ+1) = LBUF%SIG(II(1)+I)
                        WA(JJ+2) = LBUF%SIG(II(2)+I)
                        WA(JJ+3) = LBUF%SIG(II(3)+I)
                        WA(JJ+4) = LBUF%SIG(II(4)+I)
                        WA(JJ+5) = LBUF%SIG(II(5)+I)
                        IF (BUFLY%L_PLA > 0) THEN
                          WA(JJ+6) = LBUF%PLA(I)
                        ELSE
                          WA(JJ+6)=ZERO
                        ENDIF
                        JJ = JJ + 6
                      ENDDO ! DO IR = 1,NPTR
                    ENDDO ! DO IS = 1,NPTS
                  ENDDO ! DO IT=1,NPTT
                ENDDO ! DO ILAY=1,NLAY
              ENDIF ! IF (NLAY == 1)
c---
            ENDDO ! DO I=1,NEL
          ENDIF ! IF (MPT == 0)
        ELSE
C----error message------
        ENDIF ! IF (IHBE == 23)
      ENDIF ! IF (IW == 0)
C-----------
      RETURN
      END
