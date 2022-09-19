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
Chd|  SPMD_MSIN                     source/elements/initia/spmd_msin.F
Chd|-- called by -----------
Chd|        INITIA                        source/elements/initia/initia.F
Chd|-- calls ---------------
Chd|        MY_ORDERS                     ../common_source/tools/sort/my_orders.c
Chd|        STRR                          source/tools/univ/strr.F      
Chd|====================================================================
      SUBROUTINE SPMD_MSIN(
     1               IXS  ,IXQ  ,IXC  ,IXT  ,IXP  ,
     2               IXR  ,IXTG ,IXUR ,MSS  ,MSQ  ,
     3               MSC  ,MST  ,MSP  ,MSR  ,MSTG ,
     4               MSUR ,INC  ,INP  ,INR  ,INTG ,
     5               INUR ,INDEX,ITRI ,MS   ,IN   ,
     6               PTG  ,EVUR ,GEO  ,IXS10,IXS20,
     7               IXS16,MSSX ,MSNF ,MSSF ,VNS  ,
     8               VNSX ,STC  ,STT  ,STP  ,STR  ,
     9               STTG ,STUR ,BNS  ,BNSX ,VOLNOD ,
     A               BVOLNOD ,ETNOD ,STIFINT,INS  ,
     B               MCPC    ,MCP   ,MCPS ,MCPSX  ,
     C               MCPTG,SH4TREE,SH3TREE,MS_LAYERC,ZI_LAYERC,
     D               MS_LAYER , ZI_LAYER,MSZ2C,MSZ2,ZPLY,
     E               KXIG3D   ,IXIG3D  ,MSIG3D,NCTRLMAX,STRC,
     F               STRP,STRR,STRTG,STIFINTR,NSHNOD,VNIGE,BNIGE,
     G               MCPP )
C
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
#include      "scr03_c.inc"
#include      "rnur_c.inc"
#include      "scr12_c.inc"
#include      "remesh_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IXS(NIXS,*), IXQ(NIXQ,*), IXC(NIXC,*),
     .   IXT(NIXT,*),IXP(NIXP,*), IXR(NIXR,*), IXTG(6,*),
     .   IXUR(6,*), INDEX(*), ITRI(*),
     .   IXS10(6,*),IXS20(12,*),IXS16(8,*),
     .   SH4TREE(KSH4TREE,*), SH3TREE(KSH3TREE,*),KXIG3D(NIXIG3D,*),
     .   IXIG3D(*),NSHNOD(*)
C     REAL
      my_real
     .   MSS(8,*), MSQ(*),MSC(*),MST(*),MSP(*),MSR(3,*),
     .   MSTG(*),MSUR(*),MSSX(12,*),INC(*),
     .   INP(*),INR(3,*),INTG(*),INUR(*),
     .   MS(*), IN(*),PTG(3,*), EVUR(3,*), GEO(NPROPG,*),
     .   MSNF(*), MSSF(8,*),
     .   VNS(8,*) ,VNSX(12,*) ,STC(*) ,STT(*) ,STP(*) ,STR(*) ,
     .   STTG(*) ,STUR(*) ,BNS(8,*) ,BNSX(12,*) ,
     .   VOLNOD(*)  ,BVOLNOD(*) ,ETNOD(*), STIFINT(*), INS(8,*),
     .   MCP(*),MCPC(*),MCPS(8,*),MCPSX(12,*),MCPTG(*),
     .   MS_LAYERC(NUMELC,*),ZI_LAYERC(NUMELC,*),
     .   MS_LAYER(NUMNOD,*),ZI_LAYER(NUMNOD,*),MSZ2C(*),MSZ2(*),
     .   ZPLY(*),MSIG3D(NUMELIG3D,NCTRLMAX),STRC(*),STRP(*),STRR(*),
     .   STRTG(*),STIFINTR(*), VNIGE(NCTRLMAX,*),BNIGE(NCTRLMAX,*),
     .   MCPP(*)
C
      INTEGER IDEB,NCTRLMAX
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, K, N, IGTYP, WORK(70000),IP
C
      DO I = 1, NUMELS
        ITRI(I) = IXS(11,I)
      ENDDO
C
      CALL MY_ORDERS(0,WORK,ITRI,INDEX,NUMELS8,1)

      IDEB=NUMELS8+1
      CALL MY_ORDERS(0,WORK,ITRI(IDEB),INDEX(IDEB),NUMELS10,1)

      DO J=1,NUMELS10
           index(IDEB+J-1) = index(IDEB+J-1)+numels8
      ENDDO

      IDEB = IDEB + NUMELS10
      CALL MY_ORDERS(0,WORK,ITRI(IDEB),INDEX(IDEB),NUMELS20,1)
       DO j = 1, NUMELS20
           index(IDEB+J-1) = index(IDEB+J-1)+numels8+numels10
       ENDDO

      IDEB = IDEB + NUMELS20
      CALL MY_ORDERS(0,WORK,ITRI(IDEB),INDEX(IDEB),NUMELS16,1)
       DO j = 1, NUMELS16
           index(IDEB+J-1) = index(IDEB+J-1)+numels8+numels10+numels20
       ENDDO
C
      IF(ITHERM_FE == 0 ) THEN
       DO J=1,NUMELS
        I = INDEX(J)
        DO K=1,8
          N = IXS(K+1,I)
          MS(N) = MS(N) + MSS(K,I)
        ENDDO
       ENDDO
      ELSE
       DO J=1,NUMELS
        I = INDEX(J)
        DO K=1,8
          N = IXS(K+1,I)
          MS(N) = MS(N) + MSS(K,I)
          MCP(N) = MCP(N) + MCPS(K,I)
        ENDDO
       ENDDO
      ENDIF
C
      IF(IALE==1.OR.IEULER==1 .OR. IALELAG==1) THEN
        DO J=1,NUMELS
          I = INDEX(J)
          DO K=1,8
            N = IXS(K+1,I)
            MSNF(N) = MSNF(N) + MSSF(K,I)
          ENDDO
        ENDDO
      ENDIF
C
      IF(ITHERM_FE== 0 ) THEN
        IF(NUMELS10>0) THEN
          DO J=1,NUMELS10
            I = INDEX(NUMELS8+J)
            DO K=1,6
              N = IXS10(K,I-NUMELS8)
              IF (N/=0) THEN
                MS(N) = MS(N) + MSSX(K,I)
              END IF
            ENDDO
          ENDDO
        ENDIF

        IF(NUMELS20>0)THEN
          DO J=1,NUMELS20
            I = INDEX(NUMELS8+NUMELS10+J)
            DO K=1,12
              N = IXS20(K,I-NUMELS8-NUMELS10)
              IF (N/=0) THEN
                MS(N) = MS(N) + MSSX(K,I)
              END IF
            ENDDO
          ENDDO
        ENDIF
C
        IF(NUMELS16>0)THEN
          DO J=1,NUMELS16
            I = INDEX(NUMELS8+NUMELS10+NUMELS20+J)
            DO K=1,8
              N = IXS16(K,I-NUMELS8-NUMELS10-NUMELS20)
              IF (N/=0) THEN
                MS(N) = MS(N) + MSSX(K,I)
              END IF
            ENDDO
          ENDDO
        ENDIF
       ELSE
C
C  + heat transfer
C       
        IF(NUMELS10>0) THEN
          DO J=1,NUMELS10
            I = INDEX(NUMELS8+J)
            DO K=1,6
              N = IXS10(K,I-NUMELS8)
              IF (N/=0) THEN
                MS(N) = MS(N) + MSSX(K,I)
                MCP(N) = MCP(N) + MCPSX(K,I)
              END IF
            ENDDO
          ENDDO
        ENDIF

        IF(NUMELS20>0)THEN
          DO J=1,NUMELS20
            I = INDEX(NUMELS8+NUMELS10+J)
            DO K=1,12
              N = IXS20(K,I-NUMELS8-NUMELS10)
              IF (N/=0) THEN
                MS(N) = MS(N) + MSSX(K,I)
                MCP(N) = MCP(N) + MCPSX(K,I)
              END IF
            ENDDO
          ENDDO
        ENDIF
C
        IF(NUMELS16>0)THEN
          DO J=1,NUMELS16
            I = INDEX(NUMELS8+NUMELS10+NUMELS20+J)
            DO K=1,8
              N = IXS16(K,I-NUMELS8-NUMELS10-NUMELS20)
              IF (N/=0) THEN
                MS(N) = MS(N) + MSSX(K,I)
                MCP(N) = MCP(N) + MCPSX(K,I)
              END IF
            ENDDO
          ENDDO
        ENDIF
       ENDIF
C

      IF(IRODDL /= 0)THEN
          DO J=1,NUMELS8+NUMELS10
            I = INDEX(J)
            DO K=1,8
              N = IXS(K+1,I)
              IN(N) = IN(N) + INS(K,I)
            ENDDO
          ENDDO
      ENDIF
C
      IF(I7STIFS/=0)THEN
        DO J=1,NUMELS
          I = INDEX(J)
          DO K=1,8
            N = IXS(K+1,I)
            VOLNOD(N)  = VOLNOD(N)  + VNS(K,I)
            BVOLNOD(N) = BVOLNOD(N) + BNS(K,I)
          ENDDO
        ENDDO
C
        IF(NUMELS10>0) THEN
          DO J=1,NUMELS10
            I = INDEX(NUMELS8+J)
            DO K=1,6
              N = IXS10(K,I-NUMELS8)
              IF (N/=0) THEN
                VOLNOD(N)  = VOLNOD(N)  + VNSX(K,I)
                BVOLNOD(N) = BVOLNOD(N) + BNSX(K,I)
              END IF
            ENDDO
          ENDDO
        ENDIF
C
        IF(NUMELS20>0)THEN
          DO J=1,NUMELS20
            I = INDEX(NUMELS8+NUMELS10+J)
            DO K=1,12
              N = IXS20(K,I-NUMELS8-NUMELS10)
              IF (N/=0) THEN
                VOLNOD(N)  = VOLNOD(N)  + VNSX(K,I)
                BVOLNOD(N) = BVOLNOD(N) + BNSX(K,I)
              END IF
            ENDDO
          ENDDO
        ENDIF
C
        IF(NUMELS16>0)THEN
          DO J=1,NUMELS16
            I = INDEX(NUMELS8+NUMELS10+NUMELS20+J)
            DO K=1,8
              N = IXS16(K,I-NUMELS8-NUMELS10-NUMELS20)
              IF (N/=0) THEN
                VOLNOD(N)  = VOLNOD(N)  + VNSX(K,I)
                BVOLNOD(N) = BVOLNOD(N) + BNSX(K,I)
              END IF
            ENDDO
          ENDDO
        ENDIF
C
        IF(NUMELIG3D>0) THEN
          DO I = 1, NUMELIG3D
            ITRI(I) = KXIG3D(5,I)
          ENDDO
          CALL MY_ORDERS(0,WORK,ITRI,INDEX,NUMELIG3D,1)
          DO J=1,NUMELIG3D
            I = INDEX(J)
            DO K=1,KXIG3D(3,I)
              N = IXIG3D(KXIG3D(4,I)+K-1)
              IF (N/=0) THEN
                VOLNOD(N)  = VOLNOD(N)  + VNIGE(K,I)
                BVOLNOD(N) = BVOLNOD(N) + BNIGE(K,I)
              END IF
            ENDDO
          ENDDO
        ENDIF
      ENDIF
C
      DO I = 1, NUMELQ
        ITRI(I) = IXQ(7,I)
      ENDDO
      CALL MY_ORDERS(0,WORK,ITRI,INDEX,NUMELQ,1)
       DO J=1,NUMELQ
        I = INDEX(J)
        DO K=1,4
          N = IXQ(K+1,I)
          MS(N) = MS(N) + MSQ(I)
        ENDDO
      ENDDO
C
      DO I = 1, NUMELC
        ITRI(I) = IXC(7,I)
      ENDDO
C      
      CALL MY_ORDERS(0,WORK,ITRI,INDEX,NUMELC,1)
C
      IF(ITHERM_FE == 0 ) THEN
       IF(NADMESH==0)THEN
        DO J=1,NUMELC
          I = INDEX(J)
          DO K=1,4
            N = IXC(K+1,I)
            MS(N) = MS(N) + MSC(I)
            IN(N) = IN(N) + INC(I)
          ENDDO
        ENDDO
       ELSE
         IF(ISTATCND==0)THEN
           DO J=1,NUMELC
             I = INDEX(J)
             IF(SH4TREE(3,I) >= 0)THEN
               DO K=1,4
                 N = IXC(K+1,I)
                 MS(N) = MS(N) + MSC(I)
                 IN(N) = IN(N) + INC(I)
               ENDDO
             END IF
           ENDDO
         ELSE
           DO J=1,NUMELC
             I = INDEX(J)
             IF(SH4TREE(3,I) == 0 .OR. SH4TREE(3,I) == -1)THEN
               DO K=1,4
                 N = IXC(K+1,I)
                 MS(N) = MS(N) + MSC(I)
                 IN(N) = IN(N) + INC(I)
               ENDDO
             END IF
           ENDDO
         END IF
       END IF
      ELSE     ! ITHERM_FE /= 0
       IF(NADMESH==0)THEN
        DO J=1,NUMELC
          I = INDEX(J)
          DO K=1,4
            N = IXC(K+1,I)
            MS(N) = MS(N) + MSC(I)
            IN(N) = IN(N) + INC(I)
            MCP(N) = MCP(N) + MCPC(I)
          ENDDO
        ENDDO
       ELSE
         IF(ISTATCND==0)THEN
           DO J=1,NUMELC
             I = INDEX(J)
             IF(SH4TREE(3,I) >= 0)THEN
               DO K=1,4
                 N = IXC(K+1,I)
                 MS(N) = MS(N) + MSC(I)
                 IN(N) = IN(N) + INC(I)
                 MCP(N) = MCP(N) + MCPC(I)
               ENDDO
             END IF
           ENDDO
         ELSE
           DO J=1,NUMELC
             I = INDEX(J)
             IF(SH4TREE(3,I) == -1)THEN
               DO K=1,4
                 N = IXC(K+1,I)
                 MS(N) = MS(N) + MSC(I)
                 IN(N) = IN(N) + INC(I)
               ENDDO
             ELSEIF(SH4TREE(3,I) == 0) THEN
               DO K=1,4
                 N = IXC(K+1,I)
                 MS(N) = MS(N) + MSC(I)
                 IN(N) = IN(N) + INC(I)
                 MCP(N) = MCP(N) + MCPC(I)
               ENDDO
             ELSEIF(SH4TREE(3,I) > 0) THEN
               DO K=1,4
                 N = IXC(K+1,I)
                 MCP(N) = MCP(N) + MCPC(I)
               ENDDO
             END IF
           ENDDO
         END IF
       END IF
      ENDIF
C
      IF(IPLYXFEM > 0) THEN
        DO IP=1,NPLYMAX
          DO J=1,NUMELC
            I = INDEX(J)
            DO K=1,4
              N = IXC(K+1,I)
              MS_LAYER(N,IP) = MS_LAYER(N,IP) + MS_LAYERC(I,IP)
              IF(ZI_LAYERC(I,IP) == ZERO) THEN
                 ZI_LAYER(N,IP) = ZPLY(IP)
              ELSE
               ZI_LAYER(N,IP) = ZI_LAYERC(I,IP)
              ENDIF
            ENDDO

          ENDDO
        ENDDO
C     sum mi*zi*zi   
        DO J=1,NUMELC
            I = INDEX(J)
            DO K=1,4
              N = IXC(K+1,I)
              MSZ2(N) = MSZ2(N) + MSZ2C(I)
            ENDDO
          ENDDO
      ENDIF
C      
      IF(I7STIFS/=0)THEN
C
       DO J=1,NUMELC
        I = INDEX(J)
        DO K=1,4
          N = IXC(K+1,I)
          ETNOD(N) = ETNOD(N) + STC(I)
          STIFINTR(N) = STIFINTR(N) + STRC(I)/NSHNOD(N)
        ENDDO
       ENDDO
C
      ENDIF
C
      DO I = 1, NUMELT
        ITRI(I) = IXT(5,I)
      ENDDO
      CALL MY_ORDERS(0,WORK,ITRI,INDEX,NUMELT,1)
       DO J=1,NUMELT
        I = INDEX(J)
        DO K=1,2
          N = IXT(K+1,I)
          MS(N) = MS(N) + MST(I)
        ENDDO
      ENDDO
C
      IF(I7STIFS/=0)THEN
       DO J=1,NUMELT
        I = INDEX(J)
        DO K=1,2
          N = IXT(K+1,I)
          STIFINT(N) = STIFINT(N)  + STT(I)
        ENDDO
       ENDDO
      ENDIF
C
      DO I = 1, NUMELP
        ITRI(I) = IXP(6,I)
      ENDDO
      CALL MY_ORDERS(0,WORK,ITRI,INDEX,NUMELP,1)
       IF(ITHERM_FE == 0) THEN
          DO J=1,NUMELP
           I = INDEX(J)
           N = IXP(2,I)
           MS(N) = MS(N) + MSP(I)
           IN(N) = IN(N) + INP(I)
           N = IXP(3,I)
           MS(N) = MS(N) + MSP(I)
           IN(N) = IN(N) + INP(I)
          ENDDO
       ELSE
          DO J=1,NUMELP
           I = INDEX(J)
           N = IXP(2,I)
           MS(N)  = MS(N)  + MSP(I)
           IN(N)  = IN(N)  + INP(I)
           MCP(N) = MCP(N) + MCPP(I)
           N = IXP(3,I)
           MS(N)  = MS(N)  + MSP(I)
           IN(N)  = IN(N)  + INP(I)
           MCP(N) = MCP(N) + MCPP(I)
          ENDDO
       ENDIF
C
      IF(I7STIFS/=0)THEN
       DO J=1,NUMELP
        I = INDEX(J)
        N = IXP(2,I)
        STIFINT(N) = STIFINT(N)  + STP(I)
        STIFINTR(N) = STIFINTR(N) + STRP(I)
        N = IXP(3,I)
        STIFINT(N) = STIFINT(N)  + STP(I)
        STIFINTR(N) = STIFINTR(N) + STRP(I)
       ENDDO
      ENDIF
C
      DO I = 1, NUMELR
        ITRI(I) = IXR(6,I)
      ENDDO
      CALL MY_ORDERS(0,WORK,ITRI,INDEX,NUMELR,1)
       DO J=1,NUMELR
        I = INDEX(J)
        DO K=1,2
          N = IXR(K+1,I)
          MS(N) = MS(N) + MSR(K,I)
          IN(N) = IN(N) + INR(K,I)
        ENDDO
        IGTYP = NINT(GEO(12,IXR(1,I)))
        IF(IGTYP==12) THEN
          N = IXR(4,I)
          MS(N) = MS(N) + MSR(3,I)
          IN(N) = IN(N) + INR(3,I)
        ENDIF
      ENDDO
C
      IF(I7STIFS/=0)THEN
       DO J=1,NUMELR
        I = INDEX(J)
        DO K=1,2
          N = IXR(K+1,I)
          STIFINT(N) = STIFINT(N)  + STR(I)
          STIFINTR(N) = STIFINTR(N) + STRR(I)
        ENDDO
        IGTYP = NINT(GEO(12,IXR(1,I)))
        IF(IGTYP==12) THEN
          N = IXR(4,I)
          STIFINT(N) = STIFINT(N)  + TWO*STR(I)
        ENDIF
       ENDDO
      ENDIF
C
      DO I = 1, NUMELTG
        ITRI(I) = IXTG(6,I)
      ENDDO
      CALL MY_ORDERS(0,WORK,ITRI,INDEX,NUMELTG,1)
      IF(ITHERM _FE== 0 ) THEN
       IF(NADMESH==0)THEN
        DO J=1,NUMELTG
          I = INDEX(J)
          DO K=1,3
            N = IXTG(K+1,I)
            MS(N) = MS(N) + MSTG(I)*PTG(K,I)
            IN(N) = IN(N) + INTG(I)*PTG(K,I)
          ENDDO
        ENDDO
       ELSE
         IF(ISTATCND==0)THEN
           DO J=1,NUMELTG
             I = INDEX(J)
             IF(SH3TREE(3,I) >= 0)THEN
               DO K=1,3
                 N = IXTG(K+1,I)
                 MS(N) = MS(N) + MSTG(I)*PTG(K,I)
                 IN(N) = IN(N) + INTG(I)*PTG(K,I)
               ENDDO
             END IF
           ENDDO
         ELSE
           DO J=1,NUMELTG
             I = INDEX(J)
             IF(SH3TREE(3,I) == 0 .OR. SH3TREE(3,I) == -1)THEN
               DO K=1,3
                 N = IXTG(K+1,I)
                 MS(N) = MS(N) + MSTG(I)*PTG(K,I)
                 IN(N) = IN(N) + INTG(I)*PTG(K,I)
               ENDDO
             END IF
           ENDDO
         END IF
       END IF
      ELSE     ! ITHERM_FE /= 0
       IF(NADMESH==0)THEN
        DO J=1,NUMELTG
          I = INDEX(J)
          DO K=1,3
            N = IXTG(K+1,I)
            MS(N) = MS(N) + MSTG(I)*PTG(K,I)
            MCP(N) = MCP(N) + MCPTG(I)*PTG(K,I)
          ENDDO
        ENDDO
       ELSE
         IF(ISTATCND==0)THEN
           DO J=1,NUMELTG
             I = INDEX(J)
             IF(SH3TREE(3,I) >= 0)THEN
               DO K=1,3
                 N = IXTG(K+1,I)
                 MS(N) = MS(N) + MSTG(I)*PTG(K,I)
                 MCP(N) = MCP(N) + MCPTG(I)*PTG(K,I)
               ENDDO
             END IF
           ENDDO
         ELSE
           DO J=1,NUMELTG
             I = INDEX(J)
             IF(SH3TREE(3,I) == -1)THEN
               DO K=1,3
                 N = IXTG(K+1,I)
                 MS(N) = MS(N) + MSTG(I)*PTG(K,I)
               ENDDO
             ELSEIF(SH3TREE(3,I) == 0)THEN
               DO K=1,3
                 N = IXTG(K+1,I)
                 MS(N) = MS(N) + MSTG(I)*PTG(K,I)
                 MCP(N) = MCP(N) + MCPTG(I)*PTG(K,I)
               ENDDO
             ELSEIF(SH3TREE(3,I) > 0)THEN
               DO K=1,3
                 N = IXTG(K+1,I)
                 MCP(N) = MCP(N) + MCPTG(I)*PTG(K,I)
               ENDDO
             END IF
           ENDDO
         END IF
       END IF
      ENDIF
C
      IF(I7STIFS/=0)THEN
       DO J=1,NUMELTG
        I = INDEX(J)
        DO K=1,3
          N = IXTG(K+1,I)
          ETNOD(N) = ETNOD(N) + STTG(I)
          STIFINTR(N) = STIFINTR(N) + STRTG(I)/NSHNOD(N)
        ENDDO
       ENDDO
      ENDIF
C
      DO I = 1, NUMELUR
        ITRI(I) = IXUR(6,I)
      ENDDO
      CALL MY_ORDERS(0,WORK,ITRI,INDEX,NUMELUR,1)
      DO J=1,NUMELUR
        I = INDEX(J)
        N = IXUR(2,I)
        MS(N) = MS(N) + MSUR(I)
        IN(N) = IN(N) + INUR(I)
        DO K=1,3
          N = IXUR(K+2,I)
          MS(N) = MS(N) + ABS(MSUR(I)*EVUR(K,I))
          IF (IRODDL/=0)IN(N) = IN(N) + ABS(INUR(I)*EVUR(K,I))
        ENDDO
      ENDDO
C
      DO I = 1, NUMELIG3D
        ITRI(I) = KXIG3D(5,I)
      ENDDO
      CALL MY_ORDERS(0,WORK,ITRI,INDEX,NUMELIG3D,1)
      DO J=1,NUMELIG3D
        I = INDEX(J)
        DO K=1,KXIG3D(3,I)
          N = IXIG3D(KXIG3D(4,I)+K-1)
          MS(N) = MS(N) + MSIG3D(I,K)
        ENDDO
      ENDDO
C      
      RETURN
      END
