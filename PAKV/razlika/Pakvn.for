C======================================================================
C
C======================================================================
      SUBROUTINE TGRMAT(AK,NUMMAT,NETIP,II,NDIM,NASLOV,GAMA)
      USE ELEMENTS
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
C ......................................................................
C .
CE.    P R O G R A M
CE.       TO PRINT MATERIALS AND PROPERTIES IN NEU FILE
C .
C ......................................................................
C
      CHARACTER*80 NASLOV
      DIMENSION AK(3,5,*)
C
      IND=-1
      ITYP=100
	VERSION=10.0
      WRITE(II,5100) IND
      WRITE(II,5100) ITYP
      WRITE(II,2002) NASLOV
      WRITE(II,5200) VERSION
      WRITE(II,5100) IND
C
      NULA=0
      ZERO=0.
C
      EX=0.
      EY=0.
      EZ=0.
      GX=0.
      GY=0.
      GZ=0.
      VX=0.
      VY=0.
      VZ=0.
C
      IND=-1
      ITYP=601
      ITYPE1=-601
      IJEDAN=1
      IDESET=10
      IDPET=25
      IDVESTA=200
      IPEDESET=50
      ISEDAM=70
      ISHAPEBEAM=5
      ISPET=75
      APRESEK=1.0
      WRITE(II,5100) IND
      WRITE(II,5100) ITYP
c      if (nummat.gt.120) stop 'nummat.gt.120 - color - TRGRMAT'
C stampanje materijala
      DO I=1,NUMMAT
        if (nummat.gt.120) then
          ICOL=I-120
        else
          ICOL=I
         endif
c         ICOL=124-(I-1)*2
c         ICOL=120-(I-1)*3
         WRITE(II,5600) I,ITYPE1,ICOL,NULA,NULA,I,NULA
         WRITE(II,2000) I
         WRITE(II,5600) IDESET
         WRITE(II,5600) NULA,NULA,NULA,NULA,NULA,NULA,NULA,NULA,
     +                  NULA,NULA
         WRITE(II,5600) IDPET
         WRITE(II,5600) NULA,NULA,NULA,NULA,NULA,NULA,NULA,NULA,
     +                  NULA,NULA
         WRITE(II,5600) NULA,NULA,NULA,NULA,NULA,NULA,NULA,NULA,
     +                  NULA,NULA
         WRITE(II,5600) NULA,NULA,NULA,NULA,NULA
         WRITE(II,5600) IDVESTA
         WRITE(II,5500) EX,EY,EZ,GX,GY,GZ,VX,VY,VZ,ZERO
         WRITE(II,5500) ZERO,ZERO,ZERO,ZERO,ZERO,ZERO,ZERO,ZERO,
     +                  ZERO,ZERO
         WRITE(II,5500) ZERO,ZERO,ZERO,ZERO,ZERO,ZERO,ZERO,ZERO,
     +                  ZERO,ZERO
         WRITE(II,5500) ZERO,ZERO,ZERO,ZERO,ZERO,ZERO,ZERO,ZERO,
     +                  ZERO,ZERO
         WRITE(II,5500) ZERO,ZERO,AK(3,1,I),AK(3,2,I),AK(3,3,I),ZERO,
     +                  ZERO,ZERO,AK(3,4,I),GAMA
         WRITE(II,5500) ZERO,ZERO,ZERO,ZERO,ZERO,ZERO,ZERO,ZERO,
     +                  ZERO,ZERO
         WRITE(II,5500) ZERO,ZERO,ZERO,ZERO,ZERO,ZERO,ZERO,ZERO,
     +                  ZERO,ZERO
         WRITE(II,5500) ZERO,ZERO,ZERO,ZERO,ZERO,ZERO,ZERO,ZERO,
     +                  ZERO,ZERO
         WRITE(II,5500) ZERO,ZERO,ZERO,ZERO,ZERO,ZERO,ZERO,ZERO,
     +                  ZERO,ZERO
         WRITE(II,5500) ZERO,ZERO,ZERO,ZERO,ZERO,ZERO,ZERO,ZERO,
     +                  ZERO,ZERO
         WRITE(II,5500) ZERO,ZERO,ZERO,ZERO,ZERO,ZERO,ZERO,ZERO,
     +                  ZERO,ZERO
         WRITE(II,5500) ZERO,ZERO,ZERO,ZERO,ZERO,ZERO,ZERO,ZERO,
     +                  ZERO,ZERO
         WRITE(II,5500) ZERO,ZERO,ZERO,ZERO,ZERO,ZERO,ZERO,ZERO,
     +                  ZERO,ZERO
         WRITE(II,5500) ZERO,ZERO,ZERO,ZERO,ZERO,ZERO,ZERO,ZERO,
     +                  ZERO,ZERO
         WRITE(II,5500) ZERO,ZERO,ZERO,ZERO,ZERO,ZERO,ZERO,ZERO,
     +                  ZERO,ZERO
         WRITE(II,5500) ZERO,ZERO,ZERO,ZERO,ZERO,ZERO,ZERO,ZERO,
     +                  ZERO,ZERO
         WRITE(II,5500) ZERO,ZERO,ZERO,ZERO,ZERO,ZERO,ZERO,ZERO,
     +                  ZERO,ZERO
         WRITE(II,5500) ZERO,ZERO,ZERO,ZERO,ZERO,ZERO,ZERO,ZERO,
     +                  ZERO,ZERO
         WRITE(II,5500) ZERO,ZERO,ZERO,ZERO,ZERO,ZERO,ZERO,ZERO,
     +                  ZERO,ZERO
         WRITE(II,5500) ZERO,ZERO,ZERO,ZERO,ZERO,ZERO,ZERO,ZERO,
     +                  ZERO,ZERO
         WRITE(II,5600) IPEDESET
         WRITE(II,5600) NULA,NULA,NULA,NULA,NULA,NULA,NULA,NULA,
     +                  NULA,NULA
         WRITE(II,5600) NULA,NULA,NULA,NULA,NULA,NULA,NULA,NULA,
     +                  NULA,NULA
         WRITE(II,5600) NULA,NULA,NULA,NULA,NULA,NULA,NULA,NULA,
     +                  NULA,NULA
         WRITE(II,5600) NULA,NULA,NULA,NULA,NULA,NULA,NULA,NULA,
     +                  NULA,NULA
         WRITE(II,5600) NULA,NULA,NULA,NULA,NULA,NULA,NULA,NULA,
     +                  NULA,NULA
         WRITE(II,5600) ISEDAM
         WRITE(II,5600) NULA,NULA,NULA,NULA,NULA,NULA,NULA,NULA,
     +                  NULA,NULA
         WRITE(II,5600) NULA,NULA,NULA,NULA,NULA,NULA,NULA,NULA,
     +                  NULA,NULA
         WRITE(II,5600) NULA,NULA,NULA,NULA,NULA,NULA,NULA,NULA,
     +                  NULA,NULA
         WRITE(II,5600) NULA,NULA,NULA,NULA,NULA,NULA,NULA,NULA,
     +                  NULA,NULA
         WRITE(II,5600) NULA,NULA,NULA,NULA,NULA,NULA,NULA,NULA,
     +                  NULA,NULA
         WRITE(II,5600) NULA,NULA,NULA,NULA,NULA,NULA,NULA,NULA,
     +                  NULA,NULA
         WRITE(II,5600) NULA,NULA,NULA,NULA,NULA,NULA,NULA,NULA,
     +                  NULA,NULA
      ENDDO
      WRITE(II,5100) IND
C
        IND=-1
        ITYP=402
        WRITE(II,5100) IND
        WRITE(II,5100) ITYP
      DO J=1,numeltip
        SELECT CASE (eltypes(J))
        CASE (12)
          IPRO=5
        CASE (13)
          IPRO=37
        CASE (23)
          IPRO=19
        CASE (24)
          IPRO=19
        CASE (26)
          IPRO=20
        CASE (28)
          IPRO=20
        CASE (34)
          IPRO=25
        CASE (310)
          IPRO=26
        CASE (38)
          IPRO=25
        CASE (320)
          IPRO=26
        CASE DEFAULT
          WRITE(*,*) 'ELEMENT TYPE UNKNOWN NETIP=',eltypes(J)
        END SELECT
C stampanje propertija - stampa za svaki properti materijal, tj. bro properija=NUMMAT*numeltip
        DO I=1,NUMMAT
         ICOL=I
c         ICOL=124-(I-1)*2
c         ICOL=120-(I-1)*3
C broj propertija = ltypes(J)+I
         WRITE(II,5600) eltypes(J)*100+I,ICOL,I,IPRO,I,NULA
         WRITE(II,2001) I
C od 3 linije  se razlikuje properti u zavisnosti od tipa konacnog elementa
              SELECT CASE (IPRO)
C      grede bez i sa medjucvorovima
                 CASE (5, 37)
			    WRITE(II,5600) NULA,ISHAPEBEAM,IJEDAN,NULA
			    WRITE(II,5600) IDESET
			    WRITE(II,5600) NULA,NULA,NULA,NULA,NULA,NULA,NULA,NULA
			    WRITE(II,5600) NULA,NULA
			    WRITE(II,5600) ISPET
			    WRITE(II,5500) APRESEK,ZERO,ZERO,ZERO,ZERO
			    WRITE(II,5500) ZERO,ZERO,ZERO,ZERO,ZERO
			    WRITE(II,5500) ZERO,ZERO,ZERO,ZERO,ZERO
			    WRITE(II,5500) ZERO,ZERO,ZERO,ZERO,ZERO
			    WRITE(II,5500) ZERO,ZERO,ZERO,ZERO,ZERO
			    WRITE(II,5500) ZERO,ZERO,ZERO,ZERO,ZERO
			    WRITE(II,5500) ZERO,ZERO,ZERO,ZERO,ZERO
			    WRITE(II,5500) ZERO,ZERO,ZERO,ZERO,ZERO
			    WRITE(II,5500) APRESEK,ZERO,ZERO,ZERO,ZERO
			    WRITE(II,5500) ZERO,ZERO,ZERO,ZERO,ZERO
			    WRITE(II,5500) ZERO,ZERO,ZERO,ZERO,ZERO
			    WRITE(II,5500) ZERO,ZERO,ZERO,ZERO,ZERO
			    WRITE(II,5500) ZERO,ZERO,ZERO,ZERO,ZERO
			    WRITE(II,5500) ZERO,ZERO,ZERO,ZERO,ZERO
			    WRITE(II,5500) ZERO,ZERO,ZERO,ZERO,ZERO
			    WRITE(II,5600) NULA
			    WRITE(II,5600) NULA
C 
C 2D elementi
			CASE (19, 20)
			    WRITE(II,5600) NULA,NULA,NULA,NULA
			    WRITE(II,5600) IDESET
			    WRITE(II,5600) NULA,NULA,NULA,NULA,NULA,NULA,NULA,NULA
			    WRITE(II,5600) NULA,NULA
			    WRITE(II,5600) ISPET
			    WRITE(II,5500) APRESEK,ZERO,ZERO,ZERO,ZERO
			    WRITE(II,5500) ZERO,ZERO,ZERO,ZERO,ZERO
			    WRITE(II,5500) ZERO,ZERO,ZERO,ZERO,ZERO
			    WRITE(II,5500) ZERO,ZERO,ZERO,ZERO,ZERO
			    WRITE(II,5500) ZERO,ZERO,ZERO,ZERO,ZERO
			    WRITE(II,5500) ZERO,ZERO,ZERO,ZERO,ZERO
			    WRITE(II,5500) ZERO,ZERO,ZERO,ZERO,ZERO
			    WRITE(II,5500) ZERO,ZERO,ZERO,ZERO,ZERO
			    WRITE(II,5500) ZERO,ZERO,ZERO,ZERO,ZERO
			    WRITE(II,5500) ZERO,ZERO,ZERO,ZERO,ZERO
			    WRITE(II,5500) ZERO,ZERO,ZERO,ZERO,ZERO
			    WRITE(II,5500) ZERO,ZERO,ZERO,ZERO,ZERO
			    WRITE(II,5500) ZERO,ZERO,ZERO,ZERO,ZERO
			    WRITE(II,5500) ZERO,ZERO,ZERO,ZERO,ZERO
			    WRITE(II,5500) ZERO,ZERO,ZERO,ZERO,ZERO
			    WRITE(II,5600) NULA
			    WRITE(II,5600) NULA
C
C 3D elementi			
			CASE (25, 26)
			    WRITE(II,5600) NULA,NULA,NULA,NULA
			    WRITE(II,5600) IDESET
			    WRITE(II,5600) NULA,NULA,NULA,NULA,NULA,NULA,NULA,NULA
			    WRITE(II,5600) NULA,NULA
			    WRITE(II,5600) ISPET
			    WRITE(II,5500) ZERO,ZERO,ZERO,ZERO,ZERO
			    WRITE(II,5500) ZERO,ZERO,ZERO,ZERO,ZERO
			    WRITE(II,5500) ZERO,ZERO,ZERO,ZERO,ZERO
			    WRITE(II,5500) ZERO,ZERO,ZERO,ZERO,ZERO
			    WRITE(II,5500) ZERO,ZERO,ZERO,ZERO,ZERO
			    WRITE(II,5500) ZERO,ZERO,ZERO,ZERO,ZERO
			    WRITE(II,5500) ZERO,ZERO,ZERO,ZERO,ZERO
			    WRITE(II,5500) ZERO,ZERO,ZERO,ZERO,ZERO
			    WRITE(II,5500) ZERO,ZERO,ZERO,ZERO,ZERO
			    WRITE(II,5500) ZERO,ZERO,ZERO,ZERO,ZERO
			    WRITE(II,5500) ZERO,ZERO,ZERO,ZERO,ZERO
			    WRITE(II,5500) ZERO,ZERO,ZERO,ZERO,ZERO
			    WRITE(II,5500) ZERO,ZERO,ZERO,ZERO,ZERO
			    WRITE(II,5500) ZERO,ZERO,ZERO,ZERO,ZERO
			    WRITE(II,5500) ZERO,ZERO,ZERO,ZERO,ZERO
			    WRITE(II,5600) NULA
			    WRITE(II,5600) NULA
			CASE DEFAULT
			END SELECT
        ENDDO
C        WRITE(II,5100) IND
      enddo
        WRITE(II,5100) IND
      RETURN
C
 5100 FORMAT(I5)
 5500 FORMAT(10(1PE13.5))
 5600 FORMAT(10I5)
 5700 FORMAT(I4,1(1PE13.5),3I4)
 2000 FORMAT(' MAT',I4)
 2001 FORMAT(' PRO',I4)
 2002 FORMAT(A80)
 5200 FORMAT(F4.1,',')
      END
C======================================================================
C
C======================================================================
      SUBROUTINE TGRAUK(CORD,ID,NP,II)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
C ......................................................................
C .
CE.    P R O G R A M
CE.       TO PRINT COORDINATES IN UNIVERSAL FILE
CS.    P R O G R A M
CS.       ZA STAMPANJE KOORDINATA CVOROVA U UNIVERZALNI FILE
C .
C ......................................................................
C
      DIMENSION CORD(3,*)
      DIMENSION ID(1,*)
      DIMENSION IDD(6)
C
      IDD(2)=0
      IDD(3)=0
      IDD(4)=0
      IDD(5)=0
      IDD(6)=0
C
      NULA=0
C
      IND=-1
      ITYP=403
      ICOL=46
      WRITE(II,5100) IND
      WRITE(II,5100) ITYP
      DO 10 I=1,NP
           NI=I
         DO 20 IJ=1,1
            IF(ID(IJ,I).GT.0) THEN
               IDD(IJ)=0
            ELSE
               IDD(IJ)=1
            ENDIF
   20    CONTINUE
         WRITE(II,5000) NI,NULA,NULA,NULA,ICOL,(IDD(J),J=1,6),
     +                  (CORD(J,I),J=1,3),NULA
   10 CONTINUE
      WRITE(II,5100) IND
      RETURN
C
 5000 FORMAT(I10,10I3,3(1PE13.5),I2)
 5100 FORMAT(I5)
      END
C=====================================================================
      SUBROUTINE TGRAU2(NEL,NCVE,NE,IGRAF)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
C .......................................................................
C .
CE.   P R O G R A M
CE.       TO PRINTOUT 2/D ELEMENTS DATA IN NEUTRAL GRAPHICS FILE
CS.   P R O G R A M
CS.       ZA STAMPANJE 2/D ELEMENATA U NEUTRALNI FILE
C .
C .......................................................................
C 
      COMMON /MATER/ NUMMAT
      COMMON /DODAT/ NDIMM
      DIMENSION NEL(NDIMM,*)
      DIMENSION FIZ(14)         
C      COMMON /CDEBUG/ IDEBUG
C
C      IF(IDEBUG.GT.0) PRINT *, ' TGRAU2'
C
C     FIZICKE OSOBINE
C
      NULA=0
      ZERO=0.
      ONE=1.
      JEDAN=1
      INDPR=25
      INDPD=2
      I11=11
      I2=2
      I4=4
      I8=8
      I14=14
      I48=48
C
C     E L E M E N T I
C
      NNCVE=NCVE
      IF(NCVE.LT.8) NCVE=4
      IF(NCVE.EQ.9) NCVE=8
C     GRAFICKI OPIS ELEMENTA:
C     MEMBRANE - SA 4 CVOROVA = 13, SA 8 CVOROVA = 14
C     PLANE STRAIN - SA 4 CVOROVA = 19, SA 8 CVOROVA = 20
C     AXISYMMETRIC - SA 4 CVOROVA = 23, SA 8 CVOROVA = 24
C     PLATE - SA 4 CVOROVA = 17, SA 8 CVOROVA = 18
C     LAMINATE - SA 4 CVOROVA = 21, SA 8 CVOROVA = 22
      IFGD=19
      IF(NCVE.EQ.8) IFGD=20
C     VRSTA 2D ELEMENTA: SA 4 CVOROVA = 4, SA 8 CVOROVA = 5
      IFDI=4
      IF(NCVE.EQ.8) IFDI=5
C     TABELA FIZICKIH OSOBINA
C      IPTN=ISUMGR
C     BROJ CVOROVA NA ELEMENTU
      NNODS=NCVE
      IND=-1
      ITYP=404
      WRITE(IGRAF,1100) IND
      WRITE(IGRAF,1100) ITYP
      DO 10 I=1,NE
C        TABELA MATERIJALA
         MPTN=NEL(NNCVE+1,I)
         NEMA=NEL(NNCVE+2,I)
         IF(NEMA.GT.0) MPTN=NUMMAT+1
C        BOJA  
         ICOL=MPTN
c         ICOL=124-(MPTN-1)*2
c         ICOL=120-(MPTN-1)*3
         WRITE(IGRAF,1000) I,ICOL,MPTN,IFGD,IFDI,MPTN,NULA,NULA
         IF(NCVE.EQ.4) THEN
            WRITE(IGRAF,1001) (NEL(J,I),J=1,4),NULA,NULA,NULA,NULA,
     +                        NULA,NULA
            WRITE(IGRAF,1011) NULA,NULA,NULA,NULA,NULA,NULA,NULA,NULA,
     +                        NULA,NULA
         ELSE
            WRITE(IGRAF,1001) (NEL(J,I),J=1,8),
     +                        NULA,NULA
            WRITE(IGRAF,1011) NULA,NULA,NULA,NULA,NULA,NULA,NULA,NULA,
     +                        NULA,NULA
         ENDIF
         WRITE(IGRAF,1002) ZERO,ZERO,ZERO
         WRITE(IGRAF,1002) ZERO,ZERO,ZERO
         WRITE(IGRAF,1002) ZERO,ZERO,ZERO
         WRITE(IGRAF,1003) NULA,NULA,NULA,NULA,NULA,NULA,NULA,NULA,
     +                     NULA,NULA,NULA,NULA,NULA,NULA,NULA,NULA
   10 CONTINUE
      WRITE(IGRAF,1100) IND
C      ISUMEL=ISUMEL+NE
      NCVE=NNCVE
      RETURN
C
 1000 FORMAT(I10,7I4)
 1001 FORMAT(10I10)
 1011 FORMAT(10I2)
 1002 FORMAT(3F3.0)
 1003 FORMAT(16I2)
 1100 FORMAT(I5)
 1200 FORMAT(6(1PE13.5))
      END
C=====================================================================
      SUBROUTINE TGRAU3(NCVE,NE,IGRAF)
      USE ELEMENTS
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
C .......................................................................
C .
CE.   P R O G R A M
CE.       TO PRINTOUT (3/D) ELEMENTS DATA IN NEUTRAL GRAPHICS FILE
CS.   P R O G R A M
CS.       ZA STAMPANJE (3/D) ELEMENATA U NEUTRALNI FILE
C .
C .......................................................................
C 
      COMMON /MATER/ NUMMAT
      COMMON /DODAT/ NDIMM
!       DIMENSION NEL(NDIMM,*)
      DIMENSION FIZ(14)         
C      COMMON /CDEBUG/ IDEBUG
C
C      IF(IDEBUG.GT.0) PRINT *, ' TGRAU3'
C
C     FIZICKE OSOBINE
C
      NULA=0
      ZERO=0.
      ONE=1.
      JEDAN=1
      INDPR=25
      INDPD=2
      I11=11
      I2=2
      I4=4
      I8=8
      I14=14
      I48=48
C
C     E L E M E N T I
C
      NNCVE=NCVE
      IND=-1
      ITYP=404
      WRITE(IGRAF,1100) IND
      WRITE(IGRAF,1100) ITYP
      DO 10 I=1,NE
!         NTYPE=elemtip(NE)/10
!         NDIM=elemtip(NE)-10*NTYPE
C     GRAFICKI OPIS ELEMENTA: SA 8 CVOROVA = 25, SA 20 CVOROVA = 26
C     VRSTA 3/D ELEMENTA: SA 8 CVOROVA = 8, SA 20 CVOROVA = 12
      SELECT CASE (elemtip(I))
      CASE (12)
        IFGD=5
        IFDI=0
      CASE (13)
        IFGD=37
        IFDI=1
      CASE (23)
        IFGD=19
        IFDI=2
      CASE (24)
        IFGD=19
        IFDI=4
      CASE (26)
        IFGD=20
        IFDI=3
      CASE (28)
        IFGD=20
        IFDI=5
      CASE (34)
        IFGD=25
        IFDI=6
      CASE (310)
        IFGD=26
        IFDI=10
      CASE (38)
        IFGD=25
        IFDI=8
      CASE (320)
        IFGD=26
        IFDI=12
      CASE DEFAULT
        WRITE(*,*) 'ELEMENT TYPE UNKNOWN NETIP=',elemtip(I)
      END SELECT
C     TABELA FIZICKIH OSOBINA
C      IPTN=ISUMGR
C     BROJ CVOROVA NA ELEMENTU
      
C        TABELA MATERIJALA
         MPTN=NEL(NNCVE+1,I)
         NEMA=NEL(NNCVE+2,I)
         IF(NEMA.GT.0) MPTN=NUMMAT+1
C        BOJA  
         ICOL=MPTN
         IPROP=elemtip(I)*100+MPTN
c         ICOL=124-(MPTN-1)*2
c         ICOL=120-(MPTN-1)*3
C         WRITE(IGRAF,1000) I,ICOL,elemtip(I)+MPTN,IFGD,IFDI,MPTN,NULA,NULA
         WRITE(IGRAF,1111) I,ICOL,IPROP,IFGD,IFDI,MPTN,NULA,NULA
         SELECT CASE (elemtip(I))
         CASE (12)
            WRITE(IGRAF,1001) (NEL(J,I),J=1,2),NULA,NULA,NULA,NULA,NULA,
     +                     NULA,NULA,NULA
            WRITE(IGRAF,1011) NULA,NULA,NULA,NULA,NULA,NULA,NULA,NULA,
     +                     NULA,NULA
            WRITE(IGRAF,1002) ONE,ZERO,ZERO
         CASE (13)
            WRITE(IGRAF,1001) (NEL(J,I),J=1,3),NULA,NULA,NULA,NULA,NULA,
     +                     NULA,NULA
            WRITE(IGRAF,1011) NULA,NULA,NULA,NULA,NULA,NULA,NULA,NULA,
     +                     NULA,NULA
            WRITE(IGRAF,1002) ONE,ZERO,ZERO
         CASE (23)
            WRITE(IGRAF,1001) (NEL(J,I),J=1,3),NULA,NULA,NULA,NULA,
     +                        NULA,NULA,NULA
            WRITE(IGRAF,1011) NULA,NULA,NULA,NULA,NULA,NULA,NULA,NULA,
     +                        NULA,NULA
            WRITE(IGRAF,1002) ZERO,ZERO,ZERO
         CASE (24)
            WRITE(IGRAF,1001) (NEL(J,I),J=1,4),NULA,NULA,NULA,NULA,
     +                        NULA,NULA
            WRITE(IGRAF,1011) NULA,NULA,NULA,NULA,NULA,NULA,NULA,NULA,
     +                        NULA,NULA
            WRITE(IGRAF,1002) ZERO,ZERO,ZERO
         CASE (26)
            WRITE(IGRAF,1001) (NEL(J,I),J=1,3),NULA,(NEL(J,I),J=4,6),NULA,NULA
            WRITE(IGRAF,1011) NULA,NULA,NULA,NULA,NULA,NULA,NULA,NULA,
     +                        NULA,NULA
            WRITE(IGRAF,1002) ZERO,ZERO,ZERO
         CASE (28)
            WRITE(IGRAF,1001) (NEL(J,I),J=1,8),
     +                        NULA,NULA
            WRITE(IGRAF,1011) NULA,NULA,NULA,NULA,NULA,NULA,NULA,NULA,
     +                        NULA,NULA
            WRITE(IGRAF,1002) ZERO,ZERO,ZERO
         CASE (34)
            WRITE(IGRAF,1001) NEL(1,I),NEL(2,I),NEL(3,I),NULA,NEL(4,I),
     +                        NULA,NULA,NULA,NULA,NULA
            WRITE(IGRAF,1011) NULA,NULA,NULA,NULA,NULA,NULA,NULA,NULA,
     +                        NULA,NULA
            WRITE(IGRAF,1002) ZERO,ZERO,ZERO
         CASE (310)
            WRITE(IGRAF,1001) NEL(1,I),NEL(2,I),NEL(3,I),NULA,NEL(4,I),
     +                        NULA,NULA,NULA,NEL(5,I),NEL(6,I)
            WRITE(IGRAF,1001) NEL(7,I),NULA,NEL(8,I),NEL(9,I),NEL(10,I)
     +                        ,NULA,NULA,NULA,NULA,NULA
            WRITE(IGRAF,1002) ZERO,ZERO,ZERO
          CASE (38)
            WRITE(IGRAF,1001) (NEL(J,I),J=5,8),(NEL(J,I),J=1,4),
     +                        NULA,NULA
            WRITE(IGRAF,1011) NULA,NULA,NULA,NULA,NULA,NULA,NULA,NULA,
     +                        NULA,NULA
            WRITE(IGRAF,1002) ZERO,ZERO,ZERO
         CASE (320)
            WRITE(IGRAF,1001) (NEL(J,I),J=5,8),(NEL(J,I),J=1,4),
     +                        NEL(17,I),NEL(18,I)
            WRITE(IGRAF,1001) NEL(19,I),NEL(20,I),(NEL(J,I),J=9,12),
     +                        (NEL(J,I),J=13,16)
            WRITE(IGRAF,1002) ZERO,ZERO,ZERO
          CASE DEFAULT
          WRITE(*,*) 'ELEMENT TYPE UNKNOWN NETIP=',elemtip(NE)
         END SELECT

        WRITE(IGRAF,1002) ZERO,ZERO,ZERO
         WRITE(IGRAF,1002) ZERO,ZERO,ZERO
         WRITE(IGRAF,1003) NULA,NULA,NULA,NULA,NULA,NULA,NULA,NULA,
     +                     NULA,NULA,NULA,NULA,NULA,NULA,NULA,NULA
   10 CONTINUE
      WRITE(IGRAF,1100) IND
C      ISUMEL=ISUMEL+NE
      NCVE=NNCVE
      RETURN
C
 1000 FORMAT(I10,7I4)
 1111 FORMAT(I10,7I6)
 1001 FORMAT(10I10)
 1011 FORMAT(10I2)
 1002 FORMAT(3F3.0)
 1003 FORMAT(16I2)
 1100 FORMAT(I5)
 1200 FORMAT(6(1PE13.5))
      END
C=======================================================================
      SUBROUTINE STAU09(RTH,CORD,ID,NPP,II,IND,IKOL,KOR,NZAD,NUMZAD,
     +                  KONT,POT)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      COMMON /ELECTR/ INDJOT,INDFS
      COMMON /OSATEZ/ ZASIC,IOSA,ISIL,IFIL
      COMMON /VOPTIM/ NKONT,MAXLIN,LIN(10)
      DIMENSION RTH(IKOL,*),CORD(3,*),ID(1,*),FSP(6),IN(8),NZAD(3,*)
      DIMENSION KONT(9,MAXLIN,*),POT(*)
      DIMENSION KONT1(40000)
C
      FSP(3)=0.
C
      JEDAN=1
      NULA=0
      ZERO=0.
      ONE=1.
      MJ=-1
      M451=451
      M1=1
      M2=2
      M3=3
      M4=4
      M5=5
      M6=6
      M7=7
      M8=8
      M9=9
C      II=49
C
      IN(1)=IND
      IN(2)=IND+1
      IN(3)=IND+2
      IN(4)=IND+3
      IN(5)=IND+4
      IN(6)=IND+5
      IN(7)=IND+6
      IN(8)=IND+7
C
C PRIVREMENO
      IF(IN(1).EQ.10) THEN
      DO I=1,40000
         KONT1(I)=0
      ENDDO
      ENDIF
C      
      IP=0
C
   30 IP=IP+1
      WRITE(II,1000) KOR,IN(IP),JEDAN
      IF(IN(1).EQ.1.AND.IP.EQ.1) WRITE(II,3001) 
      IF(IN(1).EQ.9.AND.IP.EQ.1) WRITE(II,3002) 
      IF(IN(1).EQ.10.AND.IP.EQ.1) WRITE(II,3003) 
      IF(IN(1).EQ.11.AND.IP.EQ.1) WRITE(II,3004) 
      IF(IN(1).EQ.41.AND.IP.EQ.1) WRITE(II,3041) 
      IF(IN(1).EQ.41.AND.IP.EQ.2) WRITE(II,3042) 
      IF(IN(1).EQ.41.AND.IP.EQ.3) WRITE(II,3043) 
      IF(IN(1).EQ.41.AND.IP.EQ.4) WRITE(II,3044) 
      IF(IN(1).EQ.51.AND.IP.EQ.1) WRITE(II,3051) 
      IF(IN(1).EQ.51.AND.IP.EQ.2) WRITE(II,3052) 
      IF(IN(1).EQ.51.AND.IP.EQ.3) WRITE(II,3053) 
      IF(IN(1).EQ.51.AND.IP.EQ.4) WRITE(II,3054) 
      IF(IN(1).EQ.61.AND.IP.EQ.1) WRITE(II,3061) 
      IF(IN(1).EQ.61.AND.IP.EQ.2) WRITE(II,3062) 
      IF(IN(1).EQ.61.AND.IP.EQ.3) WRITE(II,3063) 
      IF(IN(1).EQ.61.AND.IP.EQ.4) WRITE(II,3064) 
      WRITE(II,1010) ZERO,ZERO,ZERO
C OVO SU NULE ZA TEMPERATURE
      IF(IP.EQ.1.AND.IN(1).LT.40)
     +WRITE(II,1000) NULA,NULA,NULA,NULA,NULA,NULA,NULA,NULA,NULA,NULA
      IF(IP.EQ.1.AND.IN(1).GT.40)
     +WRITE(II,1000)IN(2),IN(3),IN(4),NULA,NULA,NULA,NULA,NULA,NULA,NULA
      IF(IP.EQ.2)
     +  WRITE(II,1000)IN(2),NULA,NULA,NULA,NULA,NULA,NULA,NULA,NULA,NULA
      IF(IP.EQ.3)
     +  WRITE(II,1000)NULA,IN(3),NULA,NULA,NULA,NULA,NULA,NULA,NULA,NULA
      IF(IP.EQ.4)
     +  WRITE(II,1000)NULA,NULA,IN(4),NULA,NULA,NULA,NULA,NULA,NULA,NULA
      WRITE(II,1000) NULA,NULA,NULA,NULA,NULA,NULA,NULA,NULA,NULA,NULA
C
C 0=ANY,1=DISPL,2=ACCEL,3=FORCE,4=STRESS,5=STRAIN,6=TEMP,OTHER=USER
C     NODAL(7) ,ELEMENTAL(8)
C dodat sledeci red za verziju neu fajla 10.0 /Sneza 26.12.2016.
      WRITE(II,1000) NULA
      IF(IN(1).EQ.1) WRITE(II,1000) NULA,NULA,M6,M7
      IF(IN(1).EQ.9) WRITE(II,1000) NULA,NULA,M7,M7
      IF(IN(1).EQ.10) WRITE(II,1000) NULA,NULA,M8,M8
      IF(IN(1).EQ.11) WRITE(II,1000) NULA,NULA,M6,M7
      IF(IN(1).EQ.41.OR.IN(1).EQ.51.OR.IN(1).EQ.61)
     1WRITE(II,1000) NULA,NULA,M3,M7
C
C =1, CAN NOT LINEARY COMBINE THIS OUTPUT
C =1, COMP(0-2) ARE THE X,Y,Z COMPONENT
C =1, THIS VECTOR HAS CONTROIDAL OR NODAL OUTPUT
      IF(IN(1).GT.40) THEN
         IF(IP.EQ.1) THEN
            WRITE(II,1000) JEDAN,JEDAN,JEDAN
         ELSE
            WRITE(II,1000) NULA,JEDAN,JEDAN
         ENDIF
      ELSE
         WRITE(II,1000) JEDAN,NULA,JEDAN
      ENDIF
      DO 10 I=1,NPP
         DO 20 J=1,IKOL
            FSP(J) = 0.0D0
            IF(IKOL.EQ.1) THEN
               IF(IN(1).EQ.1) THEN
                  IF(ID(J,I).EQ.0) GO TO 20
                  K = ID(J,I)
                  IF(K.GT.0) THEN
                     IF(INDFS.GT.0) THEN
c                       POTENCIJAL ISPOD SLOBODNE POVRSINE
                        IF(POT(K).GE.CORD(IOSA,I)) then
                            FSP(J)=RTH(J,K)
                        else 
                            go to 10
                        endif
                     ELSE   
                        IF(POT(K).GE.CORD(IOSA,I)) then
                            FSP(J)=RTH(J,K)
                        else 
                            go to 10
                        endif
c  Sneza 10.9.2015  'stampa vrednost 0 iznad slobodne povrsine
C                       FSP(J)=RTH(J,K) 
                     ENDIF
                  ENDIF
               ELSEIF(IN(1).EQ.11) THEN
                  IF(ID(J,I).EQ.0) GO TO 20
                  K = ID(J,I)
                  IF(K.GT.0) THEN
C                    DUBINA VODE ILI PRITISAK VODENOG STUBA
                     FSP(J)=-(RTH(1,K)-CORD(IOSA,I))
                     IF(FSP(J).GT.0.) FSP(J)=0.     
                  ENDIF
               ELSEIF(IN(1).EQ.9) THEN
                 FSP(J)=1.0D0
                 DO L=1,NUMZAD 
                    IF(NZAD(1,L).EQ.I) GO TO 1201
                 ENDDO
                 FSP(J)=0.D0
 1201            CONTINUE
               ELSE
                 DO LL=1,NKONT
                    FSP(J)=LL
                 DO L=1,LIN(LL)
                 DO LI=2,9 
C                 DO LI=2,NNODE+1 
                    IF(KONT(LI,L,LL).EQ.I) THEN
                       IEL=KONT(1,L,LL)
                       IF(KONT1(IEL).EQ.0) THEN
                          KONT1(IEL)=1
                          GO TO 1202
                       ELSE
                          GO TO 10
                       ENDIF 
                    ENDIF
                 ENDDO
                 ENDDO
                 ENDDO
                 FSP(J)=0.D0
 1202            CONTINUE
               ENDIF
            ELSE
               IF(INDFS.GT.0) THEN
                  K = ID(1,I)
                  IF(K.GT.0) THEN
                     IF(POT(K).GT.CORD(IOSA,I)) FSP(J)=RTH(J,I)
c                  ELSE
c                     FSP(J)=RTH(J,I)
                  ENDIF
               ELSE   
                  FSP(J)=RTH(J,I)
               ENDIF
            ENDIF
   20    CONTINUE
         VAL=FSP(1)
         IF(IP.EQ.1.AND.IN(1).GT.40)
     +      VAL=DSQRT(FSP(1)*FSP(1)+FSP(2)*FSP(2)+FSP(3)*FSP(3))
         IF(IP.EQ.2) VAL=FSP(1)
         IF(IP.EQ.3) VAL=FSP(2)
         IF(IP.EQ.4) VAL=FSP(3)
czn         IF(DABS(VAL).LT.1.E-12) GO TO 10
         IF(IN(1).EQ.10) THEN
            WRITE(II,5000) IEL,VAL
         ELSE
            WRITE(II,5000) I,VAL
         ENDIF  
   10 CONTINUE
      WRITE(II,5000) MJ,ZERO
      IF(IP.LT.4.AND.IN(1).GT.40) GO TO 30
      RETURN
 1000 FORMAT(10I5)
 1001 FORMAT(I10,F12.4)
 3001 FORMAT('TOTAL POTENTIAL')
 3002 FORMAT('PRESCRIBED POTENTIAL')
 3003 FORMAT('PRESCRIBED SURFACES')
 3004 FORMAT('DEPTH OF WATER')
 3041 FORMAT('TOTAL VELOCITY')
 3042 FORMAT('VX VELOCITY')
 3043 FORMAT('VY VELOCITY')
 3044 FORMAT('VZ VELOCITY')
 3051 FORMAT('TOTAL GRADIENT')
 3052 FORMAT('GX GRADIENT')
 3053 FORMAT('GY GRADIENT')
 3054 FORMAT('GZ GRADIENT')
 3061 FORMAT('TOTAL FORCE')
 3062 FORMAT('FX FORCE')
 3063 FORMAT('FY FORCE')
 3064 FORMAT('FZ FORCE')
 1005 FORMAT('PAK CASE',I5)
 1006 FORMAT('CASE',I5,' TIME',1PD12.4)
 1010 FORMAT(3(1PD12.4))
 5000 FORMAT(I10,3(1PD12.4))
      END
C=======================================================================
C
C=======================================================================
      SUBROUTINE NEUTRA(VREME,NPER,NASLOV,IND,II)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
C ......................................................................
C .
CE.    P R O G R A M
CE.        WITH LOOP OVER TIME PERIODS AND STEPS FOR NEUTRAL FILE
CS.    P R O G R A M
CS.        SA PETLJOM PO VREMENSKIM PERIODIMA I KORACIMA ZA NEUTRAL 
C .
CE.    V A R I A B L E S
CE.        NPER  - TOTAL NUMBER OF PERIODS WITH CONSTANT TIME STEP,
CE.                SEE CARD /3/
C .
C ......................................................................
C
      CHARACTER*80 NASLOV
      DIMENSION VREME(NPER,950)
      DIMENSION IS(1)
C
      IF(IND.EQ.2) GO TO 999
C
      JEDAN=1
      NULA=0
      ZERO=0.
      ONE=1.
      MJ=-1
      M2=2
      M3=3
      M4=4
      M5=5
      M8=8
      M9=9
C
      WRITE(II,1000) MJ
      M450=450
      WRITE(II,1001) M450
C
      MM=19
C      IF(NGENL.EQ.0.AND.NDIN.EQ.0.AND.ISOPS.EQ.0) MM=1
C
CE    BASIC LOOP OVER TIME PERIODS
CS    OSNOVNA PETLJA PO VREMENSKIM PERIODIMA
C
      KKORAK=1
      VVREME=0.
      DO 100 NNPER=1,NPER
C
CE    BASIC LOOP OVER TIME STEPS
CS    OSNOVNA PETLJA PO VREMENSKIM KORACIMA
C
         KORAK=0
   35    KORAK=KORAK+1
         TIME=VREME(NNPER,KORAK)
         VVREME=VVREME+TIME
C
         KORBR=KKORAK
      WRITE(II,1000) KORBR
      NDT=2
      IF (NPER.EQ.1.AND.DABS(VREME(1,2)).LT.1.D-10) NDT=1
      IF(NDT.EQ.1) THEN
         WRITE(II,1005) KORBR
         WRITE(II,1000) M9,MM
         WRITE(II,1010) ZERO
      ELSE
         WRITE(II,1006) KORBR,VVREME
         WRITE(II,1000) M9,MM
         WRITE(II,1010) VVREME
      ENDIF
      WRITE(II,1000) JEDAN
      WRITE(II,2000) NASLOV
C
      KKORAK=KKORAK+1
      IF (DABS(VREME(NNPER,KORAK+1)).LT.1.D-10) THEN
        GOTO 100
      ELSE
        GOTO 35
      ENDIF
C
  100 CONTINUE
C
      WRITE(II,1000) MJ
C
      WRITE(II,1000) MJ
      M451=451
      WRITE(II,1001) M451
      RETURN
C
  999 WRITE(II,1000) MJ
      CLOSE(II,STATUS='KEEP')
      RETURN
C
 1000 FORMAT(10I5)
 1001 FORMAT(I10,F12.4)
 1005 FORMAT('PAK CASE',I5)
 1006 FORMAT('CASE',I5,' TIME',1PD12.4)
 1010 FORMAT(3(1PD12.4))
 2000 FORMAT(A80)
      END

C======================================================================
      SUBROUTINE STAS09(RTH,CORD,ID,NPP,II,IND,IKOL,KOR,NZAD,NUMZAD,
     +                  KONT)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      COMMON /VOPTIM/ NKONT,MAXLIN,LIN(10)
      DIMENSION RTH(IKOL,*),CORD(3,*),ID(1,*),FSP(6),IN(8),NZAD(3,*)
      DIMENSION KONT(9,MAXLIN,*)
      DIMENSION KONT1(40000)
C
      JEDAN=1
      NULA=0
      ZERO=0.
      ONE=1.
      MJ=-1
      M451=451
      M1=1
      M2=2
      M3=3
      M4=4
      M5=5
      M6=6
      M7=7
      M8=8
      M9=9
C      II=49
C
      IN(1)=IND
      IN(2)=IND+1
      IN(3)=IND+2
      IN(4)=IND+3
      IN(5)=IND+4
      IN(6)=IND+5
      IN(7)=IND+6
      IN(8)=IND+7
C
C PRIVREMENO
      IF(IN(1).EQ.10) THEN
      DO I=1,40000
         KONT1(I)=0
      ENDDO
      ENDIF
C      
      IP=0
C
   30 IP=IP+1
      WRITE(II,1000) KOR,IN(IP),JEDAN
      IF(IN(1).EQ.1.AND.IP.EQ.1) WRITE(II,3001) 
      IF(IN(1).EQ.9.AND.IP.EQ.1) WRITE(II,3002) 
      IF(IN(1).EQ.10.AND.IP.EQ.1) WRITE(II,3003) 
      IF(IN(1).EQ.41.AND.IP.EQ.1) WRITE(II,3041) 
      IF(IN(1).EQ.41.AND.IP.EQ.2) WRITE(II,3042) 
      IF(IN(1).EQ.41.AND.IP.EQ.3) WRITE(II,3043) 
      IF(IN(1).EQ.41.AND.IP.EQ.4) WRITE(II,3044) 
      IF(IN(1).EQ.51.AND.IP.EQ.1) WRITE(II,3051) 
      IF(IN(1).EQ.51.AND.IP.EQ.2) WRITE(II,3052) 
      IF(IN(1).EQ.51.AND.IP.EQ.3) WRITE(II,3053) 
      IF(IN(1).EQ.51.AND.IP.EQ.4) WRITE(II,3054) 
      WRITE(II,1010) ZERO,ZERO,ZERO
C OVO SU NULE ZA TEMPERATURE
      IF(IP.EQ.1.AND.IN(1).LT.40)
     +WRITE(II,1000) NULA,NULA,NULA,NULA,NULA,NULA,NULA,NULA,NULA,NULA
      IF(IP.EQ.1.AND.IN(1).GT.40)
     +WRITE(II,1000)IN(2),IN(3),IN(4),NULA,NULA,NULA,NULA,NULA,NULA,NULA
      IF(IP.EQ.2)
     +  WRITE(II,1000)IN(2),NULA,NULA,NULA,NULA,NULA,NULA,NULA,NULA,NULA
      IF(IP.EQ.3)
     +  WRITE(II,1000)NULA,IN(3),NULA,NULA,NULA,NULA,NULA,NULA,NULA,NULA
      IF(IP.EQ.4)
     +  WRITE(II,1000)NULA,NULA,IN(4),NULA,NULA,NULA,NULA,NULA,NULA,NULA
      WRITE(II,1000) NULA,NULA,NULA,NULA,NULA,NULA,NULA,NULA,NULA,NULA
C
C 0=ANY,1=DISPL,2=ACCEL,3=FORCE,4=STRESS,5=STRAIN,6=TEMP,OTHER=USER
C     NODAL(7) ,ELEMENTAL(8)
C dodat sledeci red za verziju neu fajla 10.0 /Sneza 26.12.2016.
      WRITE(II,1000) NULA
      IF(IN(1).EQ.1) WRITE(II,1000) NULA,NULA,M6,M7
      IF(IN(1).EQ.9) WRITE(II,1000) NULA,NULA,M7,M7
      IF(IN(1).EQ.10) WRITE(II,1000) NULA,NULA,M8,M8
      IF(IN(1).EQ.41.OR.IN(1).EQ.51) WRITE(II,1000) NULA,NULA,M3,M7
C
C =1, CAN NOT LINEARY COMBINE THIS OUTPUT
C =1, COMP(0-2) ARE THE X,Y,Z COMPONENT
C =1, THIS VECTOR HAS CONTROIDAL OR NODAL OUTPUT
      IF(IN(1).GT.40) THEN
         IF(IP.EQ.1) THEN
            WRITE(II,1000) JEDAN,JEDAN,JEDAN
         ELSE
            WRITE(II,1000) NULA,JEDAN,JEDAN
         ENDIF
      ELSE
         WRITE(II,1000) JEDAN,NULA,JEDAN
      ENDIF
C      DO 10 I=1,NPP
         DO 20 J=1,IKOL
            FSP(J) = 0.0D0
            IF(IKOL.EQ.1) THEN
               IF(IN(1).EQ.1) THEN
                  IF(ID(J,I).EQ.0) GO TO 20
                  K = ID(J,I)
                  IF(K.GT.0) THEN
C OVO JE SAMO ZA 3D SLOBODNA POVRSINA
C                     IF(RTH(1,K).GT.CORD(3,I)) FSP(J)=RTH(1,K)
                     FSP(J)=RTH(1,K)
                  ENDIF
               ELSEIF(IN(1).EQ.9) THEN
                 FSP(J)=1.0D0
                 DO L=1,NUMZAD 
                    IF(NZAD(1,L).EQ.I) GO TO 1201
                 ENDDO
                 FSP(J)=0.D0
 1201            CONTINUE
               ELSE
                 DO LL=1,NKONT
                    FSP(J)=LL
                 DO L=1,LIN(LL) 
C                 DO LI=2,5 
C                 DO LI=2,NNODE+1 
C                    IF(KONT(LI,L,LL).EQ.I) THEN
                  IEL=KONT(1,L,LL)
                  VAL=FSP(1)
                  WRITE(II,5000) IEL,VAL
C                       IF(KONT1(IEL).EQ.0) THEN
C                          KONT1(IEL)=1
C                          GO TO 1202
C                       ELSE
C                          GO TO 10
C                       ENDIF 
C                    ENDIF
C                 ENDDO
                 ENDDO
                 ENDDO
                 FSP(J)=0.D0
 1202            CONTINUE
               ENDIF
            ELSE
               FSP(J)=RTH(J,I)
            ENDIF
   20    CONTINUE
         VAL=FSP(1)
         IF(IP.EQ.1.AND.IN(1).GT.40)
     +      VAL=DSQRT(FSP(1)*FSP(1)+FSP(2)*FSP(2)+FSP(3)*FSP(3))
         IF(IP.EQ.2) VAL=FSP(1)
         IF(IP.EQ.3) VAL=FSP(2)
         IF(IP.EQ.4) VAL=FSP(3)
C         IF(DABS(VAL).LT.1.E-12) GO TO 10
C         IF(IN(1).EQ.10) THEN
C            WRITE(II,5000) IEL,VAL
C         ELSE
C            WRITE(II,5000) I,VAL
C         ENDIF  
C   10 CONTINUE
      WRITE(II,5000) MJ,ZERO
      IF(IP.LT.4.AND.IN(1).GT.40) GO TO 30
      RETURN
 1000 FORMAT(10I5)
 1001 FORMAT(I10,F12.4)
 3001 FORMAT('TOTAL POTENTIAL')
 3002 FORMAT('PRESCRIBED POTENTIAL')
 3003 FORMAT('PRESCRIBED SURFACES')
 3041 FORMAT('TOTAL VELOCITY')
 3042 FORMAT('VX VELOCITY')
 3043 FORMAT('VY VELOCITY')
 3044 FORMAT('VZ VELOCITY')
 3051 FORMAT('TOTAL GRADIENT')
 3052 FORMAT('GX GRADIENT')
 3053 FORMAT('GY GRADIENT')
 3054 FORMAT('GZ GRADIENT')
 1005 FORMAT('PAK CASE',I5)
 1006 FORMAT('CASE',I5,' TIME',1PD12.4)
 1010 FORMAT(3(1PD12.4))
 5000 FORMAT(I10,3(1PD12.4))
      END
C=====================================================================
      SUBROUTINE TGRAU3K(KONT,NEL,NDIM,IGRAF)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
C .......................................................................
C .
CE.   P R O G R A M
CE.       TO PRINTOUT 2/D ELEMENTS DATA IN NEUTRAL GRAPHICS FILE
CS.   P R O G R A M
CS.       ZA STAMPANJE 2/D ELEMENATA U NEUTRALNI FILE
C .
C .......................................................................
C 
      COMMON /VOPTIM/ NKONT,MAXLIN,LIN(10)
      COMMON /MATER/ NUMMAT
      COMMON /DODAT/ NDIMM
      DIMENSION KONT(9,MAXLIN,*),NEL(NDIMM,*)
      DIMENSION FIZ(14)         
C      COMMON /CDEBUG/ IDEBUG
C
C      IF(IDEBUG.GT.0) PRINT *, ' TGRAU3K'
C
C     FIZICKE OSOBINE
C
      NULA=0
      ZERO=0.
      ONE=1.
      JEDAN=1
      INDPR=25
      INDPD=2
      I11=11
      I2=2
      I4=4
      I8=8
      I14=14
      I48=48
C
C     E L E M E N T I
C
      NCVE=4
C     GRAFICKI OPIS ELEMENTA:
C     MEMBRANE - SA 4 CVOROVA = 13, SA 8 CVOROVA = 14
C     PLANE STRAIN - SA 4 CVOROVA = 19, SA 8 CVOROVA = 20
C     AXISYMMETRIC - SA 4 CVOROVA = 23, SA 8 CVOROVA = 24
C     PLATE - SA 4 CVOROVA = 17, SA 8 CVOROVA = 18
C     LAMINATE - SA 4 CVOROVA = 21, SA 8 CVOROVA = 22
      IFGD=19
      IF(NCVE.EQ.8) IFGD=20
C     VRSTA 2D ELEMENTA: SA 4 CVOROVA = 4, SA 8 CVOROVA = 5
      IFDI=4
      IF(NCVE.EQ.8) IFDI=5
C     TABELA FIZICKIH OSOBINA
C      IPTN=ISUMGR
C     BROJ CVOROVA NA ELEMENTU
      NNODS=NCVE
      IND=-1
      ITYP=404
      WRITE(IGRAF,1100) IND
      WRITE(IGRAF,1100) ITYP
      NLM=0
      DO 10 KK=1,NKONT
      DO 10 II=1,LIN(KK)
         NLM=NLM+1
C	
           NBREL=KONT(1,II,KK)
C          TABELA MATERIJALA
           MPTN=NEL(NDIM+1,NBREL)
           NEMA=NEL(NDIM+2,NBREL)
           IF(NEMA.GT.0) MPTN=NUMMAT+1
C        BOJA  
         ICOL=MPTN
c         ICOL=124-(MPTN-1)*2
c         ICOL=120-(MPTN-1)*3
         WRITE(IGRAF,1000) NLM,ICOL,MPTN,IFGD,IFDI,MPTN,NULA,NULA
C         IF(NCVE.EQ.4) THEN
            WRITE(IGRAF,1001) (KONT(J,II,KK),J=2,5),NULA,NULA,NULA,NULA,
     +                        NULA,NULA
            WRITE(IGRAF,1011) NULA,NULA,NULA,NULA,NULA,NULA,NULA,NULA,
     +                        NULA,NULA
C         ELSE
C            WRITE(IGRAF,1001) (NEL(J,I),J=1,8),
C     +                        NULA,NULA
C            WRITE(IGRAF,1011) NULA,NULA,NULA,NULA,NULA,NULA,NULA,NULA,
C     +                        NULA,NULA
C         ENDIF
         WRITE(IGRAF,1002) ZERO,ZERO,ZERO
         WRITE(IGRAF,1002) ZERO,ZERO,ZERO
         WRITE(IGRAF,1002) ZERO,ZERO,ZERO
         WRITE(IGRAF,1003) NULA,NULA,NULA,NULA,NULA,NULA,NULA,NULA,
     +                     NULA,NULA,NULA,NULA,NULA,NULA,NULA,NULA
   10 CONTINUE
      WRITE(IGRAF,1100) IND
C      ISUMEL=ISUMEL+NE
C      NCVE=NNCVE
      RETURN
C
 1000 FORMAT(I10,7I4)
 1001 FORMAT(10I10)
 1011 FORMAT(10I2)
 1002 FORMAT(3F3.0)
 1003 FORMAT(16I2)
 1100 FORMAT(I6)
 1200 FORMAT(6(1PE13.5))
      END
C=======================================================================
      SUBROUTINE TGRMATK(KONT,NEL,AKONST,NDIM)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      COMMON /VOPTIM/ NKONT,MAXLIN,LIN(10)
      COMMON /DODAT/ NDIMM
      DIMENSION KONT(9,MAXLIN,*),NEL(NDIMM,*)
      DIMENSION AKONST(3,5,*)
C*    BLOK ZA STAMPANJE MATERIJALA PO ELEMENTIMA ZA GRAFIKU - PRIVREMENO
      JEDAN=1
      NULA=0
      ZERO=0.
      MJ=-1
      M8=8
C
      IN=14
      IGRAF=48
      WRITE(IGRAF,1000) JEDAN,IN,JEDAN
      WRITE(IGRAF,3003) 
      WRITE(IGRAF,1010) ZERO,ZERO,ZERO
      WRITE(IGRAF,1000) NULA,NULA,NULA,NULA,NULA,NULA,NULA,NULA,NULA,NULA
      WRITE(IGRAF,1000) NULA,NULA,NULA,NULA,NULA,NULA,NULA,NULA,NULA,NULA
C
C 0=ANY,1=DISPL,2=ACCEL,3=FORCE,4=STRESS,5=STRAIN,6=TEMP,OTHER=USER
C     NODAL(7) ,ELEMENTAL(8)
      WRITE(IGRAF,1000) NULA,NULA,M8,M8
C
C =1, CAN NOT LINEARY COMBINE THIS OUTPUT
C =1, COMP(0-2) ARE THE X,Y,Z COMPONENT
C =1, THIS VECTOR HAS CONTROIDAL OR NODAL OUTPUT
      WRITE(IGRAF,1000) JEDAN,NULA,JEDAN
 1000 FORMAT(10I5)
 1010 FORMAT(3(1PD12.4))
 3003 FORMAT(' KOEFICIJENT FILTRACIJE')
C*

      NLM=0
      DO 723 KK=1,NKONT
C
	DO 134 II=1,LIN(KK)
	NLM=NLM+1
C	
      NBREL=KONT(1,II,KK)
      NMAT=NEL(NDIM+1,NBREL)
      NEMA=NEL(NDIM+2,NBREL)

      AKX=AKONST(3,1,NMAT)
C*    STAMPANJE MATERIJALA PO ELEMENTU
      WRITE(IGRAF,5000) NLM,AKX
C*
  134 CONTINUE
C
  723 CONTINUE
C
C*    KRAJ BLOKA ZA STAMPANJE PROTOKA PO ELEMENTU
      WRITE(IGRAF,5000) MJ,ZERO
 5000 FORMAT(I10,3(1PD12.4))
C*
      RETURN
      END
C=======================================================================
      SUBROUTINE TGRMATS(NEL,AKONST,NDIM,NE)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      COMMON /DODAT/ NDIMM
      DIMENSION NEL(NDIMM,*)
      DIMENSION AKONST(3,5,*)
C*    BLOK ZA STAMPANJE MATERIJALA PO ELEMENTIMA ZA GRAFIKU - PRIVREMENO
      JEDAN=1
      NULA=0
      ZERO=0.
      MJ=-1
      M8=8
C
      IN=13
      IGRAF=49
      WRITE(IGRAF,1000) JEDAN,IN,JEDAN
      WRITE(IGRAF,3003) 
      WRITE(IGRAF,1010) ZERO,ZERO,ZERO
      WRITE(IGRAF,1000) NULA,NULA,NULA,NULA,NULA,NULA,NULA,NULA,NULA,NULA
      WRITE(IGRAF,1000) NULA,NULA,NULA,NULA,NULA,NULA,NULA,NULA,NULA,NULA
C
C 0=ANY,1=DISPL,2=ACCEL,3=FORCE,4=STRESS,5=STRAIN,6=TEMP,OTHER=USER
C     NODAL(7) ,ELEMENTAL(8)
      WRITE(IGRAF,1000) NULA,NULA,M8,M8
C
C =1, CAN NOT LINEARY COMBINE THIS OUTPUT
C =1, COMP(0-2) ARE THE X,Y,Z COMPONENT
C =1, THIS VECTOR HAS CONTROIDAL OR NODAL OUTPUT
      WRITE(IGRAF,1000) JEDAN,NULA,JEDAN
 1000 FORMAT(10I5)
 1010 FORMAT(3(1PD12.4))
 3003 FORMAT(' KOEFICIJENT FILTRACIJE')
C*

	DO 134 NBREL=1,NE
C	
      NMAT=NEL(NDIM+1,NBREL)

      AKX=AKONST(3,1,NMAT)
C*    STAMPANJE MATERIJALA PO ELEMENTU
      WRITE(IGRAF,5000) NBREL,AKX
C*
  134 CONTINUE
C
C*    KRAJ BLOKA ZA STAMPANJE PROTOKA PO ELEMENTU
      WRITE(IGRAF,5000) MJ,ZERO
 5000 FORMAT(I10,3(1PD12.4))
C*
      RETURN
      END
C=======================================================================
C stampanje rezultata u odredjenim cvorovima
C=======================================================================
      SUBROUTINE STAU09c(RTH,CORD,ID,NPP,II,IND,IKOL,KOR,NZAD,NUMZAD,
     +                  KONT,POT,NEL)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      CHARACTER*6 PCIME(34)
      
      COMMON /ELECTR/ INDJOT,INDFS
      COMMON /OSATEZ/ ZASIC,IOSA,ISIL,IFIL
      COMMON /VOPTIM/ NKONT,MAXLIN,LIN(10)
      DIMENSION RTH(IKOL,*),CORD(3,*),ID(1,*),FSP(6),IN(8),NZAD(3,*)
      DIMENSION KONT(9,MAXLIN,*),POT(*),NEL(NDIMM,*)
      DIMENSION KONT1(40000), NZADPOT(176),PNODE(176),H(8)
      COMMON /PIJEZO/ CPOR(3,1000),NPIJEZ(20,100),NODP(100),NPIJ,NPOR,
     1                NPORCV(1000),NPOREL(1000),NEPOR,NPORE,NEPIJ
      COMMON /PORCEL/ RMIN(34),SMIN(34),TMIN(34)
      COMMON /DODAT/ NDIMM
C
      IF(NEPIJ.EQ.0) GO TO 100
      
C
      PCIME(1)='P07-04'
      PCIME(2)='P07-05'
      PCIME(3)='P07-07'
      PCIME(4)='P07-08'
      PCIME(5)='P07-10'
      PCIME(6)='P07-11'
      PCIME(7)='P07-12'
      PCIME(8)='P07-13'
      PCIME(9)='P11-01'
      PCIME(10)='P11-04'
      PCIME(11)='P11-05'
      PCIME(12)='P11-07'
      PCIME(13)='P11-08'
      PCIME(14)='P11-09'
      PCIME(15)='P11-10'
      PCIME(16)='P11-11'
      PCIME(17)='P11-12'
      PCIME(18)='P11-13'
      PCIME(19)='P11-14'
      PCIME(20)='P11-15'
      PCIME(21)='P11-16'
      PCIME(22)='P11-17'
      PCIME(23)='P11-18'
      PCIME(24)='P16-01'
      PCIME(25)='P16-04'
      PCIME(26)='P16-05'
      PCIME(27)='P16-06'
      PCIME(28)='P16-07'
      PCIME(29)='P16-08'
      PCIME(30)='P16-09'
      PCIME(31)='P16-10'
      PCIME(32)='P16-11'
      PCIME(33)='P16-12'
      PCIME(34)='P16-13'
C
      MJ=-1

      if(npij.gt.0) then
         WRITE(II,5000) MJ
         write(3,*) ' UCITAVANJE PODATAKA O PIJEZOMETRIMA'
         DO 12 I=1,NPIJ
            WRITE(3,*) ' PIJEZOMETAR',I,' BROJ CVOROVA',NODP(I)
            WRITE(3,*) ' CVOR,       X         Y       Z'
            DO 22 J=1,NODP(I)
               ji=NPIJEZ(J,I)
               WRITE(3,4001) ji,CORD(1,ji),CORD(2,ji),CORD(3,ji)
               if(j.gt.1) then
                  dif=CORD(3,NPIJEZ(J-1,I))-CORD(3,NPIJEZ(J,I))
                  if(dif.lt.0.) then
                     write(3,*) ' greska - pijezometar',i,' cvorovi',
     1                          NPIJEZ(J-1,I),NPIJEZ(J,I)
                     write(*,*) ' greska - pijezometar',i,' cvorovi',
     1                          NPIJEZ(J-1,I),NPIJEZ(J,I)
                     stop
                  endif
               endif
   22       CONTINUE
   12    CONTINUE
 4001    format(i10,3f10.1)
   
         DO 11 I=1,NPIJ
            DO 21 J=1,NODP(I)
               IF(RTH(1,NPIJEZ(J,I)).GT.CORD(IOSA,NPIJEZ(J,I))) THEN
                  WRITE(II,5000) I+1,RTH(1,NPIJEZ(J,I))
                  GO TO 11
               ENDIF
   21       CONTINUE
   11    CONTINUE
      endif
C
  100 IF(NEPOR.EQ.0) RETURN
C porne celije
C      if(npor.gt.0) then
C         WRITE(II,5000) MJ
C         DO 13 I=1,npor
C            WRITE(II,5000) nporcv(i),RTH(1,nporcv(i))
C   13    CONTINUE
C         WRITE(II,5000) MJ
C      endif
C porne celije ekstrapolacija
      if(NPORE.gt.0) then
         WRITE(II,5000) MJ
         DO 14 I=1,npore
C ODREDJIVANJE POTENCIJALA U PORNOJ CELIJI
              RPP=1.0+RMIN(I)
              SPP=1.0+SMIN(I)
              TPP=1.0+TMIN(I)
              RM=1.0-RMIN(I)
              TM=1.0-TMIN(I)
              SM=1.0-SMIN(I)
              RR=1.0-RMIN(I)*RMIN(I)
              SS=1.0-SMIN(I)*SMIN(I)
              TT=1.0-TMIN(I)*TMIN(I)
              DO IH=1,8
                H(IH)=0.
              ENDDO
              H(1)=0.125*RPP*SPP*TPP
              H(2)=0.125*RM*SPP*TPP
              H(3)=0.125*RM*SM*TPP
              H(4)=0.125*RPP*SM*TPP
              H(5)=0.125*RPP*SPP*TM
              H(6)=0.125*RM*SPP*TM
              H(7)=0.125*RM*SM*TM
              H(8)=0.125*RPP*SM*TM
            PPC=0.
            XPC=0.
            YPC=0.
            ZPC=0.
            DO JJH=1,8
              ncvor=NEL(JJH,NPOREL(I))          
              PPC=PPC + H(JJH)*RTH(1,ncvor)
              XPC=XPC+H(JJH)*CORD(1,ncvor)
              YPC=YPC+H(JJH)*CORD(2,ncvor)
              ZPC=ZPC+H(JJH)*CORD(3,ncvor)
              if(ppc.lt.zpc) ppc=0.D0                
C              WRITE(II,5001) I,ncvor,NPOREL(I),H(JJH),RTH(1,ncvor)
           enddo
C
            WRITE(II,5010) PCIME(I),PPC,XPC,YPC,ZPC
   14    CONTINUE
         WRITE(II,5000) MJ
      endif
C
      RETURN
 1000 FORMAT(10I6)
 1001 FORMAT(I10,F12.4)
 3001 FORMAT('TOTAL POTENTIAL')
 3004 FORMAT('DEPTH OF WATER')
 1005 FORMAT('PAK CASE',I5)
 1006 FORMAT('CASE',I5,' TIME',1PD12.4)
 1010 FORMAT(3(1PD12.4))
 1015 FORMAT('P-',I2,I10,1PD12.4)
 1016 FORMAT('P07-',I2,I10,1PD12.4)
 1017 FORMAT('P11-',I2,I10,1PD12.4)
 1018 FORMAT('P16-',I2,I10,1PD12.4)
 5000 FORMAT(I10,3(1PE12.4))
 5010 FORMAT(A6,4(1PE12.4))
 5001 FORMAT(3(I10),2(1PE12.4))
      END
C stampanje rezultata u odredjenim cvorovima
C=======================================================================
C Stampanje Temperatura na mestu termometara
C=======================================================================
      SUBROUTINE STAU09CT(RTH,CORD,II,VREME,KORAK,NEL)
      USE MESURMENTPOINTS
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      COMMON /DODAT/ NDIMM
C      COMMON /ELECTR/ INDJOT,INDFS
C      COMMON /OSATEZ/ ZASIC,IOSA,ISIL,IFIL
!       COMMON /VOPTIM/ NKONT,MAXLIN,LIN(10)
      DIMENSION RTH(1,*),CORD(3,*)
      DIMENSION NEL(NDIMM,*)
      DIMENSION H(10)
!       COMMON /PIJEZO/ CPOR(3,1000),NPIJEZ(20,100),NODP(100),NPIJ,NPOR,
!      1                NPORCV(1000),NPOREL(1000),NEPOR,NPORE,NEPIJ
!       COMMON /PORCEL/ RMIN(34),SMIN(34),TMIN(34)
C
!       IF(NEPIJ.EQ.0) GO TO 100
C
      MJ=-1
C       
      IF(MAX_MPOINTS.LE.0) RETURN
         WRITE(II,5000) MJ

 4001    format(6f10.2)
C porne celije
C      if(npor.gt.0) then
C         WRITE(II,5000) MJ
C         DO 13 I=1,npor
C            WRITE(II,5000) nporcv(i),RTH(1,nporcv(i))
C   13    CONTINUE
C         WRITE(II,5000) MJ
C      endif
C Termometri ekstrapolacija
         WRITE(II,5000) MJ
         WRITE(II,1010) VREME
         DO 14 I=1,MAX_MPOINTS
C ODREDJIVANJE POTENCIJALA na mestu termometra
C za heksa elemente
C              RPP=1.0+RMIN(I)
C              SPP=1.0+SMIN(I)
C              TPP=1.0+TMIN(I)
C              RM=1.0-RMIN(I)
C              TM=1.0-TMIN(I)
C              SM=1.0-SMIN(I)
C              RR=1.0-RMIN(I)*RMIN(I)
C              SS=1.0-SMIN(I)*SMIN(I)
C              TT=1.0-TMIN(I)*TMIN(I)
C              DO IH=1,8
C                H(IH)=0.
C              ENDDO
C              H(1)=0.125*RPP*SPP*TPP
C              H(2)=0.125*RM*SPP*TPP
C              H(3)=0.125*RM*SM*TPP
C              H(4)=0.125*RPP*SM*TPP
C              H(5)=0.125*RPP*SPP*TM
C              H(6)=0.125*RM*SPP*TM
C              H(7)=0.125*RM*SM*TM
C              H(8)=0.125*RPP*SM*TM
C TETRA ELEMENTI SA 10 CVOROVA
              RMIN=MP_COORDS(4,I)
              SMIN=MP_COORDS(5,I)
              TMIN=MP_COORDS(6,I)
              RST=1.0-RMIN-SMIN-TMIN
              DO IH=1,10
                H(IH)=0.
              ENDDO
              H(5)=4*RMIN*RST
              H(6)=4*RMIN*SMIN
              H(7)=4*SMIN*RST
              H(8)=4*TMIN*RST
              H(9)=4*RMIN*TMIN
              H(10)=4*SMIN*TMIN
              H(1)=RST-0.5*(H(5)+H(7)+H(8))
              H(2)=RMIN-0.5*(H(5)+H(6)+H(9))
              H(3)=SMIN-0.5*(H(6)+H(7)+H(10))
              H(4)=TMIN-0.5*(H(8)+H(9)+H(10))
            PPC=0.
            XPC=0.
            YPC=0.
            ZPC=0.
            DO JJH=1,10
              ncvor=NEL(JJH,MP_ELEMENT(I))          
              PPC=PPC + H(JJH)*RTH(1,ncvor)
              XPC=XPC+H(JJH)*CORD(1,ncvor)
              YPC=YPC+H(JJH)*CORD(2,ncvor)
              ZPC=ZPC+H(JJH)*CORD(3,ncvor)
C  NEMA ZA TEMPERATURE OVOG USLOVA       if(ppc.lt.zpc) ppc=0.D0                
              WRITE(II,5001) I,ncvor,MP_ELEMENT(I),H(JJH),RTH(1,ncvor)
           enddo
C
            MP_RESULTS(KORAK,I+1)=PPC
            WRITE(II,5010) MPOINT_ID(I),PPC,XPC,YPC,ZPC
   14    CONTINUE
         WRITE(II,5000) MJ
         MP_VREME(KORAK)=VREME
!          write(3,*) 'KORAK',KORAK
!           write(3,*) 'MP_RESULTS(KORAK,2)',MP_RESULTS(KORAK,2)
!          if(korak.eq.BRKORAKA) then
!             do ikbr=1,BRKORAKA
!               write(3,*) 'MP_RESULTS(',ikbr,',2)',MP_RESULTS(ikbr,2)
!             enddo
!          endif
C
      RETURN
 1000 FORMAT(10I6)
 1001 FORMAT(I10,F12.4)
 3001 FORMAT('TOTAL TEMPERATURE')
 1005 FORMAT('PAK CASE',I5)
 1006 FORMAT('CASE',I5,' TIME',1PD12.4)
 1010 FORMAT(1PD12.4)
 1015 FORMAT('P-',I2,I10,1PD12.4)
 1016 FORMAT('P07-',I2,I10,1PD12.4)
 1017 FORMAT('P11-',I2,I10,1PD12.4)
 1018 FORMAT('P16-',I2,I10,1PD12.4)
 5000 FORMAT(I10,3(1PE12.4))
 5010 FORMAT(A10,4(1PE12.4))
 5001 FORMAT(3(I10),2(1PE12.4))
      END
C=======================================================================
C=======================================================================
      SUBROUTINE STAU35(SIGMA,KOR)
      USE ELEMENTS
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
C      COMMON /IZOL4B/ NGS12,ND,MSLOJ,MXS,MSET,LNSLOJ,LMATSL,LDSLOJ,LBBET
      COMMON /NUMNPT/ NUMZAD,NPT,NDIM,MAXSIL,JEDN,NWK,NET
      COMMON /DODAT/ NDIMM
      COMMON /NIDEAS/ IDEAS
      COMMON /SRPSKI/ ISRPS
C
      DIMENSION SIGMA(NET,8),S(7),N(20)
C
      NNCVE=NDIM
      NCVE=NDIM
      IF(NDIM.LT.20) NDIM=8
      IF(NDIM.EQ.21) NDIM=20
C      
      JEDAN=1
      NULA=0
      ZERO=0.
      ONE=1.
      MJ=-1
      M451=451
      M2=2
      M3=3
      M4=4
      M5=5
      M8=8
      M9=9
C
      II=49
      NGAUSX=2
      NGAUSY=2
      NGAUSZ=2
      ISTNA=3
C
c / DA LI TREBA OVO      NPROS=MODPR2(NMODM)
c      NGXYZ=NGS12*NPROS*MXS
C
   88 IND=60031
      NND=70031
      CALL NAPUN3(N,NDIM,NND)
      KK=1
      INP=5
      GO TO 10
C
C
   10 WRITE(II,1000) KOR,IND,JEDAN
      IF(KK.EQ.1.AND.INP.NE.0) THEN
         IF(IND.EQ.60031) WRITE(II,60031) 
      ENDIF   
      WRITE(II,1010) ZERO,ZERO,ZERO
      IF(ISTNA.EQ.0) THEN
       WRITE(II,1000) N(1),N(2),N(3),N(4),N(5),N(6),N(7),N(8),N(9),N(10)
       WRITE(II,1000) 
     +       N(11),N(12),N(13),N(14),N(15),N(16),N(17),N(18),N(19),N(20)
      ELSE
        WRITE(II,1000) NULA,NULA,NULA,NULA,NULA,NULA,NULA,NULA,NULA,NULA
        WRITE(II,1000) NULA,NULA,NULA,NULA,NULA,NULA,NULA,NULA,NULA,NULA
      ENDIF
      INDM=MOD(IND,100)
C dodat sledeci red za verziju neu fajla 10.0 /Sneza 26.12.2016.
      WRITE(II,1000) NULA
      IF(INDM.GE.10.AND.INDM.LT.50) WRITE(II,1000) NULA,NULA,M4,M8
      IF(INDM.GE.50.AND.INDM.LT.99) WRITE(II,1000) NULA,NULA,M5,M8
      IF(ISTNA.EQ.0) THEN
         WRITE(II,1000) NULA,NULA,JEDAN
      ELSE
         WRITE(II,1000) JEDAN,NULA,JEDAN
      ENDIF
C      
      DO 40 NLM=1,NET
          IF(elemtip(NLM).gt.100) THEN
            NTYPE=elemtip(NLM)/100
            NTIMES=100
          ELSE
            NTYPE=elemtip(NLM)/10
            NTIMES=10
          ENDIF
          NDIMEL=elemtip(NLM)-NTIMES*NTYPE
C
          NMM=NLM
C         IF(ICVEL.EQ.1) NMM=MCVEL(NLM)
C         MAT=NMAT(NLM)
          MAT=NEL(NDIM+1,NLM)
          NGS12=8
        SELECT CASE (elemtip(NLM))
        CASE (12:13)
          NGS12=2
        CASE (23,26)
          NGS12=3
        CASE (24,28,34,310)
          NGS12=4
         CASE DEFAULT
C          WRITE(*,*) 'ELEMENT TYPE UNKNOWN NETIP=',elemtip(NLM)
        END SELECT
C
          CALL SREDK3(SIGMA,S,NLM,NGS12,NET)
        IF(KK.EQ.1.AND.INP.NE.0) THEN
           IF(IND.EQ.60031.AND.S(7).GE.0.D0) then
            WRITE(II,5000) NMM,S(7)
           ELSE
            WRITE(II,5000) NMM,0.D0
           ENDIF
         ENDIF
   40 CONTINUE
      WRITE(II,5500) MJ,ZERO
      IF(ISTNA.GT.0) GO TO 31
C
      DO 30 NC=1,NDIMEL
      KTMP=1
      DO 20 NLM=1,NET
C  preskace stampanje kod 1D i 2D elemenata
!           WRITE(*,*) NC,NLM,elemtip(NLM)
       SELECT CASE (elemtip(NLM))
         CASE (12:28)
          GOTO 20
         CASE (34,310)
          IPODT=3
          NGAUSX=4
         CASE DEFAULT
C          WRITE(*,*) 'ELEMENT TYPE UNKNOWN NETIP=',elemtip(NLM)
      END SELECT
        IF(KTMP.eq.1) THEN
          IF(elemtip(NLM).gt.100) THEN
            NTYPE=elemtip(NLM)/100
            NTIMES=100
          ELSE
            NTYPE=elemtip(NLM)/10
            NTIMES=10
          ENDIF
          NDIMEL=elemtip(NLM)-NTIMES*NTYPE
          WRITE(II,1000) KOR,N(NC),JEDAN
          IF(KK.EQ.1.AND.INP.NE.0) THEN
            IF(IND.EQ.60031) WRITE(II,70031) NC 
          ENDIF   
          WRITE(II,1010) ZERO,ZERO,ZERO
          WRITE(II,1000) NULA,NULA,NULA,NULA,NULA,NULA,NULA,NULA,NULA,NULA
          WRITE(II,1000) NULA,NULA,NULA,NULA,NULA,NULA,NULA,NULA,NULA,NULA
          IF(INDM.GE.10.AND.INDM.LT.50) WRITE(II,1000) NULA,NULA,M4,M8
          IF(INDM.GE.50.AND.INDM.LT.99) WRITE(II,1000) NULA,NULA,M5,M8
          WRITE(II,1000) NULA,NULA,NULA
          KTMP=0
        ENDIF
        
        IF(NDIMEL.EQ.8.AND.NGAUSX.EQ.2) THEN
          IF(NC.EQ.1) IJ=NGAUSY*NGAUSZ+NGAUSZ+1
          IF(NC.EQ.2) IJ=NGAUSZ+1
          IF(NC.EQ.3) IJ=1
          IF(NC.EQ.4) IJ=NGAUSY*NGAUSZ+1
          IF(NC.EQ.5) IJ=NGAUSY*NGAUSZ+NGAUSZ+2
          IF(NC.EQ.6) IJ=NGAUSZ+2
          IF(NC.EQ.7) IJ=2
          IF(NC.EQ.8) IJ=NGAUSY*NGAUSZ+2
        ENDIF
        IF(NDIMEL.EQ.8.AND.NGAUSX.EQ.3) THEN
          IF(NC.EQ.1) IJ=2*NGAUSY*NGAUSZ+2*NGAUSZ+1
          IF(NC.EQ.2) IJ=2*NGAUSZ+1
          IF(NC.EQ.3) IJ=1
          IF(NC.EQ.4) IJ=2*NGAUSY*NGAUSZ+1
          IF(NC.EQ.5) IJ=2*NGAUSY*NGAUSZ+2*NGAUSZ+3
          IF(NC.EQ.6) IJ=2*NGAUSZ+3
          IF(NC.EQ.7) IJ=3
          IF(NC.EQ.8) IJ=2*NGAUSY*NGAUSZ+3
        ENDIF
        IF(NDIMEL.EQ.20.AND.NGAUSX.EQ.2) THEN
          IF(NC.EQ.1) IJ=NGAUSY*NGAUSZ+NGAUSZ+1
          IF(NC.EQ.2) IJ=NGAUSZ+1
          IF(NC.EQ.3) IJ=1
          IF(NC.EQ.4) IJ=NGAUSY*NGAUSZ+1
          IF(NC.EQ.5) IJ=NGAUSY*NGAUSZ+NGAUSZ+2
          IF(NC.EQ.6) IJ=NGAUSZ+2
          IF(NC.EQ.7) IJ=2
          IF(NC.EQ.8) IJ=NGAUSY*NGAUSZ+2
          IF(NC.EQ.9) THEN
              IJ=-(NGAUSY*NGAUSZ+NGAUSZ+1)
              JI=NGAUSZ+1
          ENDIF
          IF(NC.EQ.10) THEN
              IJ=-(NGAUSZ+1)
              JI=1
          ENDIF
          IF(NC.EQ.11) THEN
              IJ=-1
              JI=NGAUSY*NGAUSZ+1
          ENDIF
          IF(NC.EQ.12) THEN
              IJ=-(NGAUSY*NGAUSZ+1)
              JI=NGAUSY*NGAUSZ+NGAUSZ+1
          ENDIF
          IF(NC.EQ.13) THEN
              IJ=-(NGAUSY*NGAUSZ+NGAUSZ+1)
              JI=NGAUSY*NGAUSZ+NGAUSZ+2
          ENDIF
          IF(NC.EQ.14) THEN
              IJ=-(NGAUSZ+1)
              JI=NGAUSZ+2
          ENDIF
          IF(NC.EQ.15) THEN
              IJ=-1
              JI=2
          ENDIF
          IF(NC.EQ.16) THEN
              IJ=-(NGAUSY*NGAUSZ+1)
              JI=NGAUSY*NGAUSZ+2
          ENDIF
          IF(NC.EQ.17) THEN
              IJ=-(NGAUSY*NGAUSZ+NGAUSZ+2)
              JI=NGAUSZ+2
          ENDIF
          IF(NC.EQ.18) THEN
              IJ=-(NGAUSZ+2)
              JI=2
          ENDIF
          IF(NC.EQ.19) THEN
              IJ=-2
              JI=NGAUSY*NGAUSZ+2
          ENDIF
          IF(NC.EQ.20) THEN
              IJ=-(NGAUSY*NGAUSZ+2)
              JI=NGAUSY*NGAUSZ+NGAUSZ+2
          ENDIF
        ENDIF
        IF(NDIMEL.EQ.20.AND.NGAUSX.EQ.3) THEN
          IF(NC.EQ.1) IJ=2*NGAUSY*NGAUSZ+2*NGAUSZ+1
          IF(NC.EQ.2) IJ=2*NGAUSZ+1
          IF(NC.EQ.3) IJ=1
          IF(NC.EQ.4) IJ=2*NGAUSY*NGAUSZ+1
          IF(NC.EQ.5) IJ=2*NGAUSY*NGAUSZ+2*NGAUSZ+3
          IF(NC.EQ.6) IJ=2*NGAUSZ+3
          IF(NC.EQ.7) IJ=3
          IF(NC.EQ.8) IJ=2*NGAUSY*NGAUSZ+3
          IF(NC.EQ.9) IJ=NGAUSY*NGAUSZ+2*NGAUSZ+1
          IF(NC.EQ.10) IJ=NGAUSZ+1
          IF(NC.EQ.11) IJ=NGAUSY*NGAUSZ+1
          IF(NC.EQ.12) IJ=2*NGAUSY*NGAUSZ+NGAUSZ+1
          IF(NC.EQ.13) IJ=2*NGAUSY*NGAUSZ+2*NGAUSZ+2
          IF(NC.EQ.14) IJ=2*NGAUSZ+2
          IF(NC.EQ.15) IJ=2
          IF(NC.EQ.16) IJ=2*NGAUSY*NGAUSZ+2
          IF(NC.EQ.17) IJ=NGAUSY*NGAUSZ+2*NGAUSZ+3
          IF(NC.EQ.18) IJ=NGAUSZ+3
          IF(NC.EQ.19) IJ=NGAUSY*NGAUSZ+3
          IF(NC.EQ.20) IJ=2*NGAUSY*NGAUSZ+NGAUSZ+3
        ENDIF
c    
       IF(IPODT.EQ.3) THEN
C          WRITE(*,*) 'NDIMEL,NGAUSX,NLM',NDIMEL,NGAUSX,NLM
       IF(NDIMEL.EQ.4.AND.NGAUSX.EQ.1) THEN
         IF(NC.EQ.1) IJ=1
         IF(NC.EQ.2) IJ=1
         IF(NC.EQ.3) IJ=1
         IF(NC.EQ.4) IJ=1
       ENDIF
       IF(NDIMEL.EQ.10.AND.NGAUSX.EQ.4) THEN
         IF(NC.EQ.1) IJ=1
         IF(NC.EQ.2) IJ=2
         IF(NC.EQ.3) IJ=3
         IF(NC.EQ.4) IJ=4
         IF(NC.EQ.5) THEN
            IJ=-1
            JI= 2
         ENDIF
         IF(NC.EQ.6) THEN
            IJ=-2
            JI= 3
         ENDIF
         IF(NC.EQ.7) THEN
            IJ=-1
            JI= 3
         ENDIF
         IF(NC.EQ.8) THEN
            IJ=-1
            JI= 4
         ENDIF
         IF(NC.EQ.9) THEN
            IJ=-2
            JI= 4
         ENDIF
         IF(NC.EQ.10) THEN
            IJ=-3
            JI= 4
         ENDIF
       ENDIF
       IF(NDIMEL.EQ.10.AND.NGAUSX.EQ.5) THEN
         IF(NC.EQ.1) IJ=2
         IF(NC.EQ.2) IJ=3
         IF(NC.EQ.3) IJ=4
         IF(NC.EQ.4) IJ=5
         IF(NC.EQ.5) THEN
            IJ=-3
            JI= 2
         ENDIF
         IF(NC.EQ.6) THEN
            IJ=-4
            JI= 3
         ENDIF
         IF(NC.EQ.7) THEN
            IJ=-2
            JI= 4
         ENDIF
         IF(NC.EQ.8) THEN
            IJ=-2
            JI= 5
         ENDIF
         IF(NC.EQ.9) THEN
            IJ=-3
            JI= 5
         ENDIF
         IF(NC.EQ.10) THEN
            IJ=-5
            JI= 4
         ENDIF
       ENDIF
       ENDIF
C
          NMM=NLM
C         IF(ICVEL.EQ.1) NMM=MCVEL(NLM)
            IF(KK.EQ.1) THEN
               IF(IND.EQ.60031.AND.IJ.GT.0) THEN
                 IF(SIGMA(NLM,IJ).GE.0)WRITE(II,5000) NMM,SIGMA(NLM,IJ)
               ELSEIF(IND.EQ.60031.AND.IJ.LT.0) THEN
                  POLA=(SIGMA(NLM,-IJ)+SIGMA(NLM,JI))/2
                  IF(SIGMA(NLM,IJ).GE.0)WRITE(II,5000) NMM,POLA
               ENDIF
            ENDIF  
C
   20 CONTINUE
      WRITE(II,5500) MJ,ZERO
   30 CONTINUE
C
   31 CONTINUE
C      IF(KK.EQ.1.AND.INP.NE.0) THEN
C         IF(IND.EQ.60031) GO TO 182
C      ENDIF
  999 RETURN
 1000 FORMAT(10(I10,','))
 1001 FORMAT(I10,',',F12.4,',')
60031 FORMAT('PORNI PRITISAK')
70031 FORMAT('SOLID NODE',I10,' PORNI PRITISAK')
C
 1005 FORMAT('PAK CASE',I5)
 1006 FORMAT('CASE',I5,' TIME',1PE12.4)
 1010 FORMAT(3(1PE12.4,','))
 5000 FORMAT(I10,',',3(1PE12.4,','))
 5500 FORMAT(I2,',',3(1PE12.4,','))
      END
C=======================================================================
C=======================================================================
      SUBROUTINE NAPUN3(N,NCVE,NND)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C     
      DIMENSION N(*)
C
      CALL ICLEAR(N,20)
      IDEL=200
      NND=NND-IDEL
      DO 10 I=1,NCVE
         N(I)=NND+I*IDEL
   10 CONTINUE
      RETURN
      END
C=======================================================================
C=====================================================================
      SUBROUTINE SREDK25(SIGMA,S,NPROS,NGS12,IND)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C     
      DIMENSION SIGMA(*),S(7)
C
      MINUS=0
      IF(IND.LT.0) THEN
         MINUS=1
         IND=-IND
      ENDIF
      CALL CLEAR(S,7)
      DO 10 I=1,NGS12
         GO TO (1,2,3,4,5)IND
    1    S(1)=S(1)+SIGMA(1+(I-1)*NPROS)/NGS12
         GO TO 10
    2    S(2)=S(2)+SIGMA(2+(I-1)*NPROS)/NGS12
         GO TO 10
    3    S(3)=S(3)+SIGMA(3+(I-1)*NPROS)/NGS12
         GO TO 10
    4    S(4)=S(4)+SIGMA(4+(I-1)*NPROS)/NGS12
         GO TO 10
    5    S(1)=S(1)+SIGMA(1+(I-1)*NPROS)/NGS12
         S(2)=S(2)+SIGMA(2+(I-1)*NPROS)/NGS12
         S(3)=S(3)+SIGMA(3+(I-1)*NPROS)/NGS12
         S(4)=S(4)+SIGMA(4+(I-1)*NPROS)/NGS12
   10 CONTINUE
C
      IF(IND.EQ.5) THEN
         IF(MINUS.EQ.0) THEN
            XMY=S(1)-S(2)
            YMZ=S(2)-S(4)
            ZMX=S(4)-S(1)
            EFEK=0.5D0*(XMY*XMY+YMZ*YMZ+ZMX*ZMX)+
     1           3.D0*(S(3)*S(3))
            IF(EFEK.GT.1.D-19) S(5)=DSQRT(EFEK)
         ELSE
            SR=(S(1)+S(2)+S(4))/3.
            SD1=S(1)-SR
            SD2=S(2)-SR
            SD3=S(4)-SR
            EFEK=2.*(SD1*SD1+SD2*SD2+SD3*SD3+
     1               S(4)*S(4))/3.
            IF(EFEK.GT.1.D-19) S(5)=DSQRT(EFEK)
         ENDIF
      ENDIF
      RETURN
      END
C=======================================================================
      SUBROUTINE SREDK3(SIGMA,S,NLM,NGS12,NE)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C     
      DIMENSION SIGMA(NE,*),S(7)
C
      CALL CLEAR(S,7)
      DO 10 I=1,NGS12
         S(1)=S(1)+SIGMA(NLM,I)
         S(2)=S(2)+SIGMA(NLM,I)
         S(3)=S(3)+SIGMA(NLM,I)
         S(4)=S(4)+SIGMA(NLM,I)
         S(5)=S(5)+SIGMA(NLM,I)
         S(6)=S(6)+SIGMA(NLM,I)
   10 CONTINUE
      S(1)=S(1)/NGS12
      S(2)=S(2)/NGS12
      S(3)=S(3)/NGS12
      S(4)=S(4)/NGS12
      S(5)=S(5)/NGS12
      S(6)=S(6)/NGS12
      S(7)=S(1)
c      XMY=S(1)-S(2)
c      YMZ=S(2)-S(3)
c      ZMX=S(3)-S(1)
c      EFEK=0.5D0*(XMY*XMY+YMZ*YMZ+ZMX*ZMX)+
c     1     3.D0*(S(4)*S(4)+S(5)*S(5)+S(6)*S(6))
c      IF(EFEK.GT.1.D-19) S(7)=DSQRT(EFEK)

      RETURN
      END
C======================================================================
C ZA TEMPERATURE
C======================================================================
      SUBROUTINE STAS09T(RTH,CORD,ID,NPP,II,IND,IKOL,KOR,NZAD,NUMZAD,
     +                  KONT)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      COMMON /VOPTIM/ NKONT,MAXLIN,LIN(10)
      DIMENSION RTH(IKOL,*),CORD(3,*),ID(1,*),FSP(6),IN(8),NZAD(3,*)
      DIMENSION KONT(9,MAXLIN,*)
      DIMENSION KONT1(40000)
C
      JEDAN=1
      NULA=0
      ZERO=0.
      ONE=1.
      MJ=-1
      M451=451
      M1=1
      M2=2
      M3=3
      M4=4
      M5=5
      M6=6
      M7=7
      M8=8
      M9=9
C      II=49
C
      IN(1)=IND
      IN(2)=IND+1
      IN(3)=IND+2
      IN(4)=IND+3
      IN(5)=IND+4
      IN(6)=IND+5
      IN(7)=IND+6
      IN(8)=IND+7
C
C PRIVREMENO
      IF(IN(1).EQ.10) THEN
      DO I=1,40000
         KONT1(I)=0
      ENDDO
      ENDIF
C      
      IP=0
C
   30 IP=IP+1
      WRITE(II,1000) KOR,IN(IP),JEDAN
      IF(IN(1).EQ.1.AND.IP.EQ.1) WRITE(II,3001) 
      IF(IN(1).EQ.9.AND.IP.EQ.1) WRITE(II,3002) 
      IF(IN(1).EQ.10.AND.IP.EQ.1) WRITE(II,3003) 
      IF(IN(1).EQ.41.AND.IP.EQ.1) WRITE(II,3041) 
      IF(IN(1).EQ.41.AND.IP.EQ.2) WRITE(II,3042) 
      IF(IN(1).EQ.41.AND.IP.EQ.3) WRITE(II,3043) 
      IF(IN(1).EQ.41.AND.IP.EQ.4) WRITE(II,3044) 
      IF(IN(1).EQ.51.AND.IP.EQ.1) WRITE(II,3051) 
      IF(IN(1).EQ.51.AND.IP.EQ.2) WRITE(II,3052) 
      IF(IN(1).EQ.51.AND.IP.EQ.3) WRITE(II,3053) 
      IF(IN(1).EQ.51.AND.IP.EQ.4) WRITE(II,3054) 
      WRITE(II,1010) ZERO,ZERO,ZERO
C OVO SU NULE ZA TEMPERATURE
      IF(IP.EQ.1.AND.IN(1).LT.40)
     +WRITE(II,1000) NULA,NULA,NULA,NULA,NULA,NULA,NULA,NULA,NULA,NULA
      IF(IP.EQ.1.AND.IN(1).GT.40)
     +WRITE(II,1000)IN(2),IN(3),IN(4),NULA,NULA,NULA,NULA,NULA,NULA,NULA
      IF(IP.EQ.2)
     +  WRITE(II,1000)IN(2),NULA,NULA,NULA,NULA,NULA,NULA,NULA,NULA,NULA
      IF(IP.EQ.3)
     +  WRITE(II,1000)NULA,IN(3),NULA,NULA,NULA,NULA,NULA,NULA,NULA,NULA
      IF(IP.EQ.4)
     +  WRITE(II,1000)NULA,NULA,IN(4),NULA,NULA,NULA,NULA,NULA,NULA,NULA
      WRITE(II,1000) NULA,NULA,NULA,NULA,NULA,NULA,NULA,NULA,NULA,NULA
C
C 0=ANY,1=DISPL,2=ACCEL,3=FORCE,4=STRESS,5=STRAIN,6=TEMP,OTHER=USER
C     NODAL(7) ,ELEMENTAL(8)
C dodat sledeci red za verziju neu fajla 10.0 /Sneza 26.12.2016.
      WRITE(II,1000) NULA
      IF(IN(1).EQ.1) WRITE(II,1000) NULA,NULA,M6,M7
      IF(IN(1).EQ.9) WRITE(II,1000) NULA,NULA,M7,M7
      IF(IN(1).EQ.10) WRITE(II,1000) NULA,NULA,M8,M8
      IF(IN(1).EQ.41.OR.IN(1).EQ.51) WRITE(II,1000) NULA,NULA,M3,M7
C
C =1, CAN NOT LINEARY COMBINE THIS OUTPUT
C =1, COMP(0-2) ARE THE X,Y,Z COMPONENT
C =1, THIS VECTOR HAS CONTROIDAL OR NODAL OUTPUT
      IF(IN(1).GT.40) THEN
         IF(IP.EQ.1) THEN
            WRITE(II,1000) JEDAN,JEDAN,JEDAN
         ELSE
            WRITE(II,1000) NULA,JEDAN,JEDAN
         ENDIF
      ELSE
         WRITE(II,1000) JEDAN,NULA,JEDAN
      ENDIF
C      DO 10 I=1,NPP
         DO 20 J=1,IKOL
            FSP(J) = 0.0D0
            IF(IKOL.EQ.1) THEN
               IF(IN(1).EQ.1) THEN
                  IF(ID(J,I).EQ.0) GO TO 20
                  K = ID(J,I)
                  IF(K.GT.0) THEN
C OVO JE SAMO ZA 3D SLOBODNA POVRSINA
C                     IF(RTH(1,K).GT.CORD(3,I)) FSP(J)=RTH(1,K)
                     FSP(J)=RTH(1,K)
                  ENDIF
               ELSEIF(IN(1).EQ.9) THEN
                 FSP(J)=1.0D0
                 DO L=1,NUMZAD 
                    IF(NZAD(1,L).EQ.I) GO TO 1201
                 ENDDO
                 FSP(J)=0.D0
 1201            CONTINUE
               ELSE
                 DO LL=1,NKONT
                    FSP(J)=LL
                 DO L=1,LIN(LL) 
C                 DO LI=2,5 
C                 DO LI=2,NNODE+1 
C                    IF(KONT(LI,L,LL).EQ.I) THEN
                  IEL=KONT(1,L,LL)
                  VAL=FSP(1)
                  WRITE(II,5000) IEL,VAL
C                       IF(KONT1(IEL).EQ.0) THEN
C                          KONT1(IEL)=1
C                          GO TO 1202
C                       ELSE
C                          GO TO 10
C                       ENDIF 
C                    ENDIF
C                 ENDDO
                 ENDDO
                 ENDDO
                 FSP(J)=0.D0
 1202            CONTINUE
               ENDIF
            ELSE
               FSP(J)=RTH(J,I)
            ENDIF
   20    CONTINUE
         VAL=FSP(1)
         IF(IP.EQ.1.AND.IN(1).GT.40)
     +      VAL=DSQRT(FSP(1)*FSP(1)+FSP(2)*FSP(2)+FSP(3)*FSP(3))
         IF(IP.EQ.2) VAL=FSP(1)
         IF(IP.EQ.3) VAL=FSP(2)
         IF(IP.EQ.4) VAL=FSP(3)
C         IF(DABS(VAL).LT.1.E-12) GO TO 10
C         IF(IN(1).EQ.10) THEN
C            WRITE(II,5000) IEL,VAL
C         ELSE
C            WRITE(II,5000) I,VAL
C         ENDIF  
C   10 CONTINUE
      WRITE(II,5000) MJ,ZERO
      IF(IP.LT.4.AND.IN(1).GT.40) GO TO 30
      RETURN
 1000 FORMAT(10I5)
 1001 FORMAT(I10,F12.4)
 3001 FORMAT('TEMPERATURE')
 3002 FORMAT('PRESCRIBED TEMPERATURE')
 3003 FORMAT('PRESCRIBED SURFACES')
 3041 FORMAT('TOTAL VELOCITY')
 3042 FORMAT('VX VELOCITY')
 3043 FORMAT('VY VELOCITY')
 3044 FORMAT('VZ VELOCITY')
 3051 FORMAT('TOTAL GRADIENT')
 3052 FORMAT('GX GRADIENT')
 3053 FORMAT('GY GRADIENT')
 3054 FORMAT('GZ GRADIENT')
 1005 FORMAT('PAK CASE',I5)
 1006 FORMAT('CASE',I5,' TIME',1PD12.4)
 1010 FORMAT(3(1PD12.4))
 5000 FORMAT(I10,3(1PD12.4))
      END
C=====================================================================
C=======================================================================
      SUBROUTINE STAU09T(RTH,CORD,ID,NPP,II,IND,IKOL,KOR,NZAD,NUMZAD,
     +                  KONT,POT)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      COMMON /ELECTR/ INDJOT,INDFS
      COMMON /OSATEZ/ ZASIC,IOSA,ISIL,IFIL
      COMMON /VOPTIM/ NKONT,MAXLIN,LIN(10)
      DIMENSION RTH(IKOL,*),CORD(3,*),ID(1,*),FSP(6),IN(8),NZAD(3,*)
      DIMENSION KONT(9,MAXLIN,*),POT(*)
      DIMENSION KONT1(40000)
C
      FSP(3)=0.
C
      JEDAN=1
      NULA=0
      ZERO=0.
      ONE=1.
      MJ=-1
      M451=451
      M1=1
      M2=2
      M3=3
      M4=4
      M5=5
      M6=6
      M7=7
      M8=8
      M9=9
C      II=49
C
      IN(1)=IND
      IN(2)=IND+1
      IN(3)=IND+2
      IN(4)=IND+3
      IN(5)=IND+4
      IN(6)=IND+5
      IN(7)=IND+6
      IN(8)=IND+7
C
C PRIVREMENO
      IF(IN(1).EQ.10) THEN
      DO I=1,40000
         KONT1(I)=0
      ENDDO
      ENDIF
C      
      IP=0
C
   30 IP=IP+1
      WRITE(II,1000) KOR,IN(IP),JEDAN
      IF(IN(1).EQ.1.AND.IP.EQ.1) WRITE(II,3001) 
      IF(IN(1).EQ.9.AND.IP.EQ.1) WRITE(II,3002) 
!       IF(IN(1).EQ.10.AND.IP.EQ.1) WRITE(II,3003) 
      IF(IN(1).EQ.51.AND.IP.EQ.1) WRITE(II,3051) 
      IF(IN(1).EQ.51.AND.IP.EQ.2) WRITE(II,3052) 
      IF(IN(1).EQ.51.AND.IP.EQ.3) WRITE(II,3053) 
      IF(IN(1).EQ.51.AND.IP.EQ.4) WRITE(II,3054) 
      WRITE(II,1010) ZERO,ZERO,ZERO
C OVO SU NULE ZA TEMPERATURE
      IF(IP.EQ.1.AND.IN(1).LT.40)
     +WRITE(II,1000) NULA,NULA,NULA,NULA,NULA,NULA,NULA,NULA,NULA,NULA
      IF(IP.EQ.1.AND.IN(1).GT.40)
     +WRITE(II,1000)IN(2),IN(3),IN(4),NULA,NULA,NULA,NULA,NULA,NULA,NULA
      IF(IP.EQ.2)
     +  WRITE(II,1000)IN(2),NULA,NULA,NULA,NULA,NULA,NULA,NULA,NULA,NULA
      IF(IP.EQ.3)
     +  WRITE(II,1000)NULA,IN(3),NULA,NULA,NULA,NULA,NULA,NULA,NULA,NULA
      IF(IP.EQ.4)
     +  WRITE(II,1000)NULA,NULA,IN(4),NULA,NULA,NULA,NULA,NULA,NULA,NULA
      WRITE(II,1000) NULA,NULA,NULA,NULA,NULA,NULA,NULA,NULA,NULA,NULA
C
C 0=ANY,1=DISPL,2=ACCEL,3=FORCE,4=STRESS,5=STRAIN,6=TEMP,OTHER=USER
C     NODAL(7) ,ELEMENTAL(8)
C dodat sledeci red za verziju neu fajla 10.0 /Sneza 26.12.2016.
      WRITE(II,1000) NULA
      IF(IN(1).EQ.1) WRITE(II,1000) NULA,NULA,M6,M7
      IF(IN(1).EQ.9) WRITE(II,1000) NULA,NULA,M7,M7
      IF(IN(1).EQ.10) WRITE(II,1000) NULA,NULA,M8,M8
      IF(IN(1).EQ.11) WRITE(II,1000) NULA,NULA,M6,M7
      IF(IN(1).EQ.41.OR.IN(1).EQ.51.OR.IN(1).EQ.61)
     1WRITE(II,1000) NULA,NULA,M3,M7
C
C =1, CAN NOT LINEARY COMBINE THIS OUTPUT
C =1, COMP(0-2) ARE THE X,Y,Z COMPONENT
C =1, THIS VECTOR HAS CONTROIDAL OR NODAL OUTPUT
      IF(IN(1).GT.40) THEN
         IF(IP.EQ.1) THEN
            WRITE(II,1000) JEDAN,JEDAN,JEDAN
         ELSE
            WRITE(II,1000) NULA,JEDAN,JEDAN
         ENDIF
      ELSE
         WRITE(II,1000) JEDAN,NULA,JEDAN
      ENDIF
      DO 10 I=1,NPP
         DO 20 J=1,IKOL
            FSP(J) = 0.0D0
            IF(IKOL.EQ.1) THEN
               IF(IN(1).EQ.1) THEN
                  IF(ID(J,I).EQ.0) GO TO 20
                  K = ID(J,I)
                  IF(K.GT.0) THEN
!                      IF(INDFS.GT.0) THEN
c                       POTENCIJAL ISPOD SLOBODNE POVRSINE
!                         IF(POT(K).GT.CORD(IOSA,I)) then
                            FSP(J)=RTH(J,K)
!                         else 
!                             go to 10
!                         endif
!                      ELSE   
!                         IF(POT(K).GT.CORD(IOSA,I)) then
!                             FSP(J)=RTH(J,K)
!                         else 
!                             go to 10
!                         endif
c  Sneza 10.9.2015  'stampa vrednost 0 iznad slobodne povrsine
C                       FSP(J)=RTH(J,K) 
!                      ENDIF
                  ENDIF
               ELSEIF(IN(1).EQ.11) THEN
                  IF(ID(J,I).EQ.0) GO TO 20
                  K = ID(J,I)
                  IF(K.GT.0) THEN
C                    DUBINA VODE ILI PRITISAK VODENOG STUBA
                     FSP(J)=-(RTH(1,K)-CORD(IOSA,I))
                     IF(FSP(J).GT.0.) FSP(J)=0.     
                  ENDIF
               ELSEIF(IN(1).EQ.9) THEN
                 FSP(J)=1.0D0
                 DO L=1,NUMZAD 
                    IF(NZAD(1,L).EQ.I) GO TO 1201
                 ENDDO
                 FSP(J)=0.D0
 1201            CONTINUE
               ELSE
                 DO LL=1,NKONT
                    FSP(J)=LL
                 DO L=1,LIN(LL)
                 DO LI=2,9 
C                 DO LI=2,NNODE+1 
                    IF(KONT(LI,L,LL).EQ.I) THEN
                       IEL=KONT(1,L,LL)
                       IF(KONT1(IEL).EQ.0) THEN
                          KONT1(IEL)=1
                          GO TO 1202
                       ELSE
                          GO TO 10
                       ENDIF 
                    ENDIF
                 ENDDO
                 ENDDO
                 ENDDO
                 FSP(J)=0.D0
 1202            CONTINUE
               ENDIF
            ELSE
               IF(INDFS.GT.0) THEN
                  K = ID(1,I)
                  IF(K.GT.0) THEN
!                      IF(POT(K).GT.CORD(IOSA,I)) FSP(J)=RTH(J,I)
c                  ELSE
c                     FSP(J)=RTH(J,I)
                  ENDIF
               ELSE   
                  FSP(J)=RTH(J,I)
               ENDIF
            ENDIF
   20    CONTINUE
         VAL=FSP(1)
         IF(IP.EQ.1.AND.IN(1).GT.40)
     +      VAL=DSQRT(FSP(1)*FSP(1)+FSP(2)*FSP(2)+FSP(3)*FSP(3))
         IF(IP.EQ.2) VAL=FSP(1)
         IF(IP.EQ.3) VAL=FSP(2)
         IF(IP.EQ.4) VAL=FSP(3)
czn         IF(DABS(VAL).LT.1.E-12) GO TO 10
         IF(IN(1).EQ.10) THEN
            WRITE(II,5000) IEL,VAL
         ELSE
            WRITE(II,5000) I,VAL
         ENDIF  
   10 CONTINUE
      WRITE(II,5000) MJ,ZERO
      IF(IP.LT.4.AND.IN(1).GT.40) GO TO 30
      RETURN
 1000 FORMAT(10I5)
 1001 FORMAT(I10,F12.4)
 3001 FORMAT('TEMPERATURE')
 3002 FORMAT('PRESCRIBED TEMPERATURE')
!  3003 FORMAT('PRESCRIBED SURFACES')
 3051 FORMAT('TOTAL GRADIENT')
 3052 FORMAT('GX GRADIENT')
 3053 FORMAT('GY GRADIENT')
 3054 FORMAT('GZ GRADIENT')
 1005 FORMAT('PAK CASE',I5)
 1006 FORMAT('CASE',I5,' TIME',1PD12.4)
 1010 FORMAT(3(1PD12.4))
 5000 FORMAT(I10,3(1PD12.4))
      END
C=======================================================================
