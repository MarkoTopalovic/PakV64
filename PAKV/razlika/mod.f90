      MODULE MUMPSM
        INCLUDE 'dmumps_struc.h'
        TYPE (DMUMPS_STRUC) mumps_par
      END MODULE
      
      module ppr
        double precision,dimension(:,:),allocatable :: PORNIEL
        integer,dimension(:,:),allocatable :: IDJSTAMP1
      end module ppr
      
      module STIFFNESS
        integer*8 :: stiff_n
        integer*8 :: nonzeros
        integer*8,dimension(:),allocatable :: rows
        integer*8,dimension(:),allocatable :: columns
        double precision,dimension(:),allocatable :: stiff
      end module STIFFNESS
      
      module ELEMENTS
        double precision,dimension(:),allocatable :: thick
        integer,dimension(:),allocatable :: elemtip
        integer :: numeltip
        integer,dimension(6) :: eltypes
        integer,dimension(:,:),allocatable :: NEL
      end module ELEMENTS
      
      module NODES
        integer*8,dimension(:,:),allocatable :: ID
        double precision, dimension(:,:),allocatable :: CORD
      end module NODES
      
      module MATRIXINIT
        integer*8,dimension(:),allocatable :: MAXA
        integer*8,dimension(:),allocatable :: MHT
      end module MATRIXINIT
      
      module PREDISCRIBED
        integer*8,dimension(:),allocatable :: NZADC
        integer*8,dimension(:,:),allocatable :: NELTOK
        integer*8,dimension(:,:),allocatable :: NELR
        integer*8 :: MAXTQE
        integer*8 :: MAXER
        integer*8 :: NWATERS
        integer*8 :: NSENSORS
        double precision :: Alpha
        double precision :: Rd_BOFANG
        double precision :: D_BOFANG
        integer*8,dimension(2) :: IDSENSOR
        double precision,dimension(2) :: DISTSENSOR
        integer*8,dimension(:,:),allocatable :: WATER
        integer*8,dimension(:,:),allocatable :: SENSOR
        double precision,dimension(:),allocatable :: HSENSOR
        double precision :: TPOC
        double precision,dimension(:),allocatable :: HFACE
        double precision,dimension(2) :: FSENSOR
        integer*8 :: NUMAXISPTSX
        integer*8 :: NUMAXISPTSY
        integer*8 :: NUMAXISPTSZ
        integer*8,dimension(:,:),allocatable :: INTAXISPOINTX
        integer*8,dimension(:,:),allocatable :: INTAXISPOINTY
        integer*8,dimension(:,:),allocatable :: INTAXISPOINTZ
        double precision,dimension(:),allocatable :: XAXISPTCORD
        double precision,dimension(:),allocatable :: YAXISPTCORD
        double precision,dimension(:),allocatable :: ZAXISPTCORD
      end module PREDISCRIBED
      
      module MESURMENTPOINTS
        integer*8 :: BRKORAKA
        integer*8 :: MAX_MPOINTS
        character*10,dimension(:),allocatable :: MPOINT_ID
        integer*8,dimension(:),allocatable :: MP_ELEMENT
        double precision,dimension(:,:),allocatable :: MP_COORDS
        double precision,dimension(:),allocatable :: MP_VREME
        double precision,dimension(:,:),allocatable :: MP_RESULTS
        double precision,dimension(:),allocatable :: MP_RESULTS_NIZ
      end module MESURMENTPOINTS
      
      module RESULTS
        double precision,dimension(:),allocatable :: TT1
        double precision,dimension(:),allocatable :: TT10
        double precision,dimension(:),allocatable :: PRIV
        double precision,dimension(:),allocatable :: PRIV1
        double precision,dimension(:),allocatable :: UBRZ0
        double precision,dimension(:),allocatable :: UBRZ
        double precision,dimension(:),allocatable :: BRZ0
        double precision,dimension(:),allocatable :: BRZ
        double precision,dimension(:,:),allocatable :: SKEF
        double precision,dimension(:),allocatable :: SKEFN
        double precision,dimension(:,:),allocatable :: AK
        double precision,dimension(:,:),allocatable :: DEFOR
        double precision,dimension(:,:,:),allocatable :: VG
        double precision,dimension(:,:,:),allocatable :: GG
        double precision,dimension(:,:),allocatable :: VECTJ
        double precision,dimension(:,:),allocatable :: POMER
        integer*8,dimension(:),allocatable :: IVECT
        double precision,dimension(:),allocatable :: SILE
    end module RESULTS
    
    module KONTURE
        integer*8,dimension(:),allocatable :: LIN
      end module KONTURE
