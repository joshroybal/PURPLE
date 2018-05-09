program formdata
   implicit none
   ! variable definitions
   integer, parameter :: NR = 3, NL = 25, NS = 6, NT = 20
   integer :: i, j, k, idx
   integer, dimension(NS+1,NL) :: d_sixes
   integer, dimension(NT+1,NL,NR) :: d_twenties
   integer, dimension(NS,NL) :: e_sixes, d6
   integer, dimension(NT,NL,NR) :: e_twenties, d20
   ! processing
   ! read deciphering level mappings
   open (7,file='purple.txt',status='old',action='read')
   read (7,*) d_sixes
   read (7,*) d_twenties
   close (7)
   ! determine enciphering levels from deciphering levels
   ! sixes
   do j = 1, NL
      do i = 1, NS
         idx = 1
         do while (i /= d_sixes(idx+1,j))
            idx = idx + 1
         end do
         e_sixes(i,j) = idx
      end do
   end do
   ! twenties
   do k = 1, 3
      do j = 1, NL
         do i = 1, NT
            idx = 1
            do while (i /= d_twenties(idx+1,j,k))
               idx = idx + 1
            end do
          e_twenties(i,j,k) = idx
         end do
      end do
   end do
   ! write records to direct access data file
   open (8,file='relays.dat',access='direct',form='formatted',recl=6600)
   write (8,5000,rec=1) e_sixes, e_twenties,((d_sixes(i,j),i=2,NS+1),j=1,NL), &
      (((d_twenties(i,j,k),i=2,NT+1),j=1,NL),k=1,NR)
   5000 format (3300i2)
   ! 5000 format (3300z0.2) hex with leading zeros
   close (8)
   write (*,*) 'data written to formatted direct accesss file relays.dat'
end program formdata
