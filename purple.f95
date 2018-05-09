! program emulates Japanese PURPLE machine
program purple
   implicit none
   ! interface declaration
   interface
      ! level incrementing subroutine
      subroutine step(levcnt)
         integer, intent(inout), dimension(0:3) :: levcnt
      end subroutine step
      ! function emulates pegboard mapping of sixes and twenties
      function pegboard(ptch, sixes, twenties) result(ctch)
         character, intent(in) :: ptch
         character (len = *), intent(in) :: sixes, twenties
         character :: ctch
      end function pegboard
      ! function simulates sixes relay
      function relay_six(NS, NL, NR, relay, lev, stp)
         integer :: relay_six
         integer, intent(in) :: NS, NL, NR
         integer, intent(in), dimension(NS,NL) :: relay
         integer, intent(in) :: lev
         integer, intent(inout) :: stp
      end function relay_six
      ! function simulate twenties relay
      function relay_twenty(NT, NL, NR, relay, lev, stp, flag)
         integer :: relay_twenty
         integer, intent(in) :: NT, NL, NR
         integer, intent(in), dimension(NT,NL,NR) :: relay
         integer, intent(in), dimension(0:3) :: lev
         integer, intent(inout) :: stp
         character, intent(in) :: flag
      end function relay_twenty
   end interface
   ! variable and array declarations
   character (len = 6), parameter :: vowels = 'AEIOUY', sixes = 'NOKTYU'
   character (len = 20), parameter :: consonants = 'BCDFGHJKLMNPQRSTVWXZ', &
   twenties = 'XEQLHBRMPDICJASVWGZF'
   integer, parameter :: NR = 3, NL = 25, NS = 6, NT = 20
   integer :: i, j, k, idx, recno, eof
   character :: ch, flag
   character, dimension(25) :: buffer
   character (len = 80) :: line
   integer, dimension(NS,NL) :: e6, d6, r6
   integer, dimension(NT,NL,NR) :: e20, d20, r20
   integer, dimension(0:3) :: levcnt
   ! data initializations
   data levcnt/4*1/
   ! processing
   call getarg(1, flag)
   if (flag /= 'e' .and. flag /= 'd') then
      write (*,*) 'Usage: purple e/d'
      stop 'processing terminated'
   end if
   if (flag == 'e') recno = 1
   if (flag == 'd') recno = 2
   open (7,file='relays.dat',access='direct',form='formatted',recl=3300)
   read (7,1000,rec=recno) r6, r20
   close (7)
   !write (*,'(6i2)') r6
   !write (*,'(20i3)') r20
   !stop 'under development'
   read (*,2000,iostat=eof) line
   j = 0
   do while (eof == 0)
      k = len(trim(line))
      do i = 1, k
         ch = line(i:i)
         ! to uppercase
         if (ch >= 'a' .and. ch <= 'z') ch = char(ichar(ch) - 32)
         if (ch < 'A' .or. ch > 'Z') cycle ! not alphabetic
         ch = pegboard(ch, sixes, twenties)
         idx = index(vowels, ch)
         if (idx /= 0) then
            j = j + 1
            idx = relay_six(NS, NL, NR, r6, levcnt(0), idx)
            buffer(j) = sixes(idx:idx)
            call step(levcnt)
            if (j == 25) then
               write (*,3000) (buffer(idx), idx = 1, j)
               j = 0
            end if
            cycle ! we are done here
         end if
         idx = index(consonants, ch)
         if (idx /= 0) then
            j = j + 1
            idx = relay_twenty(NT, NL, NR, r20, levcnt, idx, flag)
            call step(levcnt)
            buffer(j) = twenties(idx:idx)
            if (j == 25) then
               write (*,3000) (buffer(idx), idx = 1, j)
               j = 0
            end if
         end if
      end do
      read (*,2000,iostat=eof) line
   end do
   ! pad out final block of 5 if necessary
   do while (mod(j, 5) /= 0) 
      j = j + 1
      buffer(j) = char(int(26 * rand()) + 65)
   end do
   ! write final line if there is one
   if (j > 0) write (*,3000) (buffer(idx), idx = 1, j)
   1000 format (1650i2)
   2000 format (A)
   3000 format (4(5A1,X),5A1)
end program purple

! function emulates pegboard mapping of sixes and twenties
function pegboard(ptch, sixes, twenties) result(ctch)
   implicit none
   ! dummy arguments
   character, intent(in) :: ptch
   character (len = *), intent(in) :: sixes, twenties
   ! local variables
   character :: ctch
   character (len = 6), parameter :: vowels = 'AEIOUY'
   character (len = 20), parameter :: consonants = 'BCDFGHJKLMNPQRSTVWXZ'
   integer :: idx
   ! processing
   idx = index(sixes, ptch)
   if (idx /= 0) then
      ctch = vowels(idx:idx)
      return
   end if
   idx = index(twenties, ptch)
   if (idx /= 0) then
      ctch = consonants(idx:idx)
      return
   end if
   ctch = ptch ! fail safe
end function pegboard

! subroutine increments levels
subroutine step(levcnt)
   implicit none
   ! dummy arguments
   integer, intent(inout), dimension(0:3) :: levcnt
   ! local variables
   integer i
   ! processing
   
   ! check which of the others advances
   if (levcnt(0) == 24 .and. levcnt(2) == 25) then 
      levcnt(3) = levcnt(3) + 1
   else if (levcnt(0) == 25) then
      levcnt(2) = levcnt(2) + 1
   else
      levcnt(1) = levcnt(1) + 1
   end if
      
   levcnt(0) = levcnt(0) + 1  ! always advances
   
   do i = 0, 3
      if (levcnt(i) > 25) levcnt(i) = 1
   end do 
end subroutine step

! function simulates sixes relay
function relay_six(NS, NL, NR, relay, lev, stp)
   implicit none
   ! dummy arguments
   integer :: relay_six
   integer, intent(in) :: NS, NL, NR
   integer, intent(in), dimension(NS,NL) :: relay
   integer, intent(in) :: lev
   integer, intent(inout) :: stp
   ! processing
   relay_six = relay(stp,lev)
end function relay_six

! function simulates twenties relay
function relay_twenty(NT, NL, NR, relay, lev, stp, flag)
   implicit none
   ! dummy arguments
   integer :: relay_twenty
   integer, intent(in) :: NT, NL, NR
   integer, intent(in), dimension(NT,NL,NR) :: relay
   integer, intent(in), dimension(0:3) :: lev
   integer, intent(inout) :: stp
   character, intent(in) :: flag
   ! local variables
   integer :: i, startidx, endidx, stepidx
   if (flag == 'e') then
      startidx = 1
      endidx = NR
      stepidx = 1
   else
      startidx = NR
      endidx = 1
      stepidx = -1
   end if
   ! processing
   do i = startidx, endidx, stepidx
      stp = relay(stp,lev(i),i)
   end do
   relay_twenty = stp
end function relay_twenty
