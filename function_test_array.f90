module my_subs

implicit none

contains

function my_square(a)
  real :: my_square(3)
  integer, intent(in) :: a(3)

  my_square(1) = a(1)*a(1)
  my_square(2) = a(2)*a(2)
  my_square(3) = a(3)*a(3)
end function my_square

end module my_subs


program crosstest
  use my_subs
  implicit none

  real :: r(3)
  integer :: n(3)

  n= [ 4, 5, 6 ]
  r=my_square(n)
  write (*, *) r

end program crosstest
