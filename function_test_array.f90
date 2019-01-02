module my_subs

implicit none

contains

!scalar product helper function
function scalar_product(x,y)
	real, intent(in) :: x(3),y(3)
	real :: scalar_product
	scalar_product=x(1)*y(1)+x(2)*y(2)+x(3)*y(3)
end function scalar_product

!2-norm helper function
function norm_2(x)
	real, intent(in) :: x(3)
	real :: norm_2
	norm_2=sqrt(x(1)**2+x(2)**2+x(3)**2)
end function norm_2

!function defining the current density
function current_density(x)
	real, intent(in) :: x(3)	!absolute position vector
	real :: current_density(3)	!current density in cartesian coordinates
	real :: r0(3)	!position vector of the current density cylnder
	real :: h0		!height of the current-density cylnder
	real :: j0		!current density magnitude
	real :: rho1
	real :: rho2
	real :: d(3)	!direction vector of the current density object
	real :: r(3)	!relative position vector with respect to current density origin
	real :: rho		!rho component of relative position
	real :: h		!height variable (distance to origin of cylnder along d vector)

	j0=1e-3
	h0=30
	d=[0.0,0.0,1.0]
	r0=[0.0,0.0,0.0]
	d=d/sqrt(norm_2(d))
	r=x-r0
	rho=norm_2(r-d*(scalar_product(d,r)))
	h=scalar_product(r,d)
	
	if(h>0 .and. h<h0 .and. rho>rho1 .and. rho<rho2) then
		current_density=[0,0,0]
	else
	current_density(1) = j0/sqrt(x(1)**2+x(2)**2)
	current_density(2) = j0/sqrt(x(1)**2+x(2)**2)
	current_density(3) = j0/sqrt(x(1)**2+x(2)**2)
	end if
	
end function current_density

end module my_subs


program crosstest
	use my_subs
	implicit none

	real :: r(3)
	real :: n(3)

	n= [1.0,1.0,1.0]
	r=current_density(n)
	write (*, *) r
end program crosstest
