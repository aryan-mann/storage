function M = circle(R)
% CIRCLE Binary circle image
%
%	  M = CIRCLE(R) returns a 2*R+1 x 2*R+1 matrix with a black
%	  circle on a white background.


% Jerod Weinman 
% jerod@acm.org


% $Id: circle.m 1 2009-05-07 13:41:00Z weinman $


  [X,Y] = meshgrid(-R:R,-R:R);
  
  M = (X.^2+Y.^2 > R^2);
  
  M = uint8(M);
  
  