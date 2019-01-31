function F = gkern(T, A)
% GKERN 1-D Gaussian kernel of any spatial scale and derivative order
%
% F = GKERN(T) returns the Gaussian kernel of scale T>0, where T is
% the variance of the Gaussian, in pixels.
%
% F = GKERN(T,A) returns a kernel of the A>=0 first differences (or
% the A'th derivative).
%
% The length of the kernel F is proportional to the sqrt of the scale
% T.
%
% Implementation based on [1].
% 
% [1] T. Lindeberg, "Scale-Space for Discrete Signals," IEEE
% Transactions on Pattern Analysis and Machine Intelligence,
% vol. 12, no. 3,  pp. 234-254, March, 1990.
  
% Jerod Weinman 
% jerod@acm.org
% (c) 2008

    
if (T==0)
  % Delta function. Not very interesting.
  F=1;
  return;
end;

if (nargin==1)
  % Default: Gaussian, no derivative.
  A = 0;
elseif (A<0)
  error('Derivative requires A>=0');
end;

extent = 5; % large extent reduces error, but increases kernel size.
	
% Calculate the standard deviation
sigma = sqrt(T);

% Length of the kernel proportional to std. dev.
N = ceil(extent*sigma);

% Extra entries for calculating the finite differences
hA = ceil(A/2);
NA = N+hA;

% Discrete Gaussian from the Bessel I function (see [1]).
F = besseli(-NA:NA, T,1);

% Take the finite difference/derivative if necessary
if (A>0)
    F = diff(F,A);
end;

% If an odd derivative, we will have an asymmetric filter (even length).
% Lop off the last bit to make the filter have odd length, and thus
% a definitive center point.
if mod(A,2)==1
    F = F(1:end-1);
end;
