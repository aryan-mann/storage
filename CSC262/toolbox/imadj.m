function Y = imadj(X,in,out)
% IMADJ Adjust the range of an image
%
% Y = IMADJ(X) maps the values in image X to new values Y such that
% the smallest value in X is mapped to 0 and the largest value is
% mapped to 1.
%
% Y = IMADJ(X, [low_in; high_in], [low_out; high_out]) maps the values
% in image X to new values in Y such that values between low_in and
% high_in linearly map to values between low_out and high_out.
%
% Matlab's built-in IMADJUST places requirements on low_in and
% high_in that are not sufficiently generic.
  
    
% Jerod Weinman
% jerod@acm.org
% (c) 2010
  
  if nargin<3
    out = [0 1];
  end;
  
  if nargin<2
    in = [min(min(X)) max(max(X))];
  end;
  
  Y = (X-in(1))./(in(2)-in(1));
  Y = Y.*(out(2)-out(1)) + out(1);