function M = maxima(X)
% MAXIMA Find the maxima in an image over a four-connected neighborhood
%
%  M = MAXIMA(X) where X is a grayscale image of any class. M is a
%  logical matrix the same size as X indicating points where the
%  pixel is greater than its four neighbors along adjacent rows and
%  columns.
  
% Jerod Weinman
% jerod@acm.org
% (c) 2010, 2015
  
  M = false(size(X));
  
  % Center (valid) region
  Xc = X(2:end-1,2:end-1);
  
  % Assign center region a comparison between all four neighbors
  M(2:end-1,2:end-1) = ...
      Xc > X(1:end-2,2:end-1) & ... % Center > Above
      Xc > X(3:end,2:end-1) & ... % Center > Below
      Xc > X(2:end-1,1:end-2) & ... % Center > Left
      Xc > X(2:end-1,3:end); % Center > Right
  
