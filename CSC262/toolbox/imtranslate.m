function r = imtranslate( f, di, dj, b )
% IMTRANSLATE Translation of a 2D (one channel) image
%
% Y = IMTRANSLATE( X, di, dj) where X is a one channel (e.g.,
% grayscale or indexed) image, di is change in ROWS and dj
% change in COLS. size(Y) = size(X) + abs([di dj]).
%
% Y = IMTRANSLATE( X, di, dj, b) where b indicates whether to use
% a border (default) or 0 value.
  
 
% Jerod Weinman
% jerod@acm.org
% (c) 2003
  
  % Use a border value
  if nargin<4
	b = 1;
  end;
  

  N = size( f, 1 );
  M = size( f, 2 );
  

  % Extract row indices
  if di>0
	iind = [(di+1):N, N*ones(1,di)];
  elseif di<0
	iind = [ ones(1,-di), 1:(N+di) ];
  else
	iind = 1:N;
  end
  
  % Extract column indices
  if dj>0
	jind = [(dj+1):M, M*ones(1,dj)];
  elseif dj<0
	jind = [ ones(1,-dj), 1:(M+dj) ];
  else
	jind = 1:M;
  end
  
  % Build indexed result
  r = f( iind, jind );
  
  % Set additional/hallucinated values to zero
  if ~b
	if di>0
	  r(end-di+1:end,:) = 0;
	elseif di<0
	  r(1:(-di),:) = 0;
	end;
	
	if dj>0
	  r(:,end-dj+1:end,:) = 0;
	elseif dj<0
	  r(:,1:(-dj)) = 0;
	end;
  end;
	