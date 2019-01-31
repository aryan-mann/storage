function Z = stitch(X,Y,D)
% STITCH Stitch two images together from a translational offset
%
% Z = STITCH(X,Y,D) stitches two grayscale images X and Y, assumed
% to be the same size. D is a 1x2 vector containing the row and
% column offsets from X to Y. The resulting image Z contains the
% average of the two images where they overlap.
  
% Jerod Weinman
% jerod@acm.org
% (c) 2010
  
  % Stitched image
  Z = zeros(size(X)+abs(D));
  
  % Displaced images for intermediate procesing
  Zx = Z;
  Zy = Z;
  
  % Masks for where each image will go
  Mx = Z;
  My = Z;
  
  % Identify the rows and columns in which to place each image on
  % the result
  if D(1)>0 % Y is shifted down -- put X at top
    rrx  = 1:size(X,1);
    rry = (size(Z,1)-size(Y,1)+1):size(Z,1);
  else % Y is shifted up -- put X at bottom
    rrx = (size(Z,1)-size(X,1)+1):size(Z,1);
    rry = 1:size(Y,1);
  end;
  
  if D(2)>0 % Y is shifted right -- put X at left
    ccx  = 1:size(X,2);
    ccy = (size(Z,2)-size(Y,2)+1):size(Z,2);
  else % Y is shifted left -- put X at right
    ccx = (size(Z,2)-size(X,2)+1):size(Z,2);
    ccy = 1:size(Y,2);
  end;

  % Set the masks for each image
  Mx( rrx, ccx ) = 1;
  My( rry, ccy ) = 1;
  
  % Assign the images according to their masks
  Zx(find(Mx)) = X;
  Zy(find(My)) = Y;
  
  % Add the two images where they do not overlap, and 
  % Average the two images where they do overlap
  Z = (Zx+Zy) .* ~(Mx & My) + (Zx + Zy)/2 .* (Mx & My);
  