% RES = innerProd(MTX)
%
% Compute (MTX' * MTX) efficiently (i.e., without copying the matrix)

function res = innerProd(mtx)

%% NOTE: THIS CODE SHOULD NOT BE USED! (MEX FILE IS CALLED INSTEAD)

warning('You should compile the MEX version of "innerProd.c",\n         found in the MEX subdirectory of matlabPyrTools, and put it in your matlab path.  It is MUCH faster.\n');

res = mtx' * mtx;
