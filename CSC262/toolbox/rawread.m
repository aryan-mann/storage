function X = rawread(filename)
% RAWREAD Read a RAW image file from disk
%
% X = RAWREAD(filename) loads a RAW file of images bytes from filename into
% a 1280x960 uint8 image.
    
  
  fid = fopen(filename,'r');
  X = uint8(reshape(fread(fid,'uint8'),[1280 960]))';
  fclose(fid);