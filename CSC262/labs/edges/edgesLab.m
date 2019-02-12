%% CSC 262: Edges
% Introduction
clearvars;
% The Matlab script used to make your comparisons and generate your report
% (10 points) Horizontal and vertical partial derivatives (A.4, A.6)
% (10 points) Observations of the horizontal and vertical partial derivatives (A.5, A.7)
% (10 points) Gradient magnitude image and observations (B.2)
% Note: You may wish to place the magnitude and partial derivative images side-by-side in the same figure using montage.
% (10 points) Gradient orientation images and observations (C.4, C.3, D.6)
% (10 points) Multi-scale gradient magnitude and orientation images (E.2)
% (5 points) Observations of multi-scale gradient magnitudes (F.1)
% (10 points) Observations of multi-scale gradient orientations (F.2)
% (10 points) Multi-scale edge images (E.5)
% Note: Be sure your discussion clearly explains which axis (horizontal or vertical) varies scale and which varies threshold.
% (10 points) Multi-scale edge observations (F.3)
% (10 points) Professionalism of write-up

bugImg = im2double(imread('/home/weinman/courses/CSC262/images/bug.png'));
bugImg_size = size(bugImg);
img_width = bugImg_size(2);
img_height = bugImg_size(1);

maxIndex = 5;
thresholds = [24 32 48 64 96]/1024;
thresholdCount = size(thresholds, 2);

sgm(img_height,img_width,maxIndex+1) = 0;
woi(img_height, img_width,3,maxIndex+1) = 0;
ter(img_height, img_width, thresholdCount, maxIndex+1) = 0;

for var=0:maxIndex
    gauss = gkern(2^var);
    dgauss = gkern(2^var, 1);

    % sucks: res = conv2(dgauss, gauss', bugImg, 'valid');
    % Horizontal partial derivative 
    pd_x = conv2(gauss, dgauss, bugImg, 'valid');
    
    % A.4
    %imshowpair(bugImg, pd_x, 'montage');
    %title('Derivative With Respect to X: Comparison');

    % A.5 The derivative with respect to X bug image is brightest along
    % boundaries where regions of low brightness lead to areas of high
    % brightness horizontally to the right. An example is the boundary created
    % when the black stripes on the catipilar's body meet a white stripe. The
    % opposite is true for the dark parts of the derivative with respect to X
    % bug image. Finally, the gray regions of this image indicate a region
    % where there is no (or little) change in the brightness with respect to
    % the X.

    %A.6
    % Vertical partial derivative
    %figure;
    pd_y = conv2(dgauss, gauss, bugImg, 'valid');
    %imshowpair(bugImg, pd_y, "montage"); %this seems to auto rescale with []
    %title('Derivative With Respect to Y: Comparison');

    % A.7 The vertical derivative bug image is brightest along boundaries
    % where regions of low brightness lead to areas of high brightness
    % vertically down. This is the same pattern as before, but with a different
    % orientation. So, regions where the black stripes meet the white stripes
    % yields bright spots on the derivative with respect to Y image. Again, the
    % opposite is true for the dark parts of the derivative with respect to Y
    % bug image and the gray regions of this image indicate a region
    % where there is no (or little) change in the brightness with respect to
    % the Y.

    % Part B
    pd_mag = sqrt(pd_x .^ 2 + pd_y .^ 2);

    % B.2 
    %figure;
    %imshowpair(bugImg, pd_mag, 'montage');

    % The boundaries between the black and white stripes on the
    % caterpillars body yield the strongest response in the derivate magnitude 
    % image. We expect this since those areas display the highest change in
    % brightness values with respect to both X and Y directions.

    % B.3    
    for tVar=1:thresholdCount
        pd_mag_bin_lop =  pd_mag;
        pd_mag_bin_lop = (pd_mag_bin_lop > thresholds(tVar));
        pd_mag_bin_lop(img_height, img_width) = 0;
        ter(:,:,tVar,var+1) = pd_mag_bin_lop;
    end
    
    % imshow(pd_mag_bin, []);

    %% Part C
    pd_orientation = atan2(pd_y, pd_x);
    imshow(pd_orientation, [-pi, pi]);
    colormap(hsv);
    colorbar;

    %% Part D
    % Currently: -pi to pi
    % Transform: (value + pi)/2 pi
    pd_orientation_scl = (pd_orientation + pi)/(2*pi);
    pd_orientation_scl(img_height, img_width) = 0;
    
    % Scaled gradient magnitude
    pd_mag_scl = pd_mag / max(pd_mag(:));
    pd_mag_scl(img_height, img_width) = 0;
    sgm(:,:,var+1) = pd_mag_scl;
    
    value_mat = ones(img_height, img_width);
    pd_hsv_img = cat(3, pd_orientation_scl, pd_mag_scl, value_mat);
    
    % Weighted Orientation Images
    pd_hsv_to_rgb = hsv2rgb(pd_hsv_img);
    pd_hsv_to_rgb(img_height, img_width, 3) = 0;
    woi(:,:,:,var+1) = pd_hsv_to_rgb;
    
    % imshowpair(pd_hsv_to_rgb, bugImg, 'montage');
end

%% Finishing
%temp = reshape(ter, [img_height, img_width, thresholdCount, (maxIndex+1)]);
%S = [thresholdCount, maxIndex+1];
%temp = reshape(ter, img_height, img_width, 1, S);
temp = reshape(ter, img_height, img_width, []);

%figure;
%montage(woi, 'Size', [3,2]);
%figure;
%montage(sgm, 'Size', [3,2]);

figure(1);
montage(temp, 'Size', [thresholdCount, maxIndex+1]);