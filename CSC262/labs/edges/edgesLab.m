% The Code Used to work with the caterpillar image

% administrativia
warning('off','images:initSize:adjustingMag');
clearvars;

% initializing the image
img = im2double(imread('/home/weinman/courses/CSC262/images/bug.png'));
img_width = size(img, 2);
img_height = size(img, 1);

% setting the given thresholds and Gaussian variances 
variances = [32 16 4 2 1];
variances_size = size(variances, 2);
thresholds = [24 32 48 64 96]/1024;
thresholds_size = size(thresholds, 2);

% forcing allocation (i.e., ensure size matches img size) for collections
% of normalized gradient magnitude images (list_ses), gradient orientation
% images (list_co), and threshold-and-variance varied images (list_tk)
list_nes(img_height,img_width,variances_size) = 0;
list_co(img_height, img_width,3,variances_size) = 0;
list_tk(img_height, img_width, thresholds_size, variances_size) = 0;

for cur_kernel_index=1:variances_size
    % Create a Gaussian using one the values from variances
    gauss = gkern(variances(cur_kernel_index));
    % Get the first derivative of said Gaussian
    dgauss = gkern(variances(cur_kernel_index), 1);

    % Calculate partial derivative with respect to X
    derivative_x = conv2(gauss, dgauss, img, 'valid');
    
    % Calculate partial derivative with respect to Y
    derivative_y = conv2(dgauss, gauss, img, 'valid');

    % Create matrix representing gradient magnitudes (which represent edge
    % strength)
    edge_strength = sqrt(derivative_x .^ 2 + derivative_y .^ 2);
    
    % make binary edge strength images for each threshold value
    for cur_thld_index=1:thresholds_size
        % set the magnitudes which meet the threshold to 1, otherwise to 0
        edge_strength_01 =  edge_strength;
        edge_strength_01 = (edge_strength_01 > thresholds(cur_thld_index));
        % ensure size matches img size
        edge_strength_01(img_height, img_width) = 0;
        % add the image with respect to the current variance to collection
        list_tk(:,:,cur_thld_index,cur_kernel_index) = edge_strength_01; 
    end

    % Create matrix holding grandient orientations, stored as angles
    gradient_orientation = atan2(derivative_y, derivative_x);

    % Modify the gradient orientation matrix to accomodate hsv display
    % rescale orientation angle from [-pi, pi] to [0,1]
    
    scaled_gradient_orientation = (gradient_orientation + pi)/(2*pi);
    % note: we transform with: (value + pi)/2 pi to get 0 to 1 range
    
    % ensure size matches img size
    scaled_gradient_orientation(img_height, img_width) = 0;
    
    % normalize gradient magnitudes to map to saturation values in hsv
    norm_edge_strength = edge_strength / max(edge_strength(:));
    % ensure size matches img size
    norm_edge_strength(img_height, img_width) = 0; 
    list_nes(:,:,cur_kernel_index) = norm_edge_strength;
    
    % supply the values for the hsv (think: the v in hsv)
    hsv_values = ones(img_height, img_width);
    
    % put the hues, saturations, and values together into a hsv
    % representation of the gradients
    derivative_hsv_viz = cat(3, scaled_gradient_orientation, ...
        norm_edge_strength, hsv_values);
    
    % Convert the hsv image into rgb so that we can show it
    derivative_rgb_viz = hsv2rgb(derivative_hsv_viz);
    % ensure size matches img size
    derivative_rgb_viz(img_height, img_width, 3) = 0;
    % put this color orientation image into its collection
    list_co(:,:,:,cur_kernel_index) = derivative_rgb_viz;
end

% reshape kernel/threshold image collection so that montage can display it
kernel_threshold_collection = reshape(list_tk, img_height, img_width, []);

%% CSC 262: Edges

%% Introduction
% In this lab, we investigated methods for identifying edges in an image.
% In particular, we use convolution of the Gaussian derivative to find the
% magnitude and orientation of brightness gradients in an image of a
% caterpillar. Then, we visualized our "edges" from a myriad of
% perspectives--including multi-scale gradient magnitude and orientation
% images. We also compared edge prominance when Gaussian variance (that is,
% kernel size) and thresholds (for the sake of binary edges) were varied.
% We hope to learn how ...TODO...

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

%% Creating Gradients
% We can find edges computationally by looking at the first derivative of
% the brightness (or intensity) values of our image--in this case, the
% image of the caterpillar. Because our image has two-dimensions (an "X"
% and a "Y") we think about the first derivative as a gradient with a
% magnitude and an orientation. First, however, we calculate the partial
% derivative in the X direction with a convolution of the first derivative
% of the Gaussian which we show below. Note, for this section, we assume a
% Gaussian kernel with variance of 1.

% figure;
% subplot(1, 2, 1);
% imshow(img);
% title('Original Image');    
% subplot(1, 2, 2);
% imshow(derivative_x, []);
% title('Derivative With Respect to X');

% We observe that the derivative with respect to X of the caterpillar image
% is brightest along boundaries where regions of low brightness lead
% rightward to areas of high brightness. An example is the boundary created
% when the black stripes on the caterpillar's body meet a white stripe. The
% opposite is also true; the darkest parts of the derivative with respect
% to X caterpillar image lies where white stripes meet dark stripes along
% the x-axis. Finally, the gray regions of this image indicate a region
% where there is no (or little) change in the brightness with respect to X.
% Next, we follow a similar approach to the partial derivative with respect
% to Y (shown below).

figure;
subplot(1, 2, 1);
imshow(img);
title('Original Image');    
subplot(1, 2, 2);
imshow(derivative_y, []);
title('Derivative With Respect to Y');

% A.7 The vertical derivative bug image is brightest along boundaries
% where regions of low brightness lead to areas of high brightness
% vertically down. This is the same pattern as before, but with a different
% orientation. So, regions where the black stripes meet the white stripes
% yields bright spots on the derivative with respect to Y image. Again, the
% opposite is true for the dark parts of the derivative with respect to Y
% bug image and the gray regions of this image indicate a region
% where there is no (or little) change in the brightness with respect to
% the Y.

%% Gradient Magnitude
%

% B.2
%figure;
%imshowpair(bugImg, pd_mag, 'montage');

% The boundaries between the black and white stripes on the
% caterpillars body yield the strongest response in the derivate magnitude
% image. We expect this since those areas display the highest change in
% brightness values with respect to both X and Y directions.

%% Gradient Orientation
%

% set the range to [-pi, pi] inclusive and show with a hsv colormap
% imshow(gradient_orientation, [-pi, pi]);
% colormap(hsv);
% colorbar;
    
%% Edge Detection as Related to Scale and Threshold
%

%figure;
%montage(kernel_threshold_collection, 'Size', [thresholds_size, variances_size]);
%% Thinning Edges
%