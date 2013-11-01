function HDR = genhdr(images, g, t)

% Generate HDR Image
%  Inputs:
%   images = set of images to analyze
%   g      = camera response function
%   t      = exposure times for all the images
%  Outputs:
%   HDR    = final HDR image irradiance

height = size(images, 1);
width = size(images, 2);
channels = size(images, 3);

% Compute logt
logT = log(t);

% Generate static weighting function
w = zeros(256);

for i = 0:127
    w(i + 1) = i;
end
for i = 128:255
    w(i + 1) = 255 - i;
end

% Preallocate HDR array
HDR = zeros(height, width, channels);

% Process each pixel
for row = 1:height
    for col = 1:width
        for chan = 1:channels
            % 1. Extract the vector of intensity values (see part 1)
            H = squeeze(images(row, col, chan, :));

            % 2. Transform the intensities to log intensities using g (see part 3)
            logH = g(H + 1);

            % 3. Compute the weights for the intensities using w (see part 2)
            weights = w(H);

            % 4. Compute the vector difference between the log intensities
            %    and the log of the exposure times in t
            logH_logT = logH - logT;

            % 5. Scale the differences by the weights and take the sum
            sum_weights_logH = sum(weights .* logH_logT);

            % 6. Compute the sum of the weights
            sum_weights = sum(weights);

            % 7. Compute the final HDR values by dividing the result of
            %    step 5 by the result of step 6:
            HDR(row, col, chan) = exp(sum_weights_logH / sum_weights);
        end
    end
end