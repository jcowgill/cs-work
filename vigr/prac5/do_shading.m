function [ shaded ] = do_shading( NormMap, S, V, ka, kd, ks, eta )
%DO_SHADING Shades the image with the given normal map
% NormMap = map of normals (X x Y x 3 matrix)
% S       = light source vector
% V       = viewer vector
% l       = strength of light (0 to 1)
% kd      = diffuse constant (0 to 1)
% ks      = specular constant (0 to 1)

% Normalize S and V vectors
S = S / norm(S);
V = V / norm(V);

% Get sizes
width = size(NormMap, 1);
height = size(NormMap, 2);

% Calculate h vector
H = (S + V) / norm(S + V);

% Initialize output matrix
shaded = zeros(width, height);

% Shade each pixel
for x = 1:width
    for y = 1:height
        % Get normal vector
        N = squeeze(NormMap(x, y, :));

        % Calculate each part
        am = ka;
        di = kd * dot(N, S);
        sp = ks * dot(N, H) ^ eta;

        % Combine parts
        shaded(x, y) = am + di + sp;
    end
end

end
