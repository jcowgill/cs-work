% Create matrix
A = zeros(100);

A += 10;
A(20:80, 20:80) = 30;
A(30:70, 30:70) = 20;

% Display mesh
mesh(A);
