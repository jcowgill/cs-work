clear A B T
close all

% Create a 2D square stored as homogeneous points:
A(1,1:41)=0:0.025:1;
A(2,1:41)=0;
A(1,42:82)=1;
A(2,42:82)=0:0.025:1;
A(1,83:123)=1:-0.025:0;
A(2,83:123)=1;
A(2,124:164)=1:-0.025:0;
A(1,124:164)=0;
% Make homogeneous:
A(3,:)=1;

% Display the points:
figure;
plot(A(1,:),A(2,:),'.b');
axis equal;
axis([0 10 0 5]);
grid on;
hold;
set(gca,'XTick',0:10);
set(gca,'YTick',0:5);

% Create a transformation matrix
% consisting of a translation of +5 in the x direction
T = [1 0 5; 0 1 0; 0 0 1];

% Transform the points:
B = T*A;

% Display the transformed points:
plot(B(1,:),B(2,:),'.r');
