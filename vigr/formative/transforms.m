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
TOld = [1 0 5;
        0 1 0;
        0 0 1];

% START OF ANSWERS
%  James Cowgill
% ############################

% Enlarge by a factor of 2 (both directions) and then apply TOld
T1Enlarge = [2 0 0;
             0 2 0;
             0 0 1];

T1 = TOld * T1Enlarge;

% Enlarge by a factor of 3 in x direction, then transform by (5, 3)
T2 = [3 0 5;
      0 1 3;
      0 0 1];

% Translate by (1, 0), then rotate clockwise by pi/4
theta = pi / 4;
T3Rotate = [cos(theta) -sin(theta) 0
            sin(theta)  cos(theta) 0
            0           0          1];

T3Translate = [1 0 1
               0 1 0
               0 0 1];

T3 = T3Rotate * T3Translate;

% Translate by (-0.5, -0.5) - centre on origin
%  Rotate clockwise by pi / 4 (stolen from T3Rotate)
%  Translate by (3, 3)
T4Part1 = [1 0 -0.5
           0 1 -0.5
           0 0 1   ];

T4Part2 = T3Rotate;

T4Part3 = [1 0 3
           0 1 3
           0 0 1];

T4 = T4Part3 * T4Part2 * T4Part1;

% As T4 but enlarge by a factor of 2 (both ways) after the first translate
%  Enlargement stolen from T1Enlarge
T5Enlarge = T1Enlarge;

T5 = T4Part3 * T4Part2 * T5Enlarge * T4Part1;

% Shear by 1 in x direction, then translate by (2, 2)
T6Shear = [1 1 0
           0 1 0
           0 0 1];

T6Translate = [1 0 2
               0 1 2
               0 0 1];

T6 = T6Translate * T6Shear;

% ############################
% END OF ANSWERS

% Extra test plots
% ############################~
B = T1*A; plot(B(1,:),B(2,:),'.m');
B = T2*A; plot(B(1,:),B(2,:),'.g');
B = T3*A; plot(B(1,:),B(2,:),'.y');
B = T4*A; plot(B(1,:),B(2,:),'.c');
B = T5*A; plot(B(1,:),B(2,:),'.k');
% ############################~

% Transform the points:
T = T6;
B = T*A;

% Display the transformed points:
plot(B(1,:),B(2,:),'.r');
