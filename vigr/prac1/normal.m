% Generate normal distribution from some random numbers
iterations = 10000;
points_per_sample = 10;

% Create data matricies
X = zeros(iterations, 1);

% Generate random data
for i = 1:iterations
    X(i) = mean(rand(points_per_sample, 1));
end

% Plot results
hist(X, iterations / 10)
