## Cauchy problem

Cauchy problem (in its simplest version) is a task of solving differential equation 
of following kind:

\\[
    \frac{df}{dx}(x) = P(f(x), x)
\\]

with constrait \\( f(x_0) = f_0 \\)

## Euler method

One of methods to get numeric solution to given problem is to use euler method.

For given predicate \\(P\\), values \\(x_0\\) and \\(f_0\\), and range [a, b] it constructs
approximate values of \\(f\\) for given set of points on [a, b]

\\[
    f_i = f_{i-1} + P(f_{i-1}, x_{i-1}) * \Delta x
\\]

where \\(\Delta x\\) is distance between \\(x_i\\) and \\(x_{i-1}\\) points. Regulary step is constant, since varying step leads to faster accomulating error and its easier.

##### Note: it's not recommended to use euler method for this problem in real life. It is just used as illustration.

