def fib(n):
    if n <= 2:
        return 1
    else:
        return fib(n-1) + fib(n-2)


# for i in range(20):
#     print(i+1, fib(i+1))
print(fib(36))


def fac(n):
    if n == 1:
        return 1
    else:
        return n*fac(n-1)


# print(fac(6))
