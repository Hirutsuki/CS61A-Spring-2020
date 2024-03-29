from operator import sub, mul
HW_SOURCE_FILE = 'hw03.py'

#############
# Questions #
#############


def num_sevens(x):
    """Returns the number of times 7 appears as a digit of x.

    >>> num_sevens(3)
    0
    >>> num_sevens(7)
    1
    >>> num_sevens(7777777)
    7
    >>> num_sevens(2637)
    1
    >>> num_sevens(76370)
    2
    >>> num_sevens(12345)
    0
    >>> from construct_check import check
    >>> # ban all assignment statements
    >>> check(HW_SOURCE_FILE, 'num_sevens',
    ...       ['Assign', 'AugAssign'])
    True
    """
    "*** YOUR CODE HERE ***"
    if x < 10:
        return 1 if x == 7 else 0
    elif x % 10 == 7:
        return num_sevens(x//10)+1
    else:
        return num_sevens(x//10)


def pingpong(n):
    """Return the nth element of the ping-pong sequence.

    >>> pingpong(7)
    7
    >>> pingpong(8)
    6
    >>> pingpong(15)
    1
    >>> pingpong(21)
    -1
    >>> pingpong(22)
    0
    >>> pingpong(30)
    6
    >>> pingpong(68)
    2
    >>> pingpong(69)
    1
    >>> pingpong(70)
    0
    >>> pingpong(71)
    1
    >>> pingpong(72)
    0
    >>> pingpong(100)
    2
    >>> from construct_check import check
    >>> # ban assignment statements
    >>> check(HW_SOURCE_FILE, 'pingpong', ['Assign', 'AugAssign'])
    True
    """
    "*** YOUR CODE HERE ***"
    def pingpong_iter(pingpong_value, index, transition):
        if index == n:
            return pingpong_value
        if index % 7 == 0 or num_sevens(index):
            return pingpong_iter(pingpong_value-transition, index+1, -transition)
        else:
            return pingpong_iter(pingpong_value+transition, index+1, transition)

    return n if n < 8 else pingpong_iter(7, 7, 1)


def count_change(total):
    """Return the number of ways to make change for total.

    >>> count_change(7)
    6
    >>> count_change(10)
    14
    >>> count_change(20)
    60
    >>> count_change(100)
    9828
    >>> from construct_check import check
    >>> # ban iteration
    >>> check(HW_SOURCE_FILE, 'count_change', ['While', 'For'])
    True
    """
    "*** YOUR CODE HERE ***"
    def max_size(n):
        return n if n <= total else max_size(n//2)

    def count(int_to_count, size):
        if int_to_count == 0:
            return 1
        elif int_to_count < 0 or size == 0:
            return 0
        else:
            return count(int_to_count-size, size)+count(int_to_count, size//2)

    return count(total, max_size(2**(total//2)))


def missing_digits(n):
    """Given a number a that is in sorted, increasing order,
    return the number of missing digits in n. A missing digit is
    a number between the first and last digit of a that is not in n.
    >>> missing_digits(1248) # 3, 5, 6, 7
    4
    >>> missing_digits(1122) # No missing numbers
    0
    >>> missing_digits(123456) # No missing numbers
    0
    >>> missing_digits(3558) # 4, 6, 7
    3
    >>> missing_digits(4) # No missing numbers between 4 and 4
    0
    >>> from construct_check import check
    >>> # ban while or for loops
    >>> check(HW_SOURCE_FILE, 'missing_digits', ['While', 'For'])
    True
    """
    "*** YOUR CODE HERE ***"
    def count(accumulator, before_pointer, pointer):
        one_before_pointer = before_pointer % 10
        if one_before_pointer == 0:
            return accumulator
        if one_before_pointer == pointer:
            return count(accumulator, before_pointer//10, one_before_pointer)
        return count(accumulator+(pointer-one_before_pointer-1), before_pointer//10, one_before_pointer)
    return count(0, n//10, n % 10)

###################
# Extra Questions #
###################


def print_move(origin, destination):
    """Print instructions to move a disk."""
    print("Move the top disk from rod", origin, "to rod", destination)


def move_stack(n, start, end):
    """Print the moves required to move n disks on the start pole to the end
    pole without violating the rules of Towers of Hanoi.

    n -- number of disks
    start -- a pole position, either 1, 2, or 3
    end -- a pole position, either 1, 2, or 3

    There are exactly three poles, and start and end must be different. Assume
    that the start pole has at least n disks of increasing size, and the end
    pole is either empty or has a top disk larger than the top n start disks.

    >>> move_stack(1, 1, 3)
    Move the top disk from rod 1 to rod 3
    >>> move_stack(2, 1, 3)
    Move the top disk from rod 1 to rod 2
    Move the top disk from rod 1 to rod 3
    Move the top disk from rod 2 to rod 3
    >>> move_stack(3, 1, 3)
    Move the top disk from rod 1 to rod 3
    Move the top disk from rod 1 to rod 2
    Move the top disk from rod 3 to rod 2
    Move the top disk from rod 1 to rod 3
    Move the top disk from rod 2 to rod 1
    Move the top disk from rod 2 to rod 3
    Move the top disk from rod 1 to rod 3
    """
    assert 1 <= start <= 3 and 1 <= end <= 3 and start != end, "Bad start/end"
    "*** YOUR CODE HERE ***"
    if n == 1:
        print_move(start, end)
    else:
        move_stack(n-1, start, 6-start-end)
        print_move(start, end)
        move_stack(n-1, 6-start-end, end)


def make_anonymous_factorial():
    """Return the value of an expression that computes factorial.

    >>> make_anonymous_factorial()(5)
    120
    >>> from construct_check import check
    >>> # ban any assignments or recursion
    >>> check(HW_SOURCE_FILE, 'make_anonymous_factorial', ['Assign', 'AugAssign', 'FunctionDef', 'Recursion'])
    True
    """
    # https://stackoverflow.com/questions/481692/can-a-lambda-function-call-itself-recursively-in-python
    # https://www.jb51.net/article/103972.htm
    """
    lambda f: lambda x: 1 if x <= 1 else x*f(x-1)
    lambda f, x: 1 if x <= 1 else x*f(f, x-1)
    lambda f: lambda x: 1 if x <= 1 else x*f(f)(x-1)  # currying
    """
    return (lambda f: f(f))(lambda f: lambda x: 1 if x <= 1 else x * f(f)(x-1))
