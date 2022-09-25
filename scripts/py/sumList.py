from itertools import permutations
from joblib import Parallel, delayed
# write N in base q
# sum list with 100 / x_i for ith element
# for some length n list what values x_i sum to 100

def baseQ(n, b, length):
    """
    Convert base 10 into base b.
    """
    if n == 0:
        return [0]
    digits = []
    while n:
        digits.append(int(n % b))
        n //= b
    while len(digits) < length:
        digits = digits + [0] 
    return digits[::-1]

def sumtest(n, users):
    res = baseQ(n, maxPct, users)
    if 0 not in res:
        if 100 == sum([100//d for d in res]): # this is how its defined in haskell
            return res


def createAllPermsOf(aList):
    listOfPerms = list(permutations(aList))
    results = []
    for l in listOfPerms:
        perm = list(l)
        if perm != aList:
            results.append(perm)
    return results


if __name__ == "__main__":
    # define parameters
    users = 5
    maxPct = 25 # 100/maxpct = % contribution; eg maxPct=10 is 10% or more contribution

    # get results
    results = Parallel(n_jobs=12)(delayed(sumtest)(i, users) for i in range(1,pow(maxPct,users)))
    results =  [i for i in results if i != None]

    # filter results
    n = 0
    while True:
        try:
            outputs = createAllPermsOf(results[n])
            for o in outputs:
                if o in results:
                    results.remove(o)
        except IndexError:
            break
        n += 1


    print(results)
    print(len(results))

