def permutation(ys):
  def permutation_auxiliary(xs, acc):
    if xs:
       for i, x in enumerate(xs):
         for ps in permutation_auxiliary(xs[:i]+xs[i+1:], acc+[x]):
           yield ps
    else:
       yield acc
  return permutation_auxiliary(ys, [])

for ps in permutation([1,2,3,4]):
  print ps
