# rules-hs

**Note for both Q1 and Q2: Since we aren't, here, taking advantage of Haskell's lazy evaluation (looking at every member of a set, using `foldr`, etc.), all complexity analysis can be done the same way as with a strictly evaluated language.**

## Question 1

O(n^2) worst case; O(n lg n) average

Explanation: `pretreat` has two main tasks: converting Rules into a list of (Set Type, Type) tuples, and then sorting. The first step is linear in time complexity, but the sorting is more complex. Using a quicksort-style algorithm, for readability and efficiency, `pretreatSorter` recursively sorts each half of the list around a pivot. Since the halving is linear time, the sort is O(n*h) where h is the height of the recursion tree. In the worst case, this is ~O(n^2), but traditional average case analysis suggests that quicksort (and this function, by extension) can beat other O(nlgn) functions by up to 2-3x (see Skiena, pg 129). So, overall, `pretreat` has a dominant time complexity of O(n^2), worst case, and an average case of ~O(n lg n).

## Question 2

O(n lg n)

Explanation: `inferOutputs` has the task of linearly building the inference set by starting with inferences, and folding over rules to insert conclusions where the premises are a subset of already made inferences. Additional complexity is added at the step where the original assertions are filtered from the final inferences. To do this, we use Haskell's Sets' `difference` function. Performance of that is rated at O(m * lg(n/m + 1)) where m is the length of the smaller of the two sets, and n is the length of the other. So, overall, with respect to the number of rules, this becomes our dominant complexity: ~O(n lg n)

Note: In our fold, each member of our set has `infer` performed on it, so it's worth analyzing how much complexity this adds. `isSubsetOf` is a linear time function, and `insert` is logarithmic, but with respect to the number of accumulated assertions. While this may be larger than the number of rules, this shouldn't be included in the overall complexity with respect to rules. It might be worth adding that, at the expense of readability, `isSubsetOf` could be further optimized to a boolean accumulation checking if each prem in prems is a `member`  of knowledge (O(lgn)). I suspect that another language with a hash-based implementation of sets might be able to do reduce these lookups to constant time.

## Question 3

Conceptually, we first sort the given Rules such that for any rules A and B, if A's conclusion is a premise in B, then A comes before B. (We reverse this since we'll be folding from the right side of the list.) This allows us to capture inferences that depend on that conclusion. Since the problem spec guarantees no cycles, we don't have to worry about these cases.

The inference step considers our cumulative "knowledge" starting with our assertions. Walking through our rules in sorted order, we add conclusions to our knowledge when we come to premises that are a subset of our existing knowledge. Our final set of inferences is the difference between the total knowledge we've accumulated, and the assertions we started with.

This solution uses Sets for more efficient and readable subset checks.
