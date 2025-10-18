from typing import List

class Solution:
    def fallingSquares(self, ps0: List[List[int]]) -> List[int]:
        # segs = sorted(positions, key=lambda pair: (pair[0], pair[1]))
        ps = [[p[0], p[0] + p[1], p[1]] for p in ps0]
        # print(f"ps = {ps}")
        sorted = [ps[0]]
        i = 1
        curr = ps[i]
        remains = False
        while i < len(ps):
            idx = 0
            remains = False
            while idx < len(sorted) and curr[0] >= sorted[idx][1]:
                idx += 1

            if idx == len(sorted):
                sorted.append(curr)
                i += 1
                curr = ps[i] if i < len(ps) else None
            elif curr[1] <= sorted[idx][0]:
                sorted.insert(idx, curr)
                i += 1
                curr = ps[i] if i < len(ps) else None
            else:
                temp = sorted.pop(idx)
                if curr[0] < temp[0]:
                    sorted.insert(idx, [curr[0], temp[0], curr[2]])
                    idx += 1
                elif curr[0] > temp[0]:
                    sorted.insert(idx, [temp[0], curr[0], temp[2]])
                    idx += 1
                next0 = max(curr[0], temp[0])
                if curr[1] > temp[1]:
                    sorted.insert(idx, [next0, temp[1], curr[2] + temp[2]])
                    curr[0] = temp[1]
                    remains = True
                else:
                    sorted.insert(idx, [next0, curr[1], curr[2] + temp[2]])
                    if curr[1] < temp[1]:
                        sorted.insert(idx + 1, [curr[1], temp[1], temp[2]])
                    i += 1
                    curr = ps[i] if i < len(ps) else None

        if remains:
            sorted.append(curr)
        print("sorted = ", len(sorted))
        hs = [t[2] for t in sorted]
        return hs

sol = Solution()
ps = [[1,2],[2,3],[6,1]]
print(f"res = {sol.fallingSquares(ps)}")
