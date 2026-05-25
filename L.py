from typing import List
import pdb

class Solution1:
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

    def removeComments(self, source: List[str]) -> List[str]:
        opened = False
        lines = []
        prev = ""
        i = 0
        pdb.set_trace()

        while i < len(source) or len(prev) > 0:
            line = prev
            if len(line) == 0 and i < len(source):
                line = source[i]
                i += 1

            print(f"11: i = {i}, line = {line}")

            i0, i1 = line.find("//"), line.find("/*")
            if i1 == i0 and i0 == -1:
                lines.append(line)
                prev = ""
            elif i1 == -1 or i0 != -1 and i0 < i1:
                temp = line[0:i0]
                if len(temp) > 0: lines.append(temp)
                prev = ""
            else:
                prev += line[0:i1]
                while line.find("*/", i1 + 2) == -1:
                    line = source[i]
                    i += 1
                    i1 = -2
                i2 = line.find("*/", i1 + 2)
                prev += line[i2 + 2:]
                print(f"i = {i}, prev = {prev}")

        # if len(prev) > 0: lines.append(prev)
        return lines

    def candy_crush(self, board: List[List[int]]) -> List[List[int]]:
        # Write your code here
        rows, cols = len(board), len(board[0])
        #visited = [[0 for _ in range(cols)] for _ in range(rows)]
        #initial = [[0 for _ in range(cols)] for _ in range(rows)]
        changed = True
        def dfs(r, c):
            if visited[r][c] == 1: return []
            visited[r][c] = 1
            offs = [-1, 0, 1, 0, -1]
            ret = [[r, c]]
            for i in range(len(offs) - 1):
                nr, nc = r + offs[i], c + offs[i + 1]
                if nr < 0 or nc < 0 or nr >= rows or nc >= cols: continue
                if board[nr][nc] != board[r][c] or board[nr][nc] == 0:
                    continue

                ret += dfs(nr, nc)
            return ret

        def pack():
            for r in range(rows - 1, -1, -1):
                for c in range(cols):
                    if board[r][c] != 0: continue
                    r1 = r
                    while r1 >= 0 and board[r1][c] == 0: r1 -= 1
                    if r1 < 0: continue
                    rdiff = r - r1
                    for rr in range(r1, -1, -1):
                        board[rr + rdiff][c] = board[rr][c]
                    for rr in range(rdiff): board[rr][c] = 0

            return

        def dump():
            for row in board:
                fns = [f"{number:{4}d}" for number in row]
                print(' '.join(fns))

        while changed:
            changed = False

            for r in range(rows):
                for c in range(cols):
                    if board[r][c] == 0: continue
                    visited = [[0 for _ in range(cols)] for _ in range(rows)]

                    dups = dfs(r, c)

                    if len(dups) >= 3:
                        print(f"dups = {dups}")
                        for pair in dups: board[pair[0]][pair[1]] = 0
                        changed = True
            print("======before pack=====")
            dump()
            pack()
            print("====after pack====")
            dump()

        return board


    def areSentencesSimilarTwo(
        self, sentence1: List[str], sentence2: List[str], similarPairs: List[List[str]]
    ) -> bool:
        # If sentences have different lengths, they cannot be similar
        if len(sentence1) != len(sentence2):
            return False

        # Initialize parent array for Union-Find
        # Size is 2 * number of pairs to accommodate all possible unique words
        num_pairs = len(similarPairs)
        parent = list(range(num_pairs * 2))

        def find(node: int) -> int:
            """Find the root parent of a node with path compression."""
            if parent[node] != node:
                parent[node] = find(parent[node])  # Path compression
            return parent[node]

        # Map each unique word to a unique index
        word_to_index = {}
        current_index = 0

        # Build the Union-Find structure from similar pairs
        for word1, word2 in similarPairs:
            # Assign index to word1 if not seen before
            if word1 not in word_to_index:
                word_to_index[word1] = current_index
                current_index += 1

            # Assign index to word2 if not seen before
            if word2 not in word_to_index:
                word_to_index[word2] = current_index
                current_index += 1

            # Union the two words by connecting their root parents
            root1 = find(word_to_index[word1])
            root2 = find(word_to_index[word2])
            parent[root1] = root2

        # Check if corresponding words in both sentences are similar
        for i in range(len(sentence1)):
            word_from_s1 = sentence1[i]
            word_from_s2 = sentence2[i]

            # If words are identical, they are similar
            if word_from_s1 == word_from_s2:
                continue

            # Check if both words exist in similarity pairs and belong to same group
            if (word_from_s1 not in word_to_index or
                word_from_s2 not in word_to_index or
                find(word_to_index[word_from_s1]) != find(word_to_index[word_from_s2])):
                return False

        return True
#sol = Solution()
class Solution:
    def __init__(self):
        self.len = 4 * 81
        self.seq = ''

    def dfs0(self, visited, ts, n, k):
        print(f"vs = {len(visited)}, ts = {ts}")
        if len(ts) >= self.len:
            return
        if len(visited) == k ** n:
            #print(f"ts = {ts}")
            if len(ts) < self.len:
                print(f"seq = {ts}")
                self.seq = ts
                self.len = len(ts)
            return

        for i in range(k):
            ns = ts + str(i)
            if len(ns) < n:
                self.dfs(visited, ns, n, k)
                return

            code = ns[-n:]
            if code in visited: continue
            visited.add(code)
            self.dfs(visited, ns, n, k)
            visited.remove(code)

        return

    def dfs(self, visited, ts, n, k):
        if len(visited) == k ** n:
            print(f"ts = {ts}")
            self.seq = str(ts)
            return True

        for i in range(k):
            ns = ts + str(i)

            if len(ns) < n:
                return self.dfs(visited, ns, n, k)
            else:
                code = ns[-n:]
                if code in visited:
                    continue

                visited.add(code)
                if self.dfs(visited, ns, n, k): return True
                #self.dfs(visited, ns, n, k)
                visited.remove(code)

        return False
    def crackSafe(self, n: int, k: int) -> str:
        pds = set()
        self.dfs(pds, '', n, k)
        return self.seq

    def pour_water(self, hs, v, k):
        # Write your code here
        ws = hs[:]
        def findOne(idx, arr, level, origin):
            if idx == origin:
                return (idx == 0 or arr[idx - 1] > arr[idx]) and (idx == len(arr) - 1 or arr[idx] < arr[idx + 1])
            else:
                return (idx == 0 or arr[idx - 1] >= arr[idx]) and (idx == len(arr) - 1 or arr[idx] <= arr[idx + 1]) and (arr[idx] < level)

        #import pdb
        #pdb.set_trace()
        while v > 0:
            print(f"curr = {ws}")
            pos, dest = k, -1
            while pos > 0:
                if ws[pos - 1] > ws[pos]: break
                temp = findOne(pos, ws, ws[k], k)
                if temp and (dest == -1 or ws[pos] < ws[dest]):
                    dest = pos
                pos -= 1

            if dest != -1:
                v -= 1
                ws[dest] += 1
                continue
            pos, dest = k, -1
            while pos < len(ws):
                temp = findOne(pos, ws, ws[k], k)
                if temp and (dest == -1 or ws[pos] < ws[dest]):
                    dest = pos
                pos += 1

            if dest != -1:
                ws[dest] += 1
            else:
                ws[k] += 1

            v -= 1
        return ws


sol = Solution()
arr = [9,5,4,14,10,15,15,2,1,8]
print(f"res = {sol.pour_water(arr, 17, 8)}")
#arr = [13,7,9,6,4,4,4,10,15,9]
#print(f"res = {sol.pour_water(arr, 7, 1)}")
