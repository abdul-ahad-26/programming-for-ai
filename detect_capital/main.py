class Solution:
    def detectCapitalUse(self, word: str) -> bool:
     
     """ case 1 (all capital):
            return True
        case 2 (all small):
            return True
        case 3 (one capital and other all small)
            return True
        return False
        """
     return word.isupper() or word.islower() or (word[0].isupper() and word[1:].islower()) 

s1 = Solution()

print("USA: ", s1.detectCapitalUse("USA"))
print("leetcode: ", s1.detectCapitalUse("leetcode"))
print("Google: ", s1.detectCapitalUse("Google"))
print("FlaG: ", s1.detectCapitalUse("FlaG"))