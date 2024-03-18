import Statistic.EncodingTree
import Statistic.Bit

main :: IO ()
main = hspec $ do
  describe "isLeaf" $ do
    it "should return True for a leaf node" $ do
      isLeaf (EncodingLeaf 0 'a') `shouldBe` True
    it "should return False for a non-leaf node" $ do
      isLeaf (EncodingNode 0 (EncodingLeaf 0 'a') (EncodingLeaf 0 'b')) `shouldBe` False

  describe "count" $ do
    it "should return the count for a leaf node" $ do
      count (EncodingLeaf 5 'a') `shouldBe` 5
    it "should return the count for a non-leaf node" $ do
      count (EncodingNode 10 (EncodingLeaf 5 'a') (EncodingLeaf 5 'b')) `shouldBe` 10

  -- Example of testing encode function
  describe "encode" $ do
    it "should encode a symbol if it exists in the encoding tree" $ do
      let tree = EncodingLeaf 0 'a'
      encode tree 'a' `shouldBe` Just []
    it "should return Nothing if the symbol does not exist in the encoding tree" $ do
      let tree = EncodingLeaf 0 'a'
      encode tree 'b' `shouldBe` Nothing
