module PE1 where
import Text.Printf

data Recipe = Recipe String [(String, Double)] deriving Show

data Price = Price String Double Double deriving Show

getRounded :: Double -> Double 
getRounded x = read s :: Double
  where s = printf "%.2f" x
               
getIngredientCost :: (String, Double) -> [Price] -> Double
getIngredientCost (ingredientName, quantity) prices =
  let
    ücret (Price name _ _) = name == ingredientName
    bul _ [] = Nothing
    
    bul p (cur:fol)
      | p cur = Just cur
      | otherwise = bul p fol
      
    ingredientPrice = case bul ücret prices of
      
      Just (Price _ miktar price) -> price / miktar
  in
    getRounded (quantity * ingredientPrice)


recipeCost :: Recipe -> [Price] -> Double
recipeCost (Recipe _ []) _ = 0
recipeCost (Recipe recipeName (ingredient:restOfIngredients)) prices =
  let cost = getIngredientCost ingredient prices
  
      others = recipeCost (Recipe recipeName restOfIngredients) prices
  
  in cost + others

missingIngredients :: Recipe -> [(String, Double)] -> [(String, Double)]
missingIngredients (Recipe _ recipeIngredients) stock =
  filter isMissing (map getDifference recipeIngredients)
  where
    isMissing (_, amount) = amount > 0


    getDifference (ingredient, quantity) =
      let stockQuantity = getStockQuantity ingredient stock
      in (ingredient, quantity - stockQuantity)

    getStockQuantity ingredient stockList =
      foldr (checkStockItem ingredient) 0 stockList


    checkStockItem ingredient (stockIngredient, stockQuantity) accumulator =
      if stockIngredient == ingredient
        then stockQuantity
        else accumulator



makeRecipe :: [(String, Double)] -> Recipe -> [(String, Double)]
makeRecipe stock (Recipe _ ingredients) =
  if all isAvailable ingredients
    then mergeDuplicates [ if k == i then (k, v - q) 
    else (k, v) | (k, v) <- stock, (i, q) <- ingredients,
    k == i || not (any (\(i1, _) -> k == i1) ingredients)]
    else stock
  where
    isAvailable (ingredient, quantity) = maybe False (>= quantity) (lookup ingredient stock)
    mergeDuplicates duplicates = foldr merge [] duplicates

    merge :: (String, Double) -> [(String, Double)] -> [(String, Double)]
    merge (k, v) [] = [(k, v)]
    merge (k, v) ((k1, v1):fol)
      | k == k1 = (k, v ) : fol
      | otherwise = (k1, v1) : merge (k, v) fol

makeShoppingList :: [(String, Double)] -> [Recipe] -> [Price] -> [(String, Double, Double)]
makeShoppingList stock recipes prices =
  let
    ingredientMap = foldr mergeIngredientsMap [] recipes

    mergeIngredientsMap :: Recipe -> [(String, Double)] -> [(String, Double)]
    mergeIngredientsMap recipe acc = mergeIngredients (ingConvert recipe) acc

    pricesIncluded = [(name, quantity, getIngredientCost (name, quantity) prices) | (name, quantity) <- ingredientMap]

    result = differenceList pricesIncluded stock
  in
    result


ingConvert :: Recipe -> [(String, Double)]
ingConvert (Recipe _ ingredients) = ingredients

mergeIngredients :: [(String, Double)] -> [(String, Double)] -> [(String, Double)]
mergeIngredients prev [] = prev
mergeIngredients prev ((name, quantity):rest) =
  let 
    (found, newlist) = tryMerge prev (name, quantity)
  in
    if found
      then mergeIngredients newlist rest
      else mergeIngredients ((name, quantity):prev) rest



tryMerge :: [(String, Double)] -> (String, Double) -> (Bool, [(String, Double)])
tryMerge [] _ = (False, [])
tryMerge ((name1, quan1):prev) (name2, quan2)

    | name1 == name2 = (True, (name1, quan1 + quan2) : prev)
    | 
    otherwise =
        let (found, newlist) = tryMerge prev (name2, quan2)
        in (found, (name1, quan1) : newlist)


differenceList :: [(String, Double, Double)] -> [(String, Double)] -> [(String, Double, Double)]
differenceList ingredients stock = 
  let
    missingIngredientsList = [(name, diffResult, dividedCost) | (name, quantity, cost) <- ingredients,
      let
        stockQuantity = stockchecker name stock
        diffResult = max 0 (quantity - stockQuantity)
        dividedCost = cost * (diffResult / quantity)
      , diffResult > 0]
  in
    missingIngredientsList


stockchecker :: String -> [(String, Double)] -> Double
stockchecker name [] = 0
stockchecker name ((stockName, stockQuantity):stock)
  | name == stockName = stockQuantity
  | otherwise = stockchecker name stock
