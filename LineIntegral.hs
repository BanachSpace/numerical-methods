module LineIntegral where
  
import Data.List

data Vector = Vector Double Double Double deriving (Show, Eq)
data Parametrization = Parametrization (Double -> Double) (Double -> Double) (Double -> Double)

mean :: (Fractional a) => [a] -> a
mean xs = (sum $ xs) / (fromIntegral $ length xs)


linspace :: (Fractional a) => a -> a -> Int -> [a]
linspace a b n = take (n+1) $ iterate (+ deltaX) a
                            where
                              deltaX = (b-a) * (1.0/(fromIntegral n))
  
                            
upperSum :: (Fractional a) => (a -> a) -> a -> a -> Int -> a
upperSum f x0 x1 n = (x1 - x0) * mean(values)
                            where
                              values = [ f (x) | x <- tail $ linspace x0 x1 n]
 
                                                                       
lowerSum :: (Fractional a) => (a -> a) -> a -> a -> Int -> a
lowerSum f x0 x1 n = (x1 - x0) * mean(values)
                            where
                              values = [ (f x) | x <- list]
                                  where
                                    list = take (n) $ linspace x0 x1 n
    
                                
riemann :: (RealFrac a) => (a -> a) -> a -> a -> a
riemann f x0 x1 = ((upperSum f x0 x1 n) + (lowerSum f x0 x1 n))/2
                          where
                            n = round $ (x1-x0) * 100000   
       
                                              
parametrize :: Parametrization -> Double -> Vector
parametrize (Parametrization f g h) x = Vector (f x) (g x) (h x)


derivative :: (Fractional a) => (a -> a) -> a -> a
derivative f x = 1000 * (f x - f (x-(1e-3)))


lineElement :: (Fractional a) => Parametrization -> Double -> Double
lineElement (Parametrization x0 x1 x2) t = sqrt $ xt'
                        where
                           xt' = (derivative x0 t)**2 + (derivative x1 t)**2 + (derivative x2 t)**2
                                   
      
lineIntegral :: (RealFrac a) => Parametrization -> Double -> Double -> Double
lineIntegral xt a b = riemann (lineElement xt) a b

