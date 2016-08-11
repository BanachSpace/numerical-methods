module RiemannIntegration where
  
import Data.List

linspace :: (Fractional a) => a -> a -> Int -> [a]
linspace a b n = take (n+1) $ iterate (+ deltaX) a
                            where
                              deltaX = (b-a) * (1/(fromIntegral n))
                              
upperSum :: (Fractional a) => (a -> a) -> a -> a -> Int -> a
upperSum f x0 x1 n = sum $ values
                            where
                              values = [deltaX *f x | x <- tail $ linspace x0 x1 n]
                                   where
                                     deltaX = (x1-x0) * 1/(fromIntegral n)
                                     
lowerSum :: (Fractional a) => (a -> a) -> a -> a -> Int -> a
lowerSum f x0 x1 n = sum $ values 
                            where
                              values = [deltaX * f x | x <- list]
                                  where
                                    deltaX = (x1-x0) * 1/(fromIntegral n)
                                    list = take (n-1) $ linspace x0 x1 n
                                    
riemann :: (RealFrac a) => (a -> a) -> a -> a -> a
riemann f x0 x1 = 0.5*((upperSum f x0 x1 n) + (lowerSum f x0 x1 n))
                          where
                            n = round $ (x1-x0) * 100000                            
