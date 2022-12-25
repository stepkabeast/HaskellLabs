{-# LANGUAGE ParallelListComp #-}

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.ViewPort

windowDisplay :: Display
windowDisplay = InWindow  "Sun" (800, 800) (0, 0)

main :: IO ()
-- первый Float - время, второй Float - характеристика для поворота
type World = (Float, Float)

lerp :: Float -> Float -> Float -> Float
lerp x0 x1 p = x0 + (x1 - x0) * p

type GradientColor = (Float, Float, Float)
getColorGradient :: GradientColor -> GradientColor-> Float -> Color
getColorGradient (rhsR, rhsG, rhsB) (lhsR, lhsG, lhsB) progress =
  makeColor curR curG curB 1
  where
    fract = abs progress - fromIntegral (floor (abs progress)) :: Float
    curR = lerp rhsR lhsR fract / 256
    curG = lerp rhsG lhsG fract / 256
    curB = lerp rhsB lhsB fract / 256


backgroundColor times = getColorGradient color1 color2 times
  where
    numberHalvesPi = floor (abs times) :: Integer
    color1 = if even numberHalvesPi
      then (42, 231, 245)
      else (20,53,97)
    color2 = if even numberHalvesPi
      then (20,53,97)
      else (42, 231, 245)

earthColor times = getColorGradient color1 color2 times
  where
    numberHalvesPi = floor (abs times) :: Integer
    color1 = if even numberHalvesPi
      then (171,238,10)
      else (5,246,78)
    color2 = if even numberHalvesPi
      then (5,246,78)
      else (171,238,10)

grossColor times = getColorGradient color1 color2 times
  where
    numberHalvesPi = floor (abs times) :: Integer
    color1 = if even numberHalvesPi
      then (70, 100, 33)
      else (67, 78, 83)
    color2 = if even numberHalvesPi
      then (67, 78, 83)
      else (70, 100, 33)

main =
  play  --запускаем в игру в окне, управляя своими собственными входными событиями.
  windowDisplay
  azure
  100 --Количество шагов моделирования, которые необходимо выполнить за каждую секунду реального времени.
  (0, 1) --задаем первоначальный мир
  drawingFunc
  inputHandler
  updateFunc
  where
    drawingFunc :: World -> Picture
    --рисуем объект
    drawingFunc (x, y) = progress x y

    progress :: Float -> Float -> Picture
    progress x y = Pictures [background, earth, grossN1, grossN2, grossN3, sun, sun2, moon, moon2, moonJr, moonJr2]
      where
        rot     = x
        times   = x / 90
        earth
          = Pictures
                [ Translate 0 (-800)
                $ Color (earthColor times) (circleSolid 600) -- возвращает картинку, уже покрашенную
                ]
        
        sunN1
          = Pictures
                [ Translate 0 650
                $ Color yellow (circleSolid 50)
                ]
        sunN2
          = Pictures
                [ Translate 0 (-650)
                $ Color yellow (circleSolid 50)
                ]
        
        moonN1
          = Pictures
                [ Translate 650 0
                $ Color white (circleSolid 40)
                ]
        moonJrN1
          = Pictures
                [ Translate 650 22
                $ Color (backgroundColor times) (circleSolid 28)
                ]
        moonN2
          = Pictures
                [ Translate (-650) 0
                $ Color white (circleSolid 40)
                ]
        moonJrN2
          = Pictures
                [ Translate (-650) (-22)
                $ Color (backgroundColor times) (circleSolid 28)
                ]

        gross = Color (grossColor times) (Polygon [
                (0,0),
                (30,15),
                (30,38),
                (45,45),
                (30,45),
                (15,15),
                (8,60),
                (0,15),
                (-7,45),
                (-15,15),
                (-22,75),
                (-30,15),
                (-38,30),
                (-45,15)
              ])
        grossN1 = Pictures [ Translate (-200) (-300) gross ]
        grossN2 = Pictures [ Translate 150 (-260) 
            $ Scale 1.5 1.5 --увеличили масштаб
            gross ]
        grossN3 = Pictures [ Translate 80 (-350) gross ]

        background
          = Pictures
                [ Translate 0 0
                $ Color (backgroundColor times) (circleSolid 800)
                ]

        -- поворачиваем наши круги
        sun       = Translate 0 (-400)   $ Rotate rot sunN1
        sun2      = Translate 0 (-400)   $ Rotate rot sunN2
        moon      = Translate 0 (-400)   $ Rotate rot moonN1
        moon2     = Translate 0 (-400)   $ Rotate rot moonN2
        moonJr    = Translate 0 (-400)   $ Rotate rot moonJrN1
        moonJr2   = Translate 0 (-400)   $ Rotate rot moonJrN2

    -- набор обработчиков ввода 
    inputHandler :: Event -> World -> World
    inputHandler (EventKey (SpecialKey KeyRight) Down _ _) (x, y) = (x, dy)
      where
        dy = if y > 0 then min (y + 0.5) 2 else 1
    inputHandler (EventKey (SpecialKey KeyLeft) Down _ _) (x, y) = (x, dy)
      where
        dy = if y < 0 then max (y - 0.5) (-2) else -1
    inputHandler (EventKey (SpecialKey KeySpace) Down _ _) (x, y) = (x, 0)
    -- отключаем другие любые обработчики ввода
    inputHandler _ w = w
   
    -- обновление физики, здесь мы просто увеличиваем время (x)
    updateFunc :: Float -> World -> World
    updateFunc _ (x, y) = (\x y -> (x + y * 0.25, y)) x y