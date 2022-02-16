-- Always imported
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

windowDisplay :: Display
windowDisplay = InWindow "Window" (400,400) (10,10)

simulationRate :: Int
simulationRate = 20

-- Ship, asteroids, missiles
data World = World SpaceShip [Asteroid] [Missile]
initialWorld :: World
initialWorld = World (SpaceShip (0, 0, 90) (0,0)) 
  [Asteroid (100,0,10) (-10,-20), Asteroid (100,100,10) (0,-20)]
  []

-- (x,y,r), r is size of asteroid. (vx,vy)
data Asteroid = Asteroid (Float, Float, Float) (Float, Float)

-- (x,y,phi), phi is orientation. (vx,vy)
data SpaceShip = SpaceShip (Float, Float, Float)  (Float, Float) 
               | ExplodingSpaceShip 

-- (x,y,phi), phi is orientation. (vx,vy)
data Missile = Missile (Float, Float, Float)  (Float, Float)

rotateSpaceShip :: Float -> SpaceShip -> SpaceShip
rotateSpaceShip dPhi (SpaceShip (x, y, phi) vel) =
   (SpaceShip (x, y, phi + dPhi) vel) 

dv :: Float
dv = 2

accelerateSpaceShip :: SpaceShip -> SpaceShip
accelerateSpaceShip (SpaceShip (x,y,phi) (vx,vy)) =
  SpaceShip (x,y,phi) (vx+dv*(sin (phi*(3.1415/180))), 
                       vy+dv*(cos (phi*(3.1415/180))))

xmin :: Float
xmin = -210
xmax :: Float
xmax = 210
ymin :: Float
ymin = -210
ymax :: Float
ymax = 210
-- This is dt -> (x,y) -> (vx,vy) -> (x,y) !! Obvious rewrite candidate
-- Make the world torroidal
advanceObject :: Float -> (Float, Float) -> (Float, Float) -> (Float, Float)
advanceObject dt (x,y) (vx, vy) = 
    (loop (x+dt*vx) xmin xmax, loop (y+dt*vy) ymin ymax)
   where
     loop u umin umax 
      | u > umax = umin
      | u < umin = umax
      | otherwise = u

moveSpaceShip :: Float -> SpaceShip -> SpaceShip
moveSpaceShip dt (SpaceShip (x,y,phi) (vx,vy)) =
  let (x',y') = advanceObject dt (x,y) (vx,vy) 
  in SpaceShip (x',y',phi) (vx,vy)

moveMissile :: Float -> Missile -> Missile
moveMissile dt (Missile (x,y,phi) (vx,vy)) =
  Missile (x+dt*vx,y+dt*vy,phi) (vx,vy)

moveAsteroid :: Float -> Asteroid -> Asteroid
moveAsteroid dt (Asteroid (x,y,r) (vx,vy)) =
  let (x',y') = advanceObject dt (x,y) (vx,vy) 
  in Asteroid (x',y',r) (vx,vy)

drawSpaceShip :: SpaceShip -> Picture
drawSpaceShip (SpaceShip (x, y, phi) vel) = 
  Translate x y (rotate phi 
    (line [(-15,-15),(0,-5),(15,-15),(0,30),(-15,-15)])) 
  
--drawSpaceShip :: ExplodingSpaceShip -> Picture
--drawSpaceShip ExplodingSpaceShip = undefined 

drawMissile :: Missile -> Picture
drawMissile (Missile (x, y, phi) vel) = 
  Translate x y (rotate phi (line [(0,0),(0,5)])) 

fireMissile :: SpaceShip -> Missile
fireMissile (SpaceShip (x, y, phi) (vx, vy)) = 
  Missile (x + 30*sin (phi*3.1415/180), 
           y + 30*cos (phi*3.1415/180), phi) 
           (vx+50*sin (phi*3.1415/180), 
            vy+50*cos (phi*3.1415/180))

drawAsteroid :: Asteroid -> Picture
drawAsteroid (Asteroid (x, y, r) vel) = Translate x y (Circle r)

-- Blow up asteroids ... make smaller ones
-- Need to remove missiles too !!
{-
detectMissileAsteroidCollision :: Missile -> Asteroid -> [Asteroid]
detectMissileAsteroidCollision (Missile (xm,ym,phim) _)
                               (Asteroid (xa, ya, r) (vxa, vya)) =
 if (xm-xa)**2 + (ym-ya)**2 < r** 2 then
   [] -- collision
 else
   [Asteroid (xa, ya, r) (vxa, vya)]
-}   
detectMissileAsteroidCollision :: Missile -> Asteroid -> Bool 
detectMissileAsteroidCollision (Missile (xm,ym,phim) _)
                               (Asteroid (xa, ya, r) (vxa, vya)) =
  ((xm-xa)**2 + (ym-ya)**2 < r**2)
  --True

handleAsteroidCollision :: Asteroid -> [Asteroid]
handleAsteroidCollision (Asteroid (x, y, r) (vx, vy)) = 
  if r > 5 then
    let 
      (dvx, dvy) = (1.4*vy, -1.4*vx)
    in
      [Asteroid (x,y,r/2.0) (vx+dvx,vy+dvx), 
       Asteroid (x,y,r/2.0) (vx-dvx,vy-dvx)]
  else
    []

-- This requires the space ship geometry
-- Basic test (bounding circle - radius 30)
-- Folowed by more precice test ...
detectSpaceShipAsteroidCollision :: SpaceShip -> Asteroid -> Bool
detectSpaceShipAsteroidCollision  (SpaceShip (xs,ys,phi) _) 
                                  (Asteroid (xa,ya,r) _) =
  if (xa-xs)**2 + (ya-ys)**2 < (r+30)**2 then
    -- bounding cirlce collision : check
    True
  else
    False


main :: IO ()
main = play
  windowDisplay
  --white
  black
  simulationRate
  initialWorld
  drawingFunc
  inputHandler
  updateFunc
{-
drawingFunc :: World -> Picture
drawingFunc (World s as ms) = pictures $ (drawSpaceShip s) : 
  (map drawAsteroid as) ++ (map drawMissile ms)
-}                                    
drawingFunc :: World -> Picture
drawingFunc (World s as ms) = pictures $ map (color white)
  ((drawSpaceShip s) : (map drawAsteroid as) ++ (map drawMissile ms))

ft :: ([Missile],[Asteroid]) -> (Missile,Asteroid) -> ([Missile],[Asteroid])
ft (ms,ma) (m,a) = if detectMissileAsteroidCollision m a then
                     (ms, (handleAsteroidCollision a)++ma)
                   else
                     (m:ms,a:ma)

jt :: Missile -> (Bool,[Asteroid]) -> Asteroid -> (Bool,[Asteroid])
jt m (collision, as) a = if detectMissileAsteroidCollision m a then
                           (collision || True, 
                            (handleAsteroidCollision a)++as)
                         else
                           (collision, a:as)

ht :: ([Missile],[Asteroid]) -> Missile -> ([Missile],[Asteroid])
ht (ms,as) m = let 
                 (c, as') = foldl (jt m) (False,[]) as 
               in
                 if c then
                   (ms, as')
                 else 
                   (m:ms, as')

kt :: SpaceShip -> (Bool, [Asteroid]) -> Asteroid -> (Bool, [Asteroid]) 
kt s (collision,as) a = 
  if detectSpaceShipAsteroidCollision s a then
    (collision || True, (handleAsteroidCollision a) ++ as)
  else
    (collision || False, a:as)

updateFunc :: Float -> World -> World
updateFunc dt (World s as ms) = 
  let (World s' as' ms') = 
       World (moveSpaceShip dt s) (map (moveAsteroid dt) as)
             (map (moveMissile dt) ms)
      -- do collision detection (missiles - asteroids)
      (ms'',as'') = foldl ht ([],as') ms'
      -- do collision detection (spaceship - asteroids)
      (collision, as''') = foldl (kt s) (False, []) as''
  in
      if not collision then
        (World s' as''' ms'')
      else
        (World s' as''' ms'')
        -- (World ExplodingSpaceShip as''' ms'')
        

inputHandler :: Event -> World -> World
inputHandler (EventKey (SpecialKey KeyUp) Down _ _) 
  (World s as ms) = World (accelerateSpaceShip s) as ms
inputHandler (EventKey (SpecialKey KeyRight) Down _ _) 
  (World s as ms) = World (rotateSpaceShip 10 s) as ms
inputHandler (EventKey (SpecialKey KeyLeft) Down _ _) 
  (World s as ms) = World (rotateSpaceShip (-10) s) as ms 
--Fire missile
inputHandler (EventKey (Char ' ') Down _ _) (World s as ms) = 
  World s as (fireMissile s : ms)
inputHandler (EventKey (SpecialKey KeyDown) Down _ _) 
  (World s as ms) = World s as (fireMissile s : ms)
inputHandler _ w = w



