import Data.List.Split (splitOn)

type Area = (Vec2, Vec2)
type Vec2 = (Int, Int)
type Trajectory = [Vec2]

type Case = Area
type Soln = Int

main :: IO ()
main = interact handle

handle :: String -> String
handle = show . solve . readCase

solve :: Case -> Soln
solve area = maximum validYs
  where
    ((minX, maxX), (minY, maxY)) = area
    velocities = [(vx, vy) | vx <- [1 .. maxX], vy <- [minY .. negate minY]]
    trajectories = map (generateTrajectory area) velocities
    validTrajectories = filter (isValidTrajectory area) trajectories
    validYs = map snd $ concat validTrajectories

readCase :: String -> Area
readCase ('t':'a':'r':'g':'e':'t':' ':'a':'r':'e':'a':':':' ':remainder) = (readRange rawRangeX, readRange rawRangeY)
  where
    [rawRangeX, rawRangeY] = splitOn ", " remainder

readRange :: String -> Vec2
readRange ('x':'=':remainder) = (read from, read to) where [from, to] = splitOn ".." remainder
readRange ('y':'=':remainder) = (read from, read to) where [from, to] = splitOn ".." remainder

generateTrajectory :: Area -> Vec2 -> Trajectory
generateTrajectory ((minX, maxX), (minY, maxY)) = takeWhile (\(_, y) -> y >= minY) . stepPositions (0, 0)

stepPositions :: Vec2 -> Vec2 -> Trajectory
stepPositions (x, y) (vx, vy) = (x, y) : stepPositions (x + vx, y + vy) (vx - signum vx, vy - 1)

isValidTrajectory :: Area -> Trajectory -> Bool
isValidTrajectory area = any (isInArea area)

isInArea :: Area -> Vec2 -> Bool
isInArea ((minX, maxX), (minY, maxY)) (x, y) = minX <= x && x <= maxX && minY <= y && y <= maxY
