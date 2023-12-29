library(tidyverse)
options(digits = 20)

input <- read_lines("input24") %>%
  str_split("[,@] ") %>%
  map(as.numeric) %>%
  do.call(what = rbind)
colnames(input) <- c("px", "py", "pz", "vx", "vy", "vz")

MIN <- 200000000000000
MAX <- 400000000000000

res <- 0
for (i in 1:(nrow(input) - 1))
  for (j in i:nrow(input)) {
    px1 <- input[i,"px"]
    py1 <- input[i,"py"]
    vx1 <- input[i,"vx"]
    vy1 <- input[i,"vy"]
    px2 <- input[j,"px"]
    vx2 <- input[j,"vx"]
    py2 <- input[j,"py"]
    vy2 <- input[j,"vy"]
    if (vx2 * vy1 == vx1 * vy2)
      next
    # x1 = px1 + t * vx1
    # x2 = px2 + t * vx2
    # y = py1 + (x - px1) / vx1 * vy1
    # y = py2 + (x - px2) / vx2 * vy2
    # (x - px1) / vx1 * vy1 - (x - px2) / vx2 * vy2 = py2 - py1
    # (x - px1) * vx2 / vy2 - (x - px2) * vx1 / vy1 = (py2 - py1) * vx1 / vy1 * vx2 / vy2
    # x * (vx2 / vy2 - vx1 / vy1) = (py2 - py1) * vx1 / vy1 * vx2 / vy2 + px1 * vx2 / vy2 - px2 * vx1 / vy1
    x <- ((py2 - py1) * vx1 / vy1 * vx2 / vy2 + px1 * vx2 / vy2 - px2 * vx1 / vy1) / (vx2 / vy2 - vx1 / vy1)
    y <- py1 + (x - px1) / vx1 * vy1
    t1 <- (x - px1) / vx1
    t2 <- (x - px2) / vx2
    if (x >= MIN && x <= MAX && y >= MIN && y <= MAX && min(t1, t2) >= 0)
      res <- res + 1
  }
print(res)

# Maxima solver for part 2

# overconstrained with 4 points to get a single result
# eqns : [
# (346929738756520 - px) / (vx - 6) = (180308062329517 - py) / (vy - -5),
# (254810664927620 - px) / (vx - -144) = (353739895010289 - py) / (vy - 403),
# (295870756794909 - px) / (vx - -23) = (404627177923603 - py) / (vy - -185),
# (245963903877515 - px) / (vx - -175) = (463230951802017 - py) / (vy - -493),
# (180308062329517 - py) / (vy - -5) = (348158644025623 - pz) / (vz - -22),
# (353739895010289 - py) / (vy - 403) = (244141919277765 - pz) / (vz - -76),
# (404627177923603 - py) / (vy - -185) = (198720163538165 - pz) / (vz - 145),
# (463230951802017 - py) / (vy - -493) = (204063276689103 - pz) / (vz - 282)
# ];
# solve(eqns, [px, py, pz, vx, vy, vz]);

# Alternative with a 4x4 linear system for each pair of speeds in 2-dimension

verif_collision <- function(input, i, vx, vy, px, py, axis = 2) {
  px1 <- input[i,"px"]
  py1 <- input[i,axis]
  vx1 <- input[i,"vx"]
  vy1 <- input[i,axis + 3]
  tx <- (px1 - px) / (vx - vx1)
  ty <- (py1 - py) / (vy - vy1)
  if (vx == vx1 && (vy - vy1))
    return(px == px1 && py == py1)
  else if (vx == vx1)
    return(px == px1 && ty == round(ty) && ty >= 0)
  else if (vy == vy1)
    return(py == py1 && tx == round(tx) && tx >= 0)
  return(tx == round(tx) && tx == ty && min(tx, ty) >= 0)
}

intersct <- function(input, i, j, vx, vy, axis = 2) {
  px1 <- input[i,"px"]
  py1 <- input[i,axis]
  vx1 <- input[i,"vx"]
  vy1 <- input[i,axis + 3]
  px2 <- input[j,"px"]
  py2 <- input[j,axis]
  vx2 <- input[j,"vx"]
  vy2 <- input[j,axis + 3]
  # px + t1 * (vx - vx1) = px1
  # py + t1 * (vy - vy1) = py1
  # px + t2 * (vx - vx2) = px2
  # py + t2 * (vy - vy2) = py2
  A <- rbind(c(px = 1L, py = 0, t1 = vx - vx1, t2 = 0),
             c(0, 1, vy - vy1, 0),
             c(1, 0, 0, vx - vx2),
             c(0, 1, 0, vy - vy2))
  if (abs(det(A)) < 0.01)
    return(NULL)
  B <- c(px1, py1, px2, py2)
  sol <- solve(A, B)
  return(sol)
}

find_xy <- function(input, vxs = -300:300, vys = -300:300, axis = 2) {
  for (vx in vxs)
    for (vy in vys) {
      # vx <- 131
      # vy <- -259
      # vy <- 102
      repeat {
        inter <- intersct(input, sample(2:nrow(input), 1), sample(2:nrow(input), 1), vx, vy, axis)
        if (!is.null(inter))
          break
      }
      inter <- round(inter)
      found <- TRUE
      for (i in 1:nrow(input))
        if (!verif_collision(input, i, vx, vy, inter["px"], inter["py"], axis)) {
          found <- FALSE
          break
        }
      if (found)
        return(c(vx = vx, vy = vy, inter))
    }
}
sol1 <- find_xy(input, axis = 2)
sol2 <- find_xy(input, vx = sol1["vx"], axis = 3)
print(sol1["px"] + sol1["py"] + sol2["py"])

# Alternative with a nice trick from reddit

# Consider positions of hailstones with same speed: the speed difference
# (between rock and hailstone) must be a multiple of the position difference
# (between both hailstones)
find_speed <- function(input, axis) {
  speeds <- input[,axis + 3]
  res <- NULL
  for (i in -300:300) {
    keep <- TRUE
    for (v in as.integer(names(table(speeds))[table(speeds) >= 2])) {
      if (i == v || any(diff(input[speeds == v,axis]) %% (i - v) != 0)) {
        keep <- FALSE
        break
      }
    }
    if (keep)
      res <- c(res, i)
  }
  res
}
vx <- find_speed(input, 1)
vy <- find_speed(input, 2)
vz <- find_speed(input, 3)
sol1 <- intersct(input, 1, 2, vx, vy, axis = 2)
sol2 <- intersct(input, 1, 2, vx, vz, axis = 3)
print(sol1["px"] + sol1["py"] + sol2["py"])

# Linearization of the complete system with 6 unknowns and 6 equations

# p[i] + t[i] * v[i] = p + t[i] * v
# p[i] - p = - t[i] * (v[i] - v)
# (p[i] - p) x (v[i] - v) = 0
# p[i] x v[i] - p x v[i] - p[i] x v + p x v = 0
# - p x v[i] - p[i] x v + p x v = -p[i] x v[i]
# (v[1] - v[0]) x p + (p[0] - p[1]) x v = p[0] x v[0] - p[1] x v[1]
cross_mat <- function(vec) {
  matrix(c(0, -vec[3], vec[2],
           vec[3], 0, -vec[1],
           -vec[2], vec[1], 0), byrow = TRUE, nrow = 3)
}

A <- rbind(cbind(input[1:2,4:6] %>% diff %>% cross_mat,
                 input[2:1,1:3] %>% diff %>% cross_mat),
           cbind(input[2:3,4:6] %>% diff %>% cross_mat,
                 input[3:2,1:3] %>% diff %>% cross_mat))
B <- rbind(input[1,1:3] %>% cross_mat %*% input[1,4:6] -
             input[2,1:3] %>% cross_mat %*% input[2,4:6],
           input[2,1:3] %>% cross_mat %*% input[2,4:6] -
             input[3,1:3] %>% cross_mat %*% input[3,4:6])
sol <- solve(A, B)
print(sum(sol[1:3]))
