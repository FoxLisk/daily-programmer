import math
import sys
from distance import distance
from functools import wraps

class Boundary(object):
  '''
  Boundary((0,0), 1, 1) constructs something like this:
+---+---+
|0,1|1,1|
+---+---+
|0,0|1,0|
+---+---+
  '''
  def __init__(self, bottom_left, width, height):
    if width <= 0 or height <= 0:
      raise ValueError("Dickhead")
    self.x, self.y = bottom_left
    self.width, self.height = width, height

  @classmethod
  def centered_on(self, pt, size):
    x, y = pt
    return Boundary((x-size/2, y-size/2), size, size)


  @property
  def bottom_left(self):
    return self.x, self.y

  def __repr__(self):
    return '<Boundary((%f, %f), %f, %f)>' % (self.x, self.y, self.width, self.height)

  def contains(self, point):
    x, y = point
    return (
        (self.x <= x < self.x + self.width)
        and
        (self.y <= y < self.y + self.width))

  def intersects(self, other):
    if other.y + other.height < self.y:
      return False
    if self.y + self.height < other.y:
      return False
    if other.x + other.width < self.x:
      return False
    if self.x + self.width < other.x:
      return False
    return True

  def make_children(self):
    hw = self.width/2
    hh = self.height/2
    x, y = self.x, self.y
    sw = Boundary((x, y), hw, hh)
    se = Boundary((x+hw, y), hw, hh)
    nw = Boundary((x, y+hh), hw, hh)
    ne = Boundary((x+hw, y+hh), hw, hh)
    return nw, ne, se, sw


class QuadTree(object):
  def __init__(self, boundary, capacity):
    self.boundary = boundary
    self.capacity = capacity
    self.pts = []
    self.nw, self.ne, self.se, self.sw = None, None, None, None
    self.size = 0

  def __len__(self):
    return self.size

  def __repr__(self):
      return '<QuadTree(%r, %d)>' % (self.boundary, self.capacity)

  def add(self, pt):
    if not self.boundary.contains(pt):
      return False

    self.size += 1

    if len(self.pts) < self.capacity:
      self.pts.append(pt)
      return True

    if self.nw is None:
      self.subdivide()

    if self.nw.add(pt):
      return True
    if self.ne.add(pt):
      return True
    if self.sw.add(pt):
      return True
    if self.se.add(pt):
      return True

    raise Exception("Unable to insert point anywhere!!")

  def query(self, boundary):
    matching = []
    if not self.boundary.intersects(boundary):
      return matching

    for p in self.pts:
      if boundary.contains(p):
        matching.append(p)

    if self.nw is not None:
      for sub in (self.nw, self.ne, self.se, self.sw):
        matching.extend(sub.query(boundary))
    return matching


  def subdivide(self):
    self.nw, self.ne, self.se, self.sw = [QuadTree(b, self.capacity) for b in self.boundary.make_children()]

  def remove(self, point):
    if not self.boundary.contains(point):
      #print '%r doesnt contain %r' % (self, point)
      return False

    if point in self.pts:
      del self.pts[self.pts.index(point)]
      return True

    if self.nw is None:
      return True # it's okay if we can't remove it; just means it wasn't there

    if self.nw.remove(point):
      return True
    if self.ne.remove(point):
      return True
    if self.sw.remove(point):
      return True
    if self.se.remove(point):
      return True

    return True # it's okay if we can't remove it; just means it wasn't there


def dist_with_pop(start, pts):
  best_pt, best_dist, best_idx = None, 9999, None
  for i, p in enumerate(pts):
    dist = distance(start, p)
    if dist < best_dist:
      best_pt, best_dist, best_idx = p, dist, i

  del pts[best_idx]
  return best_pt, best_dist

def process(points, start):
  qt = QuadTree(Boundary((0.0, 0.0), 1.0, 1.0), 4)
  for p in points:
    qt.add(p)
  travel_dist = 0
  while points:
    print "%d treats remain..." % len(points)
    bsize = .02
    matching = []
    while not matching:
      boundary = Boundary.centered_on(start, bsize)
      matching = qt.query(boundary)
      bsize *= 2

    best_pt, best_dist = None, 9999
    for match in matching:
      dist = distance(start, match)
      if dist < best_dist:
        best_pt, best_dist = match, dist


    del points[points.index(best_pt)]
    qt.remove(best_pt)

    travel_dist += best_dist
    start = best_pt
  return travel_dist


def main(fn):
  with open(fn) as f:
    f.readline()  # skip number of inputs
    pts = []
    for line in f:
      x, y = line.strip().split(' ')
      pts.append((float(x), float(y)))

  print process(pts, (.5, .5))

if __name__ == '__main__':
  args = sys.argv[1:]
  if '--prof' in args:
    import cProfile
    cProfile.run('main("input.dat")')
  elif args:
    e = main(sys.argv[1])
  else:
    e = main('input.dat')
  sys.exit(e)

