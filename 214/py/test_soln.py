import unittest
from soln import Boundary, QuadTree

class TestBoundary(unittest.TestCase):
  def test_intersectiong(self):
    b = Boundary((0, 0), 1, 1)

    argss = [
      ((0,  0), 1, 1),
      ((0,  0), 2, 2),
      ((-1, -1), 3, 3),
      ((-1, -1), 1, 1),
      ((-.5, -.5), 1, 1),
      ((.5, .5), 1, 1),
      ((1, 1), 1, 1),
    ]
    
    for args in argss:
      b2 = Boundary(*args)
      self.assertTrue(b.intersects(b2))
      self.assertTrue(b2.intersects(b))

  def test_nonintersecting(self):
    b = Boundary((0, 0), 1, 1)

    argss = [
      ((-1,  -1), .5, .5),
      ((1.5,  1.5), .5, .5),
    ]
    
    for args in argss:
      b2 = Boundary(*args)
      self.assertFalse(b.intersects(b2))
      self.assertFalse(b2.intersects(b))

  def test_children(self):
    b = Boundary((0.0, 0.0), 1.0, 1.0)
    nw, ne, se, sw = b.make_children()
    self.assertEqual((0, 0), sw.bottom_left)
    self.assertEqual((.5, 0), se.bottom_left)
    self.assertEqual((0, .5), nw.bottom_left)
    self.assertEqual((.5, .5), ne.bottom_left)
    for e in (nw, ne, se, sw):
      self.assertEqual(0.5, e.width)
      self.assertEqual(0.5, e.height)

  def test_contains(self):
    b = Boundary((0.0, 0.0), 1.0, 1.0)

    self.assertTrue(b.contains((0.0, 0.0)))
    self.assertTrue(b.contains((.5, .5)))
    self.assertFalse(b.contains((0.0, 1.0)))
    self.assertFalse(b.contains((1.0, 0.0)))
    self.assertFalse(b.contains((1.0, 1.0)))


class TestQuadTree(unittest.TestCase):
  def setUp(self):
    self.bd = Boundary((0.0, 0.0), 1.0, 1.0)

  def test_build(self):
    qt = QuadTree(self.bd, 4)
    self.assertIsNone(qt.nw)

  def test_insert(self):
    qt = QuadTree(self.bd, 1)
    qt.add((.3, .4))
    self.assertIn((.3, .4), qt.pts)
    qt.add((.4, .4))
    self.assertIn((.3, .4), qt.pts)
    self.assertNotIn((.4, .4), qt.pts)
    self.assertIn((.4, .4), qt.sw.pts)

  def test_query(self):
    qt = QuadTree(self.bd, 3)
    for i in range(10):
      for j in range(10):
        pt = (float(i)/10, float(j)/10)
        qt.add(pt)

    small_bd = Boundary((.3, .3), .15, .15)
    pts = qt.query(small_bd)
    for pt in pts:
      self.assertTrue(small_bd.contains(pt))
    self.assertEqual(
        sorted([(.3, .3), (.3, .4), (.4, .3), (.4, .4)]),
        sorted(pts))

  def assertSortedEqual(self, l1, l2, key=None):
    kwargs = {}
    if key is not None:
      kwargs['key'] = key
    self.assertEqual(sorted(l1, **kwargs), sorted(l2, **kwargs))


if __name__ == '__main__':
  unittest.main()
