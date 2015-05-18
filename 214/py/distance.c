#include <Python.h>
#include <math.h>

static PyObject *
distance_distance(PyObject *self, PyObject *args)
{
  float x1, y1, x2, y2; //params
  float dx, dy, dist;

  if (!PyArg_ParseTuple(args, "(ff)(ff)", &x1, &y1, &x2, &y2)) {
    return NULL;
  }

  dx = x1 - x2;
  dy = y1 - y2;
  dist = sqrt((dx*dx) + (dy*dy));

  return Py_BuildValue("f", dist);
}

static PyMethodDef module_methods[] = {
    {"distance", distance_distance, METH_VARARGS, "blah blah"},
    {NULL, NULL, 0, NULL}
};

PyMODINIT_FUNC initdistance(void)
{
    Py_InitModule3("distance", module_methods, "blah blah");
}
