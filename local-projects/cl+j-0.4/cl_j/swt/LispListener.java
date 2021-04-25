//
// Copyright (c) 2009, Jean-Claude Beaudoin
// All rights reserved by the author.
//
// Permission is hereby granted, free of charge, to any person
// obtaining a copy of this software and associated documentation
// files (the "Software"), to deal in the Software without
// restriction, including without limitation the rights to use, copy,
// modify, merge, publish, distribute, sublicense, and/or sell copies
// of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be
// included in all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
// EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
// MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
// NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
// HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
// WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
// DEALINGS IN THE SOFTWARE.
//

package cl_j.swt;

import org.eclipse.swt.widgets.*;

public class LispListener implements Listener
{
    int ldata; // a "lisp reference" bound to some Lisp data object.
    int lcallback; // a "lisp reference" bound to 
   
    public LispListener(int fun, int data) {
	lcallback = fun;
	ldata = data;
    }

    public LispListener(int fun) {
	lcallback = fun;
	ldata = 0;
    }
   
    native void funcall(int fun, org.eclipse.swt.widgets.Event event, int data);
    //native void funcall(int fun);
    native void freeLispReference(int ref);

    protected void finalize() throws Throwable {
	freeLispReference(ldata);
	freeLispReference(lcallback);
    }

    public void handleEvent(Event event)
    {
	funcall(lcallback, event, ldata);
	//funcall(lcallback);
    }
}
