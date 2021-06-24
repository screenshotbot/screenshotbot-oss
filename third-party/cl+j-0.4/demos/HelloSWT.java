
//package com.swtjface.Ch2;

import org.eclipse.swt.*;
import org.eclipse.swt.widgets.*;

public class HelloSWT
{
    public static void main(String[] args)
    {
	Display display = new Display();
	Shell shell = new Shell(display);
	
	Text helloText = new Text(shell, SWT.CENTER);
	helloText.setText("Hello SWT!");
	helloText.pack();

	shell.pack();
	shell.open();
	while (!shell.isDisposed())
	    {
		if (!display.readAndDispatch())
		    display.sleep();
	    }
	display.dispose();
    }
}