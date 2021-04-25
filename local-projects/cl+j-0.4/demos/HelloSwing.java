
import javax.swing.*;

public class HelloSwing {

    private static void sayHelloSwing () {

	JFrame frame = new JFrame("Hello Swing World");
	//frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);

	JLabel label = new JLabel("Hello Swing World from Java!");

	java.awt.Font font = new java.awt.Font("Dialog", java.awt.Font.BOLD, 20);
	label.setFont(font);

	frame.getContentPane().add(label);

	frame.pack();
	frame.setVisible(true);
    }

    public static void main(String[] args) {
	sayHelloSwing();
    }
}

